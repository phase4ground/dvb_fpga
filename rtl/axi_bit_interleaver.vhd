--
-- DVB IP
--
-- Copyright 2019 by Suoto <andre820@gmail.com>
--
-- This file is part of DVB IP.
--
-- DVB IP is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- DVB IP is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with DVB IP.  If not, see <http://www.gnu.org/licenses/>.

---------------
-- Libraries --
---------------
library	ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library fpga_cores;
use fpga_cores.common_pkg.all;

use work.dvb_utils_pkg.all;

------------------------
-- Entity declaration --
------------------------
entity axi_bit_interleaver is
  generic (
    TDATA_WIDTH : positive := 8;
    TID_WIDTH   : positive := 8
  );
  port (
    -- Usual ports
    clk               : in  std_logic;
    rst               : in  std_logic;

    cfg_constellation : in  constellation_t;
    cfg_frame_type    : in  frame_type_t;
    cfg_code_rate     : in  code_rate_t;

    -- AXI input
    s_tvalid          : in  std_logic;
    s_tlast           : in  std_logic;
    s_tready          : out std_logic;
    s_tdata           : in  std_logic_vector(TDATA_WIDTH - 1 downto 0);
    s_tid             : in  std_logic_vector(TID_WIDTH - 1 downto 0);

    -- AXI output
    m_tready          : in  std_logic;
    m_tvalid          : out std_logic;
    m_tlast           : out std_logic;
    m_tdata           : out std_logic_vector(TDATA_WIDTH - 1 downto 0);
    m_tid             : out std_logic_vector(TID_WIDTH - 1 downto 0));
end axi_bit_interleaver;

architecture axi_bit_interleaver of axi_bit_interleaver is

  ---------------
  -- Constants --
  ---------------
  constant RAM_PTR_WIDTH : integer := 2;
  constant MAX_ROWS      : integer := 21_600 / TDATA_WIDTH;
  constant MAX_COLUMNS   : integer := 5;

  -- type addr_array_t is array (natural range <>)
  --   of unsigned(numbits(MAX_ROWS) + RAM_PTR_WIDTH - 1 downto 0);
  type data_array_t is array (natural range <>) of std_logic_vector(TDATA_WIDTH - 1 downto 0);

  -- RAM write interface
  type ram_wr_t is record
    addr : unsigned(numbits(MAX_ROWS) + RAM_PTR_WIDTH - 1 downto 0);
    data : std_logic_vector(TDATA_WIDTH - 1 downto 0);
    en   : std_logic;
  end record;

  type ram_wr_array_t is array (MAX_COLUMNS - 1 downto 0) of ram_wr_t;

  -------------
  -- Signals --
  -------------
  -- Delayed AXI data
  signal axi_tdata                 : std_logic_vector(TDATA_WIDTH - 1 downto 0);
  signal axi_tvalid                : std_logic;
  signal axi_tready                : std_logic;
  signal axi_tlast                 : std_logic;

  signal s_tready_i                : std_logic;
  signal wr_handler_ready          : std_logic;

  signal axi_dv                    : std_logic;
  signal s_tlast_reg               : std_logic;

  -- RAM base pointers to handle back to back frames by writing and reading from different
  -- regions of the RAM. This will introduce 1 frame of latency though
  signal ram_ptr_diff              : unsigned(RAM_PTR_WIDTH - 1 downto 0);
  signal wr_ram_ptr                : unsigned(RAM_PTR_WIDTH - 1 downto 0);
  signal rd_ram_ptr                : unsigned(RAM_PTR_WIDTH - 1 downto 0);

  -- Write side config
  signal cfg_wr_constellation      : constellation_t;
  signal cfg_wr_frame_type         : frame_type_t;
  signal cfg_wr_code_rate          : code_rate_t;

  signal cfg_wr_last_row           : unsigned(numbits(MAX_ROWS) - 1 downto 0);
  signal cfg_wr_last_column        : unsigned(numbits(MAX_COLUMNS) - 1 downto 0);
  signal cfg_wr_remainder          : unsigned(numbits(TDATA_WIDTH) - 1 downto 0);
  signal cfg_wr_tid                : std_logic_vector(TID_WIDTH - 1 downto 0);

  signal cfg_fifo_wren             : std_logic;
  signal cfg_fifo_full             : std_logic;
  signal cfg_fifo_upper            : std_logic;
  -- Write side counters
  signal bit_cnt                   : unsigned(2*numbits(TDATA_WIDTH) - 1 downto 0);
  signal wr_row_cnt                : unsigned(numbits(MAX_ROWS) - 1 downto 0);
  signal wr_column_cnt             : unsigned(numbits(MAX_COLUMNS) - 1 downto 0);
  -- signal wr_column_cnt_reg1        : unsigned(numbits(MAX_COLUMNS) - 1 downto 0);
  signal wr_remainder              : unsigned(numbits(TDATA_WIDTH) - 1 downto 0);

  signal wr_addr_init              : std_logic := '0';

  signal ram_wr_en                 : std_logic_vector(MAX_COLUMNS - 1 downto 0);
  signal ram_wr_addr               : unsigned(numbits(MAX_ROWS) + RAM_PTR_WIDTH - 1 downto 0);
  signal ram_wr_data               : std_logic_vector(TDATA_WIDTH - 1 downto 0);

  -- When addressing the RAMs we don't need the pointer part of the address
  signal ram_wr_addr_no_pointer    : std_logic_vector(numbits(MAX_ROWS) downto 0);
  signal ram_rd_addr_no_pointer    : std_logic_vector(numbits(MAX_ROWS) downto 0);

  signal tdata_sr_reg              : std_logic_vector(3*TDATA_WIDTH - 1 downto 0);

  -- Read side config
  signal reading_frame             : std_logic;

  signal cfg_fifo_rd_constellation : constellation_t;
  signal cfg_fifo_rd_frame_type    : frame_type_t;
  signal cfg_fifo_rd_code_rate     : code_rate_t;
  signal cfg_fifo_rd_last_row      : unsigned(numbits(MAX_ROWS) - 1 downto 0);
  signal cfg_fifo_rd_last_column   : unsigned(numbits(MAX_COLUMNS) - 1 downto 0);
  signal cfg_fifo_rd_remainder     : unsigned(numbits(TDATA_WIDTH) - 1 downto 0);
  signal cfg_fifo_rd_tid           : std_logic_vector(TID_WIDTH - 1 downto 0);
  signal cfg_fifo_rden             : std_logic;
  signal cfg_fifo_rddv             : std_logic;
  signal cfg_fifo_empty            : std_logic;

  signal cfg_rd_constellation      : constellation_t;
  signal cfg_rd_frame_type         : frame_type_t;
  signal cfg_rd_code_rate          : code_rate_t;
  signal cfg_rd_last_row           : unsigned(numbits(MAX_ROWS) - 1 downto 0);
  signal cfg_rd_last_column        : unsigned(numbits(MAX_COLUMNS) - 1 downto 0);
  signal cfg_rd_remainder          : unsigned(numbits(TDATA_WIDTH) - 1 downto 0);
  signal cfg_rd_tid                : std_logic_vector(TID_WIDTH - 1 downto 0);

  signal rd_row_cnt                : unsigned(numbits(MAX_ROWS) - 1 downto 0);

  signal rd_column_cnt             : unsigned(numbits(MAX_COLUMNS) - 1 downto 0);
  signal rd_column_cnt_reg         : unsigned(numbits(MAX_COLUMNS) - 1 downto 0);

  -- RAM read interface
  signal ram_rd_addr               : unsigned(numbits(MAX_ROWS) + RAM_PTR_WIDTH - 1 downto 0);
  signal ram_rd_data               : data_array_t(0 to MAX_COLUMNS - 1);

  signal rd_data_sr                : std_logic_vector(MAX_COLUMNS*TDATA_WIDTH - 1 downto 0);

  signal interleaved_3c_012        : std_logic_vector(3*TDATA_WIDTH - 1 downto 0);
  signal interleaved_3c_210        : std_logic_vector(3*TDATA_WIDTH - 1 downto 0);
  signal interleaved_4c_0123       : std_logic_vector(4*TDATA_WIDTH - 1 downto 0);
  signal interleaved_4c_3210       : std_logic_vector(4*TDATA_WIDTH - 1 downto 0);
  signal interleaved_4c_3201       : std_logic_vector(4*TDATA_WIDTH - 1 downto 0);
  signal interleaved_5c            : std_logic_vector(5*TDATA_WIDTH - 1 downto 0);

  signal m_wr_en                   : std_logic := '0';
  signal m_wr_en_reg               : std_logic := '0';
  signal m_wr_full                 : std_logic;
  signal m_wr_data                 : std_logic_vector(TDATA_WIDTH - 1 downto 0);
  signal m_wr_tid                  : std_logic_vector(TID_WIDTH - 1 downto 0);
  signal m_wr_last                 : std_logic := '0';
  signal m_wr_last_reg             : std_logic := '0';

  signal rd_first_word             : std_logic; -- To sample config

begin

  -------------------
  -- Port mappings --
  -------------------
  s_axi_delay_block : block
    signal tdata_in  : std_logic_vector(TDATA_WIDTH downto 0);
    signal tdata_out : std_logic_vector(TDATA_WIDTH downto 0);
  begin
    -- Delay the incoming AXI data so we can register the config
    s_axi_delay_u : entity fpga_cores.axi_stream_delay
        generic map (
            DELAY_CYCLES => 1,
            TDATA_WIDTH  => TDATA_WIDTH + 1)
        port map (
            -- Usual ports
            clk      => clk,
            rst      => rst,

            -- AXI slave input
            s_tvalid => s_tvalid,
            s_tready => s_tready_i,
            s_tdata  => tdata_in,

            -- AXI master output
            m_tvalid => axi_tvalid,
            m_tready => axi_tready,
            m_tdata  => tdata_out);

      tdata_in <= s_tlast & s_tdata;

      axi_tdata <= tdata_out(TDATA_WIDTH - 1 downto 0);
      axi_tlast <= tdata_out(TDATA_WIDTH);
  end block;

  -- Generate 1 RAM for each column, each one gets written sequentially
  generate_rams : for column in 0 to MAX_COLUMNS - 1 generate
  begin

    ram : entity fpga_cores.ram_inference
      generic map (
        ADDR_WIDTH   => numbits(MAX_ROWS) + 1,
        DATA_WIDTH   => TDATA_WIDTH,
        RAM_TYPE     => auto,
        -- TODO: Adjust the pipeline to handle OUTPUT_DELAY = 2 to get better timing on
        -- Xilinx devices (see message Synth 8-7053)
        OUTPUT_DELAY => 1)
      port map (
        -- Port A
        clk_a     => clk,
        clken_a   => '1',
        wren_a    => ram_wr_en(column),
        addr_a    => ram_wr_addr_no_pointer,
        wrdata_a  => ram_wr_data,
        rddata_a  => open,
        -- Port B
        clk_b     => clk,
        clken_b   => '1',
        addr_b    => ram_rd_addr_no_pointer,
        rddata_b  => ram_rd_data(column));
  end generate generate_rams;

  axi_master_adapter_block : block
    signal wr_data_agg : std_logic_vector(TDATA_WIDTH + TID_WIDTH - 1 downto 0);
    signal rd_data_agg : std_logic_vector(TDATA_WIDTH + TID_WIDTH - 1 downto 0);
  begin
    wr_data_agg <= m_wr_tid & m_wr_data;

    m_tdata <= rd_data_agg(TDATA_WIDTH - 1 downto 0);
    m_tid   <= rd_data_agg(TID_WIDTH + TDATA_WIDTH - 1 downto TDATA_WIDTH);

    -- Interleaved data takes 1 cycle after the address has changed, add support for
    -- a couple of cycles to stop the pipeline
    axi_master_adapter_u : entity fpga_cores.axi_stream_master_adapter
      generic map (
        MAX_SKEW_CYCLES => 3,
        TDATA_WIDTH     => wr_data_agg'length)
      port map (
        -- Usual ports
        clk      => clk,
        reset    => rst,
        -- Wannabe AXI interface
        wr_en    => m_wr_en_reg,
        wr_full  => m_wr_full,
        wr_data  => wr_data_agg,
        wr_last  => m_wr_last_reg,
        -- AXI master
        m_tvalid => m_tvalid,
        m_tready => m_tready,
        m_tdata  => rd_data_agg,
        m_tlast  => m_tlast);
  end block;

  -- Write and read side might be on different timings, decouple both sides with a FIFO to
  -- pass both the config parameters and the number of rows and columns
  cfg_fifo_block : block
    constant FIELD_WIDTHS : integer_vector_t := (
      0 => FRAME_TYPE_WIDTH,
      1 => CONSTELLATION_WIDTH,
      2 => CODE_RATE_WIDTH,
      3 => numbits(TDATA_WIDTH), -- remainder
      4 => numbits(MAX_COLUMNS), -- last column
      5 => numbits(MAX_ROWS),    -- last row
      6 => TID_WIDTH
    );

    constant CFG_FIFO_DATA_WIDTH : integer := sum(FIELD_WIDTHS);

    signal wr_data : std_logic_vector(CFG_FIFO_DATA_WIDTH - 1 downto 0);
    signal rd_data : std_logic_vector(CFG_FIFO_DATA_WIDTH - 1 downto 0);

  begin

    wr_data <= cfg_wr_tid &
               std_logic_vector(cfg_wr_last_row) &
               std_logic_vector(cfg_wr_last_column) &
               std_logic_vector(cfg_wr_remainder) &
               encode(cfg_wr_code_rate) &
               encode(cfg_wr_constellation) &
               encode(cfg_wr_frame_type);

    cfg_fifo_rd_tid           <= get_field(rd_data, 6, FIELD_WIDTHS);
    cfg_fifo_rd_last_row      <= unsigned(std_logic_vector'(get_field(rd_data, 5, FIELD_WIDTHS)));
    cfg_fifo_rd_last_column   <= unsigned(std_logic_vector'(get_field(rd_data, 4, FIELD_WIDTHS)));
    cfg_fifo_rd_remainder     <= unsigned(std_logic_vector'(get_field(rd_data, 3, FIELD_WIDTHS)));

    cfg_fifo_rd_code_rate     <= decode(get_field(rd_data, 2, FIELD_WIDTHS));
    cfg_fifo_rd_constellation <= decode(get_field(rd_data, 1, FIELD_WIDTHS));
    cfg_fifo_rd_frame_type    <= decode(get_field(rd_data, 0, FIELD_WIDTHS));

    cfg_fifo_u : entity fpga_cores.sync_fifo
      generic map (
        -- FIFO configuration
        RAM_TYPE       => auto,
        DEPTH          => 2,
        DATA_WIDTH     => CFG_FIFO_DATA_WIDTH,
        UPPER_TRESHOLD => 1,
        LOWER_TRESHOLD => 0)
      port map (
        -- Write port
        clk      => clk,
        clken    => '1',
        rst      => rst,

        -- Status
        full     => cfg_fifo_full,
        upper    => cfg_fifo_upper,
        lower    => open,
        empty    => cfg_fifo_empty,

        wr_en    => cfg_fifo_wren,
        wr_data  => wr_data,

        -- Read port
        rd_en    => cfg_fifo_rden,
        rd_data  => rd_data,
        rd_dv    => cfg_fifo_rddv);
    end block;

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  cfg_fifo_wren <= axi_dv and axi_tlast;

  m_wr_data <= mirror_bits(rd_data_sr(TDATA_WIDTH - 1 downto 0));

  -- Assign the interleaved data statically
  iter_rows : for row in 0 to TDATA_WIDTH - 1 generate
    iter_3_columns : for column in 0 to 2 generate
      interleaved_3c_012(3*TDATA_WIDTH - (3 * row + column) - 1) <= ram_rd_data(column)(TDATA_WIDTH - row - 1);
      interleaved_3c_210(3*TDATA_WIDTH - (3 * row + column) - 1) <= ram_rd_data(2 - column)(TDATA_WIDTH - row - 1);
    end generate;

    interleaved_4c_0123(4*TDATA_WIDTH - (4 * row + 0) - 1) <= ram_rd_data(0)(TDATA_WIDTH - row - 1);
    interleaved_4c_0123(4*TDATA_WIDTH - (4 * row + 1) - 1) <= ram_rd_data(1)(TDATA_WIDTH - row - 1);
    interleaved_4c_0123(4*TDATA_WIDTH - (4 * row + 2) - 1) <= ram_rd_data(2)(TDATA_WIDTH - row - 1);
    interleaved_4c_0123(4*TDATA_WIDTH - (4 * row + 3) - 1) <= ram_rd_data(3)(TDATA_WIDTH - row - 1);

    interleaved_4c_3210(4*TDATA_WIDTH - (4 * row + 0) - 1) <= ram_rd_data(3)(TDATA_WIDTH - row - 1);
    interleaved_4c_3210(4*TDATA_WIDTH - (4 * row + 1) - 1) <= ram_rd_data(2)(TDATA_WIDTH - row - 1);
    interleaved_4c_3210(4*TDATA_WIDTH - (4 * row + 2) - 1) <= ram_rd_data(1)(TDATA_WIDTH - row - 1);
    interleaved_4c_3210(4*TDATA_WIDTH - (4 * row + 3) - 1) <= ram_rd_data(0)(TDATA_WIDTH - row - 1);


    interleaved_4c_3201(4*TDATA_WIDTH - (4 * row + 0) - 1) <= ram_rd_data(3)(TDATA_WIDTH - row - 1);
    interleaved_4c_3201(4*TDATA_WIDTH - (4 * row + 1) - 1) <= ram_rd_data(2)(TDATA_WIDTH - row - 1);
    interleaved_4c_3201(4*TDATA_WIDTH - (4 * row + 2) - 1) <= ram_rd_data(0)(TDATA_WIDTH - row - 1);
    interleaved_4c_3201(4*TDATA_WIDTH - (4 * row + 3) - 1) <= ram_rd_data(1)(TDATA_WIDTH - row - 1);

    iter_5_columns : for column in 0 to 4 generate
      interleaved_5c(5*TDATA_WIDTH - (5 * row + column) - 1) <= ram_rd_data(column)(TDATA_WIDTH - row - 1);
    end generate iter_5_columns;
  end generate iter_rows;

  -- The upper bits select the column, slice off the LSB for the RAM's address
  ram_wr_addr_no_pointer <= std_logic_vector(ram_wr_addr(numbits(MAX_ROWS) downto 0));
  ram_rd_addr_no_pointer <= std_logic_vector(ram_rd_addr(numbits(MAX_ROWS) downto 0));

  ram_ptr_diff <= wr_ram_ptr - rd_ram_ptr when wr_ram_ptr > rd_ram_ptr else
                  2**RAM_PTR_WIDTH + wr_ram_ptr - rd_ram_ptr;

  axi_tready   <= wr_handler_ready and not cfg_fifo_full;

  axi_dv       <= '1' when axi_tready = '1' and axi_tvalid = '1' else '0';

  s_tready     <= s_tready_i;

  --------------------------------
  -- Handle write side pointers --
  --------------------------------
  write_side_p : process(clk, rst)
    variable wr_column_cnt_i : natural range 0 to MAX_COLUMNS - 1;
    variable tdata_sr        : std_logic_vector(tdata_sr_reg'range);
    -- Bit counter has to be unsigned instead of integer w range because of an issue with
    -- GHDL synth
    variable bit_cnt_v       : unsigned(bit_cnt'range) := (others => '0');
  begin
    if rst = '1' then
      wr_column_cnt    <= (others => '0');
      wr_row_cnt       <= (others => '0');
      wr_remainder     <= (others => '0');
      wr_ram_ptr       <= (others => '0');
      ram_wr_en        <= (others => '0');

      wr_handler_ready <= '1';
      wr_addr_init     <= '1';

      -- Assign registers to avoid accidentally having logic with reset
      ram_wr_addr  <= (others => 'U');
      ram_wr_data  <= (others => 'U');

      bit_cnt      <= (others => 'U');
      tdata_sr_reg <= (others => 'U');
      tdata_sr     := (others => 'U');

      s_tlast_reg  <= 'U';

    elsif rising_edge(clk) then

      wr_handler_ready <= '1';

      -- Only to reduce footprint of converting to integer when slicing vectors
      wr_column_cnt_i := to_integer(wr_column_cnt);

      if axi_dv = '1' then
        tdata_sr      := tdata_sr(tdata_sr'length - TDATA_WIDTH - 1 downto 0) & axi_tdata;
        bit_cnt_v     := bit_cnt_v + TDATA_WIDTH;
        s_tlast_reg   <= axi_tlast;
        if axi_tlast = '1' then
          wr_handler_ready <= '0';
        end if;
      end if;

      ram_wr_en <= (others => '0');

      -- Increment RAM addr every time any gets written
      if ram_wr_en /= (MAX_COLUMNS - 1 downto 0 => '0') then
        ram_wr_addr <= ram_wr_addr + 1;
      end if;

      if s_tlast_reg = '1' then

        s_tlast_reg                <= '0';
        ram_wr_en(wr_column_cnt_i) <= '1';

        -- FIXME: Attempts to make this independent of TDATA_WIDTH all failed with
        -- non-synthesizable constructs, but would be a nice to have
        case to_integer(bit_cnt_v) is
          when      0 => ram_wr_data <= (TDATA_WIDTH - 0 - 1 downto  0 => '0');
          when      1 => ram_wr_data <= tdata_sr(1 - 1 downto 0) & (TDATA_WIDTH - 1 - 1 downto  0 => '0');
          when      2 => ram_wr_data <= tdata_sr(2 - 1 downto 0) & (TDATA_WIDTH - 2 - 1 downto  0 => '0');
          when      3 => ram_wr_data <= tdata_sr(3 - 1 downto 0) & (TDATA_WIDTH - 3 - 1 downto  0 => '0');
          when      4 => ram_wr_data <= tdata_sr(4 - 1 downto 0) & (TDATA_WIDTH - 4 - 1 downto  0 => '0');
          when      5 => ram_wr_data <= tdata_sr(5 - 1 downto 0) & (TDATA_WIDTH - 5 - 1 downto  0 => '0');
          when      6 => ram_wr_data <= tdata_sr(6 - 1 downto 0) & (TDATA_WIDTH - 6 - 1 downto  0 => '0');
          when      7 => ram_wr_data <= tdata_sr(7 - 1 downto 0) & (TDATA_WIDTH - 7 - 1 downto  0 => '0');
          when others => ram_wr_data <= tdata_sr(TDATA_WIDTH - 1 downto 0);
        end case;

        -- This is the most concise way to express the above
        -- ram_wr_data
        --   <= tdata_sr(minimum(to_integer(bit_cnt_v), TDATA_WIDTH) - 1 downto 0)
        --      & (TDATA_WIDTH - minimum(to_integer(bit_cnt_v), TDATA_WIDTH) - 1 downto 0 => '0');

        tdata_sr      := (others => 'U');
        bit_cnt_v     := (others => '0');
        wr_column_cnt <= (others => '0');
        wr_row_cnt    <= (others => '0');
        wr_ram_ptr    <= wr_ram_ptr + 1;
        wr_addr_init  <= '1';
      elsif bit_cnt_v >= TDATA_WIDTH then

        ram_wr_en(wr_column_cnt_i) <= '1';
        ram_wr_data                <= tdata_sr(to_integer(bit_cnt_v) - 1 downto to_integer(bit_cnt_v) - TDATA_WIDTH);

        wr_addr_init               <= '0';

        -- Initialize each RAM's initial write address at every first row
        if wr_addr_init = '1' then
          ram_wr_addr  <= wr_ram_ptr & (numbits(MAX_ROWS) - 1 downto 0 => '0');
        end if;

        wr_row : if wr_row_cnt /= cfg_wr_last_row then
          wr_row_cnt   <= wr_row_cnt + 1;
          bit_cnt_v    := bit_cnt_v - TDATA_WIDTH;
        else
          wr_addr_init <= '1';
          wr_row_cnt   <= (others => '0');
          wr_remainder <= wr_remainder + cfg_wr_remainder;

          bit_cnt_v    := bit_cnt_v - to_integer(cfg_wr_remainder);

          -- Chain counters
          if wr_column_cnt = cfg_wr_last_column then
            wr_column_cnt <= (others => '0');
            wr_ram_ptr    <= wr_ram_ptr + 1;
          else
            wr_column_cnt <= wr_column_cnt + 1;
          end if;

        end if wr_row;

        if bit_cnt_v >= TDATA_WIDTH then
          wr_handler_ready <= '0';
        end if;

      end if;

      bit_cnt      <= bit_cnt_v;
      tdata_sr_reg <= tdata_sr;

    end if;
  end process write_side_p;

  -------------------------------
  -- Handle read side pointers --
  -------------------------------
  read_side_p : process(clk, rst)
  begin
    if rst = '1' then
      m_wr_en       <= '0';
      m_wr_last     <= '0';

      rd_row_cnt    <= (others => '0');
      rd_column_cnt <= (others => '0');
      rd_ram_ptr    <= (others => '0');

      ram_rd_addr   <= (others => '0');

      rd_first_word <= '1';
      cfg_fifo_rden <= '1';
      reading_frame <= '0';

    elsif clk'event and clk = '1' then

      m_wr_en           <= '0';
      m_wr_last         <= '0';

      -- Data comes out of the RAMs 1 cycle after the address has changed, need to keep
      -- track of the actual value being handled
      rd_column_cnt_reg <= rd_column_cnt;
      m_wr_en_reg       <= m_wr_en;
      m_wr_last_reg     <= m_wr_last;

      -- Sample data at the FIFO's output when reading from it
      if cfg_fifo_rddv = '1' then
        cfg_rd_tid           <= cfg_fifo_rd_tid;
        cfg_fifo_rden        <= '0';
        reading_frame        <= '1';

        if cfg_fifo_rd_remainder = 0 then
          cfg_rd_last_row    <= cfg_fifo_rd_last_row - 1;
        else
          cfg_rd_last_row    <= cfg_fifo_rd_last_row;
        end if;
        cfg_rd_last_column   <= cfg_fifo_rd_last_column;
        cfg_rd_remainder     <= cfg_fifo_rd_remainder;

        cfg_rd_constellation <= cfg_fifo_rd_constellation;
        cfg_rd_frame_type    <= cfg_fifo_rd_frame_type;
        cfg_rd_code_rate     <= cfg_fifo_rd_code_rate;
      end if;

      -- Write to the AXI adapter whenever it's not full
      if m_wr_full = '0' and reading_frame = '1' then

        rd_first_word <= '0';
        m_wr_en       <= '1';
        m_wr_tid      <= cfg_rd_tid;

        -- Read pointers control logic
        if rd_column_cnt /= cfg_rd_last_column then
          rd_column_cnt <= rd_column_cnt + 1;
        else
          rd_column_cnt <= (others => '0');

          if rd_row_cnt /= cfg_rd_last_row then
            rd_row_cnt  <= rd_row_cnt + 1;
            ram_rd_addr <= ram_rd_addr + 1;
          else
            rd_row_cnt    <= (others => '0');
            rd_ram_ptr    <= rd_ram_ptr + 1;
            m_wr_last     <= '1';
            ram_rd_addr   <= (rd_ram_ptr + 1) & (numbits(MAX_ROWS) - 1 downto 0 => '0');
            rd_first_word <= '1';

            cfg_fifo_rden <= '1';
            reading_frame <= '0';
          end if;
        end if;

        if cfg_rd_remainder /= 0 and rd_row_cnt + 1 > cfg_rd_last_row then
          rd_column_cnt <= (others => '0');
          rd_row_cnt    <= (others => '0');
          rd_ram_ptr    <= rd_ram_ptr + 1;
          m_wr_last     <= '1';
          ram_rd_addr   <= (rd_ram_ptr + 1) & (numbits(MAX_ROWS) - 1 downto 0 => '0');
          rd_first_word <= '1';

          cfg_fifo_rden <= '1';
          reading_frame <= '0';
        end if;

      end if;

      if m_wr_en = '1' then
        if rd_column_cnt_reg = 0 then
          -- Assign to undefined so we can track in simulation the parts that we not
          -- assigned
          rd_data_sr <= (others => 'U');
          -- We'll swap byte ordering (e.g ABCD becomes DCBA) because it's easier to
          -- assign the write data from the shift register's LSB
          if cfg_rd_constellation = mod_8psk then
            if cfg_rd_code_rate = C3_5 then
              rd_data_sr(interleaved_3c_210'range) <= mirror_bits(interleaved_3c_210);
            else
              rd_data_sr(interleaved_3c_012'range) <= mirror_bits(interleaved_3c_012);
            end if;

          elsif cfg_rd_constellation = mod_16apsk then
            -- DVB-S2 doesn't specify a different interleaving sequence for code rate 3/5
            -- and 16 APSK, only 8 PSK but DVB-S2(X) does
            if cfg_rd_code_rate = C3_5 then
              if cfg_rd_frame_type = fecframe_normal then
                rd_data_sr(interleaved_4c_3210'range) <= mirror_bits(interleaved_4c_3210);
              else
                rd_data_sr(interleaved_4c_3201'range) <= mirror_bits(interleaved_4c_3201);
              end if;
            else
              rd_data_sr(interleaved_4c_0123'range) <= mirror_bits(interleaved_4c_0123);
            end if;

          elsif cfg_rd_constellation = mod_32apsk then
            rd_data_sr <= mirror_bits(interleaved_5c);
          end if;

        else
          -- We'll write the LSB, shift data right
          rd_data_sr <= (TDATA_WIDTH - 1 downto 0 => 'U')
            & rd_data_sr(rd_data_sr'length - 1 downto TDATA_WIDTH);
        end if;
      end if;
    end if;
  end process read_side_p;

  cfg_sample_block : block
    signal wr_first_word : std_logic; -- To sample config
  begin
    process(clk, rst)
      variable rows      : natural;
      variable columns   : natural;
      variable remainder : natural;
    begin
      if rst = '1' then
        wr_first_word   <= '1';
      elsif rising_edge(clk) then
        -- Data handling will use the delayed AXI data -- can register config safely
        if s_tvalid = '1' and s_tready_i = '1' then
          wr_first_word  <= s_tlast;

          if wr_first_word = '1' then
            cfg_wr_constellation <= cfg_constellation;
            cfg_wr_frame_type    <= cfg_frame_type;
            cfg_wr_code_rate     <= cfg_code_rate;
            cfg_wr_tid           <= s_tid;

            if cfg_frame_type = fecframe_normal then
              if cfg_constellation = mod_8psk then
                rows := 21_600;
              elsif cfg_constellation = mod_16apsk then
                rows := 16_200;
              elsif cfg_constellation = mod_32apsk then
                rows := 12_960;
              end if;
            elsif cfg_frame_type = fecframe_short then
              if cfg_constellation = mod_8psk then
                rows := 5_400;
              elsif cfg_constellation = mod_16apsk then
                rows := 4_050;
              elsif cfg_constellation = mod_32apsk then
                rows := 3_240;
              end if;
            end if;

            if cfg_constellation = mod_8psk then
              columns := 3;
            elsif cfg_constellation = mod_16apsk then
              columns := 4;
            elsif cfg_constellation = mod_32apsk then
              columns := 5;
            end if;

            -- These divisions should be determined at compile time so it should be fine
            cfg_wr_last_row    <= to_unsigned(rows / TDATA_WIDTH, numbits(MAX_ROWS)) - 0;
            cfg_wr_last_column <= to_unsigned(columns, numbits(MAX_COLUMNS)) - 1;
            cfg_wr_remainder   <= to_unsigned(rows mod TDATA_WIDTH, numbits(TDATA_WIDTH));

          end if;
        end if;
      end if;
    end process;

  end block;

end axi_bit_interleaver;
