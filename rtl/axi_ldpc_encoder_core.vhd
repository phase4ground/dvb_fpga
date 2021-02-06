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
use fpga_cores.axi_pkg.all;

use work.dvb_utils_pkg.all;
use work.ldpc_pkg.all;

------------------------
-- Entity declaration --
------------------------
entity axi_ldpc_encoder_core is
  generic ( TID_WIDTH : integer := 0 );
  port (
    -- Usual ports
    clk               : in  std_logic;
    rst               : in  std_logic;

    cfg_constellation : in  constellation_t;
    cfg_frame_type    : in  frame_type_t;
    cfg_code_rate     : in  code_rate_t;

    -- AXI LDPC table input
    s_ldpc_tready     : out std_logic;
    s_ldpc_tvalid     : in  std_logic;
    s_ldpc_offset     : in  std_logic_vector(numbits(max(DVB_N_LDPC)) - 1 downto 0);
    s_ldpc_next       : in  std_logic;
    s_ldpc_tuser      : in  std_logic_vector(numbits(max(DVB_N_LDPC)) - 1 downto 0);
    s_ldpc_tlast      : in  std_logic;

    -- AXI data input
    s_tready          : out std_logic;
    s_tvalid          : in  std_logic;
    s_tlast           : in  std_logic;
    s_tdata           : in  std_logic_vector(7 downto 0);
    s_tid             : in  std_logic_vector(TID_WIDTH - 1 downto 0);

    -- AXI output
    m_tready          : in  std_logic;
    m_tvalid          : out std_logic;
    m_tlast           : out std_logic;
    m_tdata           : out std_logic_vector(7 downto 0);
    m_tid             : out std_logic_vector(TID_WIDTH - 1 downto 0));
end axi_ldpc_encoder_core;

architecture axi_ldpc_encoder_core of axi_ldpc_encoder_core is

  ---------------
  -- Constants --
  ---------------
  constant DATA_WIDTH           : natural := 8;
  constant ROM_DATA_WIDTH       : natural := numbits(max(DVB_N_LDPC));
  constant FRAME_RAM_DATA_WIDTH : natural := 16;
  constant FRAME_RAM_ADDR_WIDTH : natural := numbits((max(DVB_N_LDPC) + FRAME_RAM_DATA_WIDTH - 1) / FRAME_RAM_DATA_WIDTH);

  -------------
  -- Signals --
  -------------
  signal s_ldpc_dv               : std_logic;

  signal axi_ldpc_constellation  : constellation_t;
  signal axi_ldpc_frame_type     : frame_type_t;
  signal axi_ldpc_code_rate      : code_rate_t;

  signal axi_ldpc                : axi_stream_data_bus_t(tdata(DATA_WIDTH - 1 downto 0));
  signal axi_ldpc_dv             : std_logic;

  signal axi_passthrough         : axi_stream_bus_t(tdata(DATA_WIDTH - 1 downto 0), tuser(TID_WIDTH - 1 downto 0));
  signal m_tvalid_i              : std_logic;

  signal axi_bit                 : axi_stream_data_bus_t(tdata(0 downto 0));
  signal dbg_axi_bit_cnt         : unsigned(s_ldpc_tuser'length - 1 downto 0) := (others => '0');

  signal axi_bit_dv              : std_logic;
  signal axi_bit_first_word      : std_logic;
  signal axi_bit_has_data        : std_logic;
  signal axi_bit_tready_p        : std_logic;

  -- Pipeline context RAM handling is divided in stages:

  -- Processing flags
  signal stop_input_streams_p0   : std_logic  := '0';
  signal stop_input_streams_p1   : std_logic  := '0';
  signal wait_frame_completion   : std_logic  := '0';

  signal axi_bit_frame_type      : frame_type_t;

  -- AXI data synchronized to the frame RAM output data
  signal axi_bit_tdata_reg0      : std_logic;
  signal dbg_axi_bit_cnt_reg0    : unsigned(s_ldpc_tuser'length - 1 downto 0) := (others => '0');
  signal axi_bit_tdata_reg1      : std_logic;
  signal dbg_axi_bit_cnt_reg1    : unsigned(s_ldpc_tuser'length - 1 downto 0) := (others => '0');

  signal table_offset            : std_logic_vector(numbits(max(DVB_N_LDPC)) - 1 downto 0);
  signal s_ldpc_next_sampled     : std_logic;

  signal frame_bits_remaining    : unsigned(numbits(max(DVB_N_LDPC)) - 1 downto 0);

  -- Interface with the frame RAM
  signal ldpc_table_ram_ready    : std_logic := '0';
  signal ldpc_append_ram_ready   : std_logic := '0';
  signal frame_ram_en            : std_logic;
  signal frame_ram_en_reg0       : std_logic;

  signal frame_addr_in           : unsigned(FRAME_RAM_ADDR_WIDTH - 1 downto 0);

  -- Frame RAM output
  signal frame_addr_out          : std_logic_vector(FRAME_RAM_ADDR_WIDTH - 1 downto 0);
  -- frame_bit_index is sync with frame_addr_out and rame_ram_rddata
  signal frame_bit_index         : std_logic_vector(numbits(FRAME_RAM_DATA_WIDTH) - 1 downto 0);
  signal frame_ram_rddata        : std_logic_vector(FRAME_RAM_DATA_WIDTH  - 1 downto 0);

  signal frame_in_last           : std_logic;
  signal frame_in_mask           : std_logic_vector((FRAME_RAM_DATA_WIDTH + 7) / 8 - 1 downto 0);
  signal frame_out_last          : std_logic;
  signal frame_out_last_reg      : std_logic;
  signal frame_out_mask          : std_logic_vector((FRAME_RAM_DATA_WIDTH + 7) / 8 - 1 downto 0);

  -- Frame RAM data loop
  signal frame_ram_wrdata        : std_logic_vector(FRAME_RAM_DATA_WIDTH - 1 downto 0);

  signal frame_addr_rst_p0       : std_logic;
  signal frame_addr_rst_p1       : std_logic  := '0';
  signal encoded_wr_start        : std_logic  := '0';

  signal table_handle_ready      : std_logic;

  signal clearing_frame_ram_p0   : std_logic  := '0';

  signal writing_encoded_data    : std_logic;
  signal encoded_wr_en           : std_logic;
  signal encoded_wr_full         : std_logic;
  signal encoded_wr_data         : std_logic_vector(FRAME_RAM_DATA_WIDTH - 1 downto 0);
  signal encoded_wr_last         : std_logic := '0';
  signal encoded_wr_mask         : std_logic_vector((FRAME_RAM_DATA_WIDTH + 7) / 8 - 1 downto 0);

  signal axi_encoded             : axi_stream_bus_t(tdata(FRAME_RAM_DATA_WIDTH - 1 downto 0),
                                                   tuser((FRAME_RAM_DATA_WIDTH + 7) / 8 - 1 downto 0));

  signal forward_encoded_data    : std_logic;

  signal axi_out                 : axi_stream_data_bus_t(tdata(DATA_WIDTH - 1 downto 0));
  signal m_tid_reg               : std_logic_vector(TID_WIDTH - 1 downto 0);

  signal frame_bit_index_i       : natural range 0 to ROM_DATA_WIDTH - 1;
  signal s_ldpc_tready_i         : std_logic;

begin

  -------------------
  -- Port mappings --
  -------------------
  -- Duplicate input stream, one leaf will connect to the output and the other to the
  -- actual LDPC calculation that will be appended
  input_duplicate_block : block -- {{ --------------------------------------------------
    constant WIDTHS : integer_vector_t := (
      0 => DATA_WIDTH,
      1 => 1,
      2 => FRAME_TYPE_WIDTH,
      3 => CONSTELLATION_WIDTH,
      4 => CODE_RATE_WIDTH,
      5 => TID_WIDTH);

    constant AXI_DUP_DATA_WIDTH   : integer := sum(WIDTHS);

    signal s_tdata_agg            : std_logic_vector(AXI_DUP_DATA_WIDTH - 1 downto 0);

    signal axi_dup0_tdata         : std_logic_vector(AXI_DUP_DATA_WIDTH - 1 downto 0);
    signal axi_dup1_tdata         : std_logic_vector(AXI_DUP_DATA_WIDTH - 1 downto 0);

  begin

    s_tdata_agg <= s_tid &
                   encode(cfg_code_rate) &
                   encode(cfg_constellation) &
                   encode(cfg_frame_type) &
                   s_tlast &
                   s_tdata;

    axi_passthrough.tdata <= get_field(axi_dup0_tdata, 0, WIDTHS);
    axi_passthrough.tlast <= get_field(axi_dup0_tdata, 1, WIDTHS);
    axi_passthrough.tuser <= get_field(axi_dup0_tdata, 5, WIDTHS);

    axi_ldpc.tdata         <= get_field(axi_dup1_tdata, 0, WIDTHS);
    axi_ldpc.tlast         <= get_field(axi_dup1_tdata, 1, WIDTHS);
    axi_ldpc_frame_type    <= decode(get_field(axi_dup1_tdata, 2, WIDTHS));
    axi_ldpc_constellation <= decode(get_field(axi_dup1_tdata, 3, WIDTHS));
    axi_ldpc_code_rate     <= decode(get_field(axi_dup1_tdata, 4, WIDTHS));

    input_duplicate_u : entity fpga_cores.axi_stream_duplicate
      generic map ( TDATA_WIDTH => AXI_DUP_DATA_WIDTH )
      port map (
        -- Usual ports
        clk       => clk,
        rst       => rst,
        -- AXI stream input
        s_tready  => s_tready,
        s_tdata   => s_tdata_agg,
        s_tvalid  => s_tvalid,
        -- AXI stream output 0
        m0_tready => axi_passthrough.tready,
        m0_tdata  => axi_dup0_tdata,
        m0_tvalid => axi_passthrough.tvalid,
        -- AXI stream output 1
        m1_tready => axi_ldpc.tready,
        m1_tdata  => axi_dup1_tdata,
        m1_tvalid => axi_ldpc.tvalid);

  end block; -- }} ---------------------------------------------------------------------

  -- Convert from FRAME_RAM_DATA_WIDTH to the specified data width
  input_conversion_block : block -- {{ -------------------------------------------------
    signal axi_ldpc_tkeep: std_logic_vector(DATA_WIDTH / 8 - 1 downto 0);
    signal axi_ldpc_tid  : std_logic_vector(FRAME_TYPE_WIDTH - 1 downto 0);
    signal axi_bit_tid   : std_logic_vector(FRAME_TYPE_WIDTH - 1 downto 0);

  begin

    axi_bit_frame_type <= decode(axi_bit_tid);

    -- GHDL fails if this is done at the port map
    axi_ldpc_tid       <= encode(axi_ldpc_frame_type);

    axi_ldpc_tkeep     <= (others => '1') when axi_ldpc.tlast = '1' else
                          (others => '0');

    input_width_conversion_u : entity fpga_cores.axi_stream_width_converter
      generic map (
        INPUT_DATA_WIDTH  => DATA_WIDTH,
        OUTPUT_DATA_WIDTH => 1,
        AXI_TID_WIDTH     => FRAME_TYPE_WIDTH)
      port map (
        -- Usual ports
        clk        => clk,
        rst        => rst,
        -- AXI stream input
        s_tready   => axi_ldpc.tready,
        s_tdata    => mirror_bits(axi_ldpc.tdata), -- width converter is little endian, we need big endian
        s_tkeep    => axi_ldpc_tkeep,
        s_tid      => axi_ldpc_tid,
        s_tvalid   => axi_ldpc.tvalid,
        s_tlast    => axi_ldpc.tlast,
        -- AXI stream output
        m_tready   => axi_bit.tready,
        m_tdata    => axi_bit.tdata,
        m_tkeep    => open,
        m_tid      => axi_bit_tid,
        m_tvalid   => axi_bit.tvalid,
        m_tlast    => axi_bit.tlast);
    end block; -- }} -------------------------------------------------------------------

  frame_ram_u : entity fpga_cores.pipeline_context_ram
    generic map (
      ADDR_WIDTH => FRAME_RAM_ADDR_WIDTH,
      DATA_WIDTH => FRAME_RAM_DATA_WIDTH,
      RAM_TYPE   => bram)
    port map (
      clk         => clk,
      -- Checkout request interface
      en_in       => frame_ram_en,
      addr_in     => std_logic_vector(frame_addr_in),
      -- Data checkout output
      en_out      => frame_ram_en_reg0,
      addr_out    => frame_addr_out,
      context_out => frame_ram_rddata,
      -- Updated data input
      context_in  => frame_ram_wrdata);

  -- Can't stop reading from the frame RAM instantly, allow some slack
  frame_ram_adapter_block : block -- {{ ------------------------------------------------
    signal wr_data   : std_logic_vector(encoded_wr_data'length + encoded_wr_mask'length - 1 downto 0);
    signal tdata_out : std_logic_vector(encoded_wr_data'length + encoded_wr_mask'length - 1 downto 0);
    signal wr_en     : std_logic;
  begin

    wr_data           <= encoded_wr_mask & encoded_wr_data;

    axi_encoded.tuser <= tdata_out(tdata_out'length - 1 downto 16);
    axi_encoded.tdata <= tdata_out(15 downto 0);

    frame_ram_adapter_u : entity fpga_cores.axi_stream_master_adapter
      generic map (
        MAX_SKEW_CYCLES => 3,
        TDATA_WIDTH     => encoded_wr_data'length + encoded_wr_mask'length)
      port map (
        -- Usual ports
        clk      => clk,
        reset    => rst,
        -- Wanna-be AXI interface
        wr_en    => encoded_wr_en,
        wr_full  => encoded_wr_full,
        wr_data  => wr_data,
        wr_last  => encoded_wr_last,
        -- AXI master
        m_tvalid => axi_encoded.tvalid,
        m_tready => axi_encoded.tready,
        m_tdata  => tdata_out,
        m_tlast  => axi_encoded.tlast);

    end block; -- }} -------------------------------------------------------------------

  -- Convert from FRAME_RAM_DATA_WIDTH to the specified data width
  output_width_conversion_u : entity fpga_cores.axi_stream_width_converter
    generic map (
      INPUT_DATA_WIDTH  => FRAME_RAM_DATA_WIDTH,
      OUTPUT_DATA_WIDTH => DATA_WIDTH,
      AXI_TID_WIDTH     => 0)
    port map (
      -- Usual ports
      clk        => clk,
      rst        => rst,
      -- AXI stream input
      s_tready   => axi_encoded.tready,
      s_tdata    => axi_encoded.tdata,
      s_tkeep    => axi_encoded.tuser,
      s_tid      => (others => '0'),
      s_tvalid   => axi_encoded.tvalid,
      s_tlast    => axi_encoded.tlast,
      -- AXI stream output
      m_tready   => axi_out.tready,
      m_tdata    => axi_out.tdata,
      m_tkeep    => open,
      m_tid      => open,
      m_tvalid   => axi_out.tvalid,
      m_tlast    => axi_out.tlast);

  frame_addr_in_rst_u : entity fpga_cores.edge_detector
    generic map (
      SYNCHRONIZE_INPUT => False,
      OUTPUT_DELAY      => 0)
    port map (
      -- Usual ports
      clk     => clk,
      clken   => '1',
      --
      din     => stop_input_streams_p0,
      -- Edges detected
      rising  => frame_addr_rst_p0,
      falling => open,
      toggle  => open);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  -- Values synchronized with data from pipeline_context_ram
  frame_bit_index_i      <= to_integer(unsigned(frame_bit_index));
  -- TODO: Should not depend on both stop_input_streams_p0 and stop_input_streams_p1
  s_ldpc_tready_i        <= table_handle_ready and not (rst or
                                                        stop_input_streams_p0 or
                                                        stop_input_streams_p1 or
                                                        wait_frame_completion);

  -- AXI slave specifics
  axi_bit_dv             <= axi_bit.tready and axi_bit.tvalid;
  axi_ldpc_dv            <= axi_ldpc.tready and axi_ldpc.tvalid;
  s_ldpc_dv              <= s_ldpc_tready_i and s_ldpc_tvalid;
  axi_bit.tready         <= axi_bit_tready_p and not (rst or stop_input_streams_p0 or stop_input_streams_p1 or wait_frame_completion);

  -- Mux output data
  axi_out.tready         <= m_tready and forward_encoded_data;
  axi_passthrough.tready <= m_tready and not forward_encoded_data;
  m_tlast                <= forward_encoded_data and axi_out.tlast;

  m_tdata                <= axi_passthrough.tdata when forward_encoded_data = '0' else
                            mirror_bits(axi_out.tdata);

  m_tvalid_i             <= (axi_passthrough.tvalid and axi_passthrough.tready and not forward_encoded_data)
                             or axi_out.tvalid;

  m_tid                  <= (axi_passthrough.tuser and (TID_WIDTH - 1 downto 0 => not forward_encoded_data)) or
                            (m_tid_reg and (TID_WIDTH - 1 downto 0 => forward_encoded_data));

  frame_ram_en           <= (ldpc_table_ram_ready and not wait_frame_completion and axi_bit_has_data)
                            or (ldpc_append_ram_ready and not encoded_wr_full and not frame_out_last_reg);

  frame_in_last          <= stop_input_streams_p0 when frame_bits_remaining <= FRAME_RAM_DATA_WIDTH
                            else '0';

  -- Frame RAM data width is fixed to 16, so the mask is 2 and will only ever going to
  -- be either 01b or 11b
  frame_in_mask <= "00" when frame_in_last /= '1' else
                   "01" when frame_bits_remaining <= DATA_WIDTH else
                   "11";

  s_ldpc_tready <= s_ldpc_tready_i;
  m_tvalid      <= m_tvalid_i;

  -- synthesis translate_off
  assert not (rising_edge(clk) and frame_in_last = '1' and frame_in_mask = "00");
  -- synthesis translate_on

  ---------------
  -- Processes --
  ---------------
  axi_flow_ctrl_p : process(clk) -- {{ -------------------------------------------------
    -- Use variables for some processing flags to allow setting and checking a condition
    -- on the same cycle
    variable has_table_data  : boolean := False;
    variable has_axi_data    : boolean := False;

    -- Marks when the entire LDPC table has been received
    variable completed_table : boolean := False;
    -- Marks when the entire data frame has been received
    variable completed_data  : boolean := False;
  begin
    if rising_edge(clk) then

      ldpc_table_ram_ready <= '0';
      s_ldpc_next_sampled  <= '0';

      -- Set the last word according to the pipeline_context_ram ready
      if frame_out_last = '1' and frame_ram_en_reg0 = '1' then
        encoded_wr_last <= '1';
        encoded_wr_mask <= frame_out_mask and (frame_out_mask'range => frame_ram_en_reg0);
      elsif encoded_wr_en = '1' then
        encoded_wr_last <= '0';
      end if;

      -- Synchronize bit index to be used in the next stage
      frame_bit_index      <= table_offset(numbits(FRAME_RAM_DATA_WIDTH) - 1 downto 0);

      -- Sample frame_in_{last,mask} whenever data from the pipeline_context_ram is valid
      if frame_ram_en_reg0 = '1' then
        frame_out_last     <= frame_in_last;
        frame_out_mask     <= frame_in_mask;
      end if;

      frame_out_last_reg <= frame_out_last and frame_ram_en_reg0;

      if wait_frame_completion = '1' then
        frame_out_last <= '0';
      end if;

      frame_addr_rst_p1       <= frame_addr_rst_p0;
      encoded_wr_start        <= frame_addr_rst_p1;
      stop_input_streams_p1   <= stop_input_streams_p0;

      if frame_addr_rst_p1 = '1' then
        clearing_frame_ram_p0 <= '1';
      elsif encoded_wr_en = '1' and encoded_wr_last = '1' then
        clearing_frame_ram_p0 <= '0';
        table_handle_ready    <= '1';
      end if;

      -- Wrap encoded_wr_en with a valid mask to avoid writing invalid data
      if encoded_wr_start = '1' then
        writing_encoded_data  <= '1';
      elsif encoded_wr_last = '1' and encoded_wr_en = '1' and writing_encoded_data = '1' then
        writing_encoded_data  <= '0';
        wait_frame_completion <= '1';
      end if;

      -- Frame RAM addressing depends if we're calculating the codes or extracting them
      -- (stop_input_streams_p0 being 0 or 1 respectively)
      ldpc_append_addr_ctrl : if stop_input_streams_p0 = '1' and encoded_wr_full = '0' and wait_frame_completion = '0' then
        -- When extracting frame data, we need to complete the given frame. Since the
        -- frame size is not always an integer multiple of the frame length, we also need
        -- to check if data bit cnt has wrapped (MSB is 1).
        if frame_addr_rst_p0 = '1' then
          frame_addr_in        <= (others => '0');
        elsif frame_out_last = '0' then
          frame_addr_in        <= frame_addr_in + 1;
          frame_bits_remaining <= frame_bits_remaining - FRAME_RAM_DATA_WIDTH;
        end if;

      end if ldpc_append_addr_ctrl;

      -- AXI LDPC table control
      ldpc_table_control : if s_ldpc_dv = '1' then
        has_table_data     := True;

        table_offset        <= s_ldpc_offset;
        s_ldpc_next_sampled <= s_ldpc_next;
        table_handle_ready  <= '0';
        frame_addr_in       <= unsigned(s_ldpc_offset(ROM_DATA_WIDTH - 1 downto numbits(FRAME_RAM_DATA_WIDTH)));

        if stop_input_streams_p0 = '0' and s_ldpc_next = '1' then
          axi_bit_tready_p <= not s_ldpc_tlast; --'1';
        end if;

        if s_ldpc_tlast = '1' then
          completed_table := True;
        end if;
      end if ldpc_table_control;

      -- AXI frame data control
      ldpc_data_control : if axi_bit_dv = '1' then
        has_axi_data     := True;
        axi_bit_tready_p <= '0';

        dbg_axi_bit_cnt  <= dbg_axi_bit_cnt + 1;

        if axi_bit.tlast = '1' then
          completed_data  := True;
          dbg_axi_bit_cnt <= (others => '0');
        end if;

        -- Set the expected frame length. Data will passthrough and LDPC codes will be
        -- appended to complete the appropriate n_ldpc length (either 16,200 or 64,800).
        -- Timing-wise, the best way to do this is by setting the counter to - N + 2;
        -- whenever it gets to 1 it will have counted N items.
        if axi_bit_first_word = '1' then
          if axi_bit_frame_type = FECFRAME_SHORT then
            frame_bits_remaining <= to_unsigned(FECFRAME_SHORT_BIT_LENGTH - 1,
                                                frame_bits_remaining'length);
          elsif axi_bit_frame_type = FECFRAME_NORMAL then
            frame_bits_remaining <= to_unsigned(FECFRAME_NORMAL_BIT_LENGTH - 1,
                                                frame_bits_remaining'length);
          else
            report "Don't know how to handle frame type=" & quote(frame_type_t'image(axi_bit_frame_type))
              severity Error;
          end if;
        else
          frame_bits_remaining <= frame_bits_remaining - 1;
        end if;
      end if ldpc_data_control;

      -- Clear flags when the output frame makes its way completely
      if axi_out.tready = '1' and axi_out.tvalid = '1' and axi_out.tlast = '1' then
        has_axi_data          := False;
        axi_bit_tready_p      <= '1';
        wait_frame_completion <= '0';
      end if;

      if frame_out_last = '1' and frame_ram_en_reg0 = '1' then
        stop_input_streams_p0 <= '0';
      end if;

      if frame_ram_en = '1' and frame_in_last = '1' then
        ldpc_append_ram_ready <= '0';
      end if;

      -- Insert an entry into the processing pipeline whenever we have both table and data
      if has_axi_data and has_table_data then
        ldpc_table_ram_ready <= '1';
      end if;

      if stop_input_streams_p0 = '0' then
        -- Accept another table entry every time we have AXI data, unless we're doing the
        -- post frame processing
        if has_axi_data and writing_encoded_data = '0' then
          has_table_data     := False;
          table_handle_ready <= '1';
        end if;

        -- Only accept another AXI data bit when the table interface allows for it
        if has_table_data and s_ldpc_dv = '1' and s_ldpc_next = '0' then
          has_axi_data := False;
        end if;
      end if;

      -- Once we completed both the table and the data, switch to extracting data from the
      -- frame RAM
      if completed_table and completed_data then
        completed_table       := False;
        completed_data        := False;
        table_handle_ready    <= '0';
        stop_input_streams_p0 <= '1';
        ldpc_append_ram_ready <= '1';
      end if;

      -- Reset
      if rst = '1' then
        axi_bit_tready_p      <= '1';
        writing_encoded_data  <= '0';
        stop_input_streams_p0 <= '0';
        table_handle_ready    <= '1';

        completed_data        := False;
        completed_table       := False;
        has_axi_data          := False;
        has_table_data        := False;

      end if;

    end if;
  end process; -- }} -------------------------------------------------------------------

  frame_ram_data_handle_p : process(clk, rst) -- {{ ------------------------------------
    variable xored_data : std_logic_vector(FRAME_RAM_DATA_WIDTH - 1 downto 0);
  begin
    if rst = '1' then
      encoded_wr_en    <= '0';

      encoded_wr_data  <= (others => 'U');
      frame_ram_wrdata <= (others => 'U');

      xored_data       := (others => 'U');

    elsif rising_edge(clk) then

      encoded_wr_en    <= '0';

      -- Always return the context, will change only when needed
      frame_ram_wrdata <= to_01_sim(frame_ram_rddata);

      -- When calculating the final XOR'ed value, the first bit will depend on the
      -- address. We can safely assign here and avoid extra logic levels on the FF control
      if unsigned(frame_addr_out) = 0 then
        xored_data(0) := frame_ram_rddata(0);
      else
        xored_data(0) := encoded_wr_data(FRAME_RAM_DATA_WIDTH - 1) xor frame_ram_rddata(0);
      end if;

      if encoded_wr_en = '1' and encoded_wr_full = '0' and encoded_wr_last = '1' then
        encoded_wr_en <= '0';
      elsif clearing_frame_ram_p0 = '1' and encoded_wr_full = '0' then
        encoded_wr_en <= '1';
      end if;

      -- Handle data coming out of the frame RAM, either by using the input data of by
      -- calculating the actual final XOR'ed value
      if frame_ram_en_reg0 = '1' then
        -- if writing_encoded_data = '0' then --and frame_addr_rst_p1 = '0' then
        if clearing_frame_ram_p0 = '0' then
          frame_ram_wrdata(frame_bit_index_i) <= axi_bit_tdata_reg1
                                                 xor to_01_sim(frame_ram_rddata(frame_bit_index_i));
        else
          frame_ram_wrdata <= (others => '0');
        end if;

        if stop_input_streams_p1 = '1' then
          -- Calculate the final XOR between output bits
          for i in 1 to FRAME_RAM_DATA_WIDTH - 1 loop
            xored_data(i) := frame_ram_rddata(i) xor xored_data(i - 1);
          end loop;

          -- Assign data out and decrement the bits consumed
          if encoded_wr_start = '1' then
            encoded_wr_en    <= '1';
          end if;
          encoded_wr_data  <= xored_data;
        end if;
      end if;

    end if;
  end process; -- }} -------------------------------------------------------------------

  axi_bit_sync_p : process(clk, rst)
  begin
    if rst = '1' then
      axi_bit_first_word   <= '1';
      forward_encoded_data <= '0';

      -- We don't want things muxed with rst here
      axi_bit_tdata_reg0   <= 'U';
      axi_bit_tdata_reg1   <= 'U';
      dbg_axi_bit_cnt_reg0 <= (others => 'U');
      dbg_axi_bit_cnt_reg1 <= (others => 'U');

      m_tid_reg            <= (others => 'U');
    elsif rising_edge(clk) then

      -- Switch between forwarding data from s_axi or the codes calculated internally
      if axi_passthrough.tvalid = '1' and axi_passthrough.tready = '1' then
        m_tid_reg <= axi_passthrough.tuser;
        if axi_passthrough.tlast = '1' then
          forward_encoded_data <= '1';
        end if;
      end if;

      if axi_out.tvalid = '1' and axi_out.tready = '1' and axi_out.tlast = '1' then
        forward_encoded_data <= '0';
      end if;

      -- AXI input data that was converted to 1 bit needs to be synchronized properly with
      axi_bit_tdata_reg1   <= axi_bit_tdata_reg0;
      dbg_axi_bit_cnt_reg1 <= dbg_axi_bit_cnt_reg0;

      if s_ldpc_next_sampled = '1' then
        axi_bit_has_data   <= '0';
      end if;

      if axi_bit_dv = '1' then
        axi_bit_has_data     <= '1';
        axi_bit_first_word   <= axi_bit.tlast;
        axi_bit_tdata_reg0   <= axi_bit.tdata(0);
        dbg_axi_bit_cnt_reg0 <= dbg_axi_bit_cnt;
      end if;

    end if;
  end process;

end axi_ldpc_encoder_core;

-- vim: set foldmethod=marker foldmarker=--\ {{,--\ }} :
