--
-- DVB FPGA
--
-- Copyright 2019 by Suoto <andre820@gmail.com>
--
-- This file is part of DVB FPGA.
--
-- DVB FPGA is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- DVB FPGA is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with DVB FPGA.  If not, see <http://www.gnu.org/licenses/>.

---------------
-- Libraries --
---------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.dvb_utils_pkg.all;

------------------------
-- Entity declaration --
------------------------
entity bch_encoder_mux is
  generic ( DATA_WIDTH : integer := 8 );
  port (
    clk                : in std_logic;
    rst                : in std_logic;
    -- Input config/data
    cfg_frame_type_in  : in frame_type_t;
    cfg_code_rate_in   : in code_rate_t;
    first_word         : in std_logic; -- First data. 1: SEED is used (initialise and calculate), 0: Previous CRC is used (continue and calculate)
    wr_en              : in std_logic; -- New Data. wr_data input has a valid data. Calculate new CRC
    wr_data            : in std_logic_vector(DATA_WIDTH - 1 downto 0);  -- Data in

    -- Output config/data
    cfg_frame_type_out : out frame_type_t;
    cfg_code_rate_out  : out code_rate_t;
    crc_rdy            : out std_logic;
    crc                : out std_logic_vector(191 downto 0);
    data_out           : out std_logic_vector(DATA_WIDTH - 1 downto 0));
end bch_encoder_mux;

architecture bch_encoder_mux of bch_encoder_mux is

  ---------------
  -- Constants --
  ---------------
  constant MUX_WIDTH : integer := 4;
  constant CRC_WIDTH : integer := 192;

  -- Indexes
  constant CRC_128_INDEX : integer := 0;
  constant CRC_160_INDEX : integer := 1;
  constant CRC_168_INDEX : integer := 2;
  constant CRC_192_INDEX : integer := 3;

  function to_index (
    constant frame_type : in  frame_type_t;
    constant code_rate  : in  code_rate_t) return natural is
  begin
    if frame_type = fecframe_short then
      return CRC_168_INDEX;
    else
      if code_rate = C8_9 or code_rate = C9_10 then
        return CRC_128_INDEX;
      elsif code_rate = C5_6 or code_rate = C2_3 then
        return CRC_160_INDEX;
      else
        return CRC_192_INDEX;
      end if;
    end if;

    report "Unable to determine CRC length for " &
           "frame length = " & frame_type_t'image(frame_type) & ", " &
           "code rate = " & code_rate_t'image(code_rate)
    severity Failure;
    return 0;
  end;

  -----------
  -- Types --
  -----------
  type data_array_t is array(natural range <>) of std_logic_vector(DATA_WIDTH - 1 downto 0);
  type crc_array_t is array(natural range <>) of std_logic_vector(CRC_WIDTH - 1 downto 0);

  -------------
  -- Signals --
  -------------
  signal in_mux_ptr       : integer range 0 to MUX_WIDTH - 1;
  signal out_mux_ptr      : integer range 0 to MUX_WIDTH - 1;

  signal cfg_frame_type_0 : frame_type_t;
  signal cfg_code_rate_0  : code_rate_t;
  signal cfg_frame_type_1 : frame_type_t;
  signal cfg_code_rate_1  : code_rate_t;
  signal wr_en_array      : std_logic_vector(MUX_WIDTH - 1 downto 0);

  signal crc_rdy_array    : std_logic_vector(MUX_WIDTH - 1 downto 0);
  signal data_out_array   : data_array_t(MUX_WIDTH - 1 downto 0);
  signal crc_out_array    : crc_array_t(MUX_WIDTH - 1 downto 0);

begin

  -- Supporting other data widths might need quite a bit of work as we might need to deal
  -- with partially valid words (data + mask)
  assert DATA_WIDTH = 8
    report "Unsupported DATA_WIDTH " & integer'image(DATA_WIDTH)
    severity Failure;

  -------------------
  -- Port mappings --
  -------------------
  -- instantiate BCH blocks for DATA_WIDTH = 8
  data_width_8_gen : if DATA_WIDTH = 8 generate
    signal crc_192_ulogic      : std_ulogic_vector(191 downto 0);
    signal data_out_192_ulogic : std_ulogic_vector(7 downto 0);
    
    signal crc_168_ulogic      : std_ulogic_vector(167 downto 0);
    signal data_out_168_ulogic : std_ulogic_vector(7 downto 0);

    signal crc_160_ulogic      : std_ulogic_vector(159 downto 0);
    signal data_out_160_ulogic : std_ulogic_vector(7 downto 0);

    signal crc_128_ulogic      : std_ulogic_vector(127 downto 0);
    signal data_out_128_ulogic : std_ulogic_vector(7 downto 0);

  begin
    bch_192_u : entity work.bch_192x8
      generic map (SEED => (others => '0'))
      port map (
        clk   => clk,
        reset => rst,
        fd    => first_word,
        nd    => wr_en_array(CRC_192_INDEX),
        rdy   => crc_rdy_array(CRC_192_INDEX),
        d     => std_ulogic_vector(wr_data),
        c     => crc_192_ulogic,
        -- CRC output
        o     => data_out_192_ulogic);

    bch_168_u : entity work.bch_168x8
      generic map (SEED => (others => '0'))
      port map (
        clk   => clk,
        reset => rst,
        fd    => first_word,
        nd    => wr_en_array(CRC_168_INDEX),
        rdy   => crc_rdy_array(CRC_168_INDEX),
        d     => std_ulogic_vector(wr_data),
        c     => crc_168_ulogic,
        -- CRC output
        o     => data_out_168_ulogic);

    bch_160_u : entity work.bch_160x8
      generic map (SEED => (others => '0'))
      port map (
        clk   => clk,
        reset => rst,
        fd    => first_word,
        nd    => wr_en_array(CRC_160_INDEX),
        rdy   => crc_rdy_array(CRC_160_INDEX),
        d     => std_ulogic_vector(wr_data),
        c     => crc_160_ulogic,
        -- CRC output
        o     => data_out_160_ulogic);

    bch_128_u : entity work.bch_128x8
      generic map (SEED => (others => '0'))
      port map (
        clk   => clk,
        reset => rst,
        fd    => first_word,
        nd    => wr_en_array(CRC_128_INDEX),
        rdy   => crc_rdy_array(CRC_128_INDEX),
        d     => std_ulogic_vector(wr_data),
        c     => crc_128_ulogic,
        -- CRC output
        o     => data_out_128_ulogic);

    -- Assign vector outpus
    crc_out_array(CRC_192_INDEX)               <= std_logic_vector(crc_192_ulogic);
    data_out_array(CRC_192_INDEX)              <= std_logic_vector(data_out_192_ulogic);

    crc_out_array(CRC_160_INDEX)(159 downto 0) <= std_logic_vector(crc_160_ulogic);
    data_out_array(CRC_160_INDEX)              <= std_logic_vector(data_out_160_ulogic);

    crc_out_array(CRC_168_INDEX)(167 downto 0) <= std_logic_vector(crc_168_ulogic);
    data_out_array(CRC_168_INDEX)              <= std_logic_vector(data_out_168_ulogic);

    crc_out_array(CRC_128_INDEX)(127 downto 0)  <= std_logic_vector(crc_128_ulogic);
    data_out_array(CRC_128_INDEX)               <= std_logic_vector(data_out_128_ulogic);

  end generate;

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  cfg_frame_type_out         <= cfg_frame_type_1;
  cfg_code_rate_out          <= cfg_code_rate_1;

  in_mux_ptr                 <= to_index(cfg_frame_type_in, cfg_code_rate_in);
  out_mux_ptr                <= to_index(cfg_frame_type_1, cfg_code_rate_1);

  wr_en_array(CRC_128_INDEX) <= wr_en when in_mux_ptr = CRC_128_INDEX else '0';
  wr_en_array(CRC_160_INDEX) <= wr_en when in_mux_ptr = CRC_160_INDEX else '0';
  wr_en_array(CRC_168_INDEX) <= wr_en when in_mux_ptr = CRC_168_INDEX else '0';
  wr_en_array(CRC_192_INDEX) <= wr_en when in_mux_ptr = CRC_192_INDEX else '0';

  crc_rdy                    <= crc_rdy_array(out_mux_ptr);
  crc                        <= crc_out_array(out_mux_ptr);
  data_out                   <= data_out_array(out_mux_ptr);

  ---------------
  -- Processes --
  ---------------
  process(clk, rst)
  begin
    if rst = '1' then
      cfg_frame_type_0 <= not_set;
      cfg_frame_type_1 <= not_set;
      cfg_code_rate_0  <= not_set;
      cfg_code_rate_1  <= not_set;
    elsif rising_edge(clk) then
      -- Sample the input code whenever the first word is written
      if wr_en = '1' and first_word = '1' then
        cfg_frame_type_0 <= cfg_frame_type_in;
        cfg_code_rate_0  <= cfg_code_rate_in;
      end if;

      -- Output it when CRC calc is complete
      if crc_rdy_array(to_index(cfg_frame_type_0, cfg_code_rate_0)) = '1' then
        cfg_frame_type_1 <= cfg_frame_type_0;
        cfg_code_rate_1  <= cfg_code_rate_0;
      end if;
    end if;
  end process;


end bch_encoder_mux;
