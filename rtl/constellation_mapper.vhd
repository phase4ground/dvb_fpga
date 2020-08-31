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
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.MATH_PI;

library vunit_lib;
context vunit_lib.vunit_context;
context vunit_lib.com_context;

library fpga_cores;
use fpga_cores.common_pkg.all;

use work.dvb_utils_pkg.all;

------------------------
-- Entity declaration --
------------------------
entity constellation_mapper is
  generic (DATA_WIDTH   : integer  := 1); -- Data width must be even number; I and Q will be DATA_WIDTH/2 wide
  port (
    -- Usual ports
    clk               : in  std_logic;
    rst               : in  std_logic;

    cfg_constellation : in  constellation_t;
    cfg_frame_type    : in  frame_type_t;
    cfg_code_rate     : in  code_rate_t;

    -- AXI input
    s_tvalid          : in  std_logic;
    s_tdata           : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    s_tlast           : in  std_logic;
    s_tready          : out std_logic;

    -- AXI output
    m_tready          : in  std_logic;
    m_tvalid          : out std_logic;
    m_tlast           : out std_logic;
    m_tdata           : out std_logic_vector(DATA_WIDTH - 1 downto 0));
end constellation_mapper;

architecture constellation_mapper of constellation_mapper is

  -----------------
  -- Subprograms --
  -----------------
  function to_fixed_point (constant x : real) return signed is
    constant width : natural := DATA_WIDTH/2 - 1;
    constant round_a : integer := integer(ieee.math_real.round(x * real(2**width)));
    variable a     : signed(width - 1 downto 0) := to_signed(round_a, width);
  begin
    if a < 0 then
      a := to_signed(round_a, width);
      a := abs a;
      a := not a;
      a := a + 1;
      a := a + ( 2 ** width );
    end if;

    return a;
  end;

  function cos (constant x : real) return unsigned is
  begin
    return unsigned(resize(to_fixed_point(ieee.math_real.cos(x)), DATA_WIDTH/2));
  end;

  function sin (constant x : real) return unsigned is
  begin
    return unsigned(resize(to_fixed_point(ieee.math_real.sin(x)), DATA_WIDTH/2));
  end;

  function get_iq_pair (constant x : real) return std_logic_vector is
  begin
    return std_logic_vector(cos(x) & sin(x));
  end;

  ---------------
  -- Constants --
  ---------------
  constant MAX_MODULATION_SR_LENGTH : natural := 5;
  -----------
  -- Types --
  -----------
  constant MOD_8PSK_MAP : std_logic_vector_2d_t(0 to 7)(DATA_WIDTH - 1 downto 0) := (
    0 => get_iq_pair(      MATH_PI / 4.0),
    1 => get_iq_pair(0.0),
    2 => get_iq_pair(4.0 * MATH_PI / 4.0),
    3 => get_iq_pair(5.0 * MATH_PI / 4.0),
    4 => get_iq_pair(2.0 * MATH_PI / 4.0),
    5 => get_iq_pair(7.0 * MATH_PI / 4.0),
    6 => get_iq_pair(3.0 * MATH_PI / 4.0),
    7 => get_iq_pair(6.0 * MATH_PI / 4.0)
  );

  -------------
  -- Signals --
  -------------
  signal axi_dv : std_logic;
  signal s_tready_i  : std_logic;
  -- Number of bits to convert is variable (3 for 8 PSK, 4 for 16 APSK and 5 for 32 APSK)
  -- signal dbg_tdata_sr  : std_logic_vector(DATA_WIDTH + MAX_MODULATION_SR_LENGTH - 1 downto 0);
  -- signal dbg_bit_cnt : natural; -- range 0 to DATA_WIDTH + MAX_MODULATION_SR_LENGTH - 1;
  signal dbg_tdata_sr : std_logic_vector(DATA_WIDTH + MAX_MODULATION_SR_LENGTH - 1 downto 0);
  signal dbg_bit_cnt : unsigned(numbits(DATA_WIDTH + MAX_MODULATION_SR_LENGTH) - 1 downto 0);

  signal axi_tvalid : std_logic;
  signal axi_tready : std_logic;
  signal axi_tlast  : std_logic;
  signal axi_tdata  : std_logic_vector(DATA_WIDTH - 1 downto 0);

  signal key_width   : unsigned(MAX_MODULATION_SR_LENGTH - 1 downto 0);

  signal rom_addr : unsigned(4 downto 0);

  signal rom_data : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal rom_data_q : std_logic_vector(DATA_WIDTH/2 - 1 downto 0);
  signal rom_data_i : std_logic_vector(DATA_WIDTH/2 - 1 downto 0);

begin

  -------------------
  -- Port mappings --
  -------------------
  axi_delay_block : block
    signal tdata_in  : std_logic_vector(DATA_WIDTH downto 0);
    signal tdata_out : std_logic_vector(DATA_WIDTH downto 0);
  begin
    tdata_in  <= s_tlast & s_tdata;

    axi_tdata <= tdata_out(DATA_WIDTH - 1 downto 0);
    axi_tlast <= tdata_out(DATA_WIDTH);

    axi_delay_u : entity fpga_cores.axi_stream_delay
      generic map (
      DELAY_CYCLES => 1,
      TDATA_WIDTH  => DATA_WIDTH + 1)
    port map (
      -- Usual ports
      clk      => clk,
      rst      => rst,

      -- AXI slave input
      s_tvalid  => s_tvalid,
      s_tready  => s_tready_i,
      s_tdata   => tdata_in,

      -- AXI master output
      m_tvalid  => axi_tvalid,
      m_tready  => axi_tready,
      m_tdata   => tdata_out);
  end block axi_delay_block;

  mod_8psk_table : entity fpga_cores.rom_inference
    generic map (
      ROM_DATA      => MOD_8PSK_MAP,
      ROM_TYPE      => auto,
      OUTPUT_DELAY  => 1)
  port map (
    clk     => clk,
    clken   => '1',
    addr    => std_logic_vector(rom_addr(2 downto 0)),
    rddata  => rom_data);

  rom_data_i <= rom_data(DATA_WIDTH/2 - 1 downto 0);
  rom_data_q <= rom_data(DATA_WIDTH - 1 downto DATA_WIDTH/2);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  axi_dv     <= axi_tvalid and axi_tready;

  ---------------
  -- Processes --
  ---------------
  process(clk, rst)
    variable tdata_sr : std_logic_vector(dbg_tdata_sr'range);
    variable bit_cnt  : unsigned(dbg_bit_cnt'range) := (others => '0');
  begin
    if rst = '1' then
      axi_tready <= '1';
    elsif rising_edge(clk) then
      if axi_dv = '1' then
        bit_cnt := bit_cnt + DATA_WIDTH;
        tdata_sr := tdata_sr(tdata_sr'length - DATA_WIDTH - 1 downto 0) & mirror_bits(axi_tdata);

        info("adding      bit_cnt=" & integer'image(to_integer(bit_cnt)));

        -- Not enough room for more data
        if dbg_tdata_sr'length - bit_cnt < DATA_WIDTH then
          axi_tready <= '0';
        end if;
      end if;

      if bit_cnt > key_width then
        rom_addr <= unsigned(mirror_bits(tdata_sr(4 downto 0)));
        bit_cnt  := bit_cnt - key_width;
        -- Consume data from the shift register
        case to_integer(key_width) is
          when 3 => 
            tdata_sr := (2 downto 0 => 'U') & tdata_sr(tdata_sr'length - 1 downto 3);
          when 4 =>
            tdata_sr := (3 downto 0 => 'U') & tdata_sr(tdata_sr'length - 1 downto 4);
          when 5 =>
            tdata_sr := (4 downto 0 => 'U') & tdata_sr(tdata_sr'length - 1 downto 5);
          when others => null;
        end case;

        info("subtracting bit_cnt=" & integer'image(to_integer(bit_cnt)));
      end if;

      -- Allow more data to come in whenever there's room for it
      if dbg_tdata_sr'length - bit_cnt >= DATA_WIDTH then
        axi_tready <= '1';
      end if;

      dbg_bit_cnt  <= bit_cnt;
      dbg_tdata_sr <= tdata_sr;

    end if;
  end process;

  cfg_sample_block : block
    signal s_axi_dv : std_logic;
    signal first_word : std_logic;
  begin

    s_axi_dv   <= s_tvalid and s_tready_i;

    cfg_sample_p : process(clk, rst)
    begin
      if rst = '1' then
        first_word   <= '1';
        key_width    <= (others => 'U');
      elsif clk'event and clk = '1' then
        if s_axi_dv = '1' then
          first_word   <= s_tlast;

          if first_word = '1' then
            case cfg_constellation is
              when mod_8psk => key_width <= to_unsigned(3, key_width'length);
              when mod_16apsk => key_width <= to_unsigned(4, key_width'length);
              when mod_32apsk => key_width <= to_unsigned(5, key_width'length);
              when others =>
                report "don't know how to handle " & quote(constellation_t'image(cfg_constellation))
                severity warning;
            end case;
          end if;
        end if;
      end if;
    end process cfg_sample_p;
  end block cfg_sample_block;

end constellation_mapper;

-- b7 54 4d f6
-- 1011 0111 0101 0100 0100 1101 1111 0110
--
-- 
