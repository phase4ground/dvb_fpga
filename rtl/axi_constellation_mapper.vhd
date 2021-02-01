--
-- DVB IP
--
-- Copyright 2019 by Suoto <andre820@gmail.com>
--
-- This file is part of the DVB IP.
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

---------------------------------
-- Block name and description --
--------------------------------

---------------
-- Libraries --
---------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.MATH_PI;

library fpga_cores;
use fpga_cores.common_pkg.all;
use fpga_cores.axi_pkg.all;

use work.dvb_utils_pkg.all;

library str_format;
use str_format.str_format_pkg.all;

library vunit_lib;
context vunit_lib.vunit_context;
context vunit_lib.com_context;

------------------------
-- Entity declaration --
------------------------
entity axi_constellation_mapper is
  generic (
    DATA_WIDTH : integer := 8
  );
  port (
    -- Usual ports
    clk               : in  std_logic;
    rst               : in  std_logic;
    -- Parameter input
    cfg_constellation : in  constellation_t;
    cfg_frame_type    : in  frame_type_t;
    cfg_code_rate     : in  code_rate_t;

    -- AXI data input
    s_tready          : out std_logic;
    s_tvalid          : in  std_logic;
    s_tlast           : in  std_logic;
    s_tdata           : in  std_logic_vector(DATA_WIDTH - 1 downto 0);

    -- AXI output
    m_tready          : in  std_logic;
    m_tvalid          : out std_logic;
    m_tlast           : out std_logic;
    m_tdata           : out std_logic_vector(DATA_WIDTH - 1 downto 0));
end axi_constellation_mapper;

architecture axi_constellation_mapper of axi_constellation_mapper is

  function get_iq_pair ( constant x : real ) return signed is
  begin
    return cos(x, DATA_WIDTH / 2) & sin(x, DATA_WIDTH / 2);
  end function;

  -----------
  -- Types --
  -----------
  -- This ROM stores values for qpsk, 8 psk, 16 apsk and 32 apsk (in this order)
  constant CONSTELLATION_ROM : std_logic_array_t(0 to 59)(DATA_WIDTH - 1 downto 0) := (
    -- QPSK
    0 => std_logic_vector(get_iq_pair(      MATH_PI / 4.0)),
    1 => std_logic_vector(get_iq_pair(7.0 * MATH_PI / 4.0)),
    2 => std_logic_vector(get_iq_pair(3.0 * MATH_PI / 4.0)),
    3 => std_logic_vector(get_iq_pair(5.0 * MATH_PI / 4.0)),

    -- 8PSK
    4  => std_logic_vector(get_iq_pair(     MATH_PI / 4.0)),
    5  => std_logic_vector(get_iq_pair(0.0)),
    6  => std_logic_vector(get_iq_pair(4.0 * MATH_PI / 4.0)),
    7  => std_logic_vector(get_iq_pair(5.0 * MATH_PI / 4.0)),
    8  => std_logic_vector(get_iq_pair(2.0 * MATH_PI / 4.0)),
    9  => std_logic_vector(get_iq_pair(7.0 * MATH_PI / 4.0)),
    10 => std_logic_vector(get_iq_pair(3.0 * MATH_PI / 4.0)),
    11 => std_logic_vector(get_iq_pair(6.0 * MATH_PI / 4.0)),
    -- 16 APSK
    others => (others => '0'));

  -------------
  -- Signals --
  -------------
  signal m_tvalid_i  : std_logic;
  signal mux_sel     : std_logic_vector(3 downto 0);
  signal conv_tready : std_logic_vector(3 downto 0);
  signal conv_tvalid : std_logic_vector(3 downto 0);

  signal axi_qpsk    : axi_stream_data_bus_t(tdata(1 downto 0));
  signal axi_8psk    : axi_stream_data_bus_t(tdata(2 downto 0));
  signal axi_16apsk  : axi_stream_data_bus_t(tdata(3 downto 0));
  signal axi_32apsk  : axi_stream_data_bus_t(tdata(4 downto 0));

  signal addr_qpsk   : std_logic_vector(5 downto 0);
  signal addr_8psk   : std_logic_vector(5 downto 0);
  signal addr_16apsk : std_logic_vector(5 downto 0);
  signal addr_32apsk : std_logic_vector(5 downto 0);

  signal rom_addr    : std_logic_vector(5 downto 0);
  signal rom_data    : std_logic_vector(DATA_WIDTH - 1 downto 0);

begin

  -------------------
  -- Port mappings --
  -------------------
  -- Mux the input data stream to the appropriate width converter
  input_mux_u : entity fpga_cores.axi_stream_demux
    generic map (
      INTERFACES => 4,
      DATA_WIDTH => 0)
    port map (
      selection_mask => mux_sel,

      s_tvalid       => s_tvalid,
      s_tready       => s_tready,
      s_tdata        => (others => '0'),

      m_tvalid       => conv_tvalid,
      m_tready       => conv_tready,
      m_tdata        => open);

  width_converter_qpsk_u : entity fpga_cores.axi_stream_width_converter
  generic map (
    INPUT_DATA_WIDTH  => DATA_WIDTH,
    OUTPUT_DATA_WIDTH => 2,
    AXI_TID_WIDTH     => 0)
  port map (
    -- Usual ports
    clk      => clk,
    rst      => rst,
    -- AXI stream input
    s_tready => conv_tready(0),
    s_tdata  => mirror_bits(s_tdata),
    s_tkeep  => (others => '1'),
    s_tid    => (others => 'U'),
    s_tvalid => conv_tvalid(0),
    s_tlast  => s_tlast,
    -- AXI stream output
    m_tready => axi_qpsk.tready,
    m_tdata  => axi_qpsk.tdata,
    m_tkeep  => open,
    m_tid    => open,
    m_tvalid => axi_qpsk.tvalid,
    m_tlast  => axi_qpsk.tlast);

  width_converter_8psk_u : entity fpga_cores.axi_stream_width_converter
  generic map (
    INPUT_DATA_WIDTH  => DATA_WIDTH,
    OUTPUT_DATA_WIDTH => 3,
    AXI_TID_WIDTH     => 0)
  port map (
    -- Usual ports
    clk      => clk,
    rst      => rst,
    -- AXI stream input
    s_tready => conv_tready(1),
    s_tdata  => mirror_bits(s_tdata),
    s_tkeep  => (others => '1'),
    s_tid    => (others => 'U'),
    s_tvalid => conv_tvalid(1),
    s_tlast  => s_tlast,
    -- AXI stream output
    m_tready => axi_8psk.tready,
    m_tdata  => axi_8psk.tdata,
    m_tkeep  => open,
    m_tid    => open,
    m_tvalid => axi_8psk.tvalid,
    m_tlast  => axi_8psk.tlast);

  width_converter_16apsk_u : entity fpga_cores.axi_stream_width_converter
  generic map (
    INPUT_DATA_WIDTH  => DATA_WIDTH,
    OUTPUT_DATA_WIDTH => 4,
    AXI_TID_WIDTH     => 0)
  port map (
    -- Usual ports
    clk      => clk,
    rst      => rst,
    -- AXI stream input
    s_tready => conv_tready(2),
    s_tdata  => mirror_bits(s_tdata),
    s_tkeep  => (others => '1'),
    s_tid    => (others => 'U'),
    s_tvalid => conv_tvalid(2),
    s_tlast  => s_tlast,
    -- AXI stream output
    m_tready => axi_16apsk.tready,
    m_tdata  => axi_16apsk.tdata,
    m_tkeep  => open,
    m_tid    => open,
    m_tvalid => axi_16apsk.tvalid,
    m_tlast  => axi_16apsk.tlast);

  width_converter_32apsk_u : entity fpga_cores.axi_stream_width_converter
  generic map (
    INPUT_DATA_WIDTH  => DATA_WIDTH,
    OUTPUT_DATA_WIDTH => 5,
    AXI_TID_WIDTH     => 0)
  port map (
    -- Usual ports
    clk      => clk,
    rst      => rst,
    -- AXI stream input
    s_tready => conv_tready(3),
    s_tdata  => mirror_bits(s_tdata),
    s_tkeep  => (others => '1'),
    s_tid    => (others => 'U'),
    s_tvalid => conv_tvalid(3),
    s_tlast  => s_tlast,
    -- AXI stream output
    m_tready => axi_32apsk.tready,
    m_tdata  => axi_32apsk.tdata,
    m_tkeep  => open,
    m_tid    => open,
    m_tvalid => axi_32apsk.tvalid,
    m_tlast  => axi_32apsk.tlast);

  -- FIXME: Make this a RAM and let the CPU write the appropriate values esp for 16 APSK
  -- and 32 APSK since the values depend on the code rate
  coefficients_rom_u : entity fpga_cores.rom_inference
  generic map (
    ROM_DATA     => CONSTELLATION_ROM,
    ROM_TYPE     => auto,
    OUTPUT_DELAY => 1)
  port map (
    clk    => clk,
    clken  => '1',
    addr   => rom_addr,
    rddata => rom_data);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  mux_sel <= "0001" when cfg_constellation = mod_qpsk else
             "0010" when cfg_constellation = mod_8psk else
             "0100" when cfg_constellation = mod_16apsk else
             "1000" when cfg_constellation = mod_32apsk else
             (others => 'U');

  axi_qpsk.tready <= m_tready when cfg_constellation = mod_qpsk else '0';
  axi_8psk.tready <= m_tready when cfg_constellation = mod_8psk else '0';
  axi_16apsk.tready <= m_tready when cfg_constellation = mod_16apsk else '0';
  axi_32apsk.tready <= m_tready when cfg_constellation = mod_32apsk else '0';

  -- Addr CONSTELLATION_ROM offsets to the width converter output
  addr_qpsk   <= "0000" & axi_qpsk.tdata;
  addr_8psk   <= std_logic_vector("000" & unsigned(axi_8psk.tdata) + 4);
  addr_16apsk <= std_logic_vector("00" & unsigned(axi_16apsk.tdata) + 4 + 8);
  addr_32apsk <= std_logic_vector("0" & unsigned(axi_32apsk.tdata) + 4 + 8 + 16);

  -- Only one will be active at a time
  rom_addr <= (addr_qpsk   and (5 downto 0 => axi_qpsk.tvalid)) or
              (addr_8psk   and (5 downto 0 => axi_8psk.tvalid)) or
              (addr_16apsk and (5 downto 0 => axi_16apsk.tvalid)) or
              (addr_32apsk and (5 downto 0 => axi_32apsk.tvalid));

  m_tdata  <= rom_data when m_tvalid_i = '1' else (others => 'U');
  m_tvalid <= m_tvalid_i;

  ---------------
  -- Processes --
  ---------------
  process(clk)
  begin
    if clk'event and clk = '1' then
      m_tvalid_i <= axi_qpsk.tvalid or axi_8psk.tvalid or axi_16apsk.tvalid or axi_32apsk.tvalid;
      m_tlast    <= axi_qpsk.tlast or axi_8psk.tlast or axi_16apsk.tlast or axi_32apsk.tlast;
    end if;
  end process;

  process
  begin
    wait until rst = '0';
    for i in 0 to 3 loop
      info(sformat("CONSTELLATION_ROM[%d] = %r", fo(i), fo(CONSTELLATION_ROM(i))));
    end loop;
    wait;
  end process;

end axi_constellation_mapper;
