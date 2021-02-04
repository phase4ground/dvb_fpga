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

library fpga_cores;
use fpga_cores.common_pkg.all;
use fpga_cores.axi_pkg.all;

use work.dvb_utils_pkg.all;

------------------------
-- Entity declaration --
------------------------
entity axi_constellation_mapper is
  generic (
    INPUT_DATA_WIDTH  : integer := 8;
    OUTPUT_DATA_WIDTH : integer := 32
  );
  port (
    -- Usual ports
    clk               : in  std_logic;
    rst               : in  std_logic;

    -- Mapping RAM config
    ram_wren          : in  std_logic;
    ram_addr          : in  std_logic_vector(5 downto 0);
    ram_wdata         : in  std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);
    ram_rdata         : out std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);

    -- Config input
    cfg_constellation : in  constellation_t;
    cfg_frame_type    : in  frame_type_t;
    cfg_code_rate     : in  code_rate_t;

    -- AXI data input
    s_tready          : out std_logic;
    s_tvalid          : in  std_logic;
    s_tlast           : in  std_logic;
    s_tdata           : in  std_logic_vector(INPUT_DATA_WIDTH - 1 downto 0);

    -- AXI output
    m_tready          : in  std_logic;
    m_tvalid          : out std_logic;
    m_tlast           : out std_logic;
    m_tdata           : out std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0));
end axi_constellation_mapper;

architecture axi_constellation_mapper of axi_constellation_mapper is

  constant CONFIG_INPUT_WIDTHS: fpga_cores.common_pkg.integer_vector_t := (
    0 => FRAME_TYPE_WIDTH,
    1 => CONSTELLATION_WIDTH,
    2 => CODE_RATE_WIDTH);

  constant TUSER_WIDTH : integer := sum(CONFIG_INPUT_WIDTHS);

  -------------
  -- Signals --
  -------------
  signal s_tid                : std_logic_vector(TUSER_WIDTH - 1 downto 0);
  signal mux_sel              : std_logic_vector(3 downto 0);
  signal conv_tready          : std_logic_vector(3 downto 0);
  signal conv_tvalid          : std_logic_vector(3 downto 0);

  signal axi_qpsk             : axi_stream_bus_t(tdata(1 downto 0), tuser(sum(CONFIG_INPUT_WIDTHS) - 1 downto 0));
  signal axi_8psk             : axi_stream_bus_t(tdata(2 downto 0), tuser(sum(CONFIG_INPUT_WIDTHS) - 1 downto 0));
  signal axi_16apsk           : axi_stream_bus_t(tdata(3 downto 0), tuser(sum(CONFIG_INPUT_WIDTHS) - 1 downto 0));
  signal axi_32apsk           : axi_stream_bus_t(tdata(4 downto 0), tuser(sum(CONFIG_INPUT_WIDTHS) - 1 downto 0));

  signal addr_qpsk            : std_logic_vector(5 downto 0);
  signal addr_8psk            : std_logic_vector(5 downto 0);
  signal addr_16apsk          : std_logic_vector(5 downto 0);
  signal addr_32apsk          : std_logic_vector(5 downto 0);

  signal map_addr             : std_logic_vector(5 downto 0);
  signal map_data             : std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);
  signal map_cfg              : std_logic_vector(sum(CONFIG_INPUT_WIDTHS) - 1 downto 0);

  -- ROM side config, so won't need to wait until an entire frame goes through
  signal egress_constellation : constellation_t;
  signal egress_frame_type    : frame_type_t;
  signal egress_code_rate     : code_rate_t;

  signal adapter_wr_en        : std_logic;
  signal adapter_full         : std_logic;
  signal adapter_wr_last      : std_logic;

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
    INPUT_DATA_WIDTH  => INPUT_DATA_WIDTH,
    OUTPUT_DATA_WIDTH => 2,
    AXI_TID_WIDTH     => sum(CONFIG_INPUT_WIDTHS),
    ENDIANNESS        => LEFT_FIRST)
  port map (
    -- Usual ports
    clk      => clk,
    rst      => rst,
    -- AXI stream input
    s_tready => conv_tready(0),
    s_tdata  => s_tdata,
    s_tkeep  => (others => '1'),
    s_tid    => s_tid,
    s_tvalid => conv_tvalid(0),
    s_tlast  => s_tlast,
    -- AXI stream output
    m_tready => axi_qpsk.tready,
    m_tdata  => axi_qpsk.tdata,
    m_tkeep  => open,
    m_tid    => axi_qpsk.tuser,
    m_tvalid => axi_qpsk.tvalid,
    m_tlast  => axi_qpsk.tlast);

  width_converter_8psk_u : entity fpga_cores.axi_stream_width_converter
  generic map (
    INPUT_DATA_WIDTH  => INPUT_DATA_WIDTH,
    OUTPUT_DATA_WIDTH => 3,
    AXI_TID_WIDTH     => sum(CONFIG_INPUT_WIDTHS),
    ENDIANNESS        => LEFT_FIRST)
  port map (
    -- Usual ports
    clk      => clk,
    rst      => rst,
    -- AXI stream input
    s_tready => conv_tready(1),
    s_tdata  => s_tdata,
    s_tkeep  => (others => '1'),
    s_tid    => s_tid,
    s_tvalid => conv_tvalid(1),
    s_tlast  => s_tlast,
    -- AXI stream output
    m_tready => axi_8psk.tready,
    m_tdata  => axi_8psk.tdata,
    m_tkeep  => open,
    m_tid    => axi_8psk.tuser,
    m_tvalid => axi_8psk.tvalid,
    m_tlast  => axi_8psk.tlast);

  width_converter_16apsk_u : entity fpga_cores.axi_stream_width_converter
  generic map (
    INPUT_DATA_WIDTH  => INPUT_DATA_WIDTH,
    OUTPUT_DATA_WIDTH => 4,
    AXI_TID_WIDTH     => sum(CONFIG_INPUT_WIDTHS),
    ENDIANNESS        => LEFT_FIRST)
  port map (
    -- Usual ports
    clk      => clk,
    rst      => rst,
    -- AXI stream input
    s_tready => conv_tready(2),
    s_tdata  => s_tdata,
    s_tkeep  => (others => '1'),
    s_tid    => s_tid,
    s_tvalid => conv_tvalid(2),
    s_tlast  => s_tlast,
    -- AXI stream output
    m_tready => axi_16apsk.tready,
    m_tdata  => axi_16apsk.tdata,
    m_tkeep  => open,
    m_tid    => axi_16apsk.tuser,
    m_tvalid => axi_16apsk.tvalid,
    m_tlast  => axi_16apsk.tlast);

  width_converter_32apsk_u : entity fpga_cores.axi_stream_width_converter
  generic map (
    INPUT_DATA_WIDTH  => INPUT_DATA_WIDTH,
    OUTPUT_DATA_WIDTH => 5,
    AXI_TID_WIDTH     => sum(CONFIG_INPUT_WIDTHS),
    ENDIANNESS        => LEFT_FIRST)
  port map (
    -- Usual ports
    clk      => clk,
    rst      => rst,
    -- AXI stream input
    s_tready => conv_tready(3),
    s_tdata  => s_tdata,
    s_tkeep  => (others => '1'),
    s_tid    => s_tid,
    s_tvalid => conv_tvalid(3),
    s_tlast  => s_tlast,
    -- AXI stream output
    m_tready => axi_32apsk.tready,
    m_tdata  => axi_32apsk.tdata,
    m_tkeep  => open,
    m_tid    => axi_32apsk.tuser,
    m_tvalid => axi_32apsk.tvalid,
    m_tlast  => axi_32apsk.tlast);

  -- QPSK and 8 PSK values are constant but 16 APSK and 32 APSK depend on the coding
  -- rate. Currently we're supporting any number of QPSK and/or 8 PSK streams, but 16 APSK
  -- and 32 APSK are both limited to a single stream
  mapping_table_u : entity fpga_cores.ram_inference
    generic map (
      ADDR_WIDTH   => 6,
      DATA_WIDTH   => OUTPUT_DATA_WIDTH,
      RAM_TYPE     => auto,
      OUTPUT_DELAY => 1)
    port map (
      -- Port A
      clk_a     => clk,
      clken_a   => '1',
      wren_a    => ram_wren,
      addr_a    => ram_addr,
      wrdata_a  => ram_wdata,
      rddata_a  => ram_rdata,

      -- Port B
      clk_b     => clk,
      clken_b   => '1',
      addr_b    => map_addr,
      rddata_b  => map_data);

  -- map data comes with 1 cycle of latency, compensate for that
  output_adapter_u : entity fpga_cores.axi_stream_master_adapter
    generic map (
      MAX_SKEW_CYCLES => 1,
      TDATA_WIDTH     => OUTPUT_DATA_WIDTH)
    port map (
      -- Usual ports
      clk      => clk,
      reset    => rst,
      -- wanna-be AXI interface
      wr_en    => adapter_wr_en,
      wr_full  => adapter_full,
      wr_empty => open,
      wr_data  => map_data,
      wr_last  => adapter_wr_last,
      -- AXI master
      m_tvalid => m_tvalid,
      m_tready => m_tready,
      m_tdata  => m_tdata,
      m_tlast  => m_tlast);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  with cfg_constellation select
    mux_sel <= "0001" when mod_qpsk,
               "0010" when mod_8psk,
               "0100" when mod_16apsk,
               "1000" when mod_32apsk,
               (others => 'U') when others;

  -- Addr CONSTELLATION_ROM offsets to the width converter output. Each table starts
  -- immediatelly after the previous
  addr_qpsk   <= "0000" & axi_qpsk.tdata;
  addr_8psk   <= std_logic_vector("000" & unsigned(axi_8psk.tdata) + 4);          -- 8 PSK starts after QPSK
  addr_16apsk <= std_logic_vector("00" & unsigned(axi_16apsk.tdata) + 4 + 8);     -- 16 APSK starts after QPSK + 8 PSK
  addr_32apsk <= std_logic_vector("0" & unsigned(axi_32apsk.tdata) + 4 + 8 + 16); -- 32 APSK starts after QPSK + 8 PSK + 16 APSK

  -- Only one will be active at a time
  map_addr <= (addr_qpsk   and (5 downto 0 => axi_qpsk.tvalid)) or
              (addr_8psk   and (5 downto 0 => axi_8psk.tvalid)) or
              (addr_16apsk and (5 downto 0 => axi_16apsk.tvalid)) or
              (addr_32apsk and (5 downto 0 => axi_32apsk.tvalid));

  -- Select the config from the active channel (only one will be writing at a time)
  map_cfg <= (axi_qpsk.tuser   and (sum(CONFIG_INPUT_WIDTHS) - 1 downto 0 => axi_qpsk.tvalid)) or
             (axi_8psk.tuser   and (sum(CONFIG_INPUT_WIDTHS) - 1 downto 0 => axi_8psk.tvalid)) or
             (axi_16apsk.tuser and (sum(CONFIG_INPUT_WIDTHS) - 1 downto 0 => axi_16apsk.tvalid)) or
             (axi_32apsk.tuser and (sum(CONFIG_INPUT_WIDTHS) - 1 downto 0 => axi_32apsk.tvalid));

  egress_frame_type    <= decode(get_field(map_cfg, 0, CONFIG_INPUT_WIDTHS));
  egress_constellation <= decode(get_field(map_cfg, 1, CONFIG_INPUT_WIDTHS));
  egress_code_rate     <= decode(get_field(map_cfg, 2, CONFIG_INPUT_WIDTHS));

  axi_qpsk.tready      <= not adapter_full when egress_constellation = mod_qpsk else '0';
  axi_8psk.tready      <= not adapter_full when egress_constellation = mod_8psk else '0';
  axi_16apsk.tready    <= not adapter_full when egress_constellation = mod_16apsk else '0';
  axi_32apsk.tready    <= not adapter_full when egress_constellation = mod_32apsk else '0';

  s_tid    <= encode(cfg_code_rate) & encode(cfg_constellation) & encode(cfg_frame_type);

  ---------------
  -- Processes --
  ---------------
  process(clk)
  begin
    if clk'event and clk = '1' then
      adapter_wr_en   <= '0';
      adapter_wr_last <= '0';
      if not adapter_full then
        adapter_wr_en   <= axi_qpsk.tvalid or axi_8psk.tvalid or axi_16apsk.tvalid or axi_32apsk.tvalid;
        adapter_wr_last <= axi_qpsk.tlast or axi_8psk.tlast or axi_16apsk.tlast or axi_32apsk.tlast;
      end if;
    end if;
  end process;

end axi_constellation_mapper;
