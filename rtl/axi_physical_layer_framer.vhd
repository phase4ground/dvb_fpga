-- vim: set foldmethod=marker foldmarker=--\ {{,--\ }} :
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
use fpga_cores.axi_pkg.all;
use fpga_cores.common_pkg.all;

use work.dvb_utils_pkg.all;

------------------------
-- Entity declaration --
------------------------
entity axi_physical_layer_framer is
  generic (
    TDATA_WIDTH : natural := 1;
    TID_WIDTH   : natural := 1);
  port (
    -- Usual ports
    clk                : in  std_logic;
    rst                : in  std_logic;

    -- This takes effect for the following frame
    cfg_shift_reg_init : in  std_logic_vector(17 downto 0) := (0 => '1', others => '0');

    cfg_constellation  : in  constellation_t;
    cfg_frame_type     : in  frame_type_t;
    cfg_code_rate      : in  code_rate_t;

    -- AXI input
    s_tvalid           : in  std_logic;
    s_tready           : out std_logic;
    s_tlast            : in  std_logic;
    s_tdata            : in  std_logic_vector(TDATA_WIDTH - 1 downto 0);
    s_tid              : in  std_logic_vector(TID_WIDTH - 1 downto 0);

    -- AXI output
    m_tready           : in  std_logic;
    m_tvalid           : out std_logic;
    m_tlast            : out std_logic;
    m_tdata            : out std_logic_vector(TDATA_WIDTH - 1 downto 0);
    m_tid              : out std_logic_vector(TID_WIDTH - 1 downto 0));
end axi_physical_layer_framer;

architecture axi_physical_layer_framer of axi_physical_layer_framer is

  signal s_tready_i       : std_logic;
  signal s_first_word     : std_logic;
  signal s_tready_header  : std_logic;
  signal s_tvalid_header  : std_logic;
  signal header_in_dv     : std_logic;
  signal arbiter_tlast    : std_logic;
  signal selected         : std_logic_vector(1 downto 0);
  signal sampled          : axi_stream_bus_t(tdata(TDATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal scrambled        : axi_stream_bus_t(tdata(TDATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal axi_header_out   : axi_stream_bus_t(tdata(TDATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));

begin

  -------------------
  -- Port mappings --
  -------------------
  input_delay_block : block
    signal tdata_in_agg : std_logic_vector(TDATA_WIDTH downto 0);
    signal tdata_out_agg : std_logic_vector(TDATA_WIDTH downto 0);
  begin
    input_delay_u : entity fpga_cores.axi_stream_delay
      generic map (
        DELAY_CYCLES => 1,
        TDATA_WIDTH  => TDATA_WIDTH + 1)
      port map (
        -- Usual ports
        clk     => clk,
        rst     => rst,

        -- AXI slave input
        s_tvalid => s_tvalid,
        s_tready => s_tready_i,
        s_tdata  => tdata_in_agg,

        -- AXI master output
        m_tvalid => sampled.tvalid,
        m_tready => sampled.tready,
        m_tdata  => tdata_out_agg);

      tdata_in_agg <= s_tlast & s_tdata;
      sampled.tdata <= tdata_out_agg(TDATA_WIDTH - 1 downto 0);
      sampled.tlast <= tdata_out_agg(TDATA_WIDTH);
    end block;

  plframe_header_gen_u : entity work.axi_plframe_header
    generic map ( DATA_WIDTH => TDATA_WIDTH)
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,
      -- Parameter input
      cfg_constellation => cfg_constellation,
      cfg_frame_type    => cfg_frame_type,
      cfg_code_rate     => cfg_code_rate,

      -- AXI data input
      s_tready          => s_tready_header,
      s_tvalid          => s_tvalid_header,

      -- AXI output
      m_tready          => axi_header_out.tready,
      m_tvalid          => axi_header_out.tvalid,
      m_tlast           => axi_header_out.tlast,
      m_tdata           => axi_header_out.tdata);

  physical_layer_scrambler : entity work.axi_physical_layer_scrambler
    generic map (
      TDATA_WIDTH => TDATA_WIDTH,
      TID_WIDTH   => TID_WIDTH)
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      cfg_shift_reg_init => cfg_shift_reg_init,

      -- AXI input
      s_tvalid          => sampled.tvalid,
      s_tready          => sampled.tready,
      s_tlast           => sampled.tlast,
      s_tdata           => sampled.tdata,
      s_tid             => (others => 'U'),

      -- AXI output
      m_tready          => scrambled.tready,
      m_tvalid          => scrambled.tvalid,
      m_tlast           => scrambled.tlast,
      m_tdata           => scrambled.tdata,
      m_tid             => open);

  arbiter_u : entity fpga_cores.axi_stream_arbiter
    generic map (
      MODE            => "INTERLEAVED",
      INTERFACES      => 2,
      DATA_WIDTH      => TDATA_WIDTH,
      REGISTER_INPUTS => False)
    port map (
      -- Usual ports
      clk              => clk,
      rst              => rst,

      selected         => selected,
      selected_encoded => open,

      -- AXI slave input
      s_tvalid(0)      => axi_header_out.tvalid,
      s_tvalid(1)      => scrambled.tvalid,

      s_tready(0)      => axi_header_out.tready,
      s_tready(1)      => scrambled.tready,

      -- Really need to look into fixing this endianness thing...
      s_tdata(0)       => axi_header_out.tdata,
      s_tdata(1)       => scrambled.tdata,

      s_tlast(0)       => axi_header_out.tlast,
      s_tlast(1)       => scrambled.tlast,

      -- AXI master output
      m_tvalid         => m_tvalid,
      m_tready         => m_tready,
      m_tdata          => m_tdata,
      m_tlast          => arbiter_tlast);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  s_tready        <= s_tready_i;
  s_tvalid_header <= s_tvalid and s_first_word;
  header_in_dv    <= s_tvalid_header and s_tready_header;

  -- Suppress tlast from plframe header and let s_tlast pass through
  m_tlast         <= arbiter_tlast and selected(1);

  ---------------
  -- Processes --
  ---------------
  process(clk, rst)
  begin
    if rst = '1' then
      s_first_word <= '1';
    elsif clk'event and clk = '1' then
      if s_tvalid = '1' and s_tready_i = '1' then
        s_first_word <= s_tlast;
        m_tid        <= s_tid;
      end if;
    end if;
  end process;

end axi_physical_layer_framer;
