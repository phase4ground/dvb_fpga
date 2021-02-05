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
entity axi_ldpc_encoder is
  generic ( TID_WIDTH : integer := 0 );
  port (
    -- Usual ports
    clk               : in  std_logic;
    rst               : in  std_logic;

    cfg_constellation : in  constellation_t;
    cfg_frame_type    : in  frame_type_t;
    cfg_code_rate     : in  code_rate_t;

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
end axi_ldpc_encoder;

architecture axi_ldpc_encoder of axi_ldpc_encoder is

  -------------
  -- Signals --
  -------------
  signal s_tready_i   : std_logic;
  signal first_word   : std_logic;
  signal cfg_tready   : std_logic;
  signal cfg_tvalid   : std_logic;

  signal table_tready : std_logic;
  signal table_tvalid : std_logic;
  signal table_offset : std_logic_vector(numbits(max(DVB_N_LDPC)) - 1 downto 0);
  signal table_next   : std_logic;
  signal table_tuser  : std_logic_vector(numbits(max(DVB_N_LDPC)) - 1 downto 0);
  signal table_tlast  : std_logic;



begin

  table_u : entity work.axi_ldpc_table
    port map (
      -- Usual ports
      clk          => clk,
      rst          => rst,

      -- Parameter input
      s_frame_type => cfg_frame_type,
      s_code_rate  => cfg_code_rate,
      s_tready     => cfg_tready,
      s_tvalid     => cfg_tvalid,

      -- Config out
      m_tready     => table_tready,
      m_tvalid     => table_tvalid,
      m_offset     => table_offset,
      m_next       => table_next,
      m_tuser      => table_tuser,
      m_tlast      => table_tlast);

  encoder_u : entity work.axi_ldpc_encoder_core
    generic map ( TID_WIDTH => TID_WIDTH )
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,

      cfg_constellation  => cfg_constellation,
      cfg_frame_type     => cfg_frame_type,
      cfg_code_rate      => cfg_code_rate,

      -- AXI LDPC table input
      s_ldpc_tready      => table_tready,
      s_ldpc_tvalid      => table_tvalid,
      s_ldpc_offset      => table_offset,
      s_ldpc_next        => table_next,
      s_ldpc_tuser       => table_tuser,
      s_ldpc_tlast       => table_tlast,

      -- AXI data input
      s_tready           => s_tready_i,
      s_tvalid           => s_tvalid,
      s_tlast            => s_tlast,
      s_tdata            => s_tdata,
      s_tid              => s_tid,

      -- AXI output
      m_tready           => m_tready,
      m_tvalid           => m_tvalid,
      m_tlast            => m_tlast,
      m_tdata            => m_tdata,
      m_tid              => m_tid);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  s_tready   <= s_tready_i;
  cfg_tvalid <= s_tvalid and cfg_tready when first_word = '1' else '0';

  ---------------
  -- Processes --
  ---------------
  process(clk, rst)
  begin
    if rst = '1' then
      first_word <= '1';
    elsif rising_edge(clk) then
      -- Clear flag when config has been written
      if cfg_tvalid = '1' and cfg_tready = '1' then
        first_word <= '0';
      end if;

      if s_tvalid = '1' and s_tready_i = '1' then
        first_word <= s_tlast;
      end if;
    end if;
  end process;

end axi_ldpc_encoder;

-- vim: set foldmethod=marker foldmarker=--\ {{,--\ }} :
