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
-- Reference implementation:
-- https://github.com/drmpeg/gr-dvbs2/blob/ec0211000c356757c222f8d9355fd41356d6be66/lib/physical_cc_impl.cc#L162-L190

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
entity axi_physical_layer_scrambler is
  generic (
    TDATA_WIDTH : natural := 1;
    TID_WIDTH   : natural := 1);
  port (
    -- Usual ports
    clk                : in  std_logic;
    rst                : in  std_logic;

    cfg_shift_reg_init : in  std_logic_vector(17 downto 0) := (0 => '1', others => '0');

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
end axi_physical_layer_scrambler;

architecture axi_physical_layer_scrambler of axi_physical_layer_scrambler is

  signal s_tready_i : std_logic;
  signal m_tvalid_i : std_logic;
  signal x          : std_logic_vector(17 downto 0);
  signal y          : std_logic_vector(17 downto 0);

  signal xa, xb     : std_logic;
  signal ya, yb     : std_logic;
  signal zna, znb   : std_logic;

  signal sel        : std_logic_vector(1 downto 0);
  signal s_tdata_re : signed(TDATA_WIDTH/2 - 1 downto 0);
  signal s_tdata_im : signed(TDATA_WIDTH/2 - 1 downto 0);
  signal m_tdata_re : signed(TDATA_WIDTH/2 - 1 downto 0);
  signal m_tdata_im : signed(TDATA_WIDTH/2 - 1 downto 0);

begin

  -------------------
  -- Port mappings --
  -------------------

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  s_tready_i <= m_tready;
  s_tready   <= s_tready_i;
  m_tvalid   <= m_tvalid_i;

  s_tdata_re <= signed(s_tdata(TDATA_WIDTH - 1 downto TDATA_WIDTH/2));
  s_tdata_im <= signed(s_tdata(TDATA_WIDTH/2 - 1 downto 0));

  m_tdata    <= std_logic_vector(m_tdata_re & m_tdata_im) when m_tvalid_i = '1' else (others => 'U');

  xa <= x(15) xor x(6) xor x(4);
  xb <= x(7) xor x(0);

  ya <= y(10) xor y(7) xor y(5) xor y(0);
  yb <= y(15) xor
        y(14) xor
        y(13) xor
        y(12) xor
        y(11) xor
        y(10) xor
        y(9) xor
        y(8) xor
        y(6) xor
        y(5);

  zna <= x(0) xor y(0);
  znb <= xa xor yb;

  sel <= znb & zna;

  ---------------
  -- Processes --
  ---------------
  process(clk, rst, cfg_shift_reg_init)
    -- Round the negative end of the scale so it has a positive counterpart
    function round_if_needed ( constant v : signed(TDATA_WIDTH/2 - 1 downto 0) ) return signed is
    begin
      if v(v'high) = '1' and v(v'high - 1 downto v'low) = 0 then
        return v + 1;
      end if;
      return v;
    end function;
  begin
    if rst = '1' then
      x          <= cfg_shift_reg_init;
      y          <= (others => '1');
      m_tvalid_i <= '0';
      m_tlast    <= '0';
      m_tdata_re  <= (others => 'U');
      m_tdata_im  <= (others => 'U');
    elsif clk'event and clk = '1' then
      if m_tready = '1' then
        m_tvalid_i <= '0';
        m_tlast    <= '0';
        m_tid      <= (others => 'U');
      end if;

      if s_tvalid = '1' and s_tready_i = '1' then
        x <= xb & x(17 downto 1);
        y <= ya & y(17 downto 1);

        m_tvalid_i <= '1';
        m_tlast    <= s_tlast;
        m_tid      <= s_tid;

        case sel is
          when "00"   =>
            m_tdata_re <= s_tdata_re;
            m_tdata_im <= s_tdata_im;

          when "01"   =>
            m_tdata_re <= -round_if_needed(s_tdata_im);
            m_tdata_im <= s_tdata_re;

          when "10"   =>
            m_tdata_re <= -round_if_needed(s_tdata_re);
            m_tdata_im <= -round_if_needed(s_tdata_im);

          when "11"   =>
            m_tdata_re <= s_tdata_im;
            m_tdata_im <= -round_if_needed(s_tdata_re);

          when others =>
            m_tdata_re <= (others => 'U');
            m_tdata_im <= (others => 'U');
        end case;

        -- Reinitialize X for the next frame
        if s_tlast = '1' then
          x <= cfg_shift_reg_init;
          y <= (others => '1');
        end if;
      end if;
    end if;
  end process;

end axi_physical_layer_scrambler;
