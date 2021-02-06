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

---------------------------------
-- Block name and description --
--------------------------------

---------------
-- Libraries --
---------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

------------------------
-- Entity declaration --
------------------------
entity axi_baseband_scrambler is
  generic (
    TDATA_WIDTH : positive := 8;
    TID_WIDTH   : natural  := 0
  );
  port (
    -- Usual ports
    clk      : in  std_logic;
    rst      : in  std_logic;

    -- AXI input
    s_tvalid : in  std_logic;
    s_tlast  : in  std_logic;
    s_tready : out std_logic;
    s_tdata  : in  std_logic_vector(TDATA_WIDTH - 1 downto 0);
    s_tid    : in  std_logic_vector(TID_WIDTH - 1 downto 0);

    -- AXI output
    m_tready : in  std_logic;
    m_tvalid : out std_logic;
    m_tlast  : out std_logic;
    m_tdata  : out std_logic_vector(TDATA_WIDTH - 1 downto 0);
    m_tid    : out std_logic_vector(TID_WIDTH - 1 downto 0));
end axi_baseband_scrambler;

architecture axi_baseband_scrambler of axi_baseband_scrambler is

  ---------------
  -- Constants --
  ---------------
  constant LFSR_WIDTH : integer := 15;
  constant LFSR_INIT  : std_logic_vector(LFSR_WIDTH - 1 downto 0) := "000000010101001";

  -------------
  -- Signals --
  -------------
  signal lfsr       : std_logic_vector(LFSR_WIDTH - 1 downto 0);
  signal s_axi_dv   : std_logic;
  signal m_axi_dv   : std_logic;

  -- Internals
  signal s_tready_i : std_logic;
  signal m_tvalid_i : std_logic;
  signal m_tdata_i  : std_logic_vector(TDATA_WIDTH - 1 downto 0);
  signal m_tid_i    : std_logic_vector(TID_WIDTH - 1 downto 0);

begin

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  s_axi_dv <= s_tready_i and s_tvalid;
  m_axi_dv <= m_tready and m_tvalid_i;

  s_tready_i <= m_tready;

  -- Assign internals
  m_tdata  <= m_tdata_i when m_tvalid_i = '1' else (others => 'U');
  m_tid    <= m_tid_i when m_tvalid_i = '1' else (others => 'U');
  s_tready <= s_tready_i;
  m_tvalid <= m_tvalid_i;

  ---------------
  -- Processes --
  ---------------
  process(clk, rst)
    variable next_lfsr : std_logic_vector(LFSR_WIDTH - 1 downto 0);
  begin
    if rst = '1' then
      lfsr       <= LFSR_INIT;
      m_tlast    <= '0';
      m_tvalid_i <= '0';
    elsif clk'event and clk = '1' then
      if m_axi_dv = '1' then
        m_tvalid_i <= '0';
        m_tlast    <= '0';
      end if;

      if s_axi_dv = '1' then
        m_tvalid_i <= '1';
        m_tlast    <= s_tlast;
        m_tid_i    <= s_tid;

        -- Calculate the next LFSR
        next_lfsr := lfsr;
        for i in 0 to TDATA_WIDTH - 1 loop
          next_lfsr := next_lfsr(LFSR_WIDTH - 2 downto 0) & (next_lfsr(13) xor next_lfsr(14));
          m_tdata_i(TDATA_WIDTH - 1 - i) <= next_lfsr(0) xor s_tdata(TDATA_WIDTH - 1 - i);
        end loop;

        -- Reinit the LFSR at every end of frame
        lfsr   <= next_lfsr;
        if s_tlast = '1' then
          lfsr <= LFSR_INIT;
        end if;

      end if;
    end if;
  end process;

end axi_baseband_scrambler;
