--
-- DVB IP
--
-- Copyright 2019 by Andre Souto (suoto)
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.common_pkg.all;

------------------------
-- Entity declaration --
------------------------
entity axi_stream_master_adapter is
  generic (
    MAX_SKEW_CYCLES : natural := 1;
    TDATA_WIDTH     : natural := 32);
  port (
    -- Usual ports
    clk      : in  std_logic;
    reset    : in  std_logic;
    -- wanna-be AXI interface
    wr_en    : in  std_logic;
    wr_full  : out std_logic;
    wr_data  : in  std_logic_vector(TDATA_WIDTH - 1 downto 0);
    wr_last  : in  std_logic;
    -- AXI master
    m_tvalid : out std_logic;
    m_tready : in  std_logic;
    m_tdata  : out std_logic_vector(TDATA_WIDTH - 1 downto 0);
    m_tlast  : out std_logic);
end axi_stream_master_adapter;

architecture axi_stream_master_adapter of axi_stream_master_adapter is

  ---------------
  -- Constants --
  ---------------
  -- Need some more legroom to cope with internal delays
  constant BUFFER_DEPTH : positive := MAX_SKEW_CYCLES + 2;

  -----------
  -- Types --
  -----------
  type data_array_t is array (BUFFER_DEPTH - 1 downto 0)
    of std_logic_vector(TDATA_WIDTH downto 0);

  -------------
  -- Signals --
  -------------
  signal data_buffer : data_array_t;

  signal axi_tvalid  : std_logic;
  signal axi_dv      : std_logic;

  signal wr_ptr    : unsigned(numbits(BUFFER_DEPTH) - 1 downto 0);
  signal rd_ptr    : unsigned(numbits(BUFFER_DEPTH) - 1 downto 0);
  signal ptr_diff  : unsigned(numbits(BUFFER_DEPTH) - 1 downto 0);

  signal wr_full_i : std_logic;

begin

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  -- Extract data asynchronously to avoid inserting bubbles
  m_tdata <= data_buffer(to_integer(rd_ptr))(TDATA_WIDTH - 1 downto 0);
  m_tlast <= data_buffer(to_integer(rd_ptr))(TDATA_WIDTH);

  -- tvalid is asserted when pointers are different, regardless of tready. Also, there's
  -- no need to force it to 0 when reset = '1' because both pointers are asynchronously
  -- reset
  axi_tvalid <= '1' when ptr_diff /= 0 else '0';

  axi_dv     <= '1' when axi_tvalid = '1' and m_tready = '1' else '0';

  -- Assert the full flag whenever we run out of space to store more data. At this
  -- point, if the write interface doesn't respect MAX_SKEW_CYCLES *and* m_tready is
  -- deasserted, there will loss of data
  wr_full_i    <= '1' when ptr_diff >= BUFFER_DEPTH - MAX_SKEW_CYCLES else '0';

  -- Assign internals
  m_tvalid <= axi_tvalid;
  wr_full  <= wr_full_i;

  ---------------
  -- Processes --
  ---------------
  -- Put the memory write on a separate process as it can happen irrespectively of
  -- reset
  mem_write : process(clk)
  begin
      if rising_edge(clk) then
          if wr_en = '1' then
              data_buffer(to_integer(wr_ptr)) <= wr_last & wr_data;
          end if;
      end if;
  end process;

  -- Update pointers
  ptr_update : process(clk, reset)
  begin
    if reset = '1' then
      wr_ptr   <= (others => '0');
      rd_ptr   <= (others => '0');
      ptr_diff <= (others => '0');
    elsif rising_edge(clk) then
      if wr_en = '1' and  ptr_diff /= 0 and wr_ptr = rd_ptr then
        report "AXI adapter overflow"
        severity error;
      end if;

      -- Update buffer occupation
      if wr_en = '1' and axi_dv = '0' then
        ptr_diff <= ptr_diff + 1;
      elsif wr_en = '0' and axi_dv = '1' then
        ptr_diff <= ptr_diff - 1;
      end if;

      if wr_en = '1' then
        -- Manually wrap write pointer around BUFFER_DEPTH
        if wr_ptr = BUFFER_DEPTH - 1 then
          wr_ptr <= (others => '0');
        else
          wr_ptr <= wr_ptr + 1;
        end if;

      end if;

      if axi_dv = '1' then
        -- Manually wrap read pointer around BUFFER_DEPTH
        if rd_ptr = BUFFER_DEPTH - 1 then
          rd_ptr <= (others => '0');
        else
          rd_ptr <= rd_ptr + 1;
        end if;
      end if;

    end if;
  end process;

end axi_stream_master_adapter;
