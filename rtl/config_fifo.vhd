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

---------------------------------
-- Block name and description --
--------------------------------

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
entity config_fifo is
  generic (
    FIFO_DEPTH          : integer := 4;
    RAM_INFERENCE_STYLE : string := "distributed"
  );
  port (
    -- Usual ports
    clk             : in  std_logic;
    rst             : in  std_logic;

    -- Config input
    wr_en           : in  std_logic;
    full            : out std_logic;
    constellation_i : in  constellation_t;
    frame_type_i    : in  frame_type_t;
    code_rate_i     : in  code_rate_t;

    -- Config output
    rd_en           : in  std_logic;
    empty           : out std_logic;
    constellation_o : out constellation_t;
    frame_type_o    : out frame_type_t;
    code_rate_o     : out code_rate_t);
end config_fifo;

architecture config_fifo of config_fifo is

  -----------
  -- Types --
  -----------
  constant DATA_WIDTH : integer := FRAME_TYPE_WIDTH + CONSTELLATION_WIDTH + CODE_RATE_WIDTH;

  -------------
  -- Signals --
  -------------
  signal wr_data : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal rd_data : std_logic_vector(DATA_WIDTH - 1 downto 0);

  signal wr_tready : std_logic;
  signal rd_tvalid : std_logic;

begin

  -------------------
  -- Port mappings --
  -------------------
  -- fifo_u : entity work.axi_stream_fifo
  --   generic map (
  --     FIFO_DEPTH          => FIFO_DEPTH,
  --     DATA_WIDTH          => DATA_WIDTH,
  --     RAM_INFERENCE_STYLE => RAM_INFERENCE_STYLE)
  --   port map (
  --     -- Usual ports
  --     clk      => clk,
  --     rst      => rst,

  --     -- Write side
  --     s_tvalid => wr_en,
  --     s_tready => wr_tready,
  --     s_tdata  => wr_data,
  --     s_tlast  => '0',

  --     -- Read side
  --     m_tvalid => rd_tvalid,
  --     m_tready => rd_en,
  --     m_tdata  => rd_data,
  --     m_tlast  => open);

  fifo_b_u : entity work.axi_stream_master_adapter
  generic map (
    MAX_SKEW_CYCLES => 4,
    TDATA_WIDTH     => DATA_WIDTH)
  port map (
    -- Usual ports
    clk      => clk,
    reset    => rst,
    -- wanna-be AXI interface
    wr_en    => wr_en,
    wr_full  => full,
    wr_data  => wr_data,
    wr_last  => '0',
    -- AXI master
    m_tvalid => rd_tvalid,
    m_tready => rd_en,
    m_tdata  => rd_data,
    m_tlast  => open);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  -- full  <= not wr_tready;
  empty <= not rd_tvalid;

  -- Squeeze config in
  wr_data <= encode(code_rate_i) &
             encode(constellation_i) &
             encode(frame_type_i);

  -- Then extract it back
  code_rate_o     <= decode(rd_data(rd_data'length - 1 downto FRAME_TYPE_WIDTH + CONSTELLATION_WIDTH));
  constellation_o <= decode(rd_data(FRAME_TYPE_WIDTH + CONSTELLATION_WIDTH - 1 downto FRAME_TYPE_WIDTH));
  frame_type_o    <= decode(rd_data(FRAME_TYPE_WIDTH - 1 downto 0));

end config_fifo;

