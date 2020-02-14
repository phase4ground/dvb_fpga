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

---------------
-- Libraries --
---------------
library ieee;
use ieee.std_logic_1164.all;

use work.dvb_utils_pkg.all;

------------------------
-- Entity declaration --
------------------------
entity dvbs2_tx is
  generic (DATA_WIDTH : positive := 8);
  port (
    -- Usual ports
    clk               : in  std_logic;
    rst               : in  std_logic;

    cfg_constellation : in  std_logic_vector(CONSTELLATION_WIDTH - 1 downto 0);
    cfg_frame_type    : in  std_logic_vector(FRAME_TYPE_WIDTH - 1 downto 0);
    cfg_code_rate     : in  std_logic_vector(CODE_RATE_WIDTH - 1 downto 0);

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
end dvbs2_tx;

architecture dvbs2_tx of dvbs2_tx is

  ---------------
  -- Constants --
  ---------------
  constant CHAIN_LENGTH : positive := 4;

  -----------
  -- Types --
  -----------
  type tdata_array_t is array (natural range <>) of std_logic_vector(DATA_WIDTH - 1 downto 0);

  -------------
  -- Signals --
  -------------
  signal frame_type    : frame_type_array_t(CHAIN_LENGTH - 1 downto 0);
  signal constellation : constellation_array_t(CHAIN_LENGTH - 1 downto 0);
  signal code_rate     : code_rate_array_t(CHAIN_LENGTH - 1 downto 0);
  signal tdata         : tdata_array_t(CHAIN_LENGTH - 1 downto 0);
  signal tvalid        : std_logic_vector(CHAIN_LENGTH - 1 downto 0);
  signal tready        : std_logic_vector(CHAIN_LENGTH - 1 downto 0);
  signal tlast         : std_logic_vector(CHAIN_LENGTH - 1 downto 0);
  signal axi_dv        : std_logic_vector(CHAIN_LENGTH - 1 downto 0);
  signal cfg_sample_en : std_logic_vector(CHAIN_LENGTH - 1 downto 0);
  signal first_word    : std_logic_vector(CHAIN_LENGTH - 1 downto 0);

begin

  -------------------
  -- Port mappings --
  -------------------
  bb_scramber_u : entity work.axi_baseband_scrambler
    generic map (DATA_WIDTH => DATA_WIDTH)
    port map (
      -- Usual ports
      clk      => clk,
      rst      => rst,

      -- AXI input
      s_tvalid => tvalid(0),
      s_tdata  => tdata(0),
      s_tlast  => tlast(0),
      s_tready => tready(0),

      -- AXI output
      m_tready => tready(1),
      m_tvalid => tvalid(1),
      m_tlast  => tlast(1),
      m_tdata  => tdata(1));

  bch_encoder_cfg_fifo_u : entity work.config_fifo
    generic map ( FIFO_DEPTH => 4 )
    port map (
      -- Usual ports
      clk             => clk,
      rst             => rst,

      -- Write side
      wr_en           => cfg_sample_en(0),
      full            => open,
      constellation_i => constellation(0),
      frame_type_i    => frame_type(0),
      code_rate_i     => code_rate(0),

      -- Read side
      rd_en           => cfg_sample_en(1),
      empty           => open,
      constellation_o => constellation(1),
      frame_type_o    => frame_type(1),
      code_rate_o     => code_rate(1));

  bch_encoder_u : entity work.axi_bch_encoder
    generic map (DATA_WIDTH => DATA_WIDTH)
    port map (
      -- Usual ports
      clk            => clk,
      rst            => rst,

      cfg_frame_type => frame_type(1),
      cfg_code_rate  => code_rate(1),

      -- AXI input
      s_tvalid       => tvalid(1),
      s_tdata        => tdata(1),
      s_tlast        => tlast(1),
      s_tready       => tready(1),

      -- AXI output
      m_tready       => tready(2),
      m_tvalid       => tvalid(2),
      m_tlast        => tlast(2),
      m_tdata        => tdata(2));

  bit_interleaver_config_fifo_u : entity work.config_fifo
    generic map ( FIFO_DEPTH => 4 )
    port map (
      -- Usual ports
      clk             => clk,
      rst             => rst,

      -- Write side
      wr_en           => cfg_sample_en(1),
      full            => open,
      constellation_i => constellation(1),
      frame_type_i    => frame_type(1),
      code_rate_i     => code_rate(1),

      -- Read side
      rd_en           => cfg_sample_en(2),
      empty           => open,
      constellation_o => constellation(2),
      frame_type_o    => frame_type(2),
      code_rate_o     => code_rate(2));

  bit_interleaver_u : entity work.axi_bit_interleaver
    generic map (DATA_WIDTH => DATA_WIDTH)
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      cfg_frame_type    => frame_type(2),
      cfg_constellation => constellation(2),
      cfg_code_rate     => code_rate(2),

      -- AXI input
      s_tvalid          => tvalid(2),
      s_tdata           => tdata(2),
      s_tlast           => tlast(2),
      s_tready          => tready(2),

      -- AXI output
      m_tready          => tready(3),
      m_tvalid          => tvalid(3),
      m_tlast           => tlast(3),
      m_tdata           => tdata(3));

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  g_axi_data_valid : for i in 0 to CHAIN_LENGTH - 1 generate
    axi_dv(i)        <= '1' when tready(i) = '1' and tvalid(i) = '1' else '0';
    cfg_sample_en(i) <= axi_dv(i) and first_word(i);
  end generate;

  constellation(0) <= decode(cfg_constellation);
  frame_type(0)    <= decode(cfg_frame_type);
  code_rate(0)     <= decode(cfg_code_rate);

  tvalid(0)        <= s_tvalid;
  tdata(0)         <= s_tdata;
  tlast(0)         <= s_tlast;
  s_tready         <= tready(0);

  m_tvalid                 <= tvalid(CHAIN_LENGTH - 1);
  m_tdata                  <= tdata(CHAIN_LENGTH - 1);
  m_tlast                  <= tlast(CHAIN_LENGTH - 1);
  tready(CHAIN_LENGTH - 1) <= m_tready;

  ---------------
  -- Processes --
  ---------------
  process(clk, rst)
  begin
    if rst = '1' then
      first_word  <= (others => '1');
    elsif rising_edge(clk) then
      for i in 0 to CHAIN_LENGTH - 1 loop
        if axi_dv(i) = '1' then
          first_word(i) <= tlast(i);
        end if;
      end loop;
    end if;
  end process;


end dvbs2_tx;
