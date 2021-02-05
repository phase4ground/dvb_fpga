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

library fpga_cores;
use fpga_cores.common_pkg.all;

use work.dvb_utils_pkg.all;

------------------------
-- Entity declaration --
------------------------
entity dvbs2_tx is
  generic (
    DATA_WIDTH        : positive := 8;
    OUTPUT_DATA_WIDTH : positive := 8
  );
  port (
    -- Usual ports
    clk               : in  std_logic;
    rst               : in  std_logic;

    cfg_constellation : in  std_logic_vector(CONSTELLATION_WIDTH - 1 downto 0);
    cfg_frame_type    : in  std_logic_vector(FRAME_TYPE_WIDTH - 1 downto 0);
    cfg_code_rate     : in  std_logic_vector(CODE_RATE_WIDTH - 1 downto 0);

    -- Mapping RAM config
    ram_wren          : in  std_logic;
    ram_addr          : in  std_logic_vector(5 downto 0);
    ram_wdata         : in  std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);
    ram_rdata         : out std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);

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
  constant CHAIN_LENGTH         : positive := 7;

  -----------
  -- Types --
  -----------
  type tdata_array_t is array (natural range <>) of std_logic_vector(DATA_WIDTH - 1 downto 0);

  -------------
  -- Signals --
  -------------
  signal config        : std_logic_array_t(CHAIN_LENGTH - 1 downto 0)(ENCODED_CONFIG_WIDTH - 1 downto 0);
  signal frame_type    : frame_type_array_t(CHAIN_LENGTH - 1 downto 0);
  signal constellation : constellation_array_t(CHAIN_LENGTH - 1 downto 0);
  signal code_rate     : code_rate_array_t(CHAIN_LENGTH - 1 downto 0);

  signal tdata         : tdata_array_t(CHAIN_LENGTH - 1 downto 0);
  signal tvalid        : std_logic_vector(CHAIN_LENGTH - 1 downto 0);
  signal tready        : std_logic_vector(CHAIN_LENGTH - 1 downto 0);
  signal tlast         : std_logic_vector(CHAIN_LENGTH - 1 downto 0);

  signal mux_sel       : std_logic_vector(1 downto 0);

begin

  -------------------
  -- Port mappings --
  -------------------
  bb_scrambler_u : entity work.axi_baseband_scrambler
    generic map (
      TDATA_WIDTH => DATA_WIDTH,
      TID_WIDTH   => ENCODED_CONFIG_WIDTH)
    port map (
      -- Usual ports
      clk      => clk,
      rst      => rst,

      -- AXI input
      s_tvalid => tvalid(0),
      s_tdata  => tdata(0),
      s_tlast  => tlast(0),
      s_tready => tready(0),
      s_tid    => config(0),

      -- AXI output
      m_tready => tready(1),
      m_tvalid => tvalid(1),
      m_tlast  => tlast(1),
      m_tdata  => tdata(1),
      m_tid    => config(1));

  bch_encoder_u : entity work.axi_bch_encoder
    generic map (
      TDATA_WIDTH => DATA_WIDTH,
      TID_WIDTH   => ENCODED_CONFIG_WIDTH)
    port map (
      -- Usual ports
      clk            => clk,
      rst            => rst,

      cfg_frame_type => decode(config(1)).frame_type,
      cfg_code_rate  => decode(config(1)).code_rate,

      -- AXI input
      s_tvalid       => tvalid(1),
      s_tdata        => tdata(1),
      s_tlast        => tlast(1),
      s_tready       => tready(1),
      s_tid          => config(1),

      -- AXI output
      m_tready       => tready(2),
      m_tvalid       => tvalid(2),
      m_tlast        => tlast(2),
      m_tdata        => tdata(2),
      m_tid          => config(2));

  ldpc_encoder_u : entity work.axi_ldpc_encoder
    generic map ( TID_WIDTH   => ENCODED_CONFIG_WIDTH )
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      cfg_frame_type    => decode(config(2)).frame_type,
      cfg_code_rate     => decode(config(2)).code_rate,
      cfg_constellation => decode(config(2)).constellation,

      -- AXI input
      s_tvalid          => tvalid(2),
      s_tlast           => tlast(2),
      s_tready          => tready(2),
      s_tdata           => tdata(2),
      s_tid             => config(2),

      -- AXI output
      m_tready          => tready(3),
      m_tvalid          => tvalid(3),
      m_tlast           => tlast(3),
      m_tdata           => tdata(3),
      m_tid             => config(3));

  
  -- Bit interleaver is not needed for QPSK
  bit_interleaver_mux_u : entity fpga_cores.axi_stream_demux
    generic map (
      INTERFACES => 2,
      DATA_WIDTH => 0)
    port map (
      selection_mask => mux_sel,

      s_tvalid       => tvalid(3),
      s_tready       => tready(3),

      m_tvalid(0)    => tvalid(4),
      m_tvalid(1)    => tvalid(5),

      m_tready(0)    => tready(4),
      m_tready(1)    => tready(5));

  bit_interleaver_u : entity work.axi_bit_interleaver
    generic map (
      TDATA_WIDTH => DATA_WIDTH,
      TID_WIDTH   => ENCODED_CONFIG_WIDTH
    )
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      cfg_frame_type    => decode(config(3)).frame_type,
      cfg_constellation => decode(config(3)).constellation,
      cfg_code_rate     => decode(config(3)).code_rate,

      -- AXI input
      s_tvalid          => tvalid(4),
      s_tlast           => tlast(3),
      s_tready          => tready(4),
      s_tdata           => tdata(3),
      s_tid             => config(3),

      -- AXI output
      m_tready          => tready(5),
      m_tvalid          => tvalid(5),
      m_tlast           => tlast(5),
      m_tdata           => tdata(5),
      m_tid             => config(5));

  pre_constellaion_mapper_arbiter_block : block
    signal tdata_in0 : std_logic_vector(DATA_WIDTH + ENCODED_CONFIG_WIDTH - 1 downto 0);
    signal tdata_in1 : std_logic_vector(DATA_WIDTH + ENCODED_CONFIG_WIDTH - 1 downto 0);
    signal tdata_out : std_logic_vector(DATA_WIDTH + ENCODED_CONFIG_WIDTH - 1 downto 0);
  begin

    tdata_in0 <= config(4) & tdata(4);
    tdata_in1 <= config(5) & tdata(5);

    tdata(6)  <= tdata_out(DATA_WIDTH - 1 downto 0);
    config(6) <= tdata_out(ENCODED_CONFIG_WIDTH + DATA_WIDTH - 1 downto DATA_WIDTH);

    -- Merge LDPC encoder and bit interleaver streams to feed into the constellation mapper
    pre_constellaion_mapper_arbiter_u : entity fpga_cores.axi_stream_arbiter
      generic map (
        MODE       => "ROUND_ROBIN", -- ROUND_ROBIN, INTERLEAVED, ABSOLUTE
        INTERFACES => 2,
        DATA_WIDTH => DATA_WIDTH + ENCODED_CONFIG_WIDTH)
      port map (
        -- Usual ports
        clk              => clk,
        rst              => rst,

        selected         => open,
        selected_encoded => open,

        -- AXI slave input
        s_tvalid         => (0 => tvalid(4), 1 => tvalid(5)),
        s_tready(0)      => tready(0),
        s_tready(1)      => tready(1),
        s_tlast          => (0 => tlast(4), 1 => tlast(5)),
        s_tdata          => (0 => tdata_in0, 1 => tdata_in1),

        -- AXI master output
        m_tvalid         => tvalid(6),
        m_tready         => tready(6),
        m_tdata          => tdata_out,
        m_tlast          => tlast(6));
  end block;

  constellation_mapper_u : entity work.axi_constellation_mapper
    generic map (
      INPUT_DATA_WIDTH  => DATA_WIDTH,
      OUTPUT_DATA_WIDTH => 32
    )
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      -- Mapping RAM config
      ram_wren          => ram_wren,
      ram_addr          => ram_addr,
      ram_wdata         => ram_wdata,
      ram_rdata         => ram_rdata,

      cfg_frame_type    => decode(config(6)).frame_type,
      cfg_constellation => decode(config(6)).constellation,
      cfg_code_rate     => decode(config(6)).code_rate,

      -- AXI input
      s_tvalid          => tvalid(6),
      s_tlast           => tlast(6),
      s_tready          => tready(6),
      s_tdata           => tdata(6),
      -- s_tid             => config(6),

      -- AXI output
      m_tready          => tready(6),
      m_tvalid          => tvalid(6),
      m_tlast           => tlast(6),
      -- m_tid             => config(6),
      m_tdata           => tdata(6));

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  constellation(0) <= decode(cfg_constellation);
  frame_type(0)    <= decode(cfg_frame_type);
  code_rate(0)     <= decode(cfg_code_rate);

  config(0) <= encode((frame_type    => decode(cfg_frame_type), 
                       constellation => decode(cfg_constellation),
                       code_rate     => decode(cfg_code_rate)));

  tvalid(0)        <= s_tvalid;
  tdata(0)         <= s_tdata;
  tlast(0)         <= s_tlast;
  s_tready         <= tready(0);

  m_tvalid                 <= tvalid(CHAIN_LENGTH - 1);
  m_tdata                  <= tdata(CHAIN_LENGTH - 1);
  m_tlast                  <= tlast(CHAIN_LENGTH - 1);
  tready(CHAIN_LENGTH - 1) <= m_tready;

  mux_sel <= "01" when decode(config(3)).constellation = mod_qpsk else "10";

  ---------------
  -- Processes --
  ---------------


end dvbs2_tx;
