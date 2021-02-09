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
use fpga_cores.axi_pkg.all;
use fpga_cores.common_pkg.all;

use work.dvb_utils_pkg.all;

------------------------
-- Entity declaration --
------------------------
entity dvbs2_tx is
  generic (
    DATA_WIDTH        : positive := 8;
    OUTPUT_DATA_WIDTH : positive := 32
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
    m_tdata           : out std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0));
end dvbs2_tx;

architecture dvbs2_tx of dvbs2_tx is

  ---------------
  -- Constants --
  ---------------

  -----------
  -- Types --
  -----------
  type tdata_array_t is array (natural range <>) of std_logic_vector(DATA_WIDTH - 1 downto 0);

  -------------
  -- Signals --
  -------------
  signal s_tid         : std_logic_vector(ENCODED_CONFIG_WIDTH - 1 downto 0);

  signal mux_sel       : std_logic_vector(1 downto 0);

  signal bb_scrambler_out         : axi_stream_bus_t(tdata(DATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal bch_encoder_out          : axi_stream_bus_t(tdata(DATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal ldpc_encoder_out         : axi_stream_bus_t(tdata(DATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal mux_to_bit_interleaver   : axi_stream_data_bus_t(tdata(DATA_WIDTH - 1 downto 0));
  signal mux_bypass               : axi_stream_data_bus_t(tdata(DATA_WIDTH - 1 downto 0));
  signal bit_interleaver_out      : axi_stream_bus_t(tdata(DATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal constellation_mapper_in  : axi_stream_bus_t(tdata(DATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal constellation_mapper_out : axi_stream_bus_t(tdata(OUTPUT_DATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));

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
      s_tvalid => s_tvalid,
      s_tdata  => s_tdata,
      s_tlast  => s_tlast,
      s_tready => s_tready,
      s_tid    => s_tid,

      -- AXI output
      m_tready => bb_scrambler_out.tready,
      m_tvalid => bb_scrambler_out.tvalid,
      m_tlast  => bb_scrambler_out.tlast,
      m_tdata  => bb_scrambler_out.tdata,
      m_tid    => bb_scrambler_out.tuser);

  bch_encoder_u : entity work.axi_bch_encoder
    generic map (
      TID_WIDTH   => ENCODED_CONFIG_WIDTH
    )
    port map (
      -- Usual ports
      clk            => clk,
      rst            => rst,

      cfg_frame_type => decode(bb_scrambler_out.tuser).frame_type,
      cfg_code_rate  => decode(bb_scrambler_out.tuser).code_rate,

      -- AXI input
      s_tvalid       => bb_scrambler_out.tvalid,
      s_tlast        => bb_scrambler_out.tlast,
      s_tready       => bb_scrambler_out.tready,
      s_tdata        => bb_scrambler_out.tdata,
      s_tid          => bb_scrambler_out.tuser,

      -- AXI output
      m_tready       => bch_encoder_out.tready,
      m_tvalid       => bch_encoder_out.tvalid,
      m_tlast        => bch_encoder_out.tlast,
      m_tdata        => bch_encoder_out.tdata,
      m_tid          => bch_encoder_out.tuser);

  ldpc_encoder_u : entity work.axi_ldpc_encoder
    generic map ( TID_WIDTH   => ENCODED_CONFIG_WIDTH )
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      cfg_frame_type    => decode(bch_encoder_out.tuser).frame_type,
      cfg_code_rate     => decode(bch_encoder_out.tuser).code_rate,
      cfg_constellation => decode(bch_encoder_out.tuser).constellation,

      -- AXI input
      s_tready          => bch_encoder_out.tready,
      s_tvalid          => bch_encoder_out.tvalid,
      s_tlast           => bch_encoder_out.tlast,
      s_tdata           => bch_encoder_out.tdata,
      s_tid             => bch_encoder_out.tuser,

      -- AXI output
      m_tready          => ldpc_encoder_out.tready,
      m_tvalid          => ldpc_encoder_out.tvalid,
      m_tlast           => ldpc_encoder_out.tlast,
      m_tdata           => ldpc_encoder_out.tdata,
      m_tid             => ldpc_encoder_out.tuser);

  -- Bit interleaver is not needed for QPSK
  bit_interleaver_demux_u : entity fpga_cores.axi_stream_demux
    generic map (
      INTERFACES => 2,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      selection_mask => mux_sel,

      s_tvalid       => ldpc_encoder_out.tvalid,
      s_tready       => ldpc_encoder_out.tready,
      s_tdata        => ldpc_encoder_out.tdata,

      m_tvalid(0)    => mux_to_bit_interleaver.tvalid,
      m_tvalid(1)    => mux_bypass.tvalid,

      m_tready(0)    => mux_to_bit_interleaver.tready,
      m_tready(1)    => mux_bypass.tready,

      m_tdata        => open);

  bit_interleaver_u : entity work.axi_bit_interleaver
    generic map (
      TDATA_WIDTH => DATA_WIDTH,
      TID_WIDTH   => ENCODED_CONFIG_WIDTH
    )
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      cfg_frame_type    => decode(ldpc_encoder_out.tuser).frame_type,
      cfg_constellation => decode(ldpc_encoder_out.tuser).constellation,
      cfg_code_rate     => decode(ldpc_encoder_out.tuser).code_rate,

      -- AXI input
      s_tready          => mux_to_bit_interleaver.tready,
      s_tvalid          => mux_to_bit_interleaver.tvalid,
      s_tlast           => ldpc_encoder_out.tlast,
      s_tdata           => ldpc_encoder_out.tdata,
      s_tid             => ldpc_encoder_out.tuser,

      -- AXI output
      m_tready          => bit_interleaver_out.tready,
      m_tvalid          => bit_interleaver_out.tvalid,
      m_tlast           => bit_interleaver_out.tlast,
      m_tdata           => bit_interleaver_out.tdata,
      m_tid             => bit_interleaver_out.tuser);

  pre_constellaion_mapper_arbiter_block : block
    signal tdata_in0 : std_logic_vector(DATA_WIDTH + ENCODED_CONFIG_WIDTH - 1 downto 0);
    signal tdata_in1 : std_logic_vector(DATA_WIDTH + ENCODED_CONFIG_WIDTH - 1 downto 0);
    signal tdata_out : std_logic_vector(DATA_WIDTH + ENCODED_CONFIG_WIDTH - 1 downto 0);
  begin

    tdata_in0 <= ldpc_encoder_out.tuser & ldpc_encoder_out.tdata;
    tdata_in1 <= bit_interleaver_out.tuser & bit_interleaver_out.tdata;

    constellation_mapper_in.tdata <= tdata_out(DATA_WIDTH - 1 downto 0);
    constellation_mapper_in.tuser <= tdata_out(ENCODED_CONFIG_WIDTH + DATA_WIDTH - 1 downto DATA_WIDTH);

    -- Merge LDPC encoder and bit interleaver streams to feed into the constellation mapper
    pre_constellaion_mapper_arbiter_u : entity fpga_cores.axi_stream_arbiter
      generic map (
        MODE            => "ROUND_ROBIN", -- ROUND_ROBIN, INTERLEAVED, ABSOLUTE
        INTERFACES      => 2,
        DATA_WIDTH      => DATA_WIDTH + ENCODED_CONFIG_WIDTH,
        REGISTER_INPUTS => True)
      port map (
        -- Usual ports
        clk              => clk,
        rst              => rst,

        selected         => open,
        selected_encoded => open,

        -- AXI slave input
        s_tvalid(0)      => mux_bypass.tvalid,
        s_tvalid(1)      => bit_interleaver_out.tvalid,

        s_tready(0)      => mux_bypass.tready,
        s_tready(1)      => bit_interleaver_out.tready,

        s_tlast(0)       => ldpc_encoder_out.tlast,
        s_tlast(1)       => bit_interleaver_out.tlast,

        s_tdata(0)       => tdata_in0,
        s_tdata(1)       => tdata_in1,

        -- AXI master output
        m_tvalid         => constellation_mapper_in.tvalid,
        m_tready         => constellation_mapper_in.tready,
        m_tdata          => tdata_out,
        m_tlast          => constellation_mapper_in.tlast);
  end block;

  constellation_mapper_u : entity work.axi_constellation_mapper
    generic map (
      INPUT_DATA_WIDTH  => DATA_WIDTH,
      OUTPUT_DATA_WIDTH => OUTPUT_DATA_WIDTH
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

      cfg_frame_type    => decode(constellation_mapper_in.tuser).frame_type,
      cfg_constellation => decode(constellation_mapper_in.tuser).constellation,
      cfg_code_rate     => decode(constellation_mapper_in.tuser).code_rate,

      -- AXI input
      s_tvalid          => constellation_mapper_in.tvalid,
      s_tlast           => constellation_mapper_in.tlast,
      s_tready          => constellation_mapper_in.tready,
      -- s_tid             constellation_mapper_in.tuser,
      s_tdata           => constellation_mapper_in.tdata,

      -- AXI output
      m_tvalid          => constellation_mapper_out.tvalid,
      m_tlast           => constellation_mapper_out.tlast,
      m_tready          => constellation_mapper_out.tready,
      -- m_tid             constellation_mapper_in.tuser,
      m_tdata           => constellation_mapper_out.tdata);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  s_tid <= encode((frame_type    => decode(cfg_frame_type),
                   constellation => decode(cfg_constellation),
                   code_rate     => decode(cfg_code_rate)));

  mux_sel <= "10" when decode(ldpc_encoder_out.tuser).constellation = mod_qpsk else "01";

  constellation_mapper_out.tready <= m_tready;
  m_tvalid                        <= constellation_mapper_out.tvalid;
  m_tlast                         <= constellation_mapper_out.tlast;
  m_tdata                         <= constellation_mapper_out.tdata;

  ---------------
  -- Processes --
  ---------------
end dvbs2_tx;
