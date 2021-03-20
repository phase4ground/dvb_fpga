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
use ieee.numeric_std.all;

library fpga_cores;
use fpga_cores.common_pkg.all;

use work.dvb_utils_pkg.all;
use work.dvbs2_tx_wrapper_regmap_regs_pkg.all;

------------------------
-- Entity declaration --
------------------------
entity dvbs2_tx is
  generic (
    DATA_WIDTH : positive := 32
  );
  port (
    -- Usual ports
    clk               : in  std_logic;
    rst               : in  std_logic;

    -- AXI4 lite
    --write address channel
    s_axi_awvalid     : in  std_logic;
    s_axi_awready     : out std_logic;
    s_axi_awaddr      : in  std_logic_vector(31 downto 0);
    -- write data channel
    s_axi_wvalid      : in  std_logic;
    s_axi_wready      : out std_logic;
    s_axi_wdata       : in  std_logic_vector(31 downto 0);
    s_axi_wstrb       : in  std_logic_vector(3 downto 0);
    --read address channel
    s_axi_arvalid     : in  std_logic;
    s_axi_arready     : out std_logic;
    s_axi_araddr      : in  std_logic_vector(31 downto 0);
    --read data channel
    s_axi_rvalid      : out std_logic;
    s_axi_rready      : in  std_logic;
    s_axi_rdata       : out std_logic_vector(31 downto 0);
    s_axi_rresp       : out std_logic_vector(1 downto 0);
    --write response channel
    s_axi_bvalid      : out std_logic;
    s_axi_bready      : in  std_logic;
    s_axi_bresp       : out std_logic_vector(1 downto 0);

    -- Per frame config input
    cfg_constellation : in  std_logic_vector(CONSTELLATION_WIDTH - 1 downto 0);
    cfg_frame_type    : in  std_logic_vector(FRAME_TYPE_WIDTH - 1 downto 0);
    cfg_code_rate     : in  std_logic_vector(CODE_RATE_WIDTH - 1 downto 0);
    -- AXI input
    s_tvalid          : in  std_logic;
    s_tdata           : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    s_tkeep           : in  std_logic_vector(DATA_WIDTH/8 - 1 downto 0);
    s_tlast           : in  std_logic;
    s_tready          : out std_logic;
    -- AXI output
    m_tready          : in  std_logic;
    m_tvalid          : out std_logic;
    m_tlast           : out std_logic;
    m_tdata           : out std_logic_vector(DATA_WIDTH - 1 downto 0));
end dvbs2_tx;

architecture dvbs2_tx of dvbs2_tx is

  constant POLYPHASE_FILTER_NUMBER_TAPS : positive := 101;
  constant POLYPHASE_FILTER_RATE_CHANGE : positive := 2;

  -- Need a component to make this work in Yosys
  component polyphase_filter is
    generic (
      NUMBER_TAPS          : integer := 32;
      DATA_IN_WIDTH        : integer := 16;
      DATA_OUT_WIDTH       : integer := 16;
      COEFFICIENT_WIDTH    : integer := 16;
      RATE_CHANGE          : integer := 8;
      DECIMATE_INTERPOLATE : integer := 1);
    port (
      data_in_aclk     : in std_logic;
      data_in_aresetn  : in std_logic;
      data_in_tready   : out std_logic;
      data_in_tdata    : in std_logic_vector(DATA_IN_WIDTH - 1 downto 0);
      data_in_tlast    : in std_logic;
      data_in_tvalid   : in std_logic;

      data_out_aclk    : in std_logic;
      data_out_aresetn : in std_logic;
      data_out_tready  : in std_logic;
      data_out_tdata   : out std_logic_vector(DATA_OUT_WIDTH - 1 downto 0);
      data_out_tlast   : out std_logic;
      data_out_tvalid  : out std_logic;

      coeffs_wren      : in std_logic;
      coeffs_addr      : in std_logic_vector(numbits(NUMBER_TAPS) - 1 downto 0);
      coeffs_wdata     : in std_logic_vector(COEFFICIENT_WIDTH - 1 downto 0));
    end component;

  -----------
  -- Types --
  -----------
  type data_and_config_t is record
    tdata  : std_logic_vector;
    tid    : std_logic_vector(ENCODED_CONFIG_WIDTH - 1 downto 0);
    tvalid : std_logic;
    tlast  : std_logic;
    tready : std_logic;
  end record;

  -- tvalid/tready pair for control only buses
  type axi_stream_ctrl_t is record
    tvalid : std_logic;
    tready : std_logic;
  end record;

  -------------
  -- Signals --
  -------------
  signal rst_n                : std_logic;
  signal s_tid                : std_logic_vector(ENCODED_CONFIG_WIDTH - 1 downto 0);
  signal m_tvalid_i           : std_logic;
  signal m_tlast_i            : std_logic;
  signal s_tready_i           : std_logic;

  signal mux_sel              : std_logic_vector(1 downto 0);

  signal width_conv           : data_and_config_t(tdata(7 downto 0));
  signal bb_scrambler         : data_and_config_t(tdata(7 downto 0));
  signal bch_encoder          : data_and_config_t(tdata(7 downto 0));
  signal ldpc_encoder         : data_and_config_t(tdata(7 downto 0));
  signal ldpc_fifo            : data_and_config_t(tdata(7 downto 0));
  signal fork_bit_interleaver : axi_stream_ctrl_t;
  signal fork_ldpc_fifo       : axi_stream_ctrl_t;
  signal bit_interleaver      : data_and_config_t(tdata(7 downto 0));
  signal arbiter_out          : data_and_config_t(tdata(7 downto 0));
  signal constellation_mapper : data_and_config_t(tdata(DATA_WIDTH - 1 downto 0));
  signal pl_frame             : data_and_config_t(tdata(DATA_WIDTH - 1 downto 0));
  signal polyphase_filter_in  : data_and_config_t(tdata(DATA_WIDTH - 1 downto 0));

  -- User signals  :
  signal user2regs : user2regs_t;
  signal regs2user : regs2user_t;

begin

  -------------------
  -- Port mappings --
  -------------------
  force_8_bit_u : entity fpga_cores.axi_stream_width_converter
    generic map (
      INPUT_DATA_WIDTH  => DATA_WIDTH,
      OUTPUT_DATA_WIDTH => 8,
      AXI_TID_WIDTH     => ENCODED_CONFIG_WIDTH,
      ENDIANNESS        => LEFT_FIRST,
      IGNORE_TKEEP      => False)
    port map (
      -- Usual ports
      clk      => clk,
      rst      => rst,
      -- AXI stream input
      s_tready => s_tready_i,
      s_tdata  => s_tdata,
      s_tkeep  => s_tkeep,
      s_tid    => s_tid,
      s_tvalid => s_tvalid,
      s_tlast  => s_tlast,
      -- AXI stream output
      m_tready => width_conv.tready,
      m_tdata  => width_conv.tdata,
      m_tkeep  => open,
      m_tid    => width_conv.tid,
      m_tvalid => width_conv.tvalid,
      m_tlast  => width_conv.tlast);

  bb_scrambler_u : entity work.axi_baseband_scrambler
    generic map (
      TDATA_WIDTH => 8,
      TID_WIDTH   => ENCODED_CONFIG_WIDTH)
    port map (
      -- Usual ports
      clk      => clk,
      rst      => rst,
      -- AXI input
      s_tvalid => width_conv.tvalid,
      s_tdata  => width_conv.tdata,
      s_tlast  => width_conv.tlast,
      s_tready => width_conv.tready,
      s_tid    => width_conv.tid,
      -- AXI output
      m_tready => bb_scrambler.tready,
      m_tvalid => bb_scrambler.tvalid,
      m_tlast  => bb_scrambler.tlast,
      m_tdata  => bb_scrambler.tdata,
      m_tid    => bb_scrambler.tid);

  bch_encoder_u : entity work.axi_bch_encoder
    generic map (
      TID_WIDTH   => ENCODED_CONFIG_WIDTH
    )
    port map (
      -- Usual ports
      clk            => clk,
      rst            => rst,
      -- Per frame config input
      cfg_frame_type => decode(bb_scrambler.tid).frame_type,
      cfg_code_rate  => decode(bb_scrambler.tid).code_rate,
      -- AXI input
      s_tvalid       => bb_scrambler.tvalid,
      s_tlast        => bb_scrambler.tlast,
      s_tready       => bb_scrambler.tready,
      s_tdata        => bb_scrambler.tdata,
      s_tid          => bb_scrambler.tid,
      -- AXI output
      m_tready       => bch_encoder.tready,
      m_tvalid       => bch_encoder.tvalid,
      m_tlast        => bch_encoder.tlast,
      m_tdata        => bch_encoder.tdata,
      m_tid          => bch_encoder.tid);

  ldpc_encoder_u : entity work.axi_ldpc_encoder
    generic map ( TID_WIDTH   => ENCODED_CONFIG_WIDTH )
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,
      -- Per frame config input
      cfg_frame_type    => decode(bch_encoder.tid).frame_type,
      cfg_code_rate     => decode(bch_encoder.tid).code_rate,
      cfg_constellation => decode(bch_encoder.tid).constellation,
      -- AXI input
      s_tready          => bch_encoder.tready,
      s_tvalid          => bch_encoder.tvalid,
      s_tlast           => bch_encoder.tlast,
      s_tdata           => bch_encoder.tdata,
      s_tid             => bch_encoder.tid,
      -- AXI output
      m_tready          => ldpc_encoder.tready,
      m_tvalid          => ldpc_encoder.tvalid,
      m_tlast           => ldpc_encoder.tlast,
      m_tdata           => ldpc_encoder.tdata,
      m_tid             => ldpc_encoder.tid);

  mux_sel <= "10" when decode(ldpc_encoder.tid).constellation = mod_qpsk else "01";

  -- Bit interleaver is not needed for QPSK
  bit_interleaver_demux_u : entity fpga_cores.axi_stream_demux
    generic map (
      INTERFACES => 2,
      DATA_WIDTH => 8)
    port map (
      selection_mask => mux_sel,

      s_tvalid       => ldpc_encoder.tvalid,
      s_tready       => ldpc_encoder.tready,
      s_tdata        => ldpc_encoder.tdata,

      m_tvalid(0)    => fork_bit_interleaver.tvalid,
      m_tvalid(1)    => fork_ldpc_fifo.tvalid,

      m_tready(0)    => fork_bit_interleaver.tready,
      m_tready(1)    => fork_ldpc_fifo.tready,

      m_tdata        => open);

  bit_interleaver_u : entity work.axi_bit_interleaver
    generic map (
      TDATA_WIDTH => 8,
      TID_WIDTH   => ENCODED_CONFIG_WIDTH
    )
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,
      -- Per frame config input
      cfg_frame_type    => decode(ldpc_encoder.tid).frame_type,
      cfg_constellation => decode(ldpc_encoder.tid).constellation,
      cfg_code_rate     => decode(ldpc_encoder.tid).code_rate,
      -- AXI input
      s_tready          => fork_bit_interleaver.tready,
      s_tvalid          => fork_bit_interleaver.tvalid,
      s_tlast           => ldpc_encoder.tlast,
      s_tdata           => ldpc_encoder.tdata,
      s_tid             => ldpc_encoder.tid,
      -- AXI output
      m_tready          => bit_interleaver.tready,
      m_tvalid          => bit_interleaver.tvalid,
      m_tlast           => bit_interleaver.tlast,
      m_tdata           => bit_interleaver.tdata,
      m_tid             => bit_interleaver.tid);

  ldpc_fifo_block : block
    signal tdata_agg_in  : std_logic_vector(ENCODED_CONFIG_WIDTH + 7 downto 0);
    signal tdata_agg_out : std_logic_vector(ENCODED_CONFIG_WIDTH + 7 downto 0);
  begin
    ldpc_fifo_u : entity fpga_cores.axi_stream_frame_fifo
      generic map (
        FIFO_DEPTH => max(DVB_N_LDPC) / 8,
        DATA_WIDTH => ENCODED_CONFIG_WIDTH + 8,
        RAM_TYPE   => auto)
      port map (
        -- Usual ports
        clk     => clk,
        rst     => rst,
        -- status
        entries  => user2regs.ldpc_fifo_status_ldpc_fifo_entries,
        empty    => user2regs.ldpc_fifo_status_ldpc_fifo_empty(0),
        full     => user2regs.ldpc_fifo_status_ldpc_fifo_full(0),
        -- Write side
        s_tvalid => fork_ldpc_fifo.tvalid,
        s_tready => fork_ldpc_fifo.tready,
        s_tdata  => tdata_agg_in,
        s_tlast  => ldpc_encoder.tlast,
        -- Read side
        m_tvalid => ldpc_fifo.tvalid,
        m_tready => ldpc_fifo.tready,
        m_tdata  => tdata_agg_out,
        m_tlast  => ldpc_fifo.tlast);

    tdata_agg_in    <= ldpc_encoder.tid & ldpc_encoder.tdata;
    ldpc_fifo.tdata <= tdata_agg_out(7 downto 0);
    ldpc_fifo.tid <= tdata_agg_out(ENCODED_CONFIG_WIDTH + 7 downto 8);
  end block;

  pre_constellaion_mapper_arbiter_block : block
    signal tdata_in0 : std_logic_vector(8 + ENCODED_CONFIG_WIDTH - 1 downto 0);
    signal tdata_in1 : std_logic_vector(8 + ENCODED_CONFIG_WIDTH - 1 downto 0);
    signal tdata_out : std_logic_vector(8 + ENCODED_CONFIG_WIDTH - 1 downto 0);
  begin

    tdata_in0 <= ldpc_fifo.tid & ldpc_fifo.tdata;
    tdata_in1 <= bit_interleaver.tid & bit_interleaver.tdata;

    arbiter_out.tdata <= tdata_out(8 - 1 downto 0);
    arbiter_out.tid <= tdata_out(ENCODED_CONFIG_WIDTH + 8 - 1 downto 8);

    -- Merge LDPC encoder and bit interleaver streams to feed into the constellation mapper
    pre_constellaion_mapper_arbiter_u : entity fpga_cores.axi_stream_arbiter
      generic map (
        MODE            => "ROUND_ROBIN", -- ROUND_ROBIN, INTERLEAVED, ABSOLUTE
        INTERFACES      => 2,
        DATA_WIDTH      => 8 + ENCODED_CONFIG_WIDTH,
        REGISTER_INPUTS => True)
      port map (
        -- Usual ports
        clk              => clk,
        rst              => rst,

        selected         => open,
        selected_encoded => open,

        -- AXI slave input
        s_tvalid(0)      => ldpc_fifo.tvalid,
        s_tvalid(1)      => bit_interleaver.tvalid,

        s_tready(0)      => ldpc_fifo.tready,
        s_tready(1)      => bit_interleaver.tready,

        s_tlast(0)       => ldpc_fifo.tlast,
        s_tlast(1)       => bit_interleaver.tlast,

        s_tdata(0)       => tdata_in0,
        s_tdata(1)       => tdata_in1,

        -- AXI master output
        m_tvalid         => arbiter_out.tvalid,
        m_tready         => arbiter_out.tready,
        m_tdata          => tdata_out,
        m_tlast          => arbiter_out.tlast);
  end block;

  constellation_mapper_u : entity work.axi_constellation_mapper
    generic map (
      INPUT_DATA_WIDTH  => 8,
      OUTPUT_DATA_WIDTH => DATA_WIDTH,
      TID_WIDTH         => ENCODED_CONFIG_WIDTH
    )
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,
      -- Mapping RAM config
      ram_wren          => or(regs2user.bit_mapper_ram_wen),
      ram_addr          => regs2user.bit_mapper_ram_addr(5 downto 0), -- register map addresses bytes
      ram_wdata         => regs2user.bit_mapper_ram_wdata,
      ram_rdata         => user2regs.bit_mapper_ram_rdata,
      -- Per frame config input
      cfg_constellation => decode(arbiter_out.tid).constellation,
      -- AXI input
      s_tvalid          => arbiter_out.tvalid,
      s_tlast           => arbiter_out.tlast,
      s_tready          => arbiter_out.tready,
      s_tdata           => arbiter_out.tdata,
      s_tid             => arbiter_out.tid,
      -- AXI output
      m_tvalid          => constellation_mapper.tvalid,
      m_tlast           => constellation_mapper.tlast,
      m_tready          => constellation_mapper.tready,
      m_tdata           => constellation_mapper.tdata,
      m_tid             => constellation_mapper.tid);

  physical_layer_framer_u : entity work.axi_physical_layer_framer
    generic map (
      TDATA_WIDTH => DATA_WIDTH,
      TID_WIDTH   => ENCODED_CONFIG_WIDTH)
    port map (
      -- Usual ports
      clk                 => clk,
      rst                 => rst,
      -- Static config
      cfg_enable_dummy_frames => regs2user.config_enable_dummy_frames(0),
      -- Per frame config
      cfg_constellation   => decode(constellation_mapper.tid).constellation,
      cfg_frame_type      => decode(constellation_mapper.tid).frame_type,
      cfg_code_rate       => decode(constellation_mapper.tid).code_rate,
      -- AXI input
      s_tvalid            => constellation_mapper.tvalid,
      s_tlast             => constellation_mapper.tlast,
      s_tready            => constellation_mapper.tready,
      s_tdata             => constellation_mapper.tdata,
      s_tid               => constellation_mapper.tid,
      -- AXI output
      m_tready            => pl_frame.tready,
      m_tvalid            => pl_frame.tvalid,
      m_tlast             => pl_frame.tlast,
      m_tdata             => pl_frame.tdata,
      m_tid               => pl_frame.tid);

  plframe_delay_block : block
    signal tdata_agg_in  : std_logic_vector(ENCODED_CONFIG_WIDTH + DATA_WIDTH downto 0);
    signal tdata_agg_out : std_logic_vector(ENCODED_CONFIG_WIDTH + DATA_WIDTH downto 0);
  begin
    input_delay_u : entity fpga_cores.axi_stream_delay
      generic map (
        DELAY_CYCLES => 1,
        TDATA_WIDTH  => tdata_agg_in'length)
      port map (
        -- Usual ports
        clk     => clk,
        rst     => rst,

        -- AXI slave input
        s_tvalid => pl_frame.tvalid,
        s_tready => pl_frame.tready,
        s_tdata  => tdata_agg_in,

        -- AXI master output
        m_tvalid => polyphase_filter_in.tvalid,
        m_tready => polyphase_filter_in.tready,
        m_tdata  => tdata_agg_out);

      tdata_agg_in              <= pl_frame.tlast & pl_frame.tid & pl_frame.tdata;
      polyphase_filter_in.tdata <= tdata_agg_out(DATA_WIDTH - 1 downto 0);
      polyphase_filter_in.tid   <= tdata_agg_out(ENCODED_CONFIG_WIDTH + DATA_WIDTH - 1 downto DATA_WIDTH);
      polyphase_filter_in.tlast <= tdata_agg_out(ENCODED_CONFIG_WIDTH + DATA_WIDTH);

    end block;


  polyphase_filter_q : polyphase_filter
    generic map (
      NUMBER_TAPS          => POLYPHASE_FILTER_NUMBER_TAPS,
      DATA_IN_WIDTH        => DATA_WIDTH/2,
      DATA_OUT_WIDTH       => DATA_WIDTH/2,
      COEFFICIENT_WIDTH    => DATA_WIDTH/2,
      RATE_CHANGE          => POLYPHASE_FILTER_RATE_CHANGE,
      DECIMATE_INTERPOLATE => 1) -- 0 => decimate, 1 => interpolate
    port map (
      -- input data interface
      data_in_aclk            => clk,
      data_in_aresetn         => rst_n,
      data_in_tready          => polyphase_filter_in.tready,
      data_in_tdata           => polyphase_filter_in.tdata(DATA_WIDTH/2 - 1 downto 0),
      data_in_tlast           => polyphase_filter_in.tlast ,
      data_in_tvalid          => polyphase_filter_in.tvalid,
      -- output data interface
      data_out_aclk           => clk,
      data_out_aresetn        => rst_n,
      data_out_tready         => m_tready,
      data_out_tdata          => m_tdata(DATA_WIDTH/2 - 1 downto 0),
      data_out_tlast          => m_tlast_i,
      data_out_tvalid         => m_tvalid_i,
      -- coefficients input interface
      coeffs_wren             => or(regs2user.polyphase_filter_coefficients_wen),
      coeffs_addr             => regs2user.polyphase_filter_coefficients_addr(6 downto 0),
      coeffs_wdata            => regs2user.polyphase_filter_coefficients_wdata(DATA_WIDTH/2 - 1 downto 0));

  polyphase_filter_i : polyphase_filter
    generic map (
      NUMBER_TAPS          => POLYPHASE_FILTER_NUMBER_TAPS,
      DATA_IN_WIDTH        => DATA_WIDTH/2,
      DATA_OUT_WIDTH       => DATA_WIDTH/2,
      COEFFICIENT_WIDTH    => DATA_WIDTH/2,
      RATE_CHANGE          => POLYPHASE_FILTER_RATE_CHANGE,
      DECIMATE_INTERPOLATE => 1) -- 0 => decimate, 1 => interpolate
    port map (
      -- input data interface
      data_in_aclk            => clk,
      data_in_aresetn         => rst_n,
      data_in_tready          => open,
      data_in_tdata           => polyphase_filter_in.tdata(DATA_WIDTH - 1 downto DATA_WIDTH/2),
      data_in_tlast           => polyphase_filter_in.tlast ,
      data_in_tvalid          => polyphase_filter_in.tvalid,
      -- output data interface
      data_out_aclk           => clk,
      data_out_aresetn        => rst_n,
      data_out_tready         => m_tready,
      data_out_tdata          => m_tdata(DATA_WIDTH - 1 downto DATA_WIDTH/2),
      data_out_tlast          => open,
      data_out_tvalid         => open,
      -- coefficients input interface
      coeffs_wren             => or(regs2user.polyphase_filter_coefficients_wen),
      coeffs_addr             => regs2user.polyphase_filter_coefficients_addr(6 downto 0),
      coeffs_wdata            => regs2user.polyphase_filter_coefficients_wdata(DATA_WIDTH - 1 downto DATA_WIDTH/2));

  -- Register map decoder
  regmap_u : entity work.dvbs2_tx_wrapper_regmap_regs
    generic map (
      AXI_ADDR_WIDTH => 32,
      BASEADDR       => (others => '0'))
    port map (
      -- Clock and Reset
      axi_aclk    => clk,
      axi_aresetn => rst_n,
      -- AXI Write Address Channel
      s_axi_awaddr  => s_axi_awaddr,
      s_axi_awprot  => (others => '0'),
      s_axi_awvalid => s_axi_awvalid,
      s_axi_awready => s_axi_awready,
      -- AXI Write Data Channel
      s_axi_wdata   => s_axi_wdata,
      s_axi_wstrb   => s_axi_wstrb,
      s_axi_wvalid  => s_axi_wvalid,
      s_axi_wready  => s_axi_wready,
      -- AXI Read Address Channel
      s_axi_araddr  => s_axi_araddr,
      s_axi_arprot  => (others => '0'),
      s_axi_arvalid => s_axi_arvalid,
      s_axi_arready => s_axi_arready,
      -- AXI Read Data Channel
      s_axi_rdata   => s_axi_rdata,
      s_axi_rresp   => s_axi_rresp,
      s_axi_rvalid  => s_axi_rvalid,
      s_axi_rready  => s_axi_rready,
      -- AXI Write Response Channel
      s_axi_bresp   => s_axi_bresp,
      s_axi_bvalid  => s_axi_bvalid,
      s_axi_bready  => s_axi_bready,
      -- User Ports
      user2regs     => user2regs,
      regs2user     => regs2user);


  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  rst_n <= not rst;

  s_tid <= encode((frame_type    => decode(cfg_frame_type),
                   constellation => decode(cfg_constellation),
                   code_rate     => decode(cfg_code_rate)));

  m_tvalid <= m_tvalid_i;
  m_tlast  <= m_tlast_i;
  s_tready <= s_tready_i;

  ---------------
  -- Processes --
  ---------------
  status_block : block
    signal s_first_word       : std_logic;
    signal frame_in_transit_i : unsigned(7 downto 0);
  begin
    user2regs.frames_in_transit_value <= std_logic_vector(frame_in_transit_i);

    process(clk, rst)
    begin
      if rst = '1' then
        frame_in_transit_i <= (others => '0');
        s_first_word       <= '1';
      elsif rising_edge(clk) then
        if s_tvalid and s_tready_i then
          s_first_word <= s_tlast;
        end if;

        -- Only increment if only one is active
        if (s_tvalid and s_tready_i and s_first_word) xor (m_tvalid_i and m_tready and m_tlast_i) then
          if (s_tvalid and s_tready_i and s_first_word) then
            frame_in_transit_i <= frame_in_transit_i + 1;
          end if;
          if (m_tvalid_i and m_tready and m_tlast_i) then
            frame_in_transit_i <= frame_in_transit_i - 1;
          end if;
        end if;
      end if;
    end process;
  end block;

end dvbs2_tx;
