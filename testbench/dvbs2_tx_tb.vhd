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

-- vunit: run_all_in_same_sim

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library vunit_lib;
context vunit_lib.vunit_context;
context vunit_lib.com_context;

library osvvm;
use osvvm.RandomPkg.all;

library str_format;
use str_format.str_format_pkg.all;

library fpga_cores;
use fpga_cores.axi_pkg.all;
use fpga_cores.common_pkg.all;

library fpga_cores_sim;
use fpga_cores_sim.testbench_utils_pkg.all;
use fpga_cores_sim.file_utils_pkg.all;

use work.dvb_utils_pkg.all;
use work.dvb_sim_utils_pkg.all;

-- ghdl translate_off
library modelsim_lib;
use modelsim_lib.util.all;
-- ghdl translate_on

entity dvbs2_tx_tb is
  generic (
    RUNNER_CFG            : string;
    TEST_CFG              : string := "";
    NUMBER_OF_TEST_FRAMES : integer := 1);
end dvbs2_tx_tb;

architecture dvbs2_tx_tb of dvbs2_tx_tb is

  ---------------
  -- Constants --
  ---------------
  constant configs           : config_array_t := get_test_cfg(TEST_CFG);
  constant DATA_WIDTH        : integer := 8;

  constant BB_SCRAMBLERS_CHECKER_NAME : string  := "bb_scrambler";
  constant BCH_ENCODER_CHECKER_NAME   : string  := "bch_encoder";
  constant LDPC_ENCODER_CHECKER_NAME  : string  := "ldpc_encoder";

  constant CLK_PERIOD        : time := 5 ns;
  constant ERROR_CNT_WIDTH   : integer := 8;

  function get_checker_data_ratio ( constant constellation : in constellation_t)
  return string is
  begin
    case constellation is
      when   mod_8psk => return "3:8";
      when mod_16apsk => return "4:8";
      when mod_32apsk => return "5:8";
      when others =>
        report "Invalid constellation: " & constellation_t'image(constellation)
        severity Failure;
    end case;

    -- Just to avoid the warning, should never be reached
    return "";
  end;

  type axi_checker_t is record
    axi             : axi_stream_data_bus_t;
    tdata_error_cnt : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);
    tlast_error_cnt : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);
    error_cnt       : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);
    expected_tdata  : std_logic_vector(DATA_WIDTH - 1 downto 0);
    expected_tlast  : std_logic;
  end record;

  -------------
  -- Signals --
  -------------
  -- Usual ports
  signal clk                : std_logic := '1';
  signal rst                : std_logic;

  signal cfg_constellation  : constellation_t;
  signal cfg_frame_type     : frame_type_t;
  signal cfg_code_rate      : code_rate_t;

  signal data_probability   : real range 0.0 to 1.0 := 1.0;
  signal table_probability  : real range 0.0 to 1.0 := 1.0;
  signal tready_probability : real range 0.0 to 1.0 := 1.0;

  -- AXI input
  signal axi_master         : axi_stream_data_bus_t(tdata(DATA_WIDTH - 1 downto 0));
  signal m_data_valid       : std_logic;

  -- AXI LDPC table input
  signal axi_ldpc           : axi_stream_data_bus_t(tdata(2*numbits(max(DVB_N_LDPC)) + 8 - 1 downto 0));

  -- AXI output
  signal s_data_valid       : std_logic;

  signal axi_bb_scrambler   : axi_checker_t(axi(tdata(DATA_WIDTH - 1 downto 0)));
  signal axi_bch_encoder    : axi_checker_t(axi(tdata(DATA_WIDTH - 1 downto 0)));
  signal axi_ldpc_encoder_core   : axi_checker_t(axi(tdata(DATA_WIDTH - 1 downto 0)));
  signal axi_slave          : axi_checker_t(axi(tdata(DATA_WIDTH - 1 downto 0)));

begin

  -------------------
  -- Port mappings --
  -------------------
  dut : entity work.dvbs2_tx
    generic map ( DATA_WIDTH => DATA_WIDTH )
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      cfg_constellation => encode(cfg_constellation),
      cfg_frame_type    => encode(cfg_frame_type),
      cfg_code_rate     => encode(cfg_code_rate),

      -- Mapping RAM config
      ram_wren          => '0',
      ram_addr          => (others => 'U'),
      ram_wdata         => (others => 'U'),
      ram_rdata         => open,

      -- AXI input
      s_tvalid          => axi_master.tvalid,
      s_tdata           => axi_master.tdata,
      s_tlast           => axi_master.tlast,
      s_tready          => axi_master.tready,

      -- AXI output
      m_tready          => axi_slave.axi.tready,
      m_tvalid          => axi_slave.axi.tvalid,
      m_tlast           => axi_slave.axi.tlast,
      m_tdata           => axi_slave.axi.tdata);


  -- AXI file read
  axi_table_u : entity fpga_cores_sim.axi_file_reader
    generic map (
      READER_NAME => "axi_table_u",
      DATA_WIDTH  => axi_ldpc.tdata'length)
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,
      -- Config and status
      completed          => open,
      tvalid_probability => table_probability,
      -- AXI stream output
      m_tready           => axi_ldpc.tready,
      m_tdata            => axi_ldpc.tdata,
      m_tvalid           => axi_ldpc.tvalid,
      m_tlast            => axi_ldpc.tlast);

  -- AXI file read
  axi_file_reader_block : block
    constant CONFIG_INPUT_WIDTHS: fpga_cores.common_pkg.integer_vector_t := (
      0 => FRAME_TYPE_WIDTH,
      1 => CONSTELLATION_WIDTH,
      2 => CODE_RATE_WIDTH);

    signal m_tid : std_logic_vector(sum(CONFIG_INPUT_WIDTHS) - 1 downto 0);
  begin
    input_stream_u : entity fpga_cores_sim.axi_file_reader
      generic map (
        READER_NAME => "input_stream_u",
        DATA_WIDTH  => DATA_WIDTH,
        TID_WIDTH   => sum(CONFIG_INPUT_WIDTHS))
      port map (
        -- Usual ports
        clk                => clk,
        rst                => rst,
        -- Config and status
        completed          => open,
        tvalid_probability => data_probability,

        -- Data output
        m_tready           => axi_master.tready,
        m_tdata            => axi_master.tdata,
        m_tid              => m_tid,
        m_tvalid           => axi_master.tvalid,
        m_tlast            => axi_master.tlast);

    -- Decode the TID field with the actual config types
    cfg_frame_type    <= decode(get_field(m_tid, 0, CONFIG_INPUT_WIDTHS));
    cfg_constellation <= decode(get_field(m_tid, 1, CONFIG_INPUT_WIDTHS));
    cfg_code_rate     <= decode(get_field(m_tid, 2, CONFIG_INPUT_WIDTHS));
  end block axi_file_reader_block;


  bb_scrambler_checker_u : entity fpga_cores_sim.axi_file_compare
    generic map (
      READER_NAME     => BB_SCRAMBLERS_CHECKER_NAME,
      ERROR_CNT_WIDTH => ERROR_CNT_WIDTH,
      REPORT_SEVERITY => Error,
      DATA_WIDTH      => DATA_WIDTH)
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,
      -- Config and status
      tdata_error_cnt    => axi_bb_scrambler.tdata_error_cnt,
      tlast_error_cnt    => axi_bb_scrambler.tlast_error_cnt,
      error_cnt          => axi_bb_scrambler.error_cnt,
      tready_probability => 1.0,
      -- Debug stuff
      expected_tdata     => axi_bb_scrambler.expected_tdata,
      expected_tlast     => axi_bb_scrambler.expected_tlast,
      -- Data input
      s_tready           => open,
      s_tdata            => axi_bb_scrambler.axi.tdata,
      s_tvalid           => axi_bb_scrambler.axi.tvalid and axi_bb_scrambler.axi.tready,
      s_tlast            => axi_bb_scrambler.axi.tlast);

  bch_encoder_checker_u : entity fpga_cores_sim.axi_file_compare
    generic map (
      READER_NAME     => BCH_ENCODER_CHECKER_NAME,
      ERROR_CNT_WIDTH => ERROR_CNT_WIDTH,
      REPORT_SEVERITY => Error,
      DATA_WIDTH      => DATA_WIDTH)
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,
      -- Config and status
      tdata_error_cnt    => axi_bch_encoder.tdata_error_cnt,
      tlast_error_cnt    => axi_bch_encoder.tlast_error_cnt,
      error_cnt          => axi_bch_encoder.error_cnt,
      tready_probability => 1.0,
      -- Debug stuff
      expected_tdata     => axi_bch_encoder.expected_tdata,
      expected_tlast     => axi_bch_encoder.expected_tlast,
      -- Data input
      s_tready           => open,
      s_tdata            => axi_bch_encoder.axi.tdata,
      s_tvalid           => axi_bch_encoder.axi.tvalid and axi_bch_encoder.axi.tready,
      s_tlast            => axi_bch_encoder.axi.tlast);

  ldpc_encoder_checker_u : entity fpga_cores_sim.axi_file_compare
    generic map (
      READER_NAME     => LDPC_ENCODER_CHECKER_NAME,
      ERROR_CNT_WIDTH => ERROR_CNT_WIDTH,
      REPORT_SEVERITY => Error,
      DATA_WIDTH      => DATA_WIDTH)
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,
      -- Config and status
      tdata_error_cnt    => axi_ldpc_encoder_core.tdata_error_cnt,
      tlast_error_cnt    => axi_ldpc_encoder_core.tlast_error_cnt,
      error_cnt          => axi_ldpc_encoder_core.error_cnt,
      tready_probability => 1.0,
      -- Debug stuff
      expected_tdata     => axi_ldpc_encoder_core.expected_tdata,
      expected_tlast     => axi_ldpc_encoder_core.expected_tlast,
      -- Data input
      s_tready           => open,
      s_tdata            => axi_ldpc_encoder_core.axi.tdata,
      s_tvalid           => axi_ldpc_encoder_core.axi.tvalid and axi_ldpc_encoder_core.axi.tready,
      s_tlast            => axi_ldpc_encoder_core.axi.tlast);

  axi_file_compare_u : entity fpga_cores_sim.axi_file_compare
    generic map (
      READER_NAME     => "axi_file_compare_u",
      ERROR_CNT_WIDTH => ERROR_CNT_WIDTH,
      REPORT_SEVERITY => Error,
      DATA_WIDTH      => DATA_WIDTH)
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,
      -- Config and status
      tdata_error_cnt    => axi_slave.tdata_error_cnt,
      tlast_error_cnt    => axi_slave.tlast_error_cnt,
      error_cnt          => axi_slave.error_cnt,
      tready_probability => tready_probability,
      -- Debug stuff
      expected_tdata     => axi_slave.expected_tdata,
      expected_tlast     => axi_slave.expected_tlast,
      -- Data input
      s_tready           => axi_slave.axi.tready,
      s_tdata            => axi_slave.axi.tdata,
      s_tvalid           => axi_slave.axi.tvalid,
      s_tlast            => axi_slave.axi.tlast);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  clk <= not clk after CLK_PERIOD/2;

  test_runner_watchdog(runner, 300 ms);

  m_data_valid <= axi_master.tvalid and axi_master.tready;
  s_data_valid <= axi_slave.axi.tvalid and axi_slave.axi.tready;

  ---------------
  -- Processes --
  ---------------
  main : process -- {{ -----------------------------------------------------------------
    constant self                    : actor_t       := new_actor("main");
    variable file_reader             : file_reader_t := new_file_reader("input_stream_u");
    variable file_checker            : file_reader_t := new_file_reader("axi_file_compare_u");
    variable ldpc_table              : file_reader_t := new_file_reader("axi_table_u");

    variable bb_scrambler_checker    : file_reader_t := new_file_reader(BB_SCRAMBLERS_CHECKER_NAME);
    variable bch_encoder_checker     : file_reader_t := new_file_reader(BCH_ENCODER_CHECKER_NAME);
    variable ldpc_encoder_checker    : file_reader_t := new_file_reader(LDPC_ENCODER_CHECKER_NAME);

    procedure walk(constant steps : natural) is -- {{ ----------------------------------
    begin
      if steps /= 0 then
        for step in 0 to steps - 1 loop
          wait until rising_edge(clk);
        end loop;
      end if;
    end procedure walk; -- }} ----------------------------------------------------------

    procedure wait_for_completion is -- {{ ---------------------------------------------
      variable msg : msg_t;
    begin
      info("Waiting for test completion");
      wait_all_read(net, file_reader);
      wait_all_read(net, file_checker);

      wait until rising_edge(clk) and axi_slave.axi.tvalid = '0' for 1 ms;

      walk(1);
    end procedure wait_for_completion; -- }} -------------------------------------------

    procedure run_test ( -- {{ ---------------------------------------------------------
      constant config           : config_t;
      constant number_of_frames : in positive) is
      variable file_reader_msg  : msg_t;
      constant data_path        : string := strip(config.base_path, chars => (1 to 1 => nul));
    begin

      info("Running test with:");
      info(" - constellation  : " & constellation_t'image(config.constellation));
      info(" - frame_type     : " & frame_type_t'image(config.frame_type));
      info(" - code_rate      : " & code_rate_t'image(config.code_rate));
      info(" - data path      : " & data_path);

      for i in 0 to number_of_frames - 1 loop
        file_reader_msg := new_msg;
        file_reader_msg.sender := self;

        read_file(net,
          file_reader => file_reader,
          filename    => data_path & "/bb_scrambler_input.bin",
          ratio       => "1:8",
          tid         => encode(config.code_rate) & encode(config.constellation) & encode(config.frame_type)
        );

        read_file(
          net,
          file_checker,
          data_path & "/bit_interleaver_output.bin",
          get_checker_data_ratio(config.constellation)
        );

        read_file(net, ldpc_table, data_path & "/ldpc_table.bin");

        read_file(net, bb_scrambler_checker, data_path & "/bch_encoder_input.bin", "1:8");
        read_file(net, bch_encoder_checker, data_path & "/ldpc_encoder_input.bin", "1:8");
        read_file(net, ldpc_encoder_checker, data_path & "/bit_interleaver_input.bin", "1:8");

      end loop;

    end procedure run_test; -- }} ------------------------------------------------------

  begin

    test_runner_setup(runner, RUNNER_CFG);
    show(display_handler, debug);
    hide(get_logger("file_reader_t(input_stream_u)"), display_handler, debug, True);
    hide(get_logger("file_reader_t(axi_file_compare_u)"), display_handler, debug, True);
    hide(get_logger("file_reader_t(axi_table_u)"), display_handler, debug, True);

    while test_suite loop
      rst <= '1';
      walk(4);
      rst <= '0';
      walk(4);

      data_probability <= 1.0;
      tready_probability <= 1.0;

      if run("back_to_back") then
        data_probability <= 1.0;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      end if;

      wait_for_completion;

      check_equal(axi_slave.error_cnt, 0, sformat("Expected 0 errors but got %d", fo(axi_slave.error_cnt)));

      check_equal(axi_slave.axi.tvalid, '0', "axi_slave.axi.tvalid should be '0'");
      check_equal(axi_slave.error_cnt, 0);

      walk(32);

    end loop;

    test_runner_cleanup(runner);
    wait;
  end process; -- }} -------------------------------------------------------------------

-- ghdl translate_off
  signal_spy_block : block -- {{ -------------------------------------------------------
    type tdata_array_t is array (natural range <>) of std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal tdata         : tdata_array_t(4 downto 0);
    signal tvalid        : std_logic_vector(4 downto 0);
    signal tready        : std_logic_vector(4 downto 0);
    signal tlast         : std_logic_vector(4 downto 0);
  begin

    axi_bb_scrambler.axi.tdata     <= tdata(1);
    axi_bb_scrambler.axi.tvalid    <= tvalid(1);
    axi_bb_scrambler.axi.tready    <= tready(1);
    axi_bb_scrambler.axi.tlast     <= tlast(1);

    axi_bch_encoder.axi.tdata      <= tdata(2);
    axi_bch_encoder.axi.tvalid     <= tvalid(2);
    axi_bch_encoder.axi.tready     <= tready(2);
    axi_bch_encoder.axi.tlast      <= tlast(2);

    axi_ldpc_encoder_core.axi.tdata     <= tdata(3);
    axi_ldpc_encoder_core.axi.tvalid    <= tvalid(3);
    axi_ldpc_encoder_core.axi.tready    <= tready(3);
    axi_ldpc_encoder_core.axi.tlast     <= tlast(3);

    process
    begin
      init_signal_spy("/dvbs2_tx_tb/dut/tdata",  "/dvbs2_tx_tb/signal_spy_block/tdata",  0);
      init_signal_spy("/dvbs2_tx_tb/dut/tvalid", "/dvbs2_tx_tb/signal_spy_block/tvalid", 0);
      init_signal_spy("/dvbs2_tx_tb/dut/tlast",  "/dvbs2_tx_tb/signal_spy_block/tlast",  0);
      init_signal_spy("/dvbs2_tx_tb/dut/tready", "/dvbs2_tx_tb/signal_spy_block/tready", 0);
      wait;
    end process;

    check_no_axi_undefined_p: process(clk)
      function get_stage_name ( constant index : natural range 1 to 4 ) return string is
      begin
        case index is
          when 1 => return "baseband scrambler";
          when 2 => return "BCH encoder";
          when 3 => return "LDPC encoder";
          when others => null;
        end case;

        return "bit interleaver";

      end;

    begin
      if rising_edge(clk) then
        for i in 1 to 4 loop

          if tvalid(i) = '1' and tready(i) = '1' and tdata(i) /= to_01(tdata(i)) then
            warning(sformat("'%s' (%d) has undefined AXI data", get_stage_name(i), fo(i)));
          end if;

        end loop;
      end if;
    end process;
  end block signal_spy_block; -- }} ----------------------------------------------------
-- ghdl translate_on

  report_rx : process -- {{ ------------------------------------------------------------
    constant logger    : logger_t   := get_logger("axi_slave report");
    variable word_cnt  : natural := 0;
    variable frame_cnt : natural := 0;
  begin
    wait until s_data_valid = '1' and rising_edge(clk);
    word_cnt := word_cnt + 1;
    if axi_slave.axi.tlast = '1' then
      info(logger, sformat("Received frame %d with %d words", fo(frame_cnt), fo(word_cnt)));
      frame_cnt := frame_cnt + 1;
      word_cnt  := 0;
    end if;
  end process; -- }} -------------------------------------------------------------------

end dvbs2_tx_tb;
