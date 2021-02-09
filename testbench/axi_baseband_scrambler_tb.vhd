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

use std.textio.all;

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

library fpga_cores_sim;
context fpga_cores_sim.sim_context;

use work.dvb_utils_pkg.all;
use work.dvb_sim_utils_pkg.all;

entity axi_baseband_scrambler_tb is
  generic (
    RUNNER_CFG            : string;
    TEST_CFG              : string;
    TDATA_WIDTH           : integer := 8;
    NUMBER_OF_TEST_FRAMES : integer := 8);
end axi_baseband_scrambler_tb;

architecture axi_baseband_scrambler_tb of axi_baseband_scrambler_tb is

  ---------------
  -- Constants --
  ---------------
  constant configs         : config_array_t := get_test_cfg(TEST_CFG);

  constant CLK_PERIOD      : time := 5 ns;
  constant TID_WIDTH       : integer := 8;

  -------------
  -- Signals --
  -------------
  -- Usual ports
  signal clk                : std_logic := '1';
  signal rst                : std_logic;

  signal tvalid_probability : real range 0.0 to 1.0 := 1.0;
  signal tready_probability : real range 0.0 to 1.0 := 1.0;

  -- AXI input
  signal axi_master         : axi_stream_bus_t(tdata(TDATA_WIDTH - 1 downto 0), tuser(TID_WIDTH - 1 downto 0));
  signal m_data_valid       : boolean;

  signal axi_slave          : axi_stream_bus_t(tdata(TDATA_WIDTH - 1 downto 0), tuser(TID_WIDTH - 1 downto 0));
  signal s_data_valid       : boolean;

  signal expected_tdata     : std_logic_vector(TDATA_WIDTH - 1 downto 0);
  signal expected_tlast     : std_logic;
  signal tdata_error_cnt    : std_logic_vector(7 downto 0);
  signal tlast_error_cnt    : std_logic_vector(7 downto 0);
  signal error_cnt          : std_logic_vector(7 downto 0);

begin

  -------------------
  -- Port mappings --
  -------------------
  -- AXI file read
  axi_file_reader_u : entity fpga_cores_sim.axi_file_reader
    generic map (
      READER_NAME => "axi_file_reader_u",
      DATA_WIDTH  => TDATA_WIDTH,
      TID_WIDTH   => TID_WIDTH)
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,
      -- Config and status
      completed          => open,
      tvalid_probability => tvalid_probability,
      -- Data output
      m_tready           => axi_master.tready,
      m_tdata            => axi_master.tdata,
      m_tid              => axi_master.tuser,
      m_tvalid           => axi_master.tvalid,
      m_tlast            => axi_master.tlast);

  dut : entity work.axi_baseband_scrambler
    generic map (
      TDATA_WIDTH => TDATA_WIDTH,
      TID_WIDTH   => TID_WIDTH)
    port map (
      -- Usual ports
      clk           => clk,
      rst           => rst,
      -- AXI input
      s_tvalid      => axi_master.tvalid,
      s_tlast       => axi_master.tlast,
      s_tready      => axi_master.tready,
      s_tdata       => axi_master.tdata,
      s_tid         => axi_master.tuser,
      -- AXI output
      m_tready      => axi_slave.tready,
      m_tvalid      => axi_slave.tvalid,
      m_tlast       => axi_slave.tlast,
      m_tdata       => axi_slave.tdata,
      m_tid         => axi_slave.tuser);

  axi_file_compare_u : entity fpga_cores_sim.axi_file_compare
    generic map (
      READER_NAME     => "axi_file_compare_u",
      ERROR_CNT_WIDTH => 8,
      REPORT_SEVERITY => Warning,
      DATA_WIDTH      => TDATA_WIDTH)
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,
      -- Config and status
      tdata_error_cnt    => tdata_error_cnt,
      tlast_error_cnt    => tlast_error_cnt,
      error_cnt          => error_cnt,
      tready_probability => tready_probability,
      -- Debug stuff
      expected_tdata     => expected_tdata,
      expected_tlast     => expected_tlast,
      -- Data input
      s_tready           => axi_slave.tready,
      s_tdata            => axi_slave.tdata,
      s_tvalid           => axi_slave.tvalid,
      s_tlast            => axi_slave.tlast);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  clk <= not clk after CLK_PERIOD/2;

  test_runner_watchdog(runner, 3 ms);

  m_data_valid <= axi_slave.tvalid = '1' and axi_slave.tready = '1';
  s_data_valid <= axi_master.tvalid = '1' and axi_master.tready = '1';

  ---------------
  -- Processes --
  ---------------
  main : process
    constant self           : actor_t := new_actor("main");
    variable file_reader    : file_reader_t := new_file_reader("axi_file_reader_u");
    variable file_checker   : file_reader_t := new_file_reader("axi_file_compare_u");
    variable tid_rand_gen   : RandomPType;
    ------------------------------------------------------------------------------------
    procedure walk(constant steps : natural) is
    begin
      if steps /= 0 then
        for step in 0 to steps - 1 loop
          wait until rising_edge(clk);
        end loop;
      end if;
    end procedure walk;

    ------------------------------------------------------------------------------------
    procedure run_test (
      constant config           : config_t;
      constant number_of_frames : in positive) is
      constant data_path        : string := strip(config.base_path, chars => (1 to 1 => nul));
    begin

      info("Running test with:");
      info(" - data path      : " & data_path);

      for i in 0 to number_of_frames - 1 loop
        read_file(net, file_reader, data_path & "/bb_scrambler_input.bin", "1:8", tid_rand_gen.RandSlv(TID_WIDTH));
        read_file(net, file_checker, data_path & "/bch_encoder_input.bin", "1:8");
      end loop;

    end procedure run_test;

    ------------------------------------------------------------------------------------
    procedure wait_for_transfers is
    begin
      wait_all_read(net, file_reader);
      wait_all_read(net, file_checker);
    end procedure wait_for_transfers;
    ------------------------------------------------------------------------------------

  begin

    tid_rand_gen.InitSeed("seed");
    test_runner_setup(runner, RUNNER_CFG);
    show(display_handler, debug);

    while test_suite loop
      rst <= '1';
      walk(4);
      rst <= '0';
      walk(4);

      tvalid_probability <= 1.0;
      tready_probability <= 1.0;

      set_timeout(runner, configs'length * NUMBER_OF_TEST_FRAMES * 500 us);

      if run("back_to_back") then
        tvalid_probability <= 1.0;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

        wait_for_transfers;

      elsif run("slow_master") then
        tvalid_probability <= 0.5;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;
        wait_for_transfers;

      elsif run("slow_slave") then
        tvalid_probability <= 1.0;
        tready_probability <= 0.5;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;
        wait_for_transfers;

      elsif run("both_slow") then
        tvalid_probability <= 0.75;
        tready_probability <= 0.75;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;
        wait_for_transfers;

      end if;

      walk(8);
      check_equal(error_cnt, 0, sformat("Expected 0 errors but got %d", fo(error_cnt)));
      walk(32);

    end loop;

    test_runner_cleanup(runner);
    wait;
  end process;

  tid_check_p : process -- {{ ----------------------------------------------------------
    variable tid_rand_check : RandomPType;
    variable expected_tid   : std_logic_vector(TID_WIDTH - 1 downto 0);
    variable first_word     : boolean;
    variable frame_cnt    : integer := 0;
    variable word_cnt     : integer := 0;
  begin
    tid_rand_check.InitSeed("seed");
    first_word := True;
    while true loop
      wait until rising_edge(clk) and axi_slave.tvalid = '1' and axi_slave.tready = '1';
      if first_word then
        expected_tid := tid_rand_check.RandSlv(TID_WIDTH);
        info(sformat("[%d / %d] Updated expected TID to %r", fo(frame_cnt), fo(word_cnt), fo(expected_tid)));
      end if;

      check_equal(axi_slave.tuser, expected_tid, "TID check failed");
      check_equal(
        axi_slave.tuser,
        expected_tid,
        sformat(
          "[%d / %d] TID check error: got %r, expected %r",
          fo(frame_cnt),
          fo(word_cnt),
          fo(axi_slave.tuser),
          fo(expected_tid)));

      word_cnt   := word_cnt + 1;
      first_word := False;
      if axi_slave.tlast = '1' then
        info(sformat("[%d / %d] Setting first word", fo(frame_cnt), fo(word_cnt)));
        frame_cnt  := frame_cnt + 1;
        word_cnt   := 0;
        first_word := True;
      end if;
    end loop;
  end process; -- }} -------------------------------------------------------------------

end axi_baseband_scrambler_tb;
