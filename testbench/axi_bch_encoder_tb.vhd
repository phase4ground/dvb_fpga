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

entity axi_bch_encoder_tb is
  generic (
    RUNNER_CFG            : string;
    TEST_CFG              : string;
    NUMBER_OF_TEST_FRAMES : integer := 8);
end axi_bch_encoder_tb;

architecture axi_bch_encoder_tb of axi_bch_encoder_tb is

  ---------------
  -- Constants --
  ---------------
  constant configs           : config_array_t := get_test_cfg(TEST_CFG);

  constant CLK_PERIOD        : time := 5 ns;

  -------------
  -- Signals --
  -------------
  -- Usual ports
  signal clk                : std_logic := '1';
  signal rst                : std_logic;

  signal cfg_frame_type     : frame_type_t;
  signal cfg_code_rate      : code_rate_t;

  -- AXI input
  signal axi_master         : axi_stream_bus_t(tdata(7 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal axi_master_dv      : boolean;
  signal axi_slave          : axi_stream_bus_t(tdata(7 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal axi_slave_dv       : boolean;

  signal tvalid_probability : real range 0.0 to 1.0 := 1.0;
  signal tready_probability : real range 0.0 to 1.0 := 1.0;

  signal expected_tdata     : std_logic_vector(7 downto 0);
  signal expected_tlast     : std_logic;
  signal tdata_error_cnt    : std_logic_vector(7 downto 0);
  signal tlast_error_cnt    : std_logic_vector(7 downto 0);
  signal error_cnt          : std_logic_vector(7 downto 0);

begin

  -------------------
  -- Port mappings --
  -------------------
  axi_file_reader_u : entity fpga_cores_sim.axi_file_reader
    generic map (
      READER_NAME => "axi_file_reader_u",
      DATA_WIDTH  => 8,
      TID_WIDTH   => ENCODED_CONFIG_WIDTH)
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

  dut : entity work.axi_bch_encoder
    generic map ( TID_WIDTH => ENCODED_CONFIG_WIDTH )
    port map (
      -- Usual ports
      clk            => clk,
      rst            => rst,
      -- Config input
      cfg_frame_type => decode(axi_master.tuser).frame_type,
      cfg_code_rate  => decode(axi_master.tuser).code_rate,
      -- AXI input
      s_tvalid       => axi_master.tvalid,
      s_tlast        => axi_master.tlast,
      s_tready       => axi_master.tready,
      s_tdata        => axi_master.tdata,
      s_tid          => axi_master.tuser,
      -- AXI output
      m_tready       => axi_slave.tready,
      m_tvalid       => axi_slave.tvalid,
      m_tlast        => axi_slave.tlast,
      m_tdata        => axi_slave.tdata,
      m_tid          => axi_slave.tuser);

  axi_file_compare_u : entity fpga_cores_sim.axi_file_compare
    generic map (
      READER_NAME     => "axi_file_compare_u",
      ERROR_CNT_WIDTH => 8,
      -- REPORT_SEVERITY => Warning,
      DATA_WIDTH      => 8)
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

  axi_slave_dv  <= axi_slave.tvalid = '1' and axi_slave.tready = '1';
  axi_master_dv <= axi_master.tvalid = '1' and axi_master.tready = '1';

  ---------------
  -- Processes --
  ---------------
  main : process
    constant self         : actor_t       := new_actor("main");
    variable file_reader  : file_reader_t := new_file_reader("axi_file_reader_u");
    variable file_checker : file_reader_t := new_file_reader("axi_file_compare_u");
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
      constant number_of_frames : in positive := NUMBER_OF_TEST_FRAMES) is
      constant data_path        : string := strip(config.base_path, chars => (1 to 1 => nul));
      variable msg              : msg_t;
      constant config_tuple     : config_tuple_t := (code_rate => config.code_rate,
                                                     constellation => config.constellation,
                                                     frame_type => config.frame_type);
    begin

      info("Running test with:");
      info(" - constellation  : " & constellation_t'image(config.constellation));
      info(" - frame_type     : " & frame_type_t'image(config.frame_type));
      info(" - code_rate      : " & code_rate_t'image(config.code_rate));
      info(" - data path      : " & data_path);

      for i in 0 to number_of_frames - 1 loop
        read_file(net,
          file_reader => file_reader,
          filename    => data_path & "/bch_encoder_input.bin",
          ratio       => "1:8",
          tid         => encode(config_tuple));

        read_file(net,
          file_reader => file_checker,
          filename    => data_path & "/ldpc_encoder_input.bin",
          ratio       => "1:8");

        msg := new_msg;
        push(msg, encode(config_tuple));
        send(net, find("tid_check"), msg);
      end loop;

    end procedure run_test;

    ------------------------------------------------------------------------------------
    procedure wait_for_transfers is
      variable msg : msg_t;
    begin
      info("Waiting for all files to be read");
      wait_all_read(net, file_reader);
      wait_all_read(net, file_checker);
      info("File reader and checker completed reading");

      wait until rising_edge(clk) and axi_slave.tvalid = '0' for 1 ms;

      walk(1);
    end procedure wait_for_transfers;
    ------------------------------------------------------------------------------------

  begin

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
          run_test(configs(i));
        end loop;

      elsif run("slow_master") then
        tvalid_probability <= 0.5;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i));
        end loop;

      elsif run("slow_slave") then
        tvalid_probability <= 0.5;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i));
        end loop;

      elsif run("both_slow") then
        tvalid_probability <= 0.75;
        tready_probability <= 0.75;

        for i in configs'range loop
          run_test(configs(i));
        end loop;

      end if;

      wait_for_transfers;
      check_equal(axi_slave.tvalid, '0', "axi_slave.tvalid should be '0'");
      check_equal(error_cnt, 0);

      walk(32);

    end loop;

    check_equal(error_cnt, 0, sformat("Expected 0 errors but got %d", fo(error_cnt)));

    test_runner_cleanup(runner);
    wait;
  end process;

  tid_check_p : process -- {{ ----------------------------------------------------------
    constant self         : actor_t := new_actor("tid_check");
    variable msg          : msg_t;
    variable expected_tid : std_logic_vector(ENCODED_CONFIG_WIDTH - 1 downto 0);
    variable first_word   : boolean;
    variable frame_cnt    : integer := 0;
    variable word_cnt     : integer := 0;
  begin
    first_word := True;
    while true loop
      wait until rising_edge(clk) and axi_slave.tvalid = '1' and axi_slave.tready = '1';
      if first_word then
        check_true(has_message(self), "Expected TID not set");
        receive(net, self, msg);
        expected_tid := pop(msg);
        info(sformat("[%d / %d] Updated expected TID to %r", fo(frame_cnt), fo(word_cnt), fo(expected_tid)));
      end if;

      check_equal(
        axi_slave.tuser,
        expected_tid,
        sformat(
          "[%d / %d] TID check error: got %r, expected %r",
          fo(frame_cnt),
          fo(word_cnt),
          fo(axi_slave.tuser),
          fo(expected_tid)));

      first_word := False;
      word_cnt   := word_cnt + 1;
      if axi_slave.tlast = '1' then
        info(sformat("[%d / %d] Setting first word", fo(frame_cnt), fo(word_cnt)));
        frame_cnt  := frame_cnt + 1;
        word_cnt   := 0;
        first_word := True;
      end if;
    end loop;
  end process; -- }} -------------------------------------------------------------------

end axi_bch_encoder_tb;
