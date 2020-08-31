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
use fpga_cores.common_pkg.all;

library fpga_cores_sim;
use fpga_cores_sim.axi_stream_bfm_pkg.all;
use fpga_cores_sim.file_utils_pkg.all;
use fpga_cores_sim.testbench_utils_pkg.all;

use work.dvb_sim_utils_pkg.all;
use work.dvb_utils_pkg.all;
use work.ldpc_pkg.all;

-- ghdl translate_off
library modelsim_lib;
use modelsim_lib.util.all;
-- ghdl translate_on

entity constellation_mapper_tb is
  generic (
    RUNNER_CFG            : string;
    TEST_CFG              : string;
    NUMBER_OF_TEST_FRAMES : integer := 8);
end constellation_mapper_tb;

architecture constellation_mapper_tb of constellation_mapper_tb is

  constant DATA_WIDTH            : integer := 32;

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
  ---------------
  -- Constants --
  ---------------
  constant configs           : config_array_t := get_test_cfg(TEST_CFG);
  constant CLK_PERIOD        : time    := 5 ns;

  -------------
  -- Signals --
  -------------
  -- Usual ports
  signal clk                : std_logic := '1';
  signal rst                : std_logic;

  signal cfg_constellation  : constellation_t;
  signal cfg_frame_type     : frame_type_t;
  signal cfg_code_rate      : code_rate_t;

  signal tvalid_probability : real range 0.0 to 1.0 := 1.0;
  signal tready_probability : real range 0.0 to 1.0 := 1.0;

  signal axi_slave          : axi_stream_data_bus_t(tdata(DATA_WIDTH - 1 downto 0));

  -- AXI input
  signal axi_master         : axi_stream_data_bus_t(tdata(DATA_WIDTH - 1 downto 0));
  -- AXI output
  signal m_data_valid       : boolean;
  signal s_data_valid       : boolean;

begin

  -------------------
  -- Port mappings --
  -------------------
  -- AXI file read
  axi_file_reader_u : entity fpga_cores_sim.axi_file_reader
    generic map (
      READER_NAME => "axi_file_reader_u",
      DATA_WIDTH  => DATA_WIDTH)
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
      m_tvalid           => axi_master.tvalid,
      m_tlast            => axi_master.tlast);

  dut : entity work.constellation_mapper
  generic map (DATA_WIDTH => DATA_WIDTH) -- Data width must be even number; I and Q will be DATA_WIDTH/2 wide
  port map (
    -- Usual ports
    clk                => clk,
    rst                => rst,

    cfg_constellation  => cfg_constellation,
    cfg_frame_type     => cfg_frame_type,
    cfg_code_rate      => cfg_code_rate,

    -- AXI input
    s_tvalid           => axi_master.tvalid,
    s_tdata            => axi_master.tdata,
    s_tlast            => axi_master.tlast,
    s_tready           => axi_master.tready,

    -- AXI output
    m_tready           => axi_slave.tready,
    m_tvalid           => axi_slave.tvalid,
    m_tlast            => axi_slave.tlast,
    m_tdata            => axi_slave.tdata);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  clk <= not clk after CLK_PERIOD/2;

  test_runner_watchdog(runner, 2 ms);

  m_data_valid <= axi_master.tvalid = '1' and axi_master.tready = '1';
  s_data_valid <= axi_slave.tvalid = '1' and axi_slave.tready = '1';

  axi_slave.tready <= '1';

  ---------------
  -- Processes --
  ---------------
  main : process
    constant self         : actor_t := new_actor("main");
    constant logger       : logger_t := get_logger("main");
    constant input_cfg_p  : actor_t := find("input_cfg_p");
    variable ldpc_table   : file_reader_t := new_file_reader("ldpc_table_u");

    procedure walk(constant steps : natural) is  ----------------------------------
    begin
      if steps /= 0 then
        for step in 0 to steps - 1 loop
          wait until rising_edge(clk);
        end loop;
      end if;
    end procedure walk;  ----------------------------------------------------------

    procedure wait_for_completion is  ----------------------------------------------
      variable msg : msg_t;
    begin
      info("Waiting for completion");
      receive(net, self, msg);
      -- wait_all_read(net, file_checker);

      walk(4);
      wait until rising_edge(clk) and axi_slave.tvalid = '0' for 1 ms;
      check_equal(axi_slave.tvalid, '0', "axi_slave.tvalid should be '0'");
      walk(1);
    end procedure wait_for_completion;  --------------------------------------------

    procedure run_test (  ----------------------------------------------------------
      constant config           : config_t;
      constant number_of_frames : in positive) is
      constant data_path        : string := strip(config.base_path, chars => (1 to 1 => nul));
      variable msg              : msg_t;
    begin

      info("Running test with:");
      info(" - constellation  : " & constellation_t'image(config.constellation));
      info(" - frame_type     : " & frame_type_t'image(config.frame_type));
      info(" - code_rate      : " & code_rate_t'image(config.code_rate));
      info(" - data path      : " & data_path);

      for i in 0 to number_of_frames - 1 loop
        debug(logger, "Setting up frame #" & to_string(i));
        msg        := new_msg;
        msg.sender := self;

        push(msg, config.constellation);
        push(msg, config.frame_type);
        push(msg, config.code_rate);
        push(msg, data_path & "/bit_interleaver_output.bin");

        send(net, input_cfg_p, msg);

      end loop;

    end procedure run_test;  -------------------------------------------------------

  begin

    test_runner_setup(runner, RUNNER_CFG);
    show(display_handler, debug);
    -- hide(get_logger("file_reader_t(file_reader)"), display_handler, debug, True);
    -- hide(get_logger("file_reader_t(file_checker)"), display_handler, debug, True);
    -- hide(get_logger("file_reader_t(file_reader)"), display_handler, debug, True);


    while test_suite loop
      rst                <= '1';
      tready_probability <= 1.0;

      walk(32);
      rst <= '0';
      set_timeout(runner, 1 us);

      if run("back_to_back") then
        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      -- elsif run("slow_master") then
      --   tready_probability <= 1.0;

      --   for i in configs'range loop
      --     for frame in 0 to NUMBER_OF_TEST_FRAMES - 1 loop
      --       run_test(configs(i), number_of_frames => 1);
      --       wait_for_completion;
      --     end loop;
      --   end loop;

      -- elsif run("slow_slave") then
      --   tready_probability <= 0.5;

      --   for i in configs'range loop
      --     run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
      --   end loop;

      -- elsif run("slow_master,slow_slave") then
      --   tready_probability <= 0.75;

      --   for i in configs'range loop
      --     for frame in 0 to NUMBER_OF_TEST_FRAMES - 1 loop
      --       run_test(configs(i), number_of_frames => 1);
      --       wait_for_completion;
      --     end loop;
      --   end loop;
      end if;

      wait_for_completion;
      check_false(has_message(input_cfg_p));
      -- check_equal(error_cnt, 0);

      walk(32);

    end loop;

    test_runner_cleanup(runner);
    wait;
  end process;

  input_cfg_p : process
    constant logger      : logger_t := get_logger("input_cfg_p");
    constant self        : actor_t := new_actor("input_cfg_p");
    constant main        : actor_t := find("main");
    variable cfg_msg     : msg_t;
    variable file_reader : file_reader_t := new_file_reader("axi_file_reader_u");

    variable constellation  : constellation_t;
    variable frame_type     : frame_type_t;
    variable code_rate      : code_rate_t;

  begin

    receive(net, self, cfg_msg);

    constellation := pop(cfg_msg);
    frame_type    := pop(cfg_msg);
    code_rate     := pop(cfg_msg);

    -- Configure the file reader
    read_file(net, file_reader, pop(cfg_msg), get_checker_data_ratio(constellation));

    wait until rising_edge(clk);

    -- Keep the config stuff active for a single cycle to make sure blocks use the correct
    -- values
    cfg_constellation <= constellation;
    cfg_frame_type    <= frame_type;
    cfg_code_rate     <= code_rate;
    wait until m_data_valid and axi_master.tlast = '0' and rising_edge(clk);
    cfg_constellation <= not_set;
    cfg_frame_type    <= not_set;
    cfg_code_rate     <= not_set;

    wait until m_data_valid and axi_master.tlast = '1';

    -- When this is received, the file reader has finished reading the file
    wait_file_read(net, file_reader);

    -- If there's no more messages, notify the main process that we're done here
    if not has_message(self) then
      cfg_msg := new_msg;
      push(cfg_msg, True);
      cfg_msg.sender := self;
      debug(logger, "Notifying main process that there is no more data");
      send(net, main, cfg_msg);
    end if;
  end process;


end constellation_mapper_tb;
