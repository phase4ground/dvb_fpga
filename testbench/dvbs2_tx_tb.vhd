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

use work.dvb_utils_pkg.all;
use work.testbench_utils_pkg.all;
use work.file_utils_pkg.all;

entity dvbs2_tx_tb is
  generic (
    RUNNER_CFG            : string;
    TEST_CFG              : string := "";
    DATA_WIDTH            : integer := 8;
    NUMBER_OF_TEST_FRAMES : integer := 8);
end dvbs2_tx_tb;

architecture dvbs2_tx_tb of dvbs2_tx_tb is

  ---------------
  -- Constants --
  ---------------
  constant configs               : config_array_t := get_test_cfg(TEST_CFG);

  constant FILE_READER_NAME      : string := "file_reader";
  constant FILE_CHECKER_NAME     : string := "file_checker";
  constant CLK_PERIOD            : time := 5 ns;
  constant ERROR_CNT_WIDTH       : integer := 8;

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

  -- AXI input
  signal m_tready           : std_logic;
  signal m_tvalid           : std_logic;
  signal m_tdata            : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal m_tlast            : std_logic;
  signal m_data_valid       : boolean;

  -- AXI output
  signal s_tvalid           : std_logic;
  signal s_tdata            : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal s_tlast            : std_logic;
  signal s_tready           : std_logic;
  signal s_data_valid       : boolean;

  signal expected_tdata     : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal expected_tlast     : std_logic;
  signal tdata_error_cnt    : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);
  signal tlast_error_cnt    : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);
  signal error_cnt          : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);

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

      -- AXI input
      s_tvalid          => m_tvalid,
      s_tdata           => m_tdata,
      s_tlast           => m_tlast,
      s_tready          => m_tready,

      -- AXI output
      m_tready          => s_tready,
      m_tvalid          => s_tvalid,
      m_tlast           => s_tlast,
      m_tdata           => s_tdata);


  -- AXI file read
  axi_file_reader_u : entity work.axi_file_reader
    generic map (
      READER_NAME => FILE_READER_NAME,
      DATA_WIDTH  => DATA_WIDTH)
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,
      -- Config and status
      completed          => open,
      tvalid_probability => tvalid_probability,

      -- Data output
      m_tready           => m_tready,
      m_tdata            => m_tdata,
      m_tvalid           => m_tvalid,
      m_tlast            => m_tlast);

  axi_file_compare_u : entity work.axi_file_compare
    generic map (
      READER_NAME     => FILE_CHECKER_NAME,
      ERROR_CNT_WIDTH => ERROR_CNT_WIDTH,
      REPORT_SEVERITY => Warning,
      DATA_WIDTH      => DATA_WIDTH)
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
      s_tready           => s_tready,
      s_tdata            => s_tdata,
      s_tvalid           => s_tvalid,
      s_tlast            => s_tlast);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  clk <= not clk after CLK_PERIOD/2;

  test_runner_watchdog(runner, 3 ms);

  m_data_valid <= m_tvalid = '1' and m_tready = '1';
  s_data_valid <= s_tvalid = '1' and s_tready = '1';

  ---------------
  -- Processes --
  ---------------
  main : process
    constant self         : actor_t := new_actor("main");
    constant input_cfg_p  : actor_t := find("input_cfg_p");
    variable file_checker : file_reader_t := new_file_reader(FILE_CHECKER_NAME);
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
      variable file_reader_msg  : msg_t;
    begin

      info("Running test with:");
      info(" - constellation  : " & constellation_t'image(config.constellation));
      info(" - frame_type     : " & frame_type_t'image(config.frame_type));
      info(" - code_rate      : " & code_rate_t'image(config.code_rate));
      info(" - input_file     : " & config.files.input);
      info(" - reference_file : " & config.files.reference);

      for i in 0 to number_of_frames - 1 loop
        file_reader_msg := new_msg;
        file_reader_msg.sender := self;

        push(file_reader_msg, config.files.input);
        push(file_reader_msg, config.constellation);
        push(file_reader_msg, config.frame_type);
        push(file_reader_msg, config.code_rate);

        send(net, input_cfg_p, file_reader_msg);
        enqueue_file(
          net,
          file_checker,
          config.files.reference,
          get_checker_data_ratio(config.constellation)
        );

      end loop;

    end procedure run_test;

    ------------------------------------------------------------------------------------
    procedure wait_for_transfers ( constant count : in natural) is
      variable msg : msg_t;
    begin
      -- Will get one response for each frame from the file checker and one for the input
      -- config. The order shouldn't matter
      receive(net, self, msg);
      -- Failure(sformat("Got reply from '%s'", name(msg.sender)));

      wait_all_read(net, file_checker);
    end procedure wait_for_transfers;
    ------------------------------------------------------------------------------------

  begin

    test_runner_setup(runner, RUNNER_CFG);
    show(display_handler, debug);

    -- while test_suite loop
    --   rst <= '1';
    --   walk(4);
    --   rst <= '0';
    --   walk(4);

    --   tvalid_probability <= 1.0;
    --   tready_probability <= 1.0;

    --   set_timeout(runner, configs'length * NUMBER_OF_TEST_FRAMES * 4 ms / DATA_WIDTH);

    --   if run("back_to_back") then
    --     tvalid_probability <= 1.0;
    --     tready_probability <= 1.0;

    --     for i in configs'range loop
    --       run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
    --     end loop;

    --     wait_for_transfers(configs'length);

    --   end if;

    --   walk(1);

    --   check_false(has_message(input_cfg_p));

    --   check_equal(error_cnt, 0);

    --   walk(32);

    -- end loop;

    test_runner_cleanup(runner);
    wait;
  end process;

  input_cfg_p : process
    constant self        : actor_t := new_actor("input_cfg_p");
    constant main        : actor_t := find("main");
    variable cfg_msg     : msg_t;
    variable file_reader : file_reader_t := new_file_reader(FILE_READER_NAME);
  begin

    receive(net, self, cfg_msg);

    -- Configure the file reader
    enqueue_file(net, file_reader, pop(cfg_msg), "1:8");

    wait until rising_edge(clk);

    -- Keep the config stuff active for a single cycle to make sure blocks use the correct
    -- values
    cfg_constellation <= pop(cfg_msg);
    cfg_frame_type    <= pop(cfg_msg);
    cfg_code_rate     <= pop(cfg_msg);
    wait until m_data_valid and m_tlast = '0' and rising_edge(clk);
    cfg_constellation <= not_set;
    cfg_frame_type    <= not_set;
    cfg_code_rate     <= not_set;

    wait until m_data_valid and m_tlast = '1';

    -- When this is received, the file reader has finished reading the file
    wait_file_read(net, file_reader);

    -- If there's no more messages, notify the main process that we're done here
    if not has_message(self) then
      cfg_msg := new_msg;
      push(cfg_msg, True);
      cfg_msg.sender := self;
      send(net, main, cfg_msg);
    end if;
    check_equal(error_cnt, 0);
  end process;

  process
  begin
    wait until rising_edge(clk);
    if rst = '0' then
      check_equal(error_cnt, 0, sformat("Expected 0 errors but got %d", fo(error_cnt)));
    end if;
  end process;

end dvbs2_tx_tb;
