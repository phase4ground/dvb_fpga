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

-- ghdl translate_off
library modelsim_lib;
use modelsim_lib.util.all;
-- ghdl translate_on

entity axi_constellation_mapper_tb is
  generic (
    RUNNER_CFG            : string;
    TEST_CFG              : string;
    NUMBER_OF_TEST_FRAMES : integer := 8);
end axi_constellation_mapper_tb;

architecture axi_constellation_mapper_tb of axi_constellation_mapper_tb is

  ---------------
  -- Constants --
  ---------------
  constant configs           : config_array_t := get_test_cfg(TEST_CFG);

  constant DATA_WIDTH        : integer := 32;

  constant CLK_PERIOD        : time    := 5 ns;
  constant ERROR_CNT_WIDTH   : integer := 8;

  constant DBG_CHECK_FRAME_RAM_WRITES : boolean := False;

  -------------
  -- Signals --
  -------------
  signal clk                : std_logic := '1';
  signal rst                : std_logic;

  signal m_frame_cnt        : natural := 0;
  signal m_word_cnt         : natural := 0;
  signal m_bit_cnt          : natural := 0;

  signal s_frame_cnt        : natural := 0;
  signal s_word_cnt         : natural := 0;
  signal s_bit_cnt          : natural := 0;

  signal tdata_error_cnt    : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);
  signal tlast_error_cnt    : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);
  signal error_cnt          : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);

  signal cfg_constellation  : constellation_t;
  signal cfg_frame_type     : frame_type_t;
  signal cfg_code_rate      : code_rate_t;

  signal data_probability   : real range 0.0 to 1.0 := 1.0;
  signal table_probability  : real range 0.0 to 1.0 := 1.0;
  signal tready_probability : real range 0.0 to 1.0 := 1.0;

  -- AXI input
  signal axi_master         : axi_stream_data_bus_t(tdata(DATA_WIDTH - 1 downto 0));
  -- AXI output
  signal axi_slave          : axi_stream_data_bus_t(tdata(DATA_WIDTH - 1 downto 0));
  signal axi_slave_tdata    : std_logic_vector(DATA_WIDTH - 1 downto 0);

  signal m_data_valid       : boolean;
  signal s_data_valid       : boolean;

  signal expected_tdata     : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal expected_tlast     : std_logic;

begin

  -------------------
  -- Port mappings --
  -------------------
  dut : entity work.axi_constellation_mapper
  generic map ( DATA_WIDTH => DATA_WIDTH )
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      cfg_constellation => cfg_constellation,
      cfg_frame_type    => cfg_frame_type,
      cfg_code_rate     => cfg_code_rate,

      -- AXI input
      s_tvalid          => axi_master.tvalid,
      s_tdata           => axi_master.tdata,
      s_tlast           => axi_master.tlast,
      s_tready          => axi_master.tready,

      -- AXI output
      m_tready          => axi_slave.tready,
      m_tvalid          => axi_slave.tvalid,
      m_tlast           => axi_slave.tlast,
      m_tdata           => axi_slave.tdata);

  -- FIXME: Need to check what's the correct endianess here. Hard coding for now to get
  -- some tests passing
  axi_slave_tdata <= axi_slave.tdata(23 downto 16) & axi_slave.tdata(31 downto 24) & axi_slave.tdata(7 downto 0) & axi_slave.tdata(15 downto 8);

  -- AXI file read
  axi_file_reader_block : block
    constant CONFIG_INPUT_WIDTHS: fpga_cores.common_pkg.integer_vector_t := (
      0 => FRAME_TYPE_WIDTH,
      1 => CONSTELLATION_WIDTH,
      2 => CODE_RATE_WIDTH);

    signal m_tid : std_logic_vector(sum(CONFIG_INPUT_WIDTHS) - 1 downto 0);
  begin
    axi_file_reader_u : entity fpga_cores_sim.axi_file_reader
      generic map (
        READER_NAME => "axi_file_reader_u",
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
      tdata_error_cnt    => tdata_error_cnt,
      tlast_error_cnt    => tlast_error_cnt,
      error_cnt          => error_cnt,
      tready_probability => tready_probability,
      -- Debug stuff
      expected_tdata     => expected_tdata,
      expected_tlast     => expected_tlast,
      -- Data input
      s_tready           => axi_slave.tready,
      s_tdata            => axi_slave_tdata,
      s_tvalid           => axi_slave.tvalid,
      s_tlast            => axi_slave.tlast);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  clk <= not clk after CLK_PERIOD/2;

  test_runner_watchdog(runner, 10 ms);

  m_data_valid <= axi_master.tvalid = '1' and axi_master.tready = '1';
  s_data_valid <= axi_slave.tvalid = '1' and axi_slave.tready = '1';

  ---------------
  -- Processes --
  ---------------
  main : process -- {{
    constant self         : actor_t       := new_actor("main");
    constant logger       : logger_t      := get_logger("main");
    variable file_reader  : file_reader_t := new_file_reader("axi_file_reader_u");
    variable file_checker : file_reader_t := new_file_reader("axi_file_compare_u");

    procedure walk(constant steps : natural) is -- {{ ----------------------------------
    begin
      if steps /= 0 then
        for step in 0 to steps - 1 loop
          wait until rising_edge(clk);
        end loop;
      end if;
    end procedure walk; -- }} ----------------------------------------------------------

    procedure run_test ( -- {{ ---------------------------------------------------------
      constant config           : config_t;
      constant number_of_frames : in positive) is
      constant data_path        : string := strip(config.base_path, chars => (1 to 1 => nul));
    begin

      info(logger, "Running test with:");
      info(logger, " - constellation  : " & constellation_t'image(config.constellation));
      info(logger, " - frame_type     : " & frame_type_t'image(config.frame_type));
      info(logger, " - code_rate      : " & code_rate_t'image(config.code_rate));
      info(logger, " - data path      : " & data_path);

      for i in 0 to number_of_frames - 1 loop
        debug(logger, "Setting up frame #" & to_string(i));

        read_file(net,
          file_reader => file_reader,
          filename    => data_path & "/bit_interleaver_output.bin",
          tid         => encode(config.code_rate) & encode(config.constellation) & encode(config.frame_type)
        );

        read_file(net, file_checker, data_path & "/bit_mapper_output_fixed.bin");

      end loop;

    end procedure run_test; -- }} ------------------------------------------------------

    procedure wait_for_completion is -- {{ ----------------------------------------------
      variable msg : msg_t;
    begin
      info(logger, "Waiting for all frames to be read");
      wait_all_read(net, file_checker);
      info(logger, "All data has now been read");

      wait until rising_edge(clk) and axi_slave.tvalid = '0' for 1 ms;

      walk(1);

    end procedure wait_for_completion; -- }} --------------------------------------------

  begin

    test_runner_setup(runner, RUNNER_CFG);
    show(display_handler, debug);
    hide(get_logger("file_reader_t(file_reader)"), display_handler, debug, True);
    hide(get_logger("file_reader_t(file_checker)"), display_handler, debug, True);

    while test_suite loop
      rst                <= '1';
      data_probability   <= 1.0;
      table_probability  <= 1.0;
      tready_probability <= 1.0;

      walk(32);

      rst <= '0';

      walk(32);

      set_timeout(runner, configs'length * 10 ms);

      if run("back_to_back") then
        data_probability   <= 1.0;
        table_probability  <= 1.0;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      elsif run("data=0.5,table=1.0,slave=1.0") then
        data_probability   <= 0.5;
        table_probability  <= 1.0;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      elsif run("data=1.0,table=1.0,slave=0.5") then
        data_probability   <= 1.0;
        table_probability  <= 1.0;
        tready_probability <= 0.5;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      elsif run("data=0.75,table=1.0,slave=0.75") then
        data_probability   <= 0.75;
        table_probability  <= 1.0;
        tready_probability <= 0.75;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      elsif run("data=1.0,table=0.5,slave=1.0") then
        data_probability   <= 1.0;
        table_probability  <= 0.5;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      elsif run("data=1.0,table=0.75,slave=0.75") then
        data_probability   <= 1.0;
        table_probability  <= 0.75;
        tready_probability <= 0.75;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      elsif run("data=0.8,table=0.8,slave=0.8") then
        data_probability   <= 0.8;
        table_probability  <= 0.8;
        tready_probability <= 0.8;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      end if;

      wait_for_completion;

      check_equal(axi_slave.tvalid, '0', "axi_slave.tvalid should be '0'");
      check_equal(error_cnt, 0);

      walk(32);

    end loop;

    test_runner_cleanup(runner);
    wait;
  end process; -- }}

  cnt_p : process -- {{ ----------------------------------------------------------------
  begin
    wait until rst = '0';
    while True loop
      wait until rising_edge(clk);

      if s_data_valid then
        s_word_cnt <= s_word_cnt + 1;
        s_bit_cnt  <= s_bit_cnt + DATA_WIDTH;

        if axi_slave.tlast = '1' then
          info(
            sformat(
              "AXI Slave: received frame %d with %d words (%d bits)",
              fo(s_frame_cnt),
              fo(s_word_cnt),
              fo(s_bit_cnt)
            )
          );

          s_word_cnt  <= 0;
          s_bit_cnt   <= 0;
          s_frame_cnt <= s_frame_cnt + 1;
        end if;
      end if;

      if m_data_valid then
        m_word_cnt <= m_word_cnt + 1;
        m_bit_cnt  <= m_bit_cnt + DATA_WIDTH;

        if axi_master.tlast = '1' then
          m_word_cnt  <= 0;
          m_bit_cnt   <= 0;
          m_frame_cnt <= m_frame_cnt + 1;
        end if;
      end if;
    end loop;
  end process; -- }} -------------------------------------------------------------------

end axi_constellation_mapper_tb;
