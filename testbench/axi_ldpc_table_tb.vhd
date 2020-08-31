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

entity axi_ldpc_table_tb is
  generic (
    RUNNER_CFG            : string;
    TEST_CFG              : string;
    NUMBER_OF_TEST_FRAMES : integer := 8);
end axi_ldpc_table_tb;

architecture axi_ldpc_table_tb of axi_ldpc_table_tb is

  ---------------
  -- Constants --
  ---------------
  constant configs           : config_array_t := get_test_cfg(TEST_CFG);

  constant DATA_WIDTH        : integer := 8;

  constant FILE_CHECKER_NAME : string  := "file_checker";
  constant CLK_PERIOD        : time    := 5 ns;
  constant ERROR_CNT_WIDTH   : integer := 8;

  constant CONFIG_INPUT_WIDTHS: fpga_cores.common_pkg.integer_vector_t := (
    0 => FRAME_TYPE_WIDTH,
    1 => CONSTELLATION_WIDTH,
    2 => CODE_RATE_WIDTH);

  -------------
  -- Signals --
  -------------
  -- Usual ports
  signal clk                : std_logic := '1';
  signal rst                : std_logic;

  signal tdata_error_cnt    : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);
  signal tlast_error_cnt    : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);
  signal error_cnt          : std_logic_vector(ERROR_CNT_WIDTH - 1 downto 0);

  signal m_constellation    : constellation_t;
  signal m_frame_type       : frame_type_t;
  signal m_code_rate        : code_rate_t;
  signal m_tvalid           : std_logic;
  signal m_tready           : std_logic;

  signal bfm_tdata          : std_logic_vector(sum(CONFIG_INPUT_WIDTHS) - 1 downto 0);

  signal tready_probability : real range 0.0 to 1.0 := 1.0;

  signal axi_slave          : axi_stream_data_bus_t(tdata(2*numbits(max(DVB_N_LDPC)) - 0 downto 0));

  -- AXI input
  signal axi_master         : axi_stream_data_bus_t(tdata(DATA_WIDTH - 1 downto 0));
  -- AXI output
  signal m_data_valid       : boolean;
  signal s_data_valid       : boolean;

  signal expected_tdata     : std_logic_vector(axi_slave.tdata'length - 1 downto 0);

  signal axi_slave_offset   : std_logic_vector(numbits(max(DVB_N_LDPC)) - 1 downto 0);
  signal axi_slave_next     : std_logic;
  signal axi_slave_tuser    : std_logic_vector(numbits(max(DVB_N_LDPC)) - 1 downto 0);

  signal expected_offset    : std_logic_vector(numbits(max(DVB_N_LDPC)) - 1 downto 0);
  signal expected_next      : std_logic;
  signal expected_tuser     : std_logic_vector(numbits(max(DVB_N_LDPC)) - 1 downto 0);

  signal expected_tlast     : std_logic;

begin

  -------------------
  -- Port mappings --
  -------------------
  -- AXI stream BFM for the config input
  axi_config_input_u : entity fpga_cores_sim.axi_stream_bfm
    generic map (
      NAME        => "axi_config_input_u",
      TDATA_WIDTH => sum(CONFIG_INPUT_WIDTHS))
    port map (
      -- Usual ports
      clk      => clk,
      rst      => rst,
      -- AXI stream output
      m_tready => m_tready,
      m_tdata  => bfm_tdata,
      m_tuser  => open,
      m_tkeep  => open,
      m_tid    => open,
      m_tvalid => m_tvalid,
      m_tlast  => open);

  m_frame_type    <= decode(get_field(bfm_tdata, 0, CONFIG_INPUT_WIDTHS));
  m_constellation <= decode(get_field(bfm_tdata, 1, CONFIG_INPUT_WIDTHS));
  m_code_rate     <= decode(get_field(bfm_tdata, 2, CONFIG_INPUT_WIDTHS));

  dut : entity work.axi_ldpc_table
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      s_frame_type    => m_frame_type,
      s_code_rate     => m_code_rate,
      s_tvalid        => m_tvalid,
      s_tready        => m_tready,

      -- AXI output
      m_tready        => axi_slave.tready,
      m_tvalid        => axi_slave.tvalid,
      m_offset        => axi_slave_offset,
      m_tuser         => axi_slave_tuser,
      m_next          => axi_slave_next,
      m_tlast         => axi_slave.tlast);

  axi_slave.tdata <= axi_slave_next & axi_slave_tuser & axi_slave_offset;

  axi_file_compare_u : entity fpga_cores_sim.axi_file_compare
    generic map (
      READER_NAME     => "ldpc_table_u",
      ERROR_CNT_WIDTH => 8,
      REPORT_SEVERITY => Error,
      DATA_WIDTH      => axi_slave.tdata'length)
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

  expected_offset <= expected_tdata(numbits(max(DVB_N_LDPC)) - 1 downto 0);
  expected_tuser  <= expected_tdata(2*numbits(max(DVB_N_LDPC)) - 1 downto numbits(max(DVB_N_LDPC)));
  expected_next   <= expected_tdata(2*numbits(max(DVB_N_LDPC)));

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  clk <= not clk after CLK_PERIOD/2;

  test_runner_watchdog(runner, 2 ms);

  m_data_valid <= axi_master.tvalid = '1' and axi_master.tready = '1';
  s_data_valid <= axi_slave.tvalid = '1' and axi_slave.tready = '1';

  ---------------
  -- Processes --
  ---------------
  main : process -- {{
    constant self         : actor_t := new_actor("main");
    constant logger       : logger_t := get_logger("main");
    variable file_checker : file_reader_t := new_file_reader(FILE_CHECKER_NAME);
    variable ldpc_table   : file_reader_t := new_file_reader("ldpc_table_u");

    variable config_bfm : axi_stream_bfm_t := create_bfm("axi_config_input_u");

    procedure walk(constant steps : natural) is -- {{ ----------------------------------
    begin
      if steps /= 0 then
        for step in 0 to steps - 1 loop
          wait until rising_edge(clk);
        end loop;
      end if;
    end procedure walk; -- }} ----------------------------------------------------------

    procedure wait_for_completion is -- {{ ----------------------------------------------
      variable msg : msg_t;
    begin
      join(net, config_bfm);
      wait_all_read(net, file_checker);
      wait_all_read(net, ldpc_table);
      walk(4);
      wait until rising_edge(clk) and axi_slave.tvalid = '0' for 1 ms;
      check_equal(axi_slave.tvalid, '0', "axi_slave.tvalid should be '0'");
      walk(1);
    end procedure wait_for_completion; -- }} --------------------------------------------

    procedure run_test ( -- {{ -----------------------------------------------------------
      constant config           : config_t;
      constant number_of_frames : in positive) is
      constant data_path        : string := strip(config.base_path, chars => (1 to 1 => nul));
      variable file_reader_msg  : msg_t;
      variable calc_ldpc_msg    : msg_t;

      -- GHDL doens't play well with anonymous vectors, so let's be explicit
      subtype bfm_data_t is std_logic_vector_2d_t(0 to 0)(FRAME_TYPE_WIDTH + CONSTELLATION_WIDTH + CODE_RATE_WIDTH - 1 downto 0);
      constant bfm_data : std_logic_vector := encode(config.code_rate) & encode(config.constellation) & encode(config.frame_type);

      -- In the config file, the 'next' bit is actually an entire byte, so we'll read
      -- a slightly smaller ratio of the data in multiple of bytes
      constant ratio            : ratio_t := (
        axi_slave.tdata'length,
        8*((axi_slave.tdata'length + 7) / 8)
      );
    begin

      info("Running test with:");
      info(" - constellation  : " & constellation_t'image(config.constellation));
      info(" - frame_type     : " & frame_type_t'image(config.frame_type));
      info(" - code_rate      : " & code_rate_t'image(config.code_rate));
      info(" - data path      : " & data_path);

      for i in 0 to number_of_frames - 1 loop
        debug(logger, "Setting up frame #" & to_string(i));

        axi_bfm_write(net,
          bfm         => config_bfm,
          data        => bfm_data_t'(0 => bfm_data),
          probability => 1.0,
          blocking    => False);

        read_file(net, ldpc_table, data_path & "/ldpc_table.bin", ratio);
      end loop;

    end procedure run_test; -- }} --------------------------------------------------------

  begin

    test_runner_setup(runner, RUNNER_CFG);
    show(display_handler, debug);
    hide(get_logger("file_reader_t(file_reader)"), display_handler, debug, True);
    hide(get_logger("file_reader_t(file_checker)"), display_handler, debug, True);
    hide(get_logger("file_reader_t(ldpc_table)"), display_handler, debug, True);


    while test_suite loop
      rst                <= '1';
      tready_probability <= 1.0;

      walk(32);
      rst <= '0';
      walk(32);

      set_timeout(runner, configs'length * 10 ms);

      if run("back_to_back") then
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      elsif run("slow_master") then
        tready_probability <= 1.0;

        for i in configs'range loop
          for frame in 0 to NUMBER_OF_TEST_FRAMES - 1 loop
            run_test(configs(i), number_of_frames => 1);
            wait_for_completion;
          end loop;
        end loop;

      elsif run("slow_slave") then
        tready_probability <= 0.5;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      elsif run("slow_master,slow_slave") then
        tready_probability <= 0.75;

        for i in configs'range loop
          for frame in 0 to NUMBER_OF_TEST_FRAMES - 1 loop
            run_test(configs(i), number_of_frames => 1);
            wait_for_completion;
          end loop;
        end loop;
      end if;

      wait_for_completion;
      check_equal(error_cnt, 0);

      walk(32);

    end loop;

    test_runner_cleanup(runner);
    wait;
  end process; -- }}

  process
    variable failed    : boolean;
    variable word_cnt  : natural  := 0;
    variable frame_cnt : natural := 0;
  begin
    wait until axi_slave.tvalid = '1' and axi_slave.tready = '1' and rising_edge(clk);

    failed := False;
    if axi_slave_offset /= expected_offset then
      warning(
        sformat(
          "[frame %d, word %d] Offset error: Expected %r (%d) but got %r (%d)",
          fo(frame_cnt),
          fo(word_cnt),
          fo(expected_offset),
          fo(expected_offset),
          fo(axi_slave_offset),
          fo(axi_slave_offset)));

      failed := True;
    end if;
    if axi_slave_next /= expected_next then
      warning(
        sformat(
          "[frame %d, word %d] Next flag error: Expected %r but got %r",
          fo(frame_cnt),
          fo(word_cnt),
          fo(expected_next),
          fo(axi_slave_next)));
      failed := True;
    end if;
    if axi_slave_tuser /= expected_tuser then
      warning(
        sformat(
          "[frame %d, word %d] TUSER error: Expected %r but got %r",
          fo(frame_cnt),
          fo(word_cnt),
          fo(expected_tuser),
          fo(axi_slave_tuser)));
      failed := True;
    end if;

    if failed then
      error("Some checks failed");
    end if;

    word_cnt := word_cnt + 1;
    if axi_slave.tlast = '1' then
      word_cnt  := 0;
      frame_cnt := frame_cnt + 1;
    end if;

  end process;

end axi_ldpc_table_tb;
