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
context fpga_cores_sim.sim_context;

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

  constant INPUT_DATA_WIDTH  : integer := 8;
  constant OUTPUT_DATA_WIDTH : integer := 32;

  constant CLK_PERIOD        : time    := 5 ns;

  function get_checker_data_ratio ( constant constellation : in constellation_t)
  return string is
  begin
    case constellation is
      when   mod_qpsk => return "2:8";
      when   mod_8psk => return "3:8";
      when mod_16apsk => return "4:8";
      when mod_32apsk => return "5:8";
      when others =>
        report "Invalid constellation: " & constellation_t'image(constellation)
        severity Failure;
    end case;

    -- Just to avoid the warning, should never be reached
    return "";
  end function;

  -------------
  -- Signals --
  -------------
  signal clk                : std_logic := '1';
  signal rst                : std_logic;

  -- Mapping RAM config
  signal ram_wren           : std_logic;
  signal ram_addr           : std_logic_vector(5 downto 0);
  signal ram_wdata          : std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);
  signal ram_rdata          : std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);

  signal cfg_constellation  : constellation_t;
  signal cfg_frame_type     : frame_type_t;
  signal cfg_code_rate      : code_rate_t;

  signal data_probability   : real range 0.0 to 1.0 := 1.0;
  signal tready_probability : real range 0.0 to 1.0 := 1.0;

  -- AXI input
  signal axi_master         : axi_stream_data_bus_t(tdata(INPUT_DATA_WIDTH - 1 downto 0));
  -- AXI output
  signal axi_slave          : axi_stream_data_bus_t(tdata(OUTPUT_DATA_WIDTH - 1 downto 0));
  signal axi_slave_tdata    : std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);

  signal m_data_valid       : boolean;
  signal s_data_valid       : boolean;

  signal tdata_error_cnt    : std_logic_vector(7 downto 0);
  signal tlast_error_cnt    : std_logic_vector(7 downto 0);
  signal error_cnt          : std_logic_vector(7 downto 0);

begin

  -------------------
  -- Port mappings --
  -------------------
  -- AXI file read
  axi_file_reader_block : block
    constant CONFIG_INPUT_WIDTHS: fpga_cores.common_pkg.integer_vector_t := (
      0 => FRAME_TYPE_WIDTH,
      1 => CONSTELLATION_WIDTH,
      2 => CODE_RATE_WIDTH);

    signal m_tid : std_logic_vector(sum(CONFIG_INPUT_WIDTHS) - 1 downto 0);
  begin
    input_data_u : entity fpga_cores_sim.axi_file_reader
      generic map (
        READER_NAME => "input_data_u",
        DATA_WIDTH  => INPUT_DATA_WIDTH,
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

  dut : entity work.axi_constellation_mapper
  generic map ( INPUT_DATA_WIDTH => INPUT_DATA_WIDTH )
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      ram_wren          => ram_wren,
      ram_addr          => ram_addr,
      ram_wdata         => ram_wdata,
      ram_rdata         => ram_rdata,

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

  axi_file_compare_u : entity fpga_cores_sim.axi_file_compare
    generic map (
      READER_NAME     => "ref_data_u",
      ERROR_CNT_WIDTH => 8,
      REPORT_SEVERITY => Error,
      DATA_WIDTH      => OUTPUT_DATA_WIDTH)
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,
      -- Config and status
      tdata_error_cnt    => tdata_error_cnt,
      tlast_error_cnt    => tlast_error_cnt,
      error_cnt          => error_cnt,
      tready_probability => 1.0,
      -- Debug stuff
      expected_tdata     => open,
      expected_tlast     => open,
      -- Data input
      s_tready           => axi_slave.tready,
      s_tdata            => axi_slave_tdata,
      s_tvalid           => axi_slave.tvalid,
      s_tlast            => axi_slave.tlast);

  axi_slave_tdata <= axi_slave.tdata(23 downto 16) & axi_slave.tdata(31 downto 24) & axi_slave.tdata(7 downto 0) & axi_slave.tdata(15 downto 8);

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
    constant self        : actor_t       := new_actor("main");
    constant logger      : logger_t      := get_logger("main");
    variable input_data  : file_reader_t := new_file_reader("input_data_u");
    variable ref_data    : file_reader_t := new_file_reader("ref_data_u");
    variable prev_config : config_t;

    procedure walk(constant steps : natural) is -- {{ ----------------------------------
    begin
      if steps /= 0 then
        for step in 0 to steps - 1 loop
          wait until rising_edge(clk);
        end loop;
      end if;
    end procedure walk; -- }} ----------------------------------------------------------

    procedure wait_for_completion is -- {{ ---------------------------------------------
    begin
      info(logger, "Waiting for all frames to be read");
      wait_all_read(net, ref_data);
      info(logger, "All data has now been read");
      wait until rising_edge(clk) and axi_slave.tvalid = '0' for 1 ms;
      walk(1);
    end procedure wait_for_completion; -- }} -------------------------------------------

    procedure write_ram ( -- {{ --------------------------------------------------------
      constant addr : in integer;
      constant data : in std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0)) is
    begin
      ram_wren  <= '1';
      ram_addr  <= std_logic_vector(to_unsigned(addr, 6));
      ram_wdata <= data;
      walk(1);
      ram_wren  <= '0';
      ram_addr  <= (others => 'U');
      ram_wdata <= (others => 'U');
    end procedure; -- }} ---------------------------------------------------------------

    -- Write the exact value so we know data was picked up correctly without having to
    -- convert into IQ
    procedure update_mapping_ram ( -- {{ -----------------------------------------------
      constant initial_addr : integer;
      constant path         : string) is
      file file_handler     : text;
      variable L            : line;
      variable r0, r1       : real;
      variable addr         : integer := initial_addr;
    begin
      info(sformat("Updating mapping RAM from '%s' (initial address is %d)", fo(path), fo(initial_addr)));
      file_open(file_handler, path, read_mode);
      while not endfile(file_handler) loop
        readline(file_handler, L);
        read(L, r0);
        readline(file_handler, L);
        read(L, r1);
        info(
          sformat(
            "Writing RAM: %2d <= %13s (%r) / %13s (%r)",
            fo(addr),
            real'image(r0),
            fo(to_fixed_point(r0, OUTPUT_DATA_WIDTH/2)),
            real'image(r1),
            fo(to_fixed_point(r1, OUTPUT_DATA_WIDTH/2))
          )
        );

        write_ram(
          addr,
          std_logic_vector(to_fixed_point(r0, OUTPUT_DATA_WIDTH/2)) &
          std_logic_vector(to_fixed_point(r1, OUTPUT_DATA_WIDTH/2))
        );
        addr := addr + 1;
      end loop;
      file_close(file_handler);
    end procedure; -- }} ---------------------------------------------------------------

    procedure run_test ( -- {{ ---------------------------------------------------------
      constant config           : config_t;
      constant number_of_frames : in positive) is
      constant data_path        : string := strip(config.base_path, chars => (1 to 1 => nul));
      variable initial_addr     : integer := 0;
    begin
      info(logger, "Running test with:");
      info(logger, " - constellation  : " & constellation_t'image(config.constellation));
      info(logger, " - frame_type     : " & frame_type_t'image(config.frame_type));
      info(logger, " - code_rate      : " & code_rate_t'image(config.code_rate));
      info(logger, " - data path      : " & data_path);

      -- Only update the mapping RAM if the config actually requires that
      if config /= prev_config then
        wait_for_completion;
        case config.constellation is
          when mod_qpsk => initial_addr := 0;
          when mod_8psk => initial_addr := 4;
          when mod_16apsk => initial_addr := 12;
          when mod_32apsk => initial_addr := 28;
          when others => null;
        end case;
        update_mapping_ram(initial_addr, data_path & "/modulation_table.bin");
        prev_config := config;
      end if;

      for i in 0 to number_of_frames - 1 loop
        debug(logger, "Setting up frame #" & to_string(i));

        -- FIXME: Use the packed data file
        read_file(net => net,
          file_reader => input_data,
          filename    => data_path & "/bit_interleaver_output_packed.bin",
          -- filename    => data_path & "/bit_interleaver_output.bin",
          -- ratio       => get_checker_data_ratio(config.constellation),
          tid         => encode(config.code_rate) & encode(config.constellation) & encode(config.frame_type)
        );

        read_file(net, ref_data, data_path & "/bit_mapper_output_fixed.bin");

      end loop;

    end procedure run_test; -- }} ------------------------------------------------------

  begin

    ram_wren  <= '0';

    test_runner_setup(runner, RUNNER_CFG);
    show(display_handler, debug);
    hide(get_logger("file_reader_t(input_data)"), display_handler, debug, True);
    hide(get_logger("file_reader_t(ref_data)"), display_handler, debug, True);

    while test_suite loop
      rst                <= '1';
      data_probability   <= 1.0;
      tready_probability <= 1.0;

      walk(32);

      rst <= '0';

      walk(16);

      set_timeout(runner, configs'length * 10 ms);

      if run("back_to_back") then
        data_probability   <= 1.0;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      elsif run("data=0.5,slave=1.0") then
        data_probability   <= 0.5;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      elsif run("data=1.0,slave=0.5") then
        data_probability   <= 1.0;
        tready_probability <= 0.5;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

      elsif run("data=0.75,slave=0.75") then
        data_probability   <= 0.75;
        tready_probability <= 0.75;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;
      end if;

      wait_for_completion;

      check_equal(axi_slave.tvalid, '0', "axi_slave.tvalid should be '0'");

      walk(32);

    end loop;

    test_runner_cleanup(runner);
    wait;
  end process; -- }}


end axi_constellation_mapper_tb;
