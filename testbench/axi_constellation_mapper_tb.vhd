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
use ieee.math_real.all;

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
  signal clk                    : std_logic := '1';
  signal rst                    : std_logic;

  -- Mapping RAM config
  signal ram_wren               : std_logic;
  signal ram_addr               : std_logic_vector(5 downto 0);
  signal ram_wdata              : std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);
  signal ram_rdata              : std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);

  signal cfg_constellation      : constellation_t;
  signal cfg_frame_type         : frame_type_t;
  signal cfg_code_rate          : code_rate_t;

  signal data_probability       : real range 0.0 to 1.0 := 1.0;
  signal tready_probability     : real range 0.0 to 1.0 := 1.0;

  -- AXI input
  signal axi_master             : axi_stream_data_bus_t(tdata(INPUT_DATA_WIDTH - 1 downto 0));
  -- AXI output
  signal axi_slave              : axi_stream_data_bus_t(tdata(OUTPUT_DATA_WIDTH - 1 downto 0));
  signal expected               : axi_stream_data_bus_t(tdata(OUTPUT_DATA_WIDTH - 1 downto 0));
  signal dbg_expected : integer;

  signal m_data_valid           : boolean;
  signal s_data_valid           : boolean;

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

  ref_data_u : entity fpga_cores_sim.axi_file_reader
    generic map (
      READER_NAME     => "ref_data_u",
      DATA_WIDTH      => OUTPUT_DATA_WIDTH)
    port map (
        -- Usual ports
        clk                => clk,
        rst                => rst,
        -- Config and status
        completed          => open,
        tvalid_probability => 1.0,

        -- Data output
        m_tready           => expected.tready,
        m_tdata            => expected.tdata,
        m_tvalid           => expected.tvalid,
        m_tlast            => expected.tlast);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  clk <= not clk after CLK_PERIOD/2;

  test_runner_watchdog(runner, 10 ms);

  m_data_valid <= axi_master.tvalid = '1' and axi_master.tready = '1';
  s_data_valid <= axi_slave.tvalid = '1' and axi_slave.tready = '1';

  expected.tready <= '1' when s_data_valid else '0';

  ---------------
  -- Processes --
  ---------------
  main : process -- {{
    constant self       : actor_t       := new_actor("main");
    constant logger     : logger_t      := get_logger("main");
    variable input_data : file_reader_t := new_file_reader("input_data_u");
    variable ref_data   : file_reader_t := new_file_reader("ref_data_u");

    procedure walk(constant steps : natural) is -- {{ ----------------------------------
    begin
      if steps /= 0 then
        for step in 0 to steps - 1 loop
          wait until rising_edge(clk);
        end loop;
      end if;
    end procedure walk; -- }} ----------------------------------------------------------

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

        -- FIXME: Use the packed data file
        read_file(net => net,
          file_reader => input_data,
          -- filename    => data_path & "/bit_interleaver_output_packed.bin",
          filename    => data_path & "/bit_interleaver_output.bin",
          ratio       => get_checker_data_ratio(config.constellation),
          tid         => encode(config.code_rate) & encode(config.constellation) & encode(config.frame_type)
        );

        read_file(net, ref_data, data_path & "/bit_mapper_output_fixed.bin");

      end loop;

    end procedure run_test; -- }} ------------------------------------------------------

    procedure wait_for_completion is -- {{ ---------------------------------------------
      variable msg : msg_t;
    begin
      info(logger, "Waiting for all frames to be read");
      wait_all_read(net, ref_data);
      info(logger, "All data has now been read");

      wait until rising_edge(clk) and axi_slave.tvalid = '0' for 1 ms;

      walk(1);

    end procedure wait_for_completion; -- }} -------------------------------------------

    -- Write the exact value so we know data was picked up correctly without having to
    -- convert into IQ
    procedure init_ram is -- {{ --------------------------------------------------------
    begin
      -- QPSK
      for i in 0 to 3 loop
        write_ram(
          i, 
          std_logic_vector(to_unsigned(4, OUTPUT_DATA_WIDTH/2)) &
          std_logic_vector(to_unsigned(i, OUTPUT_DATA_WIDTH/2))
        );
      end loop;
      -- 8 PSK
      for i in 0 to 7 loop
        write_ram(
          i + 4, 
          std_logic_vector(to_unsigned(8, OUTPUT_DATA_WIDTH/2)) &
          std_logic_vector(to_unsigned(i, OUTPUT_DATA_WIDTH/2))
        );
      end loop;
      -- 16 APSK
      for i in 0 to 15 loop
        write_ram(
          i + 12, 
          std_logic_vector(to_unsigned(16, OUTPUT_DATA_WIDTH/2)) &
          std_logic_vector(to_unsigned(i, OUTPUT_DATA_WIDTH/2))
        );
      end loop;
      -- 32 APSK
      for i in 0 to 31 loop
        write_ram(
          i + 28, 
          std_logic_vector(to_unsigned(32, OUTPUT_DATA_WIDTH/2)) &
          std_logic_vector(to_unsigned(i, OUTPUT_DATA_WIDTH/2))
        );
      end loop;
    end procedure; -- }} ---------------------------------------------------------------

  begin

    ram_wren  <= '0';
    ram_addr  <= (others => 'U');
    ram_wdata <= (others => 'U');

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

      init_ram;

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

  receiver_p : process
    constant logger      : logger_t := get_logger("receiver");
    variable word_cnt    : natural  := 0;
    variable frame_cnt   : natural  := 0;

    variable axi_data        : unsigned(OUTPUT_DATA_WIDTH/2 - 1 downto 0);
    variable table        : integer;
    variable expected_int : integer;

    impure function demodulate ( constant v : std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0) ) return integer is
    begin
      if table = 4 then
        -- case v is
        --   when x"825A825A" => return 0;
        --   when x"FF7F0000" => return 1;
        --   when x"00800000" => return 2;
        --   when x"7EA57EA5" => return 3;
        --   when x"0000FF7F" => return 4;
        --   when x"825A7EA5" => return 5;
        --   when x"7EA5825A" => return 6;
        --   when x"00000080" => return 7;
        --   when others =>
        --     null;
        -- end case;
      end if;
      if table = 8 then
        case v is
          when x"825A825A" => return 0;
          when x"FF7F0000" => return 1;
          when x"00800000" => return 2;
          when x"7EA57EA5" => return 3;
          when x"0000FF7F" => return 4;
          when x"825A7EA5" => return 5;
          when x"7EA5825A" => return 6;
          when x"00000080" => return 7;
          when others =>
            null;
        end case;
      end if;

      report sformat("Don't know how to demodulate %r", fo(v))
      severity Warning;

      return -1;
    end;

  begin
    wait until axi_slave.tvalid = '1' and axi_slave.tready = '1' and rising_edge(clk);
    table := to_integer(unsigned(axi_slave.tdata(OUTPUT_DATA_WIDTH - 1 downto OUTPUT_DATA_WIDTH/2)));
    expected_int := demodulate(expected.tdata);
    axi_data := unsigned(axi_slave.tdata(OUTPUT_DATA_WIDTH/2 - 1 downto 0));

    dbg_expected <= expected_int;

    if axi_data /= expected_int then
      error(
        logger,
        sformat(
          "[%d, %d] got %d (%r), expected %d (%r)",
          fo(frame_cnt),
          fo(word_cnt),
          fo(axi_slave.tdata),
          fo(axi_slave.tdata),
          fo(expected_int),
          fo(expected.tdata)
        ));
    end if;

    word_cnt := word_cnt + 1;
    if axi_slave.tlast = '1' then
      info(logger, sformat("Received frame %d with %d words", fo(frame_cnt), fo(word_cnt)));
      word_cnt  := 0;
      frame_cnt := frame_cnt + 1;
    end if;
  end process;

  axi_slave_tready_gen : process(clk)
    variable tready_rand : RandomPType;
  begin
    if rising_edge(clk) then
      -- Generate a tready enable with the configured probability
      axi_slave.tready <= '0';
      if tready_rand.RandReal(1.0) <= tready_probability then
        axi_slave.tready <= '1';
      end if;
    end if;
  end process;

end axi_constellation_mapper_tb;
