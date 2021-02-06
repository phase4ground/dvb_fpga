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
use ieee.math_real.all;
use ieee.math_complex.all;

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
  constant OUTPUT_DATA_WIDTH : integer := 32;

  constant BB_SCRAMBLER_CHECKER_NAME  : string  := "bb_scrambler";
  constant BCH_ENCODER_CHECKER_NAME   : string  := "bch_encoder";
  constant LDPC_ENCODER_CHECKER_NAME  : string  := "ldpc_encoder";

  constant CLK_PERIOD        : time := 5 ns;

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
    tdata_error_cnt : std_logic_vector(7 downto 0);
    tlast_error_cnt : std_logic_vector(7 downto 0);
    error_cnt       : std_logic_vector(7 downto 0);
    expected_tdata  : std_logic_vector;
    expected_tlast  : std_logic;
  end record;

  -------------
  -- Signals --
  -------------
  -- Usual ports
  signal clk                   : std_logic := '1';
  signal rst                   : std_logic;

  signal cfg_constellation     : constellation_t;
  signal cfg_frame_type        : frame_type_t;
  signal cfg_code_rate         : code_rate_t;

  signal data_probability      : real range 0.0 to 1.0 := 1.0;
  signal table_probability     : real range 0.0 to 1.0 := 1.0;
  signal tready_probability    : real range 0.0 to 1.0 := 1.0;

  -- AXI input
  signal axi_master            : axi_stream_bus_t(tdata(DATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal m_data_valid          : std_logic;

  -- AXI LDPC table input
  signal axi_ldpc              : axi_stream_data_bus_t(tdata(2*numbits(max(DVB_N_LDPC)) + 8 - 1 downto 0));

  -- AXI output
  signal s_data_valid          : std_logic;

  signal axi_bb_scrambler      : axi_checker_t(axi(tdata(DATA_WIDTH - 1 downto 0)), expected_tdata(DATA_WIDTH - 1 downto 0));
  signal axi_bch_encoder       : axi_checker_t(axi(tdata(DATA_WIDTH - 1 downto 0)), expected_tdata(DATA_WIDTH - 1 downto 0));
  signal axi_ldpc_encoder_core : axi_checker_t(axi(tdata(DATA_WIDTH - 1 downto 0)), expected_tdata(DATA_WIDTH - 1 downto 0));
  signal axi_slave             : axi_checker_t(axi(tdata(OUTPUT_DATA_WIDTH - 1 downto 0)), expected_tdata(OUTPUT_DATA_WIDTH - 1 downto 0));
  signal axi_slave_tdata       : std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);

  -- Mapping RAM config
  signal ram_wren              : std_logic;
  signal ram_addr              : std_logic_vector(5 downto 0);
  signal ram_wdata             : std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);
  signal ram_rdata             : std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);

begin

  -------------------
  -- Port mappings --
  -------------------
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

  input_stream_u : entity fpga_cores_sim.axi_file_reader
    generic map (
      READER_NAME => "input_stream_u",
      DATA_WIDTH  => DATA_WIDTH,
      TID_WIDTH   => ENCODED_CONFIG_WIDTH)
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
      m_tid              => axi_master.tuser,
      m_tvalid           => axi_master.tvalid,
      m_tlast            => axi_master.tlast);

  dut : entity work.dvbs2_tx
    generic map (
      DATA_WIDTH        => DATA_WIDTH,
      OUTPUT_DATA_WIDTH => OUTPUT_DATA_WIDTH)
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      cfg_constellation => encode(cfg_constellation),
      cfg_frame_type    => encode(cfg_frame_type),
      cfg_code_rate     => encode(cfg_code_rate),

      -- Mapping RAM config
      ram_wren          => ram_wren,
      ram_addr          => ram_addr,
      ram_wdata         => ram_wdata,
      ram_rdata         => ram_rdata,

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

  -- ghdl translate_off
  bb_scrambler_checker_u : entity fpga_cores_sim.axi_file_compare
    generic map (
      READER_NAME     => BB_SCRAMBLER_CHECKER_NAME,
      ERROR_CNT_WIDTH => 8,
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
      ERROR_CNT_WIDTH => 8,
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
      ERROR_CNT_WIDTH => 8,
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
  -- ghdl translate_on

  -- Can't check against expected data directly because of rounding errors, so use a file
  -- reader to get the contents
  output_ref_data_u : entity fpga_cores_sim.axi_file_reader
    generic map (
      READER_NAME => "output_ref_data_u",
      DATA_WIDTH  => OUTPUT_DATA_WIDTH)
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,
      -- Config and status
      completed          => open,
      tvalid_probability => 1.0,

      -- Data output
      m_tready           => axi_slave.axi.tready and axi_slave.axi.tvalid,
      m_tdata            => axi_slave.expected_tdata,
      -- m_tid              => axi_slave.axi.tuser,
      m_tvalid           => open,
      m_tlast            => open);

  axi_slave.axi.tready <= '1';

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  clk <= not clk after CLK_PERIOD/2;

  test_runner_watchdog(runner, 300 ms);

  m_data_valid <= axi_master.tvalid and axi_master.tready;
  s_data_valid <= axi_slave.axi.tvalid and axi_slave.axi.tready;

  axi_slave_tdata <= axi_slave.axi.tdata(23 downto 16) & axi_slave.axi.tdata(31 downto 24) & axi_slave.axi.tdata(7 downto 0) & axi_slave.axi.tdata(15 downto 8);

  cfg_constellation <= decode(axi_master.tuser).constellation;
  cfg_frame_type    <= decode(axi_master.tuser).frame_type;
  cfg_code_rate     <= decode(axi_master.tuser).code_rate;

  ---------------
  -- Processes --
  ---------------
  main : process -- {{ -----------------------------------------------------------------
    constant self                 : actor_t       := new_actor("main");
    variable file_reader          : file_reader_t := new_file_reader("input_stream_u");
    variable file_checker         : file_reader_t := new_file_reader("output_ref_data_u");
    variable ldpc_table           : file_reader_t := new_file_reader("axi_table_u");

    variable bb_scrambler_checker : file_reader_t := new_file_reader(BB_SCRAMBLER_CHECKER_NAME);
    variable bch_encoder_checker  : file_reader_t := new_file_reader(BCH_ENCODER_CHECKER_NAME);
    variable ldpc_encoder_checker : file_reader_t := new_file_reader(LDPC_ENCODER_CHECKER_NAME);

    variable prev_config          : config_t;

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
      -- ghdl translate_off
      wait_all_read(net, bb_scrambler_checker);
      wait_all_read(net, bch_encoder_checker);
      wait_all_read(net, ldpc_encoder_checker);
      -- wait_all_read(net, ldpc_table);
      -- ghdl translate_on

      wait until rising_edge(clk) and axi_slave.axi.tvalid = '0' for 1 ms;

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
      variable index        : unsigned(5 downto 0) := (others => '0');
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
            "[%b] Writing RAM: %2d => %13s (%r) / %13s (%r)",
            fo(index),
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
        index := index + 1;
      end loop;
      file_close(file_handler);
    end procedure; -- }} ---------------------------------------------------------------

    procedure run_test ( -- {{ ---------------------------------------------------------
      constant config           : config_t;
      constant number_of_frames : in positive) is
      variable file_reader_msg  : msg_t;
      constant data_path        : string := strip(config.base_path, chars => (1 to 1 => nul));
      variable initial_addr     : integer := 0;
      constant config_tuple     : config_tuple_t := (code_rate => config.code_rate,
                                                     constellation => config.constellation,
                                                     frame_type => config.frame_type);
    begin

      info("Running test with:");
      info(" - constellation  : " & constellation_t'image(config.constellation));
      info(" - frame_type     : " & frame_type_t'image(config.frame_type));
      info(" - code_rate      : " & code_rate_t'image(config.code_rate));
      info(" - data path      : " & data_path);

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
        file_reader_msg := new_msg;
        file_reader_msg.sender := self;

        read_file(net,
          file_reader => file_reader,
          filename    => data_path & "/bb_scrambler_input.bin",
          ratio       => "1:8",
          tid         => encode(config_tuple)
        );

        -- if config.constellation /= mod_qpsk then
        --   read_file(
        --     net,
        --     file_checker,
        --     data_path & "/bit_interleaver_output.bin",
        --     get_checker_data_ratio(config.constellation)
        --   );
        -- end if;

        read_file(net, ldpc_table, data_path & "/ldpc_table.bin");

        -- ghdl translate_off
        read_file(net, bb_scrambler_checker, data_path & "/bch_encoder_input.bin", "1:8");
        read_file(net, bch_encoder_checker, data_path & "/ldpc_encoder_input.bin", "1:8");
        read_file(net, ldpc_encoder_checker, data_path & "/bit_interleaver_input.bin", "1:8");
        -- ghdl translate_on

        read_file(net, file_checker, data_path & "/bit_mapper_output_fixed.bin");

      end loop;

    end procedure run_test; -- }} ------------------------------------------------------

  begin

    test_runner_setup(runner, RUNNER_CFG);
    show(display_handler, debug);
    hide(get_logger("file_reader_t(input_stream_u)"), display_handler, debug, True);
    hide(get_logger("file_reader_t(output_ref_data_u)"), display_handler, debug, True);
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
      check_equal(axi_slave.axi.tvalid, '0', "axi_slave.axi.tvalid should be '0'");

      walk(32);

    end loop;

    test_runner_cleanup(runner);
    wait;
  end process; -- }} -------------------------------------------------------------------

-- ghdl translate_off
  signal_spy_block : block -- {{ -------------------------------------------------------
    signal bb_scrambler_out : axi_stream_bus_t(tdata(DATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
    signal bch_encoder_out  : axi_stream_bus_t(tdata(DATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
    signal ldpc_encoder_out : axi_stream_bus_t(tdata(DATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  begin

    axi_bb_scrambler.axi.tdata     <= bb_scrambler_out.tdata;
    axi_bb_scrambler.axi.tvalid    <= bb_scrambler_out.tvalid;
    axi_bb_scrambler.axi.tready    <= bb_scrambler_out.tready;
    axi_bb_scrambler.axi.tlast     <= bb_scrambler_out.tlast;

    axi_bch_encoder.axi.tdata      <= bch_encoder_out.tdata;
    axi_bch_encoder.axi.tvalid     <= bch_encoder_out.tvalid;
    axi_bch_encoder.axi.tready     <= bch_encoder_out.tready;
    axi_bch_encoder.axi.tlast      <= bch_encoder_out.tlast;

    axi_ldpc_encoder_core.axi.tdata     <= ldpc_encoder_out.tdata;
    axi_ldpc_encoder_core.axi.tvalid    <= ldpc_encoder_out.tvalid;
    axi_ldpc_encoder_core.axi.tready    <= ldpc_encoder_out.tready;
    axi_ldpc_encoder_core.axi.tlast     <= ldpc_encoder_out.tlast;

    process
    begin
      init_signal_spy("/dvbs2_tx_tb/dut/bb_scrambler_out",  "/dvbs2_tx_tb/signal_spy_block/bb_scrambler_out",  0);
      init_signal_spy("/dvbs2_tx_tb/dut/bch_encoder_out", "/dvbs2_tx_tb/signal_spy_block/bch_encoder_out", 0);
      init_signal_spy("/dvbs2_tx_tb/dut/ldpc_encoder_out",  "/dvbs2_tx_tb/signal_spy_block/ldpc_encoder_out",  0);
      wait;
    end process;
  end block signal_spy_block; -- }} ----------------------------------------------------
-- ghdl translate_on

  receiver_p : process -- {{ -----------------------------------------------------------
    constant logger      : logger_t := get_logger("receiver");
    variable word_cnt    : natural  := 0;
    variable frame_cnt   : natural  := 0;

    function to_real ( constant v : signed ) return real is
      constant width : integer := v'length;
    begin
      return real(to_integer(v)) / real(2**(width - 1));
    end;

    function to_complex ( constant v : std_logic_vector ) return complex is
      constant width : integer := v'length;
    begin
      return complex'(
        re => to_real(signed(v(width - 1 downto width/2))),
        im => to_real(signed(v(width/2 - 1 downto 0)))
      );
    end function;

    constant TOLERANCE        : real := 0.10;
    variable recv_r           : complex;
    variable expected_r       : complex;
    variable recv_p           : complex_polar;
    variable expected_p       : complex_polar;
    variable expected_lendian : std_logic_vector(OUTPUT_DATA_WIDTH - 1 downto 0);
    variable expected_i       : signed(OUTPUT_DATA_WIDTH/2 - 1 downto 0);
    variable expected_q       : signed(OUTPUT_DATA_WIDTH/2 - 1 downto 0);

  begin
    wait until axi_slave.axi.tvalid = '1' and axi_slave.axi.tready = '1' and rising_edge(clk);
    -- receiver_busy    <= True;
    expected_lendian := axi_slave.expected_tdata(23 downto 16) & axi_slave.expected_tdata(31 downto 24) & axi_slave.expected_tdata(7 downto 0) & axi_slave.expected_tdata(15 downto 8);

    recv_r           := to_complex(axi_slave.axi.tdata);
    expected_r       := to_complex(expected_lendian);

    recv_p           := complex_to_polar(recv_r);
    expected_p       := complex_to_polar(expected_r);

    if axi_slave.axi.tdata /= expected_lendian then
      if    (recv_p.mag >= 0.0 xor expected_p.mag >= 0.0)
         or (recv_p.arg >= 0.0 xor expected_p.arg >= 0.0)
         or abs(recv_p.mag) < abs(expected_p.mag) * (1.0 - TOLERANCE)
         or abs(recv_p.mag) > abs(expected_p.mag) * (1.0 + TOLERANCE)
         or abs(recv_p.arg) < abs(expected_p.arg) * (1.0 - TOLERANCE)
         or abs(recv_p.arg) > abs(expected_p.arg) * (1.0 + TOLERANCE) then
        error(
          logger,
          sformat(
            "[%d, %d] Comparison failed. " & lf &
            "Got      %r rect(%s, %s) / polar(%s, %s)" & lf &
            "Expected %r rect(%s, %s) / polar(%s, %s)",
            fo(frame_cnt),
            fo(word_cnt),
            fo(axi_slave.axi.tdata),
            real'image(recv_r.re), real'image(recv_r.im),
            real'image(recv_p.mag), real'image(recv_p.arg),
            fo(expected_lendian),
            real'image(expected_r.re), real'image(expected_r.im),
            real'image(expected_p.mag), real'image(expected_p.arg)
          ));
      end if;
    end if;

    word_cnt := word_cnt + 1;
    if axi_slave.axi.tlast = '1' then
      -- receiver_busy <= False;
      info(logger, sformat("Received frame %d with %d words", fo(frame_cnt), fo(word_cnt)));
      word_cnt  := 0;
      frame_cnt := frame_cnt + 1;
    end if;
  end process; -- }} -------------------------------------------------------------------
end dvbs2_tx_tb;
