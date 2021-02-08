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

library str_format;
use str_format.str_format_pkg.all;

library fpga_cores;
use fpga_cores.axi_pkg.all;

library fpga_cores_sim;
context fpga_cores_sim.sim_context;

use work.dvb_utils_pkg.all;
use work.dvb_sim_utils_pkg.all;

entity axi_physical_layer_scrambler_tb is
  generic (
    RUNNER_CFG            : string;
    TEST_CFG              : string;
    NUMBER_OF_TEST_FRAMES : integer := 1);
end axi_physical_layer_scrambler_tb;

architecture axi_physical_layer_scrambler_tb of axi_physical_layer_scrambler_tb is

  function get_init_condition ( constant gold_code : std_logic_vector ) return std_logic_vector is
    constant code_i : integer := to_integer(unsigned(gold_code));
    variable result : std_logic_vector(17 downto 0) := (0 => '1', others => '0');
  begin
    for i in 0 to code_i - 1 loop
      result := (result(7) xor result(0)) & result(17 downto 1);
    end loop;
    return result;
  end function;



  ---------------
  -- Constants --
  ---------------
  constant configs     : config_array_t := get_test_cfg(TEST_CFG);

  constant CLK_PERIOD  : time    := 5 ns;
  constant TDATA_WIDTH : integer := 32;

  -------------
  -- Signals --
  -------------
  -- Usual ports
  signal clk                : std_logic := '1';
  signal rst                : std_logic;

  signal tvalid_probability : real range 0.0 to 1.0 := 1.0;
  signal tready_probability : real range 0.0 to 1.0 := 1.0;

  -- AXI input
  signal axi_master         : axi_stream_bus_t(tdata(TDATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal m_data_valid       : boolean;

  signal axi_slave         : axi_stream_bus_t(tdata(TDATA_WIDTH - 1 downto 0), tuser(ENCODED_CONFIG_WIDTH - 1 downto 0));
  signal s_data_valid       : boolean;

  signal expected_tdata     : std_logic_vector(TDATA_WIDTH - 1 downto 0);
  signal expected_tlast     : std_logic;

  signal dbg_recv           : complex;
  signal dbg_expected       : complex;

begin

  -------------------
  -- Port mappings --
  -------------------
  -- AXI file read
  axi_file_reader_u : entity fpga_cores_sim.axi_file_reader
    generic map (
      READER_NAME => "axi_file_reader_u",
      DATA_WIDTH  => TDATA_WIDTH,
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

  dut : entity work.axi_physical_layer_scrambler
    generic map (
      TDATA_WIDTH => TDATA_WIDTH,
      TID_WIDTH   => ENCODED_CONFIG_WIDTH)
    port map (
      -- Usual ports
      clk               => clk,
      rst               => rst,

      -- cfg_constellation => decode(axi_master.tuser).constellation,
      -- cfg_frame_type    => decode(axi_master.tuser).frame_type,
      -- cfg_code_rate     => decode(axi_master.tuser).code_rate,

      -- AXI input
      s_tvalid          => axi_master.tvalid,
      s_tlast           => axi_master.tlast,
      s_tready          => axi_master.tready,
      -- s_tdata           => axi_master.tdata,
      s_tdata           => axi_master.tdata(23 downto 16) & axi_master.tdata(31 downto 24) & axi_master.tdata(7 downto 0) & axi_master.tdata(15 downto 8),
      s_tid             => axi_master.tuser,

      -- AXI output
      m_tready          => axi_slave.tready,
      m_tvalid          => axi_slave.tvalid,
      m_tlast           => axi_slave.tlast,
      m_tdata           => axi_slave.tdata,
      m_tid             => axi_slave.tuser);

  ref_data_u : entity fpga_cores_sim.axi_file_reader
    generic map (
      READER_NAME     => "ref_data_u",
      DATA_WIDTH      => TDATA_WIDTH)
    port map (
      -- Usual ports
      clk                => clk,
      rst                => rst,
      -- Data output
      m_tready           => axi_slave.tready and axi_slave.tvalid,
      -- m_tdata            => axi_slave.tdata(23 downto 16) & axi_slave.tdata(31 downto 24) & axi_slave.tdata(7 downto 0) & axi_slave.tdata(15 downto 8),
      m_tdata(31 downto 24) => expected_tdata(23 downto 16),
      m_tdata(23 downto 16) => expected_tdata(31 downto 24),
      m_tdata(15 downto 8)  => expected_tdata(7 downto 0),
      m_tdata(7 downto 0)   => expected_tdata(15 downto 8),
      m_tvalid              => open,
      m_tlast               => expected_tlast);

  ------------------------------
  -- Asynchronous assignments --
  ------------------------------
  clk <= not clk after CLK_PERIOD/2;

  test_runner_watchdog(runner, 3 ms);

  m_data_valid <= axi_slave.tvalid = '1' and axi_slave.tready = '1';
  s_data_valid <= axi_master.tvalid = '1' and axi_master.tready = '1';

  axi_slave.tready <= '1';

  ---------------
  -- Processes --
  ---------------
  main : process
    constant self         : actor_t       := new_actor("main");
    constant logger       : logger_t      := get_logger("main");
    variable ref_data     : file_reader_t := new_file_reader("ref_data_u");
    variable file_reader  : file_reader_t := new_file_reader("axi_file_reader_u");

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
    procedure run_test ( -- {{ ---------------------------------------------------------
      constant config           : config_t;
      constant number_of_frames : in positive) is
      constant data_path        : string := strip(config.base_path, chars => (1 to 1 => nul));
      variable calc_ldpc_msg    : msg_t;
      variable msg              : msg_t;
      variable config_tuple     : config_tuple_t;
    begin

      info(logger, "Running test with:");
      info(logger, " - constellation  : " & constellation_t'image(config.constellation));
      info(logger, " - frame_type     : " & frame_type_t'image(config.frame_type));
      info(logger, " - code_rate      : " & code_rate_t'image(config.code_rate));
      info(logger, " - data path      : " & data_path);

      config_tuple := (code_rate => config.code_rate, constellation => config.constellation, frame_type => config.frame_type);

      for i in 0 to number_of_frames - 1 loop
        debug(logger, "Setting up frame #" & to_string(i));

        read_file(net,
          file_reader => file_reader,
          filename    => data_path & "/bit_mapper_output_fixed.bin",
          tid         => encode(config_tuple)
        );

        read_file( net, ref_data, data_path & "/plframe_payload_pilots_off_fixed_point.bin");

        -- Update the expected TID
        msg := new_msg;
        push(msg, encode(config_tuple));
        send(net, find("tid_check"), msg);

      end loop;

    end procedure run_test; -- }} ------------------------------------------------------

    procedure wait_for_transfers ( constant count : in natural) is
      variable msg : msg_t;
    begin
      wait_all_read(net, file_reader);
      wait_all_read(net, ref_data);
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

      set_timeout(runner, 10 us);

      if run("back_to_back") then
        tvalid_probability <= 1.0;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;

        wait_for_transfers(configs'length);

      elsif run("slow_master") then
        tvalid_probability <= 0.5;
        tready_probability <= 1.0;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;
        wait_for_transfers(configs'length);

      elsif run("slow_slave") then
        tvalid_probability <= 1.0;
        tready_probability <= 0.5;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;
        wait_for_transfers(configs'length);

      elsif run("both_slow") then
        tvalid_probability <= 0.75;
        tready_probability <= 0.75;

        for i in configs'range loop
          run_test(configs(i), number_of_frames => NUMBER_OF_TEST_FRAMES);
        end loop;
        wait_for_transfers(configs'length);

      end if;

      walk(8);

    end loop;

    test_runner_cleanup(runner);
    wait;
  end process;

  -- tid_check_p : process -- {{ ----------------------------------------------------------
  --   constant self           : actor_t := new_actor("tid_check");
  --   variable msg            : msg_t;
  --   variable expected_tid   : std_logic_vector(ENCODED_CONFIG_WIDTH - 1 downto 0);
  --   variable first_word     : boolean;
  -- begin
  --   first_word := True;
  --   while true loop
  --     wait until rising_edge(clk) and axi_master.tvalid = '1' and axi_master.tready = '1';
  --     if first_word then
  --       check_true(has_message(self), "Expected TID not set");
  --       receive(net, self, msg);
  --       expected_tid := pop(msg);
  --       info(sformat("Updated expected TID to %r", fo(expected_tid)));
  --     end if;

  --     check_equal(axi_master.tuser, expected_tid, "TID check failed");

  --     first_word := False;
  --     if axi_master.tlast = '1' then
  --       info("Setting first word");
  --       first_word := True;
  --     end if;
  --   end loop;
  -- end process; -- }} -------------------------------------------------------------------

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

    constant TOLERANCE        : real := 0.05;
    variable recv_r           : complex;
    variable expected_r       : complex;
    variable recv_p           : complex_polar;
    variable expected_p       : complex_polar;
    variable expected_i       : signed(TDATA_WIDTH/2 - 1 downto 0);
    variable expected_q       : signed(TDATA_WIDTH/2 - 1 downto 0);

  begin
    wait until axi_slave.tvalid = '1' and axi_slave.tready = '1' and rising_edge(clk);
    recv_r           := to_complex(axi_slave.tdata);
    expected_r       := to_complex(expected_tdata);

    dbg_recv     <= recv_r;
    dbg_expected <= expected_r;

    recv_p           := complex_to_polar(recv_r);
    expected_p       := complex_to_polar(expected_r);

    if (recv_p.mag >= 0.0 xor expected_p.mag >= 0.0)
       or (recv_p.arg >= 0.0 xor expected_p.arg >= 0.0)
       or abs(recv_p.mag) < abs(expected_p.mag) * (1.0 - TOLERANCE)
       or abs(recv_p.mag) > abs(expected_p.mag) * (1.0 + TOLERANCE)
       or abs(recv_p.arg) < abs(expected_p.arg) * (1.0 - TOLERANCE)
       or abs(recv_p.arg) > abs(expected_p.arg) * (1.0 + TOLERANCE) then
      warning(
        logger,
        sformat(
          "[%d, %d] Comparison failed. " & lf &
          "Got      %r %r rect(%s, %s) / polar(%s, %s)" & lf &
          "Expected %r %r rect(%s, %s) / polar(%s, %s)",
          fo(frame_cnt),
          fo(word_cnt),
          fo(axi_slave.tdata(TDATA_WIDTH - 1 downto TDATA_WIDTH/2)),
          fo(axi_slave.tdata(TDATA_WIDTH/2 - 1 downto 0)),
          real'image(recv_r.re), real'image(recv_r.im),
          real'image(recv_p.mag), real'image(recv_p.arg),
          fo(expected_tdata(TDATA_WIDTH - 1 downto TDATA_WIDTH/2)),
          fo(expected_tdata(TDATA_WIDTH/2 - 1 downto 0)),
          real'image(expected_r.re), real'image(expected_r.im),
          real'image(expected_p.mag), real'image(expected_p.arg)
        ));
    else
      info(
        logger,
        sformat(
          "[%d, %d] Comparison failed. " & lf &
          "Got      %r %r rect(%s, %s) / polar(%s, %s)" & lf &
          "Expected %r %r rect(%s, %s) / polar(%s, %s)",
          fo(frame_cnt),
          fo(word_cnt),
          fo(axi_slave.tdata(TDATA_WIDTH - 1 downto TDATA_WIDTH/2)),
          fo(axi_slave.tdata(TDATA_WIDTH/2 - 1 downto 0)),
          real'image(recv_r.re), real'image(recv_r.im),
          real'image(recv_p.mag), real'image(recv_p.arg),
          fo(expected_tdata(TDATA_WIDTH - 1 downto TDATA_WIDTH/2)),
          fo(expected_tdata(TDATA_WIDTH/2 - 1 downto 0)),
          real'image(expected_r.re), real'image(expected_r.im),
          real'image(expected_p.mag), real'image(expected_p.arg)
        ));
   end if;

    assert word_cnt < 16;

    word_cnt := word_cnt + 1;
    if axi_slave.tlast = '1' then
      info(logger, sformat("Received frame %d with %d words", fo(frame_cnt), fo(word_cnt)));
      word_cnt  := 0;
      frame_cnt := frame_cnt + 1;
    end if;
  end process; -- }} -------------------------------------------------------------------

end axi_physical_layer_scrambler_tb;


