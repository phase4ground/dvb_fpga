--
-- DVB IP
--
-- Copyright 2019 by Suoto <andre820@gmail.com>
--
-- This file is part of the DVB IP.
--
-- DVB IP is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- DVB IP is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with DVB IP.  If not, see <http://www.gnu.org/licenses/>.

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
use fpga_cores.common_pkg.all;

use work.dvb_utils_pkg.all;

package dvb_sim_utils_pkg is

  type file_pair_t is record
    input : string(1 to 256);
    reference : string(1 to 256);
  end record;

  type file_pair_array_t is array (natural range <>) of file_pair_t;

  type config_t is record
    constellation : constellation_t;
    frame_type    : frame_type_t;
    code_rate     : code_rate_t;
    base_path     : string(1 to 256);
  end record;

  type config_array_t is array (natural range <>) of config_t;

  function to_string( constant config : config_t ) return string;
  function to_string( constant config : file_pair_t ) return string;

  -- Add double quotes around a string
  function quote ( constant s : string ) return string;
  function quote ( constant s : character ) return string;

  impure function get_test_cfg ( constant str : string) return config_array_t;
  impure function get_test_cfg ( constant str : string) return file_pair_array_t;

  procedure push(msg : msg_t; value : constellation_t);
  procedure push(msg : msg_t; value : frame_type_t);
  procedure push(msg : msg_t; value : code_rate_t);

  procedure push(msg : msg_t; value : config_t);

  impure function pop(msg : msg_t) return constellation_t;
  impure function pop(msg : msg_t) return frame_type_t;
  impure function pop(msg : msg_t) return code_rate_t;

  impure function pop(msg : msg_t) return config_t;

  function to_real ( constant v : signed ) return real;
  function to_complex ( constant v : std_logic_vector ) return complex;

  function get_checker_data_ratio ( constant constellation : in constellation_t) return string;

end dvb_sim_utils_pkg;

package body dvb_sim_utils_pkg is

  procedure push(msg : msg_t; value : constellation_t) is
  begin
    -- Push value as a string
    push(msg.data, constellation_t'image(value));
  end;

  impure function pop(msg : msg_t) return constellation_t is
  begin
    return constellation_t'value(pop(msg.data));
  end;

  procedure push(msg : msg_t; value : frame_type_t) is
  begin
    -- Push value as a string
    push(msg.data, frame_type_t'image(value));
  end;

  impure function pop(msg : msg_t) return frame_type_t is
  begin
    return frame_type_t 'value(pop(msg.data));
  end;

  procedure push(msg : msg_t; value : code_rate_t) is
  begin
    -- Push value as a string
    push(msg.data, code_rate_t'image(value));
  end;

  impure function pop(msg : msg_t) return code_rate_t is
  begin
    return code_rate_t'value(pop(msg.data));
  end;

  procedure push(msg : msg_t; value : file_pair_t) is
  begin
    push(msg, value.input);
    push(msg, value.reference);
  end;

  impure function pop(msg : msg_t) return file_pair_t is
    constant input     : string := pop(msg);
    constant reference : string := pop(msg);
  begin
    return (input => input, reference => reference);
  end;

  procedure push(msg : msg_t; value : config_t) is
  begin
    push(msg, value.constellation);
    push(msg, value.frame_type);
    push(msg, value.code_rate);
    push(msg, value.base_path);
  end;

  impure function pop(msg : msg_t) return config_t is
    constant constellation : constellation_t := pop(msg);
    constant frame_type    : frame_type_t    := pop(msg);
    constant code_rate     : code_rate_t     := pop(msg);
    constant base_path     : string          := pop(msg);
  begin
    return (
      constellation => constellation,
      frame_type    => frame_type,
      code_rate     => code_rate,
      base_path     => base_path);
  end;

  --
  impure function get_test_cfg ( constant str : string)
  return config_array_t is
    variable cfg_strings : lines_t := split(str, "|");
    variable cfg_items   : lines_t;
    variable result      : config_array_t(0 to cfg_strings'length - 1);
    variable current     : config_t;
  begin

    if cfg_strings'length = 0 then
      warning("Could not parse any config from " & quote(str));
      return result;
    end if;

    for i in 0 to cfg_strings'length - 1 loop
      cfg_items := split(cfg_strings(i).all, ",");

      if cfg_items'length /= 4 then
        failure("Malformed config string " & quote(cfg_strings(i).all));
      end if;

      current.constellation := constellation_t'value(cfg_items(0).all);
      current.frame_type := frame_type_t'value(cfg_items(1).all);
      current.code_rate := code_rate_t'value(cfg_items(2).all);

      current.base_path := (others => nul);
      current.base_path(cfg_items(3).all'range) := cfg_items(3).all;

      result(i) := current;

    end loop;

    info("Parsed " & integer'image(result'length) & " configuration(s):");
    for i in result'range loop
      info("- " & integer'image(i) & ": " & to_string(result(i)));
    end loop;

    return result;

  end function get_test_cfg;

  impure function get_test_cfg ( constant str : string) return file_pair_array_t is
    variable cfg_strings : lines_t := split(str, "|");
    variable cfg_items   : lines_t;
    variable result      : file_pair_array_t(0 to cfg_strings'length - 1);
    variable current     : config_t;
  begin

    if cfg_strings'length = 0 then
      warning("Could not parse any config from " & quote(str));
      return result;
    end if;

    for i in 0 to cfg_strings'length - 1 loop
      cfg_items := split(cfg_strings(i).all, ",");

      if cfg_items'length /= 2 then
        failure("Malformed config string " & quote(cfg_strings(i).all));
      end if;

      result(i) := (input => cfg_items(0).all,
                    reference => cfg_items(1).all);

    end loop;

    info("Parsed " & integer'image(result'length) & " configuration(s):");
    for i in result'range loop
      info("- " & integer'image(i) & ": " & to_string(result(i)));
    end loop;

    return result;

  end function get_test_cfg;

  -- Add double quotes around a string
  function quote ( constant s : character ) return string is
  begin
    return '"' & s & '"';
  end;

  function quote ( constant s : string ) return string is
  begin
    return '"' & s & '"';
  end function quote;

  -- Returns a string representation of config_t
  function to_string( constant config : config_t ) return string is
  begin
    return "config("
      & "constellation=" & quote(constellation_t'image(config.constellation)) & ", "
      & "frame_type=" & quote(frame_type_t'image(config.frame_type)) & ", "
      & "code_rate=" & quote(code_rate_t'image(config.code_rate)) & ", "
      & "base_path=" & quote(config.base_path) & ")";
  end function to_string;

  -- Returns a string representation of config_t
  function to_string( constant config : file_pair_t ) return string is
  begin
    return "file_pair("
      & "input=" & quote(config.input) & ", "
      & "reference=" & quote(config.reference) & ")";
  end function to_string;

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


end package body;
