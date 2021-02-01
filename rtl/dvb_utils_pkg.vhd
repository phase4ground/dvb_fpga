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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library fpga_cores;
use fpga_cores.common_pkg.all;

package dvb_utils_pkg is

  constant FECFRAME_SHORT_BIT_LENGTH  : integer := 16_200;
  constant FECFRAME_NORMAL_BIT_LENGTH : integer := 64_800;

  -- Sizes
  constant DVB_N_LDPC : integer_vector_t := (FECFRAME_SHORT_BIT_LENGTH, FECFRAME_NORMAL_BIT_LENGTH);

  type frame_type_t is (not_set, fecframe_normal, fecframe_short);

  type constellation_t is ( not_set, mod_qpsk, mod_8psk, mod_16apsk, mod_32apsk);

  -- Enum like type for LDPC codes
  type code_rate_t is (
    not_set, -- Only for sim, to allow setting an invalid value
    C1_4, C1_3, C2_5, C1_2, C3_5, C2_3, C3_4, C4_5,
    C5_6, C8_9, C9_10);

  type frame_type_array_t is array (natural range <>) of frame_type_t;
  type constellation_array_t is array (natural range <>) of constellation_t;
  type code_rate_array_t is array (natural range <>) of code_rate_t;

  constant FRAME_TYPE_LENGTH    : integer := frame_type_t'pos(frame_type_t'high) - frame_type_t'pos(frame_type_t'low) + 1;
  constant CODE_RATE_LENGTH     : integer := code_rate_t'pos(code_rate_t'high) - code_rate_t'pos(code_rate_t'low) + 1;
  constant CONSTELLATION_LENGTH : integer := constellation_t'pos(constellation_t'high) - constellation_t'pos(constellation_t'low) + 1;

  constant FRAME_TYPE_WIDTH     : integer := numbits(FRAME_TYPE_LENGTH);
  constant CONSTELLATION_WIDTH  : integer := numbits(CONSTELLATION_LENGTH);
  constant CODE_RATE_WIDTH      : integer := numbits(CODE_RATE_LENGTH);

  -- Encode/decode config types to std_logic_vectors
  function encode( constant v : frame_type_t ) return std_logic_vector;
  function encode( constant v : constellation_t ) return std_logic_vector;
  function encode( constant v : code_rate_t ) return std_logic_vector;

  function decode( constant v : std_logic_vector ) return frame_type_t ;
  function decode( constant v : std_logic_vector ) return constellation_t;
  function decode( constant v : std_logic_vector ) return code_rate_t;

  function get_crc_length (
    constant frame_type : in  frame_type_t;
    constant code_rate  : in  code_rate_t) return positive;

  function to_fixed_point (constant x : real; constant width : positive) return signed;
  function cos (constant x : real; constant width : positive) return signed;
  function sin (constant x : real; constant width : positive) return signed;

end dvb_utils_pkg;

package body dvb_utils_pkg is

  function get_crc_length (
    constant frame_type : in  frame_type_t;
    constant code_rate  : in  code_rate_t) return positive is
    variable result     : integer := -1;
  begin
    if frame_type = fecframe_short then
      result := 168;
    else
      if code_rate = C8_9 or code_rate = C9_10 then
        result := 128;
      elsif code_rate = C5_6 or code_rate = C2_3 then
        result := 160;
      else
        result := 192;
      end if;
    end if;

    assert result /= -1
      report "Unable to determine CRC length for " &
             "frame type = " & frame_type_t'image(frame_type) & ", " &
             "code rate = " & code_rate_t'image(code_rate)
      severity Failure;

    return result;
  end function get_crc_length;

  function is_ulogic ( constant v : std_logic_vector ) return boolean is
  begin
    for i in v'range loop
      if v(i) /= '0' and v(i) /= '1' then
        return False;
      end if;
    end loop;
    return True;
  end;

  function encode( constant v : frame_type_t ) return std_logic_vector is
  begin
    if v = not_set then
      return (FRAME_TYPE_WIDTH - 1 downto 0 => 'U');
    end if;
    return std_logic_vector(to_unsigned(frame_type_t'pos(v), FRAME_TYPE_WIDTH));
  end;

  function encode( constant v : constellation_t ) return std_logic_vector is
  begin
    if v = not_set then
      return (CONSTELLATION_WIDTH - 1 downto 0 => 'U');
    end if;
    return std_logic_vector(to_unsigned(constellation_t'pos(v), CONSTELLATION_WIDTH));
  end;

  function encode( constant v : code_rate_t ) return std_logic_vector is
  begin
    if v = not_set then
      return (CODE_RATE_WIDTH - 1 downto 0 => 'U');
    end if;
    return std_logic_vector(to_unsigned(code_rate_t'pos(v), CODE_RATE_WIDTH));
  end;

  function is_01 ( constant v : std_logic_vector ) return boolean is
  begin
    for i in v'range loop
      if v(i) /= '0' and v(i) /= '1' then
        return False;
      end if;
    end loop;
    return True;
  end;

  function decode( constant v : std_logic_vector ) return frame_type_t is
  begin
    if not is_01(v) then
      return not_set;
    end if;
    return frame_type_t'val(to_integer(unsigned(v)));
  end;

  function decode( constant v : std_logic_vector ) return constellation_t is
  begin
    if not is_01(v) then
      return not_set;
    end if;
    return constellation_t 'val(to_integer(unsigned(v)));
  end;

  function decode( constant v : std_logic_vector ) return code_rate_t is
  begin
    if not is_01(v) then
      return not_set;
    end if;
    return code_rate_t'val(to_integer(unsigned(v)));
  end;

  function to_fixed_point (constant x : real; constant width : positive) return signed is
  begin
    return to_signed(integer(ieee.math_real.round(x * real(2**(width - 1)))), width);
  end;

  function cos (constant x : real; constant width : positive) return signed is
  begin
    return to_fixed_point(ieee.math_real.cos(x), width);
  end;

  function sin (constant x : real; constant width : positive) return signed is
  begin
    return to_fixed_point(ieee.math_real.sin(x), width);
  end;

end package body;
