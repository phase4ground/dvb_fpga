#!/usr/bin/env python3
#
# DVB FPGA
#
# Copyright 2019 by Suoto <andre820@gmail.com>
#
# This file is part of DVB FPGA.
#
# DVB FPGA is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# DVB FPGA is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with DVB FPGA.  If not, see <http://www.gnu.org/licenses/>.
"Generates a VHDL package containing the constellation mapper tables"

import os.path as p
import struct
import sys
from math import cos, pi, sin
from os import makedirs

from dvb_common import CodeRate, ConstellationType, FrameLength  # type: ignore

ROOT = p.abspath(p.dirname(__file__))

sys.path.insert(0, p.join(ROOT, "misc"))


HEADER = """\
--
-- DVB IP
--
-- Copyright 2020 by Suoto <andre820@gmail.com>
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

"""


def _getGammaValues(constellation, code_rate):
    if constellation == ConstellationType.MOD_32APSK:
        gamma1 = gamma2 = None
        if code_rate == CodeRate.C3_4:
            gamma1 = 1.0 / 5.27
            gamma2 = gamma1 * 2.84
        if code_rate == CodeRate.C4_5:
            gamma1 = 1.0 / 4.87
            gamma2 = gamma1 * 2.72
        if code_rate == CodeRate.C5_6:
            gamma1 = 1.0 / 4.64
            gamma2 = gamma1 * 2.64
        if code_rate == CodeRate.C8_9:
            gamma1 = 1.0 / 4.33
            gamma2 = gamma1 * 2.54
        if code_rate == CodeRate.C9_10:
            gamma1 = 1.0 / 4.30
            gamma2 = gamma1 * 2.53

        if None not in (gamma1, gamma2):
            return gamma1, gamma2, 1.0

    if constellation == ConstellationType.MOD_16APSK:
        if code_rate == CodeRate.C2_3:
            return (1.0 / 3.15, 1.0)
        if code_rate == CodeRate.C3_4:
            return (1.0 / 2.85, 1.0)
        if code_rate == CodeRate.C3_5:
            return (1.0 / 3.70, 1.0)
        if code_rate == CodeRate.C4_5:
            return (1.0 / 2.75, 1.0)
        if code_rate == CodeRate.C5_6:
            return (1.0 / 2.70, 1.0)
        if code_rate == CodeRate.C8_9:
            return (1.0 / 2.60, 1.0)
        if code_rate == CodeRate.C9_10:
            return (1.0 / 2.57, 1.0)

    assert False, f"Config not supported: {constellation}, {code_rate}"


def _getTable(constellation, code_rate=None):
    if constellation == ConstellationType.MOD_8PSK:
        return (
            (cos(pi / 4.0), sin(pi / 4.0)),
            (cos(0.0), sin(0.0)),
            (cos(4 * pi / 4.0), sin(4 * pi / 4.0)),
            (cos(5 * pi / 4.0), sin(5 * pi / 4.0)),
            (cos(2 * pi / 4.0), sin(2 * pi / 4.0)),
            (cos(7 * pi / 4.0), sin(7 * pi / 4.0)),
            (cos(3 * pi / 4.0), sin(3 * pi / 4.0)),
            (cos(6 * pi / 4.0), sin(6 * pi / 4.0)),
        )

    if constellation == ConstellationType.MOD_16APSK:
        gamma1, gamma2 = _getGammaValues(constellation, code_rate)
        return (
            (gamma2 * cos(pi / 4.0), gamma2 * sin(pi / 4.0)),
            (gamma2 * cos(-pi / 4.0), gamma2 * sin(-pi / 4.0)),
            (gamma2 * cos(3 * pi / 4.0), gamma2 * sin(3 * pi / 4.0)),
            (gamma2 * cos(-3 * pi / 4.0), gamma2 * sin(-3 * pi / 4.0)),
            (gamma2 * cos(pi / 12.0), gamma2 * sin(pi / 12.0)),
            (gamma2 * cos(-pi / 12.0), gamma2 * sin(-pi / 12.0)),
            (gamma2 * cos(11 * pi / 12.0), gamma2 * sin(11 * pi / 12.0)),
            (gamma2 * cos(-11 * pi / 12.0), gamma2 * sin(-11 * pi / 12.0)),
            (gamma2 * cos(5 * pi / 12.0), gamma2 * sin(5 * pi / 12.0)),
            (gamma2 * cos(-5 * pi / 12.0), gamma2 * sin(-5 * pi / 12.0)),
            (gamma2 * cos(7 * pi / 12.0), gamma2 * sin(7 * pi / 12.0)),
            (gamma2 * cos(-7 * pi / 12.0), gamma2 * sin(-7 * pi / 12.0)),
            (gamma1 * cos(pi / 4.0), gamma1 * sin(pi / 4.0)),
            (gamma1 * cos(-pi / 4.0), gamma1 * sin(-pi / 4.0)),
            (gamma1 * cos(3 * pi / 4.0), gamma1 * sin(3 * pi / 4.0)),
            (gamma1 * cos(-3 * pi / 4.0), gamma1 * sin(-3 * pi / 4.0)),
        )

    if constellation == ConstellationType.MOD_32APSK:
        gamma1, gamma2, gamma3 = _getGammaValues(constellation, code_rate)
        return (
            (gamma2 * cos(pi / 4.0), gamma2 * sin(pi / 4.0)),
            (gamma2 * cos(5 * pi / 12.0), gamma2 * sin(5 * pi / 12.0)),
            (gamma2 * cos(-pi / 4.0), gamma2 * sin(-pi / 4.0)),
            (gamma2 * cos(-5 * pi / 12.0), gamma2 * sin(-5 * pi / 12.0)),
            (gamma2 * cos(3 * pi / 4.0), gamma2 * sin(3 * pi / 4.0)),
            (gamma2 * cos(7 * pi / 12.0), gamma2 * sin(7 * pi / 12.0)),
            (gamma2 * cos(-3 * pi / 4.0), gamma2 * sin(-3 * pi / 4.0)),
            (gamma2 * cos(-7 * pi / 12.0), gamma2 * sin(-7 * pi / 12.0)),
            (gamma3 * cos(pi / 8.0), gamma3 * sin(pi / 8.0)),
            (gamma3 * cos(3 * pi / 8.0), gamma3 * sin(3 * pi / 8.0)),
            (gamma3 * cos(-pi / 4.0), gamma3 * sin(-pi / 4.0)),
            (gamma3 * cos(-pi / 2.0), gamma3 * sin(-pi / 2.0)),
            (gamma3 * cos(3 * pi / 4.0), gamma3 * sin(3 * pi / 4.0)),
            (gamma3 * cos(pi / 2.0), gamma3 * sin(pi / 2.0)),
            (gamma3 * cos(-7 * pi / 8.0), gamma3 * sin(-7 * pi / 8.0)),
            (gamma3 * cos(-5 * pi / 8.0), gamma3 * sin(-5 * pi / 8.0)),
            (gamma2 * cos(pi / 12.0), gamma2 * sin(pi / 12.0)),
            (gamma1 * cos(pi / 4.0)),
            (gamma1 * sin(pi / 4.0)),
            (gamma2 * cos(-pi / 12.0), gamma2 * sin(-pi / 12.0)),
            (gamma1 * cos(-pi / 4.0)),
            (gamma1 * sin(-pi / 4.0)),
            (gamma2 * cos(11 * pi / 12.0), gamma2 * sin(11 * pi / 12.0)),
            (gamma1 * cos(3 * pi / 4.0)),
            (gamma1 * sin(3 * pi / 4.0)),
            (gamma2 * cos(-11 * pi / 12.0), gamma2 * sin(-11 * pi / 12.0)),
            (gamma1 * cos(-3 * pi / 4.0)),
            (gamma1 * sin(-3 * pi / 4.0)),
            (gamma3 * cos(0.0), gamma3 * sin(0.0)),
            (gamma3 * cos(pi / 4.0), gamma3 * sin(pi / 4.0)),
            (gamma3 * cos(-pi / 8.0), gamma3 * sin(-pi / 8.0)),
            (gamma3 * cos(-3 * pi / 8.0), gamma3 * sin(-3 * pi / 8.0)),
            (gamma3 * cos(7 * pi / 8.0), gamma3 * sin(7 * pi / 8.0)),
            (gamma3 * cos(5 * pi / 8.0), gamma3 * sin(5 * pi / 8.0)),
            (gamma3 * cos(pi), gamma3 * sin(pi)),
            (gamma3 * cos(-3 * pi / 4.0), gamma3 * sin(-3 * pi / 4.0)),
        )

    assert False, f"Don't know how to handle {constellation}, {code_rate}"


def _toFixedPoint(f, e):
    a = f * (2 ** e)
    b = int(round(a))
    if a < 0:
        # next three lines turns b into it's 2's complement.
        b = abs(b)
        b = ~b
        b = b + 1
        b += 2 ** e
    return b


def _generatePackage():
    package = ""
    package_body = ""

    for i, (i_value, q_value) in enumerate(_getTable(ConstellationType.MOD_8PSK)):
        i_value_h = _toFixedPoint(i_value, 16)
        q_value_h = _toFixedPoint(q_value, 16)

        #  if i_value_h < 0:
        #      i_value_h += 2**16
        #  if q_value_h < 0:
        #      q_value_h += 2**16
        #  packed = struct.pack("ii", q_value, i_value)
        #  q_value_h = struct.unpack("I", packed[:4])
        #  i_value_h = struct.unpack("I", packed[4:])

        #  if isinstance(q_value_h, tuple):
        #      q_value_h = q_value_h[0]
        #  if isinstance(i_value_h, tuple):
        #      i_value_h = i_value_h[0]
        print(
            "%3s  || " % bin(i)[2:],
            "%7.4f\t%7.4f" % (i_value, q_value),
            "\t|| %5.4x\t%5.4x" % (int(i_value_h), int(q_value_h)),
            "\t|| %5d  %5d" % (int(i_value_h), int(q_value_h)),
        )

    # Generate the ROM table for 8 PSK

    #  0 => get_iq_pair(      MATH_PI / 4.0),
    #  1 => get_iq_pair(0.0),
    #  2 => get_iq_pair(4.0 * MATH_PI / 4.0),
    #  3 => get_iq_pair(5.0 * MATH_PI / 4.0),
    #  4 => get_iq_pair(2.0 * MATH_PI / 4.0),
    #  5 => get_iq_pair(7.0 * MATH_PI / 4.0),
    #  6 => get_iq_pair(3.0 * MATH_PI / 4.0),
    #  7 => get_iq_pair(6.0 * MATH_PI / 4.0)
    return package, package_body


def main():
    lines = str(HEADER)

    lines += "\n".join(
        [
            "library ieee;",
            "use ieee.std_logic_1164.all;",
            "use ieee.numeric_std.all;",
            "",
            "library fpga_cores;",
            "use fpga_cores.common_pkg.all;",
            "",
            "use work.dvb_utils_pkg.all;",
            "",
        ]
    )

    lines += "\n".join(["", "", "package constellation_mapper_tables_pkg is", "", "",])

    package, package_body = _generatePackage()

    lines += "\n".join(
        [
            "",
            package,
            "",
            "end package constellation_mapper_tables_pkg;",
            "",
            "package body constellation_mapper_tables_pkg is",
            package_body,
            "end package body constellation_mapper_tables_pkg;",
        ]
    )

    target_file = p.abspath(
        p.join(ROOT, "..", "rtl", "constellation_mapper_tables_pkg.vhd")
    )
    if not p.exists(p.dirname(target_file)):
        makedirs(p.dirname(target_file))
    open(target_file, "w").write(lines)


main()
