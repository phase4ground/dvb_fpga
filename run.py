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
"VUnit test runner for DVB FPGA"

# pylint: disable=bad-continuation

import logging
import os
import os.path as p
import random
import re
import struct
import subprocess as subp
import sys
import time
from multiprocessing import Pool
from typing import NamedTuple

from vunit import VUnit, VUnitCLI  # type: ignore

ROOT = p.abspath(p.dirname(__file__))

sys.path.insert(0, p.join(ROOT, "misc"))

from dvb_common import CodeRate, ConstellationType, FrameLength  # type: ignore


_logger = logging.getLogger(__name__)


class TestDefinition(
    NamedTuple(
        "TestDefinition",
        [
            ("name", str),
            ("test_files_path", str),
            ("code_rate", CodeRate),
            ("frame_length", FrameLength),
            ("constellation", ConstellationType),
        ],
    )
):
    """
    Placeholder for a test config
    """

    def __init__(self, *args, **kwargs):  # pylint: disable=unused-argument
        super(TestDefinition, self).__init__()
        self.timestamp = p.join(self.test_files_path, "timestamp")

    def getTestConfigString(self):
        """
        Returns the value for the 'test_cfg' string of the testbench
        """
        return ",".join(
            [
                self.constellation.name,
                self.frame_length.name,
                self.code_rate.name,
                self.test_files_path,
            ]
        )


def _getConfigs(
    code_rates=CodeRate, frame_lengths=FrameLength, constellations=ConstellationType
):
    for code_rate in code_rates:
        for frame_length in frame_lengths:
            for constellation in constellations:
                if (
                    frame_length is FrameLength.FECFRAME_SHORT
                    and code_rate is CodeRate.C9_10
                ):
                    continue

                test_files_path = p.join(
                    ROOT,
                    "gnuradio_data",
                    f"{frame_length.name}_{constellation.name}_{code_rate.name}".upper(),
                )

                name = ",".join(
                    [
                        f"FrameLength={frame_length.value}",
                        f"ConstellationType={constellation.value}",
                        f"CodeRate={code_rate.value}",
                    ]
                )

                yield TestDefinition(
                    name, test_files_path, code_rate, frame_length, constellation
                )


TEST_CONFIGS = set(_getConfigs())


def _runGnuRadio(config):
    """
    Runs gnuradio_data/dvbs2_tx.py script via shell. Reason for not importing
    and running locally is to allow GNI Radio's Python environment to be
    independent of VUnit's Python env.
    """
    print("Generating data for %s" % config.name)

    command = [
        p.join(ROOT, "gnuradio_data", "dvbs2_tx.py"),
        "--frame-type",
        config.frame_length.name,
        "--constellation",
        config.constellation.name,
        "--code-rate",
        config.code_rate.name,
    ]

    if not p.exists(config.test_files_path):
        os.makedirs(config.test_files_path)

    try:
        subp.check_call(command, cwd=config.test_files_path)
        open(config.timestamp, "w").write(time.asctime())
    except subp.CalledProcessError:
        _logger.exception("Failed to run %s", command)
        raise


def _generateGnuRadioData():
    """
    Generates GNU Radio data for configs whose test files can't found
    """
    configs = []

    for config in TEST_CONFIGS:
        if not p.exists(config.timestamp):
            configs += [config]

    # Generate needed data on a process pool to speed up things
    pool = Pool()
    pool.map(_runGnuRadio, configs)
    pool.close()
    pool.join()


LDPC_Q = {
    (FrameLength.FECFRAME_NORMAL, CodeRate.C1_4): 135,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C1_3): 120,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C2_5): 108,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C1_2): 90,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C3_5): 72,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C2_3): 60,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C3_4): 45,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C4_5): 36,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C5_6): 30,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C8_9): 20,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C9_10): 18,
    (FrameLength.FECFRAME_SHORT, CodeRate.C1_4): 36,
    (FrameLength.FECFRAME_SHORT, CodeRate.C1_3): 30,
    (FrameLength.FECFRAME_SHORT, CodeRate.C2_5): 27,
    (FrameLength.FECFRAME_SHORT, CodeRate.C1_2): 25,
    (FrameLength.FECFRAME_SHORT, CodeRate.C3_5): 18,
    (FrameLength.FECFRAME_SHORT, CodeRate.C2_3): 15,
    (FrameLength.FECFRAME_SHORT, CodeRate.C3_4): 12,
    (FrameLength.FECFRAME_SHORT, CodeRate.C4_5): 10,
    (FrameLength.FECFRAME_SHORT, CodeRate.C5_6): 8,
    (FrameLength.FECFRAME_SHORT, CodeRate.C8_9): 5,
}

LDPC_LENGTH = {
    (FrameLength.FECFRAME_NORMAL, CodeRate.C1_4): 64_800 - 16_200,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C1_3): 64_800 - 21_600,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C2_5): 64_800 - 25_920,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C1_2): 64_800 - 32_400,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C3_5): 64_800 - 38_880,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C2_3): 64_800 - 43_200,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C3_4): 64_800 - 48_600,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C4_5): 64_800 - 51_840,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C5_6): 64_800 - 54_000,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C8_9): 64_800 - 57_600,
    (FrameLength.FECFRAME_NORMAL, CodeRate.C9_10): 64_800 - 58_320,
    (FrameLength.FECFRAME_SHORT, CodeRate.C1_4): 16_200 - 3_240,
    (FrameLength.FECFRAME_SHORT, CodeRate.C1_3): 16_200 - 5_400,
    (FrameLength.FECFRAME_SHORT, CodeRate.C2_5): 16_200 - 6_480,
    (FrameLength.FECFRAME_SHORT, CodeRate.C1_2): 16_200 - 7_200,
    (FrameLength.FECFRAME_SHORT, CodeRate.C3_5): 16_200 - 9_720,
    (FrameLength.FECFRAME_SHORT, CodeRate.C2_3): 16_200 - 10_800,
    (FrameLength.FECFRAME_SHORT, CodeRate.C3_4): 16_200 - 11_880,
    (FrameLength.FECFRAME_SHORT, CodeRate.C4_5): 16_200 - 12_600,
    (FrameLength.FECFRAME_SHORT, CodeRate.C5_6): 16_200 - 13_320,
    (FrameLength.FECFRAME_SHORT, CodeRate.C8_9): 16_200 - 14_400,
}


def _populateLdpcTable(frame_length, code_rate, src, dest):
    """
    Creates the unrolled binary LDPC table file for the LDPC encoder testbench
    a CSV file with coefficients (from DVB-S2 spec's appendices B and C).
    """
    bin_table = p.join(dest, "ldpc_table.bin")
    text_table = p.join(dest, "ldpc_table.txt")

    if p.exists(bin_table) and p.exists(text_table):
        return

    print(
        f"Generating LDPC table for FECFRAME={frame_length.value}, "
        f'code rate={code_rate.value} using "{src}" as reference. '
        f'Binary data will be written to "{dest}"',
    )

    table = [x.split(",") for x in open(src, "r").read().split("\n") if x]

    table_q = LDPC_Q[(frame_length, code_rate)]
    table_length = LDPC_LENGTH[(frame_length, code_rate)]
    word_cnt = 0

    bin_fd = open(bin_table, "wb")
    text_fd = open(text_table, "w")

    try:
        # Each offset is 16 bits (to represent 64,800 bits of FECFRAME_NORMAL),
        # but we'll also embed the s_ldpc_next values into the file as well on a
        # byte, so data width will 24: data[16] is s_ldpc_next while data[15:0] is
        # the actual offset
        bit_index = 0

        for line in table:
            for _ in range(360):
                text_fd.write(f"{bit_index:5d} || ")
                for i, coefficient in enumerate(tuple(int(x) for x in line)):
                    offset = (coefficient + (bit_index % 360) * table_q) % table_length
                    # Just to recap:
                    # - "H" -> Unsigned short
                    # - "?" -> boolean
                    bin_fd.write(
                        struct.pack(">?HH", i == len(line) - 1, bit_index, offset)
                    )

                    text_fd.write(f" {word_cnt:5d}, {offset:5d}  |")
                    word_cnt += 1

                text_fd.write("\n")
                bit_index += 1
    finally:
        bin_fd.close()
        text_fd.close()


def _createLdpcTables():
    """
    Creates the binary LDPC table files if they don't already exist
    """
    path_to_csv = p.join(ROOT, "misc", "ldpc")
    pool = Pool()

    for config in TEST_CONFIGS:
        csv_table = (
            f"{path_to_csv}/"
            f"ldpc_table_{config.frame_length.name}_{config.code_rate.name}.csv"
        )

        pool.apply_async(
            _populateLdpcTable,
            (config.frame_length, config.code_rate, csv_table, config.test_files_path),
        )

    pool.close()
    pool.join()


class GhdlPragmaHandler:
    """
    Removes code between arbitraty pragmas
    -- ghdl translate_off
    this is ignored
    -- ghdl translate_on
    """

    _PRAGMA = re.compile(
        r"\s*--\s*ghdl\s+translate_off[\r\n].*?[\n\r]\s*--\s*ghdl\s+translate_on",
        flags=re.DOTALL | re.I | re.MULTILINE,
    )

    def run(self, code, file_name):  # pylint: disable=unused-argument,no-self-use
        """
        Removes text between "-- ghdl translate_off" and "-- ghdl translate_on"
        """
        # Lazy check to avoid running the regex unnecessarily
        for word in ("ghdl", "translate_on", "translate_off"):
            if word not in code:
                return code

        lines = code.split("\n")

        # Prepend the comment characters to lines within matches so line
        # numbers are kept
        for match in self._PRAGMA.finditer(code):
            for lnum in range(
                code[: match.start()].count("\n") + 1, code[: match.end()].count("\n")
            ):
                lines[lnum] = "-- " + lines[lnum]

        return "\n".join(lines)


def main():
    "Main entry point for DVB FPGA test runner"

    _generateGnuRadioData()
    _createLdpcTables()

    cli = VUnitCLI()
    cli.parser.add_argument(
        "--individual-config-runs",
        "-i",
        action="store_true",
        help="Create individual test runs for each configuration. By default, "
        "all combinations of frame lengths, code rates and modulations are "
        "tested in the same simulation",
    )
    args = cli.parse_args()

    vunit = VUnit.from_args(args=args)
    vunit.add_osvvm()
    vunit.add_com()
    vunit.enable_location_preprocessing()
    if vunit.get_simulator_name() == "ghdl":
        vunit.add_preprocessor(GhdlPragmaHandler())

    library = vunit.add_library("lib")
    library.add_source_files(p.join(ROOT, "rtl", "*.vhd"))
    library.add_source_files(p.join(ROOT, "rtl", "ldpc", "*.vhd"))
    library.add_source_files(p.join(ROOT, "rtl", "bch_generated", "*.vhd"))
    library.add_source_files(p.join(ROOT, "testbench", "*.vhd"))

    vunit.add_library("str_format").add_source_files(
        p.join(ROOT, "third_party", "hdl_string_format", "src", "*.vhd")
    )

    vunit.add_library("fpga_cores").add_source_files(
        p.join(ROOT, "third_party", "fpga_cores", "src", "*.vhd")
    )
    vunit.add_library("fpga_cores_sim").add_source_files(
        p.join(ROOT, "third_party", "fpga_cores", "sim", "*.vhd")
    )

    if args.individual_config_runs:
        # BCH encoding doesn't depend on the constellation type, choose any
        for config in _getConfigs(constellations=(ConstellationType.MOD_8PSK,)):
            vunit.library("lib").entity("axi_bch_encoder_tb").add_config(
                name=config.name,
                generics=dict(
                    test_cfg=config.getTestConfigString(), NUMBER_OF_TEST_FRAMES=8,
                ),
            )

            vunit.library("lib").entity("dvbs2_tx_tb").add_config(
                name=config.name,
                generics=dict(
                    test_cfg=config.getTestConfigString(), NUMBER_OF_TEST_FRAMES=2,
                ),
            )

        # Only generate configs for 8 PSK since LDPC does not depend on this
        # parameter
        for config in _getConfigs(constellations=(ConstellationType.MOD_8PSK,),):
            vunit.library("lib").entity("axi_ldpc_encoder_core_tb").add_config(
                name=config.name,
                generics=dict(
                    test_cfg=config.getTestConfigString(), NUMBER_OF_TEST_FRAMES=3,
                ),
            )
            vunit.library("lib").entity("axi_ldpc_table_tb").add_config(
                name=config.name,
                generics=dict(
                    test_cfg=config.getTestConfigString(), NUMBER_OF_TEST_FRAMES=3,
                ),
            )

    else:
        addAllConfigsTest(
            entity=vunit.library("lib").entity("axi_bch_encoder_tb"),
            configs=TEST_CONFIGS,
        )

        addAllConfigsTest(
            entity=vunit.library("lib").entity("axi_ldpc_encoder_core_tb"),
            configs=_getConfigs(constellations=(ConstellationType.MOD_8PSK,),),
        )

        addAllConfigsTest(
            entity=vunit.library("lib").entity("axi_ldpc_table_tb"),
            configs=_getConfigs(
                constellations=(ConstellationType.MOD_8PSK,),
                #  frame_lengths=(FrameLength.FECFRAME_SHORT,),
            ),
        )

        # Run the DVB S2 Tx testbench with a smaller sample of configs to check
        # integration, otherwise sim takes way too long. Note that when
        # --individual-config-runs is passed, all configs are added
        addAllConfigsTest(
            entity=vunit.library("lib").entity("dvbs2_tx_tb"),
            configs=_getConfigs(code_rates=(CodeRate.C1_4, CodeRate.C9_10)),
        )

    addAllConfigsTest(
        entity=vunit.library("lib").entity("axi_baseband_scrambler_tb"),
        configs=TEST_CONFIGS,
    )

    # Generate bit interleaver tests
    for data_width in (8,):
        all_configs = []
        for config in _getConfigs():
            all_configs += [config.getTestConfigString()]

            if args.individual_config_runs:
                vunit.library("lib").entity("axi_bit_interleaver_tb").add_config(
                    name=f"data_width={data_width},{config.name}",
                    generics=dict(
                        DATA_WIDTH=data_width,
                        test_cfg=config.getTestConfigString(),
                        NUMBER_OF_TEST_FRAMES=8,
                    ),
                )

        if not args.individual_config_runs:
            vunit.library("lib").entity("axi_bit_interleaver_tb").add_config(
                name=f"data_width={data_width},all_parameters",
                generics=dict(
                    DATA_WIDTH=data_width,
                    test_cfg="|".join(all_configs),
                    NUMBER_OF_TEST_FRAMES=2,
                ),
            )

    # Gerate constellation mapper tests
    for data_width in (32, 64):
        all_configs = []
        for config in _getConfigs():
            all_configs += [config.getTestConfigString()]
            vunit.library("lib").entity("constellation_mapper_tb").add_config(
                name=f"data_width={data_width},{config.name}",
                generics=dict(
                    DATA_WIDTH=data_width,
                    test_cfg=config.getTestConfigString(),
                    NUMBER_OF_TEST_FRAMES=2,
                ),
            )

        vunit.library("lib").entity("constellation_mapper_tb").add_config(
            name=f"data_width={data_width},all_parameters",
            generics=dict(
                DATA_WIDTH=data_width,
                test_cfg="|".join(all_configs),
                NUMBER_OF_TEST_FRAMES=2,
            ),
        )

    vunit.set_compile_option("modelsim.vcom_flags", ["-explicit"])

    # Not all options are supported by all GHDL backends
    vunit.set_sim_option("ghdl.elab_flags", ["-frelaxed-rules"])
    vunit.set_compile_option("ghdl.a_flags", ["-frelaxed-rules", "-O2", "-g"])

    # Make components not bound (error 3473) an error
    vsim_flags = ["-error", "3473"]
    if args.gui:
        vsim_flags += ['-voptargs="+acc=n"']

    vunit.set_sim_option("modelsim.vsim_flags", vsim_flags)

    vunit.set_sim_option("disable_ieee_warnings", True)
    vunit.set_sim_option("modelsim.init_file.gui", p.join(ROOT, "wave.do"))
    vunit.main()


def addAllConfigsTest(entity, configs):
    """
    Adds a test config with all combinations of configurations (assuming both
    input and reference files ca be found)
    """
    params = []

    for config in configs:
        params += [config.getTestConfigString()]

    random.shuffle(params)

    assert params, "Could not find any config files!"

    entity.add_config(
        name="test_all_configs",
        generics=dict(test_cfg="|".join(params), NUMBER_OF_TEST_FRAMES=1),
    )


if __name__ == "__main__":
    sys.exit(main())
