#!/usr/bin/env python2
# -*- coding: utf-8 -*-
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

##################################################
# GNU Radio Python Flow Graph
# Title: Dvbs2 Tx
# Generated: Tue Jan 28 10:36:23 2020
##################################################


import math
from optparse import OptionParser

import numpy

from gnuradio import analog, blocks, digital, dtv, eng_notation, filter, gr
from gnuradio.eng_option import eng_option
from gnuradio.filter import firdes

CODE_RATES = {
    dtv.C1_2: 1.0 / 2,
    dtv.C1_3: 1.0 / 3,
    dtv.C1_4: 1.0 / 4,
    dtv.C2_3: 2.0 / 3,
    dtv.C2_5: 2.0 / 5,
    dtv.C3_4: 3.0 / 4,
    dtv.C3_5: 3.0 / 5,
    dtv.C4_5: 4.0 / 5,
    dtv.C5_6: 5.0 / 6,
    dtv.C8_9: 8.0 / 9,
    dtv.C9_10: 9.0 / 10,
}

BBFRAME_LENGTH = {
    dtv.FECFRAME_NORMAL: {
        dtv.C1_4: 16008,
        dtv.C1_3: 21408,
        dtv.C2_5: 25728,
        dtv.C1_2: 32208,
        dtv.C3_5: 38688,
        dtv.C2_3: 43040,
        dtv.C3_4: 48408,
        dtv.C4_5: 51648,
        dtv.C5_6: 53840,
        dtv.C8_9: 57472,
        dtv.C9_10: 58192,
    },
    dtv.FECFRAME_SHORT: {
        dtv.C1_4: 3072,
        dtv.C1_3: 5232,
        dtv.C2_5: 6312,
        dtv.C1_2: 7032,
        dtv.C3_5: 9552,
        dtv.C2_3: 10632,
        dtv.C3_4: 11712,
        dtv.C4_5: 12432,
        dtv.C5_6: 13152,
        dtv.C8_9: 14232,
        dtv.C9_10: 14412,
    },
}

PLFRAME_SLOT_NUMBER = {
    (dtv.FECFRAME_SHORT, dtv.MOD_QPSK): 90,
    (dtv.FECFRAME_SHORT, dtv.MOD_8PSK): 60,
    (dtv.FECFRAME_SHORT, dtv.MOD_16APSK): 45,
    (dtv.FECFRAME_SHORT, dtv.MOD_32APSK): 36,
    (dtv.FECFRAME_NORMAL, dtv.MOD_QPSK): 360,
    (dtv.FECFRAME_NORMAL, dtv.MOD_8PSK): 240,
    (dtv.FECFRAME_NORMAL, dtv.MOD_16APSK): 180,
    (dtv.FECFRAME_NORMAL, dtv.MOD_32APSK): 144,
}


def get_ratio(constellation):
    ratio = None

    if constellation == dtv.MOD_QPSK:
        ratio = 2, 8
    if constellation == dtv.MOD_8PSK:
        ratio = 3, 8
    if constellation == dtv.MOD_16APSK:
        ratio = 4, 8
    if constellation == dtv.MOD_32APSK:
        ratio = 5, 8

    assert ratio is not None, "??"
    return ratio


class UnknownFrameLength(Exception):
    def __init__(self, frame_type, code_rate):
        if frame_type == dtv.FECFRAME_NORMAL:
            self.frame_type = "FECFRAME_NORMAL"
        elif frame_type == dtv.FECFRAME_SHORT:
            self.frame_type = "FECFRAME_SHORT"
        else:
            self.frame_type = frame_type

        self.frame_type = frame_type
        self.code_rate = code_rate

    def __str__(self):
        return "Could not get frame length for {}, {}".format(
            self.frame_type, self.code_rate
        )


class dvbs2_tx(gr.top_block):
    def __init__(self, constellation, frame_type, code_rate):
        gr.top_block.__init__(self, "Dvbs2 Tx")

        ##################################################
        # Parameters
        ##################################################
        # Header is 10 bytes
        try:
            frame_length = 1.0 * BBFRAME_LENGTH[frame_type][code_rate] - 80
        except KeyError:
            raise UnknownFrameLength(frame_type, code_rate)

        #  print("Base frame length: %s" % frame_length)
        frame_length /= 8
        #  print("Base frame length: %s" % frame_length)
        assert (
            int(frame_length) == 1.0 * frame_length
        ), "Frame length {0} won't work because {0}/8 = {1}!".format(
            frame_length, frame_length / 8.0
        )
        frame_length = int(frame_length)
        self.frame_length = frame_length

        bits_per_input, bits_per_output = get_ratio(constellation)

        ##################################################
        # Variables
        ##################################################
        self.frame_type = frame_type
        self.constellation = constellation
        self.symbol_rate = symbol_rate = 5000000
        self.taps = taps = 100
        self.samp_rate = samp_rate = symbol_rate * 2
        self.rolloff = rolloff = 0.2
        self.noise = noise = 0
        self.gain = gain = 1
        self.center_freq = center_freq = 1280e6

        self.physical_layer_gold_code = physical_layer_gold_code = 0
        self.physical_layer_header_length = physical_layer_header_length = 90

        ##################################################
        # Blocks
        ##################################################
        self.undo_bit_stuffing_pilots_off = blocks.keep_m_in_n(
            gr.sizeof_gr_complex, 1, 2, 0
        )
        self.undo_bit_stuffing_pilots_on = blocks.keep_m_in_n(
            gr.sizeof_gr_complex, 1, 2, 0
        )
        self.plframe_pilots_on_float = blocks.file_sink(
            gr.sizeof_gr_complex * 1, "plframe_pilots_on_float.bin", False
        )
        self.plframe_pilots_on_float.set_unbuffered(False)
        self.plframe_pilots_on_fixed_point = blocks.file_sink(
            gr.sizeof_short * 1, "plframe_pilots_on_fixed_point.bin", False
        )
        self.plframe_pilots_on_fixed_point.set_unbuffered(False)
        self.plframe_pilots_off_float = blocks.file_sink(
            gr.sizeof_gr_complex * 1, "plframe_pilots_off_float.bin", False
        )
        self.plframe_pilots_off_float.set_unbuffered(False)
        self.plframe_pilots_off_fixed_point = blocks.file_sink(
            gr.sizeof_short * 1, "plframe_pilots_off_fixed_point.bin", False
        )
        self.plframe_pilots_off_fixed_point.set_unbuffered(False)
        self.plframe_payload_pilots_on_float = blocks.file_sink(
            gr.sizeof_gr_complex * 1, "plframe_payload_pilots_on_float.bin", False
        )
        self.plframe_payload_pilots_on_float.set_unbuffered(False)
        self.plframe_payload_pilots_on_fixed_point = blocks.file_sink(
            gr.sizeof_short * 1, "plframe_payload_pilots_on_fixed_point.bin", False
        )
        self.plframe_payload_pilots_on_fixed_point.set_unbuffered(False)
        self.plframe_payload_pilots_off_float = blocks.file_sink(
            gr.sizeof_gr_complex * 1, "plframe_payload_pilots_off_float.bin", False
        )
        self.plframe_payload_pilots_off_float.set_unbuffered(False)
        self.plframe_payload_pilots_off_fixed_point = blocks.file_sink(
            gr.sizeof_short * 1, "plframe_payload_pilots_off_fixed_point.bin", False
        )
        self.plframe_payload_pilots_off_fixed_point.set_unbuffered(False)
        self.plframe_header_pilots_on_float = blocks.file_sink(
            gr.sizeof_gr_complex * 1, "plframe_header_pilots_on_float.bin", False
        )
        self.plframe_header_pilots_on_float.set_unbuffered(False)
        self.plframe_header_pilots_on_fixed_point = blocks.file_sink(
            gr.sizeof_short * 1, "plframe_header_pilots_on_fixed_point.bin", False
        )
        self.plframe_header_pilots_on_fixed_point.set_unbuffered(False)
        self.plframe_header_pilots_off_float = blocks.file_sink(
            gr.sizeof_gr_complex * 1, "plframe_header_pilots_off_float.bin", False
        )
        self.plframe_header_pilots_off_float.set_unbuffered(False)
        self.plframe_header_pilots_off_fixed_point = blocks.file_sink(
            gr.sizeof_short * 1, "plframe_header_pilots_off_fixed_point.bin", False
        )
        self.plframe_header_pilots_off_fixed_point.set_unbuffered(False)
        self.pl_complex_to_float_1 = blocks.complex_to_float(1)
        self.pl_complex_to_float_0_1 = blocks.complex_to_float(1)
        self.pl_complex_to_float_0_0_0 = blocks.complex_to_float(1)
        self.pl_complex_to_float_0_0 = blocks.complex_to_float(1)
        self.pl_complex_to_float_0 = blocks.complex_to_float(1)
        self.pl_complex_to_float = blocks.complex_to_float(1)
        self.organize = blocks.multiply_const_vcc((1,))
        self.ldpc_encoder_input = blocks.file_sink(
            gr.sizeof_char * 1, "ldpc_encoder_input.bin", False
        )
        self.ldpc_encoder_input.set_unbuffered(False)
        self.keep_plframe_payload_pilots_off = blocks.keep_m_in_n(
            gr.sizeof_gr_complex,
            self.get_physical_layer_frame_size(dtv.PILOTS_OFF)
            - physical_layer_header_length,
            self.get_physical_layer_frame_size(dtv.PILOTS_OFF),
            physical_layer_header_length,
        )
        self.keep_plframe_payload_pilots_on = blocks.keep_m_in_n(
            gr.sizeof_gr_complex,
            self.get_physical_layer_frame_size(dtv.PILOTS_ON)
            - physical_layer_header_length,
            self.get_physical_layer_frame_size(dtv.PILOTS_ON),
            physical_layer_header_length,
        )
        self.keep_plframe_header_pilots_off = blocks.keep_m_in_n(
            gr.sizeof_gr_complex,
            physical_layer_header_length,
            self.get_physical_layer_frame_size(dtv.PILOTS_OFF),
            0,
        )
        self.keep_plframe_header_pilots_on = blocks.keep_m_in_n(
            gr.sizeof_gr_complex,
            physical_layer_header_length,
            self.get_physical_layer_frame_size(dtv.PILOTS_ON),
            0,
        )
        self.dtv_dvbs2_physical_cc_with_pilots = dtv.dvbs2_physical_cc(
            frame_type,
            code_rate,
            constellation,
            dtv.PILOTS_ON,
            physical_layer_gold_code,
        )
        self.dtv_dvbs2_physical_cc_pilots_off = dtv.dvbs2_physical_cc(
            frame_type,
            code_rate,
            constellation,
            dtv.PILOTS_OFF,
            physical_layer_gold_code,
        )
        self.dtv_dvbs2_modulator_bc_0 = dtv.dvbs2_modulator_bc(
            frame_type, code_rate, constellation, dtv.INTERPOLATION_OFF
        )
        self.dtv_dvbs2_interleaver_bb_0 = dtv.dvbs2_interleaver_bb(
            frame_type, code_rate, constellation
        )
        self.dtv_dvb_ldpc_bb_0 = dtv.dvb_ldpc_bb(
            dtv.STANDARD_DVBS2, frame_type, code_rate, dtv.MOD_OTHER
        )
        self.dtv_dvb_bch_bb_0 = dtv.dvb_bch_bb(
            dtv.STANDARD_DVBS2, frame_type, code_rate
        )
        self.dtv_dvb_bbscrambler_bb_0 = dtv.dvb_bbscrambler_bb(
            dtv.STANDARD_DVBS2, frame_type, code_rate
        )
        self.dtv_dvb_bbheader_bb_0 = dtv.dvb_bbheader_bb(
            dtv.STANDARD_DVBS2,
            frame_type,
            code_rate,
            dtv.RO_0_20,
            dtv.INPUTMODE_NORMAL,
            dtv.INBAND_OFF,
            168,
            4000000,
        )
        self.blocks_stream_mux_0_2 = blocks.stream_mux(gr.sizeof_short * 1, (1, 1))
        self.blocks_stream_mux_0_1 = blocks.stream_mux(gr.sizeof_short * 1, (1, 1))
        self.blocks_stream_mux_0_0_1 = blocks.stream_mux(gr.sizeof_short * 1, (1, 1))
        self.blocks_stream_mux_0_0_0_0 = blocks.stream_mux(gr.sizeof_short * 1, (1, 1))
        self.blocks_stream_mux_0_0_0 = blocks.stream_mux(gr.sizeof_short * 1, (1, 1))
        self.blocks_stream_mux_0_0 = blocks.stream_mux(gr.sizeof_short * 1, (1, 1))
        self.blocks_stream_mux_0 = blocks.stream_mux(gr.sizeof_short * 1, (1, 1))
        self.blocks_repack_bits_bb_0 = blocks.repack_bits_bb(
            bits_per_input, bits_per_output, "", False, gr.GR_MSB_FIRST
        )
        self.blocks_float_to_short_0_3 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_2 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_1_1 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_1_0_0 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_1_0 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_1 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_0_2 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_0_1 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_0_0_1 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_0_0_0_0 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_0_0_0 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_0_0 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0_0 = blocks.float_to_short(1, 32768)
        self.blocks_float_to_short_0 = blocks.float_to_short(1, 32768)
        self.bit_mapper_output_fixed = blocks.file_sink(
            gr.sizeof_short * 1, "bit_mapper_output_fixed.bin", False
        )
        self.bit_mapper_output_fixed.set_unbuffered(False)
        self.bit_mapper_output_float = blocks.file_sink(
            gr.sizeof_gr_complex * 1, "bit_mapper_output_float.bin", False
        )
        self.bit_mapper_output_float.set_unbuffered(False)
        self.bit_mapper_complex_to_float = blocks.complex_to_float(1)
        self.bit_interleaver_output_packed = blocks.file_sink(
            gr.sizeof_char * 1, "bit_interleaver_output_packed.bin", False
        )
        self.bit_interleaver_output_packed.set_unbuffered(False)
        self.bit_interleaver_output = blocks.file_sink(
            gr.sizeof_char * 1, "bit_interleaver_output.bin", False
        )
        self.bit_interleaver_output.set_unbuffered(False)
        self.bit_interleaver_input = blocks.file_sink(
            gr.sizeof_char * 1, "bit_interleaver_input.bin", False
        )
        self.bit_interleaver_input.set_unbuffered(False)
        self.bch_encoder_input = blocks.file_sink(
            gr.sizeof_char * 1, "bch_encoder_input.bin", False
        )
        self.bch_encoder_input.set_unbuffered(False)
        self.bb_scrambler_input_0 = blocks.file_sink(
            gr.sizeof_char * 1, "bb_scrambler_input.bin", False
        )
        self.bb_scrambler_input_0.set_unbuffered(False)
        self.analog_random_source_x_0 = blocks.vector_source_b(
            map(int, numpy.random.randint(0, 255, self.frame_length)), False
        )

        ##################################################
        # Connections
        ##################################################
        self.connect(
            (self.analog_random_source_x_0, 0), (self.dtv_dvb_bbheader_bb_0, 0)
        )
        self.connect(
            (self.bit_mapper_complex_to_float, 0), (self.blocks_float_to_short_0_0_2, 0)
        )
        self.connect(
            (self.bit_mapper_complex_to_float, 1), (self.blocks_float_to_short_0_3, 0)
        )
        self.connect((self.blocks_float_to_short_0, 0), (self.blocks_stream_mux_0, 1))
        self.connect((self.blocks_float_to_short_0_0, 0), (self.blocks_stream_mux_0, 0))
        self.connect(
            (self.blocks_float_to_short_0_0_0, 0), (self.blocks_stream_mux_0_0, 0)
        )
        self.connect(
            (self.blocks_float_to_short_0_0_0_0, 0), (self.blocks_stream_mux_0_0_0, 0)
        )
        self.connect(
            (self.blocks_float_to_short_0_0_0_0_0, 0),
            (self.blocks_stream_mux_0_0_0_0, 0),
        )
        self.connect(
            (self.blocks_float_to_short_0_0_0_1, 0), (self.blocks_stream_mux_0_0_1, 0)
        )
        self.connect(
            (self.blocks_float_to_short_0_0_1, 0), (self.blocks_stream_mux_0_1, 0)
        )
        self.connect(
            (self.blocks_float_to_short_0_0_2, 0), (self.blocks_stream_mux_0_2, 0)
        )
        self.connect(
            (self.blocks_float_to_short_0_1, 0), (self.blocks_stream_mux_0_0, 1)
        )
        self.connect(
            (self.blocks_float_to_short_0_1_0, 0), (self.blocks_stream_mux_0_0_0, 1)
        )
        self.connect(
            (self.blocks_float_to_short_0_1_0_0, 0), (self.blocks_stream_mux_0_0_0_0, 1)
        )
        self.connect(
            (self.blocks_float_to_short_0_1_1, 0), (self.blocks_stream_mux_0_0_1, 1)
        )
        self.connect(
            (self.blocks_float_to_short_0_2, 0), (self.blocks_stream_mux_0_1, 1)
        )
        self.connect(
            (self.blocks_float_to_short_0_3, 0), (self.blocks_stream_mux_0_2, 1)
        )
        self.connect(
            (self.blocks_repack_bits_bb_0, 0), (self.bit_interleaver_output_packed, 0)
        )
        self.connect(
            (self.blocks_stream_mux_0, 0), (self.plframe_pilots_on_fixed_point, 0)
        )
        self.connect(
            (self.blocks_stream_mux_0_0, 0),
            (self.plframe_payload_pilots_on_fixed_point, 0),
        )
        self.connect(
            (self.blocks_stream_mux_0_0_0, 0),
            (self.plframe_header_pilots_on_fixed_point, 0),
        )
        self.connect(
            (self.blocks_stream_mux_0_0_0_0, 0),
            (self.plframe_header_pilots_off_fixed_point, 0),
        )
        self.connect(
            (self.blocks_stream_mux_0_0_1, 0),
            (self.plframe_payload_pilots_off_fixed_point, 0),
        )
        self.connect(
            (self.blocks_stream_mux_0_1, 0), (self.plframe_pilots_off_fixed_point, 0)
        )
        self.connect((self.blocks_stream_mux_0_2, 0), (self.bit_mapper_output_fixed, 0))
        self.connect((self.dtv_dvb_bbheader_bb_0, 0), (self.bb_scrambler_input_0, 0))
        self.connect(
            (self.dtv_dvb_bbheader_bb_0, 0), (self.dtv_dvb_bbscrambler_bb_0, 0)
        )
        self.connect((self.dtv_dvb_bbscrambler_bb_0, 0), (self.bch_encoder_input, 0))
        self.connect((self.dtv_dvb_bbscrambler_bb_0, 0), (self.dtv_dvb_bch_bb_0, 0))
        self.connect((self.dtv_dvb_bch_bb_0, 0), (self.dtv_dvb_ldpc_bb_0, 0))
        self.connect((self.dtv_dvb_bch_bb_0, 0), (self.ldpc_encoder_input, 0))
        self.connect((self.dtv_dvb_ldpc_bb_0, 0), (self.bit_interleaver_input, 0))
        self.connect((self.dtv_dvb_ldpc_bb_0, 0), (self.dtv_dvbs2_interleaver_bb_0, 0))
        self.connect(
            (self.dtv_dvbs2_interleaver_bb_0, 0), (self.bit_interleaver_output, 0)
        )
        self.connect(
            (self.dtv_dvbs2_interleaver_bb_0, 0), (self.blocks_repack_bits_bb_0, 0)
        )
        self.connect(
            (self.dtv_dvbs2_interleaver_bb_0, 0), (self.dtv_dvbs2_modulator_bc_0, 0)
        )
        self.connect(
            (self.dtv_dvbs2_modulator_bc_0, 0), (self.bit_mapper_complex_to_float, 0)
        )
        self.connect(
            (self.dtv_dvbs2_modulator_bc_0, 0), (self.bit_mapper_output_float, 0)
        )
        self.connect((self.dtv_dvbs2_modulator_bc_0, 0), (self.organize, 0))
        self.connect(
            (self.dtv_dvbs2_physical_cc_pilots_off, 0),
            (self.undo_bit_stuffing_pilots_off, 0),
        )
        self.connect(
            (self.dtv_dvbs2_physical_cc_with_pilots, 0),
            (self.undo_bit_stuffing_pilots_on, 0),
        )
        self.connect(
            (self.keep_plframe_header_pilots_off, 0),
            (self.pl_complex_to_float_0_0_0, 0),
        )
        self.connect(
            (self.keep_plframe_header_pilots_off, 0),
            (self.plframe_header_pilots_off_float, 0),
        )
        self.connect(
            (self.keep_plframe_header_pilots_on, 0), (self.pl_complex_to_float_0_0, 0)
        )
        self.connect(
            (self.keep_plframe_header_pilots_on, 0),
            (self.plframe_header_pilots_on_float, 0),
        )
        self.connect(
            (self.keep_plframe_payload_pilots_off, 0), (self.pl_complex_to_float_0_1, 0)
        )
        self.connect(
            (self.keep_plframe_payload_pilots_off, 0),
            (self.plframe_payload_pilots_off_float, 0),
        )
        self.connect(
            (self.keep_plframe_payload_pilots_on, 0), (self.pl_complex_to_float_0, 0)
        )
        self.connect(
            (self.keep_plframe_payload_pilots_on, 0),
            (self.plframe_payload_pilots_on_float, 0),
        )
        self.connect((self.organize, 0), (self.dtv_dvbs2_physical_cc_pilots_off, 0))
        self.connect((self.organize, 0), (self.dtv_dvbs2_physical_cc_with_pilots, 0))
        self.connect((self.pl_complex_to_float, 1), (self.blocks_float_to_short_0, 0))
        self.connect((self.pl_complex_to_float, 0), (self.blocks_float_to_short_0_0, 0))
        self.connect(
            (self.pl_complex_to_float_0, 0), (self.blocks_float_to_short_0_0_0, 0)
        )
        self.connect(
            (self.pl_complex_to_float_0, 1), (self.blocks_float_to_short_0_1, 0)
        )
        self.connect(
            (self.pl_complex_to_float_0_0, 0), (self.blocks_float_to_short_0_0_0_0, 0)
        )
        self.connect(
            (self.pl_complex_to_float_0_0, 1), (self.blocks_float_to_short_0_1_0, 0)
        )
        self.connect(
            (self.pl_complex_to_float_0_0_0, 0),
            (self.blocks_float_to_short_0_0_0_0_0, 0),
        )
        self.connect(
            (self.pl_complex_to_float_0_0_0, 1), (self.blocks_float_to_short_0_1_0_0, 0)
        )
        self.connect(
            (self.pl_complex_to_float_0_1, 0), (self.blocks_float_to_short_0_0_0_1, 0)
        )
        self.connect(
            (self.pl_complex_to_float_0_1, 1), (self.blocks_float_to_short_0_1_1, 0)
        )
        self.connect(
            (self.pl_complex_to_float_1, 0), (self.blocks_float_to_short_0_0_1, 0)
        )
        self.connect(
            (self.pl_complex_to_float_1, 1), (self.blocks_float_to_short_0_2, 0)
        )
        self.connect(
            (self.undo_bit_stuffing_pilots_off, 0),
            (self.keep_plframe_header_pilots_off, 0),
        )
        self.connect(
            (self.undo_bit_stuffing_pilots_off, 0),
            (self.keep_plframe_payload_pilots_off, 0),
        )
        self.connect(
            (self.undo_bit_stuffing_pilots_off, 0), (self.pl_complex_to_float_1, 0)
        )
        self.connect(
            (self.undo_bit_stuffing_pilots_off, 0), (self.plframe_pilots_off_float, 0)
        )
        self.connect(
            (self.undo_bit_stuffing_pilots_on, 0),
            (self.keep_plframe_header_pilots_on, 0),
        )
        self.connect(
            (self.undo_bit_stuffing_pilots_on, 0),
            (self.keep_plframe_payload_pilots_on, 0),
        )
        self.connect(
            (self.undo_bit_stuffing_pilots_on, 0), (self.pl_complex_to_float, 0)
        )
        self.connect(
            (self.undo_bit_stuffing_pilots_on, 0), (self.plframe_pilots_on_float, 0)
        )

    def get_frame_length(self):
        return self.frame_length

    def set_frame_length(self, frame_length):
        self.frame_length = frame_length

    def get_symbol_rate(self):
        return self.symbol_rate

    def set_symbol_rate(self, symbol_rate):
        self.symbol_rate = symbol_rate
        self.set_samp_rate(self.symbol_rate * 2)

    def get_taps(self):
        return self.taps

    def set_taps(self, taps):
        self.taps = taps

    def get_samp_rate(self):
        return self.samp_rate

    def set_samp_rate(self, samp_rate):
        self.samp_rate = samp_rate

    def get_rolloff(self):
        return self.rolloff

    def set_rolloff(self, rolloff):
        self.rolloff = rolloff

    def get_physical_layer_header_length(self):
        return self.physical_layer_header_length

    def get_physical_layer_gold_code(self):
        return self.physical_layer_gold_code

    def set_physical_layer_gold_code(self, physical_layer_gold_code):
        self.physical_layer_gold_code = physical_layer_gold_code

    def get_physical_layer_frame_size(self, has_pilots):
        slots = PLFRAME_SLOT_NUMBER[(self.frame_type, self.constellation)]
        length = 90 * (slots + 1)
        assert has_pilots in (
            dtv.PILOTS_ON,
            dtv.PILOTS_OFF,
        ), "Unknown pilots value: " + repr(has_pilots)
        if has_pilots == dtv.PILOTS_ON:
            return length + ((slots - 1) // 16)
        return length

    def get_noise(self):
        return self.noise

    def set_noise(self, noise):
        self.noise = noise

    def get_gain(self):
        return self.gain

    def set_gain(self, gain):
        self.gain = gain

    def get_center_freq(self):
        return self.center_freq

    def set_center_freq(self, center_freq):
        self.center_freq = center_freq


def argument_parser():
    parser = OptionParser(usage="%prog: [options]", option_class=eng_option)
    parser.add_option(
        "--frame-type",
        default="FECFRAME_NORMAL",
        choices=("FECFRAME_NORMAL", "FECFRAME_SHORT"),
    )
    parser.add_option(
        "--constellation",
        default="MOD_8PSK",
        choices=("MOD_QPSK", "MOD_8PSK", "MOD_16APSK", "MOD_32APSK"),
    )
    parser.add_option(
        "--code-rate",
        default="C1_2",
        choices=(
            "C1_2",
            "C1_3",
            "C1_4",
            "C2_3",
            "C2_5",
            "C3_4",
            "C3_5",
            "C4_5",
            "C5_6",
            "C8_9",
            "C9_10",
        ),
    )

    args, _ = parser.parse_args()

    args.frame_type = getattr(dtv, args.frame_type)
    args.constellation = getattr(dtv, args.constellation)
    args.code_rate = getattr(dtv, args.code_rate)

    return args


def main(top_block_cls=dvbs2_tx, options=None):
    args = argument_parser()

    #  def __init__(self, constellation, frame_type, code_rate):
    tb = top_block_cls(
        constellation=args.constellation,
        frame_type=args.frame_type,
        code_rate=args.code_rate,
    )
    tb.start()
    tb.wait()


if __name__ == "__main__":
    main()
