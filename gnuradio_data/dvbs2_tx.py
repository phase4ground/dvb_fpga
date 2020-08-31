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


from gnuradio import analog
from gnuradio import blocks
from gnuradio import digital
from gnuradio import dtv
from gnuradio import eng_notation
from gnuradio import filter
from gnuradio import gr
from gnuradio.eng_option import eng_option
from gnuradio.filter import firdes
from optparse import OptionParser
import math
import numpy


CODE_RATES = {
    dtv.C1_2:  1./2,
    dtv.C1_3:  1./3,
    dtv.C1_4:  1./4,
    dtv.C2_3:  2./3,
    dtv.C2_5:  2./5,
    dtv.C3_4:  3./4,
    dtv.C3_5:  3./5,
    dtv.C4_5:  4./5,
    dtv.C5_6:  5./6,
    dtv.C8_9:  8./9,
    dtv.C9_10: 9./10}

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
    },
}


def get_ratio(constellation):
    ratio = None

    if constellation == dtv.MOD_8PSK:
        ratio = 3, 8
    if constellation == dtv.MOD_16APSK:
        ratio = 4, 8
    if constellation == dtv.MOD_32APSK:
        ratio = 5, 8

    #  print("Ratio is %s" % (ratio,))

    assert ratio is not None, "??"
    return ratio

class UnknownFrameLength(Exception):
    def __init__(self, frame_type, code_rate):
        self.frame_type = frame_type
        self.code_rate = code_rate

    def __str__(self):
        return "Could not get frame length for {}, {}".format(self.frame_type, self.code_rate)

class dvbs2_tx(gr.top_block):

    def __init__(self, constellation, frame_type, code_rate):
        gr.top_block.__init__(self, "Dvbs2 Tx")

        ##################################################
        # Parameters
        ##################################################
        # Header is 10 bytes
        try:
            frame_length = 1.0*BBFRAME_LENGTH[frame_type][code_rate] - 80
        except KeyError:
            raise UnknownFrameLength(frame_type, code_rate)

        #  print("Base frame length: %s" % frame_length)
        frame_length /= 8
        #  print("Base frame length: %s" % frame_length)
        assert int(frame_length) == 1.0*frame_length, "Frame length {0} won't work because {0}/8 = {1}!".format(frame_length, frame_length / 8.0)
        frame_length = int(frame_length)
        self.frame_length = frame_length

        bits_per_input, bits_per_output = get_ratio(constellation)

        ##################################################
        # Variables
        ##################################################
        self.symbol_rate = symbol_rate = 5000000
        self.taps = taps = 100
        self.samp_rate = samp_rate = symbol_rate * 2
        self.rolloff = rolloff = 0.2
        self.noise = noise = 0
        self.gain = gain = 1
        self.center_freq = center_freq = 1280e6

        ##################################################
        # Blocks
        ##################################################
        self.ldpc_encoder_input = blocks.file_sink(gr.sizeof_char*1, 'ldpc_encoder_input.bin', False)
        self.ldpc_encoder_input.set_unbuffered(False)
        self.fir_filter_xxx_0_0 = filter.fir_filter_ccc(1, (numpy.conj([0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j]  + [0,]*(89-32))))
        self.fir_filter_xxx_0_0.declare_sample_delay(0)
        self.fir_filter_xxx_0 = filter.fir_filter_ccc(1, (numpy.conj([0,]*(89-25) + [   0.00000 - 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 - 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 - 1.00000j,   0.00000 - 1.00000j,   0.00000 - 1.00000j,   0.00000 - 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 + 1.00000j,   0.00000 + 0.00000j])))
        self.fir_filter_xxx_0.declare_sample_delay(0)
        self.fft_filter_xxx_0 = filter.fft_filter_ccc(1, (firdes.root_raised_cosine(1.0, samp_rate, samp_rate/2, rolloff, taps)), 1)
        self.fft_filter_xxx_0.declare_sample_delay(0)
        self.dtv_dvbs2_physical_cc_0 = dtv.dvbs2_physical_cc(frame_type, code_rate, dtv.MOD_BPSK, dtv.PILOTS_ON, 0)
        self.dtv_dvbs2_modulator_bc_0 = dtv.dvbs2_modulator_bc(frame_type,
        code_rate, constellation, dtv.INTERPOLATION_OFF)
        self.dtv_dvbs2_interleaver_bb_0 = dtv.dvbs2_interleaver_bb(frame_type, code_rate, constellation)
        self.dtv_dvb_ldpc_bb_0 = dtv.dvb_ldpc_bb(dtv.STANDARD_DVBS2, frame_type, code_rate, dtv.MOD_OTHER)
        self.dtv_dvb_bch_bb_0 = dtv.dvb_bch_bb(dtv.STANDARD_DVBS2, frame_type, code_rate)
        self.dtv_dvb_bbscrambler_bb_0 = dtv.dvb_bbscrambler_bb(dtv.STANDARD_DVBS2, frame_type, code_rate)
        self.dtv_dvb_bbheader_bb_0 = dtv.dvb_bbheader_bb(dtv.STANDARD_DVBS2, frame_type, code_rate, dtv.RO_0_20, dtv.INPUTMODE_NORMAL, dtv.INBAND_OFF, 168, 4000000)
        self.digital_pfb_clock_sync_xxx_0 = digital.pfb_clock_sync_ccf(2, math.pi/100.0, (firdes.root_raised_cosine(2*32, 32, 1.0/float(2), 0.35, 11*2*32)), 32, 16, 1.5, 1)
        self.digital_costas_loop_cc_0 = digital.costas_loop_cc(math.pi/100.0, 4, False)
        self.blocks_throttle_0 = blocks.throttle(gr.sizeof_float*1, samp_rate/10,True)
        self.blocks_sub_xx_0 = blocks.sub_cc(1)
        self.blocks_repack_bits_bb_0 = blocks.repack_bits_bb(bits_per_input, bits_per_output, "", False, gr.GR_MSB_FIRST)
        self.blocks_multiply_xx_0 = blocks.multiply_vcc(1)
        self.blocks_multiply_const_vxx_0 = blocks.multiply_const_vcc((gain, ))
        self.blocks_max_xx_0 = blocks.max_ff(1,1)
        self.blocks_file_sink_1 = blocks.file_sink(gr.sizeof_float*1, 'output.bin', False)
        self.blocks_file_sink_1.set_unbuffered(False)
        self.blocks_delay_0 = blocks.delay(gr.sizeof_gr_complex*1, 1)
        self.blocks_conjugate_cc_0 = blocks.conjugate_cc()
        self.blocks_complex_to_mag_0_0 = blocks.complex_to_mag(1)
        self.blocks_complex_to_mag_0 = blocks.complex_to_mag(1)
        self.blocks_add_xx_2 = blocks.add_vcc(1)
        self.blocks_add_xx_0 = blocks.add_vcc(1)
        self.bit_mapper_output = blocks.file_sink(gr.sizeof_gr_complex*1, 'bit_mapper_output.bin', False)
        self.bit_mapper_output.set_unbuffered(False)
        self.bit_interleaver_output_packed = blocks.file_sink(gr.sizeof_char*1, 'bit_interleaver_output_packed.bin', False)
        self.bit_interleaver_output_packed.set_unbuffered(False)
        self.bit_interleaver_output = blocks.file_sink(gr.sizeof_char*1, 'bit_interleaver_output.bin', False)
        self.bit_interleaver_output.set_unbuffered(False)
        self.bit_interleaver_input = blocks.file_sink(gr.sizeof_char*1, 'bit_interleaver_input.bin', False)
        self.bit_interleaver_input.set_unbuffered(False)
        self.bch_encoder_input = blocks.file_sink(gr.sizeof_char*1, 'bch_encoder_input.bin', False)
        self.bch_encoder_input.set_unbuffered(False)
        self.bb_scrambler_input_0 = blocks.file_sink(gr.sizeof_char*1, 'bb_scrambler_input.bin', False)
        self.bb_scrambler_input_0.set_unbuffered(False)
        self.analog_random_source_x_0 = blocks.vector_source_b(map(int, numpy.random.randint(0, 255, frame_length)), False)
        self.analog_noise_source_x_1 = analog.noise_source_c(analog.GR_GAUSSIAN, noise, 0)
        self.analog_agc_xx_0 = analog.agc_cc(1e-4, 1.0, 1.0)
        self.analog_agc_xx_0.set_max_gain(8192)

        ##################################################
        # Connections
        ##################################################
        self.connect((self.analog_agc_xx_0, 0), (self.digital_pfb_clock_sync_xxx_0, 0))
        self.connect((self.analog_noise_source_x_1, 0), (self.blocks_add_xx_2, 1))
        self.connect((self.analog_random_source_x_0, 0), (self.dtv_dvb_bbheader_bb_0, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.blocks_complex_to_mag_0, 0))
        self.connect((self.blocks_add_xx_2, 0), (self.analog_agc_xx_0, 0))
        self.connect((self.blocks_complex_to_mag_0, 0), (self.blocks_max_xx_0, 1))
        self.connect((self.blocks_complex_to_mag_0_0, 0), (self.blocks_max_xx_0, 0))
        self.connect((self.blocks_conjugate_cc_0, 0), (self.blocks_multiply_xx_0, 1))
        self.connect((self.blocks_delay_0, 0), (self.blocks_multiply_xx_0, 0))
        self.connect((self.blocks_max_xx_0, 0), (self.blocks_throttle_0, 0))
        self.connect((self.blocks_multiply_const_vxx_0, 0), (self.fft_filter_xxx_0, 0))
        self.connect((self.blocks_multiply_xx_0, 0), (self.fir_filter_xxx_0, 0))
        self.connect((self.blocks_multiply_xx_0, 0), (self.fir_filter_xxx_0_0, 0))
        self.connect((self.blocks_repack_bits_bb_0, 0), (self.bit_interleaver_output_packed, 0))
        self.connect((self.blocks_sub_xx_0, 0), (self.blocks_complex_to_mag_0_0, 0))
        self.connect((self.blocks_throttle_0, 0), (self.blocks_file_sink_1, 0))
        self.connect((self.digital_costas_loop_cc_0, 0), (self.blocks_conjugate_cc_0, 0))
        self.connect((self.digital_costas_loop_cc_0, 0), (self.blocks_delay_0, 0))
        self.connect((self.digital_pfb_clock_sync_xxx_0, 0), (self.digital_costas_loop_cc_0, 0))
        self.connect((self.dtv_dvb_bbheader_bb_0, 0), (self.bb_scrambler_input_0, 0))
        self.connect((self.dtv_dvb_bbheader_bb_0, 0), (self.dtv_dvb_bbscrambler_bb_0, 0))
        self.connect((self.dtv_dvb_bbscrambler_bb_0, 0), (self.bch_encoder_input, 0))
        self.connect((self.dtv_dvb_bbscrambler_bb_0, 0), (self.dtv_dvb_bch_bb_0, 0))
        self.connect((self.dtv_dvb_bch_bb_0, 0), (self.dtv_dvb_ldpc_bb_0, 0))
        self.connect((self.dtv_dvb_bch_bb_0, 0), (self.ldpc_encoder_input, 0))
        self.connect((self.dtv_dvb_ldpc_bb_0, 0), (self.bit_interleaver_input, 0))
        self.connect((self.dtv_dvb_ldpc_bb_0, 0), (self.dtv_dvbs2_interleaver_bb_0, 0))
        self.connect((self.dtv_dvbs2_interleaver_bb_0, 0), (self.bit_interleaver_output, 0))
        self.connect((self.dtv_dvbs2_interleaver_bb_0, 0), (self.blocks_repack_bits_bb_0, 0))
        self.connect((self.dtv_dvbs2_interleaver_bb_0, 0), (self.dtv_dvbs2_modulator_bc_0, 0))
        self.connect((self.dtv_dvbs2_modulator_bc_0, 0), (self.bit_mapper_output, 0))
        self.connect((self.dtv_dvbs2_modulator_bc_0, 0), (self.dtv_dvbs2_physical_cc_0, 0))
        self.connect((self.dtv_dvbs2_physical_cc_0, 0), (self.blocks_multiply_const_vxx_0, 0))
        self.connect((self.fft_filter_xxx_0, 0), (self.blocks_add_xx_2, 0))
        self.connect((self.fir_filter_xxx_0, 0), (self.blocks_add_xx_0, 0))
        self.connect((self.fir_filter_xxx_0, 0), (self.blocks_sub_xx_0, 0))
        self.connect((self.fir_filter_xxx_0_0, 0), (self.blocks_add_xx_0, 1))
        self.connect((self.fir_filter_xxx_0_0, 0), (self.blocks_sub_xx_0, 1))

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
        self.fft_filter_xxx_0.set_taps((firdes.root_raised_cosine(1.0, self.samp_rate, self.samp_rate/2, self.rolloff, self.taps)))

    def get_samp_rate(self):
        return self.samp_rate

    def set_samp_rate(self, samp_rate):
        self.samp_rate = samp_rate
        self.fft_filter_xxx_0.set_taps((firdes.root_raised_cosine(1.0, self.samp_rate, self.samp_rate/2, self.rolloff, self.taps)))
        self.blocks_throttle_0.set_sample_rate(self.samp_rate/10)

    def get_rolloff(self):
        return self.rolloff

    def set_rolloff(self, rolloff):
        self.rolloff = rolloff
        self.fft_filter_xxx_0.set_taps((firdes.root_raised_cosine(1.0, self.samp_rate, self.samp_rate/2, self.rolloff, self.taps)))

    def get_noise(self):
        return self.noise

    def set_noise(self, noise):
        self.noise = noise
        self.analog_noise_source_x_1.set_amplitude(self.noise)

    def get_gain(self):
        return self.gain

    def set_gain(self, gain):
        self.gain = gain
        self.blocks_multiply_const_vxx_0.set_k((self.gain, ))

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
        choices=("MOD_8PSK", "MOD_16APSK", "MOD_32APSK"),
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
    tb = top_block_cls(constellation=args.constellation, frame_type=args.frame_type, code_rate=args.code_rate)
    tb.start()
    tb.wait()


if __name__ == '__main__':
    main()
