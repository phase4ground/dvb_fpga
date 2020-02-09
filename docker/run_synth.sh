#!/usr/bin/env bash
#
# DVP IP
#
# Copyright 2019 by Suoto <andre820@gmail.com>
#
# This file is part of DVP IP.
#
# DVP IP is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# DVP IP is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with DVP IP.  If not, see <http://www.gnu.org/licenses/>.

set -e

PATH_TO_REPO=$(git rev-parse --show-toplevel)
CONTAINER="ghdl/synth:beta"

# This will be run inside the container
RUN_COMMAND="
set -e

PATH_TO_THIS_SCRIPT=\$(readlink -f \"\$(dirname \$0)\")

pushd \$PATH_TO_THIS_SCRIPT/..

addgroup $USER --gid $(id -g) > /dev/null 2>&1

adduser --disabled-password \
  --gid $(id -g)            \
  --uid $UID                \
  --home /home/$USER $USER > /dev/null 2>&1

# Run test with GHDL
su -l $USER -c \"                          \
  cd /project                           && \
  yosys -m ghdl -p '                       \
    ghdl                                   \
      rtl/bch_generated/bch_128x64.vhd     \
      rtl/bch_generated/bch_192x64.vhd     \
      rtl/bch_generated/bch_128x32.vhd     \
      rtl/bch_generated/bch_160x16.vhd     \
      rtl/bch_generated/bch_128x16.vhd     \
      rtl/bch_generated/bch_168x8.vhd      \
      rtl/bch_generated/bch_192x32.vhd     \
      rtl/bch_generated/bch_192x8.vhd      \
      rtl/bch_generated/bch_128x8.vhd      \
      rtl/bch_generated/bch_160x32.vhd     \
      rtl/bch_generated/bch_160x64.vhd     \
      rtl/bch_generated/bch_192x16.vhd     \
      rtl/bch_generated/bch_168x8.vhd      \
      rtl/bch_generated/bch_160x8.vhd      \
      rtl/common_pkg.vhd                   \
      rtl/dvb_utils_pkg.vhd                \
      rtl/axi_stream_delay.vhd             \
      rtl/skidbuffer.vhd                   \
      rtl/synchronizer.vhd                 \
      rtl/sr_delay.vhd                     \
      rtl/edge_detector.vhd                \
      rtl/ram_inference.vhd                \
      rtl/axi_stream_fifo.vhd              \
      rtl/bch_encoder_mux.vhd              \
      rtl/axi_baseband_scrambler.vhd       \
      rtl/axi_stream_master_adapter.vhd    \
      rtl/axi_bch_encoder.vhd              \
      rtl/config_fifo.vhd                  \
      rtl/axi_bit_interleaver.vhd          \
      rtl/dvbs2_tx.vhd                     \
      -e dvbs2_tx'
  \"
"

# Need to add some variables so that uploading coverage from witihin the
# container to codecov works
docker run                                                 \
  --rm                                                     \
  --mount type=bind,source="$PATH_TO_REPO",target=/project \
  --net=host                                               \
  --env="DISPLAY"                                          \
  --volume="$HOME/.Xauthority:/root/.Xauthority:rw"        \
  $CONTAINER /bin/bash -c "$RUN_COMMAND"

