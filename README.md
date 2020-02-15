# DVB FPGA

![Unit tests](https://github.com/phase4ground/dvb_fpga/workflows/Unit%20tests/badge.svg)
![Yosys Synth](https://github.com/phase4ground/dvb_fpga/workflows/Yosys%20Synth/badge.svg)

This project aims to implement RTL components for DVB-S2, initially focusing on
the transmission side.

| ![System architecture](misc/system_architecture.png)                        |
| :---:                                                                       |
| Functional block diagram of the DVB-S2 System from ETSI EN 302 307-1 V1.4.1 |

## Functional guidelines

* Behaviour should match exactly what GNU Radio produces for every combination of
  parameters described on the DVB-S2 base spec (no extensions yet). This means
  components should handle
  * Frame types: Normal and short
  * Constellations: 8 PSK, 16 APSK and 32 APSK
  * Code rates: 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 8/9, 9/10,
* Components should also handle parameters changing on every frame, that is, they
  should handle frame with config A then a frame with config B immediately
  afterwards without requiring reset or wait cycles
* Use AXI-Stream interfaces

## Components' status

<!-- Definitions of each development stage: -->

<!-- * Simulation -->
<!--   * Data must match GNU Radio's output -->
<!--   * **Back to back data:** check no bubbles are inserted -->
<!--   * **Slow reader:** check that AXI backpressure works as expected -->
<!--   * **Slow writer:** check that output data is throttled correctly -->
<!--   * **Slow write and reader:** check AXI handshaking works as expected -->
<!-- * Hardware testing -->
<!--   * Check components is synthesized/implemented as expected by EDA tools -->
<!--   * Check for possible timing issues (too many logic levels, unintended latches, -->
<!--     etc) -->
<!--   [> * Still TBD. Basic idea is to stream data in/out via PCIe or DDR where the host <] -->
<!--   [>   compares the actual and the expected results <] -->
<!--   [> * Might be interesting to have a generic GNU Radio to PCIe bridge so that one <] -->
<!--   [>   could offload create a block diagram on GRC and run parts of it on a real <] -->
<!--   [>   card <] -->
<!-- * Hardware integration -->
<!--   * Same as hardware testing but chaining components together to check it handles -->
<!--     multiple configs as expected -->
<!--   * Check that timing constraints are met when components are used together -->

### Core DVB-S2 components

| Component name         | Simulation | HW testing | HW integration | Notes                                  |
| :---                   | :---:      | :--:       | :--:           | :---                                   |
| Baseband scrambler     | Done       | TODO       | TODO           |                                        |
| BCH encoder            | Done       | TODO       | TODO           | Different data widths can be generated |
| Bit interleaver        | Done       | TODO       | TODO           | Tested data widths: 1 and 8            |
| Constellation mapper   | TODO       | TODO       | TODO           |                                        |
| LDPC Encoder           | TODO       | TODO       | TODO           |                                        |
| Physical layer framing | TODO       | TODO       | TODO           |                                        |

### Simulation helpers

| Component name   | Simulation | HW testing | HW integration | Notes               |
| :---             | :---:      | :--:       | :--:           | :---                |
| AXI file reader  | Done       | -          | -              | [Issue #1][issue_1] |
| AXI file compare | Done       | -          | -              | [Issue #1][issue_1] |

## Running tests

Tests can be run locally or on a Docker container. Running locally will require
GNU Radio, VUnit and a VHDL simulator.

### Using Docker

Uses the same container used for CI

```sh
# Clone this repo and submodules
git clone --recurse-submodules  https://github.com/phase4ground/dvb_fpga
cd dvb_fpga
# Run the tests
./docker/run_tests.sh
```

Arguments passed to `docker/run_tests.sh` will be passed to `run.py` and, by
extension, to VUnit (no environment variable is passed on though).

### Running locally

* Requirements
  * GNU Radio
  * A VHDL simulator
  * [VUnit][vunit]

```sh
# Install VUnit
pip install vunit-hdl
# Clone this repo and submodules
git clone --recurse-submodules  https://github.com/phase4ground/dvb_fpga
cd dvb_fpga
# Generate test data
cd gnuradio_data/
make all
cd ..
# Run the tests
./run.py
```

[vunit]: https://vunit.github.io/
[issue_1]: https://github.com/phase4ground/dvb_fpga/issues/1
