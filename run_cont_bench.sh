#!/bin/bash

# this script should be run from the root of the manticore source dir.
# it assumes the following:
#
# 1. Manticore's already built.
# 2. SML/NJ 64-bit is available in /usr/smlnj64/bin

set -ex

MC_DIR=$(pwd)
TRIALS=5
RESULTS_DIR=${MC_DIR}/results

# make sure `perf stat` works, since the benchmark's conf script doesn't
# check for this.
perf stat echo

# run the benchmarks
mkdir "${RESULTS_DIR}"
cd src/benchmarks/drivers
PATH=/usr/smlnj64/bin:${PATH} ./pldi20.sh "${MC_DIR}" "${RESULTS_DIR}" ${TRIALS}

# generate plots
LANG=C.UTF-8 LC_ALL=C.UTF-8 ./plotall.sh "${RESULTS_DIR}"

echo "Benchmarking complete. See results in ${RESULTS_DIR}"
