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

# make the message below not ugly
set +ex
echo "----------------------------------------------------------------------"
echo "Benchmarking complete!"
echo "Results are in ${RESULTS_DIR}"
echo "Keep this Docker session alive and follow instructions in the README"
echo "to copy the results to your local filesystem!"
