#!/bin/bash

# assumes that current directory is the root of the repository.

set -x # echo on
set -e # exit on failure

# for GitLab's CI, we allow 2 concurrent builds, and want to limit
# the number of LLVM jobs to half the number of CPUs.
CPUS=`getconf _NPROCESSORS_ONLN`
JOBS=`expr $CPUS / 2`

pushd llvm
rm -rf build install
mkdir build install
pushd build
cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=../install -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD="X86" ../src
make -j $JOBS opt llc
