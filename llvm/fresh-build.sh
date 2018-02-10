#!/bin/bash

OPTIONS="-DLLVM_ENABLE_ASSERTIONS=ON -DLLVM_ENABLE_EXPENSIVE_CHECKS=ON -DLLVM_USE_LINKER=gold -DLLVM_OPTIMIZED_TABLEGEN=ON"

# this script is meant to help run continuous integration on Linux. If you'd
# like to build LLVM so that it compiles programs quickly, uncomment the line below:
# OPTIONS=""

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
cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=../install -DCMAKE_BUILD_TYPE=Release $OPTIONS -DLLVM_TARGETS_TO_BUILD="X86" ../src
make -j $JOBS opt llc
./bin/opt --version
./bin/llc --version
