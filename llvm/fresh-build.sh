#!/bin/bash

OPTIONS="-DLLVM_ENABLE_ASSERTIONS=ON -DLLVM_USE_LINKER=gold -DLLVM_OPTIMIZED_TABLEGEN=ON"

# this script is meant to help run continuous integration on Linux. If you'd
# like to build LLVM so that it compiles programs quickly, uncomment the line below:
# OPTIONS=""

# assumes that current directory is the root of the repository.

set -x # echo on
set -e # exit on failure

# we try to not use all the CPUs on the system.
CPUS=$(getconf _NPROCESSORS_ONLN)
JOBS=$((CPUS - 2))

pushd llvm
rm -rf build install
mkdir build install
pushd build
cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=../install -DCMAKE_BUILD_TYPE=Release "$OPTIONS" -DLLVM_TARGETS_TO_BUILD="X86" ../src/llvm

if [ "$1" != "docker" ]; then
  make -j $JOBS opt llc
  ./bin/opt --version
  ./bin/llc --version
else
  make -j $JOBS install-opt install-llc
fi
