#!/bin/bash

# assumes that current directory is the root of the repository.

set -x # echo on

pushd llvm
rm -rf build install
mkdir build install
pushd build
cmake -G "Ninja" -DCMAKE_INSTALL_PREFIX=../install -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD="X86" ../src
ninja opt llc
