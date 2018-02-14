#!/bin/bash

# must be run in the root of the repository.

set -x # echo on
set -e # exit on failure

# some system info
uname -a

echo -e "\n\n\t----- building llvm -----\n\n"

./llvm/fresh-build.sh

echo -e "\n\n\t----- building manticore -----\n\n"

autoheader -Iconfig
autoconf -Iconfig
./configure --with-llvm=./llvm/build
make local-install


########## start of testing stuff ##########

# we want to try all configurations and _then_ 
# report failure if any one of them fails,
# so we use || here to collect failure results.

failed=0

llvmOptions=(
  # "-O0"   FIXME: right now, big-tuples causes LLVM to hang without optimization.
  "-O1"
  "-O2"
  "-O3"
  "-O4"
  "-O5"
)

stacks=(
  ""
  "-contigstack"
  "-segstack"
  # now we test with the cshim
  "-Ccshim=true"
  "-Ccshim=true -contigstack"
  "-Ccshim=true -segstack"
  # now we test noras
  "-noras "
  "-noras -contigstack"
  "-noras -segstack"
)

echo -e "\n\n\t----- testing with MLRISC -----\n\n"
BACKEND="-mlrisc" ./src/regression-tests/bash-scripts/run-seq.bsh || failed=1
# ./src/regression-tests/bash-scripts/run-par.bsh || failed=1
echo -e "\n\n\t----- done -----\n\n"

for stack in "${stacks[@]}"; do
    for llvm in  "${llvmOptions[@]}"; do
        config="$llvm $stack"
        echo -e "\n\n\t----- testing configuration: $config -----\n\n"
        BACKEND="$config" timeout 20m ./src/regression-tests/bash-scripts/run-seq.bsh || failed=1
        # BACKEND="$config" ./src/regression-tests/bash-scripts/run-par.bsh || failed=1
        echo -e "\n\n\t----- done -----\n\n"
    done
done

# Exit with error if any tests failed
if [ "$failed" -ne 0 ] ; then
    echo -e "\n\nA failure was detected!! See above output."
    exit 1
fi

echo -e "\n\nAll tests passed successfully!"
