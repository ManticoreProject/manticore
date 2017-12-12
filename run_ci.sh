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
  "-llopt0"
  "-llopt1"
  "-llopt2"
  "-llopt3"
  "-llopt4"
  "-llopt5"
)

stacks=(
  ""
  "-direct"
  "-segstack"
)

echo -e "\n\n\t----- testing with MLRISC -----\n\n"
./src/regression-tests/bash-scripts/run-seq.bsh || failed=1
# ./src/regression-tests/bash-scripts/run-par.bsh || failed=1
echo -e "\n\n\t----- done -----\n\n"

for stack in "${stacks[@]}"; do
    for llvm in  "${llvmOptions[@]}"; do
        config="$llvm $stack"
        echo -e "\n\n\t----- testing configuration: $config -----\n\n"
        BACKEND="$config" ./src/regression-tests/bash-scripts/run-seq.bsh || failed=1
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
