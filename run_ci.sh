#!/bin/bash

# must be run in the root of the repository.

TIMEOUT=timeout

# set -x # echo on
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
  "-linkstack"
  # now we test with the cshim
  "-Ccshim=true"
  "-Ccshim=true -contigstack"
  "-Ccshim=true -segstack"
  # now we test noras
  "-noras"
  "-noras -contigstack"
  "-noras -segstack"
  "-noras -linkstack"
)

CI_REPORT="gitlab_report.txt"
rm -f $CI_REPORT
touch $CI_REPORT

# $1 = config
# $2 = script, e.g., run-seq.bsh
runTest () {
    BACKEND="$1" $TIMEOUT 20m ./src/regression-tests/bash-scripts/$2
    if [ "$?" -ne 0 ]; then
        echo "$1, $2" >> $CI_REPORT
    fi
}

# TODO: run the par tests on CPS LLVM options

for stack in "${stacks[@]}"; do
    for llvm in  "${llvmOptions[@]}"; do
        config="$llvm $stack"
        echo -e "\n\n\t----- testing configuration: $config -----\n\n"
        runTest "$config" run-seq.bsh
        runTest "$config" run-cml.bsh
        echo -e "\n\n\t----- done -----\n\n"
    done
done

echo -e "\n\n\t----- testing with MLRISC -----\n\n"
runTest -mlrisc run-seq.bsh
runTest -mlrisc run-par.bsh
echo -e "\n\n\t----- done -----\n\n"

# Exit with error if any tests failed
if [ `wc -l < $CI_REPORT` -ne 0 ] ; then
    echo -e "\n\nA failure was detected in the following configurations:"
    cat $CI_REPORT
    exit 1
fi

echo -e "\n\nAll tests passed successfully!"
