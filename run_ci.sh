#!/bin/bash

# must be run in the root of the repository.
# if the 1st arg to the script is "local" then we skip llvm fresh build.

TIMEOUT=timeout

# set -x # echo on
# set -e # exit on failure

# some system info
uname -a

echo -e "\n\n\t----- building llvm -----\n\n"

if [ "$1" != "local" ]; then
  ./llvm/fresh-build.sh
fi

echo -e "\n\n\t----- building manticore -----\n\n"

if [ "$1" != "local" ]; then
  autoheader -Iconfig
  autoconf -Iconfig
  ./configure --with-llvm=./llvm/build
  make local-install
fi


########## start of testing stuff ##########

llvmOptions=(
  # "-O0"
  # "-O1"
  # "-O2"
  # "-O3"
  # "-O4"
  "-O5"
)

SMALL_SEG_SIZE="4k"
smallSegmentConfigs=(
  # "-segstack"
  # "-resizestack"
  #
  # "-segstack -sealingcapture"
  # "-resizestack -sealingcapture"
)

stacks=(
  ""
  "-contigstack"
  "-segstack"
  "-segstack -lazyunderflow"
  "-resizestack"
  "-hybridstack"
  "-linkstack"

  # now we test with / without the cshim
  "-Ccshim=true"
  "-contigstack -Ccshim=true"
  "-segstack -Ccshim=false"
  # "-segstack -lazyunderflow -Ccshim=false"  # this config is too inefficient; it mprotects too many segments!
  "-resizestack -Ccshim=false"
  "-hybridstack -Ccshim=false"

  # now we test noras
  "-noras"
  "-noras -contigstack"
  "-noras -segstack"
  "-noras -segstack -lazyunderflow"
  "-noras -linkstack"
)

CI_REPORT="gitlab_report.txt"
rm -f $CI_REPORT
touch $CI_REPORT

# $1 = config
# $2 = script, e.g., run-seq.sh
# $3 = aout flags
runTest () {
    AOUTFLAGS="$3" BACKEND="$1" $TIMEOUT 5m "./src/regression-tests/bash-scripts/$2"
    RET_CODE="$?"
    if [ "$RET_CODE" -ne 0 ]; then
      if [ "$RET_CODE" -eq 124 ]; then
        RET_CODE="timeout"
      fi
        echo "$RET_CODE fails for $1, $2, $3" >> $CI_REPORT
    fi
}

# TODO: run the par tests on CPS LLVM options

for stack in "${stacks[@]}"; do
    for llvm in  "${llvmOptions[@]}"; do
        config="$llvm $stack"
        echo -e "\n\n\t----- testing configuration: $config -----\n\n"
        echo -e "\t--SEQUENTIAL --"
        runTest "$config" run-seq.sh
        echo -e "\n\t-- CML --"
        runTest "$config" run-cml.sh "-p 1"
        echo -e "\n\n\t----- done -----\n\n"
    done
done

##########
# stress the overflow / underflow handling

for stack in "${smallSegmentConfigs[@]}"; do
  echo -e "\n\n\t----- testing $stack with small segments -----\n\n"

  echo -e "\t--SEQUENTIAL --"
  runTest "$stack" run-seq.sh "-stacksz ${SMALL_SEG_SIZE}"

  echo -e "\n\t-- CML --"
  runTest "$stack" run-cml.sh "-p 1 -stacksz ${SMALL_SEG_SIZE}"

  echo -e "\n\n\t----- done -----\n\n"
done


echo -e "\n\n\t----- testing with MLRISC -----\n\n"
echo -e "\t--SEQUENTIAL --"
runTest -mlrisc run-seq.sh
echo -e "\n\t-- CML --"
runTest -mlrisc run-cml.sh
echo -e "\n\n\t----- done -----\n\n"

# Exit with error if any tests failed
numFailed="$(wc -l < $CI_REPORT)"
if [ "$numFailed" -ne 0 ] ; then
    echo -e "\n\nA failure was detected in $numFailed configurations:"
    cat $CI_REPORT
    exit 1
fi

echo -e "\n\nAll tests passed successfully!"
