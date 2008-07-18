#!/bin/sh
#
# COPYRIGHT (c) 2007 Manticore project. (http://manticore.cs.uchicago.edu)
# All rights reserved.
#
# This script is run as part of the configuration process to generate
# the runtime-constants.sml file.
#

BUILD_DIR=src/lib/parallel-rt/build/config

function gen {
  PROG=$1
  OUTFILE=$2
  (cd $BUILD_DIR; make $PROG || exit 1)

  echo "$BUILD_DIR/$PROG > $OUTFILE"
  $BUILD_DIR/$PROG > $OUTFILE

  rm -f $BUILD_DIR/$PROG
}

gen gen-runtime-constants src/tools/mc/driver/runtime-constants.sml
gen gen-runtime-offsets src/lib/hlops/include/runtime-offsets.def
gen gen-log-events src/lib/hlops/include/log-events.def
# moving away from hlop files
gen gen-runtime-offsets src/lib/basis/runtime/runtime-offsets.def
gen gen-log-events src/lib/basis/runtime/log-events.def

exit 0
