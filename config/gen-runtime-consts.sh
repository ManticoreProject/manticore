#!/bin/sh
#
# COPYRIGHT (c) 2007 Manticore project. (http://manticore.cs.uchicago.edu)
# All rights reserved.
#
# This script is run as part of the configuration process to generate
# the runtime-constants.sml file.
#

BUILD_DIR=src/lib/parallel-rt/build
DRIVER_DIR=src/tools/mc/driver
PROG=gen-runtime-constants
OUTFILE=runtime-constants.sml

(cd $BUILD_DIR; make $PROG || exit 1)

echo "$BUILD_DIR/$PROG > $DRIVER_DIR/$OUTFILE"
$BUILD_DIR/$PROG > $DRIVER_DIR/$OUTFILE

rm -f $BUILD_DIR/$PROG

exit 0
