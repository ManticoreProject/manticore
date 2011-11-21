#!/bin/bash

NUM_POINTS=8000000
SMLNJ="sml"

if [ ! -f ../../input-data/points.txt ]; then
   echo "SMLofNJ.exportFn(\"gen-points\", Main.main);" | $SMLNJ gen-points.cm
   $SMLNJ @SMLload=gen-points $NUM_POINTS file > ../../input-data/points.txt
fi