#!/bin/bash

LIST_LEN=$1
if [ "$LIST_LEN" == "" ]; then
    LIST_LEN="5000000"
fi
SMLNJ="sml"

if [ ! -f ../../input-data/random-int-list.txt ]; then
   echo "SMLofNJ.exportFn(\"gen-random-int-list\", Main.main);" | $SMLNJ gen-random-int-list.cm
   $SMLNJ @SMLload=gen-random-int-list $LIST_LEN file > ../../input-data/random-int-list.txt
fi
