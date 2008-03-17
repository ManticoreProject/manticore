#!/bin/bash

MANT_FILES="ray-tracer.pml ray-tracer-server.pml main.pml"
TEMP_FILE="tmp.pml"

cat $MANT_FILES > $TEMP_FILE

MC="sml @SMLcmdname=mc @SMLload=../../mc"
MANT_FLAGS="-Cclos.convert-style=flatWithCFA -Cscheduler=work-stealers"

$MC $MANT_FLAGS $TEMP_FILE

rm -f $TEMP_FILE *.s
mv a.out rts