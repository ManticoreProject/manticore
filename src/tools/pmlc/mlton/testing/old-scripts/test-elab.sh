#!/bin/sh
#

f=$1

sml <<XXX
CM.make "sources.cm";
PMLFrontEnd.init();
PMLFrontEnd.compilePML {input=["$f"]};
XXX
