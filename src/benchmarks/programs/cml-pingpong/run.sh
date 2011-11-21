#!/bin/sh
#
# script to run the benchmarks
#

d=$(date +%Y%m%d-%H%M)

f=DATA-$d

echo $d > $f

echo "running CML benchmarks"
echo "" >> $f
echo "CML:" >> $f
for i in 0 1 2 3 4 5 6 7 8 9 ; do
  /stage/sml/current/bin/sml @SMLload=pingpong-smlnj >> $f
done

echo "running Manticore (1P) benchmarks"
echo "" >> $f
echo "Manticore 1P:" >> $f
for i in 0 1 2 3 4 5 6 7 8 9 ; do
  pingpong-mc1 -p 1 >> $f
done

echo "running Manticore (2P) benchmarks"
echo "" >> $f
echo "Manticore 2P:" >> $f
for i in 0 1 2 3 4 5 6 7 8 9 ; do
  pingpong-mc2 -p 2 >> $f
done

