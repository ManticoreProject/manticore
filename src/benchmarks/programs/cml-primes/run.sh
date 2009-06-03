#!/bin/sh
#
# script to run the primes benchmarks
#

d=$(date +%Y%m%d-%H%M)

f=DATA-$d

echo $d > $f

echo "running CML benchmarks"
echo "" >> $f
echo "CML:" >> $f
for i in 0 1 2 3 4 5 6 7 8 9 ; do
  /stage/sml/current/bin/sml @SMLload=primes-smlnj >> $f
done

echo "running Manticore 1P benchmarks"
echo "" >> $f
echo "Manticore 1P:" >> $f
for i in 0 1 2 3 4 5 6 7 8 9 ; do
  primes-mc1 -p 1 >> $f
done

for np in 2 3 4 5 6 7 8 ; do
  name="Manticore "$np"P"
  echo "running $name benchmarks"
  echo "" >> $f
  echo "$name:" >> $f
  for i in 0 1 2 3 4 5 6 7 8 9 ; do
    primes-mc2 -p $np >> $f
  done
done

# parallel version with grouping of threads
#
for np in 2 3 4 5 6 7 8 ; do
  name="Manticore "$np"P4"
  echo "running $name benchmarks"
  echo "" >> $f
  echo "$name:" >> $f
  for i in 0 1 2 3 4 5 6 7 8 9 ; do
    primes-mc3 -p $np >> $f
  done
done

