#!/bin/sh
#
# script to run the primes benchmarks
#

# the problem size
#
if [ $# = 1 ] ; then
  N=$1; shift
else
  N=5000
fi
echo "problem size = $N"

SML=sml
if [ -x /stage/sml/current/bin/sml ] ; then
  SML=/stage/sml/current/bin/sml
fi

d=$(date +%Y%m%d-%H%M)
h=$(hostname)

f=DATA-$d

echo "$h $d" > $f

echo "running CML benchmarks"
echo "" >> $f
echo "CML:" >> $f
for i in 0 1 2 3 4 5 6 7 8 9 ; do
  $SML @SMLload=primes-smlnj $N >> $f
done

echo "running Manticore 1P benchmarks"
echo "" >> $f
echo "Manticore 1P:" >> $f
for i in 0 1 2 3 4 5 6 7 8 9 ; do
  primes-mc1 -p 1 $N >> $f
done

#for np in 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ; do
for np in 2 3 4 5 6 7 8 ; do
  name="Manticore "$np"P"
  echo "running $name benchmarks"
  echo "" >> $f
  echo "$name:" >> $f
  for i in 0 1 2 3 4 5 6 7 8 9 ; do
    primes-mc2 -p $np $N >> $f
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
    primes-mc3 -p $np $N >> $f
  done
done

