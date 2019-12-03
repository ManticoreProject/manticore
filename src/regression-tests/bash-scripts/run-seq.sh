#!/bin/bash
#
# COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
# All rights reserved.
#

ABSPATH="$(cd "${0%/*}" 2>/dev/null; echo "$PWD"/"${0##*/}")"
DIR=`dirname "$ABSPATH"`/..
# set the BACKEND flag
PMLC="sml @SMLcmdname=pmlc @SMLload=$DIR/../../src/tools/mc/pmlc"
PMLCFLAGS="-sequential $BACKEND"
#PMLCFLAGS="-sequential -Cmatch.check=true -Ccps.enable-ho-inline=true -Cstats.report=true -Ccps.copy-propagation-reflow=true"

DATE=$( date +"%F-%H-%M-%S" )
REPORT_FILE=report-$DATE
REPORT="$DIR/reports/$REPORT_FILE"
LOG="$DIR/LOG-SEQ"

FAIL_MSG="test has FAILED"

# allow core dumps
ulimit -c unlimited

( cd $DIR/goals;

rm -rf $LOG

echo "pmlc is $PMLC" | tee -a $REPORT_FILE
echo "compilation flags: $PMLCFLAGS" | tee -a $REPORT_FILE

for g in basics seq-*
do
( cd $g;
   for f in *.pml *.mlb
   do
     fname=${f%.*}
     if [ -e "$fname.ok" ] ; then
       echo -n "checking $g/$f ..."
       echo "checking $g/$f" >> $LOG
       $PMLC $PMLCFLAGS $f 2>> $LOG 1>> $LOG
       if [ "$?" -eq "0" ] ; then
         ./a.out $AOUTFLAGS > $fname.test
         diffs=$(diff $fname.test $fname.ok)
         status=$(diff $fname.test $fname.ok | wc -l)
         if [ $status -eq "0" ] ; then
           echo "***** Check succeeded for goal $g in file $f." >> $REPORT
           echo "ok" >> $LOG
           echo "ok"
         else
           echo "***** Check failed for goal $g in file $f." >> $REPORT
           diff $fname.test $fname.ok >> $REPORT
           echo "$FAIL_MSG" >> $LOG
           echo "CHECK FAIL"
         fi
       else
         echo "***** Compile failed for goal $g in file $f." >> $REPORT
         echo "$FAIL_MSG" >> $LOG
         echo "COMPILE FAIL"
       fi
       rm -f a.out $fname.s $fname.test $fname.ll ${fname}_opt.bc
       rm -rf a.out.dSYM
     fi
   done
)
done

)

echo "Report available at: $REPORT"

NUM_FAILS=$(grep -c "test has FAILED" < $LOG)
exit $NUM_FAILS