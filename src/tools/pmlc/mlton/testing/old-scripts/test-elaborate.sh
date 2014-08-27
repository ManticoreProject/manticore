#!/bin/bash
#

TIMEOUT=1.5
SUCCESS="Completed generation of sxml"
RED="\e[0;31m"
GREEN="\e[0;32m"
ENDCOLOR="\e[0m"
ROOT="elaborate-tests"

result=""
verbose=false
file=""

while getopts "vf:" opt; do
	case $opt in
		v)
			verbose=true
			;;
		f)
			file=$OPTARG
			;;
	esac
done
file_path=$ROOT/$file

result=$(timeout $TIMEOUT ./test-elab.sh $file_path 2>&1)

if $verbose; then
	# echo "do verbose"
	echo "$result"
fi

cat $file_path | grep -q "\(* FAIL *\)"
should_fail=$?

echo "$result" | egrep -q "$SUCCESS"

if [ $? -ne $should_fail ]; then
	echo -e "${GREEN}TEST PASSED${ENDCOLOR} for $file"
else
	echo -e "${RED}TEST FAILED${ENDCOLOR} for $file"
fi
