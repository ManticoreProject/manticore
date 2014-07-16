#!/bin/bash
#

TIMEOUT=1.5
SUCCESS="(\[nonexhaustive match failure\])|(Completed generation of sxml)"
RED="\e[0;31m"
GREEN="\e[0;32m"
ENDCOLOR="\e[0m"

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

result=$(timeout $TIMEOUT ./test.sh $file 2>&1)

if $verbose; then
	# echo "do verbose"
	echo "$result"
fi

echo "$result" | egrep -q "$SUCCESS"

if [ $? -eq 0 ]; then
	echo -e "${GREEN}TEST PASSED${ENDCOLOR} for $file"
else
	echo -e "${RED}TEST FAILED${ENDCOLOR} for $file"
fi
