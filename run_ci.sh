#!/bin/bash

# must be run in the root of the repository.

set -x # echo on
set -e # exit on failure

# some system info
uname -a

echo -e "\n\n\t----- building llvm -----\n\n"

./llvm/fresh-build.sh

echo -e "\n\n\t----- building manticore -----\n\n"

autoheader -Iconfig
autoconf -Iconfig
./configure
make local-install


########## start of testing stuff ##########

# we want to try all configurations and _then_ 
# report failure if any one of them fails,
# so we use || here to collect failure results.

failed=0

echo -e "\n\n\t----- testing with MLRISC -----\n\n"
./src/regression-tests/bash-scripts/run-seq.bsh || failed=1
./src/regression-tests/bash-scripts/run-par.bsh || failed=1
echo -e "\n\n\t----- done -----\n\n"


llvmOptions="-llvm -llopt1 -llopt2 -llopt3 -llopt4 -llopt5"

for llvmKind in  $llvmOptions; do
    echo -e "\n\n\t----- testing with $llvmKind -----\n\n"
    BACKEND="$llvmKind" ./src/regression-tests/bash-scripts/run-seq.bsh || failed=1
    BACKEND="$llvmKind" ./src/regression-tests/bash-scripts/run-par.bsh || failed=1
    echo -e "\n\n\t----- done -----\n\n"
done

# Exit with error if any tests failed
if [ "$failed" -ne 0 ] ; then
    exit 1
fi
