#! /bin/sh

# A script to run autoheader, autoconf, etc., b/c I was getting tired of typing it.
# Alter your paths accordingly. - ams

TRUNK=/home/adamshaw/Academics/Projects/Manticore/manticore/trunk
MLRISC_HOME=/home/adamshaw/Academics/Projects/Manticore/manticore/mlrisc/

(cd $TRUNK
 autoheader -Iconfig
 autoconf -Iconfig
 ./configure --with-mlrisc=$MLRISC_HOME/MLRISC
)

(cd src/lib/hlops
 $TRUNK/config/make-file-cache.sh $PWD
)

(cd src/lib/parallel-runtime/build
 make
)

(cd src/tools/mc
 make
)
