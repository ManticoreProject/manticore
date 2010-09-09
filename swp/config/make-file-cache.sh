#!/bin/sh
#
# COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
# All rights reserved.
#
# Create a cache file for HLOp-definition and rewriting-rule files.
#

if test $# != "1" ; then
  echo "usage: make-file-cache <dir>"
  exit 1
fi

case $1 in
  /*) ;;
  *) echo "make-file-cache: directory must be absolute path"; exit 1;;
esac

if test -d $1 ; then
  find $1 \( -name "*.hlop" -o -name "*.hlrw" \) -print > $1/.cache
  exit 0
else
  echo "make-file-cache: \"$1\" is not a directory"
  exit 1
fi

