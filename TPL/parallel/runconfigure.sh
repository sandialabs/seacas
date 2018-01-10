#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi

COMPILER="${COMPILER:-gnu}"
if [ "$COMPILER" == "gnu" ]
then
    export CC=gcc
fi
if [ "$COMPILER" == "clang" ]
then
    export CC=clang
fi
CFLAGS="-I${ACCESS}/include"; export CFLAGS
CPPFLAGS='-DNDEBUG'; export CPPFLAGS
LDFLAGS="-L${ACCESS}/lib"; export LDFLAGS

rm -f config.cache
./configure --prefix=${ACCESS} $1

echo ""
echo "COMPILER: ${CC}"
echo "  ACCESS: ${ACCESS}"
echo ""
