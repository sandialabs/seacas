#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

COMPILER="${COMPILER:-gnu}"
if [ "$COMPILER" == "gnu" ]
then
    export CC=gcc
fi
if [ "$COMPILER" == "clang" ]
then
    export CC=clang
fi
if [ "$COMPILER" == "intel" ]
then
    export CC=icc
fi
if [ "$COMPILER" == "ibm" ]
then
    export CC=xlc
fi

CFLAGS="-I${INSTALL_PATH}/include"; export CFLAGS
CPPFLAGS='-DNDEBUG'; export CPPFLAGS
LDFLAGS="-L${INSTALL_PATH}/lib"; export LDFLAGS

rm -f config.cache
./configure --prefix=${INSTALL_PATH} $1

echo ""
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
