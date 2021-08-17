#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

. ${ACCESS}/TPL/compiler.sh

rm -f config.cache

CFLAGS="-I${INSTALL_PATH}/include"; export CFLAGS
CPPFLAGS='-DNDEBUG'; export CPPFLAGS

# Find hdf5 library...
LDFLAGS="-L${INSTALL_PATH}/lib"; export LDFLAGS

SHARED="${SHARED:-YES}"
if [[ "$SHARED" == "ON" || "$SHARED" == "YES" ]]
then
    USE_SHARED="--enable-shared"
else
    USE_SHARED="--disable-shared"
fi

./configure --with-hdf5=${INSTALL_PATH} --enable-mat73 ${USE_SHARED} --prefix=${INSTALL_PATH} $1

echo ""
echo "         MPI: ${MPI}"
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
