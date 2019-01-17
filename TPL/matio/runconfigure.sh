#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

MPI="${MPI:-OFF}"
if [ "$MPI" == "ON" ]
then
  if [ "$CRAY" == "ON" ]
  then
    export CC=cc
  else
    export CC=mpicc
  fi
else
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
fi

rm -f config.cache
CC="${CC}"; export CC
CFLAGS="-I${INSTALL_PATH}/include"; export CFLAGS
CPPFLAGS='-DNDEBUG'; export CPPFLAGS

# Find hdf5 library...
LDFLAGS="-L${INSTALL_PATH}/lib"; export LDFLAGS
if [ "$CRAY" == "ON" ]
then
    USE_SHARED="--disable-shared"
else
    SHARED="${SHARED:-ON}"
    if [[ "$SHARED" == "ON" || "$SHARED" == "YES" ]]
    then
	USE_SHARED="--enable-shared"
    else
	USE_SHARED="--disable-shared"
    fi
fi

./configure --with-hdf5=${INSTALL_PATH} --enable-mat73 ${USE_SHARED} --prefix=${INSTALL_PATH} $1

echo ""
echo "         MPI: ${MPI}"
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
