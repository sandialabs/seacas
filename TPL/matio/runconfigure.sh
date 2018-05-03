#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi

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
fi

rm -f config.cache
CC="${CC}"; export CC
CFLAGS="-I${ACCESS}/include"; export CFLAGS
CPPFLAGS='-DNDEBUG'; export CPPFLAGS

# Find hdf5 library...
LDFLAGS="-L${ACCESS}/lib"; export LDFLAGS
if ! [ "$CRAY" == "ON" ]
then
  SHARED="--enable-shared"
fi

./configure --with-hdf5=${ACCESS} --enable-mat73 ${SHARED} --prefix=${ACCESS} $1

echo ""
echo "     MPI: ${MPI}"
echo "COMPILER: ${CC}"
echo "  ACCESS: ${ACCESS}"
echo ""
