#! /usr/bin/env bash

MPI="${MPI:-OFF}"
echo "MPI set to ${MPI}"
COMPILER="${COMPILER:-gnu}"
echo "COMPILER set to ${COMPILER}"

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi

if [ "$MPI" == "ON" ]
then
  export CC=mpicc
else
  if [ "$COMPILER" == "gnu" ]
  then
      export CC=gcc
  else
      export CC=clang
  fi
fi

rm -f config.cache
CC="${CC}"; export CC
CFLAGS="-I${ACCESS}/include"; export CFLAGS
CPPFLAGS='-DNDEBUG'; export CPPFLAGS

# Find hdf5 library...
LDFLAGS="-L${ACCESS}/lib"; export LDFLAGS
SHARED="--enable-shared"

./configure --with-hdf5=${ACCESS} --enable-mat73 ${SHARED} --prefix=${ACCESS} $1

echo ""
echo "     MPI: ${MPI}"
echo "COMPILER: ${CC}"
echo "  ACCESS: ${ACCESS}"
echo ""
