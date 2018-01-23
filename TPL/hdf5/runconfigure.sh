#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi

NEEDS_ZLIB="${NEEDS_ZLIB:-NO}"
if [ "$NEEDS_ZLIB" == "YES" ]
then
   ZLIB_ON_OFF="--with-zlib=${ACCESS}"
fi
   
MPI="${MPI:-OFF}"
if [ "$MPI" == "ON" ]
then
  PARALLEL_ON_OFF="--enable-parallel"
  export CC=mpicc
else
  PARALLEL_ON_OFF="--disable-parallel"
  COMPILER="${COMPILER:-gnu}"
  if [ "$COMPILER" == "gnu" ]
  then
      export CC=gcc
  fi
  if [ "$COMPILER" == "clang" ]
  then
      export CC=clang
  fi
fi

CPPFLAGS='-DNDEBUG'; export CPPFLAGS

rm -f config.cache


### SEACAS does not use the Fortran, F90, or C++ versions of hdf5 library
FC=''; export FC
F90=''; export F90

SHARED="--enable-shared"

./configure --prefix=${ACCESS} ${ZLIB_ON_OFF} ${SHARED} ${PARALLEL_ON_OFF} --enable-static-exec $1

echo ""
echo "     MPI: ${MPI}"
echo "COMPILER: ${CC}"
echo "  ACCESS: ${ACCESS}"
echo ""
