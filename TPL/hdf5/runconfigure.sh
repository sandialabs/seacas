#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi

H5VERSION=${H5VERSION:-V110}

NEEDS_ZLIB="${NEEDS_ZLIB:-NO}"
if [ "$NEEDS_ZLIB" == "YES" ]
then
   ZLIB_ON_OFF="--with-zlib=${ACCESS}"
fi
   
MPI="${MPI:-OFF}"
if [ "$MPI" == "ON" ]
then
  PARALLEL_ON_OFF="--enable-parallel"
  if [ "$CRAY" == "ON" ]
  then
    export CC=cc
  else
    export CC=mpicc
  fi
  echo "Checking MPI Version -- "
  mpiexec --version
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
  if [ "$COMPILER" == "intel" ]
  then
      export CC=icc
  fi
  if [ "$COMPILER" == "ibm" ]
  then
      export CC=xlc
  fi
fi

CPPFLAGS='-DNDEBUG'; export CPPFLAGS

rm -f config.cache


### SEACAS does not use the Fortran, F90, or C++ versions of hdf5 library
FC=''; export FC
F90=''; export F90

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

if [ "${H5VERSION}" == "V18" ]
then
    ./configure --prefix=${ACCESS} ${ZLIB_ON_OFF} ${USE_SHARED} ${PARALLEL_ON_OFF} --enable-static-exec $1
else
    ./configure --prefix=${ACCESS} ${ZLIB_ON_OFF} ${USE_SHARED} ${PARALLEL_ON_OFF} --with-default-api-version=v18 --enable-static-exec $1
fi

echo ""
echo "     MPI: ${MPI}"
echo "COMPILER: ${CC}"
echo "  ACCESS: ${ACCESS}"
echo ""
