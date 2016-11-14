#! /bin/sh

MPI="${MPI:-OFF}"
echo "MPI set to ${MPI}"

COMPILER="${COMPILER:-gnu}"
echo "COMPILER set to ${COMPILER}"
#COMPILER="clang"

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
fi
echo "ACCESS set to ${ACCESS}"

if [ "$MPI" == "ON" ]
then
  PARALLEL_ON_OFF="-enable-parallel"
  if [ "$COMPILER" == "gnu" ]
  then
      export CC=mpicc
  else
      export CC=mpicc-openmpi-clang38
  fi
else
  PARALLEL_ON_OFF="-disable-parallel"
  if [ "$COMPILER" == "gnu" ]
  then
      export CC=gcc
  else
      export CC=clang
  fi
fi

CPPFLAGS='-DNDEBUG'; export CPPFLAGS

rm -f config.cache

### SEACAS does not use the Fortran, F90, or C++ versions of hdf5 library
FC=''; export FC
F90=''; export F90

SHARED="--enable-shared"

./configure --prefix=${ACCESS} ${SHARED} ${PARALLEL_ON_OFF} --enable-asserts --enable-build-mode=debug --enable-static-exec $1

echo ""
echo "     MPI: ${MPI}"
echo "COMPILER: ${COMPILER}"
echo "  ACCESS: ${ACCESS}"
echo ""
