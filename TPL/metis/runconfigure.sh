#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi

MPI="${MPI:-OFF}"

if [ "$MPI" == "ON" ]
then
  export CC=mpicc
else
  export CC=gcc
fi

make config cc=${CC} prefix=${ACCESS}

echo ""
echo "     MPI: ${MPI}"
echo "COMPILER: ${CC}"
echo "  ACCESS: ${ACCESS}"
echo ""
