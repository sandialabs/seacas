#! /bin/sh

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
fi
echo "ACCESS set to ${ACCESS}"

MPI="${MPI:-OFF}"
echo "MPI set to ${MPI}"

if [ "$MPI" == "ON" ]
then
  export CC=mpicc
else
  export CC=gcc
fi

make config cc=${CC} prefix=${ACCESS}
