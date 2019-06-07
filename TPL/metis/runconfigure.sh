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
  if [ "$COMPILER" == "ibm" ]
  then
      export CC=xlc
  fi
fi

make config cc=${CC} prefix=${ACCESS}

echo ""
echo "     MPI: ${MPI}"
echo "COMPILER: ${CC}"
echo "  ACCESS: ${ACCESS}"
echo ""
