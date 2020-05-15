#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi

SHARED="${SHARED:-ON}"
if [[ "$SHARED" == "ON" || "$SHARED" == "YES" ]]
then
  USE_SHARED="1"
else
  USE_SHARED="0"
fi

MPI="${MPI:-ON}"
if [ "$MPI" == "ON" ]
then
  if [ "$CRAY" == "ON" ]
  then
    export CC=cc
  else
    export CC=mpicc
  fi
else
   echo "MPI Must be ON"
   exit 1
fi

METIS_PATH=${ACCESS}/TPL/metis/metis-5.1.0
make config cc=${CC} prefix=${ACCESS} shared=${USE_SHARED} metis_path=${METIS_PATH} gklib_path=${METIS_PATH}/GKlib

echo ""
echo "     MPI: ${MPI}"
echo "COMPILER: ${CC}"
echo "  ACCESS: ${ACCESS}"
echo ""
