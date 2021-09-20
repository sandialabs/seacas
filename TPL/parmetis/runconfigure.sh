#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

SHARED="${SHARED:-YES}"
if [[ "$SHARED" == "ON" || "$SHARED" == "YES" ]]
then
  USE_SHARED="1"
else
  USE_SHARED="0"
fi

MPI="${MPI:-YES}"
if [ "$MPI" != "YES" ] ; then
   echo "MPI Must be YES"
   exit 1
fi
. ${ACCESS}/TPL/compiler.sh

METIS_PATH=${ACCESS}/TPL/metis/METIS-5.1.0.1
make config cc=${CC} prefix=${INSTALL_PATH} shared=${USE_SHARED} metis_path=${METIS_PATH}/src gklib_path=${METIS_PATH}/src/GKlib

echo ""
echo "         MPI: ${MPI}"
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
