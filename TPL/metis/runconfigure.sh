#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

SHARED="${SHARED:-YES}"

. ${ACCESS}/TPL/compiler.sh

mkdir build
cd build
cmake -DCMAKE_C_COMPILER:FILEPATH=${CC} \
               -DBUILD_SHARED_LIBS:BOOL=${SHARED} \
               -DCMAKE_INSTALL_PREFIX=${INSTALL_PATH} \
               -DCMAKE_BUILD_TYPE=${BUILD_TYPE} ..

cd ..

echo ""
echo "         MPI: ${MPI}"
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
