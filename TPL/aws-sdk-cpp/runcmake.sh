#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

DEBUG="${DEBUG:-NO}"
if [ "$DEBUG" == "YES" ]
then
  BUILD_TYPE="Debug"
else
  BUILD_TYPE="Release"
fi

if [ "$CRAY" == "YES" ]
then
    SHARED="${SHARED:-NO}"
else
    SHARED="${SHARED:-YES}"
fi

. ${ACCESS}/TPL/compiler.sh

# If using an XLF compiler on an IBM system, may need to add the following:
# -DCMAKE_Fortran_FLAGS="-qfixed=72" \
# -DCMAKE_EXE_LINKER_FLAGS:STRING="-lxl -lxlopt"

rm -f config.cache

cmake .. -DCMAKE_C_COMPILER:FILEPATH=${CC} \
         -DBUILD_SHARED_LIBS:BOOL=${SHARED} \
         -DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON \
         -DBUILD_TESTING:BOOL=OFF \
         -DCMAKE_INSTALL_PREFIX=${INSTALL_PATH} \
	 -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
         \
         -DAUTORUN_UNIT_TESTS:BOOL=OFF

echo ""
echo "         MPI: ${MPI}"
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
