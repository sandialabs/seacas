#! /usr/bin/env bash
EXTRA_ARGS=$@

#MPI="${MPI:-YES}"
MPI="${MPI:-NO}"

echo "MPI set to ${MPI}"

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

SHARED="${SHARED:-YES}"
DEBUG="${DEBUG:-NO}"

if [ "$DEBUG" == "YES" ]
then
  BUILD_TYPE="DEBUG"
else
  BUILD_TYPE="RELEASE"
fi

. ${ACCESS}/TPL/compiler.sh

CFLAGS="-I${INSTALL_PATH}/include"; export CFLAGS
CPPFLAGS="-DNDEBUG"; export CPPFLAGS
OS=$(uname -s)

rm -f CMakeCache.txt

if [ "$OS" == "Darwin" ] ; then
  RPATH="-D CMAKE_MACOSX_RPATH:BOOL=YES"
fi

cmake \
${RPATH} \
-D CMAKE_INSTALL_RPATH:PATH=${INSTALL_PATH}/lib \
-D BUILD_SHARED_LIBS:BOOL=${SHARED} \
-D CMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON \
-D CMAKE_INSTALL_LIBDIR:PATH=lib \
-D CMAKE_INSTALL_PREFIX:PATH=${INSTALL_PATH} \
-D CMAKE_BUILD_TYPE=${BUILD_TYPE} \
$EXTRA_ARGS \
..

echo ""
echo "         MPI: ${MPI}"
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
