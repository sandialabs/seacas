#! /usr/bin/env bash
EXTRA_ARGS=$@

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

SHARED="${SHARED:-ON}"

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

CFLAGS="-I${INSTALL_PATH}/include"; export CFLAGS
CPPFLAGS="-DNDEBUG"; export CPPFLAGS

rm -f CMakeCache.txt

cmake \
${RPATH} \
-D BUILD_SHARED_LIBS:BOOL=${SHARED} \
-D CMAKE_PREFIX_PATH:PATH=${INSTALL_PATH}/lib \
-D CMAKE_INSTALL_PREFIX:PATH=${INSTALL_PATH} \
-D CMAKE_INSTALL_LIBDIR:PATH=lib \
-D BUILD_GMOCK:BOOL=OFF \
$EXTRA_ARGS \
..

echo ""
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
