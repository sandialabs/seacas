#! /usr/bin/env bash
EXTRA_ARGS=$@

MPI="${MPI:-NO}"

echo "MPI set to ${MPI}"

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

SHARED="${SHARED:-YES}"

if [ "$MPI" == "YES" ] && [ "$CRAY" = "YES" ]
then
  export CC=cc
  export CXX=cxx
elif [ "$MPI" == "YES" ]
then
  export CC=mpicc
  export CXX=mpicxx
else
  COMPILER="${COMPILER:-gnu}"
  if [ "$COMPILER" == "gnu" ]
  then
      export CC=gcc
      export CXX=g++
  fi
  if [ "$COMPILER" == "clang" ]
  then
      export CC=clang
      export CXX=clang++
  fi
  if [ "$COMPILER" == "intel" ]
  then
      export CC=icc
      export CXX=icpc
  fi
  if [ "$COMPILER" == "ibm" ]
  then
      export CC=xlc
      export CXX=xlC
  fi
fi

CFLAGS="-I${INSTALL_PATH}/include"; export CFLAGS
CPPFLAGS="-DNDEBUG"; export CPPFLAGS

rm -f CMakeCache.txt

cmake \
${RPATH} \
-D CATALYST_BUILD_SHARED_LIBS:BOOL=${SHARED} \
-D CMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON \
-D CMAKE_PREFIX_PATH:PATH=${INSTALL_PATH}/lib \
-D CMAKE_INSTALL_PREFIX:PATH=${INSTALL_PATH} \
-D CMAKE_INSTALL_LIBDIR:PATH=lib \
-D CATALYST_BUILD_STUB_IMPLEMENTATION:BOOL=ON \
-D CATALYST_USE_MPI:BOOL=${MPI} \
-D CATALYST_BUILD_TESTING:BOOL=OFF \
-D CATALYST_WRAP_PYTHON:BOOL=OFF \
$EXTRA_ARGS \
..

echo ""
echo "         MPI: ${MPI}"
echo "    COMPILER: ${CC}"
echo "C++ COMPILER: ${CXX}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
