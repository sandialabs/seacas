#! /usr/bin/env bash
EXTRA_ARGS=$@

#MPI="${MPI:-ON}"
MPI="${MPI:-OFF}"

USE_64BIT_INT="${USE_64BIT_INT:-OFF}"

echo "MPI set to ${MPI}"

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi

SHARED="${SHARED:-ON}"

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

CFLAGS="-I${ACCESS}/include"; export CFLAGS
CPPFLAGS="-DNDEBUG"; export CPPFLAGS
OS=`uname -s`

rm -f CMakeCache.txt

if [ "$OS" == "Darwin" ] ; then
  RPATH="-D CMAKE_MACOSX_RPATH:BOOL=ON -D CMAKE_INSTALL_RPATH:PATH=${INSTALL_PATH}/lib"
  if [[ "$SHARED" == "ON" || "$SHARED" == "YES" ]]
  then
      LD_EXT="dylib"
  else
      LD_EXT="a"
  fi
  LIB="-DHDF5_LIBRARY:PATH=${ACCESS}/lib/libhdf5.${LD_EXT} -DHDF5_LIBRARIES:PATH=${ACCESS}/lib/libhdf5.${LD_EXT}"
else
  LIB="-DHDF5_LIBRARY:PATH=${ACCESS}/lib"
fi

cmake \
${RPATH} \
-D CGNS_BUILD_SHARED:BOOL=${SHARED} \
-D CGNS_ENABLE_HDF5:BOOL=ON \
${LIB} \
-D HDF5_ROOT=${ACCESS} \
-D HDF5_NEED_ZLIB:BOOL=ON \
-D CGNS_ENABLE_64BIT:BOOL=${USE_64BIT_INT} \
-D CGNS_ENABLE_SCOPING:BOOL=ON \
-D CGNS_ENABLE_FORTRAN:BOOL=OFF \
-D CGNS_ENABLE_PARALLEL:BOOL=${MPI} \
-D CMAKE_PREFIX_PATH:PATH=${ACCESS}/lib \
-D CMAKE_INSTALL_PREFIX:PATH=${ACCESS} \
-D HDF5_NEED_MPI:BOOL=${MPI} \
-D HDF5_IS_PARALLEL:BOOL=${MPI} \
$EXTRA_ARGS \
..

echo ""
echo "     MPI: ${MPI}"
echo "COMPILER: ${CC}"
echo "  ACCESS: ${ACCESS}"
echo ""
