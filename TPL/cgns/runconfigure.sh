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

if [ "$MPI" == "ON" ]
then
  export CC=mpicc
else
  export CC=gcc
fi

CFLAGS="-I${ACCESS}/include"; export CFLAGS
CPPFLAGS="-DNDEBUG"; export CPPFLAGS
OS=`uname -s`

rm -f CMakeCache.txt

if [ "$OS" == "Darwin" ] ; then
  RPATH="-D CMAKE_MACOSX_RPATH:BOOL=ON -D CMAKE_INSTALL_RPATH:PATH=${INSTALL_PATH}/lib"
  LD_EXT="dylib"
  LIB="-DHDF5_LIBRARY:PATH=${ACCESS}/lib/libhdf5.${LD_EXT} -DHDF5_LIBRARIES:PATH=${ACCESS}/lib/libhdf5.${LD_EXT}"
else
  LD_EXT="so"
  LIB="-DHDF5_LIBRARY:PATH=${ACCESS}/lib"
fi

cmake \
${RPATH} \
-D CGNS_BUILD_SHARED:BOOL=ON \
-D CGNS_ENABLE_HDF5:BOOL=ON \
${LIB} \
-D HDF5_INCLUDE_DIRS:PATH=${ACCESS}/include \
-D HDF5_NEED_ZLIB:BOOL=ON \
-D CGNS_ENABLE_64BIT:BOOL=${USE_64BIT_INT} \
-D CGNS_ENABLE_SCOPING:BOOL=ON \
-D CGNS_ENABLE_FORTRAN:BOOL=OFF \
-D CGNS_ENABLE_PARALLEL:BOOL=${MPI} \
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
