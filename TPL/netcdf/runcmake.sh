#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi

OS=`uname -s`
if [ "$OS" = "Darwin" ] ; then
LD_EXT="dylib"
else
LD_EXT="so"
fi

NEEDS_ZLIB="${NEEDS_ZLIB:-NO}"
if [ "$NEEDS_ZLIB" == "YES" ]
then
   LOCAL_ZLIB="-DZLIB_INCLUDE_DIR:PATH=${ACCESS}/include -DZLIB_LIBRARY:FILEPATH=${ACCESS}/lib/libz.${LD_EXT}"
fi

MPI="${MPI:-OFF}"
if [ "$MPI" == "ON" ]
then
  export CC=mpicc
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
fi

rm -f config.cache

cmake .. -DCMAKE_C_COMPILER:FILEPATH=${CC} \
         -DBUILD_SHARED_LIBS:BOOL=ON \
	 -DBUILD_TESTING:BOOL=OFF \
         -DCMAKE_INSTALL_PREFIX=${ACCESS} \
         -DCMAKE_INSTALL_LIBDIR:PATH=lib \
         -DENABLE_NETCDF_4:BOOL=ON \
         -DENABLE_PNETCDF:BOOL=${MPI} \
         -DENABLE_CDF5:BOOL=TRUE \
         -DENABLE_MMAP:BOOL=ON \
         -DENABLE_DAP:BOOL=OFF \
         -DENABLE_V2_API:BOOL=ON \
	 ${LOCAL_ZLIB} \
	 -DENABLE_CONVERSION_WARNINGS:BOOL=OFF \
         -DHDF5_C_LIBRARY:PATH=${ACCESS}/lib/libhdf5.${LD_EXT} \
         -DHDF5_HL_LIBRARY:PATH=${ACCESS}/lib/libhdf5_hl.${LD_EXT} \
         -DHDF5_INCLUDE_DIR:PATH=${ACCESS}/include

echo ""
echo "     MPI: ${MPI}"
echo "COMPILER: ${CC}"
echo "  ACCESS: ${ACCESS}"
echo ""
