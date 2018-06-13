#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi

SHARED="${SHARED:-ON}"
if [[ "$SHARED" == "ON" || "$SHARED" == "YES" ]]
then
  OS=`uname -s`
  if [ "$OS" = "Darwin" ] ; then
    LD_EXT="dylib"
  else
    LD_EXT="so"
  fi
else
  LD_EXT="a"
fi

export LIBS="-ldl -lzlib"
NEEDS_ZLIB="${NEEDS_ZLIB:-NO}"
if [ "$NEEDS_ZLIB" == "YES" ]
then
   LOCAL_ZLIB="-DZLIB_INCLUDE_DIR:PATH=${ACCESS}/include -DZLIB_LIBRARY:FILEPATH=${ACCESS}/lib/libz.${LD_EXT}"
fi

MPI="${MPI:-OFF}"
if [ "$MPI" == "ON" ] && [ "$CRAY" = "ON" ]
then
  export CC=cc
elif [ "$MPI" == "ON" ]
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
  if [ "$COMPILER" == "intel" ]
  then
      export CC=icc
  fi

fi

# If using an XLF compiler on an IBM system, may need to add the following:
# -DCMAKE_Fortran_FLAGS="-qfixed=72" \
# -DCMAKE_EXE_LINKER_FLAGS:STRING="-lxl -lxlopt"

rm -f config.cache

cmake .. -DCMAKE_C_COMPILER:FILEPATH=${CC} \
         -DBUILD_SHARED_LIBS:BOOL=${SHARED} \
	 -DBUILD_TESTING:BOOL=OFF \
         -DCMAKE_INSTALL_PREFIX=${ACCESS} \
         -DCMAKE_INSTALL_LIBDIR:PATH=lib \
         -DENABLE_NETCDF_4:BOOL=ON \
         -DENABLE_PNETCDF:BOOL=${MPI} \
	 -DNC_EXTRA_DEPS="z" \
	 -DENABLE_CDF5:BOOL=TRUE \
         -DENABLE_MMAP:BOOL=ON \
         -DENABLE_DAP:BOOL=OFF \
         -DENABLE_V2_API:BOOL=OFF \
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
