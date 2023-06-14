#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

. ${ACCESS}/TPL/compiler.sh

rm -f config.cache

HDF5="${HDF5:-YES}"
if [ "$HDF5" == "YES" ]
then
  DEFAULT_VERSION="7.3"
else
  DEFAULT_VERSION="5"
fi

DEBUG="${DEBUG:-NO}"
if [ "$DEBUG" == "YES" ]
then
  BUILD_TYPE="DEBUG"
else
  BUILD_TYPE="RELEASE"
fi

SHARED="${SHARED:-YES}"
if [[ "$SHARED" == "ON" || "$SHARED" == "YES" ]]
then
  OS=$(uname -s)
  if [ "$OS" = "Darwin" ] ; then
    LD_EXT="dylib"
  else
    LD_EXT="so"
  fi
else
  LD_EXT="a"
  EXTRA_DEPS="-DNC_EXTRA_DEPS=-ldl\;-lz"
fi

NEEDS_ZLIB="${NEEDS_ZLIB:-NO}"
if [ "$NEEDS_ZLIB" == "YES" ]
then
   LOCAL_ZLIB="-DZLIB_INCLUDE_DIR:PATH=${INSTALL_PATH}/include -DZLIB_LIBRARY:FILEPATH=${INSTALL_PATH}/lib/libz.${LD_EXT}"
fi

if [ "$MPI" == "YES" ]
then
    cd ..
    patch -p1 < ../mpi.patch
    cd -
fi

rm -f config.cache

cmake .. -DCMAKE_C_COMPILER:FILEPATH=${CC} \
         -DBUILD_SHARED_LIBS:BOOL=${SHARED} \
         -DMATIO_SHARED:BOOL=${SHARED} \
	 -DMATIO_DEFAULT_FILE_VERSION=${DEFAULT_VERSION} \
         -DMATIO_MAT73:BOOL=${HDF5} \
         -DCMAKE_INSTALL_PREFIX=${INSTALL_PATH} \
         -DCMAKE_INSTALL_LIBDIR:PATH=lib \
	 -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
         -DMATIO_WITH_HDF5:BOOL=${HDF5} \
         -DMATIO_WITH_ZLIB:BOOL=ON \
         -DHDF5_ROOT:PATH=${INSTALL_PATH} \
         -DHDF5_DIR:PATH=${INSTALL_PATH} \
	 ${LOCAL_ZLIB} ${EXTRA_DEPS}


echo ""
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
