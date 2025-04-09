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
if [ "$NEEDS_ZLIB" == "YES" ] || [ "$NEEDS_ZLIB" == "ON" ]
then
   LOCAL_ZLIB="-DZLIB_INCLUDE_DIR:PATH=${INSTALL_PATH}/include -DZLIB_LIBRARY:FILEPATH=${INSTALL_PATH}/lib/libz.${LD_EXT}"
fi

NEEDS_SZIP="${NEEDS_SZIP:-NO}"
if [ "$NEEDS_SZIP" == "YES" ] || [ "$NEEDS_SZIP" == "ON" ]
then
   LOCAL_SZIP="-DHDF5_ENABLE_SZIP_SUPPORT:BOOL=${NEEDS_SZIP} -DSZIP_INCLUDE_DIR:PATH=${INSTALL_PATH}/include -DSZIP_LIBRARY:FILEPATH=${INSTALL_PATH}/lib/libsz.${LD_EXT}"
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
	 -DDEFAULT_API_VERSION=V18 \
	 -DH5_DEFAULT_PLUGINDIR:PATH=${INSTALL_PATH}/lib/hdf5/lib/plugin \
	 -DHDF5_ENABLE_PARALLEL:BOOL=${MPI} \
	 -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON \
	 -DHDF5_BUILD_CPP_LIB:BOOL=${H5CPP} \
	 -DHDF5_BUILD_FORTRAN:BOOL=OFF \
	 -DHDF5_BUILD_HL_LIB:BOOL=ON \
	 -DHDF5_DISABLE_COMPILER_WARNINGS:BOOL=ON \
	 ${LOCAL_ZLIB} ${LOCAL_SZIP} \
         ${EXTRA_DEPS}

echo ""
echo "         MPI: ${MPI}"
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
