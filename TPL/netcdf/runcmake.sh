#! /usr/bin/env bash
### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

HDF5="${HDF5:-YES}"
DEBUG="${DEBUG:-NO}"
if [ "$DEBUG" == "YES" ]
then
  BUILD_TYPE="DEBUG"
else
  BUILD_TYPE="RELEASE"
fi

PREFIX="NETCDF_"

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

if [[ "$HDF5" == "ON" || "$HDF5" == "YES" ]]
then
    export HDF5_PLUGIN_PATH=${INSTALL_PATH}/lib/hdf5/lib/plugin
    HDF5_INFO="-DHDF5_ROOT:PATH=${INSTALL_PATH} -DHDF5_DIR:PATH=${INSTALL_PATH} -D${PREFIX}ENABLE_NETCDF4:BOOL=ON -D${PREFIX}PLUGIN_INSTALL_DIR=YES"
else
    HDF5_INFO="-DENABLE_HDF5=OFF -D${PREFIX}ENABLE_NETCDF4:BOOL=OFF"
fi

NEEDS_ZLIB="${NEEDS_ZLIB:-NO}"
if [ "$NEEDS_ZLIB" == "YES" ]
then
   LOCAL_ZLIB="-DZLIB_INCLUDE_DIR:PATH=${INSTALL_PATH}/include -DZLIB_LIBRARY:FILEPATH=${INSTALL_PATH}/lib/libz.${LD_EXT}"
fi

NEEDS_SZIP="${NEEDS_SZIP:-NO}"
if [ "$NEEDS_SZIP" == "YES" ]
then
   LOCAL_SZIP="-DSZIP_INCLUDE_DIR:PATH=${INSTALL_PATH}/include -DSZIP_LIBRARY:FILEPATH=${INSTALL_PATH}/lib/libsz.${LD_EXT}"
else
   LOCAL_SZIP="-D${PREFIX}ENABLE_FILTER_SZIP=NO"
fi

. ${ACCESS}/TPL/compiler.sh


# If using an XLF compiler on an IBM system, may need to add the following:
# -DCMAKE_Fortran_FLAGS="-qfixed=72" \
# -DCMAKE_EXE_LINKER_FLAGS:STRING="-lxl -lxlopt"

rm -f config.cache

cmake .. -DCMAKE_C_COMPILER:FILEPATH=${CC} \
         -DBUILD_SHARED_LIBS:BOOL=${SHARED} \
         -DBUILD_TESTING:BOOL=OFF \
         -DCMAKE_INSTALL_PREFIX=${INSTALL_PATH} \
         -DCMAKE_INSTALL_LIBDIR:PATH=lib \
	 -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
	 -DCMAKE_PREFIX_PATH=${INSTALL_PATH} \
         -D${PREFIX}ENABLE_PNETCDF:BOOL=${MPI} \
         -D${PREFIX}ENABLE_CDF5=ON \
         -D${PREFIX}ENABLE_MMAP:BOOL=ON \
         -D${PREFIX}ENABLE_DAP:BOOL=OFF \
         -D${PREFIX}ENABLE_BYTERANGE:BOOL=OFF \
	 -D${PREFIX}ENABLE_NCZARR_FILTERS:BOOL=OFF \
	 -D${PREFIX}ENABLE_NCZARR:BOOL=OFF \
         -D${PREFIX}ENABLE_V2_API:BOOL=OFF \
	 -D${PREFIX}ENABLE_FILTER_TESTING:BOOL=OFF \
	 -DENABLE_PLUGIN_INSTALL:BOOL=ON \
	 -D${PREFIX}ENABLE_TESTS:BOOL=OFF \
         -D${PREFIX}ENABLE_QUANTIZE:BOOL=ON \
         ${LOCAL_ZLIB} \
         ${LOCAL_SZIP} \
         ${EXTRA_DEPS} \
	 ${HDF5_INFO} \
         -D${PREFIX}ENABLE_CONVERSION_WARNINGS:BOOL=OFF

echo ""
echo "         MPI: ${MPI}"
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
