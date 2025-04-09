#! /usr/bin/env bash
EXTRA_ARGS=$@

MPI="${MPI:-NO}"
CUDA="${CUDA:-NO}"

if [ "$CUDA" == "ON" ] || [ "$CUDA" == "YES" ] ; then
    if [ "X$CUDA_PATH" == "X" ] ; then
	echo "ERROR: CUDA_PATH must be set if CUDA is enabled in kokkos build."
	exit 1
    else
	echo "CUDA_PATH set to ${CUDA_PATH}"
    fi
fi

echo "MPI set to ${MPI}"

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

SHARED="${SHARED:-YES}"

. ${ACCESS}/TPL/compiler.sh

CFLAGS="-I${INSTALL_PATH}/include"; export CFLAGS
CPPFLAGS="-DNDEBUG"; export CPPFLAGS

rm -f CMakeCache.txt

if [ "$CUDA" == "YES" ]
then
    export "OMPI_CXX=../config/nvcc_wrapper"
    export CUDA_MANAGED_FORCE_DEVICE_ALLOC=1
    KOKKOS_SYMBOLS="-D CUDA_TOOLKIT_ROOT_DIR:PATH=${CUDA_PATH}"
  else
    export OMPI_CXX=$(which gcc)
    unset CUDA_MANAGED_FORCE_DEVICE_ALLOC
fi

cmake \
${RPATH} \
-D BUILD_SHARED_LIBS:BOOL=${SHARED} \
-D CMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON \
-D CMAKE_PREFIX_PATH:PATH=${INSTALL_PATH}/lib \
-D CMAKE_INSTALL_PREFIX:PATH=${INSTALL_PATH} \
-D CMAKE_INSTALL_LIBDIR:PATH=lib \
-D Kokkos_ENABLE_CUDA:BOOL=${CUDA} \
-D Kokkos_ENABLE_DEPRECATED_CODE:BOOL=OFF \
-D Kokkos_ENABLE_PTHREAD:BOOL=OFF \
-D Kokkos_ARCH_NONE=ON \
${KOKKOS_SYMBOLS} \
$EXTRA_ARGS \
..

echo ""
echo "        CUDA: ${CUDA}"
echo "         MPI: ${MPI}"
echo "    COMPILER: ${CC}"
echo "C++ COMPILER: ${CXX}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
