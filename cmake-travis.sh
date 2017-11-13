#! /usr/bin/env bash

BUILDDIR=${1:-build}
KOKKOS=${KOKKOS:-OFF}
MPI=${MPI:-OFF}
CGNS=${CGNS:-ON}
MATIO=${MATIO:-ON}
FORTRAN=${FORTRAN:-ON}
ACCESS=`pwd`
THREAD_SAFE=${THREAD_SAFE:-OFF}

# ==================== CONFIGURE SEACAS ====================
mkdir $BUILDDIR && cd $BUILDDIR

if [ "${MPI}" == "ON" ]
then
   MPI_EXEC=`which mpiexec`
   MPI_BIN=`dirname "${MPI_EXEC}"`
   MPI_SYMBOLS="-DCMAKE_CXX_COMPILER:FILEPATH=mpicxx -DCMAKE_C_COMPILER:FILEPATH=mpicc -DCMAKE_Fortran_COMPILER:FILEPATH=mpif77 -DMPI_BIN_DIR:PATH=${MPI_BIN}"
fi

### TPLs -- 
### Make sure these point to the locations to find the libraries and includes in lib and include
### subdirectories of the specified paths.
### For example, netcdf.h should be in ${NETCDF_PATH}/include
NETCDF_PATH=${ACCESS}
PNETCDF_PATH=${ACCESS}
MATIO_PATH=${ACCESS}
HDF5_PATH=${ACCESS}
CGNS_PATH=${ACCESS}
DATAWAREHOUSE_PATH=${ACCESS}

function check_enable()
{
    local path=$1
    if [ -e "${path}" ]
    then
	echo "ON"
    else
	echo "OFF"
    fi
}

HAVE_NETCDF=`check_enable "${NETCDF_PATH}/include/netcdf.h"`
HAVE_PNETCDF=`check_enable "${PNETCDF_PATH}/include/pnetcdf.h"`
HAVE_MATIO=`check_enable "${MATIO_PATH}/include/matio.h"`
HAVE_CGNS=`check_enable "${CGNS_PATH}/include/cgnslib.h"`
HAVE_DATAWAREHOUSE=OFF

### Define to NO to *enable* exodus deprecated functions
OMIT_DEPRECATED_CODE="NO"

CUDA_PATH=${CUDA_ROOT} #Set this to the appropriate path

### Set to ON for CUDA compile; otherwise OFF (default)
CUDA="OFF"

if [ "$KOKKOS" == "ON" ]
then
  if [ "$CUDA" == "ON" ]
  then
    export CUDA_MANAGED_FORCE_DEVICE_ALLOC=1
    KOKKOS_SYMBOLS="-DTPL_ENABLE_CUDA:Bool=ON -DCUDA_TOOLKIT_ROOT_DIR:PATH=${CUDA_PATH} -DTPL_ENABLE_Pthread:Bool=OFF"
  else
    unset CUDA_MANAGED_FORCE_DEVICE_ALLOC
    KOKKOS_SYMBOLS="-DSEACASProj_ENABLE_OpenMP:Bool=ON -DTPL_ENABLE_Pthread:Bool=OFF"
  fi
else
  KOKKOS_SYMBOLS="-D SEACASProj_ENABLE_Kokkos:BOOL=OFF"
fi

BUILD_TYPE="${BUILD_TYPE:-RELEASE}"

if [ "$THREAD_SAFE" == "ON" ]
then
  TS_SYMBOLS="-D TPL_ENABLE_Pthread:BOOL=ON -D SEACASExodus_ENABLE_THREADSAFE:BOOL=ON -D SEACASIoss_ENABLE_THREADSAFE:BOOL=ON"
else
  TS_SYMBOLS="-D SEACASExodus_ENABLE_THREADSAFE:BOOL=OFF -D SEACASIoss_ENABLE_THREADSAFE:BOOL=OFF"
fi

###------------------------------------------------------------------------
cmake  \
${MPI_SYMBOLS} \
-D CMAKE_CXX_FLAGS="-Wall -Wunused -pedantic" \
-D CMAKE_C_FLAGS="-Wall -Wunused -pedantic" \
-D CMAKE_MACOSX_RPATH:BOOL=ON \
-D CMAKE_INSTALL_RPATH:PATH=${INSTALL_PATH}/lib \
-D BUILD_SHARED_LIBS:BOOL=OFF \
-D CMAKE_BUILD_TYPE=${BUILD_TYPE} \
-D SEACASProj_ENABLE_ALL_PACKAGES:BOOL=ON \
-D SEACASProj_ENABLE_ALL_OPTIONAL_PACKAGES:BOOL=ON \
-D SEACASProj_ENABLE_SECONDARY_TESTED_CODE:BOOL=ON \
-D SEACASProj_ENABLE_TESTS=ON \
-D CMAKE_INSTALL_PREFIX:PATH=${INSTALL_PATH} \
-D SEACASProj_SKIP_FORTRANCINTERFACE_VERIFY_TEST:BOOL=ON \
-D SEACASProj_HIDE_DEPRECATED_CODE:BOOL=${OMIT_DEPRECATED_CODE} \
\
-D TPL_ENABLE_Netcdf:BOOL=${HAVE_NETCDF} \
-D TPL_ENABLE_Matio:BOOL=${HAVE_MATIO} \
-D TPL_ENABLE_CGNS:BOOL=${HAVE_CGNS} \
-D TPL_ENABLE_MPI:BOOL=${MPI} \
-D TPL_ENABLE_Pamgen:BOOL=OFF \
-D TPL_ENABLE_Pthread:BOOL=${THREADSAFE} \
-D SEACASExodus_ENABLE_THREADSAFE:BOOL=${THREADSAFE} \
-D SEACASIoss_ENABLE_THREADSAFE:BOOL=${THREADSAFE} \
-D TPL_X11_INCLUDE_DIRS:PATH=/opt/local/include \
-D TPL_ENABLE_X11:BOOL=${HAVE_X11} \
\
${KOKKOS_SYMBOLS} \
${DW_SYMBOLS} \
\
-D MPI_BIN_DIR:PATH=${MPI_BIN} \
-D NetCDF_ROOT:PATH=${NETCDF_PATH} \
-D HDF5_ROOT:PATH=${HDF5_PATH} \
-D HDF5_NO_SYSTEM_PATHS=ON \
-D CGNS_ROOT:PATH=${CGNS_PATH} \
-D Matio_ROOT:PATH=${MATIO_PATH} \
-D PNetCDF_ROOT:PATH=${PNETCDF_PATH} \
\
$EXTRA_ARGS \
..

echo ""
echo "     ACCESS: ${ACCESS}"
echo "        MPI: ${MPI}"
echo "THREAD_SAFE: ${THREAD_SAFE}"
echo "HAVE_NETCDF: ${HAVE_NETCDF}"
echo " HAVE_MATIO: ${HAVE_MATIO}"
echo "  HAVE_CGNS: ${HAVE_CGNS}"
echo "     KOKKOS: ${KOKKOS}"
echo ""

make -j2

cd ${ACCESS}
