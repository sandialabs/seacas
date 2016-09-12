#! /usr/bin/env bash
if [ "$1" != "" ]; then
   BUILDDIR=$1
else
   BUILDDIR=build
fi

ACCESS=`pwd`

# ==================== INSTALL CGNS ====================
cd TPL/cgns
git clone https://github.com/cgns/CGNS
cd CGNS
mkdir build
cd build
MPI=OFF sh ../../runconfigure.sh
make && sudo make install

cd $ACCESS
pwd

mkdir $BUILDDIR && cd $BUILDDIR

CUDA_PATH=${CUDA_ROOT} #Set this to the appropriate path

### Set to ON for CUDA compile; otherwise OFF (default)
CUDA="OFF"

if [ "$CUDA" == "ON" ]
then
  export CUDA_MANAGED_FORCE_DEVICE_ALLOC=1
  KOKKOS_SYMBOLS="-DTPL_ENABLE_CUDA:Bool=ON -DCUDA_TOOLKIT_ROOT_DIR:PATH=${CUDA_PATH} -DTPL_ENABLE_Pthread:Bool=OFF"
else
  unset CUDA_MANAGED_FORCE_DEVICE_ALLOC
  KOKKOS_SYMBOLS="-DSEACASProj_ENABLE_OpenMP:Bool=ON -DTPL_ENABLE_Pthread:Bool=OFF"
fi

cmake \
  -DBUILD_SHARED_LIBS:BOOL=ON \
  -DCMAKE_CXX_FLAGS="-Wall -pedantic -pthread" \
  -DCMAKE_C_FLAGS="-Wall -pedantic" \
  -DHDF5_ROOT:PATH=/usr/ \
  -DSEACASProj_ENABLE_ALL_PACKAGES:BOOL=ON \
  -DSEACASProj_ENABLE_SECONDARY_TESTED_CODE:BOOL=ON \
  -DSEACASProj_ENABLE_TESTS:BOOL=ON \
  -DSEACASProj_USE_GNUINSTALLDIRS:BOOL=ON \
  ${KOKKOS_SYMBOLS} \
  -DTPL_ENABLE_CGNS:BOOL=ON \
  -DCGNS_ROOT:PATH=${ACCESS} \
  -DTPL_ENABLE_Matio:BOOL=ON \
  -DTPL_ENABLE_METIS:BOOL=OFF \
  -DTPL_ENABLE_ParMETIS:BOOL=OFF \
  -DTPL_ENABLE_Netcdf:BOOL=ON \
  -DTPL_ENABLE_MPI:BOOL=OFF \
  -DTPL_ENABLE_Pamgen:BOOL=OFF \
  -DTPL_ENABLE_X11:BOOL=ON \
  -DTPL_ENABLE_Zlib:BOOL=ON \
  ../

make -j2

cd ..
