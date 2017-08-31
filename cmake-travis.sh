#! /usr/bin/env bash

BUILDDIR=${1:-build}
KOKKOS=${KOKKOS:-OFF}
CGNS=${CGNS:-ON}
FORTRAN=${FORTRAN:-ON}
ACCESS=`pwd`

# =================== INSTALL PNETCDF and NETCDF (if mpi) ===============
if [ "$MPI" == "ON" ]
then

cd TPL/pnetcdf
wget http://cucis.ece.northwestern.edu/projects/PnetCDF/Release/parallel-netcdf-1.8.1.tar.gz
tar -xzf parallel-netcdf-1.8.1.tar.gz
cd parallel-netcdf-1.8.1
ACCESS="/usr/local" bash ../runconfigure.sh
make && sudo make install

cd $ACCESS

cd TPL/netcdf
wget https://github.com/Unidata/netcdf-c/archive/v4.4.1.1.tar.gz
tar -xzf v4.4.1.1.tar.gz
cd netcdf-c-4.4.1.1
MPI=${MPI} ACCESS="/usr/local" bash ../runconfigure.sh
make && sudo make install

cd $ACCESS
pwd

fi

# ==================== INSTALL CGNS ====================
if [ "$CGNS" == "ON" ]
then
    
cd TPL/cgns
git clone https://github.com/cgns/CGNS
cd CGNS
# git checkout develop
mkdir build
cd build
MPI=${MPI} ACCESS="/usr/local" bash ../../runconfigure.sh
make && sudo make install

cd $ACCESS
pwd

fi

# ==================== CONFIGURE SEACAS ====================
mkdir $BUILDDIR && cd $BUILDDIR

if [ "${MPI}" == "ON" ]
then
   MPI_EXEC=`which mpiexec`
   MPI_BIN=`dirname "${MPI_EXEC}"`
   MPI_SYMBOLS="-DCMAKE_CXX_COMPILER:FILEPATH=mpicxx -DCMAKE_C_COMPILER:FILEPATH=mpicc -DCMAKE_Fortran_COMPILER:FILEPATH=mpif77 -DMPI_BIN_DIR:PATH=${MPI_BIN}"
fi

CUDA_PATH=${CUDA_ROOT} #Set this to the appropriate path

### Set to ON for CUDA compile; otherwise OFF (default)
CUDA="OFF"

echo "KOKKOS = ${KOKKOS}"

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

echo "THREAD_SAFE = ${THREAD_SAFE}"
if [ "$THREAD_SAFE" == "ON" ]
then
  TS_SYMBOLS="-D TPL_ENABLE_Pthread:BOOL=ON -D SEACASExodus_ENABLE_THREADSAFE:BOOL=ON -D SEACASIoss_ENABLE_THREADSAFE:BOOL=ON"
else
  TS_SYMBOLS="-D SEACASExodus_ENABLE_THREADSAFE:BOOL=OFF -D SEACASIoss_ENABLE_THREADSAFE:BOOL=OFF"
fi

cmake \
  -DTPL_ENABLE_MPI=${MPI} \
  ${MPI_SYMBOLS} \
  -DBUILD_SHARED_LIBS:BOOL=ON \
  -DCMAKE_CXX_FLAGS="-Wall -pedantic -pthread" \
  -DCMAKE_C_FLAGS="-Wall -pedantic" \
  -DHDF5_ROOT:PATH=/usr/ \
  -DSEACASProj_ENABLE_ALL_PACKAGES:BOOL=ON \
  -DSEACASProj_ENABLE_SECONDARY_TESTED_CODE:BOOL=ON \
  -DSEACASProj_ENABLE_TESTS:BOOL=ON \
  -DSEACASProj_ENABLE_Fortran:BOOL=${FORTRAN} \
  -DSEACASProj_USE_GNUINSTALLDIRS:BOOL=ON \
  ${KOKKOS_SYMBOLS} \
  ${TS_SYMBOLS} \
  -DTPL_ENABLE_CGNS:BOOL=${CGNS} \
  -DTPL_ENABLE_Matio:BOOL=ON \
  -DTPL_ENABLE_METIS:BOOL=OFF \
  -DTPL_ENABLE_ParMETIS:BOOL=OFF \
  -DTPL_ENABLE_Netcdf:BOOL=ON \
  -DTPL_ENABLE_Pamgen:BOOL=OFF \
  -DTPL_ENABLE_X11:BOOL=ON \
  -DTPL_ENABLE_Zlib:BOOL=ON \
  -DZoltan_ENABLE_TESTS:BOOL=OFF \
  ../

make -j2

cd ${ACCESS}
