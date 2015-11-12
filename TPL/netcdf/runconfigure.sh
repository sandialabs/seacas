#! /bin/sh

### Set to ON for parallel compile; otherwise OFF for serial (default)
MPI=OFF
SHARED="ON"

### Change this to point to the compilers you want to use
if [ "$MPI" == "ON" ]
then
  CXX=mpicxx
  CC=mpicc
  FC=mpif77
  PNETCDF="ON"
else
  CXX=g++
  CC=gcc
  FC=gfortran
  PNETCDF="OFF"
fi


rm -f CMakeCache.txt

cmake .. -DCMAKE_C_COMPILER:FILEPATH=${CC} \
         -DHDF5_DIR=${ACCESS} \
         -DENABLE_PNETCDF=${PNETCDF} \
         -DENABLE_PARALLEL4=${MPI} \
         -DENABLE_PARALLEL_TESTS=${MPI} \
         -DENABLE_NETCDF_4=ON \
         -DENABLE_DAP=OFF \
         -DENABLE_FSYNC=OFF \
         -DENABLE_V2_API=OFF \
         -DBUILD_SHARED_LIBS=${SHARED} \
         -DCMAKE_INSTALL_PREFIX=${ACCESS}
