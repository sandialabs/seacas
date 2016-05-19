#! /usr/bin/env sh

# Install parallel version of netcdf library...
ACCESS=`pwd`
pwd
cd TPL/netcdf
wget https://github.com/Unidata/netcdf-c/archive/v4.4.0.tar.gz; fi
tar -xzvf v4.4.0.tar.gz; fi
cd netcdf-c-4.4.0
CC=mpicc ./configure --prefix=$ACCESS --enable-netcdf4 --disable-v2 --disable-fsync --disable-dap && make && sudo make install
cd -
pwd

mkdir build && cd build

cmake \
  -DTPL_ENABLE_MPI=ON \
  -DCMAKE_CXX_COMPILER:FILEPATH=mpicxx \
  -DCMAKE_C_COMPILER:FILEPATH=mpicc \
  -DCMAKE_Fortran_COMPILER:FILEPATH=mpif77 \
  -DBUILD_SHARED_LIBS:BOOL=ON \
  -DCMAKE_CXX_FLAGS="-Wall -pedantic" \
  -DCMAKE_C_FLAGS="-Wall -pedantic" \
  -DNetCDF_DIR:PATH=${ACCESS} \
  -DHDF5_ROOT:PATH=/usr/ \
  -DSEACASProj_ENABLE_ALL_PACKAGES:BOOL=ON \
  -DSEACASProj_ENABLE_SECONDARY_TESTED_CODE:BOOL=ON \
  -DSEACASProj_ENABLE_TESTS:BOOL=ON \
  -DSEACASProj_USE_GNUINSTALLDIRS:BOOL=ON \
  -DTPL_ENABLE_CGNS:BOOL=ON \
  -DTPL_ENABLE_Matio:BOOL=ON \
  -DTPL_ENABLE_METIS:BOOL=OFF \
  -DTPL_ENABLE_ParMETIS:BOOL=OFF \
  -DTPL_ENABLE_Netcdf:BOOL=ON \
  -DTPL_ENABLE_MPI:BOOL=OFF \
  -DTPL_ENABLE_Pamgen:BOOL=OFF \
  -DTPL_ENABLE_X11:BOOL=ON \
  -DTPL_ENABLE_Zlib:BOOL=ON \
  ../
