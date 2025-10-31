#!/bin/bash
set -e
pwd

echo $HOME
mkdir build
cd build

X11=NO NETCDF_PATH=/mingw64 HDF5_PATH=/mingw64 CGNS_PATH=/mingw64 MPI=NO bash ../cmake-config
make -k -j4
make install
#ctest -j 4 --output-on-failure
