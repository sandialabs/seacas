#!/bin/bash
set -e
pwd

echo $HOME
mkdir build
cd build
ls /mingw64
ls /mingw64/bin
ls /mingw64/include
ls /mingw64/lib

NETCDF_PATH=/mingw64 HDF5_PATH=/mingw64 CGNS_PATH=/mingw64 MPI=NO bash ../cmake-config
make -j4
ctest -j 4 --output-on-failure
