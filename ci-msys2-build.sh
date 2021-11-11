#!/bin/bash
set -e
pwd

echo $HOME
mkdir build
cd build
MPI=NO bash ../cmake-config
make -j4
ctest -j 4 --output-on-failure
