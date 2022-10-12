#!/bin/bash
set -e
pwd

echo "$HOME"
mkdir build
cd build
bash ../cmake-config
make -j4
ctest -j 4 --output-on-failure
