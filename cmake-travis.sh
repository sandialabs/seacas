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

cmake \
  -DBUILD_SHARED_LIBS:BOOL=ON \
  -DCMAKE_CXX_FLAGS="-Wall -pedantic" \
  -DCMAKE_C_FLAGS="-Wall -pedantic" \
  -DHDF5_ROOT:PATH=/usr/ \
  -DSEACASProj_ENABLE_ALL_PACKAGES:BOOL=ON \
  -DSEACASProj_ENABLE_SECONDARY_TESTED_CODE:BOOL=ON \
  -DSEACASProj_ENABLE_TESTS:BOOL=ON \
  -DSEACASProj_USE_GNUINSTALLDIRS:BOOL=ON \
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
