#! /usr/bin/env bash

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}
. ${ACCESS}/TPL/compiler.sh

if [ "X$BOOST_ROOT" == "X" ] ; then
  echo "Faodel requires these Boost packages: atomic log log_setup serialization"
  echo "Please set BOOST_ROOT."
fi

rm -f CMakeCache.txt

echo "+++++++++++++++++++++ $(pwd)"
# export BOOST_ROOT=/opt/local
# export GTEST_ROOT=/opt/local
cmake ..                                 \
  -DCMAKE_CXX_COMPILER=$CXX              \
  -DCMAKE_C_COMPILER=$CC                 \
  -DCMAKE_INSTALL_PREFIX=${INSTALL_PATH} \
  -DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON \
  -DCMAKE_BUILD_TYPE=Release             \
  -DBUILD_SHARED_LIBS:BOOL=ON            \
  -DGTEST_ROOT=${GTEST_ROOT}             \
  -DBOOST_ROOT=${BOOST_ROOT}             \
  -DFaodel_NETWORK_LIBRARY=nnti          \
  -DFaodel_ENABLE_MPI_SUPPORT=${MPI}     \
  -DFaodel_ENABLE_CEREAL:BOOL=ON         \
  -DBUILD_TESTS=ON                       \
  -DBUILD_DOCS=OFF

echo ""
echo "         MPI: ${MPI}"
echo "  C COMPILER: ${CC}"
echo "CXX COMPILER: ${CXX}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
