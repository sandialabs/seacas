#! /bin/sh

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi

MPI="${MPI:-OFF}"
PARALLEL="${MPI:-OFF}"

rm -f config.cache
export CFLAGS="-I${ACCESS}/include"
export CPPFLAGS="-DNDEBUG ${CFLAGS}"

# Find the hdf5 library
export LDFLAGS="-L${ACCESS}/lib"

if [ "$PARALLEL" == "OFF" ] ; then
  export CC='gcc'
  PNETCDF=""
  PARALLEL_TESTS=""
else
  export CC='mpicc'
  PNETCDF="--enable-pnetcdf"
  PARALLEL_TESTS="--enable-parallel-tests"
fi

SHARED="--enable-shared"

./configure --enable-netcdf-4 ${PNETCDF} ${SHARED} ${PARALLEL_TESTS} --disable-v2 --disable-fsync --prefix=${ACCESS} --disable-dap $1

echo ""
echo "     MPI: ${MPI}"
echo "COMPILER: ${CC}"
echo "  ACCESS: ${ACCESS}"
echo ""
