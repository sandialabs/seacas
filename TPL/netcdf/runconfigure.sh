#! /bin/sh

if [ "X$ACCESS" == "X" ] ; then
  echo "ERROR: Please set the ACCESS environment variable before executing this script."
  exit
fi

PARALLEL="${MPI:-OFF}"

rm -f config.cache
export CFLAGS="-I${ACCESS}/include"
export CPPFLAGS="-DNDEBUG"

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
