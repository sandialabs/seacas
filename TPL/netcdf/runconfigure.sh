#! /bin/sh
rm -f config.cache
CC='gcc'; export CC
CFLAGS="-I${ACCESS}/include"; export CFLAGS
CPPFLAGS="-DNDEBUG"; export CPPFLAGS

# Find the hdf5 library
LDFLAGS="-L${ACCESS}/lib"; export LDFLAGS

PNETCDF=""
PARALLEL_TESTS=""
# Only if parallel...
#PNETCDF="--enable-pnetcdf"
#PARALLEL_TESTS="--enable-parallel-tests"

#SHARED="--enable-shared"

./configure --enable-netcdf-4 ${PNETCDF} ${SHARED} ${PARALLEL_TESTS} --disable-fsync --prefix ${ACCESS} --disable-dap --disable-cdmremote $1
