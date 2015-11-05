#! /bin/sh
rm -f config.cache
CC='gcc'; export CC
CPPFLAGS='-DNDEBUG'; export CPPFLAGS

### SEACAS does not use the Fortran, F90, or C++ versions of hdf5 library
FC=''; export FC
F90=''; export F90

SHARED="--enable-shared"
#SHARED=""

#PARALLEL_ON_OFF="--enable-parallel"
PARALLEL_ON_OFF="--disable-parallel"

./configure --prefix=${ACCESS} $(SHARED) ${PARALLEL_ON_OFF} --enable-production --enable-debug=no --enable-static-exec $1
