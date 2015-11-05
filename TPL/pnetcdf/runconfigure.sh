#! /bin/sh
rm -f config.cache
CC=mpicc
MPICC='$(CC)'; export MPICC
CFLAGS='-I${ACCESS}/include'; export CFLAGS
CPPFLAGS='-DNDEBUG'; export CPPFLAGS
AR_FLAGS='cru'; export AR_FLAGS

LDFLAGS='-L${ACCESS}/lib'; export LDFLAGS

./configure --disable-fortran --prefix ${ACCESS}
