#! /bin/sh
rm -f config.cache
CC='gcc'; export CC
CFLAGS="-I${ACCESS}/include"; export CFLAGS
CPPFLAGS='-DNDEBUG'; export CPPFLAGS

# Find hdf5 library...
LDFLAGS="-L${ACCESS}/lib"; export LDFLAGS
SHARED="--enable-shared"

./configure --with-hdf5=${ACCESS} --enable-mat73 ${SHARED} --prefix ${ACCESS} $1
