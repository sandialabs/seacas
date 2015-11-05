#! /bin/sh
rm -f config.cache
CC='gcc'; export CC
CFLAGS="-I${ACCESS}/include"; export CFLAGS
CPPFLAGS='-DNDEBUG'; export CPPFLAGS
LDFLAGS="-L${ACCESS}/lib"; export LDFLAGS

./configure --prefix ${ACCESS} $1
