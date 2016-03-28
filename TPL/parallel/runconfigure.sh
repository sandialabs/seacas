#! /bin/sh

if [ "X$ACCESS" == "X" ] ; then
  echo "ERROR: Please set the ACCESS environment variable before executing this script."
  exit
fi

rm -f config.cache
CC='gcc'; export CC
CFLAGS="-I${ACCESS}/include"; export CFLAGS
CPPFLAGS='-DNDEBUG'; export CPPFLAGS
LDFLAGS="-L${ACCESS}/lib"; export LDFLAGS

./configure --prefix=${ACCESS} $1
