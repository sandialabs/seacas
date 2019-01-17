#! /bin/sh

### The following assumes you are building in a subdirectory of ACCESS Root
if [ "X$ACCESS" == "X" ] ; then
  ACCESS=$(cd ../../..; pwd)
  echo "ACCESS set to ${ACCESS}"
fi
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

if [ "$BB" == "ON" ]
then
   USE_BB="--enable-burst-buffering"
fi
   
if [ "$CRAY" == "ON" ]
then
    USE_SHARED="--disable-shared"
else
    SHARED="${SHARED:-ON}"
    if [[ "$SHARED" == "ON" || "$SHARED" == "YES" ]]
    then
	USE_SHARED="--enable-shared"
    else
	USE_SHARED="--disable-shared"
    fi
fi

rm -f config.cache
if [ "$CRAY" == "ON" ]
then
  CC=cc
else
  CC=mpicc
fi
MPICC=$(CC); export MPICC
CFLAGS='-fPIC -I${INSTALL_PATH}/include'; export CFLAGS
CPPFLAGS='-DNDEBUG'; export CPPFLAGS
AR_FLAGS='cru'; export AR_FLAGS

LDFLAGS='-L${INSTALL_PATH}/lib'; export LDFLAGS
./configure --disable-fortran ${USE_SHARED} ${USE_BB} --disable-cxx --prefix=${INSTALL_PATH}

echo ""
echo "    COMPILER: ${CC}"
echo "      ACCESS: ${ACCESS}"
echo "INSTALL_PATH: ${INSTALL_PATH}"
echo ""
