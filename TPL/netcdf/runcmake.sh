#! /bin/sh

if [ "X$ACCESS" = "X" ] ; then
  echo "ERROR: Please set the ACCESS environment variable before executing this script."
  exit
fi

OS=`uname -s`
if [ "$OS" = "Darwin" ] ; then
LD_EXT="dylib"
else
LD_EXT="so"
fi

rm -f config.cache
CC='gcc'; export CC

cmake .. -DCMAKE_C_COMPILER:FILEPATH=${CC} \
         -DBUILD_SHARED_LIBS=ON \
         -DCMAKE_INSTALL_PREFIX=${ACCESS} \
         -DENABLE_NETCDF_4=ON \
         -DENABLE_PNETCDF=OFF \
         -DENABLE_MMAP:BOOL=ON \
         -DENABLE_DAP:BOOL=OFF \
         -DENABLE_V2_API:BOOL=OFF \
         -DHDF5_C_LIBRARY:PATH=${ACCESS}/lib/libhdf5.${LD_EXT} \
         -DHDF5_HL_LIBRARY:PATH=${ACCESS}/lib/libhdf5_hl.${LD_EXT} \
         -DHDF5_INCLUDE_DIR:PATH=${ACCESS}/include
