#! /bin/sh

if [ "X$ACCESS" == "X" ] ; then
  echo "ERROR: Please set the ACCESS environment variable before executing this script."
  exit
fi

rm -f config.cache
CC='gcc'; export CC

cmake .. -DCMAKE_C_COMPILER:FILEPATH=${CC} \
         -DBUILD_SHARED_LIBS=ON \
         -DCMAKE_INSTALL_PREFIX=${ACCESS} \
         -DENABLE_NETCDF_4=ON \
	 -DENABLE_MMAP:BOOL=ON \
         -DENABLE_DAP:BOOL=OFF \
	 -DENABLE_V2_API:BOOL=OFF


