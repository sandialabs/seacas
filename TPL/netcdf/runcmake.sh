#! /bin/sh

MPI="${MPI:-OFF}"

echo "MPI set to ${MPI}"

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

if [ "$MPI" == "ON" ]
then
  export CC=mpicc
else
  export CC=gcc
fi

rm -f config.cache

cmake .. -DCMAKE_C_COMPILER:FILEPATH=${CC} \
         -DBUILD_SHARED_LIBS:BOOL=ON \
         -DCMAKE_INSTALL_PREFIX=${ACCESS} \
         -DCMAKE_INSTALL_LIBDIR:PATH=lib \
         -DENABLE_NETCDF_4:BOOL=ON \
         -DENABLE_PNETCDF:BOOL=${MPI} \
         -DENABLE_MMAP:BOOL=ON \
         -DENABLE_DAP:BOOL=OFF \
         -DENABLE_V2_API:BOOL=OFF \
         -DHDF5_C_LIBRARY:PATH=${ACCESS}/lib/libhdf5.${LD_EXT} \
         -DHDF5_HL_LIBRARY:PATH=${ACCESS}/lib/libhdf5_hl.${LD_EXT} \
         -DHDF5_INCLUDE_DIR:PATH=${ACCESS}/include
