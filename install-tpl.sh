#! /usr/bin/env bash

SUDO=${SUDO:-}
CGNS=${CGNS:-ON}
MATIO=${MATIO:-ON}
JOBS=${JOBS:-2}
ACCESS=`pwd`
pwd

NEEDS_ZLIB=${NEEDS_ZLIB:-NO}

DOWNLOAD=${DOWNLOAD:-YES}
INSTALL=${INSTALL:-YES}

if [ "$MPI" == "ON" ]
then
    CC=/usr/bin/mpicc; export CC
fi

if [ "$NEEDS_ZLIB" == "YES" ]
then
    cd $ACCESS
    cd TPL
    if [ "$DOWNLOAD" == "YES" ]
    then
	rm -rf zlib-1.2.11.tar.gz
	wget --no-check-certificate https://zlib.net/zlib-1.2.11.tar.gz
    fi
    
    if [ "$INSTALL" == "YES" ]
    then
	tar -xzf zlib-1.2.11.tar.gz
	cd zlib-1.2.11
	./configure --prefix=${ACCESS}
	make -j${JOBS} && ${SUDO} make install
    fi
fi

# =================== INSTALL HDF5 ===============
hdf_version="1.8.20"
#hdf_version="1.10.1"

cd $ACCESS
cd TPL/hdf5
if [ "$DOWNLOAD" == "YES" ]
then
    rm -f hdf5-${hdf_version}.tar.bz2
    wget --no-check-certificate https://support.hdfgroup.org/ftp/HDF5/current18/src/hdf5-${hdf_version}.tar.bz2
fi

if [ "$INSTALL" == "YES" ]
then
    tar -jxf hdf5-${hdf_version}.tar.bz2
    cd hdf5-${hdf_version}
    NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash ../runconfigure.sh
    make -j${JOBS} && ${SUDO} make install
fi

# =================== INSTALL PNETCDF (if mpi) ===============
if [ "$MPI" == "ON" ]
then
    
    pnet_version="1.8.1"
    #pnet_version="1.9.0.pre1"
    
    cd $ACCESS
    cd TPL/pnetcdf
    if [ "$DOWNLOAD" == "YES" ]
    then
	rm -f parallel-netcdf-${pnet_version}.tar.gz
	wget http://cucis.ece.northwestern.edu/projects/PnetCDF/Release/parallel-netcdf-${pnet_version}.tar.gz
    fi
    
    if [ "$INSTALL" == "YES" ]
    then
	tar -xzf parallel-netcdf-${pnet_version}.tar.gz
	cd parallel-netcdf-${pnet_version}
	bash ../runconfigure.sh
	make -j${JOBS} && ${SUDO} make install
    fi
fi

# =================== INSTALL NETCDF ===============
cd $ACCESS
cd TPL/netcdf
if [ "$DOWNLOAD" == "YES" ]
then
    rm -rf netcdf-c
    git clone https://github.com/Unidata/netcdf-c netcdf-c
fi

if [ "$INSTALL" == "YES" ]
then
    cd netcdf-c
    mkdir build
    cd build
    NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash ../../runcmake.sh
    make -j${JOBS} && ${SUDO} make install
fi

# =================== INSTALL CGNS ===============
if [ "$CGNS" == "ON" ]
then
    
    cd $ACCESS
    cd TPL/cgns
    if [ "$DOWNLOAD" == "YES" ]
    then
	rm -rf CGNS
	git clone https://github.com/cgns/CGNS
    fi
    
    if [ "$INSTALL" == "YES" ]
    then
	cd CGNS
	mkdir build
	cd build
	MPI=${MPI} bash ../../runconfigure.sh
	make -j${JOBS} && ${SUDO} make install
    fi
fi

# =================== INSTALL MATIO  ===============
if [ "$MATIO" == "ON" ]
then
    
    cd $ACCESS
    cd TPL/matio
    if [ "$DOWNLOAD" == "YES" ]
    then
	rm -rf matio
	git clone https://github.com/tbeu/matio.git
    fi
    
    if [ "$INSTALL" == "YES" ]
    then
	cd matio
	./autogen.sh
	bash ../runconfigure.sh
	make -j${JOBS} && ${SUDO} make install
    fi
fi

# ==================================
cd $ACCESS
ls -l include
ls -l lib
