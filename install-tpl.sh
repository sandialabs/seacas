#! /usr/bin/env bash

SUDO=${SUDO:-}
CGNS=${CGNS:-ON}
MATIO=${MATIO:-ON}
ACCESS=`pwd`
pwd

# =================== INSTALL HDF5 ===============
hdf_version="1.8.19"

cd $ACCESS
cd TPL/hdf5
wget https://support.hdfgroup.org/ftp/HDF5/current18/src/hdf5-${hdf_version}.tar.bz2
tar -jxf hdf5-${hdf_version}.tar.bz2
cd hdf5-${hdf_version}
MPI=${MPI} bash ../runconfigure.sh
make && ${SUDO} make install

# =================== INSTALL PNETCDF (if mpi) ===============
if [ "$MPI" == "ON" ]
then

pnet_version="1.8.1"
#pnet_version="1.9.0.pre1"

cd $ACCESS
cd TPL/pnetcdf
wget http://cucis.ece.northwestern.edu/projects/PnetCDF/Release/parallel-netcdf-${pnet_version}.tar.gz
tar -xzf parallel-netcdf-${pnet_version}.tar.gz
cd parallel-netcdf-${pnet_version}
bash ../runconfigure.sh
make && ${SUDO} make install
fi

# =================== INSTALL NETCDF ===============
cd $ACCESS
cd TPL/netcdf
git clone https://github.com/Unidata/netcdf-c netcdf-c
cd netcdf-c
mkdir build
cd build
MPI=${MPI} bash ../../runcmake.sh
make && ${SUDO} make install

# =================== INSTALL CGNS ===============
if [ "$CGNS" == "ON" ]
then

cd $ACCESS
cd TPL/cgns
git clone https://github.com/cgns/CGNS
cd CGNS
mkdir build
cd build
MPI=${MPI} bash ../../runconfigure.sh
make && ${SUDO} make install

fi

# =================== INSTALL MATIO  ===============
if [ "$MATIO" == "ON" ]
then

cd $ACCESS
cd TPL/matio
git clone https://github.com/tbeu/matio.git
cd matio
./autogen.sh
bash ../runconfigure.sh
make && ${SUDO} make install

fi

# ==================================
cd $ACCESS
ls -l include
ls -l lib
