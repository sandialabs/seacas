#! /usr/bin/env bash

ACCESS=`pwd`
pwd

# =================== INSTALL HDF5 ===============
cd TPL/hdf5
wget https://support.hdfgroup.org/ftp/HDF5/current18/src/hdf5-1.8.19.tar.bz2
tar -jxf hdf5-1.8.19.tar.bz2
cd hdf5-1.8.19
MPI=${MPI} bash ../runconfigure.sh
make && sudo make install

cd $ACCESS

# =================== INSTALL PNETCDF (if mpi) ===============
if [ "$MPI" == "ON" ]
then

pnet_version="1.8.1"
#pnet_version="1.9.0.pre1"

cd TPL/pnetcdf
wget http://cucis.ece.northwestern.edu/projects/PnetCDF/Release/parallel-netcdf-${pnet_version}.tar.gz
tar -xzf parallel-netcdf-${pnet_version}.tar.gz
cd parallel-netcdf-${pnet_version}
bash ../runconfigure.sh
make && sudo make install

cd $ACCESS
fi

# =================== INSTALL NETCDF ===============
cd TPL/netcdf
git clone https://github.com/Unidata/netcdf-c netcdf-c
cd netcdf-c
MPI=${MPI} bash ../runconfigure.sh
make && sudo make install

cd $ACCESS

# =================== INSTALL CGNS ===============
cd TPL/cgns
git clone https://github.com/cgns/CGNS
cd CGNS
mkdir build
cd build
MPI=${MPI} bash ../../runconfigure.sh
make && sudo make install

# =================== INSTALL CGNS ===============
cd TPL/matio
git clone https://github.com/tbeu/matio.git
cd matio
./autogen.sh
sh ../runconfigure.sh
make && sudo make install
