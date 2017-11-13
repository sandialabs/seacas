cd TPL/cgns
git clone https://github.com/cgns/CGNS
cd CGNS
# git checkout develop
mkdir build
cd build
MPI=${MPI} ACCESS="/usr/local" bash ../../runconfigure.sh HDF5_ROOT=/usr/lib/x86_64-linux-gnu/
make && sudo make install
