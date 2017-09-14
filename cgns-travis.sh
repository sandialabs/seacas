cd TPL/cgns
git clone https://github.com/cgns/CGNS
cd CGNS
# git checkout develop
mkdir build
cd build
MPI=${MPI} ACCESS="/usr/local" bash ../../runconfigure.sh
make && sudo make install
