A capability in testing is to build a "parallel" version of
SEACAS. This includes a "parallel-aware" version of the exodus library
and a parallel version of the Ioss library. The modifications to the
build process described in the README file are shown below:

1. Additional libraries. You will also need the metis, parmetis, and
parallel-netcdf (pnetcdf) libraries.

  metis: Download http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/metis-5.1.0.tar.gz
  * cd TPL/metis
  * tar zxvf metis-5.1.0.tar.gz
  * edit metis-5.1.0/include/metis.h and change IDXTYPEWIDTH
and REALTYPEWIDTH to 64

  parmetis: Download http://glaros.dtc.umn.edu/gkhome/fetch/sw/parmetis/parmetis-4.0.3.tar.gz
  * cd TPL/metis
  * tar zxvf parmetis-4.0.3.tar.gz
  * edit parmetis-4.0.3/metis/include/metis.h and change IDXTYPEWIDTH
and REALTYPEWIDTH to 64
  
  parallel-netcdf: Download http://cucis.ece.northwestern.edu/projects/PnetCDF/Release/parallel-netcdf-1.6.1.tar.gz
  * cd TPL/pnetcdf
  * tar zxvf parallel-netcdf-1.6.1.tar.gz

Build as shown in README.md with MPI set to ON

NOTE: May be issues with metis/parmetis build... I had to go into
TPL/metis subdirectories and run the runconfigure manually... then at
metis level run "make -k install"


