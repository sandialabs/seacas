A capability in testing is to build a "parallel" version of
SEACAS. This includes a "parallel-aware" version of the exodus library
and a parallel version of the Ioss library. The modifications to the
build process described in the README file are shown below:

## Additional Libraries

You will need the following libraries:

* [ParMetis](#parmetis)
* [Parallel-NetCDF](#parallel-netcdf)

In addition, the HDF5 and NetCDF libraries need to be compiled with parallel capability enabled.

* [HDF5](#hdf5)
* [NetCDF](#netcdf)

For all of the libraries, there should exist a
`TPL/{lib_name}/runconfigure.sh` file.  You can look at this file and make any changes needed; then cd to the library source directory, do `../runconfigure.sh` and it will hopefully correctly configure the library.

### ParMetis (Includes metis)

  * Download <http://glaros.dtc.umn.edu/gkhome/fetch/sw/parmetis/parmetis-4.0.3.tar.gz>

  * `cd TPL/metis`

  * `tar zxvf parmetis-4.0.3.tar.gz`

  * edit `parmetis-4.0.3/metis/include/metis.h` and change `IDXTYPEWIDTH` and `REALTYPEWIDTH` to 64

  * cd to the parmetis directory and enter the command:
    ```bash
    MPI=ON sh ../runconfigure.sh
	make && make install
    ```
  
### Parallel-NetCDF
  * Download <http://cucis.ece.northwestern.edu/projects/PnetCDF/Release/parallel-netcdf-1.10.0.tar.gz>

  * `cd TPL/pnetcdf`

  * `tar zxvf parallel-netcdf-1.10.0.tar.gz`

  * NOTE: If using version 1.9.0 or later, the changes below are not required.

  * Make the same modifications to the defines in `src/lib/pnetcdf.h` that are modified for NetCDF:
    ```c
    #define NC_MAX_DIMS     65536    /* max dimensions per file */
    #define NC_MAX_VARS     524288   /* max variables per file */
    ```

  * `cd` to the parallel-netcdf source directory and enter the command:
    ```bash
	sh ../runconfigure.sh
	make && make install
    ```

### HDF5
  * `cd` to the hdf5 source directory and enter the command:
   
    ```bash
    MPI=ON sh ../runconfigure.sh
	make && make install
    ```

### NetCDF
  * Make the modifications to include/netcdf.h as documented in README.md

  * `cd netcdf-4.6.1` and enter the command:
 
    ```bash
	mkdir build
	cd build
	MPI=ON sh ../../runcmake.sh
	make && make install
    ```

## Configure, Build, and Install SEACAS
Build as described in README.md.  If you are using the `cmake-config`
script, change `MPI` to `ON` and then continue.

If using your own cmake script or directly calling cmake, specify the
correct mpi-aware compilers 

```bash
-D CMAKE_CXX_COMPILER:FILEPATH=${CXX} 
-D CMAKE_C_COMPILER:FILEPATH=${CC} 
-D CMAKE_Fortran_COMPILER:FILEPATH=${FC} 
-D TPL_ENABLE_MPI:BOOL=ON
```
