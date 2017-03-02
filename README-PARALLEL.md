A capability in testing is to build a "parallel" version of
SEACAS. This includes a "parallel-aware" version of the exodus library
and a parallel version of the Ioss library. The modifications to the
build process described in the README file are shown below:

## Additional Libraries

You will need the following libraries:

* [Metis](#metis)
* [ParMetis](#parmetis)
* [Parallel-NetCDF](#parallel-netcdf)

In addition, the HDF5 and NetCDF libraries need to be compiled with parallel capability enabled.

* [HDF5](#hdf5)
* [NetCDF](#netcdf)

For all of the libraries, there should exist a
`TPL/{lib_name}/runconfigure.sh` file.  You can look at this file and make any changes needed; then cd to the library source directory, do `../runconfigure.sh` and it will hopefully correctly configure the library.

#### Metis

  * Download http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/metis-5.1.0.tar.gz
  * `cd TPL/metis`
  * `tar zxvf metis-5.1.0.tar.gz`
  *  edit `metis-5.1.0/include/metis.h` and change `IDXTYPEWIDTH`
and `REALTYPEWIDTH` to 64

#### ParMetis

  * Download http://glaros.dtc.umn.edu/gkhome/fetch/sw/parmetis/parmetis-4.0.3.tar.gz
  * `cd TPL/metis`
  * `tar zxvf parmetis-4.0.3.tar.gz`
  * edit `parmetis-4.0.3/metis/include/metis.h` and change `IDXTYPEWIDTH`
and `REALTYPEWIDTH` to 64
  
#### Parallel-NetCDF
  * Download http://cucis.ece.northwestern.edu/projects/PnetCDF/Release/parallel-netcdf-1.7.0.tar.gz
  * `cd TPL/pnetcdf`
  * `tar zxvf parallel-netcdf-1.7.0.tar.gz`
  * Make the same modifications to the defines in `src/lib/pnetcdf.h` that are modified for NetCDF:
  
    ```
    #define NC_MAX_DIMS     65536    /* max dimensions per file */
    #define NC_MAX_VARS     524288   /* max variables per file */
    ```

#### HDF5
   * `cd` to the hdf5 source directory and enter the command:
   
    ```
    export CC=mpicc
    ./configure --prefix=${ACCESS} --enable-shared --enable-production --enable-debug=no --enable-static-exec --enable-parallel
    ```

#### NetCDF
 * `cd netcdf-4.4.1.1` and enter the command:
 
    ```
    export CC=mpicc
    ./configure --enable-netcdf-4  --enable-shared \
      --disable-fsync --prefix ${ACCESS} \
      --disable-dap --enable-parallel --enable-pnetcdf
    ```

 * If the configure step complains about not being able to find the
   HDF5 library, you may need to do the following and then redo the
   configure step
    ```
    CFLAGS='-I{HDF5_ROOT}/include'; export CFLAGS
    LDFLAGS='-L{HDF5_ROOT}/lib   '; export LDFLAGS
    ```

	Where `HDF5_ROOT` points to the root of where the HDF5 library and include files
    are installed (HDF5_ROOT = ACCESS if you followed the example above for building HDF5).

## Configure, Build, and Install SEACAS
Build as described in README.md.  If you are using the `cmake-config`
script, change `MPI` to `ON` and then continue.

If using your own cmake script or directly calling cmake, specify the
correct mpi-aware compilers 

```
-D CMAKE_CXX_COMPILER:FILEPATH=${CXX} 
-D CMAKE_C_COMPILER:FILEPATH=${CC} 
-D CMAKE_Fortran_COMPILER:FILEPATH=${FC} 
-D TPL_ENABLE_MPI:BOOL=ON
```

