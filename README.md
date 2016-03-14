# SEACAS  [[Documentation](http://gsjaardema.github.io/seacas/)]
[![Build Status](https://travis-ci.org/gsjaardema/seacas.svg?branch=master)](https://travis-ci.org/gsjaardema/seacas)
[![Analysis Status](https://scan.coverity.com/projects/2205/badge.svg?flat=1)](https://scan.coverity.com/projects/gsjaardema-seacas)
[![Documentation](https://codedocs.xyz/gsjaardema/seacas.svg)](https://codedocs.xyz/gsjaardema/seacas/)

* [Get the sources](#get-the-sources)
* [Build instructions](#build-instructions)
* [Configure, Build, and Install SEACAS](#configure-build-and-install-seacas)
* [Testing](#testing)
* [Exodus](#exodus)
* [License](#license)
* [Ubuntu](#ubuntu)
* [Contact information](#contact-information)
* For information on building with MPI, see [README-PARALLEL.md](README-PARALLEL.md)
* NOTE: The old imake-based build has been removed.

## Get the sources
```
git clone https://github.com/gsjaardema/seacas.git
```
This will create a directory that will be referred to as _seacas_ in
the instructions that follow. You can rename this directory to any
other name you desire. Set an environment variable pointing to this
location by doing:
```
cd seacas && export ACCESS=`pwd`
```

## Build instructions

### Download and build dependencies

There are a few externally developed third-party libraries (TPL) that are required to
build SEACAS. 

 * [Zoltan](#zoltan) -- required, supplied
 * [HDF5](#hdf5) -- optional
 * [NetCDF](#netcdf) -- required with modifications
 * [MatIO](#matio) -- optional
 * [GNU Parallel](#gnu-parallel) -- optional
 * [CGNS](#cgns) -- experimental optional

#### Zoltan
A snapshot of [zoltan_distrib_v3.82.tar.gz](http://www.cs.sandia.gov/Zoltan/Zoltan_download.html) is provided in seacas/packages/zoltan.  This will be built automatically as part of the SEACAS build process.

#### HDF5
If you are using the netcdf-4 capability in the netcdf library or are using the MatIO library for conversion of exodus to/from matlab format, then you will need the hdf5 library. 

The hdf5 library is used for the netcdf4 capability in netcdf which in
turn is used by exodus.  The netcdf4 capability is typically used for
large models (>150 million elements); if you are not planning to
create or read models of this size, you do not have to build hdf5. 

   * Download HDF5 from http://www.hdfgroup.org/HDF5/release/obtain5.html and put it inside `seacas/TPL/hdf5`
   * untar it, creating a directory will will refer to as `hdf5-X.X.X`
   * `cd` to that directory and enter the command:
    ```
    ./configure --prefix=${ACCESS} --enable-shared --enable-production --enable-debug=no --enable-static-exec
    ```
   * `make && make install`

#### NetCDF
The most recent released version is recommended. For use with Exodus, some local modifications to the netcdf.h include file are required.  See [NetCDF-Mapping.md](NetCDF-Mapping.md) for an explanation of why these modifications are required (or highly recommended)

 * Download the latest netcdf-c release from http://www.unidata.ucar.edu/downloads/netcdf/index.jsp and put it inside `seacas/TPL/netcdf`
 * `cd TPL/netcdf`
 * `tar zxvf netcdf-4.3.3.1.tar.gz`
 * Modify the following defines in seacas/TPL/netcdf/netcdf-4.3.3.1/include/netcdf.h.

    ```
    #define NC_MAX_DIMS     65536    /* max dimensions per file */
    #define NC_MAX_VARS     524288   /* max variables per file */
    ```

 * `cd netcdf-4.3.3.1` and enter the command:
    ```
    ./configure --enable-netcdf-4  --enable-shared \
      --disable-fsync --prefix ${ACCESS} \
      --disable-dap --disable-cdmremote
    ```

 * If the configure step complains about not being able to find the
   HDF5 library, you may need to do the following and then redo the
   configure step
    ```
    CFLAGS="-I{HDF5_ROOT}/include"; export CFLAGS
    LDFLAGS="-L{HDF5_ROOT}/lib   "; export LDFLAGS
    ```

 * `make && make install`

#### MatIO
The MatIO library is used in the exo2mat and mat2exo programs which convert an exodus file to and from a MATLAB binary file.  To use this do:

 * Download matio via git:
 * `cd TPL/matio`
 * `git clone https://github.com/tbeu/matio.git`
 * `cd matio` and enter the command:
    ```
    ./autogen.sh
    # The -L is to find the hdf5 library...
    export LDFLAGS="-L${ACCESS}/lib"
    ./configure --with-hdf5=${ACCESS} --enable-mat73 --enable-shared --prefix=${ACCESS}
    ```
    
 * `make && make install`

#### GNU Parallel

GNU Parallel is a shell tool for executing jobs in parallel using one or more computers. A job is typically a single command or a small script that has to be run for each of the lines in the input. The typical input is a list of files, a list of hosts, a list of users, or a list of tables.  In SEACAS, this is only used by epup which runs multiple epu jobs concurrently.  To build:

   * Download the most recent version of the library from ftp://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2.

    ```
    cd TPL/parallel
    tar jxvf /path/to/parallel-latest.tar.bz2
    cd parallel-20150522
    ./configure --prefix ${ACCESS}
    ```
    
#### CGNS
Experimental support for CGNS in the IOSS library is being added.  To use this capability, you will need to download and install the CGNS library:

   * Download CGNS via git:

    ```
    cd TPL/cgns
    git clone https://github.com/CGNS/CGNS.git
    ```
   * Build using CMake.
     * Modify `TPL/cgns/runconfigure.sh` to meet your environment
     * `mkdir build`
     * `cd build`
     * `../../runconfigure.sh`
     * `make && make install`

## Configure, Build, and Install SEACAS
At this time, you should have all external TPL libraries built and
installed into `${ACCESS}/lib` and `${ACCESS}/include`. You are now ready
to configure the SEACAS CMake build.

   * `cd $ACCESS`
   * `mkdir build`
   * `cd build`
   * edit the `${ACCESS}cmake-config` file and adjust compilers and
     other settings as needed. 
   * enter the command `../cmake-config` and cmake should configure everything for the build.
   * `make && make install`
   * If everything works, your applications should be in `${ACCESS}/bin`

## Testing
There are a few unit tests for zoltan, exodus, and aprepro that can be run via `make test` if you configured with `-D SEACASProj_ENABLE_TESTS=ON`.

There is also a system-level test that just verifies that the applications can read and write exodus files correctly.  This test runs off of the installed applications.  To run do:

 * `make install`
 * `cd ../SEACAS-Test`
 * `make clean; make`

This will run through several of the SEACAS applications creating a mesh (exodus file) and then performing various manipulations on the mesh.  If the test runs successfully, there is some hope that everything has built and is running correctly. 

## Exodus
If you only want the exodus library, then follow most of the above instructions with the following exceptions:

  * You can either clone entire source tree as above, or you can
	download a zip file containing only the exodus source (and
	build-related files).  The url for the zip file is
	https://github.com/gsjaardema/seacas/archive/exodus.zip. 
  * You only need the netcdf and optionally hdf5 libraries
  * Use the `cmake-exodus` file instead of `cmake-config`.
  * This will build, by default, a shared exodus library and also install the exodus.py Python interface.

## License

SEACAS is licensed under the Modified BSD License.  See the LICENSE  file for details.

## Ubuntu
There is a [PPA](https://launchpad.net/~nschloe/+archive/ubuntu/seacas-nightly/) available for SEACAS that is updated nightly from SEACAS `master`. Anyone using Ubuntu can now just add the PPA and do
```
sudo add-apt-repository ppa:nschloe/seacas-nightly
sudo apt-get update
```
and then
```
sudo apt-get install seacas-bin
```
to get the SEACAS binaries. You can also install 'libseacas-dev' or 'libseacas0'. This is provided by Nico Schlömer.

## Contact information

 Greg Sjaardema  (gsjaardema@gmail.com, gdsjaar@sandia.gov)
      
