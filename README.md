# SEACAS  [[Documentation](http://gsjaardema.github.io/seacas/)]
[![Build Status](https://travis-ci.org/gsjaardema/seacas.svg?branch=master)](https://travis-ci.org/gsjaardema/seacas)
[![Analysis Status](https://scan.coverity.com/projects/2205/badge.svg?flat=1)](https://scan.coverity.com/projects/gsjaardema-seacas)

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

### Automatically download and build dependencies (Third-Party Libraries)

There are a few externally developed third-party libraries (TPL) that
are required to build SEACAS. You can build the libraries manually as
detailed in the following section, or you can use the `install-tpl.sh` script which will
download and install the HDF5, NetCDF, CGNS, MatIO, and (if MPI set)
PNetCDF libraries.

* To use the script, simply type `./install-tpl.sh`
* The default behavior can be modified via a few environment variables:
 
| Variable | Values | Default | Description |
|----------|:------:|:-------:|-------------|
| COMPILER | clang, gnu | gnu | What compiler should be used for non-parallel build |
| JOBS     | {count}|  2      | Number of "jobs" used for simultaneous compiles |
| DOWNLOAD | YES, NO | YES |  Should TPLs be downloaded. |
| BUILD    | YES, NO | YES | Should TPLs be built and installed. |
| FORCE    | YES, NO | NO  | Force downloading and building even if lib is already installed. |
| SHARED   | YES, NO | YES | Build shared libraries is YES, archive (.a) if NO |
| MPI      | ON, OFF | OFF | If ON, then build parallel capability |
| NEEDS_ZLIB| YES, NO| NO  | If system does not have zlib installed, download and install it. |
| CGNS     | YES, NO | YES | Should CGNS TPL be built.  |
| MATIO    | YES, NO | YES | Should matio TPL be built. |
| GNU_PARALLEL | YES, NO | YES | Should GNU parallel script be built. |

* NOTE: The `DOWNLOAD` and `BUILD` options can be used to download all TPL source; move to a system with no outside internet access and then build/install the TPLs.
* The arguments can either be set in the environment as: `export COMPILER=gnu`, or passed on the script invocation line: `COMPILER=gnu ./install-tpl.sh`
 
### Download and build dependencies (Third-Party Libraries)

There are a few externally developed third-party libraries (TPL) that
are required to build SEACAS. You can build the libraries manually as
detailed below, or you can use the `install-tpl.sh` script as
described in the previous section.

 * [Zoltan](#zoltan) -- required, supplied
 * [HDF5](#hdf5) -- optional
 * [NetCDF](#netcdf) -- required with modifications
 * [MatIO](#matio) -- optional, required for exo2mat and mat2exo
 * [GNU Parallel](#gnu-parallel) -- optional
 * [CGNS](#cgns) -- experimental optional
 * [DataWarehouse](#data_warehouse) -- optional

#### Zoltan
A snapshot of [zoltan_distrib\_v3.83.tar.gz](http://www.cs.sandia.gov/Zoltan/Zoltan_download.html) is provided in seacas/packages/zoltan.  This will be built automatically as part of the SEACAS build process.

#### HDF5
If you are using the netcdf-4 capability in the netcdf library or are using the MatIO library for conversion of exodus to/from matlab format, then you will need the hdf5 library.  

**
It is recommended at this time that you do *not* use the hdf5-1.10.X release series.  Please use the hdf5-1.8.X releases.
**

The hdf5 library is used for the netcdf4 capability in netcdf which in
turn is used by exodus.  The netcdf4 capability is typically used for
large models (>150 million elements); if you are not planning to
create or read models of this size, you do not have to build hdf5. 

   * Download HDF5 from <http://www.hdfgroup.org/HDF5/release/obtain5.html> and put it inside `seacas/TPL/hdf5`
   * untar it
   * `cd` to that directory and enter the command:
    ```
    sh ../runconfigure.sh
    ```
   * `make && make install`

#### NetCDF
The most recent released version is recommended. For use with Exodus, some local modifications to the netcdf.h include file are required.  See [NetCDF-Mapping.md](NetCDF-Mapping.md) for an explanation of why these modifications are required (or highly recommended)

 * Download the latest netcdf-c release from <http://www.unidata.ucar.edu/downloads/netcdf/index.jsp> and put it inside `seacas/TPL/netcdf`
 * `cd TPL/netcdf`
 * `tar zxvf netcdf-4.6.1.tar.gz`
 * If the version is *prior* to 4.5.1, then you need to modify the
   following defines in
   seacas/TPL/netcdf/netcdf-4.6.1/include/netcdf.h.  Versions *4.5.1 or
   later* do not check these limits and can be run unmodified.

    ```
    #define NC_MAX_DIMS     65536    /* max dimensions per file */
    #define NC_MAX_VARS     524288   /* max variables per file */
    ```

 * If you did *not* build HDF5, then you will need to edit the runcmake.sh script and remove all lines mentioning HDF5 and also set `ENABLE_NETCDF_4` to `OFF`
 * `cd netcdf-4.6.1` and enter the command:

    ```
    mkdir build
    cd build
	sh ../../runcmake.sh
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
    sh ../runconfigure.sh
    ```
    
 * `make && make install`

#### GNU Parallel

GNU Parallel is a shell tool for executing jobs in parallel using one or more computers. A job is typically a single command or a small script that has to be run for each of the lines in the input. The typical input is a list of files, a list of hosts, a list of users, or a list of tables.  In SEACAS, this is only used by epup which runs multiple epu jobs concurrently.  To build:

 * Download the most recent version of the library from <ftp://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2>.

    ```
    cd TPL/parallel
    tar jxvf /path/to/parallel-latest.tar.bz2
    cd parallel-20150522
    sh ../runconfigure.sh
    ```

 *  `make && make install`

#### CGNS
Experimental support for CGNS in the IOSS library is being added.  To use this capability, you will need to download and install the CGNS library:

   * Download CGNS via git:

    ```
    cd TPL/cgns
    git clone https://github.com/CGNS/CGNS.git
    ```
   * Build using CMake.
     * Modify `TPL/cgns/runconfigure.sh` to meet your environment
     * `cd CGNS`
     * `mkdir build`
     * `cd build`
     * `../../runconfigure.sh`
     * `make && make install`
#### DataWarehouse
The Data Warehouse is a collection of data management tools that Sandia is currently developing to improve how datasets migrate between memory and storage resources in a distributed system. While the software is currently only available internally to Sandia, there will be an open source release of the tools during Fall of 2017. The Data Warehouse extensions to SEACAS are intended to serve as a placeholder for the upcoming release and are not intended for use by external developers at this time.

The repository [data-warehouse-release](https://gitlab.sandia.gov/nessie-dev/data-warehouse-release) is a superbuild for the Data Warehouse tools and was created to make the build process as easy as possible. It includes the files INSTALL.md and INSTALL_TPL.md which contain instructions for building Data Warehouse and it's TPLs: Boost, googletest, libfabric, and libhio (optional). These builds are straightforward so a "runconfigure.sh" script is left to the end-user. Note that it's possible to supply your own build of these tools. Following the SEACAS pattern for building TPLs:


    cd TPL
    git clone git@gitlab.sandia.gov:nessie-dev/data-warehouse-release.git
    follow the instructions in INSTALL_TPL.md and then INSTALL.md
    consider installing data-warehouse-release and it's TPLs to the directory set in  the $ACCESS env. var.


To build SEACAS with an installation of data-warehouse-release and it's TPLs, add the following lines to the list of cmake command arguments as found in the top-level _cmake-config_ file. Where DataWarehouse_PATH in environment variable that contains the path to the top-level install directory for the Data Warehouse, and HAVE_DATA_WAREHOUSE={ON|OFF} is a variable defined in _cmake-config_.


    -DTPL_ENABLE_DATAWAREHOUSE:BOOL=${HAVE_DATA_WAREHOUSE}           \
    -DDataWarehouse_LIBRARY_DIRS:PATH=${DataWarehouse_PATH}/lib     \
    -DDataWarehouse_INCLUDE_DIRS:PATH=${DataWarehouse_PATH}/include \


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
	<https://github.com/gsjaardema/seacas/archive/exodus.zip>. 
  * You only need the netcdf and optionally hdf5 libraries
  * Use the `cmake-exodus` file instead of `cmake-config`.
  * This will build, by default, a shared exodus library and also install the exodus.py Python interface.

## License

SEACAS is licensed under the Modified BSD License.  See the LICENSE  file for details.

The following externally-developed software routines are used in some of the SEACAS applications and are under 
a separate license:

| Routine | Where Used  | License |
|---------|-------------|:-------:|
| getline | `packages/seacas/libraries/aprepro_lib/apr_getline_int.c`  | [MIT](https://opensource.org/licenses/MIT) |
| getline | `packages/seacas/libraries/suplib_c/getline.c`             | [BSD](https://opensource.org/licenses/BSD-3-Clause) |
| [GetLongOpt](https://searchcode.com/codesearch/view/64130032/) | `packages/seacas/libraries/suplib_cpp/GetLongOpt.C` | public domain |
| [adler hash](https://en.wikipedia.org/wiki/Adler-32)	| `packages/seacas/libraries/suplib_c/adler.c` | [zlib](https://opensource.org/licenses/zlib) |
| [MurmurHash](https://github.com/aappleby/smhasher) | `packages/seacas/libraries/ioss/src/Ioss_FaceGenerator.C` | public domain |
| [json include file](http://jsoncpp.sourceforge.net) | `packages/seacas/libraries/ioss/src/visualization/` | [MIT](https://opensource.org/licenses/MIT) |
| [terminal_color](https://github.com/matovitch/trmclr) | `packages/seacas/libraries/suplib_cpp` | [zlib](https://opensource.org/licenses/zlib) |

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

 Greg Sjaardema  (<gsjaardema@gmail.com>, <gdsjaar@sandia.gov>)
      
