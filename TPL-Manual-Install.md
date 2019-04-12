There are a few externally developed third-party libraries (TPL) that
are required to build SEACAS. You can build the libraries manually as
detailed below, or you can use the `install-tpl.sh` script as
described in the previous section.

 * [Zoltan](#zoltan) -- required, supplied
 * [HDF5](#hdf5) -- optional
 * [NetCDF](#netcdf) -- required with possible modifications
 * [MatIO](#matio) -- optional, required for exo2mat and mat2exo
 * [GNU Parallel](#gnu-parallel) -- optional
 * [CGNS](#cgns) -- experimental optional
 * [DataWarehouse](#data_warehouse) -- optional

## Zoltan
A snapshot of [zoltan_distrib\_v3.83.tar.gz](http://www.cs.sandia.gov/Zoltan/Zoltan_download.html) is provided in seacas/packages/zoltan.  This will be built automatically as part of the SEACAS build process.

## HDF5
If you are using the netcdf-4 capability in the netcdf library or are using the MatIO library for conversion of exodus to/from matlab format, then you will need the hdf5 library.

### WARNING
There are some issues with using HDF5-1.10.0 through HDF5-1.10.2 since
it will possibly create files which are unreadable by applications
using an earlier version of the library.  As of HDF5-1.10.3 and later,
the HDF5 team added an option that makes it possible for the library
to create files readable by those applications. This flag is currently
being used by NetCDF, but not CGNS.  Therefore, you should only use
hdf5-1.10.3 or later if you are only using NetCDF, or if you do not
need compatability with applications using an HDF5-1.8.X version.

The hdf5 library is used for the netcdf4 capability in netcdf which in
turn is used by exodus.  The netcdf4 capability is typically used for
large models (>150 million elements); if you are not planning to
create or read models of this size, you do not have to build hdf5.

   * Download HDF5 from either:
     * <https://www.hdfgroup.org/HDF5/release/obtain5.html> for HDF5-1.10.X or
     * <https://support.hdfgroup.org/HDF5/release/obtain518.html> for HDF5-1.8.X

   * Download to `seacas/TPL/hdf5` and untar it

   * `cd` to that directory and enter the command:
     ```bash
     sh ../runconfigure.sh
     ```

   * `make && make install`

## NetCDF
The most recent released version is recommended. For use with Exodus, some local modifications to the netcdf.h include file are required if using verions prior to 4.5.1.  See [NetCDF-Mapping.md](NetCDF-Mapping.md) for an explanation of why these modifications are required (or highly recommended)

 * Download the latest netcdf-c release from <https://www.unidata.ucar.edu/downloads/netcdf/index.jsp> and put it inside `seacas/TPL/netcdf`

 * `cd TPL/netcdf`

 * `tar zxvf netcdf-4.6.1.tar.gz`

 * If the version is *prior* to 4.5.1, then you need to modify the
   following defines in
   seacas/TPL/netcdf/netcdf-4.6.1/include/netcdf.h.  Versions *4.5.1 or
   later* do not check these limits and can be run unmodified.
   ```c
   #define NC_MAX_DIMS     65536    /* max dimensions per file */
   #define NC_MAX_VARS     524288   /* max variables per file */
   ```

 * If you did *not* build HDF5, then you will need to edit the runcmake.sh script and remove all lines mentioning HDF5 and also set `ENABLE_NETCDF
_4` to `OFF`

 * `cd netcdf-4.6.1` and enter the command:
   ```bash
   mkdir build
   cd build
   sh ../../runcmake.sh
   ```

 * `make && make install`

## MatIO
The MatIO library is used in the exo2mat and mat2exo programs which convert an exodus file to and from a MATLAB binary file.  To use this do:

 * Download matio via git:

 * `cd TPL/matio`

 * `git clone https://github.com/tbeu/matio.git`

 * `cd matio` and enter the command:
   ```bash
   ./autogen.sh
   sh ../runconfigure.sh
   ```

 * `make && make install`

## GNU Parallel

GNU Parallel is a shell tool for executing jobs in parallel using one or more computers. A job is typically a single command or a small script that has to be run for each of the lines in the input. The typical input is a list of files, a list of hosts, a list of users, or a list of tables.  In SEACAS, this is only used by epup which runs multiple epu jobs concurrently.  To build:

 * Download the most recent version of the library from <ftp://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2>.
   ```bash
   cd TPL/parallel
   tar jxvf /path/to/parallel-latest.tar.bz2
   cd parallel-20150522
   sh ../runconfigure.sh
   ```

 *  `make && make install`

## CGNS
Support for CGNS in the IOSS library is being added.  To use this capability, you will need to download and install the CGNS library:

  * Download CGNS via git:
    ```bash
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

## DataWarehouse
The Data Warehouse is a collection of data management tools that
Sandia is currently developing to improve how datasets migrate between
memory and storage resources in a distributed system. While the
software is currently only available internally to Sandia, there will
be an open source release of the tools during Fall of 2017. The Data
Warehouse extensions to SEACAS are intended to serve as a placeholder
for the upcoming release and are not intended for use by external
developers at this time.

The repository
[data-warehouse-release](https://gitlab.sandia.gov/nessie-dev/data-warehouse-release)
is a superbuild for the Data Warehouse tools and was created to make
the build process as easy as possible. It includes the files
INSTALL.md and INSTALL_TPL.md which contain instructions for building
Data Warehouse and it's TPLs: Boost, googletest, libfabric, and libhio
(optional). These builds are straightforward so a "runconfigure.sh"
script is left to the end-user. Note that it's possible to supply your
own build of these tools. Following the SEACAS pattern for building
TPLs:

```bash
    cd TPL
    git clone git@gitlab.sandia.gov:nessie-dev/data-warehouse-release.git
    follow the instructions in INSTALL_TPL.md and then INSTALL.md
    consider installing data-warehouse-release and it's TPLs to the directory set in  the $ACCESS env. var.
```
To build SEACAS with an installation of data-warehouse-release and
it's TPLs, add the following lines to the list of cmake command
arguments as found in the top-level `cmake-config` file. Where
DataWarehouse_PATH in environment variable that contains the path to
the top-level install directory for the Data Warehouse, and
HAVE_DATA_WAREHOUSE={ON|OFF} is a variable defined in `cmake-config`.
```bash
    -DTPL_ENABLE_DATAWAREHOUSE:BOOL=${HAVE_DATA_WAREHOUSE}           \
    -DDataWarehouse_LIBRARY_DIRS:PATH=${DataWarehouse_PATH}/lib     \
    -DDataWarehouse_INCLUDE_DIRS:PATH=${DataWarehouse_PATH}/include \
```