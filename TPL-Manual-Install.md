There are a few externally developed third-party libraries (TPL) that
are required to build SEACAS. You can build the libraries manually as
detailed below, or you can use the `install-tpl.sh` script as
described in the previous section.

*  [Zoltan](#zoltan) -- required, supplied
*  [HDF5](#hdf5) -- optional
*  [NetCDF](#netcdf) -- required with possible modifications
*  [Kokkos](#kokkos) -- optional
*  [MatIO](#matio) -- optional, required for exo2mat and mat2exo
*  [GNU Parallel](#gnu-parallel) -- optional
*  [CGNS](#cgns) -- experimental optional
*  [Faodel](#faodel) -- optional
*  [AWS SDK for C++](#aws-sdk-cpp) -- optional
*  [Cereal](#cereal) -- optional

## Zoltan
A snapshot of [zoltan_distrib\_v3.83.tar.gz](http://www.cs.sandia.gov/Zoltan/Zoltan_download.html) is provided in seacas/packages/zoltan.  This will be built automatically as part of the SEACAS build process.

## HDF5
If you are using the netcdf-4 capability in the netcdf library or are using the MatIO library for conversion of exodus to/from matlab format, or using CGNS, then you will need the hdf5 library.

### WARNING
There are some issues with using HDF5-1.10.0 through HDF5-1.10.2 since
it will possibly create files which are unreadable by applications
using an earlier version of the library.  As of HDF5-1.10.3 and later,
the HDF5 team added an option that makes it possible for the library
to create files readable by those applications. This flag is currently
being used by NetCDF and CGNS.

The hdf5 library is used for the netcdf4 capability in netcdf which in
turn is used by exodus.  The netcdf4 capability is typically used for
large models (>150 million elements); if you are not planning to
create or read models of this size, you do not have to build hdf5.

*  Download HDF5 from either:

  *  <https://www.hdfgroup.org/HDF5/release/obtain5.html> for HDF5-1.10.X or

  *  <https://support.hdfgroup.org/HDF5/release/obtain518.html> for HDF5-1.8.X

*  Download to `seacas/TPL/hdf5` and untar it

*  `cd` to that directory and enter the command:
   ```bash
   sh ../runconfigure.sh
   ```

*  `make && make install`

## NetCDF
The most recent released version is recommended. For use with Exodus, some local modifications to the netcdf.h include file are required if using versions prior to 4.5.1.  See [NetCDF-Mapping.md](NetCDF-Mapping.md) for an explanation of why these modifications are required (or highly recommended)

*  Download the latest netcdf-c release from <https://www.unidata.ucar.edu/downloads/netcdf/index.jsp> and put it inside `seacas/TPL/netcdf`

*  `cd TPL/netcdf`

*  `tar zxvf netcdf-4.6.1.tar.gz`

*  If the version is *prior* to 4.5.1, then you need to modify the
   following defines in
   seacas/TPL/netcdf/netcdf-4.6.1/include/netcdf.h.  Versions *4.5.1 or
   later* do not check these limits and can be run unmodified.
   ```c
   #define NC_MAX_DIMS     65536    /* max dimensions per file */
   #define NC_MAX_VARS     524288   /* max variables per file */
   ```

*  If you did *not* build HDF5, then you will need to edit the runcmake.sh script and remove all lines mentioning HDF5 and also set `ENABLE_NETCDF
_4` to `OFF`

*  `cd netcdf-4.6.1` and enter the command:
   ```bash
   mkdir build
   cd build
   sh ../../runcmake.sh
   ```

*  `make && make install`

## Kokkos
The most recent released version is recommended. 

*  Download the latest Kokkos release from <https://github.com/kokkos/kokkos/releases> and put it inside `seacas/TPL/kokkos`

*  `cd TPL/kokkos`

*  `tar zxvf 2.8.0.tar.gz`

*  `cd kokkos-2.8.0` and enter the command:
   ```bash
   mkdir build
   cd build
   sh ../../runcmake.sh
   ```

*  The default `runcmake.sh` will only build the `serial` device. Modify build options to tailor for your system as documented in the Kokkos documentation.

*  `make && make install`

## MatIO
The MatIO library is used in the exo2mat and mat2exo programs which convert an exodus file to and from a MATLAB binary file.  To use this do:

*  Download matio via git:

*  `cd TPL/matio`

*  `git clone https://github.com/tbeu/matio.git`

*  `cd matio` and enter the command:
   ```bash
   ./autogen.sh
   sh ../runconfigure.sh
   ```

*  `make && make install`

## GNU Parallel

GNU Parallel is a shell tool for executing jobs in parallel using one or more computers. A job is typically a single command or a small script that has to be run for each of the lines in the input. The typical input is a list of files, a list of hosts, a list of users, or a list of tables.  In SEACAS, this is only used by epup which runs multiple epu jobs concurrently.  To build:

*  Download the most recent version of the library from <ftp://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2>.
   ```bash
   cd TPL/parallel
   tar jxvf /path/to/parallel-latest.tar.bz2
   cd parallel-20150522
   sh ../runconfigure.sh
   ```

*  `make && make install`

## CGNS
The IOSS library supports using CGNS for structured and unstructred meshes.  To use this capability, you will need to download and install the CGNS library:

*  Download CGNS via git:
   ```bash
   cd TPL/cgns
   git clone https://github.com/CGNS/CGNS.git
   ```

*  Build using CMake.

  *  Modify `TPL/cgns/runcmake.sh` to meet your environment

  *  `cd CGNS`

  *  `mkdir build`

  *  `cd build`

  *  `../../runcmake.sh`

  *  `make && make install`

## Faodel
Faodel is a collection of data management tools that Sandia is developing to improve how datasets migrate between memory and storage resources in a distributed system. For SEACAS Faodel support means adding a new backend to IOSS. This enables additional data storage capabilities and the chance to communicate data between execution spaces.

Faodel is available at [Faodel](https://github.com/faodel/faodel). And is built here as a SEACAS TPL.

## AWS SDK for C++
The AWS SDK for C++ is the official C++ interface to the entire Amazon
Web Services ecosystem.  This includes packages for transferring data
to and from S3 (Simple Storage Service) object stores.  For SEACAS,
AWS support means adding a new backend to IOSS that stores mesh data
in S3.

The AWS SDK for C++ is available at
[aws-sdk-cpp](https://github.com/aws/aws-sdk-cpp). And is built here
as a SEACAS TPL.

The AWS SDK for C++ is in multiple github repositories and composed
using submodules.  Below is an example of building the tested version
1.11.77.  Complete build instructions for the AWS SDK for C++ can be
found here:
https://docs.aws.amazon.com/sdk-for-cpp/v1/developer-guide/setup-linux.html


*  Cloning from github.

  * `cd TPL/aws-sdk-cpp`
  
  * `git clone --recurse-submodules https://github.com/aws/aws-sdk-cpp`
  
  * `cd aws-sdk-cpp`
  
  * `git branch branch/tag-1.11.77 1.11.77`
  
  * `git checkout branch/tag-1.11.77`
  
  * `git submodule update --recursive`

*  Build using CMake.

  *  Modify `TPL/aws-sdk-cpp/runcmake.sh` to meet your environment

  *  `mkdir build`

  *  `cd build`

  *  `../../runcmake.sh`

  *  `make && make install`


## Cereal
cereal is a serialization library that transforms C++ objects into
transportable representations that can be transferred between process
spaces and storage.  For SEACAS, cereal support is required to store
complex IOSS objects in Faodel and S3.  The Faodel and S3 database
components require that cereal be installed.

cereal is available at
[cereal](https://github.com/USCiLab/cereal). And is built here as a
SEACAS TPL.

cereal is a header-only library so installation is as simple as
copying the files to your install directory.

  * `mkdir TPL/cereal`
  
  * `cd TPL/cereal`
  
  * `curl -O -L --insecure https://github.com/USCiLab/cereal/archive/v1.3.2.tar.gz`
  
  * `tar xzf v1.3.2.tar.gz`
  
  * `cp -R cereal-1.3.2/include/cereal $INSTALL_PATH/include/`
