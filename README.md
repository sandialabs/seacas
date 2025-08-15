# SEACAS  [[Documentation](http://sandialabs.github.io/seacas-docs/)] [[Wiki](https://github.com/sandialabs/seacas/wiki)]
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/838c6d845e9e4ce4a7cd02bd06b4d2ad)](https://www.codacy.com/gh/gsjaardema/seacas/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=gsjaardema/seacas&amp;utm_campaign=Badge_Grade)
[![Analysis Status](https://scan.coverity.com/projects/2205/badge.svg?flat=1)](https://scan.coverity.com/projects/gsjaardema-seacas)
[![Appveyor Build](https://ci.appveyor.com/api/projects/status/pis4gok72yh0wwfs/branch/master?svg=true)](https://ci.appveyor.com/project/gsjaardema/seacas/branch/master)
[![SEACAS Docker](https://img.shields.io/github/actions/workflow/status/sandialabs/seacas/docker-seacas.yml?branch=master&label=SEACAS&logo=docker&logoColor=0db7ed)](https://hub.docker.com/r/mrbuche/seacas)
[![Exodus Docker](https://img.shields.io/github/actions/workflow/status/sandialabs/seacas/docker-exodus.yml?branch=master&label=Exodus&logo=docker&logoColor=0db7ed)](https://hub.docker.com/r/mrbuche/exodus)
[![Github Actions -- CI Serial](https://github.com/sandialabs/seacas/actions/workflows/build_test.yml/badge.svg)](https://github.com/sandialabs/seacas)
[![Github Actions -- CI Variants](https://github.com/sandialabs/seacas/actions/workflows/build_variant.yml/badge.svg)](https://github.com/sandialabs/seacas)
[![Github Actions -- CI Intel](https://github.com/sandialabs/seacas/actions/workflows/intel-build.yml/badge.svg)](https://github.com/sandialabs/seacas)
[![Github Actions -- CI MSYS2](https://github.com/sandialabs/seacas/actions/workflows/msys2.yml/badge.svg)](https://github.com/sandialabs/seacas)
[![Github Actions -- CI Static](https://github.com/sandialabs/seacas/actions/workflows/build_static.yml/badge.svg)](https://github.com/sandialabs/seacas)
[![Github Actions -- CI External Lib](https://github.com/sandialabs/seacas/actions/workflows/build_external_lib.yml/badge.svg)](https://github.com/sandialabs/seacas)
[![Github Actions -- CI Spack](https://github.com/sandialabs/seacas/actions/workflows/spack.yml/badge.svg)](https://github.com/sandialabs/seacas)

*  [Get the sources](#get-the-sources)
*  [Build instructions](#build-instructions)
*  [Configure, Build, and Install SEACAS](#configure-build-and-install-seacas)
*  [Parallel Build](#parallel-build)
*  [Testing](#testing)
*  [Exodus](#exodus)
*  [Trilinos](#trilinos)
*  [SPACK](#spack)
*  [Docker](#docker)
*  [CMake Example Usage](#cmake-example-usage)
*  [Required Software: Mac](#required-software)
*  [License](#license)
*  [Contact information](#contact-information)
*  NOTE: The old imake-based build has been removed.

## Get the sources
```sh
git clone https://github.com/sandialabs/seacas.git
```
This will create a directory that will be referred to as _seacas_ in
the instructions that follow. You can rename this directory to any
other name you desire. Set an environment variable pointing to this
location by doing:

```sh
cd seacas && export ACCESS=`pwd`
```

## Build instructions

### Automatically download and build dependencies (Third-Party Libraries)

There are a few externally developed third-party libraries (TPL) that
are required (or optional) to build SEACAS: HDF5, NetCDF, CGNS, MatIO,
Kokkos, and (if MPI set) PnetCDF libraries. You can build the
libraries using the `install-tpl.sh` script, or you can install them
manually as detailed in
[TPL-Manual-Install.md](TPL-Manual-Install.md).

*  The script requires bash and curl, which you may need to install.
*  To use the script, simply type `bash ./install-tpl.sh`.
*  The default behavior can be modified via a few environment variables:

| Variable        | Values          | Default | Description |
|-----------------|:---------------:|:-------:|-------------|
| INSTALL_PATH    | path to install | pwd | Root of install path; default is current location |
| COMPILER        | clang, gnu, intel, ibm nvidia | gnu | What compiler should be used for non-parallel build. Must have C++-17 capability. |
| MPI             | YES, NO | NO  | If YES, then build parallel capability |
| FORCE           | YES, NO | NO  | Force downloading and building even if lib is already installed. |
| BUILD           | YES, NO | YES | Should TPLs be built and installed. |
| DOWNLOAD        | YES, NO | YES | Should TPLs be downloaded. |
| USE_PROXY       | YES, NO | NO  | Sandia specific -- use proxy when downloading tar files |
| DEBUG           | YES, NO | NO  | Build debug executable; default is optimized
| SHARED          | YES, NO | YES | Build shared libraries if YES, archive (.a) if NO |
| CRAY            | YES, NO | YES | Is this a Cray system (special parallel options) |
| NEEDS_ZLIB      | YES, NO | NO  | If system does not have zlib installed, download and install it (HDF5 compression). |
| USE\_ZLIB\_NG   | YES, NO | NO  | Should the improved [zlib-ng](https://github.com/zlib-ng/zlib-ng) library be used to provide ZLIB capability |
| NEEDS_SZIP      | YES, NO | NO  | If system does not have szip installed, download and install it (HDF5 compression). |
| USE\_64BIT\_INT | YES, NO | NO  | In CGNS, enable 64-bit integers |
| CGNS            | YES, NO | YES | Should CGNS TPL be built.  |
| MATIO           | YES, NO | YES | Should matio TPL be built. |
| METIS           | YES, NO | NO  | Should metis TPL be built (parallel decomposition). |
| PARMETIS        | YES, NO | NO  | Should parmetis TPL be built (parallel decomposition). |
| ADIOS2          | YES, NO | NO  | Should adios2 TPL be built. |
| CATALYST2       | YES, NO | NO  | Should catalyst 2 TPL be built. |
| KOKKOS          | YES, NO | NO  | Should Kokkos TPL be built. |
| GNU_PARALLEL    | YES, NO | YES | Should GNU parallel script be built. |
| FMT             | YES, NO | YES | Should Lib::FMT TPL be built. |
| CATCH2          | YES, NO | YES | Should Catch2 be built (used for testing). |
| H5VERSION       | V114, V110 | V114 | Use HDF5-1.14.X or HDF5-1.10.X |
| H5CPP           | YES, NO | NO  | Should the HDF5 C++ library be built/installed |
| BB              | YES, NO | NO  | Enable Burst Buffer support in PnetCDF |
| JOBS            | {count} |  2   | Number of "jobs" used for simultaneous compiles |
| SUDO            | "" or sudo | "" | If need to be superuser to install |
*  NOTE: The `DOWNLOAD` and `BUILD` options can be used to download all TPL source; move to a system with no outside internet access and then build/install the TPLs.
*  The arguments can either be set in the environment as: `export COMPILER=gnu`, or passed on the script invocation line: `COMPILER=gnu ./install-tpl.sh`

## Configure, Build, and Install SEACAS
At this time, you should have all external TPL libraries built and
installed into `${ACCESS}/lib` and `${ACCESS}/include`. You are now ready
to configure the SEACAS CMake build.

*  `cd $ACCESS`
*  `mkdir build`
*  `cd build`
*  edit the `${ACCESS}/cmake-config` file and adjust compilers and other settings as needed.
*  enter the command `../cmake-config` and cmake should configure everything for the build.
*  `make && make install`
*  If everything works, your applications should be in `${ACCESS}/bin`
*  To install in a different location, do `INSTALL_PATH={path_to_install} ../cmake-config`
*  The default behavior can be modified via a few environment variables:

| Variable        | Values          | Default | Description |
|-----------------|:---------------:|:-------:|-------------|
| INSTALL_PATH    | path to install | pwd | Root of install path; default is current location |
| BUILDDIR        | {dir}   | `pwd`/build | Directory to do config and build |
| COMPILER        | clang, gnu, intel, ibm | gnu | What compiler should be used for non-parallel build |
| SHARED          | YES, NO | YES  | Build and use shared libraries is YES |
| APPLICATIONS    | YES, NO | YES  | Should all SEACAS applications be built (see `cmake-config`) |
| LEGACY          | YES, NO | YES  | Should the legacy SEACAS applications be built (see `cmake-config`) |
| FORTRAN         | YES, NO | YES  | Should fortran libraries and applications be built (see `cmake-config`) |
| ZOLTAN          | YES, NO | YES  | Should zoltan library and nem_slice be built |
| BUILD_TYPE      | debug, release | release | what type of build |
| MODERN          | YES, NO | NO   | Use "modern" CMake configuration files for netCDF and HDF5 |
| DEBUG           | -none-  |      | If specified, then do a debug build. Can't be used with `BUILD_TYPE` |
| HAVE_X11        | YES, NO | YES  | Does the system have X11 libraries and include files; used for blot, fastq |
| THREADSAFE      | YES, NO | NO   | Compile a thread-safe IOSS and Exodus library |
| USE_SRUN        | YES, NO | NO   | If MPI enabled, then use srun instead of mpiexec to run parallel tests |
| DOXYGEN         | YES, NO | NO   | Run doxygen on several packages during build to generate documentation |
| OMIT_DEPRECATED | YES, NO | NO   | Should the deprecated code be omitted; NO will enable deprecated code |
| EXTRA_WARNINGS  | YES, NO | NO   | Build with extra warnings enabled; see list in `cmake-config` |
| SANITIZER       | many    | NO   | If not NO, build using specified sanitizer; see list in `cmake-config` |
| GENERATOR       | many    | "Unix Makefiles" | what generator should CMake use; see cmake doc |
*  The arguments can either be set in the environment as: `export COMPILER=gnu`, or passed on the script invocation line: `COMPILER=gnu ./install-tpl.sh`

## Parallel Build

For some areas of use, a parallel version of SEACAS is required.  This
will build a "parallel-aware" version of the exodus library and a
parallel version of the Ioss library.

The only modification to the serial build described above is to make
sure that the mpicc parallel C compiler is in your path and to add the
`MPI=YES` argument to the `install-tpl.sh` script invocation when
building the TPLs.  For example:
```sh
   MPI=YES ./install-tpl.sh
```
This will download all requested libraries and build them with
parallel capability enabled (if applicable).  You can then continue
with the steps outlined in the previous section.

## Testing
There are a few unit tests for zoltan, exodus, ioss, and aprepro that can be run via `make test` or `ctest` if you configured with `-D Seacas_ENABLE_TESTS=YES`.

There is also a system-level test that just verifies that the applications can read and write exodus files correctly.  This test runs off of the installed applications.  To run do:

*  `make install`
*  `cd ../SEACAS-Test`
*  `make clean; make`

This will run through several of the SEACAS applications creating a mesh (exodus file) and then performing various manipulations on the mesh.  If the test runs successfully, there is some hope that everything has built and is running correctly.

## Exodus
If you only want the exodus library, then follow most of the above instructions with the following exceptions:

*  Clone entire source tree as above. (There used to be a zip file, but difficult to keep up-to-date)
*  You only need the netcdf and optionally hdf5 libraries
*  Use the `cmake-exodus` file instead of `cmake-config`.
*  This will build, by default, a shared exodus library and also install the exodus.py and exomerge.py Python interfaces.

## Trilinos

Although SEACAS is included in Trilinos
(https://github.com/trilinos/Trilinos), it is also possible to use the
SEACAS code from this repository to override the possibly older SEACAS
code in Trilinos.  The steps are to directly pull SEACAS from github
under Trilinos and then build SEACAS under Trilinos with that version
using `SEACAS_SOURCE_DIR_OVERRIDE`.  Here is how you do it:

```sh
cd Trilinos/
git clone https://github.com/sandialabs/seacas.git
cd BUILD/
cmake -DSEACAS_SOURCE_DIR_OVERRIDE:STRING=seacas/packages/seacas -DTrilinos_ENABLE_SEACAS [other options] ..
```

## SPACK

The SPACK package manager (https://spack.io/) can be used to install
SEACAS and all dependent third-party libraries.  SEACAS is a supported
package in SPACK as of December 2018.

```sh
git clone https://github.com/spack/spack.git
. spack/share/spack/setup-env.sh
spack install seacas~mpi   # Serial build (most common)
```

## Docker

An Ubuntu-based Docker image, with SEACAS built and installed, is available on [Docker Hub](https://hub.docker.com/r/mrbuche/seacas).

```sh
docker pull mrbuche/seacas
```

SEACAS is located in `/seacas` when running the container. There is also a similar image available on [Docker Hub](https://hub.docker.com/r/mrbuche/exodus) with only Exodus built and installed.

```sh
docker pull mrbuche/exodus
```

## CMake Example Usage
A simple example of using the SEACAS Exodus library in your external project.  Here is the CMakeLists.txt file:

```sh
project(ExodusCMakeExample VERSION 1.0 LANGUAGES C Fortran)
cmake_minimum_required(VERSION 3.1...3.26)

#### C ####
find_package(SEACASExodus CONFIG)
add_executable(ExodusWriteC ExodusWrite.c)
target_link_libraries(ExodusWriteC PRIVATE SEACASExodus::all_libs)

#### FORTRAN #####
IF ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fcray-pointer -fdefault-real-8 -fdefault-integer-8 -fno-range-check")
ELSEIF ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "XL")
  SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qintsize=8 -qrealsize=8")
ELSEIF ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
  SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -sdefault64")
ELSE()
  SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -i8")
ENDIF()

find_package(SEACASExodus_for CONFIG)
add_executable(ExodusReadFor ExodusRead.f)
target_link_libraries(ExodusReadFor PRIVATE SEACASExodus_for::all_libs)
```

A similar CMakeLists.txt file for using the IOSS library would be something like:

```sh
cmake_minimum_required(VERSION 3.1...3.26)
project(IossCMakeExample VERSION 1.0 LANGUAGES CXX)

#### C++ IOSS ####
find_package(SEACASIoss CONFIG)
add_executable(IossExample IossExample.C)
target_link_libraries(IossExample PRIVATE SEACASIoss::all_libs)
```

The `cmake-use-example` directory contains Exodus example files in the
`exodus` subdirectory and Ioss example files in the `ioss` subdirectory.
These provide short examples of how to build and link a program with the 
Exodus and/or Ioss libraries.

To use this, copy the contents of the directory to your own filespace
and modify the contents as needed.  

To configure and build, you would do something like:

```sh
  mkdir build; cd build
  CMAKE_PREFIX_PATH={path_to_root_of_seacas_install} cmake ..
  make
```

And you would then get an executable (`ExodusWriteC` and
`ExodusReadFor` for Exodus, `IossExample` for Ioss) compiled and linked
against the Exodus and/or Ioss libraries.

## Required Software

The SEACAS system requires that there be some libraries and
applications already existing on the chosen system prior to building
SEACAS and its required Third-Party Liberaries.

These include:
* The `git` application is used to access the SEACAS and TPL git
repositories

* CMake is used to generate the build system.

* C, C++, and (optionally) Fortran compilers.  The C compiler must
support the C11 standard and C++ must support C+++17.  GNU, Clang,
Intel, Cray, compilers are supported and tested regularly.

* For parallel capability, an MPI library is needed.  We have used
openmpi, mpich, mvapich, intel, and cray MPI libraries. It is
recommended to use as current an MPI library as possible.

* Automake is used to configure some of the TPL builds

* curl is needed to download some of the TPL library source code.

* python is required to use the `exodus.py` and `exomerge.py`
Python interfaces to Exodus databases. Python3 is recommended.

* To use the `blot` and `fastq` applications, an X11 development
environment is needed.

* Flex and Bison are optional if you are developing new capabilities
in aprepro.

* M4 is needed to build the netCDF library.

### Mac
On a mac system, I use the `brew` system which provides all of the
applications listed above.  The X11 system I use is `XQuartz`.  The
Mac also requires `Xcode`

### Linux
On an ubuntu system, the following is used to set up the basic
packages needed to compile SEACAS:
```sh
apt install -y libaec-dev zlib1g-dev automake autoconf \
libcurl4-openssl-dev libjpeg-dev wget curl bzip2 m4 flex bison cmake \
libzip-dev openmpi-bin libopenmpi-dev \
```

### Windows
On windows, I have used the following packages for MINGW64:
```sh
git mingw-w64-x86_64-toolchain make mingw-w64-x86_64-hdf5 \
mingw-w64-x86_64-cgns mingw-w64-x86_64-netcdf mingw-w64-x86_64-zlib \
mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-gcc-libgfortran \
mingw-w64-x86_64-cmake mingw-w64-x86_64-fmt
```
There is also a Visual Studio build performed at each commit to the
SEACAS git repository.  See the file `.appveyor.yml` for more details.

### Windows Powershell
A script provided by a user shows how they build on Windows using Powershell.  See [WIKI: Windows Powershell Script](https://github.com/sandialabs/seacas/wiki/Windows-Powershell-Script).

## License

SEACAS is licensed under the Modified BSD License.  See the [LICENSE](LICENSE) file for details.

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
| [Tessil Hash](https://github.com/Tessil/) | `packages/seacas/libraries/ioss/src/hash` |  [MIT](https://opensource.org/licenses/MIT) |
| [pdqsort](https://github.com/orlp/pdqsort) | `packages/seacas/libraries/ioss/src` | [Zlib License](https://github.com/orlp/pdqsort/blob/master/license.txt) |
## Contact information

 Greg Sjaardema  (<gsjaardema@gmail.com>, <gdsjaar@sandia.gov>)
