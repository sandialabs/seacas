# SEACAS  [[Documentation](http://gsjaardema.github.io/seacas/)] [[Wiki](https://github.com/gsjaardema/seacas/wiki)]
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/c82efc90be9943e08b71e2d16abaa370)](https://app.codacy.com/app/gsjaardema/seacas?utm_source=github.com&utm_medium=referral&utm_content=gsjaardema/seacas&utm_campaign=Badge_Grade_Dashboard)
[![Build Status](https://travis-ci.org/gsjaardema/seacas.svg?branch=master)](https://travis-ci.org/gsjaardema/seacas)
[![Analysis Status](https://scan.coverity.com/projects/2205/badge.svg?flat=1)](https://scan.coverity.com/projects/gsjaardema-seacas)
[![Spack Version](https://img.shields.io/spack/v/adios2.svg)](https://spack.readthedocs.io/en/latest/package_list.html#seacas)
[![Appveyor Build](https://ci.appveyor.com/api/projects/status/pis4gok72yh0wwfs/branch/master?svg=true)](https://ci.appveyor.com/project/gsjaardema/seacas/branch/master)

* [Get the sources](#get-the-sources)
* [Build instructions](#build-instructions)
* [Configure, Build, and Install SEACAS](#configure-build-and-install-seacas)
* [Parallel Build](#parallel-build)
* [Testing](#testing)
* [Exodus](#exodus)
* [Trilinos](#trilinos)
* [SPACK](#spack)
* [License](#license)
* [Contact information](#contact-information)
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
are required (or optional) to build SEACAS: HDF5, NetCDF, CGNS, MatIO,
Kokkos, and (if MPI set) PnetCDF libraries. You can build the
libraries using the `install-tpl.sh` script, or you can install them
manually as detailed in
[TPL-Manual-Install.md](TPL-Manual-Install.md).

  * To use the script, simply type `./install-tpl.sh`
  * The default behavior can be modified via a few environment variables:

| Variable        | Values          | Default | Description |
|-----------------|:---------------:|:-------:|-------------|
| INSTALL_PATH    | path to install | pwd | Root of install path; default is current location |
| COMPILER        | clang, gnu, intel, ibm | gnu | What compiler should be used for non-parallel build |
| MPI             | ON, OFF | OFF | If ON, then build parallel capability |
| FORCE           | YES, NO | NO  | Force downloading and building even if lib is already installed. |
| BUILD           | YES, NO | YES | Should TPLs be built and installed. |
| DOWNLOAD        | YES, NO | YES | Should TPLs be downloaded. |
| USE_PROXY       | YES, NO | NO  | Sandia specific -- use proxy when downloading tar files |
| DEBUG           | YES, NO | NO  | Build debug executable; default is optimized
| SHARED          | YES, NO | YES | Build shared libraries is YES, archive (.a) if NO |
| CRAY            | ON, OFF | ON  | Is this a Cray system (special parallel options) |
| NEEDS_ZLIB      | YES, NO | NO  | If system does not have zlib installed, download and install it (HDF5 compression). |
| NEEDS_SZIP      | YES, NO | NO  | If system does not have szip installed, download and install it (HDF5 compression). |
| USE\_64BIT\_INT | YES, NO | NO  | In CGNS, enable 64-bit integers |
| CGNS            | ON, OFF | ON  | Should CGNS TPL be built.  |
| MATIO           | ON, OFF | ON  | Should matio TPL be built. |
| METIS           | ON, OFF | OFF | Should metis TPL be built (parallel decomposition). |
| PARMETIS        | ON, OFF | OFF | Should parmetis TPL be built (parallel decomposition). |
| ADIOS2          | ON, OFF | OFF | Should adios2 TPL be built. |
| KOKKOS          | ON, OFF | OFF | Should Kokkos TPL be built. |
| GNU_PARALLEL    | ON, OFF | ON  | Should GNU parallel script be built. |
| H5VERSION       | V12, V110, V18 | V110 | Use HDF5-1.12.X, HDF5-1.10.X or HDF5-1.8.X |
| BB              | YES, NO | NO  | Enable Burst Buffer support in PnetCDF |
| JOBS            | {count}|  2      | Number of "jobs" used for simultaneous compiles |
| SUDO            | "" or sudo | "" | If need to be superuser to install |
  * NOTE: The `DOWNLOAD` and `BUILD` options can be used to download all TPL source; move to a system with no outside internet access and then build/install the TPLs.
  * The arguments can either be set in the environment as: `export COMPILER=gnu`, or passed on the script invocation line: `COMPILER=gnu ./install-tpl.sh`

## Configure, Build, and Install SEACAS
At this time, you should have all external TPL libraries built and
installed into `${ACCESS}/lib` and `${ACCESS}/include`. You are now ready
to configure the SEACAS CMake build.

* `cd $ACCESS`
* `mkdir build`
* `cd build`
* edit the `${ACCESS}cmake-config` file and adjust compilers and other settings as needed.
* enter the command `../cmake-config` and cmake should configure everything for the build.
* `make && make install`
* If everything works, your applications should be in `${ACCESS}/bin`
* To install in a different location, do `INSTALL_PATH={path_to_install} ../cmake-config`
* The default behavior can be modified via a few environment variables:

| Variable        | Values          | Default | Description |
|-----------------|:---------------:|:-------:|-------------|
| INSTALL_PATH    | path to install | pwd | Root of install path; default is current location |
| BUILDDIR        | {dir}   | `pwd`/build | Directory to do config and build |
| COMPILER        | clang, gnu, intel, ibm | gnu | What compiler should be used for non-parallel build |
| SHARED          | ON, OFF | ON  | Build and use shared libraries is ON |
| APPLICATIONS    | ON, OFF | ON  | Should all SEACAS applications be built (see `cmake-config`) |
| LEGACY          | ON, OFF | ON  | Should the legacy SEACAS applications be built (see `cmake-config`) |
| FORTRAN         | ON, OFF | ON  | Should fortran libraries and applications be built (see `cmake-config`) |
| ZOLTAN          | ON, OFF | ON  | Should zoltan library and nem_slice be built |
| PYTHON_VER      | 2.7 3.0 | 3.0 | Minimum version of python which should be found |
| BUILD_TYPE      | debug, release| release | what type of build |
| DEBUG           | -none- |      | If specified, then do a debug build. Can't be used with `BUILD_TYPE` |
| HAVE_X11        | ON, OFF | ON  | Does the system have X11 libraries and include files; used for blot, fastq |
| THREADSAFE      | ON, OFF | OFF | Compile a thread-safe IOSS and Exodus library |
| USE_SRUN        | ON, OFF | OFF | If MPI enabled, then use srun instead of mpiexec to run parallel tests |
| DOXYGEN         | ON, OFF | OFF | Run doxygen on several packages during build to generate documentation |
| OMIT_DEPRECATED | YES, NO | NO  | Should the deprecated code be omitted; NO will enable deprecated code |
| EXTRA_WARNINGS  | YES, NO | NO  | Build with extra warnings enabled; see list in `cmake-config` |
| SANITIZER       | many    | NO  | If not NO, build using specified sanitizer; see list in `cmake-config` |
| GENERATOR       | many    | "Unix Makefiles" | what generator should CMake use; see cmake doc |
* The arguments can either be set in the environment as: `export COMPILER=gnu`, or passed on the script invocation line: `COMPILER=gnu ./install-tpl.sh`

## Parallel Build

For some areas of use, a parallel version of SEACAS is required.  This
will build a "parallel-aware" version of the exodus library and a
parallel version of the Ioss library.

The only modification to the serial build described above is to make
sure that the mpicc parallel C compiler is in your path and to add the
`MPI=ON` argument to the `install-tpl.sh` script invokation when
building the TPLs.  For example:
```
   MPI=ON ./install-tpl.sh
```
This will download all requested libraries and build them with
parallel capability enabled (if applicable).  You can then continue
with the steps outlined in the previous section.

## Testing
There are a few unit tests for zoltan, exodus, and aprepro that can be run via `make test` if you configured with `-D SEACASProj_ENABLE_TESTS=ON`.

There is also a system-level test that just verifies that the applications can read and write exodus files correctly.  This test runs off of the installed applications.  To run do:

* `make install`
* `cd ../SEACAS-Test`
* `make clean; make`

This will run through several of the SEACAS applications creating a mesh (exodus file) and then performing various manipulations on the mesh.  If the test runs successfully, there is some hope that everything has built and is running correctly.

## Exodus
If you only want the exodus library, then follow most of the above instructions with the following exceptions:

* Clone entire source tree as above. (There used to be a zip file, but difficult to keep up-to-date)
* You only need the netcdf and optionally hdf5 libraries
* Use the `cmake-exodus` file instead of `cmake-config`.
* This will build, by default, a shared exodus library and also install the exodus.py and exomerge.py Python interfaces.

## Trilinos

Although SEACAS is included in Trilinos
(https://github.com/trilinos/Trilinos), it is also possible to use the
SEACAS code from this repository to override the possibly older SEACAS
code in Trilinos.  The steps are to directly pull SEACAS from github
under Trilinos and then build SEACAS under Trilinos with that version
using `SEACAS_SOURCE_DIR_OVERRIDE`.  Here is how you do it:

```
cd Trilinos/
git clone https://github.com/gsjaardema/seacas.git
cd BUILD/
cmake -DSEACAS_SOURCE_DIR_OVERRIDE:STRING=seacas/packages/seacas -DTrilinos_ENABLE_SEACAS [other options] ..
```

## SPACK

The SPACK package manager (https://spack.io/) can be used to install
SEACAS and all depedent third-party libaries.  SEACAS is a supported
package in SPACK as of December 2018.

```
git clone https://github.com/spack/spack.git
. spack/share/spack/setup-env.sh
spack install seacas~mpi   # Serial build (most common)
```

Enter `spack info seacas` to see information on supported variants and other information about the SEACAS package.

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
| [terminal_color](https://github.com/matovitch/trmclr) | `packages/seacas/libraries/suplib_cpp` | [zlib](https://opensource.org/licenses/zlib) |
| [Tessil Hash](https://github.com/Tessil/) | `packages/seacas/libraries/ioss/src/hash` |  [MIT](https://opensource.org/licenses/MIT) |
| [Catch2](https://github.com/catchorg/Catch2) | `packages/seacas/libraries/ioss/src/catch.hpp` | [Boost](http://www.boost.org/LICENSE_1_0.txt) |
| [{fmt}](https://github.com/fmtlib/fmt) | `packages/seacas/libraries/ioss/src/fmt` | [BSD-2-Clause](https://github.com/fmtlib/fmt/blob/master/LICENSE.rst) |
## Contact information

 Greg Sjaardema  (<gsjaardema@gmail.com>, <gdsjaar@sandia.gov>)

