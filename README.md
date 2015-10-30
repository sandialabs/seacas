# Instructions for building SEACAS
 * See the bottom of the file for contact information
 * The old imake-based build has been removed.
 * If you just need the exodus library, see EXODUS below.

## Clone the SEACAS distribution to the directory of your choice:
```
   git clone https://github.com/gdsjaar/seacas.git
```
This will create a directory that will be referred to as _seacas.git_ in
the instructions that follow. You can rename this directory to any
other name you desire. Set an environment variable pointing to this
location by doing:

```
     "cd seacas.git; export ACCESS=`pwd`"
```
## Download external libraries -- netcdf, hdf5, matio, parallel
There are a few externally developed libraries that are required to
build SEACAS.  The netcdf library is required; the hdf5, matio, and
parallel libraries are optional.  The old imake system used to build
the libraries for you, but with the current cmake-based build system,
you must build and install the libraries manually prior to building
SEACAS.


#### HDF5: 
If you are using the netcdf-4 capability in the netcdf library or are using the matio library for conversion of exodus to/from matlab format, then you will need the hdf5 library. 

The hdf5 library is used for the netcdf4 capability in netcdf which in
turn is used by exodus.  The netcdf4 capability is typically used for
large models (>150 million elements); if you are not planning to
create or read models of this size, you do not have to build hdf5. 

   * Download hdf5 from http://www.hdfgroup.org/HDF5/release/obtain5.html and put it inside seacas.git/TPL/hdf5
   * untar it, creating a directory will will refer to as hdf5-X.X.X
   * cd to that directory and enter the command:

```
      ./configure --prefix=${ACCESS} --enable-shared --enable-production --enable-debug=no --enable-static-exec
```
   * make; make install

#### NetCDF:
The most recent released version is recommended. 

 * Download the latest netcdf-c release (currently netcdf-4.3.3.1.tar.gz) from htttp://www.unidata.ucar.edu/downloads/netcdf/index.jsp and put it inside seacas.git/TPL/netcdf
 * cd TPL/netcdf
 * tar zxvf netcdf-4.3.3.1.tar.gz
 * Modify the following defines in seacas.git/TPL/netcdf/netcdf-4.3.3.1/include/netcdf.h

```
	#define NC_MAX_DIMS     65536    /* max dimensions per file */
	#define NC_MAX_VARS     524288   /* max variables per file */
	#define NC_MAX_VAR_DIMS 8        /* max per variable dimensions */
```

 * cd netcdf-4.3.3.1 and enter the command:

```
	./configure --enable-netcdf-4  --enable-shared \
	            --disable-fsync --prefix ${ACCESS} \
		    --disable-dap --disable-cdmremote
```

 * If the configure step complains about not being able to find the
   HDF5 library, you may need to do the following and then redo the
   configure step
```
	CFLAGS='-I{HDF5_ROOT}/include'; export CFLAGS
	LDFLAGS='-L{HDF5_ROOT}/lib   '; export LDFLAGS
```

 * make; make install

#### MATIO:
The matio library is used in the exo2mat and mat2exo programs which convert an exodus file to and from a matlab binary file.  To use this do:

 * Download the most recent version of the library from http://sourceforge.net/projects/matio/. Currently this is matio-1.5.2.tar.gz. 
 * cd TPL/matio
 * tar zxvf /path/to/matio-1.5.2.tar.gz
 * There is a bug related to reading version 7.3 files.  To fix the
bug, enter the command:

```
   patch -p3 < MATIO-fix-issue-reading-version-7.3-files.patch
```

 * cd matio-1.5.2 and enter the command:

```
     # The -L is to find the hdf5 library...
     export LDFLAGS='-L${ACCESS}/lib'
     ./configure --with-hdf5=${ACCESS} --enable-mat73 --enable-shared --prefix=${ACCESS}
```
 * make; make install

#### PARALLEL: 
GNU Parallel is a shell tool for executing jobs in parallel using one or more computers. A job is typically a single command or a small script that has to be run for each of the lines in the input. The typical input is a list of files, a list of hosts, a list of users, or a list of tables.  In SEACAS, this is only used by epup which runs multiple epu jobs concurrently.  To build:

   * Download the most recent version of the library from ftp://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2. Currently this is parallel-20150522

```
   cd TPL/parallel
   tar jxvf /path/to/parallel-latest.tar.bz2
   cd parallel-20150522
   ./configure --prefix ${ACCESS}
```
#### CGNS:
Experimental support for CGNS in the IOSS library is being added.  To use this capability, you will need to download the CGNS library.  To build:

   * Download CGNS via git:

```
   cd TPL/cgns
   git clone https://github.com/CGNS/CGNS.git
```
   * build using cmake.  See TPL/cgns/cmake-config.in as example

## Configure, Build, and Install SEACAS
At this time, you should have all external TPL libraries built and
installed into ${ACCESS}/lib and ${ACCESS}/include. You are now ready
to configure the SEACAS CMake build.

   * `cd $ACCESS`
   * `mkdir build`
   * `cd build`
   * edit the ${ACCESS}cmake-config file and adjust compilers and
     other settings as needed. The hdf5 linking with netcdf isn't
configured correctly yet, so the line specifying the
SEACAS_EXTRA_LINK_FLAGS may be needed for a proper link.
   * enter the command `../cmake-config` and cmake should configure
everything for the build.
   * `make -j4; make install`
   * If everything works, your applications should be in ${ACCESS}/bin

## EXODUS
If you only want the exodus library, then follow most of the above
instructions with the following exceptions:
  * You only need the netcdf and optionally hdf5 libraries
  * Use the `cmake-exodus` file instead of cmake-config.
  * This will build, by default, a shared exodus library and also
install the exodus.py python interface.

## LICENSE

SEACAS is licensed under the Modified BSD License.  See the LICENSE 
file for details.

## Contact information

 Greg Sjaardema  (gsjaardema@gmail.com,  gdsjaar@sandia.gov)
      
