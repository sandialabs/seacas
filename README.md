# Instructions for building SEACAS
(See the bottom of the file for contact information)

The SEACAS system uses imake to generate Makefiles for each of its
modules.  To build the SEACAS system you must first build and install
imake and its associated tools, set your SEACAS environment variable
and path, then generate Makefiles for SEACAS to build and install
SEACAS.

If all you need is the exodus library, it can be built by itself using
cmake.  See the "exodus cmake" section below for more information.

## Clone the SEACAS distribution to the directory of your choice:
```
git clone https://github.com/gdsjaar/seacas.git SEACAS
```
This will create a directory that will be referred to as _SEACAS_ in the instructions that follow. You can rename this directory to any other name you desire.

## Download external libraries -- netcdf, hdf5, matio, parallel
There are a few externally developed libraries that are required to build SEACAS.  The netcdf library is required; the hdf5, matio, and parallel libraries are optional. Note that you do not need to build these libraries manually; they will be built by the SEACAS build system. 

#### netcdf:
The most recent released version is recommended. 

   * Download the latest netcdf-c release (currently netcdf-4.3.3.1.tar.gz) from htttp://www.unidata.ucar.edu/downloads/netcdf/index.jsp and put it inside SEACAS/TPL/netcdf
   * cd TPL/netcdf
   * tar zxvf netcdf-4.3.3.1.tar.gz
   * If the untar does not create a netcdf-4.3.3.1 directory, modify the Imakefile in the current directory such that V_NUM specifies the correct name
   * Modify the following defines in SEACAS/TPL/netcdf/netcdf-4.3.3.1/include/netcdf.h

```
	#define NC_MAX_DIMS     65536    /* max dimensions per file */
	#define NC_MAX_VARS     524288   /* max variables per file */
	#define NC_MAX_VAR_DIMS 8        /* max per variable dimensions */
```

#### HDF5: 
Download hdf5 and get it ready to be built by the buildSEACAS script below. The hdf5 library is used for the netcdf4 capability in netcdf which in turn is used by exodus.  The netcdf4 capability is typically used for large models (>150 million elements); if you are not planning to create or read models of this size, you do not have to build hdf5. In this case, edit the SEACAS/TPL/Imakefile and remove $(HDF5SRC) line. You will also need to make sure that UseNetcdf4 (see below) is defined to NO.

   * Download hdf5 from http://www.hdfgroup.org/HDF5/release/obtain5.html and put it inside SEACAS/TPL/hdf5
   * untar it, creating a directory will will refer to as hdf5-X.X.X
   * Edit the Imakefile in the TPL/hdf5 directory and make sure that TRUE_HDF5 is set to the version of hdf5 that you downloaded.

#### MATIO:
The matio library is used in the exo2mat and mat2exo programs which convert an exodus file to and from a matlab binary file.  To use this do:

   * Download the most recent version of the library from http://sourceforge.net/projects/matio/. Currently this is matio-1.5.2.tar.gz. 
   * cd TPL/matio
   * tar zxvf /path/to/matio-1.5.2.tar.gz
   * Edit the Imakefile in the TPL/matio directory and make sure that TRUE_MATIO is set to the version of matio that you downloaded.
    * If you do not want to build matio, you can edit the TPL/Imakefile and remove 'matio' from the list. Also remove the '\' on the previous line.

#### PARALLEL: 
GNU Parallel is a shell tool for executing jobs in parallel using one or more computers. A job is typically a single command or a small script that has to be run for each of the lines in the input. The typical input is a list of files, a list of hosts, a list of users, or a list of tables.  In SEACAS, this is only used by epup which runs multiple epu jobs concurrently.  To build:

   * Download the most recent version of the library from ftp://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2. Currently this is parallel-20150522
   * cd TPL/parallel
   * tar jxvf /path/to/parallel-latest.tar.bz2
   * Edit the Imakefile in the TPL/parallel directory and make sure that TRUE_PARALLEL is set to the version of parallel that you downloaded.

#### CGNS
Experimental support for CGNS in the IOSS library is being added.  To use this capability, you will need to download the CGNS library.  To build:

   * Download CGNS via git.
   * cd TPL/cgns
   * git clone https://github.com/CGNS/CGNS.git

## Edit Configuration Files:

cd back to the top-level SEACAS subdirectory. The current directory (SEACAS unless you renamed if) will be your SEACAS root.  Set the environment variable SEACAS to point to this area. 
```
    setenv SEACAS `pwd`  (csh/tcsh)
    export SEACAS=`pwd`  (sh/bash)
```
If you do `ls $SEACAS/src`, you should see something similar to:
```
   applications  Imakefile  itools  libraries  scripts  
```

NOTE: The site.def and platform.cf files describe below are processed 
      by the C preprocessor (CPP). Therefore, all data in the files must be 
      valid CPP syntax:
      
 * `XCOMM` indicates a comment line that will appear in the processed file
 * `/* .... */` is a C-comment that will be removed by the CPP
 * The `#` indicates code processed by CPP; typically the files use `#define`, `#else`, `#endif`, and `#ifdef` to determine options and define values.
 * Most other syntax will cause problems.

 * Move into the `$SEACAS/src/itools/config/cf` directory and edit the `site.def` and the configuration file associated with your platform (e.g. `linux.cf` for linux OS; `darwin.cf` for MacOS)

### site.def
The `site.def` file contains settings that determine where SEACAS is going to be installed, who owns the source, and where the X11 libraries and includes files are located on your system The following setting should be entered in the site.def file:

```
	#define        Owner owner_username
	#define        Group group_name 
```
The owner and group name of the user installing the code should be set.  We have created a group called seacas on our systems for the support staff that installs and maintains the SEACAS system. If you do not have or need a group, you can delete the `#define Group` line. As an example:

```
	#define        Owner gdsjaar
	#define        Group seacas 
```

 * `#define AccessRoot` - path to SEACAS source directory. It should be the same directory path as you set as the SEACAS environment variable.

 * `#define BuildSharedExodusLibrary YES` Leave this set to YES unless there are problems building shared libraries on your system.

 * `#define UseNetcdf4 NO or YES` Set to YES to use the new hdf5-based netcdf-4; leave as NO to use the classic netcdf-4.
      
 * `#define HDF5_Root AccessRoot` If `UseNetcdf4` is `YES`, then `HDF5_Root` needs to point to the root of an installed HDF5 library (both libhdf5.a and libhdf5_hl.a) and include files.  `HDF5_Root/lib` holds the libs and `HDF5_Root/include` has the include files. Leave the value as `AccessRoot` unless you have an external installation of HDF5 that you want to use.
         
 * `#define Platform normal`  For most cases, leave the Platform define as "normal"; however,
    * if building on "interix", define it as interix;
    * if building for "redstorm", define it as redstorm;

 * If you want to enable the experimental CGNS support, change the line "#define UseCGNS NO" to "#define UseCGNS YES"

### platform.cf
Edit `platform.cf` (platform=hardware or os type) [`linux.cf`, `darwin.cf`, ?.cf]
If you are building on a 64-bit system, define `Build64BitAccess YES`.

NOTE: If you are using a gcc version prior to 4.1, then you cannot build the fortran-based utilities for 64-bit systems; you must do a 32-bit build. I can tell you the details if you want...  If you are using a non-gnu compiler or using gcc-4.1.0 or later (which gfortran as the fortran compiler), you can do a 64-bit build.

NOTE: If you are building on MacOS and using the Mac ports compilers, then define `USE_MACPORTS` as the version number of the compiler (4.4, 4.5, 4.6, ...)

The platform.cf configuration file contains system-specific settings; in particular compiler paths.  The linux.cf file is the most complicated due to the multitude of compilers available.  Pick the define corresponding to the compilers you will be using and then search for that #if block and make sure paths are correct; the paths typically only need changing for parallel compiles and if your system has multiple versions of compilers.

## Build 
Use the script `src/scripts/buildSEACAS`, which enters the commands for you.  You should be in the directory specified as your `AccessRoot` above. Then enter:

```
      sh src/scripts/buildSEACAS -auto
```

If you have any problems, send an email to `gdsjaar@sandia.gov`. Include any log files that were created during your attempted build. These will be in `$SEACAS/Logs`.  Also include the `site.def` and `platform.cf` (e.g. `linux.cf`, `darwin.cf`).  The subject of your email should include "SEACAS build error".

## Testing

Once everything builds, you can do a basic correctness test.  cd to the SEACAS-Test subdirectory and type `make`.  It will run through several of the programs building several meshes, joining them into a single mesh, decomposing it for use in a parallel file-per-processor analysis, joining the per-processor files back to a single file using epu and then converting the mesh file to text format and to matlab format.

The exodus library can also be tested by changing directory to the src/libraries/exodus subdirectory and typing `make check`.  This will test both the C and fortran API.

# Exodus CMake
If you only need the exodus library and do not want to mess with imake and the building of all the SEACAS applications and libraries, you can use cmake and just build the exodus library.

To do this, cd to the exodus subdirectory at src/libraries/exodus and enter the cmake command

 "cmake -DUSE_TRIBITS=OFF ." 

followed by "make" if the cmake step is successful.  You may have to tell cmake how to find your netcdf and optionally hdf5 and hdf5_hl libraries if it cannot find them automatically.

Additional options for cmake are:
 -DSEACAS_ENABLE_NETCDF4_SUPPORT=ON  -- if your netcdf has netcdf-4 enabled
 -DBUILD_TESTING=1 -- to build the exodus tests.  Use "make check" to run them
 -DBUILD_SHARED_LIBS=ON -- to build shared libraries instead of static.

## Contact information

 Greg Sjaardema  (gsjaardema@gmail.com,  gdsjaar@sandia.gov)
      
