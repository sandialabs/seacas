/* 
 * analysis.h 
 * $Id: analysis.h,v 1.1 2008/10/31 05:19:56 gdsjaar Exp $
 */

/* 
Setup definitions for CPP 
The following are used in this script:

ANALYSIS	- script is for an analysis code
EXECUTABLE	- the name of the analysis code executable
D_PRECISION	- analysis code is double precision
EXODUSII2	- analysis code links with exodusIIv2 libraries
SEACO		- analysis code writes seaco file format
STATIC		- use FORTRAN static flags for compiling user subroutine
XCODE		- script will run cvs head version of analysis code
ISPARALLEL	- analysis code can be executed in parallel
MESH		- FORTRAN unit number for mesh file.
VIEWFACTOR	- FORTRAN unit number for viewfactor file (coyote)
USR		- FORTRAN unit number for usr file (coyote)
DIST_LOAD	- FORTRAN unit number for distributed load file
EXTERNAL	- FORTRAN unit number for external file
PLOT		- FORTRAN unit number for plot file
HISTORY		- FORTRAN unit number for history file
RSOUT		- FORTRAN unit number for output restart file
RSIN		- FORTRAN unit number for input restart file
THERMAL		- FORTRAN unit number for thermal file
INTERPOLATED	- FORTRAN unit number for interpolated file (merlin2)
CONFIGFILE	- FORTRAN unit number for cfg file (for parallel execution)
SHUTDOWNFILE	- FORTRAN unit number for shutdown file
IOSET1/IOSET2	- Checks for existance of specified files  
FORMAT1		- Code reads ExodusI input and restart files.
			Translate file format if necessary.
FORMAT2		- Code reads ExodusII input and restart files.
			Translate file format if necessary.
FORMAT3		- Code reads ExodusII input file. Translate file if necessary.

*/

#define ANALYSIS 1

#ifdef MPI_IBM
setenv MP_RESD	yes
setenv MP_EUILIB us
#endif

#define SUBOPTION

#if defined(PRONTO2D)
#define EXECUTABLE pronto2d
#define STATIC
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define THERMAL FOR056
#define DIST_LOAD FOR038
#define EXTERNAL FOR039
#define HISTORY FOR013
#define EXODUSII2
#define D_PRECISION
#define IOSET1
#define FORMAT2
#endif

#if defined(XPRONTO2D)
#define XCODE
#define EXECUTABLE pronto2d
#define STATIC
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define THERMAL FOR056
#define DIST_LOAD FOR038
#define EXTERNAL FOR039
#define HISTORY FOR013
#define EXODUSII2
#define D_PRECISION
#define IOSET1
#define FORMAT2
#endif

#if defined(PRONTO3D)
#define EXECUTABLE pronto3d
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define THERMAL FOR056
#define DIST_LOAD FOR038
#define EXTERNAL FOR039
#define HISTORY FOR013
#define CONFIGFILE FOR062
#define SHUTDOWNFILE FOR063
#define EXODUSII2
#define D_PRECISION
#define ISPARALLEL
#define IOSET1
#define FORMAT2
#endif

#if defined(PRONTO3D94)
#define EXECUTABLE pronto3d-9.4
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define THERMAL FOR056
#define DIST_LOAD FOR038
#define EXTERNAL FOR039
#define HISTORY FOR013
#define CONFIGFILE FOR062
#define SHUTDOWNFILE FOR063
#define EXODUSII2
#define D_PRECISION
#define ISPARALLEL
#define IOSET1
#define FORMAT2
#endif

#if defined(XPRONTO3D)
#define XCODE
#define EXECUTABLE pronto3d
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define THERMAL FOR056
#define DIST_LOAD FOR038
#define EXTERNAL FOR039
#define HISTORY FOR013
#define CONFIGFILE FOR062
#define SHUTDOWNFILE FOR063
#define EXODUSII2
#define D_PRECISION
#define ISPARALLEL
#define IOSET1
#define FORMAT2
#endif

#if defined(JAS3D)
#define EXECUTABLE jas3d
#define MESH FOR009
#define CONSTRAINT FOR010
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define HISTORY FOR013
#define CONFIGFILE FOR062
#define SHUTDOWNFILE FOR063
#define SUMMARY FOR008
#define EXODUSII2
#define D_PRECISION
#define ISPARALLEL
#define IOSET1
#define FORMAT2
#endif

#if defined(XJAS3D)
#define XCODE
#define EXECUTABLE jas3d
#define MESH FOR009
#define CONSTRAINT FOR010
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define HISTORY FOR013
#define CONFIGFILE FOR062
#define SHUTDOWNFILE FOR063
#define SUMMARY FOR008
#define EXODUSII2
#define D_PRECISION
#define ISPARALLEL
#define IOSET1
#define FORMAT2
#endif

#if defined(SANTOSQA)
#define EXECUTABLE santosqa
#define STATIC
#define MESH FOR009
#define PLOT FOR011
#define SEACO
#define RSIN FOR032
#define RSOUT FOR030
#define THERMAL FOR056
#define DIST_LOAD FOR038
#define D_PRECISION
#define IOSET1
#define FORMAT1
#endif

#if defined(SANTOS)
#define EXECUTABLE santos
#define STATIC
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define THERMAL FOR056
#define DIST_LOAD FOR038
#define D_PRECISION
#define EXODUSII2
#define IOSET1
#define FORMAT3
#endif

#if defined(SANTOS3D)
#define EXECUTABLE santos3d
#define STATIC
#define MESH FOR009
#define PLOT FOR011
#define SEACO
#define RSIN FOR032
#define RSOUT FOR030
#define THERMAL FOR056
#define DIST_LOAD FOR038
#define IOSET1
#define FORMAT1
#endif

#if defined (COYOTE)
#define EXECUTABLE coyote
#define EXODUSII2
#define MESH FOR010
#define PLOT FOR012
#define EXTERNAL FOR013
#define RSIN FOR014
#define VIEWFACTOR FOR015
#define USR FOR016
#define IOSET1
#define FORMAT3
#endif

#if defined(JACQ3D)
#define EXECUTABLE jacq3d
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define THERMAL FOR056
#define DIST_LOAD FOR038
#define EXODUSII2
#define D_PRECISION
#define IOSET1
#define FORMAT2
#endif

#if defined(JAC2D)
#define EXECUTABLE jac2d
#define STATIC
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define THERMAL FOR058
#define DIST_LOAD FOR038
#define EXODUSII2
#define D_PRECISION
#define IOSET1
#define FORMAT2
#endif

#if defined(JAC3D)
#define EXECUTABLE jac3d
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define THERMAL FOR058
#define DIST_LOAD FOR038
#define EXODUSII2
#define D_PRECISION
#define IOSET1
#define FORMAT2
#endif

#if defined(SANCHO)
#define EXECUTABLE sancho
#define STATIC
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR019
#define RSOUT FOR018
#define THERMAL FOR056
#define SEACO
#define IOSET1
#define FORMAT1
#endif

#if defined(SUBWAY)
#define EXECUTABLE subway
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define IOSET1
#define FORMAT1
#endif

#if defined(CONCHAS)
#define EXECUTABLE conchas
#define MESH FOR009
#define PLOT FOR011
#define RSIN FOR032
#define RSOUT FOR030
#define IOSET1
#define FORMAT1
#endif

#if defined(MERLIN2)
#define EXECUTABLE merlin2
#define EMESH FOR013
#define EPLOT FOR012
#define RSIN FOR032
#define RSOUT FOR030
#define INTERPOLATED FOR014
#define THERMAL FOR015
#define EXODUSII2
#define IOSET2
#endif

#if defined(MAPVAR)
#define EXECUTABLE mapvar
#define EMESH FOR013
#define EPLOT FOR012
#define INTERPOLATED FOR014
#define EXODUSII2
#define IOSET2
#endif

#if defined(MAPVARKD)
#define EXECUTABLE mapvar-kd
#define EMESH FOR013
#define EPLOT FOR012
#define INTERPOLATED FOR014
#define EXODUSII2
#define IOSET2
#endif

/* Cray system environment variables */
#if defined(CRAY)
setenv NCPUS 1
#if defined EXODUSII2
setenv NETCDF_FFIOSPEC bufa:336:2
#endif
#endif

