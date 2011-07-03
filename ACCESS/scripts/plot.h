/*
 * plot.h
 * $Id: plot.h,v 1.1 2008/10/31 05:19:57 gdsjaar Exp $
 */

/*
Setup definitions for CPP
The following are used in this script:

GRAPHICS	- script is for a plotting code
EXECUTABLE	- the name of the plotting code executable
EXOI		- the plotting code reads exodusI file format
EXOI_NAME	- the name of the plotting code that reads exodusI file format
EXODUSII2	- the plotting code reads exodusII file format
EXOII_NAME	- the name of the plotting code that reads exodusII file format
EXODUS_SWITCH   - the code parses the -exodus 1/2 flag
INFILE		- FORTRAN unit number for infile (BLOT)
INPUT		- FORTRAN unit number for input file
MESH		- FORTRAN unit number for mesh file (fastq)
HARDCOPY	- code is capable of writing hardcopy output file
IOSET1		- Blot input
IOSET2		- Fastq input
IOSET3		- Graph input
FORMAT1		- Code has both an ExodusI and ExodusII version.
			Choose version of code based on file type. (blot)
RUN1		- run blot
RUN2		- run fastq
RUN3		- run graph

*/

#define GRAPHICS 1

#if defined(BLOT)
#define EXECUTABLE blotII2
#define EXODUSII2
#define EXOII_NAME blotII2
#define INFILE FOR011
#define FORMAT3
#define HARDCOPY
#define INPUT FOR007
#define IOSET1
#define RUN1
set USAGE = \
"Usage: blot [-help] [-options option] [--] filename [device]"
#endif

#if defined(FASTQ)
#define EXECUTABLE fastq
#define EXOI
#define EXOI_NAME fastq
#define INPUT FOR001
#define MESH FOR009
#define HARDCOPY
#define EXODUS_SWITCH
#define IOSET2
#define RUN2
set USAGE = \
"Usage: fastq [-help] [-options option] [--] filename [device]"
#endif

#if defined(GRAPH)
#define EXECUTABLE graph
#define EXOI
#define INPUT FOR095
#define HARDCOPY
#define IOSET3
#define RUN3
set USAGE = \
"Usage: graph [-help] [-options option] [--] [device]"
#endif


