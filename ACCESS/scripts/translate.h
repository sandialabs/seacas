/*
 * $Id: translate.h,v 1.1 2008/10/31 05:19:57 gdsjaar Exp $
 * translate.h
*/

/*
Setup definitions for CPP
The following are used in this script:

TRANSLATE	- script is for a translator code
EXECUTABLE	- the name of the translator code
EXOI		- the translator code reads exodusI file format
EXOI_NAME	- name of the translator code that reads exodusI file format
EXODUSII2	- the translator code reads exodusII file format
EXOII_NAME	- name of the translator code that reads exodusII file format
INFILE		- FORTRAN unit number for input file
INPUT		- FORTRAN unit number for output file
TAPE		- FORTRAN unit number for tape input file (emerge)
PREPOST		- 
SUBOPTION	- script has subroutine option feature at run time
DIM		- 
TRANSFORM	- 
MATERIAL	- 
IOSET1		- 
IOSET2		- 
IOSET3		- 
IOSET4		- 
IOSET5		- 
IOSET6		- 
IOSET7		- 
FORMAT  - set versflg if file is exodusI or exodusIIv2 format.
          generate error if file is of neither format.
FORMAT1 - code has both an ExodusI and ExodusII version.
          Choose version of code base on file type
FORMAT2 - code will only accept ExodusI format files
          Translate file to ExodusI format
FORMAT3 - code will only accept ExodusII format files.
          Translate file to ExodusII format
FORMAT4 - If file is already the targeted format, exit script
          and do not run translators (ex1ex2v2 or ex2ex1v2)
	  (unless -force is set, then copy input to output)
FORMAT5 - No test for file format
FORMAT6	- Used for exoxdr translator
FORMAT7	- code will only accept ExodusII format files.
          Translate file to ExodusII format (used for conex2)
FORMAT8 - txtexo translator.  Grep first line of input (text) file
          for 'exo2txt' or to determine which format to use for output. 

*/

#define TRANSLATOR 

#if defined(CONEX)
#define EXECUTABLE conex
#define EXOI
#define EXOII_NAME conex2
#define EXODUSII2
#define IOSET6
#define FORMAT7
#define INFILE FOR011
#define OUTFILE FOR012
set USAGE = \
"Usage: conex  [-help] [-options option] [--] first_exoII_in combined_exoII_out"
#endif
#if defined(ALGEBRA)
#define EXECUTABLE algebra2
#define EXODUSII2
#define EXOII_NAME algebra2
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR012
#define FORMAT3
#define PREPOST
set USAGE = \
"Usage: algebra [-help] [-options option] [--] exo_in exo_out"
#endif
#if defined (EMERGE)
#define EXECUTABLE emerge
#define EXODUSII2
#define IOSET2
#define INFILE FOR011
#define TAPE FOR012
#define OUTFILE FOR013
#define FORMAT3
set USAGE = \
"Usage: emerge [-help] [-options option] [--] exo_in tape_in exo_out"
#endif
#if defined(GEN3D)
#define EXECUTABLE gen3d2
#define EXOII_NAME gen3d2
#define EXODUSII2
#define IOSET1
#define INFILE FOR009
#define OUTFILE FOR010
#define FORMAT3
#define PREPOST
set USAGE = \
"Usage: gen3d [-help] [-options option] [--] exo_in exo_out"
#endif
#if defined(GENSHELL)
#define EXECUTABLE genshell2
#define EXOII_NAME genshell2
#define EXODUSII2
#define IOSET1
#define INFILE FOR009
#define OUTFILE FOR010
#define FORMAT3
#define PREPOST
set USAGE = \
"Usage: genshell [-help] [-options option] [--] exo_in exo_out"
#endif
#if defined(GREPOS)
#define EXECUTABLE grepos2
#define EXOII_NAME grepos2
#define EXODUSII2
#define IOSET1
#define INFILE FOR009
#define OUTFILE FOR010
#define FORMAT3
#define SUBOPTION
#define PREPOST
set USAGE = \
"Usage: grepos [-help] [-options option] [--] exo_in exo_out"
#endif
#if defined(GENHXSHL)
#define EXECUTABLE genhxshl
#define EXOI
#define IOSET1
#define INFILE FOR009
#define OUTFILE FOR010
#define FORMAT2
#define SUBOPTION
#define PREPOST
set USAGE = \
"Usage: genhxshl [-help] [-options option] [--] exo_in exo_out"
#endif
#if defined(NUMBERS)
#define EXECUTABLE numbers2
#define EXOII_NAME numbers2
#define EXODUSII2
#define IOSET3
#define INFILE FOR009
#define OUTPUT FOR007
#define FORMAT3
#define PREPOST
set USAGE = \
"Usage: numbers [-help] [-options option] [--] exo_in"
setenv FOR007 "numbers.o"
setenv FOR020 "grope.o"
#endif
#if defined(GROPE)
#define EXECUTABLE grope2
#define EXOII_NAME grope2
#define EXODUSII2
#define IOSET3
#define INFILE FOR011
#define OUTPUT FOR020
#define FORMAT3
#define PREPOST
set USAGE = \
"Usage: grope [-help] [-options option] [--] exo_in"
XCOMM Default for numbers output file
setenv FOR007 "numbers.o"
XCOMM Default for grope output file
setenv FOR020 "grope.o"
#endif
#if defined(EXOTEC2)
#define EXECUTABLE exotec2
#define EXODUSII
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR020
#define FORMAT3
set USAGE = \
"Usage: exotec2 [-help] [-options option] [--] exo_in exo_out"
#endif
#if defined(EXOTXT)
#define EXECUTABLE exotxt2
#define EXOII_NAME exotxt2
#define EXODUSII2
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR020
#define FORMAT3
set USAGE = \
"Usage: exotxt [-help] [-options option] [--] exo_in exo_out"
#endif
#if defined(TXTEXO)
#define EXECUTABLE txtexo
#define EXOI_NAME txtexo
#define EXOI
#define EXOII_NAME txtexo2
#define IOSET1
#define INFILE FOR020
#define OUTFILE FOR012
#define FORMAT8
set USAGE = \
"Usage: txtexo [-help] [-options option] [--] exo_in exo_out"
#endif
#if defined(SEAEXO)
#define EXECUTABLE seaexo
#define EXOI
#define IOSET1
#define INFILE FOR010
#define OUTFILE FOR011
#define FORMAT5
set USAGE = \
"Usage: seaexo [-help] [-options option] [--] sea_in exo_out"
#endif
#if defined(ABAEXO)
#define JOBNAME $infile:r
#define EXECUTABLE abaexo
#define EXOI
#define IOSET5
#define OUTFILE FOR011
#define FORMAT5
set USAGE = \
"Usage: abaexo [-help] [-options option] [--] aba_in[.fil] [exo_out]"
#else
#define JOBNAME /**/
#endif
#if defined(XDREXO)
#define EXECUTABLE xdrexo
#define EXOI
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR012
set USAGE = \
"Usage: xdrexo [-help] [-options option] [--] xdr_in exo_out"
#endif
#if defined(EXOXDR)
#define EXECUTABLE exoxdr
#define EXOI
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR012
#define FORMAT6
set USAGE = \
"Usage: exoxdr [-help] [-options option] [--] exo_in xdr_out"
#endif
#if defined(PATEXO)
#define EXECUTABLE patexo
#define EXOI
#define IOSET1
#define INFILE FOR008
#define OUTFILE FOR009
#define DIM
#define FORMAT5
set USAGE = \
"Usage: patexo -dimension=ndim [-help] [--] pat_in exo_out"
#endif
#if defined(EXOPAT)
#define EXECUTABLE exopat
#define EXOI
#define INFILE FOR010
#define IOSET7
#define FORMAT2
set USAGE = \
"Usage: exopat [-help] [--] exo_in base_name"
#endif
#if defined(EXOCTH)
#define EXECUTABLE exocth
#define EXOI
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR012
#define FORMAT2
set USAGE = \
"Usage: exocth [-help] [-options option] [--] exo_in exo_cth"
#endif
#if defined(EXOTRC)
#define EXECUTABLE exotrc
#define EXOI
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR012
#define FORMAT2
set USAGE = \
"Usage: exotrc [-help] [-options option] [--] exo_in exo_trc"
#endif
#if defined(EX1EX2V2)
#define EXECUTABLE ex1ex2v2
#define EXODUSII2
#define EXOI
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR020
#define PRECISION
#define FORMAT4
set USAGE = \
"Usage: ex1ex2v2 [-help] [-options option] [--] exoI_in exoII_out"
#endif
#if defined(EX2EX1V2)
#define EXECUTABLE ex2ex1v2
#define EXODUSII2
#define EXOI
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR020
#define FORMAT4
set USAGE = \
"Usage: ex2ex1v2 [-help] [-options option] [--] exoII_in exoI_out"
#endif
#if defined(EXOMATLAB)
#define EXECUTABLE exomatlab
#define EXODUSII2
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR020
#define FORMAT3
set USAGE = \
"Usage: exomatlab [-help] [-options option] [--] exo2v2_in matlab_out"
#endif
#if defined(EXOGEN)
#define EXECUTABLE exogen
#define EXOI
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR012
#define FORMAT2
setenv EXT01 "Unset"
setenv EXT02 "Unset"
set USAGE = \
"Usage: exogen [-help] [-options option] [--] exo_in exo_out"
#endif
#if defined(EXOSYM)
#define EXECUTABLE exosym
#define EXOI
#define TRANSFORM
#define MATERIAL
#define IOSET1
#define INFILE FOR011
#define OUTFILE FOR012
#define FORMAT2
setenv EXT01 "Unset"
setenv EXT02 1
set USAGE = \
"Usage: exosym [-help] -transform=8chars [-Material=mat_off] [--] exo_in exo_out"
#endif
#if defined(SPHGEN2D)
#define EXECUTABLE sphgen2d
#define EXOI
#define IOSET1
#define INFILE FOR008
#define OUTFILE FOR009
#define FORMAT2
set USAGE = \
"Usage: sphgen2d [-help] [-options option] [--] exo_in sph_out"
#endif
#if defined(SPHGEN3D)
#define EXECUTABLE sphgen3d
#define EXOI
#define EXOI_NAME sphgen3d
#define EXODUSII2
#define EXOII_NAME sphgen3d2
#define IOSET1
#define INFILE FOR008
#define OUTFILE FOR009
#define FORMAT2
set USAGE = \
"Usage: sphgen3d [-help] [-options option] [--] exo_in sph_out"
#endif

#if defined(EXOFSQ)
#define EXECUTABLE exofsq
#define IOSET1
#define EXODUSII2
#define INFILE FOR011
#define OUTFILE FOR020
#define FORMAT3
#define TIME 
#define STEP
set USAGE = \
"Usage: exofsq [-help] [-options option] [--] exo_in fsq_out"
#endif


