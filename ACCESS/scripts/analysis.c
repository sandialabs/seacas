XCOMM!/bin/csh -f
XCOMM $Id: analysis.c,v 1.1 2008/10/31 05:19:56 gdsjaar Exp $
XCOMM This script is used to execute the finite element analysis codes
XCOMM for Sandia National Laboratories ACCESS system
XCOMM
XCOMM   Usage: codename [options] [--] [base_file_name]
XCOMM
XCOMM The generated scripts should be installed in $ACCESS/etc,
XCOMM where $ACCESS is the path to the Access installation directories
XCOMM (i.e. bin, etc, config, lib, inc, ...)
XCOMM
XCOMM This script uses the GNU "getopt" and "getopt_long_only" procedures
XCOMM which are compiled into the code "options" ($ACCESS/bin/options)
XCOMM
XCOMM "options" processes the command line passed by this script
XCOMM The syntax for the options passed to "options" is:
XCOMM
XCOMM   1. Permits long (multicharacter) options preceded by a "-" or "+"
XCOMM   2. All options can be abbreviated to the shortest unique string.
XCOMM   3. The argument can be separated from the option by " " or "="
XCOMM   4. Returns nonabbreviated form of option, followed by argument
XCOMM   5. All options are returned first, followed by "--", followed by
XCOMM      all non-option arguments.
XCOMM   6. Returns 0 if no error, 1 if unrecognized option found
XCOMM
XCOMM Following parsing by "options", the script determines whether a
XCOMM "base_file_name" has been entered.  If so, it assigns all files to
XCOMM the "base_file_name" with an appropriate extension.  If not, it
XCOMM assigns all files to "fort.nn" where "nn" is the 1 or 2 digit unit
XCOMM number.
XCOMM
XCOMM Next, all required files are checked for existance. If any files
XCOMM that are written exist, they are removed a warning message that
XCOMM they will be over-written is echoed.
XCOMM
XCOMM Finally, the code is executed.
XCOMM
XCOMM If there is an error at any time during this process, a usage
XCOMM and error message are printed and the script exits.
XCOMM
/*
	Analysis Codes:
	pronto2d  pronto3d  jas3d     santos    santos3d
	jacq3d    jac2d     jac3d     sancho    subway 
	merlin2   coyote
	xpronto2d xpronto3d xjas3d
	
	SCRIPT FORMAT:

	Include defines for each program
		#include "analysis.h"
	Set environment variables
		#include "environment.h"
	Initialize script variables
		#include "variables.h"
	Execute options program to read command line input
	parse command line input
		#include "parse.h"
	Set I/O environment variables
		#include "setio.h"
	Translate files if needed
		#include "format.h"
	Compile suboptions
		#include "subop.h"
	Run program
                #include "run.h"
	Log information
                #include "log.h"
	Delete scratch files
		#include "rmscr.h"
	Usage
		#include "usage.h"
*/

/* Include the definitions for each plot program */
#include "analysis.h"

XCOMM cpp will not substitute inside strings
set codename = EXECUTABLE

/* Define environment variables */
#include "environment.h"

/* define variables for script and suboption */
#include "variables.h"

/* process command line options */
#include "preparse.h"

XCOMM Read command line options
set argv  = (`$ACCESS/bin/options $*`)

XCOMM Set options exit condition
set errflg = $status

XCOMM echo command line input
echo "Input options: $argv"

XCOMM Check errflg - nonzero indicates error in options, print usage and exit:
if ($errflg != 0) then
	goto usage
endif

/* process command line options */
#include "parse.h"

XCOMM Check errflg - if error in options, print usage and exit:
if ($errflg != 0) then
	goto usage
endif

/* Set I/O environment variables */
#include "setio.h"

/* Translate files if needed */
#include "format.h"

/* Compile suboptions */
#include "subop.h"

/* Run program */ 
#include "run.h"

/* Log information */
#include "log.h"

/* Delete scratch files */
#include "rmscr.h"

/* Exit and show usage */ 
#include "usage.h"

