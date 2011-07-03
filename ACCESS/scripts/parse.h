/*
 * parse.h - This .h file includes the while loop that is
 *           used to parse the users command line
 * $Id: parse.h,v 1.1 2008/10/31 05:19:57 gdsjaar Exp $
*/

/*
  The design of the file will be the following
  
  #if defined(TRANSLATOR)
  ...
  #endif

  #if defined(GRAPHICS)
  ...
  #endif

  #if define(ANALYSIS)
  ...
  #endif
*/

#if defined(TRANSLATOR)
/*
   Variables used in parsing the command line input
	OUTPUT     - defined if -output is a command line options
	SUBOPTION  - defined if can link user-supplied subroutine
	TRANSFORM  - defined if -transform is a command line option
	MATERIAL   - defined if -material is a command line option
	DIM        - defined if -dimension is a command line option
	TIME       - defined if -time is a command line option
	STEP       - defined if -step is a command line option
*/
XCOMM Process options for this code
while ("$argv[1]" != "--")
	switch ($argv[1])
	case -help:
		goto usage
		breaksw
	case -noexecute:
		set no_exe = "true"
		breaksw
	case -debug:
		set rundebug = "true"
		switch ($argv[2])
                case -*:
			set precommand = "${debugcmd}"
                        breaksw
                default:
			set precommand = "$argv[2]"
                        shift argv
                        breaksw
                endsw
		echo "Debugging with "${debugcmd}
		breaksw
	case -executable:
		shift argv
		set local_exe = $argv[1]
		if (-e ${local_exe} && -x ${local_exe}) then
			echo "Executable: ${local_exe}"
		else
			echo "ERROR: ${local_exe} does not exist,"
			echo "       or is not executable"
			set errflg = 1
		endif
		breaksw
#if defined(FORMAT4)
	case -force:
		set force="true"
		breaksw
#endif
#if defined(PRECISION)
	case -double:
		set precision=8
		setenv EXT05 8
		breaksw
        case -single:
		set precision=4
		setenv EXT05 4
		breaksw
#endif
#if defined(OUTPUT)
	case -output:
		shift argv
		setenv OUTPUT $argv[1]
		if (-e ${OUTPUT}) then
			echo ${OUTPUT}" already exists"
			echo ${OUTPUT}" will be overwritten"
			rm ${OUTPUT}
		endif
		breaksw
#endif
#if defined(OUTFILE)
	case -output:
		shift argv
		setenv OUTFILE $argv[1]
		if (-e ${OUTFILE}) then
			echo ${OUTFILE}" already exists"
			echo ${OUTFILE}" will be overwritten"
			rm ${OUTFILE}
		endif
		breaksw
#endif
#if defined(SUBOPTION)
	case -subroutine:
		shift argv
		set subroutine = $argv[1]
		set inc = ${ACCESSSRC}/translate/${codename}
		breaksw
	case -Include:
		shift argv
		set inc = $argv[1]
	        breaksw
	case -Library_path:
		shift argv
		set local_library_path = $argv[1]
	        breaksw
#endif
#if defined(TRANSFORM)
	case -transform:
		shift argv
		setenv EXT01 $argv[1]
		breaksw
#endif
#if defined(MATERIAL)
	case -Material:
		shift argv
		setenv EXT02 $argv[1]
		breaksw
#endif
#if defined(DIM)
	case -dimension
		shift argv
		set ndim = $argv[1]
		breaksw
#endif
#if defined(TIME)
	case -time:
		shift argv
		setenv EXT01 "time"
		setenv EXT02 $argv[1]
		breaksw
#endif
#if defined(STEP)
	case -step:
		shift argv
		setenv EXT01 "step"
		setenv EXT02 $argv[1]
		breaksw
#endif
	case -aprepro:
		set precommand="${ACCESS}/bin/aprepro --interactive --exit_on"
		set pipe_input = "true"
		set aprepro = "true"
		set temp1 = $argv[2]
XCOMM Determine if next chars are aprepro options or EXECUTABLE options
XCOMM aprepro options are of the form: "(aprepro options)"
		switch ($temp1)
		case (*):
			shift argv
XCOMM Substitute spaces for the character
			set apr_opt=`echo $temp1 |sed -e 's/[(),]/\ /g'`
			echo "aprepro options = "${apr_opt}
			breaksw
		case "(*)":
			shift argv
XCOMM Substitute spaces for the character
			set apr_opt=`echo $temp1 |sed -e 's/[(),]/\ /g'`
			echo "aprepro options = "${apr_opt}
			breaksw
		default:
			breaksw
		endsw
		breaksw
	default:
		echo "ERROR: unrecognized option $argv[1]"
		set errflg=1
		breaksw
	endsw
XCOMM Get next commmand line argument
	shift argv
end
XCOMM End processing command line arguments - End While Loop

#endif /* TRANSLATOR */


/***************************************************************/
/***************************************************************/
/***************************************************************/


#if defined(GRAPHICS)
/*
   Variables used in parsing the command line input
   MESH  - defined if -mesh is a command line option (fastq)
   INPUT - defined if -input is a command line option
*/
XCOMM Now process options for this code
while ("$argv[1]" != "--")
	switch ($argv[1])
#if defined(MESH)
        case -mesh:
		shift argv
		setenv MESH $argv[1]
		setenv EXT03 BAT
		if (-e ${MESH}) then
			echo ${MESH}" will be overwritten"
			rm ${MESH}
		endif
		set batch = "true"
		setenv EXT03 BAT
		breaksw
#endif
#if defined(INPUT)
	case -input:
		shift argv
		setenv INPUT $argv[1]
		setenv EXT04 YES
		if (!(-e ${INPUT})) then
			echo ${INPUT}" does not exist"
			goto usage
		endif
		breaksw
#endif
	case -device:
		shift argv
		set device = $argv[1]
		if (${device} == "list" || ${device} == "help") then
			goto show_device
		endif
		breaksw
#if defined(HARDCOPY)
	case -hardcopy:
		shift argv
		set filename = $argv[1]
		breaksw
#endif
#if defined(EXODUS_SWITCH)
	case -exodus:
	        shift argv
		if ($argv[1] == "1" || $argv[1] == "i" || $argv[1] == "I") then
		        setenv EXT04 GENESIS
	        endif   
		if ($argv[1] == "2" || $argv[1] == "ii" || $argv[1] == "II") then
		        setenv EXT04 EXODUSII
		endif   
		breaksw
#endif		
	case -debug:
		set rundebug = "true"
		switch ($argv[2])
                case -*:
			set precommand = "${debugcmd}"
                        breaksw
                default:
			set precommand = "$argv[2]"
                        shift argv
                        breaksw
                endsw
		echo "Debugging with "${debugcmd}
		breaksw
	case -help:
		goto usage
		breaksw
	case -noexecute:
		set no_exe = "true"
		breaksw
	case -executable:
		shift argv
		set local_exe = $argv[1]
		if (-e ${local_exe} && -x ${local_exe}) then
			echo "Executable: ${local_exe}"
		else
			echo "ERROR: ${local_exe} does not exist,"
			echo "       or is not executable"
			set errflg = 1
		endif
		breaksw
	case -aprepro:
		set aprepro = "true"
		set temp1 = $argv[2]
XCOMM Determine if next chars are aprepro options or EXECUTABLE options
XCOMM aprepro options are of the form: "(aprepro options)"
		switch ($temp1)
		case (*):
			shift argv
XCOMM Substitute spaces for the characters: '(' ')' ','
			set apr_opt=`echo $temp1 |sed -e 's/[(),]/\ /g'`
			breaksw
		default:
			breaksw
		endsw
		breaksw
	default:
		echo "ERROR: unrecognized option $argv[1]"
		set errflg=1
		breaksw
	endsw
	shift argv
end

#endif /* GRAPHICS */

/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(ANALYSIS)
XCOMM Process options for this code

#include "arguments.h"

XCOMM Now assign all other files that may have been explicitly specified
while ("$argv[1]" != "--")
	switch ($argv[1])
	case -help:
		goto usage
		breaksw
	case -input:
		shift argv
		setenv FOR005 $argv[1]
		setenv FOR015 $argv[1]
		breaksw
	case -output:
		shift argv
#if defined(MAPVAR) || defined (MERLIN2) || defined(MAPVARKD)
		setenv FOR007 $argv[1]
#else
		setenv FOR006 $argv[1]
#endif
		setenv FOR016 $argv[1]
		breaksw
#if defined(SUMMARY)
	case -summary:
		shift argv
		setenv SUMMARY $argv[1]
		breaksw
#endif
#if defined(ISPARALLEL)
	case -config:
		shift argv
		setenv CONFIGFILE $argv[1]
		set serial = "false"
		set config = "true"
		breaksw
	case -parallel:
#if _PARALLEL
		setenv MESH $temp[1]'.par'
#endif
		set serial = "false"
#if defined(SUN) || defined(DEC) || (defined(linux) && _NEEDS_HOSTFILE == 1)
		switch ($argv[2])
		case -*: 
			set user_hf = ""
			breaksw
		default: 
			set user_hf = $argv[2]
			shift argv
			breaksw
		endsw
		set basename_hf = $temp[1]'.hosts'
#endif
		breaksw
#endif
#if defined(VIEWFACTOR)
	case -viewfactor:
		shift argv
		setenv VIEWFACTOR $argv[1]
		breaksw
#endif
#if defined(USR)
	case -userplot:
		shift argv
		setenv USR $argv[1]
		breaksw
#endif
#if defined(DIST_LOAD)
	case -distributed:
		shift argv
		setenv DIST_LOAD $argv[1]
		breaksw
#endif
#if defined(EXTERNAL)
	case -external:
		shift argv
		setenv EXTERNAL $argv[1]
		breaksw
#endif
#if defined(MESH)
	case -mesh:
		shift argv
		setenv MESH $argv[1] 
		breaksw
#endif
#if defined(EMESH)
	case -mesh:
		shift argv
		setenv EMESH $argv[1] 
		breaksw
#endif
#if defined(EPLOT)
	case -plot:
		shift argv
		setenv EPLOT $argv[1]
		breaksw
#endif
#if defined(PLOT)
	case -plot:
		shift argv
		setenv PLOT $argv[1]
#if defined(HISTORY)
		setenv HISTORY $argv[1]'h'
#endif
		breaksw
#endif
# if defined(CONSTRAINT)
	case -constraint:
		shift argv
		setenv CONSTRAINT $argv[1]
		breaksw
#endif
#if defined(RSOUT)
	case -rsout:
	case -dump:
		shift argv
		setenv RSOUT $argv[1]
		breaksw
#endif
#if defined(RSIN)
	case -rsin:
	case -restart:
		shift argv
		setenv RSIN $argv[1]
		breaksw
#endif
#if defined(THERMAL)
	case -thermal:
		shift argv
		setenv THERMAL $argv[1]
		breaksw
#endif
#if defined(INTERPOLATED)
	case -interpolated:
		shift argv
		setenv INTERPOLATED $argv[1]
		breaksw
#endif
	case -subroutine:
		shift argv
		set subroutine = $argv[1]
		breaksw
	case -noexecute:
		set no_exe = "true"
		breaksw
	case -executable:
		shift argv
		set local_exe = $argv[1]
		breaksw
	case -aprepro:
		set aprepro = "true"
		set temp1 = $argv[2]
XCOMM Determine if next chars are aprepro options or EXECUTABLE options.
XCOMM aprepro options are of the form: "(aprepro options)"
		switch ($temp1) 
		case "(*)":
			shift argv
XCOMM Substitute spaces for the characters: '(' ')' ','
			set apr_opt=`echo $temp1 |sed -e 's/[(),]/\ /g'`
			breaksw
		default:
			breaksw
		endsw
		breaksw
	case -debug:
		set rundebug = "true"
		set timenicecmd  = ""
		if (${?user_debugcmd}) shift
		breaksw
	case -Include:
		shift argv
		if ($argv[1] == "standard") then
#if defined(JAS3D)
		  set include = ${ACCESSSRC}/analysis/jas3d/COMPILE
#elif defined(XJAS3D)
		  set include = ${ACCESS}/XACCESS/analysis/jas3d/COMPILE
#elif defined(XCODE)
		  set include = ${ACCESS}/XACCESS/analysis/${codename}
#else
		  set include = ${ACCESSSRC}/analysis/${codename}
#endif
		else
			set include = $argv[1]
		endif
		breaksw
	case -Library_path:
		shift argv
		set local_library_path = $argv[1]
	        breaksw
	case -shutdown:
		shift argv
		set shutdown_time = $argv[1]
		set hms = (`echo ${shutdown_time} | sed 's/:/ /g'`)
		set first_char = `expr ${shutdown_time} : '\(.\).*'`
		set last_char = `expr ${shutdown_time} : '.*\(.\)'`
		if (($#hms != 1 && $#hms != 3) || "${first_char}" == ":" || "${last_char}" == ":") then
		   echo "ERROR: invalid time-format used for -shutdown flag" 
		   echo "Valid formats:  seconds  or  hours:minutes:seconds"
		   goto usage
		endif
		if ($#hms == 3) then
		   @ cnt = 1
		   @ secs = 0
		   while (${cnt} <= $#hms)
		      if (${cnt} == 1) @ secs = ${secs} + 3600 * ${hms}[${cnt}]
		      if (${cnt} == 2) @ secs = ${secs} + 60 * ${hms}[${cnt}]
		      if (${cnt} == 3) @ secs = ${secs} + ${hms}[${cnt}]
		      @ cnt++
		   end
		   set shutdown_time = ${secs}
		else
		   set hms = (`echo ${hms} | sed 's/^0*//'`)
		   set shutdown_time = ${hms}
		endif
		breaksw
	default:
		echo "ERROR: unrecognized option $argv[1]"
		set errflg=1
		breaksw
	endsw
	shift argv
end

#endif /* ANALYSIS */

/***************************************************************/
/***************************************************************/
/***************************************************************/

