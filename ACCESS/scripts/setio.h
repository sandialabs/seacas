/*
 * setio.h
 * $Id: setio.h,v 1.1 2008/10/31 05:19:57 gdsjaar Exp $
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
  IOSET1 - This program expects an INFILE and OUTFILE define
  IOSET2 - This program expects an INFILE, TAPE, and OUTFILE
           (So far emerge is the only program utilizing this feature)
  IOSET3 - This program expects an INFILE.  A OUTFILE may be defined.
           The OUTFILE has a default settings in the defines for the
           program.  This may be overridden by the -output command
           line option.
  IOSET4 - 
  IOSET5 - 
*/
#if defined(IOSET1)
XCOMM This program expects an INFILE and OUTFILE defined
XCOMM argv now points to "--", assign first option to input file
shift argv
if ($#argv <= 0) then
	echo "No input file specified"
	set errflg = 1
	goto usage
else
	setenv INFILE $argv[1]
	test -e ${INFILE}
	if ($status != 0) then
		echo ${INFILE}" does not exist"
		set errflg = 1
		goto usage
	endif
endif
XCOMM output file
shift argv
if ($#argv <= 0) then
XCOMM  argv <= 0
	set errflg = 1
	echo "No output file specified"
	goto usage
else
XCOMM argv > 0
	setenv OUTFILE $argv[1]
	test -e ${OUTFILE}
	if ($status == 0) then
		echo ${OUTFILE}" will be overwritten"
		rm ${OUTFILE}
	endif
endif
#endif /* IOSET1 */

#if defined(IOSET2)
XCOMM This program expects an INFILE, TAPE, and OUTFILE
XCOMM argv now points to "--", assign first option to input file
shift argv
if ($#argv <= 0) then
	echo "No input file specified"
	set errflg = 1
	goto usage
else
	setenv INFILE $argv[1]
	test -e ${INFILE}
	if ($status != 0) then
		echo ${INFILE}" does not exist"
		set errflg = 1
		goto usage
	endif
endif
XCOMM input tape file
shift argv
if ($#argv <= 0) then
	echo "No input file specified"
	set errflg = 1
	goto usage
else
	setenv TAPE $argv[1]
	test -e ${TAPE}
	if ($status != 0) then
		echo ${TAPE}" does not exist"
		set errflg = 1
		goto usage
	endif
endif
XCOMM output file
shift argv
if ($#argv <= 0) then
XCOMM  argv <= 0
	set errflg = 1
	echo "No output file specified"
	goto usage
else
XCOMM argv > 0
	setenv OUTFILE $argv[1]
	test -e ${OUTFILE}
	if ($status == 0) then
		echo ${OUTFILE}" will be overwritten"
		rm ${OUTFILE}
	endif
endif
#endif /* IOSET2 */

#if defined(IOSET3)
XCOMM This program expects an INFILE.  A OUTFILE may be defined.
XCOMM The OUTFILE has a default settings in the defines for the
XCOMM program.  This may be overridden by the -output command line option.
XCOMM argv now points to "--", assign first option to input file
shift argv
if ($#argv <= 0) then
	echo "No input file specified"
	set errflg = 1
	goto usage
else
	setenv INFILE $argv[1]
	test -e ${INFILE}
	if ($status != 0) then
		echo ${INFILE}" does not exist"
		goto usage
	endif
endif
shift argv
if ($#argv > 0) then
        setenv OUTPUT $argv[1]
	test -e ${OUTPUT}
	if ($status == 0) then
                echo ${OUTPUT}" will be overwritten"
                rm -f ${OUTPUT}
        endif
else        
	test -e ${OUTPUT}
	if ($status == 0) then
		echo ${OUTPUT}" will be overwritten"
		rm -f ${OUTPUT}
	endif
endif
#endif /* IOSET3 */

#if defined(IOSET4)
XCOMM argv now points to "--", assign first option to input file
shift argv
if ($#argv <= 0) then
	echo "No input file specified"
	set errflg = 1
	goto usage
else
	setenv INFILE $argv[1]
	test -e ${INFILE}
	if ($status != 0) then
		echo ${INFILE}" does not exist"
		set errflg = 1
		goto usage
	endif
endif
shift argv
if ($#argv <= 0) then
XCOMM  argv <= 0
	set infile = ${FOR010}
	setenv EXT01 $infile:r
	echo "Base filename used for output files = ${EXT01}"
else
XCOMM argv > 0
	setenv EXT01 $argv[1]
	echo "Base filename used for output files = ${EXT01}"
endif
XCOMM Assign the scratch files (if any). Remove the old output files (if any)
if (-e ${EXT01}.msg) then
	echo "File ${EXT01}.msg will be overwritten"
	rm -f ${EXT01}.msg
endif
if (-e ${EXT01}.neu) then
	echo "File ${EXT01}.neu will be overwritten"
	rm -f ${EXT01}.neu
endif
if (-e ${EXT01}.nod.1) then
	echo "Files ${EXT01}.nod.* will be overwritten (if they exist)"
	rm -f ${EXT01}.nod.* >& /dev/null
endif
if (-e ${EXT01}.els.1) then
	echo "Files ${EXT01}.els.* will be overwritten (if they exist)"
	rm -f ${EXT01}.els.* >& /dev/null
endif
#endif /* IOSET4 */

#if defined(IOSET5)
shift argv
if ($#argv <= 0) then
	echo "No input file specified"
	set errflg = 1
	goto usage
else
	set infile = $argv[1]
	if ($infile:r != ${infile}) then
XCOMM We have a file with an extension, it must be .fil to be valid
		if ($infile:e != "fil") then
			echo "ERROR - input file must end with .fil"
		endif
	else
XCOMM	We have a file with no extension, add on the .fil
		set infile = $argv[1].fil
	endif
	if (!(-e ${infile})) then
		echo "File ${infile} does not exist"
		set errflg = 1
		goto usage
	endif
endif
if (${errflg} != 0) then
	goto usage
endif
shift argv
if ($#argv <= 0) then
XCOMM argv <= 0
	setenv OUTFILE $infile:r.e
	test -e ${OUTFILE}
	if ($status == 0) then
		echo ${OUTFILE}" will be overwritten"
		rm ${OUTFILE}
	endif
else
XCOMM argv > 0
	setenv OUTFILE $argv[1]
	test -e ${OUTFILE}
	if ($status == 0) then
		echo ${OUTFILE}" will be overwritten"
		rm ${OUTFILE}
	endif
endif

#endif /* IOSET5 */

#if defined(IOSET6)
XCOMM This program expects an INFILE and OUTFILE defined
XCOMM argv now points to "--", assign first option to input file
shift argv
if ($#argv == 0) then
        echo ""
	echo "ERROR: Initial input file and combined output file must be specified."
        echo "ERROR: If you want the exodus I version, use 'conex1'"
	set errflg = 1
	goto usage
else
	set codename = EXOII_NAME
	if ($#argv <= 0) then
		echo "ERROR: No input file specified"
		set errflg = 1
		goto usage
	else
		setenv INFILE $argv[1]
	        test -e ${INFILE}
                if ($status != 0) then
			echo ${INFILE}" does not exist"
			set errflg = 1
			goto usage
		endif
	endif
XCOMM output file
	shift argv
	if ($#argv <= 0) then
XCOMM  argv <= 0
		set errflg = 1
		echo "ERROR: No output file specified"
		goto usage
	else
XCOMM argv > 0
		setenv OUTFILE $argv[1]
	        test -e ${OUTFILE}
                if ($status == 0) then
			echo ${OUTFILE}" will be overwritten"
			rm ${OUTFILE}
		endif
	endif
endif
#endif /* IOSET6 */

#if defined(IOSET7)
XCOMM This program expects an INFILE defined
XCOMM argv now points to "--", assign first option to input file
shift argv
if ($#argv <= 0) then
        echo "No input file specified"
        set errflg = 1
        goto usage
else
        setenv INFILE $argv[1]
	test -e ${INFILE}
	if ($status != 0) then
                echo ${INFILE}" does not exist"
                set errflg = 1
                goto usage
        endif
endif
XCOMM output file
shift argv
if ($#argv <= 0) then
XCOMM argv <= 0
        set errflg = 1
        echo "No output file specified"
        goto usage
else
XCOMM argv > 0
        setenv EXT01 $argv[1]
endif
#endif /* IOSET7 */

#if defined(IOSET8)
XCOMM This program expects two input files and one optional output file
XCOMM argv now points to "--", assign first option to input file
shift argv
if ($#argv <= 0) then
	echo "No input file specified"
	set errflg = 1
	goto usage
else
	setenv INFILE1 $argv[1]
	test -e ${INFILE1}
	if ($status != 0) then
		echo ${INFILE1}" does not exist"
		set errflg = 1
		goto usage
	else
		echo "Input file 1: "${INFILE1}
	endif
endif
shift argv
if ($#argv <= 0) then
	echo "No input file specified"
	set errflg = 1
	goto usage
else
	setenv INFILE2 $argv[1]
	test -e ${INFILE2}
	if ($status != 0) then
		echo ${INFILE2}" does not exist"
		set errflg = 1
		goto usage
	else
		echo "Input file 2: "${INFILE2}
	endif
endif
XCOMM output file
shift argv
if ($#argv <= 0) then
XCOMM  argv <= 0
	setenv OUTFILE ''
	echo "No output file specified"
else
XCOMM argv > 0
	setenv OUTFILE $argv[1]
	echo "Output file:  "${OUTFILE}
	test -e ${OUTFILE}
	if ($status == 0) then
		echo ${OUTFILE}" will be overwritten"
		rm ${OUTFILE}
	endif
endif
#endif /* IOSET8 */

#if defined(EXOGEN) || defined(EXORAY)
if (${EXT01} == "Unset") then
	echo "Either the -time or the -step option must be specified"
	goto usage
else
	echo " "
	echo "*** Deform mesh at ${EXT01} ${EXT02}"
	echo " "
endif
#endif /* EXOGEN || EXORAY */

#if defined(EXOSYM)
if (${EXT01} == "Unset") then
	echo "The -transform option must be specified"
	goto usage
endif
#endif /* EXOSYM */

#if defined(PATEXO) 
XCOMM Assign the scratch files (if any)
if (${ndim} == "unset") then
	echo "Model spatial dimension must be specified. e.g. -dimension=2"
	goto usage
else
	setenv EXT01 ${ndim}
endif
setenv FOR011 ftn11
setenv FOR012 ftn12
setenv FOR013 ftn13
setenv FOR014 ftn14
setenv FOR015 ftn15
#endif /* PATEXO */

#endif /* TRANSLATOR */

/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(GRAPHICS)
/*
  IOSET1 - Blot input
  IOSET2 - Fastq input
  IOSET3 - Graph input
*/

XCOMM argv now points to "--", assign first option to input file
XCOMM input file
shift argv

#if defined(IOSET1)
if ($#argv <= 0) then
	echo "No plot file specified"
	set errflg = 1
	goto usage
else
	setenv INFILE $argv[1]
	test -e ${INFILE}
	if ($status != 0) then
		echo ${INFILE}" does not exist"
		set errflg = 1
		goto usage
	endif
endif
XCOMM Check for device specified, use ${device} if none specified
if ($#argv > 1) then
XCOMM assign next option, if any, to device
	shift argv
	set device = $argv[1]
endif
setenv FOR090 ${TEMP}/Blot.90.$$
if (${no_exe} != "true") then
	if (${filename} == "") then
		set filename = ${INFILE}
XCOMM Remove the trailing suffix of filename
		set filename = $filename:r
		setenv FOR020 ${filename}.xmgr
		if (-e ${FOR020}) rm -f ${FOR020}
		setenv FOR021 ${filename}.lis
		if (-e ${FOR021}) rm -f ${FOR021}
		setenv FOR022 ${filename}.grf
		if (-e ${FOR022}) rm -f ${FOR022}
		setenv FOR023 ${filename}.csv
		if (-e ${FOR023}) rm -f ${FOR023}
		setenv FOR099 ${filename}.blot.log
		if (-e ${FOR099}) rm -f ${FOR099}
		setenv FOR095 ${filename}.ray
	endif
	cp ${ACCESS}/bin/SSRFNT SSRFNT
	cp ${ACCESS}/bin/STKFNT STKFNT
	cp ${ACCESS}/bin/ROMFNT ROMFNT
	if (${device} == "dual") then
		setenv EXT01 "x11"
		setenv EXT02 "met"
		setenv DUAL_FILENAME ${filename}.met
	else
  	    if (${device} == "xcps") then
		setenv EXT01 "x11"
		setenv EXT02 "cps"
		setenv DUAL_FILENAME ${filename}.cps
	    else
		setenv EXT01 ${device}
            endif
	endif
endif

#endif /* IOSET1 */

#if defined(IOSET2)
XCOMM  assign next option, if any, to device
if ($#argv > 0) then
        test -e $argv[1]
        if ($status != 0) then
		echo "File $argv[1] does not exist"
		goto usage
	endif
	if (${aprepro} == "true") then
		set aprepro="file"
		setenv INPUT ${TEMP}/fastq.$$
		${ACCESS}/bin/aprepro ${apr_opt} $argv[1] ${INPUT}
	else
		setenv INPUT $argv[1]
	endif
else
	if(${aprepro} == "true") then
		set precommand = "${ACCESS}/bin/aprepro --interactive --exit_on ${apr_opt}"
		set pipe_input = "true"
	endif
endif

XCOMM Check for device specified, use ${device} if none specified
if ($#argv > 1) then
XCOMM assign next option, if any, to device
	shift argv
	set device = $argv[1]
endif
if (${no_exe} != "true") then
	cp ${ACCESS}/bin/SSRFNT SSRFNT
	cp ${ACCESS}/bin/STKFNT STKFNT
	cp ${ACCESS}/bin/ROMFNT ROMFNT
	if (${device} == "dual") then
		setenv EXT01 "x11"
		setenv EXT02 "met"
		setenv DUAL_FILENAME ${filename}.met
	else
  	    if (${device} == "xcps") then
		setenv EXT01 "x11"
		setenv EXT02 "cps"
		setenv DUAL_FILENAME ${filename}.cps
	    else
		setenv EXT01 ${device}
            endif
	endif
endif
#endif /* IOSET2 */

#if defined(IOSET3)
XCOMM Check for device specified, use ${device} if none specified
XCOMM  assign first option, if any, to device
if ($#argv > 0) then
	set device = $argv[1]
endif
if (${no_exe} != "true") then
	cp ${ACCESS}/bin/SSRFNT SSRFNT
	cp ${ACCESS}/bin/STKFNT STKFNT
	cp ${ACCESS}/bin/ROMFNT ROMFNT
	if (${device} == "dual") then
		setenv EXT01 "x11"
		setenv EXT02 "met"
		setenv DUAL_FILENAME ${filename}.met
	else
  	    if (${device} == "xcps") then
		setenv EXT01 "x11"
		setenv EXT02 "cps"
		setenv DUAL_FILENAME ${filename}.cps
	    else
		setenv EXT01 ${device}
            endif
	endif
endif
#endif /* IOSET3 */

#endif /* GRAPHICS */


/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(ANALYSIS)
/*
 * Always check for the FOR005 and FOR006 file
 * If running in parallel, check for the CONFIGFILE
 */

#if defined(ISPARALLEL)
XCOMM Running in parallel
if (-e ${CONFIGFILE} && ${serial} == "false") then
	source ${CONFIGFILE}
else if ( ${serial} == "false" ) then
	echo "Configuration file "${CONFIGFILE}" is not found"
endif
#endif

XCOMM Check *.i and *.o files:

#if defined(PRONTO3D) || defined(XPRONTO3D) || \
  defined(JAS3D) || defined(XJAS3D) || defined(PRONTO3D94)
XCOMM Unit 5 - input file, must exist
if (-e ${FOR005}) then
	echo "Input File                  = "${FOR005}
else
	echo "Input File                  = "${FOR005}" does not exist"
	set errflg = 1
endif

if (${aprepro} == "true") then
	set apr_inp = ${FOR005}
	setenv FOR005 ${TEMP}/aprepro.$$
	setenv FOR015 ${TEMP}/aprepro.$$
	${ACCESS}/bin/aprepro ${apr_opt} ${apr_inp} ${FOR005}
endif

XCOMM Unit 6 - output file
if (-e ${FOR006}) then
	@ i = 1
	while ( -e ${FOR006}.$i)
	  @ i++
	end
	mv ${FOR006} ${FOR006}.$i
	echo "Output File                 = "${FOR006}" (previous output file"
	echo "                              moved to "${FOR006}"."$i")"
else
	echo "Output File                 = "${FOR006}
endif
#else
#if defined(MAPVAR) || defined(MAPVARKD)
XCOMM Unit 7 - output file
if (-e ${FOR007}) then
        echo "Output File                 = "${FOR007}" will be overwritten"
        rm ${FOR007}
else
        echo "Output File                 = "${FOR007}
endif
#else
XCOMM Unit 5 - input file, must exist
if (-e ${FOR005}) then
	echo "Input File                  = "${FOR005}
else
	echo "Input File                  = "${FOR005}" does not exist"
	set errflg = 1
endif

if (${aprepro} == "true") then
	set apr_inp = ${FOR005}
	setenv FOR005 ${TEMP}/aprepro.$$
	setenv FOR015 ${TEMP}/aprepro.$$
	${ACCESS}/bin/aprepro ${apr_opt} ${apr_inp} ${FOR005}
endif

XCOMM Unit 6 - output file
if (-e ${FOR006}) then
	echo "Output File                 = "${FOR006}" will be overwritten"
	rm ${FOR006}
else
	echo "Output File                 = "${FOR006}
endif
#endif
#endif

#if defined(SUMMARY)
XCOMM Unit 8 - summary file
if (-e ${FOR008}) then
	@ i = 1
	while ( -e ${FOR008}.$i)
	  @ i++
	end
	mv ${FOR008} ${FOR008}.$i
	echo "Summary File                = "${FOR008}" (previous summary file"
	echo "                              moved to "${FOR008}"."$i")"
else
	echo "Summary File                = "${FOR008}
endif
#endif

if (${serial} == "true") then
#if defined(IOSET1)

XCOMM mesh file, must exist
#if defined(MESH)
if (-e ${MESH}) then
	echo "Mesh File                   = "${MESH}
else
	echo "Mesh File                   = "${MESH}" does not exist"
	set errflg = 1
endif
#endif

XCOMM plot file, backup if exists
#if defined(PLOT)
test -e ${PLOT}
if ($status == 0) then
#if defined(PRONTO3D) || defined(XPRONTO3D) || defined(JAS3D) || defined(XJAS3D) || defined(PRONTO3D94)
	echo "Plot File                   = "${PLOT}" will be appended or"
	echo "                              overwritten"
#else
	echo "Plot File                   = "${PLOT}" will be overwritten"
	rm ${PLOT}
#endif
else
	echo "Plot File                   = "${PLOT}
endif
#endif

#else

/* The case below is for both the merlin2 and the mapvar script */ 
XCOMM Both mesh and plot file must exist
XCOMM Mesh file
#if defined(MERLIN2) || defined(MAPVAR) || defined(MAPVARKD)
test -e ${EPLOT}
if ($status == 0) then
        echo "Mesh A + Sol                = "${EPLOT}
else
        echo "Mesh A - (+ Sol)            = "${EPLOT}" does not exist"
        set errflg = 1
endif
test -e ${EMESH}
if ($status == 0) then
	echo "Mesh B                      = "${EMESH}
else
	echo "Mesh B                      = "${EMESH}" does not exist"
	set errflg = 1
endif
#endif

#if defined(MESH)
test -e ${MESH}
if ($status == 0) then
	echo "Mesh File                   = "${MESH}
else
	echo "Mesh File                   = "${MESH}" does not exist"
	set errflg = 1
endif
#endif

#if defined(PLOT)
XCOMM Plot file
test -e ${PLOT}
if ($status == 0) then
	echo "Plot File                   = "${PLOT}
else
	echo "Plot File                   = "${PLOT}" does not exist"
	set errflg = 1
endif
#endif

#if defined(INTERPOLATED)
XCOMM interpolated exodus output file, overwritten
test -e ${INTERPOLATED}
if ($status == 0) then
	echo "Interpolated/Exodus         = "${INTERPOLATED}" will be overwritten"
	rm ${INTERPOLATED}
else
	echo "Interpolated/Exodus         = "${INTERPOLATED}
endif
#endif

#endif /* if IOSET1 */

#if defined(HISTORY)
XCOMM  history file, overwritten
test -e ${HISTORY}
if ($status == 0) then
#if defined(PRONTO3D) || defined(XPRONTO3D) || \
    defined(JAS3D) || defined(XJAS3D) || defined(PRONTO3D94)
	echo "History File                = "${HISTORY}" will be appended or"
	echo "                              overwritten"
#else
	echo "History File                = "${HISTORY}" will be overwritten"
	rm ${HISTORY}
#endif
else
	echo "History File                = "${HISTORY}
endif
#endif

#if defined(DIST_LOAD)
XCOMM distribution load
test -e ${DIST_LOAD}
if ($status == 0) then
	echo "Distribution Load File      = "${DIST_LOAD}
else
	echo "Distribution Load File      = "
endif
#endif

#if defined(EXTERNAL)
XCOMM External file - input file
test -e ${EXTERNAL}
if ($status == 0) then
	echo "External File               = "${EXTERNAL}
else
	echo "External File               = "
endif
#endif

#if defined(THERMAL)
XCOMM thermal data unit input file
test -e ${THERMAL}
if ($status == 0) then
	echo "Thermal File                = "${THERMAL}
else
	echo "Thermal File                = "
endif
#endif

#if defined(RSOUT)
XCOMM restart output file
test -e ${RSOUT}
if ($status == 0) then
#if defined(PRONTO3D) || defined(XPRONTO3D) || \
    defined(JAS3D) || defined(XJAS3D) || defined(PRONTO3D94)
	echo "Restart (*.rsout)           = "${RSOUT}" will be appended or"
	echo "                              overwritten"
#else
	echo "Restart (*.rsout)           = "${RSOUT}" will be overwritten"
	rm ${RSOUT}
#endif
else
	echo "Restart (*.rsout)           = "${RSOUT}
endif
#endif

#if defined(RSIN)
XCOMM restart input file
test -e ${RSIN}
if ($status == 0) then
	echo "Restart (*.rsin)            = "${RSIN}
else
	echo "Restart (*.rsin)            = "
endif
#endif

#if defined(CONSTRAINT)
XCOMM constraint input file
test -e ${CONSTRAINT}
if ($status == 0) then
	echo "Constraint Mesh (*.gc)      = "${CONSTRAINT}
else
	echo "Constraint Mesh (*.gc)      = "
endif
#endif

#if defined(VIEWFACTOR)
XCOMM  Viewfactor, overwritten
test -e ${VIEWFACTOR}
if ($status == 0) then
	echo "Viewfactor File             = "${VIEWFACTOR}
else
	echo "Viewfactor File             = "
endif
#endif

#if defined(USR)
XCOMM  User-defined plot file, overwritten
test -e ${USR}
if ($status == 0) then
	echo "User Plot File (*.usr)      = "${USR}" will be overwritten"
	rm ${USR}
else
	echo "User Plot File              = "${USR}
endif
#endif

endif
if (${errflg} != 0) goto usage

#if defined(COYOTE)
XCOMM Assign Coyote2 scratch files to temp area
setenv FOR011 ${TEMP}/coyote11.$$

XCOMM remove these temporary files if they exist 
if (-e ${FOR011}) rm -f ${FOR011}
#endif /* coyote2 */

#if defined(IOSET2)
#if defined(MERLIN2)
setenv FOR010 Merlin10.$$
setenv FOR011 Merlin11.$$
#endif
#endif /* ioset2  */


#endif /* ANALYSIS */

