/*
 * format.h - test the file format (exodusI vs exodusII) of codes input
 *            files. If necessary, translate file to correct format.
 * $Id: format.h,v 1.1 2008/10/31 05:19:56 gdsjaar Exp $
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
/* DEFINITIONS FOR TRANSLATOR SCRIPTS
   Variables used in determining the file format needed for execution of
   the program
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
   FORMAT5 - No test for file format
*/

#if defined(FORMAT)
/*
   FORMAT  - set versflg if file is exodusI or exodusIIv2 format.
             generate error if file is of neither format.
*/
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver) then
	${ACCESS}/bin/exo-test-ver ${INFILE} >& /dev/null
	set versflg = ${status}
XCOMM  On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 253) set versflg = -2
	if (${versflg} == 254) set versflg = -2
XCOMM  Format of file based on the exit status of exo-test-ver
XCOMM   3 = netCDF file but not exodusII format
XCOMM   2 = Not a netCDF file - Assume ExodusI format
XCOMM  <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM  -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
		echo ${INFILE}" is an ExodusIIv2 file"
		breaksw
	case  2:
		echo ${INFILE}" is an ExodusI file"
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "No conversions will be done on file "${INFILE}
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif
#endif /* FORMAT */

#if defined(FORMAT1)
/*
   FORMAT1 - code has both an ExodusI and ExodusII version.
             Choose version of code base on file type
*/
XCOMM This code has both an ExodusI and ExodusII version
XCOMM Choose executable based on the file type
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver) then
	${ACCESS}/bin/exo-test-ver ${INFILE} >& /dev/null
	set versflg = ${status}
XCOMM  On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
		echo ${INFILE}" is an ExodusIIv2 file"
		set codename = EXOII_NAME
		breaksw
	case  2:
		echo ${INFILE}" is an ExodusI file"
		set codename = EXECUTABLE
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "No conversions will be done on file "${INFILE}
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif
#endif /* FORMAT1 */

#if defined(FORMAT2)
/*
   FORMAT2 - code will only accept ExodusI format files
             Translate file to ExodusI format
*/
XCOMM This code will only read ExodusI format
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver) then
	${ACCESS}/bin/exo-test-ver ${INFILE} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
XCOMM File format is ExodusII. Translate to ExodusI
		echo ${INFILE}" is an ExodusIIv2 file. Translate to ExodusI"
		setenv exotmp2 ${INFILE}
		setenv INFILE ${TEMP}/ex2ex1v2.$$
		echo "Translating EXODUSII file ${exotmp2} to EXODUSI file"
		${ACCESS}/etc/ex2ex1v2 ${exotmp2} ${INFILE} >& /dev/null
		echo "End of translation using ex2ex1v2"
		echo ""
		breaksw
	case  2:
XCOMM File format is ExodusI. No translation is needed
		echo ${INFILE}" is an ExodusI file. No translation is needed"
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "No conversions will be done on file "${INFILE}
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif

#endif /* FORMAT2 */

#if defined(FORMAT3)
/*
   FORMAT3 - code will only accept ExodusII format files.
             Translate file to ExodusII format
*/
XCOMM This code will only read ExodusII format
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver) then
	${ACCESS}/bin/exo-test-ver ${INFILE} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
XCOMM File format is ExodusII. No translation is needed
		echo ${INFILE}" is an ExodusIIv2 file. No translation is needed"
		breaksw
	case  2:
XCOMM File format is ExodusI. Translate to ExodusII
		echo ${INFILE}" is an ExodusI file. Translate to ExodusII"
		setenv exotmp1 ${INFILE}
		setenv INFILE ${TEMP}/ex1ex2v2.$$
		echo "Translating EXODUSI file ${exotmp1} to EXODUSII file"
		${ACCESS}/etc/ex1ex2v2 ${exotmp1} ${INFILE} >& /dev/null
		echo "End of translation using ex1ex2v2"
		echo ""
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "No conversions will be done on file "${INFILE}
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif

#endif /* FORMAT3 */

#if defined(FORMAT4)
/*
   FORMAT4 - If file is already the targeted format, exit script
             and do not run translators (ex1ex2v2 or ex2ex1v2)
	     (Unless -force was specified; then copy input to output)
*/
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver) then
	${ACCESS}/bin/exo-test-ver ${INFILE} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
		echo ${INFILE}" is an ExodusIIv2 file"
#if defined(EX1EX2V2)
XCOMM File format is ExodusII. No translation is needed
	        if (${force} == "true") then
		   echo "No translation is needed, copying file"
		   cp ${FOR011} ${FOR020}
		   exit
		else
		   echo "No translation is needed"
		   set errflg = 1
		   goto usage
		endif
#endif
		breaksw
	case  2:
		echo ${INFILE}" is an ExodusI file"
#if defined(EX2EX1V2)
XCOMM File format is ExodusI. No translation is needed
		if (${force} == "true") then
		   echo "No translation is needed, copying file"
		   cp ${FOR011} ${FOR020}
		   exit
		else
		   echo "No translation is needed"
		   set errflg = 1
		   goto usage
		endif
#endif
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "No conversions will be done on file "${INFILE}
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif

#endif /* FORMAT4 */

#if defined(FORMAT5)
/*
   No test for file format
*/
#endif /* FORMAT5 */

#if defined(FORMAT6)
/*
   FORMAT6 - Used for the exoxdr translator
*/
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver) then
	${ACCESS}/bin/exo-test-ver ${INFILE} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 253) set versflg = -2
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 255) set versflg = -1
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case  3:
		echo ${INFILE}" is a valid xdr file"
		breaksw
	case -2:
	case -3:
XCOMM File is a platform independent netcdf file. No need to translate.
		echo "*** "${INFILE}" is an ExodusII2 platform independent file"
		echo "*** No translation needed to move file between platforms"
		set errflg = 1
		breaksw
	case -1:
XCOMM File is a platform independent netcdf file. No need to translate.
		echo "*** "${INFILE}" is an ExodusII1 platform independent file"
		echo "*** No translation needed to move file between platforms"
		set errflg = 1
		breaksw
	case  2:
		echo ${INFILE}" is an ExodusI file"
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "No conversions will be done on file "${INFILE}
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif
#endif /* FORMAT6 */

#if defined(FORMAT7)
/*
   FORMAT7 - code will only accept ExodusII format files.
             Can't translate since it reads multiple files that aren't specified
	     at run time.  Exit if exodusI file.
*/
if (${codename} == EXOII_NAME) then
XCOMM This code will only read ExodusII format
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver) then
	${ACCESS}/bin/exo-test-ver ${INFILE} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
XCOMM File format is ExodusII. No translation is needed
		echo ${INFILE}" is an ExodusIIv2 file. No translation is needed"
		breaksw
	case  2:
XCOMM File format is ExodusI. Run conex1 instead of conex
                echo ""
		echo "ERROR: "${INFILE}" is an ExodusI file. Try using conex1"
		set errflg = 1
		breaksw
	default:
		echo "ERROR: Unable to determine type of EXODUS file format."
		echo "ERROR: No conversions will be done on file "${INFILE}
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif
endif /* codename = EXOII_NAME */

#endif /* FORMAT7 */

#if defined(FORMAT8)
/*
   FORMAT8 - grep input (text) file for 'exo2txt'. If found, translate
             to exodusII format using txtexo2; otherwise, use txtexo
	     This is currently used only for txtexo script.
*/
XCOMM This code has both an ExodusI and ExodusII version
XCOMM Choose executable based on the file type
XCOMM Determine format (exodus I text or exodus II text version ) of input file
/* NOTE: The /usr/bin/grep on Solaris does not support the -e option */
head -1 ${INFILE} |grep exo2txt >& /dev/null
set versflg = ${status}
XCOMM Format of file based on the exit status of grep
XCOMM  0 = match found, exodusII text
XCOMM  1 = no match found, exodusI text
	switch(${versflg})
	case 0:
		echo ${INFILE}" is an ExodusIIv2 text file"
		set codename = EXOII_NAME
		breaksw
	case 1:
		echo ${INFILE}" is an ExodusI text file"
		set codename = EXOI_NAME
		breaksw
	default:
		echo "Unable to determine type of input text file format."
		echo "No conversions will be done on file "${INFILE}
		set errflg = 1
		breaksw
	endsw
if (${errflg} != 0) then
	goto usage
endif
#endif /* FORMAT8 */

#if defined(FORMAT9)
/*
   FORMAT9 - code will only accept ExodusII format files.
             Translate file to ExodusII format
*/
XCOMM This code will only read ExodusII format
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver) then
        ${ACCESS}/bin/exo-test-ver ${INFILE1} >& /dev/null
        set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
        if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
        switch(${versflg})
        case -2:
	case -3:
XCOMM File format is ExodusII. No translation is needed
                echo ${INFILE1}" is an ExodusIIv2 file. No translation is needed"
                breaksw
        case  2:
XCOMM File format is ExodusI. Translate to ExodusII
                echo ${INFILE1}" is an ExodusI file. Translate to ExodusII"
                setenv exotmp1 ${INFILE1}
                setenv INFILE1 ${TEMP}/ex1ex2v2.1.$$
                echo "Translating EXODUSI file ${exotmp1} to EXODUSII file"
                ${ACCESS}/etc/ex1ex2v2 ${exotmp1} ${INFILE1} >& /dev/null
                echo "End of translation using ex1ex2v2"
                echo ""
                breaksw
        default:
                echo "Unable to determine type of EXODUS file format."
                echo "No conversions will be done on file "${INFILE1}
                set errflg = 1
                breaksw
        endsw
        ${ACCESS}/bin/exo-test-ver ${INFILE2} >& /dev/null
        set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
        if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
        switch(${versflg})
        case -2:
	case -3:
XCOMM File format is ExodusII. No translation is needed
                echo ${INFILE2}" is an ExodusIIv2 file. No translation is needed"
                breaksw
        case  2:
XCOMM File format is ExodusI. Translate to ExodusII
                echo ${INFILE2}" is an ExodusI file. Translate to ExodusII"
                setenv exotmp1 ${INFILE2}
                setenv INFILE2 ${TEMP}/ex1ex2v2.2.$$
                echo "Translating EXODUSI file ${exotmp1} to EXODUSII file"
                ${ACCESS}/etc/ex1ex2v2 ${exotmp1} ${INFILE2} >& /dev/null
                echo "End of translation using ex1ex2v2"
                echo ""
                breaksw
        default:
                echo "Unable to determine type of EXODUS file format."
                echo "No conversions will be done on file "${INFILE2}
                set errflg = 1
                breaksw
        endsw
endif
if (${errflg} != 0) then
        goto usage
endif

#endif /* FORMAT9 */

#endif /* TRANSLATOR */

/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(GRAPHICS)
/* DEFINITIONS FOR PLOT SCRIPTS
   Variables used in determining the file format needed for execution of
   the program
   FORMAT1 - code has both an ExodusI and ExodusII version.
             Choose version of code base on file type
*/
#if defined(FORMAT1)
/*
   FORMAT1 - code has both an ExodusI and ExodusII version.
             Choose version of code base on file type
*/
XCOMM This code has both an ExodusI and ExodusII version
XCOMM Choose executable based on the file type
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver) then
	${ACCESS}/bin/exo-test-ver ${INFILE} >& /dev/null
	set versflg = ${status}
XCOMM  On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	set makexoI  = "false"
	set makexoII = "false"
	switch(${versflg})
	case -2:
	case -3:
		echo ${INFILE}" is an ExodusIIv2 file"
		set codename = EXOII_NAME
		set makexoII = "true"
		breaksw
	case  2:
		echo ${INFILE}" is an ExodusI file"
		set codename = EXOI_NAME
		set makexoI  = "true"
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "No conversions will be done on file "${INFILE}
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif
#endif /* FORMAT1 */

#if defined(FORMAT2)
/*
 * FORMAT2  - This code can only read exodusI file formats.
 *            Translate exodusII files to exodusI.
*/
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver) then
	${ACCESS}/bin/exo-test-ver ${INPUT} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	set makexoI  = "false"
	set makexoII = "false"
	switch(${versflg})
	case -2:
	case -3:
		echo ${INPUT}" is an ExodusIIv2 file"
XCOMM File format is ExodusII. Translate to ExodusI
		setenv exo2tmp ${INPUT}
		setenv MESH ${TEMP}/ex2ex1v2.$$
		echo "Translating EXODUSII file ${exo2tmp} to EXODUSI file"
		${ACCESS}/etc/ex2ex1v2 ${exo2tmp} ${MESH} >& /dev/null
		echo "End of translation using ex2ex1v2"
		echo ""
		breaksw
		set makexoI = "true"
		breaksw
	case  2:
		echo ${INPUT}" is an ExodusI file"
		set codename = EXOI_NAME
		set makexoI  = "true"
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "No conversions will be done on file "${INPUT}
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif
#endif /* FORMAT2 */

#if defined(FORMAT3)
/*
   FORMAT3 - code will only accept ExodusII format files.
             Translate file to ExodusII format
*/
XCOMM This code will only read ExodusII format
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver) then
	${ACCESS}/bin/exo-test-ver ${INFILE} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
XCOMM File format is ExodusII. No translation is needed
		echo ${INFILE}" is an ExodusIIv2 file. No translation is needed"
		breaksw
	case  2:
XCOMM File format is ExodusI. Translate to ExodusII
		echo ${INFILE}" is an ExodusI file. Translate to ExodusII"
		setenv exotmp1 ${INFILE}
		setenv INFILE ${TEMP}/ex1ex2v2.$$
		echo "Translating EXODUSI file ${exotmp1} to EXODUSII file"
		${ACCESS}/etc/ex1ex2v2 ${exotmp1} ${INFILE} >& /dev/null
		echo "End of translation using ex1ex2v2"
		echo ""
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "No conversions will be done on file "${INFILE}
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif

#endif /* FORMAT3 */

#endif /* GRAPHICS */

/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(ANALYSIS)

#if defined(FORMAT1)
/*
 * FORMAT1 - code reads ExodusI format MESH and RSIN files
 *           If necessary, translate files to ExodusI format.
 */
XCOMM This code will only read ExodusI format
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver && ${serial} == "true") then
	${ACCESS}/bin/exo-test-ver ${MESH} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
		echo ${MESH}" is an ExodusIIv2 file"
XCOMM File format is ExodusII. Translate to ExodusI
		setenv exo2tmp ${MESH}
		setenv MESH ${codename}.g.$$
		echo "Translating EXODUSII file ${exo2tmp} to EXODUSI file"
		${ACCESS}/etc/ex2ex1v2 ${exo2tmp} ${MESH} >& /dev/null
		echo "End of translation using ex2ex1v2"
		echo ""
		breaksw
	case  2:
XCOMM File format is ExodusI.  No translation is needed
		echo ${MESH}" is an ExodusI file. No translation is needed"
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "This code does not read this file format"
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif
if (-e ${RSIN}) then
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver && ${serial} == "true") then
	${ACCESS}/bin/exo-test-ver ${RSIN} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file. Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
		echo ${RSIN}" is an ExodusIIv2 file"
XCOMM File format is ExodusII. Translate to ExodusI
		setenv exo2tmp ${RSIN}
		setenv RSIN ${codename}.rsin.$$
		echo "Translating EXODUS II file ${exo2tmp} to EXODUS I file"
		${ACCESS}/etc/ex2ex1v2 ${exo2tmp} ${RSIN} >& /dev/null
		echo "End of translation using ex2ex1v2"
		echo ""
		breaksw
	case  2:
XCOMM File format is ExodusI.  No translation is needed
		echo ${RSIN}" is an ExodusI file. No translation is needed"
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "This code does not read this file format"
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif
endif /* -e restart */
#endif /* FORMAT1 */

#if defined(FORMAT2)
/*
 * FORMAT2 - code reads ExodusII format MESH and RSIN files.
 *           If necessary, translate files to ExodusII format.
 */
XCOMM This code will only read ExodusII format
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver && ${serial} == "true") then
	${ACCESS}/bin/exo-test-ver ${MESH} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
XCOMM File format is ExodusII. No translation is needed
		echo ${MESH}" is an ExodusIIv2 file"
		breaksw
	case  2:
		echo ${MESH}" is an ExodusI file"
XCOMM File format is ExodusI. Translate to ExodusII
		setenv exo1tmp ${MESH}
		setenv MESH ${codename}.g.$$
		echo "Translating EXODUSI file ${exo1tmp} to EXODUSII file"
		${ACCESS}/etc/ex1ex2v2 ${exo1tmp} ${MESH} 
		echo "End of translation using ex1ex2v2"
		echo ""
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "This code does not read this file format"
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif

if (-e ${RSIN} && ${serial} == "true") then
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver && ${serial} == "true") then
	${ACCESS}/bin/exo-test-ver ${RSIN} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
XCOMM File format is ExodusII. No translation is needed
		echo ${RSIN}" is an ExodusIIv2 file"
		breaksw
	case  2:
		echo ${RSIN}" is an ExodusI file"
XCOMM File format is ExodusI. Translate to ExodusII
		setenv exo1tmp ${RSIN}
		setenv RSIN ${codename}.rsin.$$
		echo "Translating EXODUS I file ${exo1tmp} to EXODUS II file"
		${ACCESS}/etc/ex1ex2v2 ${exo1tmp} ${RSIN} >& /dev/null
		echo "End of translation using ex1ex2v2"
		echo ""
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "This code does not read this file format"
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif
endif  /* -e restart */

#if defined CONSTRAINT
if (-e ${CONSTRAINT} && ${serial} == "true") then
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver && ${serial} == "true") then
	${ACCESS}/bin/exo-test-ver ${CONSTRAINT} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
XCOMM File format is ExodusII. No translation is needed
		echo ${CONSTRAINT}" is an ExodusIIv2 file"
		breaksw
	case  2:
		echo ${CONSTRAINT}" is an ExodusI file"
XCOMM File format is ExodusI. Translate to ExodusII
		setenv exo1tmp ${CONSTRAINT}
		setenv CONSTRAINT ${codename}.constraint.$$
		echo "Translating EXODUS I file ${exo1tmp} to EXODUS II file"
		${ACCESS}/etc/ex1ex2v2 ${exo1tmp} ${CONSTRAINT} >& /dev/null
		echo "End of translation using ex1ex2v2"
		echo ""
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "This code does not read this file format"
		set errflg = 1
		breaksw
	endsw
endif
endif
if (${errflg} != 0) then
	goto usage
endif
#endif
#endif /* FORMAT2 */

#if defined(FORMAT3)
/*
 * FORMAT3 - code reads a ExodusII format MESH file.
 *           If necessary, translate file to ExodusII format.
 */
XCOMM This code will only read ExodusII format
XCOMM Determine format (exodus I or exodus II version ) of input file
if (-x ${ACCESS}/bin/exo-test-ver && ${serial} == "true") then
	${ACCESS}/bin/exo-test-ver ${MESH} >& /dev/null
	set versflg = ${status}
XCOMM On sun systems, exo-test-ver may not return negative status values.
	if (${versflg} == 254) set versflg = -2
	if (${versflg} == 253) set versflg = -2
XCOMM Format of file based on the exit status of exo-test-ver
XCOMM  3 = netCDF file but not exodusII format
XCOMM  2 = Not a netCDF file - Assume ExodusI format
XCOMM <0 = ExodusII/netCDF file.  Major version number is negative
XCOMM -2 = ExodusIIv2
	switch(${versflg})
	case -2:
	case -3:
XCOMM File format is ExodusII. No translation is needed
		echo ${MESH}" is an ExodusIIv2 file"
		breaksw
	case  2:
		echo ${MESH}" is an ExodusI file"
XCOMM File format is ExodusI. Translate to ExodusII
		setenv exo1tmp ${MESH}
		setenv MESH ${codename}.g.$$
		echo "Translating EXODUSI file ${exo1tmp} to EXODUSII file"
		${ACCESS}/etc/ex1ex2v2 ${exo1tmp} ${MESH} 
		echo "End of translation using ex1ex2v2"
		echo ""
		breaksw
	default:
		echo "Unable to determine type of EXODUS file format."
		echo "This code does not read this file format"
		set errflg = 1
		breaksw
	endsw
endif
if (${errflg} != 0) then
	goto usage
endif
#endif /* FORMAT3 */

#endif /* ANALYSIS */

