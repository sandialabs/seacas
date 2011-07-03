/*
 * environment.h
 * $Id: environment.h,v 1.1 2008/10/31 05:19:56 gdsjaar Exp $
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

XCOMM Set path to ACCESS installation directories - etc, bin, config, ...
XCOMM Set path to ACCESS source directory (ACCESSSRC)
XCOMM Defined for everything that includes this file

XCOMM Determine ACCESS and ACCESSSRC based on the location of the script
if ("`echo $0 | sed 's/\///'`" != "$0") then
XCOMM Location of the script found
   if ("`ls -l $0 | sed 's/^l.*/l/'`" != "l") then
XCOMM Script is not a softlink
      setenv ACCESS `echo $0 | sed 's/\/etc\/.*//'`
      setenv ACCESSSRC ${ACCESS}/ACCESS
      if (! -e ${ACCESS}/config/site.def) then
         setenv ACCESS _ACCESSDIR
         setenv ACCESSSRC _ACCESSSRC
      endif
   else
XCOMM Script is a softlink
      setenv ACCESS _ACCESSDIR
      setenv ACCESSSRC _ACCESSSRC
   endif
else
XCOMM Determining the location of the script failed, use default values
   setenv ACCESS _ACCESSDIR
   setenv ACCESSSRC _ACCESSSRC
endif

#if defined(TRANSLATOR)

XCOMM Store temporary files in TEMP
if (${?TMPDIR}) then
	set TEMP = ${TMPDIR}
else
	set TEMP = _EXETMP
endif

#endif /* end TRANSLATOR */


/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(GRAPHICS)

XCOMM Store temporary files in TEMP
if (${?TMPDIR}) then
	set TEMP = ${TMPDIR}
else
	set TEMP = _EXETMP
endif

#endif /* end GRAPHICS */

/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(ANALYSIS)
XCOMM Store temporary files in TEMP
if (${?TMPDIR}) then
	set TEMP = ${TMPDIR}
else
	set TEMP = _EXETMP
endif

set USAGE = \
"Usage: codename [-help] [-file_options filename] [-options option] [--] [base]"

#endif /* end ANALYSIS */

XCOMM Print header:
echo " "
echo "=============================================================="
echo "| Sandia Engineering Analysis Access procedure for: ${codename}"
#if defined(PRONTO2D) || defined(PRONTO3D) || defined(XPRONTO2D) || defined(XPRONTO3D)
echo "| Send email to PRONTO@sandia.gov for help"
#else
echo "| Send email to SEACAS-HELP@sandia.gov for help"
#endif
echo "=============================================================="
echo " "

#if defined(XCODE)
if (-e ${ACCESS}/XACCESS/analysis/${codename}/'x'${codename}'.mes') then
echo " "
echo "===================== x${codename} Information ====================="
        cat ${ACCESS}/XACCESS/analysis/${codename}/'x'${codename}'.mes'
echo "======================================================================"
echo " "
endif
#endif


