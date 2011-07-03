/*
 * $Id: log.h,v 1.1 2008/10/31 05:19:57 gdsjaar Exp $
 */

/*
 *  This include file is used to log information to the audit.log
 * file located in the ${ACCESS}/etc directory
 */

/**********************************************************************/
if ($?SEACAS_NO_LOGGING) then
  echo "SEACAS Audit logging disabled via SEACAS_NO_LOGGING setting"
else
#if defined(LOADBAL)
XCOMM Store temporary files in TEMP
if (${?TMPDIR}) then
	set TEMP = ${TMPDIR}
else
	set TEMP = _EXETMP
endif
#endif /* end of LOADBAL */

/* Log information */
XCOMM Echo audit information to ${ACCESS}/etc/audit.log
time >${TEMP}/time.$$
set time_used=`cat ${TEMP}/time.$$`; rm ${TEMP}/time.$$

XCOMM Get name of user running code
if (${?USER}) then
	set username = ${USER}
else
	if (${?LOGNAME}) then
		set username = ${LOGNAME}
	else
		set username = "UNKNOWN"
	endif
endif

XCOMM Write info to audit.log file
if (-w ${ACCESS}/etc/audit.log) then
#if defined(GRAPHICS)
  echo ${codename}.${device} ${username} `date` ${time_used} `hostname` >>${ACCESS}/etc/audit.log
#else
  echo ${codename} ${username} `date` ${time_used} `hostname` >>${ACCESS}/etc/audit.log
#endif 
endif
endif /* SEACAS_NO_LOGGING */
/**********************************************************************/

