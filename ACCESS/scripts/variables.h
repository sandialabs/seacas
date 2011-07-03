/*
 * variables.h
 * $Id: variables.h,v 1.1 2008/10/31 05:19:57 gdsjaar Exp $
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

set precommand	= "time"
set no_exe 	= "false"
set local_exe	= "false"
set pipe_input	= "false"
set aprepro	= "false"
set subroutine	= "false"
set apr_opt	= " "
set include     = "./"
set rundebug    = "false"
set debugcmd    = _SERIALDEBUGCMD
set local_library_path = ""
#if defined(FORMAT4)
set force       = "false"
#endif
#if defined(DIM)
set ndim        = "unset"
#endif

#endif /* end TRANSLATOR */

/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(GRAPHICS)

set no_exe 	= "false"
set local_exe	= "false"
set rundebug    = "false"
set precommand	= "time"
set pipe_input	= "false"
set aprepro	= "false"
set makexoI     = "false"
set apr_opt	= " "
set device	= "x11"
#if defined(FASTQ)
set filename    = "${codename}"
set makexoII    = "true"
#else
set filename    = ""
set makexoII    = "false"
#endif
set local_library_path = ""
set include     = ${ACCESSSRC}/graphics/${codename}
set debugcmd    = _SERIALDEBUGCMD
#if defined(BLOT) || defined(GRAPH)
setenv EXT04 NO
#endif
#if defined(FASTQ)
set batch       = "false"
setenv EXTO3 INT
#endif

#endif /* GRAPHICS */

/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(ANALYSIS)

XCOMM Initialize variables
set no_exe       = "false"
set local_exe    = "false"
set subroutine   = "false"
set serial       = "true"
set parallel     = _PARALLEL
#if _PARALLEL 
#if defined(PRONTO2D) || defined(XPRONTO2D) || defined(MERLIN2)
set npcmd        = " "
set num_procs    = 0
#if defined(SUN) || (defined(linux) && _NEEDS_HOSTFILE == 1)
set hfcmd        = _HFCMD
set hosts_file   = "${TEMP}/hostsfile.hosts.$$"
set user_hf      = ""
set tmp_hf       = "${TEMP}/hostsfile.tmp.$$"
set codename_hf  = "${codename}.hf.$$"
#else
#if defined(DEC)
set hfcmd        = _HFCMD
set hosts_file   = ""
set user_hf      = ""
#else
set user_hf      = ""
#endif
#endif
set runcommand   = " "
#else
set npcmd        = _NPCMD
set num_procs    = 1
#if defined(SUN) || (defined(linux) && _NEEDS_HOSTFILE == 1)
set hfcmd        = _HFCMD
set hosts_file   = "${TEMP}/hostsfile.hosts.$$"
set user_hf      = ""
set tmp_hf       = "${TEMP}/hostsfile.tmp.$$"
set codename_hf  = "${codename}.hf.$$"
#else
#if defined(DEC)
set hfcmd        = _HFCMD
set hosts_file   = ""
set user_hf      = ""
#else
set user_hf      = ""
#endif
#endif
#if defined(MAPVAR) || defined (MERLIN2) || defined(MAPVARKD)
set runcommand   = ""
#else
set runcommand   = _PARRUNCMD
#endif
#endif
#else
set npcmd        = " "
set num_procs    = 0
set runcommand   = " "
#endif
set config       = "false"
set aprepro      = "false"
set apr_opt      = " "
set rundebug     = "false"
set debugcmd     = _SERIALDEBUGCMD
#if _PARALLEL==1
if (${?YOD_OPTIONS}) then
	set yod_options = "${YOD_OPTIONS}" 
else
	set yod_options = " "
endif
#endif
#if defined(XCODE)
set include      = ${ACCESS}/XACCESS/analysis/${codename}
#else
set include      = ./
#endif
set local_library_path = ""
set timenicecmd  = "time"
set shutdown_time = ""

#endif /* ANALYSIS */

/***************************************************************/
/***************************************************************/
/***************************************************************/

