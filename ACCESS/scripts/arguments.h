/* 
 * arguments.h  - this file assigns FORTRAN unit numbers to file names
 *                specified on the command line. (used only in analysis scripts)
 * $Id: arguments.h,v 1.1 2008/10/31 05:19:56 gdsjaar Exp $
*/

#if defined(ANALYSIS)

XCOMM Check for basename argument (Will follow "--" in argument list)
set temp = ($argv)
while ("$temp[1]" != "--")
	shift temp
end
XCOMM temp points to --
shift temp
XCOMM temp now points to first non-option value which is defined to be
XCOMM the default basename for all files.  If non-null, assign to
XCOMM files. Otherwise, 'fort'.unit_number already assigned in definition
if ($#temp > 0) then

	setenv FOR005 $temp[1]'.i'
	setenv FOR015 $temp[1]'.i'
	setenv FOR006 $temp[1]'.o'
	setenv FOR016 $temp[1]'.o'
	setenv FOR007 $temp[1]'.o'

#if defined(SUMMARY)
	setenv SUMMARY $temp[1]'.s'
#endif
  
#if defined(MESH)
	setenv MESH $temp[1]'.g'
#endif

#if defined(EMESH)
	setenv EMESH $temp[1]'.g'
#endif

#if defined(EPLOT)
	setenv EPLOT $temp[1]'.e'
#endif

#if defined(PLOT)
#if defined(SEACO)
	setenv PLOT $temp[1]'.s'
#else
	setenv PLOT $temp[1]'.e'
#endif
#endif

#if defined(RSIN)
	setenv RSIN $temp[1]'.rsin'
#endif

#if defined(RSOUT)
	setenv RSOUT $temp[1]'.rsout'
#endif

#if defined(EXTERNAL)
	setenv EXTERNAL $temp[1]'.ext'
#endif

#if defined(DIST_LOAD)
	setenv DIST_LOAD $temp[1]'.dist'
#endif

#if defined(HISTORY)
	setenv HISTORY $temp[1]'.h'
#endif

#if defined(CONSTRAINT)
	setenv CONSTRAINT $temp[1]'.gc'
#endif

#if defined(THERMAL)
	setenv THERMAL $temp[1]'.th'
#endif

#if defined(INTERPOLATED)
	setenv INTERPOLATED $temp[1]'.int'
#endif

#if defined(VIEWFACTOR)
	setenv VIEWFACTOR $temp[1]'.vf'
#endif

#if defined(ISPARALLEL)
#if defined(CONFIGFILE)
	setenv CONFIGFILE $temp[1]'.cfg'
#endif
#endif

#if defined(SHUTDOWNFILE)
	setenv SHUTDOWNFILE $temp[1]'.shutdown'
#endif

#if defined(USR)
	setenv USR $temp[1]'.usr'
#endif

else
XCOMM Default FORTRAN unit number assignment
	setenv FOR005 fort.5
	setenv FOR015 fort.5
	setenv FOR006 fort.6
	setenv FOR016 fort.6

#if defined(MERLIN2) || defined(MAPVAR) || defined(MAPVARKD)
	setenv EPLOT fort.12
	setenv EMESH fort.13
#endif

#if defined(SUMMARY)
	setenv SUMMARY fort.8
#endif

#if defined(MESH)
#if defined(COYOTE)
	setenv MESH fort.10
#else
	setenv MESH fort.9
#endif
#endif

#if defined(PLOT)
#if defined(COYOTE)
	setenv PLOT fort.12
#else
	setenv PLOT fort.11
#endif
#endif

#if defined(RSIN)
#if defined(COYOTE)
	setenv RSIN fort.14
#else
#if defined(SANCHO)
	setenv RSIN fort.19
#else
	setenv RSIN fort.32
#endif
#endif
#endif

#if defined(RSOUT)
#if defined(SANCHO)
	setenv RSOUT fort.18
#else
	setenv RSOUT fort.30
#endif
#endif

#if defined(EXTERNAL)
#if defined(COYOTE)
	setenv EXTERNAL fort.13
#else
	setenv EXTERNAL fort.39
#endif
#endif

#if defined(DIST_LOAD)
	setenv DIST_LOAD fort.38
#endif

#if defined(HISTORY)
	setenv HISTORY fort.13
#endif

#if defined(CONSTRAINT)
	setenv CONSTRAINT fort.10
#endif

#if defined(THERMAL)
#if defined(MERLIN2)
	setenv THERMAL fort.15
#else
	setenv THERMAL fort.56
#endif
#endif

#if defined(INTERPOLATED)
	setenv INTERPOLATED fort.14
#endif

#if defined(VIEWFACTOR)
	setenv VIEWFACTOR fort.15
#endif

#if defined(ISPARALLEL)
#if defined(CONFIGFILE)
	setenv CONFIGFILE fort.62
#endif
#endif

#if defined(SHUTDOWNFILE)
	setenv SHUTDOWNFILE fort.63
#endif

#if defined(USR)
	setenv USR fort.16
#endif

endif

#endif /* ANALYSIS */
