/*
 * usage.h
 * $Id: usage.h,v 1.1 2008/10/31 05:19:57 gdsjaar Exp $
 *
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
exit
usage:
	echo ""
	echo "${USAGE}"
	echo "	Other options:	Argument:	Default:	Note:"
	echo "	-executable	executable	Access copy"
	echo "	-debug		debug command   none"
#if defined(PRECISION)
	echo "	-single         (none)  output single precision file"
	echo "	-double         (none)  output double precision file"
#endif
#if defined(FORMAT4)
	echo "  -force          (none)  copy file if no translation needed"
#endif
#if defined(TRANSFORM)
	echo "	-transform	8chars		none		(*EXOSYM)"
#endif
#if defined(MATERIAL)
	echo "	-Material	mat_id_offset	1"
#endif
#if defined(TIME)
	echo "	-time		timestep time    none"
#endif
#if defined(STEP)
	echo "	-step		timestep number  none"		
#endif
#if defined(DIM)
	echo "	-dimension	2 or 3           none"
#endif
#if defined(PREPRO)
        echo "	-aprepro	(none)	pipes input through aprepro"
#endif
#if defined(SUBOPTION)
	echo "	-subroutine	subroutine_file	-none-		(*sub)"
        echo "	-Include	path-to-include -none-		(*inc)"
        echo "	-Library_path	path-to-library -none-		"
#endif
	echo "	-help		(Prints this message)"
	echo "	"
#if defined(SUBOPTION)
	echo "NOTES:"
	echo "	(*sub) 	If the subroutine file ends in .o, it is assumed"
	echo "		to be an object file (already compiled), else it"
	echo "		is compiled and then linked."
	echo "	(*inc)	If -Include=standard is input, then search in"
	echo "		${ACCESS}/ACCESS/translate/${codename}"
        echo "  "
#endif
        echo "  Sponsor:     SEACAS-HELP@sandia.gov (code, script)"
        echo "  Information: http://www.jal.sandia.gov/SEACAS/SEACAS.html (IRN)"
        echo "               http://endo.sandia.gov/SEACAS/SEACAS.html    (EON)"
        echo "  "
exit
#endif /* TRANSLATOR */


/***************************************************************/
/***************************************************************/
/***************************************************************/


#if defined(GRAPHICS)

exit
usage:
   echo ""
   echo "${USAGE}"
   echo "	Other options:	Argument:	Default:	Note:"
   echo "	-executable	executable	Access copy"
   echo "	-device		dev_code	x11"
#if defined(INPUT)
   echo "	-input          cmd_file        -none-"
#endif
#if defined(MESH)
   echo "	-mesh		mesh-file	-none-"
#endif
#if defined(EXODUS_SWITCH)
   echo "	-exodus         1/2 or I/II     1 or I (exodusI)"
#endif
#if defined(HARDCOPY)
   echo "	-hardcopy	met_filename	*.met"
#endif
   echo "	-help		(Prints this message)"
   echo "	"
   echo "NOTES:"
   echo "  "
   echo "  "
   echo "  Sponsor: SEACAS-HELP@sandia.gov (code, script)"
   echo "  Information: http://www.jal.sandia.gov/SEACAS/SEACAS.html (IRN)"
   echo "               http://endo.sandia.gov/SEACAS/SEACAS.html    (EON)"
   echo "  "
show_device:
   echo " "
   set tempA=`ls ${ACCESS}/bin/${codename}.* |grep -v ${codename}.o |sed -e s:${ACCESS}/bin/${codename}.::`
   echo "Standard options for 'device':" 
   echo "	" $tempA
   echo " "
exit    

#endif /* GRAPHICS */


/***************************************************************/
/***************************************************************/
/***************************************************************/


#if defined(ANALYSIS)
exit
usage:
	echo ""
	echo "${USAGE}"
	echo "	File_options:	Argument:	Default:	Extension:"

#if !defined(MAPVAR) && !defined(MAPVARKD)
#if defined(XJAS3D) || defined(XPRONTO3D)
	echo "	-input		input_file	("FOR015")	.i"
	echo "	-output		ouput_file	("FOR016")	.o"
#else
	echo "	-input		input_file	("FOR005")	.i"
	echo "	-output		ouput_file	("FOR006")	.o"
#endif
#else  /* ifdef MAPVAR */
	echo "	-output		ouput_file	("FOR007")	.o"
#endif /* ifndef MAPVAR */

#if defined(SUMMARY)
	echo "	-summary	summary_file	("FOR008")	.s"
#endif

#if defined(MESH)
	echo "	-mesh (Genesis)	mesh_file	("MESH")   	.g"
#endif

#if defined(CONSTRAINT)
	echo "	-constraint   cnstrnt_mesh_file ("CONSTRAINT")        .gc"
#endif

#if defined(PLOT)
	echo "	-plot (Exodus)	plot_file	("PLOT") 	.e"
#endif

#if defined(VIEWFACTOR)
	echo "	-viewfactor	view_file	("VIEWFACTOR")	.vf"
#endif

#if defined(RSIN)
	echo "	-restart	restart_in	("RSIN")	.rsin"
#endif

#if defined(EPLOT)
	echo "	-plot   	result_file	("EPLOT")	.e"
#endif

#if defined(EMESH)
	echo "	-mesh (Genesis)	mesh_file	("EMESH")   	.g"
#endif

#if defined(INTERPOLATED)
	echo "	-interpolated	int_file	("INTERPOLATED")	.int"
#endif

#if defined(THERMAL)
	echo "	-thermal	thrm_file	("THERMAL")	.th"
#endif

#if defined(RSOUT)
	echo "	-rsout		restart_out	("RSOUT")	.rsout"
	echo "	-dump		restart_out	("RSOUT")	.rsout"
#endif

#if defined(ISPARALLEL)
	echo "	-parallel	MPI hosts file	 -none-		.hosts"
#endif

#if defined(DIST_LOAD)
	echo "	-distributed	dist_load	("DIST_LOAD")	.dist"
#endif

#if defined(EXTERNAL)
	echo "	-external	external	("EXTERNAL")	.ext"
#endif

#if defined(USR)
	echo "	-userplot	user_plot	("USR")		.usr"
#endif

	echo ""
	echo " If BASE is specified, then all files not explicitly specified"
	echo " 	will be read from/written to BASE.extension"
	echo " "
	echo "	Other options:	Argument:	Default:	Note:"
	echo "	-executable	executable	Access copy"
	echo "	-aprepro	aprepro_options -none-"
	echo "	-subroutine	subroutine_file	-none-	  	(*sub)"
	echo "	-debug		debug command   -none-"
	echo "	-Include	path-to-include -none-          (*inc)"
        echo "	-Library_path	path-to-library -none-		"
	echo "	-shutdown	shutdown_time   -none-          s or h:m:s"
	echo "	-help		(Prints this message)"
	echo "	"
	echo "NOTES:"
	echo "	(*sub) 	If the subroutine file ends in .o, it is assumed"
	echo "		to be an object file (already compiled), else it"
	echo "		is compiled and then linked."
	echo "	(*inc)	If -Include=standard is input, then search in"
#if defined(JAS3D)
	echo "		${ACCESS}/ACCESS/analysis/${codename}/COMPILE"
#else
	echo "		${ACCESS}/ACCESS/analysis/${codename}"
#endif
        echo "  "
	echo "  "
#if defined(SANCHO) || defined(SANTOS) || defined(SANTOS3D)
	echo " Sponsor: Mike Stone (code), SEACAS-HELP@sandia.gov (script)"
#endif
#if defined(JAC2D) || defined (JAC3D) || defined(JACQ3D) || defined(JAS3D) || defined(XJAS3D)
	echo " Sponsor: Mark Blanford (code), SEACAS-HELP@sandia.gov (script)"
#endif
#if defined(PRONTO2D) || defined(PRONTO3D) || defined(XPRONTO2D) || defined(XPRONTO3D) || defined(PRONTO3D94)
	echo " Sponsor: Steve Attaway, PRONTO@sandia.gov (code), SEACAS-HELP@sandia.gov (script)"
#endif
#if defined(SUBWAY)
	echo " Sponsor: Stephen Montgomery (code), SEACAS-HELP@sandia.gov (script)"
#endif
#if defined(MERLIN2) 
	echo " Sponsor: Dave Gartling (code), SEACAS-HELP@sandia.gov (script)"
#endif
#if defined(MAPVAR) 
	echo " Sponsor: Jerry Wellman (code), SEACAS-HELP@sandia.gov (script)"
#endif
#if defined(MAPVARKD) 
	echo " Sponsor: Greg Sjaardema (code), SEACAS-HELP@sandia.gov (script)"
#endif
        echo " Information: http://www.jal.sandia.gov/SEACAS/SEACAS.html (IRN)"
        echo "              http://endo.sandia.gov/SEACAS/SEACAS.html    (EON)"
	echo "  "
exit

#endif /* ANALYSIS */


/***************************************************************/
/***************************************************************/
/***************************************************************/

