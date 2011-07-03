/* subop.h */

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

#if defined(TRANSLATOR) || defined(MAPVAR) || defined(MAPVARKD) || defined(SANTOS)

#if defined(SUBOPTION)
XCOMM Check subroutine option.
if ($subroutine != "false") then
XCOMM Set variables for suboption
        set FC              = (_FC)
#if defined(EXODUSII2)
	set NETCDF          = (_NETCDF)
	set EXOIIFOR        = _EXOIIV2FOR
	set EXOIIC          = _EXOIIV2C
#else
	set NETCDF          = " "
	set EXOIIFOR        = " "
	set EXOIIC          = " "
#endif /* EXODUSII2 */
#if defined(D_PRECISION)
	set SUPES           = _SUPESDP
#else
	set SUPES           = _SUPES
#endif
	set F77DEBUG        = (_F77DEBUG)
	set F77DEBUGFLAGS   = (_F77DEBUGFLAGS)
	set SUPLIB          = _SUPLIB
	set F77OPTIONS      = (_F77OPTIONS)
	set F77PRECOPTIONS  = (_F77PRECOPTIONS)
	set F77DEBUG        = (_F77DEBUG)
	set EXTRA_LIBRARIES = (_EXTRA_LIBRARIES)
	set LDOPTIONS       = (_EXTRA_LDOPTIONS)
	set subf = "false"
	set subo = "false"
	switch($subroutine:e)
	case F:
	case f:
		set subf = "true"
		breaksw
	case o:
		set subo = $subroutine
		breaksw
	default:
		echo "ERROR - Subroutine file $subroutine does not exist"
		goto usage
		breaksw
	endsw
	if (${subf} == "true") then
		echo "Compiling $subroutine ..."
		if (${include}  != " ") then
			echo "Include Directory = ${include}"
		endif
		if (${rundebug} == "true") then
		  ${FC} -c ${F77DEBUG} ${F77OPTIONS} ${F77PRECOPTIONS} -I${include} -I./ -I${ACCESS}/inc $subroutine
		else
		  ${FC} -c ${F77DEBUGFLAGS} ${F77OPTIONS} ${F77PRECOPTIONS} -I${include} -I./ -I${ACCESS}/inc $subroutine
		endif
		set errflg = ${status}
		if (${errflg} != 0) then
			echo "ERROR -- Problems during compilation"
			goto usage
		endif
XCOMM If the subroutine file is not in the current directory, we have
XCOMM to update the path since the object file will be written to the
XCOMM current directory.
		set subroutine = $subroutine:t
		set subo = $subroutine:r'.o'
	endif
XCOMM Determine if single object file or object + library
	if (${local_library_path} != "") then
	  if (-e ${local_library_path}/lib/lib${codename}.a) then
		set code_lib = ${local_library_path}/lib/lib${codename}.a
	  else
		echo "ERROR - Library lib${codename}.a does not exist in ${local_library_path}."
		goto usage
		breaksw
	  endif
	else
	  if (-e ${ACCESS}/lib/lib${codename}.a) then
		set code_lib = ${ACCESS}/lib/lib${codename}.a
	  else
		set code_lib = " "
	  endif
	endif
		  
XCOMM Link the subroutine to the code
	if (${local_library_path} != "") then
	  if (-e ${local_library_path}/${codename}.o) then
	      set code_main = ${local_library_path}/${codename}'.o'
	  else
	      echo "ERROR - Library lib${codename}.a does not exist in ${local_library_path}."
	      goto usage
	      breaksw
	  endif  
	else
	   set code_main = ${ACCESS}/bin/${codename}'.o'
	endif
	set OBJS      = "${code_main} ${code_lib}"
#if defined(EXODUSII2)
	set EXOLIBS = "${EXOIIFOR} ${EXOIIC} ${NETCDF}"
#else
	set EXOLIBS = " "
#endif
	set LIBS    = "${code_lib} ${SUPLIB} ${SUPES} ${EXTRA_LIBRARIES}"
	set local_exe = ./${codename}
	echo "linking $subroutine to ${OBJS}, executable = ${local_exe}"
	${FC} -o ${codename} $subroutine:r'.o' ${OBJS} -L./ -L${ACCESS}/lib ${LIBS} ${EXOLIBS} ${LDOPTIONS} 
	set errflg = ${status}
	if (${errflg} != 0) then
		echo "ERROR -- Problems during linking."
		goto usage
	endif
endif

#endif /* SUBOPTION */

#endif /* TRANSLATOR */

/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(GRAPHICS)

XCOMM Check ${ACCESS}/bin for executable
if (-x ${ACCESS}/bin/${codename}.${device}) then
  set EXEC_DIR = ${ACCESS}/bin
 else
   echo "ERROR: ${ACCESS}/bin/${codename}.${device} does not exist."
   echo "       Contact seacas-help@sandia.gov if you need this device;"
   echo "       otherwise use one of the supported devices:"
   goto usage
endif

#endif /* GRAPHICS */

/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(ANALYSIS) && !defined(MAPVAR) && !defined(MAPVARKD) && !defined(SANTOS)

XCOMM Check subroutine option.  The subroutine variable is initialy set
XCOMM to false.  If the sub option is used it is set to the name of
XCOMM of the user subroutine 
if ($subroutine != "false") then
XCOMM Define compiler and debug commands
#if defined(ISPARALLEL)
  if (${parallel} == 0) then
XCOMM Compilers for serial codes
    set FC              = (_FC)
    set CC              = (_CC)
    set F90C            = (_F90C)
  else
XCOMM Compilers for parallel codes
    set FC              = (_PAR_FC)
    set CC              = (_PAR_CC)
    set F90C            = (_PAR_F90C)
  endif
#else
XCOMM FORTRAN and C compilers.
    set FC              = (_FC)
    set CC              = (_CC)
    set F90C            = (_F90C)
#endif
XCOMM Define libraries to build executables
#if defined(EXODUSII2)
    set NETCDF          = (_NETCDF)
    set EXOIIFOR        = _EXOIIV2FORANALYSIS
    set EXOIIC          = _EXOIIV2C
    set NEMESISF        = _NEMFORANALYSIS
    set NEMESISC        = _NEMESISC
    set EXOLIBS         = "${NEMESISF} ${NEMESISC} ${EXOIIFOR} ${EXOIIC} ${NETCDF}"
#else
    set NETCDF          = " "
    set EXOIIFOR        = " "
    set EXOIIC          = " "
    set NEMESISF        = " "
    set NEMESISC        = " "
    set EXOLIBS         = " "
#endif
    set SUPLIB          = _SUPLIB
#if defined(D_PRECISION)
XCOMM Double precision codes need supesdp library
    set SUPES           = _SUPESDP
#else
XCOMM Single precision codes need supes library
    set SUPES           = _SUPES
#endif

#ifdef STATIC
XCOMM Set static variable to FORTRAN compiler static flag.
    set static          = _F77STATICFLAGS
#else
XCOMM FORTRAN static flag not used during compilation of this code.
    set static          = " "
#endif
#if defined(JAS3D) || defined(XJAS3D)
XCOMM Jas3d has an additional library
     set contact_lib = "-lLegacyContact"
#else
    set contact_lib = " "
#endif		   
#if defined(COYOTE)
    set KRYSOLV         = _KRYSOLV
    set SPARSE          = _SPARSE
    set VF              = _VF
    set PROGRAM_LIBS    = "${KRYSOLV} ${SPARSE} ${VF}"
#else
    set PROGRAM_LIBS    = " "
#endif
    if (${rundebug} == "true") then
XCOMM Compiler flags for debugging in FORTRAN and C.
	set F77DEBUGFLAGS = (_F77DEBUG)
#if defined(SUN)
#if defined(XCODE)
	set F90DEBUGFLAGS = (_F90DEBUG -M${ACCESS}/XACCESS/analysis/${codename})
#else
	set F90DEBUGFLAGS = (_F90DEBUG -M${ACCESS}/ACCESS/analysis/${codename})
#endif
#else
	set F90DEBUGFLAGS = (_F90DEBUG)
#endif
	set CDEBUGFLAGS   = (_CDEBUG)
    else
XCOMM Compiler flags for optimized FORTRAN and C code.
	set F77DEBUGFLAGS = (_F77DEBUGFLAGS)
#if defined(SUN)
#if defined(XCODE)
	set F90DEBUGFLAGS = (_F90DEBUGFLAGS -M${ACCESS}/XACCESS/analysis/${codename})
#else
	set F90DEBUGFLAGS = (_F90DEBUGFLAGS -M${ACCESS}/ACCESS/analysis/${codename})
#endif
#else
	set F90DEBUGFLAGS = (_F90DEBUGFLAGS)
#endif
	set CDEBUGFLAGS   = (_CDEBUGFLAGS)
    endif
#if defined(ISPARALLEL)
    if (${parallel} == 0) then
XCOMM Compiler options for serial codes
	set F77OPTIONS      = (_F77OPTIONS)
	set F90OPTIONS      = (_F90OPTIONS)
	set CCOPTIONS       = (_CCOPTIONS)
    else
XCOMM Compiler options for parallel codes
	set F77OPTIONS      = (_PARF77OPTIONS)
	set F90OPTIONS      = (_PARF90OPTIONS)
	set CCOPTIONS       = (_PARCCOPTIONS)
    endif
#else
XCOMM FORTRAN and C compiler options.
    set F77OPTIONS      = (_F77OPTIONS)
    set CCOPTIONS       = (_CCOPTIONS)
#endif
#if defined(ISPARALLEL)
    if (${parallel} == 0) then
XCOMM libraries for serial codes
	set ANALYSIS_LIBS   = "-lm"
    else
XCOMM libraries for parallel codes
        set PARALLELLIBS   = (_PARFLIBS)
	set ANALYSIS_LIBS   = "-lm ${PARALLELLIBS}"
    endif
#else
    set ANALYSIS_LIBS   = -lm
#endif
    set EXTRA_LIBRARIES = (_EXTRA_LIBRARIES)
    set XLIBS           = "${EXTRA_LIBRARIES} ${ANALYSIS_LIBS}"

XCOMM Extra F90 link options:
    set F90EXTRALDOPTIONS = _F90EXTRALDOPTIONS

XCOMM parse $subroutine here
    echo $subroutine >! varfile
    set var = ('' '' '' '' '' '' '' '' '' '' '' '' '' '' '' '' '' '' '' '')
    set addedfiles = ()
 
XCOMM get the number of "," delimited fields in the subroutine variable here
    set num_var = `awk -F, '{print NF}' varfile`
    if (${num_var} > 20) then
       echo "Too many subroutine files listed."
       exit
    endif
    set count = 1 
XCOMM  process each file in the subroutine field
    while ($count <= ${num_var})
        set var[$count] = `awk -F, '{print $'$count'}' varfile`
        set subroutine = $var[$count]
        echo ""
        echo "File $count :  $subroutine"
        echo ""

        set subc   = "false"
        set subf   = "false"
	set subf90 = "false"
	switch($subroutine:e)
	    case F:
	    case f:
		set subf = "true"
                set linkfile = `echo $subroutine:r'.o' | sed 's/.*\///'`
		breaksw
	    case c:
		set subc = "true"
                set linkfile = `echo $subroutine:r'.o' | sed 's/.*\///'`
		breaksw
	    case f90:
		set subf90 = "true"
                set linkfile = `echo $subroutine:r'.o' | sed 's/.*\///'`
		breaksw
	    case o:
	    case a:
	    case so:
                set linkfile = $subroutine
		breaksw
	    default:
		echo "ERROR - Subroutine file $subroutine does not exist"
		goto usage
		breaksw
	endsw
XCOMM Check for compiled object file

XCOMM Check file type of subroutine file.  If .o, then just link, else compile
	if (${subf} == "true" || ${subc} == "true" || ${subf90} == "true") then
	    echo "Compiling $subroutine ... Include Directory = ${include}"
XCOMM Compile FORTRAN subroutine
	    if (${subf} == "true") then
		${FC} -c ${F77DEBUGFLAGS} ${F77OPTIONS} -I${include} -I./ -I${ACCESS}/inc $subroutine 
            endif
XCOMM Compile C subroutine
	    if (${subc} == "true") then
		${CC} -c ${CDEBUGFLAGS} ${CCOPTIONS} -I${include} -I./ -I${ACCESS}/inc _ALLDEFINES $subroutine 
	    endif
XCOMM Compile FORTRAN 90 subroutine
            if (${subf90} == "true") then
		${F90C} -c ${F90DEBUGFLAGS} ${F90OPTIONS} -I${include} -I./ -I${ACCESS}/inc $subroutine 
	    endif
            set errflg = ${status}
	    if (${errflg} != 0) then
		echo "ERROR -- Problems during compilation"
	        goto usage
	    endif
XCOMM If the subroutine file is not in the current directory, we have to update
XCOMM the path since the object file will be written to the current directory.
	    set subroutine = $subroutine:t
	    set subo = $subroutine:r'.o'
	    if (!(-e ${subo})) then
		echo "ERROR -- Problems during compilation"
		goto usage
	    endif
	endif

XCOMM Link the subroutine to the code
        if ($count == ${num_var}) then

	if (${local_library_path} != "") then
	  if (-e ${local_library_path}/lib${codename}.a) then
		set code_lib = ${local_library_path}/lib${codename}.a
	  else
		echo "ERROR - Library lib${codename}.a does not exist in ${local_library_path}."
		goto usage
		breaksw
	  endif
	  if (-e ${local_library_path}/${codename}.o) then
		set code_main = ${local_library_path}/${codename}.o
	  else
		echo "ERROR - File ${codename}.o does not exist in ${local_library_path}."
		goto usage
		breaksw
	  endif
	else
#if defined(XCODE)
	    if (${rundebug} == "true") then
		set code_main = ${ACCESS}/XACCESS/analysis/${codename}'_dbg'/${codename}'.o'
	    else
		set code_main = ${ACCESS}/XACCESS/analysis/${codename}/${codename}'.o'
	    endif
#else
	    set code_main = ${ACCESS}/bin/${codename}'.o'
#endif
XCOMM Determine if single object file or object + library
#if _PARALLEL==1
#if defined(XCODE)
	    if (${rundebug} == "true") then
		set code_lib = ${ACCESS}/XACCESS/analysis/${codename}'_dbg'/lib${codename}.a
	    else
		set code_lib = ${ACCESS}/XACCESS/analysis/${codename}/lib${codename}.a
	    endif
#else
	    if (-e ${ACCESS}/plib/lib${codename}.a) then
		set code_lib = ${ACCESS}/plib/lib${codename}.a
	    else
		set code_lib = " "
	    endif
#endif /* endif XCODE     */
#else
#if defined(XCODE)
	    if (${rundebug} == "true") then
		set code_lib = ${ACCESS}/XACCESS/analysis/${codename}'_dbg'/lib${codename}.a
	    else
		set code_lib = ${ACCESS}/XACCESS/analysis/${codename}/lib${codename}.a
	    endif
#else
	    if (-e ${ACCESS}/lib/lib${codename}.a) then
		set code_lib = ${ACCESS}/lib/lib${codename}.a
	    else
		set code_lib = " "
	    endif
#endif /* endif XCODE     */
#endif /* endif _PARALLEL==1 */
            endif /* local library path */
	    set OBJS = "${code_main} ${code_lib}"
#if defined(XCODE)
	    set LIBS = "${SUPLIB} ${SUPES} ${PROGRAM_LIBS} ${XLIBS}"
#else
	    set LIBS = "${code_lib} ${SUPLIB} ${SUPES} ${PROGRAM_LIBS} ${XLIBS}"
#endif
	    echo ""
#if _PARALLEL == 1
            echo "Linking: "${addedfiles} ${linkfile} ${OBJS} -L./ -L${ACCESS}/plib ${LIBS} ${EXOLIBS}
#else
            echo "Linking: "${addedfiles} ${linkfile} ${OBJS} -L./ -L${ACCESS}/lib ${LIBS} ${EXOLIBS}
#endif
            echo "Executable: "./${codename}
            echo ""
#if _PARALLEL
	    set LDOPTIONS  = (_PARLDOPTIONS _EXTRA_LDOPTIONS)
#else
	    set LDOPTIONS  = (_EXTRA_LDOPTIONS)
#endif
#if _PARALLEL==1
#if defined(PRONTO3D) || defined(XPRONTO3D)
            ${F90C} -o ${codename} ${include}/blkdat.o ${addedfiles} ${linkfile} ${OBJS} -L./ -L${ACCESS}/plib ${LIBS} ${EXOLIBS} ${LDOPTIONS} ${F90EXTRALDOPTIONS}
#endif
#if defined(PRONTO3D94) 
            ${FC} -o ${codename} ${include}/blkdat.o ${addedfiles} ${linkfile} ${OBJS} -L./ -L${ACCESS}/plib ${LIBS} ${EXOLIBS} ${LDOPTIONS}
#endif
#if defined(JAS3D) || defined(XJAS3D)
            ${F90C} -o ${codename} ${addedfiles} ${linkfile} ${OBJS} -L./ -L${ACCESS}/plib ${contact_lib} ${LIBS} ${EXOLIBS} ${LDOPTIONS} ${F90EXTRALDOPTIONS}
#endif

#else /* PARALLEL != 1 */
#if defined(PRONTO3D) || defined(XPRONTO3D) || defined(JAS3D) || defined(XJAS3D)
            ${F90C} -o ${codename} ${addedfiles} ${linkfile} ${OBJS} -L./ -L${ACCESS}/lib ${contact_lib} ${LIBS} ${EXOLIBS} ${LDOPTIONS} ${F90EXTRALDOPTIONS}
#endif
#if defined(PRONTO3D94)
            ${FC} -o ${codename} ${include}/blkdat.o ${addedfiles} ${linkfile} ${OBJS} -L./ -L${ACCESS}/plib ${LIBS} ${EXOLIBS} ${LDOPTIONS}
#endif
#if defined(SANTOS)
            ${FC} -o ${codename} ${addedfiles} ${linkfile} ${OBJS} -L./ -L${ACCESS}/lib ${LIBS} ${EXOLIBS} ${LDOPTIONS}
#endif
#endif

	    set errflg = ${status}
	    if (${errflg} != 0) then
		echo "ERROR -- Problems during linking."
		goto usage
	    endif
	    set local_exe = ./${codename}
	endif
        set addedfiles = (${addedfiles} ${linkfile})
        @ count ++
XCOMM end of while loop on subroutine field
    end
    rm -r varfile
endif

#endif /* ANALYSIS */

/***************************************************************/
/***************************************************************/
/***************************************************************/

