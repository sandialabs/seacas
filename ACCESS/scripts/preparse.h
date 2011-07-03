/*
 * preparse.h - This .h file is used to pre-parse the users 
 * command line before it gets parsed by getopts or options
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


/***************************************************************/
/***************************************************************/
/***************************************************************/


#if defined(GRAPHICS)

XCOMM Pre-parsing the user command line
@ i = 1
while ($i < $#argv)
    switch ($argv[$i])
    case -m:
        set argv[$i] = "-mesh"
        breaksw
    default:
        breaksw
    endsw
    @ i++
end

#endif /* GRAPHICS */

/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(ANALYSIS)

XCOMM Pre-parsing the user command line
@ i = 1
while ($i < $#argv)
	set user_flag = `echo $argv[$i] | sed -e "s/=.*/=/"`
	switch (${user_flag})
	case -debug=:
XCOMM flag is -debug=... (argument provided, not simply -debug) 
		set user_debugcmd = `echo $argv[$i] | sed -e "s/^.*=//" |                                         sed -e 's/"//g' | sed -e "s/'//g"`
		set argv[$i] = "-debug=user_defined"
		if ("${user_debugcmd}" == "`echo ${user_debugcmd} |                                                           sed -e 's/ .*//'`") then
XCOMM user entered only single word for the debug command
   		   if ("${user_debugcmd}" == "tv") then
		      set user_debugcmd = "totalview"
		   endif
		   if ("${user_debugcmd}" == "x") then
		      set user_debugcmd = "xdebug"
		   endif
		endif
		breaksw
	default:
XCOMM other flags
		breaksw
	endsw
	@ i++
end 

#endif /* ANALYSIS */

/***************************************************************/
/***************************************************************/
/***************************************************************/

