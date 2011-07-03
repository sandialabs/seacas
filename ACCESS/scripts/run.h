/*
 * run.h
 * $Id: run.h,v 1.1 2008/10/31 05:19:57 gdsjaar Exp $
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

/*
   Variables used in setting the i/o environment variables
        RUNn       - variable indicating how to execute the code
 	RUN1       - Blot input
	RUN2       - Fastq input
	RUN3       - Graph input
*/


/***************************************************************/
/***************************************************************/
/***************************************************************/


#if defined(TRANSLATOR)
XCOMM
XCOMM Run the code, --
XCOMM
if (${no_exe} != "true") then
XCOMM  Check for local copy of executable, run if available.
	if (${local_exe} != "false") then
		if (${pipe_input} == "true") then
			${precommand} ${apr_opt} | ${local_exe} JOBNAME
		else
			${precommand} ${local_exe} JOBNAME
		endif
	else
		if (${pipe_input} == "true") then
			${precommand} ${apr_opt} | ${ACCESS}/bin/${codename} JOBNAME
		else
			${precommand} ${ACCESS}/bin/${codename} JOBNAME
		endif
	endif
endif
#endif


/***************************************************************/
/***************************************************************/
/***************************************************************/


#if defined(GRAPHICS)

#if defined(RUN2)
/* Run program */
XCOMM Run the code, --
if (${no_exe} != "true") then
XCOMM  Check for local copy of executable, run if available.
	if (${local_exe} != "false") then
		if (${batch} == "true") then
			${precommand} ${local_exe} </dev/null
		else
			${precommand} ${local_exe}
		endif
	else
		if (${batch} == "true") then
			${precommand} ${EXEC_DIR}/${codename}.${device} </dev/null  
		else
			if (${pipe_input} == "true") then
				${precommand} | ${EXEC_DIR}/${codename}.${device}
			else
				${precommand} ${EXEC_DIR}/${codename}.${device}  
			endif
		endif
	endif
endif
#endif

#if defined(RUN1) || defined(RUN3)
/* Run program */
XCOMM Run the code, --
if (${no_exe} != "true") then
XCOMM  Check for local copy of executable, run if available.
	if (${local_exe} != "false") then
		${precommand} ${local_exe}
	else
		if (${pipe_input} == "true") then
			${precommand} | ${EXEC_DIR}/${codename}.${device}
		else
			${precommand} ${EXEC_DIR}/${codename}.${device}  
		endif
	endif
endif
#endif /* RUN1 || RUN3 */

#if defined(RUN1)
if (${filename} != "" && ${device} != "dual" && ${device} != "xcps") then
	if (${device} == "met") mv cgimet1     ${filename}.met
	if (${device} == "cps") mv vdicps.ps   ${filename}.ps
	if (${device} == "eps") mv cgiout.epsi ${filename}.eps
	if (${device} == "pst") mv cgiout1     ${filename}.ps
endif
#endif /* RUN1 */


#endif /* GRAPHICS */


/***************************************************************/
/***************************************************************/
/***************************************************************/


#if defined(ANALYSIS)

XCOMM Set the runcommand to the debug command if running in debug:
if (${rundebug} == "true") then
XCOMM Run executable in debug
        switch(${parallel})
        case 0:
                if (${?user_debugcmd}) then
                    set debugcmd = "${user_debugcmd}"
                else
                    set debugcmd = _SERIALDEBUGCMD
                endif
                breaksw
        case 1:
        case 2:
                if (${?user_debugcmd}) then
		   if ("${user_debugcmd}" ==                                                             "`echo ${user_debugcmd} | sed 's/ .*//'`") then
XCOMM user entered only single word for the debug command
#if _GM_LIBS==1
		      if ("`echo ${runcommand} |                                                            sed 's/.*mpirun.ch_gm.*/mpirun.ch_gm/'`" == "mpirun.ch_gm") then
	                 if ("${user_debugcmd}" == "dbx") then
		            set user_debugcmd = `echo  ${runcommand} |                                            sed "s/mpirun.ch_gm/mpirun.ch_gm -dbx/"`
	                 endif
   	                 if ("${user_debugcmd}" == "totalview") then
		            set user_debugcmd = `echo ${runcommand} |                                             sed "s/mpirun.ch_gm/mpirun.ch_gm -totalview/"`
#else
		      if ("`echo ${runcommand} |                                                            sed 's/.*mpirun.*/mpirun/'`" == "mpirun") then
	                 if ("${user_debugcmd}" == "dbx") then
		            set user_debugcmd = `echo  ${runcommand} |                                            sed "s/mpirun/mpirun -dbx/"`
	                 endif
	                 if ("${user_debugcmd}" == "ladebug") then
		            set user_debugcmd = `echo ${runcommand} |                                             sed "s/mpirun/mpirun -ladebug/"`
	                 endif
   	                 if ("${user_debugcmd}" == "totalview") then
#if defined(DEC)
		            set user_debugcmd = `echo ${runcommand} |                                             sed "s/prun/totalview prun/"`
#else
		            set user_debugcmd = `echo ${runcommand} |                                             sed "s/mpirun/mpirun -tv/"`
#endif
#endif			    
	                 endif
		      endif 	
	           endif
                   set debugcmd = "${user_debugcmd}"
                else
                   if (${num_procs} <= 1) then
XCOMM Run debug on single processor - serial
                      set debugcmd = _PARDEBUGCMDSNGL
                   else
XCOMM Run debug on multiple processors - parallel
                      set debugcmd = _PARDEBUGCMDMULT
                   endif
                endif
/* The variable 'procs' is actually only used for parallel == 2 */
XCOMM If num_procs <= 1 and the debugger is ddd then leave procs empty 
		if (${num_procs} <= 1 && ("`echo ${debugcmd} | sed 's/^ddd//'`" != "${debugcmd}")) then
		   set procs = " "
		endif
		breaksw
	default:
                echo "Error: not allowed parallel setting"
                goto usage
                breaksw
  	endsw
#if _PARALLEL==1
#if defined(XCODE)
	if ("${debugcmd}" == "dxladebug" || \
    	    "${debugcmd}" == "debug" || \
    	    "${debugcmd}" == "xdebug") then
   	   set runcommand = "${debugcmd}"
	else
   	   set runcommand = "${debugcmd} -I${include}"
	endif
#else
	set runcommand = "${debugcmd}"
#endif
#else
	set runcommand = "${debugcmd}"
#endif
	echo "Debugging with ${runcommand}"
endif /* ${rundebug} == "true" */
XCOMM Run the code
#if defined(ISPARALLEL)

#if (_SET_GLOBMEM == 1)
XCOMM Testing the following to see if fixes a "xx_shmalloc: returning NULL" error.  GDS 11/08/2004
setenv P4_GLOBMEMSIZE 8000000
#endif				

#if _PASS_ENVIRONMENT == 1
/*
 * Rogue/Renegade seem to need TOTALVIEWs environment variable defined to
 * debug correctly. Do it here to avoid problems with user defining in
 * wrong place.  Also need to pass the LM_LICENSE_FILE setting. This
 * should also be done by user, but will do here to avoid
 * problems.
 */
setenv TOTALVIEW /usr/local/totalview/toolworks/totalview.6.4.0-1/bin/totalview

/* The mpirun command used for the GM communication on the cluster 
 * does not pass environment variables by default. They must all be passed
 * on the command line
 */
#if defined(JAS3D) || defined(XJAS3D)
set runcommand="$runcommand FOR005=$FOR005 FOR006=$FOR006 FOR009=$FOR009 FOR010=$FOR010 FOR011=$FOR011 FOR013=$FOR013 FOR015=$FOR015 FOR016=$FOR016 FOR030=$FOR030 FOR032=$FOR032 FOR062=$FOR062 FOR063=$FOR063 LM_LICENSE_FILE=17050@sass1411"
#endif
#if defined(PRONTO3D) || defined(XPRONTO3D) || defined(PRONTO3D94)
set runcommand="$runcommand FOR005=$FOR005 FOR006=$FOR006 FOR007=$FOR007 FOR009=$FOR009 FOR011=$FOR011 FOR013=$FOR013 FOR015=$FOR015 FOR016=$FOR016 FOR030=$FOR030 FOR032=$FOR032 FOR038=$FOR038 FOR039=$FOR039 FOR056=$FOR056 FOR062=$FOR062 FOR063=$FOR063 LM_LICENSE_FILE=17050@sass1411"
#endif
#endif	
XCOMM Analysis code may be executed in serial or parallel.
if (${no_exe} != "true") then

XCOMM Check for local copy of executable
	if (${local_exe} != "false") then
XCOMM RUN LOCAL EXECUTABLE
		if (-e ${local_exe} && -x ${local_exe}) then
			switch(${parallel})
			case 0:
				echo "Executable: ${local_exe}"
				set curdir = `pwd`
				if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
				rm ${curdir}/shutdown.${codename}.* >& /dev/null
				rm ${curdir}/recover.${codename}.* >& /dev/null
				if ("${shutdown_time}" != "") then
			   	   cat >! ${curdir}/shutdown.${codename}.$$ << EOF
XCOMM!/bin/sh
echo \$\$ > ${curdir}/shutdown.${codename}.pid.$$
sleep ${shutdown_time}
touch ${FOR063}
EOF
				   chmod +x ${curdir}/shutdown.${codename}.$$
#if defined (COUGAR)
				   csh ${curdir}/shutdown.${codename}.$$ &
#else
				   ${curdir}/shutdown.${codename}.$$ &
#endif
				endif
				${timenicecmd} ${runcommand} ${local_exe}
				if ("${shutdown_time}" != "" && ! -e ${curdir}/${FOR063}) then
				   echo "${codename} finished before shutdown time -> terminating shutdown job..."
				   set ShutdownId = `cat ${curdir}/shutdown.${codename}.pid.$$` 
				   set SleepId = `ps -f -u ${USER} | grep ${ShutdownId} | grep sleep | grep -v grep | grep -v sed | sed "s/${ShutdownId}.*//" | sed "s/${USER}//"`
				   kill -9 ${ShutdownId} >& /dev/null
				   kill -9 ${SleepId} >& /dev/null
				endif
				if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
				rm ${curdir}/shutdown.${codename}.* >& /dev/null
				breaksw
			case 1:
/* Run local executable in parallel -  janus or paragon */
XCOMM Run local executable in parallel on janus or paragon
				set precommand = "${runcommand} ${npcmd} ${num_procs} ${yod_options}"
				set curdir = `pwd`
				if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
				rm ${curdir}/shutdown.${codename}.* >& /dev/null
				rm ${curdir}/recover.${codename}.* >& /dev/null
				if ("${shutdown_time}" != "") then
			   	   cat >! ${curdir}/shutdown.${codename}.$$ << EOF
XCOMM!/bin/sh
echo \$\$ > ${curdir}/shutdown.${codename}.pid.$$
sleep ${shutdown_time}
touch ${FOR063}
EOF
				   chmod +x ${curdir}/shutdown.${codename}.$$
#if defined (COUGAR)
				   csh ${curdir}/shutdown.${codename}.$$ &
#else
				   ${curdir}/shutdown.${codename}.$$ &
#endif
				endif
				${precommand} ${local_exe}
				if ("${shutdown_time}" != "" && ! -e ${curdir}/${FOR063}) then
				   echo "${codename} finished before shutdown time -> terminating shutdown job..."
				   set ShutdownId = `cat ${curdir}/shutdown.${codename}.pid.$$` 
				   set SleepId = `ps -f -u ${USER} | grep ${ShutdownId} | grep sleep | grep -v grep | grep -v sed | sed "s/${ShutdownId}.*//" | sed "s/${USER}//"`
				   kill -9 ${ShutdownId} >& /dev/null
				   kill -9 ${SleepId} >& /dev/null
				endif
				if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
				rm ${curdir}/shutdown.${codename}.* >& /dev/null
				breaksw
			case 2:
				set curdir = `pwd`
				rm ${curdir}/${codename}.ln.* >& /dev/null
/* Run local executable in parallel -  cray, sun, dec, sgi */
				if (! ${?procs}) then
#if (defined(SUN) && _NEEDS_HOSTFILE == 1)
				   set procs = "${hfcmd} ${hosts_file}"
#elif defined(DEC)
				   set procs = "${npcmd} ${num_procs} ${hfcmd} ${hosts_file}"
#elif (defined(linux) && _NEEDS_HOSTFILE == 1)
				   set procs = "${npcmd} ${num_procs} ${hfcmd} ${hosts_file}"
#else
				   set procs = "${npcmd} ${num_procs}"
#endif
				endif
				set precommand = "${runcommand} ${procs}"
				set mpcodename = ${codename}.ln.`echo $$`
#if defined(SUN)
				if (! -e ${HOME}/.rhosts) then
				   set local_host = `hostname`
			   	   echo ""; echo "Warning: If code does not execute, create the file ~/.rhosts"
				   echo "         and include ${local_host}."; echo ""
				else
				   set local_host = `hostname`
				   grep ${local_host} ${HOME}/.rhosts >& /dev/null
				   if (${status} != 0) then 
				      echo ""; echo "Warning: If code does not execute, add ${local_host} to your ~/.rhosts file."; echo ""
				   endif
				endif
				if (${serial} == "true") then
				cat >! ${hosts_file} << EOF
`hostname` 0 `pwd`/${codename_hf}
EOF
				else  /* script called with -parallel flag */
				   set number_of_processors = ${num_procs}
				   if (${number_of_processors} < 1) then
				      echo "Error: number of processors < 1"
				      goto usage
				   endif
			  	   if (${user_hf} == "" && ! -e ${curdir}/${basename_hf}) then
				      cat >! ${hosts_file} << EOF
`hostname` 0 `pwd`/${codename_hf}
EOF
				      @ number_of_processors--
				      while (${number_of_processors})
					 cat >> ${hosts_file} << EOF
`hostname` 1 `pwd`/${codename_hf}
EOF
					 @ number_of_processors--
				      end
				   else
				      if (${user_hf} == "") \
					 set user_hf = ${curdir}/${basename_hf}
				      sed '/^$/d' ${user_hf} >! ${tmp_hf}
				      set number_of_lines = \
`wc ${tmp_hf} | awk '{print substr($1,0)}'`
				      if (${number_of_lines} != \
					 ${number_of_processors}) then
					 echo "Error: # of processors and # of hosts in the hosts file do not match"
					 goto usage
				      endif
				      sed -n '1p' ${tmp_hf} | \
sed -e s%\$%" 0 "`pwd`"/""${codename_hf}"% >! ${hosts_file}
				      sed -n '2,${p}' ${tmp_hf} | \
sed -e s%\$%" 1 "`pwd`"/""${codename_hf}"% >> ${hosts_file}
				      rm -f ${tmp_hf}
				   endif
				endif
				if (-e ${curdir}/${codename_hf}) \
				   rm -f ${curdir}/${codename_hf}
				_LNCMD ${local_exe} ${curdir}/${codename_hf}
#endif /* defined(SUN) */
#if defined(DEC) || (defined(linux) && _NEEDS_HOSTFILE == 1)
				if (${serial} == "true") then
				   cat >! ${hosts_file} << EOF
`hostname`
EOF
				else  /* script called with -parallel flag */
			  	   if (${user_hf} == "" && ! -e ${curdir}/${basename_hf}) then
				      cat >! ${hosts_file} << EOF
`hostname`
EOF
				   else
				      if (${user_hf} == "") \
					 set user_hf = ${curdir}/${basename_hf}
				      sed '/^$/d' ${user_hf} >! ${hosts_file}
				   endif
				endif
#endif /* defined(DEC) */
#if defined(SUN) || defined(DEC) || (defined(linux) && _NEEDS_HOSTFILE == 1)
				if (${user_hf} == "") then
				   echo "Hosts File                  = supplied by the script"
				else
				   echo "Hosts File                  = ${user_hf}"
				   set local_host = `hostname`
				   head -1 ${user_hf} |grep ${local_host} >& /dev/null
				   if (${status} != 0) then
				      echo "Error: The first host listed in the hosts file needs to"
				      echo "get changed to the local host. Exiting script..."
				      exit
				   endif
				endif
#endif /* defined(SUN) || defined(DEC) */
				_LNCMD ${local_exe} ${curdir}/${mpcodename}
				if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
				rm ${curdir}/shutdown.${codename}.* >& /dev/null
				rm ${curdir}/recover.${codename}.* >& /dev/null
				if ("${shutdown_time}" != "") then
			   	   cat >! ${curdir}/shutdown.${codename}.$$ << EOF
XCOMM!/bin/sh
echo \$\$ > ${curdir}/shutdown.${codename}.pid.$$
sleep ${shutdown_time}
touch ${FOR063}
EOF
				   chmod +x ${curdir}/shutdown.${codename}.$$
#if defined (COUGAR)
				   csh ${curdir}/shutdown.${codename}.$$ &
#else
				   ${curdir}/shutdown.${codename}.$$ &
#endif
				endif
#if defined(SGI)
				${timenicecmd} ${precommand} ${curdir}/${mpcodename} < /dev/null
#else
				${timenicecmd} ${precommand} ${curdir}/${mpcodename}
#endif
				if ("${shutdown_time}" != "" && ! -e ${curdir}/${FOR063}) then
				   echo "${codename} finished before shutdown time -> terminating shutdown job..."
				   set ShutdownId = `cat ${curdir}/shutdown.${codename}.pid.$$` 
				   set SleepId = `ps -f -u ${USER} | grep ${ShutdownId} | grep sleep | grep -v grep | grep -v sed | sed "s/${ShutdownId}.*//" | sed "s/${USER}//"`
				   kill -9 ${ShutdownId} >& /dev/null
				   kill -9 ${SleepId} >& /dev/null
				endif
				if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
				rm ${curdir}/shutdown.${codename}.* >& /dev/null
				rm ${mpcodename}
#if defined(SUN)
				rm ${curdir}/${codename_hf}
				rm ${hosts_file}
#endif 
#if defined(DEC) || (defined(linux) && _NEEDS_HOSTFILE == 1)
				rm ${hosts_file}
#endif
				breaksw
			default:
				echo "Error: parallel not set"
				goto usage
				breaksw
			endsw
		else
			echo "ERROR: ${local_exe} does not exist, or is not executable"
			goto usage
		endif
	else /* local_exe == "false" */
XCOMM RUN INSTALLED EXECUTABLES
		switch(${parallel})
		case 0:
			set curdir = `pwd`
			if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
			rm ${curdir}/shutdown.${codename}.* >& /dev/null
			rm ${curdir}/recover.${codename}.* >& /dev/null
			if ("${shutdown_time}" != "") then
			   cat >! ${curdir}/shutdown.${codename}.$$ << EOF
XCOMM!/bin/sh
echo \$\$ > ${curdir}/shutdown.${codename}.pid.$$
sleep ${shutdown_time}
touch ${FOR063}
EOF
			   chmod +x ${curdir}/shutdown.${codename}.$$
#if defined (COUGAR)
			   csh ${curdir}/shutdown.${codename}.$$ &
#else
			   ${curdir}/shutdown.${codename}.$$ &
#endif
			endif
#if defined(XCODE)
			if (${rundebug} == "false") then
				${timenicecmd} ${runcommand} ${ACCESS}/XACCESS/analysis/${codename}/${codename}
			else
				${runcommand} ${ACCESS}/XACCESS/analysis/${codename}'_dbg'/${codename}
			endif
#else
			${timenicecmd} ${runcommand} ${ACCESS}/bin/${codename}
#endif
			if ("${shutdown_time}" != "" && ! -e ${curdir}/${FOR063}) then
			   echo "${codename} finished before shutdown time -> terminating shutdown job..."
			   set ShutdownId = `cat ${curdir}/shutdown.${codename}.pid.$$` 
			   set SleepId = `ps -f -u ${USER} | grep ${ShutdownId} | grep sleep | grep -v grep | grep -v sed | sed "s/${ShutdownId}.*//" | sed "s/${USER}//"`
			   kill -9 ${ShutdownId} >& /dev/null
			   kill -9 ${SleepId} >& /dev/null
			endif
			if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
			rm ${curdir}/shutdown.${codename}.* >& /dev/null
			breaksw
		case 1:
#if defined(XCODE)
			if (${rundebug} == "false") then
				set exec_code = ${ACCESS}/XACCESS/analysis/${codename}/${codename} 
			else
				set exec_code = ${ACCESS}/XACCESS/analysis/${codename}'_dbg'/${codename} 
			endif
#else
			set exec_code = ${ACCESS}/bin/${codename}
#endif
			set precommand = "${runcommand} ${npcmd} ${num_procs} ${yod_options}"
			set curdir = `pwd`
			if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
			rm ${curdir}/shutdown.${codename}.* >& /dev/null
       			rm ${curdir}/recover.${codename}.* >& /dev/null
			if ("${shutdown_time}" != "") then
			   cat >! ${curdir}/shutdown.${codename}.$$ << EOF
XCOMM!/bin/sh
echo \$\$ > ${curdir}/shutdown.${codename}.pid.$$
sleep ${shutdown_time}
touch ${FOR063}
EOF
			   chmod +x ${curdir}/shutdown.${codename}.$$
#if defined (COUGAR)
			   csh ${curdir}/shutdown.${codename}.$$ &
#else
			   ${curdir}/shutdown.${codename}.$$ &
#endif
			endif
			${precommand} ${exec_code}
			if ("${shutdown_time}" != "" && ! -e ${curdir}/${FOR063}) then
			   echo "${codename} finished before shutdown time -> terminating shutdown job..."
			   set ShutdownId = `cat ${curdir}/shutdown.${codename}.pid.$$` 
			   set SleepId = `ps -f -u ${USER} | grep ${ShutdownId} | grep sleep | grep -v grep | grep -v sed | sed "s/${ShutdownId}.*//" | sed "s/${USER}//"`
			   kill -9 ${ShutdownId} >& /dev/null
			   kill -9 ${SleepId} >& /dev/null
			endif
			if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
			rm ${curdir}/shutdown.${codename}.* >& /dev/null
			breaksw
		case 2:
			set curdir = `pwd`
			rm ${curdir}/${codename}.ln.* >& /dev/null
/* Run codes on mp cray, dec, sgi, sun with 1 processor*/
			if (! ${?procs}) then
#if (defined(SUN) && _NEEDS_HOSTFILE == 1)
			   set procs = "${hfcmd} ${hosts_file}"
#elif defined(DEC)
			   set procs = "${npcmd} ${num_procs} ${hfcmd} ${hosts_file}"
#elif (defined(linux) && _NEEDS_HOSTFILE == 1)
			   set procs = "${npcmd} ${num_procs} ${hfcmd} ${hosts_file}"
#else
			   set procs = "${npcmd} ${num_procs}"
#endif
			endif
			set precommand = "${runcommand} ${procs}"
			set mpcodename = ${codename}.ln.`echo $$`
#if defined(SUN)
			if (! -e ${HOME}/.rhosts) then
			   set local_host = `hostname`
			   echo ""; echo "Warning: If code does not execute, create the file ~/.rhosts"
			   echo "         and include ${local_host}."; echo ""
			else
			   set local_host = `hostname`
			   grep ${local_host} ${HOME}/.rhosts >& /dev/null
			   if (${status} != 0) then
			      echo ""; echo "Warning: If code does not execute, add ${local_host} to your ~/.rhosts file."; echo ""
			   endif
			endif
			if (${serial} == "true") then
			   cat >! ${hosts_file} << EOF
`hostname` 0 `pwd`/${codename_hf}
EOF
			else  /* script called with -parallel flag */
			   set number_of_processors = ${num_procs}
			   if (${number_of_processors} < 1) then
			      echo "Error: number of processors < 1"
			      goto usage
			   endif
			   if (${user_hf} == "" && ! -e ${curdir}/${basename_hf}) then
			      cat >! ${hosts_file} << EOF
`hostname` 0 `pwd`/${codename_hf}
EOF
			      @ number_of_processors--
			      while (${number_of_processors})
				 cat >> ${hosts_file} << EOF
`hostname` 1 `pwd`/${codename_hf}
EOF
				 @ number_of_processors--
			      end
			   else
			      if (${user_hf} == "") \
				 set user_hf = ${curdir}/${basename_hf}
			      sed '/^$/d' ${user_hf} >! ${tmp_hf}
			      set number_of_lines = \
`wc ${tmp_hf} | awk '{print substr($1,0)}'`
			      if (${number_of_lines} != \
				  ${number_of_processors}) then 
				 echo "Error: # of processors and # of hosts in the hosts file do not match"
				 goto usage
			      endif
			      sed -n '1p' ${tmp_hf} | \
sed -e s%\$%" 0 "`pwd`"/""${codename_hf}"% >! ${hosts_file}
			      sed -n '2,${p}' ${tmp_hf} | \
sed -e s%\$%" 1 "`pwd`"/""${codename_hf}"% >> ${hosts_file}
			      rm -f ${tmp_hf}
			   endif
			endif
			if (-e ${curdir}/${codename_hf}) \
			   rm -f ${curdir}/${codename_hf}
#endif /* defined(SUN) */
#if defined(DEC) || (defined(linux) && _NEEDS_HOSTFILE == 1)
			if (${serial} == "true") then
			   cat >! ${hosts_file} << EOF
`hostname`
EOF
			else  /* script called with -parallel flag */
			   if (${user_hf} == "" && ! -e ${curdir}/${basename_hf}) then
			      cat >! ${hosts_file} << EOF
`hostname`
EOF
			   else
			      if (${user_hf} == "") \
				 set user_hf = ${curdir}/${basename_hf}
			      sed '/^$/d' ${user_hf} >! ${hosts_file}
			   endif
			endif
#endif /* defined(DEC) */
#if defined(SUN) || defined(DEC) || (defined(linux) && _NEEDS_HOSTFILE == 1)
			if (${user_hf} == "") then
			   echo "Hosts File                  = supplied by the script"
			else
			   echo "Hosts File                  = ${user_hf}"
			   set local_host = `hostname`
			   head -1 ${user_hf} | grep ${local_host} >& /dev/null
			   if (${status} != 0) then
			      echo "The first host listed in the hosts file needs to"
			      echo "get changed to the local host. Exiting script..."
			      exit
			   endif
			endif
#endif /* defined(SUN) || defined(DEC) */
#if defined(XCODE)
			if (${rundebug} == "false") then
				_LNCMD ${ACCESS}/XACCESS/analysis/${codename}/${codename} ${curdir}/${mpcodename}
#if defined(SUN)
				_LNCMD ${ACCESS}/XACCESS/analysis/${codename}/${codename} ${curdir}/${codename_hf}
#endif
			else
				_LNCMD ${ACCESS}/XACCESS/analysis/${codename}'_dbg'/${codename} ${curdir}/${mpcodename}
#if defined(SUN)
				_LNCMD ${ACCESS}/XACCESS/analysis/${codename}'_dbg'/${codename} ${curdir}/${codename_hf}
#endif
			endif
#else
			_LNCMD ${ACCESS}/bin/${codename} ${curdir}/${mpcodename}
#if defined(SUN)
			_LNCMD ${ACCESS}/bin/${codename} ${curdir}/${codename_hf}
#endif
#endif
			if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
			rm ${curdir}/shutdown.${codename}.* >& /dev/null
			rm ${curdir}/recover.${codename}.* >& /dev/null
			if ("${shutdown_time}" != "") then
			   cat >! ${curdir}/shutdown.${codename}.$$ << EOF
XCOMM!/bin/sh
echo \$\$ > ${curdir}/shutdown.${codename}.pid.$$
sleep ${shutdown_time}
touch ${FOR063}
EOF
			   chmod +x ${curdir}/shutdown.${codename}.$$
#if defined (COUGAR)
			   csh ${curdir}/shutdown.${codename}.$$ &
#else
			   ${curdir}/shutdown.${codename}.$$ &
#endif
			endif
#if defined(SGI)
			${timenicecmd} ${precommand} ${curdir}/${mpcodename} < /dev/null
#else
			${timenicecmd} ${precommand} ${curdir}/${mpcodename}
#endif
			if ("${shutdown_time}" != "" && ! -e ${curdir}/${FOR063}) then
			   echo "${codename} finished before shutdown time -> terminating shutdown job..."
			   set ShutdownId = `cat ${curdir}/shutdown.${codename}.pid.$$` 
			   set SleepId = `ps -f -u ${USER} | grep ${ShutdownId} | grep sleep | grep -v grep | grep -v sed | sed "s/${ShutdownId}.*//" | sed "s/${USER}//"`
			   kill -9 ${ShutdownId} >& /dev/null
			   kill -9 ${SleepId} >& /dev/null
			endif
			if (-e ${curdir}/${FOR063}) rm ${curdir}/${FOR063}
			rm ${curdir}/shutdown.${codename}.* >& /dev/null
			rm ${mpcodename}
#if defined(SUN)
			rm ${curdir}/${codename_hf}
			rm ${hosts_file}
#endif 
#if defined(DEC) || (defined(linux) && _NEEDS_HOSTFILE == 1)
			rm ${hosts_file}
#endif
			breaksw
		default:
			echo "Error: parallel not set"
			goto usage
			breaksw
		endsw

	endif /* end local_exe == "false" */
endif /* end !no_exe */
#else	/* !defined ISPARALLEL */
if (${no_exe} != "true") then
XCOMM Check for local copy of executable
	if (${local_exe} != "false") then
		if (-e ${local_exe} && -x ${local_exe}) then
			echo "Executable: ${local_exe}"
			${runcommand} ${local_exe}
		else
			echo "ERROR: ${local_exe} does not exist, or is not executable"
			set errflg = 1
			goto usage
		endif
	else /* !local_exe */
#if !defined(XCODE)
		${runcommand} ${ACCESS}/bin/${codename}
#else
		if (${rundebug} == "true") then
		   ${runcommand} ${ACCESS}/XACCESS/analysis/${codename}'_dbg'/${codename}
		else
		   ${runcommand} ${ACCESS}/XACCESS/analysis/${codename}/${codename}
		endif
#endif
	endif
endif       /* no_exe    */
#endif


#endif /* ANALYSIS */


/***************************************************************/
/***************************************************************/
/***************************************************************/

