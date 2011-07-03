XCOMM!/bin/csh -f
set USAGE = "Usage: post filename device"
set codename = post
set i_made_exe 	= "false"        
set UseInstalledCode = "yes"

XCOMM Store temporary files in TEMP
set TEMP = _EXETMP

XCOMM Determine ACCESS based on the location of the script
if ("`echo $0 | sed 's/\///'`" != "$0") then
XCOMM Location of the script found
   if ("`ls -l $0 | sed 's/^l.*/l/'`" != "l") then
XCOMM Script is not a softlink
      setenv ACCESS `echo $0 | sed 's/\/etc\/.*//'`
      if (! -e $ACCESS/config/site.def) then
         setenv ACCESS _ACCESSDIR
      endif
   else
XCOMM Script is a softlink
      setenv ACCESS _ACCESSDIR
   endif
else
XCOMM Determining the location of the script failed, use default values
   setenv ACCESS _ACCESSDIR
endif

if ($#argv < 2) then
        goto usage
endif

setenv file55 $argv[1]
if (!(-e $file55)) then
	echo "File $file55 does not exist"
        goto usage
endif

set device = $argv[2]

XCOMM Check if there is another option following the device type. 
XCOMM If there is, use it as the option number to the cps/eps device.
if ($#argv == 3 && ($device == "cps" || $device == "eps")) then
	set devopt = $argv[3]
else
	set devopt = "false"
endif

XCOMM Have a valid device code, now check if we have an executable
if (!(-x $ACCESS/bin/$codename.$device)) then
   echo "ERROR: ${ACCESS}/bin/${codename}.${device} does not exist."
   echo "       Contact seacas@sandia.gov if you need this device;"
   echo "       otherwise use one of the supported devices:"
   goto usage
endif

XCOMM Tell users what to do
if ($device == "x11") then
	echo "Place cursor in window and hit space bar to progress through frames"
endif

XCOMM Run the code, --
if ($devopt != "false") then
   echo $devopt | $ACCESS/bin/$codename.$device
else
   $ACCESS/bin/$codename.$device
endif

XCOMM Echo audit information to $ACCESS/etc/audit.log
time > $TEMP/time.$$
set time_used=`cat $TEMP/time.$$`; rm $TEMP/time.$$
if (-w $ACCESS/etc/audit.log) then
	if ($?USER) then
		echo $codename.$device $USER `date` $time_used `hostname` >>$ACCESS/etc/audit.log
	else
		if ($?LOGNAME) then
			echo $codename.$device $LOGNAME `date` $time_used `hostname` >>$ACCESS/etc/audit.log
		else
			echo $codename.$device "UNKNOWN" `date` $time_used `hostname` >>$ACCESS/etc/audit.log
		endif
	endif
endif

exit

usage:
   echo $USAGE		  
   set tempA=`ls ${ACCESS}/bin/${codename}.* |grep -v ${codename}.o |sed -e s:${ACCESS}/bin/${codename}.::`
   echo " "
   echo "Standard options for 'device':" 
   echo "	" $tempA
   echo " "
exit    
   
 
