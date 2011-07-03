XCOMM!/bin/sh

XCOMM
XCOMM make a Makefile from an Imakefile from inside or outside the sources
XCOMM 
XCOMM $XConsortium: xmkmf.cpp /main/22 1996/09/28 16:17:05 rws $

usage="usage:  $0 [-a] [top_of_sources_pathname [current_directory]]"

configdirspec=CONFIGDIRSPEC
topdir=
defines=
extradefines=
curdir=.
do_all=
etc_dir=ETCDIR

#ifdef CrossCompiling
#ifdef crossredstorm
XCOMM FIX THIS
if [ "`hostname`" != "janus" -a "`hostname`" != "janus-s" ]; then
   ImakeExe=$etc_dir/imake
else
   ImakeExe=imake_cross
   RedstormAccessRoot=ACCESSDIR
   configdirspec="-I"$RedstormAccessRoot"/config"
   extradefines=-DUseCrossFiles
fi
#endif
#ifdef crosscougar
if [ "`hostname`" != "janus" -a "`hostname`" != "janus-s" ]; then
   ImakeExe=$etc_dir/imake
else
   ImakeExe=imake_cross
   TflopAccessRoot=TFLOPROOT
   configdirspec="-I"$TflopAccessRoot"/config"
   extradefines=-DUseCrossFiles
fi
#endif
#ifdef crosslinux
if [ "`hostname | sed 's/juneau//'`" = "`hostname`" ]; then
   ImakeExe=$etc_dir/imake
   CplantAccessRoot=CPLANTROOT
   configdirspec="-I"$CplantAccessRoot"/config"
   extradefines=-DUseCrossFiles
else
   ImakeExe=$etc_dir/imake
fi
#endif
#else
ImakeExe=$etc_dir/imake
#endif

case "$1" in
-a)
    do_all="yes"
    shift
    ;;
esac

XCOMM check for -C option (specify standard config directory)
if [ $# -gt 0 -a "$1" = -C ]; then
	if [ $# -lt 2 ]; then
		echo "$usage" 1>&2
		exit 1
	fi
	configdirspec="-I"$2
	shift;shift
fi

XCOMM check for -D option (specify other defines)
if [ $# -gt 0 -a "$1" = -D ]; then
	if [ $# -lt 2 ]; then
		echo "$usage" 1>&2
		exit 1
	fi
	defines=-D$2
	shift;shift
fi

case $# in 
    0) ;;
    1) topdir=$1 ;;
    2) topdir=$1  curdir=$2 ;;
    *) echo "$usage" 1>&2; exit 1 ;;
esac

case "$topdir" in
    -*) echo "$usage" 1>&2; exit 1 ;;
esac

if [ -f Makefile ]; then 
    echo mv -f Makefile Makefile.bak
    mv -f Makefile Makefile.bak
fi

if [ "$topdir" = "" ]; then
    args="-DUseInstalled $defines $extradefines $configdirspec"
else
    args="-I$topdir/config/cf $extradefines -DTOPDIR=$topdir -DCURDIR=$curdir"
fi

echo $ImakeExe $args
case "$do_all" in
yes)
    $ImakeExe $args && 
    echo "make Makefiles" &&
    make Makefiles &&
    echo "make includes" &&
    make includes &&
    echo "make depend" &&
    make depend
    ;;
*)
    $ImakeExe $args
    ;;
esac
