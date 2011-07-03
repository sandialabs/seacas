XCOMM!/bin/sh

XCOMM  imboot - imake bootstrapper (bootstrap Makefile from Imakefile)

XCOMM Usage:  imboot [ flags ] [ topdir [ curdir ] ]

XCOMM flags can be:
XCOMM	-c name		specify name of configuration files
XCOMM	-C name		specify name of configuration files, extensible
XCOMM			architecture
XCOMM	-Danything	pass -Danything to imake
XCOMM	-Ianything	pass -Ianything to imake
XCOMM	-d		force UseInstalled on
XCOMM	-u		force UseInstalled off

XCOMM topdir = path to top of project tree (default ".")
XCOMM curdir = path to current directory from top of project tree (default ".")

XCOMM If no -c name or -C name option is given, configuration files from
XCOMM the config directory under the project root (i.e., from topdir/config)
XCOMM are used.  UseInstalled is not defined.

XCOMM If -c name is given, configuration files from CONFIGROOTDIR/name
XCOMM are used instead of the files in topdir/config.  UseInstalled is defined.

XCOMM If -c name is given, configuration files from CONFIGROOTDIR/name
XCOMM are used in conjunction with the files in topdir/config.  UseInstalled
XCOMM is defined.

XCOMM For -c or -C, if name is an absolute path, CONFIGROOTDIR is not
XCOMM prepended.

XCOMM You can also specify other -D and -I flags, to allow arbitrary
XCOMM symbols to be defined or undefined, or other search directories to
XCOMM be given.

XCOMM The -d and -u options may be used to force UseInstalled on or off,
XCOMM overriding the default behavior.

XCOMM 11 Apr 93 Paul DuBois dubois@primate.wisc.edu

XCOMM 11 Apr 93 V1.00
XCOMM - Created.
XCOMM 02 Jun 93 V1.01
XCOMM - Bug fixes.
XCOMM 02 Mar 96 V1.02
XCOMM - Modified to check first for top/config/cf, then top/config if
XCOMM   within-project configuration files are used.  This is allows
XCOMM   imboot to work within the X11R6 source tree, which changed the
XCOMM   location of the configuration files relative to earlier X11 releases.

usage="usage: $0 [-c|-C name] [-d] [-u] [-Dsym] [-Idir] [topdir [curdir]]"

configrootdir=CONFIGROOTDIR
mv=MV
rm=RM
configname=
topdir=.
curdir=.
dopt=n
uopt=n
args=

XCOMM check for -c, -C, -D, -I, -d, -u options

while [ $# -gt 0 ]; do
	case "$1" in
		-c)
			if [ $# -lt 2 ]; then
				echo "$usage" 1>&2
				exit 1
			fi
			configname=$2
			shift;shift
			;;
		-C)
			if [ $# -lt 2 ]; then
				echo "$usage" 1>&2
				exit 1
			fi
			configname=$2
			useboth=y
			shift;shift
			;;
		-d)			# force UseInstalled on unconditionally
			dopt=y
			uopt=n
			shift
			;;
		-u)			# force UseInstalled off unconditionally
			dopt=n
			uopt=y
			shift
			;;
		-[DI]*)			# pass through -D, -I options
			args="$args $1"
			shift
			;;
		-*)			# unrecognized flag
			echo "imboot: unknown flag: $1"
			echo "$usage" 1>&2
			exit 1
			;;
		*)			# non-flag; quit flag processing
			break
			;;
	esac
done

if [ $# -gt 0 ]; then
	topdir="$1"
	shift
fi
if [ $# -gt 0 ]; then
	curdir="$1"
fi

XCOMM find within-project configuration file directory, if there is one
if [ -d "$topdir/config/cf" ]; then
	wpconfigdir="$topdir/config/cf"
else
	wpconfigdir="$topdir/config"
fi

if [ "$configname" = "" ]; then
	useinstalled=
	configdir="-I$wpconfigdir"
elif [ "$useboth" != "y" ]; then
	useinstalled=-DUseInstalled
	case "$configname" in
	/?*) configdir="-I$configname" ;;
	*) configdir="-I$configrootdir/$configname" ;;
	esac
else
	useinstalled=-DUseInstalled
	case "$configname" in
	/?*) configdir="-I$wpconfigdir -I$configname" ;;
	*) configdir="-I$wpconfigdir -I$configrootdir/$configname" ;;
	esac
fi

XCOMM force UseInstalled on/off if -d/-u were given
if [ "$dopt" = "y" ]; then
	useinstalled=-DUseInstalled
fi
if [ "$uopt" = "y" ]; then
	useinstalled=
fi

XCOMM backup current Makefile if it exists
if [ -f Makefile ]; then
	echo $rm Makefile.bak
	$rm Makefile.bak
	echo $mv Makefile Makefile.bak
	$mv Makefile Makefile.bak
fi

XCOMM echo command, then run it
echo imake $args $useinstalled $configdir -DTOPDIR=$topdir -DCURDIR=$curdir
imake $args $useinstalled $configdir -DTOPDIR=$topdir -DCURDIR=$curdir
