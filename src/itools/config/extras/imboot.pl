#!$(PERLPATH)

#  imboot - imake bootstrapper (bootstrap Makefile from Imakefile)

# Usage:  imboot [ flags ] [ topdir [ curdir ] ]

# flags can be:
#	-c name		specify name of configuration files
#	-C name		specify name of configuration files, extensible
#			architecture
#	-Danything	pass -Danything to imake
#	-Ianything	pass -Ianything to imake
#	-d		force UseInstalled on
#	-u		force UseInstalled off

# topdir = path to top of project tree (default ".")
# curdir = path to current directory from top of project tree (default ".")

# If no -c name or -C name option is given, configuration files from
# the config directory under the project root (i.e., from topdir/config)
# are used.  UseInstalled is not defined.

# If -c name is given, configuration files from "/usr/local/lib/config"/name
# are used instead of the files in topdir/config.  UseInstalled is defined.

# If -c name is given, configuration files from "/usr/local/lib/config"/name
# are used in conjunction with the files in topdir/config.  UseInstalled
# is defined.

# For -c or -C, if name is an absolute path, "/usr/local/lib/config" is not
# prepended.

# You can also specify other -D and -I flags, to allow arbitrary
# symbols to be defined or undefined, or other search directories to
# be given.

# The -d and -u options may be used to force UseInstalled on or off,
# overriding the default behavior.

# 11 Apr 93 Paul DuBois dubois@primate.wisc.edu

# 11 Apr 93 V1.00
# - Created.
# 02 Jun 93 V1.01
# - Bug fixes.
# 02 Mar 96 V1.02
# - Modified to check first for top/config/cf, then top/config if
#   within-project configuration files are used.  This is allows
#   imboot to work within the X11R6 source tree, which changed the
#   location of the configuration files relative to earlier X11 releases.
# 31 Mar 97 V1.03
# - Modified to work under Windows NT

($prog = $0) =~ s|.*/||;	# get script name for messages

$usage="usage: $prog [-c|-C name] [-d] [-u] [-Dsym] [-Idir] [topdir [curdir]]";

$configrootdir = "$(CONFIGROOTDIR)";

$configname = "";
$topdir = ".";
$curdir = ".";
$dopt = "n";
$uopt = "n";
$useboth = "n";
$args = "";

# check for -c, -C, -D, -I, -d, -u options

while (@ARGV && $ARGV[0] =~ /^-/)
{
	$_ = shift (@ARGV);
	if (/^-[cC]$/)
	{
		die "$usage\n" unless @ARGV;
		$configname = shift (@ARGV);
		$useboth = "y" if $_ eq "-C";
	}
	elsif ($_ eq "-d")	# force UseInstalled on unconditionally
	{
		$dopt = "y";
		$uopt = "n";
	}
	elsif ($_ eq "-u")	# force UseInstalled off unconditionally
	{
		$dopt = "n";
		$uopt = "y";
	}
	elsif (/^-[DI]/)	# pass through -D, -I options
	{
		$args .= " $_";
	}
	else			# unrecognized flag
	{
		die "$prog: unknown flag: $_\n$usage\n";
	}
}

$topdir = shift (@ARGV) if @ARGV;
$curdir = shift (@ARGV) if @ARGV;

# find within-project configuration file directory, if there is one

if (-d "$topdir/config/cf")
{
	$wpconfigdir = "$topdir/config/cf";
}
else
{
	$wpconfigdir = "$topdir/config";
}

if ($configname eq "")
{
	$useinstalled = "";
	$configdir = "-I$wpconfigdir";
}
elsif ($useboth ne "y")
{
	$useinstalled = "-DUseInstalled";
	if ($configname =~ m|^/\.|)
	{
		$configdir = "-I$configname";
	}
	else
	{
		$configdir = "-I$configrootdir/$configname";
	}
}
else
{
	$useinstalled = "-DUseInstalled";
	if ($configname =~ m|^/\.|)
	{
		$configdir = "-I$wpconfigdir -I$configname";
	}
	else
	{
		$configdir = "-I$wpconfigdir -I$configrootdir/$configname";
	}
}

# force UseInstalled on/off if -d/-u were given

$useinstalled = "-DUseInstalled" if $dopt eq "y";
$useinstalled = "" if $uopt eq "y";

# back up current Makefile if it exists

if (-f "Makefile")
{
	print "rm Makefile.bak\n";
	unlink "Makefile.bak";
	print "mv Makefile Makefile.bak\n";
	rename ("Makefile", "Makefile.bak");
}

$cmd = "imake $args $useinstalled $configdir -DTOPDIR=$topdir -DCURDIR=$curdir";

# echo command, then run it

print "$cmd\n";
system "$cmd";
