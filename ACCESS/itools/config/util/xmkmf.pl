#!$(PERLPATH)

#
# xmkmf - make a Makefile from an Imakefile from inside or outside the sources
# 
# Based on:
# $XConsortium: xmkmf.cpp /main/22 1996/09/28 16:17:05 rws $
# Rewritten in perl by:
# Paul DuBois
# dubois@primate.wisc.edu


# 30 Mar 97 V1.00
# - Created.

($prog = $0) =~ s|.*/||;	# get script name for messages

$usage = "Usage: $prog [-a] [top_of_sources_pathname [current_directory]]";

$configdirspec = "-I$(CONFIGDIR)";
$make = "$(MAKE)";

# topdir = path to top of project tree
# curdir = path to current directory from top of project tree

$topdir = "";
$curdir = ".";

$do_all = 0;
$args = "";

# check for -a option

if (@ARGV && $ARGV[0] eq "-a")
{
	$do_all = 1;
	shift (@ARGV);
}

$topdir = shift (@ARGV) if @ARGV;	# first arg if present
$curdir = shift (@ARGV) if @ARGV;	# second arg if present
die "$usage\n" if @ARGV;		# should be no more

die "$usage\n" if $topdir =~ /^-/;

# back up current Makefile if it exists

if (-f "Makefile")
{
	print "rm Makefile.bak\n";
	unlink "Makefile.bak";
	print "mv Makefile Makefile.bak\n";
	rename ("Makefile", "Makefile.bak");
}

if ($topdir eq "")
{
	$args = "-DUseInstalled $configdirspec";
}
else
{
	$args = "-I$topdir/config/cf -DTOPDIR=$topdir -DCURDIR=$curdir";
}

# echo commands before running them

if ($do_all == 0)
{
	print "imake $args\n";
	system "imake $args";
}
else
{
	print "imake $args\n";
	system "imake $args";
	print "$make Makefiles\n";
	system "$make Makefiles";
	print "$make includes\n";
	system "$make includes";
	print "$make depend\n";
	system "$make depend";
}
