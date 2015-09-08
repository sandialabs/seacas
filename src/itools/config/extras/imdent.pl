#!$(PERLPATH)

# imdent - imake #-directive indenting utility

# Not recommended for use with rules files!

# Syntax: imdent [ -n ] [ file ]

# -n	Specify indent increment (n is a number). Default is 2 spaces.
#	"imdent -0" removes all indenting, so no "imundent" program
# 	is necessary.

# 10 Mar 93	Paul DuBois	dubois@primate.wisc.edu

# 10 Mar 93 V1.00.
# - Created.
# 18 Mar 93 V1.01
# - Added warning for extraneous #else/#endif.
# 21 Mar 93 V1.02
# - Detect non-zero indent at EOF (missing #endif).
# 29 Mar 97 V1.03
# - Minor cleanup

($prog = $0) =~ s|.*/||;	# get script name for messages

$indent = 0;
$increment = 2;

# proccess indent-increment-specifying flag if present

if (@ARGV && $ARGV[0] =~ /^-/)
{
	die "$prog: bad flag: $ARGV[0]\n" unless $ARGV[0] =~ /^-[0-9]+$/;
	$increment = int (substr ($ARGV[0], 1));
	shift (@ARGV);
}

# read input

while (<>)
{
	if (!/^# *(\S+)(.*)$/)	# skip non-directives
	{
		print;
		next;
	}
	chop;
	$directive = $1; $rest = $2;

	$before = $after = 0;
	$before = -$increment if $directive eq "else";
	$before = -$increment if $directive eq "elif";
	$before = -$increment if $directive eq "endif";
	$after = $increment if substr ($directive, 0, 2) eq "if";
	$after = $increment if $directive eq "else";
	$after = $increment if $directive eq "elif";

	$indent += $before;
	if ($indent < 0)
	{
		warn "$prog:line $.:extraneous \"$directive\"\n";
		$indent = 0;
	}
	print "#";
	print " " x $indent;
	print "$directive$rest\n";
	$indent += $after;
	if ($indent < 0)
	{
		warn "$prog:line $.:extraneous \"$directive\"\n";
		$indent = 0;
	}
}

if ($indent > 0)
{
	warn "$prog:line $.:missing \"#endif\"\n";
	$indent = 0;
}

exit (0);
