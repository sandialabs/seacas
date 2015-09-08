.TH IMBOOT 1
.\"
.SH NAME
imboot \- bootstrap a
.I Makefile
from an
.I Imakefile
.\"
.SH SYNOPSIS
.B imboot
[
.B \-c
.I name
] [
.B \-C
.I name
] [
.BI \-D define
] [
.BI \-I dir
] [
.B \-d
] [
.B \-u
] [
.I topdir
[
.I curdir
] ]
.\"
.SH DESCRIPTION
.I imboot
is a general-purpose
.I imake
bootstrapper for
generating a
.I Makefile
from an
.IR Imakefile .
By default,
.I imboot
looks for configuration files in a private project-specific
directory (i.e., located within the current project's source tree).
The
.I \-c
(or
.IR \-C )
option may be given to tell
.I imboot
to
use a set of publicly installed configuration files
instead of (or in addition to) any files in the project's private
directory.
.PP
The private configuration file directory, if used, is taken to be
.I config
or
.I config/cf
at the top of the project source tree.
.I config
is the usual location, but by looking in
.I config/cf
as well,
.I imboot
works within the X11R6 source tree.
The public configuration file directory will be one of those located
under
.IR CONFIGROOTDIR .
The name of the directory is specified by the
.I \-c
and
.I \-C
options.
.PP
The
.I topdir
argument specifies the location of the project root.
The default is
``\fB.\fP''
and thus may be omitted if the current directory is the project root.
Otherwise it may be specified as an absolute pathname or as a path relative
to the current directory.
.IR curdir ,
if given, specifies the name of the current directory, relative to the project
root.
.I curdir
is usually omitted.
.\"
.SH OPTIONS
.I imboot
understands the following options:
.TP 8
.B "\-c \fIname\fP
Use the named set of public configuration files instead of the
files in the project's private configuration file
directory.
.TP 8
.B "\-C \fIname\fP
Use the named set of public configuration files in addition
to the files in the project's private configuration file
directory.
.I imboot
tells
.I imake
to look in the private directory under the project root before looking in
the public directory when searching for configuration files.
The intent is to support an extensible configuration file architecture such
that the public files define a baseline configuration that individual
projects can extend or override by providing project-specific information
in files in their private configuration file
directory.
.TP 8
.BI \-D define
This option is passed directly to
.IR imake ,
which passes it to
.IR cpp .
.TP 8
.BI \-I dir
This option is passed directly to
.IR imake ,
which passes it to
.IR cpp .
It can be used to specify additional directories in which to look
to look for configuration files.
.TP 8
.B \-d
Force definition of UseInstalled.
This is shorthand for
.BR \-DUseInstalled ,
which is turned on automatically when you specify
.I \-c
or
.IR \-C .
.TP 8
.B \-u
Force undefinition of UseInstalled.
.PP
For the
.I \-c
.I name
or
.I \-C
.I name
options, the
.I name
argument may also be an absolute pathname, in which case it is used as given
for locating configuration files.
.\"
.SH EXAMPLES
Suppose you want to use a set of public configuration files named XYZ.
If you're in the root directory of a project, you can bootstrap a
.I Makefile
using the XYZ configuration files like this:
.sp .5v
.RS
% imboot \-c XYZ
.RE
.sp .5v
If you're in a subdirectory, say,
.IR src/prog1
(two levels down), the location of the project root is
.B ..\^/..
and you bootstrap the
.I Makefile
with:
.sp .5v
.RS
% imboot \-c XYZ \fB..\^/..\fR
.RE
.sp .5v
If the XYZ configuration files use an extensible architecture and your project
has private configuration files in a
.I config
directory, you can use the private files as well.
The commands just shown become:
.sp .5v
.RS
.nf
% imboot \-C XYZ
% imboot \-C XYZ \fB..\^/..\fR
.fi
.RE
.sp .5v
.\"
.SH "WHO-TO-BLAME"
Paul DuBois, dubois@primate.wisc.edu
.\"
.SH "BUGS"
It's difficult to get
.B \-D
or
.B \-I
arguments that require quotes passed through to
.IR imake.
