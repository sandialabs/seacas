.LP
If you use DECUS
.I cpp ,
be warned that it probably won't predefine any vendor-specific symbol
as a native
.I cpp
might.
For instance, on my Sun,
.I /lib/cpp
predefines
.Cw sun ,
but DECUS
.I cpp
doesn't.
Thus, if you build
.I imake
to use DECUS
.I cpp ,
you
.I must
include the vendor-specific trigger symbol in the value of
.Cw BOOTSTRAPCFLAGS .
