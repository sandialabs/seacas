AC_INIT(README)
AM_INIT_AUTOMAKE(siMPI,0.1a6)
AM_CONFIG_HEADER(mpi_config.h)
 
AC_PROG_CC
AC_PROG_RANLIB


AC_HEADER_STDC
AC_CHECK_FUNCS(memcpy)
AC_CHECK_FUNCS(sysconf)

dnl if test "_SC_CLK_TCK" =""; then
dnl  AC_MSG_WARN([_SC_CLK_TCK is being defined to be 60])
dnl  AC_DEFINE(_SC_CLK_TCK, 60, [Clock tick defined to be 60])
dnl fi

AC_MSG_CHECKING(for --with-debug)
AC_ARG_WITH(debug,[  --with-debug          Build with debugging symbols],[
        if test "$withval" = "yes"; then
           withval="-g"
        fi
        CFLAGS="$withval $CFLAGS"
        AC_MSG_RESULT($withval)
],[
        AC_MSG_RESULT(no)
])

AC_MSG_CHECKING(for --with-preallocation)
AC_ARG_WITH(preallocation,[  --with-preallocation      Build with user selected preallocation buffer size],[
        if test "$withval" = "yes"; then
           withval="10"
        fi
	CFLAGS="-DPREALLOCATION_SIZE=$withval $CFLAGS"
        AC_MSG_RESULT($withval)
],[
        AC_MSG_RESULT(no)
])


AC_OUTPUT(Makefile mpi_path.h)
