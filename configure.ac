#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ([2.69])
AC_INIT([liboslayer], [0.1], [bjtj10@gmail.com])

AC_CONFIG_SRCDIR([liboslayer/os.cpp])
AC_CONFIG_HEADERS([config.h])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE
LT_PREREQ([2.2])
LT_INIT([dlopen])

# Checks for programs.
AC_PROG_CXX

# Checks for libraries.
ACX_PTHREAD

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h netdb.h netinet/in.h sys/socket.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
#AC_CHECK_HEADER_STDBOOL
AC_TYPE_MODE_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_CHECK_FUNCS([clock_gettime memset mkdir select socket SSL_library_init dlopen])

if test $ac_cv_func_clock_gettime = no; then
    AC_CHECK_LIB(rt, clock_gettime,
		[LIBS="-lrt $LIBS"
			AC_DEFINE(HAVE_CLOCK_GETTIME, 1,
				[Define to 1 if you have the 'clock_gettime' function.])])
fi

if test $ac_cv_func_SSL_library_init = no; then
   AC_CHECK_LIB(ssl, SSL_library_init, [LIBS="-lssl -lcrypto $LIBS"
   AC_DEFINE(HAVE_OPENSSL, 1, [Define to 1 if you have the 'SSL_library_init' function.])])
fi

if test $ac_cv_func_dlopen = no; then
   AC_CHECK_LIB(dl, dlopen, [LIBS="-ldl $LIBS"
   AC_DEFINE(HAVE_DLOPEN, 1, [Define to 1 if you have the 'dlopen' function.])])
fi

AC_CONFIG_FILES([Makefile
				 liboslayer/Makefile
				 examples/Makefile
				 oslayer.pc])
AC_OUTPUT
