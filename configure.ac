#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ([2.69])
AC_INIT([liboslayer], [1.0.0], [bjtj10@gmail.com])

AC_CONFIG_SRCDIR([liboslayer/os.cpp])
AC_CONFIG_HEADERS([config.h])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build-aux])

AM_SILENT_RULES([yes])

AM_INIT_AUTOMAKE([serial-tests])
LT_PREREQ([2.2])
LT_INIT([dlopen])

# Checks for programs.
AC_PROG_CXX

# Checks for libraries.
ACX_PTHREAD
AX_VALGRIND_DFLT([sgcheck], [off])
AX_VALGRIND_CHECK

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h netdb.h netinet/in.h sys/socket.h sys/time.h unistd.h])

# Debugging
AC_ARG_ENABLE(debug,
        AS_HELP_STRING([--enable-debug], [enable debugging]),,
        enable_debug=no)
if test "x$enable_debug" = "xyes"; then
        CFLAGS="$CFLAGS -g -Wall"
		CXXFLAGS="$CXXFLAGS -g -Wall"
fi

# Checks for typedefs, structures, and compiler characteristics.
#AC_CHECK_HEADER_STDBOOL
AC_TYPE_MODE_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_CHECK_FUNCS([clock_gettime memset mkdir select socket SSL_library_init sqlite3_open dlopen])

# clock_gettime
if test $ac_cv_func_clock_gettime = no; then
    AC_CHECK_LIB(rt, clock_gettime,
		[LIBS="-lrt $LIBS"
			AC_DEFINE(HAVE_CLOCK_GETTIME, 1,
				[Define to 1 if you have the 'clock_gettime' function.])])
fi

# dlopen
if test $ac_cv_func_dlopen = no; then
   AC_CHECK_LIB(dl, dlopen, [LIBS="-ldl $LIBS"
   AC_DEFINE(HAVE_DLOPEN, 1, [Define to 1 if you have the 'dlopen' function.])])
fi

# openssl
AC_ARG_ENABLE([openssl],
  [AS_HELP_STRING([--enable-openssl], [enable openssl build @<:@default: no@:>@])],
    [enable_openssl=${enableval}], [enable_openssl=no])

AS_IF([test "x$enable_openssl" = xyes],
  [PKG_CHECK_MODULES([LIBOPENSSL],[openssl],
    [HAVE_PKG_OPENSSL=yes],[HAVE_PKG_OPENSSL=no])],
  [HAVE_PKG_OPENSSL=no])

AS_IF([test "x$HAVE_PKG_OPENSSL" = xyes], [AC_DEFINE([HAVE_OPENSSL],1,[support openssl])])
AM_CONDITIONAL([SUPPORT_OPENSSL], [test "x$HAVE_PKG_OPENSSL" = xyes])

# sqlite3
AC_ARG_ENABLE([sqlite3],
  [AS_HELP_STRING([--enable-sqlite3], [enable sqlite3 build @<:@default: no@:>@])]
    [enable_sqlite3=${enableval}], [enable_sqlite=no])

if test "x$enable_sqlite3" = "xyes"; then
  if test $ac_cv_func_sqlite3_open = no; then
    AC_CHECK_LIB(sqlite3, sqlite3_open, [LIBS="-lsqlite3 $LIBS"
      AC_DEFINE(HAVE_SQLITE3, 1, [Define to 1 if you have the 'sqlite3_open' function.])])
  fi
fi

# misc.
AC_CONFIG_LINKS([tests/data/cert.pem:tests/data/cert.pem])
AC_CONFIG_LINKS([tests/data/key.pem:tests/data/key.pem])

# config files
AC_CONFIG_FILES([Makefile
				 liboslayer/Makefile
				 examples/Makefile
				 tests/Makefile
				 tests/data/Makefile
				 oslayer.pc])
AC_OUTPUT

# print result
echo \
"====================================
openssl support : ${HAVE_PKG_OPENSSL}
====================================="
