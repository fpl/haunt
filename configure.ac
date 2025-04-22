dnl -*- Autoconf -*-

AC_INIT(Haunt, 0.3.0)
AC_CONFIG_SRCDIR(haunt)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile example/Makefile haunt/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([test-env], [chmod +x test-env])
AC_CONFIG_FILES([scripts/haunt], [chmod +x scripts/haunt])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS

dnl Compute complete installation directories for .scm and .go files
dnl so that the Haunt executable can automagically add them to Guile's
dnl load path.
haunt_prefix="`eval echo $prefix | sed -e"s|NONE|/usr/local|g"`"
haunt_libdir="`eval echo $libdir | sed -e"s|NONE|$haunt_prefix|g"`"
guilemoduledir="`eval echo ${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION | sed -e"s|NONE|$haunt_prefix|g"`"
guileobjectdir="`eval echo ${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache | sed -e"s|NONE|$haunt_prefix|g"`"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

dnl Guile-reader is needed for Skribe support
GUILE_MODULE_AVAILABLE([have_guile_reader], [(system reader)])
AM_CONDITIONAL([HAVE_GUILE_READER], [test "x$have_guile_reader" = "xyes"])

GUILE_MODULE_AVAILABLE([have_guile_commonmark], [(commonmark)])
AM_CONDITIONAL([HAVE_GUILE_COMMONMARK], [test "x$have_guile_commonmark" = "xyes"])

AC_CHECK_FUNC([inotify_init], [AM_CONDITIONAL(HAVE_INOTIFY, true)], [AM_CONDITIONAL(HAVE_INOTIFY, false)])

dnl Attempt to find these programs, but if they're not found just
dnl default to searching for them on $PATH at runtime.
AC_PATH_PROG([RSYNC], [rsync], [rsync])
AC_PATH_PROG([HUT], [hut], [hut])
AC_PATH_PROG([TAR], [tar], [tar])

AC_OUTPUT
