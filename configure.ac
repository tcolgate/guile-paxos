#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ(2.63)
AC_INIT([Guile Paxos],[0.1],[tcolgate <at> gmail.com],[guile-paxos],[http://github.com/tcolgate/guile-paxos])
AC_CONFIG_MACRO_DIR([m4])
AC_SUBST(PACKAGE_TARNAME)
AC_SUBST(PACKAGE)
AC_SUBST(PACKAGE_URL)
AC_SUBST(PACKAGE_BUGREPORT)

# Checks for programs.
AM_INIT_AUTOMAKE
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

GUILE_PREFIX=`$GUILE_CONFIG info prefix`
AC_SUBST(GUILE_PREFIX)

if $GUILE_TOOLS | grep -q compile; then
   GUILE_COMPILE="$GUILE_TOOLS compile"
   HAVE_GUILE_COMPILE=1
else
   GUILE_COMPILE=""
   HAVE_GUILE_COMPILE=0
fi

AC_SUBST(GUILE_COMPILE)
AM_CONDITIONAL(HAVE_GUILE_COMPILE, test "$HAVE_GUILE_COMPILE" = 1)

AC_CONFIG_FILES([build-environ], [chmod +x build-environ])

AC_CONFIG_FILES([Makefile 
                 src/Makefile 
                 src/paxos/Makefile 
                 src/paxos/server/Makefile 
                 src/paxos/server/db/Makefile 
                 src/paxos/client/Makefile 
                 src/paxos/net/Makefile 
                 src/paxos/misc/Makefile 
                 tests/Makefile ])

AC_CONFIG_FILES([guile-paxos.spec])

AC_OUTPUT

