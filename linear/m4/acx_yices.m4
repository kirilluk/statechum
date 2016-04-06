# Detects Yices. Based on BLAS detection.

# SYNOPSIS
#
#   ACX_YICES()
#
# DESCRIPTION
#
#   Crudely attempts to find Yices and checks whether it can be linked to
#   and a simple program runs.
#   If successful, sets YICES_INCLUDES and YICES_LIBS to the detected values.
#
#   This macro requires autoconf 2.50 or later.
#
#
# COPYLEFT
#   Written by Kirill Bogdanov <kirill_uk@sourceforge.net>
#   based on ACX_BLAS, original (unchanged) copyleft below:
#
#   Copyright (c) 2008 Steven G. Johnson <stevenj@alum.mit.edu>
#
#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the
#   Free Software Foundation, either version 3 of the License, or (at your
#   option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#   Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program. If not, see <http://www.gnu.org/licenses/>.
#
#   As a special exception, the respective Autoconf Macro's copyright owner
#   gives unlimited permission to copy, distribute and modify the configure
#   scripts that are the output of Autoconf when processing the Macro. You
#   need not follow the terms of the GNU General Public License when using
#   or distributing such scripts, even though portions of the text of the
#   Macro appear in them. The GNU General Public License (GPL) does govern
#   all other use of the material that constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the Autoconf
#   Macro released by the Autoconf Macro Archive. When you make and
#   distribute a modified version of the Autoconf Macro, you may extend this
#   special exception to the GPL to apply to your modified version as well.


# ACX_YICES_CHECK(DIR)
# Checks if 
# (a) Yices is installed in DIR/YICES
# (b) it is possible to build a simple program using Yices
# 
# Sets YICES_INCLUDES, YICES_LIBS
# Upon successful completion, sets acx_yices_ok=yes
#
AC_DEFUN([ACX_YICES_CHECK],[
	acx_yices_include_ok=no
	YICESDIR=[$1]
	YICES_INCLUDES="-I$YICESDIR/include"

	for JNI_INCLUDE_DIR in $JNI_INCLUDE_DIRS;do YICES_INCLUDES="$YICES_INCLUDES -I$JNI_INCLUDE_DIR";done
	YICES_LIBS="-L$YICESDIR/lib -lyices -Wl,-rpath,$YICESDIR/lib,--enable-new-dtags "
	save_CPPFLAGS="$CPPFLAGS"; CPPFLAGS="$CPPFLAGS $YICES_INCLUDES"
	save_LIBS="$LIBS"; LIBS="$YICES_LIBS $LIBS"
	AC_CHECK_HEADER(yices_c.h,
		[AC_CHECK_HEADER(yicesl_c.h,
			[acx_yices_include_ok=yes],[acx_yices_include_ok=no])]
		,[acx_yices_include_ok=no])

	if test $acx_yices_include_ok = yes; then
		AC_MSG_CHECKING([if a simple program can be run using Yices])
dnl this is from default aclocal.m4, checking if a simple program can be built

if test "$cross_compiling" = no; then
  y_defsUnknown=0;y_yicesRuns=1;y_yicesBroken=2;
  lt_status=$y_defsUnknown
  cat > conftest.$ac_ext <<EOF
[#line __oline__ "configure"
#include "confdefs.h"

#include "yicesl_c.h"
#include "smt.h"

int main(int argc,char **argv)
{
  jboolean result = JNI_FALSE;
  yicesl_context context = yicesl_mk_context();
  yicesl_read(context,"(push)");

  yicesl_read(context,"(define x::int)\n(assert (> x 1))");
  if (!yicesl_inconsistent(context))
  {
     yicesl_read(context,"(pop) (push)");
     yicesl_read(context,"(define x::int)\n(assert (> x 1))\n(assert (< x 0) )");
     if (yicesl_inconsistent(context))
	result = JNI_TRUE;
  }

  yicesl_del_context(context);
  return (result == JNI_TRUE)? $y_yicesRuns:$y_yicesBroken;
}]
EOF
  if AC_TRY_EVAL(ac_link) 2>/dev/null; then
    (./conftest; exit; ) >&AS_MESSAGE_LOG_FD 2>/dev/null
    lt_status=$?
    case x$lt_status in
      x$y_yicesRuns) 
	acx_yices_ok=yes 
    	AC_MSG_RESULT([yes])
	;; # everything ok, we're finished
      x$y_yicesBroken) 
	AC_MSG_RESULT([simple program failed to return the expected result])
	;;
    esac
  else :
    # compilation failed
    AC_MSG_RESULT([failure])
  fi
  rm -fr conftest*
else
  AC_MSG_RESULT([cannot check when cross-compiling])
fi # test "$cross_compiling" = no

	fi # test $acx_yices_include_ok = yes

	CPPFLAGS="$save_CPPFLAGS"
	LIBS="$save_LIBS"
])dnl ACX_YICES_CHECK


AC_DEFUN([ACX_YICES], [
AC_PREREQ(2.50)
acx_yices_ok=no

AC_ARG_WITH(yices,
	[AC_HELP_STRING([--with-yices=<dir>], [use yices installed in <dir>])])
case $with_yices in
	yes | "") ;;
	no) acx_yices_ok=disable ;;
	*) YICES_ROOT="$with_yices" ;;
esac

acx_yices_save_LIBS="$LIBS"

# First, check YICES_ROOT environment variable
if test $acx_yices_ok = no; then
if test "x$YICES_ROOT" != x; then
	ACX_YICES_CHECK([$YICES_ROOT])
fi
fi

AC_SUBST(YICES_INCLUDES)
AC_SUBST(YICES_LIBS)

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_yices_ok" = xyes; then
        :
else
        AC_MSG_ERROR([failed to find a reasonable YICES distribution])
fi
])dnl ACX_YICES
