# Detects UMFPACK, UFCONFIG and AMD in one go. Based on BLAS detection.

# SYNOPSIS
#
#   ACX_UMFPACK()
#
# DESCRIPTION
#
#   Crudely attempts to find UMFPACK and checks whether definitions of 
#   int and double have the same length between JNI and UMFPACK.
#   Aborts configure if UMFPACK with consistent definitions is not found.
#   Otherwise, sets UMFPACK_INCLUDES and UMFPACK_LIBS to the detected values.
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


# ACX_UMFPACK_CHECKUMF(DIR)
# Checks if 
# (a) UMFPACK is installed in DIR/UMFPACK
# (b) AMD is installed in DIR/AMD
# (c) it is possible to build a simple program using UMFPACK
# (d) if JNI data types agree with UMFPACK ones, so that
# data can be passed between the two without any conversion.
# 
# Sets UMFPACK_INCLUDES, UMFPACK_LIBS
# Upon successful completion, sets acx_umfpack_ok=yes
#
AC_DEFUN([ACX_UMFPACK_CHECKUMF],[
	acx_umfpack_include_ok=no
	UMFDIR=[$1]
	UMFPACK_INCLUDES="-I$UMFDIR/UMFPACK/Include -I$UMFDIR/UFconfig -I$UMFDIR/AMD/Include $BLAS_DIR"

	for JNI_INCLUDE_DIR in $JNI_INCLUDE_DIRS;do UMFPACK_INCLUDES="$UMFPACK_INCLUDES -I$JNI_INCLUDE_DIR";done
	UMFPACK_LIBS="-L$UMFDIR/UMFPACK/Lib -L$UMFDIR/AMD/Lib -lumfpack -lamd $BLAS_LIBS"
	save_CPPFLAGS="$CPPFLAGS"; CPPFLAGS="$CPPFLAGS $UMFPACK_INCLUDES"
	save_LIBS="$LIBS"; LIBS="$UMFPACK_LIBS $LIBS"
	AC_CHECK_HEADER(umfpack.h,
		[acx_umfpack_include_ok=yes],[acx_umfpack_include_ok=no])

	if test $acx_umfpack_include_ok = yes; then
		AC_MSG_CHECKING([if data types are the same between UMFPACK and JNI])
dnl this is from default aclocal.m4, checking both if a simple program can be built
dnl and whether sizes are appropriate.

if test "$cross_compiling" = no; then
  umf_defsUnknown=0;umf_defsConsistent=1;umf_defsInconsistent=2;
  lt_status=$umf_defsUnknown
  cat > conftest.$ac_ext <<EOF
[#line __oline__ "configure"
#include "confdefs.h"

#include "umfpack.h"
#include "solver.h"

int main(int argc,char **argv)
{
  if (sizeof(SOLVER_INT) != sizeof(jint))
    return $umf_defsInconsistent;

  if (sizeof(double) != sizeof(jdouble))
    return $umf_defsInconsistent;

  return $umf_defsConsistent;
}]
EOF
  if AC_TRY_EVAL(ac_link) && test -s conftest${ac_exeext} 2>/dev/null; then
    (./conftest; exit; ) >&AS_MESSAGE_LOG_FD 2>/dev/null
    lt_status=$?
    case x$lt_status in
      x$umf_defsConsistent) 
	acx_umfpack_ok=yes 
    	AC_MSG_RESULT([yes])
	;; # everything ok, we're finished
      x$umf_defsInconsistent) 
	AC_MSG_RESULT([inconsistent definitions of int/double between jni and UMFPACK])
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

	fi # test $acx_umfpack_include_ok = yes

	CPPFLAGS="$save_CPPFLAGS"
	LIBS="$save_LIBS"
])dnl ACX_UMFPACK_CHECKUMF


AC_DEFUN([ACX_UMFPACK], [
AC_PREREQ(2.50)
acx_umfpack_ok=no

AC_ARG_WITH(umfpack,
	[AC_HELP_STRING([--with-umfpack=<dir>], [use umfpack rooted at <dir>])])
case $with_umfpack in
	yes | "") ;;
	no) acx_umfpack_ok=disable ;;
	*) UMFPACK_ROOT="$with_umfpack" ;;
esac

acx_umfpack_save_LIBS="$LIBS"

# First, check UMFPACK_ROOT environment variable
if test $acx_umfpack_ok = no; then
if test "x$UMFPACK_ROOT" != x; then
	ACX_UMFPACK_CHECKUMF([$UMFPACK_ROOT])
fi
fi

AC_SUBST(UMFPACK_INCLUDES)
AC_SUBST(UMFPACK_LIBS)

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_umfpack_ok" = xyes; then
        :
else
        AC_MSG_ERROR([failed to find a reasonable UMFPACK distribution])
fi
])dnl ACX_UMFPACK
