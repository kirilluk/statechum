# ===========================================================================
#                http://autoconf-archive.cryp.to/acx_blas.html
# ===========================================================================
#
# SYNOPSIS
#
#   ACX_BLAS([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
#
# DESCRIPTION
#
#   This macro looks for a library that implements the BLAS linear-algebra
#   interface (see http://www.netlib.org/blas/). On success, it sets the
#   BLAS_LIBS output variable to hold the requisite library linkages.
#
#   To link with BLAS, you should link with:
#
#   	$BLAS_LIBS $LIBS $FLIBS
#
#   in that order. FLIBS is the output variable of the
#   AC_F77_LIBRARY_LDFLAGS macro (called if necessary by ACX_BLAS), and is
#   sometimes necessary in order to link with F77 libraries. Users will also
#   need to use AC_F77_DUMMY_MAIN (see the autoconf manual), for the same
#   reason.
#
#   Many libraries are searched for, from ATLAS to CXML to ESSL. The user
#   may also use --with-blas=<lib> in order to use some specific BLAS
#   library <lib>. In order to link successfully, however, be aware that you
#   will probably need to use the same Fortran compiler (which can be set
#   via the F77 env. var.) as was used to compile the BLAS library.
#
#   ACTION-IF-FOUND is a list of shell commands to run if a BLAS library is
#   found, and ACTION-IF-NOT-FOUND is a list of commands to run it if it is
#   not found. If ACTION-IF-FOUND is not specified, the default action will
#   define HAVE_BLAS.
#
#   This macro requires autoconf 2.50 or later.
#
# LAST MODIFICATION
#   
#   2008-04-18 Kirill Bogdanov - added BLAS_DIR support via with-blasdir
#   2012-11-30 Kirill Bogdanov - added checking for OpenBLAS
# COPYLEFT
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

AC_DEFUN([ACX_BLAS], [
AC_PREREQ(2.50)
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
acx_blas_ok=no

AC_ARG_WITH(blas,
	[AC_HELP_STRING([--with-blas=<lib>], [use BLAS library <lib>])])
case $with_blas in
	yes | "") ;;
	no) acx_blas_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) BLAS_LIBS="$with_blas" ;;
	*) BLAS_LIBS="-l$with_blas" ;;
esac

BLAS_INCLUDES=
AC_ARG_WITH(blasdir,
	[AC_HELP_STRING([--with-blasdir=<dir>], [use BLAS libraries in <dir>])])
BLAS_DIR=
if test "x$with_blasdir" != x; then
	BLAS_DIR="-L$with_blasdir/lib"
	BLAS_INCLUDES="-I$with_blasdir/include"
fi

AC_SUBST(BLAS_INCLUDES)

# Get fortran linker names of BLAS functions to check for.
AC_F77_FUNC(sgemm)
AC_F77_FUNC(dgemm)

acx_blas_save_LIBS="$LIBS"
LIBS="$LIBS $FLIBS $BLAS_DIR"

# First, check BLAS_LIBS environment variable
if test $acx_blas_ok = no; then
if test "x$BLAS_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$BLAS_LIBS $LIBS"
	AC_MSG_CHECKING([for $sgemm in $BLAS_LIBS])
	AC_TRY_LINK_FUNC($sgemm, [acx_blas_ok=yes], [BLAS_LIBS=""])
	AC_MSG_RESULT($acx_blas_ok)
	LIBS="$save_LIBS"
fi
fi


# BLAS linked to by default?  (happens on some supercomputers)
if test $acx_blas_ok = no; then
	save_LIBS="$LIBS"; LIBS="$LIBS"
	AC_CHECK_FUNC($sgemm, [acx_blas_ok=yes])
	LIBS="$save_LIBS"
fi

# BLAS in ATLAS library? (http://math-atlas.sourceforge.net/)
# We attempt to link statically, because the aim is to use UMFPACK that links statically.
if test $acx_blas_ok = no -a "x$with_blasdir" != x; then
	AC_CHECK_LIB(atlas, ATL_xerbla,[ATLAS_LIBS="$with_blasdir/lib/libatlas.a"],[ATLAS_LIBS=""])	
	AC_CHECK_LIB(f77blas, $sgemm,[F77BLAS_LIBS="$with_blasdir/lib/libf77blas.a"],[F77BLAS_LIBS=""],[-latlas])
	AC_CHECK_LIB(ptf77blas, $sgemm,[F77BLAS_LIBS="$with_blasdir/lib/libptf77blas.a"],[],[-latlas])
	AC_CHECK_LIB(cblas, cblas_dgemm,[acx_blas_ok=yes
			CBLAS_LIBS="$with_blasdir/lib/libcblas.a"],[CBLAS_LIBS=""],[$with_blasdir/lib/libatlas.a])
	AC_CHECK_LIB(ptcblas, cblas_dgemm,[CBLAS_LIBS="$with_blasdir/lib/libptcblas.a"],[],[$with_blasdir/lib/libatlas.a])
	BLAS_LIBS="$CBLAS_LIBS $F77BLAS_LIBS $ATLAS_LIBS"
fi

# check for OpenBLAS
if test $acx_blas_ok = no -a "x$with_blasdir" != x ; then
	AC_CHECK_LIB(openblas, $sgemm,[acx_blas_ok=yes;OPENBLAS_LIBS="$with_blasdir/lib/libopenblas.a"],[OPENBLAS_LIBS=""])
	
	save_LIBS="$LIBS"; LIBS="$LIBS $OPENBLAS_LIBS"
	AC_CHECK_FUNC(openblas_set_num_threads, [AC_DEFINE([HAVE_SETTHREADS],[1],[Define if openblas permits setting thread number])],[])
	LIBS="$save_LIBS"
	
	BLAS_LIBS="$OPENBLAS_LIBS"
fi

# BLAS in PhiPACK libraries? (requires generic BLAS lib, too)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(blas, $sgemm,
		[AC_CHECK_LIB(dgemm, $dgemm,
		[AC_CHECK_LIB(sgemm, $sgemm,
			[acx_blas_ok=yes; BLAS_LIBS="-lsgemm -ldgemm -lblas"],
			[], [-lblas])],
			[], [-lblas])])
fi

# BLAS in Intel MKL library?
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(mkl, $sgemm, [acx_blas_ok=yes;BLAS_LIBS="-lmkl"])
fi

# BLAS in Apple vecLib library?
if test $acx_blas_ok = no; then
	save_LIBS="$LIBS"; LIBS="-framework vecLib $LIBS"
	AC_CHECK_FUNC($sgemm, [acx_blas_ok=yes;BLAS_LIBS="-framework vecLib"])
	LIBS="$save_LIBS"
fi

# BLAS in Alpha CXML library?
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(cxml, $sgemm, [acx_blas_ok=yes;BLAS_LIBS="-lcxml"])
fi

# BLAS in Alpha DXML library? (now called CXML, see above)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(dxml, $sgemm, [acx_blas_ok=yes;BLAS_LIBS="-ldxml"])
fi

# BLAS in Sun Performance library?
if test $acx_blas_ok = no; then
	if test "x$GCC" != xyes; then # only works with Sun CC
		AC_CHECK_LIB(sunmath, acosp,
			[AC_CHECK_LIB(sunperf, $sgemm,
        			[BLAS_LIBS="-xlic_lib=sunperf -lsunmath"
                                 acx_blas_ok=yes],[],[-lsunmath])])
	fi
fi

# BLAS in SCSL library?  (SGI/Cray Scientific Library)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(scs, $sgemm, [acx_blas_ok=yes; BLAS_LIBS="-lscs"])
fi

# BLAS in SGIMATH library?
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(complib.sgimath, $sgemm,
		     [acx_blas_ok=yes; BLAS_LIBS="-lcomplib.sgimath"])
fi

# BLAS in IBM ESSL library? (requires generic BLAS lib, too)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(blas, $sgemm,
		[AC_CHECK_LIB(essl, $sgemm,
			[acx_blas_ok=yes; BLAS_LIBS="-lessl -lblas"],
			[], [-lblas $FLIBS])])
fi

# Generic BLAS library?
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(blas, $sgemm, [acx_blas_ok=yes; BLAS_LIBS="-lblas"])
fi

BLAS_LIBS="$BLAS_DIR $BLAS_LIBS"
AC_SUBST(BLAS_LIBS)
AC_SUBST(BLAS_DIR)

LIBS="$acx_blas_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_blas_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_BLAS,1,[Define if you have a BLAS library.]),[$1])
        ifelse([$1],,AC_DEFINE(HAVE_BLAS,1,[Define if you have a BLAS library.]),[$1])
        :
else
        acx_blas_ok=no
        $2
fi
])dnl ACX_BLAS
