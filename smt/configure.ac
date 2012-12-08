dnl  Based on http://sources.redhat.com/autobook/autobook/autobook_254.html

dnl 
dnl  Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
dnl  
dnl  This file is part of StateChum
dnl  
dnl  StateChum is free software: you can redistribute it and/or modify it under
dnl  the terms of the GNU General Public License as published by the Free Software
dnl  Foundation, either version 3 of the License, or (at your option) any later
dnl  version.
dnl  
dnl  StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
dnl  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
dnl  A PARTICULAR PURPOSE. See the GNU General Public License for more details.
dnl  
dnl  You should have received a copy of the GNU General Public License along with
dnl  StateChum. If not, see <http://www.gnu.org/licenses/>.
dnl  

dnl  Process this file with autoconf to create configure.

AC_PREREQ(2.64)
AC_INIT(smtsolver,1.0)
AC_CONFIG_AUX_DIR(config)
AM_INIT_AUTOMAKE(smt_yices, 1.0)
AC_CONFIG_MACRO_DIR([m4])

AC_PROG_LIBTOOL

AC_PROG_CC
AC_PROG_CXX

AM_PROG_CC_STDC

AC_C_CONST

AC_CHECK_FUNCS(memset dup2)
AC_FUNC_MALLOC

AC_LIBTOOL_WIN32_DLL
AC_DISABLE_STATIC

JAVAC=javac
AC_JNI_INCLUDE_DIR()

ACX_YICES()

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
