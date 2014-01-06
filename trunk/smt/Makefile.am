#
# Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
# 
# This file is part of StateChum
# 
# StateChum is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
# 
# StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# StateChum. If not, see <http://www.gnu.org/licenses/>.
# 

# This Makefile aims to build the jni interface to UMFPACK, using ATLAS.
# The aim is to solve sparse systems of linear equations with millions of 
# variables.

## Process this file with automake to produce Makefile.in.
## Based on http://sources.redhat.com/autobook/autobook/autobook_253.html#SEC253


AM_CFLAGS = -I../include -Iinclude @YICES_INCLUDES@ -DNDEBUG -Wall
ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES		= libSMT_Yices.la
libSMT_Yices_la_SOURCES     = yices_caller.c
libSMT_Yices_la_LDFLAGS     = -no-undefined -version-info 1:0:0 -mno-cygwin @YICES_LIBS@

include_HEADERS         = smt.h

##bin_PROGRAMS            = hello
##hello_SOURCES           = main.c
##hello_LDADD             = libhello.la

