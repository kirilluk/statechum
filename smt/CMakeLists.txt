#-------------------------------------------------------------------------------
# SuiteSparse/Example/CMakeLists.txt:  cmake for Example user library, "my"
#-------------------------------------------------------------------------------

# Copyright (c) 2022-2023, Timothy A. Davis, All Rights Reserved.
# SPDX-License-Identifier: BSD-3-clause

#-------------------------------------------------------------------------------
# get the version
#-------------------------------------------------------------------------------

cmake_minimum_required ( VERSION 3.19 )

set ( STATECHUMSMT_DATE "Apr 3, 2026" )
set ( STATECHUMSMT_VERSION_MAJOR 1 )
set ( STATECHUMSMT_VERSION_MINOR 0 )
set ( STATECHUMSMT_VERSION_PATCH 0 )

message ( STATUS "Building statechum SMT library version: v"
    ${STATECHUMSMT_VERSION_MAJOR}.
    ${STATECHUMSMT_VERSION_MINOR}.
    ${STATECHUMSMT_VERSION_PATCH} " (" ${STATECHUMSMT_DATE} ")" )

#-------------------------------------------------------------------------------
# policies
#-------------------------------------------------------------------------------

cmake_policy ( SET CMP0042 NEW )    # enable MACOSX_RPATH by default
cmake_policy ( SET CMP0048 NEW )    # VERSION variable policy
cmake_policy ( SET CMP0054 NEW )    # if ( expression ) handling policy

if ( WIN32 )
    set ( CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS true )
endif ( )

set ( CMAKE_MACOSX_RPATH TRUE )
enable_language ( C CXX )
include ( GNUInstallDirs )

#-------------------------------------------------------------------------------
# define my project
#-------------------------------------------------------------------------------

project ( StatechumSmt
    VERSION "${STATECHUMSMT_VERSION_MAJOR}.${STATECHUMSMT_VERSION_MINOR}.${STATECHUMSMT_VERSION_PATCH}"
    LANGUAGES C )

find_library(YICES_LIB NAMES ${CMAKE_FIND_LIBRARY_PREFIXES}yices.so PATHS ${YICES_ROOT}/lib)
find_path(YICES_INCLUDE_DIR NAMES yices_c.h PATHS ${YICES_ROOT}/include)

# look for all external libaries:
find_package ( JNI REQUIRED )

#-------------------------------------------------------------------------------
# my dynamic library properties
#-------------------------------------------------------------------------------

add_library ( StatechumSmt SHARED yices_caller.c )
set_target_properties ( StatechumSmt PROPERTIES
    VERSION ${STATECHUMSMT_VERSION_MAJOR}.${STATECHUMSMT_VERSION_MINOR}.${STATECHUMSMT_VERSION_PATCH}
    C_STANDARD_REQUIRED 11
    SOVERSION ${STATECHUMSMT_VERSION_MAJOR}
    PUBLIC_HEADER "smt.h"
    WINDOWS_EXPORT_ALL_SYMBOLS ON )

#-------------------------------------------------------------------------------
# add the library dependencies
#-------------------------------------------------------------------------------

# libm:
if ( NOT WIN32 )
    target_link_libraries ( StatechumSmt PUBLIC m )
endif ( )

target_include_directories(StatechumSmt PRIVATE ${JNI_INCLUDE_DIRS} ${YICES_INCLUDE_DIR})
target_link_libraries(StatechumSmt PUBLIC ${JNI_LIBRARIES} ${YICES_LIB})

#-------------------------------------------------------------------------------
# installation location
#-------------------------------------------------------------------------------

# The installation path mimics old configure-selected install path used both 
# in IdeaJ and in ant-build.
SET(STATECHUM_LIBS "${CMAKE_CURRENT_LIST_DIR}/.libs")
install ( TARGETS StatechumSmt
    LIBRARY DESTINATION ${STATECHUM_LIBS}
    ARCHIVE DESTINATION ${STATECHUM_LIBS}
    RUNTIME DESTINATION ${STATECHUM_LIBS}
    PUBLIC_HEADER DESTINATION ${STATECHUM_LIBS} )
