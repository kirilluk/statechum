#-------------------------------------------------------------------------------
# SuiteSparse/Example/CMakeLists.txt:  cmake for Example user library, "my"
#-------------------------------------------------------------------------------

# Copyright (c) 2022-2023, Timothy A. Davis, All Rights Reserved.
# SPDX-License-Identifier: BSD-3-clause

#-------------------------------------------------------------------------------
# get the version
#-------------------------------------------------------------------------------

cmake_minimum_required ( VERSION 3.19 )

set ( STATECHUMSOLVER_DATE "Apr 3, 2026" )
set ( STATECHUMSOLVER_VERSION_MAJOR 1 )
set ( STATECHUMSOLVER_VERSION_MINOR 0 )
set ( STATECHUMSOLVER_VERSION_PATCH 0 )

message ( STATUS "Building statechum solver library version: v"
    ${STATECHUMSOLVER_VERSION_MAJOR}.
    ${STATECHUMSOLVER_VERSION_MINOR}.
    ${STATECHUMSOLVER_VERSION_PATCH} " (" ${STATECHUMSOLVER_DATE} ")" )

#-------------------------------------------------------------------------------
# policies
#-------------------------------------------------------------------------------

cmake_policy ( SET CMP0042 NEW )    # enable MACOSX_RPATH by default
cmake_policy ( SET CMP0048 NEW )    # VERSION variable policy
cmake_policy ( SET CMP0054 NEW )    # if ( expression ) handling policy
cmake_policy ( SET CMP0104 NEW )    # initialize CUDA architectures

if ( WIN32 )
    set ( CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS true )
endif ( )

set ( CMAKE_MACOSX_RPATH TRUE )
enable_language ( C CXX )
include ( GNUInstallDirs )

# set the module path for all Find*.cmake files.
set ( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH}
    ${CMAKE_SOURCE_DIR}/cmake_modules
    ${CMAKE_SOURCE_DIR}/../lib/cmake/SuiteSparse
    ${CMAKE_INSTALL_PREFIX}/lib/cmake/SuiteSparse )

#-------------------------------------------------------------------------------
# define my project
#-------------------------------------------------------------------------------

project ( StatechumSolver
    VERSION "${STATECHUMSOLVER_VERSION_MAJOR}.${STATECHUMSOLVER_VERSION_MINOR}.${STATECHUMSOLVER_VERSION_PATCH}"
    LANGUAGES C )

if (EXISTS ${CMAKE_SOURCE_DIR}/../lib/cmake/SuiteSparse OR EXISTS ${CMAKE_INSTALL_PREFIX}/lib/cmake/SuiteSparse)
#-------------------------------------------------------------------------------
# find library dependencies
#-------------------------------------------------------------------------------

# look for relevant SuiteSparse packages:
find_package ( SuiteSparse_config 7.0.0 REQUIRED )
find_package ( AMD 3.0.3 REQUIRED )
find_package ( CAMD 3.0.3 REQUIRED )
find_package ( CCOLAMD 3.0.3 REQUIRED )
find_package ( COLAMD 3.0.3 REQUIRED )
find_package ( CHOLMOD 4.0.3 REQUIRED )
find_package ( UMFPACK 6.1.0 REQUIRED )
SET(SUITESPARSE_LIBS
        ${AMD_STATIC}
        ${CAMD_static} ${CCOLAMD_STATIC} ${COLAMD_STATIC}
        ${SUITESPARSE_CONFIG_STATIC} ${UMFPACK_STATIC})
include ( SuiteSparseBLAS )
else()
find_library(SUITESPARSE_CONFIG_LIB NAMES ${CMAKE_FIND_LIBRARY_PREFIXES}suitesparseconfig.so) 
find_library(AMD_LIB NAMES ${CMAKE_FIND_LIBRARY_PREFIXES}amd.so)
find_library(CAMD_LIB ${CMAKE_FIND_LIBRARY_PREFIXES}camd.so)
find_library(CCOLAMD_LIB ${CMAKE_FIND_LIBRARY_PREFIXES}ccolamd.so)
find_library(COLAMD_LIB ${CMAKE_FIND_LIBRARY_PREFIXES}colamd.so)
find_library(UMFPACK_LIB ${CMAKE_FIND_LIBRARY_PREFIXES}umfpack.so)
SET(SUITESPARSE_LIBS ${SUITESPARSE_CONFIG_LIB} ${AMD_LIB} ${CAMD_LIB} ${CCOLAMD_LIB} ${COLAMD_LIB} ${UMFPACK_LIB})

find_path(UMFPACK_INCLUDE_DIR NAMES umfpack.h PATHS "/usr/include/suitesparse")
endif()

# look for all external libaries:
find_package ( JNI REQUIRED )

#-------------------------------------------------------------------------------
# my dynamic library properties
#-------------------------------------------------------------------------------

add_library ( StatechumSolver SHARED umfsolver.c )
set_target_properties ( StatechumSolver PROPERTIES
    VERSION ${STATECHUMSOLVER_VERSION_MAJOR}.${STATECHUMSOLVER_VERSION_MINOR}.${STATECHUMSOLVER_VERSION_PATCH}
    C_STANDARD_REQUIRED 11
    SOVERSION ${STATECHUMSOLVER_VERSION_MAJOR}
    PUBLIC_HEADER "solver.h"
    WINDOWS_EXPORT_ALL_SYMBOLS ON )

#-------------------------------------------------------------------------------
# add the library dependencies
#-------------------------------------------------------------------------------

# libm:
if ( NOT WIN32 )
    target_link_libraries ( StatechumSolver PUBLIC m )
endif ( )

target_include_directories(StatechumSolver PRIVATE ${JNI_INCLUDE_DIRS} ${UMFPACK_INCLUDE_DIR})
target_link_libraries(StatechumSolver PUBLIC ${JNI_LIBRARIES})

# dependencies
target_link_libraries(StatechumSolver PRIVATE
        ${BLAS_LIBRARIES}
        ${LAPACK_LIBRARIES}
	${SUITESPARSE_LIBS}
        )

#-------------------------------------------------------------------------------
# installation location
#-------------------------------------------------------------------------------

# The installation path mimics old configure-selected install path used both 
# in IdeaJ and in ant-build.
SET(STATECHUM_LIBS "${CMAKE_CURRENT_LIST_DIR}/.libs")
install ( TARGETS StatechumSolver
    LIBRARY DESTINATION ${STATECHUM_LIBS}
    ARCHIVE DESTINATION ${STATECHUM_LIBS}
    RUNTIME DESTINATION ${STATECHUM_LIBS}
    PUBLIC_HEADER DESTINATION ${STATECHUM_LIBS} )
