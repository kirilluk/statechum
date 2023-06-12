#-------------------------------------------------------------------------------
# SuiteSparse/Example/CMakeLists.txt:  cmake for Example user library, "my"
#-------------------------------------------------------------------------------

# Copyright (c) 2022-2023, Timothy A. Davis, All Rights Reserved.
# SPDX-License-Identifier: BSD-3-clause

#-------------------------------------------------------------------------------
# get the version
#-------------------------------------------------------------------------------

cmake_minimum_required ( VERSION 3.19 )

set ( STATECHUMSOLVER_DATE "Jun 12, 2023" )
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

# look for all external libaries:
find_package ( JNI REQUIRED )
include ( SuiteSparseBLAS )

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

target_include_directories(StatechumSolver PRIVATE ${JNI_INCLUDE_DIRS})
target_link_libraries(StatechumSolver PUBLIC ${JNI_LIBRARIES})

# dependencies
target_link_libraries(StatechumSolver PUBLIC
        ${BLAS_LIBRARIES}
        ${LAPACK_LIBRARIES}
        ${AMD}
        ${CAMD} ${CCOLAMD} ${COLAMD}
        ${SUITESPARSE_CONFIG}
        )

# umfpack:
target_link_libraries ( StatechumSolver PUBLIC ${UMFPACK_LIBRARIES} )
include_directories ( ${UMFPACK_INCLUDE_DIR} )

#-------------------------------------------------------------------------------
# installation location
#-------------------------------------------------------------------------------

install ( TARGETS StatechumSolver
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} )
