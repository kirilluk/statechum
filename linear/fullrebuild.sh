#!/bin/sh
#./bootstrap && ./configure --with-blasdir=/cygdrive/d/experiment/ATLAS/builddir/lib/dll --with-umfpack=/cygdrive/d/experiment && make

# Essentially from http://www.kevinsheppard.org/research/matlabatlas/

ATLAS=/usr/local/soft/atlas-3.8.1/lib
UMFROOT=/cygdrive/d/experiment/umfpack

./bootstrap && ./configure --with-blasdir=${ATLAS} --with-umfpack=${UMFROOT}
touch umfsolver.c

if uname | grep -q CYGWIN;then
# Win32 build

LIBDIR=lib
GCC=gcc

[ -r ${LIBDIR} ] || mkdir ${LIBDIR}

(rm ${LIBDIR}/*.o ${LIBDIR}/*.a) >/dev/null 2>&1
(cd ${LIBDIR} && ar x ${ATLAS}/libatlas.a && ar x ${ATLAS}/libf77blas.a && \
ar x ${UMFROOT}/UMFPACK/Lib/libumfpack.a && ar x ${UMFROOT}/AMD/Lib/libamd.a && \
ar cr umf.a *.o && ranlib umf.a)

echo 'Linking DLL and creating gcc import library...'
(cd ${LIBDIR} && ${GCC} -mno-cygwin -shared -o libumfpack.dll ${UMFROOT}/UMFPACK/Lib/libumfpack.def \
    umf.a -lpthread -lg2c \
    -Wl,--enable-auto-import \
    -Wl,--no-whole-archive \
    -Wl,--out-implib=libumfpack_win32.a)

cat Makefile|sed -e "s%^libStatechumSolver_la_LDFLAGS.*%libStatechumSolver_la_LDFLAGS = -no-undefined -version-info 1:0:0 -mno-cygwin -L${LIBDIR} -lumfpack_win32%" > Makefile_win32
make -f Makefile_win32

fi


