#!/bin/sh
#./bootstrap && ./configure --with-blasdir=/cygdrive/d/experiment/ATLAS/builddir/lib/dll --with-umfpack=/cygdrive/d/experiment && make

# Essentially from http://www.kevinsheppard.org/research/matlabatlas/

#ATLAS=/usr/local/soft/atlas-3.10.0
#/usr/local/soft/atlas-3.8.3

# list of preferential locations of umfpack

[ -r /usr/local/src/UMFPACK/include ] && UMFROOT=/usr/local/src
[ -r /usr/local/soft/umfpack-5.6.1 ] && UMFROOT=/usr/local/soft/umfpack-5.6.1
[ -r /usr/local/src/umfpack ] && UMFROOT=/usr/local/src/umfpack
[ -r /cygdrive/d/experiment/umfpack ] && UMFROOT=/cygdrive/d/experiment/umfpack

if [ -r /usr/local/soft/umfpack-5.6.1 ];then
# override paths for openblas, currently everything is hardwired, for testing that autoconfiguration works
    UMFROOT=/usr/local/soft/umfpack-5.6.1
    ATLAS=/usr/local/soft/OpenBLAS-be853da
fi

HOST=
# For building on Windows-64, there is a special kludge, only tested on Win64.
if uname | grep -q WOW64;then
	[ -r ${ATLAS}/lib/libopenblas.a ] || (cd ${ATLAS}/lib;ln -s libopenblas.lib libopenblas.a) # this is to make sure ./configure finds the library
	HOST="--host=x86_64-w64-mingw32"
	export PATH="${PATH}:/cygdrive/c/Program Files/Java/jdk1.7.0_07/bin"
	#export CC=x86_64-w64-mingw32-gcc CXX=x86_64-w64-mingw32-g++ F77=x86_64-w64-mingw32-gfortran AR=x86_64-w64-mingw32-ar RANLIB=x86_64-w64-mingw32-ranlib
fi


MACOSVECDIR=/System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A
[ -r ${MACOSVECDIR} ] && UMFROOT=${MACOSVECDIR}
echo Running autoconfiguration with --with-blasdir=${ATLAS} --with-umfpack=${UMFROOT}
./bootstrap && ./configure --with-blasdir=${ATLAS} --with-umfpack=${UMFROOT} ${HOST}
touch umfsolver.c

if uname | grep -q CYGWIN;then
if uname | grep -q WOW64;then
# Win64 build

echo '.libs/libStatechumSolver.dll:umfsolver.lo'>>Makefile
# the idea of implib is from http://www.mingw.org/wiki/sampleDLL
echo -e '\t$(CC) -shared -o .libs/libStatechumSolver.dll .libs/umfsolver.o -Wl,--out-implib,.libs/libStatechumSolver.a $(UMFPACK_LIBS)' >>Makefile
make .libs/libStatechumSolver.dll

else
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

else
# anything not Windows
	make
fi

