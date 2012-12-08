#!/bin/sh

# Essentially from http://www.kevinsheppard.org/research/matlabatlas/

[ -r /usr/local/soft/yices-1.0.36 ] && YICES_ROOT=/usr/local/soft/yices-1.0.36

HOST=
# For building on Windows-64, there is a special kludge, only tested on Win64.
if uname | grep -q WOW64;then
	HOST="--host=x86_64-w64-mingw32"
	export PATH="${PATH}:/cygdrive/c/Program Files/Java/jdk1.7.0_07/bin"
	#export CC=x86_64-w64-mingw32-gcc CXX=x86_64-w64-mingw32-g++ F77=x86_64-w64-mingw32-gfortran AR=x86_64-w64-mingw32-ar RANLIB=x86_64-w64-mingw32-ranlib
fi

./bootstrap && ./configure --with-yices=${YICES_ROOT} ${HOST}
touch yices_caller.c

if uname | grep -q CYGWIN;then
if uname | grep -q WOW64;then
# Win64 build

echo '.libs/libSMT_Yices.dll:yices_caller.lo'>>Makefile
# the idea of implib is from http://www.mingw.org/wiki/sampleDLL
echo -e '\t$(CC) -shared -o .libs/libSMT_Yices.dll .libs/yices_caller.o -Wl,--out-implib,.libs/libSMT_Yices.a $(YICES_LIBS)' >>Makefile
make .libs/libSMT_Yices.dll

else
# Win32 build

LIBDIR=lib
GCC=gcc

[ -r ${LIBDIR} ] || mkdir ${LIBDIR}

(rm ${LIBDIR}/*.o ${LIBDIR}/*.a) >/dev/null 2>&1

echo 'Creating gcc import library...'
# based on http://www.cygwin.com/cygwin-ug-net/dll.html
(cd ${LIBDIR} && dlltool --def ../libyices.def --dllname ${YICES_ROOT}/libyices.dll --output-lib libyices_win32.a )

cat Makefile|sed -e "s%^libSMT_Yices_la_LDFLAGS.*%libSMT_Yices_la_LDFLAGS = -no-undefined -version-info 1:0:0 -mno-cygwin -L${LIBDIR} -lyices_win32%" > Makefile_win32
make -f Makefile_win32

fi

else
# anything not Windows
	make
fi
