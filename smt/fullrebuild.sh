#!/bin/sh

# Essentially from http://www.kevinsheppard.org/research/matlabatlas/

[ -r /usr/local/soft/yices-1.0.16 ] && YICES_ROOT=/usr/local/soft/yices-1.0.16

./bootstrap && ./configure --with-blasdir=${ATLAS} --with-yices=${YICES_ROOT}
touch yices_caller.c

if uname | grep -q CYGWIN;then
# Win32 build

LIBDIR=lib
GCC=gcc

[ -r ${LIBDIR} ] || mkdir ${LIBDIR}

(rm ${LIBDIR}/*.o ${LIBDIR}/*.a) >/dev/null 2>&1

echo 'Linking DLL and creating gcc import library...'
(cd ${LIBDIR} && ${GCC} -mno-cygwin -shared -o libyices.dll ${YICES_ROOT}/lib/libyices.def \
    yices.a -lpthread -lg2c \
    -Wl,--enable-auto-import \
    -Wl,--no-whole-archive \
    -Wl,--out-implib=libyices_win32.a)

cat Makefile|sed -e "s%^libSMT_Yices_la_LDFLAGS.*%libSMT_Yices_la_LDFLAGS = -no-undefined -version-info 1:0:0 -mno-cygwin -L${LIBDIR} -lyices_win32%" > Makefile_win32
make -f Makefile_win32
else
make
fi


