#!/bin/sh

# Essentially from http://www.kevinsheppard.org/research/matlabatlas/

YICES_ROOT=
for p in /usr/local/soft/yices-1.0.* $HOME/yices-1.0.* $HOME/experiment/yices-1.0.* ;do
	if [ -r ${p} ];then
		YICES_ROOT=${p}
		break
	fi
done

if [ -z "${YICES_ROOT}" ];then
	echo Failed to find yices 1.0 install
	exit
fi

[ -r build ] && rm -rf build
mkdir build
(cd build;cmake -DYICES_ROOT=${YICES_ROOT} .. && cmake --build . && cmake --install .)
