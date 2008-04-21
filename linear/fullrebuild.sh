#!/bin/sh
#./bootstrap && ./configure --with-blasdir=/cygdrive/d/experiment/ATLAS/builddir/lib/dll --with-umfpack=/cygdrive/d/experiment && make
./bootstrap && ./configure --with-blasdir=/usr/local/soft/atlas-3.8.1/lib --with-umfpack=/usr/local/src/umfpack && make

