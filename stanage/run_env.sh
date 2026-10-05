#!/bin/sh
module use $HOME/modules
#module load Java/11.0.20
#module load R/4.4.1-foss-2022b
#module load SuiteSparse/5.13.0-foss-2022b-METIS-5.1.0
#module load ant/1.10.12-Java-11

module load CMake/3.31.8-GCCcore-14.3.0
module load OpenBLAS/0.3.30-GCC-14.3.0
module load Java/21.0.8
# now make sure that libjvm.so is accessible
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${LIBRARY_PATH}/server"
module load R/4.5.2-gfbf-2025b
module load yices
module load ant
module load suitesparse

export R_HOME=`R RHOME`
export JRI_PATH=$HOME/R/x86_64-pc-linux-gnu-library/4.5/rJava/jri

