#!/bin/sh
echo "Setting up environment by loading modules"
module use $HOME/modules
module load Java/11.0.20
module load R/4.4.1-foss-2022b
module load CMake/3.24.3-GCCcore-12.2.0
module load SuiteSparse/5.13.0-foss-2022b-METIS-5.1.0
module load ant/1.10.12-Java-11
module load yices
module load PCRE2/10.40-GCCcore-12.2.0
module load libiconv/1.17-GCCcore-12.2.0
module load zlib/1.2.12-GCCcore-12.2.0
module load libpng/1.6.38-GCCcore-12.2.0
module load ncurses/6.3-GCCcore-12.2.0

export R_HOME=`R RHOME`
export JRI_PATH=$HOME/R/x86_64-pc-linux-gnu-library/4.4/rJava/jri

