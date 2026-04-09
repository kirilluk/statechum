#!/bin/sh
module use $HOME/modules
module load Java/11.0.20
module load R/4.4.1-foss-2022b
module load SuiteSparse/5.13.0-foss-2022b-METIS-5.1.0
module load ant/1.10.12-Java-11
module load yices

export R_HOME=`R RHOME`
export JRI_PATH=$HOME/R/x86_64-pc-linux-gnu-library/4.4/rJava/jri

