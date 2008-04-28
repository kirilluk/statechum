#!/bin/sh
if [ ${#1} == 0 -o ! -r ${1:-.} ];then echo Arguments: file containing machines to run; exit;fi
MACHINES=$1
OUTPUT=output_$MACHINES
JOBNAME=LEARNER-`whoami`-$$
DATASETNO=`../jr.sh statechum.analysis.learning.AccuracyAndQuestionsExperiment $MACHINES $OUTPUT -1`
if [ $DATASETNO == 0 ];then echo No files to process. ;exit;fi
if [ $[DATASETNO % 100] != 0 ];then echo Job number has to be divisible by 100.; exit;fi
COUNT=$[DATASETNODATASETNO / 100]
mkdir -p $OUTPUT 
rm $OUTPUT/*
/bin/date > $OUTPUT/started
# The first argument to tasks is the machines file and the second one is the output directory
qsub -cwd -l qp=LOW -l mclass=c1 -t 1-$[DATASETNO + 1]:$COUNT -N $JOBNAME -j y -o $OUTPUT/LOG learner.sh $MACHINES $OUTPUT $COUNT || exit 
qsub -cwd -l qp=LOW -l mclass=c3 -j y -o $OUTPUT/finished -hold_jid $JOBNAME -M `whoami`@dcs.shef.ac.uk -m e finished.sh $MACHINES $OUTPUT || exit 
echo Succesfully submitted

