#!/bin/sh
if [ ${#1} == 0 -o ! -r ${1:-.} ];then echo Arguments: file containing machines to run; exit;fi
MACHINES=$1
OUTPUT=output_$MACHINES
JOBNAME=LEARNER-`whoami`-$$
DATASETNO=`../jr.sh statechum.analysis.learning.AccuracyAndQuestionsExperiment $MACHINES $OUTPUT -1`
if [ $DATASETNO == 0 ];then echo No files to process. ;exit;fi
mkdir -p $OUTPUT 
rm $OUTPUT/*
/bin/date > $OUTPUT/started
# The first argument to tasks is the machines file and the second one is the output directory
qsub -cwd -l qp=LOW -l mclass=c1 -t 1-$DATASETNO -N $JOBNAME -j y -o $OUTPUT/LOG learner.sh $MACHINES $OUTPUT || exit 
qsub -cwd -l qp=LOW -l mclass=c3 -j y -o $OUTPUT/finished -M `whoami`@dcs.shef.ac.uk -m e -hold_jid $JOBNAME finished.sh $MACHINES $OUTPUT || exit 
echo Succesfully submitted

