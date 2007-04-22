#!/bin/sh
if [ ${#1} == 0 -o ! -r ${1:-.} ];then echo Arguments: file containing machines to run; exit;fi
MACHINES=$1
OUTPUT=output_$MACHINES
JOBNAME=LEARNER-`whoami`-$$
[ -r $OUTPUT ] || mkdir $OUTPUT 
echo rm $OUTPUT/*
/bin/date > $OUTPUT/started
DATASETNO=`../jr.sh statechum.analysis.learning.AccuracyAndQuestionsExperiment $MACHINES -1`
# The first argument to tasks is the machines file and the second one is the output directory
echo qsub -cwd -l qp=LOW -l mclass=c1 -t 1-$DATASETNO -N $JOBNAME -j y -o $OUTPUT/LOG learner.sh $MACHINES $OUTPUT
qsub -cwd -l qp=LOW -l mclass=c3 -j y -o $OUTPUT/finished -m e -hold_jid $JOBNAME finished.sh $MACHINES $OUTPUT || exit 
echo Succesfully submitted

