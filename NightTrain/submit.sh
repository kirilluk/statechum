#!/bin/sh
MACHINES=machines
MACHNO=`wc -l $MACHINES|awk '{ print $1; }'`
qsub -cwd -l qp=LOW -l mclass=c1 -t 1-$MACHNO -j y -o output/LOG learner.sh $MACHINES

