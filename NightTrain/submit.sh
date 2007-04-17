#!/bin/sh
MACHINES=machines
/bin/date > output/started
MACHNO=`wc -l $MACHINES|awk '{ print $1; }'`
qsub -cwd -l qp=LOW -l mclass=c1 -t 1-$MACHNO -j y -o output/LOG learner.sh $MACHINES 
#| awk '{ gsub("\\..*","",$3);print $3; }'`
qsub -cwd -l qp=LOW -l mclass=c3 -j y -o output/finished -hold_jid learner.sh finished.sh

