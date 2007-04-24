#!/bin/sh
MACHINES=$1
OUTPUT=$2
COUNT=$3
TASK_ID=`expr $SGE_TASK_ID - 1`
STATUS=$OUTPUT/status_$TASK_ID
for((i=$TASK_ID;i<$TASK_ID+$COUNT;++i));do TASK_RANGE="${TASK_RANGE:-} $i";done
date> $STATUS
free>>$STATUS
date> $OUTPUT/running_$TASK_ID
../jr.sh -ea -Xmx850m -XX:NewRatio=1 -Xms800m statechum.analysis.learning.AccuracyAndQuestionsExperiment $MACHINES $OUTPUT $TASK_RANGE 
date>>$STATUS
free>>$STATUS
rm    $OUTPUT/running_$TASK_ID

