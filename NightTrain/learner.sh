#!/bin/sh
FILE=`awk 'BEGIN { k = 1; } { if (k == ENVIRON["SGE_TASK_ID"]) { print $1 } ++k; }' $1`
STATECHUM=..
java -cp $STATECHUM/lib/commons-collections-3.1.jar:$STATECHUM/lib/colt.jar:$STATECHUM/lib/jung-1.7.4.jar:$STATECHUM/NightTrain/lib/junit-4.1.jar:$STATECHUM/bin -ea -Xmx600m -XX:NewRatio=1 statechum.analysis.learning.AccuracyAndQuestionsExperiment $FILE junk >output/$SGE_TASK_ID.txt 2>&1


