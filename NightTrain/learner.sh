#!/bin/sh
PERCENTSTAGES=10
TASK_ID=`expr $SGE_TASK_ID - 1`
FILENO=`expr $TASK_ID '/' $PERCENTSTAGES`
echo number : $FILENO \$ $$
FILE=`awk "BEGIN { k = 0; } { if (k == $FILENO ) { print ; } ++k; }" $1`
STATECHUM=..
java -cp $STATECHUM/lib/commons-collections-3.1.jar:$STATECHUM/lib/colt.jar:$STATECHUM/lib/jung-1.7.4.jar:$STATECHUM/NightTrain/lib/junit-4.1.jar:$STATECHUM/bin -ea -Xmx600m -XX:NewRatio=1 statechum.analysis.learning.AccuracyAndQuestionsExperiment $FILE `expr $TASK_ID '%' $PERCENTSTAGES` 


