#!/bin/sh
STATECHUM=..
java -cp $STATECHUM/lib/commons-collections-3.1.jar:$STATECHUM/lib/colt.jar:$STATECHUM/lib/jung-1.7.4.jar:$STATECHUM/NightTrain/lib/junit-4.1.jar:$STATECHUM/bin -ea -Xmx512m  -XX:NewRatio=1 "$@"

