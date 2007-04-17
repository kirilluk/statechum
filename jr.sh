#!/bin/sh
java -cp lib/commons-collections-3.1.jar:lib/colt.jar:lib/jung-1.7.4.jar:NightTrain/lib/junit-4.1.jar:bin -ea -Xmx512m  -XX:NewRatio=1 "$@"

