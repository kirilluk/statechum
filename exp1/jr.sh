#!/bin/sh
STATECHUM=/home/kirill/workspace/XMachineTool
java -cp ${STATECHUM}/bin:${STATECHUM}/lib/commons-collections-3.1.jar:$STATECHUM/lib/colt.jar:$STATECHUM/lib/jung-1.7.6.jar:$STATECHUM/NightTrain/lib/junit-4.1.jar:$STATECHUM/bin -ea -Xmx512m -DVIZ_CONFIG=kirill_home -Dthreadnum=2 -XX:NewRatio=1 -XX:+CITime "$@"
