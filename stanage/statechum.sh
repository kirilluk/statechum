#!/bin/sh
. ./run_env.sh
TEMPDIR=-Djava.io.tempdir=./java_tempdir
java -cp ../bin:../lib/junit-statechum:../lib/modified_collections:../lib/colt.jar:../lib/commons-collections-3.1.jar:../lib/javaGD.jar:../lib/jltl2ba.jar:../lib/JRI.jar:../lib/jung-1.7.6.jar:../lib/junit-4.8.1.jar:../lib/OtpErlang/24/OtpErlang.jar:../lib/polyglotclasses-1.3.4.jar:../lib/sootclasses.jar:../lib/weka.jar -ea -XX:+UseCompressedOops -DASSERT_ENABLED=true -DVIZ_DIR=../resources/graphLayout -Dthreadnum=1 -Djava.library.path=../linear/.libs:../smt/.libs:${JRI_PATH} -DLTL2BA=../lib/ltl2ba-1.1/ltl2ba -DERLANGOUTPUT_ENABLED=true -DSGE_DISABLEGRAPHSAVE=true ${TEMPDIR} "$@"
