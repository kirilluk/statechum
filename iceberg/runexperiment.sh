#!/bin/sh
[ -r data ] || mkdir data
export LD_LIBRARY_PATH=/data/acp95keb/local/soft/gmp-5.1.3/lib:/home/acp95keb/R/x86_64-unknown-linux-gnu-library/3.1/rJava/jri:/data/acp95keb/local/soft/yices-1.0.38/lib:"$JAVA_LD_LIBRARY_PATH":"$LD_LIBRARY_PATH"
export R_HOME=/usr/local/packages6/R/3.1.2/lib64/R
java -cp ../bin:../lib/junit-statechum:../lib/modified_collections:../lib/colt.jar:../lib/commons-collections-3.1.jar:../lib/javaGD.jar:../lib/jltl2ba.jar:../lib/JRI.jar:../lib/jung-1.7.6.jar:../lib/junit-4.8.1.jar:../lib/OtpErlang.jar:../lib/polyglotclasses-1.3.4.jar:../lib/sootclasses.jar:../lib/weka.jar -ea -XX:+UseCompressedOops -DASSERT_ENABLED=true -DVIZ_DIR=../resources/graphLayout -Dthreadnum=1 -Djava.library.path=../linear/.libs:../smt/.libs:/home/acp95keb/R/x86_64-unknown-linux-gnu-library/3.1/rJava/jri -DLTL2BA=../lib/ltl2ba-1.1/ltl2ba -DERLANGHOME=/data/acp95keb/local/soft/otp-R16B03-1 -DERLANGOUTPUT_ENABLED=true "$@"
