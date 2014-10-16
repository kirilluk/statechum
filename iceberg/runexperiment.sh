#!/bin/sh
[ -r data ] || mkdir data
export PATH=/data/acp95keb/local/bin:"$PATH"
export LD_LIBRARY_PATH=/data/acp95keb/local/soft/gmp-5.1.3/lib:/data/acp95keb/local/soft/jdk1.7.0_60/jre/lib/amd64/server/:/data/acp95keb/local/soft/R-3.1.0/lib64/R/lib/:/data/acp95keb/local/soft/R-3.1.0/lib64/R/library/rJava/jri:/data/acp95keb/local/soft/yices-1.0.38/lib:"$LD_LIBRARY_PATH"
export R_HOME=/data/acp95keb/local/soft/R-3.1.0/lib64/R
java -cp ../bin:../lib/junit-statechum:../lib/modified_collections:../lib/colt.jar:../lib/commons-collections-3.1.jar:../lib/javaGD.jar:../lib/jltl2ba.jar:../lib/JRI.jar:../lib/jung-1.7.6.jar:../lib/junit-4.8.1.jar:../lib/OtpErlang.jar:../lib/polyglotclasses-1.3.4.jar:../lib/sootclasses.jar:../lib/weka.jar -ea -XX:+UseCompressedOops -DASSERT_ENABLED=true -DVIZ_DIR=../resources/graphLayout -Dthreadnum=2 -Djava.library.path=../linear/.libs:../smt/.libs:/data/acp95keb/local/soft/R-3.1.0/lib64/R/library/rJava/jri -DLTL2BA=../lib/ltl2ba-1.1/ltl2ba -DERLANGHOME=/data/acp95keb/local/soft/otp-R16B03-1 -DERLANGOUTPUT_ENABLED=true "$@"