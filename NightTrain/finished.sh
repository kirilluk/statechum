#!/bin/sh
# All output from this one goes into standard output, piped into log by qsub
# It is possible to run this one directly in order to have a look at what it would produce
MACHINES=$1
OUTPUT=$2
rm $OUTPUT/result.csv
for file in `ls $OUTPUT/*result*txt`;do awk '{ print;exit }' $file >> $OUTPUT/result.csv;done

awk 'BEGIN {FS=",";minAcc=2.;failureNumber=0;} { if ($3 != "SUCCESS") failureNumber++; else { questionSum+=$4;lineCount++;accuracySum+=$5;if (minAcc>$5) minAcc=$5; }} END {print "Total entries: ",lineCount+failureNumber, ", of them failures: ",failureNumber,", total questions : ",questionSum," average questions: ",questionSum/lineCount, " min accuracy: ",minAcc," average accuracy: ",accuracySum/lineCount; }' $OUTPUT/result.csv 

date

