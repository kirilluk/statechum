#!/bin/sh

WHATTORUN=runfase2015.sh
EMAIL=acp95keb@sheffield.ac.uk

[ -r data ] || mkdir data
TASKNUMBER=`STATECHUM_COUNT=count sh ${WHATTORUN}`
if [ -z ${TASKNUMBER+x} ];then
# thanks to http://stackoverflow.com/questions/3601515/how-to-check-if-a-variable-is-set-in-bash
	echo Failed to obtain the number of tasks.
	exit
fi

# thanks to http://collaborate.bu.edu/linga/SGE/JobArray
JID=`qsub -terse -cwd -o /dev/null -e /dev/null -t 1-$TASKNUMBER ${WHATTORUN} | sed -r "s/\.(.*)//"`
qsub -cwd -terse -cwd -o data/collate.o -e data/collate.e -hold_jid $JID -m e  -M ${EMAIL} ${WHATTORUN}

