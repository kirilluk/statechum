#!/bin/sh
EXPERIMENTNAME=edsmmarkov
WHATTORUN=w-run${EXPERIMENTNAME}.sh
EMAIL=acp95keb@sheffield.ac.uk

[ -r data ] || mkdir data
rm -f tmp/${EXPERIMENTNAME}-OUT.txt tmp/${EXPERIMENTNAME}-ERR.txt

 /data/acp95keb/local/soft/apache-ant-1.9.4/bin/ant compileStatechum

TASKNUMBER=`STATECHUM_COUNT=count sh ${WHATTORUN}`
if [ -z ${TASKNUMBER+x} ];then
# thanks to http://stackoverflow.com/questions/3601515/how-to-check-if-a-variable-is-set-in-bash
	echo Failed to obtain the number of tasks.
	exit
fi

# thanks to http://collaborate.bu.edu/linga/SGE/JobArray
JID=`qsub -terse -cwd -o tmp/${EXPERIMENTNAME}-OUT.txt -e tmp/${EXPERIMENTNAME}-ERR.txt -l h_rt=576000 -t 1-$TASKNUMBER ${WHATTORUN} | sed -r "s/\.(.*)//"`
qsub -cwd -terse -cwd -o tmp/${EXPERIMENTNAME}-OUT.txt -e tmp/${EXPERIMENTNAME}-ERR.txt -hold_jid $JID -m e  -M ${EMAIL} ${WHATTORUN}

