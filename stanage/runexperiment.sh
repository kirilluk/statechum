#!/bin/sh

WHATTORUN=${1}

if [ -z "${WHATTORUN}" ];then
	echo No experiment to run specified.
	exit
fi

EMAIL=acp95keb@sheffield.ac.uk

if [ -z "$R_HOME" ]; then
	source run_env.sh
fi
source ./defaultjobparameters.sh

[ -r data ] || mkdir data
(cd ..;ant compileStatechum )

# sets the environment variable and then runs the script which will request the number of tasks to be computed
TASKNUMBER=`STATECHUM_COUNT=count sh ${WHATTORUN}`
if [ -z ${TASKNUMBER+x} ];then
# thanks to http://stackoverflow.com/questions/3601515/how-to-check-if-a-variable-is-set-in-bash
	echo Failed to obtain the number of tasks.
	exit
fi

JID=`sbatch --parsable --job-name=${WHATTORUN} ${DEFAULTJOBPARAMETERS} --array=1-$TASKNUMBER ${WHATTORUN} | cut -d ";" -f 1`
sbatch --dependency=afterany:$JID ${DEFAULTJOBPARAMETERS} ${WHATTORUN}

