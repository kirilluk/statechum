#!/bin/sh
#SBATCH --mail-user=acp95keb@sheffield.ac.uk
#SBATCH --output=log/Array_test.%A_%a.log

EXPERIMENT=statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment

if [ -z ${SLURM_ARRAY_TASK_ID+x} ] || [ "${SLURM_ARRAY_TASK_ID}" == "undefined" ];then
	if [ -z ${STATECHUM_COUNT+x} ];then
		# thanks to http://stackoverflow.com/questions/3601515/how-to-check-if-a-variable-is-set-in-bash
		sh ./statechum.sh -Xmx13000m ${EXPERIMENT} COLLECT_RESULTS
	else
		sh ./statechum.sh -Xmx13000m ${EXPERIMENT} COUNT_TASKS 50
	fi
else
# if task id is not "undefined", it means we are running an array task
	sh ./statechum.sh -Xmx13000m ${EXPERIMENT} RUN_TASK $SLURM_ARRAY_TASK_ID
fi

