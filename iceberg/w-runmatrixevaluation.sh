#!/bin/sh
#$ -l mem=16G -l rmem=12g -l 'arch=intel*'

EXPERIMENT=statechum.analysis.learning.experiments.EvaluationOfLearners.EvaluationOfLearnersAndMatrixTypes

if [ -z ${SGE_TASK_ID+x} ] || [ "${SGE_TASK_ID}" == "undefined" ];then
	if [ -z ${STATECHUM_COUNT+x} ];then
		# thanks to http://stackoverflow.com/questions/3601515/how-to-check-if-a-variable-is-set-in-bash
		sh ./runexperiment.sh -Xmx11000m ${EXPERIMENT} COLLECT_RESULTS
	else
		sh ./runexperiment.sh -Xmx3000m ${EXPERIMENT} COUNT_TASKS 30
	fi
else
# if task id is not "undefined", it means we are running an array task
	sh ./runexperiment.sh -Xmx11000m ${EXPERIMENT} RUN_TASK $SGE_TASK_ID
fi

