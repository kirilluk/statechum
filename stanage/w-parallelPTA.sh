#!/bin/sh
#SBATCH --mail-user=acp95keb@sheffield.ac.uk
#SBATCH --output=log/Array_test.%A_%a.log

EXPERIMENT=statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment

sh ./statechum.sh -Xmx7900m ${EXPERIMENT} COUNT_TASKS_PARALLELPTA 4

