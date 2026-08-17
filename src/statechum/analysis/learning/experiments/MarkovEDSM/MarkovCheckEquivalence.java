package statechum.analysis.learning.experiments.MarkovEDSM;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static java.lang.Math.abs;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.*;
import static statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.RESULT_VALUES.*;

public class MarkovCheckEquivalence {

    public static void main(String []args) {
        SGE_ExperimentRunner.PhaseEnum curPhase = null;
        List<DrawGraphs.CSVExperimentResult> twoExperiments = new ArrayList<>();
        String [] experimentsToCompare = new String[]{"markov-fast_computation","markov-supposedly_original_computation"};
        for(String namePrefix: experimentsToCompare) {
            String outDir = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_EXPERIMENTRESULTS) + File.separator + namePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
            UASExperiment.mkDir(outDir);

            MarkovExperiment.LearningExperimentGroupParameters learningGroup = new MarkovExperiment.LearningExperimentGroupParameters();

            learningGroup.outPathPrefix = outDir + File.separator;
            learningGroup.eval = UASExperiment.constructLearnerInitConfiguration();
            learningGroup.eval.config.setTransitionMatrixImplType(Configuration.STATETREE.STATETREE_LINKEDHASH);// small automata hence no need for array STATETREE.STATETREE_ARRAY);
            //STATETREE_ARRAY);
            learningGroup.eval.config.setLearnerScoreMode(Configuration.ScoreMode.GENERAL_NOFULLMERGE);
            learningGroup.eval.config.setTimeOut(3600000L * 16L);// timeout for tasks, in milliseconds, equivalent to 16hrs runtime for an old Xeon 5670 @ 2.93Ghz, modern E5/i7 are 3x faster.
            learningGroup.eval.config.setOverride_usePTAMerging(false);

            SGE_ExperimentRunner.configureCPUFreqNormalisation();

            learningGroup.experimentRunner = new SGE_ExperimentRunner.RunSubExperiment<>(ExperimentRunner.getCpuNumber(), learningGroup.outPathPrefix + directoryExperimentResult, args);
            learningGroup.phase = learningGroup.experimentRunner.getPhase();
            if (null == curPhase)
                curPhase = learningGroup.phase;
            twoExperiments.add(E_MarkovBaselineLearn.runExperiment(learningGroup));
        }

        if (curPhase == SGE_ExperimentRunner.PhaseEnum.COLLECT_AVAILABLE || curPhase == SGE_ExperimentRunner.PhaseEnum.COLLECT_RESULTS) {// by the time we are here, experiments for the current number of states have completed, hence record the outcomes.
            Set<RESULT_VALUES> invalidCellValuesA = obtainValidityOfCellValues(twoExperiments.get(0));
            Set<RESULT_VALUES> invalidCellValuesB = obtainValidityOfCellValues(twoExperiments.get(1));
            DrawGraphs gr = new DrawGraphs();
            String pathToResult = GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.PATH_EXPERIMENTRESULTS) + File.separator;
            final DrawGraphs.RBagPlot gr_Time = new DrawGraphs.RBagPlot(experimentsToCompare[0], experimentsToCompare[1],
                    new File(pathToResult+"time_difference.pdf"));
            final DrawGraphs.RBoxPlot<String> time_fast_divided_by_original = new DrawGraphs.RBoxPlot<>("Fast/Original", "Value", new File(pathToResult + "TimeFastDividedByOriginal.pdf"));
            boolean differenceObserved = false;
            int countAabove = 0,countBabove=0;
            for (Map.Entry<String, Map<String, String>> rowEntryA : twoExperiments.get(0).rowColumnText.entrySet()) {
                Map<String, String> entryB = twoExperiments.get(1).rowColumnText.get(rowEntryA.getKey());
                ColumnAndValue cellsA = getValueFromMapGivenSelector(rowEntryA.getValue(), new MarkovExperiment.ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV),invalidCellValuesA);
                double valueA = obtainDoubleValueFromCell(cellsA.value, E_DIFF, cellsA.column);
                double valueBCRA = obtainDoubleValueFromCell(cellsA.value, E_BCR, cellsA.column);
                double valueStructuralA = obtainDoubleValueFromCell(cellsA.value, E_DIFF, cellsA.column);

                double timeA = obtainDoubleValueFromCell(cellsA.value, E_RUNTIME, cellsA.column);
                ColumnAndValue cellsB = getValueFromMapGivenSelector(entryB, new MarkovExperiment.ColLearner(LearningAlgorithms.ScoringToApply.SCORING_MARKOV),invalidCellValuesB);
                double valueBCRB = obtainDoubleValueFromCell(cellsB.value, E_BCR, cellsB.column);
                double valueStructuralB = obtainDoubleValueFromCell(cellsB.value, E_DIFF, cellsB.column);
                double timeB = obtainDoubleValueFromCell(cellsB.value, E_RUNTIME, cellsB.column);

                if (timeA > 5 || timeB > 5) {
                    gr_Time.add(timeA, timeB);
                    if (timeA > timeB)
                        countAabove++;
                    if (timeA < timeB)
                        countBabove++;

                    time_fast_divided_by_original.add("F/O",timeA/timeB);
                }

                if (abs(valueBCRB-valueBCRA) > 1e-6) {
                    System.out.println("Difference: " + rowEntryA.getKey() + " between " + valueBCRA + " and " + valueBCRB);
                    differenceObserved = true;
                }
                if (abs(valueStructuralB-valueStructuralA) > 1e-6) {
                    System.out.println("Difference: " + rowEntryA.getKey() + " between " + valueStructuralA + " and " + valueStructuralB);
                    differenceObserved = true;
                }
            }

            gr_Time.reportResults(gr);time_fast_divided_by_original.reportResults(gr);
            if (!differenceObserved)
                System.out.println("No difference in BCR or structural scores");
            System.out.println("CountAabove: " + countAabove+ ", countBabove: " + countBabove);
        }

        DrawGraphs.end();
    }
}
