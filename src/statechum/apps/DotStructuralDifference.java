package statechum.apps;

import ext_lib.collections.HashMapWithSearch;
import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.LabelInputOutput;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.FsmParserDot;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.collections.MapWithSearch;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

// How to run:
// java -cp bin;lib/modified_collections;lib/colt.jar;lib/commons-collections-3.1.jar;lib/jung-1.7.6.jar;lib/OtpErlang/24/OtpErlang.jar statechum.apps.DotStructuralDifference A.dot B.dot
public class DotStructuralDifference {
    public static void main(String[] args) throws IOException {// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.CLOSE_TERMINATE, "true");
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.ESC_TERMINATE,"false");
        final Configuration configAtomicPairs = Configuration.getDefaultConfiguration().copy();
        configAtomicPairs.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        String referenceDot = Helper.loadFile(new File(args[0]));
        String outcomeDot = Helper.loadFile(new File(args[1]));
        MapWithSearch<String,String,Integer> useExistingNumbering = new HashMapWithSearch<>(20);
        Map<LabelInputOutput,Integer> labelToNumber = new TreeMap<>();
        LearnerGraph referenceGraphWithRejects = FsmParserDot.buildLearnerGraph(referenceDot, configAtomicPairs, null,true, false, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_ACCEPT_FOUND).
                transform.numberOutputsAndStates(true,"S",null,useExistingNumbering,labelToNumber);
        LearnerGraph referenceGraph = new LearnerGraph(configAtomicPairs), actualAutomaton = new LearnerGraph(configAtomicPairs);
        AbstractPathRoutines.removeRejectStates(referenceGraphWithRejects, referenceGraph);
        LearnerGraph actualAutomatonWithRejects = FsmParserDot.buildLearnerGraph(outcomeDot, configAtomicPairs, null,true, false, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_ACCEPT_FOUND).
                transform.numberOutputsAndStates(true,"T",null,useExistingNumbering,labelToNumber);
        AbstractPathRoutines.removeRejectStates(actualAutomatonWithRejects, actualAutomaton);
        System.out.println(PairQualityLearner.DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, configAtomicPairs, 1).getValue());
    }

}
