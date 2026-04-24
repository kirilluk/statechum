package statechum.apps;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;
import ext_lib.collections.HashMapWithSearch;
import statechum.*;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.FsmParserDot;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.collections.MapWithSearch;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

// How to run:
// java -cp bin;lib/modified_collections;lib/colt.jar;lib/commons-collections-3.1.jar;lib/jung-1.7.6.jar;lib/OtpErlang/24/OtpErlang.jar statechum.apps.DotStructuralDifference HOWTOFINDINITIAL A.dot B.dot
public class DotPlotStructuralDifference {
    public static void main(String[] args) throws IOException {// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.CLOSE_TERMINATE, "true");
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.ESC_TERMINATE,"false");
        final Configuration configAtomicPairs = Configuration.getDefaultConfiguration().copy();
        configAtomicPairs.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        FsmParserDot.HOW_TO_FIND_INITIAL_STATE howToFindInitial = FsmParserDot.HOW_TO_FIND_INITIAL_STATE.valueOf(args[0]);
        String referenceDot = Helper.loadFile(new File(args[1]));
        String outcomeDot = Helper.loadFile(new File(args[2]));
        MapWithSearch<String,String,Integer> useExistingNumbering = new HashMapWithSearch<>(20);
        Map<LabelInputOutput,Integer> labelToNumber = new TreeMap<>();
        LearnerGraph referenceGraphWithRejects = FsmParserDot.buildLearnerGraph(referenceDot, configAtomicPairs, null,true, false, howToFindInitial).
                transform.numberOutputsAndStates(true,"S",null,useExistingNumbering,labelToNumber);
        LearnerGraph referenceGraph = new LearnerGraph(configAtomicPairs), actualAutomaton = new LearnerGraph(configAtomicPairs);
        AbstractPathRoutines.removeRejectStates(referenceGraphWithRejects, referenceGraph);
        LearnerGraph actualAutomatonWithRejects = FsmParserDot.buildLearnerGraph(outcomeDot, configAtomicPairs, null,true, false, howToFindInitial).
                transform.numberOutputsAndStates(true,"T",null,useExistingNumbering,labelToNumber);
        AbstractPathRoutines.removeRejectStates(actualAutomatonWithRejects, actualAutomaton);
        statechum.analysis.learning.linear.GD<DeterministicDirectedSparseGraph.CmpVertex, DeterministicDirectedSparseGraph.CmpVertex, LearnerGraphCachedData,LearnerGraphCachedData> gd = new statechum.analysis.learning.linear.GD<>();
        DirectedSparseGraph gr = gd.showGD(
                referenceGraph,actualAutomaton,
                ExperimentRunner.getCpuNumber());gr.setUserDatum(JUConstants.TITLE, "diff_"+referenceGraph.getName()+"-"+actualAutomaton.getName(), UserData.SHARED);
        Visualiser.updateFrameWithPos(gr,2);Visualiser.waitForKey();
    }

}
