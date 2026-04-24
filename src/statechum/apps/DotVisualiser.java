package statechum.apps;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.FsmParserDot;
import statechum.analysis.learning.rpnicore.LearnerGraph;

import java.io.File;
import java.io.IOException;

// How to run:
// java -cp bin;lib/modified_collections;lib/colt.jar;lib/commons-collections-3.1.jar;lib/jung-1.7.6.jar;lib/OtpErlang/24/OtpErlang.jar statechum.apps.DotVisualiser HOWTOFINDINITIAL A.dot
public class DotVisualiser {
    public static void main(String[] args) throws IOException {// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.CLOSE_TERMINATE, "true");
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.ESC_TERMINATE,"false");
        final Configuration configAtomicPairs = Configuration.getDefaultConfiguration().copy();
        configAtomicPairs.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        FsmParserDot.HOW_TO_FIND_INITIAL_STATE howToFindInitial = FsmParserDot.HOW_TO_FIND_INITIAL_STATE.valueOf(args[0]);
        String referenceDot = Helper.loadFile(new File(args[1]));
        LearnerGraph graphAToPlot = FsmParserDot.buildLearnerGraph(referenceDot, configAtomicPairs, null,true, false, howToFindInitial).
                transform.convertIO().transform.numberOutputsAndStates(true,"S",null,null, null);

        LearnerGraph graphBToPlot = null;
        if (args.length > 2) {
            String actualDot = Helper.loadFile(new File(args[2]));
            graphBToPlot = FsmParserDot.buildLearnerGraph(actualDot, configAtomicPairs, null,true, false, howToFindInitial).
                    transform.convertIO().transform.numberOutputsAndStates(true,"T",null,null, null);
        }
        Visualiser.updateFrame(graphAToPlot,graphBToPlot);Visualiser.waitForKey();
    }

}
