package statechum.apps;

import statechum.Configuration;
import statechum.Helper;
import statechum.Label;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.rpnicore.FsmParserDot;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.List;

class WMethodBetweenDotAutomata {
        public static final Configuration config = Configuration.getDefaultConfiguration().copy();
        /** Label converter to use. */
        public static final Transform.ConvertALabel converter = new Transform.InternStringLabel();

        public static String returnCodeToString(int val) {
                if (val < 0) {
                        assert val == AbstractOracle.USER_ACCEPTED;
                        return "ACCEPT";
                }
                else
                        return Integer.toString(val);
        }

        /** Constructs W test set from the graph loaded from the first file (removing error-transitions on load)
         * and applies it to the graph loaded from the second file, using
         * the number of extra state supplied as the third argument.
         *
         * For example, running this from the Statechum root directory (the one containing bin and lib directories):
         * (Windows)
         * java -cp bin;lib/modified_collections;lib/colt.jar;lib/commons-collections-3.1.jar;lib/jung-1.7.6.jar;lib/OtpErlang.jar statechum.apps.WMethodBetweenDotAutomata fileReference.dot fileHypothesis.dot 2
         * (Linux)
         * java -cp bin:lib/modified_collections:lib/colt.jar:lib/commons-collections-3.1.jar:lib/jung-1.7.6.jar:lib/OtpErlang.jar statechum.apps.WMethodBetweenDotAutomata fileReference.dot fileHypothesis.dot 2
         */
        public static void main(String[] args) throws IOException {
                String referenceDot = Helper.loadFile(new File(args[0]));
                LearnerGraph reference = FsmParserDot.buildLearnerGraph(referenceDot, config, converter,true);
                System.out.println("Size of reference graph: "+reference.getAcceptStateNumber()+" states");
                String hypothesisDot = Helper.loadFile(new File(args[1]));
                LearnerGraph hyp = FsmParserDot.buildLearnerGraph(hypothesisDot, config, converter, true);
                System.out.println("Size of hypothesis graph: "+hyp.getAcceptStateNumber()+" states");

                int extra_states = Integer.parseInt(args[2]);
                Collection<List<Label>> ts = hyp.wmethod.getFullTestSet(extra_states);

                System.out.println("Test set size is : " + ts.size());
                for (List<Label> seq : ts) {
                        int hyp_value = hyp.paths.tracePathPrefixClosed(seq);
                        int ref_value = reference.paths.tracePathPrefixClosed(seq);
                        if (hyp_value != ref_value) {
                                System.out.println(returnCodeToString(hyp_value) + " [hypothesis] " + returnCodeToString(ref_value) + " [reference] " + seq);
                        }
                }
        }
}