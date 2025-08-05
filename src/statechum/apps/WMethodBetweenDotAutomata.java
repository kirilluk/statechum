package statechum.apps;

import statechum.Configuration;
import statechum.Helper;
import statechum.Label;
import statechum.LabelInputOutput;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.FsmParserDot;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;

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
         * and applies it to the graph loaded from the second file,
         * the initial state either pointed at by a transition from the __start state (USE_START0 argument)
         * or by the first encountered state (FIRST_FOUND argument)
         * The number of extra states is the fourth argument.
         *
         * For example, running this from the Statechum root directory (the one containing bin and lib directories):
         * (Windows)
         * java -cp bin;lib/modified_collections;lib/colt.jar;lib/commons-collections-3.1.jar;lib/jung-1.7.6.jar;lib/OtpErlang/24/OtpErlang.jar statechum.apps.WMethodBetweenDotAutomata fileReference.dot fileHypothesis.dot __start 2
         * (Linux)
         * java -cp bin:lib/modified_collections:lib/colt.jar:lib/commons-collections-3.1.jar:lib/jung-1.7.6.jar:lib/OtpErlang/24/OtpErlang.jar statechum.apps.WMethodBetweenDotAutomata fileReference.dot fileHypothesis.dot __start 2
         */
        public static void main(String[] args) throws IOException {
                String referenceDot = Helper.loadFile(new File(args[0]));
                FsmParserDot.HOW_TO_FIND_INITIAL_STATE whereToFindInitial = FsmParserDot.HOW_TO_FIND_INITIAL_STATE.valueOf(args[2]);
                LearnerGraph reference = FsmParserDot.buildLearnerGraph(referenceDot, config, converter,true,whereToFindInitial);

                System.out.println("Size of reference graph: "+reference.getAcceptStateNumber()+" states");
                String hypothesisDot = Helper.loadFile(new File(args[1]));
                LearnerGraph hyp = FsmParserDot.buildLearnerGraph(hypothesisDot, config, converter, true,whereToFindInitial);
                System.out.println("Size of hypothesis graph: "+hyp.getAcceptStateNumber()+" states");

                int extra_states = Integer.parseInt(args[2]);
                Collection<List<Label>> ts = hyp.wmethod.getFullTestSet(extra_states);

                System.out.println("Test set size is : " + ts.size());
                for (List<Label> seq : ts) {
                        List<LabelInputOutput> io_hypothesis = hyp.paths.pathToInputOutputPairs(seq,hyp.getInit());
                        List<LabelInputOutput> io_reference = reference.paths.pathToInputOutputPairs(seq,reference.getInit());

                        // Equality for LabelInputOutput is based on inputs only hence we need to do 'deep' equals.
                        String difference = LabelInputOutput.deepDiffCollection(io_hypothesis,io_reference);
                        if (difference != null)
                                System.out.println(LabelInputOutput.toInput(seq) + ": \n"+difference);
                }

                //Visualiser.updateFrame(hyp,reference);Visualiser.waitForKey();
        }
}