package statechum.analysis.learning.rpnicore;

import statechum.DeterministicDirectedSparseGraph;
import statechum.Label;
import statechum.LabelInputOutput;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.StatePair;

import java.util.*;

import static org.junit.Assert.fail;

public class HelperWMethod {

    /** Checks if the two graphs have the same set of states. */
    public static boolean sameStateSet(LearnerGraph expected, LearnerGraph graph)
    {
        Set<DeterministicDirectedSparseGraph.CmpVertex> A=expected.transitionMatrix.keySet(), B=graph.transitionMatrix.keySet();
        Set<DeterministicDirectedSparseGraph.CmpVertex> AmB,BmA;
        AmB = new TreeSet<DeterministicDirectedSparseGraph.CmpVertex>(A);AmB.removeAll(B);
        BmA = new TreeSet<DeterministicDirectedSparseGraph.CmpVertex>(B);BmA.removeAll(A);
        if (!A.equals(B))
        {
            System.out.println("different sets of states,\nA-B="+AmB+"\nB-A="+BmA);
        }
        return A.equals(B);
    }

    /** Given a W set, checks if it is a valid W set for the current state machine and throws if not.
     *
     * @param wset the set to check validity of.
     * @param prefixClosed whether we are talking of prefix-closed languages
     * @param equivalentVertices the set of equivalent vertices which should be ignored. Can be null if not used.
     */
    public static void checkW_is_corrent(final LearnerGraph coregraph, Collection<List<Label>> wset, boolean prefixClosed, Set<StatePair> equivalentVertices)
    {
        String result = coregraph.wmethod.checkW_is_corrent_boolean(wset,prefixClosed,equivalentVertices);
        if (result != null)
            fail(result);
    }

    /** Given a W set, checks if it is a valid W set for the current state machine and throws if not.
     *
     * @param wset the set to check validity of.
     * @param equivalentVertices the set of equivalent vertices which should be ignored. Can be null if not used.
     */
    public static String checkW_Mealy_is_correct_boolean(final LearnerGraph coregraph, Collection<List<Label>> wset, Set<StatePair> equivalentVertices)
    {
        for(DeterministicDirectedSparseGraph.CmpVertex stateA:coregraph.transitionMatrix.keySet())
        {
            for(DeterministicDirectedSparseGraph.CmpVertex stateB:coregraph.transitionMatrix.keySet())
                if (stateA != stateB && (equivalentVertices == null ||
                        (!equivalentVertices.contains(new StatePair(stateA, stateB)) &&
                                !equivalentVertices.contains(new StatePair(stateB, stateA)))))
                {
                    boolean foundString = false;
                    Iterator<List<Label>> pathIt = wset.iterator();
                    while(pathIt.hasNext() && !foundString)
                    {
                        List<Label> path = pathIt.next();
                        List<LabelInputOutput> ioA = coregraph.paths.pathToInputOutputPairs(path,stateA);
                        List<LabelInputOutput> ioB = coregraph.paths.pathToInputOutputPairs(path,stateB);

                        // Equality for LabelInputOutput is based on inputs only hence we need to do 'deep' equals.
                        if (!LabelInputOutput.deepEqualsCollection(ioA,ioB))
                            foundString = true;
                    }

                    if (!foundString)
                        return "W set "+wset+" does not distinguish between "+stateA+" and "+stateB;
                }
        }

        return null;
    }

}
