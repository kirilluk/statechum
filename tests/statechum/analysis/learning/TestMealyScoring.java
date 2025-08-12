package statechum.analysis.learning;

import org.junit.Assert;
import org.junit.Test;
import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.Label;
import statechum.LabelInputOutput;
import statechum.analysis.learning.rpnicore.*;

import java.util.LinkedList;
import java.util.List;

public class TestMealyScoring {
    Configuration testConfig = new Configuration();
    Transform.ConvertALabel converter = new Transform.ConvertALabel() {

        @Override
        public Label convertLabelToLabel(Label label) {
            if (label instanceof LabelInputOutput)
                return label;

            String text = label.toErlangTerm();
            String [] elems = text.split("\\.");
            if (elems.length != 2)
                throw new IllegalArgumentException("Invalid label, should contain input and output pair separated with a dot");
            return new LabelInputOutput(elems[0],elems[1], true,true);
        }
    };

    @Test
    public final void testMealyScoring1() {
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a.b->B / B-b.c->C", "testMealyScoring1",testConfig,converter);
        List<EquivalenceClass<DeterministicDirectedSparseGraph.CmpVertex, LearnerGraphCachedData>> mergedVertices = new LinkedList<>();
        long value = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("A"),fsm.findVertex("B")), null, mergedVertices, false);
        Assert.assertEquals(1, value);
        Assert.assertTrue(fsm.pairscores.mergePossibleForDeterministicMealyAutomaton(mergedVertices));
    }

    @Test
    public final void testMealyScoring2() {
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a.b->B / B-b.c-#C", "testMealyScoring1",testConfig,converter);
        List<EquivalenceClass<DeterministicDirectedSparseGraph.CmpVertex, LearnerGraphCachedData>> mergedVertices = new LinkedList<>();
        long value = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("A"),fsm.findVertex("B")), null, mergedVertices, false);
        Assert.assertEquals(1, value);
        Assert.assertTrue(fsm.pairscores.mergePossibleForDeterministicMealyAutomaton(mergedVertices));
    }

    @Test
    public final void testMealyScoring3() {
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a.b->B / B-a.c->C", "testMealyScoring1",testConfig,converter);
        List<EquivalenceClass<DeterministicDirectedSparseGraph.CmpVertex, LearnerGraphCachedData>> mergedVertices = new LinkedList<>();
        long value = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("A"),fsm.findVertex("B")), null, mergedVertices, false);
        Assert.assertEquals(1, value);
        Assert.assertFalse(fsm.pairscores.mergePossibleForDeterministicMealyAutomaton(mergedVertices));
    }

    @Test
    public final void testMealyScoring4() {
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a.b->B / B-a.c-#C", "testMealyScoring1",testConfig,converter);
        List<EquivalenceClass<DeterministicDirectedSparseGraph.CmpVertex, LearnerGraphCachedData>> mergedVertices = new LinkedList<>();
        long value = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("A"),fsm.findVertex("B")), null, mergedVertices, false);
        Assert.assertEquals(1, value);
        Assert.assertTrue(fsm.pairscores.mergePossibleForDeterministicMealyAutomaton(mergedVertices));
    }
}
