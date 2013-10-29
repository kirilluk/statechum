package statechum.analysis.learning.experiments.PairSelection;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import junit.framework.Assert;

import org.junit.Test;

import statechum.Configuration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.Label;
import statechum.Trace;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.MarkovUniversalLearner;
import statechum.analysis.learning.MarkovUniversalLearner.UpdatableOutcome;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;

public class TestMarkovLearner {
	final Configuration config = Configuration.getDefaultConfiguration().copy();
	final ConvertALabel converter = null;

	final Label lblA = AbstractLearnerGraph.generateNewLabel("a", config, converter),lblB = AbstractLearnerGraph.generateNewLabel("b", config, converter),lblC = AbstractLearnerGraph.generateNewLabel("c", config, converter),
			lblD = AbstractLearnerGraph.generateNewLabel("d", config, converter),lblU = AbstractLearnerGraph.generateNewLabel("u", config, converter);
	
	@Test
	public void testGetChunks1() {
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{})),1);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks2a() {
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC})),4);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks2b() {
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC})),10);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks3() {
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC})),1);
		Assert.assertEquals(3,l.size());
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblA})),l.get(0));
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblB})),l.get(1));
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblC})),l.get(2));
	}

	@Test
	public void testGetChunks4() {
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC})),2);
		Assert.assertEquals(2,l.size());
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblA,lblB})),l.get(0));
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblB,lblC})),l.get(1));
	}

	@Test
	public void testGetChunks5() {
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC})),3);
		Assert.assertEquals(1,l.size());
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC})),l.get(0));
	}

	
	@Test
	public void testCreateMarkovMatrixFail()
	{
		Helper.checkForCorrectException(new whatToRun() {
			@SuppressWarnings("unused")
			@Override
			public void run() throws NumberFormatException
			{
				new MarkovUniversalLearner(1);
			}
		}, IllegalArgumentException.class, "chunkLen");
	}
	
	@Test
	public void testCreateMarkovMatrix1()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","b","c","d"}, new String[]{"a","u"} },config,converter);
		Map<Trace, UpdatableOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		Assert.assertEquals(11,matrix.size());
		Assert.assertSame(UpdatableOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))));

		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblD,lblC}))));

		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblC}))));

		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB}))));

		Assert.assertSame(UpdatableOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblD}))));

		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblD}))));

		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))));
		
		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblB}))));
		
		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblC}))));
		
		Assert.assertSame(UpdatableOutcome.failure, matrix.get(new Trace(Arrays.asList(new Label[]{lblD}))));
		
		Assert.assertSame(UpdatableOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))));
	}	

	@Test
	public void testCreateMarkovMatrix2()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter), minusStrings = new HashSet<List<Label>>();
		Map<Trace, UpdatableOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		Assert.assertEquals(3,matrix.size());
		
		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))));

		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))));

		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))));
	}
	
	@Test
	public void testCreateMarkovMatrix3a()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		Map<Trace, UpdatableOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		Assert.assertEquals(3,matrix.size());

		Assert.assertSame(UpdatableOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))));

		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))));

		Assert.assertSame(UpdatableOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))));
	}
	
	/** Only differs from 3a by the use of chunk_length of 3. The outcome should be the same. */
	@Test
	public void testCreateMarkovMatrix3b()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(3);
		Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		Map<Trace, UpdatableOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		Assert.assertEquals(3,matrix.size());

		Assert.assertSame(UpdatableOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))));

		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))));

		Assert.assertSame(UpdatableOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))));
	}
	
	@Test
	public void testCreateMarkovMatrix4()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{"u"} },config,converter);
		Map<Trace, UpdatableOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		Assert.assertEquals(1,matrix.size());

		Assert.assertSame(UpdatableOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))));
	}
	
	/** Same as testCreateMarkovMatrix3 but contains an empty trace which is ignored since it does not match any of the valid chunk sizes. */
	@Test
	public void testCreateMarkovMatrix5()
	{
		final MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		final Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{},new String[]{"a","u"} },config,converter);
		Map<Trace, UpdatableOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		Assert.assertEquals(3,matrix.size());
		
		Assert.assertSame(UpdatableOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))));

		Assert.assertSame(UpdatableOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))));

		Assert.assertSame(UpdatableOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))));
	}
	
	@Test
	public void testCreateMarkovMatrix6()
	{
		final MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		final Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = new HashSet<List<Label>>();
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				m.createMarkovLearner(plusStrings, minusStrings);
			}
		}, IllegalArgumentException.class, "empty");
	}
	
	@Test
	public void testCreateMarkovMatrix7()
	{
		final MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		final Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{},new String[]{} },config,converter);
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				m.createMarkovLearner(plusStrings, minusStrings);
			}
		}, IllegalArgumentException.class, "empty");
	}
	
	/** Nothing to add because there not enough evidence. */
	@Test
	public void testConstructExtendedGraph1()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","p"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-u->B-p->B","testConstructExtendedGraph1",config, converter);
		Map<CmpVertex, Map<Label, UpdatableOutcome>> newTransitions = m.constructMarkovTentative(graph);
		Assert.assertTrue(newTransitions.isEmpty());// not enough evidence to update, hence nothing should be recorded.
		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-u->B-p->B","testConstructExtendedGraph1",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	/** Nothing to add because the alphabet of the graph of interest consists of a single letter for which there is no statistical data. */
	@Test
	public void testConstructExtendedGraph2()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","p"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B","testConstructExtendedGraph2",config, converter);
		Map<CmpVertex, Map<Label, UpdatableOutcome>> newTransitions = m.constructMarkovTentative(graph);
		Assert.assertTrue(newTransitions.isEmpty());// not enough evidence to update, hence nothing should be recorded.
		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B","testConstructExtendedGraph2",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	/** A bit has been added. */
	@Test
	public void testConstructExtendedGraph3()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3a",config, converter);
		Map<CmpVertex, Map<Label, UpdatableOutcome>> newTransitions = m.constructMarkovTentative(graph);
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertSame(UpdatableOutcome.negative, newTransitions.get(graph.findVertex("B")).get(lblU));
		
		Assert.assertSame(UpdatableOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / T-b->T-u->T","testConstructExtendedGraph3b",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	/** A bit has been added, but reject-transition was not because is present both in the positive and negative light in the initial traces. */
	@Test
	public void testConstructExtendedGraph4()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"a","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3a",config, converter);
		Map<CmpVertex, Map<Label, UpdatableOutcome>> newTransitions = m.constructMarkovTentative(graph);
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertFalse(newTransitions.get(graph.findVertex("B")).containsKey(lblU));// failure ignored
		
		Assert.assertSame(UpdatableOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-b->C / T-b->T-u->T","testConstructExtendedGraph4b",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	
	/** A bit has been added, but reject-transition was not because is present both in the positive and negative light in the initial traces. */
	@Test
	public void testConstructExtendedGraph5()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);// w below is to ensure that all elements of the alphabet are included in traces.
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"},new String[]{"w"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-w->M-c->B / T-b->T-u->T","testConstructExtendedGraph5a",config, converter);// the purpose of the w-transition is to ensure transition c is taken into account in graph comparison
		Map<CmpVertex, Map<Label, UpdatableOutcome>> newTransitions = m.constructMarkovTentative(graph);
		Assert.assertEquals(1,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		
		Assert.assertSame(UpdatableOutcome.positive,newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-b->C / A-w->M-c->B / T-b->T-u->T","testConstructExtendedGraph5b",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	/** Nothing has been added. u has been seen both in the positive and negative light and lead to inconsistency. */
	@Test
	public void testConstructExtendedGraph6()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / T-b->T-u->T","testConstructExtendedGraph6a",config, converter);
		Map<CmpVertex, Map<Label, UpdatableOutcome>> newTransitions = m.constructMarkovTentative(graph);
		
		Assert.assertEquals(1,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		
		Assert.assertSame(UpdatableOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-b->C / T-b->T-u->T","testConstructExtendedGraph6b",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	/** A bit has been added. u has been seen both in the positive and negative light. */
	@Test
	public void testConstructExtendedGraph7()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B-c->Z / T-b->T-u->T","testConstructExtendedGraph7a",config, converter);
		Map<CmpVertex, Map<Label, UpdatableOutcome>> newTransitions = m.constructMarkovTentative(graph);
		
		Assert.assertEquals(2,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		Assert.assertEquals(1,newTransitions.get(graph.findVertex("Z")).size());
		
		Assert.assertSame(UpdatableOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));
		Assert.assertSame(UpdatableOutcome.positive, newTransitions.get(graph.findVertex("Z")).get(lblU));

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B / A-c->B-c->Z-u->Y / B-b->C / T-b->T-u->T","testConstructExtendedGraph7b",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	/** No outgoing from B, hence no inconsistencies. */
	@Test
	public void testCheckFanoutInconsistency1a()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / T-b->T-u->T","testCheckFanoutInconsistency1",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(0,m.checkFanoutInconsistency(Inverse_Graph,graph,graph.findVertex("B"),m.getChunkLen()));
	}
	
	/** One from B with inconsistent predictions. */
	@Test
	public void testCheckFanoutInconsistency1b1()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency1",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,graph,graph.findVertex("B"),m.getChunkLen()));
	}
	
	/** One from B where Markov cannot make up its mind. */
	@Test
	public void testCheckFanoutInconsistency1b2()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"a","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency1",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,graph,graph.findVertex("B"),m.getChunkLen()));
	}
	
	/** Transition d exists as positive but should be present as negative according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1c()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"},new String[]{"a","d"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-d->F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,graph,graph.findVertex("B"),m.getChunkLen()));
	}
	
	/** Transition d exists as positive but should be absent according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1d()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"}},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-d->F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,graph,graph.findVertex("B"),m.getChunkLen()));
	}
	
	/** Transition b exists as negative but should be present as positive according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1e()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"}},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-b-#F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,graph,graph.findVertex("B"),m.getChunkLen()));
	}
	
	/** Transition d exists as negative but should be absent according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1f()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-d-#F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,graph,graph.findVertex("B"),m.getChunkLen()));
	}
	
	/** Two inconsistencies, transition u and transition b which should not exist after c. */
	@Test
	public void testCheckFanoutInconsistency2()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B-b->C / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency2",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(2,m.checkFanoutInconsistency(Inverse_Graph,graph,graph.findVertex("B"),m.getChunkLen()));
	}
	
	/** One inconsistency: transition u. */
	@Test
	public void testCheckFanoutInconsistency3()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B-u->C / T-b->T-u->T","testCheckFanoutInconsistency3",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,graph,graph.findVertex("B"),m.getChunkLen()));
	}
	
	/** No inconsistencies since there are very few paths. */
	@Test
	public void testCheckFanoutInconsistency4()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / T-b->T-u->T","testCheckFanoutInconsistency4",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(0,m.checkFanoutInconsistency(Inverse_Graph,graph,graph.findVertex("B"),m.getChunkLen()));// everything as expected.
		Assert.assertEquals(0,m.checkFanoutInconsistency(Inverse_Graph,graph,graph.findVertex("D"),m.getChunkLen()));// missing reject-transition with label u is ignored because we are only considering actual outgoing transitions
	}
	
	
	@Test
	public void testLabelStatesAwayFromRoot1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->C-a->A-b->B-a->D / C-b->C","testLabelStatesAwayFromRoot1",config, converter);
		MarkovPassivePairSelection.labelStatesAwayFromRoot(graph,0);
		Assert.assertSame(JUConstants.RED, graph.findVertex("A").getColour());
		Assert.assertNull(graph.findVertex("B").getColour());
		
		Assert.assertNull(graph.findVertex("C").getColour());Assert.assertNull(graph.findVertex("D").getColour());
		
		Assert.assertNull(graph.additionalExplorationRoot);
	}


	@Test
	public void testLabelStatesAwayFromRoot2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->C-a->A-b->B-a->D / C-b->C","testLabelStatesAwayFromRoot1",config, converter);
		MarkovPassivePairSelection.labelStatesAwayFromRoot(graph,1);
		Assert.assertSame(JUConstants.RED, graph.findVertex("C").getColour());
		Assert.assertSame(JUConstants.BLUE, graph.findVertex("B").getColour());
		
		Assert.assertNull(graph.findVertex("A").getColour());Assert.assertNull(graph.findVertex("D").getColour());

		Set<CmpVertex> expected = new TreeSet<CmpVertex>(Arrays.asList(new CmpVertex[]{graph.findVertex("A")})), actual = new TreeSet<CmpVertex>(graph.additionalExplorationRoot);
		Assert.assertEquals(expected,actual);
	}
	
	@Test
	public void testLabelStatesAwayFromRoot3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->C-a->A-b->B-a->D / C-b->C","testLabelStatesAwayFromRoot1",config, converter);
		MarkovPassivePairSelection.labelStatesAwayFromRoot(graph,2);
		
		Assert.assertNull(graph.findVertex("A").getColour());Assert.assertNull(graph.findVertex("B").getColour());Assert.assertNull(graph.findVertex("C").getColour());
		Assert.assertSame(JUConstants.RED, graph.findVertex("D").getColour());

		Set<CmpVertex> expected = new TreeSet<CmpVertex>(Arrays.asList(new CmpVertex[]{graph.findVertex("B"),graph.findVertex("C")})), actual = new TreeSet<CmpVertex>(graph.additionalExplorationRoot);
		Assert.assertEquals(expected,actual);
	}
	
	@Test
	public void testLabelStatesAwayFromRoot4()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->C-a->A-b->B-a->D / C-b->C","testLabelStatesAwayFromRoot1",config, converter);
		
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				MarkovPassivePairSelection.labelStatesAwayFromRoot(graph,3);
			}
		}, IllegalArgumentException.class, "no states");
	}
	
	@Test
	public void testUpdateMarkovFromGraph1()
	{
		
	}
	
	@Test
	public void testIdentifyUncoveredTransitions1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E","testIdentifyUncoveredTransitions1a",config, converter);
		final LearnerGraph reference = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / T-b->T-u->T","testIdentifyUncoveredTransitions1b",config, converter);
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				MarkovPassivePairSelection.identifyUncoveredTransitions(graph,reference);
			}
		}, IllegalArgumentException.class, "PTA is not");		
	}
	
	@Test
	public void testIdentifyUncoveredTransitions2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C1 / B-u->E","testIdentifyUncoveredTransitions2a",config, converter);
		final LearnerGraph reference = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / T-b->T-u->T","testIdentifyUncoveredTransitions1b",config, converter);
		Map<CmpVertex,Set<Label>> uncovered = MarkovPassivePairSelection.identifyUncoveredTransitions(graph,reference);
		Assert.assertEquals(reference.transitionMatrix.size(),uncovered.size());
		for(CmpVertex v:reference.transitionMatrix.keySet())
			if (v.toString().equals("B"))
			{
				Set<Label> uncoveredFromThisVertex = uncovered.get(v);
				Assert.assertEquals(1,uncoveredFromThisVertex.size());
				Assert.assertEquals("x", uncoveredFromThisVertex.iterator().next().toString());
			}	
			else
				if (v.toString().equals("T"))
				{
					Set<Label> uncoveredFromThisVertex = uncovered.get(v);
					Assert.assertEquals(2,uncoveredFromThisVertex.size());
					Iterator<Label> elems =  uncoveredFromThisVertex.iterator();
					Assert.assertEquals("b", elems.next().toString());Assert.assertEquals("u", elems.next().toString());
				}	
				else
					Assert.assertEquals(0,uncovered.get(v).size());
	}
	
	@Test
	public void testIdentifyUncoveredTransitions3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B / B-u->E","testIdentifyUncoveredTransitions3a",config, converter);
		final LearnerGraph reference = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / T-b->T-u->T","testIdentifyUncoveredTransitions1b",config, converter);
		Map<CmpVertex,Set<Label>> uncovered = MarkovPassivePairSelection.identifyUncoveredTransitions(graph,reference);
		Assert.assertEquals(reference.transitionMatrix.size(),uncovered.size());
		for(CmpVertex v:reference.transitionMatrix.keySet())
			if (v.toString().equals("B"))
			{
				Set<Label> uncoveredFromThisVertex = uncovered.get(v);
				Assert.assertEquals(2,uncoveredFromThisVertex.size());
				Iterator<Label> elems =  uncoveredFromThisVertex.iterator();
				Assert.assertEquals("b", elems.next().toString());Assert.assertEquals("x", elems.next().toString());
			}	
			else
				if (v.toString().equals("T"))
				{
					Set<Label> uncoveredFromThisVertex = uncovered.get(v);
					Assert.assertEquals(2,uncoveredFromThisVertex.size());
					Iterator<Label> elems =  uncoveredFromThisVertex.iterator();
					Assert.assertEquals("b", elems.next().toString());Assert.assertEquals("u", elems.next().toString());
				}	
				else
					Assert.assertEquals(0,uncovered.get(v).size());
	}
	
	@Test
	public void testIdentifyUncoveredTransitions4()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->D-b->C","testIdentifyUncoveredTransitions4a",config, converter);
		final LearnerGraph reference = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / T-b->T-u->T","testIdentifyUncoveredTransitions1b",config, converter);
		Map<CmpVertex,Set<Label>> uncovered = MarkovPassivePairSelection.identifyUncoveredTransitions(graph,reference);
		Assert.assertEquals(reference.transitionMatrix.size(),uncovered.size());
		for(CmpVertex v:reference.transitionMatrix.keySet())
			if (v.toString().equals("A"))
			{
				Set<Label> uncoveredFromThisVertex = uncovered.get(v);
				Assert.assertEquals(1,uncoveredFromThisVertex.size());
				Assert.assertEquals("c", uncoveredFromThisVertex.iterator().next().toString());
			}	
			else
			if (v.toString().equals("B"))
			{
				Set<Label> uncoveredFromThisVertex = uncovered.get(v);
				Assert.assertEquals(3,uncoveredFromThisVertex.size());
				Iterator<Label> elems =  uncoveredFromThisVertex.iterator();
				Assert.assertEquals("b", elems.next().toString());Assert.assertEquals("u", elems.next().toString());Assert.assertEquals("x", elems.next().toString());
			}	
			else
				if (v.toString().equals("T"))
				{
					Set<Label> uncoveredFromThisVertex = uncovered.get(v);
					Assert.assertEquals(2,uncoveredFromThisVertex.size());
					Iterator<Label> elems =  uncoveredFromThisVertex.iterator();
					Assert.assertEquals("b", elems.next().toString());Assert.assertEquals("u", elems.next().toString());
				}	
				else
					Assert.assertEquals(0,uncovered.get(v).size());
	}
	
	@Test
	public void testIdentifyUncoveredTransitions5()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->D-b->C","testIdentifyUncoveredTransitions4a",config, converter);
		final LearnerGraph reference = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / E-z->F / T-b->T-u->T","testIdentifyUncoveredTransitions5b",config, converter);
		Map<CmpVertex,Set<Label>> uncovered = MarkovPassivePairSelection.identifyUncoveredTransitions(graph,reference);
		Assert.assertEquals(reference.transitionMatrix.size(),uncovered.size());
		for(CmpVertex v:reference.transitionMatrix.keySet())
			if (v.toString().equals("A"))
			{
				Set<Label> uncoveredFromThisVertex = uncovered.get(v);
				Assert.assertEquals(1,uncoveredFromThisVertex.size());
				Assert.assertEquals("c", uncoveredFromThisVertex.iterator().next().toString());
			}	
			else
			if (v.toString().equals("B"))
			{
				Set<Label> uncoveredFromThisVertex = uncovered.get(v);
				Assert.assertEquals(3,uncoveredFromThisVertex.size());
				Iterator<Label> elems =  uncoveredFromThisVertex.iterator();
				Assert.assertEquals("b", elems.next().toString());Assert.assertEquals("u", elems.next().toString());Assert.assertEquals("x", elems.next().toString());
			}	
			else
			if (v.toString().equals("E"))
			{
				Set<Label> uncoveredFromThisVertex = uncovered.get(v);
				Assert.assertEquals(1,uncoveredFromThisVertex.size());
				Assert.assertEquals("z", uncoveredFromThisVertex.iterator().next().toString());
			}	
			else
				if (v.toString().equals("T"))
				{
					Set<Label> uncoveredFromThisVertex = uncovered.get(v);
					Assert.assertEquals(2,uncoveredFromThisVertex.size());
					Iterator<Label> elems =  uncoveredFromThisVertex.iterator();
					Assert.assertEquals("b", elems.next().toString());Assert.assertEquals("u", elems.next().toString());
				}	
				else
					Assert.assertEquals(0,uncovered.get(v).size());
	}
	
	@Test
	public void testIdentifyUncoveredTransitions6()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C1 / B-u->E","testIdentifyUncoveredTransitions2a",config, converter);
		final LearnerGraph reference = FsmParser.buildLearnerGraph("A-a->D","testIdentifyUncoveredTransitions3",config, converter);
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				MarkovPassivePairSelection.identifyUncoveredTransitions(graph,reference);
			}
		}, IllegalArgumentException.class, "coverage has more transitions");		
	}
	
	
}

