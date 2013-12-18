package statechum.analysis.learning.experiments.PairSelection;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;

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
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.MarkovUniversalLearner.MarkovOutcome;
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
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{}),true),1);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks2a() {
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true),4);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks2b() {
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true),10);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks3() {
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true),1);
		Assert.assertEquals(3,l.size());
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblA}),true),l.get(0));
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblB}),true),l.get(1));
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblC}),true),l.get(2));
	}

	@Test
	public void testGetChunks4() {
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true),2);
		Assert.assertEquals(2,l.size());
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true),l.get(0));
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblB,lblC}),true),l.get(1));
	}

	@Test
	public void testGetChunks5() {
		List<Trace> l = MarkovUniversalLearner.get_chunks(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true),3);
		Assert.assertEquals(1,l.size());
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true),l.get(0));
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
		Map<Trace, MarkovOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings,false);
		Assert.assertEquals(11,matrix.size());
		Assert.assertSame(MarkovOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}),true)));

		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblD,lblC}),true)));

		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblC}),true)));

		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));

		Assert.assertSame(MarkovOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblD}),true)));

		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblD}),true)));

		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));
		
		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblB}),true)));
		
		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblC}),true)));
		
		Assert.assertSame(MarkovOutcome.failure, matrix.get(new Trace(Arrays.asList(new Label[]{lblD}),true)));
		
		Assert.assertSame(MarkovOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}),true)));
	}	

	@Test
	public void testCreateMarkovMatrix2()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter), minusStrings = new HashSet<List<Label>>();
		Map<Trace, MarkovOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings,false);
		Assert.assertEquals(3,matrix.size());
		
		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}),true)));

		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));

		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}),true)));
	}
	
	@Test
	public void testCreateMarkovMatrix3a()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		Map<Trace, MarkovOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings,false);
		Assert.assertEquals(3,matrix.size());

		Assert.assertSame(MarkovOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}),true)));

		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));

		Assert.assertSame(MarkovOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}),true)));
	}
	
	/** Only differs from 3a by the use of chunk_length of 3. The outcome should be the same. */
	@Test
	public void testCreateMarkovMatrix3b()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(3);
		Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		Map<Trace, MarkovOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings,false);
		Assert.assertEquals(3,matrix.size());

		Assert.assertSame(MarkovOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}),true)));

		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));

		Assert.assertSame(MarkovOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}),true)));
	}
	
	@Test
	public void testCreateMarkovMatrix4()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{"u"} },config,converter);
		Map<Trace, MarkovOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings,false);
		Assert.assertEquals(1,matrix.size());

		Assert.assertSame(MarkovOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}),true)));
	}
	
	/** Same as testCreateMarkovMatrix3 but contains an empty trace which is ignored since it does not match any of the valid chunk sizes. */
	@Test
	public void testCreateMarkovMatrix5()
	{
		final MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		final Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{},new String[]{"a","u"} },config,converter);
		Map<Trace, MarkovOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings,false);
		Assert.assertEquals(3,matrix.size());
		
		Assert.assertSame(MarkovOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}),true)));

		Assert.assertSame(MarkovOutcome.positive, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));

		Assert.assertSame(MarkovOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}),true)));
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
				m.createMarkovLearner(plusStrings, minusStrings,false);
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
				m.createMarkovLearner(plusStrings, minusStrings,false);
			}
		}, IllegalArgumentException.class, "empty");
	}
	
	/** Nothing to add because there not enough evidence. */
	@Test
	public void testConstructExtendedGraph1()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","p"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-u->B-p->B","testConstructExtendedGraph1",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = m.constructMarkovTentative(graph,true);
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
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B","testConstructExtendedGraph2",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = m.constructMarkovTentative(graph,true);
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
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3a",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = m.constructMarkovTentative(graph,true);
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertSame(MarkovOutcome.negative, newTransitions.get(graph.findVertex("B")).get(lblU));
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

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
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3a",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = m.constructMarkovTentative(graph,true);
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertFalse(newTransitions.get(graph.findVertex("B")).containsKey(lblU));// failure ignored
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

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
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-w->M-c->B / T-b->T-u->T","testConstructExtendedGraph5a",config, converter);// the purpose of the w-transition is to ensure transition c is taken into account in graph comparison
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = m.constructMarkovTentative(graph,true);
		Assert.assertEquals(1,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		
		Assert.assertSame(MarkovOutcome.positive,newTransitions.get(graph.findVertex("B")).get(lblB));

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
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / T-b->T-u->T","testConstructExtendedGraph6a",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = m.constructMarkovTentative(graph,true);
		
		Assert.assertEquals(1,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

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
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B-c->Z / T-b->T-u->T","testConstructExtendedGraph7a",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = m.constructMarkovTentative(graph,true);
		
		Assert.assertEquals(2,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		Assert.assertEquals(1,newTransitions.get(graph.findVertex("Z")).size());
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("Z")).get(lblU));

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
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / T-b->T-u->T","testCheckFanoutInconsistency1a",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(0,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency()));
	}
	
	/** One from B with inconsistent predictions. */
	@Test
	public void testCheckFanoutInconsistency1b1()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency1b1",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency()));
	}
	
	/** One from B where Markov cannot make up its mind. */
	@Test
	public void testCheckFanoutInconsistency1b2()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"a","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency1b2",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency()));
	}
	
	/** Transition d exists as positive but should be present as negative according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1c()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"},new String[]{"a","d"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-d->F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1c",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency()));
	}
	
	/** Transition d exists as positive but should be absent according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1d()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"}},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-d->F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1d",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency()));
	}
	
	/** Transition b exists as negative but should be present as positive according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1e()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"}},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-b-#F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1e",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency()));
	}
	
	/** Transition d exists as negative but should be absent according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1f()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-d-#F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1f",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency()));
		
		Assert.assertEquals(4.,MarkovUniversalLearner.computeInconsistency(graph, true, m, new MarkovUniversalLearner.DifferentPredictionsInconsistency()),Configuration.fpAccuracy);// inconsistencies detected are mostly due to state T
	}
	
	/** Two inconsistencies, transition u and transition b which should not exist after c. */
	@Test
	public void testCheckFanoutInconsistency2()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B-b->C / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency2",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(2,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency()));
	}
	
	/** One inconsistency: transition u. */
	@Test
	public void testCheckFanoutInconsistency3()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B-u->C / T-b->T-u->T","testCheckFanoutInconsistency3",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency()));
	}
	
	/** No inconsistencies since there are very few paths. */
	@Test
	public void testCheckFanoutInconsistency4()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / T-b->T-u->T","testCheckFanoutInconsistency4",config, converter);
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraphND Inverse_Graph = new LearnerGraphND(shallowCopy);
		AbstractPathRoutines.buildInverse(graph,LearnerGraphND.ignoreNone,Inverse_Graph);  // do the inverse to the tentative graph 
		Assert.assertEquals(0,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency()));// everything as expected.
		Assert.assertEquals(0,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("D"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency()));// missing reject-transition with label u is ignored because we are only considering actual outgoing transitions
	}

	@Test
	public void testMarkovUpdate1()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovUniversalLearner mOther = new MarkovUniversalLearner(2);
		mOther.predictTransitionsAndUpdateMarkov(graph,true,true);
		Assert.assertEquals(m.getMarkov(true),mOther.getMarkov(true));Assert.assertTrue(m.getMarkov(false).isEmpty());
	}
	
	@Test
	public void testMarkovUpdate2()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] {},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovUniversalLearner mOther = new MarkovUniversalLearner(2);mOther.predictTransitionsAndUpdateMarkov(graph,true,true);
		Assert.assertEquals(m.getMarkov(true),mOther.getMarkov(true));Assert.assertTrue(m.getMarkov(false).isEmpty());
	}
	
	@Test
	public void testMarkovUpdate3()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] {},config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovUniversalLearner mOther = new MarkovUniversalLearner(2);mOther.predictTransitionsAndUpdateMarkov(graph,true,true);
		Assert.assertEquals(m.getMarkov(true),mOther.getMarkov(true));Assert.assertTrue(m.getMarkov(false).isEmpty());
	}
	
	@Test
	public void testMarkovUpdate4()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovUniversalLearner mOther = new MarkovUniversalLearner(2);mOther.predictTransitionsAndUpdateMarkov(graph,true,true);
		Assert.assertEquals(m.getMarkov(true),mOther.getMarkov(true));Assert.assertTrue(m.getMarkov(false).isEmpty());
	}
	
	@Test
	public void testUpdateMarkovSideways1a()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->C","testUpdateMarkovSideways1",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());Assert.assertEquals(4,m.getMarkov(false).size());
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));
	}
	
	@Test
	public void testUpdateMarkovSideways1b()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->C","testUpdateMarkovSideways1",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,false);
		Assert.assertTrue(m.getMarkov(true).isEmpty());Assert.assertEquals(6,m.getMarkov(false).size());
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));

		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB}),true)));
	}
	
	/** This one is similar to the {@link #testUpdateMarkovSideways1b}, except that there are a few additional negative transitions. */
	@Test
	public void testUpdateMarkovSideways1c()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->C-a-#D / B-c-#D","testUpdateMarkovSideways1c",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,false);
		Assert.assertTrue(m.getMarkov(true).isEmpty());Assert.assertEquals(9,m.getMarkov(false).size());
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblC}),true)));

		Assert.assertEquals(MarkovOutcome.failure,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC}),true)));
	}
	
	/** This one is similar to the {@link #testUpdateMarkovSideways1b}, except that there are a few additional negative transitions and the computation is forward. */
	@Test
	public void testUpdateMarkovSideways1d()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->C-a-#D / B-c-#D","testUpdateMarkovSideways1c",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,true,false);
		Assert.assertTrue(m.getMarkov(false).isEmpty());Assert.assertEquals(7,m.getMarkov(true).size());
		Assert.assertEquals(MarkovOutcome.failure,m.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblA,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));

		Assert.assertEquals(MarkovOutcome.failure,m.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblC}),true)));
		
		Set<List<Label>> plusStrings = buildSet(new String[][] {},config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","a","a"},new String[]{"a","b","a"},new String[]{"a","c"} },config,converter);
		MarkovUniversalLearner another = new MarkovUniversalLearner(2);
		another.createMarkovLearner(plusStrings, minusStrings, false);

		Assert.assertTrue(another.getMarkov(false).isEmpty());Assert.assertEquals(7,another.getMarkov(true).size());
		Assert.assertEquals(MarkovOutcome.failure,another.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,another.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,another.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblA,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.negative,another.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));

		Assert.assertEquals(MarkovOutcome.failure,another.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,another.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,another.getMarkov(true).get(new Trace(Arrays.asList(new Label[]{lblC}),true)));
	}

	@Test
	public void testUpdateMarkovSideways2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->C / B-b-#D","testUpdateMarkovSideways2",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(3,m.getMarkov(false).size());
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC,lblC}),true)));
	}
	
	@Test
	public void testUpdateMarkovSideways3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC,lblA}),true)));
		
		Assert.assertEquals(MarkovOutcome.negative,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblU}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));
		
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblU,lblU}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC,lblU}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblU,lblC}),true)));
	}
	
	@Test
	public void testUpdateMarkovSideways4()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(3);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(6,m.getMarkov(false).size());
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblA}),true)));
		
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC,lblU,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC,lblU,lblA}),true)));
		
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC,lblC,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC,lblC,lblA}),true)));
	}
	
	@Test
	public void testUpdateMarkovSideways5()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(4);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());Assert.assertTrue(m.getMarkov(false).isEmpty());
	}

	@Test
	public void testPredictTransitionsSideways1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		Map<Trace, MarkovOutcome> markovMatrix = m.getMarkov(false);
		
		List<List<Label>> interestingPaths = new LinkedList<List<Label>>();
		// nothing in Markov matrix hence no predictions.
		Assert.assertTrue(MarkovUniversalLearner.predictTransitionsFromState(m.getMarkov(true),graph,true,graph.findVertex("B"),graph.pathroutines.computeAlphabet(),null,2,interestingPaths).isEmpty());
		Assert.assertEquals(1,interestingPaths.size());Assert.assertEquals(Arrays.asList(new Label[]{lblB}),interestingPaths.get(0));
		
		interestingPaths.clear();
		Map<Label, MarkovOutcome> outcome1 = MarkovUniversalLearner.predictTransitionsFromState(markovMatrix,graph,false,graph.findVertex("B"),graph.pathroutines.computeAlphabet(),null,2,interestingPaths);
		Assert.assertEquals(1,interestingPaths.size());Assert.assertEquals(Arrays.asList(new Label[]{lblB}),interestingPaths.get(0));
		Assert.assertEquals(2,outcome1.size());
		Assert.assertEquals(MarkovOutcome.negative,outcome1.get(lblU));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblB));
		
		
		interestingPaths.clear();
		outcome1 = MarkovUniversalLearner.predictTransitionsFromState(markovMatrix,graph,false,graph.findVertex("E"),graph.pathroutines.computeAlphabet(),null,2,interestingPaths);
		Assert.assertEquals(2,interestingPaths.size());Assert.assertEquals(Arrays.asList(new Label[]{lblC}),interestingPaths.get(0));Assert.assertEquals(Arrays.asList(new Label[]{lblU}),interestingPaths.get(1));
		Assert.assertEquals(3,outcome1.size());
		Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblU));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblC));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblA));
	}		

	@Test
	public void testPredictTransitionsSideways2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-b->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		
		Assert.assertEquals(8,m.getMarkov(false).size());
		
		Assert.assertEquals(MarkovOutcome.failure,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblU}),true)));
		
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblC,lblC}),true)));
		
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblU,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblU,lblU}),true)));
	}

	/** Tests that upon a label labelled as invalid, subsequent inconsistency checks are stopped. It is hence equivalent to a single incoming path. */
	@Test
	public void testPredictTransitionsFromStatesSideways1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		Map<Trace, MarkovOutcome> markovMatrix = m.getMarkov(false);
		Map<Label, MarkovOutcome> predictions = MarkovUniversalLearner.predictTransitionsFromState(markovMatrix,graph2,false,graph2.getInit(),graph.pathroutines.computeAlphabet(),null,m.getChunkLen(),null);
		
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblC));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblA));
	}
	
	@Test
	public void testPredictTransitionsFromStatesForward1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> predictions = m.predictTransitions(graph2,true);
		Assert.assertTrue(predictions.isEmpty());// empty Markov means no predictions.
	}
	
	@Test
	public void testPredictTransitionsFromStatesForward2a()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,true,true);
		Assert.assertEquals(4,m.getMarkov(true).size());
		Assert.assertTrue(m.getMarkov(false).isEmpty());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> predictions = m.predictTransitions(graph2,true);
		Assert.assertEquals(2,predictions.size());Assert.assertEquals(2,predictions.get(graph2.findVertex("A")).size());Assert.assertEquals(2,predictions.get(graph2.findVertex("B")).size());
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(graph2.findVertex("A")).get(lblU));// because c is looping in the A state
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(graph2.findVertex("A")).get(lblC));
		Assert.assertEquals(MarkovOutcome.negative,predictions.get(graph2.findVertex("B")).get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(graph2.findVertex("B")).get(lblB));
	}

	
	/** Very similar to {@link #testPredictTransitionsFromStatesForward2a()} except that the graph to predict from has a single state and even that reject. */
	@Test
	public void testPredictTransitionsFromStatesForward2b()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,true,true);
		Assert.assertEquals(4,m.getMarkov(true).size());
		Assert.assertTrue(m.getMarkov(false).isEmpty());
		
		final LearnerGraph graph2 = new LearnerGraph(config);graph2.getInit().setAccept(false);
		Map<CmpVertex, Map<Label, MarkovOutcome>> predictions = m.predictTransitions(graph2,true);
		Assert.assertTrue(predictions.isEmpty());
		predictions = m.predictTransitions(graph2,false);
		Assert.assertTrue(predictions.isEmpty());
	}
	
	@Test
	public void testPredictTransitionsFromStatesForward3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,true,true);
		Assert.assertEquals(4,m.getMarkov(true).size());
		Assert.assertTrue(m.getMarkov(false).isEmpty());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A/ T-a->T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		m.constructMarkovTentative(graph2, true);
		Assert.assertNull(WMethod.checkM(FsmParser.buildLearnerGraph("A-a->B / A-c->A / A-u->E / B-b->C / B-u-#D","testPredictTransitionsFromStatesForward3",config, converter), m.get_extension_model()));
		Assert.assertTrue(m.get_extension_model().findVertex("T") != null);// extended graph starts as a replica of an original one.
	}

	/** Here the alphabet is limited to what is an the tentative automaton, hence nothing is added. */
	@Test
	public void testPredictTransitionsFromStatesSideways2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B","testCheckFanoutInconsistencySideways4",config, converter);
		m.constructMarkovTentative(graph2, false);
		Assert.assertNull(WMethod.checkM(graph2,m.get_extension_model()));
	}
	
	/** Tests that upon a label labelled as invalid, subsequent inconsistency checks are stopped. It is hence equivalent to a single incoming path. */
	@Test
	public void testPredictTransitionsFromStatesSideways3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / T-a->T-u->T-b->T-c->T","testPredictTransitionsFromStatesSideways3",config, converter);
		m.constructMarkovTentative(graph2, false);
		Assert.assertNull(WMethod.checkM(FsmParser.buildLearnerGraph("A-a->B / A-c->F","testPredictTransitionsFromStatesForward3",config, converter), m.get_extension_model()));// FSM comparison ignores unreachable states here
		Assert.assertTrue(m.get_extension_model().findVertex("T") != null);// extended graph starts as a replica of an original one.
	}

	/** Same as {@link #testPredictTransitionsFromStatesSideways1()}, except that the path beyond is empty rather than null. */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		Map<Trace, MarkovOutcome> markovMatrix = m.getMarkov(false);
		Map<Label, MarkovOutcome> predictions = MarkovUniversalLearner.predictTransitionsFromState(markovMatrix,graph2,false,graph2.getInit(),graph.pathroutines.computeAlphabet(),Arrays.asList(new Label[]{}),m.getChunkLen(),null);
		
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblC));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblA));
	}
	
	/** Same as {@link #testPredictTransitionsFromStatesSideways1()}, except that the path beyond is non-empty. */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		final MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B","testPredictTransitionsFromStatesWithPathBeyondCurrentState2",config, converter);
		final Map<Trace, MarkovOutcome> markovMatrix = m.getMarkov(false);
		
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				MarkovUniversalLearner.predictTransitionsFromState(markovMatrix,graph2,false,graph2.getInit(),graph.pathroutines.computeAlphabet(),Arrays.asList(new Label[]{lblC}),m.getChunkLen(),null);
			}
		}, IllegalArgumentException.class, "cannot be made by extension");
	}
	
	/** Almost the same as {@link #testPredictTransitionsFromStatesForward2()} except that the path beyond is empty rather than null. */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,true,true);
		Assert.assertEquals(4,m.getMarkov(true).size());
		Assert.assertTrue(m.getMarkov(false).isEmpty());
		final Map<Trace, MarkovOutcome> markovMatrix = m.getMarkov(true);
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		Map<Label,MarkovOutcome> outgoing_labels_probabilities=m.predictTransitionsFromState(markovMatrix,m.computeInverseGraph(graph2, true),true,graph2.findVertex("B"),graph2.getCache().getAlphabet(),Arrays.asList(new Label[]{}),m.getChunkLen(),null);
		Assert.assertEquals(2,outgoing_labels_probabilities.size());
		Assert.assertEquals(MarkovOutcome.negative,outgoing_labels_probabilities.get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,outgoing_labels_probabilities.get(lblB));
	}

	/** Almost the same as {@link #testPredictTransitionsFromStatesForward2()} except that the path beyond is not empty. Transition <i>d</i> from A to B is not present in Markov, so routinely there will be no prediction. Nevertheless, 
	 * there is one because we assume that the considered paths leading to the state B of interest all start with label b. 
	 */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState4()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,true,true);
		Assert.assertEquals(4,m.getMarkov(true).size());
		Assert.assertTrue(m.getMarkov(false).isEmpty());
		final Map<Trace, MarkovOutcome> markovMatrix = m.getMarkov(true);

		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-d->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		Map<Label,MarkovOutcome> outgoing_labels_probabilities=m.predictTransitionsFromState(markovMatrix,m.computeInverseGraph(graph2, true),true,graph2.findVertex("B"),graph2.getCache().getAlphabet(),Arrays.asList(new Label[]{lblA}),m.getChunkLen(),null);
		Assert.assertEquals(2,outgoing_labels_probabilities.size());
		Assert.assertEquals(MarkovOutcome.negative,outgoing_labels_probabilities.get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,outgoing_labels_probabilities.get(lblB));
	}

	/** Almost the same as {@link #testPredictTransitionsFromStatesForward2()} except that the path beyond is not empty. Transition <i>d</i> from A to B is not present in Markov, so routinely there will be no prediction. Nevertheless, 
	 * there is one because we assume that the considered paths leading to the state B of interest all start with paths a b. 
	 */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState5()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		final MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,true,true);
		Assert.assertEquals(4,m.getMarkov(true).size());
		Assert.assertTrue(m.getMarkov(false).isEmpty());
		final Map<Trace, MarkovOutcome> markovMatrix = m.getMarkov(true);

		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-d->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				m.predictTransitionsFromState(markovMatrix,m.computeInverseGraph(graph2, true),true,graph2.findVertex("B"),graph2.getCache().getAlphabet(),Arrays.asList(new Label[]{lblA,lblB}),m.getChunkLen(),null);
			}
		}, IllegalArgumentException.class, "supplied path");
	}

	@Test
	public void testCheckFanoutInconsistencySideways1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-s->B","testCheckFanoutInconsistencySideways1",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,m.checkFanoutInconsistency(graph2,false,graph2,graph2.getInit(),m.getChunkLen(), new MarkovUniversalLearner.ConsistencyChecker(){

			@Override
			public boolean consistent(MarkovOutcome actual,MarkovOutcome predicted) {
				Assert.assertEquals(MarkovOutcome.positive,actual);Assert.assertNull(predicted);
				counterA.addAndGet(1);
				return true;
			}

			@Override
			public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				Assert.assertEquals(MarkovOutcome.positive,actual);Assert.assertNull(predicted);
				counterB.addAndGet(1);
				return actual;
			}

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") LearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph2.pathroutines.computeAlphabet();
			}
		}));
		Assert.assertEquals(1, counterA.get());Assert.assertEquals(1, counterB.get());
	}
	
	@Test
	public void testCheckFanoutInconsistencySideways2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B-a->B-b->B","testCheckFanoutInconsistencySideways2",config, converter);
		graph2.transitionMatrix.get(graph2.getInit()).clear();// make it look like a graph has no transitions
		Assert.assertEquals(0,m.checkFanoutInconsistency(graph2,false,graph2,graph2.getInit(),m.getChunkLen(), new MarkovUniversalLearner.ConsistencyChecker(){

			@SuppressWarnings("unused")
			@Override
			public boolean consistent(MarkovOutcome actual,MarkovOutcome predicted) {
				Assert.fail("should not be called");
				return true;
			}

			@SuppressWarnings("unused")
			@Override
			public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				Assert.fail("should not be called");
				return actual;
			}
			

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") LearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graphToConsider.getCache().getAlphabet();
			}
		}));
	}
	
	
	@Test
	public void testCheckFanoutInconsistencySideways3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B","testCheckFanoutInconsistencySideways3",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,m.checkFanoutInconsistency(graph2,false,graph2,graph2.getInit(),m.getChunkLen(), new MarkovUniversalLearner.ConsistencyChecker(){

			@SuppressWarnings("unused")
			@Override
			public boolean consistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterA.addAndGet(1);
				return true;
			}

			@SuppressWarnings("unused")
			@Override
			public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterB.addAndGet(1);
				return actual;
			}

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") LearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph.pathroutines.computeAlphabet();// returns the alphabet of the Markov matrix
			}
		}));
		Assert.assertEquals(4, counterA.get());
		Assert.assertEquals(4, counterB.get());
	}
	
	/** Similar to 3 but with multiple outgoing paths. */
	@Test
	public void testCheckFanoutInconsistencySideways4()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,m.checkFanoutInconsistency(graph2,false,graph2,graph2.getInit(),m.getChunkLen(), new MarkovUniversalLearner.ConsistencyChecker(){

			@SuppressWarnings("unused")
			@Override
			public boolean consistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterA.addAndGet(1);
				return true;
			}

			@SuppressWarnings("unused")
			@Override
			public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterB.addAndGet(1);
				return actual;
			}

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") LearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph.pathroutines.computeAlphabet();// returns the alphabet of the Markov matrix
			}
		}));
		Assert.assertEquals(8, counterA.get());
		Assert.assertEquals(8, counterB.get());
	}
	
	/** Tests that with only one path, we do a single check anyway, hence returning false from obtainAlphabet does not change anything. */
	@Test
	public void testCheckFanoutInconsistencySideways5()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B","testCheckFanoutInconsistencySideways3",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,m.checkFanoutInconsistency(graph2,false,graph2,graph2.getInit(),m.getChunkLen(), new MarkovUniversalLearner.ConsistencyChecker(){

			@SuppressWarnings("unused")
			@Override
			public boolean consistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterA.addAndGet(1);
				return true;
			}

			@SuppressWarnings("unused")
			@Override
			public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterB.addAndGet(1);
				return actual;
			}

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") LearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph.pathroutines.computeAlphabet();// returns the alphabet of the Markov matrix
			}
		}));
		Assert.assertEquals(4, counterA.get());
		Assert.assertEquals(4, counterB.get());
	}

	/** Tests that upon a label labelled as invalid, subsequent inconsistency checks are stopped. It is hence equivalent to a single incoming path. */
	@Test
	public void testCheckFanoutInconsistencySideways6()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkov(graph,false,true);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,m.checkFanoutInconsistency(graph2,false,graph2,graph2.getInit(),m.getChunkLen(), new MarkovUniversalLearner.ConsistencyChecker(){

			@SuppressWarnings("unused")
			@Override
			public boolean consistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterA.addAndGet(1);
				return true;
			}

			@SuppressWarnings("unused")
			@Override
			public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterB.addAndGet(1);
				return MarkovOutcome.failure;
			}

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") LearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph.pathroutines.computeAlphabet();// returns the alphabet of the Markov matrix
			}
		}));
		Assert.assertEquals(4, counterA.get());
		Assert.assertEquals(4, counterB.get());
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
	
	@Test
	public void testTrimUncoveredTransitions1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->D-b->C","testIdentifyUncoveredTransitions4a",config, converter);
		final LearnerGraph reference = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / E-z->F / T-b->T-u->T","testIdentifyUncoveredTransitions5b",config, converter);
		LearnerGraph trimmedReference = MarkovPassivePairSelection.trimUncoveredTransitions(graph,reference);
		WMethod.checkM(FsmParser.buildLearnerGraph("A-a->D-b->C","testTrimUncoveredTransitions1",config, converter), trimmedReference);
	}
	
	@Test
	public void testTrimUncoveredTransitions2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C1 / B-u->E","testIdentifyUncoveredTransitions2a",config, converter);
		final LearnerGraph reference = FsmParser.buildLearnerGraph("A-a->D","testIdentifyUncoveredTransitions3",config, converter);
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				MarkovPassivePairSelection.trimUncoveredTransitions(graph,reference);
			}
		}, IllegalArgumentException.class, "coverage has more transitions");		
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques1a()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", config, converter)}));
		Set<CmpVertex> uniqueStates=new TreeSet<CmpVertex>();Collection<List<Label>> invalidSequences =new LinkedList<List<Label>>();
		MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions2a",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertTrue(uniqueStates.isEmpty());Assert.assertEquals(uniques,invalidSequences);Assert.assertNotSame(uniques, invalidSequences);
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques1b()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", config, converter)}));
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingSequences(FsmParser.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions2a",config, converter), uniques);
		Assert.assertEquals(2,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
		Assert.assertEquals("B",uniqueVertices.next().getStringId());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques1c()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("b", config, converter)}));
		Set<CmpVertex> uniqueStates=new TreeSet<CmpVertex>();Collection<List<Label>> invalidSequences =new LinkedList<List<Label>>();
		MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions2a",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertTrue(uniqueStates.isEmpty());Assert.assertTrue(invalidSequences.isEmpty());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques1d()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("b", config, converter)}));
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingSequences(FsmParser.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions2a",config, converter), uniques);
		Assert.assertTrue(uniqueStates.isEmpty());
	}

	@Test
	public void testStatesIdentifiedUsingUniques2a()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", config, converter)}));
		Set<CmpVertex> uniqueStates=new TreeSet<CmpVertex>();Collection<List<Label>> invalidSequences =new LinkedList<List<Label>>();
		MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-b->C","testIdentifyUncoveredTransitions2a",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertEquals(1,uniqueStates.size());
		Assert.assertEquals("A",uniqueStates.iterator().next().getStringId());Assert.assertTrue(invalidSequences.isEmpty());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques2b()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", config, converter)}));
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingSequences(FsmParser.buildLearnerGraph("A-a->B-b->C","testIdentifyUncoveredTransitions2b",config, converter), uniques);
		Assert.assertEquals(1,uniqueStates.size());
		Assert.assertEquals("A",uniqueStates.iterator().next().getStringId());
	}

	@Test
	public void testStatesIdentifiedUsingUniques3a()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", config, converter),AbstractLearnerGraph.generateNewLabel("a", config, converter)}));
		Set<CmpVertex> uniqueStates=new TreeSet<CmpVertex>();Collection<List<Label>> invalidSequences =new LinkedList<List<Label>>();
		MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions3a",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertEquals(1,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());Assert.assertTrue(invalidSequences.isEmpty());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques3b()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", config, converter),AbstractLearnerGraph.generateNewLabel("a", config, converter)}));
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingSequences(FsmParser.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions3a",config, converter), uniques);
		Assert.assertEquals(1,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques3c()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", config, converter),AbstractLearnerGraph.generateNewLabel("a", config, converter)}));
		Set<CmpVertex> uniqueStates=new TreeSet<CmpVertex>();Collection<List<Label>> invalidSequences =new LinkedList<List<Label>>();
		MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-a->C-a->D","testIdentifyUncoveredTransitions3b",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertTrue(uniqueStates.isEmpty());Assert.assertEquals(uniques,invalidSequences);Assert.assertNotSame(uniques, invalidSequences);
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques3d()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", config, converter),AbstractLearnerGraph.generateNewLabel("a", config, converter)}));
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingSequences(FsmParser.buildLearnerGraph("A-a->B-a->C-a->D","testIdentifyUncoveredTransitions3b",config, converter), uniques);
		Assert.assertEquals(2,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
		Assert.assertEquals("B",uniqueVertices.next().getStringId());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques4a()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();
		uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", config, converter)}));uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("b", config, converter)}));
		Set<CmpVertex> uniqueStates=new TreeSet<CmpVertex>();Collection<List<Label>> invalidSequences =new LinkedList<List<Label>>();
		MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-b->C","testIdentifyUncoveredTransitions2a",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertEquals(2,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
		Assert.assertEquals("B",uniqueVertices.next().getStringId());
		Assert.assertTrue(invalidSequences.isEmpty());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques4b()
	{
		Collection<List<Label>> uniques = new LinkedList<List<Label>>();
		uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", config, converter)}));uniques.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("b", config, converter)}));
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingSequences(FsmParser.buildLearnerGraph("A-a->B-b->C","testIdentifyUncoveredTransitions2b",config, converter), uniques);
		Assert.assertEquals(2,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
		Assert.assertEquals("B",uniqueVertices.next().getStringId());
	}

	@Test
	public void testInvertPaths1()
	{
		Assert.assertTrue(PairQualityLearner.invertPaths(new LinkedList<List<Label>>()).isEmpty());
	}
	
	@Test
	public void testInvertPaths2()
	{
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{}));
		Collection<List<Label>>  outcome = PairQualityLearner.invertPaths(paths);
		Assert.assertEquals(1, outcome.size());
		Assert.assertTrue(outcome.iterator().next().isEmpty());
	}
	
	@Test
	public void testInvertPaths3()
	{
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA,lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblA,lblB,lblC}));
		Collection<List<Label>>  outcome = PairQualityLearner.invertPaths(paths);
		Assert.assertEquals(3, outcome.size());
		Iterator<List<Label>> iter = outcome.iterator();
		Assert.assertEquals(Arrays.asList(new Label[]{lblA,lblA}),iter.next());
		Assert.assertEquals(Arrays.asList(new Label[]{lblB}),iter.next());
		Assert.assertEquals(Arrays.asList(new Label[]{lblC,lblB,lblA}),iter.next());
	}
	
	@Test
	public void testCollectionOfSetsToPair1()
	{
		Collection<Set<CmpVertex>> collectionOfSets = new LinkedList<Set<CmpVertex>>();
		List<StatePair> outcome = PairQualityLearner.collectionOfSetsToPairs(collectionOfSets);
		Assert.assertTrue(outcome.isEmpty());
	}

	@Test
	public void testCollectionOfSetsToPair2()
	{
		Collection<Set<CmpVertex>> collectionOfSets = new LinkedList<Set<CmpVertex>>();
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B-a->C-a->D-a->E","testIdentifyUncoveredTransitions3a",config, converter);
		Set<CmpVertex> P=new TreeSet<CmpVertex>(),Q=new TreeSet<CmpVertex>(),R=new TreeSet<CmpVertex>();
		P.add(gr.findVertex("A"));Q.add(gr.findVertex("B"));Q.add(gr.findVertex("C"));Q.add(gr.findVertex("D"));Q.add(gr.findVertex("E"));
		collectionOfSets.add(P);collectionOfSets.add(Q);collectionOfSets.add(R);
		
		// Here P is a singleton and R is empty, hence they are ignored.
		List<StatePair> outcome = PairQualityLearner.collectionOfSetsToPairs(collectionOfSets);
		// three pairs, here we have a hardwired expected response that unfortunately depends on the implementation of collectionOfSetsToPairs.
		Assert.assertEquals(3,outcome.size());
		Assert.assertTrue(outcome.contains(new StatePair(gr.findVertex("B"),gr.findVertex("C"))));
		Assert.assertTrue(outcome.contains(new StatePair(gr.findVertex("C"),gr.findVertex("D"))));
		Assert.assertTrue(outcome.contains(new StatePair(gr.findVertex("D"),gr.findVertex("E"))));
	}
	
	@Test
	public void testCollectionOfSetsToPair3()
	{
		Collection<Set<CmpVertex>> collectionOfSets = new LinkedList<Set<CmpVertex>>();
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B-a->C-a->D-a->E","testIdentifyUncoveredTransitions3a",config, converter);
		Set<CmpVertex> P=new TreeSet<CmpVertex>(),R=new TreeSet<CmpVertex>();
		P.add(gr.findVertex("A"));
		collectionOfSets.add(P);collectionOfSets.add(R);
		
		// Here P is a singleton and R is empty, hence they are ignored.
		List<StatePair> outcome = PairQualityLearner.collectionOfSetsToPairs(collectionOfSets);
		Assert.assertTrue(outcome.isEmpty());
	}
	
	@Test
	public void testBuildVerticesToMergeForPath1()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B / A-b->A / B-a->C-b->D-a->E / D-c->D / E-c->E","testBuildVerticesToMergeForPath1",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));
		Map<CmpVertex,LearnerGraph> grForPaths=PairQualityLearner.constructPathsFromEachState(gr,true);
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=PairQualityLearner.buildVerticesToMergeForPath(paths,grForPaths);
		Assert.assertEquals(1,collectionOfSets.size());
		Assert.assertEquals(gr.transitionMatrix.keySet(), collectionOfSets.iterator().next());
	}
	
	/** Last state not to be merged. */
	@Test
	public void testBuildVerticesToMergeForPath2()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B / A-b->A / B-a->C-b->D-a->E / D-c->D / E-d->E","testBuildVerticesToMergeForPath1",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));
		Map<CmpVertex,LearnerGraph> grForPaths=PairQualityLearner.constructPathsFromEachState(gr,true);
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=PairQualityLearner.buildVerticesToMergeForPath(paths,grForPaths);
		Assert.assertEquals(1,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<CmpVertex>();partA.addAll(gr.transitionMatrix.keySet());partA.remove(gr.findVertex("E"));
		
		Assert.assertEquals(partA, iterator.next());
	}
	
	/** State E is only identified with d that is not shared with other states. */
	@Test
	public void testBuildVerticesToMergeForPath3()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B / A-b->A / B-a->C-b->D-a->E / D-c->D / E-d->E","testBuildVerticesToMergeForPath3",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));paths.add(Arrays.asList(new Label[]{lblD}));
		Map<CmpVertex,LearnerGraph> grForPaths=PairQualityLearner.constructPathsFromEachState(gr,true);
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=PairQualityLearner.buildVerticesToMergeForPath(paths,grForPaths);
		Assert.assertEquals(2,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<CmpVertex>();partA.addAll(gr.transitionMatrix.keySet());partA.remove(gr.findVertex("E"));
		Set<CmpVertex> partB = new TreeSet<CmpVertex>();partB.add(gr.findVertex("E"));
		
		Assert.assertEquals(partA, iterator.next());
		Assert.assertEquals(partB, iterator.next());
	}
	
	/** States C and E are identified with d that is not shared with other states. */
	@Test
	public void testBuildVerticesToMergeForPath4()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B / A-b->A / B-a->C-d->D-a->E / D-c->D / E-d->E","testBuildVerticesToMergeForPath4",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));paths.add(Arrays.asList(new Label[]{lblD}));
		Map<CmpVertex,LearnerGraph> grForPaths=PairQualityLearner.constructPathsFromEachState(gr,true);
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=PairQualityLearner.buildVerticesToMergeForPath(paths,grForPaths);
		Assert.assertEquals(2,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<CmpVertex>();partA.addAll(gr.transitionMatrix.keySet());partA.remove(gr.findVertex("E"));partA.remove(gr.findVertex("C"));
		Set<CmpVertex> partB = new TreeSet<CmpVertex>();partB.add(gr.findVertex("E"));partB.add(gr.findVertex("C"));
		
		Assert.assertEquals(partA, iterator.next());
		Assert.assertEquals(partB, iterator.next());
	}

	@Test
	public void testBuildVerticesToMergeForPath5()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B / A-b->A / B-a->C-b->D-a->E / D-c->D / E-c->E-d->F-d->F-u->G-u->G","testBuildVerticesToMergeForPath5",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));paths.add(Arrays.asList(new Label[]{lblD}));paths.add(Arrays.asList(new Label[]{lblU}));
		Map<CmpVertex,LearnerGraph> grForPaths=PairQualityLearner.constructPathsFromEachState(gr,true);
		Collection<Set<CmpVertex>> collectionOfSets=PairQualityLearner.buildVerticesToMergeForPath(paths,grForPaths);
		Assert.assertEquals(1,collectionOfSets.size());
		Assert.assertEquals(gr.transitionMatrix.keySet(), collectionOfSets.iterator().next());
	}
	
	/** Same as {@link #testBuildVerticesToMergeForPath1} but with different state labelling. */
	@Test
	public void testBuildVerticesToMergeForPath6()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("B-a->C-b->D-a->E / D-c->D / E-c->E-d->F-d->F-u->G-u->G / Z-a->B / Z-b->Z","testBuildVerticesToMergeForPath6",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));paths.add(Arrays.asList(new Label[]{lblD}));paths.add(Arrays.asList(new Label[]{lblU}));
		Map<CmpVertex,LearnerGraph> grForPaths=PairQualityLearner.constructPathsFromEachState(gr,true);
		Collection<Set<CmpVertex>> collectionOfSets=PairQualityLearner.buildVerticesToMergeForPath(paths,grForPaths);
		Assert.assertEquals(1,collectionOfSets.size());
		Assert.assertEquals(gr.transitionMatrix.keySet(), collectionOfSets.iterator().next());
	}
	
	/** No states identified. */
	@Test
	public void testBuildVerticesToMergeForPath7a()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("B-a->C-b->D-a->E / D-c->D / E-c->E-d->F-d->F-u->G-u->G / Z-a->B / Z-b->Z","testBuildVerticesToMergeForPath6",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();
		Map<CmpVertex,LearnerGraph> grForPaths=PairQualityLearner.constructPathsFromEachState(gr,true);
		Collection<Set<CmpVertex>> collectionOfSets=PairQualityLearner.buildVerticesToMergeForPath(paths,grForPaths);
		Assert.assertTrue(collectionOfSets.isEmpty());
	}
	
	/** No states identified. */
	@Test
	public void testBuildVerticesToMergeForPath7b()
	{
		LearnerGraph gr=new LearnerGraph(config);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));
		Map<CmpVertex,LearnerGraph> grForPaths=PairQualityLearner.constructPathsFromEachState(gr,true);
		Collection<Set<CmpVertex>> collectionOfSets=PairQualityLearner.buildVerticesToMergeForPath(paths,grForPaths);
		Assert.assertTrue(collectionOfSets.isEmpty());
	}
	
	/** A few different clusters identified. */
	@Test
	public void testBuildVerticesToMergeForPath8()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B / A-b->A / B-a->C-d->D-a->E / D-c->D / E-d->E-e->F-d->F-u->F / G-u->G","testBuildVerticesToMergeForPath8",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));paths.add(Arrays.asList(new Label[]{lblD}));paths.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("e", config, converter)}));paths.add(Arrays.asList(new Label[]{lblU}));
		Map<CmpVertex,LearnerGraph> grForPaths=PairQualityLearner.constructPathsFromEachState(gr,true);
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=PairQualityLearner.buildVerticesToMergeForPath(paths,grForPaths);
		Assert.assertEquals(2,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<CmpVertex>();partA.add(gr.findVertex("A"));partA.add(gr.findVertex("B"));partA.add(gr.findVertex("D"));
		Set<CmpVertex> partB = new TreeSet<CmpVertex>();partB.add(gr.findVertex("C"));partB.add(gr.findVertex("E"));partB.add(gr.findVertex("F"));partB.add(gr.findVertex("G"));
		
		Assert.assertEquals(partA, iterator.next());
		Assert.assertEquals(partB, iterator.next());
	}

	/** A few different clusters identified. */
	@Test
	public void testBuildVerticesToMergeForPath9()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B / A-b->A / B-a->C-d->D-a->E / D-c->D / E-d->E-e->F-d->F-u->F / G-e->G","testBuildVerticesToMergeForPath8",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));paths.add(Arrays.asList(new Label[]{lblD}));paths.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("e", config, converter)}));paths.add(Arrays.asList(new Label[]{lblU}));
		Map<CmpVertex,LearnerGraph> grForPaths=PairQualityLearner.constructPathsFromEachState(gr,true);
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=PairQualityLearner.buildVerticesToMergeForPath(paths,grForPaths);
		Assert.assertEquals(2,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<CmpVertex>();partA.add(gr.findVertex("A"));partA.add(gr.findVertex("B"));partA.add(gr.findVertex("D"));
		Set<CmpVertex> partB = new TreeSet<CmpVertex>();partB.add(gr.findVertex("C"));partB.add(gr.findVertex("E"));partB.add(gr.findVertex("F"));partB.add(gr.findVertex("G"));
		
		Assert.assertEquals(partA, iterator.next());
		Assert.assertEquals(partB, iterator.next());
	}
	
	/** A few different clusters identified. */
	@Test
	public void testBuildVerticesToMergeForPath10()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B / A-b->A / B-a->C-d->D-a->E / D-c->D / E-d->E-e->F-d->F-u->F / G-f->G","testBuildVerticesToMergeForPath8",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));paths.add(Arrays.asList(new Label[]{lblD}));paths.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("e", config, converter)}));paths.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("f", config, converter)}));paths.add(Arrays.asList(new Label[]{lblU}));
		Map<CmpVertex,LearnerGraph> grForPaths=PairQualityLearner.constructPathsFromEachState(gr,true);
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=PairQualityLearner.buildVerticesToMergeForPath(paths,grForPaths);
		Assert.assertEquals(3,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<CmpVertex>();partA.add(gr.findVertex("A"));partA.add(gr.findVertex("B"));partA.add(gr.findVertex("D"));
		Set<CmpVertex> partB = new TreeSet<CmpVertex>();partB.add(gr.findVertex("C"));partB.add(gr.findVertex("E"));partB.add(gr.findVertex("F"));
		Set<CmpVertex> partC = new TreeSet<CmpVertex>();partC.add(gr.findVertex("G"));
		
		Assert.assertEquals(partA, iterator.next());
		Assert.assertEquals(partB, iterator.next());
		Assert.assertEquals(partC, iterator.next());
	}
}

