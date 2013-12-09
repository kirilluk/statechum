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
		Assert.assertEquals(0,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)));
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
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)));
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
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)));
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
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)));
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
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)));
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
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)));
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
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)));
		
		Assert.assertEquals(4.,MarkovUniversalLearner.computeInconsistency(graph, true, m, new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)),Configuration.fpAccuracy);// inconsistencies detected are mostly due to state T
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
		Assert.assertEquals(2,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)));
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
		Assert.assertEquals(1,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)));
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
		Assert.assertEquals(0,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("B"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)));// everything as expected.
		Assert.assertEquals(0,m.checkFanoutInconsistency(Inverse_Graph,true,graph,graph.findVertex("D"),m.getChunkLen(), new MarkovUniversalLearner.DifferentPredictionsInconsistency(graph)));// missing reject-transition with label u is ignored because we are only considering actual outgoing transitions
	}

	@Test
	public void testMarkovUpdate1()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovUniversalLearner mOther = new MarkovUniversalLearner(2);
		mOther.predictTransitionsAndUpdateMarkovForward(graph);
		Assert.assertEquals(m.getMarkov(true),mOther.getMarkov(true));Assert.assertTrue(m.getMarkov(false).isEmpty());
	}
	
	@Test
	public void testMarkovUpdate2()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] {},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovUniversalLearner mOther = new MarkovUniversalLearner(2);mOther.predictTransitionsAndUpdateMarkovForward(graph);
		Assert.assertEquals(m.getMarkov(true),mOther.getMarkov(true));Assert.assertTrue(m.getMarkov(false).isEmpty());
	}
	
	@Test
	public void testMarkovUpdate3()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] {},config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovUniversalLearner mOther = new MarkovUniversalLearner(2);mOther.predictTransitionsAndUpdateMarkovForward(graph);
		Assert.assertEquals(m.getMarkov(true),mOther.getMarkov(true));Assert.assertTrue(m.getMarkov(false).isEmpty());
	}
	
	@Test
	public void testMarkovUpdate4()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovUniversalLearner mOther = new MarkovUniversalLearner(2);mOther.predictTransitionsAndUpdateMarkovForward(graph);
		Assert.assertEquals(m.getMarkov(true),mOther.getMarkov(true));Assert.assertTrue(m.getMarkov(false).isEmpty());
	}
	
	@Test
	public void testUpdateMarkovSideways1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->C","testUpdateMarkovSideways1",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkovSideways(graph);
		Assert.assertTrue(m.getMarkov(true).isEmpty());Assert.assertEquals(4,m.getMarkov(false).size());
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.getMarkov(false).get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));
	}
	
	@Test
	public void testUpdateMarkovSideways2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->C / B-b-#D","testUpdateMarkovSideways2",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkovSideways(graph);
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
		m.predictTransitionsAndUpdateMarkovSideways(graph);
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
		m.predictTransitionsAndUpdateMarkovSideways(graph);
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
		m.predictTransitionsAndUpdateMarkovSideways(graph);
		Assert.assertTrue(m.getMarkov(true).isEmpty());Assert.assertTrue(m.getMarkov(false).isEmpty());
	}

	@Test
	public void testPredictTransitionsSideways1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkovSideways(graph);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		Configuration shallowCopy = graph.config.copy();shallowCopy.setLearnerCloneGraph(false);
		
		List<List<Label>> interestingPaths = new LinkedList<List<Label>>();
		// nothing in Markov matrix hence no predictions.
		Assert.assertTrue(m.predictTransitionsFromState(graph,true,graph.findVertex("B"),graph.pathroutines.computeAlphabet(),null,2,interestingPaths).isEmpty());
		Assert.assertEquals(1,interestingPaths.size());Assert.assertEquals(Arrays.asList(new Label[]{lblB}),interestingPaths.get(0));
		
		interestingPaths.clear();
		Map<Label, MarkovOutcome> outcome1 = m.predictTransitionsFromState(graph,false,graph.findVertex("B"),graph.pathroutines.computeAlphabet(),null,2,interestingPaths);
		Assert.assertEquals(1,interestingPaths.size());Assert.assertEquals(Arrays.asList(new Label[]{lblB}),interestingPaths.get(0));
		Assert.assertEquals(2,outcome1.size());
		Assert.assertEquals(MarkovOutcome.negative,outcome1.get(lblU));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblB));
		
		
		interestingPaths.clear();
		outcome1 = m.predictTransitionsFromState(graph,false,graph.findVertex("E"),graph.pathroutines.computeAlphabet(),null,2,interestingPaths);
		Assert.assertEquals(2,interestingPaths.size());Assert.assertEquals(Arrays.asList(new Label[]{lblC}),interestingPaths.get(0));Assert.assertEquals(Arrays.asList(new Label[]{lblU}),interestingPaths.get(1));
		Assert.assertEquals(3,outcome1.size());
		Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblU));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblC));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblA));
	}		

	@Test
	public void testPredictTransitionsSideways2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-b->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkovSideways(graph);
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
		m.predictTransitionsAndUpdateMarkovSideways(graph);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		Map<Label, MarkovOutcome> predictions = m.predictTransitionsFromState(graph2,false,graph2.getInit(),graph.pathroutines.computeAlphabet(),null,m.getChunkLen(),null);
		
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblC));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblA));
	}
	
	@Test
	public void testPredictTransitionsFromStatesForward1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkovSideways(graph);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> predictions = m.predictTransitions(graph2,true);
		Assert.assertTrue(predictions.isEmpty());// empty Markov means no predictions.
	}
	
	@Test
	public void testPredictTransitionsFromStatesForward2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkovForward(graph);
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

	@Test
	public void testPredictTransitionsFromStatesForward3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkovForward(graph);
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
		m.predictTransitionsAndUpdateMarkovSideways(graph);
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
		m.predictTransitionsAndUpdateMarkovSideways(graph);
		Assert.assertTrue(m.getMarkov(true).isEmpty());
		Assert.assertEquals(9,m.getMarkov(false).size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / T-a->T-u->T-b->T-c->T","testPredictTransitionsFromStatesSideways3",config, converter);
		m.constructMarkovTentative(graph2, false);
		Assert.assertNull(WMethod.checkM(FsmParser.buildLearnerGraph("A-a->B / A-c->F","testPredictTransitionsFromStatesForward3",config, converter), m.get_extension_model()));// FSM comparison ignores unreachable states here
		Assert.assertTrue(m.get_extension_model().findVertex("T") != null);// extended graph starts as a replica of an original one.
	}
	
	@Test
	public void testCheckFanoutInconsistencySideways1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkovSideways(graph);
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
			public boolean labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				Assert.assertEquals(MarkovOutcome.positive,actual);Assert.assertNull(predicted);
				counterB.addAndGet(1);
				return true;
			}

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") CmpVertex v) {
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
		m.predictTransitionsAndUpdateMarkovSideways(graph);
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
			public boolean labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				Assert.fail("should not be called");
				return true;
			}
			

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") CmpVertex v) {
				return graph2.pathroutines.computeAlphabet();
			}
		}));
	}
	
	
	@Test
	public void testCheckFanoutInconsistencySideways3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		m.predictTransitionsAndUpdateMarkovSideways(graph);
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
			public boolean labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterB.addAndGet(1);
				return true;
			}

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") CmpVertex v) {
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
		m.predictTransitionsAndUpdateMarkovSideways(graph);
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
			public boolean labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterB.addAndGet(1);
				return true;
			}

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") CmpVertex v) {
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
		m.predictTransitionsAndUpdateMarkovSideways(graph);
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
			public boolean labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterB.addAndGet(1);
				return false;
			}

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") CmpVertex v) {
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
		m.predictTransitionsAndUpdateMarkovSideways(graph);
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
			public boolean labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				counterB.addAndGet(1);
				return false;
			}

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") CmpVertex v) {
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
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions2a",config, converter), uniques);
		Assert.assertTrue(uniqueStates.isEmpty());
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
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions2a",config, converter), uniques);
		Assert.assertTrue(uniqueStates.isEmpty());
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
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-b->C","testIdentifyUncoveredTransitions2a",config, converter), uniques);
		Assert.assertEquals(1,uniqueStates.size());
		Assert.assertEquals("A",uniqueStates.iterator().next().getStringId());
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
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions3a",config, converter), uniques);
		Assert.assertEquals(1,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
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
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-a->C-a->D","testIdentifyUncoveredTransitions3b",config, converter), uniques);
		Assert.assertTrue(uniqueStates.isEmpty());
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
		Collection<CmpVertex> uniqueStates=MarkovPassivePairSelection.statesIdentifiedUsingUniques(FsmParser.buildLearnerGraph("A-a->B-b->C","testIdentifyUncoveredTransitions2a",config, converter), uniques);
		Assert.assertEquals(2,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
		Assert.assertEquals("B",uniqueVertices.next().getStringId());
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
	
}

