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
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.MarkovModel.UpdatablePairInteger;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.MarkovModel.MarkovOutcome;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;

public class TestMarkovLearner 
{
	final Configuration config = Configuration.getDefaultConfiguration().copy();
	final ConvertALabel converter = null;

	final Label lblA = AbstractLearnerGraph.generateNewLabel("a", config, converter),lblB = AbstractLearnerGraph.generateNewLabel("b", config, converter),lblC = AbstractLearnerGraph.generateNewLabel("c", config, converter),
			lblD = AbstractLearnerGraph.generateNewLabel("d", config, converter),lblU = AbstractLearnerGraph.generateNewLabel("u", config, converter);
	
	@Test
	public void testGetChunks1() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Arrays.asList(new Label[]{}),true),1);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks2a() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true),4);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks2b() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true),10);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks3() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true),1);
		Assert.assertEquals(3,l.size());
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblA}),true),l.get(0));
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblB}),true),l.get(1));
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblC}),true),l.get(2));
	}

	@Test
	public void testGetChunks4() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true),2);
		Assert.assertEquals(2,l.size());
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true),l.get(0));
		Assert.assertEquals(new Trace(Arrays.asList(new Label[]{lblB,lblC}),true),l.get(1));
	}

	@Test
	public void testGetChunks5() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true),3);
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
				new MarkovModel(1,true,true);
			}
		}, IllegalArgumentException.class, "chunkLen");
	}
	
	@Test
	public void testCreateMarkovMatrix1()
	{
		MarkovModel m = new MarkovModel(2,true,true);
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
		MarkovModel m = new MarkovModel(2,true,true);
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
		MarkovModel m = new MarkovModel(2,true,true);
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
		MarkovModel m = new MarkovModel(3,true,true);
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
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{"u"} },config,converter);
		Map<Trace, MarkovOutcome> matrix = m.createMarkovLearner(plusStrings, minusStrings,false);
		Assert.assertEquals(1,matrix.size());

		Assert.assertSame(MarkovOutcome.negative, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}),true)));
	}
	
	/** Same as testCreateMarkovMatrix3 but contains an empty trace which is ignored since it does not match any of the valid chunk sizes. */
	@Test
	public void testCreateMarkovMatrix5()
	{
		final MarkovModel m = new MarkovModel(2,true,true);
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
		final MarkovModel m = new MarkovModel(2,true,true);
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
		final MarkovModel m = new MarkovModel(2,true,true);
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
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","p"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-u->B-p->B","testConstructExtendedGraph1",config, converter);
		MarkovClassifier cl = new MarkovClassifier(m,graph);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		Assert.assertTrue(newTransitions.isEmpty());// not enough evidence to update, hence nothing should be recorded.
		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-u->B-p->B","testConstructExtendedGraph1",config, converter);
		LearnerGraph actual = cl.constructMarkovTentative();
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, actual);
	}
	
	/** Nothing to add because the alphabet of the graph of interest consists of a single letter for which there is no statistical data. */
	@Test
	public void testConstructExtendedGraph2()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","p"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B","testConstructExtendedGraph2",config, converter);
		MarkovClassifier cl = new MarkovClassifier(m,graph);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		Assert.assertTrue(newTransitions.isEmpty());// not enough evidence to update, hence nothing should be recorded.
		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B","testConstructExtendedGraph2",config, converter);
		LearnerGraph actual = cl.constructMarkovTentative();
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, actual);
	}
	
	/** A bit has been added. */
	@Test
	public void testConstructExtendedGraph3()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3a",config, converter);
		MarkovClassifier cl = new MarkovClassifier(m,graph);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertSame(MarkovOutcome.negative, newTransitions.get(graph.findVertex("B")).get(lblU));
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / T-b->T-u->T","testConstructExtendedGraph3b",config, converter);
		LearnerGraph actual = cl.constructMarkovTentative();
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, actual);
	}
	
	/** A bit has been added, but reject-transition was not because is present both in the positive and negative light in the initial traces. */
	@Test
	public void testConstructExtendedGraph4()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"a","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3a",config, converter);
		MarkovClassifier cl = new MarkovClassifier(m,graph);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertFalse(newTransitions.get(graph.findVertex("B")).containsKey(lblU));// failure ignored
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-b->C / T-b->T-u->T","testConstructExtendedGraph4b",config, converter);
		LearnerGraph actual = cl.constructMarkovTentative();
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, actual);
	}
	
	
	/** A bit has been added, but reject-transition was not because is present both in the positive and negative light in the initial traces. */
	@Test
	public void testConstructExtendedGraph5()
	{
		MarkovModel m = new MarkovModel(2,true,true);// w below is to ensure that all elements of the alphabet are included in traces.
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"},new String[]{"w"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-w->M-c->B / T-b->T-u->T","testConstructExtendedGraph5a",config, converter);// the purpose of the w-transition is to ensure transition c is taken into account in graph comparison
		MarkovClassifier cl = new MarkovClassifier(m,graph);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		Assert.assertEquals(1,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		
		Assert.assertSame(MarkovOutcome.positive,newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-b->C / A-w->M-c->B / T-b->T-u->T","testConstructExtendedGraph5b",config, converter);
		LearnerGraph actual = cl.constructMarkovTentative();
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, actual);
	}
	
	/** Nothing has been added. u has been seen both in the positive and negative light and lead to inconsistency. */
	@Test
	public void testConstructExtendedGraph6()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / T-b->T-u->T","testConstructExtendedGraph6a",config, converter);
		MarkovClassifier cl = new MarkovClassifier(m,graph);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		
		Assert.assertEquals(1,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-b->C / T-b->T-u->T","testConstructExtendedGraph6b",config, converter);
		LearnerGraph actual = cl.constructMarkovTentative();
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, actual);
	}
	
	/** A bit has been added. u has been seen both in the positive and negative light. */
	@Test
	public void testConstructExtendedGraph7()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B-c->Z / T-b->T-u->T","testConstructExtendedGraph7a",config, converter);
		MarkovClassifier cl = new MarkovClassifier(m,graph);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		
		Assert.assertEquals(2,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		Assert.assertEquals(1,newTransitions.get(graph.findVertex("Z")).size());
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("Z")).get(lblU));

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B / A-c->B-c->Z-u->Y / B-b->C / T-b->T-u->T","testConstructExtendedGraph7b",config, converter);
		LearnerGraph actual = cl.constructMarkovTentative();
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, actual);
	}
	
	/** No outgoing from B, hence no inconsistencies. */
	@Test
	public void testCheckFanoutInconsistency1a()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / T-b->T-u->T","testCheckFanoutInconsistency1a",config, converter);
		
		Assert.assertEquals(0,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** One from B with inconsistent predictions. */
	@Test
	public void testCheckFanoutInconsistency1b1()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency1b1",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** One from B where Markov cannot make up its mind for au+ v.s. au- and c-transition is ignored. Label b from A is not reported by the checker as something to check and hence is ignored. */
	@Test
	public void testCheckFanoutInconsistency1b2()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"a","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency1b2",config, converter);
		
		Assert.assertEquals(0,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** One from B where Markov cannot make up its mind for au+ v.s. au- and c-transition is ignored. Label b from A IS reported by the checker as something to check and hence is causes an inconsistency. */
	@Test
	public void testCheckFanoutInconsistency1b3()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"a","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency1b2",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()
		{

			/**
			 * @see statechum.analysis.learning.MarkovClassifier.DifferentPredictionsInconsistency#obtainAlphabet(statechum.analysis.learning.rpnicore.AbstractLearnerGraph, statechum.DeterministicDirectedSparseGraph.CmpVertex)
			 */
			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public Collection<Label> obtainAlphabet(AbstractLearnerGraph gr,@SuppressWarnings("unused") CmpVertex v) 
			{
				return gr.getCache().getAlphabet();
			}
			
		}
		));
	}
	
	/** Transition d exists as positive but should be present as negative according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1c()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"},new String[]{"a","d"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-d->F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1c",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** Transition d exists as positive but should be absent according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1d()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"}},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-d->F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1d",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** Transition b exists as negative but should be present as positive according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1e()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"}},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-b-#F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1e",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** Transition d exists as negative but should be absent according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1f()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-d-#F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1f",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
		
		Assert.assertEquals(4.,MarkovClassifier.computeInconsistency(graph,  m, new MarkovClassifier.DifferentPredictionsInconsistency(),false),Configuration.fpAccuracy);// inconsistencies detected are mostly due to state T
	}
	
	/** Two inconsistencies, transition u and transition b which should not exist after c. */
	@Test
	public void testCheckFanoutInconsistency2()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B-b->C / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency2",config, converter);
		
		Assert.assertEquals(2,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** One inconsistency: transition u. */
	@Test
	public void testCheckFanoutInconsistency3()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B-u->C / T-b->T-u->T","testCheckFanoutInconsistency3",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** No inconsistencies since there are very few paths. */
	@Test
	public void testCheckFanoutInconsistency4()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / T-b->T-u->T","testCheckFanoutInconsistency4",config, converter);
		
		
		Assert.assertEquals(0,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));// everything as expected.
		Assert.assertEquals(0,new MarkovClassifier(m,graph).checkFanoutInconsistency(graph.findVertex("D"),new MarkovClassifier.DifferentPredictionsInconsistency()));// missing reject-transition with label u is ignored because we are only considering actual outgoing transitions
	}

	/** Tests that creating a model from PTA and from initial traces gives the same result. */
	@Test
	public void testMarkovUpdate1_longest()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovModel mOther = new MarkovModel(2,true,true);
		new MarkovClassifier(mOther,graph).updateMarkov(true);
		Assert.assertEquals(m.predictionsMatrix,mOther.predictionsMatrix);
		Assert.assertEquals(m.occurrenceMatrix,mOther.occurrenceMatrix);
	}
		
	/** Tests that creating a model from PTA and from initial traces gives almost the same result. The difference is in PTA-based construction mis-counting the number of times shorter traces occur since it
	 * can see that they exist but not the number of tails they lead to. This is left in because I do not use specific values occurrence counts. 
	 */
	@Test
	public void testMarkovUpdate1_prefixclosed()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovModel mOther = new MarkovModel(2,true,true);
		new MarkovClassifier(mOther,graph).updateMarkov(false);
		Assert.assertEquals(m.predictionsMatrix,mOther.predictionsMatrix);
		
		// Workaround around a deficiency in the calculation of occurrences of prefixes by the PTA-based construction of Markov model.
		Assert.assertEquals(new UpdatablePairInteger(2, 0), m.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));
		Assert.assertEquals(new UpdatablePairInteger(1, 0), mOther.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));
		
		m.occurrenceMatrix.remove(new Trace(Arrays.asList(new Label[]{lblA}),true));mOther.occurrenceMatrix.remove(new Trace(Arrays.asList(new Label[]{lblA}),true));
		Assert.assertEquals(m.occurrenceMatrix,mOther.occurrenceMatrix);
	}

	/** Tests that creating a model from PTA and from initial traces give the same result. */
	@Test
	public void testMarkovUpdate2()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] {},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovModel mOther = new MarkovModel(2,true,true);new MarkovClassifier(mOther,graph).updateMarkov(true);
		Assert.assertEquals(m.predictionsMatrix,mOther.predictionsMatrix);
		Assert.assertEquals(m.occurrenceMatrix,mOther.occurrenceMatrix);
	}
	
	/** Tests that creating a model from PTA and from initial traces give the same result. */
	@Test
	public void testMarkovUpdate3()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] {},config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovModel mOther = new MarkovModel(2,true,true);new MarkovClassifier(mOther,graph).updateMarkov(true);
		Assert.assertEquals(m.predictionsMatrix,mOther.predictionsMatrix);
		Assert.assertEquals(m.occurrenceMatrix,mOther.occurrenceMatrix);
	}
	
	@Test
	/** Tests that creating a model from PTA and from initial traces give the same result. */
	public void testMarkovUpdate4()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovModel mOther = new MarkovModel(2,true,true);new MarkovClassifier(mOther,graph).updateMarkov(true);
		Assert.assertEquals(m.predictionsMatrix,mOther.predictionsMatrix);
		Assert.assertEquals(m.occurrenceMatrix,mOther.occurrenceMatrix);
	}
	
	@Test
	public void testUpdateMarkovSideways1a()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->C","testUpdateMarkovSideways1",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(4,m.predictionsMatrix.size());
		Assert.assertEquals(4,m.occurrenceMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));Assert.assertEquals(new UpdatablePairInteger(2, 0),m.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));Assert.assertEquals(new UpdatablePairInteger(1, 0),m.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));Assert.assertEquals(new UpdatablePairInteger(1, 0),m.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));Assert.assertEquals(new UpdatablePairInteger(1, 0),m.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));
	}
	
	@Test
	public void testUpdateMarkovSideways1b()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->C","testUpdateMarkovSideways1",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(6,m.predictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));Assert.assertEquals(new UpdatablePairInteger(2, 0),m.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));Assert.assertEquals(new UpdatablePairInteger(1, 0),m.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));Assert.assertEquals(new UpdatablePairInteger(1, 0),m.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));Assert.assertEquals(new UpdatablePairInteger(1, 0),m.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));

		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));Assert.assertEquals(new UpdatablePairInteger(2, 0),m.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB}),true)));Assert.assertEquals(new UpdatablePairInteger(1, 0),m.occurrenceMatrix.get(new Trace(Arrays.asList(new Label[]{lblB}),true)));
	}
	
	/** This one is similar to the {@link #testUpdateMarkovSideways1b}, except that there are a few additional negative transitions. */
	@Test
	public void testUpdateMarkovSideways1c()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->C-a-#D / B-c-#D","testUpdateMarkovSideways1c",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(9,m.predictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblC}),true)));

		Assert.assertEquals(MarkovOutcome.failure,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC}),true)));
	}
	
	/** This one is similar to the {@link #testUpdateMarkovSideways1b}, except that there are a few additional negative transitions and the computation is forward. */
	@Test
	public void testUpdateMarkovSideways1d()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->C-a-#D / B-c-#D","testUpdateMarkovSideways1c",config, converter);
		MarkovModel m = new MarkovModel(2,true,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(7,m.predictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.failure,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));

		Assert.assertEquals(MarkovOutcome.failure,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC}),true)));
		
		Set<List<Label>> plusStrings = buildSet(new String[][] {},config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","a","a"},new String[]{"a","b","a"},new String[]{"a","c"} },config,converter);
		MarkovModel another = new MarkovModel(2,true,true);
		another.createMarkovLearner(plusStrings, minusStrings, false);

		Assert.assertEquals(7,another.predictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.failure,another.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,another.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,another.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.negative,another.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblA}),true)));

		Assert.assertEquals(MarkovOutcome.failure,another.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,another.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB}),true)));
		Assert.assertEquals(MarkovOutcome.negative,another.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC}),true)));
	}

	@Test
	public void testUpdateMarkovSideways2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->C / B-b-#D","testUpdateMarkovSideways2",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(3,m.predictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.negative,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblC}),true)));
	}
	
	@Test
	public void testUpdateMarkovSideways3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(9,m.predictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblA}),true)));
		
		Assert.assertEquals(MarkovOutcome.negative,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblU}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));
		
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblU,lblU}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblU}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblU,lblC}),true)));
	}
	
	@Test
	public void testUpdateMarkovSideways4()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(3,false,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(6,m.predictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB,lblA}),true)));
		
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblU,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblU,lblA}),true)));
		
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblC,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblC,lblA}),true)));
	}
	
	@Test
	public void testUpdateMarkovSideways5()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(4,false,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertTrue(m.predictionsMatrix.isEmpty());
	}

	@Test
	public void testPredictTransitionsSideways1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel mSideways = new MarkovModel(2,false,true), mForward = new MarkovModel(2,true,true);
		new MarkovClassifier(mSideways,graph).updateMarkov(true);
		Assert.assertEquals(9,mSideways.predictionsMatrix.size());Assert.assertTrue(mForward.predictionsMatrix.isEmpty());
		
		List<List<Label>> interestingPaths = new LinkedList<List<Label>>();
		// nothing in Markov matrix hence no predictions.
		Assert.assertTrue(new MarkovClassifier(mForward,graph).predictTransitionsFromState(graph.findVertex("B"),null,2,interestingPaths).isEmpty());
		Assert.assertEquals(1,interestingPaths.size());Assert.assertEquals(Arrays.asList(new Label[]{lblA}),interestingPaths.get(0));
		
		interestingPaths.clear();
		Map<Label, MarkovOutcome> outcome1 = new MarkovClassifier(mSideways,graph).predictTransitionsFromState(graph.findVertex("B"),null,2,interestingPaths);
		Assert.assertEquals(1,interestingPaths.size());Assert.assertEquals(Arrays.asList(new Label[]{lblB}),interestingPaths.get(0));
		Assert.assertEquals(2,outcome1.size());
		Assert.assertEquals(MarkovOutcome.negative,outcome1.get(lblU));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblB));
		
		
		interestingPaths.clear();
		outcome1 = new MarkovClassifier(mSideways,graph).predictTransitionsFromState(graph.findVertex("E"),null,2,interestingPaths);
		Assert.assertEquals(2,interestingPaths.size());Assert.assertEquals(Arrays.asList(new Label[]{lblC}),interestingPaths.get(0));Assert.assertEquals(Arrays.asList(new Label[]{lblU}),interestingPaths.get(1));
		Assert.assertEquals(3,outcome1.size());
		Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblU));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblC));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblA));
	}		

	@Test
	public void testPredictTransitionsSideways2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-b->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		
		Assert.assertEquals(8,m.predictionsMatrix.size());
		
		Assert.assertEquals(MarkovOutcome.failure,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblU}),true)));
		
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblA}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblC}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblC}),true)));
		
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblU,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblB}),true)));
		Assert.assertEquals(MarkovOutcome.positive,m.predictionsMatrix.get(new Trace(Arrays.asList(new Label[]{lblU,lblU}),true)));
	}

	/** Tests that upon a label labelled as invalid, subsequent inconsistency checks are stopped. It is hence equivalent to a single incoming path. */
	@Test
	public void testPredictTransitionsFromStatesSideways1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A / T-u->T-b->T","testCheckFanoutInconsistencySideways4",config, converter);// T is there to ensure that graph2's alphabet is the same as that of graph.
		Map<Label, MarkovOutcome> predictions = new MarkovClassifier(m,graph2).predictTransitionsFromState(graph2.getInit(),null,m.getChunkLen(),null);
		
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblC));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblA));
	}
	
	@Test
	public void testPredictTransitionsFromStatesForward1()
	{
		MarkovModel m = new MarkovModel(2,true,true);
		Assert.assertTrue(m.predictionsMatrix.isEmpty());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> predictions = new MarkovClassifier(m, graph2).predictTransitions();
		Assert.assertTrue(predictions.isEmpty());// empty Markov means no predictions.
	}
	
	@Test
	public void testPredictTransitionsFromStatesForward2a()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(4,m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> predictions = new MarkovClassifier(m, graph2).predictTransitions();
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
		MarkovModel m = new MarkovModel(2,true,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(4,m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = new LearnerGraph(config);graph2.getInit().setAccept(false);
		Map<CmpVertex, Map<Label, MarkovOutcome>> predictions = new MarkovClassifier(m, graph2).predictTransitions();
		Assert.assertTrue(predictions.isEmpty());
		predictions = new MarkovClassifier(new MarkovModel(2,false,true),graph2).predictTransitions();
		Assert.assertTrue(predictions.isEmpty());
	}
	
	@Test
	public void testPredictTransitionsFromStatesForward3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(4,m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A/ T-a->T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		LearnerGraph extendedGraph = new MarkovClassifier(m,graph2).constructMarkovTentative();
		Assert.assertNull(WMethod.checkM(FsmParser.buildLearnerGraph("A-a->B / A-c->A / A-u->E / B-b->C / B-u-#D","testPredictTransitionsFromStatesForward3",config, converter), extendedGraph));
		Assert.assertTrue(extendedGraph.findVertex("T") != null);// extended graph starts as a replica of an original one.
	}

	/** Here the alphabet is limited to what is an the tentative automaton, hence nothing is added. */
	@Test
	public void testPredictTransitionsFromStatesSideways2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(9,m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B","testCheckFanoutInconsistencySideways4",config, converter);
		LearnerGraph extendedGraph = new MarkovClassifier(m,graph2).constructMarkovTentative();
		Assert.assertNull(WMethod.checkM(graph2,extendedGraph));
	}
	
	/** Tests that upon a label labelled as invalid, subsequent inconsistency checks are stopped. It is hence equivalent to a single incoming path. */
	@Test
	public void testPredictTransitionsFromStatesSideways3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(9,m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / T-a->T-u->T-b->T-c->T","testPredictTransitionsFromStatesSideways3",config, converter);
		LearnerGraph extendedGraph = new MarkovClassifier(m,graph2).constructMarkovTentative();
		Assert.assertNull(WMethod.checkM(FsmParser.buildLearnerGraph("A-a->B / A-c->F","testPredictTransitionsFromStatesForward3",config, converter), extendedGraph));// FSM comparison ignores unreachable states here
		Assert.assertTrue(extendedGraph.findVertex("T") != null);// extended graph starts as a replica of an original one.
	}

	/** Same as {@link #testPredictTransitionsFromStatesSideways1()}, except that the path beyond is empty rather than null. */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A / T-u->T-b->T","testCheckFanoutInconsistencySideways4",config, converter);// T is there to ensure that graph2's alphabet is the same as that of graph.
		Map<Label, MarkovOutcome> predictions = new MarkovClassifier(m, graph2).predictTransitionsFromState(graph2.getInit(),Arrays.asList(new Label[]{}),m.getChunkLen(),null);
		
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblC));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblA));
	}
	
	/** Same as {@link #testPredictTransitionsFromStatesSideways1()}, except that the path beyond is non-empty. */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		final MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B","testPredictTransitionsFromStatesWithPathBeyondCurrentState2",config, converter);
		
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				new MarkovClassifier(m, graph2).predictTransitionsFromState(graph2.getInit(),Arrays.asList(new Label[]{lblC}),m.getChunkLen(),null);
			}
		}, IllegalArgumentException.class, "cannot be made by extension");
	}
	
	/** Almost the same as {@link #testPredictTransitionsFromStatesForward2()} except that the path beyond is empty rather than null. */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(4,m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		Map<Label,MarkovOutcome> outgoing_labels_probabilities=new MarkovClassifier(m, graph2).predictTransitionsFromState(graph2.findVertex("B"),Arrays.asList(new Label[]{}),m.getChunkLen(),null);
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
		MarkovModel m = new MarkovModel(2,true,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(4,m.predictionsMatrix.size());

		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-d->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		Map<Label,MarkovOutcome> outgoing_labels_probabilities=new MarkovClassifier(m, graph2).predictTransitionsFromState(graph2.findVertex("B"),Arrays.asList(new Label[]{lblA}),m.getChunkLen(),null);
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
		final MarkovModel m = new MarkovModel(2,true,true);
		new MarkovClassifier(m,graph).updateMarkov(true);
		Assert.assertEquals(4,m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-d->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				new MarkovClassifier(m, graph2).predictTransitionsFromState(graph2.findVertex("B"),Arrays.asList(new Label[]{lblA,lblB}),m.getChunkLen(),null);
			}
		}, IllegalArgumentException.class, "supplied path");
	}

	/** Here we look for path "s" in Markov that has never been seen, hence we cannot do much about it and it is being ignored. */ 
	@Test
	public void testCheckFanoutInconsistencySideways1_s()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-s->B","testCheckFanoutInconsistencySideways1",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifier(m, graph2).checkFanoutInconsistency(graph2.getInit(),new MarkovClassifier.ConsistencyChecker(){

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

			@SuppressWarnings("rawtypes")
			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph2.pathroutines.computeAlphabet();
			}
		}));
		Assert.assertEquals(0, counterA.get());Assert.assertEquals(0, counterB.get());
	}
	
	/** Here we look for path "s" in Markov that has never been seen, hence we cannot do much about it and it is being ignored. */
	@Test
	public void testCheckFanoutInconsistencySideways1_a()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B","testCheckFanoutInconsistencySideways1",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifier(m, graph2).checkFanoutInconsistency(graph2.getInit(),new MarkovClassifier.ConsistencyChecker(){

			@Override
			public boolean consistent(MarkovOutcome actual,MarkovOutcome predicted) {
				Assert.assertEquals(MarkovOutcome.positive,actual);Assert.assertNotNull(predicted);
				counterA.addAndGet(1);
				return true;
			}

			@Override
			public MarkovOutcome labelConsistent(MarkovOutcome actual,MarkovOutcome predicted) {
				Assert.assertEquals(MarkovOutcome.positive,actual);Assert.assertNotNull(predicted);
				counterB.addAndGet(1);
				return actual;
			}

			@SuppressWarnings("rawtypes")
			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph2.pathroutines.computeAlphabet();
			}
		}));
		Assert.assertEquals(1, counterA.get());Assert.assertEquals(1, counterB.get());
	}
	
	/** Checks that where we did not build prefix-closed paths, nothing can be predicted because we check whether shorter chunklen-1 -long paths exist and ignore anything where such paths do not exist. */
	@Test
	public void testCheckFanoutInconsistencySideways2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B-a->B-b->B","testCheckFanoutInconsistencySideways2",config, converter);
		graph2.transitionMatrix.get(graph2.getInit()).clear();// make it look like a graph has no transitions
		Assert.assertEquals(0,new MarkovClassifier(m, graph2).checkFanoutInconsistency(graph2.getInit(), new MarkovClassifier.ConsistencyChecker(){

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
			

			@SuppressWarnings({ "rawtypes", "unchecked" })
			@Override
			public Collection<Label> obtainAlphabet(AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graphToConsider.getCache().getAlphabet();
			}
		}));
	}
	
	@Test
	public void testCheckFanoutInconsistencySideways3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B","testCheckFanoutInconsistencySideways3",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifier(m, graph2).checkFanoutInconsistency(graph2.getInit(), new MarkovClassifier.ConsistencyChecker(){

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

			@SuppressWarnings("rawtypes")
			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
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
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifier(m, graph2).checkFanoutInconsistency(graph2.getInit(), new MarkovClassifier.ConsistencyChecker(){

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

			@SuppressWarnings("rawtypes")
			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
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
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B","testCheckFanoutInconsistencySideways3",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifier(m, graph2).checkFanoutInconsistency(graph2.getInit(), new MarkovClassifier.ConsistencyChecker(){

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

			@SuppressWarnings("rawtypes")
			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
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
		MarkovModel m = new MarkovModel(2,false,true);
		new MarkovClassifier(m,graph).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.predictionsMatrix.size());
		
		final LearnerGraph graph2 = FsmParser.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifier(m, graph2).checkFanoutInconsistency(graph2.getInit(), new MarkovClassifier.ConsistencyChecker(){

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

			
			@SuppressWarnings("rawtypes")
			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
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
	public void testCollectionOfSetsToPair1()
	{
		Collection<Set<CmpVertex>> collectionOfSets = new LinkedList<Set<CmpVertex>>();
		List<StatePair> outcome = MarkovClassifier.collectionOfSetsToPairs(collectionOfSets);
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
		List<StatePair> outcome = MarkovClassifier.collectionOfSetsToPairs(collectionOfSets);
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
		List<StatePair> outcome = MarkovClassifier.collectionOfSetsToPairs(collectionOfSets);
		Assert.assertTrue(outcome.isEmpty());
	}
	
	@Test
	public void testBuildVerticesToMergeForPath1()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B / A-b->A / B-a->C-b->D-a->E / D-c->D / E-c->E","testBuildVerticesToMergeForPath1",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));
		
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifier(new MarkovModel(2,true,true),gr).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(1,collectionOfSets.size());
		Assert.assertEquals(gr.transitionMatrix.keySet(), collectionOfSets.iterator().next());
	}
	
	/** Last state not to be merged. */
	@Test
	public void testBuildVerticesToMergeForPath2()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B / A-b->A / B-a->C-b->D-a->E / D-c->D / E-d->E","testBuildVerticesToMergeForPath1",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets = new MarkovClassifier(new MarkovModel(2,true,true),gr).buildVerticesToMergeForPaths(paths);
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
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifier(new MarkovModel(2,true,true),gr).buildVerticesToMergeForPaths(paths);
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
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifier(new MarkovModel(2,true,true),gr).buildVerticesToMergeForPaths(paths);
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
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifier(new MarkovModel(2,true,true),gr).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(1,collectionOfSets.size());
		Assert.assertEquals(gr.transitionMatrix.keySet(), collectionOfSets.iterator().next());
	}
	
	/** Same as {@link #testBuildVerticesToMergeForPath1} but with different state labelling. */
	@Test
	public void testBuildVerticesToMergeForPath6()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("B-a->C-b->D-a->E / D-c->D / E-c->E-d->F-d->F-u->G-u->G / Z-a->B / Z-b->Z","testBuildVerticesToMergeForPath6",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));paths.add(Arrays.asList(new Label[]{lblD}));paths.add(Arrays.asList(new Label[]{lblU}));
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifier(new MarkovModel(2,true,true),gr).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(1,collectionOfSets.size());
		Assert.assertEquals(gr.transitionMatrix.keySet(), collectionOfSets.iterator().next());
	}
	
	/** No states identified. */
	@Test
	public void testBuildVerticesToMergeForPath7a()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("B-a->C-b->D-a->E / D-c->D / E-c->E-d->F-d->F-u->G-u->G / Z-a->B / Z-b->Z","testBuildVerticesToMergeForPath6",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifier(new MarkovModel(2,true,true),gr).buildVerticesToMergeForPaths(paths);
		Assert.assertTrue(collectionOfSets.isEmpty());
	}
	
	/** No states identified. */
	@Test
	public void testBuildVerticesToMergeForPath7b()
	{
		LearnerGraph gr=new LearnerGraph(config);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifier(new MarkovModel(2,true,true),gr).buildVerticesToMergeForPaths(paths);
		Assert.assertTrue(collectionOfSets.isEmpty());
	}
	
	/** A few different clusters identified. */
	@Test
	public void testBuildVerticesToMergeForPath8()
	{
		LearnerGraph gr=FsmParser.buildLearnerGraph("A-a->B / A-b->A / B-a->C-d->D-a->E / D-c->D / E-d->E-e->F-d->F-u->F / G-u->G","testBuildVerticesToMergeForPath8",config, converter);
		Collection<List<Label>> paths = new LinkedList<List<Label>>();paths.add(Arrays.asList(new Label[]{lblA}));paths.add(Arrays.asList(new Label[]{lblB}));paths.add(Arrays.asList(new Label[]{lblC}));paths.add(Arrays.asList(new Label[]{lblD}));paths.add(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("e", config, converter)}));paths.add(Arrays.asList(new Label[]{lblU}));
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifier(new MarkovModel(2,true,true),gr).buildVerticesToMergeForPaths(paths);
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
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifier(new MarkovModel(2,true,true),gr).buildVerticesToMergeForPaths(paths);
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
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifier(new MarkovModel(2,true,true),gr).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(3,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<CmpVertex>();partA.add(gr.findVertex("A"));partA.add(gr.findVertex("B"));partA.add(gr.findVertex("D"));
		Set<CmpVertex> partB = new TreeSet<CmpVertex>();partB.add(gr.findVertex("C"));partB.add(gr.findVertex("E"));partB.add(gr.findVertex("F"));
		Set<CmpVertex> partC = new TreeSet<CmpVertex>();partC.add(gr.findVertex("G"));
		
		Assert.assertEquals(partA, iterator.next());
		Assert.assertEquals(partB, iterator.next());
		Assert.assertEquals(partC, iterator.next());
	}

	/** Tests non-deterministic case of {@link MarkovClassifier#tracePath}, deterministic case is tested with {@link TestPathTracing}. */
	@Test
	public void testTracePath1()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C", "testTracePath1",config,converter);
		LearnerGraphND ndFSM = new LearnerGraphND(fsm,config);
		synchronized(AbstractLearnerGraph.syncObj)
		{
			ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("A"), true, lblA),true,lblA);
		}
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","b"}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a"}),config,converter),fsm.findVertex("A")));
	}
	
	/** Tests non-deterministic case of {@link MarkovClassifier#tracePath}, deterministic case is tested with {@link TestPathTracing}. */
	@Test
	public void testTracePath2()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C", "testTracePath1",config,converter);
		LearnerGraphND ndFSM = new LearnerGraphND(fsm,config);
		synchronized(AbstractLearnerGraph.syncObj)
		{
			CmpVertex AA=ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("A"), true, lblA),true,lblA);
			ndFSM.addVertex(AA, true, lblA);ndFSM.addVertex(AA, true, lblC);
		}
		Assert.assertEquals(7,ndFSM.getStateNumber());
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","c"}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","a"}),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","b"}),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","a","a"}),config,converter),fsm.findVertex("A")));
	}

	/** Tests non-deterministic case of {@link MarkovClassifier#tracePath}, deterministic case is tested with {@link TestPathTracing}. */
	@Test
	public void testTracePath3()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C", "testTracePath1",config,converter);
		LearnerGraphND ndFSM = new LearnerGraphND(fsm,config);
		synchronized(AbstractLearnerGraph.syncObj)
		{
			ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("A"), true, lblA),true,lblA),true,lblA),true,lblA);
			ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("A"), true, lblA),true,lblA),true,lblA);
		}
		Assert.assertEquals(10,ndFSM.getStateNumber());
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","b"}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","a"}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","a","a"}),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","a","a","a"}),config,converter),fsm.findVertex("A")));
	}

	/** Tests non-deterministic case of {@link MarkovClassifier#tracePath}, deterministic case is tested with {@link TestPathTracing}. */
	@Test
	public void testTracePath4()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		LearnerGraphND ndFSM = new LearnerGraphND(fsm,config);
		synchronized(AbstractLearnerGraph.syncObj)
		{
			ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("B"), true, lblA),true,lblA),true,lblA),true,lblA);
			ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("B"), true, lblA),true,lblA),true,lblC);
		}
		Assert.assertEquals(10,ndFSM.getStateNumber());
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","b"}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","a","a"}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","a","a","a"}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","a","c"}),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","a","a","c"}),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","a","a","a","a"}),config,converter),fsm.findVertex("A")));
	}
	
	/** Tests non-deterministic case of {@link MarkovClassifier#tracePath}, deterministic case is tested with {@link TestPathTracing}. */
	@Test
	public void testTracePath5()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		LearnerGraphND ndFSM = new LearnerGraphND(fsm,config);
		CmpVertex Brej=null;
		synchronized(AbstractLearnerGraph.syncObj)
		{
			ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("B"), true, lblA),true,lblA),true,lblA),true,lblA);
			ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("B"), true, lblA),true,lblA),false,lblC);
			Brej=ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("B"), true, lblA),false,lblB);
			ndFSM.addVertex(Brej,false,lblC);
		}
		Assert.assertEquals(13,ndFSM.getStateNumber());
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","b"}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","a","a"}),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","a","a","a"}),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","a","c"}),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","b"}),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","b","c"}),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","a","a","c"}),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t","a","a","a","a","a"}),config,converter),fsm.findVertex("A")));

		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{}),config,converter),fsm.findVertex("B")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"b"}),config,converter),fsm.findVertex("B")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","a"}),config,converter),fsm.findVertex("B")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","a","a"}),config,converter),fsm.findVertex("B")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","c"}),config,converter),fsm.findVertex("B")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","b"}),config,converter),fsm.findVertex("B")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","b","c"}),config,converter),fsm.findVertex("B")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","a","c"}),config,converter),fsm.findVertex("B")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"a","a","a","a","a"}),config,converter),fsm.findVertex("B")));

		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{}),config,converter),Brej));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"t"}),config,converter),Brej));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList(new String[]{"b"}),config,converter),Brej));
	}
	
	
	public String collectionOfTransitionsToString(Collection<Map.Entry<Label,CmpVertex>> c)
	{
		StringBuffer outcome = new StringBuffer();
		for(Map.Entry<Label,CmpVertex> entry:c)
		{
			outcome.append('{');outcome.append(entry.getKey().toString());outcome.append(',');outcome.append(entry.getValue().toString());outcome.append('}');
		}
		return outcome.toString();
	}
	
	@Test
	public void testConstructSurroundingTransitions1()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = MarkovPassivePairSelection.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("A"));
		Assert.assertEquals("{t,B}",collectionOfTransitionsToString(surroundingTransitions));
		Assert.assertEquals(1,MarkovPassivePairSelection.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("A")));
		Assert.assertEquals(2,MarkovPassivePairSelection.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("B")));
		Assert.assertEquals(1,MarkovPassivePairSelection.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("C")));
		Assert.assertEquals(fsm.findVertex("B"),MarkovPassivePairSelection.findVertexWithMostTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm)));
	}

	@Test
	public void testConstructSurroundingTransitions2()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = MarkovPassivePairSelection.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("B"));
		Assert.assertEquals("{b,C}{t,A}",collectionOfTransitionsToString(surroundingTransitions));
	}
	
	@Test
	public void testConstructSurroundingTransitions3()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = MarkovPassivePairSelection.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("C"));
		Assert.assertEquals("{b,B}",collectionOfTransitionsToString(surroundingTransitions));
	}
	
	/** Now with RED labels. */
	@Test
	public void testConstructSurroundingTransitions4()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		fsm.findVertex("B").setColour(JUConstants.RED);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = MarkovPassivePairSelection.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("C"));
		Assert.assertEquals("",collectionOfTransitionsToString(surroundingTransitions));
	}

	/** This one contains self-loops that should only be reported once. */ 
	@Test
	public void testConstructSurroundingTransitions5()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-u->B-b->C / A-a->A / A-b->B / A-c->B /T-a->B/B-a->A/B-g->T/B-c->Z/B-p->B-q->B/B-u->A", "testConstructSurroundingTransitions5",config,converter);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = MarkovPassivePairSelection.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("B"));
		Assert.assertEquals("{a,A}{b,C}{c,Z}{g,T}{p,B}{q,B}{u,A}{a,T}{b,A}{c,A}{u,A}",collectionOfTransitionsToString(surroundingTransitions));

		Assert.assertEquals(6,MarkovPassivePairSelection.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("A")));
		Assert.assertEquals(11,MarkovPassivePairSelection.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("B")));
		Assert.assertEquals(1,MarkovPassivePairSelection.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("C")));
		Assert.assertEquals(fsm.findVertex("B"),MarkovPassivePairSelection.findVertexWithMostTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm)));
	}
	
	
	/** Similar to above, but with RED states. */
	@Test
	public void testConstructSurroundingTransitions6()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-u->B-b->C / A-a->A / A-b->B / A-c->B /T-a->B/B-a->A/B-g->T/B-c->Z/B-p->B/B-u->A", "testConstructSurroundingTransitions5",config,converter);
		fsm.findVertex("B").setColour(JUConstants.RED);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = MarkovPassivePairSelection.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("B"));
		Assert.assertEquals("{a,A}{b,C}{c,Z}{g,T}{u,A}{a,T}{b,A}{c,A}{u,A}",collectionOfTransitionsToString(surroundingTransitions));

		Assert.assertEquals(6,MarkovPassivePairSelection.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("A")));
		Assert.assertEquals(10,MarkovPassivePairSelection.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("B")));
		Assert.assertEquals(1,MarkovPassivePairSelection.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("C")));
	}
	
	/** Similar to above, but with RED states. */
	@Test
	public void testConstructSurroundingTransitions7()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-u->B-b->C / A-a->A / A-b->B / A-c->B /T-a->B/B-a->A/B-g->T/B-c->Z/B-p->B/B-u->A", "testConstructSurroundingTransitions5",config,converter);
		fsm.findVertex("B").setColour(JUConstants.RED);fsm.findVertex("A").setColour(JUConstants.RED);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = MarkovPassivePairSelection.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("B"));
		Assert.assertEquals("{b,C}{c,Z}{g,T}{a,T}",collectionOfTransitionsToString(surroundingTransitions));
	}
	
	/** Similar to above, but with RED states. */
	@Test
	public void testConstructSurroundingTransitions8()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-u->B-b->C / A-a->A / A-b->B / A-c->B /T-a->B/B-a->A/B-g->T/B-c->Z/B-p->B/B-u->A", "testConstructSurroundingTransitions5",config,converter);
		fsm.findVertex("A").setColour(JUConstants.RED);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = MarkovPassivePairSelection.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("B"));
		Assert.assertEquals("{b,C}{c,Z}{g,T}{p,B}{a,T}",collectionOfTransitionsToString(surroundingTransitions));
	}
	
	@Test
	public void testConstructSurroundingTransitions9()
	{
		final LearnerGraph fsm = FsmParser.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		final Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = MarkovPassivePairSelection.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("A"));
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				surroundingTransitions.iterator().next().setValue(fsm.findVertex("A"));
			}
		}, UnsupportedOperationException.class, "changing values of this map entry is not permitted");
	}
	
	@Test
	public void testConstructSurroundingTransitions10()
	{
		final LearnerGraph fsm = new LearnerGraph(config);
		Assert.assertEquals(0,MarkovPassivePairSelection.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.getInit()));
		Assert.assertEquals(fsm.getInit(),MarkovPassivePairSelection.findVertexWithMostTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm)));
	}	
	@Test
	public void testConstructSurroundingTransitions11()
	{
		final LearnerGraph fsm = new LearnerGraph(config);fsm.initEmpty();
		Assert.assertNull(MarkovPassivePairSelection.findVertexWithMostTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm)));
	}	
}

