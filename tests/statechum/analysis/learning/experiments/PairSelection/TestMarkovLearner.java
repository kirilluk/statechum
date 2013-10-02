package statechum.analysis.learning.experiments.PairSelection;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Test;

import statechum.Configuration;
import statechum.Helper;
import statechum.Label;
import statechum.Trace;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.MarkovUniversalLearner;
import statechum.analysis.learning.MarkovUniversalLearner.UpdatablePairDouble;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
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
		Map<Trace, UpdatablePairDouble> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		int counter=0;
		for(Entry<Trace,UpdatablePairDouble> entry:matrix.entrySet())
			if (entry.getValue() != MarkovUniversalLearner.zero) ++counter;
		Assert.assertEquals(11,counter);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.2, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(1, matrix.get(new Trace(Arrays.asList(new Label[]{lblD,lblC}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblD,lblC}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0.6666666666666666, matrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblC}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblB,lblC}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0.6, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblB}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblD}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.3333333333333333, matrix.get(new Trace(Arrays.asList(new Label[]{lblC,lblD}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0.2, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblD}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblD}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0.35714285714285715, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))).secondElem,Configuration.fpAccuracy);
		
		Assert.assertEquals(0.21428571428571427, matrix.get(new Trace(Arrays.asList(new Label[]{lblB}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblB}))).secondElem,Configuration.fpAccuracy);
		
		Assert.assertEquals(0.21428571428571427, matrix.get(new Trace(Arrays.asList(new Label[]{lblC}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblC}))).secondElem,Configuration.fpAccuracy);
		
		Assert.assertEquals(0.07142857142857142, matrix.get(new Trace(Arrays.asList(new Label[]{lblD}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.07142857142857142, matrix.get(new Trace(Arrays.asList(new Label[]{lblD}))).secondElem,Configuration.fpAccuracy);
		
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.07142857142857142, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).secondElem,Configuration.fpAccuracy);
	}	

	@Test
	public void testCreateMarkovMatrix2()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter), minusStrings = new HashSet<List<Label>>();
		Map<Trace, UpdatablePairDouble> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		int counter=0;
		for(Entry<Trace,UpdatablePairDouble> entry:matrix.entrySet())
			if (entry.getValue() != MarkovUniversalLearner.zero) ++counter;
		Assert.assertEquals(3,counter);
		
		Assert.assertEquals(1, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0.5, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0.5, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).secondElem,Configuration.fpAccuracy);
	}
	
	@Test
	public void testCreateMarkovMatrix3a()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		Map<Trace, UpdatablePairDouble> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		int counter=0;
		for(Entry<Trace,UpdatablePairDouble> entry:matrix.entrySet())
			if (entry.getValue() != MarkovUniversalLearner.zero) ++counter;
		Assert.assertEquals(3,counter);

		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(1, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0.5, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.5, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).secondElem,Configuration.fpAccuracy);
	}
	
	/** Only differs from 3a by the use of chunk_length of 3. The outcome should be the same. */
	@Test
	public void testCreateMarkovMatrix3b()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(3);
		Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		Map<Trace, UpdatablePairDouble> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		int counter=0;
		for(Entry<Trace,UpdatablePairDouble> entry:matrix.entrySet())
			if (entry.getValue() != MarkovUniversalLearner.zero) ++counter;
		Assert.assertEquals(3,counter);

		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(1, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0.5, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.5, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).secondElem,Configuration.fpAccuracy);
	}
	
	@Test
	public void testCreateMarkovMatrix4()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{"u"} },config,converter);
		Map<Trace, UpdatablePairDouble> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		int counter=0;
		for(Entry<Trace,UpdatablePairDouble> entry:matrix.entrySet())
			if (entry.getValue() != MarkovUniversalLearner.zero) ++counter;
		Assert.assertEquals(1,counter);

		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(1, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).secondElem,Configuration.fpAccuracy);
	}
	
	/** Same as testCreateMarkovMatrix3 but contains an empty trace which is ignored since it does not match any of the valid chunk sizes. */
	@Test
	public void testCreateMarkovMatrix5()
	{
		final MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		final Set<List<Label>> plusStrings = new HashSet<List<Label>>(), minusStrings = buildSet(new String[][] { new String[]{},new String[]{"a","u"} },config,converter);
		Map<Trace, UpdatablePairDouble> matrix = m.createMarkovLearner(plusStrings, minusStrings);
		int counter=0;
		for(Entry<Trace,UpdatablePairDouble> entry:matrix.entrySet())
			if (entry.getValue() != MarkovUniversalLearner.zero) ++counter;
		Assert.assertEquals(3,counter);
		
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(1, matrix.get(new Trace(Arrays.asList(new Label[]{lblA,lblU}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0.5, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblA}))).secondElem,Configuration.fpAccuracy);

		Assert.assertEquals(0, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.5, matrix.get(new Trace(Arrays.asList(new Label[]{lblU}))).secondElem,Configuration.fpAccuracy);
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
		Map<CmpVertex, Map<Label, UpdatablePairDouble>> newTransitions = m.constructMarkovTentative(graph,0.1,0);
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
		Map<CmpVertex, Map<Label, UpdatablePairDouble>> newTransitions = m.constructMarkovTentative(graph,0.1,0);
		Assert.assertTrue(newTransitions.isEmpty());// not enough evidence to update, hence nothing should be recorded.
		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B","testConstructExtendedGraph2",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	/** Nothing to add because thresholds are zero. */
	@Test
	public void testConstructExtendedGraph3()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3",config, converter);
		Map<CmpVertex, Map<Label, UpdatablePairDouble>> newTransitions = m.constructMarkovTentative(graph,1,0);
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertEquals(0, newTransitions.get(graph.findVertex("B")).get(lblU).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.5, newTransitions.get(graph.findVertex("B")).get(lblU).secondElem,Configuration.fpAccuracy);
		
		Assert.assertEquals(0.5, newTransitions.get(graph.findVertex("B")).get(lblB).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, newTransitions.get(graph.findVertex("B")).get(lblB).secondElem,Configuration.fpAccuracy);

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	/** A bit has been added. */
	@Test
	public void testConstructExtendedGraph4()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3",config, converter);
		Map<CmpVertex, Map<Label, UpdatablePairDouble>> newTransitions = m.constructMarkovTentative(graph,0.1,0);
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertEquals(0, newTransitions.get(graph.findVertex("B")).get(lblU).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.5, newTransitions.get(graph.findVertex("B")).get(lblU).secondElem,Configuration.fpAccuracy);
		
		Assert.assertEquals(0.5, newTransitions.get(graph.findVertex("B")).get(lblB).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, newTransitions.get(graph.findVertex("B")).get(lblB).secondElem,Configuration.fpAccuracy);

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-b->C / B-u-#D / T-b->T-u->T","testConstructExtendedGraph3",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	/** A bit has been added, but reject-transition was not because is present both in the positive and negative light in the initial traces. */
	@Test
	public void testConstructExtendedGraph5()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"a","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3",config, converter);
		Map<CmpVertex, Map<Label, UpdatablePairDouble>> newTransitions = m.constructMarkovTentative(graph,0.1,0);
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertEquals(0.333333333333, newTransitions.get(graph.findVertex("B")).get(lblU).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.333333333333, newTransitions.get(graph.findVertex("B")).get(lblU).secondElem,Configuration.fpAccuracy);
		
		Assert.assertEquals(0.333333333333, newTransitions.get(graph.findVertex("B")).get(lblB).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, newTransitions.get(graph.findVertex("B")).get(lblB).secondElem,Configuration.fpAccuracy);

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-b->C / T-b->T-u->T","testConstructExtendedGraph3",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	
	/** A bit has been added, but reject-transition was not because is present both in the positive and negative light in the initial traces. */
	@Test
	public void testConstructExtendedGraph6()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);// w below is to ensure that all elements of the alphabet are included in traces.
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"},new String[]{"w"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-w->M-c->B / T-b->T-u->T","testConstructExtendedGraph3",config, converter);// the purpose of the w-transition is to ensure transition c is taken into account in graph comparison
		Map<CmpVertex, Map<Label, UpdatablePairDouble>> newTransitions = m.constructMarkovTentative(graph,0.1,0);
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertEquals(1, newTransitions.get(graph.findVertex("B")).get(lblU).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.5, newTransitions.get(graph.findVertex("B")).get(lblU).secondElem,Configuration.fpAccuracy);
		
		Assert.assertEquals(0.5, newTransitions.get(graph.findVertex("B")).get(lblB).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, newTransitions.get(graph.findVertex("B")).get(lblB).secondElem,Configuration.fpAccuracy);

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-b->C / A-w->M-c->B / T-b->T-u->T","testConstructExtendedGraph3",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	/** A bit has been added. u has been seen both in the positive and negative light and thresholds permit it to be added in the positive way. */
	@Test
	public void testConstructExtendedGraph7()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / T-b->T-u->T","testConstructExtendedGraph3",config, converter);
		Map<CmpVertex, Map<Label, UpdatablePairDouble>> newTransitions = m.constructMarkovTentative(graph,0.1,1);
		
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertEquals(1, newTransitions.get(graph.findVertex("B")).get(lblU).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.5, newTransitions.get(graph.findVertex("B")).get(lblU).secondElem,Configuration.fpAccuracy);
		
		Assert.assertEquals(0.5, newTransitions.get(graph.findVertex("B")).get(lblB).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, newTransitions.get(graph.findVertex("B")).get(lblB).secondElem,Configuration.fpAccuracy);

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-b->C / B-u->D / T-b->T-u->T","testConstructExtendedGraph3",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
	
	/** A bit has been added. u has been seen both in the positive and negative light and thresholds do not permit it to be added. */
	@Test
	public void testConstructExtendedGraph8()
	{
		MarkovUniversalLearner m = new MarkovUniversalLearner(2);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings);
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / T-b->T-u->T","testConstructExtendedGraph3",config, converter);
		Map<CmpVertex, Map<Label, UpdatablePairDouble>> newTransitions = m.constructMarkovTentative(graph,0.1,0);
		
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertEquals(1, newTransitions.get(graph.findVertex("B")).get(lblU).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0.5, newTransitions.get(graph.findVertex("B")).get(lblU).secondElem,Configuration.fpAccuracy);
		
		Assert.assertEquals(0.5, newTransitions.get(graph.findVertex("B")).get(lblB).firstElem,Configuration.fpAccuracy);
		Assert.assertEquals(0, newTransitions.get(graph.findVertex("B")).get(lblB).secondElem,Configuration.fpAccuracy);

		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B / A-c->B / B-b->C / T-b->T-u->T","testConstructExtendedGraph3",config, converter);
		DifferentFSMException ex = WMethod.checkM(expected, m.get_extension_model());
		if (ex != null)
			throw ex;
		Assert.assertNotSame(graph, m.get_extension_model());
	}
}
