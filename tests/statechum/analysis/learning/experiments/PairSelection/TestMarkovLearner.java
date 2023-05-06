package statechum.analysis.learning.experiments.PairSelection;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized.Parameters;
import junit_runners.ParameterizedWithName;
import junit_runners.ParameterizedWithName.ParametersToString;
import statechum.*;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovClassifierLG;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.MarkovModel.MarkovOutcome;
import statechum.analysis.learning.MarkovModel.UpdatablePairInteger;
import statechum.analysis.learning.experiments.MarkovEDSM.WaveBlueFringe;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.MarkovModel.MarkovOutcome;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.FsmParserStatechum;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;

@RunWith(ParameterizedWithName.class)
public class TestMarkovLearner 
{
	final Configuration config = Configuration.getDefaultConfiguration().copy();
	final ConvertALabel converter = new ConvertALabel() {
		final Map<Label,StringLabel> map = new TreeMap<>();
		
		@Override
		public Label convertLabelToLabel(final Label lbl) {
			StringLabel outcome = map.get(lbl);
			if (outcome == null)
			{
				outcome = new StringLabel(lbl.toErlangTerm()){

					@Override
					public int compareTo(Label o) {
						return super.compareTo(o);
					}

					@Override
					public int toInt() {
						String text=toErlangTerm();
						if (text.length() != 1)
							throw new IllegalArgumentException("label "+text+" should have a length of 1");
						return text.codePointAt(0);
					}

					@Override
					public String toErlangTerm() {
						return super.toErlangTerm();
					}};
				
				map.put(lbl, outcome);
			}
			return outcome;
		}
	};

	final Label lblA,lblB,lblC,lblD,lblU;

	LearnerGraph trainingGraphForClosures = null;
	final boolean markovPTAUseMatrix;
	
	public TestMarkovLearner(boolean useMatrix) {
		markovPTAUseMatrix = useMatrix;
		lblA = AbstractLearnerGraph.generateNewLabel("a", config, converter);
		lblB = AbstractLearnerGraph.generateNewLabel("b", config, converter);
		lblC = AbstractLearnerGraph.generateNewLabel("c", config, converter);
		lblD = AbstractLearnerGraph.generateNewLabel("d", config, converter);
		lblU = AbstractLearnerGraph.generateNewLabel("u", config, converter);
	}
	
		
	@Parameters
	public static Collection<Object[]> data() 
	{
		return Arrays.asList(new Object[]{true},new Object[]{false});
	}
	/** Given a test configuration, returns a textual description of its purpose. 
	 * 
	 * @return description.
	 */ 
	@ParametersToString
	public static String parametersToString(Boolean value)
	{
		return value ?"using matrix":"no matrix";
	}
	
	@Before
	public void beforeTest()
	{
		trainingGraphForClosures = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-b->E / C-c->C / B-a->C / D-a->E","testComputeClosure",
				config, converter);
	}
	
	@Test
	public void testGetChunks1() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Collections.emptyList(),true),1);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks2a() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Arrays.asList(lblA,lblB,lblC),true),4);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks2b() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Arrays.asList(lblA,lblB,lblC),true),10);
		Assert.assertTrue(l.isEmpty());
	}

	@Test
	public void testGetChunks3() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Arrays.asList(lblA,lblB,lblC),true),1);
		Assert.assertEquals(3,l.size());
		Assert.assertEquals(new Trace(Collections.singletonList(lblA),true),l.get(0));
		Assert.assertEquals(new Trace(Collections.singletonList(lblB),true),l.get(1));
		Assert.assertEquals(new Trace(Collections.singletonList(lblC),true),l.get(2));
	}

	@Test
	public void testGetChunks4() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Arrays.asList(lblA,lblB,lblC),true),2);
		Assert.assertEquals(2,l.size());
		Assert.assertEquals(new Trace(Arrays.asList(lblA,lblB),true),l.get(0));
		Assert.assertEquals(new Trace(Arrays.asList(lblB,lblC),true),l.get(1));
	}

	@Test
	public void testGetChunks5() {
		List<Trace> l = MarkovModel.splitTrace(new Trace(Arrays.asList(lblA,lblB,lblC),true),3);
		Assert.assertEquals(1,l.size());
		Assert.assertEquals(new Trace(Arrays.asList(lblA,lblB,lblC),true),l.get(0));
	}

	
	@Test
	public void testCreateMarkovMatrixFail()
	{
		TestHelper.checkForCorrectException(() ->
				new MarkovModel(1,true, true,true,markovPTAUseMatrix), IllegalArgumentException.class, "chunkLen");
	}
	
	@Test
	public void testCreateMarkovMatrix1()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","b","c","d"}, new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		Map<List<Label>, MarkovOutcome> matrix = m.computePredictionMatrix();
		Assert.assertEquals(11,matrix.size());
		
		Assert.assertSame(MarkovOutcome.negative, matrix.get(Arrays.asList(lblA,lblU)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Arrays.asList(lblD,lblC)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Arrays.asList(lblB,lblC)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Arrays.asList(lblA,lblB)));
		Assert.assertSame(MarkovOutcome.negative, matrix.get(Arrays.asList(lblC,lblD)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Arrays.asList(lblA,lblD)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Collections.singletonList(lblA)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Collections.singletonList(lblB)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Collections.singletonList(lblC)));
		Assert.assertSame(MarkovOutcome.failure, matrix.get(Collections.singletonList(lblD)));
		Assert.assertSame(MarkovOutcome.negative, matrix.get(Collections.singletonList(lblU)));
	}	

	@Test
	public void testCreateMarkovMatrix2()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter), minusStrings = new HashSet<>();
		m.createMarkovLearner(plusStrings, minusStrings,false);
		Map<List<Label>, MarkovOutcome> matrix = m.computePredictionMatrix();
		Assert.assertEquals(3,matrix.size());
		
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Arrays.asList(lblA,lblU)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Collections.singletonList(lblA)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Collections.singletonList(lblU)));
	}
	
	@Test
	public void testCreateMarkovMatrix3a()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = new HashSet<>(), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		Map<List<Label>, MarkovOutcome> matrix = m.computePredictionMatrix();
		Assert.assertEquals(3,matrix.size());

		Assert.assertSame(MarkovOutcome.negative, matrix.get(Arrays.asList(lblA,lblU)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Collections.singletonList(lblA)));
		Assert.assertSame(MarkovOutcome.negative, matrix.get(Collections.singletonList(lblU)));
	}
	
	/** Only differs from 3a by the use of chunk_length of 3. The outcome should be the same. */
	@Test
	public void testCreateMarkovMatrix3b()
	{
		MarkovModel m = new MarkovModel(3,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = new HashSet<>(), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		Map<List<Label>, MarkovOutcome> matrix = m.computePredictionMatrix();
		Assert.assertEquals(3,matrix.size());

		Assert.assertSame(MarkovOutcome.negative, matrix.get(Arrays.asList(lblA,lblU)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Collections.singletonList(lblA)));
		Assert.assertSame(MarkovOutcome.negative, matrix.get(Collections.singletonList(lblU)));
	}
	
	@Test
	public void testCreateMarkovMatrix4()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = new HashSet<>(), minusStrings = buildSet(new String[][] { new String[]{"u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		Map<List<Label>, MarkovOutcome> matrix = m.computePredictionMatrix();
		Assert.assertEquals(1,matrix.size());

		Assert.assertSame(MarkovOutcome.negative, matrix.get(Collections.singletonList(lblU)));
	}
	
	/** Same as testCreateMarkovMatrix3 but contains an empty trace which is ignored since it does not match any of the valid chunk sizes. */
	@Test
	public void testCreateMarkovMatrix5()
	{
		final MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		final Set<List<Label>> plusStrings = new HashSet<>(), minusStrings = buildSet(new String[][] { new String[]{},new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		Map<List<Label>, MarkovOutcome> matrix = m.computePredictionMatrix();
		Assert.assertEquals(3,matrix.size());
		
		Assert.assertSame(MarkovOutcome.negative, matrix.get(Arrays.asList(lblA,lblU)));
		Assert.assertSame(MarkovOutcome.positive, matrix.get(Collections.singletonList(lblA)));
		Assert.assertSame(MarkovOutcome.negative, matrix.get(Collections.singletonList(lblU)));
	}
	
	@Test
	public void testCreateMarkovMatrix6()
	{
		final MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		final Set<List<Label>> plusStrings = new HashSet<>(), minusStrings = new HashSet<>();
		TestHelper.checkForCorrectException(
				() -> m.createMarkovLearner(plusStrings, minusStrings,false),
				IllegalArgumentException.class, "empty");
	}
	
	@Test
	public void testCreateMarkovMatrix7()
	{
		final MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		final Set<List<Label>> plusStrings = new HashSet<>(), minusStrings = buildSet(new String[][] { new String[]{},new String[]{} },config,converter);
		TestHelper.checkForCorrectException(
				() -> m.createMarkovLearner(plusStrings, minusStrings,false),
				IllegalArgumentException.class, "empty");
	}
	
	/** Nothing to add because there not enough evidence. */
	@Test
	public void testConstructExtendedGraph1()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","p"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-u->B-p->B","testConstructExtendedGraph1",config, converter);
		MarkovClassifierLG cl = new MarkovClassifierLG(m,graph,null);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		Assert.assertTrue(newTransitions.isEmpty());// not enough evidence to update, hence nothing should be recorded.
		final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-u->B-p->B","testConstructExtendedGraph1",config, converter);
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
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","p"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B","testConstructExtendedGraph2",config, converter);
		MarkovClassifierLG cl = new MarkovClassifierLG(m,graph,null);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		Assert.assertTrue(newTransitions.isEmpty());// not enough evidence to update, hence nothing should be recorded.
		final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-a->B","testConstructExtendedGraph2",config, converter);
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
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3a",config, converter);
		MarkovClassifierLG cl = new MarkovClassifierLG(m,graph,null);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertSame(MarkovOutcome.negative, newTransitions.get(graph.findVertex("B")).get(lblU));
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / T-b->T-u->T","testConstructExtendedGraph3b",config, converter);
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
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"a","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / T-b->T-u->T","testConstructExtendedGraph3a",config, converter);
		MarkovClassifierLG cl = new MarkovClassifierLG(m,graph,null);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		Assert.assertEquals(1,newTransitions.size());// not enough evidence to update, hence nothing should be recorded.

		Assert.assertFalse(newTransitions.get(graph.findVertex("B")).containsKey(lblU));// failure ignored
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / T-b->T-u->T","testConstructExtendedGraph4b",config, converter);
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
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);// w below is to ensure that all elements of the alphabet are included in traces.
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"},new String[]{"w"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-w->M-c->B / T-b->T-u->T","testConstructExtendedGraph5a",config, converter);// the purpose of the w-transition is to ensure transition c is taken into account in graph comparison
		MarkovClassifierLG cl = new MarkovClassifierLG(m,graph,null);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		Assert.assertEquals(1,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		
		Assert.assertSame(MarkovOutcome.positive,newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / A-w->M-c->B / T-b->T-u->T","testConstructExtendedGraph5b",config, converter);
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
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B / T-b->T-u->T","testConstructExtendedGraph6a",config, converter);
		MarkovClassifierLG cl = new MarkovClassifierLG(m,graph,null);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		
		Assert.assertEquals(1,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));

		final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B / B-b->C / T-b->T-u->T","testConstructExtendedGraph6b",config, converter);
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
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B-c->Z / T-b->T-u->T","testConstructExtendedGraph7a",config, converter);
		MarkovClassifierLG cl = new MarkovClassifierLG(m,graph,null);
		Map<CmpVertex, Map<Label, MarkovOutcome>> newTransitions = cl.predictTransitions();
		
		Assert.assertEquals(2,newTransitions.size());

		Assert.assertEquals(1,newTransitions.get(graph.findVertex("B")).size());
		Assert.assertEquals(1,newTransitions.get(graph.findVertex("Z")).size());
		
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("B")).get(lblB));
		Assert.assertSame(MarkovOutcome.positive, newTransitions.get(graph.findVertex("Z")).get(lblU));

		final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B-c->Z-u->Y / B-b->C / T-b->T-u->T","testConstructExtendedGraph7b",config, converter);
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
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B / T-b->T-u->T","testCheckFanoutInconsistency1a",config, converter);
		
		Assert.assertEquals(0,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** One from B with inconsistent predictions. */
	@Test
	public void testCheckFanoutInconsistency1b1()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency1b1",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** One from B where Markov cannot make up its mind for au+ v.s. au- and c-transition is ignored. Label b from A is not reported by the checker as something to check and hence is ignored. */
	@Test
	public void testCheckFanoutInconsistency1b2()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"a","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency1b2",config, converter);
		
		Assert.assertEquals(0,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** One from B where Markov cannot make up its mind for au+ v.s. au- and c-transition is ignored. Label b from A IS reported by the checker as something to check and hence is causes an inconsistency. */
	@Test
	public void testCheckFanoutInconsistency1b3()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"a","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency1b2",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()
		{

			/**
			 * @see statechum.analysis.learning.MarkovClassifier.DifferentPredictionsInconsistency#obtainAlphabet(statechum.analysis.learning.rpnicore.AbstractLearnerGraph, statechum.DeterministicDirectedSparseGraph.CmpVertex)
			 */
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
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"},new String[]{"a","d"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B / B-d->F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1c",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** Transition d exists as positive but should be absent according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1d()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"}},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B / B-d->F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1d",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** Transition b exists as negative but should be present as positive according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1e()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"}},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B / B-b-#F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1e",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** Transition d exists as negative but should be absent according to Markov. */
	@Test
	public void testCheckFanoutInconsistency1f()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B / B-d-#F / T-b->T-u->T-d->T","testCheckFanoutInconsistency1f",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
		Assert.assertEquals(4.,MarkovClassifier.computeInconsistency(graph,  null, m, new MarkovClassifier.DifferentPredictionsInconsistency(), false),Configuration.fpAccuracy);// inconsistencies detected are mostly due to state T
	}
	
	/** Two inconsistencies, transition u and transition b which should not exist after c. */
	@Test
	public void testCheckFanoutInconsistency2()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B-b->C / B-u->F / T-b->T-u->T","testCheckFanoutInconsistency2",config, converter);
		
		Assert.assertEquals(2,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** One inconsistency: transition u. */
	@Test
	public void testCheckFanoutInconsistency3()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->B-u->C / T-b->T-u->T","testCheckFanoutInconsistency3",config, converter);
		
		Assert.assertEquals(1,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));
	}
	
	/** No inconsistencies since there are very few paths. */
	@Test
	public void testCheckFanoutInconsistency4()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / T-b->T-u->T","testCheckFanoutInconsistency4",config, converter);
		
		
		Assert.assertEquals(0,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("B"),new MarkovClassifier.DifferentPredictionsInconsistency()));// everything as expected.
		Assert.assertEquals(0,new MarkovClassifierLG(m,graph,null).checkFanoutInconsistency(graph.findVertex("D"),new MarkovClassifier.DifferentPredictionsInconsistency()));// missing reject-transition with label u is ignored because we are only considering actual outgoing transitions
	}

	/** Tests that creating a model from PTA and from initial traces gives the same result. */
	@Test
	public void testMarkovUpdate1_longest()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovModel mOther = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		new MarkovClassifierLG(mOther,graph,null).updateMarkov(true);
		Assert.assertEquals(m.computePredictionMatrix(),mOther.computePredictionMatrix());
		Assert.assertEquals(m.computeOccurrenceMatrix(),mOther.computeOccurrenceMatrix());
	}
		
	/** Tests that creating a model from PTA and from initial traces gives almost the same result. The difference is in PTA-based construction mis-counting the number of times shorter traces occur since it
	 * can see that they exist but not the number of tails they lead to. This is left in because I do not use specific values occurrence counts. 
	 */
	@Test
	public void testMarkovUpdate1_prefixclosed()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,false);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovModel mOther = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		new MarkovClassifierLG(mOther,graph,null).updateMarkov(false);
		Assert.assertEquals(m.computePredictionMatrix(),mOther.computePredictionMatrix());
		
		// Workaround around a deficiency in the calculation of occurrences of prefixes by the PTA-based construction of Markov model.
		Assert.assertEquals(new UpdatablePairInteger(2, 0), m.computeOccurrenceMatrix().get(Collections.singletonList(lblA)));
		Assert.assertEquals(new UpdatablePairInteger(1, 0), mOther.computeOccurrenceMatrix().get(Collections.singletonList(lblA)));

		Map<List<Label>,UpdatablePairInteger> mOccurrenceMatrix = m.computeOccurrenceMatrix(), mOtherOccurrenceMatrix = mOther.computeOccurrenceMatrix();
		mOccurrenceMatrix.remove(Collections.singletonList(lblA));mOtherOccurrenceMatrix.remove(Collections.singletonList(lblA));
		Assert.assertEquals(mOccurrenceMatrix,mOtherOccurrenceMatrix);
	}

	/** Tests that creating a model from PTA and from initial traces give the same result. */
	@Test
	public void testMarkovUpdate2()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"},new String[]{"c","b"},new String[]{"c","u"} },config,converter), minusStrings = buildSet(new String[][] {},config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovModel mOther = new MarkovModel(2,true, true,true,markovPTAUseMatrix);new MarkovClassifierLG(mOther,graph,null).updateMarkov(true);
		Assert.assertEquals(m.computePredictionMatrix(),mOther.computePredictionMatrix());
		Assert.assertEquals(m.computeOccurrenceMatrix(),mOther.computeOccurrenceMatrix());
	}
	
	/** Tests that creating a model from PTA and from initial traces give the same result. */
	@Test
	public void testMarkovUpdate3()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] {},config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovModel mOther = new MarkovModel(2,true, true,true,markovPTAUseMatrix);new MarkovClassifierLG(mOther,graph,null).updateMarkov(true);
		Assert.assertEquals(m.computePredictionMatrix(),mOther.computePredictionMatrix());
		Assert.assertEquals(m.computeOccurrenceMatrix(),mOther.computeOccurrenceMatrix());
	}

	/** Tests that creating a model from PTA and from initial traces give the same result. */
	@Test
	public void testMarkovUpdate4()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[]{"a","b"} },config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","u"} },config,converter);
		m.createMarkovLearner(plusStrings, minusStrings,true);

		final LearnerGraph graph = new LearnerGraph(config);graph.paths.augmentPTA(plusStrings, true, false);graph.paths.augmentPTA(minusStrings, false, false);
		MarkovModel mOther = new MarkovModel(2,true, true,true,markovPTAUseMatrix);new MarkovClassifierLG(mOther,graph,null).updateMarkov(true);
		Assert.assertEquals(m.computePredictionMatrix(),mOther.computePredictionMatrix());
		Assert.assertEquals(m.computeOccurrenceMatrix(),mOther.computeOccurrenceMatrix());
	}
	
	@Test
	/* Tests for exploration of paths. */
	public void testExplorePaths1()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C / B-b->C","testUpdateMarkovSideways1",config, converter);
		final Set<List<Label>> pathsExpected = buildSet(new String[][] { new String[]{"a"},new String[]{"b"} },config,converter), pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sequences(graph,graph.findVertex(VertexID.parseID("B")),1, pathsActual::add
		);
		Assert.assertEquals(pathsExpected,pathsActual);
	}
	
	@Test
	/* Tests for exploration of paths. */
	public void testExplorePaths2()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C / B-b->C","testUpdateMarkovSideways1",config, converter);
		LearnerGraphND inverseGraph = MarkovClassifier.computeInverseGraph(graph);
		final Set<List<Label>> pathsExpected = buildSet(new String[][] { new String[]{"a"} },config,converter), pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sequences(inverseGraph,inverseGraph.findVertex(VertexID.parseID("B")),1, pathsActual::add
		);
		Assert.assertEquals(pathsExpected,pathsActual);
	}
	
	@Test
	/* Tests for exploration of paths. */
	public void testExplorePaths3a()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C / B-b->C","testUpdateMarkovSideways1",config, converter);
		LearnerGraphND inverseGraph = MarkovClassifier.computeInverseGraph(graph);
		final Set<List<Label>> pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sequences(inverseGraph,inverseGraph.findVertex(VertexID.parseID("B")),2, pathsActual::add
		);
		Assert.assertTrue(pathsActual.isEmpty());
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExplorePaths3b()
	{
		final LearnerGraph graph = new LearnerGraph(config);
		final Set<List<Label>> pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sequences(graph,graph.getInit(),2, pathsActual::add
		);
		Assert.assertTrue(pathsActual.isEmpty());
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExplorePaths3c()
	{
		final LearnerGraph graph = new LearnerGraph(config);
		final Set<List<Label>> pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sequences(graph,graph.getInit(),1, pathsActual::add
		);
		Assert.assertTrue(pathsActual.isEmpty());
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExplorePaths3d()
	{
		final LearnerGraph graph = new LearnerGraph(config);
		final Set<List<Label>> pathsExpected = buildSet(new String[][] { new String[]{} },config,converter), pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sequences(graph,graph.getInit(),0, pathsActual::add
		);
		Assert.assertEquals(pathsExpected,pathsActual);
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExplorePaths3e()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C / B-b->C","testUpdateMarkovSideways1",config, converter);
		final Set<List<Label>> pathsExpected = buildSet(new String[][] { new String[]{} },config,converter), pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sequences(graph,graph.getInit(),0, pathsActual::add
		);
		Assert.assertEquals(pathsExpected,pathsActual);
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExplorePaths4()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C / B-b->C / D-a->E-a->C","testExplorePaths4",config, converter);
		LearnerGraphND inverseGraph = MarkovClassifier.computeInverseGraph(graph);
		final Set<List<Label>> pathsExpected = buildSet(new String[][] { new String[]{"a","a"}, new String[]{"b","a"} },config,converter), pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sequences(inverseGraph,inverseGraph.findVertex(VertexID.parseID("C")),2, pathsActual::add
		);
		Assert.assertEquals(pathsExpected,pathsActual);
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExplorePaths5()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->A-b->C / D-a->E-a->C","testExplorePaths4",config, converter);
		LearnerGraphND inverseGraph = MarkovClassifier.computeInverseGraph(graph);
		final Set<List<Label>> pathsExpected = buildSet(new String[][] { new String[]{"b","a","a"} },config,converter), pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sequences(inverseGraph,inverseGraph.findVertex(VertexID.parseID("C")),3, pathsActual::add
		);
		Assert.assertEquals(pathsExpected,pathsActual);
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExploreSets1()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->A-b->C / D-a->E-a->C","testExplorePaths4",config, converter);
		LearnerGraphND inverseGraph = MarkovClassifier.computeInverseGraph(graph);
		final Set<List<Label>> pathsExpected = buildSet(new String[][] { new String[]{"a"}, new String[]{"b"} },config,converter), pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sets(inverseGraph,inverseGraph.findVertex(VertexID.parseID("C")),1, pathsActual::add
		);
		Assert.assertEquals(pathsExpected,pathsActual);
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExploreSets2()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->A-b->C / D-a->E-a->C","testExplorePaths4",config, converter);
		LearnerGraphND inverseGraph = MarkovClassifier.computeInverseGraph(graph);
		final Set<List<Label>> pathsExpected = buildSet(new String[][] { new String[]{"a","b"} },config,converter), pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sets(inverseGraph,inverseGraph.findVertex(VertexID.parseID("C")),2, pathsActual::add
		);
		Assert.assertEquals(pathsExpected,pathsActual);
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExploreSets3()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->A-b->C / D-a->E-a->C","testExplorePaths4",config, converter);
		LearnerGraphND inverseGraph = MarkovClassifier.computeInverseGraph(graph);
		final Set<List<Label>> pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sets(inverseGraph,inverseGraph.findVertex(VertexID.parseID("C")),3, pathsActual::add
		);
		Assert.assertTrue(pathsActual.isEmpty());
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExploreSets4()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->A-b->C / D-a->E-a->C","testExplorePaths4",config, converter);
		LearnerGraphND inverseGraph = MarkovClassifier.computeInverseGraph(graph);
		final Set<List<Label>> pathsExpected = buildSet(new String[][] { new String[]{} },config,converter), pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sets(inverseGraph,inverseGraph.findVertex(VertexID.parseID("C")),0, pathsActual::add
		);
		Assert.assertEquals(pathsExpected,pathsActual);
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExploreSets5()
	{
		final LearnerGraph graph = new LearnerGraph(config);
		final Set<List<Label>> pathsExpected = buildSet(new String[][] { new String[]{} },config,converter), pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sets(graph,graph.getInit(),0, pathsActual::add
		);
		Assert.assertEquals(pathsExpected,pathsActual);
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExploreSets6()
	{
		final LearnerGraph graph = new LearnerGraph(config);
		final Set<List<Label>> pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sets(graph,graph.getInit(),1, pathsActual::add
		);
		Assert.assertTrue(pathsActual.isEmpty());
	}

	@Test
	/* Tests for exploration of paths. */
	public void testExploreSets7()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->A-b->C / D-a->E-a->C / E-b->C / E-c->C","testExploreSets7",config, converter);
		LearnerGraphND inverseGraph = MarkovClassifier.computeInverseGraph(graph);
		final Set<List<Label>> pathsExpected = buildSet(new String[][] { new String[]{"a","b"},new String[]{"a","c"},new String[]{"b","c"} },config,converter), pathsActual = new HashSet<>();
		MarkovClassifier.WalkThroughAllPathsOfSpecificLength_Sets(inverseGraph,inverseGraph.findVertex(VertexID.parseID("C")),2, pathsActual::add);
		Assert.assertEquals(pathsExpected,pathsActual);
	}

	@Test
	public void testUpdateMarkovSideways1a()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C / B-b->C","testUpdateMarkovSideways1",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Map<List<Label>,UpdatablePairInteger> mOccurrenceMatrix = m.computeOccurrenceMatrix();Map<List<Label>,MarkovOutcome> mPredictionsMatrix = m.computePredictionMatrix();
		Assert.assertEquals(4,mPredictionsMatrix.size());
		Assert.assertEquals(4,mOccurrenceMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblA)));Assert.assertEquals(new UpdatablePairInteger(2, 0),mOccurrenceMatrix.get(Arrays.asList(lblA,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblB)));Assert.assertEquals(new UpdatablePairInteger(1, 0),mOccurrenceMatrix.get(Arrays.asList(lblA,lblB)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblB,lblA)));Assert.assertEquals(new UpdatablePairInteger(1, 0),mOccurrenceMatrix.get(Arrays.asList(lblB,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblB,lblB)));Assert.assertEquals(new UpdatablePairInteger(1, 0),mOccurrenceMatrix.get(Arrays.asList(lblB,lblB)));
	}
	
	@Test
	public void testUpdateMarkovSideways1b()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C / B-b->C","testUpdateMarkovSideways1",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Map<List<Label>,UpdatablePairInteger> mOccurrenceMatrix = m.computeOccurrenceMatrix();Map<List<Label>,MarkovOutcome> mPredictionsMatrix = m.computePredictionMatrix();
		Assert.assertEquals(6,mPredictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblA)));Assert.assertEquals(new UpdatablePairInteger(2, 0),mOccurrenceMatrix.get(Arrays.asList(lblA,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblB)));Assert.assertEquals(new UpdatablePairInteger(1, 0),mOccurrenceMatrix.get(Arrays.asList(lblA,lblB)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblB,lblA)));Assert.assertEquals(new UpdatablePairInteger(1, 0),mOccurrenceMatrix.get(Arrays.asList(lblB,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblB,lblB)));Assert.assertEquals(new UpdatablePairInteger(1, 0),mOccurrenceMatrix.get(Arrays.asList(lblB,lblB)));

		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Collections.singletonList(lblA)));Assert.assertEquals(new UpdatablePairInteger(2, 0),mOccurrenceMatrix.get(Collections.singletonList(lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Collections.singletonList(lblB)));Assert.assertEquals(new UpdatablePairInteger(1, 0),mOccurrenceMatrix.get(Collections.singletonList(lblB)));
	}
	
	/** This one is similar to the {@link #testUpdateMarkovSideways1b}, except that there are a few additional negative transitions. */
	@Test
	public void testUpdateMarkovSideways1c()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C / B-b->C-a-#D / B-c-#D","testUpdateMarkovSideways1c",config, converter);

		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Map<List<Label>,MarkovOutcome> mPredictionsMatrix = m.computePredictionMatrix();
		Assert.assertEquals(9,mPredictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblB)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblB,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblB,lblB)));
		Assert.assertEquals(MarkovOutcome.negative,mPredictionsMatrix.get(Arrays.asList(lblC,lblA)));
		Assert.assertEquals(MarkovOutcome.negative,mPredictionsMatrix.get(Arrays.asList(lblC,lblB)));

		Assert.assertEquals(MarkovOutcome.failure,mPredictionsMatrix.get(Collections.singletonList(lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Collections.singletonList(lblB)));
		Assert.assertEquals(MarkovOutcome.negative,mPredictionsMatrix.get(Collections.singletonList(lblC)));
	}
	
	/** This one is similar to the {@link #testUpdateMarkovSideways1b}, except that there are a few additional negative transitions and the computation is forward. */
	@Test
	public void testUpdateMarkovSideways1d()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C / B-b->C-a-#D / B-c-#D","testUpdateMarkovSideways1c",config, converter);
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Map<List<Label>,MarkovOutcome> mPredictionsMatrix = m.computePredictionMatrix();
		Assert.assertEquals(7,mPredictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.failure,mPredictionsMatrix.get(Arrays.asList(lblA,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblB)));
		Assert.assertEquals(MarkovOutcome.negative,mPredictionsMatrix.get(Arrays.asList(lblA,lblC)));
		Assert.assertEquals(MarkovOutcome.negative,mPredictionsMatrix.get(Arrays.asList(lblB,lblA)));

		Assert.assertEquals(MarkovOutcome.failure,mPredictionsMatrix.get(Collections.singletonList(lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Collections.singletonList(lblB)));
		Assert.assertEquals(MarkovOutcome.negative,mPredictionsMatrix.get(Collections.singletonList(lblC)));
		
		Set<List<Label>> plusStrings = buildSet(new String[][] {},config,converter), minusStrings = buildSet(new String[][] { new String[]{"a","a","a"},new String[]{"a","b","a"},new String[]{"a","c"} },config,converter);
		MarkovModel another = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		another.createMarkovLearner(plusStrings, minusStrings, false);

		Map<List<Label>,MarkovOutcome> anotherPredictionsMatrix = another.computePredictionMatrix();
		Assert.assertEquals(7,anotherPredictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.failure,anotherPredictionsMatrix.get(Arrays.asList(lblA,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,anotherPredictionsMatrix.get(Arrays.asList(lblA,lblB)));
		Assert.assertEquals(MarkovOutcome.negative,anotherPredictionsMatrix.get(Arrays.asList(lblA,lblC)));
		Assert.assertEquals(MarkovOutcome.negative,anotherPredictionsMatrix.get(Arrays.asList(lblB,lblA)));

		Assert.assertEquals(MarkovOutcome.failure,anotherPredictionsMatrix.get(Collections.singletonList(lblA)));
		Assert.assertEquals(MarkovOutcome.positive,anotherPredictionsMatrix.get(Collections.singletonList(lblB)));
		Assert.assertEquals(MarkovOutcome.negative,anotherPredictionsMatrix.get(Collections.singletonList(lblC)));
	}

	@Test
	public void testUpdateMarkovSideways2()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-c->C / B-b-#D","testUpdateMarkovSideways2",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Map<List<Label>,MarkovOutcome> mPredictionsMatrix = m.computePredictionMatrix();
		Assert.assertEquals(3,mPredictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblA)));// this is obvious: if we have a label 'a' leading from a state then the prediction reports that in this case we can find 'a'.
		Assert.assertEquals(MarkovOutcome.negative,mPredictionsMatrix.get(Arrays.asList(lblB,lblC)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblC,lblC)));
	}
	
	@Test
	public void testUpdateMarkovSideways3()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Map<List<Label>,MarkovOutcome> mPredictionsMatrix = m.computePredictionMatrix();
		Assert.assertEquals(9,mPredictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblC)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblC,lblA)));
		
		Assert.assertEquals(MarkovOutcome.negative,mPredictionsMatrix.get(Arrays.asList(lblU,lblB)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblB,lblB)));
		
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblC,lblC)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblU,lblU)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblC,lblU)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblU,lblC)));
	}
	
	@Test
	public void testUpdateMarkovSideways4()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(3,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Map<List<Label>,MarkovOutcome> mPredictionsMatrix = m.computePredictionMatrix();
		Assert.assertEquals(6,mPredictionsMatrix.size());
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblC,lblB,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblB,lblA)));
		
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblC,lblU,lblC)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblU,lblC)));
		
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblC,lblC,lblC)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblC,lblC)));
	}
	
	@Test
	public void testUpdateMarkovSideways5()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(4,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Map<List<Label>,MarkovOutcome> mPredictionsMatrix = m.computePredictionMatrix();
		Assert.assertTrue(mPredictionsMatrix.isEmpty());
	}

	@Test
	public void testPredictTransitionsSideways1()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel mSideways = new MarkovModel(2,true, false,true,markovPTAUseMatrix), mForward = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		new MarkovClassifierLG(mSideways,graph,null).updateMarkov(true);
		Assert.assertEquals(9,mSideways.computePredictionMatrix().size());Assert.assertTrue(mForward.computePredictionMatrix().isEmpty());
		
		List<List<Label>> interestingPaths = new LinkedList<>();
		// nothing in Markov matrix hence no predictions.
		Assert.assertTrue(new MarkovClassifierLG(mForward,graph,null).predictTransitionsFromState(graph.findVertex("B"),null,2,true, interestingPaths).isEmpty());
		Assert.assertEquals(1,interestingPaths.size());Assert.assertEquals(Collections.singletonList(lblA),interestingPaths.get(0));
		
		interestingPaths.clear();
		Map<Label, MarkovOutcome> outcome1 = new MarkovClassifierLG(mSideways,graph,null).predictTransitionsFromState(graph.findVertex("B"),null,2,true, interestingPaths);
		Assert.assertEquals(1,interestingPaths.size());Assert.assertEquals(Collections.singletonList(lblB),interestingPaths.get(0));
		Assert.assertEquals(2,outcome1.size());
		Assert.assertEquals(MarkovOutcome.negative,outcome1.get(lblU));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblB));
		
		
		interestingPaths.clear();
		outcome1 = new MarkovClassifierLG(mSideways,graph,null).predictTransitionsFromState(graph.findVertex("E"),null,2,true, interestingPaths);
		Assert.assertEquals(2,interestingPaths.size());Assert.assertEquals(Collections.singletonList(lblC),interestingPaths.get(0));Assert.assertEquals(Collections.singletonList(lblU),interestingPaths.get(1));
		Assert.assertEquals(3,outcome1.size());
		Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblU));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblC));Assert.assertEquals(MarkovOutcome.positive,outcome1.get(lblA));
	}		

	@Test
	public void testPredictTransitionsSideways2()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-b->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		
		Map<List<Label>,MarkovOutcome> mPredictionsMatrix = m.computePredictionMatrix();
		Assert.assertEquals(8,mPredictionsMatrix.size());
		
		Assert.assertEquals(MarkovOutcome.failure,mPredictionsMatrix.get(Arrays.asList(lblU,lblB)));
		
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblC,lblA)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblA,lblC)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblC,lblC)));
		
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblB,lblU)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblB,lblB)));
		Assert.assertEquals(MarkovOutcome.positive,mPredictionsMatrix.get(Arrays.asList(lblU,lblU)));
	}

	/** Tests that upon a label labelled as invalid, subsequent inconsistency checks are stopped. It is hence equivalent to a single incoming path. */
	@Test
	public void testPredictTransitionsFromStatesSideways1()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->A / T-u->T-b->T","testCheckFanoutInconsistencySideways4",config, converter);// T is there to ensure that graph2's alphabet is the same as that of graph.
		Map<Label, MarkovOutcome> predictions = new MarkovClassifierLG(m,graph2,null).predictTransitionsFromState(graph2.getInit(),null,m.getChunkLen(),true, null);
		
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblC));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblA));
	}
	
	@Test
	public void testPredictTransitionsFromStatesForward1()
	{
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		Assert.assertTrue(m.computePredictionMatrix().isEmpty());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> predictions = new MarkovClassifierLG(m, graph2,null).predictTransitions();
		Assert.assertTrue(predictions.isEmpty());// empty Markov means no predictions.
	}
	
	@Test
	public void testPredictTransitionsFromStatesForward2a()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Assert.assertEquals(4,m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		Map<CmpVertex, Map<Label, MarkovOutcome>> predictions = new MarkovClassifierLG(m, graph2,null).predictTransitions();
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
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Assert.assertEquals(4,m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = new LearnerGraph(config);graph2.getInit().setAccept(false);
		Map<CmpVertex, Map<Label, MarkovOutcome>> predictions = new MarkovClassifierLG(m, graph2,null).predictTransitions();
		Assert.assertTrue(predictions.isEmpty());
		predictions = new MarkovClassifierLG(new MarkovModel(2,true, false,true,markovPTAUseMatrix),graph2,null).predictTransitions();
		Assert.assertTrue(predictions.isEmpty());
	}
	
	@Test
	public void testPredictTransitionsFromStatesForward3()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Assert.assertEquals(4,m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->A/ T-a->T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		LearnerGraph extendedGraph = new MarkovClassifierLG(m,graph2,null).constructMarkovTentative();
		Assert.assertNull(WMethod.checkM(FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->A / A-u->E / B-b->C / B-u-#D","testPredictTransitionsFromStatesForward3",config, converter), extendedGraph));
		Assert.assertNotNull(extendedGraph.findVertex("T"));// extended graph starts as a replica of an original one.
	}

	/** Here the alphabet is limited to what is an the tentative automaton, hence nothing is added. */
	@Test
	public void testPredictTransitionsFromStatesSideways2()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Assert.assertEquals(9,m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B","testCheckFanoutInconsistencySideways4",config, converter);
		LearnerGraph extendedGraph = new MarkovClassifierLG(m,graph2,null).constructMarkovTentative();
		Assert.assertNull(WMethod.checkM(graph2,extendedGraph));
	}
	
	/** Tests that upon a label labelled as invalid, subsequent inconsistency checks are stopped. It is hence equivalent to a single incoming path. */
	@Test
	public void testPredictTransitionsFromStatesSideways3()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Assert.assertEquals(9,m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B / T-a->T-u->T-b->T-c->T","testPredictTransitionsFromStatesSideways3",config, converter);
		LearnerGraph extendedGraph = new MarkovClassifierLG(m,graph2,null).constructMarkovTentative();
		Assert.assertNull(WMethod.checkM(FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->F","testPredictTransitionsFromStatesForward3",config, converter), extendedGraph));// FSM comparison ignores unreachable states here
		Assert.assertNotNull(extendedGraph.findVertex("T"));// extended graph starts as a replica of an original one.
	}

	/** Same as {@link #testPredictTransitionsFromStatesSideways1()}, except that the path beyond is empty rather than null. */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState1()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->A / T-u->T-b->T","testCheckFanoutInconsistencySideways4",config, converter);// T is there to ensure that graph2's alphabet is the same as that of graph.
		Map<Label, MarkovOutcome> predictions = new MarkovClassifierLG(m, graph2,null).predictTransitionsFromState(graph2.getInit(), Collections.emptyList(),m.getChunkLen(),true, null);
		
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblC));
		Assert.assertEquals(MarkovOutcome.positive,predictions.get(lblA));
	}
	
	/** Same as {@link #testPredictTransitionsFromStatesSideways1()}, except that the path beyond is non-empty. */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState2()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		final MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B","testPredictTransitionsFromStatesWithPathBeyondCurrentState2",config, converter);
		
		TestHelper.checkForCorrectException(() -> new MarkovClassifierLG(m, graph2,null).predictTransitionsFromState(graph2.getInit(), Collections.singletonList(lblC),m.getChunkLen(),true, null), IllegalArgumentException.class, "cannot be made by extension");
	}
	
	/** Almost the same as {@link TestMarkovLearner#testPredictTransitionsFromStatesForward2a()} except that the path beyond is empty rather than null. */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState3()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Assert.assertEquals(4,m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		Map<Label,MarkovOutcome> outgoing_labels_probabilities=new MarkovClassifierLG(m, graph2,null).predictTransitionsFromState(graph2.findVertex("B"), Collections.emptyList(),m.getChunkLen(),true, null);
		Assert.assertEquals(2,outgoing_labels_probabilities.size());
		Assert.assertEquals(MarkovOutcome.negative,outgoing_labels_probabilities.get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,outgoing_labels_probabilities.get(lblB));
	}

	/** Almost the same as {@link #testPredictTransitionsFromStatesForward2a()} except that the path beyond is not empty. Transition <i>d</i> from A to B is not present in Markov, so routinely there will be no prediction. Nevertheless,
	 * there is one because we assume that the considered paths leading to the state B of interest all start with label b. 
	 */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState4()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Assert.assertEquals(4,m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-d->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		Map<Label,MarkovOutcome> outgoing_labels_probabilities=new MarkovClassifierLG(m, graph2,null).predictTransitionsFromState(graph2.findVertex("B"), Collections.singletonList(lblA),m.getChunkLen(),true, null);
		Assert.assertEquals(2,outgoing_labels_probabilities.size());
		Assert.assertEquals(MarkovOutcome.negative,outgoing_labels_probabilities.get(lblU));
		Assert.assertEquals(MarkovOutcome.positive,outgoing_labels_probabilities.get(lblB));
	}

	/** Almost the same as {@link TestMarkovLearner#testPredictTransitionsFromStatesForward2a} except that the path beyond is not empty. Transition <i>d</i> from A to B is not present in Markov, so routinely there will be no prediction. Nevertheless,
	 * there is one because we assume that the considered paths leading to the state B of interest all start with paths a b. 
	 */
	@Test
	public void testPredictTransitionsFromStatesWithPathBeyondCurrentState5()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		final MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(true);
		Assert.assertEquals(4,m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-d->B / A-c->A/ T-u->T-b->T","testPredictTransitionsFromStatesForward2",config, converter);
		TestHelper.checkForCorrectException(() -> new MarkovClassifierLG(m, graph2,null).predictTransitionsFromState(graph2.findVertex("B"),Arrays.asList(lblA,lblB),m.getChunkLen(),true, null), IllegalArgumentException.class, "supplied path");
	}

	
	private static final boolean considerPathsWithPrefixMissing = false;
	
	/** Here we look for path "s" in Markov that has never been seen, hence we cannot do much about it and it is being ignored. */ 
	@Test
	public void testCheckFanoutInconsistencySideways1_s1()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-s->B","testCheckFanoutInconsistencySideways1",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifierLG(m, graph2,null).checkFanoutInconsistency(graph2.getInit(),new MarkovClassifier.ConsistencyChecker(){

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
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph2.pathroutines.computeAlphabet();
			}

			@Override
			public boolean considerPathsWithPrefixMissingInMarkov() {
				return false;
			}
		}));
		Assert.assertEquals(0, counterA.get());Assert.assertEquals(0, counterB.get());
	}
	
	/** Unlike the above, the missing path is not being ignored, leading to an inconsistency. */
	@Test
	public void testCheckFanoutInconsistencySideways1_s2()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-s->B","testCheckFanoutInconsistencySideways1",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifierLG(m, graph2,null).checkFanoutInconsistency(graph2.getInit(),new MarkovClassifier.ConsistencyChecker(){

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
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph2.pathroutines.computeAlphabet();
			}

			@Override
			public boolean considerPathsWithPrefixMissingInMarkov() {
				return true;
			}
		}));
		Assert.assertEquals(1, counterA.get());Assert.assertEquals(1, counterB.get());
	}
	
	/** Here we look for path "s" in Markov that has never been seen, hence we cannot do much about it and it is being ignored. */
	@Test
	public void testCheckFanoutInconsistencySideways1_a()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B","testCheckFanoutInconsistencySideways1",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifierLG(m, graph2,null).checkFanoutInconsistency(graph2.getInit(),new MarkovClassifier.ConsistencyChecker(){

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

			@Override
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph2.pathroutines.computeAlphabet();
			}

			@Override
			public boolean considerPathsWithPrefixMissingInMarkov() {
				return considerPathsWithPrefixMissing;
			}
		}));
		Assert.assertEquals(1, counterA.get());Assert.assertEquals(1, counterB.get());
	}
	
	/** Checks that where we did not build prefix-closed paths, nothing can be predicted because we check whether shorter chunklen-1 -long paths exist and ignore anything where such paths do not exist. */
	@Test
	public void testCheckFanoutInconsistencySideways2()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B-a->B-b->B","testCheckFanoutInconsistencySideways2",config, converter);
		graph2.transitionMatrix.get(graph2.getInit()).clear();// make it look like a graph has no transitions
		Assert.assertEquals(0,new MarkovClassifierLG(m, graph2,null).checkFanoutInconsistency(graph2.getInit(), new MarkovClassifier.ConsistencyChecker(){

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
			public Collection<Label> obtainAlphabet(AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graphToConsider.getCache().getAlphabet();
			}

			@Override
			public boolean considerPathsWithPrefixMissingInMarkov() {
				return considerPathsWithPrefixMissing;
			}
		}));
	}
	
	@Test
	public void testCheckFanoutInconsistencySideways3()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B","testCheckFanoutInconsistencySideways3",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifierLG(m, graph2,null).checkFanoutInconsistency(graph2.getInit(), new MarkovClassifier.ConsistencyChecker(){

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
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph.pathroutines.computeAlphabet();// returns the alphabet of the Markov matrix
			}

			@Override
			public boolean considerPathsWithPrefixMissingInMarkov() {
				return considerPathsWithPrefixMissing;
			}
		}));
		Assert.assertEquals(4, counterA.get());
		Assert.assertEquals(4, counterB.get());
	}
	
	/** Similar to 3 but with multiple outgoing paths. */
	@Test
	public void testCheckFanoutInconsistencySideways4()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifierLG(m, graph2,null).checkFanoutInconsistency(graph2.getInit(), new MarkovClassifier.ConsistencyChecker(){

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
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph.pathroutines.computeAlphabet();// returns the alphabet of the Markov matrix
			}

			@Override
			public boolean considerPathsWithPrefixMissingInMarkov() {
				return considerPathsWithPrefixMissing;
			}
		}));
		Assert.assertEquals(8, counterA.get());
		Assert.assertEquals(8, counterB.get());
	}
	
	/** Tests that with only one path, we do a single check anyway, hence returning false from obtainAlphabet does not change anything. */
	@Test
	public void testCheckFanoutInconsistencySideways5()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B","testCheckFanoutInconsistencySideways3",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifierLG(m, graph2,null).checkFanoutInconsistency(graph2.getInit(), new MarkovClassifier.ConsistencyChecker(){

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
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph.pathroutines.computeAlphabet();// returns the alphabet of the Markov matrix
			}

			@Override
			public boolean considerPathsWithPrefixMissingInMarkov() {
				return considerPathsWithPrefixMissing;
			}
		}));
		Assert.assertEquals(4, counterA.get());
		Assert.assertEquals(4, counterB.get());
	}

	/** Tests that upon a label labelled as invalid, subsequent inconsistency checks are stopped. It is hence equivalent to a single incoming path. */
	@Test
	public void testCheckFanoutInconsistencySideways6()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, false,true,markovPTAUseMatrix);
		new MarkovClassifierLG(m,graph,null).updateMarkov(false);
		Assert.assertEquals(9+graph.getCache().getAlphabet().size(),m.computePredictionMatrix().size());
		
		final LearnerGraph graph2 = FsmParserStatechum.buildLearnerGraph("A-a->B / A-c->A","testCheckFanoutInconsistencySideways4",config, converter);
		final AtomicInteger counterA=new AtomicInteger(0),counterB=new AtomicInteger(0);
		Assert.assertEquals(0,new MarkovClassifierLG(m, graph2,null).checkFanoutInconsistency(graph2.getInit(), new MarkovClassifier.ConsistencyChecker(){

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
			public Collection<Label> obtainAlphabet(@SuppressWarnings("unused") AbstractLearnerGraph graphToConsider, @SuppressWarnings("unused") CmpVertex v) {
				return graph.pathroutines.computeAlphabet();// returns the alphabet of the Markov matrix
			}

			@Override
			public boolean considerPathsWithPrefixMissingInMarkov() {
				return considerPathsWithPrefixMissing;
			}
		}));
		Assert.assertEquals(4, counterA.get());
		Assert.assertEquals(4, counterB.get());
	}

	@Test
	public void testLabelStatesAwayFromRoot1()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->C-a->A-b->B-a->D / C-b->C","testLabelStatesAwayFromRoot1",config, converter);
		Collection<CmpVertex> previousFrontline = LearningSupportRoutines.labelStatesAwayFromRoot(graph,0);
		Assert.assertSame(JUConstants.RED, graph.findVertex("A").getColour());
		Assert.assertNull(graph.findVertex("B").getColour());
		
		Assert.assertNull(graph.findVertex("C").getColour());Assert.assertNull(graph.findVertex("D").getColour());
		
		Assert.assertNull(previousFrontline);
	}


	@Test
	public void testLabelStatesAwayFromRoot2()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->C-a->A-b->B-a->D / C-b->C","testLabelStatesAwayFromRoot1",config, converter);
		Collection<CmpVertex> previousFrontline = LearningSupportRoutines.labelStatesAwayFromRoot(graph,1);
		Assert.assertSame(JUConstants.RED, graph.findVertex("C").getColour());
		
		Assert.assertNull(graph.findVertex("A").getColour());Assert.assertNull(graph.findVertex("D").getColour());

		Set<CmpVertex> expected = new TreeSet<>(Collections.singletonList(graph.findVertex("A"))), actual = new TreeSet<>(previousFrontline);
		Assert.assertEquals(expected,actual);
	}
	
	@Test
	public void testLabelStatesAwayFromRoot3()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->C-a->A-b->B-a->D / C-b->C","testLabelStatesAwayFromRoot1",config, converter);
		Collection<CmpVertex> previousFrontline = LearningSupportRoutines.labelStatesAwayFromRoot(graph,2);
		
		Assert.assertNull(graph.findVertex("A").getColour());Assert.assertNull(graph.findVertex("B").getColour());Assert.assertNull(graph.findVertex("C").getColour());
		Assert.assertSame(JUConstants.RED, graph.findVertex("D").getColour());

		Set<CmpVertex> expected = new TreeSet<>(Arrays.asList(graph.findVertex("B"), graph.findVertex("C"))), actual = new TreeSet<>(previousFrontline);
		Assert.assertEquals(expected,actual);
	}
	
	@Test
	public void testLabelStatesAwayFromRoot4()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->C-a->A-b->B-a->D / C-b->C","testLabelStatesAwayFromRoot1",config, converter);
		
		TestHelper.checkForCorrectException(() -> LearningSupportRoutines.labelStatesAwayFromRoot(graph,3), IllegalArgumentException.class, "no states");
	}

	@Test
	public void testIdentifyUncoveredTransitions1()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E","testIdentifyUncoveredTransitions1a",config, converter);
		final LearnerGraph reference = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / T-b->T-u->T","testIdentifyUncoveredTransitions1b",config, converter);
		TestHelper.checkForCorrectException(() -> LearningSupportRoutines.identifyUncoveredTransitions(graph,reference), IllegalArgumentException.class, "PTA is not");
	}
	
	@Test
	public void testIdentifyUncoveredTransitions2()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C1 / B-u->E","testIdentifyUncoveredTransitions2a",config, converter);
		final LearnerGraph reference = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / T-b->T-u->T","testIdentifyUncoveredTransitions1b",config, converter);
		Map<CmpVertex,Set<Label>> uncovered = LearningSupportRoutines.identifyUncoveredTransitions(graph,reference);
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
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B / B-u->E","testIdentifyUncoveredTransitions3a",config, converter);
		final LearnerGraph reference = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / T-b->T-u->T","testIdentifyUncoveredTransitions1b",config, converter);
		Map<CmpVertex,Set<Label>> uncovered = LearningSupportRoutines.identifyUncoveredTransitions(graph,reference);
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
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C","testIdentifyUncoveredTransitions4a",config, converter);
		final LearnerGraph reference = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / T-b->T-u->T","testIdentifyUncoveredTransitions1b",config, converter);
		Map<CmpVertex,Set<Label>> uncovered = LearningSupportRoutines.identifyUncoveredTransitions(graph,reference);
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
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C","testIdentifyUncoveredTransitions4a",config, converter);
		final LearnerGraph reference = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / E-z->F / T-b->T-u->T","testIdentifyUncoveredTransitions5b",config, converter);
		Map<CmpVertex,Set<Label>> uncovered = LearningSupportRoutines.identifyUncoveredTransitions(graph,reference);
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
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C1 / B-u->E","testIdentifyUncoveredTransitions2a",config, converter);
		final LearnerGraph reference = FsmParserStatechum.buildLearnerGraph("A-a->D","testIdentifyUncoveredTransitions3",config, converter);
		TestHelper.checkForCorrectException(() -> LearningSupportRoutines.identifyUncoveredTransitions(graph,reference), IllegalArgumentException.class, "coverage has more transitions");
	}
	
	@Test
	public void testTrimUncoveredTransitions1()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C","testIdentifyUncoveredTransitions4a",config, converter);
		final LearnerGraph reference = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C / B-u->E / B-x->B / E-z->F / T-b->T-u->T","testIdentifyUncoveredTransitions5b",config, converter);
		LearnerGraph trimmedReference = LearningSupportRoutines.trimUncoveredTransitions(graph,reference);
		DifferentFSMException diffException = WMethod.checkM(FsmParserStatechum.buildLearnerGraph("A-a->D-b->C","testTrimUncoveredTransitions1",config, converter), trimmedReference);
		Assert.assertNull(diffException);
	}
	
	@Test
	public void testTrimUncoveredTransitions2()
	{
		final LearnerGraph graph = FsmParserStatechum.buildLearnerGraph("A-a->D-b->C / A-c->B-b->C1 / B-u->E","testIdentifyUncoveredTransitions2a",config, converter);
		final LearnerGraph reference = FsmParserStatechum.buildLearnerGraph("A-a->D","testIdentifyUncoveredTransitions3",config, converter);
		TestHelper.checkForCorrectException(() -> LearningSupportRoutines.trimUncoveredTransitions(graph,reference), IllegalArgumentException.class, "coverage has more transitions");
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques1a()
	{
		Collection<List<Label>> uniques = new LinkedList<>();uniques.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("a", config, converter)));
		Set<CmpVertex> uniqueStates= new TreeSet<>();Collection<List<Label>> invalidSequences = new LinkedList<>();
		LearningSupportRoutines.statesIdentifiedUsingUniques(FsmParserStatechum.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions2a",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertTrue(uniqueStates.isEmpty());Assert.assertEquals(uniques,invalidSequences);Assert.assertNotSame(uniques, invalidSequences);
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques1b()
	{
		Collection<List<Label>> uniques = new LinkedList<>();uniques.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("a", config, converter)));
		Collection<CmpVertex> uniqueStates=LearningSupportRoutines.statesIdentifiedUsingSequences(FsmParserStatechum.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions2a",config, converter), uniques);
		Assert.assertEquals(2,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
		Assert.assertEquals("B",uniqueVertices.next().getStringId());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques1c()
	{
		Collection<List<Label>> uniques = new LinkedList<>();uniques.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("b", config, converter)));
		Set<CmpVertex> uniqueStates= new TreeSet<>();Collection<List<Label>> invalidSequences = new LinkedList<>();
		LearningSupportRoutines.statesIdentifiedUsingUniques(FsmParserStatechum.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions2a",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertTrue(uniqueStates.isEmpty());Assert.assertTrue(invalidSequences.isEmpty());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques1d()
	{
		Collection<List<Label>> uniques = new LinkedList<>();uniques.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("b", config, converter)));
		Collection<CmpVertex> uniqueStates=LearningSupportRoutines.statesIdentifiedUsingSequences(FsmParserStatechum.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions2a",config, converter), uniques);
		Assert.assertTrue(uniqueStates.isEmpty());
	}

	@Test
	public void testStatesIdentifiedUsingUniques2a()
	{
		Collection<List<Label>> uniques = new LinkedList<>();uniques.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("a", config, converter)));
		Set<CmpVertex> uniqueStates= new TreeSet<>();Collection<List<Label>> invalidSequences = new LinkedList<>();
		LearningSupportRoutines.statesIdentifiedUsingUniques(FsmParserStatechum.buildLearnerGraph("A-a->B-b->C","testIdentifyUncoveredTransitions2a",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertEquals(1,uniqueStates.size());
		Assert.assertEquals("A",uniqueStates.iterator().next().getStringId());Assert.assertTrue(invalidSequences.isEmpty());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques2b()
	{
		Collection<List<Label>> uniques = new LinkedList<>();uniques.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("a", config, converter)));
		Collection<CmpVertex> uniqueStates=LearningSupportRoutines.statesIdentifiedUsingSequences(FsmParserStatechum.buildLearnerGraph("A-a->B-b->C","testIdentifyUncoveredTransitions2b",config, converter), uniques);
		Assert.assertEquals(1,uniqueStates.size());
		Assert.assertEquals("A",uniqueStates.iterator().next().getStringId());
	}

	@Test
	public void testStatesIdentifiedUsingUniques3a()
	{
		Collection<List<Label>> uniques = new LinkedList<>();uniques.add(Arrays.asList(AbstractLearnerGraph.generateNewLabel("a", config, converter),AbstractLearnerGraph.generateNewLabel("a", config, converter)));
		Set<CmpVertex> uniqueStates= new TreeSet<>();Collection<List<Label>> invalidSequences = new LinkedList<>();
		LearningSupportRoutines.statesIdentifiedUsingUniques(FsmParserStatechum.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions3a",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertEquals(1,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());Assert.assertTrue(invalidSequences.isEmpty());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques3b()
	{
		Collection<List<Label>> uniques = new LinkedList<>();uniques.add(Arrays.asList(AbstractLearnerGraph.generateNewLabel("a", config, converter),AbstractLearnerGraph.generateNewLabel("a", config, converter)));
		Collection<CmpVertex> uniqueStates=LearningSupportRoutines.statesIdentifiedUsingSequences(FsmParserStatechum.buildLearnerGraph("A-a->B-a->C","testIdentifyUncoveredTransitions3a",config, converter), uniques);
		Assert.assertEquals(1,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques3c()
	{
		Collection<List<Label>> uniques = new LinkedList<>();uniques.add(Arrays.asList(AbstractLearnerGraph.generateNewLabel("a", config, converter),AbstractLearnerGraph.generateNewLabel("a", config, converter)));
		Set<CmpVertex> uniqueStates= new TreeSet<>();Collection<List<Label>> invalidSequences = new LinkedList<>();
		LearningSupportRoutines.statesIdentifiedUsingUniques(FsmParserStatechum.buildLearnerGraph("A-a->B-a->C-a->D","testIdentifyUncoveredTransitions3b",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertTrue(uniqueStates.isEmpty());Assert.assertEquals(uniques,invalidSequences);Assert.assertNotSame(uniques, invalidSequences);
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques3d()
	{
		Collection<List<Label>> uniques = new LinkedList<>();uniques.add(Arrays.asList(AbstractLearnerGraph.generateNewLabel("a", config, converter),AbstractLearnerGraph.generateNewLabel("a", config, converter)));
		Collection<CmpVertex> uniqueStates=LearningSupportRoutines.statesIdentifiedUsingSequences(FsmParserStatechum.buildLearnerGraph("A-a->B-a->C-a->D","testIdentifyUncoveredTransitions3b",config, converter), uniques);
		Assert.assertEquals(2,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
		Assert.assertEquals("B",uniqueVertices.next().getStringId());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques4a()
	{
		Collection<List<Label>> uniques = new LinkedList<>();
		uniques.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("a", config, converter)));uniques.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("b", config, converter)));
		Set<CmpVertex> uniqueStates= new TreeSet<>();Collection<List<Label>> invalidSequences = new LinkedList<>();
		LearningSupportRoutines.statesIdentifiedUsingUniques(FsmParserStatechum.buildLearnerGraph("A-a->B-b->C","testIdentifyUncoveredTransitions2a",config, converter), uniques,uniqueStates,invalidSequences);
		Assert.assertEquals(2,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
		Assert.assertEquals("B",uniqueVertices.next().getStringId());
		Assert.assertTrue(invalidSequences.isEmpty());
	}
	
	@Test
	public void testStatesIdentifiedUsingUniques4b()
	{
		Collection<List<Label>> uniques = new LinkedList<>();
		uniques.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("a", config, converter)));uniques.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("b", config, converter)));
		Collection<CmpVertex> uniqueStates=LearningSupportRoutines.statesIdentifiedUsingSequences(FsmParserStatechum.buildLearnerGraph("A-a->B-b->C","testIdentifyUncoveredTransitions2b",config, converter), uniques);
		Assert.assertEquals(2,uniqueStates.size());
		Iterator<CmpVertex> uniqueVertices = uniqueStates.iterator(); 
		Assert.assertEquals("A",uniqueVertices.next().getStringId());
		Assert.assertEquals("B",uniqueVertices.next().getStringId());
	}

	@Test
	public void testCollectionOfSetsToPair1()
	{
		Collection<Set<CmpVertex>> collectionOfSets = new LinkedList<>();
		List<StatePair> outcome = MarkovClassifier.collectionOfSetsToPairs(collectionOfSets);
		Assert.assertTrue(outcome.isEmpty());
	}

	@Test
	public void testCollectionOfSetsToPair2()
	{
		Collection<Set<CmpVertex>> collectionOfSets = new LinkedList<>();
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("A-a->B-a->C-a->D-a->E","testIdentifyUncoveredTransitions3a",config, converter);
		Set<CmpVertex> P= new TreeSet<>(),Q= new TreeSet<>(),R= new TreeSet<>();
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
		Collection<Set<CmpVertex>> collectionOfSets = new LinkedList<>();
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("A-a->B-a->C-a->D-a->E","testIdentifyUncoveredTransitions3a",config, converter);
		Set<CmpVertex> P= new TreeSet<>(),R= new TreeSet<>();
		P.add(gr.findVertex("A"));
		collectionOfSets.add(P);collectionOfSets.add(R);
		
		// Here P is a singleton and R is empty, hence they are ignored.
		List<StatePair> outcome = MarkovClassifier.collectionOfSetsToPairs(collectionOfSets);
		Assert.assertTrue(outcome.isEmpty());
	}
	
	@Test
	public void testBuildVerticesToMergeForPath1()
	{
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("A-a->B / A-b->A / B-a->C-b->D-a->E / D-c->D / E-c->E","testBuildVerticesToMergeForPath1",config, converter);
		Collection<List<Label>> paths = new LinkedList<>();paths.add(Collections.singletonList(lblA));paths.add(Collections.singletonList(lblB));paths.add(Collections.singletonList(lblC));
		
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifierLG(new MarkovModel(2,true, true,true,markovPTAUseMatrix),gr,null).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(1,collectionOfSets.size());
		Assert.assertEquals(gr.transitionMatrix.keySet(), collectionOfSets.iterator().next());
	}
	
	/** Last state not to be merged. */
	@Test
	public void testBuildVerticesToMergeForPath2()
	{
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("A-a->B / A-b->A / B-a->C-b->D-a->E / D-c->D / E-d->E","testBuildVerticesToMergeForPath1",config, converter);
		Collection<List<Label>> paths = new LinkedList<>();paths.add(Collections.singletonList(lblA));paths.add(Collections.singletonList(lblB));paths.add(Collections.singletonList(lblC));
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets = new MarkovClassifierLG(new MarkovModel(2,true, true,true,markovPTAUseMatrix),gr,null).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(1,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<>(gr.transitionMatrix.keySet());
		partA.remove(gr.findVertex("E"));
		
		Assert.assertEquals(partA, iterator.next());
	}
	
	/** State E is only identified with d that is not shared with other states. */
	@Test
	public void testBuildVerticesToMergeForPath3()
	{
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("A-a->B / A-b->A / B-a->C-b->D-a->E / D-c->D / E-d->E","testBuildVerticesToMergeForPath3",config, converter);
		Collection<List<Label>> paths = new LinkedList<>();paths.add(Collections.singletonList(lblA));paths.add(Collections.singletonList(lblB));paths.add(Collections.singletonList(lblC));paths.add(Collections.singletonList(lblD));
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifierLG(new MarkovModel(2,true, true,true,markovPTAUseMatrix),gr,null).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(2,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<>(gr.transitionMatrix.keySet());
		partA.remove(gr.findVertex("E"));
		Set<CmpVertex> partB = new TreeSet<>();partB.add(gr.findVertex("E"));
		
		Assert.assertEquals(partA, iterator.next());
		Assert.assertEquals(partB, iterator.next());
	}
	
	/** States C and E are identified with d that is not shared with other states. */
	@Test
	public void testBuildVerticesToMergeForPath4()
	{
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("A-a->B / A-b->A / B-a->C-d->D-a->E / D-c->D / E-d->E","testBuildVerticesToMergeForPath4",config, converter);
		Collection<List<Label>> paths = new LinkedList<>();paths.add(Collections.singletonList(lblA));paths.add(Collections.singletonList(lblB));paths.add(Collections.singletonList(lblC));paths.add(Collections.singletonList(lblD));
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifierLG(new MarkovModel(2,true, true,true,markovPTAUseMatrix),gr,null).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(2,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<>(gr.transitionMatrix.keySet());
		partA.remove(gr.findVertex("E"));partA.remove(gr.findVertex("C"));
		Set<CmpVertex> partB = new TreeSet<>();partB.add(gr.findVertex("E"));partB.add(gr.findVertex("C"));
		
		Assert.assertEquals(partA, iterator.next());
		Assert.assertEquals(partB, iterator.next());
	}

	@Test
	public void testBuildVerticesToMergeForPath5()
	{
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("A-a->B / A-b->A / B-a->C-b->D-a->E / D-c->D / E-c->E-d->F-d->F-u->G-u->G","testBuildVerticesToMergeForPath5",config, converter);
		Collection<List<Label>> paths = new LinkedList<>();paths.add(Collections.singletonList(lblA));paths.add(Collections.singletonList(lblB));paths.add(Collections.singletonList(lblC));paths.add(Collections.singletonList(lblD));paths.add(Collections.singletonList(lblU));
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifierLG(new MarkovModel(2,true, true,true,markovPTAUseMatrix),gr,null).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(1,collectionOfSets.size());
		Assert.assertEquals(gr.transitionMatrix.keySet(), collectionOfSets.iterator().next());
	}
	
	/** Same as {@link #testBuildVerticesToMergeForPath1} but with different state labelling. */
	@Test
	public void testBuildVerticesToMergeForPath6()
	{
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("B-a->C-b->D-a->E / D-c->D / E-c->E-d->F-d->F-u->G-u->G / Z-a->B / Z-b->Z","testBuildVerticesToMergeForPath6",config, converter);
		Collection<List<Label>> paths = new LinkedList<>();paths.add(Collections.singletonList(lblA));paths.add(Collections.singletonList(lblB));paths.add(Collections.singletonList(lblC));paths.add(Collections.singletonList(lblD));paths.add(Collections.singletonList(lblU));
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifierLG(new MarkovModel(2,true, true,true,markovPTAUseMatrix),gr,null).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(1,collectionOfSets.size());
		Assert.assertEquals(gr.transitionMatrix.keySet(), collectionOfSets.iterator().next());
	}
	
	/** No states identified. */
	@Test
	public void testBuildVerticesToMergeForPath7a()
	{
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("B-a->C-b->D-a->E / D-c->D / E-c->E-d->F-d->F-u->G-u->G / Z-a->B / Z-b->Z","testBuildVerticesToMergeForPath6",config, converter);
		Collection<List<Label>> paths = new LinkedList<>();
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifierLG(new MarkovModel(2,true, true,true,markovPTAUseMatrix),gr,null).buildVerticesToMergeForPaths(paths);
		Assert.assertTrue(collectionOfSets.isEmpty());
	}
	
	/** No states identified. */
	@Test
	public void testBuildVerticesToMergeForPath7b()
	{
		LearnerGraph gr=new LearnerGraph(config);
		Collection<List<Label>> paths = new LinkedList<>();paths.add(Collections.singletonList(lblA));paths.add(Collections.singletonList(lblB));paths.add(Collections.singletonList(lblC));
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifierLG(new MarkovModel(2,true, true,true,markovPTAUseMatrix),gr,null).buildVerticesToMergeForPaths(paths);
		Assert.assertTrue(collectionOfSets.isEmpty());
	}
	
	/** A few different clusters identified. */
	@Test
	public void testBuildVerticesToMergeForPath8()
	{
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("A-a->B / A-b->A / B-a->C-d->D-a->E / D-c->D / E-d->E-e->F-d->F-u->F / G-u->G","testBuildVerticesToMergeForPath8",config, converter);
		Collection<List<Label>> paths = new LinkedList<>();paths.add(Collections.singletonList(lblA));paths.add(Collections.singletonList(lblB));paths.add(Collections.singletonList(lblC));paths.add(Collections.singletonList(lblD));paths.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("e", config, converter)));paths.add(Collections.singletonList(lblU));
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifierLG(new MarkovModel(2,true, true,true,markovPTAUseMatrix),gr,null).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(2,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<>();partA.add(gr.findVertex("A"));partA.add(gr.findVertex("B"));partA.add(gr.findVertex("D"));
		Set<CmpVertex> partB = new TreeSet<>();partB.add(gr.findVertex("C"));partB.add(gr.findVertex("E"));partB.add(gr.findVertex("F"));partB.add(gr.findVertex("G"));
		
		Assert.assertEquals(partA, iterator.next());
		Assert.assertEquals(partB, iterator.next());
	}

	/** A few different clusters identified. */
	@Test
	public void testBuildVerticesToMergeForPath9()
	{
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("A-a->B / A-b->A / B-a->C-d->D-a->E / D-c->D / E-d->E-e->F-d->F-u->F / G-e->G","testBuildVerticesToMergeForPath8",config, converter);
		Collection<List<Label>> paths = new LinkedList<>();paths.add(Collections.singletonList(lblA));paths.add(Collections.singletonList(lblB));paths.add(Collections.singletonList(lblC));paths.add(Collections.singletonList(lblD));paths.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("e", config, converter)));paths.add(Collections.singletonList(lblU));
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifierLG(new MarkovModel(2,true, true,true,markovPTAUseMatrix),gr,null).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(2,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<>();partA.add(gr.findVertex("A"));partA.add(gr.findVertex("B"));partA.add(gr.findVertex("D"));
		Set<CmpVertex> partB = new TreeSet<>();partB.add(gr.findVertex("C"));partB.add(gr.findVertex("E"));partB.add(gr.findVertex("F"));partB.add(gr.findVertex("G"));
		
		Assert.assertEquals(partA, iterator.next());
		Assert.assertEquals(partB, iterator.next());
	}
	
	/** A few different clusters identified. */
	@Test
	public void testBuildVerticesToMergeForPath10()
	{
		LearnerGraph gr= FsmParserStatechum.buildLearnerGraph("A-a->B / A-b->A / B-a->C-d->D-a->E / D-c->D / E-d->E-e->F-d->F-u->F / G-f->G","testBuildVerticesToMergeForPath8",config, converter);
		Collection<List<Label>> paths = new LinkedList<>();paths.add(Collections.singletonList(lblA));paths.add(Collections.singletonList(lblB));paths.add(Collections.singletonList(lblC));paths.add(Collections.singletonList(lblD));paths.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("e", config, converter)));paths.add(Collections.singletonList(AbstractLearnerGraph.generateNewLabel("f", config, converter)));paths.add(Collections.singletonList(lblU));
		//for(LearnerGraph g:grForPaths.values())	System.out.println(g.transitionMatrix);
		Collection<Set<CmpVertex>> collectionOfSets=new MarkovClassifierLG(new MarkovModel(2,true, true,true,markovPTAUseMatrix),gr,null).buildVerticesToMergeForPaths(paths);
		Assert.assertEquals(3,collectionOfSets.size());
		Iterator<Set<CmpVertex>> iterator = collectionOfSets.iterator();
		Set<CmpVertex> partA = new TreeSet<>();partA.add(gr.findVertex("A"));partA.add(gr.findVertex("B"));partA.add(gr.findVertex("D"));
		Set<CmpVertex> partB = new TreeSet<>();partB.add(gr.findVertex("C"));partB.add(gr.findVertex("E"));partB.add(gr.findVertex("F"));
		Set<CmpVertex> partC = new TreeSet<>();partC.add(gr.findVertex("G"));
		
		Assert.assertEquals(partA, iterator.next());
		Assert.assertEquals(partB, iterator.next());
		Assert.assertEquals(partC, iterator.next());
	}

	/** Tests non-deterministic case of {@link MarkovClassifier#tracePath}, deterministic case is tested with {@link statechum.analysis.learning.TestPathTracing}. */
	@Test
	public void testTracePath1()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C", "testTracePath1",config,converter);
		LearnerGraphND ndFSM = new LearnerGraphND(fsm,config);
		synchronized(AbstractLearnerGraph.syncObj)
		{
			ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("A"), true, lblA),true,lblA);
		}
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Collections.emptyList(),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","b"),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a"),config,converter),fsm.findVertex("A")));
	}
	
	/** Tests non-deterministic case of {@link MarkovClassifier#tracePath}, deterministic case is tested with {@link statechum.analysis.learning.TestPathTracing}. */
	@Test
	public void testTracePath2()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C", "testTracePath1",config,converter);
		LearnerGraphND ndFSM = new LearnerGraphND(fsm,config);
		synchronized(AbstractLearnerGraph.syncObj)
		{
			CmpVertex AA=ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("A"), true, lblA),true,lblA);
			ndFSM.addVertex(AA, true, lblA);ndFSM.addVertex(AA, true, lblC);
		}
		Assert.assertEquals(7,ndFSM.getStateNumber());
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","c"),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","a"),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","b"),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","a","a"),config,converter),fsm.findVertex("A")));
	}

	/** Tests non-deterministic case of {@link MarkovClassifier#tracePath}, deterministic case is tested with {@link statechum.analysis.learning.TestPathTracing}. */
	@Test
	public void testTracePath3()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C", "testTracePath1",config,converter);
		LearnerGraphND ndFSM = new LearnerGraphND(fsm,config);
		synchronized(AbstractLearnerGraph.syncObj)
		{
			ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("A"), true, lblA),true,lblA),true,lblA),true,lblA);
			ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("A"), true, lblA),true,lblA),true,lblA);
		}
		Assert.assertEquals(10,ndFSM.getStateNumber());
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Collections.emptyList(),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","b"),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","a"),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","a","a"),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","a","a","a"),config,converter),fsm.findVertex("A")));
	}

	/** Tests non-deterministic case of {@link MarkovClassifier#tracePath}, deterministic case is tested with {@link statechum.analysis.learning.TestPathTracing}. */
	@Test
	public void testTracePath4()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		LearnerGraphND ndFSM = new LearnerGraphND(fsm,config);
		synchronized(AbstractLearnerGraph.syncObj)
		{
			ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("B"), true, lblA),true,lblA),true,lblA),true,lblA);
			ndFSM.addVertex(ndFSM.addVertex(ndFSM.addVertex(fsm.findVertex("B"), true, lblA),true,lblA),true,lblC);
		}
		Assert.assertEquals(10,ndFSM.getStateNumber());
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Collections.emptyList(),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","b"),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","a","a"),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","a","a","a"),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","a","c"),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","a","a","c"),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","a","a","a","a"),config,converter),fsm.findVertex("A")));
	}
	
	/** Tests non-deterministic case of {@link MarkovClassifier#tracePath}, deterministic case is tested with {@link statechum.analysis.learning.TestPathTracing}. */
	@Test
	public void testTracePath5()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
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
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Collections.emptyList(),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","b"),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","a","a"),config,converter),fsm.findVertex("A")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","a","a","a"),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","a","c"),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","b"),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","b","c"),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","a","a","c"),config,converter),fsm.findVertex("A")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("t","a","a","a","a","a"),config,converter),fsm.findVertex("A")));

		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Collections.emptyList(),config,converter),fsm.findVertex("B")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Collections.singletonList("b"),config,converter),fsm.findVertex("B")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","a"),config,converter),fsm.findVertex("B")));
		Assert.assertTrue(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","a","a"),config,converter),fsm.findVertex("B")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","c"),config,converter),fsm.findVertex("B")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","b"),config,converter),fsm.findVertex("B")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","b","c"),config,converter),fsm.findVertex("B")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","a","c"),config,converter),fsm.findVertex("B")));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Arrays.asList("a","a","a","a","a"),config,converter),fsm.findVertex("B")));

		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Collections.emptyList(),config,converter),Brej));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Collections.singletonList("t"),config,converter),Brej));
		Assert.assertFalse(MarkovClassifier.tracePath(ndFSM,AbstractLearnerGraph.buildList(Collections.singletonList("b"),config,converter),Brej));
	}
	
	
	public String collectionOfTransitionsToString(Collection<Map.Entry<Label,CmpVertex>> c)
	{
		StringBuilder outcome = new StringBuilder();
		for(Map.Entry<Label,CmpVertex> entry:c)
		{
			outcome.append('{');outcome.append(entry.getKey().toString());outcome.append(',');outcome.append(entry.getValue().toString());outcome.append('}');
		}
		return outcome.toString();
	}
	
	@Test
	public void testConstructSurroundingTransitions1a()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = WaveBlueFringe.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("A"));
		Assert.assertEquals("{t,B}",collectionOfTransitionsToString(surroundingTransitions));
		Assert.assertEquals(1,WaveBlueFringe.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("A")));
		Assert.assertEquals(2,WaveBlueFringe.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("B")));
		Assert.assertEquals(1,WaveBlueFringe.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("C")));
		Assert.assertEquals(fsm.findVertex("B"),WaveBlueFringe.findVertexWithMostTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),0));
	}
	
	/** Tests that the second best can be returned. */
	@Test
	public void testConstructSurroundingTransitions1b()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		Assert.assertEquals(fsm.getInit(),WaveBlueFringe.findVertexWithMostTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),1));
	}

	/** Tests that the third best can be returned. */
	@Test
	public void testConstructSurroundingTransitions1c()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->A-t->B-b->C-b->C", "testTracePath1",config,converter);
		Assert.assertEquals(fsm.findVertex(VertexID.parseID("C")),WaveBlueFringe.findVertexWithMostTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),2));
	}

	/** Tests that fourth best is the same as third best. */
	@Test
	public void testConstructSurroundingTransitions1d()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->A-t->B-b->C", "testTracePath1",config,converter);
		Assert.assertEquals(fsm.findVertex(VertexID.parseID("C")),WaveBlueFringe.findVertexWithMostTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),3));
	}

	@Test
	public void testConstructSurroundingTransitions2()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = WaveBlueFringe.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("B"));
		Assert.assertEquals("{b,C}{t,A}",collectionOfTransitionsToString(surroundingTransitions));
	}
	
	@Test
	public void testConstructSurroundingTransitions3()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = WaveBlueFringe.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("C"));
		Assert.assertEquals("{b,B}",collectionOfTransitionsToString(surroundingTransitions));
	}
	
	/** Now with RED labels. */
	@Test
	public void testConstructSurroundingTransitions4()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		fsm.findVertex("B").setColour(JUConstants.RED);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = WaveBlueFringe.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("C"));
		Assert.assertEquals("",collectionOfTransitionsToString(surroundingTransitions));
	}

	/** This one contains self-loops that should only be reported once. */ 
	@Test
	public void testConstructSurroundingTransitions5()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-u->B-b->C / A-a->A / A-b->B / A-c->B /T-a->B/B-a->A/B-g->T/B-c->Z/B-p->B-q->B/B-u->A", "testConstructSurroundingTransitions5",config,converter);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = WaveBlueFringe.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("B"));
		Assert.assertEquals("{a,A}{b,C}{c,Z}{g,T}{p,B}{q,B}{u,A}{a,T}{b,A}{c,A}{u,A}",collectionOfTransitionsToString(surroundingTransitions));

		Assert.assertEquals(6,WaveBlueFringe.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("A")));
		Assert.assertEquals(11,WaveBlueFringe.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("B")));
		Assert.assertEquals(1,WaveBlueFringe.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("C")));
		Assert.assertEquals(fsm.findVertex("B"),WaveBlueFringe.findVertexWithMostTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),0));
	}
	
	
	/** Similar to above, but with RED states. */
	@Test
	public void testConstructSurroundingTransitions6()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-u->B-b->C / A-a->A / A-b->B / A-c->B /T-a->B/B-a->A/B-g->T/B-c->Z/B-p->B/B-u->A", "testConstructSurroundingTransitions5",config,converter);
		fsm.findVertex("B").setColour(JUConstants.RED);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = WaveBlueFringe.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("B"));
		Assert.assertEquals("{a,A}{b,C}{c,Z}{g,T}{u,A}{a,T}{b,A}{c,A}{u,A}",collectionOfTransitionsToString(surroundingTransitions));

		Assert.assertEquals(6,WaveBlueFringe.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("A")));
		Assert.assertEquals(10,WaveBlueFringe.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("B")));
		Assert.assertEquals(1,WaveBlueFringe.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.findVertex("C")));
	}
	
	/** Similar to above, but with RED states. */
	@Test
	public void testConstructSurroundingTransitions7()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-u->B-b->C / A-a->A / A-b->B / A-c->B /T-a->B/B-a->A/B-g->T/B-c->Z/B-p->B/B-u->A", "testConstructSurroundingTransitions5",config,converter);
		fsm.findVertex("B").setColour(JUConstants.RED);fsm.findVertex("A").setColour(JUConstants.RED);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = WaveBlueFringe.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("B"));
		Assert.assertEquals("{b,C}{c,Z}{g,T}{a,T}",collectionOfTransitionsToString(surroundingTransitions));
	}
	
	/** Similar to above, but with RED states. */
	@Test
	public void testConstructSurroundingTransitions8()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-u->B-b->C / A-a->A / A-b->B / A-c->B /T-a->B/B-a->A/B-g->T/B-c->Z/B-p->B/B-u->A", "testConstructSurroundingTransitions5",config,converter);
		fsm.findVertex("A").setColour(JUConstants.RED);
		Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = WaveBlueFringe.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("B"));
		Assert.assertEquals("{b,C}{c,Z}{g,T}{p,B}{a,T}",collectionOfTransitionsToString(surroundingTransitions));
	}
	
	@Test
	public void testConstructSurroundingTransitions9()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-b->C", "testTracePath1",config,converter);
		final Collection<Map.Entry<Label,CmpVertex>> surroundingTransitions = WaveBlueFringe.obtainSurroundingTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm), fsm.findVertex("A"));
		TestHelper.checkForCorrectException(
				() -> surroundingTransitions.iterator().next().setValue(fsm.findVertex("A")),
				UnsupportedOperationException.class, "changing values of this map entry is not permitted");
	}
	
	@Test
	public void testConstructSurroundingTransitions10()
	{
		final LearnerGraph fsm = new LearnerGraph(config);
		Assert.assertEquals(0,WaveBlueFringe.countTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),fsm.getInit()));
		Assert.assertEquals(fsm.getInit(),WaveBlueFringe.findVertexWithMostTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),0));
	}	
	@Test
	public void testConstructSurroundingTransitions11()
	{
		final LearnerGraph fsm = new LearnerGraph(config);fsm.initEmpty();
		Assert.assertNull(WaveBlueFringe.findVertexWithMostTransitions(fsm, MarkovClassifier.computeInverseGraph(fsm),0));
	}
		
	@Test
	public void testUniquePaths1()
	{
		final LearnerGraph fsm = new LearnerGraph(config);fsm.initEmpty();
		TestHelper.checkForCorrectException(
				() -> MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(fsm),
				IllegalArgumentException.class, "empty reference");
		TestHelper.checkForCorrectException(
				() -> MarkovClassifier.calculateFractionOfIdentifiedStates(fsm,null),
				IllegalArgumentException.class, "empty reference");
	}
	
	@Test
	public void testUniquePaths2a()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-u->C-s->C", "testUniquePaths2",config,converter);
		Assert.assertEquals(1.,MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(fsm),Configuration.fpAccuracy);
		Assert.assertEquals(0,MarkovClassifier.calculateFractionOfIdentifiedStates(fsm,TestFSMAlgo.buildSet(new String[][]{
				new String[]{"b"},
				new String[]{"b","a"}},config,converter)),Configuration.fpAccuracy);
	}

	@Test
	public void testUniquePaths2b()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-b->C", "testUniquePaths2",config,converter);
		Assert.assertEquals(2/3.,MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(fsm),Configuration.fpAccuracy);
		Assert.assertEquals(0,MarkovClassifier.calculateFractionOfIdentifiedStates(fsm,TestFSMAlgo.buildSet(new String[][]{
				new String[]{}}
		,config,converter)),Configuration.fpAccuracy);
	}

	@Test
	public void testUniquePaths2c()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-u->C-s->C", "testUniquePaths2",config,converter);
		Assert.assertEquals(1.,MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(fsm),Configuration.fpAccuracy);
		Assert.assertEquals(0,MarkovClassifier.calculateFractionOfIdentifiedStates(fsm,TestFSMAlgo.buildSet(new String[][]{}
		,config,converter)),Configuration.fpAccuracy);
	}


	@Test
	public void testUniquePaths2d()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-u->C-s->C", "testUniquePaths2",config,converter);
		Assert.assertEquals(1.,MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(fsm),Configuration.fpAccuracy);
		Assert.assertEquals(1./3,MarkovClassifier.calculateFractionOfIdentifiedStates(fsm,TestFSMAlgo.buildSet(new String[][]{
				new String[]{"t"}
				}
		,config,converter)),Configuration.fpAccuracy);
	}

	@Test
	public void testUniquePaths2e()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-u->C-s->C", "testUniquePaths2",config,converter);
		Assert.assertEquals(1.,MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(fsm),Configuration.fpAccuracy);
		Assert.assertEquals(1./3,MarkovClassifier.calculateFractionOfIdentifiedStates(fsm,TestFSMAlgo.buildSet(new String[][]{
				new String[]{"s","s"}}
		,config,converter)),Configuration.fpAccuracy);
	}

	@Test
	public void testUniquePaths2f()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-b->C", "testUniquePaths2",config,converter);
		Assert.assertEquals(2/3.,MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(fsm),Configuration.fpAccuracy);
		Assert.assertEquals(1/3.,MarkovClassifier.calculateFractionOfIdentifiedStates(fsm,TestFSMAlgo.buildSet(new String[][]{
				new String[]{"b"},
				new String[]{"b","a"}}
		,config,converter)),Configuration.fpAccuracy);
	}

	
	@Test
	public void testUniquePaths3()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-b->C-b->C", "testUniquePaths2",config,converter);
		Assert.assertEquals(1/3.,MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(fsm),Configuration.fpAccuracy);		
	}

	@Test
	public void testUniquePaths4()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B", "testUniquePaths2",config,converter);
		Assert.assertEquals(0.5,MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(fsm),Configuration.fpAccuracy);		
	}
	
	@Test
	public void testUniquePaths5()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-t-#C", "testUniquePaths2",config,converter);
		Assert.assertEquals(1/3.,MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(fsm),Configuration.fpAccuracy);		
	}
	
	@Test
	public void testUniquePaths6()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-t->B-t-#C / A-a->A", "testUniquePaths2",config,converter);
		Assert.assertEquals(1/3.,MarkovClassifier.calculateFractionOfStatesIdentifiedBySingletons(fsm),Configuration.fpAccuracy);		
	}
	
	@Test
	public void testMarkovPerformance1()
	{
		final LearnerGraph trainingGraph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		MarkovClassifierLG cl=new MarkovClassifierLG(m,trainingGraph,null);cl.updateMarkov(false);
		statechum.Pair<Double,Double> pairTraining = cl.evaluateCorrectnessOfMarkov();
		Assert.assertEquals(2./3,pairTraining.firstElem,Configuration.fpAccuracy);// reflects that transitions u and c from G are not present but predicted
		Assert.assertEquals(2./3.,pairTraining.secondElem,Configuration.fpAccuracy);// reflects that transitions a and c are not predicted but present.
		
		MarkovClassifierLG eval = new MarkovClassifierLG(m, FsmParserStatechum.buildLearnerGraph("A-t->B","testMarkovPerformance1a",config, converter),null);
		statechum.Pair<Double,Double> pair = eval.evaluateCorrectnessOfMarkov();
		Assert.assertEquals(0,pair.firstElem,Configuration.fpAccuracy);Assert.assertEquals(0,pair.secondElem,Configuration.fpAccuracy);

		MarkovClassifierLG evalB = new MarkovClassifierLG(m, FsmParserStatechum.buildLearnerGraph("A-a->B","testMarkovPerformance1b",config, converter),null);
		statechum.Pair<Double,Double> pairB = evalB.evaluateCorrectnessOfMarkov();
		Assert.assertEquals(0,pairB.firstElem,Configuration.fpAccuracy);Assert.assertEquals(0,pairB.secondElem,Configuration.fpAccuracy);
	}
	
	@Test
	public void testMarkovPerformance2()
	{
		final LearnerGraph trainingGraph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		MarkovClassifierLG cl=new MarkovClassifierLG(m,trainingGraph,null);cl.updateMarkov(false);
		
		MarkovClassifierLG eval = new MarkovClassifierLG(m, FsmParserStatechum.buildLearnerGraph("A-a->B-u-#D / B-b->G","testMarkovPerformance2",config, converter),null);
		statechum.Pair<Double,Double> pair = eval.evaluateCorrectnessOfMarkov();
		Assert.assertEquals(1,pair.firstElem,Configuration.fpAccuracy);Assert.assertEquals(2./3,pair.secondElem,Configuration.fpAccuracy);// transition a is not predicted
	}
	
	@Test
	public void testMarkovPerformance3()
	{
		final LearnerGraph trainingGraph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		MarkovClassifierLG cl=new MarkovClassifierLG(m,trainingGraph,null);cl.updateMarkov(false);
		
		MarkovClassifierLG eval = new MarkovClassifierLG(m, FsmParserStatechum.buildLearnerGraph("A-a->B-u-#D / B-b->G / B-e->Z","testMarkovPerformance3",config, converter),null);
		statechum.Pair<Double,Double> pair = eval.evaluateCorrectnessOfMarkov();
		Assert.assertEquals(1,pair.firstElem,Configuration.fpAccuracy);Assert.assertEquals(0.5,pair.secondElem,Configuration.fpAccuracy);// transition a is not predicted
	}
	
	@Test
	public void testMarkovPerformance4()
	{
		final LearnerGraph trainingGraph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		MarkovClassifierLG cl=new MarkovClassifierLG(m,trainingGraph,null);cl.updateMarkov(false);
		
		MarkovClassifierLG eval = new MarkovClassifierLG(m, FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-c->D-u->E","testMarkovPerformance4",config, converter),null);
		statechum.Pair<Double,Double> pair = eval.evaluateCorrectnessOfMarkov();
		Assert.assertEquals(3./4,pair.firstElem,Configuration.fpAccuracy);// u is predicted as negative and is indeed missing, b is correctly predicted as a positive; u after c is correctly predicted as positive and c after c is not correctly predicted.
		Assert.assertEquals(0.5,pair.secondElem,Configuration.fpAccuracy);// transition a is not predicted
	}
	
	@Test
	public void testMarkovPerformance5()
	{
		final LearnerGraph trainingGraph = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C / B-u-#D / A-c->E-u->F / E-c->G","testUpdateMarkovSideways3",config, converter);
		MarkovModel m = new MarkovModel(2,true, true,true,markovPTAUseMatrix);
		MarkovClassifierLG cl=new MarkovClassifierLG(m,trainingGraph,null);cl.updateMarkov(false);
		
		MarkovClassifierLG eval = new MarkovClassifierLG(m, FsmParserStatechum.buildLearnerGraph("A-a->B-b->G","testMarkovPerformance5",config, converter),null);
		statechum.Pair<Double,Double> pair = eval.evaluateCorrectnessOfMarkov();
		Assert.assertEquals(1,pair.firstElem,Configuration.fpAccuracy);// u is predicted as negative and is indeed missing, b is correctly predicted as a positive
		Assert.assertEquals(0.5,pair.secondElem,Configuration.fpAccuracy);// transition a is not predicted
	}
	
	@Test
	public void testComputeClosure1()
	{
		Set<CmpVertex> verts= new TreeSet<>();
		MarkovClassifier.computeClosure(trainingGraphForClosures,verts,0);
		Assert.assertTrue(verts.isEmpty());
	}
	
	@Test
	public void testComputeClosure2()
	{
		Set<CmpVertex> verts= new TreeSet<>();
		MarkovClassifier.computeClosure(trainingGraphForClosures,verts,2);
		Assert.assertTrue(verts.isEmpty());
	}
	
	@Test
	public void testComputeClosure3()
	{
		Set<CmpVertex> verts= new TreeSet<>();verts.add(trainingGraphForClosures.findVertex("B"));
		MarkovClassifier.computeClosure(trainingGraphForClosures,verts,0);
		Assert.assertEquals(1,verts.size());Assert.assertSame(trainingGraphForClosures.findVertex("B"),verts.iterator().next());
	}
	
	@Test
	public void testComputeClosure4()
	{
		Set<CmpVertex> verts= new TreeSet<>();verts.add(trainingGraphForClosures.findVertex("B"));
		MarkovClassifier.computeClosure(trainingGraphForClosures,verts,1);
		Assert.assertEquals(2,verts.size());
		Iterator<CmpVertex> iter=verts.iterator();Assert.assertSame(trainingGraphForClosures.findVertex("B"),iter.next());Assert.assertSame(trainingGraphForClosures.findVertex("C"),iter.next());
	}
	
	@Test
	public void testComputeClosure5()
	{
		Set<CmpVertex> verts= new TreeSet<>();verts.add(trainingGraphForClosures.findVertex("B"));
		MarkovClassifier.computeClosure(trainingGraphForClosures,verts,2);
		Assert.assertEquals(3,verts.size());
		Iterator<CmpVertex> iter=verts.iterator();Assert.assertSame(trainingGraphForClosures.findVertex("B"),iter.next());Assert.assertSame(trainingGraphForClosures.findVertex("C"),iter.next());Assert.assertSame(trainingGraphForClosures.findVertex("D"),iter.next());
	}
	
	@Test
	public void testExistenceOfPathsFromVertex1()
	{
		Assert.assertTrue(MarkovClassifier.checkIfThereIsPathOfSpecificLength(trainingGraphForClosures,trainingGraphForClosures.findVertex("E"),0));
		Assert.assertFalse(MarkovClassifier.checkIfThereIsPathOfSpecificLength(trainingGraphForClosures,trainingGraphForClosures.findVertex("E"),1));
		Assert.assertTrue(MarkovClassifier.checkIfThereIsPathOfSpecificLength(trainingGraphForClosures,trainingGraphForClosures.findVertex("E"),0));
		Assert.assertFalse(MarkovClassifier.checkIfThereIsPathOfSpecificLength(trainingGraphForClosures,trainingGraphForClosures.findVertex("E"),1));
	}
	
	
	@Test
	public void testExistenceOfPathsFromVertex2()
	{
		for(int i=0;i<100;++i) // due to a loop, paths of any length are possible
			Assert.assertTrue(MarkovClassifier.checkIfThereIsPathOfSpecificLength(trainingGraphForClosures,trainingGraphForClosures.findVertex("A"),i));
	}
	
	
	@Test
	public void testExistenceOfPathsFromVertex3()
	{
		Assert.assertTrue(MarkovClassifier.checkIfThereIsPathOfSpecificLength(trainingGraphForClosures,trainingGraphForClosures.findVertex("D"),0));
		Assert.assertTrue(MarkovClassifier.checkIfThereIsPathOfSpecificLength(trainingGraphForClosures,trainingGraphForClosures.findVertex("D"),1));
		Assert.assertFalse(MarkovClassifier.checkIfThereIsPathOfSpecificLength(trainingGraphForClosures,trainingGraphForClosures.findVertex("D"),2));// only paths of length 1 are possible from D
	}
	
}

