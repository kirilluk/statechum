package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static statechum.analysis.learning.TestFSMAlgo.buildSet;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.ComputeStateScores.PairScore;
import statechum.xmachine.model.testset.PTASequenceSetAutomaton;
import statechum.xmachine.model.testset.PTATestSequenceEngine;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class TestGraphBasicAlgorithms {
	static public PairScore constructPairScore(String a,String b, int score)
	{
		DirectedSparseVertex aV = new DirectedSparseVertex(), bV = new DirectedSparseVertex();
		aV.addUserDatum(JUConstants.LABEL, a, UserData.SHARED);
		bV.addUserDatum(JUConstants.LABEL, b, UserData.SHARED);
		return new ComputeStateScores.PairScore(aV,bV, score,score);
	}

	static protected void checkLess(String a, String b, int abS, String c, String d, int cdS)
	{
		StatePair p = constructPairScore(a,b,abS), q=constructPairScore(c,d,cdS);
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}

	@Test
	public void testPairScoreEquality()
	{
		StatePair p = constructPairScore("a","b",4), q=constructPairScore("a","b",4);
		assertTrue(p.equals(p));
		assertTrue(p.equals(q));
		assertFalse(p.equals(null));
		assertFalse(p.equals("test"));
		assertFalse(p.equals(constructPairScore("a","c",4)));
		assertFalse(p.equals(constructPairScore("a","b",6)));
		assertFalse(p.equals(constructPairScore("b","b",4)));
	}
	
	@Test
	public void testStatePairScoreComparison()
	{
		checkLess("a","b",4,"a","b",6);
		checkLess("z","z",4,"a","b",6);
		checkLess("a","b",4,"z","z",6);

		checkLess("a","b",4,"c","d",4);
		checkLess("a","b",4,"a","c",4);
		checkLess("a","b",4,"c","b",4);
	}

	@Test
	public void testDeterministicVertexComparison1()
	{
		DeterministicVertex p = new DeterministicVertex("P"), q= new DeterministicVertex("Q");
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}
		
	@Test
	public void testDeterministicVertexComparison2()
	{
		DeterministicVertex p = new DeterministicVertex(), q= new DeterministicVertex();
		p.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		q.addUserDatum(JUConstants.LABEL, "B", UserData.SHARED);
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}

	@Test
	public void testDeterministicVertexComparison3()
	{
		DeterministicVertex p = new DeterministicVertex("P"), q= new DeterministicVertex("P");
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)==0);
	}

	@Test
	public final void testFindVertex0()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testFindVertex"));
		Assert.assertNull(s.findVertex("Z"));
		Assert.assertEquals("A", s.findVertex("A").getUserDatum(JUConstants.LABEL));
		Assert.assertEquals("C", s.findVertex("C").getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public final void testFindVertex1()
	{
		ComputeStateScores s = new ComputeStateScores(0);
		Assert.assertNull(s.findVertex("Z"));
		Assert.assertEquals("Init", s.findVertex("Init").getUserDatum(JUConstants.LABEL));
	}
	
	/** Similar to testComputePathsToRed2 but tests the contents of a set returned by computePathsSBetween. */
	@Test
	public final void testComputePathsSBetween1()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->A", "testComputePathsSBetween1"));
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.computePathsSBetween(s.findVertex("A"), s.findVertex("C"),initSet,paths);
		Map<String,String> actual = engine.getDebugDataMap(paths),expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void testComputePathsSBetween2()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->A", "testComputePathsSBetween2"));
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.computePathsSBetween(s.findVertex("A"), s.findVertex("A"),initSet,paths);
		Map<String,String> actual = engine.getDebugDataMap(paths),expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	/** Similar to testComputePathsToRed3 but tests that all four nodes are returned. */
	@Test
	public final void testComputePathsSBetween3()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C", "testComputePathsSBetween3"));
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.computePathsSBetween(s.findVertex("A"), s.findVertex("C"),initSet,paths);
		Map<String,String> actual = engine.getDebugDataMap(paths),expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	/** Similar to testComputePathsToRed5 but tests that all four nodes are returned. */
	@Test
	public final void testComputePathsSBetween4()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsSBetween4"));
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.computePathsSBetween(s.findVertex("A"), s.findVertex("C"),initSet,paths);
		Map<String,String> actual = engine.getDebugDataMap(paths),expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"p","q"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	/** Similar to testComputePathsToRed5 but tests that all four nodes are concatenated with existing sequences. */
	@Test
	public final void testComputePathsSBetween5()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsSBetween4"));
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity();
		Collection<List<String>> initSeq = TestFSMAlgo.buildSet(new String[][]{
				new String[] { "sequenceA","sequenceB" },
		});
		initSet = initSet.cross(initSeq);
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.computePathsSBetween(s.findVertex("A"), s.findVertex("C"),initSet,paths);
		Map<String,String> actual = engine.getDebugDataMap(paths),expectedSrc=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"p","q"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Map<String,String> expected = new HashMap<String,String>();
		for(Entry<String,String> expSrc:expectedSrc.entrySet()) for(List<String> is:initSeq) expected.put(PTATestSequenceEngine.seqToString(is)+PTATestSequenceEngine.separator+expSrc.getKey(), expSrc.getValue());
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** Similar to testComputePathsToRed5 but tests that all four nodes are concatenated with existing sequences. */
	@Test
	public final void testComputePathsSBetween6()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsSBetween4"));
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity();
		Collection<List<String>> initSeq = TestFSMAlgo.buildSet(new String[][]{
				new String[] { "sequenceA","sequenceB" },
				new String[] { "sequenceC" },
				new String[] { "sA","sB","sC"}
		});
		initSet = initSet.cross(initSeq);
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.computePathsSBetween(s.findVertex("A"), s.findVertex("C"),initSet,paths);
		Map<String,String> actual = engine.getDebugDataMap(paths),expectedSrc=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"p","q"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Map<String,String> expected = new HashMap<String,String>();
		for(Entry<String,String> expSrc:expectedSrc.entrySet()) for(List<String> is:initSeq) expected.put(PTATestSequenceEngine.seqToString(is)+PTATestSequenceEngine.separator+expSrc.getKey(), expSrc.getValue());
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testComputePathsToRed0a()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testComputePathsToRed1"));
		Set<List<String>> expected = buildSet(new String[][] {
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(new DirectedSparseVertex()));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testComputePathsToRed0b()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testComputePathsToRed1"));
		Set<List<String>> expected = buildSet(new String[][] {
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(null));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testComputePathsToRed0c()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nQ-a->Q", "testComputePathsToRed1"));
		Set<List<String>> expected = buildSet(new String[][] {
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(s.findVertex("Q")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** State to look for is the initial one. */
	@Test
	public final void testComputePathsToRed1()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->A", "testComputePathsToRed1"));
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(s.findVertex("A")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** State to look for is the last one, but there is only one path. */
	@Test
	public final void testComputePathsToRed2()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->A", "testComputePathsToRed2"));
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {"a","b"}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(s.findVertex("C")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** Single path but parallel transitions in Jung. */
	@Test
	public final void testComputePathsToRed3()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C", "testComputePathsToRed3"));
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {"a","b"},
				new String[] {"a","d"},
				new String[] {"c","b"},
				new String[] {"c","d"}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(s.findVertex("C")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** Single path but parallel transitions in Jung; the shortest path is really short. */
	@Test
	public final void testComputePathsToRed4()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->C\nA-q->C", "testComputePathsToRed4"));
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {"p"},
				new String[] {"q"}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(s.findVertex("C")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** Multiple paths to target state. */
	@Test
	public final void testComputePathsToRed5()
	{
		ComputeStateScores s = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsToRed5"));
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {"a","b"},
				new String[] {"a","d"},
				new String[] {"c","b"},
				new String[] {"c","d"},
				new String[] {"p","q"}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(s.findVertex("C")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void testGetVertex1()
	{
		ComputeStateScores score = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-a->C-b->D\n","testFindVertex1"));
		Assert.assertTrue(score.getVertex(new LinkedList<String>()).getUserDatum(JUConstants.LABEL).equals("A"));
	}

	@Test
	public final void testGetVertex2()
	{
		ComputeStateScores score = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-b->D\n","testFindVertex2"));
		Assert.assertTrue(score.getVertex(Arrays.asList(new String[]{"a","b"})).getUserDatum(JUConstants.LABEL).equals("C"));
	}

	@Test
	public final void testGetVertex3()
	{
		ComputeStateScores score = new ComputeStateScores(TestFSMAlgo.buildGraph("A-a->B-a->C-b->D\n","testFindVertex3"));
		Assert.assertNull(score.getVertex(Arrays.asList(new String[]{"a","d"})));
	}

	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{
		ComputeStateScores.testMode = true;
		Visualiser.disposeFrame();
	}
	
	@AfterClass
	public static void cleanUp()
	{
		Visualiser.disposeFrame();
	}
}
