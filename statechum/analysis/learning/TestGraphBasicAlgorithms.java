/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

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
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.ArrayOperations;
import statechum.StringVertex;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.xmachine.model.testset.PTASequenceSetAutomaton;
import statechum.xmachine.model.testset.PTATestSequenceEngine;

@RunWith(Parameterized.class)
public class TestGraphBasicAlgorithms extends RPNIBlueFringeLearnerTestComponent
{
	@Parameters
	public static Collection<Object[]> data() 
	{
		return Configuration.configurationsForTesting();
	}
	
	public TestGraphBasicAlgorithms(Configuration conf) {
		super(null);mainConfiguration = conf;
	}

	@Before
	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	public void beforeTest()
	{
		config = (Configuration)mainConfiguration.clone();
	}

	/** The configuration to use when running tests. */
	private Configuration config = null, mainConfiguration = null;

	static public PairScore constructPairScore(String a,String b, int score, Configuration config)
	{
		CmpVertex aV = LearnerGraph.generateNewCmpVertex(a, config), bV = LearnerGraph.generateNewCmpVertex(b,config);
		return new PairScore(aV,bV, score,score);
	}

	static protected void checkLess(String a, String b, int abS, String c, String d, int cdS, Configuration config)
	{
		StatePair p = constructPairScore(a,b,abS,config), q=constructPairScore(c,d,cdS,config);
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
		StatePair p = constructPairScore("a","b",4,config), q=constructPairScore("a","b",4,config);
		assertTrue(p.equals(p));
		assertTrue(p.equals(q));
		assertFalse(p.equals(null));
		assertFalse(p.equals("test"));
		assertFalse(p.equals(constructPairScore("a","c",4,config)));
		assertFalse(p.equals(constructPairScore("a","b",6,config)));
		assertFalse(p.equals(constructPairScore("b","b",4,config)));
	}
	
	@Test
	public void testStatePairScoreComparison()
	{
		checkLess("a","b",4,"a","b",6,config);
		checkLess("z","z",4,"a","b",6,config);
		checkLess("a","b",4,"z","z",6,config);

		checkLess("a","b",4,"c","d",4,config);
		checkLess("a","b",4,"a","c",4,config);
		checkLess("a","b",4,"c","b",4,config);
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
		DeterministicVertex p = new DeterministicVertex("A"), q= new DeterministicVertex("B");
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
		assertTrue(p.equals(q));
		assertTrue(p.compareTo(q)==0);
	}

	@Test
	public final void testFindVertex0()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testFindVertex"), config);
		Assert.assertNull(s.findVertex("Z"));
		Assert.assertEquals("A", s.findVertex("A").getName());
		Assert.assertEquals("C", s.findVertex("C").getName());
	}
	
	@Test
	public final void testFindVertex1()
	{
		LearnerGraph s = new LearnerGraph(config);
		Assert.assertNull(s.findVertex("Z"));
		Assert.assertEquals("Init", s.findVertex("Init").getName());
	}
	
	/** Similar to testComputePathsToRed2 but tests the contents of a set returned by computePathsSBetween. */
	@Test
	public final void testComputePathsSBetween1()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->A", "testComputePathsSBetween1"), config);
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.paths.computePathsSBetween(s.findVertex("A"), s.findVertex("C"),initSet,paths);
		Map<String,String> actual = engine.getDebugDataMap(paths),expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void testComputePathsSBetween2()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->A", "testComputePathsSBetween2"), config);
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.paths.computePathsSBetween(s.findVertex("A"), s.findVertex("A"),initSet,paths);
		Map<String,String> actual = engine.getDebugDataMap(paths),expected=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	/** Similar to testComputePathsToRed3 but tests that all four nodes are returned. */
	@Test
	public final void testComputePathsSBetween3()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C", "testComputePathsSBetween3"), config);
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.paths.computePathsSBetween(s.findVertex("A"), s.findVertex("C"),initSet,paths);
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
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsSBetween4"), config);
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.paths.computePathsSBetween(s.findVertex("A"), s.findVertex("C"),initSet,paths);
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
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsSBetween4"), config);
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity();
		Collection<List<String>> initSeq = TestFSMAlgo.buildSet(new String[][]{
				new String[] { "sequenceA","sequenceB" },
		});
		initSet = initSet.cross(initSeq);
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.paths.computePathsSBetween(s.findVertex("A"), s.findVertex("C"),initSet,paths);
		Map<String,String> actual = engine.getDebugDataMap(paths),expectedSrc=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"p","q"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Map<String,String> expected = new HashMap<String,String>();
		for(Entry<String,String> expSrc:expectedSrc.entrySet()) for(List<String> is:initSeq) expected.put(ArrayOperations.seqToString(is)+ArrayOperations.separator+expSrc.getKey(), expSrc.getValue());
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** Similar to testComputePathsToRed5 but tests that all four nodes are concatenated with existing sequences. */
	@Test
	public final void testComputePathsSBetween6()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsSBetween4"), config);
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity();
		Collection<List<String>> initSeq = TestFSMAlgo.buildSet(new String[][]{
				new String[] { "sequenceA","sequenceB" },
				new String[] { "sequenceC" },
				new String[] { "sA","sB","sC"}
		});
		initSet = initSet.cross(initSeq);
		PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
		s.paths.computePathsSBetween(s.findVertex("A"), s.findVertex("C"),initSet,paths);
		Map<String,String> actual = engine.getDebugDataMap(paths),expectedSrc=TestFSMAlgo.buildStringMap(new Object[][] {
				new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"p","q"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Map<String,String> expected = new HashMap<String,String>();
		for(Entry<String,String> expSrc:expectedSrc.entrySet()) for(List<String> is:initSeq) expected.put(ArrayOperations.seqToString(is)+ArrayOperations.separator+expSrc.getKey(), expSrc.getValue());
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testComputePathsToRed0a()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testComputePathsToRed1"), config);
		Set<List<String>> expected = buildSet(new String[][] {
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.paths.computePathsToRed(new StringVertex("non-existing-vertex")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testComputePathsToRed0b()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testComputePathsToRed1"), config);
		Set<List<String>> expected = buildSet(new String[][] {
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.paths.computePathsToRed(null));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testComputePathsToRed0c()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nQ-a->Q", "testComputePathsToRed1"), config);
		Set<List<String>> expected = buildSet(new String[][] {
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.paths.computePathsToRed(s.findVertex("Q")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** State to look for is the initial one. */
	@Test
	public final void testComputePathsToRed1()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->A", "testComputePathsToRed1"), config);
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.paths.computePathsToRed(s.findVertex("A")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** State to look for is the last one, but there is only one path. */
	@Test
	public final void testComputePathsToRed2()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->A", "testComputePathsToRed2"), config);
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {"a","b"}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.paths.computePathsToRed(s.findVertex("C")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** Single path but parallel transitions in Jung. */
	@Test
	public final void testComputePathsToRed3()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C", "testComputePathsToRed3"), config);
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {"a","b"},
				new String[] {"a","d"},
				new String[] {"c","b"},
				new String[] {"c","d"}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.paths.computePathsToRed(s.findVertex("C")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** Single path but parallel transitions in Jung; the shortest path is really short. */
	@Test
	public final void testComputePathsToRed4()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->C\nA-q->C", "testComputePathsToRed4"), config);
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {"p"},
				new String[] {"q"}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.paths.computePathsToRed(s.findVertex("C")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	/** Multiple paths to target state. */
	@Test
	public final void testComputePathsToRed5()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsToRed5"), config);
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {"a","b"},
				new String[] {"a","d"},
				new String[] {"c","b"},
				new String[] {"c","d"},
				new String[] {"p","q"}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.paths.computePathsToRed(s.findVertex("C")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}

	@Test
	public final void testGetVertex1()
	{
		LearnerGraph score = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-b->D\n","testFindVertex1"), config);
		Assert.assertTrue(score.getVertex(new LinkedList<String>()).getName().equals("A"));
	}

	@Test
	public final void testGetVertex2()
	{
		LearnerGraph score = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-b->D\n","testFindVertex2"), config);
		Assert.assertTrue(score.getVertex(Arrays.asList(new String[]{"a","b"})).getName().equals("C"));
	}

	@Test
	public final void testGetVertex3()
	{
		LearnerGraph score = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-b->D\n","testFindVertex3"), config);
		Assert.assertNull(score.getVertex(Arrays.asList(new String[]{"a","d"})));
	}

	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{
		LearnerGraph.testMode = true;
		Visualiser.disposeFrame();
	}
	
	@AfterClass
	public static void cleanUp()
	{
		Visualiser.disposeFrame();
	}
}
