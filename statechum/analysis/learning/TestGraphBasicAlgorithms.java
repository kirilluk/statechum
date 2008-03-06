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

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.ArrayOperations;
import statechum.Configuration;
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
		super(null,conf);mainConfiguration = conf;
	}

	@Before
	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	public void beforeTest()
	{
		testConfig = (Configuration)mainConfiguration.clone();
	}

	/** The configuration to use when running tests. */
	private Configuration testConfig = null, mainConfiguration = null;

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
		StatePair p = constructPairScore("a","b",4,testConfig), q=constructPairScore("a","b",4,testConfig);
		assertTrue(p.equals(p));
		assertTrue(p.equals(q));
		assertFalse(p.equals(null));
		assertFalse(p.equals("test"));
		assertFalse(p.equals(constructPairScore("a","c",4,testConfig)));
		assertFalse(p.equals(constructPairScore("a","b",6,testConfig)));
		assertFalse(p.equals(constructPairScore("b","b",4,testConfig)));
	}
	
	@Test
	public void testStatePairScoreComparison()
	{
		checkLess("a","b",4,"a","b",6,testConfig);
		checkLess("z","z",4,"a","b",6,testConfig);
		checkLess("a","b",4,"z","z",6,testConfig);

		checkLess("a","b",4,"c","d",4,testConfig);
		checkLess("a","b",4,"a","c",4,testConfig);
		checkLess("a","b",4,"c","b",4,testConfig);
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
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testFindVertex"), testConfig);
		Assert.assertNull(s.findVertex("Z"));
		Assert.assertEquals("A", s.findVertex("A").getName());
		Assert.assertEquals("C", s.findVertex("C").getName());
	}
	
	@Test
	public final void testFindVertex1()
	{
		LearnerGraph s = new LearnerGraph(testConfig);
		Assert.assertNull(s.findVertex("Z"));
		Assert.assertEquals("Init", s.findVertex("Init").getName());
	}
	
	/** Helps testing the routines which find the shortest path between a pair of nodes in a graph.
	 * 
	 * @param machine the graph to deal with
	 * @param testName the name to give to the graph
	 * @param FirstState the state to find a path from
	 * @param SecondState the state to find a path to
	 * @param initSeq the set of sequences to which the paths found should be concatenated.
	 * @param expectedResult the expected outcome.
	 */
	private void TestComputePathsBetweenHelper(String machine, String testName, String FirstState, String SecondState, String[][] initSeq, Object[][] expectedResult)
	{
		Map<String,String> expected = new HashMap<String,String>();
		if (initSeq != null)
			for(Entry<String,String> expSrc:TestFSMAlgo.buildStringMap(expectedResult).entrySet())
				for(List<String> is:TestFSMAlgo.buildSet(initSeq)) expected.put(ArrayOperations.seqToString(is)+ArrayOperations.separator+expSrc.getKey(), expSrc.getValue());
		else // initSeq == null
			expected.putAll(TestFSMAlgo.buildStringMap(expectedResult));
		
		{// testing the orig part
			LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph(machine, testName), testConfig);
			PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
			PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
			PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
			if (initSeq != null) initSet=initSet.cross(TestFSMAlgo.buildSet(initSeq));
			s.paths.ORIGcomputePathsSBetween(s.findVertex(FirstState), s.findVertex(SecondState),initSet,paths);
			Map<String,String> actual = engine.getDebugDataMap(paths);
			Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
		}

		{// testing the new part
			LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph(machine, testName), testConfig);
			PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
			PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
			PTATestSequenceEngine.sequenceSet paths = engine.new sequenceSet();
			if (initSeq != null) initSet=initSet.cross(TestFSMAlgo.buildSet(initSeq));
			s.paths.computePathsSBetween(s.findVertex(FirstState), s.findVertex(SecondState),initSet,paths);
			Map<String,String> actual = engine.getDebugDataMap(paths);
			Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
		}
	}
	
	
	/** Similar to testComputePathsToRed2 but tests the contents of a set returned by computePathsSBetween. */
	@Test
	public final void testComputePathsSBetween1()
	{
		TestComputePathsBetweenHelper("A-a->B-b->C-a->A\nA-c->A", "testComputePathsSBetween1", 
				"A","C",
				null,
				new Object [][]{
					new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	@Test
	public final void testComputePathsSBetween2()
	{
		TestComputePathsBetweenHelper("A-a->B-b->C-a->A\nA-c->A", "testComputePathsSBetween2",
				"A","A",
				null,
				new Object[][] {
					new Object[]{new String[] {}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	/** Similar to testComputePathsToRed3 but tests that all four nodes are returned. */
	@Test
	public final void testComputePathsSBetween3()
	{
		TestComputePathsBetweenHelper("A-a->B-b->C-a->A\nA-c->B-d->C", "testComputePathsSBetween3",
				"A","C",
				null,
			new Object[][] {
				new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	/** Similar to testComputePathsToRed5 but tests that all four nodes are returned. */
	@Test
	public final void testComputePathsSBetween4()
	{
		TestComputePathsBetweenHelper("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsSBetween4",
				"A","C",
				null,
			new Object[][] {
				new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"p","q"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	/** Similar to testComputePathsToRed5 but tests that all four nodes are concatenated with existing sequences. */
	@Test
	public final void testComputePathsSBetween5()
	{
		TestComputePathsBetweenHelper("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsSBetween5",
				"A","C",
				new String[][]{
					new String[] { "sequenceA","sequenceB" }},
				new Object[][] {
					new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
					new Object[]{new String[] {"a","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
					new Object[]{new String[] {"c","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
					new Object[]{new String[] {"c","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
					new Object[]{new String[] {"p","q"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}
	
	/** Similar to testComputePathsToRed5 but tests that all four nodes are concatenated with existing sequences. */
	@Test
	public final void testComputePathsSBetween6()
	{
		TestComputePathsBetweenHelper("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsSBetween6",
				"A","C",
				new String[][]{
					new String[] { "sequenceA","sequenceB" },
					new String[] { "sequenceC" },
					new String[] { "sA","sB","sC"}},
				new Object[][] {
					new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
					new Object[]{new String[] {"a","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
					new Object[]{new String[] {"c","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
					new Object[]{new String[] {"c","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
					new Object[]{new String[] {"p","q"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	private final String complexGraphA = "A-a->B-d->C-d->D-a->D-c->D-b->C\nA-c->B-b->C-c->B-e->A-d->A\nB-a->A\nC-b->A\nC-a->E\nC-e->E\nF-a->D\nF-b->D\nG-a->G";
	
	@Test
	public final void testComputePathsSBetween7()
	{
		TestComputePathsBetweenHelper(complexGraphA, "ComputePathsSBetween_complexGraphA",
				"A","A",
				null,
			new Object[][] {
				new Object[]{new String[] {}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	@Test
	public final void testComputePathsSBetween8()
	{
		TestComputePathsBetweenHelper(complexGraphA, "ComputePathsSBetween_complexGraphA",
				"A","B",
				null,
			new Object[][] {
				new Object[]{new String[] {"a"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	@Test
	public final void testComputePathsSBetween9()
	{
		TestComputePathsBetweenHelper(complexGraphA, "ComputePathsSBetween_complexGraphA",
				"A","C",
				null,
			new Object[][] {
				new Object[]{new String[] {"a","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c","d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	@Test
	public final void testComputePathsSBetween10()
	{
		TestComputePathsBetweenHelper(complexGraphA, "ComputePathsSBetween_complexGraphA",
				"C","C",
				null,
			new Object[][] {
				new Object[]{new String[] {}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	/** Similar to testComputePathsToRed5 but tests that all four nodes are returned. */
	@Test
	public final void testComputePathsSBetween11()
	{
		TestComputePathsBetweenHelper(complexGraphA, "ComputePathsSBetween_complexGraphA",
				"D","D",
				null,
			new Object[][] {
				new Object[]{new String[] {}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	@Test
	public final void testComputePathsSBetween12()
	{
		TestComputePathsBetweenHelper(complexGraphA, "ComputePathsSBetween_complexGraphA",
				"F","B",
				null,
			new Object[][] {
				new Object[]{new String[] {"a","b","c"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"b","b","c"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	@Test
	public final void testComputePathsSBetween13()
	{
		TestComputePathsBetweenHelper(complexGraphA, "ComputePathsSBetween_complexGraphA",
				"F","A",
				null,
			new Object[][] {
				new Object[]{new String[] {"a","b","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"b","b","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	@Test
	public final void testComputePathsSBetween14()
	{
		TestComputePathsBetweenHelper(complexGraphA, "ComputePathsSBetween_complexGraphA",
				"C","A",
				null,
			new Object[][] {
				new Object[]{new String[] {"b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	@Test
	public final void testComputePathsSBetween15()
	{
		TestComputePathsBetweenHelper(complexGraphA, "ComputePathsSBetween_complexGraphA",
				"F","E",
				null,
			new Object[][] {
				new Object[]{new String[] {"a","b","a"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","b","e"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"b","b","a"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"b","b","e"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	/** Checks that concatenation works for real paths, using the new routine. */
	@Test
	public final void testComputePathsSBetween16()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph(complexGraphA, "ComputePathsSBetween_complexGraphA"), testConfig);
		PTATestSequenceEngine engine = new PTATestSequenceEngine();engine.init(new PTASequenceSetAutomaton());
		PTATestSequenceEngine.sequenceSet initSet = engine.new sequenceSet();initSet.setIdentity(); 
		PTATestSequenceEngine.sequenceSet pathsA = engine.new sequenceSet();
		s.paths.ORIGcomputePathsSBetween(s.findVertex("F"), s.findVertex("D"),initSet,pathsA);
		PTATestSequenceEngine.sequenceSet pathsB = engine.new sequenceSet();
		s.paths.ORIGcomputePathsSBetween(s.findVertex("B"), s.findVertex("C"),pathsA,pathsB);
		PTATestSequenceEngine.sequenceSet pathsC = engine.new sequenceSet();
		s.paths.ORIGcomputePathsSBetween(s.findVertex("C"), s.findVertex("A"),pathsB,pathsC);
		Map<String,String> actual = engine.getDebugDataMap(pathsC),expected = TestFSMAlgo.buildStringMap(
			new Object[][] {
				new Object[]{new String[] {"a","d","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"a","b","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"b","d","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"b","b","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void testComputePathsSBetween17()
	{
		TestComputePathsBetweenHelper("A-q->A\nB-a->A\nB-b->A\nB-c->A\nB-d->B-e->B", "testComputePathsSBetween16",
				"B","A",
				null,
			new Object[][] {
				new Object[]{new String[] {"a"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)},
				new Object[]{new String[] {"c"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	private final String simpleGraphA = "A-a->B-a->D-d->B\nD-b->C-c->A";
	
	@Test
	public final void testComputePathsSBetween18()
	{
		TestComputePathsBetweenHelper(simpleGraphA, "ComputePathsSBetween_simpleGraphA",
				"B","A",
				null,
			new Object[][] {
				new Object[]{new String[] {"a","b","c"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	@Test
	public final void testComputePathsSBetween19()
	{
		TestComputePathsBetweenHelper(simpleGraphA, "ComputePathsSBetween_simpleGraphA",
				"A","C",
				null,
			new Object[][] {
				new Object[]{new String[] {"a","a","b"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	@Test
	public final void testComputePathsSBetween20()
	{
		TestComputePathsBetweenHelper(simpleGraphA, "ComputePathsSBetween_simpleGraphA",
				"D","B",
				null,
			new Object[][] {
				new Object[]{new String[] {"d"}, PTATestSequenceEngine.DebugDataValues.booleanToString(true, true)}
		});
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testComputePathsToRed0a()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testComputePathsToRed1"), testConfig);
		s.paths.computePathsToRed(new StringVertex("non-existing-vertex"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testComputePathsToRed0b()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testComputePathsToRed1"), testConfig);
		s.paths.computePathsToRed(null);
	}
	
	/** Unreachable vertex. */
	@Test(expected = IllegalArgumentException.class)
	public final void testComputePathsToRed0c()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nQ-a->Q", "testComputePathsToRed1"), testConfig);
		s.paths.computePathsToRed(s.findVertex("Q"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testComputePathsToRed0d()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph(complexGraphA, "ComputePathsSBetween_complexGraphA"), testConfig);
		s.paths.computePathsToRed(new StringVertex("F"));
	}

	/** State to look for is the initial one. */
	@Test
	public final void testComputePathsToRed1()
	{
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->A", "testComputePathsToRed1"), testConfig);
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
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->A", "testComputePathsToRed2"), testConfig);
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
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C", "testComputePathsToRed3"), testConfig);
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
		LearnerGraph s = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->C\nA-q->C", "testComputePathsToRed4"), testConfig);
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
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->D-q->C", "testComputePathsToRed5");
		LearnerGraph s = new LearnerGraph(g, testConfig);
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
		LearnerGraph score = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-b->D\n","testFindVertex1"), testConfig);
		Assert.assertTrue(score.getVertex(new LinkedList<String>()).getName().equals("A"));
	}

	@Test
	public final void testGetVertex2()
	{
		LearnerGraph score = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-b->C-b->D\n","testFindVertex2"), testConfig);
		Assert.assertTrue(score.getVertex(Arrays.asList(new String[]{"a","b"})).getName().equals("C"));
	}

	@Test
	public final void testGetVertex3()
	{
		LearnerGraph score = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-b->D\n","testFindVertex3"), testConfig);
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
