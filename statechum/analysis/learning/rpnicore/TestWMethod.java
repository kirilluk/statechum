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

package statechum.analysis.learning.rpnicore;

import static org.junit.Assert.*;
import static statechum.analysis.learning.rpnicore.WMethod.cross;
import static statechum.analysis.learning.rpnicore.WMethod.crossWithSet;
import static statechum.analysis.learning.rpnicore.WMethod.crossWithSet_One;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import junit.framework.Assert;
import junit.framework.JUnit4TestAdapter;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.StringVertex;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.WMethod.EquivalentStatesException;
import statechum.analysis.learning.rpnicore.WMethod.FsmPermutator;
import statechum.model.testset.PrefixFreeCollection;
import statechum.model.testset.SlowPrefixFreeCollection;
import static statechum.analysis.learning.TestFSMAlgo.buildGraph;
import static statechum.analysis.learning.TestFSMAlgo.buildSet;
import static statechum.analysis.learning.TestFSMAlgo.buildList;
import static statechum.model.testset.PrefixFreeCollection.isPrefix;

/**
 * @author kirr
 *
 */
public class TestWMethod {

	public TestWMethod()
	{
		mainConfiguration = Configuration.getDefaultConfiguration();
	}
	
	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public void beforeTest()
	{
		
		config = (Configuration)mainConfiguration.clone();
	}

	/** The configuration to use when running tests. */
	private Configuration config = null, mainConfiguration = null;

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover1() {
		Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"b"},new String[]{"b","a"}}),
			actual = new HashSet<List<String>>();
			actual.addAll(new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S","computeStateCover1"),config).wmethod.computeStateCover());
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover2() {
		Collection<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"b"},new String[]{"b","a"}}),
			actual = new HashSet<List<String>>();
			actual.addAll(new LearnerGraph(buildGraph("A-d->A-b->B-c->B-a->C\nQ-d->S","computeStateCover2"),config).wmethod.computeStateCover());
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover3() {
		Collection<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"b"},new String[]{"d"},new String[]{"b","a"}}),
			actual = new HashSet<List<String>>();
			actual.addAll(new LearnerGraph(buildGraph("A-a->A\nD<-d-A-b->B-c->B-a->C\nQ-d->S","computeStateCover3"),config).wmethod.computeStateCover());
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover4() {
		Collection<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"a"},new String[]{"b"},new String[]{"d"},new String[]{"b","a"},new String[]{"b","a","a"}}),
			actual = new HashSet<List<String>>();
			actual.addAll(new LearnerGraph(buildGraph("A-a->S\nD<-d-A-b->B-c->B-a->C-a->Q\nQ-d->S","computeStateCover3"),config).wmethod.computeStateCover());
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix0() {
		Assert.assertTrue(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{})));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix1() {
		Assert.assertTrue(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{"a"})));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix2() {
		Assert.assertFalse(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{"a","b","c","d"})));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix3() {
		Assert.assertTrue(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{"a","b","c"})));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross1a() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}), 
		buildList(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}})));
		Set<List<String>> expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"},
						new String[]{"z","x","c","d"},
						new String[]{"z","x","q","w"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross1b() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{},new String[]{"a","b"},new String[]{"z","x"}}), 
				buildList(new String[][] {new String[]{},new String[]{"c","d"}, new String[]{"q","w"}})));
		Set<List<String>> expected = buildSet(new String[][] {
						new String[]{},
						new String[]{"a","b"},new String[]{"z","x"},
						new String[]{"c","d"},new String[]{"q","w"},
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"},
						new String[]{"z","x","c","d"},
						new String[]{"z","x","q","w"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross2() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"}}), 
				buildList(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}})));
		Set<List<String>> expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross3() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}), 
				buildList(new String[][] {new String[]{"c","d"}})));
		Set<List<String>> expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"z","x","c","d"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross4() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {}), 
				buildList(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}})));
				
		Assert.assertTrue(actual.isEmpty());		
	}				

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross5() {
		Set<List<String>> actual = new HashSet<List<String>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}), 
				buildList(new String[][] {})));
				
		Assert.assertTrue(actual.isEmpty());		
	}
	
	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet1() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{"p","q"});
		actualA.addAll(crossWithSet(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		actualB.addAll(crossWithSet_One(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		Set<List<String>> expected = buildSet(new String[][] {
				new String[]{"a","b","p"},
				new String[]{"z","x","p"},
				new String[]{"a","b","q"},
				new String[]{"z","x","q"}});
		Assert.assertTrue("expected: "+expected+" actual: "+actualA,expected.equals(actualA));
		Assert.assertTrue("expected: "+expected+" actual: "+actualB,expected.equals(actualB));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet2() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{});
		actualA.addAll(crossWithSet(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		actualB.addAll(crossWithSet_One(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet3() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{"p","q"});
		actualA.addAll(crossWithSet(buildList(new String[][] {}),with));
		actualB.addAll(crossWithSet(buildList(new String[][] {}),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	@Test
	public final void testCrossWithSet4() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{"p"});
		LinkedList<List<String>> data = new LinkedList<List<String>>();
		LinkedList<String> seq = new LinkedList<String>();
		seq.clear();seq.addAll(Arrays.asList(new String[]{"a","b"}));data.add((List<String>)seq.clone());
		seq.clear();seq.addAll(Arrays.asList(new String[]{"z","x"}));data.add((List<String>)seq.clone());// cannot use buildList here because it returns a collection of immutable sequences
		
		actualA.addAll(crossWithSet((LinkedList<List<String>>)data.clone(),with));
		actualB.addAll(crossWithSet_One((LinkedList<List<String>>)data.clone(),with));
		Set<List<String>> expected = buildSet(new String[][] {
				new String[]{"a","b","p"},
				new String[]{"z","x","p"}});
		Assert.assertTrue("expected: "+expected+" actual: "+actualA,expected.equals(actualA));
		Assert.assertTrue("expected: "+expected+" actual: "+actualB,expected.equals(actualB));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet5() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{});
		actualA.addAll(crossWithSet(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		actualB.addAll(crossWithSet_One(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet6() {
		Set<List<String>> actualA = new HashSet<List<String>>(), actualB = new HashSet<List<String>>();
		List<String> with = Arrays.asList(new String[]{"p"});
		actualA.addAll(crossWithSet(buildList(new String[][] {}),with));
		actualB.addAll(crossWithSet(buildList(new String[][] {}),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	/**
	 * Test method for makeSingleton.
	 */
	@Test
	public final void testGetStimuli1() {
		Assert.assertTrue(WMethod.makeSingleton(Arrays.asList(new String[]{})).isEmpty());
	}	

	/**
	 * Test method for makeSingleton.
	 */
	@Test
	public final void testGetStimuli2() {
		Set<List<String>> singleton = new HashSet<List<String>>();
		singleton.addAll(WMethod.makeSingleton(Arrays.asList(new String[]{"a","c","p","d","b"})));
		Assert.assertTrue(singleton
				.equals(buildSet(new String[][]{
						new String[]{"p"},
						new String[]{"d"},
						new String[]{"b"},
						new String[]{"c"},
						new String[]{"a"}
				})));
	}

	@Test
	public final void testTruncateSeq1()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq1"),config);
		Assert.assertEquals(Arrays.asList(new String[]{"p","b"}), fsm.paths.truncateSequence(Arrays.asList(new String[]{"p","b"})));
	}

	@Test
	public final void testTruncateSeq2()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq2"),config);
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","c"}), fsm.paths.truncateSequence(Arrays.asList(new String[]{"p","b","c"})));
	}
	
	@Test
	public final void testTruncateSeq3()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq3"),config);
		Assert.assertEquals(Arrays.asList(new String[]{}), fsm.paths.truncateSequence(Arrays.asList(new String[]{})));
	}

	@Test
	public final void testTruncateSeq4()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq4"),config);
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","z"}), fsm.paths.truncateSequence(Arrays.asList(new String[]{"p","b","z","e"})));
	}

	@Test
	public final void testTruncateSeq5()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq5"),config);
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","d"}), fsm.paths.truncateSequence(Arrays.asList(new String[]{"p","b","d"})));
	}

	@Test
	public final void testTruncateSeq5b()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-f->S","testTruncateSeq5b"),config);
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","d"}), fsm.paths.truncateSequence(Arrays.asList(new String[]{"p","b","d"})));
	}

	@Test
	public final void testTruncateSeq6()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq6"),config);
		Assert.assertEquals(Arrays.asList(new String[]{"a"}), fsm.paths.truncateSequence(Arrays.asList(new String[]{"a"})));
	}
	
	@Test
	public final void testTruncateSeq7()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq7"),config);
		Assert.assertEquals(Arrays.asList(new String[]{"p","p","p"}), fsm.paths.truncateSequence(Arrays.asList(new String[]{"p","p","p"})));
	}
	
	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence0() 
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence0"),config);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{}
			});
		
		fsm.wmethod.appendSequence(sequences, Arrays.asList(new String[]{}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence0b() 
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence0b"),config);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{}));

		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{}
			});
		
		fsm.wmethod.appendSequence(sequences, Arrays.asList(new String[]{}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence1() 
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence1"),config);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b"}
			});
		
		fsm.wmethod.appendSequence(sequences, Arrays.asList(new String[]{}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence2() 
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence2"),config);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b"}
			});
		
		fsm.wmethod.appendSequence(sequences, Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence3() 
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence3"),config);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b","c"}
			});
		
		fsm.wmethod.appendSequence(sequences, Arrays.asList(new String[]{"p","b","c"}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence4() 
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence4"),config);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"p","p"}
			});
		
		fsm.wmethod.appendSequence(sequences, Arrays.asList(new String[]{"p","p"}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence5() 
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence5"),config);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"p","p","p"}
			});
		
		fsm.wmethod.appendSequence(sequences, Arrays.asList(new String[]{"p"}));
		fsm.wmethod.appendSequence(sequences, Arrays.asList(new String[]{"p","p"}));
		fsm.wmethod.appendSequence(sequences, Arrays.asList(new String[]{"p","p","p"}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences0() {
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences0"),config);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","p","p"},
				new String[]{"g"}
			});
		
		fsm.wmethod.appendAllSequences(sequences, buildList(new String[][] {
				new String[]{"p"},new String[]{"p","p"},new String[] {}, new String [] {"g"},new String[]{"p","p","p"}}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences0b() {
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences0b"),config);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		Set<List<String>> expected = buildSet(new String[][]{
			});
		
		fsm.wmethod.appendAllSequences(sequences, buildList(new String[][] {}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences1() {
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences1"),config);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(Arrays.asList(new String[]{"p","b"}));
		Set<List<String>> expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"g"},
				new String[]{"p","p","p"}
			});
		
		fsm.wmethod.appendAllSequences(sequences, buildList(new String[][] {
				new String[]{"p"},new String[]{"p","p"},new String[] {}, new String [] {"g"},new String[]{"p","p","p"}}));
		Set<List<String>> actual = new HashSet<List<String>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	public void testWsetconstruction(String machine, boolean equivalentExpected, boolean reductionExpected)
	{
		DirectedSparseGraph g = buildGraph(machine,"testWset");
		LearnerGraph fsm = new LearnerGraph(g,config);
		Set<List<String>> origWset = new HashSet<List<String>>(); 
		try
		{
			origWset.addAll(WMethod.computeWSetOrig(fsm));
			Assert.assertEquals(false, equivalentExpected);
			fsm.wmethod.checkW_is_corrent(origWset);			
		}
		catch(EquivalentStatesException e)
		{
			Assert.assertEquals(true, equivalentExpected);
			WMethod.checkM(fsm,fsm,e.getA(),e.getB());
		}

		try
		{
			Set<List<String>> wset = new HashSet<List<String>>();wset.addAll(WMethod.computeWSet_reducedmemory(fsm));
			Assert.assertEquals(false, equivalentExpected);
			fsm.wmethod.checkW_is_corrent(wset);// we are not checking for W reduction here since space-saving way to compute W
			// does not lead to the W set as small as the computeWSet_reduced one because I compute the distribution of
			// distinguishing labels only once rather than every time it is needed. This way, if I have a pair of states
			// which can be distinguished by many different labels, the distribution becomes skewed, but I do not wish to keep 
			// recomputing because it will take a long time, I think. Storing separating characters as per computeWSet_reduced
			// does not appear to be feasible because of space constraints (we'd like to operate on 16k states in under of 4g memory).
		}
		catch(EquivalentStatesException e)
		{
			Assert.assertEquals(true, equivalentExpected);
			WMethod.checkM(fsm,fsm,e.getA(),e.getB());
		}

		try
		{
			Set<List<String>> wset = new HashSet<List<String>>();wset.addAll(WMethod.computeWSet_reducedw(fsm));
			Assert.assertEquals(false, equivalentExpected);
			fsm.wmethod.checkW_is_corrent(wset);
			int reduction = origWset.size() - wset.size();
			Assert.assertTrue(reduction >= 0 || !reductionExpected);
		}
		catch(EquivalentStatesException e)
		{
			Assert.assertEquals(true, equivalentExpected);
			WMethod.checkM(fsm,fsm,e.getA(),e.getB());
		}
	}	
	
	@Test
	public final void testWset1()
	{
		testWsetconstruction("A-p->A-b->B-c->B-a->C",false,true);
	}
	
	@Test
	public final void testWset2()
	{
		testWsetconstruction("A-a->C-b->Q\nB-a->D-a->Q",false,true);
	}
	
	@Test
	public final void testWset3() // equivalent states
	{
		testWsetconstruction("A-a->C-b->Q\nB-a->D-b->Q",true,true);
	}

	@Test
	public final void testWset4()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-a->A","testWset4"),config);
		Assert.assertTrue(WMethod.computeWSet_reducedmemory(fsm).isEmpty());
	}

	@Test
	public final void testWset5a() // equivalent states
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3",
				true,true);
	}
	
	@Test
	public final void testWset5b() // equivalent states
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3",
				true,true);
	}
	
	@Test
	public final void testWset6() // no equivalent states
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-m->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-z->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3",
				false,true);
	}

	@Test
	public final void testWset7()
	{
		testWsetconstruction("A-a->B-a->C-a->A-b->C-b->B",false,true);
	}
	
	@Test
	public final void testWset8()
	{
		testWsetconstruction("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->M-a->N\nN-b->M-b->N",true,true);
	}
	
	@Test
	public final void testWset9()
	{
		testWsetconstruction("A-a->D\nB-a->C\nA-b->B\nD-b->C",false,true);
	}
	
	@Test
	public final void testWset10()
	{
		String machineOrig = "0--a4->0--a2->0--a5->2\n0--a7->4\n0--a9->3\n0--a0->1\n1--a5->0\n1--a3->0\n4--a1->0\n4--a8->3\n3--a4->1\n3--a6->2\n2--a8->4\n2--a4->0\n3--a9->0",
			machine = null;
		int a = 4, b = 2;
		machine = machineOrig.replaceAll(a+"--", "Q"+"--").replaceAll(">"+a, ">"+"Q")
			.replaceAll(b+"--", a+"--").replaceAll(">"+b, ">"+a)
			.replaceAll("Q"+"--", b+"--").replaceAll(">"+"Q", ">"+b);
		testWsetconstruction(machine,false,true);
	}

	@Test
	public final void testWset11()
	{
		testWsetconstruction("0-a0->1\n0-a1->9\n0-a2->6\n0-a3->1\n0-a5->0\n0-a7->7\n0-a10->7\n0-a12->5\n1-a0->9\n1-a1->5\n1-a3->3\n1-a6->3\n1-a8->7\n1-a14->9\n1-a17->9\n1-a18->6\n2-a0->8\n2-a2->8\n2-a3->6\n2-a4->4\n2-a7->3\n2-a9->2\n2-a10->4\n2-a15->5\n3-a0->5\n3-a1->2\n3-a2->2\n3-a7->3\n3-a9->8\n3-a10->0\n3-a15->6\n3-a16->5\n4-a0->8\n4-a4->8\n4-a5->0\n4-a7->4\n4-a11->0\n4-a12->3\n4-a16->0\n4-a19->5\n5-a0->1\n5-a2->1\n5-a5->6\n5-a6->2\n5-a7->9\n5-a9->0\n5-a11->3\n5-a19->5\n6-a0->1\n6-a2->4\n6-a4->7\n6-a9->8\n6-a10->0\n6-a12->1\n6-a18->1\n6-a19->3\n7-a1->6\n7-a5->4\n7-a7->9\n7-a10->9\n7-a12->7\n7-a13->4\n7-a14->6\n7-a15->9\n8-a2->7\n8-a4->1\n8-a5->6\n8-a6->4\n8-a9->0\n8-a11->2\n8-a13->2\n8-a14->7\n9-a2->7\n9-a3->3\n9-a5->4\n9-a6->2\n9-a9->5\n9-a11->2\n9-a16->8\n9-a17->8\n",false,true);
	}

	@Test
	public final void testWset12()
	{
		testWsetconstruction("0-a0->1\n0-a1->8\n0-a2->7\n0-a7->3\n0-a9->3\n0-a11->8\n0-a12->6\n0-a15->0\n1-a2->0\n1-a4->6\n1-a6->4\n1-a12->5\n1-a13->5\n1-a16->8\n1-a18->6\n1-a19->2\n2-a2->2\n2-a5->7\n2-a8->0\n2-a10->8\n2-a12->8\n2-a13->1\n2-a14->5\n2-a16->8\n3-a3->3\n3-a6->2\n3-a8->7\n3-a10->4\n3-a11->6\n3-a14->9\n3-a15->3\n3-a16->7\n4-a0->4\n4-a3->1\n4-a5->6\n4-a6->7\n4-a10->7\n4-a12->3\n4-a17->4\n4-a18->4\n5-a0->0\n5-a6->3\n5-a7->0\n5-a11->0\n5-a14->4\n5-a16->3\n5-a17->3\n5-a18->4\n6-a0->6\n6-a2->2\n6-a4->1\n6-a10->9\n6-a11->2\n6-a12->1\n6-a17->5\n6-a19->9\n7-a1->5\n7-a2->9\n7-a3->5\n7-a5->1\n7-a7->2\n7-a10->1\n7-a11->0\n7-a16->9\n8-a3->9\n8-a4->9\n8-a5->6\n8-a6->8\n8-a7->6\n8-a12->8\n8-a17->5\n8-a18->9\n9-a1->7\n9-a5->5\n9-a9->1\n9-a10->7\n9-a15->2\n9-a17->0\n9-a18->2\n9-a19->4\n",false,true);
	}

	@Test
	public final void testWset13()
	{
		testWsetconstruction("0-a0->1\n0-a1->9\n0-a2->5\n0-a4->9\n0-a5->5\n0-a10->9\n0-a11->7\n0-a12->9\n0-a15->8\n0-a16->0\n0-a17->3\n0-a18->8\n1-a0->7\n1-a2->0\n1-a3->2\n1-a4->7\n1-a5->6\n1-a7->6\n1-a8->2\n1-a11->0\n1-a15->0\n1-a17->4\n1-a18->1\n1-a19->4\n2-a0->5\n2-a4->7\n2-a5->0\n2-a6->1\n2-a8->9\n2-a10->9\n2-a11->6\n2-a12->2\n2-a13->6\n2-a15->3\n2-a16->1\n2-a17->0\n3-a1->8\n3-a3->3\n3-a4->5\n3-a6->4\n3-a7->6\n3-a9->1\n3-a10->4\n3-a11->3\n3-a15->6\n3-a16->6\n3-a17->5\n3-a19->6\n4-a0->4\n4-a1->0\n4-a2->5\n4-a3->3\n4-a6->2\n4-a7->2\n4-a8->8\n4-a9->0\n4-a10->5\n4-a16->2\n4-a17->5\n4-a19->4\n5-a0->9\n5-a2->6\n5-a5->1\n5-a7->5\n5-a8->4\n5-a9->2\n5-a11->4\n5-a12->7\n5-a15->7\n5-a17->1\n5-a18->7\n5-a19->7\n6-a4->4\n6-a6->7\n6-a7->9\n6-a9->2\n6-a10->8\n6-a12->3\n6-a13->7\n6-a14->1\n6-a15->8\n6-a16->0\n6-a17->1\n6-a18->2\n7-a1->6\n7-a3->1\n7-a5->2\n7-a6->0\n7-a7->6\n7-a8->3\n7-a9->0\n7-a10->5\n7-a11->4\n7-a15->8\n7-a17->8\n7-a19->2\n8-a0->7\n8-a1->3\n8-a3->9\n8-a4->7\n8-a5->6\n8-a6->1\n8-a8->3\n8-a9->0\n8-a10->3\n8-a11->9\n8-a14->8\n8-a18->4\n9-a0->4\n9-a1->9\n9-a5->8\n9-a6->5\n9-a7->3\n9-a9->9\n9-a10->3\n9-a13->8\n9-a14->5\n9-a15->1\n9-a16->8\n9-a17->2\n",false,false);
	}

	
	public class EmptyPermutator implements FsmPermutator {
		public ArrayList<Pair<CmpVertex, String>> getPermutation(
				Collection<Pair<CmpVertex, String>> from) {
			ArrayList<Pair<CmpVertex, String>> result = new ArrayList<Pair<CmpVertex,String>>(from.size());
			result.addAll(from);
			return result;
		}
	}

	public class RandomPermutator implements FsmPermutator {
		private Random rnd = null;
		
		public RandomPermutator(int randomArg)
		{
			rnd = new Random(randomArg);
		}
		/** Returns an array representing an order in which elements of an FSM should be placed in a string. */
		public ArrayList<Pair<CmpVertex, String>> getPermutation(
				Collection<Pair<CmpVertex, String>> from) {
			ArrayList<Pair<CmpVertex, String>> result = new ArrayList<Pair<CmpVertex,String>>(from.size());
			result.addAll(from);
			
			for(int i=0;i< from.size();++i)
			{
				int first = rnd.nextInt(from.size()), second = rnd.nextInt(from.size());
				Pair<CmpVertex, String> firstObj = result.get(first);result.set(first,result.get(second));result.set(second,firstObj);
			}
			return result;
		}
	}
	
	/** Given a machine, this method permutes its states. Tested as a part of testing that 
	 * W set generation is not affected by the order in which states are presented in
	 * a string from which a machine is built.
	 * 
	 * @param machine machine to permute
	 * @param perm the permutator function
	 * @param testName the name to give to the generated machine
	 */
	public void testWsetDeterministic(String machine, FsmPermutator perm, String testName)
	{
		DirectedSparseGraph g = buildGraph(machine,"testDeterminism_"+testName);
		LearnerGraph fsm = new LearnerGraph(g,config);//visFrame.update(null, g);
		Set<List<String>> origWset = new HashSet<List<String>>();origWset.addAll(WMethod.computeWSet_reducedmemory(fsm));
		LearnerGraph permFsm = fsm.wmethod.Permute(perm);
		WMethod.checkM(fsm,permFsm);
		
		Set<List<String>> newWset = new HashSet<List<String>>();newWset.addAll(WMethod.computeWSet_reducedmemory(permFsm));
		fsm.wmethod.checkW_is_corrent(newWset);
		fsm.wmethod.checkW_is_corrent(origWset);
		permFsm.wmethod.checkW_is_corrent(newWset);
		permFsm.wmethod.checkW_is_corrent(origWset);
		
		Assert.assertTrue(origWset.equals(newWset));
	}

	@Test
	public final void testDeterminism()
	{
		String machine = "0-a0->1\n0-a1->9\n0-a2->6\n0-a3->1\n0-a5->0\n0-a7->7\n0-a10->7\n0-a12->5\n1-a0->9\n1-a1->5\n1-a3->3\n1-a6->3\n1-a8->7\n1-a14->9\n1-a17->9\n1-a18->6\n2-a0->8\n2-a2->8\n2-a3->6\n2-a4->4\n2-a7->3\n2-a9->2\n2-a10->4\n2-a15->5\n3-a0->5\n3-a1->2\n3-a2->2\n3-a7->3\n3-a9->8\n3-a10->0\n3-a15->6\n3-a16->5\n4-a0->8\n4-a4->8\n4-a5->0\n4-a7->4\n4-a11->0\n4-a12->3\n4-a16->0\n4-a19->5\n5-a0->1\n5-a2->1\n5-a5->6\n5-a6->2\n5-a7->9\n5-a9->0\n5-a11->3\n5-a19->5\n6-a0->1\n6-a2->4\n6-a4->7\n6-a9->8\n6-a10->0\n6-a12->1\n6-a18->1\n6-a19->3\n7-a1->6\n7-a5->4\n7-a7->9\n7-a10->9\n7-a12->7\n7-a13->4\n7-a14->6\n7-a15->9\n8-a2->7\n8-a4->1\n8-a5->6\n8-a6->4\n8-a9->0\n8-a11->2\n8-a13->2\n8-a14->7\n9-a2->7\n9-a3->3\n9-a5->4\n9-a6->2\n9-a9->5\n9-a11->2\n9-a16->8\n9-a17->8\n";
		testWsetDeterministic(machine, new EmptyPermutator(), "testDeterminism1_empty");
		for(int i=0;i<100;++i)
			testWsetDeterministic(machine, new RandomPermutator(i), "testDeterminism1_random");
	}
	
	
	/** Adds an entry to the supplied map of pairs of states to labels which distinguish among those states.
	 * 
	 * @param distinguishingLabels map of pairs of states to labels which distinguish among those states
	 * @param stateA first state
	 * @param stateB second state
	 * @param labels labels to distinguish between states.
	 */
	private final static void addDistLabels(Map<CmpVertex,Map<CmpVertex,Set<String>>> distinguishingLabels, String stateA, String stateB, String [] labels)
	{
		Set<String> distLabels = new HashSet<String>();Map<CmpVertex,Set<String>> stateToDist = new HashMap<CmpVertex,Set<String>>();
		distLabels.clear();distLabels.addAll(Arrays.asList(labels));stateToDist.put(new StringVertex(stateB), distLabels);
		distinguishingLabels.put(new StringVertex(stateA),stateToDist);		
	}
	
	@Test
	public final void testComputeTopLabe0()
	{
		Map<CmpVertex,Map<CmpVertex,Set<String>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<String>>>();
		Assert.assertNull(WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabel1()
	{
		Map<CmpVertex,Map<CmpVertex,Set<String>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<String>>>();
		Assert.assertNull(WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe2()
	{
		Map<CmpVertex,Map<CmpVertex,Set<String>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<String>>>();
		addDistLabels(distinguishingLabels, "0","1",new String[] {"a"});
		Assert.assertEquals("a",WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe3()
	{
		Map<CmpVertex,Map<CmpVertex,Set<String>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<String>>>();
		addDistLabels(distinguishingLabels, "0","1",new String[] {"a"});
		addDistLabels(distinguishingLabels, "0","2",new String[] {"a"});
		Assert.assertEquals("a",WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe4()
	{
		Map<CmpVertex,Map<CmpVertex,Set<String>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<String>>>();
		addDistLabels(distinguishingLabels, "1","2",new String[] {"a"});
		addDistLabels(distinguishingLabels, "0","1",new String[] {"b"});
		addDistLabels(distinguishingLabels, "3","1",new String[] {"b"});
		Assert.assertEquals("b",WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe5()
	{
		Map<CmpVertex,Map<CmpVertex,Set<String>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<String>>>();
		addDistLabels(distinguishingLabels, "1","2",new String[] {"a"});
		addDistLabels(distinguishingLabels, "0","1",new String[] {"b"});
		addDistLabels(distinguishingLabels, "3","1",new String[] {"b"});
		addDistLabels(distinguishingLabels, "4","1",new String[] {});// empty entry
		Assert.assertEquals("b",WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testCheckEquivalentStates1() // equivalent states
	{
		DirectedSparseGraph g = buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1");
		LearnerGraph fsm = new LearnerGraph(g,config);
		Assert.assertEquals(true, WMethod.checkEquivalentStates(fsm));
	}
	
	@Test
	public final void testCheckEquivalentStates2() // no equivalent states
	{
		DirectedSparseGraph g = buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-m->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-z->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates2");
		LearnerGraph fsm = new LearnerGraph(g,config);
		Assert.assertEquals(false, WMethod.checkEquivalentStates(fsm));
	}

	@Test
	public final void testCheckEquivalentStates3() // no equivalent states
	{
		int [][]table = new int[][] {
				{1,-1},
				{-1,1}
			};
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{0,1}, -1,config);
		Assert.assertEquals(false, WMethod.checkEquivalentStates(fsm));
	}

	@Test
	public final void testCheckEquivalentStates4() // equivalent states
	{
		int [][]table = new int[][] {
				{1,-1},
				{1,-1}
			};
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{0,1}, -1,config);
		Assert.assertEquals(true, WMethod.checkEquivalentStates(fsm));
	}
	
	private void checkTestGenerationResult(LearnerGraph fsm, int extraStates, String [][] expected)
	{
		Set<List<String>> expectedSet = buildSet(expected),
			actualA = new HashSet<List<String>>(),actualB = new HashSet<List<String>>();
		actualA.addAll(fsm.wmethod.computeOldTestSet(extraStates));actualB.addAll(fsm.wmethod.computeNewTestSet(extraStates));
		assertTrue("old test generator failure, received "+actualA+" expected "+expectedSet,expectedSet.equals(actualA));
		assertTrue("new test generator failure, received "+actualB+" expected "+expectedSet,expectedSet.equals(actualB));
	}
	
	@Test
	public final void testTestGeneration1a()
	{
		config.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
		g.addVertex(init);
		checkTestGenerationResult(new LearnerGraph(g,config),0, new String[][] {
				new String[] {}
			});
	}
	
	@Test
	public final void testTestGeneration1b()
	{
		config.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseGraph g = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.LABEL, "A", UserData.SHARED);
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
		g.addVertex(init);
		checkTestGenerationResult(new LearnerGraph(g,config),2, new String[][] {
				new String[] {}
			});
	}
	
	@Test
	public final void testTestGeneration2()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-a->A", "testTestGeneration2"),config);
		checkTestGenerationResult(fsm,0, new String[][] {
				new String[] {"a"}
			});
	}
	
	@Test
	public final void testTestGeneration3()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-a->A", "testTestGeneration3"),config);
		checkTestGenerationResult(fsm,1, new String[][] {
				new String[] {"a","a"}
			});
	}
	
	@Test
	public final void testTestGeneration4()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-a->A-b->B", "testTestGeneration4"),config);
		checkTestGenerationResult(fsm,0, new String[][] {
				new String[] {"a","a"},
				new String[] {"b","a"},
				new String[] {"b","b"}
			});
	}
	
	@Test
	public final void testTestGeneration5()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-a->A-b->B", "testTestGeneration5"),config);
		checkTestGenerationResult(fsm,1, new String[][] {
				new String[] {"a","a","a"},
				new String[] {"a","b","a"},
				new String[] {"b","a"},
				new String[] {"b","b"}
			});
	}

	@Test
	public final void testTestGeneration6()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph(
				"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA<-c-A2<-c-A3\n"+"A<-d-A5<-a-A3-b->A4-f->AA4-a->S\n"+
				"A-d->A\nA5-b->AA6", "testTestGeneration6"),config);
		Set<List<String>> 
			actualA = new HashSet<List<String>>(),actualB = new HashSet<List<String>>();
			actualA.addAll(fsm.wmethod.computeOldTestSet(0));actualB.addAll(fsm.wmethod.computeNewTestSet(0));
			assertTrue("the two test generators return different values, old returns "+
					actualA+" and the new one - "+actualB,
			actualA.equals(actualB));
	}
	
	@Test
	public final void testTestGeneration7()
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph(
				"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA<-c-A2<-c-A3\n"+"A<-d-A5<-a-A3-b->A4-f->AA4-a->S\n"+
				"A-d->A\nA5-b->AA6", "testTestGeneration7"),config);
		Set<List<String>> 
			actualA = new HashSet<List<String>>(),actualB = new HashSet<List<String>>();
			actualA.addAll(fsm.wmethod.computeOldTestSet(2));actualB.addAll(fsm.wmethod.computeNewTestSet(2));
			assertTrue("the two test generators return different values, old returns "+
					actualA+" and the new one - "+actualB,
			actualA.equals(actualB));
	}
	
	@Test
	public final void testCheckUnreachable1()
	{
		assertFalse(new LearnerGraph(buildGraph("A-a->A", "testCheckUnreachable1"),config).wmethod.checkUnreachableStates());	
	}
	
	@Test
	public final void testCheckUnreachable2()
	{
		assertFalse(new LearnerGraph(buildGraph("A-a->A-c->C\nB-a->A\nC-b->B", "testCheckUnreachable2"),config).wmethod.checkUnreachableStates());	
	}
	
	@Test
	public final void testCheckUnreachable3()
	{
		assertTrue(new LearnerGraph(buildGraph("A-a->A-c->C\nB-a->A", "testCheckUnreachable3"),config).wmethod.checkUnreachableStates());	
	}
	
	@Test
	public final void testCheckGraphNumeric1()
	{
		Assert.assertFalse(new LearnerGraph(buildGraph("A-a->A-c->C","testCheckGraphNumeric"),config).wmethod.checkGraphNumeric());
	}
	
	@Test
	public final void testCheckGraphNumeric2()
	{
		Assert.assertTrue(new LearnerGraph(config).wmethod.checkGraphNumeric());
	}
	
	@Test
	public final void testCheckGraphNumeric3()
	{
		LearnerGraph textGraph = new LearnerGraph(buildGraph("A-a->A-c->C","testCheckGraphNumeric"),config);
		LearnerGraph numericGraph = new LearnerGraph(config);CmpVertex newInit = Transform.addToGraph(numericGraph, textGraph);
		numericGraph = MergeStates.mergeAndDeterminize_general(numericGraph, new StatePair(numericGraph.paths.getVertex(new LinkedList<String>()),newInit));
		Assert.assertTrue(numericGraph.wmethod.checkGraphNumeric());
	}
	
	@Test
	public final void testVertexToInt1()
	{
		LearnerGraph textGraph = new LearnerGraph(config);
		CmpVertex A = textGraph.paths.getVertex(Arrays.asList(new String[]{}));
		Assert.assertEquals(0,textGraph.wmethod.vertexToInt(A,A));
	}
	
	@Test
	public final void testVertexToInt2()
	{
		LearnerGraph textGraph = new LearnerGraph(buildGraph("A-a->A-b->B-c->C","testCheckGraphNumeric"),config);
		LearnerGraph numericGraph = new LearnerGraph(config);CmpVertex newInit = Transform.addToGraph(numericGraph, textGraph);
		numericGraph = MergeStates.mergeAndDeterminize_general(numericGraph, new StatePair(newInit,numericGraph.paths.getVertex(new LinkedList<String>())));
		CmpVertex A = numericGraph.paths.getVertex(Arrays.asList(new String[]{})),
			B = numericGraph.paths.getVertex(Arrays.asList(new String[]{"b"})),
			C = numericGraph.paths.getVertex(Arrays.asList(new String[]{"b","c"}));
		/*  ABC
		 *A 013
		 *B 124
		 *C 345
		*/
		Assert.assertEquals(0,numericGraph.wmethod.vertexToInt(A,A));
		Assert.assertEquals(1,numericGraph.wmethod.vertexToInt(A,B));
		Assert.assertEquals(1,numericGraph.wmethod.vertexToInt(B,A));

		Assert.assertEquals(3,numericGraph.wmethod.vertexToInt(A,C));
		Assert.assertEquals(3,numericGraph.wmethod.vertexToInt(C,A));

		Assert.assertEquals(2,numericGraph.wmethod.vertexToInt(B,B));

		Assert.assertEquals(4,numericGraph.wmethod.vertexToInt(B,C));
		Assert.assertEquals(4,numericGraph.wmethod.vertexToInt(C,B));

		Assert.assertEquals(5,numericGraph.wmethod.vertexToInt(C,C));
}
	
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{
		Visualiser.disposeFrame();
	}

	@AfterClass
	public static void cleanUp()
	{
		Visualiser.disposeFrame();
	}
	
	/** In order to be able to use old junit runner. */
	public static junit.framework.Test suite()
	{
		return new JUnit4TestAdapter(TestWMethod.class);
	}
}