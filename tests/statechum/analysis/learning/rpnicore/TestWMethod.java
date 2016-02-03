/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */ 

package statechum.analysis.learning.rpnicore;

import static org.junit.Assert.*;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraphND;
import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildList;
import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;
import static statechum.analysis.learning.rpnicore.WMethod.cross;
import static statechum.analysis.learning.rpnicore.WMethod.crossWithSet;
import static statechum.analysis.learning.rpnicore.WMethod.crossWithSet_One;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.StringVertex;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.linear.GDLearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod.EquivalentStatesException;
import statechum.model.testset.PrefixFreeCollection;
import statechum.model.testset.SlowPrefixFreeCollection;
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
		config = mainConfiguration.copy();
	}

	/** Converts arrays of labels to lists of labels using config - it does not really matter which configuration is used 
	 * because all of them start from a default one and do not modify label type.
	 * 
	 * @param labels what to convert
	 * @return the outcome of conversion.
	 */
	protected List<Label> labelList(String [] labels)
	{
		return AbstractLearnerGraph.buildList(Arrays.asList(labels),config,converter);
	}
	
	/** The configuration to use when running tests. */
	private Configuration config = null, mainConfiguration = null;
	ConvertALabel converter = null;
	
	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover1() {
		LearnerGraph gr=buildLearnerGraph("A-a->A-b->B-c->B-a->C\nQ-d->S","computeStateCover1",config,converter);
		Set<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{
				new String[]{},
				new String[]{"b"},
				new String[]{"b","a"}}
		,config,converter),
			actual = new HashSet<List<Label>>();
			actual.addAll(gr.pathroutines.computeStateCover(gr.getInit()));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover2() {
		LearnerGraph gr = buildLearnerGraph("A-d->A-b->B-c->B-a->C\nQ-d->S","computeStateCover2",config,converter);
		Collection<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{
				new String[]{},
				new String[]{"b"},
				new String[]{"b","a"}}
		,config,converter),
			actual = new HashSet<List<Label>>();
			actual.addAll(gr.pathroutines.computeStateCover(gr.getInit()));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover3() {
		LearnerGraph gr=buildLearnerGraph("A-a->A\nD<-d-A-b->B-c->B-a->C\nQ-d->S","computeStateCover3",config,converter);
		Collection<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{
				new String[]{},
				new String[]{"b"},
				new String[]{"d"},
				new String[]{"b","a"}}
		,config,converter),
			actual = new HashSet<List<Label>>();
			actual.addAll(gr.pathroutines.computeStateCover(gr.getInit()));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover3_ND() {
		LearnerGraphND gr=buildLearnerGraphND("A-a->A\nD<-d-A-b->B-c->B-a->C\nQ-d->S\nA-a->C","computeStateCover3_ND",config,converter);
		Collection<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{
				new String[]{},
				new String[]{"b"},
				new String[]{"d"},
				new String[]{"a"}}
		,config,converter),
			actual = new HashSet<List<Label>>();
			actual.addAll(gr.pathroutines.computeStateCover(gr.getInit()));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover4() {
		LearnerGraph gr = buildLearnerGraph("A-a->S\nD<-d-A-b->B-c->B-a->C-a->Q\nQ-d->S","computeStateCover3",config,converter);
		Collection<List<Label>> expected = TestFSMAlgo.buildSet(new String[][]{
				new String[]{},
				new String[]{"a"},
				new String[]{"b"},
				new String[]{"d"},
				new String[]{"b","a"},
				new String[]{"b","a","a"}}
		,config,converter),
			actual = new HashSet<List<Label>>();
			actual.addAll(gr.pathroutines.computeStateCover(gr.getInit()));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix0() {
		Assert.assertTrue(isPrefix(labelList(new String[]{"a","b","c"}),labelList(new String[]{})));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix1() {
		Assert.assertTrue(isPrefix(labelList(new String[]{"a","b","c"}),labelList(new String[]{"a"})));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix2() {
		Assert.assertFalse(isPrefix(labelList(new String[]{"a","b","c"}),labelList(new String[]{"a","b","c","d"})));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix3() {
		Assert.assertTrue(isPrefix(labelList(new String[]{"a","b","c"}),labelList(new String[]{"a","b","c"})));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross1a() {
		Set<List<Label>> actual = new HashSet<List<Label>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}},config,converter), 
		buildList(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}},config,converter)));
		Set<List<Label>> expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"},
						new String[]{"z","x","c","d"},
						new String[]{"z","x","q","w"}},config,converter);
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross1b() {
		Set<List<Label>> actual = new HashSet<List<Label>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{},new String[]{"a","b"},new String[]{"z","x"}},config,converter), 
				buildList(new String[][] {new String[]{},new String[]{"c","d"}, new String[]{"q","w"}},config,converter)));
		Set<List<Label>> expected = buildSet(new String[][] {
						new String[]{},
						new String[]{"a","b"},new String[]{"z","x"},
						new String[]{"c","d"},new String[]{"q","w"},
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"},
						new String[]{"z","x","c","d"},
						new String[]{"z","x","q","w"}},config,converter);
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross2() {
		Set<List<Label>> actual = new HashSet<List<Label>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"}},config,converter), 
				buildList(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}},config,converter)));
		Set<List<Label>> expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"}},config,converter);
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross3() {
		Set<List<Label>> actual = new HashSet<List<Label>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}},config,converter), 
				buildList(new String[][] {new String[]{"c","d"}},config,converter)));
		Set<List<Label>> expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"z","x","c","d"}},config,converter);
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross4() {
		Set<List<Label>> actual = new HashSet<List<Label>>();
		actual.addAll(cross(buildList(new String[][] {},config,converter), 
				buildList(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}},config,converter)));
				
		Assert.assertTrue(actual.isEmpty());		
	}				

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross5() {
		Set<List<Label>> actual = new HashSet<List<Label>>();
		actual.addAll(cross(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}},config,converter), 
				buildList(new String[][] {},config,converter)));
				
		Assert.assertTrue(actual.isEmpty());		
	}
	
	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet1() {
		Set<List<Label>> actualA = new HashSet<List<Label>>(), actualB = new HashSet<List<Label>>();
		List<Label> with = labelList(new String[]{"p","q"});
		actualA.addAll(crossWithSet(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}},config,converter),with));
		actualB.addAll(crossWithSet_One(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}},config,converter),with));
		Set<List<Label>> expected = buildSet(new String[][] {
				new String[]{"a","b","p"},
				new String[]{"z","x","p"},
				new String[]{"a","b","q"},
				new String[]{"z","x","q"}}
				,config,converter);
		Assert.assertTrue("expected: "+expected+" actual: "+actualA,expected.equals(actualA));
		Assert.assertTrue("expected: "+expected+" actual: "+actualB,expected.equals(actualB));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet2() {
		Set<List<Label>> actualA = new HashSet<List<Label>>(), actualB = new HashSet<List<Label>>();
		List<Label> with = labelList(new String[]{});
		actualA.addAll(crossWithSet(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}},config,converter),with));
		actualB.addAll(crossWithSet_One(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}},config,converter),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet3() {
		Set<List<Label>> actualA = new HashSet<List<Label>>(), actualB = new HashSet<List<Label>>();
		List<Label> with = labelList(new String[]{"p","q"});
		actualA.addAll(crossWithSet(buildList(new String[][] {},config,converter),with));
		actualB.addAll(crossWithSet(buildList(new String[][] {},config,converter),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	@SuppressWarnings("unchecked")
	@Test
	public final void testCrossWithSet4() {
		Set<List<Label>> actualA = new HashSet<List<Label>>(), actualB = new HashSet<List<Label>>();
		List<Label> with = labelList(new String[]{"p"});
		LinkedList<List<Label>> data = new LinkedList<List<Label>>();
		LinkedList<Label> seq = new LinkedList<Label>();
		seq.clear();seq.addAll(labelList(new String[]{"a","b"}));data.add((List<Label>)seq.clone());
		seq.clear();seq.addAll(labelList(new String[]{"z","x"}));data.add((List<Label>)seq.clone());// cannot use buildList here because it returns a collection of immutable sequences
		
		actualA.addAll(crossWithSet((LinkedList<List<Label>>)data.clone(),with));
		actualB.addAll(crossWithSet_One((LinkedList<List<Label>>)data.clone(),with));
		Set<List<Label>> expected = buildSet(new String[][] {
				new String[]{"a","b","p"},
				new String[]{"z","x","p"}},config,converter);
		Assert.assertTrue("expected: "+expected+" actual: "+actualA,expected.equals(actualA));
		Assert.assertTrue("expected: "+expected+" actual: "+actualB,expected.equals(actualB));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet5() {
		Set<List<Label>> actualA = new HashSet<List<Label>>(), actualB = new HashSet<List<Label>>();
		List<Label> with = labelList(new String[]{});
		actualA.addAll(crossWithSet(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}},config,converter),with));
		actualB.addAll(crossWithSet_One(buildList(new String[][] {new String[]{"a","b"},new String[]{"z","x"}},config,converter),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#crossWithSet(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCrossWithSet6() {
		Set<List<Label>> actualA = new HashSet<List<Label>>(), actualB = new HashSet<List<Label>>();
		List<Label> with = labelList(new String[]{"p"});
		actualA.addAll(crossWithSet(buildList(new String[][] {},config,converter),with));
		actualB.addAll(crossWithSet(buildList(new String[][] {},config,converter),with));
		Assert.assertEquals(0, actualA.size());
		Assert.assertEquals(0, actualB.size());
	}

	/**
	 * Test method for makeSingleton.
	 */
	@Test
	public final void testGetStimuli1() {
		Assert.assertTrue(WMethod.makeSingleton(labelList(new String[]{})).isEmpty());
	}	

	/**
	 * Test method for makeSingleton.
	 */
	@Test
	public final void testGetStimuli2() {
		Set<List<Label>> singleton = new HashSet<List<Label>>();
		singleton.addAll(WMethod.makeSingleton(labelList(new String[]{"a","c","p","d","b"})));
		Assert.assertTrue(singleton
				.equals(buildSet(new String[][]{
						new String[]{"p"},
						new String[]{"d"},
						new String[]{"b"},
						new String[]{"c"},
						new String[]{"a"}
				},config,converter)));
	}

	@Test
	public final void testTruncateSeq1()
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq1",config,converter);
		Assert.assertEquals(labelList(new String[]{"p","b"}), fsm.paths.truncateSequence(labelList(new String[]{"p","b"})));
	}

	@Test
	public final void testTruncateSeq2()
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq2",config,converter);
		Assert.assertEquals(labelList(new String[]{"p","b","c"}), fsm.paths.truncateSequence(labelList(new String[]{"p","b","c"})));
	}
	
	@Test
	public final void testTruncateSeq3()
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq3",config,converter);
		Assert.assertEquals(labelList(new String[]{}), fsm.paths.truncateSequence(labelList(new String[]{})));
	}

	@Test
	public final void testTruncateSeq4()
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq4",config,converter);
		Assert.assertEquals(labelList(new String[]{"p","b","z"}), fsm.paths.truncateSequence(labelList(new String[]{"p","b","z","e"})));
	}

	@Test
	public final void testTruncateSeq5a()
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq5",config,converter);
		Assert.assertEquals(labelList(new String[]{"p","b","d"}), fsm.paths.truncateSequence(labelList(new String[]{"p","b","d"})));
	}

	@Test
	public final void testTruncateSeq5b()
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-f->S","testTruncateSeq5b",config,converter);
		Assert.assertEquals(labelList(new String[]{"p","b","d"}), fsm.paths.truncateSequence(labelList(new String[]{"p","b","d"})));
	}

	@Test
	public final void testTruncateSeq6()
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq6",config,converter);
		Assert.assertEquals(labelList(new String[]{"a"}), fsm.paths.truncateSequence(labelList(new String[]{"a"})));
	}
	
	@Test
	public final void testTruncateSeq7()
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq7",config,converter);
		Assert.assertEquals(labelList(new String[]{"p","p","p"}), fsm.paths.truncateSequence(labelList(new String[]{"p","p","p"})));
	}
	
	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence0() 
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence0",config,converter);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{}
			},config,converter);
		
		fsm.wmethod.appendSequence(sequences, labelList(new String[]{}));
		Set<List<Label>> actual = new HashSet<List<Label>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence0b() 
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence0b",config,converter);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(labelList(new String[]{}));

		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{}
			},config,converter);
		
		fsm.wmethod.appendSequence(sequences, labelList(new String[]{}));
		Set<List<Label>> actual = new HashSet<List<Label>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence1() 
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence1",config,converter);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(labelList(new String[]{"p","b"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"p","b"}
			},config,converter);
		
		fsm.wmethod.appendSequence(sequences, labelList(new String[]{}));
		Set<List<Label>> actual = new HashSet<List<Label>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence2() 
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence2",config,converter);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(labelList(new String[]{"p","b"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"p","b"}
			},config,converter);
		
		fsm.wmethod.appendSequence(sequences, labelList(new String[]{"p","b"}));
		Set<List<Label>> actual = new HashSet<List<Label>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence3() 
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence3",config,converter);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(labelList(new String[]{"p","b"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"p","b","c"}
			},config,converter);
		
		fsm.wmethod.appendSequence(sequences, labelList(new String[]{"p","b","c"}));
		Set<List<Label>> actual = new HashSet<List<Label>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence4() 
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence4",config,converter);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(labelList(new String[]{"p","b"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"p","p"}
			},config,converter);
		
		fsm.wmethod.appendSequence(sequences, labelList(new String[]{"p","p"}));
		Set<List<Label>> actual = new HashSet<List<Label>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence5() 
	{
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence5",config,converter);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(labelList(new String[]{"p","b"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"p","p","p"}
			},config,converter);
		
		fsm.wmethod.appendSequence(sequences, labelList(new String[]{"p"}));
		fsm.wmethod.appendSequence(sequences, labelList(new String[]{"p","p"}));
		fsm.wmethod.appendSequence(sequences, labelList(new String[]{"p","p","p"}));
		Set<List<Label>> actual = new HashSet<List<Label>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences0() {
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences0",config,converter);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"p","p","p"},
				new String[]{"g"}
			},config,converter);
		
		fsm.wmethod.appendAllSequences(sequences, buildList(new String[][] {
				new String[]{"p"},
				new String[]{"p","p"},
				new String[] {}, 
				new String [] {"g"},
				new String[]{"p","p","p"}}
		,config,converter));
		Set<List<Label>> actual = new HashSet<List<Label>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences0b() {
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences0b",config,converter);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		Set<List<Label>> expected = buildSet(new String[][]{
			},config,converter);
		
		fsm.wmethod.appendAllSequences(sequences, buildList(new String[][] {},config,converter));
		Set<List<Label>> actual = new HashSet<List<Label>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.analysis.learning.rpnicore.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences1() {
		LearnerGraph fsm = buildLearnerGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences1",config,converter);
		PrefixFreeCollection sequences = new SlowPrefixFreeCollection();
		sequences.addSequence(labelList(new String[]{"p","b"}));
		Set<List<Label>> expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"g"},
				new String[]{"p","p","p"}
			},config,converter);
		
		fsm.wmethod.appendAllSequences(sequences, buildList(new String[][] {
				new String[]{"p"},
				new String[]{"p","p"},
				new String[] {}, 
				new String [] {"g"},
				new String[]{"p","p","p"}}
		,config,converter));
		Set<List<Label>> actual = new HashSet<List<Label>>();actual.addAll(sequences.getData());
		Assert.assertTrue("expected : "+expected+" got: "+actual,expected.equals(actual));
	}

	public static void checkEquivalentStatesException(EquivalentStatesException e,LearnerGraph fsm)
	{
		for(EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqStatesGroup:e.getEquivalentStates())
		{
			// Checks that all states recorded as equivalent are indeed equivalent
			for(CmpVertex eA:eqStatesGroup.getStates())
				for(CmpVertex eB:eqStatesGroup.getStates())
				{
					DifferentFSMException exception = WMethod.checkM(fsm,eA,fsm,eB,WMethod.VERTEX_COMPARISON_KIND.NONE);
					Assert.assertNull("states "+eA+" and "+eB+" should belong to the same equivalence class, got "+(exception == null?"":exception.getMessage()),exception);
				}
			// Checks that all states in different equivalence classes are not equivalent. 
			for(EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqStatesGroupAnother:e.getEquivalentStates())
				if (eqStatesGroup != eqStatesGroupAnother)
				{
					for(CmpVertex eA:eqStatesGroup.getStates()) // our states
						for(CmpVertex eB:eqStatesGroupAnother.getStates()) // other states
						{
							DifferentFSMException exception = WMethod.checkM(fsm,eA,fsm,eB,WMethod.VERTEX_COMPARISON_KIND.NONE);
							if (exception == null)
							{// failure tracing
								fsm.config.setPrefixClosed(true);
								DifferentFSMException ex = WMethod.checkM(fsm,eA,fsm,eB,WMethod.VERTEX_COMPARISON_KIND.NONE);
								System.out.println("B: "+fsm.transitionMatrix.get(fsm.findVertex("B"))+" ; D:  "+fsm.transitionMatrix.get(fsm.findVertex("D")));
								System.out.println(ex);
								fsm.config.setPrefixClosed(false);
								DifferentFSMException ex2 = WMethod.checkM(fsm,eA,fsm,eB,WMethod.VERTEX_COMPARISON_KIND.NONE);
								System.out.println(ex2==null?"NULL":ex);
							}
							Assert.assertNotNull("states "+eA+" and "+eB+" should belong to different equivalence classes",exception);
						}
				}
		}
	}
	

	public void testWsetconstruction(String machine, boolean equivalentExpected, boolean reductionExpected, boolean prefixClosed)
	{
		LearnerGraph fsm = buildLearnerGraph(machine,"testWset",config,converter);
		testWsetconstruction(fsm,equivalentExpected,reductionExpected,prefixClosed);
	}
	
	public static void testWsetConstructionWithEquivalentStates(LearnerGraph fsm, boolean prefixClosed,Set<StatePair> equivalentVertices)
	{
		fsm.config.setPrefixClosed(prefixClosed);
		fsm.config.setEquivalentStatesAllowedForW(false);
		Set<List<Label>> computedWset = new TreeSet<List<Label>>(); 
		try
		{
			computedWset.addAll(WMethod.computeWSet_reducedmemory(fsm));
			fail("exception not thrown");
		}
		catch(EquivalentStatesException e)
		{// exception is expected here
			checkEquivalentStatesException(e,fsm);
		}

		fsm.config.setEquivalentStatesAllowedForW(true);
		Set<List<Label>> wset = new HashSet<List<Label>>();wset.addAll(WMethod.computeWSet_reducedmemory(fsm));
		fsm.wmethod.checkW_is_corrent(wset,prefixClosed,equivalentVertices);// we are not checking for W reduction here since space-saving way to compute W
		
	}

	public static void testWsetconstruction(LearnerGraph fsm, boolean equivalentExpected, boolean reductionExpected, boolean prefixClosed)
	{
		fsm.config.setPrefixClosed(prefixClosed);
		Set<List<Label>> origWset = new HashSet<List<Label>>();// This cannot be a TreeSet because lists store in it may not always implement Comparable (this is the case for LinkedList).
		try
		{
			origWset.addAll(WMethod.computeWSetOrig(fsm));
			Assert.assertEquals(false, equivalentExpected);
			fsm.wmethod.checkW_is_corrent(origWset,prefixClosed,null);			
		}
		catch(EquivalentStatesException e)
		{
			Assert.assertEquals(true, equivalentExpected);
			checkEquivalentStatesException(e,fsm);
		}

		try
		{
			Set<List<Label>> wset = new HashSet<List<Label>>();wset.addAll(WMethod.computeWSet_reducedmemory(fsm));
			Assert.assertEquals(false, equivalentExpected);
			fsm.wmethod.checkW_is_corrent(wset,prefixClosed,null);// we are not checking for W reduction here since space-saving way to compute W
			// does not lead to the W set as small as the computeWSet_reduced one because I compute the distribution of
			// distinguishing labels only once rather than every time it is needed. This way, if I have a pair of states
			// which can be distinguished by many different labels, the distribution becomes skewed, but I do not wish to keep 
			// recomputing because it will take a long time, I think. Storing separating characters as per computeWSet_reduced
			// does not appear to be feasible because of space constraints (we'd like to operate on 16k states in under of 4g memory).
		}
		catch(EquivalentStatesException e)
		{
			Assert.assertEquals(true, equivalentExpected);
			checkEquivalentStatesException(e,fsm);
		}

		try
		{
			Set<List<Label>> wset = new HashSet<List<Label>>();wset.addAll(WMethod.computeWSet_reducedw(fsm));
			Assert.assertEquals(false, equivalentExpected);
			fsm.wmethod.checkW_is_corrent(wset,prefixClosed,null);
			int reduction = origWset.size() - wset.size();
			Assert.assertTrue(reduction >= 0 || !reductionExpected);
		}
		catch(EquivalentStatesException e)
		{
			Assert.assertEquals(true, equivalentExpected);
			checkEquivalentStatesException(e,fsm);
		}
	}	
	
	/** Checking generation of a sink state. */
	@Test
	public final void testGenerateSink1()
	{
		LearnerGraph fsm = new LearnerGraph(config);
		CmpVertex sink = WMethod.generateSinkState(fsm);Assert.assertNotNull(sink);Assert.assertFalse(sink.isAccept());
	}
	
	/** Checking generation of a sink state.  */
	@Test
	public final void testGenerateSink2a()
	{
		LearnerGraph fsm = buildLearnerGraph("A-a->B","testFindSink2a",config,converter);
		CmpVertex sink = WMethod.generateSinkState(fsm);Assert.assertFalse(sink.isAccept());
		Assert.assertFalse(fsm.findVertex(VertexID.parseID("A")).equals(sink));		
		Assert.assertFalse(fsm.findVertex(VertexID.parseID("B")).equals(sink));		
	}
	
	/** Checking generation of a sink state. */
	@Test
	public final void testGenerateSink2b()
	{
		LearnerGraph fsm = buildLearnerGraph("A-a->B","testFindSink2a",config,converter);
		fsm.findVertex(VertexID.parseID("B")).setAccept(false);
		CmpVertex sink = WMethod.generateSinkState(fsm);Assert.assertFalse(sink.isAccept());
		//Assert.assertEquals(fsm.findVertex(VertexID.parseID("B")),WMethod.generateSinkState(fsm));		
		Assert.assertFalse(fsm.findVertex(VertexID.parseID("A")).equals(sink));		
		Assert.assertFalse(fsm.findVertex(VertexID.parseID("B")).equals(sink));		
	}
	
	/** Checking identification of a sink state. B is reject but has outgoing transitions, hence not a sink state. 
	@Test
	public final void testFindSink2c()
	{
		LearnerGraph fsm = buildLearnerGraph("A-a->B-b->B","testFindSink2c",config);
		fsm.findVertex(VertexID.parseID("B")).setAccept(false);
		Assert.assertFalse(fsm.findVertex(VertexID.parseID("A")).equals(WMethod.generateSinkState(fsm)));		
		Assert.assertFalse(fsm.findVertex(VertexID.parseID("B")).equals(WMethod.generateSinkState(fsm)));		
	}
	
	/** Checking identification of a sink state. 
	@Test
	public final void testFindSink3()
	{
		LearnerGraph fsm = buildLearnerGraph("A-a->B","testWset",config);
		Assert.assertFalse(fsm.findVertex(VertexID.parseID("A")).equals(WMethod.generateSinkState(fsm)));		
		Assert.assertFalse(fsm.findVertex(VertexID.parseID("B")).equals(WMethod.generateSinkState(fsm)));		
	}
	*/
	
	/** All states reject-states. */
	@Test
	public final void testW_nonAccept1()
	{
		String machine = "A-a->A";
		LearnerGraph fsm = buildLearnerGraph(machine,"testWset",config,converter);
		fsm.findVertex(VertexID.parseID("A")).setAccept(false);
		testWsetconstruction(fsm,false,true,false);
	}
	
	/** Not all states accept-states. */
	@Test
	public final void testW_nonAccept2()
	{
		String machine = "A-a->A\nB-a->B";
		LearnerGraph fsm = buildLearnerGraph(machine,"testWset",config,converter);
		fsm.findVertex(VertexID.parseID("A")).setAccept(false);
		testWsetconstruction(fsm,false,true,false);
	}

	/** C and D are distinguishable via transitions to A and B. */
	@Test
	public final void testW_nonAccept3()
	{
		String machine = "A-a->A\nB-a->B\nC-b->A\nD-b->B";
		LearnerGraph fsm = buildLearnerGraph(machine,"testWset",config,converter);
		fsm.findVertex(VertexID.parseID("A")).setAccept(false);
		testWsetconstruction(fsm,false,true,false);
	}

	/** C and D are not distinguishable. */
	@Test
	public final void testW_nonAccept4()
	{
		String machine = "A-a->A\nB-a->B\nC-b->A\nD-b->B";
		LearnerGraph fsm = buildLearnerGraph(machine,"testWset",config,converter);
		testWsetconstruction(fsm,true,true,false);
	}

	/** Loop of sink states. */
	@Test
	public final void testW_nonAccept5()
	{
		String machine = "A-a->A\nB-b->C-a->D-b->B";
		LearnerGraph fsm = buildLearnerGraph(machine,"testWset",config,converter);
		fsm.findVertex(VertexID.parseID("B")).setAccept(false);
		fsm.findVertex(VertexID.parseID("C")).setAccept(false);
		fsm.findVertex(VertexID.parseID("D")).setAccept(false);
		testWsetconstruction(fsm,true,true,false);
	}
	
	/** Loop of sink states with an outgoing transition to another leaf sink. */
	@Test
	public final void testW_nonAccept6()
	{
		String machine = "A-a->A\nB-b->C-a->D-b->B\nC-p-#S";
		LearnerGraph fsm = buildLearnerGraph(machine,"testWset",config,converter);
		fsm.findVertex(VertexID.parseID("B")).setAccept(false);
		fsm.findVertex(VertexID.parseID("C")).setAccept(false);
		fsm.findVertex(VertexID.parseID("D")).setAccept(false);
		testWsetconstruction(fsm,true,true,false);
	}
	
	/** Loop of sink states with an outgoing transition to a non-sink state. */
	@Test
	public final void testW_nonAccept7()
	{
		String machine = "A-a->A\nB-b->C-a->D-b->B\nC-p->Q-p->R";
		LearnerGraph fsm = buildLearnerGraph(machine,"testWset",config,converter);
		fsm.findVertex(VertexID.parseID("B")).setAccept(false);
		fsm.findVertex(VertexID.parseID("C")).setAccept(false);
		fsm.findVertex(VertexID.parseID("D")).setAccept(false);
		fsm.findVertex(VertexID.parseID("Q")).setAccept(false);
		testWsetconstruction(fsm,false,true,false);
	}
	
	private void testReduction(String fsm, String expected)
	{
		LearnerGraph fsmL = buildLearnerGraph(fsm,"testReductionA",config,converter),
			expectedL = buildLearnerGraph(expected,"testReductionB",config,converter);
		WMethod.checkM(expectedL, fsmL.paths.reduce());
	}
	
	
	/** Tests that a process to minimise an automaton works, first with minimal machines. */
	@Test
	public final void testReduction1()
	{
		LearnerGraph fsmL = new LearnerGraph(config),
		expectedL = new LearnerGraph(config);
		WMethod.checkM(expectedL, fsmL.paths.reduce());
	}
	
	/** Tests that a process to minimise an automaton works, first with minimal machines. */
	@Test
	public final void testReduction2()
	{
		testReduction("A-a->B-a->C", "A-a->B-a->C");
	}
	
	/** Tests that a process to minimise an automaton works, first with minimal machines. */
	@Test
	public final void testReduction3()
	{
		testReduction("A-a->B-a->C-b->A", "A-a->B-a->C-b->A");
	}
	
	/** Tests that a process to minimise an automaton works, now with non minimal machines. */
	@Test
	public final void testReduction4()
	{
		testReduction("A-a->B-a->C-a->A", "A-a->A");
	}
	
	/** Tests that a process to minimise an automaton works, now with non minimal machines. */
	@Test
	public final void testReduction5()
	{
		testReduction("I-c->B / I-b->A-a->B-a->C-a->A", "I-c->A / I-b->A-a->A");
	}
	
	/** Tests that a process to minimise an automaton works, now with non minimal machines. */
	@Test
	public final void testReduction6()
	{
		testReduction("A-a->B-a->B2-a->B-b->C-b->C-c->D / B2-b->C2-b->C3-b->C4-b->C2-c->D2 / C3-c->D2 / C4-c->D2", 
				"A-a->B-a->B-b->C-b->C-c->D");
	}
	
	/** Tests that a process to minimise an automaton works, now with non minimal machines. */
	@Test
	public final void testReduction7()
	{
		testReduction("A-a-#B / A-b->A","A-b->A");
	}
	
	/** Tests that a process to minimise an automaton works, now with non minimal machines. */
	@Test
	public final void testReduction8()
	{
		testReduction("A-a-#B / A-b->A / A-c-#C","A-b->A");
	}
	
	/** All states are reject-states. */
	@Test
	public final void testReduction9()
	{
		LearnerGraph fsmL = buildLearnerGraph("A-a->B-a->C-b->A","testReduction9a",config,converter),
		expectedL = buildLearnerGraph("A-a->A","testReduction9b",config,converter);
		fsmL.findVertex(VertexID.parseID("A")).setAccept(false);
		fsmL.findVertex(VertexID.parseID("B")).setAccept(false);
		fsmL.findVertex(VertexID.parseID("C")).setAccept(false);
		expectedL.findVertex(VertexID.parseID("A")).setAccept(false);
		
		// this one directly verifies the machine.
		LearnerGraph outcome = fsmL.paths.reduce();
		Assert.assertEquals(0, outcome.getAcceptStateNumber());
		Assert.assertEquals(1, outcome.getStateNumber());
		Assert.assertEquals(0, outcome.pathroutines.countEdges());

		// there is a transition between them in the expected machine.
		LearnerGraph expectedA = new LearnerGraph(expectedL,config);
		WMethod.checkM(expectedA, fsmL.paths.reduce());
		
		// there are no transitions in the expected machine.
		LearnerGraph expectedB = new LearnerGraph(expectedL,config);
		expectedB.removeTransition(expectedL.transitionMatrix.get(
				expectedB.findVertex(VertexID.parseID("A"))), 
				AbstractLearnerGraph.generateNewLabel("a",config,converter),
				expectedB.findVertex(VertexID.parseID("A")));
		WMethod.checkM(expectedB, fsmL.paths.reduce());
	}
	
	/** There are some non-terminal states. */
	@Test
	public final void testReduction10()
	{
		LearnerGraph fsmL = buildLearnerGraph("A-a->B-a->C-c->A / A-b->B2-a->C2-c->A","testReduction10a",config,converter),
		expectedL = buildLearnerGraph("A-a->B-a->C-c->A","testReduction10b",config,converter);
		WMethod.checkM(expectedL, fsmL.paths.reduce());
	}
	
	/** There are some non-terminal states. */
	@Test
	public final void testReduction11()
	{
		LearnerGraph fsmL = buildLearnerGraph("A-a->B-a->C-c->A / A-b->B2-a->C2-c->A","testReduction11a",config,converter),
		expectedL = buildLearnerGraph("A-a->B-a->C-c->A","testReduction11b",config,converter);
		fsmL.findVertex(VertexID.parseID("B")).setAccept(false);
		WMethod.checkM(expectedL, fsmL.paths.reduce());
	}
	
	/** There are some non-terminal states. */
	@Test
	public final void testReduction12()
	{
		LearnerGraph fsmL = buildLearnerGraph("A-a->B-a->C-c->A / A-b->B2-a->C2-c->A","testReduction12a",config,converter),
		expectedL = buildLearnerGraph("A-a->B-a->C-c->A","testReduction12b",config,converter);
		fsmL.findVertex(VertexID.parseID("B")).setAccept(false);
		fsmL.findVertex(VertexID.parseID("B2")).setAccept(false);
		expectedL.findVertex(VertexID.parseID("B")).setAccept(false);
		WMethod.checkM(expectedL, fsmL.paths.reduce());
	}
	
	/** Adds an entry to the supplied map of pairs of states to labels which distinguish among those states.
	 * 
	 * @param distinguishingLabels map of pairs of states to labels which distinguish among those states
	 * @param stateA first state
	 * @param stateB second state
	 * @param labels labels to distinguish between states.
	 */
	private final void addDistLabels(Map<CmpVertex,Map<CmpVertex,Set<Label>>> distinguishingLabels, String stateA, String stateB, String [] labels)
	{
		Set<Label> distLabels = new HashSet<Label>();Map<CmpVertex,Set<Label>> stateToDist = new HashMap<CmpVertex,Set<Label>>();
		distLabels.clear();distLabels.addAll(labelList(labels));stateToDist.put(new StringVertex(stateB), distLabels);
		distinguishingLabels.put(new StringVertex(stateA),stateToDist);		
	}
	
	@Test
	public final void testComputeTopLabe0()
	{
		Map<CmpVertex,Map<CmpVertex,Set<Label>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<Label>>>();
		Assert.assertNull(WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabel1()
	{
		Map<CmpVertex,Map<CmpVertex,Set<Label>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<Label>>>();
		Assert.assertNull(WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe2()
	{
		Map<CmpVertex,Map<CmpVertex,Set<Label>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<Label>>>();
		addDistLabels(distinguishingLabels, "0","1",new String[] {"a"});
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("a",config,converter),WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe3()
	{
		Map<CmpVertex,Map<CmpVertex,Set<Label>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<Label>>>();
		addDistLabels(distinguishingLabels, "0","1",new String[] {"a"});
		addDistLabels(distinguishingLabels, "0","2",new String[] {"a"});
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("a",config,converter),WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe4()
	{
		Map<CmpVertex,Map<CmpVertex,Set<Label>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<Label>>>();
		addDistLabels(distinguishingLabels, "1","2",new String[] {"a"});
		addDistLabels(distinguishingLabels, "0","1",new String[] {"b"});
		addDistLabels(distinguishingLabels, "3","1",new String[] {"b"});
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("b",config,converter),WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testComputeTopLabe5()
	{
		Map<CmpVertex,Map<CmpVertex,Set<Label>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<Label>>>();
		addDistLabels(distinguishingLabels, "1","2",new String[] {"a"});
		addDistLabels(distinguishingLabels, "0","1",new String[] {"b"});
		addDistLabels(distinguishingLabels, "3","1",new String[] {"b"});
		addDistLabels(distinguishingLabels, "4","1",new String[] {});// empty entry
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("b",config,converter),WMethod.computeTopLabel(distinguishingLabels));
	}
	
	@Test
	public final void testCheckEquivalentStates1() // equivalent states
	{
		LearnerGraph fsm = 
				buildLearnerGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates1",config,null);
		Assert.assertEquals(true, WMethod.checkEquivalentStates(fsm));
	}
	
	@Test
	public final void testCheckEquivalentStates2() // no equivalent states
	{
		LearnerGraph fsm = 
				buildLearnerGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-m->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-z->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckEquivalentStates2",config,null);
		Assert.assertEquals(false, WMethod.checkEquivalentStates(fsm));
	}

	@Test
	public final void testCheckEquivalentStates3() // no equivalent states
	{
		int [][]table = new int[][] {
				{1,-1},
				{-1,1}
			};
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{0,1}, -1,config,converter);
		Assert.assertEquals(false, WMethod.checkEquivalentStates(fsm));
	}

	@Test
	public final void testCheckEquivalentStates4() // equivalent states
	{
		int [][]table = new int[][] {
				{1,-1},
				{1,-1}
			};
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{0,1}, -1,config,converter);
		Assert.assertEquals(true, WMethod.checkEquivalentStates(fsm));
	}
	
	private void checkTestGenerationResult(LearnerGraph fsm, int extraStates, String [][] expected)
	{
		Set<List<Label>> expectedSet = buildSet(expected,config,converter),
			actualA = new HashSet<List<Label>>(),actualB = new HashSet<List<Label>>();
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
		LearnerGraph fsm = buildLearnerGraph("A-a->A", "testTestGeneration2",config,converter);
		checkTestGenerationResult(fsm,0, new String[][] {
				new String[] {"a"}
			});
	}
	
	@Test
	public final void testTestGeneration3()
	{
		LearnerGraph fsm = buildLearnerGraph("A-a->A", "testTestGeneration3",config,converter);
		checkTestGenerationResult(fsm,1, new String[][] {
				new String[] {"a","a"}
			});
	}
	
	@Test
	public final void testTestGeneration4()
	{
		LearnerGraph fsm = buildLearnerGraph("A-a->A-b->B", "testTestGeneration4",config,converter);
		checkTestGenerationResult(fsm,0, new String[][] {
				new String[] {"a","a"},
				new String[] {"b","a"},
				new String[] {"b","b"}
			});
	}
	
	@Test
	public final void testTestGeneration5()
	{
		LearnerGraph fsm = buildLearnerGraph("A-a->A-b->B", "testTestGeneration5",config,converter);
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
		LearnerGraph fsm = buildLearnerGraph(
				"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA<-c-A2<-c-A3\n"+"A<-d-A5<-a-A3-b->A4-f->AA4-a->S\n"+
				"A-d->A\nA5-b->AA6", "testTestGeneration6",config,converter);
		Set<List<Label>> 
			actualA = new HashSet<List<Label>>(),actualB = new HashSet<List<Label>>();
			actualA.addAll(fsm.wmethod.computeOldTestSet(0));actualB.addAll(fsm.wmethod.computeNewTestSet(0));
			assertTrue("the two test generators return different values, old returns "+
					actualA+" and the new one - "+actualB,
			actualA.equals(actualB));
	}
	
	@Test
	public final void testTestGeneration7()
	{
		LearnerGraph fsm = buildLearnerGraph(
				"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA<-c-A2<-c-A3\n"+"A<-d-A5<-a-A3-b->A4-f->AA4-a->S\n"+
				"A-d->A\nA5-b->AA6", "testTestGeneration7",config,converter);
		Set<List<Label>> 
			actualA = new HashSet<List<Label>>(),actualB = new HashSet<List<Label>>();
			actualA.addAll(fsm.wmethod.computeOldTestSet(2));actualB.addAll(fsm.wmethod.computeNewTestSet(2));
			assertTrue("the two test generators return different values, old returns "+
					actualA+" and the new one - "+actualB,
			actualA.equals(actualB));
	}
	
	/** Almost the same as testTestGeneration7 but using a different initial state. */
	@Test
	public final void testTestGeneration8()
	{
		LearnerGraph fsmWW = buildLearnerGraph(
				"WW-s->WW\nS-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA<-c-A2<-c-A3\n"+"A<-d-A5<-a-A3-b->A4-f->AA4-a->S\n"+
				"A-d->A\nA5-b->AA6", "testTestGeneration7",config,converter);
		LearnerGraph fsmWOther = new LearnerGraph(fsmWW,fsmWW.config);fsmWOther.init=fsmWOther.findVertex(VertexID.parseID("S"));
		Set<List<Label>> 
			actualA = new HashSet<List<Label>>(),actualB = new HashSet<List<Label>>();
			actualA.addAll(fsmWOther.wmethod.computeOldTestSet(4));
			actualB.addAll(fsmWW.wmethod.computeNewTestSet(fsmWW.findVertex(VertexID.parseID("S")),4).getData());
			assertTrue("the two test generators return different values, old returns "+
					actualA+" and the new one - "+actualB,
			actualA.equals(actualB));
	}
	
	@Test
	public final void testEquivalentStates1()
	{
		LearnerGraph fsmWW = buildLearnerGraph(
				"A-a->B-b->C-b->B", "testEquivalentStates1",config,converter);
		Set<StatePair> equiv=new TreeSet<StatePair>();
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("B")),fsmWW.findVertex(VertexID.parseID("C"))));
		testWsetConstructionWithEquivalentStates(fsmWW,true,equiv);
		
	}
	
	@Test
	public final void testEquivalentStates2()
	{
		LearnerGraph fsmWW = buildLearnerGraph(
				"A-a->B-b-#C / B-c->B / D-b-#E", "testEquivalentStates2",config,converter);
		Set<StatePair> equiv=new TreeSet<StatePair>();
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("B")),fsmWW.findVertex(VertexID.parseID("D"))));
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("B")),fsmWW.findVertex(VertexID.parseID("C"))));
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("B")),fsmWW.findVertex(VertexID.parseID("E"))));
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("C")),fsmWW.findVertex(VertexID.parseID("D"))));
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("C")),fsmWW.findVertex(VertexID.parseID("E"))));
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("D")),fsmWW.findVertex(VertexID.parseID("E"))));
		testWsetConstructionWithEquivalentStates(fsmWW,true,equiv);
		
	}
	
	@Test
	public final void testEquivalentStates3()
	{
		LearnerGraph fsmWW = buildLearnerGraph(
				"A-a->B-b-#C / D-b-#E", "testEquivalentStates3",config,converter);
		Set<StatePair> equiv=new TreeSet<StatePair>();
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("B")),fsmWW.findVertex(VertexID.parseID("D"))));
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("B")),fsmWW.findVertex(VertexID.parseID("C"))));
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("B")),fsmWW.findVertex(VertexID.parseID("E"))));
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("C")),fsmWW.findVertex(VertexID.parseID("D"))));
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("C")),fsmWW.findVertex(VertexID.parseID("E"))));
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("D")),fsmWW.findVertex(VertexID.parseID("E"))));
		testWsetConstructionWithEquivalentStates(fsmWW,true,equiv);
		
	}
	
	@Test
	public final void testEquivalentStates4()
	{
		LearnerGraph fsmWW = buildLearnerGraph(
				"A-a->B-b-#C / D-b-#E", "testEquivalentStates3",config,converter);
		Set<StatePair> equiv=new TreeSet<StatePair>();
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("B")),fsmWW.findVertex(VertexID.parseID("D"))));
		equiv.add(new StatePair(fsmWW.findVertex(VertexID.parseID("C")),fsmWW.findVertex(VertexID.parseID("E"))));
		testWsetConstructionWithEquivalentStates(fsmWW,false,equiv);
	}
	
	@Test
	public final void testCheckUnreachable1()
	{
		assertFalse(buildLearnerGraph("A-a->A", "testCheckUnreachable1",config,converter).pathroutines.checkUnreachableStates());	
	}
	
	@Test
	public final void testCheckUnreachable2()
	{
		assertFalse(buildLearnerGraph("A-a->A-c->C\nB-a->A\nC-b->B", "testCheckUnreachable2",config,converter).pathroutines.checkUnreachableStates());	
	}
	
	@Test
	public final void testCheckUnreachable3()
	{
		assertTrue(buildLearnerGraph("A-a->A-c->C\nB-a->A", "testCheckUnreachable3",config,converter).pathroutines.checkUnreachableStates());	
	}
	
	/** Numerical labels are generated for short labels, long ones are text. */
	@Test
	public final void testCheckGraphNumeric1a()
	{
		Assert.assertFalse(buildLearnerGraph("Anonnum-a->Anonnum-c->Cnonnum","testCheckGraphNumeric",config,converter).wmethod.checkGraphNumeric());
	}
	
	/** Numerical labels are generated for short labels, long ones are text. */
	@Test
	public final void testCheckGraphNumeric1b()
	{
		Assert.assertTrue(buildLearnerGraph("A-a->A-c->C","testCheckGraphNumeric",config,converter).wmethod.checkGraphNumeric());
	}
	
	@Test
	public final void testCheckGraphNumeric2()
	{
		Assert.assertTrue(new LearnerGraph(config).wmethod.checkGraphNumeric());
	}
	
	@Test
	public final void testCheckGraphNumeric3()
	{
		LearnerGraph textGraph = buildLearnerGraph("A-a->A-c->C","testCheckGraphNumeric",config,converter);
		LearnerGraph numericGraph = new LearnerGraph(config);CmpVertex newInit = AbstractPathRoutines.addToGraph(numericGraph, textGraph,null);
		numericGraph = MergeStates.mergeAndDeterminize_general(numericGraph, new StatePair(numericGraph.paths.getVertex(new LinkedList<Label>()),newInit));
		Assert.assertTrue(numericGraph.wmethod.checkGraphNumeric());
	}
	
	@Test
	public final void testVertexToInt0()
	{
		LearnerGraph textGraph = buildLearnerGraph("A-a->A-b->B-c-#C","testVertexToInt0",config,converter);
		GDLearnerGraph ndGraph = new GDLearnerGraph(textGraph,LearnerGraphND.ignoreRejectStates,false);
		Assert.assertTrue(ndGraph.getStatesToNumber().containsKey(textGraph.findVertex("A")));
		Assert.assertTrue(ndGraph.getStatesToNumber().containsKey(textGraph.findVertex("B")));
		Assert.assertFalse(ndGraph.getStatesToNumber().containsKey(textGraph.findVertex("C")));
	}
	@Test
	public final void testVertexToInt1()
	{
		LearnerGraph textGraph = new LearnerGraph(config);
		GDLearnerGraph ndGraph = new GDLearnerGraph(textGraph,LearnerGraphND.ignoreRejectStates,false);
		CmpVertex A = textGraph.paths.getVertex(labelList(new String[]{}));
		Assert.assertEquals(0,textGraph.wmethod.vertexToInt(A,A));
		Assert.assertEquals(0,ndGraph.vertexToIntNR(A,A));
	}
	
	@Test
	public final void testVertexToInt2()
	{
		Configuration conf = config.copy();conf.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		LearnerGraph textGraph = buildLearnerGraph("A-a->A-b->B-c->C","testCheckGraphNumeric",config,converter);
		LearnerGraph numericGraph = new LearnerGraph(config);CmpVertex newInit = AbstractPathRoutines.addToGraph(numericGraph, textGraph,null);
		numericGraph = MergeStates.mergeAndDeterminize_general(numericGraph, new StatePair(newInit,numericGraph.paths.getVertex(new LinkedList<Label>())));
		GDLearnerGraph numericNDGraph = new GDLearnerGraph(numericGraph,LearnerGraphND.ignoreRejectStates,false);
		CmpVertex A = numericGraph.paths.getVertex(labelList(new String[]{})),
			B = numericGraph.paths.getVertex(labelList(new String[]{"b"})),
			C = numericGraph.paths.getVertex(labelList(new String[]{"b","c"}));
		/*  ABC
		 *A 013
		 *B 124
		 *C 345
		*/
		Assert.assertEquals(0,numericGraph.wmethod.vertexToInt(A,A));
		Assert.assertEquals(1,numericGraph.wmethod.vertexToInt(A,B));
		Assert.assertEquals(1,numericGraph.wmethod.vertexToInt(B,A));
		Assert.assertEquals(0,numericNDGraph.vertexToIntNR(A,A));
		Assert.assertEquals(1,numericNDGraph.vertexToIntNR(A,B));
		Assert.assertEquals(1,numericNDGraph.vertexToIntNR(B,A));

		Assert.assertEquals(3,numericGraph.wmethod.vertexToInt(A,C));
		Assert.assertEquals(3,numericGraph.wmethod.vertexToInt(C,A));
		Assert.assertEquals(3,numericNDGraph.vertexToIntNR(A,C));
		Assert.assertEquals(3,numericNDGraph.vertexToIntNR(C,A));

		Assert.assertEquals(2,numericGraph.wmethod.vertexToInt(B,B));
		Assert.assertEquals(2,numericNDGraph.vertexToIntNR(B,B));

		Assert.assertEquals(4,numericGraph.wmethod.vertexToInt(B,C));
		Assert.assertEquals(4,numericGraph.wmethod.vertexToInt(C,B));
		Assert.assertEquals(4,numericNDGraph.vertexToIntNR(B,C));
		Assert.assertEquals(4,numericNDGraph.vertexToIntNR(C,B));

		Assert.assertEquals(5,numericGraph.wmethod.vertexToInt(C,C));
		Assert.assertEquals(5,numericNDGraph.vertexToIntNR(C,C));
	}
	
	@Test
	public final void testVerifySameMergeResults1()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->A-b->B-c->C","testCheckGraphNumeric",config,converter);
		Configuration cloneConfig = graph.config.copy();cloneConfig.setLearnerCloneGraph(true);cloneConfig.setLearnerUseStrings(true);
		LearnerGraph g=new LearnerGraph(graph,cloneConfig);
		Assert.assertNull(WMethod.checkM_and_colours(graph,g,WMethod.VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertNull(WMethod.checkM_and_colours(g,graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** When vertices are being cloned, attributes are preserved. */
	@Test
	public final void testVerifySameMergeResults2()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->A-b->B-c->C","testCheckGraphNumeric",config,converter);
		graph.findVertex("B").setColour(JUConstants.BLUE);graph.findVertex("C").setColour(JUConstants.RED);
		VertexID vertid = VertexID.parseID("P78");
		graph.findVertex("C").setOrigState(vertid);
		Configuration cloneConfig = graph.config.copy();cloneConfig.setLearnerCloneGraph(true);cloneConfig.setLearnerUseStrings(true);
		LearnerGraph g=new LearnerGraph(graph,cloneConfig);
		g.findVertex("B").setHighlight(true);
		Assert.assertNotSame(vertid, g.findVertex("B").getOrigState());
		Assert.assertNull(WMethod.checkM(graph,g));
		Assert.assertNull(WMethod.checkM(g,graph));
		Assert.assertNotNull(WMethod.checkM_and_colours(graph,g,WMethod.VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertNotNull(WMethod.checkM_and_colours(g,graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	@Test
	public final void testVerifySameMergeResults3()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->D-b->B-c->C","testCheckGraphNumeric",config,converter);
		graph.findVertex("B").setColour(JUConstants.BLUE);graph.findVertex("C").setColour(JUConstants.RED);
		Configuration cloneConfig = graph.config.copy();cloneConfig.setLearnerCloneGraph(true);cloneConfig.setLearnerUseStrings(true);
		LearnerGraph g=new LearnerGraph(graph,cloneConfig);
		g.findVertex("B").setHighlight(true);g.findVertex("B").setColour(JUConstants.RED);
		Assert.assertNotNull(WMethod.checkM_and_colours(graph,g,WMethod.VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertNotNull(WMethod.checkM_and_colours(g,graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Graphs with different attributes on some of the states. */
	@Test
	public final void testVerifySameMergeResults4()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->D-b->B-c->C","testCheckGraphNumeric",config,converter);
		graph.findVertex("B").setColour(JUConstants.BLUE);graph.findVertex("C").setColour(JUConstants.RED);
		Configuration cloneConfig = graph.config.copy();cloneConfig.setLearnerCloneGraph(true);cloneConfig.setLearnerUseStrings(true);
		LearnerGraph g=new LearnerGraph(graph,cloneConfig);
		g.findVertex("B").setHighlight(true);g.findVertex("D").setColour(JUConstants.RED);
		Assert.assertNotNull(WMethod.checkM_and_colours(graph,g,WMethod.VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertNotNull(WMethod.checkM_and_colours(g,graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** Graphs with different attributes on the initial states. */
	@Test
	public final void testVerifySameMergeResults5()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->D-b->B-c->C","testCheckGraphNumeric",config,converter);
		graph.findVertex("A").setColour(JUConstants.BLUE);
		Configuration cloneConfig = graph.config.copy();cloneConfig.setLearnerCloneGraph(true);cloneConfig.setLearnerUseStrings(true);
		LearnerGraph g=new LearnerGraph(graph,cloneConfig);
		g.findVertex("A").setColour(JUConstants.RED);
		Assert.assertNotNull(WMethod.checkM_and_colours(graph,g,WMethod.VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertNotNull(WMethod.checkM_and_colours(g,graph,WMethod.VERTEX_COMPARISON_KIND.DEEP));
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
	
	/** In order to be able to use old junit runner.
	public static junit.framework.Test suite()
	{
		return new JUnit4TestAdapter(TestWMethod.class);
	}
	*/
}
