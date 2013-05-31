/* Copyright (c) 2013 The University of Sheffield.
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */ 
package statechum.analysis.learning.experiments.PairSelection;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Before;

import org.junit.Test;

import com.sun.org.apache.xpath.internal.operations.Minus;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper;
import statechum.Helper.whatToRun;
import statechum.JUConstants;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.PairMeasurements;
import statechum.analysis.learning.experiments.PairSelection.WekaPairClassifier.PairComparator;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;

/** This class tests that a simple Weka classifier can be instantiated and used to classify instances. It does not replace full Weka tests. */
public class TestWekaPairClassifier {
	final Configuration mainConfiguration = Configuration.getDefaultConfiguration().copy();
	final ConvertALabel converter = null;
	
	public TestWekaPairClassifier() {
		correctGraph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-a-#C", "testSplitFSM", mainConfiguration,converter);
		tentativeGraph = FsmParser.buildLearnerGraph("A1-a->B1-c->B2 / B1-b->A2 / B2-a-#C1 / B1-a-#C2", "testSplitPTA", mainConfiguration,converter);
	}

	
	private LearnerGraph correctGraph = null, tentativeGraph = null;

	@Test
	public void testSplitSetOfPairsIntoRightAndWrong1()
	{
		Collection<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		int outcome = PairQualityLearner.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{}), correctPairs, wrongPairs);
		Assert.assertEquals(JUConstants.intUNKNOWN, outcome);
		Assert.assertTrue(correctPairs.isEmpty());Assert.assertTrue(wrongPairs.isEmpty());
	}
	
	@Test
	public void testSplitSetOfPairsIntoRightAndWrong2()
	{
		Collection<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		PairScore pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A1"),0,0);
		
		int outcome = PairQualityLearner.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{
				pairA
		}), correctPairs, wrongPairs);
		Assert.assertEquals(0, outcome);
		Assert.assertEquals(1,correctPairs.size());Assert.assertEquals(pairA,correctPairs.iterator().next());
		Assert.assertTrue(wrongPairs.isEmpty());
	}
	
	@Test
	public void testSplitSetOfPairsIntoRightAndWrong3()
	{
		Collection<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		PairScore pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),0,0),pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),0,0),
				pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0);
		
		int outcome = PairQualityLearner.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{
				pairA,pairC,pairB
		}), correctPairs, wrongPairs);
		Assert.assertEquals(0, outcome);
		Assert.assertEquals(2,correctPairs.size());Iterator<PairScore> pairScoreIter=correctPairs.iterator();Assert.assertEquals(pairA,pairScoreIter.next());Assert.assertEquals(pairB,pairScoreIter.next());
		Assert.assertEquals(1,wrongPairs.size());Assert.assertEquals(pairC,wrongPairs.iterator().next());
	}
	
	@Test
	public void testSplitSetOfPairsIntoRightAndWrong4()
	{
		Collection<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		PairScore pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),0,0),pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),0,0),
				pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0);
		
		int outcome = PairQualityLearner.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{
				pairC,pairB,pairB,pairA
		}), correctPairs, wrongPairs);
		Assert.assertEquals(1, outcome);
		Assert.assertEquals(3,correctPairs.size());Iterator<PairScore> pairScoreIter=correctPairs.iterator();Assert.assertEquals(pairB,pairScoreIter.next());Assert.assertEquals(pairB,pairScoreIter.next());Assert.assertEquals(pairA,pairScoreIter.next());
		Assert.assertEquals(1,wrongPairs.size());Assert.assertEquals(pairC,wrongPairs.iterator().next());
	}
	
	@Test
	public void testSplitSetOfPairsIntoRightAndWrong5()
	{
		Collection<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		PairScore pairA = new PairScore(tentativeGraph.findVertex("C1"), tentativeGraph.findVertex("A2"),0,0),pairB=new PairScore(tentativeGraph.findVertex("C1"), tentativeGraph.findVertex("C2"),0,0),
				pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0);
		
		int outcome = PairQualityLearner.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{
				pairC,pairB,pairB,pairA
		}), correctPairs, wrongPairs);
		Assert.assertEquals(1, outcome);
		Assert.assertEquals(2,correctPairs.size());Iterator<PairScore> pairScoreIter=correctPairs.iterator();Assert.assertEquals(pairB,pairScoreIter.next());Assert.assertEquals(pairB,pairScoreIter.next());
		Assert.assertEquals(2,wrongPairs.size());pairScoreIter=wrongPairs.iterator();Assert.assertEquals(pairC,pairScoreIter.next());Assert.assertEquals(pairA,pairScoreIter.next());
	}
	
	@Test
	public void testSplitSetOfPairsIntoRightAndWrong6()
	{
		Collection<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		PairScore pairA = new PairScore(tentativeGraph.findVertex("C1"), tentativeGraph.findVertex("A2"),0,0),pairB=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("C2"),0,0),
				pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0);
		
		int outcome = PairQualityLearner.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{
				pairC,pairB,pairB,pairA
		}), correctPairs, wrongPairs);
		Assert.assertEquals(JUConstants.intUNKNOWN, outcome);
		Assert.assertTrue(correctPairs.isEmpty());Assert.assertEquals(4,wrongPairs.size());
		Iterator<PairScore> pairScoreIter=wrongPairs.iterator();Assert.assertEquals(pairC,pairScoreIter.next());Assert.assertEquals(pairB,pairScoreIter.next());
		Assert.assertEquals(pairB,pairScoreIter.next());Assert.assertEquals(pairA,pairScoreIter.next());
	}


	@Test
	public void testComputeTreeSize1()
	{
		LearnerGraph graph = new LearnerGraph(mainConfiguration);
		Assert.assertEquals(0,PairQualityLearner.computeTreeSize(graph,graph.getInit()));
	}
	
	@Test
	public void testComputeTreeSize2()
	{
		Assert.assertEquals(0,PairQualityLearner.computeTreeSize(tentativeGraph,tentativeGraph.findVertex(VertexID.parseID("A2"))));
	}
	
	@Test
	public void testComputeTreeSize3()
	{
		Assert.assertEquals(4,PairQualityLearner.computeTreeSize(tentativeGraph,tentativeGraph.findVertex(VertexID.parseID("B1"))));
	}
	
	@Test
	public void testComputeTreeSize4()
	{
		Assert.assertEquals(JUConstants.intUNKNOWN,PairQualityLearner.computeTreeSize(correctGraph,correctGraph.findVertex(VertexID.parseID("B"))));
	}
	
	@Test
	public void testComputeTreeSize5()
	{
		Assert.assertEquals(0,PairQualityLearner.computeTreeSize(correctGraph,correctGraph.findVertex(VertexID.parseID("C"))));
	}
	
	@Test
	public void testBuildSetsForComparators1()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		Map<StatePair,PairMeasurements> map = classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{}), tentativeGraph);
		Assert.assertTrue(map.isEmpty());
	}
	
	@Test
	public void testBuildSetsForComparators2()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("A1")),1,0);
		Map<StatePair,PairMeasurements> map = classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{
				pairA
		}), tentativeGraph);
		Assert.assertEquals(1,map.size());Assert.assertEquals(pairA,map.keySet().iterator().next());
		PairMeasurements m = map.get(pairA);
		Assert.assertFalse(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
	}
	
	@Test
	public void testBuildSetsForComparators3()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0)
				;
		Map<StatePair,PairMeasurements> map = classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{
				pairA
		}), tentativeGraph);
		Assert.assertEquals(1,map.size());Assert.assertEquals(pairA,map.keySet().iterator().next());
		PairMeasurements m = map.get(pairA);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
	}
	
	@Test
	public void testBuildSetsForComparators4()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairB = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A2")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0)
				;
		Map<StatePair,PairMeasurements> map = classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{
				pairA,pairB
		}), tentativeGraph);
		Assert.assertEquals(2,map.size());Iterator<StatePair> iter = map.keySet().iterator();Assert.assertEquals(pairA,iter.next());Assert.assertEquals(pairB,iter.next());
		PairMeasurements m = map.get(pairA);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
		m = map.get(pairB);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
	}
	
	@Test
	public void testBuildSetsForComparators5()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairB = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A2")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairC = new PairScore(tentativeGraph.findVertex(VertexID.parseID("C1")),tentativeGraph.findVertex(VertexID.parseID("C2")),2,0) // the score of 2 ensures it will be at the end of the keySet
				;
		Map<StatePair,PairMeasurements> map = classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{
				pairA,pairB,pairC
		}), tentativeGraph);
		Assert.assertEquals(3,map.size());Iterator<StatePair> iter = map.keySet().iterator();Assert.assertEquals(pairA,iter.next());Assert.assertEquals(pairB,iter.next());Assert.assertEquals(pairC,iter.next());
		PairMeasurements m = map.get(pairA);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
		m = map.get(pairB);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
		m = map.get(pairC);
		Assert.assertFalse(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
	}
	
	@Test
	public void testBuildSetsForComparators6()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairB = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A2")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairC = new PairScore(tentativeGraph.findVertex(VertexID.parseID("C1")),tentativeGraph.findVertex(VertexID.parseID("C2")),2,0) // the score of 2 ensures it will be at the end of the keySet
				;
		Map<StatePair,PairMeasurements> map = classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{
				pairA,pairB,pairC,pairC,pairC,pairC // we add the same pair a few times and it then seems to buildSetsForComparators that it has a few alternatives
		}), tentativeGraph);
		Assert.assertEquals(3,map.size());Iterator<StatePair> iter = map.keySet().iterator();Assert.assertEquals(pairA,iter.next());Assert.assertEquals(pairB,iter.next());Assert.assertEquals(pairC,iter.next());
		PairMeasurements m = map.get(pairA);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
		m = map.get(pairB);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
		m = map.get(pairC);
		Assert.assertFalse(m.adjacent);Assert.assertEquals(3,m.nrOfAlternatives);
	}
	
	/** Adjacency in Blue rather than in Red should not be considered */
	public void testBuildSetsForComparators7()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairB = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B2")),2,0) // the score of 2 ensures it will be at the end of the keySet
				;
		Map<StatePair,PairMeasurements> map = classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{
				pairA,pairB
		}), tentativeGraph);
		Assert.assertEquals(2,map.size());Iterator<StatePair> iter = map.keySet().iterator();Assert.assertEquals(pairA,iter.next());Assert.assertEquals(pairB,iter.next());
		PairMeasurements m = map.get(pairA);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
		m = map.get(pairB);
		Assert.assertFalse(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
	}
	
	@Test
	public void testSgn()
	{
		Assert.assertEquals(0, PairQualityLearner.sgn(0));
		Assert.assertEquals(1, PairQualityLearner.sgn(10));
		Assert.assertEquals(-1, PairQualityLearner.sgn(-10));
	}
	
	
	/** A simple test involving calling Weka functions directly. */
	@Test
	public void TestCreateInstances1()
	{
		int attributeNumber = 4;
		FastVector vecA = new FastVector(3);vecA.addElement(WekaPairClassifier.MINUSONE);vecA.addElement(WekaPairClassifier.ZERO);vecA.addElement(WekaPairClassifier.ONE);
		FastVector vecBool = new FastVector(2);vecBool.addElement(Boolean.TRUE.toString());vecBool.addElement(Boolean.FALSE.toString());
		Attribute attrA = new Attribute("a", vecA), attrB= new Attribute("b",vecA), attrC=new Attribute("c",vecA),attrClass=new Attribute("class",vecBool);
		
		FastVector attributes = new FastVector(attributeNumber);attributes.addElement(attrA);attributes.addElement(attrB);attributes.addElement(attrC);attributes.addElement(attrClass);
		Instances trainingData = new Instances("trainingdata",attributes,10);// this assigns indices to attributes, without these indices I cannot create instances.
		trainingData.setClassIndex(attrClass.index());
		Instance inst = new Instance(attributeNumber);
		inst.setValue(attrA,0);inst.setValue(attrB, 1);inst.setValue(attrC, 1);inst.setValue(attrClass, 0);
		Assert.assertEquals(4,trainingData.numAttributes());
		Assert.assertEquals(0,trainingData.numInstances());
		trainingData.add(inst);
		Assert.assertEquals(4,trainingData.numAttributes());
		Assert.assertEquals(1,trainingData.numInstances());
	}
	
	@Test
	public void testConstructEmptyInstance1()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		classifier.initialise("TestCreateInstances2", 10, new ArrayList<PairComparator>());
		Instance instance = classifier.constructInstance(new int []{},false);
		Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(1,instance.numValues());
		Assert.assertFalse(instance.hasMissingValue());
		Assert.assertTrue(instance.classAttribute().isNominal());
		Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
		Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
		Assert.assertEquals("false",instance.stringValue(instance.classAttribute()));
	}
	
	/** Construction of instances. */
	@Test
	public void testConstructEmptyInstance2()
	{
		final WekaPairClassifier classifier = new WekaPairClassifier();
		List<PairComparator> comps = new ArrayList<PairComparator>(20);
		classifier.initialise("TestCreateInstances2", 10, comps);
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{1},false);
			}
		}, IllegalArgumentException.class, "does not match");
		
	}
	
	/** Construction of instances. */
	@Test
	public void TestCreateInstances2()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		List<PairComparator> comps = new ArrayList<PairComparator>(20);
		comps.add(classifier.new PairComparator("conventional score")
		{// 1

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  PairQualityLearner.sgn(o1.getScore() - o2.getScore());
			}

		});
		classifier.initialise("TestCreateInstances2", 0, comps);
		
		Instance instance = classifier.constructInstance(new int []{1},true);
		Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(2,instance.numValues());
		Assert.assertFalse(instance.hasMissingValue());
		Assert.assertTrue(instance.classAttribute().isNominal());
		Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
		Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
		Assert.assertEquals(WekaPairClassifier.ONE,instance.stringValue(comps.get(0).att));
	}

	/** Construction of instances. Same as TestCreateInstances2 but initialises with zero max number of values in the training set. */
	@Test
	public void TestCreateInstances3()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		List<PairComparator> comps = new ArrayList<PairComparator>(20);
		comps.add(classifier.new PairComparator("conventional score")
		{// 1

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  PairQualityLearner.sgn(o1.getScore() - o2.getScore());
			}

		});
		classifier.initialise("TestCreateInstances2", 10, comps);
		
		Instance instance = classifier.constructInstance(new int []{1},true);
		Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(2,instance.numValues());
		Assert.assertFalse(instance.hasMissingValue());
		Assert.assertTrue(instance.classAttribute().isNominal());
		Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
		Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
		Assert.assertEquals(WekaPairClassifier.ONE,instance.stringValue(comps.get(0).att));
		Assert.assertTrue(comps.get(0).att.isNominal());
		Assert.assertEquals("conventional score",comps.get(0).att.name());
	}
	
	/** Construction of instances. Invalid value for an attribute. */
	@Test
	public void TestCreateInstances4()
	{
		final WekaPairClassifier classifier = new WekaPairClassifier();
		List<PairComparator> comps = new ArrayList<PairComparator>(20);
		comps.add(classifier.new PairComparator("conventional score")
		{// 1

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  PairQualityLearner.sgn(o1.getScore() - o2.getScore());
			}

		});
		classifier.initialise("TestCreateInstances2", 10, comps);

		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{2},true);
			}
		}, IllegalArgumentException.class, "invalid");
		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{-2},true);
			}
		}, IllegalArgumentException.class, "invalid");
	}
	
	/** Construction of instances. Same as TestCreateInstances2 but initialises with zero max number of values in the training set. */
	@Test
	public void TestCreateInstances5()
	{
		WekaPairClassifier classifier = new WekaPairClassifier();
		List<PairComparator> comps = new ArrayList<PairComparator>(20);
		comps.add(classifier.new PairComparator("conventional score")
		{// 1

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  PairQualityLearner.sgn(o1.getScore() - o2.getScore());
			}

		});
		comps.add(classifier.new PairComparator("another score")
		{// 2

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  PairQualityLearner.sgn(o1.getScore() - o2.getScore());
			}

		});
		classifier.initialise("TestCreateInstances2", 10, comps);
		
		Instance instance = classifier.constructInstance(new int []{1,0},true);
		Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(3,instance.numValues());
		Assert.assertFalse(instance.hasMissingValue());
		Assert.assertTrue(instance.classAttribute().isNominal());
		Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
		Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));

		Assert.assertEquals(WekaPairClassifier.ONE,instance.stringValue(comps.get(0).att));
		Assert.assertTrue(comps.get(0).att.isNominal());
		Assert.assertEquals("conventional score",comps.get(0).att.name());

		Assert.assertEquals(WekaPairClassifier.ZERO,instance.stringValue(comps.get(1).att));
		Assert.assertTrue(comps.get(1).att.isNominal());
		Assert.assertEquals("another score",comps.get(1).att.name());
	}
	
	
	WekaPairClassifier testClassifier;
	
	@Before
	public void beforeTest()
	{
		testClassifier = new WekaPairClassifier();
		List<PairComparator> comps = new ArrayList<PairComparator>(20);
		comps.add(testClassifier.new PairComparator("statechum score")
		{// 1

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  PairQualityLearner.sgn(o1.getScore() - o2.getScore());
			}

		});
		comps.add(testClassifier.new PairComparator("statechum compatibility score")
		{// 2

			@Override
			public int compare(PairScore o1, PairScore o2) {
				return  PairQualityLearner.sgn(o1.getAnotherScore() - o2.getAnotherScore());
			}

		});
		testClassifier.initialise("TestCreateInstances2", 10, comps);
	}
	
	/** Tests comparison of a pair to other pairs. All values are equal. */
	@Test
	public void TestCreateComparePairs1()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),0,0),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),0,0),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0);

		Assert.assertArrayEquals(new int[]{0,0},testClassifier.comparePairWithOthers(pairA, Arrays.asList(new PairScore[]{pairB,pairC})));
	}	
	
	/** Tests comparison of a pair to other pairs. The case of domination. */
	@Test
	public void TestCreateComparePairs2()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,0),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),0,0),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0);

		Assert.assertArrayEquals(new int[]{1,0},testClassifier.comparePairWithOthers(pairA, Arrays.asList(new PairScore[]{pairB,pairC})));
	}	
	
	/** Tests comparison of a pair to other pairs. The case of dominates or equals and that of being dominated */
	@Test
	public void TestCreateComparePairs3()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,-1),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),1,0),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0);

		Assert.assertArrayEquals(new int[]{1,-1},testClassifier.comparePairWithOthers(pairA, Arrays.asList(new PairScore[]{pairB,pairC})));
	}	
	
	/** Tests comparison of a pair to other pairs. Tests that contradictory responses are flagged. */
	@Test
	public void TestCreateComparePairs4()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,0),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),0,1),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),2,0);

		Assert.assertArrayEquals(new int[]{0,-1},testClassifier.comparePairWithOthers(pairA, Arrays.asList(new PairScore[]{pairB,pairC})));
	}
	
	/** Tests comparison of a pair to other pairs. Tests that contradictory responses are flagged. */
	@Test
	public void TestCreateComparePairs5()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,0),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),0,1),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),2,0);

		Assert.assertArrayEquals(new int[]{-1,1},testClassifier.comparePairWithOthers(pairB, Arrays.asList(new PairScore[]{pairA,pairC})));
	}
	
	/** Construction of instances. */
	@Test
	public void TestAddToDataset1()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,0),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),0,1),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),2,0);

		testClassifier.updateDatasetWithPairs(Arrays.asList(new PairScore[]{pairA,pairB,pairC}), tentativeGraph, correctGraph);
		@SuppressWarnings("unchecked")
		Enumeration<Instance> instEnum = testClassifier.trainingData.enumerateInstances();

		{// pairA - the correct pair
			Instance instance = instEnum.nextElement();
			Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(3,instance.numValues());
			Assert.assertFalse(instance.hasMissingValue());
			Assert.assertTrue(instance.classAttribute().isNominal());
			Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
	
			Assert.assertEquals(WekaPairClassifier.MINUSONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertTrue(testClassifier.comparators.get(0).att.isNominal());
			Assert.assertEquals("statechum score",testClassifier.comparators.get(0).att.name());
	
			Assert.assertEquals(WekaPairClassifier.ZERO,instance.stringValue(testClassifier.comparators.get(1).att));
			Assert.assertTrue(testClassifier.comparators.get(1).att.isNominal());
			Assert.assertEquals("statechum compatibility score",testClassifier.comparators.get(1).att.name());
		}
		
		{// pairB - another correct pair
			Instance instance = instEnum.nextElement();
			Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(3,instance.numValues());
			Assert.assertFalse(instance.hasMissingValue());
			Assert.assertTrue(instance.classAttribute().isNominal());
			Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
	
			Assert.assertEquals(WekaPairClassifier.MINUSONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertTrue(testClassifier.comparators.get(0).att.isNominal());
			Assert.assertEquals("statechum score",testClassifier.comparators.get(0).att.name());
	
			Assert.assertEquals(WekaPairClassifier.ONE,instance.stringValue(testClassifier.comparators.get(1).att));
			Assert.assertTrue(testClassifier.comparators.get(1).att.isNominal());
			Assert.assertEquals("statechum compatibility score",testClassifier.comparators.get(1).att.name());
		}
		
		{// pairC - incorrect pair
			Instance instance = instEnum.nextElement();
			Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(3,instance.numValues());
			Assert.assertFalse(instance.hasMissingValue());
			Assert.assertTrue(instance.classAttribute().isNominal());
			Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
	
			Assert.assertEquals(WekaPairClassifier.ONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertTrue(testClassifier.comparators.get(0).att.isNominal());
			Assert.assertEquals("statechum score",testClassifier.comparators.get(0).att.name());
	
			Assert.assertEquals(WekaPairClassifier.MINUSONE,instance.stringValue(testClassifier.comparators.get(1).att));
			Assert.assertTrue(testClassifier.comparators.get(1).att.isNominal());
			Assert.assertEquals("statechum compatibility score",testClassifier.comparators.get(1).att.name());
		}
		
		Assert.assertFalse(instEnum.hasMoreElements());
	}
	
	/** Construction of instances. */
	@Test
	public void TestAddToDataset2()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,0),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),0,1),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0);

		testClassifier.updateDatasetWithPairs(Arrays.asList(new PairScore[]{pairA,pairB,pairC}), tentativeGraph, correctGraph);
		@SuppressWarnings("unchecked")
		Enumeration<Instance> instEnum = testClassifier.trainingData.enumerateInstances();

		{// pairA - the correct pair
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaPairClassifier.ONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertEquals(WekaPairClassifier.ZERO,instance.stringValue(testClassifier.comparators.get(1).att));
		}
		
		{// pairB - another correct pair
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaPairClassifier.ZERO,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertEquals(WekaPairClassifier.ONE,instance.stringValue(testClassifier.comparators.get(1).att));
		}
		
		{// pairC - incorrect pair
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaPairClassifier.MINUSONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertEquals(WekaPairClassifier.MINUSONE,instance.stringValue(testClassifier.comparators.get(1).att));
		}
		
		Assert.assertFalse(instEnum.hasMoreElements());
	}
	
	/** Classification of instances. 
	 * @throws Exception */
	@Test
	public void TestClassification() throws Exception
	{
		testClassifier.trainingData.add(testClassifier.constructInstance(new int []{1,0},true));
		testClassifier.trainingData.add(testClassifier.constructInstance(new int []{0,1},true));
		testClassifier.trainingData.add(testClassifier.constructInstance(new int []{-1,-1},false));
		weka.classifiers.trees.J48 cl = new weka.classifiers.trees.J48();
		cl.buildClassifier(testClassifier.trainingData);
		
		Assert.assertEquals(1,cl.classifyInstance(testClassifier.constructInstance(new int []{1,0},false)), Configuration.fpAccuracy);
	}

}


