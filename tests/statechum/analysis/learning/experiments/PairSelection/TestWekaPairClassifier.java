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
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Before;

import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper;
import statechum.Helper.whatToRun;
import statechum.JUConstants;
import statechum.Label;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.CollectionOfPairsEstimator;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.PairMeasurements;
import statechum.analysis.learning.experiments.PairSelection.WekaDataCollector.PairRank;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure;
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
		WekaDataCollector classifier = new WekaDataCollector();
		Map<StatePair,PairMeasurements> map = classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{}), tentativeGraph);
		Assert.assertTrue(map.isEmpty());
		Assert.assertEquals(0, classifier.valueAverage.length);Assert.assertEquals(0, classifier.valueSD.length);
	}
	
	@Test
	public void testBuildSetsForComparators2()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("A1")),1,0);
		Map<StatePair,PairMeasurements> map = classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{
				pairA
		}), tentativeGraph);
		Assert.assertEquals(1,map.size());Assert.assertEquals(pairA,map.keySet().iterator().next());
		PairMeasurements m = map.get(pairA);
		Assert.assertFalse(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
		Assert.assertEquals(0, classifier.valueAverage.length);Assert.assertEquals(0, classifier.valueSD.length);
	}
	
	@Test
	public void testBuildSetsForComparators3()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0)
				;
		Map<StatePair,PairMeasurements> map = classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{
				pairA
		}), tentativeGraph);
		Assert.assertEquals(1,map.size());Assert.assertEquals(pairA,map.keySet().iterator().next());
		PairMeasurements m = map.get(pairA);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
		Assert.assertEquals(0, classifier.valueAverage.length);Assert.assertEquals(0, classifier.valueSD.length);
	}
	
	@Test
	public void testBuildSetsForComparators4()
	{
		WekaDataCollector classifier = new WekaDataCollector();
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
		Assert.assertEquals(0, classifier.valueAverage.length);Assert.assertEquals(0, classifier.valueSD.length);
	}
	
	@Test
	public void testBuildSetsForComparators5()
	{
		WekaDataCollector classifier = new WekaDataCollector();
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
		Assert.assertEquals(0, classifier.valueAverage.length);Assert.assertEquals(0, classifier.valueSD.length);
	}
	
	@Test
	public void testBuildSetsForComparators6()
	{
		WekaDataCollector classifier = new WekaDataCollector();
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
		Assert.assertEquals(0, classifier.valueAverage.length);Assert.assertEquals(0, classifier.valueSD.length);
	}
	
	/** Adjacency in Blue rather than in Red should not be considered */
	public void testBuildSetsForComparators7()
	{
		WekaDataCollector classifier = new WekaDataCollector();
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
		Assert.assertEquals(0, classifier.valueAverage.length);Assert.assertEquals(0, classifier.valueSD.length);
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
		FastVector vecA = new FastVector(3);vecA.addElement(WekaDataCollector.MINUSONE);vecA.addElement(WekaDataCollector.ZERO);vecA.addElement(WekaDataCollector.ONE);
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
		WekaDataCollector classifier = new WekaDataCollector();
		classifier.initialise("TestCreateInstances2", 10, new ArrayList<PairRank>());
		Instance instance = classifier.constructInstance(new int []{},new int []{},false);
		Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(1,instance.numValues());
		Assert.assertFalse(instance.hasMissingValue());
		Assert.assertTrue(instance.classAttribute().isNominal());
		Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
		Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
		Assert.assertEquals("false",instance.stringValue(instance.classAttribute()));
	}
	
	/** Construction of instances. */
	@Test
	public void testConstructEmptyInstance2a()
	{
		final WekaDataCollector classifier = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		classifier.initialise("TestCreateInstances2", 10, assessors);
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{1},new int []{}, false);
			}
		}, IllegalArgumentException.class, "does not match");
		
	}
	
	/** Construction of instances. */
	@Test
	public void testConstructEmptyInstance2b()
	{
		final WekaDataCollector classifier = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		classifier.initialise("TestCreateInstances2", 10, assessors);
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{},new int []{0}, false);
			}
		}, IllegalArgumentException.class, "does not match");
	}
	
	/** Construction of instances. */
	@Test
	public void TestCreateInstances2()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(classifier.new PairRank("statechum score")
		{
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}
		});
		classifier.initialise("TestCreateInstances2", 0, assessors);
		
		Instance instance = classifier.constructInstance(new int []{1},new int []{0},true);
		Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(3,instance.numValues());
		Assert.assertFalse(instance.hasMissingValue());
		Assert.assertTrue(instance.classAttribute().isNominal());
		Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
		Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
		Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(classifier.comparators.get(0).att));
		Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(assessors.get(0).att));
	}

	/** Construction of instances. Same as TestCreateInstances2 but initialises with zero max number of values in the training set. */
	@Test
	public void TestCreateInstances3()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(classifier.new PairRank("conventional score")
		{
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}
		});
		classifier.initialise("TestCreateInstances2", 10, assessors);

		Instance instance = classifier.constructInstance(new int []{1},new int []{-1},true);
		Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(3,instance.numValues());
		Assert.assertFalse(instance.hasMissingValue());
		Assert.assertTrue(instance.classAttribute().isNominal());
		Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
		Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));

		Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(classifier.comparators.get(0).att));
		Assert.assertTrue(assessors.get(0).att.isNominal());
		Assert.assertEquals("REL conventional score",classifier.comparators.get(0).att.name());

		Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(assessors.get(0).att));
		Assert.assertTrue(assessors.get(0).att.isNominal());
		Assert.assertEquals("conventional score",assessors.get(0).att.name());
	}
	
	/** Construction of instances. Invalid value for an attribute. */
	@Test
	public void TestCreateInstances4()
	{
		final WekaDataCollector classifier = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(classifier.new PairRank("conventional score")
		{
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}
		});
		classifier.initialise("TestCreateInstances2", 10, assessors);

		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{3},new int []{1},true);
			}
		}, IllegalArgumentException.class, "invalid");
		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{-3},new int []{1},true);
			}
		}, IllegalArgumentException.class, "invalid");
		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{1},new int []{3},true);
			}
		}, IllegalArgumentException.class, "invalid");
		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{1},new int []{-3},true);
			}
		}, IllegalArgumentException.class, "invalid");
	}
	
	/** Construction of instances. Same as TestCreateInstances2 but initialises with zero max number of values in the training set. */
	@Test
	public void TestCreateInstances5()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(classifier.new PairRank("conventional score")
		{// 1
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}
		});
		assessors.add(classifier.new PairRank("another score")
		{// 2
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}
		});
		classifier.initialise("TestCreateInstances2", 10, assessors);
		
		Instance instance = classifier.constructInstance(new int []{1,0},new int []{0,-1}, true);
		Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(5,instance.numValues());
		Assert.assertFalse(instance.hasMissingValue());
		Assert.assertTrue(instance.classAttribute().isNominal());
		Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
		Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));

		Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(classifier.comparators.get(0).att));
		Assert.assertTrue(classifier.comparators.get(0).att.isNominal());
		Assert.assertEquals("REL conventional score",classifier.comparators.get(0).att.name());

		Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(classifier.comparators.get(1).att));
		Assert.assertTrue(classifier.comparators.get(1).att.isNominal());
		Assert.assertEquals("REL another score",classifier.comparators.get(1).att.name());

		Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(assessors.get(0).att));
		Assert.assertTrue(assessors.get(0).att.isNominal());
		Assert.assertEquals("conventional score",assessors.get(0).att.name());

		Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(assessors.get(1).att));
		Assert.assertTrue(assessors.get(1).att.isNominal());
		Assert.assertEquals("another score",assessors.get(1).att.name());
	}
	
	
	@Test
	public void TestCreateInstances6()
	{
		final WekaDataCollector classifier = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(classifier.new PairRank("conventional score")
		{// 1
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}
		});
		assessors.add(classifier.new PairRank("another score")
		{// 2
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}
		});
		classifier.initialise("TestCreateInstances2", 10, assessors);
		
		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
			classifier.constructInstance(new int []{2,0},new int []{0,-2}, true);
			}
		}, IllegalArgumentException.class, "Value not defined");
	}
	
	@Test
	public void testComputeAverageAndSD0()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0);
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(classifier.new PairRank("conventional score")
		{
			@Override
			public long getValue(PairScore pair) {
				return pair.getScore();
			}
		});
		classifier.initialise("testComputeAverageAndSD1", 10, assessors);
		
		classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{
				pairA
		}), tentativeGraph);
		Assert.assertEquals(1,classifier.valueAverage.length);Assert.assertEquals(1,classifier.valueSD.length);
		double ave = 1;
		Assert.assertEquals(ave,classifier.valueAverage[0], Configuration.fpAccuracy);
		Assert.assertEquals( 0.,classifier.valueSD[0], Configuration.fpAccuracy);
	}
	
	@Test
	public void testComputeAverageAndSD1()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairB = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A2")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairC = new PairScore(tentativeGraph.findVertex(VertexID.parseID("C1")),tentativeGraph.findVertex(VertexID.parseID("C2")),2,0) // the score of 2 ensures it will be at the end of the keySet
				;
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(classifier.new PairRank("conventional score")
		{
			@Override
			public long getValue(PairScore pair) {
				return pair.getScore();
			}
		});
		classifier.initialise("testComputeAverageAndSD1", 10, assessors);
		
		classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{
				pairA,pairB,pairC
		}), tentativeGraph);
		Assert.assertEquals(1,classifier.valueAverage.length);Assert.assertEquals(1,classifier.valueSD.length);
		double ave = 4d/3;
		Assert.assertEquals(ave,classifier.valueAverage[0], Configuration.fpAccuracy);
		Assert.assertEquals( Math.sqrt(((1d-ave)*(1d-ave)*2+(2d-ave)*(2d-ave))/3),classifier.valueSD[0], Configuration.fpAccuracy);
	}

	
	WekaDataCollector testClassifier;
	
	@Before
	public void beforeTest()
	{
		testClassifier = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(testClassifier.new PairRank("statechum score")
		{// 1
			@Override
			public long getValue(PairScore pair) {
				return pair.getScore();
			}
		});
		assessors.add(testClassifier.new PairRank("statechum compatibility score")
		{// 2
			@Override
			public long getValue(PairScore pair) {
				return pair.getAnotherScore();
			}
		});
		testClassifier.initialise("TestCreateInstances2", 10, assessors);
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
	
	/** Tests comparison of a pair scores to average and SD, where there is only one pair. */
	@Test
	public void TestAssessPair0()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,-1);

		testClassifier.buildSetsForComparators(Arrays.asList(new PairScore[]{pairA}),tentativeGraph);
		Assert.assertArrayEquals(new int[]{0,0},testClassifier.assessPair(pairA));
	}	
	
	/** Tests comparison of a pair scores to average and SD. */
	@Test
	public void TestAssessPair1()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,-1),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),2,0),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0);

		testClassifier.buildSetsForComparators(Arrays.asList(new PairScore[]{pairA,pairB,pairC}),tentativeGraph);
		Assert.assertArrayEquals(new int[]{0,-1},testClassifier.assessPair(pairA));
		Assert.assertArrayEquals(new int[]{1,0},testClassifier.assessPair(pairB));
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
			Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(5,instance.numValues());
			Assert.assertFalse(instance.hasMissingValue());
			Assert.assertTrue(instance.classAttribute().isNominal());
			Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
	
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertTrue(testClassifier.comparators.get(0).att.isNominal());
			Assert.assertEquals("REL statechum score",testClassifier.comparators.get(0).att.name());
	
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(1).att));
			Assert.assertTrue(testClassifier.comparators.get(1).att.isNominal());
			Assert.assertEquals("REL statechum compatibility score",testClassifier.comparators.get(1).att.name());
		}
		
		{// pairB - another correct pair
			Instance instance = instEnum.nextElement();
			Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(5,instance.numValues());
			Assert.assertFalse(instance.hasMissingValue());
			Assert.assertTrue(instance.classAttribute().isNominal());
			Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
	
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertTrue(testClassifier.comparators.get(0).att.isNominal());
			Assert.assertEquals("REL statechum score",testClassifier.comparators.get(0).att.name());
	
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.comparators.get(1).att));
			Assert.assertTrue(testClassifier.comparators.get(1).att.isNominal());
			Assert.assertEquals("REL statechum compatibility score",testClassifier.comparators.get(1).att.name());
		}
		
		{// pairC - incorrect pair
			Instance instance = instEnum.nextElement();
			Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(5,instance.numValues());
			Assert.assertFalse(instance.hasMissingValue());
			Assert.assertTrue(instance.classAttribute().isNominal());
			Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
	
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertTrue(testClassifier.comparators.get(0).att.isNominal());
			Assert.assertEquals("REL statechum score",testClassifier.comparators.get(0).att.name());
	
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(1).att));
			Assert.assertTrue(testClassifier.comparators.get(1).att.isNominal());
			Assert.assertEquals("REL statechum compatibility score",testClassifier.comparators.get(1).att.name());
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
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(1).att));
		}
		
		{// pairB - another correct pair
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.comparators.get(1).att));
		}
		
		{// pairC - incorrect pair
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(1).att));
		}
		
		Assert.assertFalse(instEnum.hasMoreElements());
	}
	
	/** Construction of instances, reject-pairs are ignored. */
	@Test
	public void TestAddToDataset3()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairD1=new PairScore(tentativeGraph.findVertex("C1"), tentativeGraph.findVertex("A2"),0,0),// wrong pair
			pairD2=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("C2"),0,0),// wrong pair
			pairE=new PairScore(tentativeGraph.findVertex("C1"), tentativeGraph.findVertex("C2"),0,-1);// correct pair

		testClassifier.updateDatasetWithPairs(Arrays.asList(new PairScore[]{pairD1,pairD2,pairE}), tentativeGraph, correctGraph);
		Assert.assertEquals(0,testClassifier.trainingData.numInstances());
	}
	
	/** Construction of instances, multiple adds. */
	@Test
	public void TestAddToDataset4()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,0),// correct pair
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),0,1),// correct pair
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0),// wrong pair
			
			pairD=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("A2"),0,0),// wrong pair
			pairE=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,-1);// wrong pair

		testClassifier.updateDatasetWithPairs(Arrays.asList(new PairScore[]{pairA,pairB,pairC}), tentativeGraph, correctGraph);
		testClassifier.updateDatasetWithPairs(Arrays.asList(new PairScore[]{pairD,pairE}), tentativeGraph, correctGraph);
		@SuppressWarnings("unchecked")
		Enumeration<Instance> instEnum = testClassifier.trainingData.enumerateInstances();

		{// pairA - the correct pair, only compared with B and C
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(1).att));
			
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.assessors.get(0).att));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.assessors.get(1).att));
		}
		
		{// pairB - another correct pair, only compared with A and C
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.comparators.get(1).att));

			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.assessors.get(0).att));
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.assessors.get(1).att));
		}
		
		{// pairC - incorrect pair, only compared with A and B
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(1).att));

			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.assessors.get(0).att));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.assessors.get(1).att));
		}
		
		{// pairD - incorrect pair, compared only with E
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.comparators.get(1).att));

			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.assessors.get(0).att));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.assessors.get(1).att));		
		}
		
		{// pairE - incorrect pair, compared only with D
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.comparators.get(0).att));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.comparators.get(1).att));

			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.assessors.get(0).att));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.assessors.get(1).att));// this should perhaps return ONE but we are effectively comparing floating-point values by equality so the outcome is not ceertain  
		}
		
		Assert.assertFalse(instEnum.hasMoreElements());
	}
	
	/** Classification of instances. 
	 * @throws Exception if classification fails which signifies a test failure 
	 */
	@Test
	public void TestClassification() throws Exception
	{
		for(int i=0;i<10;++i)
		{// we add a lot of duplicate data because the learner expects a minimal number of entries per class  
			testClassifier.trainingData.add(testClassifier.constructInstance(new int []{1,0},new int []{1,0},true));
			testClassifier.trainingData.add(testClassifier.constructInstance(new int []{0,1},new int []{1,0},true));
			testClassifier.trainingData.add(testClassifier.constructInstance(new int []{-1,-1},new int []{1,0},false));
		}
		weka.classifiers.trees.J48 cl = new weka.classifiers.trees.J48();
		cl.buildClassifier(testClassifier.trainingData);
		Instance instance = testClassifier.constructInstance(new int []{1,0},new int []{1,0},true);
		Assert.assertEquals(instance.classValue(), cl.classifyInstance(instance),Configuration.fpAccuracy);
		
		instance = testClassifier.constructInstance(new int []{-1,0},new int []{1,0},false);
		Assert.assertEquals(instance.classValue(), cl.classifyInstance(instance),Configuration.fpAccuracy);
		
		instance = testClassifier.constructInstance(new int []{0,0},new int []{1,0},true);
		Assert.assertEquals(instance.classValue(), cl.classifyInstance(instance),Configuration.fpAccuracy);
	}
	
	@Test
	public void TestUniqueIntoState1()
	{
		LearnerGraph graph = new LearnerGraph(mainConfiguration);
		Assert.assertTrue(PairQualityLearner.uniqueIntoState(graph).isEmpty());
	}
	
	@Test
	public void TestUniqueIntoState2()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-a-#C", "testSplitFSM", mainConfiguration,converter);
		Map<Label,CmpVertex> map=PairQualityLearner.uniqueIntoState(graph);
		Assert.assertEquals(2, map.size());
		Iterator<Entry<Label,CmpVertex>> entryIter = map.entrySet().iterator();
		Entry<Label,CmpVertex> entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("b", mainConfiguration),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue());
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("c", mainConfiguration),entry.getKey());Assert.assertEquals(graph.findVertex("B"),entry.getValue());
	}

	@Test
	public void TestUniqueIntoState3()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->A", "TestUniqueIntoInitial3", mainConfiguration,converter);
		Assert.assertTrue(PairQualityLearner.uniqueIntoState(graph).isEmpty());
	}

	/** Multiple labels to choose from. */
	@Test
	public void TestUniqueIntoState4()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-d->A / B-a-#C", "TestUniqueIntoInitial4", mainConfiguration,converter);
		Map<Label,CmpVertex> map=PairQualityLearner.uniqueIntoState(graph);
		Assert.assertEquals(3, map.size());
		Iterator<Entry<Label,CmpVertex>> entryIter = map.entrySet().iterator();
		Entry<Label,CmpVertex> entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("b", mainConfiguration),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue());
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("c", mainConfiguration),entry.getKey());Assert.assertEquals(graph.findVertex("B"),entry.getValue());
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("d", mainConfiguration),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue());
	}

	/** Multiple labels to choose from. */
	@Test
	public void TestUniqueIntoState5()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-d->A / B-e-#C", "TestUniqueIntoInitial4", mainConfiguration,converter);
		Map<Label,CmpVertex> map=PairQualityLearner.uniqueIntoState(graph);
		Assert.assertEquals(5, map.size());
		Iterator<Entry<Label,CmpVertex>> entryIter = map.entrySet().iterator();
		Entry<Label,CmpVertex> entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("a", mainConfiguration),entry.getKey());Assert.assertEquals(graph.findVertex("B"),entry.getValue());
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("b", mainConfiguration),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue());
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("c", mainConfiguration),entry.getKey());Assert.assertEquals(graph.findVertex("B"),entry.getValue());
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("d", mainConfiguration),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue());
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("e", mainConfiguration),entry.getKey());Assert.assertEquals(graph.findVertex("C"),entry.getValue());
	}
	
	
	@Test
	public void TestConstructIfThenForUniques1()
	{
		LearnerEvaluationConfiguration evaluationConfiguration = new LearnerEvaluationConfiguration(mainConfiguration);
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-a-#C", "testSplitFSM", mainConfiguration,converter);
		Map<Label,CmpVertex> map=PairQualityLearner.uniqueIntoState(graph);
		Assert.assertNull(evaluationConfiguration.ifthenSequences);
		PairQualityLearner.addIfThenForMandatoryMerge(evaluationConfiguration, map);
		Assert.assertEquals(2,evaluationConfiguration.ifthenSequences.size());
		Iterator<String> ifthenIterator = evaluationConfiguration.ifthenSequences.iterator();
		Assert.assertEquals("Mandatory_1_to_A A- !b || toMerge_1_A ->A-b->B - b ->B / B- !b || toMerge_1_A ->A / B == THEN == C / C-toMerge_1_A->D",ifthenIterator.next());
		Assert.assertEquals("Mandatory_2_to_B A- !c || toMerge_2_B ->A-c->B - c ->B / B- !c || toMerge_2_B ->A / B == THEN == C / C-toMerge_2_B->D",ifthenIterator.next());
	}

	@Test
	public void TestConstructIfThenForUniques2()
	{
		LearnerEvaluationConfiguration evaluationConfiguration = new LearnerEvaluationConfiguration(mainConfiguration);
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-a-#C", "testSplitFSM", mainConfiguration,converter);
		Map<Label,CmpVertex> map=PairQualityLearner.uniqueIntoState(graph);
		evaluationConfiguration.ifthenSequences = new LinkedList<String>();evaluationConfiguration.ifthenSequences.add("junk");
		PairQualityLearner.addIfThenForMandatoryMerge(evaluationConfiguration, map);
		Assert.assertEquals(3,evaluationConfiguration.ifthenSequences.size());
		Iterator<String> ifthenIterator = evaluationConfiguration.ifthenSequences.iterator();
		Assert.assertEquals("junk",ifthenIterator.next());
		Assert.assertEquals("Mandatory_1_to_A A- !b || toMerge_1_A ->A-b->B - b ->B / B- !b || toMerge_1_A ->A / B == THEN == C / C-toMerge_1_A->D",ifthenIterator.next());
		Assert.assertEquals("Mandatory_2_to_B A- !c || toMerge_2_B ->A-c->B - c ->B / B- !c || toMerge_2_B ->A / B == THEN == C / C-toMerge_2_B->D",ifthenIterator.next());
	}
	
	@Test
	public void TestConstructIfThenForUniques3()
	{
		LearnerEvaluationConfiguration evaluationConfiguration = new LearnerEvaluationConfiguration(mainConfiguration);
		LearnerGraph graph = new LearnerGraph(mainConfiguration);
		Map<Label,CmpVertex> map=PairQualityLearner.uniqueIntoState(graph);
		evaluationConfiguration.ifthenSequences = new LinkedList<String>();evaluationConfiguration.ifthenSequences.add("junk");
		PairQualityLearner.addIfThenForMandatoryMerge(evaluationConfiguration, map);
		Assert.assertEquals(1,evaluationConfiguration.ifthenSequences.size());
		Iterator<String> ifthenIterator = evaluationConfiguration.ifthenSequences.iterator();
		Assert.assertEquals("junk",ifthenIterator.next());
	}
	
	/** No states are marked blue in the graph, hence the outcome is zero. */
	@Test
	public void TestComputeBlueStates1()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->D / B-c->E","TestComputeBlueStates1",mainConfiguration, converter);
		Set<CmpVertex> reds = new TreeSet<CmpVertex>();reds.add(graph.findVertex("B"));// only B is red
		Set<CmpVertex> tentativeReds = new TreeSet<CmpVertex>();tentativeReds.add(graph.findVertex("C"));
		
		List<CmpVertex> blueStates = LearnerThatUsesWekaResults.computeBlueStates(graph, reds, tentativeReds);
		Assert.assertEquals(0, blueStates.size());
	}
	
	@Test
	public void TestComputeBlueStates2()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->D / B-c->E","TestComputeBlueStates1",mainConfiguration, converter);
		graph.findVertex("C").setColour(JUConstants.BLUE);graph.findVertex("D").setColour(JUConstants.BLUE);graph.findVertex("E").setColour(JUConstants.BLUE);
		Set<CmpVertex> reds = new TreeSet<CmpVertex>();reds.add(graph.findVertex("B"));// only B is red
		Set<CmpVertex> tentativeReds = new TreeSet<CmpVertex>();tentativeReds.add(graph.findVertex("C"));
		
		List<CmpVertex> blueStates = LearnerThatUsesWekaResults.computeBlueStates(graph, reds, tentativeReds);
		Assert.assertEquals(2, blueStates.size());Assert.assertTrue(blueStates.contains(graph.findVertex("D")));Assert.assertTrue(blueStates.contains(graph.findVertex("E")));
	}
	
	/** All blue states are tentative red ones, hence not returned. */  
	@Test
	public void TestComputeBlueStates3()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->D / B-c->E","TestComputeBlueStates1",mainConfiguration, converter);
		graph.findVertex("C").setColour(JUConstants.BLUE);graph.findVertex("D").setColour(JUConstants.BLUE);graph.findVertex("E").setColour(JUConstants.BLUE);
		Set<CmpVertex> reds = new TreeSet<CmpVertex>();reds.add(graph.findVertex("B"));// only B is red
		Set<CmpVertex> tentativeReds = new TreeSet<CmpVertex>();tentativeReds.add(graph.findVertex("C"));tentativeReds.add(graph.findVertex("B"));tentativeReds.add(graph.findVertex("D"));tentativeReds.add(graph.findVertex("E"));
		
		List<CmpVertex> blueStates = LearnerThatUsesWekaResults.computeBlueStates(graph, reds, tentativeReds);
		Assert.assertEquals(0, blueStates.size());
	}
	
	/** Unconnected states, not marked as blue. */
	@Test
	public void TestComputeBlueStates4()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->D / B-c->E / F-a->G","TestComputeBlueStates4",mainConfiguration, converter);
		graph.findVertex("C").setColour(JUConstants.BLUE);graph.findVertex("D").setColour(JUConstants.BLUE);graph.findVertex("E").setColour(JUConstants.BLUE);
		Set<CmpVertex> reds = new TreeSet<CmpVertex>();reds.add(graph.findVertex("B"));reds.add(graph.findVertex("F"));// only B and F are red
		Set<CmpVertex> tentativeReds = new TreeSet<CmpVertex>();tentativeReds.add(graph.findVertex("C"));
		
		List<CmpVertex> blueStates = LearnerThatUsesWekaResults.computeBlueStates(graph, reds, tentativeReds);
		Assert.assertEquals(2, blueStates.size());Assert.assertTrue(blueStates.contains(graph.findVertex("D")));Assert.assertTrue(blueStates.contains(graph.findVertex("E")));
	}
	
	/** Unconnected states, marked as blue. */
	@Test
	public void TestComputeBlueStates5()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C / B-b->D / B-c->E / F-a->G","TestComputeBlueStates4",mainConfiguration, converter);
		graph.findVertex("C").setColour(JUConstants.BLUE);graph.findVertex("D").setColour(JUConstants.BLUE);graph.findVertex("E").setColour(JUConstants.BLUE);graph.findVertex("G").setColour(JUConstants.BLUE);
		Set<CmpVertex> reds = new TreeSet<CmpVertex>();reds.add(graph.findVertex("B"));reds.add(graph.findVertex("F"));// only B and F are red
		Set<CmpVertex> tentativeReds = new TreeSet<CmpVertex>();tentativeReds.add(graph.findVertex("C"));
		
		List<CmpVertex> blueStates = LearnerThatUsesWekaResults.computeBlueStates(graph, reds, tentativeReds);
		Assert.assertEquals(3, blueStates.size());Assert.assertTrue(blueStates.contains(graph.findVertex("D")));Assert.assertTrue(blueStates.contains(graph.findVertex("E")));Assert.assertTrue(blueStates.contains(graph.findVertex("G")));
	}
	
	
	/** This one uses chooseStatePairs with a stub of decision maker to compute different sets of pairs depending on the choices made by the decision procedure, 
	 * and compares them to choices made by the evaluator of the quality of the selection of red states. 
	 */
	@Test
	public void TestDecisionProcedureForRedStates1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C-a-#Rc / B-b->D-a-#Rd / B-c->E-a-#Re / F-a->G-a-#Rg","TestDecisionProcedureForRedStates1",mainConfiguration, converter);
		final Collection<CmpVertex> redToBeExpected = new ArrayList<CmpVertex>();redToBeExpected.addAll(Arrays.asList(new CmpVertex[]{graph.findVertex("C"),graph.findVertex("D"),graph.findVertex("E"),graph.findVertex("G")}));
		final Collection<CmpVertex> redsAlways = new ArrayList<CmpVertex>();redsAlways.addAll(Arrays.asList(new CmpVertex[]{graph.findVertex("A"),graph.findVertex("B"),graph.findVertex("F")}));
		TestDecisionProcedureForRedStatesHelper(graph,redToBeExpected,redsAlways, new LinkedList<CmpVertex>());
	}
	
	/** This one uses chooseStatePairs with a stub of decision maker to compute different sets of pairs depending on the choices made by the decision procedure, 
	 * and compares them to choices made by the evaluator of the quality of the selection of red states. 
	 */
	@Test
	public void TestDecisionProcedureForRedStates2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C-a-#Rc / B-b->D-a-#Rd / B-c->E-a-#Re / B-d->F","TestDecisionProcedureForRedStates2",mainConfiguration, converter);
		final Collection<CmpVertex> redToBeExpected = new ArrayList<CmpVertex>();redToBeExpected.addAll(Arrays.asList(new CmpVertex[]{graph.findVertex("C"),graph.findVertex("D"),graph.findVertex("E")}));
		final Collection<CmpVertex> redsAlways = new ArrayList<CmpVertex>();redsAlways.addAll(Arrays.asList(new CmpVertex[]{graph.findVertex("A"),graph.findVertex("B")}));
		TestDecisionProcedureForRedStatesHelper(graph,redToBeExpected,redsAlways, Arrays.asList(new CmpVertex[]{graph.findVertex("F")}));
	}
	
	protected void TestDecisionProcedureForRedStatesHelper(final LearnerGraph graph, final Collection<CmpVertex> redToBeExpected, final Collection<CmpVertex> redsAlways,final Collection<CmpVertex> blueStates)
	{
		final Map<CmpVertex,Collection<PairScore>> redToPairsObtained = new TreeMap<CmpVertex,Collection<PairScore>>();
		final Set<CmpVertex> tentativeRedsChosen = new TreeSet<CmpVertex>();// vertices already chosen.
		
		for(CmpVertex bestVertex:redToBeExpected)
		{
			graph.clearColours();
			for(CmpVertex vertRed:redsAlways) vertRed.setColour(JUConstants.RED);
			for(CmpVertex vertToColourBlue:redToBeExpected)	vertToColourBlue.setColour(JUConstants.BLUE);for(CmpVertex vertToColourBlue:blueStates)	vertToColourBlue.setColour(JUConstants.BLUE);
			final CmpVertex bestVertexFinal = bestVertex;
			
			Collection<PairScore> pairsReturned = graph.pairscores.chooseStatePairs(new RedNodeSelectionProcedure() {
				
				CmpVertex redChosen = null;
				
				@Override
				public CmpVertex selectRedNode(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
				{
					Assert.assertNull(redChosen);
					Assert.assertSame(graph,coregraph);
					Assert.assertTrue(reds.contains(graph.findVertex("A")));Assert.assertTrue(reds.contains(graph.findVertex("B")));
					Assert.assertTrue(redToBeExpected.equals(tentativeRedNodes));
					
					Set<CmpVertex> available = new TreeSet<CmpVertex>();available.addAll(tentativeRedNodes);available.removeAll(tentativeRedsChosen);
					redChosen = bestVertexFinal;
					return redChosen;
				}
				
				@SuppressWarnings("unused")
				@Override
				public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<PairScore> pairs) 
				{
					Assert.assertNotNull(redChosen);
					ArrayList<PairScore> copyOfPairs = new ArrayList<PairScore>(pairs);Collections.sort(copyOfPairs);
					redToPairsObtained.put(redChosen,copyOfPairs);
					return null;// no resolution
				}
			});
			
			Assert.assertEquals(redToPairsObtained.get(bestVertex),pairsReturned);
		}
		
		
		// Now I verify that if my quality selection routine returns the expected value for a collection of pairs corre
		for(CmpVertex bestVertex:redToBeExpected)
		{
			graph.clearColours();
			for(CmpVertex vertRed:redsAlways) vertRed.setColour(JUConstants.RED);
			for(CmpVertex vertToColourBlue:redToBeExpected)	vertToColourBlue.setColour(JUConstants.BLUE);for(CmpVertex vertToColourBlue:blueStates)	vertToColourBlue.setColour(JUConstants.BLUE);

			final CmpVertex bestVertexFinal = bestVertex;
			Collection<PairScore> pairsReturned = graph.pairscores.chooseStatePairs(new RedNodeSelectionProcedure() {
				
				@Override
				public CmpVertex selectRedNode(LearnerGraph coregraph, final Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
				{
					final Set<Collection<PairScore>> collectionOfPairsToSee = new HashSet<Collection<PairScore>>();collectionOfPairsToSee.addAll(redToPairsObtained.values());
					CmpVertex nodeSelected = LearnerThatUsesWekaResults.selectRedNode(coregraph, reds, tentativeRedNodes, new CollectionOfPairsEstimator() {
	
						@Override
						public double obtainEstimateOfTheQualityOfTheCollectionOfPairs(LearnerGraph argGraph, Collection<PairScore> pairs) 
						{
							Assert.assertSame(graph,argGraph);Assert.assertEquals(redsAlways,reds);
							Assert.assertTrue(collectionOfPairsToSee.remove(pairs));// we check that pairs passed to us were the same as those computed during the forward-run of the chooseStatePairs.
								// It is important to do this check because pairs passed to obtainEstimateOfTheQualityOfTheCollectionOfPairs are computed by selectRedNode.
							
							if (pairs.equals(redToPairsObtained.get(bestVertexFinal))) // pairs passed to this one should be some of the same collections of pairs seen by resolvePotentialDeadEnd above. 
								return 1;
							return 0;
						}
						
					});
					Assert.assertTrue(collectionOfPairsToSee.isEmpty());
					Assert.assertEquals(bestVertexFinal,nodeSelected);// check that the expected vertex has been selected
					return bestVertexFinal;
				}
	
				@SuppressWarnings("unused")
				@Override
				public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<PairScore> pairs) {
					return null;
				}
			});
			
			Assert.assertEquals(redToPairsObtained.get(bestVertex),pairsReturned);
		}
	}
}


