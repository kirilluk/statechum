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
import java.util.LinkedHashSet;
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
import statechum.Pair;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.CollectionOfPairsEstimator;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.PairMeasurements;
import statechum.analysis.learning.experiments.PairSelection.WekaDataCollector.PairRank;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.apps.QSMTool;
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
		int outcome = LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{}), correctPairs, wrongPairs);
		Assert.assertEquals(JUConstants.intUNKNOWN, outcome);
		Assert.assertTrue(correctPairs.isEmpty());Assert.assertTrue(wrongPairs.isEmpty());
	}
	
	@Test
	public void testSplitSetOfPairsIntoRightAndWrong2()
	{
		Collection<PairScore> correctPairs = new LinkedList<PairScore>(), wrongPairs = new LinkedList<PairScore>();
		PairScore pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A1"),0,0);
		
		int outcome = LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{
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
		
		int outcome = LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{
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
		
		int outcome = LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{
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
		
		int outcome = LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{
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
		
		int outcome = LearningSupportRoutines.SplitSetOfPairsIntoRightAndWrong(tentativeGraph, correctGraph, Arrays.asList(new PairScore[]{
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
		List<PairScore> pairs = Arrays.asList(new PairScore[]{});
		classifier.buildSetsForComparators(pairs, tentativeGraph);
		Assert.assertTrue(classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.isEmpty());
		Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage.length);Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueSD.length);
	}
	
	@Test
	public void testBuildSetsForComparators2()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("A1")),1,0);
		List<PairScore> pairs = Arrays.asList(new PairScore[]{
				pairA
		});
		classifier.buildSetsForComparators(pairs, tentativeGraph);

		Assert.assertEquals(1,classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.size());
		Assert.assertEquals(pairA,classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.keySet().iterator().next());
		PairMeasurements m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairA);
		Assert.assertFalse(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
		Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage.length);Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueSD.length);
	}
	
	@Test
	public void testBuildSetsForComparators3()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0)
				;
		List<PairScore> pairs = Arrays.asList(new PairScore[]{
				pairA
		});
		classifier.buildSetsForComparators(pairs, tentativeGraph);

		Assert.assertEquals(1,classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.size());
		Assert.assertEquals(pairA,classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.keySet().iterator().next());
		PairMeasurements m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairA);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
		Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage.length);Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueSD.length);
	}
	
	@Test
	public void testBuildSetsForComparators4()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairB = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A2")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0)
				;
		List<PairScore> pairs = Arrays.asList(new PairScore[]{
				pairA,pairB
		});
		classifier.buildSetsForComparators(pairs, tentativeGraph);

		Assert.assertEquals(2,classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.size());
		Set<StatePair> expectedInMap = new LinkedHashSet<StatePair>();expectedInMap.add(pairA);expectedInMap.add(pairB);
		Assert.assertEquals(expectedInMap,classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.keySet());
		PairMeasurements m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairA);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
		m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairB);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
		Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage.length);Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueSD.length);
	}
	
	@Test
	public void testBuildSetsForComparators5()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairB = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A2")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairC = new PairScore(tentativeGraph.findVertex(VertexID.parseID("C1")),tentativeGraph.findVertex(VertexID.parseID("C2")),2,0) // the score of 2 ensures it will be at the end of the keySet
				;
		List<PairScore> pairs = Arrays.asList(new PairScore[]{
				pairA,pairB,pairC
		});
		classifier.buildSetsForComparators(pairs, tentativeGraph);

		Assert.assertEquals(3,classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.size());
		Set<StatePair> expectedInMap = new LinkedHashSet<StatePair>();expectedInMap.add(pairA);expectedInMap.add(pairB);expectedInMap.add(pairC);
		Assert.assertEquals(expectedInMap,classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.keySet());
		
		PairMeasurements m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairA);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
		m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairB);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
		m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairC);
		Assert.assertFalse(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
		Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage.length);Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueSD.length);
	}
	
	@Test
	public void testBuildSetsForComparators6()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairB = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A2")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairC = new PairScore(tentativeGraph.findVertex(VertexID.parseID("C1")),tentativeGraph.findVertex(VertexID.parseID("C2")),2,0) // the score of 2 ensures it will be at the end of the keySet
				;
		List<PairScore> pairs = Arrays.asList(new PairScore[]{
				pairA,pairB,pairC,pairC,pairC,pairC // we add the same pair a few times and it then seems to buildSetsForComparators that it has a few alternatives
		});
		classifier.buildSetsForComparators(pairs, tentativeGraph);

		Assert.assertEquals(3,classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.size());
		Set<StatePair> expectedInMap = new LinkedHashSet<StatePair>();expectedInMap.add(pairA);expectedInMap.add(pairB);expectedInMap.add(pairC);
		Assert.assertEquals(expectedInMap,classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.keySet());

		PairMeasurements m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairA);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
		m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairB);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(1,m.nrOfAlternatives);
		m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairC);
		Assert.assertFalse(m.adjacent);Assert.assertEquals(3,m.nrOfAlternatives);
		Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage.length);Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueSD.length);
	}
	
	/** Adjacency in Blue rather than in Red should not be considered */
	public void testBuildSetsForComparators7()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		PairScore pairA = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B1")),1,0),
				pairB = new PairScore(tentativeGraph.findVertex(VertexID.parseID("A1")),tentativeGraph.findVertex(VertexID.parseID("B2")),2,0) // the score of 2 ensures it will be at the end of the keySet
				;
		List<PairScore> pairs = Arrays.asList(new PairScore[]{
				pairA,pairB
		});
		classifier.buildSetsForComparators(pairs, tentativeGraph);

		Assert.assertEquals(2,classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.size());
		Iterator<StatePair> iter = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.keySet().iterator();Assert.assertEquals(pairA,iter.next());Assert.assertEquals(pairB,iter.next());
		PairMeasurements m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairA);
		Assert.assertTrue(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
		m = classifier.measurementsForUnfilteredCollectionOfPairs.measurementsForComparators.get(pairB);
		Assert.assertFalse(m.adjacent);Assert.assertEquals(0,m.nrOfAlternatives);
		Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage.length);Assert.assertEquals(0, classifier.measurementsForUnfilteredCollectionOfPairs.valueSD.length);
	}
		
	@Test
	public void testSgn()
	{
		Assert.assertEquals(0, LearningSupportRoutines.signum(0));
		Assert.assertEquals(1, LearningSupportRoutines.signum(10));
		Assert.assertEquals(-1, LearningSupportRoutines.signum(-10));
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
	public void testConstructTooBig1()
	{
		final WekaDataCollector classifier = new WekaDataCollector();
		final List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(classifier.new PairRank("statechum score")
		{
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				classifier.initialise("TestCreateInstances2", 10, assessors,40);
			}
		}, IllegalArgumentException.class, "too many levels");
	}
	
	@Test
	public void testConstructTooBig2()
	{
		final WekaDataCollector classifier = new WekaDataCollector();
		final List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(classifier.new PairRank("statechum score")
		{
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		for(int j=0;j<10;++j)
			assessors.add(classifier.new PairRank("another score"+j)
			{
				@Override
				public long getValue(@SuppressWarnings("unused") PairScore pair) {
					throw new UnsupportedOperationException("in this test, this method should not be called");
				}
	
				@Override
				public boolean isAbsolute() {
					return false;
				}
			});
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				classifier.initialise("TestCreateInstances2", 10, assessors,15);
			}
		}, IllegalArgumentException.class, "too many attributes per instance");
	}
	
	@Test
	public void testConstructTooBig3()
	{
		final WekaDataCollector classifier = new WekaDataCollector();
		final List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(classifier.new PairRank("statechum score")
		{
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		for(int j=0;j<70;++j)
			assessors.add(classifier.new PairRank("another score"+j)
			{
				@Override
				public long getValue(@SuppressWarnings("unused") PairScore pair) {
					throw new UnsupportedOperationException("in this test, this method should not be called");
				}
	
				@Override
				public boolean isAbsolute() {
					return false;
				}
			});
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				classifier.initialise("TestCreateInstances2", 10, assessors,15);
			}
		}, IllegalArgumentException.class, "will not fit into");
	}
	
	@Test
	public void testConstructEmptyInstance1()
	{
		WekaDataCollector classifier = new WekaDataCollector();
		classifier.initialise("TestCreateInstances2", 10, new ArrayList<PairRank>(),0);
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
	public void testConstructEmptyInstance2a()
	{
		final WekaDataCollector classifier = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		classifier.initialise("TestCreateInstances2", 10, assessors,0);
		Helper.checkForCorrectException(new whatToRun() {
			@Override
			public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{1}, false);
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

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		classifier.initialise("TestCreateInstances2", 0, assessors,0);
		
		Instance instance = classifier.constructInstance(new int []{1,0},true);
		Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(3,instance.numValues());
		Assert.assertFalse(instance.hasMissingValue());
		Assert.assertTrue(instance.classAttribute().isNominal());
		Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
		Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
		Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(classifier.attributesOfAnInstance[0]));
		Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(classifier.attributesOfAnInstance[1]));
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

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		classifier.initialise("TestCreateInstances2", 10, assessors,0);

		Instance instance = classifier.constructInstance(new int []{1,-1},true);
		Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(3,instance.numValues());
		Assert.assertFalse(instance.hasMissingValue());
		Assert.assertTrue(instance.classAttribute().isNominal());
		Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
		Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));

		Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(classifier.attributesOfAnInstance[0]));
		Assert.assertTrue(classifier.attributesOfAnInstance[0].isNominal());
		Assert.assertEquals("REL conventional score",classifier.attributesOfAnInstance[0].name());

		Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(classifier.attributesOfAnInstance[1]));
		Assert.assertTrue(classifier.attributesOfAnInstance[1].isNominal());
		Assert.assertEquals("conventional score",classifier.attributesOfAnInstance[1].name());
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

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		classifier.initialise("TestCreateInstances2", 10, assessors,0);

		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{3,1},true);
			}
		}, IllegalArgumentException.class, "invalid");
		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{-3,1},true);
			}
		}, IllegalArgumentException.class, "invalid");
		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{1,3},true);
			}
		}, IllegalArgumentException.class, "invalid");
		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
				classifier.constructInstance(new int []{1,-3},true);
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

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		assessors.add(classifier.new PairRank("another score")
		{// 2
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		classifier.initialise("TestCreateInstances2", 10, assessors,0);
		
		Instance instance = classifier.constructInstance(new int []{1,0,0,-1}, true);
		Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(5,instance.numValues());
		Assert.assertFalse(instance.hasMissingValue());
		Assert.assertTrue(instance.classAttribute().isNominal());
		Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
		Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));

		Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(classifier.attributesOfAnInstance[0]));
		Assert.assertTrue(classifier.attributesOfAnInstance[0].isNominal());
		Assert.assertEquals("REL conventional score",classifier.attributesOfAnInstance[0].name());

		Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(classifier.attributesOfAnInstance[1]));
		Assert.assertTrue(classifier.attributesOfAnInstance[1].isNominal());
		Assert.assertEquals("REL another score",classifier.attributesOfAnInstance[1].name());

		Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(classifier.attributesOfAnInstance[2]));
		Assert.assertTrue(classifier.attributesOfAnInstance[2].isNominal());
		Assert.assertEquals("conventional score",classifier.attributesOfAnInstance[2].name());

		Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(classifier.attributesOfAnInstance[3]));
		Assert.assertTrue(classifier.attributesOfAnInstance[3].isNominal());
		Assert.assertEquals("another score",classifier.attributesOfAnInstance[3].name());
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

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		assessors.add(classifier.new PairRank("another score")
		{// 2
			@Override
			public long getValue(@SuppressWarnings("unused") PairScore pair) {
				throw new UnsupportedOperationException("in this test, this method should not be called");
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		classifier.initialise("TestCreateInstances2", 10, assessors,0);
		
		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
			classifier.constructInstance(new int []{2,0,0,-2}, true);
			}
		}, IllegalArgumentException.class, "was not defined");
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

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		classifier.initialise("testComputeAverageAndSD1", 10, assessors,0);
		
		List<PairScore> pairs = Arrays.asList(new PairScore[]{
				pairA
		});
		classifier.buildSetsForComparators(pairs, tentativeGraph);

		Assert.assertEquals(1,classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage.length);Assert.assertEquals(1,classifier.measurementsForUnfilteredCollectionOfPairs.valueSD.length);
		double ave = 1;
		Assert.assertEquals(ave,classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage[0], Configuration.fpAccuracy);
		Assert.assertEquals( 0.,classifier.measurementsForUnfilteredCollectionOfPairs.valueSD[0], Configuration.fpAccuracy);
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

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		classifier.initialise("testComputeAverageAndSD1", 10, assessors,0);
		
		List<PairScore> pairs = Arrays.asList(new PairScore[]{
				pairA,pairB,pairC
		});
		classifier.buildSetsForComparators(pairs, tentativeGraph);

		Assert.assertEquals(1,classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage.length);Assert.assertEquals(1,classifier.measurementsForUnfilteredCollectionOfPairs.valueSD.length);
		double ave = 4d/3;
		Assert.assertEquals(ave,classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage[0], Configuration.fpAccuracy);
		Assert.assertEquals( Math.sqrt(((1d-ave)*(1d-ave)*2+(2d-ave)*(2d-ave))/3),classifier.measurementsForUnfilteredCollectionOfPairs.valueSD[0], Configuration.fpAccuracy);
	}

	@Test
	public void testComputeAverageAndSD1_absolute()
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

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		classifier.initialise("testComputeAverageAndSD1", 10, assessors,0);
		
		List<PairScore> pairs = Arrays.asList(new PairScore[]{
				pairA,pairB,pairC
		});
		classifier.buildSetsForComparators(pairs, tentativeGraph);
		Assert.assertEquals(1,classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage.length);Assert.assertEquals(1,classifier.measurementsForUnfilteredCollectionOfPairs.valueSD.length);
		double ave = 4d/3;
		Assert.assertEquals(ave,classifier.measurementsForUnfilteredCollectionOfPairs.valueAverage[0], Configuration.fpAccuracy);
		Assert.assertEquals( Math.sqrt(((1d-ave)*(1d-ave)*2+(2d-ave)*(2d-ave))/3),classifier.measurementsForUnfilteredCollectionOfPairs.valueSD[0], Configuration.fpAccuracy);
		
		Assert.assertEquals(1, assessors.get(0).getValue(pairA) );
		Assert.assertEquals(1, assessors.get(0).getValue(pairB) );
		Assert.assertEquals(2, assessors.get(0).getValue(pairC) );
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

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		assessors.add(testClassifier.new PairRank("statechum compatibility score")
		{// 2
			@Override
			public long getValue(PairScore pair) {
				return pair.getAnotherScore();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		testClassifier.initialise("TestCreateInstances2", 10, assessors,0);
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

		List<PairScore> pairs = Arrays.asList(new PairScore[]{pairB,pairC});
		testClassifier.buildSetsForComparators(pairs, tentativeGraph);
		int [] buffer= new int[2];
		testClassifier.comparePairWithOthers(pairA, pairs,buffer,0);
		Assert.assertArrayEquals(new int[]{0,0},buffer);
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

		List<PairScore> pairs = Arrays.asList(new PairScore[]{pairB,pairC});
		testClassifier.buildSetsForComparators(pairs, tentativeGraph);
		int [] buffer= new int[2];
		testClassifier.comparePairWithOthers(pairA, pairs,buffer,0);
		Assert.assertArrayEquals(new int[]{1,0},buffer);
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

		List<PairScore> pairs = Arrays.asList(new PairScore[]{pairB,pairC});
		testClassifier.buildSetsForComparators(pairs, tentativeGraph);
		int [] buffer= new int[2];
		testClassifier.comparePairWithOthers(pairA, pairs,buffer,0);
		Assert.assertArrayEquals(new int[]{1,-1},buffer);
	}	
	
	/** Tests comparison of a pair to other pairs. The case of dominates or equals and that of being dominated. Checks that values can be recorded at an offset. */
	@Test
	public void TestCreateComparePairs3a()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,-1),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),1,0),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0);

		List<PairScore> pairs = Arrays.asList(new PairScore[]{pairB,pairC});
		testClassifier.buildSetsForComparators(pairs, tentativeGraph);
		int [] buffer= new int[]{9,8,7,6};
		testClassifier.comparePairWithOthers(pairA, pairs,buffer,1);
		Assert.assertArrayEquals(new int[]{9,1,-1,6},buffer);
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

		List<PairScore> pairs = Arrays.asList(new PairScore[]{pairB,pairC});
		testClassifier.buildSetsForComparators(pairs, tentativeGraph);
		int [] buffer= new int[2];
		testClassifier.comparePairWithOthers(pairA, pairs,buffer,0);
		Assert.assertArrayEquals(new int[]{0,-1},buffer);
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

		List<PairScore> pairs = Arrays.asList(new PairScore[]{pairA,pairC});
		testClassifier.buildSetsForComparators(pairs, tentativeGraph);
		int [] buffer= new int[2];
		testClassifier.comparePairWithOthers(pairB, pairs,buffer,0);
		Assert.assertArrayEquals(new int[]{-1,1},buffer);
	}
	
	
	/** Tests comparison of a pair to other pairs, taking into account if-then conditions. */
	@Test
	public void TestCreateComparePairs6()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,-1),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),0,0),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,1),
			pairD=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),1,0);

		WekaDataCollector dataCollector = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(dataCollector.new PairRank("statechum score")
		{// 1
			@Override
			public long getValue(PairScore pair) {
				return pair.getScore();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		assessors.add(testClassifier.new PairRank("statechum compatibility score")
		{// 2
			@Override
			public long getValue(PairScore pair) {
				return pair.getAnotherScore();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		dataCollector.initialise("TestCreateInstances2", 10, assessors,1);
		List<PairScore> pairs = Arrays.asList(new PairScore[]{pairA,pairB,pairC,pairD});
		dataCollector.buildSetsForComparators(pairs, tentativeGraph);
		int []comparisonResults = new int[dataCollector.getInstanceLength()];
		dataCollector.fillInPairDetails(comparisonResults,pairA, pairs);
		Assert.assertEquals(1,comparisonResults[0]); // a
		Assert.assertEquals(-1,comparisonResults[1]);// b

		// a=-1 4+4*0 0, 0,0,0
		// a=1  4+4*1 0,-1,@,@
		// b=-1 4+4*2 0, 0,0,0
		// b=1  4+4*3 0, 0,0,0
		
		Assert.assertEquals(0,comparisonResults[4]);
		Assert.assertEquals(0,comparisonResults[5]);
		Assert.assertEquals(0,comparisonResults[6]);
		Assert.assertEquals(0,comparisonResults[7]);

		Assert.assertEquals(0,comparisonResults[8]);
		Assert.assertEquals(-1,comparisonResults[9]);

		Assert.assertEquals(0,comparisonResults[12]);
		Assert.assertEquals(0,comparisonResults[13]);
		Assert.assertEquals(0,comparisonResults[14]);
		Assert.assertEquals(0,comparisonResults[15]);

		Assert.assertEquals(0,comparisonResults[16]);
		Assert.assertEquals(0,comparisonResults[17]);
		Assert.assertEquals(0,comparisonResults[18]);
		Assert.assertEquals(0,comparisonResults[19]);
	}
	
	/** Tests comparison of a pair to other pairs, taking into account if-then conditions. */
	@Test
	public void TestCreateComparePairs7()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		final PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,1),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),1,1),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0),
			pairD=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),1,0);

		WekaDataCollector dataCollector = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(dataCollector.new PairRank("statechum score")
		{// 1
			@Override
			public long getValue(PairScore pair) {
				return pair.getScore();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		assessors.add(testClassifier.new PairRank("statechum compatibility score")
		{// 2
			@Override
			public long getValue(PairScore pair) {
				return pair.getAnotherScore();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		assessors.add(testClassifier.new PairRank("1 for B or C, 0 otherwise")
		{// 3
			@Override
			public long getValue(PairScore pair) {
				return (pair == pairB || pair == pairC)?1:0;
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		dataCollector.initialise("TestCreateInstances2", 10, assessors,2);
		List<PairScore> pairs = Arrays.asList(new PairScore[]{pairA,pairB,pairC,pairD});
		dataCollector.buildSetsForComparators(pairs, tentativeGraph);
		int []comparisonResults = new int[dataCollector.getInstanceLength()];
		dataCollector.fillInPairDetails(comparisonResults,pairA, pairs);
		Assert.assertEquals(1,comparisonResults[0]); // a
		Assert.assertEquals(1,comparisonResults[1]);// b
		Assert.assertEquals(-1,comparisonResults[2]);// c

		// a=-1 6+6*0 0, 0,0
		// a=1  6+6*1 0,-1,@
		// b=-1 6+6*2 0, 0,0
		// b=1  6+6*3 0, 0,0
		// c=-1 6+6*4 0, 0,0
		// c=1  6+6*5 0, 0,0
		
		// The pairs are: 			A=(1,1,0), B=(1,1,1), C=(0,0,1), D=(1,0,0) 
		//comparing A with others, 	  (1,1,-1)
		Assert.assertEquals(0,comparisonResults[6+0]); // a
		Assert.assertEquals(0,comparisonResults[6+1]);// b
		Assert.assertEquals(0,comparisonResults[6+2]);// c

		// a is at 1, A is being compared to B and D
		Assert.assertEquals(0, comparisonResults[6+6*1+0]); // a
		Assert.assertEquals(1, comparisonResults[6+6*1+1]);// if a=1 then b=1
		Assert.assertEquals(-1,comparisonResults[6+6*1+2]);// if a=1 then c=-1

		Assert.assertEquals(0, comparisonResults[6+6*2+0]); // a
		Assert.assertEquals(0, comparisonResults[6+6*2+1]);// b
		Assert.assertEquals(0, comparisonResults[6+6*2+2]);// c

		// b is at 1, A is being compared to B
		Assert.assertEquals(0, comparisonResults[6+6*3+0]); // a
		Assert.assertEquals(0, comparisonResults[6+6*3+1]);// b
		Assert.assertEquals(-1,comparisonResults[6+6*3+2]);// if b=1 then c=-1

		// c is at -1, A is being compared to D
		Assert.assertEquals(0, comparisonResults[6+6*4+0]); // a
		Assert.assertEquals(1, comparisonResults[6+6*4+1]);// if c=-1 then b=1
		Assert.assertEquals(0, comparisonResults[6+6*4+2]);// c

		Assert.assertEquals(0, comparisonResults[6+6*5+0]); // a
		Assert.assertEquals(0, comparisonResults[6+6*5+1]);// b
		Assert.assertEquals(0, comparisonResults[6+6*5+2]);// c
		
		//      a=-1 	a=1 	b=-1 	b=1		c=-1	c=1 (the "if" component above)
		// a=-1  				8		12		16		20
		// a=1   				9		13		17		21
		// b=-1  0		4						18		22
		// b=1   1		[5]						[19]	23
		// c=-1  2		[6]		10		[14]	
		// c=1   3		7		11		15
		// ^
		// |
		// the "then" component above
		
		// if a=1 then b=1 hence we are comparing A with B at this stage
		// if a=1 then c=-1 hence we are comparing A with D at this stage
		// if b=1 then c=-1, then no pair other than A with b at 1 and c at -1
		// if a=1 then b=1, then no pair other than A with b at 1 and c at -1
		final int s=6+6*6;
		for(int i=0;i<16;++i)
			if (i != 5 && i != 6)
			{
				Assert.assertEquals("position "+i,0, comparisonResults[s+6*i+0]); // a
				Assert.assertEquals("position "+i,0, comparisonResults[s+6*i+1]);// b
				Assert.assertEquals("position "+i,0, comparisonResults[s+6*i+2]);// c
			}
		
		// if a=1 then b=1, then comparing A and B
		Assert.assertEquals(0, comparisonResults[s+6*5+0]); // a
		Assert.assertEquals(0, comparisonResults[s+6*5+1]);// b
		Assert.assertEquals(-1,comparisonResults[s+6*5+2]);// c=-1
		
		// if a=1 then c=-1, then comparing A and D
		Assert.assertEquals(0, comparisonResults[s+6*6+0]); // a
		Assert.assertEquals(1, comparisonResults[s+6*6+1]);// b = 1
		Assert.assertEquals(0, comparisonResults[s+6*6+2]);// c=-1
		
		
	}
	
	/** Tests comparison of a pair to other pairs, taking into account if-then conditions. */
	@Test
	public void TestCreateComparePairs6_arrayTooSmall()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		final PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,-1),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),0,0),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,1),
			pairD=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),1,0);

		final WekaDataCollector dataCollector = new WekaDataCollector();
		List<PairRank> assessors = new ArrayList<PairRank>(20);
		assessors.add(dataCollector.new PairRank("statechum score")
		{// 1
			@Override
			public long getValue(PairScore pair) {
				return pair.getScore();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		assessors.add(testClassifier.new PairRank("statechum compatibility score")
		{// 2
			@Override
			public long getValue(PairScore pair) {
				return pair.getAnotherScore();
			}

			@Override
			public boolean isAbsolute() {
				return false;
			}
		});
		dataCollector.initialise("TestCreateInstances2", 10, assessors,1);
		final List<PairScore> pairs = Arrays.asList(new PairScore[]{pairB,pairC,pairD});
		dataCollector.buildSetsForComparators(pairs, tentativeGraph);
		final int []comparisonResults = new int[1];
		Helper.checkForCorrectException(new whatToRun() { @Override	public void run() throws NumberFormatException
			{
				dataCollector.fillInPairDetails(comparisonResults,pairA, pairs);
			}
		}, IllegalArgumentException.class, "array is too short");
	}
	
	/** Tests comparison of a pair scores to average and SD, where there is only one pair. */
	@Test
	public void TestAssessPair0()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,-1);

		List<PairScore> pairs = Arrays.asList(new PairScore[]{pairA});
		testClassifier.buildSetsForComparators(pairs, tentativeGraph);
		int [] buffer= new int[2];
		testClassifier.assessPair(pairA,testClassifier.measurementsForUnfilteredCollectionOfPairs,buffer,0);Assert.assertArrayEquals(new int[]{0,0},buffer);
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

		List<PairScore> pairs = Arrays.asList(new PairScore[]{pairA,pairB,pairC});
		testClassifier.buildSetsForComparators(pairs, tentativeGraph);
		int [] buffer= new int[2];
		testClassifier.assessPair(pairA,testClassifier.measurementsForUnfilteredCollectionOfPairs,buffer,0);Assert.assertArrayEquals(new int[]{0,-1},buffer);
		testClassifier.assessPair(pairB,testClassifier.measurementsForUnfilteredCollectionOfPairs,buffer,0);Assert.assertArrayEquals(new int[]{1,0},buffer);
	}	
	
	/** Tests comparison of a pair scores to average and SD. Takes an offset into account. */
	@Test
	public void TestAssessPair1a()
	{
		// Using test data from testSplitSetOfPairsIntoRightAndWrong3, pairs A and B are right and C is wrong. 
		PairScore 
			pairA = new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("A2"),1,-1),
			pairB=new PairScore(tentativeGraph.findVertex("B1"), tentativeGraph.findVertex("B2"),2,0),
			pairC=new PairScore(tentativeGraph.findVertex("A1"), tentativeGraph.findVertex("B2"),0,0);

		List<PairScore> pairs = Arrays.asList(new PairScore[]{pairA,pairB,pairC});
		testClassifier.buildSetsForComparators(pairs, tentativeGraph);
		int [] buffer= new int[]{9,8,7,6};
		testClassifier.assessPair(pairA,testClassifier.measurementsForUnfilteredCollectionOfPairs,buffer,1);Assert.assertArrayEquals(new int[]{9,0,-1,6},buffer);
		testClassifier.assessPair(pairB,testClassifier.measurementsForUnfilteredCollectionOfPairs,buffer,1);Assert.assertArrayEquals(new int[]{9,1,0,6},buffer);
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
	
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.attributesOfAnInstance[0]));
			Assert.assertTrue(testClassifier.attributesOfAnInstance[0].isNominal());
			Assert.assertEquals("REL statechum score",testClassifier.attributesOfAnInstance[0].name());
	
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[1]));
			Assert.assertTrue(testClassifier.attributesOfAnInstance[1].isNominal());
			Assert.assertEquals("REL statechum compatibility score",testClassifier.attributesOfAnInstance[1].name());
		}
		
		{// pairB - another correct pair
			Instance instance = instEnum.nextElement();
			Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(5,instance.numValues());
			Assert.assertFalse(instance.hasMissingValue());
			Assert.assertTrue(instance.classAttribute().isNominal());
			Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
	
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[0]));
			Assert.assertTrue(testClassifier.attributesOfAnInstance[0].isNominal());
			Assert.assertEquals("REL statechum score",testClassifier.attributesOfAnInstance[0].name());
	
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.attributesOfAnInstance[1]));
			Assert.assertTrue(testClassifier.attributesOfAnInstance[1].isNominal());
			Assert.assertEquals("REL statechum compatibility score",testClassifier.attributesOfAnInstance[1].name());
		}
		
		{// pairC - incorrect pair
			Instance instance = instEnum.nextElement();
			Assert.assertFalse(instance.classIsMissing());Assert.assertEquals(5,instance.numValues());
			Assert.assertFalse(instance.hasMissingValue());
			Assert.assertTrue(instance.classAttribute().isNominal());
			Assert.assertEquals(2,instance.classAttribute().numValues());// true/false
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
	
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.attributesOfAnInstance[0]));
			Assert.assertTrue(testClassifier.attributesOfAnInstance[0].isNominal());
			Assert.assertEquals("REL statechum score",testClassifier.attributesOfAnInstance[0].name());
	
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[1]));
			Assert.assertTrue(testClassifier.attributesOfAnInstance[1].isNominal());
			Assert.assertEquals("REL statechum compatibility score",testClassifier.attributesOfAnInstance[1].name());
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
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.attributesOfAnInstance[0]));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[1]));
		}
		
		{// pairB - another correct pair
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[0]));
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.attributesOfAnInstance[1]));
		}
		
		{// pairC - incorrect pair
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[0]));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[1]));
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
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.attributesOfAnInstance[0]));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[1]));
			
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.attributesOfAnInstance[2]));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.attributesOfAnInstance[3]));
		}
		
		{// pairB - another correct pair, only compared with A and C
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("true",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[0]));
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.attributesOfAnInstance[1]));

			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.attributesOfAnInstance[2]));
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.attributesOfAnInstance[3]));
		}
		
		{// pairC - incorrect pair, only compared with A and B
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[0]));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[1]));

			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.attributesOfAnInstance[2]));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.attributesOfAnInstance[3]));
		}
		
		{// pairD - incorrect pair, compared only with E
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.attributesOfAnInstance[0]));
			Assert.assertEquals(WekaDataCollector.ONE,instance.stringValue(testClassifier.attributesOfAnInstance[1]));

			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.attributesOfAnInstance[2]));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.attributesOfAnInstance[3]));		
		}
		
		{// pairE - incorrect pair, compared only with D
			Instance instance = instEnum.nextElement();
			Assert.assertEquals("false",instance.classAttribute().value((int) instance.value(instance.classAttribute())));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.attributesOfAnInstance[0]));
			Assert.assertEquals(WekaDataCollector.MINUSONE,instance.stringValue(testClassifier.attributesOfAnInstance[1]));

			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.attributesOfAnInstance[2]));
			Assert.assertEquals(WekaDataCollector.ZERO,instance.stringValue(testClassifier.attributesOfAnInstance[3]));// this should perhaps return ONE but we are effectively comparing floating-point values by equality so the outcome is not ceertain  
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
			testClassifier.trainingData.add(testClassifier.constructInstance(new int []{1,0,1,0},true));
			testClassifier.trainingData.add(testClassifier.constructInstance(new int []{0,1,1,0},true));
			testClassifier.trainingData.add(testClassifier.constructInstance(new int []{-1,-1,1,0},false));
		}
		weka.classifiers.trees.J48 cl = new weka.classifiers.trees.J48();
		cl.buildClassifier(testClassifier.trainingData);
		Instance instance = testClassifier.constructInstance(new int []{1,0,1,0},true);
		Assert.assertEquals(instance.classValue(), cl.classifyInstance(instance),Configuration.fpAccuracy);
		
		instance = testClassifier.constructInstance(new int []{-1,0,1,0},false);
		Assert.assertEquals(instance.classValue(), cl.classifyInstance(instance),Configuration.fpAccuracy);
		
		instance = testClassifier.constructInstance(new int []{0,0,1,0},true);
		Assert.assertEquals(instance.classValue(), cl.classifyInstance(instance),Configuration.fpAccuracy);
	}
	
	@Test
	public void TestUniqueIntoState1()
	{
		LearnerGraph graph = new LearnerGraph(mainConfiguration);
		Assert.assertTrue(LearningSupportRoutines.uniqueIntoState(graph).isEmpty());
	}
	
	@Test
	public void TestUniqueFromState1()
	{
		LearnerGraph graph = new LearnerGraph(mainConfiguration);
		Assert.assertTrue(LearningSupportRoutines.uniqueFromState(graph).isEmpty());
	}
	
	@Test
	public void TestUniqueIntoState2()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-a-#C", "testSplitFSM", mainConfiguration,converter);
		Map<Label,Pair<CmpVertex,CmpVertex>> mapUniqueInto=LearningSupportRoutines.uniqueIntoState(graph);
		Assert.assertEquals(2, mapUniqueInto.size());
		for(Entry<Label,Pair<CmpVertex,CmpVertex>> entry:mapUniqueInto.entrySet()) Assert.assertSame(entry.getValue().secondElem, graph.transitionMatrix.get(entry.getValue().firstElem).get(entry.getKey()));
		Iterator<Entry<Label,Pair<CmpVertex,CmpVertex>>> entryIter = mapUniqueInto.entrySet().iterator();
		Entry<Label,Pair<CmpVertex,CmpVertex>> entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("b", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("c", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.findVertex("B"),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
	}

	@Test
	public void TestUniqueIntoState3()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->A", "TestUniqueIntoInitial3", mainConfiguration,converter);
		Assert.assertTrue(LearningSupportRoutines.uniqueIntoState(graph).isEmpty());
	}

	@Test
	public void TestUniqueFromState3()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->A", "TestUniqueIntoInitial3", mainConfiguration,converter);
		Assert.assertTrue(LearningSupportRoutines.uniqueFromState(graph).isEmpty());
	}

	/** Multiple labels to choose from. */
	@Test
	public void TestUniqueIntoState4a()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-d->A / B-a-#C", "TestUniqueIntoInitial4", mainConfiguration,converter);
		Map<Label,Pair<CmpVertex,CmpVertex>> mapUniqueInfo=LearningSupportRoutines.uniqueIntoState(graph);
		for(Entry<Label,Pair<CmpVertex,CmpVertex>> entry:mapUniqueInfo.entrySet()) Assert.assertSame(entry.getValue().secondElem, graph.transitionMatrix.get(entry.getValue().firstElem).get(entry.getKey()));
		Assert.assertEquals(3, mapUniqueInfo.size());
		Iterator<Entry<Label,Pair<CmpVertex,CmpVertex>>> entryIter = mapUniqueInfo.entrySet().iterator();
		Entry<Label,Pair<CmpVertex,CmpVertex>> entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("b", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("c", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.findVertex("B"),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("d", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
	}

	/** Multiple labels to choose from. */
	@Test
	public void TestUniqueIntoState4b()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-c->A / A-a->B-c->B-b->A / B-d->A / B-a-#C", "TestUniqueIntoInitial4b", mainConfiguration,converter);
		Map<Label,Pair<CmpVertex,CmpVertex>> mapUniqueInfo=LearningSupportRoutines.uniqueIntoState(graph);
		for(Entry<Label,Pair<CmpVertex,CmpVertex>> entry:mapUniqueInfo.entrySet()) Assert.assertSame(entry.getValue().secondElem, graph.transitionMatrix.get(entry.getValue().firstElem).get(entry.getKey()));
		Assert.assertEquals(2, mapUniqueInfo.size());
		Iterator<Entry<Label,Pair<CmpVertex,CmpVertex>>> entryIter = mapUniqueInfo.entrySet().iterator();
		Entry<Label,Pair<CmpVertex,CmpVertex>> entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("b", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("d", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
	}

	/** d is incoming into A from both A and B. */
	@Test
	public void TestUniqueIntoState4c()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-c->A / A-a->B-c->B-b->A / A-d->A / B-d->A / B-a-#C", "TestUniqueIntoInitial4b", mainConfiguration,converter);
		Map<Label,Pair<CmpVertex,CmpVertex>> mapUniqueInfo=LearningSupportRoutines.uniqueIntoState(graph);
		for(Entry<Label,Pair<CmpVertex,CmpVertex>> entry:mapUniqueInfo.entrySet()) if (entry.getValue().firstElem != null) Assert.assertSame(entry.getValue().secondElem, graph.transitionMatrix.get(entry.getValue().firstElem).get(entry.getKey()));
		Assert.assertEquals(2, mapUniqueInfo.size());
		Iterator<Entry<Label,Pair<CmpVertex,CmpVertex>>> entryIter = mapUniqueInfo.entrySet().iterator();
		Entry<Label,Pair<CmpVertex,CmpVertex>> entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("b", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("d", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue().secondElem);Assert.assertNull(entry.getValue().firstElem);
	}

	/** Multiple labels to choose from. */
	@Test
	public void TestUniqueIntoState5()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-d->A / B-e-#C", "TestUniqueIntoInitial4", mainConfiguration,converter);
		Map<Label,Pair<CmpVertex,CmpVertex>> mapUniqueInto=LearningSupportRoutines.uniqueIntoState(graph);
		for(Entry<Label,Pair<CmpVertex,CmpVertex>> entry:mapUniqueInto.entrySet()) Assert.assertSame(entry.getValue().secondElem, graph.transitionMatrix.get(entry.getValue().firstElem).get(entry.getKey()));
		
		Assert.assertEquals(5, mapUniqueInto.size());
		Iterator<Entry<Label,Pair<CmpVertex,CmpVertex>>> entryIter = mapUniqueInto.entrySet().iterator();
		Entry<Label,Pair<CmpVertex,CmpVertex>> entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("a", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.findVertex("B"),entry.getValue().secondElem);Assert.assertEquals(graph.getInit(),entry.getValue().firstElem);
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("b", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("c", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.findVertex("B"),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("d", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.getInit(),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
		entry = entryIter.next();
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("e", mainConfiguration,converter),entry.getKey());Assert.assertEquals(graph.findVertex("C"),entry.getValue().secondElem);Assert.assertEquals(graph.findVertex("B"),entry.getValue().firstElem);
	}
		
	@Test
	public void TestMergeBasedOnUniques()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->A1-a->B1-b->A2-b->A3-c-#C / A3 -a->B3-a->D / B3-b->A", "TestMergeBasedOnUniques", mainConfiguration,converter);
		LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(graph,Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("b", mainConfiguration, converter)}),Collections.<Label>emptyList());
		Set<StatePair> pairsSet = new HashSet<StatePair>();pairsSet.addAll(pairsList);
		Assert.assertTrue(pairsSet.contains(new StatePair(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A3"), mainConfiguration),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"), mainConfiguration))));
		Assert.assertTrue(pairsSet.contains(new StatePair(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A1"), mainConfiguration),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A2"), mainConfiguration))));
		Assert.assertTrue(pairsSet.contains(new StatePair(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A2"), mainConfiguration),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A3"), mainConfiguration))));
		Assert.assertTrue(graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, false) >= 0);
	}
	
	@Test
	public void TestMergeBasedOnUniquesFail()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->A1-a->B1-b->A2-b->A3-c-#C / A3 -a-#D", "TestMergeBasedOnUniquesFail", mainConfiguration,converter);
		LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(graph,Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("b", mainConfiguration, converter)}),Collections.<Label>emptyList());
		Assert.assertTrue(graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, false) < 0);
	}
	
	@Test
	public void TestConstructIfThenForUniques1()
	{
		LearnerEvaluationConfiguration evaluationConfiguration = new LearnerEvaluationConfiguration(mainConfiguration);
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-a-#C", "testSplitFSM", mainConfiguration,converter);
		Map<Label,Pair<CmpVertex,CmpVertex>> map=LearningSupportRoutines.uniqueIntoState(graph);
		Assert.assertNull(evaluationConfiguration.ifthenSequences);
		LearningSupportRoutines.addIfThenForMandatoryMerge(evaluationConfiguration, map.keySet());
		Assert.assertEquals(2,evaluationConfiguration.ifthenSequences.size());
		Iterator<String> ifthenIterator = evaluationConfiguration.ifthenSequences.iterator();
		Assert.assertEquals(QSMTool.cmdIFTHENAUTOMATON +" "+"Mandatory_1_via_b A- !b || toMerge_1_b ->A-b->B - b ->B / B- !b || toMerge_1_b ->A / B == THEN == C / C-toMerge_1_b->D",ifthenIterator.next());
		Assert.assertEquals(QSMTool.cmdIFTHENAUTOMATON +" "+"Mandatory_2_via_c A- !c || toMerge_2_c ->A-c->B - c ->B / B- !c || toMerge_2_c ->A / B == THEN == C / C-toMerge_2_c->D",ifthenIterator.next());
	}

	@Test
	public void TestConstructIfThenForUniques2()
	{
		LearnerEvaluationConfiguration evaluationConfiguration = new LearnerEvaluationConfiguration(mainConfiguration);
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-a-#C", "testSplitFSM", mainConfiguration,converter);
		Map<Label,Pair<CmpVertex,CmpVertex>> map=LearningSupportRoutines.uniqueIntoState(graph);
		evaluationConfiguration.ifthenSequences = new LinkedList<String>();evaluationConfiguration.ifthenSequences.add("junk");
		LearningSupportRoutines.addIfThenForMandatoryMerge(evaluationConfiguration, map.keySet());
		Assert.assertEquals(3,evaluationConfiguration.ifthenSequences.size());
		Iterator<String> ifthenIterator = evaluationConfiguration.ifthenSequences.iterator();
		Assert.assertEquals("junk",ifthenIterator.next());
		Assert.assertEquals(QSMTool.cmdIFTHENAUTOMATON +" "+"Mandatory_1_via_b A- !b || toMerge_1_b ->A-b->B - b ->B / B- !b || toMerge_1_b ->A / B == THEN == C / C-toMerge_1_b->D",ifthenIterator.next());
		Assert.assertEquals(QSMTool.cmdIFTHENAUTOMATON +" "+"Mandatory_2_via_c A- !c || toMerge_2_c ->A-c->B - c ->B / B- !c || toMerge_2_c ->A / B == THEN == C / C-toMerge_2_c->D",ifthenIterator.next());
	}
	
	@Test
	public void TestDetectionOfMandatoryTransitions()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-"+LearningSupportRoutines.prefixOfMandatoryMergeTransition+"_1->B-c->B-b->A / B-"+LearningSupportRoutines.prefixOfMandatoryMergeTransition+"_1->C-"+LearningSupportRoutines.prefixOfMandatoryMergeTransition+"_2->D", "TestDetectionOfMandatoryTransitions", mainConfiguration,converter);
		Assert.assertTrue(LearningSupportRoutines.checkForMerge(new PairScore(graph.findVertex("B"),graph.findVertex("A"),0,0),graph));
		Assert.assertTrue(LearningSupportRoutines.checkForMerge(new PairScore(graph.findVertex("A"),graph.findVertex("A"),0,0),graph));
		Assert.assertTrue(LearningSupportRoutines.checkForMerge(new PairScore(graph.findVertex("A"),graph.findVertex("B"),0,0),graph));
		Assert.assertFalse(LearningSupportRoutines.checkForMerge(new PairScore(graph.findVertex("C"),graph.findVertex("A"),0,0),graph));
		Assert.assertFalse(LearningSupportRoutines.checkForMerge(new PairScore(graph.findVertex("A"),graph.findVertex("C"),0,0),graph));
		Assert.assertFalse(LearningSupportRoutines.checkForMerge(new PairScore(graph.findVertex("D"),graph.findVertex("A"),0,0),graph));
		Assert.assertFalse(LearningSupportRoutines.checkForMerge(new PairScore(graph.findVertex("A"),graph.findVertex("D"),0,0),graph));
	}
	
	@Test
	public void TestConstructIfThenForUniques3()
	{
		LearnerEvaluationConfiguration evaluationConfiguration = new LearnerEvaluationConfiguration(mainConfiguration);
		LearnerGraph graph = new LearnerGraph(mainConfiguration);
		Map<Label,Pair<CmpVertex,CmpVertex>> map=LearningSupportRoutines.uniqueIntoState(graph);
		evaluationConfiguration.ifthenSequences = new LinkedList<String>();evaluationConfiguration.ifthenSequences.add("junk");
		LearningSupportRoutines.addIfThenForMandatoryMerge(evaluationConfiguration, map.keySet());
		Assert.assertEquals(1,evaluationConfiguration.ifthenSequences.size());
		Iterator<String> ifthenIterator = evaluationConfiguration.ifthenSequences.iterator();
		Assert.assertEquals("junk",ifthenIterator.next());
	}
	
	@Test
	public void TestConstructPairsOfInfeasibleTransitions0()
	{
		LearnerGraph graph = new LearnerGraph(mainConfiguration);
		Map<Label,Set<Label>> mapOfLabelToSet = LearningSupportRoutines.computeInfeasiblePairs(graph);
		Assert.assertTrue(mapOfLabelToSet.isEmpty());
	}
	
	@Test
	public void TestConstructIfThenForInfeasible0()
	{
		LearnerGraph graph = new LearnerGraph(mainConfiguration);
		LearnerEvaluationConfiguration evaluationConfiguration = new LearnerEvaluationConfiguration(mainConfiguration);
		LearningSupportRoutines.addIfThenForPairwiseConstraints(evaluationConfiguration, LearningSupportRoutines.computeInfeasiblePairs(graph));
		Assert.assertTrue(evaluationConfiguration.ifthenSequences.isEmpty());
	}

	@Test
	public void TestConstructPairsOfInfeasibleTransitions1()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-a-#C", "testSplitFSM", mainConfiguration,converter);
		// the list of lists below is in the order of labels, a,b,c
		List<List<Label>> expected = statechum.analysis.learning.rpnicore.TestFSMAlgo.buildList(new String[][]{new String[]{},new String[]{"b","c"},new String[]{}}, mainConfiguration,converter);
		Map<Label,Set<Label>> mapOfLabelToSet = LearningSupportRoutines.computeInfeasiblePairs(graph);
		Assert.assertEquals(graph.pathroutines.computeAlphabet(),mapOfLabelToSet.keySet());
		Iterator<List<Label>> listsIterator = expected.iterator();
		for(String lblString:new String[]{"a","b","c"})
		{
			Set<Label> infeasible = new TreeSet<Label>();infeasible.addAll(listsIterator.next());
			Assert.assertEquals(infeasible,mapOfLabelToSet.get(AbstractLearnerGraph.generateNewLabel(lblString, mainConfiguration, converter)));
		}
	}
	
	@Test
	public void TestConstructIfThenForInfeasible1()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-a-#C", "testSplitFSM", mainConfiguration,converter);
		LearnerEvaluationConfiguration evaluationConfiguration = new LearnerEvaluationConfiguration(mainConfiguration);
		evaluationConfiguration.ifthenSequences = new LinkedList<String>();evaluationConfiguration.ifthenSequences.add("junk");
		LearningSupportRoutines.addIfThenForPairwiseConstraints(evaluationConfiguration, LearningSupportRoutines.computeInfeasiblePairs(graph));
		Assert.assertEquals(2,evaluationConfiguration.ifthenSequences.size());
		Iterator<String> ifthenIterator = evaluationConfiguration.ifthenSequences.iterator();
		Assert.assertEquals("junk",ifthenIterator.next());
		Assert.assertEquals(QSMTool.cmdIFTHENAUTOMATON +" "+LearningSupportRoutines.pairwiseAutomata+"_b A- !b ->A-b->B - b ->B / B- !b ->A / C -b-#D/ C -c-#D/ B == THEN == C",ifthenIterator.next());
	}
	
	@Test
	public void TestConstructPairsOfInfeasibleTransitions2()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-c->A-a->B-c->B-b->A / B-a-#C", "TestConstructPairsOfInfeasibleTransitions2", mainConfiguration,converter);
		// the list of lists below is in the order of labels, a,b,c
		List<List<Label>> expected = statechum.analysis.learning.rpnicore.TestFSMAlgo.buildList(new String[][]{new String[]{},new String[]{"b"},new String[]{}}, mainConfiguration,converter);
		Map<Label,Set<Label>> mapOfLabelToSet = LearningSupportRoutines.computeInfeasiblePairs(graph);
		Assert.assertEquals(graph.pathroutines.computeAlphabet(),mapOfLabelToSet.keySet());
		Iterator<List<Label>> listsIterator = expected.iterator();
		for(String lblString:new String[]{"a","b","c"})
		{
			Set<Label> infeasible = new TreeSet<Label>();infeasible.addAll(listsIterator.next());
			Assert.assertEquals(infeasible,mapOfLabelToSet.get(AbstractLearnerGraph.generateNewLabel(lblString, mainConfiguration, converter)));
		}
	}
	
	@Test
	public void TestConstructPairsOfInfeasibleTransitions3()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-b->A-a->B-a->C-a->A / D-e->D", "TestConstructPairsOfInfeasibleTransitions3", mainConfiguration,converter);
		// the list of lists below is in the order of labels, a,b,c
		List<List<Label>> expected = statechum.analysis.learning.rpnicore.TestFSMAlgo.buildList(new String[][]{new String[]{"e"},new String[]{"e"},new String[]{"a","b"}}, mainConfiguration,converter);
		Map<Label,Set<Label>> mapOfLabelToSet = LearningSupportRoutines.computeInfeasiblePairs(graph);
		Assert.assertEquals(graph.pathroutines.computeAlphabet(),mapOfLabelToSet.keySet());
		Iterator<List<Label>> listsIterator = expected.iterator();
		for(String lblString:new String[]{"a","b","e"})
		{
			Set<Label> infeasible = new TreeSet<Label>();infeasible.addAll(listsIterator.next());
			Assert.assertEquals(infeasible,mapOfLabelToSet.get(AbstractLearnerGraph.generateNewLabel(lblString, mainConfiguration, converter)));
		}
	}
	
	@Test
	public void TestConstructIfThenForInfeasible3()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-b->A-a->B-a->C-a->A / D-e->D", "TestConstructPairsOfInfeasibleTransitions3", mainConfiguration,converter);
		LearnerEvaluationConfiguration evaluationConfiguration = new LearnerEvaluationConfiguration(mainConfiguration);
		LearningSupportRoutines.addIfThenForPairwiseConstraints(evaluationConfiguration, LearningSupportRoutines.computeInfeasiblePairs(graph));
		Assert.assertEquals(3,evaluationConfiguration.ifthenSequences.size());
		Iterator<String> ifthenIterator = evaluationConfiguration.ifthenSequences.iterator();
		Assert.assertEquals(QSMTool.cmdIFTHENAUTOMATON +" "+LearningSupportRoutines.pairwiseAutomata+"_a A- !a ->A-a->B - a ->B / B- !a ->A / C -e-#D/ B == THEN == C",ifthenIterator.next());
		Assert.assertEquals(QSMTool.cmdIFTHENAUTOMATON +" "+LearningSupportRoutines.pairwiseAutomata+"_b A- !b ->A-b->B - b ->B / B- !b ->A / C -e-#D/ B == THEN == C",ifthenIterator.next());
		Assert.assertEquals(QSMTool.cmdIFTHENAUTOMATON +" "+LearningSupportRoutines.pairwiseAutomata+"_e A- !e ->A-e->B - e ->B / B- !e ->A / C -a-#D/ C -b-#D/ B == THEN == C",ifthenIterator.next());
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
		TestDecisionProcedureForRedStatesHelper(graph,redToBeExpected,redsAlways, new LinkedList<CmpVertex>(),true);
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
		TestDecisionProcedureForRedStatesHelper(graph,redToBeExpected,redsAlways, Arrays.asList(new CmpVertex[]{graph.findVertex("F")}),true);
	}
	
	/** This one uses chooseStatePairs with a stub of decision maker to compute different sets of pairs depending on the choices made by the decision procedure, 
	 * and compares them to choices made by the evaluator of the quality of the selection of red states. 
	 */
	@Test
	public void TestDecisionProcedureForRedStates3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C-a->D-b->F / C-b->G / B-b-#E","TestDecisionProcedureForRedStates3",mainConfiguration, converter);
		
		final Collection<CmpVertex> redToBeExpected = new ArrayList<CmpVertex>();redToBeExpected.addAll(Arrays.asList(new CmpVertex[]{graph.findVertex("C"),graph.findVertex("E")}));
		final Collection<CmpVertex> redsAlways = new ArrayList<CmpVertex>();redsAlways.addAll(Arrays.asList(new CmpVertex[]{graph.findVertex("A"),graph.findVertex("B")}));
		TestDecisionProcedureForRedStatesHelper(graph,redToBeExpected,redsAlways, Arrays.asList(new CmpVertex[]{}),false);// the decision procedure is tasked with choosing between E and C which both generate the same set of pairs so any of the two can be chosen. This is why we disable a check for the specific vertex chosen.
	}
	
	protected void TestDecisionProcedureForRedStatesHelper(final LearnerGraph graph, final Collection<CmpVertex> redToBeExpected, final Collection<CmpVertex> redsAlways,
			final Collection<CmpVertex> blueStates, final boolean checkCorrectNodeChosen)
	{
		final Map<CmpVertex,Collection<PairScore>> redToPairsObtained = new TreeMap<CmpVertex,Collection<PairScore>>();
		final Set<CmpVertex> tentativeRedsChosen = new TreeSet<CmpVertex>();// vertices already chosen.
		
		for(final CmpVertex bestVertex:redToBeExpected)
		{
			graph.clearColours();
			for(CmpVertex vertRed:redsAlways) vertRed.setColour(JUConstants.RED);
			for(CmpVertex vertToColourBlue:redToBeExpected)	vertToColourBlue.setColour(JUConstants.BLUE);for(CmpVertex vertToColourBlue:blueStates)	vertToColourBlue.setColour(JUConstants.BLUE);
			
			Collection<PairScore> pairsReturned = graph.pairscores.chooseStatePairs(new RedNodeSelectionProcedure() {
				
				CmpVertex redChosen = null;
				
				@Override
				public CmpVertex selectRedNode(LearnerGraph coregraph, Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
				{
					Assert.assertNull(redChosen);// we choose graphs in such a way that red states chosen subsequently are decided without calls to selectRedNode before chooseStatePairs returns.
					Assert.assertSame(graph,coregraph);
					Assert.assertTrue(reds.contains(graph.findVertex("A")));Assert.assertTrue(reds.contains(graph.findVertex("B")));
					Assert.assertTrue(redToBeExpected.equals(tentativeRedNodes));
					
					Set<CmpVertex> available = new TreeSet<CmpVertex>();available.addAll(tentativeRedNodes);available.removeAll(tentativeRedsChosen);
					redChosen = bestVertex;
					return redChosen;
				}
				
				@SuppressWarnings("unused")
				@Override
				public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, List<PairScore> pairs) 
				{
					Assert.assertNotNull(redChosen);// ensure that selectRedNode was called.
					Set<PairScore> copyOfPairs = new TreeSet<PairScore>(pairs);
					redToPairsObtained.put(redChosen,copyOfPairs);// record the pairs we got, these should be the same pairs as those to be returned from chooseStatePairs so long as resolvePotentialDeadEnd returns null.
					return null;// no resolution
				}
				
				@Override
				public void initComputation(@SuppressWarnings("unused") LearnerGraph gr) {
					// dummy
				}

				@Override
				public long overrideScoreComputation(PairScore p) {
					return p.getScore();// dummy
				}

				@Override
				public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(@SuppressWarnings("unused") CmpVertex currentRed) 
				{
					return null;// dummy, ignored if null.
				}
			});
			
			Set<PairScore> pairsReturnedAsSet = new TreeSet<PairScore>(pairsReturned);
			Assert.assertEquals(redToPairsObtained.get(bestVertex),pairsReturnedAsSet);
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
					final Set<Collection<PairScore>> collectionOfPairsSeen = new HashSet<Collection<PairScore>>();
					CmpVertex nodeSelected = LearnerThatUsesWekaResults.selectRedNodeUsingQualityEstimator(coregraph, tentativeRedNodes, new CollectionOfPairsEstimator() {
	
						@Override
						public double obtainEstimateOfTheQualityOfTheCollectionOfPairs(LearnerGraph argGraph, Collection<PairScore> pairs) 
						{
							Assert.assertSame(graph,argGraph);Assert.assertEquals(redsAlways,reds);
							Set<PairScore> AdditionalPairs = new TreeSet<PairScore>(pairs);
							collectionOfPairsSeen.add(AdditionalPairs);// we'll check that pairs passed to us were the same as those computed during the forward-run of the chooseStatePairs.
								// It is important to do this check because pairs passed to obtainEstimateOfTheQualityOfTheCollectionOfPairs are computed by selectRedNode.
							
							if (AdditionalPairs.equals(redToPairsObtained.get(bestVertexFinal))) // pairs passed to this one should be some of the same collections of pairs seen by resolvePotentialDeadEnd above. 
								return 1;
							return 0;
						}
						
					});
					final Set<Collection<PairScore>> collectionOfPairsExpected = new HashSet<Collection<PairScore>>();collectionOfPairsExpected.addAll(redToPairsObtained.values());
					Assert.assertEquals(collectionOfPairsExpected,collectionOfPairsSeen);
					if (checkCorrectNodeChosen) Assert.assertEquals(bestVertexFinal,nodeSelected);// check that the expected vertex has been selected
					return bestVertexFinal;
				}
	
				@SuppressWarnings("unused")
				@Override
				public CmpVertex resolvePotentialDeadEnd(LearnerGraph coregraph, Collection<CmpVertex> reds, List<PairScore> pairs) {
					return null;
				}
				
				@Override
				public void initComputation(@SuppressWarnings("unused") LearnerGraph gr) {
					// dummy
				}

				@Override
				public long overrideScoreComputation(PairScore p) {
					return p.getScore();// dummy
				}

				@Override
				public Collection<Entry<Label, CmpVertex>> getSurroundingTransitions(@SuppressWarnings("unused") CmpVertex currentRed) 
				{
					return null;// dummy, ignored if null.
				}
			});
			
			Set<PairScore> pairsReturnedAsSet = new TreeSet<PairScore>(pairsReturned);
			Assert.assertEquals(redToPairsObtained.get(bestVertex),pairsReturnedAsSet);
		}
	}
	
	@Test
	public void testAutomatonConnected1()
	{
		LearnerGraph graph = new LearnerGraph(mainConfiguration);graph.initEmpty();
		Assert.assertNull(LearningSupportRoutines.uniqueFromInitial(graph));
		Assert.assertTrue(graph.pathroutines.automatonConnected());
		LearnerGraph actual = new LearnerGraph(mainConfiguration);graph.pathroutines.removeReachableStatesFromWhichInitIsNotReachable(actual);
		Assert.assertTrue(actual.transitionMatrix.isEmpty());Assert.assertNull(actual.getInit());
	}
	
	@Test
	public void testAutomatonConnected2()
	{
		LearnerGraph graph = new LearnerGraph(mainConfiguration);
		Assert.assertNull(LearningSupportRoutines.uniqueFromInitial(graph));
		Assert.assertTrue(graph.pathroutines.automatonConnected());
		LearnerGraph actual = new LearnerGraph(mainConfiguration);graph.pathroutines.removeReachableStatesFromWhichInitIsNotReachable(actual);
		LearnerGraph expected = new LearnerGraph(mainConfiguration);
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null) throw ex;
	}
	
	@Test
	public void testAutomatonConnected3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C-b->A","testAutomatonConnected3",mainConfiguration, converter);
		Assert.assertNull(LearningSupportRoutines.uniqueFromInitial(graph));
		Assert.assertTrue(graph.pathroutines.automatonConnected());
		LearnerGraph actual = new LearnerGraph(mainConfiguration);graph.pathroutines.removeReachableStatesFromWhichInitIsNotReachable(actual);
		DifferentFSMException ex = WMethod.checkM(graph, actual);
		if (ex != null) throw ex;
	}
	
	@Test
	public void testUniqueFromInitial1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->C-b->A","testAutomatonConnected3",mainConfiguration, converter);
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("a", mainConfiguration, converter), LearningSupportRoutines.uniqueFromInitial(graph));
	}
	@Test
	public void testUniqueFromInitial2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-c->C-b->A-b->B / C-a->A-t->A","testAutomatonConnected3",mainConfiguration, converter);
		Assert.assertEquals(AbstractLearnerGraph.generateNewLabel("t", mainConfiguration, converter), LearningSupportRoutines.uniqueFromInitial(graph));
	}
	
	@Test
	public void testAutomatonConnected4()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C","testAutomatonConnected4",mainConfiguration, converter);
		Assert.assertFalse(graph.pathroutines.automatonConnected());
		LearnerGraph actual = new LearnerGraph(mainConfiguration);graph.pathroutines.removeReachableStatesFromWhichInitIsNotReachable(actual);
		LearnerGraph expected = new LearnerGraph(mainConfiguration);
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null) throw ex;
	}
	
	@Test
	public void testAutomatonConnected5()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C-b->A / D-a->D","testAutomatonConnected5",mainConfiguration, converter);
		Assert.assertFalse(graph.pathroutines.automatonConnected());
		LearnerGraph actual = new LearnerGraph(mainConfiguration);graph.pathroutines.removeReachableStatesFromWhichInitIsNotReachable(actual);
		final LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-a->C-b->A","testAutomatonConnected3a",mainConfiguration, converter);
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null) throw ex;
	}
		
	@Test
	public void testConstructPairsToMergeWithOutgoing1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("M1-c->C1 / M1-a->A1-b->M2-c->C2-a->A3-b->M4-c->C4-a->A4 / M2-a->A2-b->M3-c->C3-d-#D / M2-b->B","testCconstructPairsToMergeWithOutgoing1",mainConfiguration, converter);
		Set<CmpVertex> actual = new TreeSet<CmpVertex>(), expectedTargets = new TreeSet<CmpVertex>(), actualTargets = new TreeSet<CmpVertex>();
		Label unique = AbstractLearnerGraph.generateNewLabel("c", mainConfiguration,converter);
		actual.addAll(LearningSupportRoutines.constructPairsToMergeWithOutgoing(graph, unique));
		Assert.assertEquals(4,actual.size());
		expectedTargets.addAll(Arrays.asList(new CmpVertex[]{graph.findVertex("C1"),graph.findVertex("C2"),graph.findVertex("C3"),graph.findVertex("C4")}));
		for(CmpVertex v:actual)
		{
			Map<Label,CmpVertex> next = graph.transitionMatrix.get(v);
			if (!v.equals(graph.getInit())) Assert.assertEquals(1,next.size());
			Assert.assertTrue(next.keySet().contains(unique));
			actualTargets.add(next.get(unique));
		}
		Assert.assertEquals(expectedTargets,actualTargets);
	}
	
	@Test
	public void testConstructPairsToMergeWithOutgoing2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("M1-c-#C1 / M1-a->A1-b->M2-c->C2-a->A3-b->M4-c->C4-a->A4 / M2-a->A2-b->M3-c->C3-d-#D / M2-b->B","testCconstructPairsToMergeWithOutgoing2",mainConfiguration, converter);
		Set<CmpVertex> actual = new TreeSet<CmpVertex>(), expectedTargets = new TreeSet<CmpVertex>(), actualTargets = new TreeSet<CmpVertex>();
		Label unique = AbstractLearnerGraph.generateNewLabel("c", mainConfiguration,converter);
		actual.addAll(LearningSupportRoutines.constructPairsToMergeWithOutgoing(graph, unique));
		Assert.assertEquals(3,actual.size());
		expectedTargets.addAll(Arrays.asList(new CmpVertex[]{graph.findVertex("C2"),graph.findVertex("C3"),graph.findVertex("C4")}));
		for(CmpVertex v:actual)
		{
			Map<Label,CmpVertex> next = graph.transitionMatrix.get(v);Assert.assertEquals(1,next.size());
			if (!v.equals(graph.getInit())) Assert.assertEquals(1,next.size());
			Assert.assertTrue(next.keySet().contains(unique));
			actualTargets.add(next.get(unique));
		}
		Assert.assertEquals(expectedTargets,actualTargets);
	}
	
	// Here transition from M3 goes to a reject state, hence M3 is not considered a candidate for a valid merge with other M-transitions. 
	@Test
	public void testConstructPairsToMergeWithOutgoing3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("M1-c->C1 / M1-a->A1-b->M2-c->C2-a->A3-b->M4-c->C4-a->A4 / M2-a->A2-b->M3-c-#D / M2-b->B","testConstructPairsToMergeWithOutgoing3",mainConfiguration, converter);
		Set<CmpVertex> actual = new TreeSet<CmpVertex>(), expectedTargets = new TreeSet<CmpVertex>(), actualTargets = new TreeSet<CmpVertex>();
		Label unique = AbstractLearnerGraph.generateNewLabel("c", mainConfiguration,converter);
		actual.addAll(LearningSupportRoutines.constructPairsToMergeWithOutgoing(graph, unique));
		Assert.assertEquals(3,actual.size());
		expectedTargets.addAll(Arrays.asList(new CmpVertex[]{graph.findVertex("C1"),graph.findVertex("C2"),graph.findVertex("C4")}));
		for(CmpVertex v:actual)
		{
			Map<Label,CmpVertex> next = graph.transitionMatrix.get(v);
			if (!v.equals(graph.getInit())) Assert.assertEquals(1,next.size());
			Assert.assertTrue(next.keySet().contains(unique));
			actualTargets.add(next.get(unique));
		}
		Assert.assertEquals(expectedTargets,actualTargets);
	}
	
	@Test
	public void testConstructPairsToMergeWithOutgoing4()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("M1-c-#C1 / M1-a->A1-b->M2-c->C2-a->A3-b->M4-c->C4-a->A4 / M2-a->A2-b->M3-c->C3-d-#D / M2-b->B","testCconstructPairsToMergeWithOutgoing2",mainConfiguration, converter);
		Set<CmpVertex> actual = new TreeSet<CmpVertex>();
		actual.addAll(LearningSupportRoutines.constructPairsToMergeWithOutgoing(graph, AbstractLearnerGraph.generateNewLabel("d", mainConfiguration,converter)));
		Assert.assertTrue(actual.isEmpty());
	}
	
	@Test
	public void testConstructPairsToMergeWithOutgoing5()
	{
		final LearnerGraph graph = new LearnerGraph(mainConfiguration);
		Set<CmpVertex> actual = new TreeSet<CmpVertex>();
		actual.addAll(LearningSupportRoutines.constructPairsToMergeWithOutgoing(graph, AbstractLearnerGraph.generateNewLabel("d", mainConfiguration,converter)));
		Assert.assertTrue(actual.isEmpty());
	}
	
	@Test
	public void testMergeStatesForUniqueFromInitial1()
	{
		String graphSrc = "M1-c->C1 / M1-a->A1-b->M2-c->C2-a->A3-b->M4-c->C4-a->A4 / M2-a->A2-b->M3-c->C3-d-#D / M2-b->B";
		final LearnerGraph graph = FsmParser.buildLearnerGraph(graphSrc,"testCconstructPairsToMergeWithOutgoing1",mainConfiguration, converter),
				graphCopy = FsmParser.buildLearnerGraph(graphSrc,"testCconstructPairsToMergeWithOutgoing1",mainConfiguration, converter);// create a completely unrelated graph with same states and structure
		final LearnerGraph expected = FsmParser.buildLearnerGraph("M1-c->C1-d-#D / C1-a->A3-b->M4-c->P03 /  M1-a->A1-b->M2-c->C2 / M2-a->A2-b->M3-c->C3 / M2-b->B","testMergeStatesForUniqueFromInitial1b",mainConfiguration, converter);

		LearnerGraph actual = LearningSupportRoutines.mergeStatesForUnique(graph, AbstractLearnerGraph.generateNewLabel("c", mainConfiguration,converter));
		Visualiser.updateFrame(actual,graph);
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null)
			throw ex;
		ex = WMethod.checkM(graphCopy, graph);
		if (ex != null)
			throw ex;
	}
	
	@Test
	public void testMergeStatesForUniqueFromInitial2()
	{
		String graphSrc = "M1-c->C1 / M1-a->A1-b->M2-c->C2-a->A3-b->M4-c->C4-a->A4 / M2-a->A2-b->M3-c->C3-d-#D / M2-b->B";
		final LearnerGraph graph = FsmParser.buildLearnerGraph(graphSrc,"testCconstructPairsToMergeWithOutgoing1",mainConfiguration, converter),
				graphCopy = FsmParser.buildLearnerGraph(graphSrc,"testCconstructPairsToMergeWithOutgoing1",mainConfiguration, converter);// create a completely unrelated graph with same states and structure
		final LearnerGraph expected = FsmParser.buildLearnerGraph("M1-c->C1 / M1-a->A1-b->M2-c->C2-a->A3-b->M4-c->C4-a->A4 / M2-a->A2-b->M3-c->C3-d-#D / M2-b->B","testCconstructPairsToMergeWithOutgoing1",mainConfiguration, converter);
		LearnerGraph actual = LearningSupportRoutines.mergeStatesForUnique(graph, AbstractLearnerGraph.generateNewLabel("d", mainConfiguration,converter));
		DifferentFSMException ex = WMethod.checkM(expected, actual);
		if (ex != null)
			throw ex;
		ex = WMethod.checkM(graphCopy, graph);
		if (ex != null)
			throw ex;
	}
	
	@Test
	public void testMergeStatesForUniqueFromInitial3()
	{
		final LearnerGraph graph = new LearnerGraph(mainConfiguration);
		LearnerGraph actual = LearningSupportRoutines.mergeStatesForUnique(graph, AbstractLearnerGraph.generateNewLabel("d", mainConfiguration,converter));
		Assert.assertEquals(1,actual.getStateNumber());
		Assert.assertTrue(actual.transitionMatrix.get(actual.getInit()).isEmpty());
	}
}


