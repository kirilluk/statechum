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
package statechum.analysis.learning.experiments;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

import junit.framework.Assert;

import org.junit.Test;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairQualityLearner.PairMeasurements;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;

/** This class tests that a simple Weka classifier can be instantiated and used to classify instances. It does not replace full Weka tests. */
public class TestWekaPairClassifier {
	final Configuration mainConfiguration = Configuration.getDefaultConfiguration().copy();
	final ConvertALabel converter = null;
	
	public TestWekaPairClassifier() {
		correctGraph = FsmParser.buildLearnerGraph("A-a->B-c->B-b->A / B-a-#C", "testSplitCorrect", mainConfiguration,converter);
		tentativeGraph = FsmParser.buildLearnerGraph("A1-a->B1-c->B2 / B1-b->A2 / B2-a-#C1 / B1-a-#C2", "testSplitWrong", mainConfiguration,converter);
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
		PairQualityLearner.WekaPairClassifier classifier = new PairQualityLearner.WekaPairClassifier("",10);
		Map<StatePair,PairMeasurements> map = classifier.buildSetsForComparators(Arrays.asList(new PairScore[]{}), tentativeGraph);
		Assert.assertTrue(map.isEmpty());
	}
	
	@Test
	public void testBuildSetsForComparators2()
	{
		PairQualityLearner.WekaPairClassifier classifier = new PairQualityLearner.WekaPairClassifier("",10);
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
		PairQualityLearner.WekaPairClassifier classifier = new PairQualityLearner.WekaPairClassifier("",10);
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
		PairQualityLearner.WekaPairClassifier classifier = new PairQualityLearner.WekaPairClassifier("",10);
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
		PairQualityLearner.WekaPairClassifier classifier = new PairQualityLearner.WekaPairClassifier("",10);
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
		PairQualityLearner.WekaPairClassifier classifier = new PairQualityLearner.WekaPairClassifier("",10);
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
		PairQualityLearner.WekaPairClassifier classifier = new PairQualityLearner.WekaPairClassifier("",10);
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
	
	
	@Test
	public void TestCreateInstances()
	{
		int attributeNumber = 4;
		FastVector vecA = new FastVector(3);vecA.addElement("-1");vecA.addElement("0");vecA.addElement("1");
		FastVector vecBool = new FastVector(2);vecBool.addElement(Boolean.TRUE.toString());vecBool.addElement(Boolean.FALSE.toString());
		Attribute attrA = new Attribute("a", vecA), attrB= new Attribute("b",vecA), attrC=new Attribute("c",vecA),attrClass=new Attribute("class",vecBool);
		
		FastVector attributes = new FastVector(attributeNumber);attributes.addElement(attrA);attributes.addElement(attrB);attributes.addElement(attrC);attributes.addElement(attrClass);
		//Instances trainingData = new Instances(attributes);
		Instance inst = new Instance(attributeNumber);
		inst.setValue(attrA,0);inst.setValue(attrB, 1);inst.setValue(attrC, 1);inst.setValue(attrClass, 0);
	}
}
