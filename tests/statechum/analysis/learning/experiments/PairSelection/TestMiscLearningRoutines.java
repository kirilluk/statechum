package statechum.analysis.learning.experiments.PairSelection;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.CollectionOfPairsEstimator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.PairScoreComputation.RedNodeSelectionProcedure;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.apps.QSMTool;

public class TestMiscLearningRoutines 
{
	final Configuration mainConfiguration = Configuration.getDefaultConfiguration().copy();
	final ConvertALabel converter = null;
	
	public TestMiscLearningRoutines() 
	{
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
	public void TestMergeBasedOnUniques1()
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
	public void TestMergeBasedOnUniques2()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a-#C", "TestMergeBasedOnUniques2", mainConfiguration,converter);
		List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(graph,Collections.<Label>emptyList(),Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", mainConfiguration, converter)}));
		Assert.assertTrue(pairsList.isEmpty());
		pairsList =  LearningSupportRoutines.buildVerticesToMergeForPathsFrom(graph, AbstractLearnerGraph.generateNewLabel("a", mainConfiguration, converter));
		Assert.assertTrue(pairsList.isEmpty());
	}

	@Test
	public void TestMergeBasedOnUniques3()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a-#C / D-a->D-b->F", "TestMergeBasedOnUniques3", mainConfiguration,converter);
		List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(graph,Collections.<Label>emptyList(),Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", mainConfiguration, converter)}));
		Set<StatePair> pairsSet = new HashSet<StatePair>();pairsSet.addAll(pairsList);
		Assert.assertEquals(1, pairsList.size());
		Assert.assertTrue(pairsSet.contains(new StatePair(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"), mainConfiguration),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("D"), mainConfiguration))));

		pairsList =  LearningSupportRoutines.buildVerticesToMergeForPathsFrom(graph, AbstractLearnerGraph.generateNewLabel("a", mainConfiguration, converter));
		Assert.assertEquals(1, pairsList.size());
		Assert.assertTrue(pairsSet.contains(new StatePair(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"), mainConfiguration),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("D"), mainConfiguration))));
	}

	@Test
	public void TestMergeBasedOnUniques4()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a-#C / D-a->D-b->F-a->F", "TestMergeBasedOnUniques4", mainConfiguration,converter);
		List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(graph,Collections.<Label>emptyList(),Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("a", mainConfiguration, converter)}));
		Set<StatePair> pairsSet = new HashSet<StatePair>();pairsSet.addAll(pairsList);
		Assert.assertEquals(2, pairsList.size());
		Assert.assertTrue(pairsSet.contains(new StatePair(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"), mainConfiguration),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("D"), mainConfiguration))));
		Assert.assertTrue(pairsSet.contains(new StatePair(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("D"), mainConfiguration),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("F"), mainConfiguration))));

		pairsList =  LearningSupportRoutines.buildVerticesToMergeForPathsFrom(graph, AbstractLearnerGraph.generateNewLabel("a", mainConfiguration, converter));
		Assert.assertEquals(2, pairsList.size());
		Assert.assertTrue(pairsSet.contains(new StatePair(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"), mainConfiguration),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("D"), mainConfiguration))));
		Assert.assertTrue(pairsSet.contains(new StatePair(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("D"), mainConfiguration),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("F"), mainConfiguration))));
	}

	@Test
	public void TestMergeBasedOnUniquesFail()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-b->A1-a->B1-b->A2-b->A3-c-#C / A3 -a-#D", "TestMergeBasedOnUniquesFail", mainConfiguration,converter);
		LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(graph,Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("b", mainConfiguration, converter)}),Collections.<Label>emptyList());
		Assert.assertTrue(graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, false) < 0);
	}
	
	// buildVerticesToMergeForPathsFrom
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
				protected LearnerGraphND inverseTentativeGraph = null;
				
				@Override
				public CmpVertex selectRedNode(LearnerGraph coregraph, final Collection<CmpVertex> reds, Collection<CmpVertex> tentativeRedNodes) 
				{
					final Set<Collection<PairScore>> collectionOfPairsSeen = new HashSet<Collection<PairScore>>();
					CmpVertex nodeSelected = LearnerThatUsesWekaResults.selectRedNodeUsingQualityEstimator(coregraph, inverseTentativeGraph, tentativeRedNodes, new CollectionOfPairsEstimator() {
	
						@Override
						public double obtainEstimateOfTheQualityOfTheCollectionOfPairs(LearnerGraph argGraph, @SuppressWarnings("unused") LearnerGraphND inverseGraph, Collection<PairScore> pairs) 
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
				public void initComputation(LearnerGraph gr) 
				{
					inverseTentativeGraph = MarkovClassifier.computeInverseGraph(gr);
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
	
	@Test
	public void testRemoveNegatives1()
	{
		LearnerGraph graph = new LearnerGraph(mainConfiguration);
		LearnerGraph actual = LearningSupportRoutines.removeAllNegatives(graph);
		DifferentFSMException ex = WMethod.checkM(graph,actual);
		if (ex != null)
			throw ex;
		
	}

	@Test
	public void testRemoveNegatives2()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->A","testRemoveNegatives2",mainConfiguration,converter);
		LearnerGraph actual = LearningSupportRoutines.removeAllNegatives(graph);
		DifferentFSMException ex = WMethod.checkM(graph,actual);
		if (ex != null)
			throw ex;
		
	}

	@Test
	public void testRemoveNegatives3()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->A","testRemoveNegatives3",mainConfiguration,converter);
		LearnerGraph actual = LearningSupportRoutines.removeAllNegatives(graph);
		DifferentFSMException ex = WMethod.checkM(graph,actual);
		if (ex != null)
			throw ex;
		
	}

	@Test
	public void testRemoveNegatives4()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a-#C / A-b->A","testRemoveNegatives4a",mainConfiguration,converter);
		LearnerGraph actual = LearningSupportRoutines.removeAllNegatives(graph);
		DifferentFSMException ex = WMethod.checkM(FsmParser.buildLearnerGraph("A-a->B / A-b->A","testRemoveNegatives4b",mainConfiguration,converter),actual);
		if (ex != null)
			throw ex;
		
	}

	@Test
	public void testRemoveNegatives5()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a-#C / A-b->A-c-#D","testRemoveNegatives5a",mainConfiguration,converter);
		LearnerGraph actual = LearningSupportRoutines.removeAllNegatives(graph);
		DifferentFSMException ex = WMethod.checkM(FsmParser.buildLearnerGraph("A-a->B / A-b->A","testRemoveNegatives5b",mainConfiguration,converter),actual);
		if (ex != null)
			throw ex;
		
	}

	// This one covers the case of a graph with a single state
	@Test
	public void testRemoveNegatives6()
	{
		LearnerGraph graph = new LearnerGraph(mainConfiguration);graph.getInit().setAccept(false);
		LearnerGraph actual = LearningSupportRoutines.removeAllNegatives(graph);
		Assert.assertEquals(0,actual.getStateNumber());				
	}

	// This one covers the case of a graph that will develop holes if negatives are removed.
	@Test
	public void testRemoveNegatives7()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C-a->D","testRemoveNegatives7a",mainConfiguration,converter);
		graph.findVertex("B").setAccept(false);
		LearnerGraph actual = LearningSupportRoutines.removeAllNegatives(graph);
		DifferentFSMException ex = WMethod.checkM(new LearnerGraph(mainConfiguration),actual);
		if (ex != null)
			throw ex;
		
	}

	// This one covers the case of a graph that will develop holes if negatives are removed.
	@Test
	public void testRemoveNegatives8()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C-a->D","testRemoveNegatives8a",mainConfiguration,converter);
		graph.findVertex("C").setAccept(false);
		LearnerGraph actual = LearningSupportRoutines.removeAllNegatives(graph);
		DifferentFSMException ex = WMethod.checkM(FsmParser.buildLearnerGraph("A-a->B","testRemoveNegatives8b",mainConfiguration,converter),actual);
		if (ex != null)
			throw ex;
		
	}

	@Test
	public void testIdentifyInitialState1()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("T-a->A-a->B-a->B-b->A-b->C","testIdentifyInitialState1",mainConfiguration,converter);
		LearnerGraph pta = FsmParser.buildLearnerGraph("A-a->B-a->C-a->D","testRemoveNegatives8a",mainConfiguration,converter);
		Assert.assertEquals("T", LearningSupportRoutines.findBestMatchForInitialVertexInGraph(graph, pta).getStringId());
	}

	@Test
	public void testIdentifyInitialState2()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("T-b->A-a->B-a->B-b->A-b->C","testIdentifyInitialState2",mainConfiguration,converter);
		LearnerGraph pta = FsmParser.buildLearnerGraph("A-a->B-a->C-a->D","testRemoveNegatives8a",mainConfiguration,converter);
		Assert.assertEquals("A", LearningSupportRoutines.findBestMatchForInitialVertexInGraph(graph, pta).getStringId());
	}

	@Test
	public void testIdentifyInitialState3()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("T-b->A-c->B-a->B-b->A-b->C","testIdentifyInitialState3",mainConfiguration,converter);
		LearnerGraph pta = FsmParser.buildLearnerGraph("A-a->B-a->C-a->D","testRemoveNegatives8a",mainConfiguration,converter);
		Assert.assertEquals("B", LearningSupportRoutines.findBestMatchForInitialVertexInGraph(graph, pta).getStringId());
	}

	/** Since there is no meaningful match between PTA and any of the states, the first state of the graph is returned. */ 
	@Test
	public void testIdentifyInitialState4()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("T-b->A-a->B-a->B-b->A-b->C","testIdentifyInitialState4",mainConfiguration,converter);
		LearnerGraph pta = FsmParser.buildLearnerGraph("A-c->B-a->C-a->D","testRemoveNegatives8a",mainConfiguration,converter);
		Assert.assertEquals("T", LearningSupportRoutines.findBestMatchForInitialVertexInGraph(graph, pta).getStringId());
	}

	@Test
	public void testIdentifyInitialState5()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("T-b->A","testIdentifyInitialState4",mainConfiguration,converter);
		graph.initEmpty();// with an empty graph, the only possible result is a null since there are no states to match.
		LearnerGraph pta = FsmParser.buildLearnerGraph("A-a->B-a->C-a->D","testRemoveNegatives8a",mainConfiguration,converter);
		Assert.assertNull(LearningSupportRoutines.findBestMatchForInitialVertexInGraph(graph, pta));
	}


	@Test
	public void testIdentifyInitialState6()
	{
		LearnerGraph graph = FsmParser.buildLearnerGraph("T-b->A","testIdentifyInitialState4",mainConfiguration,converter);
		graph.initPTA();graph.getInit().setAccept(false);// with a single-state reject graph pta will not match and the outcome will be null.
		LearnerGraph pta = FsmParser.buildLearnerGraph("A-a->B-a->C-a->D","testRemoveNegatives8a",mainConfiguration,converter);
		Assert.assertNull(LearningSupportRoutines.findBestMatchForInitialVertexInGraph(graph, pta));
	}
}
