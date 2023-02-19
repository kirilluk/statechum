/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

package statechum.analysis.learning;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;

import static statechum.analysis.learning.rpnicore.FsmParserStatechum.buildLearnerGraph;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.ParameterizedWithName;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Label;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.FsmParserStatechum;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestWithMultipleConfigurations;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.model.testset.PTASequenceEngine.DebugDataValues;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import static statechum.TestHelper.checkForCorrectException;
import static statechum.TestHelper.whatToRun;
import static statechum.analysis.learning.rpnicore.TestEquivalenceChecking.checkM;

@RunWith(ParameterizedWithName.class)
public class TestPTAConstruction extends TestWithMultipleConfigurations
{
	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		return TestWithMultipleConfigurations.data();
	}
	
	@org.junit.runners.ParameterizedWithName.ParametersToString
	public static String parametersToString(Configuration config)
	{
		return TestWithMultipleConfigurations.parametersToString(config);
	}

	public TestPTAConstruction(Configuration argConfig)
	{
		super(argConfig);
	}

	@Test
	public void testAugmentPTA_Simple() // only two traces, both accept
	{
		Configuration config = mainConfiguration.copy();config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
		Set<List<Label>> plusStrings = buildSet(new String[][] { new String[] {"a","b","c"},new String[]{"a","d","c"} },config,converter);
		DirectedSparseGraph actualA = Test_Orig_RPNIBlueFringeLearner.augmentPTA(DeterministicDirectedSparseGraph.initialise(), plusStrings, true);
		DeterministicDirectedSparseGraph.numberVertices(actualA);// Numbering is necessary to ensure uniqueness of labels used by LearnerGraph constructor.
		config.setAllowedToCloneNonCmpVertex(true);
		LearnerGraph l = new LearnerGraph(config);
		LearnerGraph actualC = l.paths.augmentPTA(plusStrings, true,false);
		DeterministicDirectedSparseGraph.numberVertices(actualA);
		String expectedPTA = "A-a->B--b->C-c->Z1\nB--d->C2-c->Z2";
		checkM(expectedPTA,new LearnerGraph(actualA,config), config,converter);
		checkM(expectedPTA,actualC, config,converter);
	}
	
	public static PTASequenceEngine buildPTA(Set<List<Label>> plusStrings,Set<List<Label>> minusStrings)
	{
		final Boolean accept = Boolean.valueOf(true), reject = Boolean.valueOf(false);
		boolean theOnlyStateReject = false;
		for(List<Label> seq:minusStrings)
			if (seq.isEmpty())
			{
				theOnlyStateReject = true;break;
			}
		final Boolean rejectAllStates = Boolean.valueOf(theOnlyStateReject);
		final AtomicBoolean statesAccept = new AtomicBoolean(true);
		PTASequenceEngine allSequences = new PTASequenceEngine();
		
		// Here we are building a PTA which consists of accept states for plusStrings and reject-states
		// for minusStrings. This way if a plus string is a prefix of a plusString, the prefix will
		// be labelled with Boolean(true) states and the suffix - with Boolean(false) states.
		// Note that the suffix may contain many states.
		allSequences.init(new PTASequenceSetAutomaton()
		{
			@Override
			public Object getTheOnlyState() {
				return statesAccept.get()?accept:reject;
			}
			@Override
			public boolean shouldBeReturned(Object elem) {
				return elem != null && ((Boolean)elem).booleanValue();
			}
			@Override
			public boolean isAccept(@SuppressWarnings("unused")	Object elem)
			{
				return !rejectAllStates.booleanValue();
			}
		});
		SequenceSet initSeq = allSequences.new SequenceSet();initSeq.setIdentity();
		initSeq.cross(plusStrings);statesAccept.getAndSet(false);initSeq.cross(minusStrings);
		
		return allSequences;
	}

	/** Checks if a PTA constructed is consistent with provided sequences. */
	private void checkPTAConsistency(PTASequenceEngine engine,Set<List<Label>> sequences, boolean accept)
	{
		SequenceSet initSeq = engine.new SequenceSet();initSeq.setIdentity();
		int leafNumber = engine.getDebugDataMapDepth(null).size();
		// Now we check the consistency
		for(List<Label> seq:sequences)
		{
			SequenceSet endOfSeq = initSeq.crossWithSequence(seq);// this is aimed to follow a path in a PTA
				// to a state which is not necessarily a tail-state (hence no use calling getDebugDataMapDepth(null),
				// but can inadvertently add a new sequence, hence we have to check that it has to happened at the end.
			Map<String,String> map = engine.getDebugDataMapDepth(endOfSeq);
			assert map.size() == 1: "expected size of 1, got "+map.size();
			String attrs = map.values().iterator().next();
			// For reject-sequences,
			// If the end of the sequence is not a leaf, it should be considered an accept-sequence, hence throw.
			// If the end of the sequence is a leaf but should be returned, this is also an accept-sequence (see the sequence engine construction above) throw.
			// The only remaining case is that the end is a leaf and should not be returned - this is ok.
			if (!accept && !attrs.equals(DebugDataValues.booleanToString(true,false)))
				throw new IllegalArgumentException("reject-sequence "+seq+" is present in PTA with a positive ending");
			// For accept-sequences, the only erroneous case is when a leaf is not returned.
			// (we assume that non-leaf is always returned.) 
			if (accept && attrs.equals(DebugDataValues.booleanToString(true,false)))
				throw new IllegalArgumentException("reject-sequence "+seq+" is present in PTA with a negative ending");
		}
		Assert.assertEquals(leafNumber, engine.getDebugDataMapDepth(null).size());
	}
	
	private void checkEmptyPTA(String[][] arrayPlusStrings,String [][] arrayMinusStrings, boolean expectMaxAutomataToBeTheSameAsPTA)
	{
		Configuration conf = mainConfiguration.copy();
		Set<List<Label>> plusStrings = buildSet(arrayPlusStrings,conf,converter), minusStrings = buildSet(arrayMinusStrings,conf,converter);
		DirectedSparseGraph actualA = null, actualC = null, actualD = null, actualE = null, actualF = null;
		IllegalArgumentException eA = null, eC = null, eD = null, eE = null, eF = null;
		try
		{
			actualA = Test_Orig_RPNIBlueFringeLearner.createAugmentedPTA(plusStrings, minusStrings);
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eA = e;
		}

		try
		{
			Configuration config = mainConfiguration.copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			l.init(plusStrings, minusStrings);
			actualC = l.getTentativeAutomaton().pathroutines.getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eC = e;
		}

		try
		{
			Configuration config = mainConfiguration.copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			PTASequenceEngine engine = buildPTA(plusStrings, minusStrings);
			checkPTAConsistency(engine, plusStrings, true);if (engine.numberOfLeafNodes()>0) checkPTAConsistency(engine, minusStrings, false);
			l.init(engine,0,0);
			actualD = l.getTentativeAutomaton().pathroutines.getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eD = e;
		}

		try
		{
			Configuration config = mainConfiguration.copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			l.init(buildPTA(plusStrings, buildSet(new String[][] {},config,converter)),0,0);
			for(List<Label> seq:minusStrings)
			{
				Set<List<Label>> negativeSeq = new HashSet<List<Label>>();negativeSeq.add(seq);
				l.getTentativeAutomaton().paths.augmentPTA(buildPTA(buildSet(new String[][] {},config,converter),negativeSeq));
			}
			actualE = l.getTentativeAutomaton().pathroutines.getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eE = e;
		}

		try
		{
			Configuration config = mainConfiguration.copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			l.getTentativeAutomaton().initPTA();
			l.getTentativeAutomaton().paths.augmentPTA(minusStrings, false,true);
			l.getTentativeAutomaton().paths.augmentPTA(plusStrings, true,true);
			actualF = l.getTentativeAutomaton().pathroutines.getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eF = e;
		}

		if (eA != null)
		{// an exception has been thrown, hence verify that all way of PTA construction have thrown too.
			Assert.assertNotNull(eC);
			Assert.assertNotNull(eD);
			Assert.assertNotNull(eE);
			if (expectMaxAutomataToBeTheSameAsPTA) Assert.assertNotNull(eF);
			throw eA;
		}
		
		Assert.assertNull(eA);
		Assert.assertNull(eC);
		Assert.assertNull(eD);
		Assert.assertNull(eE);
		if (expectMaxAutomataToBeTheSameAsPTA) Assert.assertNull(eF);
		
		Assert.assertEquals(1, actualA.getVertices().size());Assert.assertEquals(true, DeterministicDirectedSparseGraph.isAccept( ((Vertex)actualA.getVertices().iterator().next()) ));
		Assert.assertEquals(0, actualA.getEdges().size());

		Assert.assertEquals(1, actualC.getVertices().size());Assert.assertEquals(true, DeterministicDirectedSparseGraph.isAccept( ((Vertex)actualC.getVertices().iterator().next()) )); 
		Assert.assertEquals(0,((CmpVertex)(actualC.getVertices().iterator().next())).getDepth());
		Assert.assertEquals(0, actualC.getEdges().size());

		Assert.assertEquals(1, actualD.getVertices().size());Assert.assertEquals(true, DeterministicDirectedSparseGraph.isAccept( ((Vertex)actualD.getVertices().iterator().next()) )); 
		Assert.assertEquals(0,((CmpVertex)(actualD.getVertices().iterator().next())).getDepth());
		Assert.assertEquals(0, actualD.getEdges().size());

		Assert.assertEquals(1, actualE.getVertices().size());Assert.assertEquals(true, DeterministicDirectedSparseGraph.isAccept( ((Vertex)actualE.getVertices().iterator().next()) )); 
		Assert.assertEquals(0,((CmpVertex)(actualE.getVertices().iterator().next())).getDepth());
		Assert.assertEquals(0, actualE.getEdges().size());

		if (expectMaxAutomataToBeTheSameAsPTA)
		{
			Assert.assertEquals(1, actualF.getVertices().size());Assert.assertEquals(true, DeterministicDirectedSparseGraph.isAccept( ((Vertex)actualF.getVertices().iterator().next()) )); 
			Assert.assertEquals(0,((CmpVertex)(actualF.getVertices().iterator().next())).getDepth());
			Assert.assertEquals(0, actualF.getEdges().size());
		}
	}
	
	/** An empty accept trace. */
	@Test
	public void testPTAconstruction1a()
	{
		checkEmptyPTA(
				new String[][] { new String[]{}},
				new String[][] { }
			,true);
	}

	/** An empty reject trace. */
	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction1b()
	{
		checkEmptyPTA(
				new String[][] { },
				new String[][] { new String[]{} }
			,false);
	}

	/** Make sure that we can augment a graph with a single state which is a reject-state. */
	@Test
	public void testPTAconstruction_singleRejectState()
	{
		Configuration config = mainConfiguration.copy();
		RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
		config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
		// set the initial state to be reject
		l.getTentativeAutomaton().initPTA();l.getTentativeAutomaton().getVertex(new LinkedList<Label>()).setAccept(false);
		// and check how augmentPTA works with such a PTA
		for(boolean maxAutomaton:new boolean[]{true,false})
		{
			for(List<Label> sequence:buildSet(new String[][] { new String[]{} },config,converter))
				l.getTentativeAutomaton().paths.augmentPTA(sequence, false,maxAutomaton,null);
			for(List<Label> sequence:buildSet(new String[][] { },config,converter))
				l.getTentativeAutomaton().paths.augmentPTA(sequence, true,maxAutomaton,null);
			DirectedSparseGraph actualC = l.getTentativeAutomaton().pathroutines.getGraph();
			Assert.assertEquals(1, actualC.getVertices().size());Assert.assertEquals(false, DeterministicDirectedSparseGraph.isAccept( ((Vertex)actualC.getVertices().iterator().next()) )); 
			Assert.assertEquals(0,((CmpVertex)(actualC.getVertices().iterator().next())).getDepth());
			Assert.assertEquals(0, actualC.getEdges().size());
		}
	}

	/** For a maximal automaton, a reject-path overrides an accept-one. This is a test with a single-state graph. */
	@Test
	public void testPTAconstruction_singleRejectState_max()
	{
		Configuration config = mainConfiguration.copy();
		RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
		config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
		l.getTentativeAutomaton().initPTA();
		for(List<Label> sequence:buildSet(new String[][] { new String[]{} },config,converter))
			l.getTentativeAutomaton().paths.augmentPTA(sequence, false,true,null);
		for(List<Label> sequence:buildSet(new String[][] { },config,converter))
			l.getTentativeAutomaton().paths.augmentPTA(sequence, true,true,null);
		DirectedSparseGraph actualC = l.getTentativeAutomaton().pathroutines.getGraph();
		Assert.assertEquals(1, actualC.getVertices().size());Assert.assertEquals(false, DeterministicDirectedSparseGraph.isAccept( ((Vertex)actualC.getVertices().iterator().next()) )); 
		Assert.assertEquals(0,((CmpVertex)(actualC.getVertices().iterator().next())).getDepth());
		Assert.assertEquals(0, actualC.getEdges().size());
	}
	
	/** Empty traces. */
	@Test
	public void testPTAconstruction1c()
	{
		checkEmptyPTA(
				new String[][] {},
				new String[][] {}
			,true);
	}

	/** Builds a PTA from the supplied arguments using two different methods. If any of them throws, checks that another one throws too and then rethrows the exception. 
	 * 
	 * @param arrayPlusStrings allowed sequences
	 * @param arrayMinusStrings sequences ending at a reject state
	 * @param expectedPTA a textual representation of a PTA which should be built.
	 * @param expectMaxAutomataToBeTheSameAsPTA whether we expect augmentation of a maximal automaton to yield the same result as that of a normal PTA.
	 */
	private void checkPTAconstruction(String[][] arrayPlusStrings,String [][] arrayMinusStrings, String expectedPTA, boolean expectMaxAutomataToBeTheSameAsPTA)
	{
		Configuration conf = mainConfiguration.copy();
		Set<List<Label>> plusStrings = buildSet(arrayPlusStrings,conf,converter), minusStrings = buildSet(arrayMinusStrings,conf,converter);
		LearnerGraph actualA = null, actualC =null, actualD = null, actualE = null, actualF = null;
		IllegalArgumentException eA = null, eC = null, eD = null, eE = null, eF = null;
		try
		{
			actualA = new LearnerGraph(Test_Orig_RPNIBlueFringeLearner.createAugmentedPTA(plusStrings, minusStrings),conf);
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eA = e;
		}

		try
		{
			Configuration config = mainConfiguration.copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			l.init(plusStrings, minusStrings);
			actualC = l.getTentativeAutomaton();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eC = e;
		}
		
		try
		{
			Configuration config = mainConfiguration.copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			PTASequenceEngine engine = buildPTA(plusStrings, minusStrings);
			checkPTAConsistency(engine, plusStrings, true);if (engine.numberOfLeafNodes()>0) checkPTAConsistency(engine, minusStrings, false);
			l.init(engine,0,0);
			actualD = l.getTentativeAutomaton();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eD = e;
		}

		try
		{
			Configuration config = mainConfiguration.copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			l.init(buildPTA(plusStrings, buildSet(new String[][] {},config,converter)),0,0);
			for(List<Label> seq:minusStrings)
			{	Set<List<Label>> negativeSeq = new HashSet<List<Label>>();negativeSeq.add(seq);
				l.getTentativeAutomaton().paths.augmentPTA(buildPTA(buildSet(new String[][] {},config,converter),negativeSeq));
			}
			actualE = l.getTentativeAutomaton();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eE = e;
		}

		try
		{
			Configuration config = mainConfiguration.copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			l.getTentativeAutomaton().initPTA();
			l.getTentativeAutomaton().paths.augmentPTA(minusStrings, false,true);
			l.getTentativeAutomaton().paths.augmentPTA(plusStrings, true,true);
			actualF = l.getTentativeAutomaton();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eF = e;
		}

		if (eA != null)
		{
			Assert.assertNotNull(eC);
			Assert.assertNotNull(eD);
			Assert.assertNotNull(eE);
			if (expectMaxAutomataToBeTheSameAsPTA) Assert.assertNotNull(eF);
			throw eA;
		}

		Assert.assertNull(eA);
		Assert.assertNull(eC);
		Assert.assertNull(eD);
		Assert.assertNull(eE);
		if (expectMaxAutomataToBeTheSameAsPTA) Assert.assertNull(eF);

		Configuration config = mainConfiguration.copy();
		config.setAllowedToCloneNonCmpVertex(true);
		checkM(expectedPTA,actualA, config,converter);
		checkM(expectedPTA,actualC, config,converter);checkDepthLabelling(actualC);
		//Visualiser.updateFrame(actualE,FsmParser.buildGraph(expectedPTA,"expected graph"));Visualiser.waitForKey();
		checkM(expectedPTA,actualD,config,converter);checkDepthLabelling(actualD);
		checkM(expectedPTA,actualE,config,converter);checkDepthLabelling(actualE);
		
		if (expectMaxAutomataToBeTheSameAsPTA) checkM(expectedPTA,actualF,config,converter);
		checkDepthLabelling(actualF);
	}
	
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void checkDepthLabelling(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> coregraph)
	{
		Map<CmpVertex,Integer> stateToDepth = AbstractLearnerGraph.constructMap(coregraph.config,coregraph);
		CmpVertex from = coregraph.getInit();
		stateToDepth.put(from, 0);
		Queue<CmpVertex> fringe = new LinkedList<CmpVertex>();
		Map<CmpVertex,CmpVertex> statesInFringe = AbstractLearnerGraph.constructMap(coregraph.config,coregraph);// in order not to iterate through the list all the time.
				
		fringe.add(from);statesInFringe.put(from,from);
		while(!fringe.isEmpty())
		{
			CmpVertex currentState = fringe.remove();
			Integer currentDepth = stateToDepth.get(currentState);
			Map<Label,TARGET_TYPE> targets = coregraph.transitionMatrix.get(currentState);
			if(targets != null && !targets.isEmpty())
				for(Entry<Label,TARGET_TYPE> labelstate:targets.entrySet())
					for(CmpVertex target:coregraph.getTargets(labelstate.getValue()))
					{
						if (!statesInFringe.containsKey(target)) // put returns the old value, so if it returned null, it means that target was not already in the list (but it has since been added)
						{
							int newDepth = currentDepth+1;
							stateToDepth.put(target,newDepth);
							Assert.assertEquals("state "+target,newDepth,target.getDepth());
							fringe.offer(target);statesInFringe.put(target, target);
						}
					}
		}
	}
	
	/** Builds a maximal automaton from the supplied arguments. 
	 * 
	 * @param arrayPlusStrings allowed sequences.
	 * @param arrayMinusStrings sequences ending at a reject state.
	 * @param initialMax the maximal automaton to augment.
	 * @param expectedMAX a textual representation of a maximal automaton 
	 * which should be an outcome of augmenting the supplied one with the provided sequences.
	 */
	private void checkMAXconstruction(String[][] arrayPlusStrings,String [][] arrayMinusStrings, String initialMax,String expectedMAX)
	{
		Configuration config = mainConfiguration.copy();
		Set<List<Label>> plusStrings = buildSet(arrayPlusStrings,config,converter), minusStrings = buildSet(arrayMinusStrings,config,converter);
		
		LearnerGraph graph = new LearnerGraph(config);
		if (initialMax != null)
			graph= FsmParserStatechum.buildLearnerGraph(initialMax, "initial_max",config,converter);
		for(Entry<CmpVertex,List<Label>> entry:graph.pathroutines.computeShortPathsToAllStates().entrySet())
			entry.getKey().setDepth(entry.getValue().size());// add depth information to states.
		
		graph.config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
		graph.paths.augmentPTA(plusStrings, true,true);
		graph.paths.augmentPTA(minusStrings, false,true);
		checkM(expectedMAX,graph,graph.config,converter);
		checkDepthLabelling(graph);
	}
	
	/** A trace goes through a reject-state. */
	@Test
	public void testPTAconstruction2()
	{
		checkPTAconstruction(
			new String[][] { new String[]{"a","b","c"}, new String[]{"a","d","c"}},
			new String[][] { new String[]{"a","b","c","d"} },
			"A-a->B--b->C-c->Z1-d-#R\nB--d->C2-c->E2"
			,true);
	}

	/** A trace goes through a reject-state. */
	@Test
	public void testPTAconstruction3()
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, new String[]{"a","u"} },
				"A-a->B--b->C-c->E1-d-#R\nB--d->C2-c->E2\nB-u-#A2",true);
	}

	/** A trace goes through a reject-state. */
	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction4()
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, 
						new String[]{"a","b"} },
				"junk",false);
	}


	/** A trace goes through a reject-state. */
	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction5()
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, 
						new String[]{"a","b","c"} },
				"junk",false);
	}

	/** A trace goes through a reject-state. */
	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction6()
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, 
						new String[]{"a","b","c","d","e"} },
				"junk",true);
	}
	
	/** A trace goes through a reject-state. */
	@Test
	public void testPTAconstruction_max1()
	{
		checkMAXconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { 
						new String[]{"a","b","c"} },null,
				"A-a->B-b->C1-c-#R\nB-d->D-c->C2\n");
	}
	
	/** Checks that if we add a positive path over a negative, we get an exception regardless of whether we add this to 
	 * a maximal automaton or to a PTA.
	 */
	@Test
	public void testAddPositive()
	{
		final Configuration conf = mainConfiguration;
		Set<List<Label>> plusStrings = buildSet(new String[][] { 
				new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},conf,converter), 
		minusStrings = buildSet(new String[][] { new String[]{"a","b","c","d"}},conf,converter);

		for(boolean max:new boolean[]{true,false})
		{
			final boolean maxAutomaton = max;
			Configuration config = mainConfiguration.copy();
			final RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			l.getTentativeAutomaton().initPTA();
			l.getTentativeAutomaton().paths.augmentPTA(minusStrings, false,maxAutomaton);
			l.getTentativeAutomaton().paths.augmentPTA(plusStrings, true,maxAutomaton);
			
			checkForCorrectException(new whatToRun() { public @Override void run() throws NumberFormatException {
				l.getTentativeAutomaton().paths.augmentPTA(buildSet(new String[][] { new String[]{"a","b","c","d"}},conf,converter),true,maxAutomaton);
			}},IllegalArgumentException.class,"incompatible ");
		}
	}
	
	/** Tests that updating a maximal automata can chop off parts of a tree. */
	@Test
	public void testPTAconstruction_max2()
	{
		checkMAXconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c"} }, null,
						"A-a->B-b->C1-c-#R\nB-d->D-c->C2\n");
	}
	
	/** Tests that updating a maximal automata can chop off parts of a tree. */
	@Test
	public void testPTAconstruction_max3()
	{
		checkMAXconstruction(
				new String[][] { },
				new String[][] { new String[]{"a","b","c"} }, "A-a->B-b->C-c->A",
						"A-a->B-b->C1-c-#R\n");
	}

	/** Tests that updating a maximal automata can chop off parts of a tree. */
	@Test
	public void testPTAconstruction_max4()
	{
		checkMAXconstruction(
				new String[][] { },
				new String[][] { new String[]{"a","b"} }, "A-a->B-b->C-c->A\nA-b->C-d->C",
					"A-a->B-b-#R\nC-c->A\nA-b->C-d->C");
	}
	
	/** Tests that updating a maximal automata can chop off parts of a tree. */
	@Test
	public void testPTAconstruction_max5()
	{
		Configuration config = mainConfiguration;
		Set<List<Label>> minusStrings = buildSet(new String[][] { new String[]{} },config,converter);
		
		LearnerGraph graph = buildLearnerGraph("A-a->B-b->C-c->A\nA-b->C-d->C", "initial_max",mainConfiguration.copy(),converter);
		graph.findVertex(VertexID.parseID("A")).setDepth(0);
		graph.config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
		graph.paths.augmentPTA(minusStrings, false,true);

		final LearnerGraph expected = new LearnerGraph(mainConfiguration.copy());
		expected.getVertex(new LinkedList<Label>()).setAccept(false);
		DifferentFSMException result = WMethod.checkM(expected,graph);
		Assert.assertNull(result);
		checkDepthLabelling(graph);
	}
}
