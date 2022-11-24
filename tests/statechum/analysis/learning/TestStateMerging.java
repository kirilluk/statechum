/* Copyright (c) 2006-2016 The University of Sheffield
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
 */ package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static statechum.analysis.learning.rpnicore.FsmParserStatechum.buildLearnerGraph;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.ParameterizedWithName;
import org.junit.runners.Parameterized.Parameters;

import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.Test_Orig_RPNIBlueFringeLearner.OrigStatePair;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.rpnicore.FsmParserStatechum;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.TestEquivalenceChecking;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;
import statechum.analysis.learning.rpnicore.old_generalised_merge_routines.OldMergeStates;
import statechum.analysis.learning.rpnicore.old_generalised_merge_routines.OldPairScoreComputation;

@RunWith(ParameterizedWithName.class)
public class TestStateMerging 
{
	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		return Configuration.configurationsForTesting();
	}
	
	/** Typically the max score is computed depending on a graph which 
	 * is not convenient for testing. We hence set it to a predefined constant.
	 */
	public static final int maxScoreConstant = 9000;
	
	/** Given a test configuration, returns a textual description of its purpose. 
	 * 
	 * @param config configuration to consider
	 * @return description.
	 */ 
	@org.junit.runners.ParameterizedWithName.ParametersToString
	public static String parametersToString(Configuration config)
	{
		return Configuration.parametersToString(config);
	}
	
	public TestStateMerging(Configuration conf) 
	{
		mainConfiguration = conf;
		labelConverter = conf.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?new Transform.InternStringLabel():null;
	}

	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public void beforeTest()
	{
		config = mainConfiguration.copy();
	}

    protected final ConvertALabel labelConverter;
    
	public ConvertALabel getLabelConverter()
    {
    	return labelConverter;
    }

    /** The working configuration to use when running tests. */
	private Configuration config = null;
	
	
	
	/** Each test starts with this configuration. */
	private Configuration mainConfiguration = null;


	/** Tests merging of states <em>stateRed</em> and <em>stateBlue</em> of <em>machineToMerge</em>.
	 * The outcome of merging has to be equivalent to <em>expectedFSM</em>. 
	 * <p>
	 * Important: testing of <em>buildVertexToEqClassMap</em> is included in testFSMAlgo.
	 * 
	 * @param machineToMerge machine to merge
	 * @param expectedFSM the expected result
	 * @param stateBlue the name of the second state to merge.
	 * @param stateRed the name of the first state to merge
	 * @param name of the graph - important to store the layout information 
	 * @param checkWithEquals whether the equivalence between the result of merging is to be assessed by 
	 * running a language equivalence query or by doing a .equals on FSMStructures corresponding to them. This will usually
	 * be false.
	 */
	public void checkCorrectnessOfMerging(String machineToMerge, String expectedFSM, 
			String stateBlue, String stateRed, String graphName, boolean checkWithEquals)
	{
		DirectedSparseGraph g= FsmParserStatechum.buildLearnerGraph(machineToMerge, graphName,config,getLabelConverter()).pathroutines.getGraph(),
			g2=(DirectedSparseGraph)g.copy();
		DeterministicVertex 
			a = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID(stateRed), g),
			b = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID(stateBlue), g);
				
		Assert.assertNotNull("state "+stateRed+" was not found", a);
		Assert.assertNotNull("state "+stateBlue+" was not found", b);
		
		OrigStatePair pairOrig = new OrigStatePair(b,a);
		StatePair pairNew1 = new StatePair(DeterministicDirectedSparseGraph.findVertexNamed(VertexID.parseID(stateBlue), g),DeterministicDirectedSparseGraph.findVertexNamed(VertexID.parseID(stateRed), g));
		LearnerGraph l = new LearnerGraph(g, config);
		StatePair pairNew2 = new StatePair(l.findVertex(VertexID.parseID(stateBlue)),l.findVertex(VertexID.parseID(stateRed)));
		LearnerGraph 
			mergeResultA = new LearnerGraph(Test_Orig_RPNIBlueFringeLearner.mergeAndDeterminize(g, pairOrig),config), 
			mergeResultB = new LearnerGraph(MergeStates.mergeAndDeterminize(g2, pairNew1,config),config),
			mergeResultC = new LearnerGraph(MergeStates.mergeAndDeterminize(l, pairNew2).pathroutines.getGraph(),config),
			mergeResultD = new LearnerGraph(MergeStates.mergeAndDeterminize_general(l, pairNew2).pathroutines.getGraph(),config),
			mergeResultE = null,// computed below
			mergeResultF = null,// computed below
			mergeResultG = OldPairScoreComputation.mergeAndDeterminize_general(l, pairNew2),
			expectedMachine = buildLearnerGraph(expectedFSM, "expected machine",config,getLabelConverter());

		{
			Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			if (l.pairscores.computePairCompatibilityScore_general(pairNew2,null,mergedVertices, false) < 0)
				throw new IllegalArgumentException("elements of the pair "+pairNew2+" are incompatible, orig score was "+l.pairscores.computePairCompatibilityScore(pairNew2));
			mergeResultE = MergeStates.mergeCollectionOfVertices(l,pairNew2.getR(),mergedVertices,null,false);			
		}
		
		{
			Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			if (l.pairscores.computePairCompatibilityScore_general(pairNew2,null,mergedVertices, false) < 0)
				throw new IllegalArgumentException("elements of the pair "+pairNew2+" are incompatible, orig score was "+l.pairscores.computePairCompatibilityScore(pairNew2));
			mergeResultF = MergeStates.mergeCollectionOfVertices(l,pairNew2.getR(),mergedVertices,null,true);			
		}

		
		
		TestEquivalenceChecking.checkM(machineToMerge, new LearnerGraph(g2,config), config, getLabelConverter());
		
		Assert.assertFalse("unreachable states - original",mergeResultA.pathroutines.checkUnreachableStates());
		Assert.assertFalse("unreachable states",mergeResultB.pathroutines.checkUnreachableStates());
		Assert.assertFalse("unreachable states",mergeResultC.pathroutines.checkUnreachableStates());
		Assert.assertFalse("unreachable states",mergeResultD.pathroutines.checkUnreachableStates());
		Assert.assertFalse("unreachable states",mergeResultE.pathroutines.checkUnreachableStates());
		Assert.assertFalse("unreachable states",mergeResultF.pathroutines.checkUnreachableStates());
		Assert.assertFalse("unreachable states",mergeResultG.pathroutines.checkUnreachableStates());
		Assert.assertFalse("unreachable states",expectedMachine.pathroutines.checkUnreachableStates());
		
		if (checkWithEquals)
		{
			Assert.assertTrue("incorrect merging - original",expectedMachine.equals(mergeResultA));
			Assert.assertTrue("incorrect merging - first improved",expectedMachine.equals(mergeResultB));
			Assert.assertTrue("incorrect merging - second improved",expectedMachine.equals(mergeResultC));
		}
		else
		{
			Assert.assertNull(WMethod.checkM(mergeResultA, expectedMachine));
			Assert.assertNull(WMethod.checkM(mergeResultB, expectedMachine));
			Assert.assertNull(WMethod.checkM(mergeResultC, expectedMachine));
		}
		Assert.assertNull(WMethod.checkM(mergeResultD, expectedMachine));
		Assert.assertNull(WMethod.checkM(mergeResultE, expectedMachine));
		Assert.assertNull(WMethod.checkM(mergeResultF, expectedMachine));
		Assert.assertNull(WMethod.checkM(mergeResultG, expectedMachine));
	}

	@Test
	public final void testMerge1a()
	{
		DirectedSparseGraph g= FsmParserStatechum.buildLearnerGraphND("S-p->A-a->S\nA-b->S\nA-c->D\nA-b->D\nA-d->E\nS-n->U", "testMerge1a",config,getLabelConverter()).pathroutines.getGraph();
		DeterministicVertex 
			s = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("S"), g),
			d = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("U"), g);
		OrigStatePair pair = new OrigStatePair(d,s);
		LearnerGraph 
			mergeResultA = new LearnerGraph(Test_Orig_RPNIBlueFringeLearner.mergeAndDeterminize(g, pair),config),
			expectedResult = buildLearnerGraph("S-p->A-a->S\nA-b->S\nA-c->S\nA-d->E\nS-n->S", "expected",config,getLabelConverter());
		Assert.assertTrue(expectedResult.equals(mergeResultA));
	}
	
	@Test
	public final void testMerge1b()
	{
		checkCorrectnessOfMerging("S-p->A-a->S\nA-b->S\nA-c->D\nU-b->E\nA-d->D\nA-n->U",
				"S-p->A-a->S\nA-b->S\nA-c->D\nA-d->D\nA-n->A",
				"U","A",
				"testMerge1b",true);
	}
	
	@Test
	public final void testMerge2()
	{
		checkCorrectnessOfMerging("S-p->A-a->S\nA-b->S\nA-c->D\nA-e->D\nA-d->E\nS-n->U",
				"S-p->A-a->S\nA-b->S\nA-c->S\nA-e->S\nA-d->E\nS-n->U",
				"D","S",
				"testMerge2",true);
	}

	@Test
	public final void testMerge3()
	{
		checkCorrectnessOfMerging(
				"P-a->P1-b->P-b->P2-c->S-p->A-a->S\nA-b->S\nA-c->D\nA-e->D\nA-d->E\nS-n->U",
				"P-a->P1-b->P-b->P2-c->S-p->A-a->S\nA-b->S\nA-c->S\nA-e->S\nA-d->E\nS-n->U",
				"D","S",
				"testMerge3",true);
	}
	
	@Test
	public final void testMerge4()
	{
		checkCorrectnessOfMerging(
				"S-p->S1-b->S1-a->S2\nS1-c->A-a->S1\nA-f->S2\nA-b->A1-b->A2\nA-d->B-c->B7\nB-b->B1-b->B2-d->B3\nB-a->B4-a->B5-e->B6",
				"S-p->S1-b->S1-a->S2\nS1-c->A-a->S1\nA-f->S2\nA-b->A1-b->A2\nA-d->A-c->B7\nS2-e->S3\nA2-d->A3",
				"B","A",
				"testMerge4",false);
	}

	@Test
	public final void testMerge5()
	{
		checkCorrectnessOfMerging(
				"S-p->A-a->B-a->B1-a->B2-d->B3\nA-b->A1-b->A2",
				"S-p->A-a->A-d->A2\nA-b->A1-b->A2",
				"B","A",
				"testMerge5",false);
	}

	@Test
	public final void testMerge6()
	{
		checkCorrectnessOfMerging(
				"S-p->A-a->A3-b->B-a->B1-c->B2\nA-b->A1-b->A2",
				"S-p->A-a->A3-c->A4\nA3-b->A\nA-b->A1-b->A2",
				"B","A",
				"testMerge6",false);
	}


	public static final String 
		largeGraph1 =
		"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA-c->A2-c->A3\n"+"A5<-a-A3-b->A4\n"+
		"A-d->B\n"+
		"B-c->B1-b->B2-a->B3-b->B4-d->B5\n"+
			"B1-c->B6-b->B7\n"+
		"B-a->BD1-a->BD2-a->BD3-b->BD4-c->BD5\n"+
		"B-b->BB1-b->BB2-b->BB3-f->BB4\n",
	
		largeGraph1_invalid1 =
		"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA-c->A2-c->A3\n"+"A5<-a-A3-b->A4\n"+
		"A-d->B\n"+
		"B-c->B1-b->B2-a->B3-b->B4-d->B5\n"+
			"B1-c->B6-b->B7\n"+
		"B-a->BD1-a->BD2-a->BD3-b-#BD4\n"+
		"B-b->BB1-b->BB2-b->BB3-f->BB4\n",

		largeGraph1_invalid2 =
		"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA-c->A2-c->A3\n"+"A5<-a-A3-b->A4\n"+
		"A-d->B\n"+
		"B-c->B1-b->B2-a->B3-b->B4-d->B5\n"+
			"B1-c->B6-b->B7\n"+
		"B-a->BD1-a->BD2-a->BD3-b->BD4-d-#BD5\n"+
		"B-b->BB1-b->BB2-b->BB3-f->BB4\n",
	
		largeGraph1_invalid3 =
		"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA-c->A2-c->A3\n"+"A5<-a-A3-b->A4\n"+
		"A-d->B\n"+
		"B-c->B1-b->B2-a->B3-b->B4-d->B5\n"+
			"B1-c->B6-b->B7\n"+
		"B-a->BD1-a->BD2-a->BD3-b->BD4-c->BD5\n"+
		"B-b->BB1-b->BB2-b-#BB3",
	
		largeGraph1_invalid4 =
		"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA-c->A2-c->A3\n"+"A5<-a-A3-b->A4\n"+
		"A-d->B\n"+
		"B-c->B1-b->B2-a->B3-b->B4-d->B5\n"+
			"B1-c->B6-b-#B7\n"+
		"B-a->BD1-a->BD2-a->BD3-b->BD4-c->BD5\n"+
		"B-b->BB1-b->BB2-b->BB3-f->BB4\n",

		largeGraph1_invalid5 =
		"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA-c->A2-c->A3\n"+"A5<-a-A3-b->A4\n"+
		"A-d->B\n"+
		"B-c->B1-b->B2-a->B3-b->B4-d->B5\n"+
			"B1-c->BC6-b->BC7-f-#BC8\n"+
		"B-a->BD1-a->BD2-a->BD3-b->BD4-c->BD5\n"+
		"B-b->BB1-b->BB2-b->BB3-f->BB4\n",
		
		largeGraph2 =
		"S-a->A-a->S\nA-d->B\nA-c->B\n"+
		"B-a->BL1-a->BL2-d->BL3\n"+
			"BL3-b->BL4-c->BL5\n"+
			"BL3-a->BL6-a->BL7-c->BL8\n"+
				"BL8-b->BL9\n",

		largeGraph3 =
			"S-a->A-a->S\nA-d->B\nA-c->B\n"+
			"B-a->BL1-a->BL2-d->BL3\n"+
			"B-f->B1\n"+
				"BL3-b->BL4-c->BL5\n"+
				"BL3-a->BL6-a->BL7-c->BL8\n"+
					"BL8-b->BL9\n",
					
		largeGraph2_invalid1 =
			"S-a->A-a->S\nA-d->B\nA-c->B\n"+
			"B-a->BL1-a->BL2-d->BL3\n"+
				"BL3-b->BL4-c->BL5\n"+
				"BL3-a->BL6-a->BL7-c->BL8\n"+
					"BL8-b-#BL9\n",

		largeGraph4_invalid1 =
			"S-a->A-a->S\nA-d->B\nA-c->B\n"+
			"B-a->BL1-a->BL2-d->BL3\n"+
				"BL3-b-#BL4\n"+
				"BL3-a->BL6-a->BL7-c->BL8\n"+
					"BL8-b->BL9\n",

		largeGraph5 = 
			"S-n->A-n->An-n->B\n"+
			"A-a->A1-c->A2\nA-b->A1-d->A2\n"+
			"B-a->B1-c->B3-p->B5\n"+
			"B-b->B2-d->B4";
	
	@Test
	public final void testMerge7()
	{
		checkCorrectnessOfMerging(largeGraph1,
			"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA-c->A2-c->A3\n"+"A5<-a-A3-b->A4-f->AA4\n"+
			"A-d->A\nA5-b->AA6-d->AA7\nAA6-c->AA8",
			
			"B","A",
			"testMerge7",false);
	}

	@Test
	public final void testMerge8()
	{
		checkCorrectnessOfMerging(largeGraph2,
			"S-a->A-a->S\n"+
			"A-d->A-c->A-b->BL4-c->BL5",
			
			"B","A",
			"testMerge8",false);
	}

	@Test
	public final void testMerge9()
	{
		checkCorrectnessOfMerging(largeGraph3,
			"S-a->A-a->S\n"+
			"A-d->A-c->A-b->BL4-c->BL5\n"+
			"A-f->B1",
			
			"B","A",
			"testMerge9",false);
	}

	@Test
	public final void testMerge10()
	{
		checkCorrectnessOfMerging(
				"S-a->A-b->A1-c->A2-d->A3-e->A4\n"+
				"S-n->B-b->B1-c->B2-d->B3-e->B4\n"+
				"B-h->C1\nB2-g->C2\nB3-f->C3",
			"S-a->A-b->A1-c->A2-d->A3-e->A4\n"+
			"S-n->A-h->C1\nA2-g->C2\nA3-f->C3",
			
			"B","A",
			"testMerge10",false);
	}

	@Test
	public final void testMerge11()
	{
		checkCorrectnessOfMerging(largeGraph5,
			"S-n->A-n->An-n->A\n"+
			"A-a->A1-c->A2\nA-b->A1-d->A2-p->A3\n",
			
			"B","A",
			"testMerge11",false);
	}

	@Test
	public final void testMerge12()
	{
		checkCorrectnessOfMerging(
				"S-n->A-d->A-a->A1-b->A2\n"+
				"S-m->B-d->B0-a->B1-b->B2-c->B3\n"+
				"B0-d->C0-a->C1-b->C2-c->C3\nC2-f->C4",
			"S-n->A-d->A-a->A1-b->A2\n"+
			"S-m->A\nA2-c->B3\nA2-f->C4\n",
			
			"B","A",
			"testMerge12",false);
	}

	
	@Test(expected = IllegalArgumentException.class)
	public final void testMerge_fail1a()
	{
		DirectedSparseGraph g= FsmParserStatechum.buildLearnerGraph(largeGraph1_invalid5,"testMerge_fail1",config,getLabelConverter()).pathroutines.getGraph();
		CmpVertex 
			a = DeterministicDirectedSparseGraph.findVertexNamed(VertexID.parseID("A"), g),
			b = DeterministicDirectedSparseGraph.findVertexNamed(VertexID.parseID("B"), g);
		StatePair pair = new StatePair(b,a);// A is red
		MergeStates.mergeAndDeterminize(g, pair,config);
	}
		
	@Test(expected = IllegalArgumentException.class)
	public final void testMerge_fail2()
	{
		LearnerGraph l=buildLearnerGraph(largeGraph1_invalid5,"testMerge_fail1",config,getLabelConverter());
		CmpVertex 
			a = l.findVertex(VertexID.parseID("A")),
			b = l.findVertex(VertexID.parseID("B"));
		StatePair pair = new StatePair(b,a);// A is red
		MergeStates.mergeAndDeterminize(l, pair);
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testMerge_fail3()
	{
		LearnerGraph l=buildLearnerGraph(largeGraph1_invalid5,"testMerge_fail1",config,getLabelConverter());
		CmpVertex 
			a = l.findVertex(VertexID.parseID("A")),
			b = l.findVertex(VertexID.parseID("B"));
		StatePair pair = new StatePair(b,a);// A is red
		MergeStates.mergeAndDeterminize_general(l, pair);
	}

	private static void matchCollectionsOfVertices(Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> what, String[][] expectedSrc)
	{
		Set<Set<String>> expectedSets = new HashSet<Set<String>>();
		for(String []seq:expectedSrc)
		{
			Set<String> whatToAdd = new HashSet<String>();
			whatToAdd.addAll(Arrays.asList(seq));expectedSets.add(whatToAdd);
		}
		
		for(EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass:what) 
		{
			Set<String> oneOfTheSets = new HashSet<String>();
			for(CmpVertex vert:eqClass.getStates()) oneOfTheSets.add(vert.getStringId());
			Assert.assertTrue("received an unexpected set "+oneOfTheSets,expectedSets.contains(oneOfTheSets));expectedSets.remove(oneOfTheSets);
		}
		Assert.assertEquals(0, expectedSets.size());		
	}
	
	private void testGeneralPairScoreComputation(String machine, String graphName, int expectedScore,
			String[][] expectedSrc,String [][]incompatibles)
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph(machine, graphName,mainConfiguration,getLabelConverter());

		if (incompatibles != null)
			for(String [] incompatibleRow:incompatibles)
			{
				assert incompatibleRow.length == 2;
				fsm.addToCompatibility(fsm.findVertex(incompatibleRow[0]), fsm.findVertex(incompatibleRow[1]),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
			}
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> result = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		int score = -2;
		score = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex(VertexID.parseID("A")),fsm.findVertex(VertexID.parseID("B"))),null,result, true);
		//Visualiser.updateFrame(g, result);Visualiser.waitForKey();
		Assert.assertEquals(expectedScore, score);
		if (score >=0)
			matchCollectionsOfVertices(result, expectedSrc);
		
		result.clear();score = -2;
		score = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex(VertexID.parseID("B")),fsm.findVertex(VertexID.parseID("A"))),null,result, true);
		Assert.assertEquals(expectedScore, score);
		if (score >=0)
			matchCollectionsOfVertices(result, expectedSrc);

		result.clear();score = -2;
		score = new OldPairScoreComputation(fsm).computePairCompatibilityScore_general(new StatePair(fsm.findVertex(VertexID.parseID("A")),fsm.findVertex(VertexID.parseID("B"))),null,result);
		Assert.assertEquals(expectedScore, score);
		if (score >=0)
			matchCollectionsOfVertices(result, expectedSrc);

		result.clear();score = -2;
		score = new OldPairScoreComputation(fsm).computePairCompatibilityScore_general(new StatePair(fsm.findVertex(VertexID.parseID("B")),fsm.findVertex(VertexID.parseID("A"))),null,result);
		Assert.assertEquals(expectedScore, score);
		if (score >=0)
			matchCollectionsOfVertices(result, expectedSrc);

		if (score >= 0)
		{
			LearnerGraph mergedGraph = MergeStates.mergeCollectionOfVertices(fsm, fsm.findVertex(VertexID.parseID("A")), result, null, true);

			{// try with computePairCompatibilityScore_general with generation of full equivalence classes
				int score2 = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex(VertexID.parseID("B")),fsm.findVertex(VertexID.parseID("A"))),null,result, true);
				Assert.assertEquals(expectedScore, score2);
		
				LearnerGraph mergedTmp = MergeStates.mergeCollectionOfVertices(fsm, fsm.findVertex(VertexID.parseID("A")), result, null, true);
				DifferentFSMException diff = WMethod.checkM(mergedGraph, mergedTmp);
			    if (diff != null)
			       throw diff;
			
			    mergedTmp = MergeStates.mergeCollectionOfVertices(fsm, fsm.findVertex(VertexID.parseID("A")), result, null, false);
				diff = WMethod.checkM(mergedGraph, mergedGraph.getInit(), mergedTmp, mergedTmp.getInit(), VERTEX_COMPARISON_KIND.NONE, false);
			    if (diff != null)
			       throw diff;
			}
		    
			{// try with computePairCompatibilityScore_general with generation of reduced equivalence classes
				int score2 = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex(VertexID.parseID("B")),fsm.findVertex(VertexID.parseID("A"))),null,result, false);
				Assert.assertEquals(expectedScore, score2);
		
				LearnerGraph mergedTmp = MergeStates.mergeCollectionOfVertices(fsm, fsm.findVertex(VertexID.parseID("A")), result, null, true);
				DifferentFSMException diff = WMethod.checkM(mergedGraph, mergedTmp);
			    if (diff != null)
			       throw diff;
			
			    mergedTmp = MergeStates.mergeCollectionOfVertices(fsm, fsm.findVertex(VertexID.parseID("A")), result, null, false);
				diff = WMethod.checkM(mergedGraph, mergedGraph.getInit(), mergedTmp, mergedTmp.getInit(), VERTEX_COMPARISON_KIND.NONE, false);
			    if (diff != null)
			       throw diff;
			}
			
			{// now with the original scoring and merging routines
				int score2 = new OldPairScoreComputation(fsm).computePairCompatibilityScore_general(new StatePair(fsm.findVertex(VertexID.parseID("B")),fsm.findVertex(VertexID.parseID("A"))),null,result);
				Assert.assertEquals(expectedScore, score2);
		
				LearnerGraph mergedTmp = OldMergeStates.mergeCollectionOfVertices(fsm, fsm.findVertex(VertexID.parseID("A")), result);
				DifferentFSMException diff = WMethod.checkM(mergedGraph, mergedTmp);
			    if (diff != null)
			       throw diff;
			}
		}
	}
	
	@Test
	public final void testPairCompatible1_general()
	{
		testGeneralPairScoreComputation(
				"A-a->B-a->C-b->D\n"+
				"A-b->E",
				"testPairCompatible1",
				3, new String[][] {new String[]{"A","B","C"}, new String[]{"D","E"} }
				,null
		);
	}

	@Test
	public final void testPairCompatible2()
	{
		testScoreAndCompatibilityComputation(
				"A-a->B-a->C-b->D\n"+
				"A-b-#E",
				1,-1,-1,
				maxScoreConstant,1,1,
				"testPairCompatible2");
	}

	@Test
	public final void testPairCompatible2_general()
	{
		testGeneralPairScoreComputation(
				"A-a->B-a->C-b->D\n"+
				"A-b-#E",
				"testPairCompatible1",
				-1, null
				,null
		);
	}
	
	@Test
	public final void testPairCompatible_general_A()
	{
		testGeneralPairScoreComputation(
				"A-a->P1-c->B1-b->C1-e->D1\n"+
				"B-a->P2-c->B\n"+
				"A-b->C2-e->D2\n"+
				"B-b->C3-e->D3",
				"testPairCompatible_general_A",
				7, new String[][] {new String[]{"A","B1","B"}, new String[]{"P1","P2"},
						new String[]{"C1","C2","C3"}, new String[]{"D1","D2","D3"}}
				,null
		);
	}

	@Test
	public final void testPairCompatible_general_B()
	{
		testGeneralPairScoreComputation(
				"A-a->B\nA-b->B\nA-e->B\n"+
				"B-e->B4-c->D3-a->T1\n"+
				"B4-d->C3-e->T1\n"+
				"B-c->D1-a->T2\n"+
				"B-b->B5-c->D2-a->T3\n"+
				"B-a->B1-d->C1-e->T4\n"+
				"B1-a->B2-a->B3-d->C2-e->T5",
				"testPairCompatible_general_B",
				14, new String[][] {new String[]{"A","B","B1","B2","B3","B4","B5"}, new String[]{"C1","C2","C3"},
						new String[]{"D1","D2","D3"}, new String[]{"T1","T2","T3","T4","T5"}}
				,null
		);
	}

	@Test
	public final void testPairCompatible_general_C()
	{
		testGeneralPairScoreComputation(
				"A-a->B\nA-b->B\nA-e->B\n"+
				"B4-c->D3-a->T1\n"+
				"B-e->B10-e->B11-e->B12-e->B13-e->B14-e->B15-e->B4-d->C3-e->T1\n"+
				"B-c->D1-a->T2\n"+
				"B-b->B5-c->D2-a->T3\n"+
				"B-a->B1-d->C1-e->T4\n"+
				"B1-a->B2-a->B3-d->C2-e->T5",
				"testPairCompatible_general_C",
				20, new String[][] {new String[]{"A","B","B1","B2","B3","B4","B5","B10","B11","B12","B13","B14","B15"}, new String[]{"C1","C2","C3"},
						new String[]{"D1","D2","D3"}, new String[]{"T1","T2","T3","T4","T5"}}
				,null
		);
	}

	public static final String testGeneralD_fsm =
		"S-p->A-a->A1-a->A3\n"+"A-b->A2-b->A3\nA-c->A2-c->A3\n"+"A5<-a-A3-b->A4\n"+
		"A-d->B\n"+
		"B-c->B1-b->B2-a->B3-b->B4-d->B5\n"+
			"B1-c->B6-b->B7\n"+
		"B-a->BD1-a->BD2-a->BD3-b->BD4-c->BD5\n"+
		"B-b->BB1-b->BB2";

	
	@Test
	public final void testPairCompatible_general_D()
	{
		testGeneralPairScoreComputation(testGeneralD_fsm,
		"testPairCompatible5",
		12, new String[][] {
				new String[]{"A","B"}, new String[]{"BB1","B1","A2" },
				new String[]{"BB2","B2","B6","A3","BD2"},
				new String[]{"A1","BD1"},new String[]{"B7","A4"},
				new String[]{"B3","A5","BD3"}, new String[]{"B4","BD4"},
				new String[]{"B5"}, new String[]{"BD5"},new String[]{"S"}}
		,null
		);
	}

	/** Incompatible vertices do not mess up the merging here. */
	@Test
	public final void testPairCompatible_general_D_incompatible1()
	{
		testGeneralPairScoreComputation(testGeneralD_fsm,
		"testPairCompatible5",
		12, new String[][] {
				new String[]{"A","B"}, new String[]{"BB1","B1","A2" },
				new String[]{"BB2","B2","B6","A3","BD2"},
				new String[]{"A1","BD1"},new String[]{"B7","A4"},
				new String[]{"B3","A5","BD3"}, new String[]{"B4","BD4"},
				new String[]{"B5"}, new String[]{"BD5"},new String[]{"S"}}
		,new String[][]{
				new String[]{"B5","BD5"},new String[]{"B5","S"},new String[]{"A","BB2"}
		}
		);
	}

	/** Incompatible vertices cause merge to fail. */
	@Test
	public final void testPairCompatible_general_D_fail1()
	{
		testGeneralPairScoreComputation(testGeneralD_fsm,
		"testPairCompatible5",
		-1, new String[][] {}
		,new String[][]{
				new String[]{"A","B"}
		}
		);
	}

	/** Incompatible vertices cause merge to fail. */
	@Test
	public final void testPairCompatible_general_D_fail2()
	{
		testGeneralPairScoreComputation(testGeneralD_fsm,
		"testPairCompatible5",
		-1, new String[][] {}
		,new String[][]{
				new String[]{"BB2","BD2"}
		}
		);
	}

	/** Incompatible vertices cause merge to fail. */
	@Test
	public final void testPairCompatible_general_D_fail3()
	{
		testGeneralPairScoreComputation(testGeneralD_fsm,
		"testPairCompatible5",
		-1, new String[][] {}
		,new String[][]{
				new String[]{"B2","A3"}
		}
		);
	}

	/** Incompatible vertices cause merge to fail. */
	@Test
	public final void testPairCompatible_general_D_fail4()
	{
		testGeneralPairScoreComputation(testGeneralD_fsm,
		"testPairCompatible5",
		-1, new String[][] {}
		,new String[][]{
				new String[]{"B4","BD4"}
		}
		);
	}
	
	/** Checks that revised version of the generalised merger is not affected by the true/false switch. */
	public final static void checkScoringAndMerging(List<StatePair> pairsToMerge, LearnerGraph orig, int expectedScore, LearnerGraph expected)
	{
		for(boolean gA:new boolean[]{true,false})
			for(boolean gB:new boolean[]{true,false})
			{
				Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
				int score = orig.pairscores.computePairCompatibilityScore_general(null,pairsToMerge, verticesToMerge, gA);
				Assert.assertEquals(expectedScore, score);
				LearnerGraph mergedOutcome =  MergeStates.mergeCollectionOfVertices(orig, null, verticesToMerge, null,gB);
				DifferentFSMException diffEx = WMethod.checkM(expected, mergedOutcome);
				if (diffEx != null)
					throw diffEx;
			}
		{// now with the original scoring and merging routines
			Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			int score2 = new OldPairScoreComputation(orig).computePairCompatibilityScore_general(null,pairsToMerge,verticesToMerge);
			Assert.assertEquals(expectedScore, score2);
	
			LearnerGraph mergedTmp = OldMergeStates.mergeCollectionOfVertices(orig,null, verticesToMerge);
			DifferentFSMException diff = WMethod.checkM(expected, mergedTmp);
		    if (diff != null)
		       throw diff;
		}
	}
	
	@Test
	public final void testPairCompatible_general_E()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("I-d->A1-a->A2-b->A3-c->A4 / I-b->B1-a->B2-b->B3 / I-c->C1-a->C2-b->C3", "testPairCompatible_general_Ea",config,getLabelConverter());
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsToMerge = Arrays.asList(new StatePair[]{
				new StatePair(fsm.findVertex("I"),fsm.findVertex("A1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("B1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("C1"))
				});
		int score = fsm.pairscores.computePairCompatibilityScore_general(null,pairsToMerge, verticesToMerge, true);
		LearnerGraph mergedOutcome =  MergeStates.mergeCollectionOfVertices(fsm, null, verticesToMerge, null,true);
		LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("I-d->I-b->I-c->I / I-a->B2-b->B3-c->C1", "testPairCompatible_general_Eb",config,getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, mergedOutcome);
		if (diffEx != null)
			throw diffEx;
		
		// Now test all the versions of state merging routine.
		checkScoringAndMerging(pairsToMerge, fsm,score, expected);
	}

	@Test
	public final void testPairCompatible_general_F()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("I-a->A1-a->A2-b->A3-c->A4 / I-b->B1-a->B2-b->B3 / I-c->C1-a->C2-b->C3", "testPairCompatible_general_Fa",config,getLabelConverter());
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsToMerge = Arrays.asList(new StatePair[]{
				new StatePair(fsm.findVertex("I"),fsm.findVertex("A1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("B1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("C1"))
				});
		int score = fsm.pairscores.computePairCompatibilityScore_general(null,pairsToMerge, verticesToMerge, true);
		LearnerGraph mergeOutcome =  MergeStates.mergeCollectionOfVertices(fsm, null, verticesToMerge, null,true);
		LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("I-b->I-c->I / I-a->I", "testPairCompatible_general_Fb",config,getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, mergeOutcome);
		if (diffEx != null)
			throw diffEx;

		// Now test all the versions of state merging routine.
		checkScoringAndMerging(pairsToMerge, fsm,score, expected);
	}

	@Test
	public final void testPairCompatible_general_G()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("I-d->A1-a->A2-b->A3-c->A4 / I-b->B1-a->B2-b->B3 / I-c->C1-a->C2-b->C3 / A1-b->T1-c->T1 / A1-c->T2-a->A2 / B1-b->T2-e->B2 / C1-c->T3-a->T4", "testPairCompatible_general_Ga",config,getLabelConverter());
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsToMerge = Arrays.asList(new StatePair[]{
				new StatePair(fsm.findVertex("I"),fsm.findVertex("A1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("B1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("C1"))
				});
		int score = fsm.pairscores.computePairCompatibilityScore_general(null,pairsToMerge, verticesToMerge, true);
		LearnerGraph mergeOutcome =  MergeStates.mergeCollectionOfVertices(fsm, null, verticesToMerge, null,true);
		LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("I-d->I-b->I-c->I / I-a->B2-b->B3-c->C1 / I-e->B2", "testPairCompatible_general_Gb",config,getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, mergeOutcome);
		if (diffEx != null)
			throw diffEx;

		// Now test all the versions of state merging routine.
		checkScoringAndMerging(pairsToMerge, fsm,score, expected);
	}

	@Test
	public final void testPairCompatible_general_H()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("I-d->A1-a->A2-b->A3-c->A4 / I-b->B1-a->B2-b->B3 / I-c->C1-a->C2-b->C3 / A1-b->T1-c->T1 / A1-c->T2-a->A2 / B1-b->T2-d->B2 / C1-c->T3-a->T4", "testPairCompatible_general_Ha",config,getLabelConverter());
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsToMerge = Arrays.asList(new StatePair[]{
				new StatePair(fsm.findVertex("I"),fsm.findVertex("A1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("B1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("C1"))
				});
		int score = fsm.pairscores.computePairCompatibilityScore_general(null,pairsToMerge, verticesToMerge, true);
		LearnerGraph mergeOutcome =  MergeStates.mergeCollectionOfVertices(fsm, null, verticesToMerge, null,true);
		LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("I-b->I-c->I / I-a->I-d->I", "testPairCompatible_general_Hb",config,getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, mergeOutcome);
		if (diffEx != null)
			throw diffEx;

		// Now test all the versions of state merging routine.
		checkScoringAndMerging(pairsToMerge, fsm,score, expected);
	}

	@RunWith(ParameterizedWithName.class)
	public static class TestRandomFSMMergers
	{
		@Parameters
		public static Collection<Object[]> data() 
		{
			Collection<Object[]> outcome = new LinkedList<Object[]>();
			for(Object[] data:Configuration.configurationsForTesting())
				for(int i=1;i<10;++i)
				{
					Object[] outcomeEntry = new Object[data.length+1];System.arraycopy(data, 0, outcomeEntry, 0, data.length);outcomeEntry[data.length]=i;
					outcome.add(outcomeEntry);
				}
			
			return outcome;
		}
		
		/** Given a test configuration, returns a textual description of its purpose. 
		 * 
		 * @param config configuration to consider
		 * @parma i the parameter used to generate random FSMs
		 * @return description.
		 */ 
		public static String parametersToString(Configuration config, Integer i)
		{
			return Configuration.parametersToString(config)+" "+i;
		}		
		
		protected final Configuration config;
		protected final ConvertALabel converter;
		protected final int fsmNumber;
		
		public ConvertALabel getLabelConverter()
		{
			return converter;
		}
		
		public TestRandomFSMMergers(Configuration conf, Integer i) 
		{
			converter = conf.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?new Transform.InternStringLabel():null;
			config = conf;fsmNumber = i;
		}
		
		
		public interface RequestScoreComputation 
		{
			public void init(LearnerGraph g);
			public int computeScore(StatePair pair, Collection<StatePair> pairsToMerge, Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices);
		}
		
		
		// computePairCompatibilityScore_general with full equivalence class generation
		@Test
		public final void testRandomMergers1() throws IncompatibleStatesException
		{
			testRandomFSMMergers(new RequestScoreComputation() {
				private LearnerGraph graph;
				@Override
				public void init(LearnerGraph g) {
					graph = g;
				}
				
				@Override
				public int computeScore(StatePair pair, Collection<StatePair> pairsToMerge, Collection<EquivalenceClass<CmpVertex, LearnerGraphCachedData>> mergedVertices) {
					return graph.pairscores.computePairCompatibilityScore_general(pair,pairsToMerge,mergedVertices, true);
				}
			});
		}
		
		// computePairCompatibilityScore_general with partial equivalence class generation
		@Test
		public final void testRandomMergers2() throws IncompatibleStatesException
		{
			testRandomFSMMergers(new RequestScoreComputation() {
				private LearnerGraph graph;
				@Override
				public void init(LearnerGraph g) {
					graph = g;
				}
				
				@Override
				public int computeScore(StatePair pair, Collection<StatePair> pairsToMerge, Collection<EquivalenceClass<CmpVertex, LearnerGraphCachedData>> mergedVertices) {
					return graph.pairscores.computePairCompatibilityScore_general(pair,pairsToMerge,mergedVertices, false);
				}
			});
		}
		
		// old computePairCompatibilityScore_general (since old, with full equivalence class generation)
		@Test
		public final void testRandomMergers3() throws IncompatibleStatesException
		{
			testRandomFSMMergers(new RequestScoreComputation() {
				private LearnerGraph graph;
				@Override
				public void init(LearnerGraph g) {
					graph = g;
				}
				
				@Override
				public int computeScore(StatePair pair, Collection<StatePair> pairsToMerge, Collection<EquivalenceClass<CmpVertex, LearnerGraphCachedData>> mergedVertices) {
					return new OldPairScoreComputation(graph).computePairCompatibilityScore_general(pair,pairsToMerge,mergedVertices);
				}
			});
		}
		
		// This one generates a number of random machines; for every pair of vertices in those machines that can be merged, it constructs the set of vertices to merge and then
		// merges some of them (randomly) in one go. Such a merge should generate the same number of states and give the same score.
		public final void testRandomFSMMergers(RequestScoreComputation scoring) throws IncompatibleStatesException
		{
			final int states = 50;
			MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
			LearnerGraph referenceGraph = mg.nextMachine(states/2, -1.0,fsmNumber, config,getLabelConverter()).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, in the mergers below we can still attempt to merge arbitrary subsets of states.
			scoring.init(referenceGraph);
			for(CmpVertex a:referenceGraph.transitionMatrix.keySet())
				for(CmpVertex b:referenceGraph.transitionMatrix.keySet())
				{
					Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					int score = scoring.computeScore(new StatePair(a,b),null,verticesToMerge);
					if (score >= 0)
					{
						Set<Set<CmpVertex>> origVertexPairs = new HashSet<Set<CmpVertex>>();
						for(EquivalenceClass<CmpVertex, LearnerGraphCachedData> cls:verticesToMerge)
							if (cls.getStates().size() > 1)
								origVertexPairs.add(cls.getStates());
						Random rnd = new Random(fsmNumber);
						for(int experiment=0;experiment < 10;++experiment)
						{
							List<StatePair> pairs = new ArrayList<StatePair>(1000);
							for(EquivalenceClass<CmpVertex, LearnerGraphCachedData> cls:verticesToMerge)
							{
								CmpVertex bufferOfVertices[] = cls.getStates().toArray(new CmpVertex[]{});
								for(int i=0;i<10;++i)
									pairs.add(new StatePair(bufferOfVertices[rnd.nextInt(bufferOfVertices.length)],bufferOfVertices[rnd.nextInt(bufferOfVertices.length)]));
									
							}
							Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> newVerticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
							
							int newScore = scoring.computeScore(new StatePair(a,b),pairs,newVerticesToMerge);
							/*
							System.out.println("scores: "+score+" "+newScore);
							if (score != newScore)
							{
								System.out.println("State pair: "+(new StatePair(a,b))+"states originally merged: "+verticesToMerge+" states to be merged: "+newVerticesToMerge);
								LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> new2VerticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
								int anotherScore = referenceGraph.pairscores.computePairCompatibilityScore_general(new StatePair(a,b),pairs,new2VerticesToMerge);
								
							}*/
							Assert.assertEquals(score,newScore);
							Set<Set<CmpVertex>> newVertexPairs = new HashSet<Set<CmpVertex>>();
							for(EquivalenceClass<CmpVertex, LearnerGraphCachedData> cls:newVerticesToMerge) 
								if (cls.getStates().size() > 1) 
									newVertexPairs.add(cls.getStates());
							Assert.assertEquals(origVertexPairs,newVertexPairs);
						}
					}
				}
		}
	}
	
	/** Checks that scores are correctly computed on complex graphs, taking the ability of pairCompatibilityScore
	 * to verify compatibility.
	 * 
	 * @param fsm the graph to operate
	 * @param expectedComputeScore the expected score
	 * @param compatibility the expected pair compatibility score
	 * @param k1 k-tails score for k=1
	 * @param k2 k-tails score for k=2
	 * @param k3 k-tails score for k=3
	 * @param graphName the name to give to the graph
	 */
	private void testScoreAndCompatibilityComputation(String fsm, 
			int expectedComputedScore,int pairCompatibilityScore, 
			int pairCompatibility,
			int k1,int k2,int k3,
			String graphName)
	{
		//buildRubyTests(fsm,expectedComputedScore,graphName);
		
		DirectedSparseGraph g = FsmParserStatechum.buildLearnerGraph(fsm, graphName,config,getLabelConverter()).pathroutines.getGraph();
		OrigStatePair pairOrig = new OrigStatePair(
				DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("B"), g),
				DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("A"), g));
		
		LearnerGraph s = new LearnerGraph(g, config);
		StatePair pairNew = new StatePair(
				s.findVertex(VertexID.parseID("B")),
				s.findVertex(VertexID.parseID("A")));
		
		Test_Orig_RPNIBlueFringeLearner origLearner = new Test_Orig_RPNIBlueFringeLearner(null,config,getLabelConverter());origLearner.doneEdges = new HashSet<DirectedSparseEdge>();
		
		s.config.setLearnerScoreMode(Configuration.ScoreMode.CONVENTIONAL);s.setMaxScore(TestStateMerging.maxScoreConstant-1);
		long origScore = origLearner.computeScore(g, pairOrig),
			newScoreA = s.pairscores.computeStateScore(pairNew),
			newScoreB = s.pairscores.computePairCompatibilityScore(pairNew);
		s.config.setLearnerScoreMode(Configuration.ScoreMode.COMPATIBILITY);
		long newScoreC = s.pairscores.computePairCompatibilityScore(pairNew);
		s.config.setLearnerScoreMode(Configuration.ScoreMode.CONVENTIONAL);
		assertEquals(expectedComputedScore, origScore); 
		assertEquals(expectedComputedScore, newScoreA);
		
		
		
		assertEquals(pairCompatibility,newScoreB);
		assertEquals(pairCompatibilityScore,newScoreC);
		
		// Now check what happens in the KTails mode.
		s.config.setLearnerScoreMode(Configuration.ScoreMode.KTAILS);
		s.config.setKlimit(10000);
		assertEquals(expectedComputedScore, s.pairscores.computeStateScore(pairNew));
		
		//Visualiser.updateFrame(g,null);Visualiser.waitForKey();
		
		s.config.setKlimit(1);
		assertEquals(k1, s.pairscores.computeStateScore(pairNew));
		s.config.setKlimit(2);
		assertEquals(k2, s.pairscores.computeStateScore(pairNew));
		s.config.setKlimit(3);
		assertEquals(k3, s.pairscores.computeStateScore(pairNew));
	}
		
	@Test
	public final void testPairCompatible1()
	{
		testScoreAndCompatibilityComputation(
				"A-a->B-a->C-b->D\n"+
				"A-b->E",
				1,2,0,
				TestStateMerging.maxScoreConstant,1,1,
				"testPairCompatible1");
	}

	@Test
	public final void testPairCompatible3()
	{
		testScoreAndCompatibilityComputation(largeGraph1_invalid1,11,-1,-1,
				maxScoreConstant,maxScoreConstant,maxScoreConstant,"testPairCompatible3");
		testGeneralPairScoreComputation(largeGraph1_invalid1, "testPairCompatible3",-1,null
				,null
				);
	}

	@Test
	public final void testPairCompatible4()
	{
		testScoreAndCompatibilityComputation(largeGraph1_invalid2,11,-1,-1,
				maxScoreConstant,maxScoreConstant,maxScoreConstant,"testPairCompatible4");
		testGeneralPairScoreComputation(largeGraph1_invalid2, "testPairCompatible4",-1,null
				,null
				);
	}

	@Test
	public final void testPairCompatible5()
	{
		testScoreAndCompatibilityComputation(largeGraph1_invalid3,-1,-1,-1,
				maxScoreConstant,maxScoreConstant,maxScoreConstant,"testPairCompatible5");
					// the fact that there is a path of length 3 which is 
					// incompatible may or may not cause ktails to choke, 
					// depending on the order of traversal. At present, it does not.
		testGeneralPairScoreComputation(largeGraph1_invalid3, "testPairCompatible5",-1,null
				,null
				);
	}

	@Test
	public final void testPairCompatible6()
	{
		testScoreAndCompatibilityComputation(largeGraph1_invalid4,-1,-1,-1,
				maxScoreConstant,maxScoreConstant,maxScoreConstant,"testPairCompatible6");
		testGeneralPairScoreComputation(largeGraph1_invalid4, "testPairCompatible6",-1,null
				,null
				);
	}

	@Test
	public final void testPairCompatible7()
	{
		testScoreAndCompatibilityComputation(largeGraph1_invalid5,11,-1,-1,
				maxScoreConstant,maxScoreConstant,maxScoreConstant,"testPairCompatible7");
		testGeneralPairScoreComputation(largeGraph1_invalid5, "testPairCompatible7",-1,null
				,null
				);
	}

	@Test
	public final void testPairCompatible2_1()
	{
		testScoreAndCompatibilityComputation(largeGraph2,5,7,2,
				maxScoreConstant,maxScoreConstant,maxScoreConstant,"testPairCompatibl2_1");
		testGeneralPairScoreComputation(largeGraph2, "testPairCompatible2_1",
				8,new String[][] {
				new String[]{"S","BL1","BL6"}, new String[]{"A","B","BL2","BL3","BL7","BL8"},
				new String[]{"BL9","BL4"},new String[]{"BL5"}}
				,null
			);
	}

	@Test
	public final void testPairCompatible2_2()
	{
		testScoreAndCompatibilityComputation(largeGraph3,5,7,2,
				maxScoreConstant,maxScoreConstant,maxScoreConstant,"testPairCompatible2_2");
		testGeneralPairScoreComputation(largeGraph3, "testPairCompatible2_2",
				8,new String[][] {
				new String[]{"S","BL1","BL6"}, new String[]{"A","B","BL2","BL3","BL7","BL8"},
				new String[]{"BL9","BL4"},new String[]{"BL5"}, new String[]{"B1"}}
				,null
		);
	}

	@Test
	public final void testPairCompatible2_3()
	{
		testScoreAndCompatibilityComputation(largeGraph2_invalid1,5,-1,-1,
				maxScoreConstant,maxScoreConstant,maxScoreConstant,"testPairCompatible2_3");
		testGeneralPairScoreComputation(largeGraph2_invalid1, "testPairCompatible2_3",-1,null
				,null
				);
	}

	@Test
	public final void testPairCompatible2_4()
	{
		testScoreAndCompatibilityComputation(largeGraph4_invalid1,5,-1,-1,
				maxScoreConstant,maxScoreConstant,maxScoreConstant,"testPairCompatible2_4");
		testGeneralPairScoreComputation(largeGraph4_invalid1, "testPairCompatible2_4",-1,null
				,null
				);
	}
	
	@Test
	public final void testPairCompatible3_1()
	{
		String fsm = "A-a->B-a->C-a->D";
		testScoreAndCompatibilityComputation(fsm,2,2,0,
				maxScoreConstant,maxScoreConstant,2,"testPairCompatible3_1");
		testGeneralPairScoreComputation(fsm, "testPairCompatible3_1",3,new String[][] {
				new String[]{"A","B","C","D"}}
				,null
		);
	}

	@Test
	public final void testPairCompatible3_2()
	{
		String fsm = "A-a->C-b->B-c->E\nA-c->J\nB-a->F-b->G-a->H-b->I";
		testScoreAndCompatibilityComputation(fsm,5,5,2,
				maxScoreConstant,maxScoreConstant,maxScoreConstant,"testPairCompatible3_2");
		testGeneralPairScoreComputation(fsm, "testPairCompatible3_2",6,new String[][] {
				new String[]{"J","E"},new String[]{"A","B","G","I"},new String[]{"C","F","H"}}
				,null
		);
	}
}
