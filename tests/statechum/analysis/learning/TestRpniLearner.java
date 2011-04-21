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

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.FsmParser;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.TestEquivalenceChecking;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.apps.QSMTool;
import statechum.model.testset.PTASequenceSet;

import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;
import static statechum.analysis.learning.rpnicore.TestGraphBasicAlgorithms.constructPairScore;

@RunWith(Parameterized.class)
public class TestRpniLearner extends Test_Orig_RPNIBlueFringeLearnerTestComponent
{
	@Parameters
	public static Collection<Object[]> data() 
	{
		return Configuration.configurationsForTesting();
	}
	
	/** Given a test configuration, returns a textual description of its purpose. 
	 * 
	 * @param config configuration to consider
	 * @return description.
	 */ 
	public static String parametersToString(Configuration config)
	{
		return Configuration.parametersToString(config);
	}
	
	public TestRpniLearner(Configuration conf) {
		super(null,conf);mainConfiguration = conf;
	}

	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public void beforeTest()
	{
		testConfig = mainConfiguration.copy();
	}

	/** The working configuration to use when running tests. */
	private Configuration testConfig = null;
	
	/** Each test starts with this configuration. */
	private Configuration mainConfiguration = null;
	
	protected void checkLearner(String fsmString, String name,String [][] plus, String [][] minus)
	{
		final DirectedSparseGraph g = FsmParser.buildGraph(fsmString, name,config);
		final LearnerGraph expected = new LearnerGraph(g,testConfig);
		
		// now sanity checking on the plus and minus sets
		for(String [] path:plus)
			assert AbstractOracle.USER_ACCEPTED == expected.paths.tracePathPrefixClosed(QSMTool.buildList(path,config));
		for(String [] path:minus)
			assert AbstractOracle.USER_ACCEPTED != expected.paths.tracePathPrefixClosed(QSMTool.buildList(path,config));
		// Visualiser.getVisualiser()
		Learner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,testConfig,null,null))
		{
			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					List<Label> question, @SuppressWarnings("unused") int valueForNoRestart,
					@SuppressWarnings("unused") List<Boolean> acceptedElements,
					@SuppressWarnings("unused") PairScore pairBeingMerged,
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				return new Pair<Integer,String>(expected.paths.tracePathPrefixClosed(question),null);
			}
		};
		config.setDebugMode(false);
		//l.setPairsMergedPerHypothesis(0);
		//l.setGeneralisationThreshold(1);
		//l.setCertaintyThreshold(5);
		testConfig.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);
		LearnerGraph learntStructureA = new LearnerGraph(l.learnMachine(buildSet(plus,config), buildSet(minus,config)),expected.config);
		// Now do the same with ptasets instead of real sets
		PTASequenceSet plusPTA = new PTASequenceSet();plusPTA.addAll(buildSet(plus,config));PTASequenceSet minusPTA = new PTASequenceSet();minusPTA.addAll(buildSet(minus,config));
		LearnerGraph learntStructureB = new LearnerGraph(l.learnMachine(plusPTA, minusPTA),expected.config);
		Assert.assertNull(WMethod.checkM(learntStructureA, learntStructureB));
		LearnerGraph learntMachineNoRejects = new LearnerGraph(expected.config);
		AbstractPathRoutines.removeRejectStates(learntStructureA,learntMachineNoRejects);
		Assert.assertNull(WMethod.checkM(learntMachineNoRejects, expected));
	}
	
	/**Machines learnt are not identical to reference machines because reference
	 * machines are incomplete and learnt machines contain transitions
	 * to reject-states. 
	 */
	@Test
	public void testLearner1()
	{
		checkLearner("A-a->B<-a-A\nA-b->A","testLearner1",
				new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b"}}, 
				new String[][]{new String[]{"a","b"},new String[]{"a","a"}});
	}

	@Test
	public void testLearner2a()
	{
		checkLearner("A-a->B<-a-C-b->A\nA-b->C\nC-c->C\n","testLearner2a",
				new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b","c"}, new String[]{"b","c","c"}  }, 
				new String[][]{new String[]{"c"}});
	}

	@Test
	public void testLearner2b()
	{
		checkLearner("A-a->B<-a-C-b->A\nA-b->C\nC-c->C\n","testLearner2b",new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b","c"},new String[]{"b","c","c"}}, new String[][]{new String[]{"c"},new String[]{"b","b","c"}	});
	}

	@Test
	public void testLearner3()
	{
		checkLearner("A-text->B-text->B\nA-figure->C-figure->C\nB-figure->C\nC-text->B\nB-set_position->F\nF-edit->G\nG-finalize->A\nC-set_position->D\nD-set_dimensions->E-set_dimensions->E-figure->C\nE-text->B",
				"testLearner3",
				new String[][]{new String[]{"figure", "figure","set_position","set_dimensions","set_dimensions","set_dimensions","set_dimensions", "figure", "set_position", "set_dimensions"}, 
				new String[]{"figure", "figure","set_position","set_dimensions","set_dimensions","set_dimensions","text", "set_position", "edit"}, 
				new String[]{"text","text","set_position","edit","finalize","text"}, 
				new String[]{"text","figure","figure"}, 
				new String[]{"text","text","set_position","edit","finalize","figure"}}, 
				
				new String[][]{
				new String[]{"text","text","set_position","edit","finalize","set_dimensions"},
				new String[]{"text","text","set_position","edit","finalize","set_position"}
		});
		
	}

	DeterministicVertex p = new DeterministicVertex("P"), q= new DeterministicVertex("Q");
	/** Checks that both the old and the new algorithm reports a pair of states as incompatible. */
	public final void testNewLearnerIncompatible(String fsm, String name)
	{
		DirectedSparseGraph g = FsmParser.buildGraph(fsm, name,config);
		//Visualiser.updateFrame(g, null);Visualiser.waitForKey();
		LearnerGraph s = new LearnerGraph(g, testConfig);
		OrigStatePair pairOrig = new OrigStatePair(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("B"), g),
				DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("A"), g));
		StatePair pairNew = new StatePair(s.findVertex(new VertexID("B")),s.findVertex(new VertexID("A")));
		doneEdges = new HashSet<DirectedSparseEdge>();
		int origScore = computeScore(g, pairOrig),
			newScoreA = s.pairscores.computeStateScore(pairNew);
		Assert.assertEquals(-1, origScore);
		Assert.assertEquals(-1, newScoreA);
	}
	
	/** Typically the max score is computed depending on a graph which 
	 * is not convenient for testing. We hence set it to a predefined constant.
	 */
	public final int maxScoreConstant = 9000;
	
	/** Checks that both the old and the two new algorithms report the same score for a pair of states and ask the same questions.
	 * @param states being merged are called "A" and "B". 
	 */
	public final void testNewLearnerQuestions(String fsm, int expectedScore, String learnerName)
	{
		DirectedSparseGraph g = FsmParser.buildGraph(fsm, learnerName,config);
		//Visualiser.updateFrame(g, null);Visualiser.waitForKey();
		LearnerGraph s = new LearnerGraph(g, testConfig);
		OrigStatePair pairOrig = 
			new OrigStatePair(
					DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("B"), g),
					DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("A"), g));
		StatePair pairNew1 = new StatePair(s.findVertex(new VertexID("B")),s.findVertex(new VertexID("A")));
		DirectedSparseGraph 
			temp = mergeAndDeterminize((Graph)g.copy(), pairOrig),
			tempB = MergeStates.mergeAndDeterminize(g, pairNew1,testConfig);
		
		// Now check that ComputeStateScores properly does  mergeAndDeterminize 
		// (on the test data we are dealing with in these tests, there are separate tests for mergeAndDeterminize)
		LearnerGraph tempG = new LearnerGraph(temp,testConfig), tempBG = new LearnerGraph(tempB,testConfig);
		Assert.assertEquals(false, tempG.pathroutines.checkUnreachableStates());Assert.assertEquals(false, tempBG.pathroutines.checkUnreachableStates());
		Assert.assertNull(WMethod.checkM(tempG, tempBG));
		
		
		doneEdges = new HashSet<DirectedSparseEdge>();
		int origScore = computeScore(g, pairOrig),
			newScoreA = s.pairscores.computeStateScore(pairNew1),
			newScoreB = s.pairscores.computePairCompatibilityScore(pairNew1),
			newScoreC = s.pairscores.computePairCompatibilityScore_general(pairNew1, new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>());

		LearnerGraph learner2 = new LearnerGraph(g, testConfig);
		StatePair pairNew2 = new StatePair(learner2.findVertex(new VertexID("B")),learner2.findVertex(new VertexID("A")));
		//Visualiser.updateFrame(g, MergeStates.mergeAndDeterminize_general(learner2, pairNew2).pathroutines.getGraph(learnerName));Visualiser.waitForKey();
		Collection<List<Label>> 
			// Since computeQS assumes that red names remain unchanged in the merged version, I have to use a specific merging procedure
			questionsB = ComputeQuestions.computeQS_orig(pairNew2, learner2,MergeStates.mergeAndDeterminize(learner2, pairNew2)),
			questionsC = ComputeQuestions.computeQS_orig(pairNew2, learner2,MergeStates.mergeAndDeterminize_general(learner2, pairNew2)),
			questionsD = ComputeQuestions.computeQS_general(pairNew2, learner2, MergeStates.mergeAndDeterminize_general(learner2, pairNew2), 
					new ComputeQuestions.QSMQuestionGenerator()).getData();
		Assert.assertTrue("these states should be compatible - correct test data",origScore >= 0);
		Assert.assertEquals(expectedScore, origScore);
		Assert.assertEquals(expectedScore, newScoreA);
		Assert.assertTrue( expectedScore < 0? (newScoreB < 0):(newScoreB >= 0));
		Assert.assertTrue( expectedScore < 0? (newScoreC < 0):(newScoreC >= 0));
		if (expectedScore != -1)
		{
			Set<List<Label>> oldQuestions = new HashSet<List<Label>>();oldQuestions.addAll(generateQuestions(g,temp, pairOrig));
			//Assert.assertTrue(oldQuestions.size() > 0);
			Set<List<Label>> newQuestionsB = new HashSet<List<Label>>();newQuestionsB.addAll(questionsB);
			Set<List<Label>> newQuestionsC = new HashSet<List<Label>>();newQuestionsC.addAll(questionsC);
			Set<List<Label>> newQuestionsD = new HashSet<List<Label>>();newQuestionsD.addAll(questionsD);
			Assert.assertTrue("different questions: old "+oldQuestions+", new "+questionsB,oldQuestions.equals(newQuestionsB));
			Assert.assertTrue("different questions: old "+oldQuestions+", new "+questionsC,oldQuestions.equals(newQuestionsC));
			Assert.assertTrue("different questions: old "+oldQuestions+", new "+questionsD,oldQuestions.equals(newQuestionsD));
		}
	}
	
	public static final String PTA1 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2\nB1-b->B3-b->B4\n";
	
	@Test(expected = IllegalArgumentException.class)
	public final void testLearnerFailsWhenRedNotFound()
	{
		ComputeQuestions.computeQS_orig(new StatePair(null,new DeterministicVertex("non-existing")), new LearnerGraph(testConfig), new LearnerGraph(testConfig));
	}
	
	@Test
	public final void testNewLearner0()
	{
		testNewLearnerIncompatible("A-a->A1\nA-p->P1-q-#B","testNewLearner0");
	}
	
	@Test
	public final void testNewLearner1a()
	{
		testNewLearnerIncompatible("A-a->A"+PTA1,"testNewLearner1a");
	}

	@Test
	public final void testNewLearner1b()
	{
		testNewLearnerQuestions("A-a->A"+"\nA-p->I-q->B"+"\nB-a->B1-a->B2\nB1-b->B3-b->B4\n",2,"testNewLearner1b");
	}
	
	@Test
	public final void testNewLearner2()
	{
		testNewLearnerIncompatible("A-a->A1-a->A2"+PTA1,"testNewLearner2");
	}

	@Test
	public final void testNewLearner3()
	{
		testNewLearnerQuestions("A-a->A1-b->A1"+PTA1,3,"testNewLearner3");
	}
	
	@Test
	public final void testNewLearner4()
	{
		testNewLearnerQuestions("A-a->A1-b->A1-a-#A2"+PTA1,4,"testNewLearner4");
	}
	
	@Test
	public final void testNewLearner5()
	{
		testNewLearnerIncompatible("A-a->A1-a-#ARej\nA1-b-#A2"+PTA1,"testNewLearner5");
	}

	@Test
	public final void testNewLearner6()
	{
		testNewLearnerIncompatible("A-a->A1-a-#ARej\nA1-b->A2-b-#A3"+PTA1,"testNewLearner6");
	}
	
	@Test
	public final void testNewLearner7()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-b->A2-b->A3"+PTA1,4,"testNewLearner7");
	}
	
	public static final String PTA2 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2\nB1-b->B3-b->B4\nB1-c->BB1-c->BB2\n";
	
	@Test
	public final void testNewLearner8()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-b->A2\nA1-c->A1"+PTA2,5,"testNewLearner8");
	}
	
	@Test
	public final void testNewLearner9()
	{
		testNewLearnerIncompatible("A-a->A1-a-#ARej\nA1-b->A2\nA1-c-#A3"+PTA2,"testNewLearner9");
	}
	
	@Test
	public final void testNewLearner10()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-b->A2\nA1-c->A3"+PTA2,4,"testNewLearner10");
	}
	
	@Test
	public final void testNewLearner11()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-b->A1\nA1-c->A3"+PTA2,5,"testNewLearner11");
	}
	
	protected static final String PTA_4 = "\nB1-d->B3a-d->B4a-c->B5a-c->B6a\nB3a-c->B4c-c->B5c-c->B6c\nB3b-d->B4d-c->B5d-c->B6d\nB1-c->B3b-c->B4b-c->B5b-c->B6b\n";
	public static final String PTA3 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2"+PTA_4;
	
	@Test
	public final void testNewLearner_2_1()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2\nA1-c->A1"+PTA3,8,"testNewLearner_2_1");
	}
	
	@Test
	public final void testNewLearner_2_2()
	{
		testNewLearnerIncompatible("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2\nA1-c-#A3"+PTA3,"testNewLearner_2_2");
	}
	
	@Test
	public final void testNewLearner_2_3()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2\nA1-c->A3"+PTA3,4,"testNewLearner_2_3");
	}
	
	@Test
	public final void testNewLearner_2_4()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A3"+PTA3,7,"testNewLearner_2_4");
	}
	
	@Test
	public final void testNewLearner_2_6()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A1"+PTA3,16,"testNewLearner_2_6");
	}
	
	@Test
	public final void testNewLearner_2_7()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+PTA3,8,"testNewLearner_2_7");
	}

	@Test
	public final void testNewLearner_3_1()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A2\nA1-c->A1"+PTA3,8,"testNewLearner_3_1");
	}
	
	@Test
	public final void testNewLearner_3_2()
	{
		testNewLearnerIncompatible("A-a->A1-a-#ARej\nA1-d->A2\nA1-c-#A3"+PTA3,"testNewLearner_3_2");
	}
	
	@Test
	public final void testNewLearner_3_3()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A2\nA1-c->A3"+PTA3,4,"testNewLearner_3_3");
	}
	
	@Test
	public final void testNewLearner_3_4()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A3"+PTA3,7,"testNewLearner_3_4");
	}
	
	@Test
	public final void testNewLearner_3_5a()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A3-c->A3"+PTA3,13,"testNewLearner_3_5a");
	}
	
	@Test
	public final void testNewLearner_3_5b()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A3-c->A4"+PTA3,10,"testNewLearner_3_5b");
	}
	
	@Test
	public final void testNewLearner_3_6()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A1"+PTA3,16,"testNewLearner_3_6");
	}
	
	@Test
	public final void testNewLearner_3_7()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+PTA3,8,"testNewLearner_3_7");
	}

	@Test
	public final void testNewLearner_4_1() // red and blue are adjacent
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A2\nA1-c->A1"+"\nA-p->B"+"\nB-a->B1-a-#B2"+PTA_4,8,"testNewLearner_4_1");
	}
	
	@Test
	public final void testNewLearner_4_2() // blue node has no access successors
	{
		testNewLearnerQuestions("A-d->A1\nA1-d->A2\nA1-c->A1"+"\nA-p->Atmp-q->B"+"\nB2#-c-B-a-#B1\n",0,"testNewLearner_4_2");
	}
	
	@Test
	public final void testNewLearner_4_3() // testing of folding of a long path into a complex machine
	{
		testNewLearnerQuestions("S-d->A-a->S\nA-b->B-a->D-a->E\nS-a->P-b->Q-b->P-t->R",2,"testNewLearner_4_3");
	}

	@Test
	public final void testNewLearner_4_4() // testing that loops around the red states are being dealt with correctly
	{
		testNewLearnerQuestions("S-m->A\nS-n->A-d->A-a->A1\nA-b->A2\nA-c->B-c->B1-p->B2",1,"testNewLearner_4_4");
	}

	@Test
	public final void testNewLearner_4_4_simple() // a simplified version of test 4_4
	{
		testNewLearnerQuestions("S-m->A\nA-d->A-a->A1\nA-b->A2\nA-c->B-c->B1-p->B2",1,"testNewLearner_4_4_simple");
	}

	@Test
	public final void testNewLearner_4_5() // testing that shortest paths actually work
	{
		testNewLearnerQuestions("A-a->A1-c->A2\nA-b->A2\nA-n->B-a->B1-c->B2-p->B3-f->B4-g->B5",2,"testNewLearner_4_5");
	}

	@Test
	public final void testNewLearner_4_6() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-c->A2\nA-b->A1-d->A2\nA-n->B\nB-a->B1-c->B3\nB-b->B2-d->B4-p->B5\nB1-d->B6-f->B7",5,"testNewLearner_4_6");
	}

	@Test
	public final void testNewLearner_4_7() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-c->A2\nA-b->A1-d->A2\nA-n->An-n->B\nAn-m->B\nB-a->B1-c->B3\nB-b->B2-d->B4-p->B5",4,"testNewLearner_4_7");
	}

	@Test
	public final void testNewLearner_4_8a() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-c->A2\nA-b->A1-d->A2\nA-n->An-n->B\nAn-k->A\nA-j->B\nA-v->P-l->P1\nA-u->P\nAn-m->B\nB-a->B1-c->B3\nB-b->B2-d->B4-p->B5",4,"testNewLearner_4_8a");
	}

	@Test
	public final void testNewLearner_4_8b() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-c->A2\nA-b->A1-d->A2\nA-n->An-n->B\nAn-k->A\nA-j->Atmp-j->B\nA-v->P-l->P1\nA-u->P\nAn-m->B\nB-a->B1-c->B3\nB-b->B2-d->B4-p->B5",4,"testNewLearner_4_8b");
	}

	@Test
	public final void testNewLearner_4_9() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-c->A2\nA-b->A1-d->A2\nA-n->B\nB-a->B1-c->B3\nB-b->B2-d->B4-p->B5\nA2-r->A3-r->A4-i->A5",4,"testNewLearner_4_9");
	}
	
	@Test
	public final void testNewLearner_4_10() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-b->A2\n"+
				"A-d->B-d->B0-a->B1-b->B2-c->B3\n"+
				"B0-d->C0-a->C1-b->C2-c->C3\nC2-f->C4",5,"testNewLearner_4_10");
	}
	
	@Test
	public final void testNewLearner_4_11a() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-d->A-a->A1-b->A2\n"+
				"S-m->B-d->B0-a->B1-b->B2-c->B3\n"+
				"B0-d->C0-a->C1-b->C2-c->C3\nC2-f->C4",6,"testNewLearner_4_11a");
	}
	
	@Test
	public final void testNewLearner_4_11b() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("R-q->S-n->A-d->A-a->A1-b->A2\n"+
				"R-p->S-m->B-d->B0-a->B1-b->B2-c->B3\n"+
				"B0-d->C0-a->C1-b->C2-c->C3\nC2-f->C4",6,"testNewLearner_4_11b");
	}
	
	@Test
	public final void testNewLearner_4_12() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-b->A2\n"+
				"S-m->B-d->B0-a->B1-b->B2-c->B3\n"+
				"B0-d->C0-a->C1-b->C2-c->C3\nC2-f->C4",0,"testNewLearner_4_12");
	}
	
	@Test
	public final void testNewLearner_4_13() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-d->A-a->A1-b->A2\n"+
				"S-m->B-d->B0-a->B1-b->B2-c->B3\n"+
				"B0-d->C0-a->C1-b->C2-c->C3\nC2-f->C4",6,"testNewLearner_4_13");
	}
	
	private static Vertex getTempRed_DijkstraShortestPath(DirectedSparseGraph model, Vertex r, DirectedSparseGraph temp)
	{
		DijkstraShortestPath p = new DijkstraShortestPath(model);
		List<Edge> pathToRed = p.getPath(DeterministicDirectedSparseGraph.findInitial(model), r);
		Vertex tempRed = null;
		if(!pathToRed.isEmpty()){
			List<Label> pathToRedString = new LinkedList<Label>();
			for(Edge e:pathToRed)
				pathToRedString.add( ((Collection<Label>)e.getUserDatum(JUConstants.LABEL)).iterator().next() );
			tempRed = Test_Orig_RPNIBlueFringeLearner.getVertex(temp, pathToRedString);
		}
		else
			tempRed = DeterministicDirectedSparseGraph.findInitial(temp);
		return tempRed;
	}
		
	@Test
	public final void testGetTempRed1()
	{
		DirectedSparseGraph a=FsmParser.buildGraph("A-a->B", "testGetTempRed1 model",config),
			temp=FsmParser.buildGraph("C-d->Q", "testGetTempRed1 temp",config);
		Vertex foundA = getTempRed_DijkstraShortestPath(a, DeterministicDirectedSparseGraph.findInitial(a), temp);
		Vertex foundB =Test_Orig_RPNIBlueFringeLearnerTestComponent.getTempRed(a, DeterministicDirectedSparseGraph.findInitial(a), temp);
		Assert.assertTrue(DeterministicDirectedSparseGraph.findInitial(temp).equals(foundA));
		Assert.assertTrue(DeterministicDirectedSparseGraph.findInitial(temp).equals(foundB));
	}
	
	@Test
	public final void testGetTempRed2()
	{
		DirectedSparseGraph a=FsmParser.buildGraph("A-a->B-a->B-c->C-c->D", "testGetTempRed1 model",config),
			temp=FsmParser.buildGraph("C-a->Q-a->Q-c->Q", "testGetTempRed1 temp",config);
		Vertex foundA = getTempRed_DijkstraShortestPath(a, DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("D"), a), temp);
		Vertex foundB = Test_Orig_RPNIBlueFringeLearnerTestComponent.getTempRed(a, DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("D"), a), temp);
		Assert.assertTrue(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("Q"), temp).equals(foundA));
		Assert.assertTrue(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("Q"), temp).equals(foundB));
	}
	
	@Test
	public final void findMergeablePair1()
	{
		DirectedSparseGraph g=FsmParser.buildGraph("A-a->B\nA-b->B\nA-c->C\nA-d->D", "findMergeablePair1",config);
		Assert.assertNull(Test_Orig_RPNIBlueFringeLearner.findMergablePair(g));
	}
	
	@Test
	public final void findMergeablePair2()
	{
		DirectedSparseGraph g=FsmParser.buildGraph("A-a->B\nA-b->B\nA-c->D\nA-b->D\nA-d->E", "findMergeablePair2",config);
		Vertex 
			b = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("B"), g),
			d = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("D"), g);
		Set<Vertex> expected = new HashSet<Vertex>();expected.add(d);expected.add(b);
		Set<Vertex> actualA = new HashSet<Vertex>();
		OrigStatePair value = Test_Orig_RPNIBlueFringeLearner.findMergablePair(g);actualA.add(value.getQ());actualA.add(value.getR());
		Assert.assertTrue("expected: B, D in either order got: "+actualA,expected.equals(actualA));
	}
	
	@Test
	public final void findMergeablePair3a()
	{
		DirectedSparseGraph g=FsmParser.buildGraph("S-p->A-a->S\nA-b->S\nA-c->D\nA-b->D\nA-d->E", "findMergeablePair3a",config);
		Vertex 
			s = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("S"), g),
			d = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("D"), g);
		OrigStatePair expected = new OrigStatePair(d,s),
		actualA = Test_Orig_RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertTrue("expected: "+expected+" got: "+actualA,expected.equals(actualA));
	}

	@Test
	public final void findMergeablePair3b()
	{
		DirectedSparseGraph g=FsmParser.buildGraph("S-p->A-a->B\nA-b->B\nA-c->S\nA-b->S\nA-d->E", "findMergeablePair3b",config);
		Vertex 
			b = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("B"), g),
			s = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("S"), g);
		OrigStatePair expected = new OrigStatePair(b,s),
		actualA = Test_Orig_RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertTrue("expected: "+expected+" got: "+actualA,expected.equals(actualA));
	}

	@Test
	public final void findMergeablePair4()
	{
		DirectedSparseGraph g=FsmParser.buildGraph("S-p->A-a->B\nA-b->B\nA-c-#D\nA-b-#D\nA-d->E", "findMergeablePair4",config);
		OrigStatePair actualA = Test_Orig_RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertNull(actualA);
	}

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
		DirectedSparseGraph g=FsmParser.buildGraph(machineToMerge, graphName,config),
			g2=(DirectedSparseGraph)g.copy();
		Vertex 
			a = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID(stateRed), g),
			b = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID(stateBlue), g);
				
		Assert.assertNotNull("state "+stateRed+" was not found", a);
		Assert.assertNotNull("state "+stateBlue+" was not found", b);
		
		OrigStatePair pairOrig = new OrigStatePair(b,a);
		StatePair pairNew1 = new StatePair(DeterministicDirectedSparseGraph.findVertexNamed(new VertexID(stateBlue), g),DeterministicDirectedSparseGraph.findVertexNamed(new VertexID(stateRed), g));
		LearnerGraph l = new LearnerGraph(g, testConfig);
		StatePair pairNew2 = new StatePair(l.findVertex(new VertexID(stateBlue)),l.findVertex(new VertexID(stateRed)));
		LearnerGraph 
			mergeResultA = new LearnerGraph(Test_Orig_RPNIBlueFringeLearner.mergeAndDeterminize(g, pairOrig),testConfig), 
			mergeResultB = new LearnerGraph(MergeStates.mergeAndDeterminize(g2, pairNew1,testConfig),testConfig),
			mergeResultC = new LearnerGraph(MergeStates.mergeAndDeterminize(l, pairNew2).pathroutines.getGraph(),testConfig),
			mergeResultD = new LearnerGraph(MergeStates.mergeAndDeterminize_general(l, pairNew2).pathroutines.getGraph(),testConfig),
			expectedMachine = buildLearnerGraph(expectedFSM, "expected machine",testConfig);

		TestEquivalenceChecking.checkM(machineToMerge, g2, testConfig);
		
		Assert.assertFalse("unreachable states - original",mergeResultA.pathroutines.checkUnreachableStates());
		Assert.assertFalse("unreachable states",mergeResultB.pathroutines.checkUnreachableStates());
		Assert.assertFalse("unreachable states",mergeResultC.pathroutines.checkUnreachableStates());
		Assert.assertFalse("unreachable states",mergeResultD.pathroutines.checkUnreachableStates());
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
	}

	@Test
	public final void testMerge1a()
	{
		DirectedSparseGraph g=FsmParser.buildGraph("S-p->A-a->S\nA-b->S\nA-c->D\nA-b->D\nA-d->E\nS-n->U", "testMerge1a",config);
		Vertex 
			s = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("S"), g),
			d = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("U"), g);
		OrigStatePair pair = new OrigStatePair(d,s);
		LearnerGraph 
			mergeResultA = new LearnerGraph(Test_Orig_RPNIBlueFringeLearner.mergeAndDeterminize(g, pair),testConfig),
			expectedResult = buildLearnerGraph("S-p->A-a->S\nA-b->S\nA-c->S\nA-d->E\nS-n->S", "expected",testConfig);
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
		DirectedSparseGraph g=FsmParser.buildGraph(largeGraph1_invalid5,"testMerge_fail1",config);
		CmpVertex 
			a = DeterministicDirectedSparseGraph.findVertexNamed(new VertexID("A"), g),
			b = DeterministicDirectedSparseGraph.findVertexNamed(new VertexID("B"), g);
		StatePair pair = new StatePair(b,a);// A is red
		MergeStates.mergeAndDeterminize(g, pair,testConfig);
	}
		
	@Test(expected = IllegalArgumentException.class)
	public final void testMerge_fail2()
	{
		LearnerGraph l=buildLearnerGraph(largeGraph1_invalid5,"testMerge_fail1",testConfig);
		CmpVertex 
			a = l.findVertex(new VertexID("A")),
			b = l.findVertex(new VertexID("B"));
		StatePair pair = new StatePair(b,a);// A is red
		MergeStates.mergeAndDeterminize(l, pair);
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testMerge_fail3()
	{
		LearnerGraph l=buildLearnerGraph(largeGraph1_invalid5,"testMerge_fail1",testConfig);
		CmpVertex 
			a = l.findVertex(new VertexID("A")),
			b = l.findVertex(new VertexID("B"));
		StatePair pair = new StatePair(b,a);// A is red
		MergeStates.mergeAndDeterminize_general(l, pair);
	}

	protected interface InterfaceChooserToTest {
		Stack<? extends StatePair> choosePairs();
	}

	/**
	 *  Tests the pair selection function of both the original and the optimised implementation.
	 *  <p>
	 * Important: the improved and the original versions of score computation are only compatible if 
	 * the machine has no parallel edges - these are the only allowed machines in tests of
	 * the choice of states.
	 * 
	 * @param fsm the graph to choose states in
	 * @param initialReds the initial set of reds
	 * @param expectedReds the set of sets of expected reds (there could be a number of possible outcomes, depending on the traversal taken by a method under test)
	 * @param expectedPairs a set of pairs which has to be returned
	 * @param graphName the name to give to the constructed graph
	 */
	public final void testChooseStatePairs(String fsm, String [] initialReds, String [][] expectedReds, List<PairScore> expectedPairs, String graphName)
	{
		final DirectedSparseGraph gB = FsmParser.buildGraph(fsm, graphName,config);
		//Visualiser.updateFrame(new LearnerGraph(gB,Configuration.getDefaultConfiguration()), null);Visualiser.waitForKey();
		// check how the reference pair selection function performs
		Configuration conf = testConfig.copy();conf.setLearnerUseStrings(false);conf.setLearnerCloneGraph(false);
		testChooseStatePairsInternal(gB,new LearnerGraph(gB, conf), initialReds, expectedReds, expectedPairs, new InterfaceChooserToTest() {
			public @Override Stack<StatePair> choosePairs() {// Here I need to convert the old type of pairs to the new one.
				Stack<OrigStatePair> pairs = chooseStatePairs(gB, new HashSet<List<Label>>(), new HashSet<List<Label>>());
				Stack<StatePair> result = new Stack<StatePair>();
				for(OrigStatePair pair:pairs) result.add(new StatePair((CmpVertex)pair.getQ(),(CmpVertex)pair.getR()));
				return result;
			}
		});

		final DirectedSparseGraph gA = FsmParser.buildGraph(fsm, graphName,config);
		// check how the revised pair selection function performs
		final LearnerGraph s = new LearnerGraph(gA, testConfig);
		testChooseStatePairsInternal(gA,s, initialReds, expectedReds, expectedPairs, new InterfaceChooserToTest() {
			public @Override Stack<? extends StatePair> choosePairs() {
				return s.pairscores.chooseStatePairs();
			}
		});
	}
	
	private final void testChooseStatePairsInternal(DirectedSparseGraph g,LearnerGraph l, String [] initialReds, String [][] expectedReds, 
			List<PairScore> expectedPairs,InterfaceChooserToTest chooser)
	{
		for(String red:initialReds)
		{
			CmpVertex v = l.findVertex(new VertexID(red));v.setColour(JUConstants.RED);
		}
		Stack<? extends StatePair> pairs = chooser.choosePairs();

		Map<Integer,Set<PairScore>> distribution = new HashMap<Integer,Set<PairScore>>();// maps scores to sets of states which should correspond to them. The aim is to verify the contents of the stack regardless of the order in which elements with the same score are arranged.

		Set<Set<String>> expectedRedsAsSet = new HashSet<Set<String>>();
		for(int i=0;i<expectedReds.length;++i) 
		{
			Set<String> possibleReds = new HashSet<String>();possibleReds.addAll(Arrays.asList(expectedReds[i]));
			expectedRedsAsSet.add(possibleReds);
		}
		Set<String> finalReds = new HashSet<String>();
		DirectedSparseGraph grf = l.pathroutines.getGraph();
		for(Vertex red:DeterministicDirectedSparseGraph.findVertices(JUConstants.COLOUR, JUConstants.RED, grf))
				finalReds.add(((VertexID)red.getUserDatum(JUConstants.LABEL)).toString());
		Assert.assertTrue("expected red states, any of: "+expectedRedsAsSet+" actual : "+finalReds,expectedRedsAsSet.contains(finalReds));
		for(PairScore ps:expectedPairs)
		{
			Set<PairScore> currScore = distribution.get(ps.getScore()); 
			if (currScore == null)
			{
				currScore = new HashSet<PairScore>();distribution.put(ps.getScore(),currScore);
			}
			currScore.add(ps);
		}
		int lastScore = -1;
		for(StatePair elem:pairs)
		{
			doneEdges = new HashSet<DirectedSparseEdge>();
			DeterministicVertex origBlue = DeterministicDirectedSparseGraph.findVertexNamed(elem.getQ().getID(), g);
			DeterministicVertex origRed = DeterministicDirectedSparseGraph.findVertexNamed(elem.getR().getID(), g);
			int currentScore = computeScore(g, new OrigStatePair(origBlue,origRed));// This one returns vertices from g, but elem may easily contain StringVertices and such, hence convert elem to Vertex-pair.
			PairScore elA = constructPairScore(elem.getQ().getID().toString(),elem.getR().getID().toString(),currentScore, testConfig);
			PairScore elB = constructPairScore(elem.getR().getID().toString(),elem.getQ().getID().toString(),currentScore, testConfig);
			Assert.assertTrue(elem.getR().getColour() == JUConstants.RED);
			Assert.assertTrue(elem.getQ().getColour() == JUConstants.BLUE);
			Assert.assertTrue(currentScore >= 0);
			Assert.assertTrue("unexpected pair returned: "+elem+", score "+currentScore,distribution.containsKey(currentScore));
			Set<PairScore> ps = distribution.get(currentScore);
			Assert.assertTrue("unexpected state pair "+elem+" with score "+currentScore,
					ps.contains(elA) || ps.contains(elB));
			ps.remove(elA);ps.remove(elB);if (ps.isEmpty()) distribution.remove(currentScore);
			
			if (lastScore >= 0)
				Assert.assertTrue("elements were returned in the wrong order, current is "+currentScore+" previous was "+lastScore,lastScore <= currentScore);
			lastScore = currentScore;
		}
		
		Assert.assertEquals("unused entries : "+distribution,0, distribution.size());
	}
	
	@Test
	public final void testNewchooseStatePairs1()
	{
		List<PairScore> pairsAndScores = new LinkedList<PairScore>();
		pairsAndScores.add(constructPairScore("A2", "A", 0,testConfig));
		pairsAndScores.add(constructPairScore("U1", "A", 2,testConfig));
		pairsAndScores.add(constructPairScore("S1", "A", 3,testConfig));
		pairsAndScores.add(constructPairScore("R1", "A", 2,testConfig));
		pairsAndScores.add(constructPairScore("Q1", "A", 1,testConfig));
		pairsAndScores.add(constructPairScore("P2", "A", 0,testConfig));
		for(String state:new String[]{"A2","U1","S1","R1","Q1","P2"})
			pairsAndScores.add(constructPairScore(state, "P1", 0,testConfig));

		testChooseStatePairs(
				"A-a-#Arej\nA-d->A2-c->A3-c->A4-c->A5\n"+
				"A-p->P1-a->P2\n"+
				"A-s->S1-d->S2-c->S3-c->S4\n"+
				"A-r->R1-d->R2-c->R3\n"+
				"A-u->U1-d->U2-c->U3\n"+
				"A-q->Q1-d->Q2",
				new String[]{"A"},
				new String[][] {new String[]{"A","P1","Arej"}},
				pairsAndScores,"testNewchooseStatePairs1");
	}
	
	@Test
	public final void testNewchooseStatePairs2()
	{
		List<PairScore> pairsAndScores = new LinkedList<PairScore>();
		pairsAndScores.add(constructPairScore("A2", "A", 0,testConfig));
		pairsAndScores.add(constructPairScore("U1", "A", 2,testConfig));
		pairsAndScores.add(constructPairScore("S1", "A", 3,testConfig));
		pairsAndScores.add(constructPairScore("R1", "A", 2,testConfig));
		pairsAndScores.add(constructPairScore("Q1", "A", 1,testConfig));
		pairsAndScores.add(constructPairScore("P2", "A", 0,testConfig));
		for(String state:new String[]{"A2","U1","S1","R1","Q1","P2"})
			pairsAndScores.add(constructPairScore(state, "P1", 0,testConfig));
		pairsAndScores.add(constructPairScore("P3", "Arej", 0,testConfig));

		testChooseStatePairs(
				"A-a-#Arej\nA-d->A2-c->A3-c->A4-c->A5\n"+
				"A-p->P1-a->P2\n"+"P1-b-#P3\n"+
				"A-s->S1-d->S2-c->S3-c->S4\n"+
				"A-r->R1-d->R2-c->R3\n"+
				"A-u->U1-d->U2-c->U3\n"+
				"A-q->Q1-d->Q2",
				new String[]{"A"},
				new String[][] {new String[]{"A","P1","Arej"},new String[]{"A","P1","P3"}},
				pairsAndScores,"testNewchooseStatePairs2");
	}
	
	@Test
	public final void testNewchooseStatePairs3()
	{
		List<PairScore> pairsAndScores = new LinkedList<PairScore>();
		pairsAndScores.add(constructPairScore("REJ1", "REJ2", 0,testConfig));
		pairsAndScores.add(constructPairScore("A1", "A", 3,testConfig));

		testChooseStatePairs(
				"A-a->B1-a-#REJ1\nB1-b-#REJ2\n"+
				"A-b->A1-a->B2\n"+
				"A1-a->B2\n"+
				"A1-b->A2-a->B3\n",
				new String[]{"A"},
				new String[][] {new String[]{"A","B1","REJ2"},new String[]{"A","B1","REJ1"}},
				pairsAndScores,"testNewchooseStatePairs3");
	}
	
	/*
	private static int testNumber = 1;
	private void buildRubyTests(String fsm,int expectedScore,String graphName)
	{
		LearnerGraph s = buildLearnerGraph(fsm, graphName, testConfig);
		StringBuffer test = new StringBuffer();
		
		test.append("def testScores");test.append(testNumber++);test.append("\n\tdfa = ADL::parse <<-AUTOMATON\n");
		test.append(s.pathroutines.toADL());test.append("AUTOMATON\n\n");
		test.append("assert_equal(");test.append(expectedScore);
		test.append(",dfa.compute_score(dfa.get_state(\"A\"), dfa.get_state(\"B\")))\nend\n");
		System.out.println(test.toString());
	}
	*/
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
		
		DirectedSparseGraph g = FsmParser.buildGraph(fsm, graphName,config);
		OrigStatePair pairOrig = new OrigStatePair(
				DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("B"), g),
				DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("A"), g));
		
		LearnerGraph s = new LearnerGraph(g, testConfig);
		StatePair pairNew = new StatePair(
				s.findVertex(new VertexID("B")),
				s.findVertex(new VertexID("A")));
		doneEdges = new HashSet<DirectedSparseEdge>();
		s.config.setLearnerScoreMode(Configuration.ScoreMode.CONVENTIONAL);s.setMaxScore(maxScoreConstant-1);
		int origScore = computeScore(g, pairOrig),
			newScoreA = s.pairscores.computeStateScore(pairNew),
			newScoreB = s.pairscores.computePairCompatibilityScore(pairNew);
		s.config.setLearnerScoreMode(Configuration.ScoreMode.COMPATIBILITY);
		int newScoreC = s.pairscores.computePairCompatibilityScore(pairNew);
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
				maxScoreConstant,1,1,
				"testPairCompatible1");
	}
	
	private static void matchCollectionsOfVertices(Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> what, String[][] expectedSrc)
	{
		Set<Set<String>> expectedSets = new HashSet<Set<String>>();
		for(String []seq:expectedSrc)
		{
			Set<String> whatToAdd = new HashSet<String>();
			whatToAdd.addAll(Arrays.asList(seq));expectedSets.add(whatToAdd);
		}
		
		for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:what) 
		{
			Set<String> oneOfTheSets = new HashSet<String>();
			for(CmpVertex vert:eqClass.getStates()) oneOfTheSets.add(vert.getID().toString());
			Assert.assertTrue("received an unexpected set "+oneOfTheSets,expectedSets.contains(oneOfTheSets));expectedSets.remove(oneOfTheSets);
		}
		Assert.assertEquals(0, expectedSets.size());		
	}
	
	private void testGeneralPairScoreComputation(String machine, String graphName, int expectedScore,
			String[][] expectedSrc,String [][]incompatibles)
	{
		DirectedSparseGraph g=FsmParser.buildGraph(machine, graphName,config);
		LearnerGraph fsm = new LearnerGraph(g,config);

		if (incompatibles != null)
			for(String [] incompatibleRow:incompatibles)
			{
				assert incompatibleRow.length == 2;
				fsm.addToCompatibility(fsm.findVertex(incompatibleRow[0]), fsm.findVertex(incompatibleRow[1]),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
			}
		Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> result = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		int score = -2;
		score = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex(new VertexID("A")),fsm.findVertex(new VertexID("B"))),result);
		//Visualiser.updateFrame(g, result);Visualiser.waitForKey();
		Assert.assertEquals(expectedScore, score);
		if (score >=0)
			matchCollectionsOfVertices(result, expectedSrc);
		
		result.clear();score = -2;
		score = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex(new VertexID("B")),fsm.findVertex(new VertexID("A"))),result);
		Assert.assertEquals(expectedScore, score);
		if (score >=0)
			matchCollectionsOfVertices(result, expectedSrc);
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
				"B-e->B4-d->C3-e->T1\n"+
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
/*
	@Test
	public final void testPairCompatible8()
	{
		String fsm = "A-a->A\nB-a->C-a->D-a->E\nB-b->B1-b->B2\nC-c->C1-c->C2\nD-d->D1-d->D2";
		testGeneralPairScoreComputation(fsm, "testPairCompatible8",4,
				new String[][] {new String[]{"A","B","C","D","E"}}
				,null
		);
	}
*/
	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
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
}
