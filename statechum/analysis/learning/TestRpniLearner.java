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

package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static statechum.analysis.learning.TestFSMAlgo.buildSet;

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
import org.junit.BeforeClass;
import org.junit.Test;

import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.LearnerGraph.IDMode;
import statechum.xmachine.model.testset.PTASequenceSet;
import statechum.xmachine.model.testset.WMethod;

import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

import static statechum.analysis.learning.TestGraphBasicAlgorithms.constructPairScore;

public class TestRpniLearner extends RPNIBlueFringeLearnerTestComponent
{
	public TestRpniLearner() {
		super(null);
	}

	protected int checkPath(FSMStructure expected, DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
	{
		int answer = WMethod.tracePath(expected, question);
		return answer;
	}

	protected void checkLearner(String fsmString, String [][] plus, String [][] minus)
	{
		final DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsmString, "sample FSM");
		final DirectedSparseGraph completedGraph = (DirectedSparseGraph)g.copy();TestFSMAlgo.completeGraph(completedGraph, "REJECT");
		final FSMStructure expected = WMethod.getGraphData(g);
		
		//updateFrame(g, g);

		// now sanity checking on the plus and minus sets
		for(String [] path:plus)
			assert RPNIBlueFringeLearner.USER_ACCEPTED == WMethod.tracePath(expected, Arrays.asList(path));
		for(String [] path:minus)
			assert RPNIBlueFringeLearner.USER_ACCEPTED != WMethod.tracePath(expected, Arrays.asList(path));
		
		RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(Visualiser.getVisualiser())
		{
			protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
			{
				return checkPath(expected, g, question, moreOptions);
			}
		};
		l.setDebugMode(false);
		//l.setPairsMergedPerHypothesis(0);
		//l.setGeneralisationThreshold(1);
		//l.setCertaintyThreshold(5);
		l.addObserver(Visualiser.getVisualiser());
		l.getScoreComputer().setMode(IDMode.POSITIVE_NEGATIVE);
		l.init(buildSet(plus), buildSet(minus));
		DirectedSparseGraph learningOutcomeA = l.learnMachine();
		//updateFrame(learningOutcome,g);
		FSMStructure learntStructureA = WMethod.getGraphData(learningOutcomeA);

		// Now do the same with ptasets instead of real sets
		PTASequenceSet plusPTA = new PTASequenceSet();plusPTA.addAll(buildSet(plus));PTASequenceSet minusPTA = new PTASequenceSet();minusPTA.addAll(buildSet(minus));
		l.init(plusPTA, minusPTA);
		DirectedSparseGraph learningOutcomeB = l.learnMachine();
		FSMStructure learntStructureB = WMethod.getGraphData(learningOutcomeB);
		
		TestFSMAlgo.checkMBoolean(learntStructureA, learntStructureB, learntStructureA.init, learntStructureB.init);
		//TestFSMAlgo.checkM(learntStructure,completedGraph,learntStructure.init,expected.init);
	}
	
	@Test
	public void testLearner1()
	{
		checkLearner("A-a->B<-a-A\nA-b->A",
				new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b"}}, 
				new String[][]{new String[]{"a","b"},new String[]{"a","a"}});
	}
	
	@Test
	public void testLearner2()
	{
		checkLearner("A-a->B<-a-C-b->A\nA-b->C\nC-c->C\n",new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b","c"}}, new String[][]{new String[]{"c"}});
	}

	@Test
	public void testLearner2b()
	{
		checkLearner("A-a->B<-a-C-b->A\nA-b->C\nC-c->C\n",new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b","c"}}, new String[][]{new String[]{"c"},new String[]{"b","b","c"}	});
	}

	@Test
	public void testLearner3()
	{
		checkLearner("A-text->B-text->B\nA-figure->C-figure->C\nB-figure->C\nC-text->B\nB-set_position->F\nF-edit->G\nG-finalize->A\nC-set_position->D\nD-set_dimensions->E-set_dimensions->E-figure->C\nE-text->B",
				new String[][]{new String[]{"figure", "figure","set_position","set_dimensions","set_dimensions","set_dimensions","set_dimensions", "figure", "set_position", "set_dimensions"}, new String[]{"figure", "figure","set_position","set_dimensions","set_dimensions","set_dimensions","text", "set_position", "edit"}, new String[]{"text","text","set_position","edit","finalize","text"}, new String[]{"text","text","set_position","edit","finalize","figure"}}, new String[][]{});
		
	}
			
	DeterministicVertex p = new DeterministicVertex("P"), q= new DeterministicVertex("Q");
	/** Checks that both the old and the new algorithm reports a pair of states as incompatible. */
	public final void testNewLearnerIncompatible(String fsm, String name)
	{
		DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsm, name);
		LearnerGraph s = new LearnerGraph(g);
		OrigStatePair pairOrig = new OrigStatePair(findVertex(JUConstants.LABEL, "B", g),findVertex(JUConstants.LABEL, "A", g));
		StatePair pairNew = new StatePair(findVertexNamed("B", g),findVertexNamed("A", g));
		doneEdges = new HashSet();
		int origScore = computeScore(g, pairOrig),
			newScoreA = s.pairscores.computeStateScore(pairNew);
		Assert.assertEquals(-1, origScore);
		Assert.assertEquals(-1, newScoreA);
	}
	
	/** Checks that both the old and the new algorithm report the same score for a pair of states and asks the same questions. */
	public final void testNewLearnerQuestions(String fsm, int expectedScore, String learnerName)
	{
		DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsm, learnerName);
		LearnerGraph 	s = new LearnerGraph(g);
		OrigStatePair pairOrig = new OrigStatePair(findVertex(JUConstants.LABEL, "B", g),findVertex(JUConstants.LABEL, "A", g));
		StatePair pairNew = new StatePair(findVertexNamed("B", g),findVertexNamed("A", g));
		DirectedSparseGraph temp = mergeAndDeterminize((Graph)g.copy(), pairOrig),
			tempB = MergeStates.mergeAndDeterminize(g, pairNew);
		
		//setDebugMode(true);updateFrame(g, ComputeStateScores.mergeAndDeterminize(new ComputeStateScores(g),pair).getGraph());

		// Now check that ComputeStateScores properly does  mergeAndDeterminize 
		// (on the test data we are dealing with in these tests, there are separate tests for mergeAndDeterminize)
		FSMStructure tempG = WMethod.getGraphData(temp), tempBG = WMethod.getGraphData(tempB);
		Assert.assertEquals(false, WMethod.checkUnreachableStates(tempG));Assert.assertEquals(false, WMethod.checkUnreachableStates(tempBG));
		Assert.assertEquals(true, TestFSMAlgo.checkMBoolean(tempG, tempBG, tempG.init, tempBG.init));
		
		doneEdges = new HashSet();
		int origScore = computeScore(g, pairOrig),
			newScoreA = s.pairscores.computeStateScore(pairNew),
			newScoreB = s.pairscores.computePairCompatibilityScore(pairNew);
		//Visualiser.updateFrame(s.paths.getGraph(),MergeStates.mergeAndDeterminize(new ComputeStateScores(g), pairNew).paths.getGraph());
		//Visualiser.waitForKey();
		Collection<List<String>> 
			// Since computeQS assumes that red names remain unchanged in the merged version, I have to use a specific merging procedure
			questionsB = ComputeQuestions.computeQS(pairNew, s,MergeStates.mergeAndDeterminize(new LearnerGraph(g), pairNew));
				
		Assert.assertTrue("these states should be compatible - correct test data",origScore >= 0);
		//System.out.println("computed scores, orig: "+origScore+" and the new one is "+newScoreA);
		Assert.assertEquals(expectedScore, origScore);
		Assert.assertEquals(expectedScore, newScoreA);
		Assert.assertTrue( expectedScore < 0? (newScoreB < 0):(newScoreB >= 0));
		if (expectedScore != -1)
		{
			Set<List<String>> oldQuestions = new HashSet<List<String>>();oldQuestions.addAll(generateQuestions(g,temp, pairOrig));
			//Assert.assertTrue(oldQuestions.size() > 0);
			Set<List<String>> newQuestionsB = new HashSet<List<String>>();newQuestionsB.addAll(questionsB);
			Assert.assertTrue("different questions: old "+oldQuestions+", new "+questionsB,oldQuestions.equals(newQuestionsB));
		}
	}
	
	public static final String PTA1 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2\nB1-b->B3-b->B4\n";
	
	@Test(expected = IllegalArgumentException.class)
	public final void testLearnerFailsWhenRedNotFound()
	{
		ComputeQuestions.computeQS(new StatePair(null,new DeterministicVertex("non-existing")), new LearnerGraph(), new LearnerGraph());
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
	
	private static Vertex getTempRed_DijkstraShortestPath(DirectedSparseGraph model, Vertex r, DirectedSparseGraph temp){
		DijkstraShortestPath p = new DijkstraShortestPath(model);
		List<Edge> pathToRed = p.getPath(RPNIBlueFringeLearner.findInitial(model), r);
		Vertex tempRed = null;
		if(!pathToRed.isEmpty()){
			List<String> pathToRedString = new LinkedList<String>();
			for(Edge e:pathToRed)
				pathToRedString.add( ((Collection<String>)e.getUserDatum(JUConstants.LABEL)).iterator().next() );
			tempRed = RPNIBlueFringeLearner.getVertex(temp, pathToRedString);
		}
		else
			tempRed = RPNIBlueFringeLearner.findInitial(temp);
		return tempRed;
	}
		
	@Test
	public final void testGetTempRed1()
	{
		DirectedSparseGraph a=TestFSMAlgo.buildGraph("A-a->B", "testGetTempRed1 model"),
			temp=TestFSMAlgo.buildGraph("C-d->Q", "testGetTempRed1 temp");
		Vertex foundA = getTempRed_DijkstraShortestPath(a, findInitial(a), temp);
		Vertex foundB =RPNIBlueFringeLearnerTestComponent.getTempRed(a, findInitial(a), temp);
		Assert.assertTrue(findInitial(temp).equals(foundA));
		Assert.assertTrue(findInitial(temp).equals(foundB));
	}
	
	@Test
	public final void testGetTempRed2()
	{
		DirectedSparseGraph a=TestFSMAlgo.buildGraph("A-a->B-a->B-c->C-c->D", "testGetTempRed1 model"),
			temp=TestFSMAlgo.buildGraph("C-a->Q-a->Q-c->Q", "testGetTempRed1 temp");
		Vertex foundA = getTempRed_DijkstraShortestPath(a, findVertex(JUConstants.LABEL, "D", a), temp);
		Vertex foundB = RPNIBlueFringeLearnerTestComponent.getTempRed(a, findVertex(JUConstants.LABEL, "D", a), temp);
		Assert.assertTrue(findVertex(JUConstants.LABEL, "Q", temp).equals(foundA));
		Assert.assertTrue(findVertex(JUConstants.LABEL, "Q", temp).equals(foundB));
	}
	
	@Test
	public final void testCopyGraph0()
	{
		DirectedSparseGraph g=new DirectedSparseGraph();
		g.addVertex(new DirectedSparseVertex());
		g.addVertex(new DirectedSparseVertex());
		DirectedSparseGraph copy = RPNIBlueFringeLearner.copy(g);
		Assert.assertTrue(copy.getEdges().isEmpty() && copy.getVertices().isEmpty());
	}
	
	// TODO to test FSMStructure's equals
	@Test
	public final void testCopyGraph1()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-a->S1", "testCopyGraph");
		DirectedSparseGraph copy=RPNIBlueFringeLearner.copy(g);
		FSMStructure gS = WMethod.getGraphData(g),gC = WMethod.getGraphData(copy);
		
		Assert.assertTrue(gS.equals(gC));
	}
	
	@Test
	public final void testCopyGraph2()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+PTA3, "testCopyGraph");
		DirectedSparseGraph copy=RPNIBlueFringeLearner.copy(g);
		FSMStructure gS = WMethod.getGraphData(g),gCopy = WMethod.getGraphData(copy);
		
		Assert.assertTrue(gS.equals(gCopy));
		
		// now test if all clones are faithful
		for(Edge e:(Set<Edge>)g.getEdges())
			((Set<String>)e.getUserDatum(JUConstants.LABEL)).add("junk");
		
		FSMStructure gS_Modified = WMethod.getGraphData(copy);
		
		Assert.assertTrue(gS_Modified.equals(gCopy));
	}
	
	@Test
	public final void testCopyGraph3() // this one tests that clone works
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+PTA3, "testCopyGraph");
		LearnerGraph orig = new LearnerGraph(g);
		LearnerGraph copy;
		copy = (LearnerGraph)orig.clone();
		FSMStructure gS = WMethod.getGraphData(orig.paths.getGraph()),gCopy = WMethod.getGraphData(copy.paths.getGraph());
		
		Assert.assertTrue(gS.equals(gCopy));
		
		// now test if all clones are faithful by clearing the first graph.
		orig.initPTA();
		
		FSMStructure gS_afterChange = WMethod.getGraphData(orig.paths.getGraph()), gCopy_afterChange = WMethod.getGraphData(copy.paths.getGraph());
		
		Assert.assertTrue(gCopy_afterChange.equals(gCopy));
		Assert.assertTrue(gCopy_afterChange.equals(gS));
		Assert.assertFalse(gS_afterChange.equals(gCopy));
		Assert.assertFalse(gS_afterChange.equals(gS));		
	}
	
	@Test
	public final void findMergablePair1()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("A-a->B\nA-b->B\nA-c->C\nA-d->D", "findMergablePair1");
		Assert.assertNull(RPNIBlueFringeLearner.findMergablePair(g));
	}
	
	@Test
	public final void findMergablePair2()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("A-a->B\nA-b->B\nA-c->D\nA-b->D\nA-d->E", "findMergablePair2");
		Vertex 
			b = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g),
			d = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "D", g);
		Set<Vertex> expected = new HashSet<Vertex>();expected.add(d);expected.add(b);
		Set<Vertex> actualA = new HashSet<Vertex>();
		OrigStatePair value = RPNIBlueFringeLearner.findMergablePair(g);actualA.add(value.getQ());actualA.add(value.getR());
		Assert.assertTrue("expected: B, D in either order got: "+actualA,expected.equals(actualA));
	}
	
	@Test
	public final void findMergablePair3a()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-p->A-a->S\nA-b->S\nA-c->D\nA-b->D\nA-d->E", "findMergablePair3a");
		Vertex 
			s = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "S", g),
			d = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "D", g);
		OrigStatePair expected = new OrigStatePair(d,s),
		actualA = RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertTrue("expected: "+expected+" got: "+actualA,expected.equals(actualA));
	}

	@Test
	public final void findMergablePair3b()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-p->A-a->B\nA-b->B\nA-c->S\nA-b->S\nA-d->E", "findMergablePair3b");
		Vertex 
			b = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g),
			s = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "S", g);
		OrigStatePair expected = new OrigStatePair(b,s),
		actualA = RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertTrue("expected: "+expected+" got: "+actualA,expected.equals(actualA));
	}

	@Test
	public final void findMergablePair4()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-p->A-a->B\nA-b->B\nA-c-#D\nA-b-#D\nA-d->E", "findMergablePair4");
		OrigStatePair actualA = RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertNull(actualA);
	}

	/** Tests merging of states <em>stateRed</em> and <em>stateBlue</em> of <em>machineToMerge</em>.
	 * The outcome of merging has to be equivalent to <em>expectedFSM</em>. 
	 * 
	 * @param machineToMerge machine to merge
	 * @param expectedFSM the expected result
	 * @param stateBlue the name of the second state to merge.
	 * @param stateRed the name of the first state to merge
	 * @param checkWithEquals whether the equivalence between the result of merging is to be assessed by 
	 * running a language equivalence query or by doing a .equals on FSMStructures corresponding to them. This will usually
	 * be false. 
	 */
	public void checkCorrectnessOfMerging(String machineToMerge, String expectedFSM, String stateBlue, String stateRed, boolean checkWithEquals)
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph(machineToMerge, "Machine to merge"),
			g2=(DirectedSparseGraph)g.copy();
		Vertex 
			a = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, stateRed, g),
			b = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, stateBlue, g);
		
		Assert.assertNotNull("state "+stateRed+" was not found", a);
		Assert.assertNotNull("state "+stateBlue+" was not found", b);
		
		OrigStatePair pairOrig = new OrigStatePair(b,a);
		StatePair pairNew = new StatePair(RPNIBlueFringeLearner.findVertexNamed(stateBlue, g),RPNIBlueFringeLearner.findVertexNamed(stateRed, g));
		FSMStructure 
			mergeResultA = WMethod.getGraphData(new RPNIBlueFringeLearner(null).mergeAndDeterminize(g, pairOrig)), 
			mergeResultB = WMethod.getGraphData(MergeStates.mergeAndDeterminize(g2, pairNew)),
			mergeResultC = WMethod.getGraphData(MergeStates.mergeAndDeterminize(new LearnerGraph(g), pairNew).paths.getGraph()),
			expectedMachine = WMethod.getGraphData(TestFSMAlgo.buildGraph(expectedFSM, "expected machine"));

		TestFSMAlgo.checkM(g2, machineToMerge);
		
		//Visualiser.updateFrame(g, MergeStates.mergeAndDeterminize(new ComputeStateScores(g), pairNew).paths.getGraph());
		//Visualiser.waitForKey();
		Assert.assertFalse("unreachable states - original",WMethod.checkUnreachableStates(mergeResultA));
		Assert.assertFalse("unreachable states",WMethod.checkUnreachableStates(mergeResultB));
		Assert.assertFalse("unreachable states",WMethod.checkUnreachableStates(mergeResultC));
		Assert.assertFalse("unreachable states",WMethod.checkUnreachableStates(expectedMachine));
		
		if (checkWithEquals)
		{
			Assert.assertTrue("incorrect merging - original",expectedMachine.equals(mergeResultA));
			Assert.assertTrue("incorrect merging - first improved",expectedMachine.equals(mergeResultB));
			Assert.assertTrue("incorrect merging - second improved",expectedMachine.equals(mergeResultC));
		}
		else
		{
			Assert.assertTrue("incorrect merging - original",TestFSMAlgo.checkMBoolean(mergeResultA, expectedMachine, mergeResultA.init, expectedMachine.init));
			Assert.assertTrue("incorrect merging - first improved",TestFSMAlgo.checkMBoolean(mergeResultB, expectedMachine, mergeResultB.init, expectedMachine.init));
			Assert.assertTrue("incorrect merging - second improved",TestFSMAlgo.checkMBoolean(mergeResultC, expectedMachine, mergeResultC.init, expectedMachine.init));
		}
	}

	@Test
	public final void testMerge1a()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-p->A-a->S\nA-b->S\nA-c->D\nA-b->D\nA-d->E\nS-n->U", "testMerge");
		Vertex 
			s = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "S", g),
			d = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "U", g);
		OrigStatePair pair = new OrigStatePair(d,s);
		FSMStructure 
			mergeResultA = WMethod.getGraphData(new RPNIBlueFringeLearner(null).mergeAndDeterminize(g, pair)),
			expectedResult = WMethod.getGraphData(TestFSMAlgo.buildGraph("S-p->A-a->S\nA-b->S\nA-c->S\nA-d->E\nS-n->S", "expected"));
		Assert.assertTrue(expectedResult.equals(mergeResultA));
	}
	
	@Test
	public final void testMerge1b()
	{
		checkCorrectnessOfMerging("S-p->A-a->S\nA-b->S\nA-c->D\nU-b->E\nA-d->D\nA-n->U",
				"S-p->A-a->S\nA-b->S\nA-c->D\nA-d->D\nA-n->A",
				"U","A",
				true);
	}
	
	@Test
	public final void testMerge2()
	{
		checkCorrectnessOfMerging("S-p->A-a->S\nA-b->S\nA-c->D\nA-e->D\nA-d->E\nS-n->U",
				"S-p->A-a->S\nA-b->S\nA-c->S\nA-e->S\nA-d->E\nS-n->U",
				"D","S",
				true);
	}

	@Test
	public final void testMerge3()
	{
		checkCorrectnessOfMerging(
				"P-a->P1-b->P-b->P2-c->S-p->A-a->S\nA-b->S\nA-c->D\nA-e->D\nA-d->E\nS-n->U",
				"P-a->P1-b->P-b->P2-c->S-p->A-a->S\nA-b->S\nA-c->S\nA-e->S\nA-d->E\nS-n->U",
				"D","S",
				true);
	}
	
	@Test
	public final void testMerge4()
	{
		checkCorrectnessOfMerging(
				"S-p->S1-b->S1-a->S2\nS1-c->A-a->S1\nA-f->S2\nA-b->A1-b->A2\nA-d->B-c->B7\nB-b->B1-b->B2-d->B3\nB-a->B4-a->B5-e->B6",
				"S-p->S1-b->S1-a->S2\nS1-c->A-a->S1\nA-f->S2\nA-b->A1-b->A2\nA-d->A-c->B7\nS2-e->S3\nA2-d->A3",
				"B","A",
				false);
	}

	@Test
	public final void testMerge5()
	{
		checkCorrectnessOfMerging(
				"S-p->A-a->B-a->B1-a->B2-d->B3\nA-b->A1-b->A2",
				"S-p->A-a->A-d->A2\nA-b->A1-b->A2",
				"B","A",
				false);
	}

	@Test
	public final void testMerge6()
	{
		checkCorrectnessOfMerging(
				"S-p->A-a->A3-b->B-a->B1-c->B2\nA-b->A1-b->A2",
				"S-p->A-a->A3-c->A4\nA3-b->A\nA-b->A1-b->A2",
				"B","A",
				false);
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
			false);
	}

	@Test
	public final void testMerge8()
	{
		checkCorrectnessOfMerging(largeGraph2,
			"S-a->A-a->S\n"+
			"A-d->A-c->A-b->BL4-c->BL5",
			
			"B","A",
			false);
	}

	@Test
	public final void testMerge9()
	{
		checkCorrectnessOfMerging(largeGraph3,
			"S-a->A-a->S\n"+
			"A-d->A-c->A-b->BL4-c->BL5\n"+
			"A-f->B1",
			
			"B","A",
			false);
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
			false);
	}

	@Test
	public final void testMerge11()
	{
		checkCorrectnessOfMerging(largeGraph5,
			"S-n->A-n->An-n->A\n"+
			"A-a->A1-c->A2\nA-b->A1-d->A2-p->A3\n",
			
			"B","A",
			false);
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
			false);
	}

	
	@Test(expected = IllegalArgumentException.class)
	public final void testMerge_fail1()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph(largeGraph1_invalid5,"testMerge10");
		CmpVertex 
			a = RPNIBlueFringeLearner.findVertexNamed("A", g),
			b = RPNIBlueFringeLearner.findVertexNamed("B", g);
		StatePair pair = new StatePair(b,a);// A is red
		MergeStates.mergeAndDeterminize(g, pair);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testMerge_fail2()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph(largeGraph1_invalid5,"testMerge10");
		CmpVertex 
			a = RPNIBlueFringeLearner.findVertexNamed("A", g),
			b = RPNIBlueFringeLearner.findVertexNamed("B", g);
		StatePair pair = new StatePair(b,a);// A is red
		MergeStates.mergeAndDeterminize(new LearnerGraph(g), pair);
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
	 */
	public final void testChooseStatePairs(String fsm, String [] initialReds, String [][] expectedReds, List<PairScore> expectedPairs)
	{
		final DirectedSparseGraph gB = TestFSMAlgo.buildGraph(fsm, "testChooseStatePairs_Ref");
		// check how the reference pair selection function performs
		testChooseStatePairsInternal(gB, initialReds, expectedReds, expectedPairs, new InterfaceChooserToTest() {
			public Stack<StatePair> choosePairs() {// Here I need to convert the old type of pairs to the new one.
				Stack<OrigStatePair> pairs = chooseStatePairs(gB, new HashSet<List<String>>(), new HashSet<List<String>>());
				Stack<StatePair> result = new Stack<StatePair>();
				for(OrigStatePair pair:pairs) result.add(new StatePair((CmpVertex)pair.getQ(),(CmpVertex)pair.getR()));
				return result;
			}
		});

		final DirectedSparseGraph gA = TestFSMAlgo.buildGraph(fsm, "testChooseStatePairs_Opt");

		// check how the revised pair selection function performs
		final LearnerGraph s = new LearnerGraph(gA);
		testChooseStatePairsInternal(gA, initialReds, expectedReds, expectedPairs, new InterfaceChooserToTest() {
			public Stack<? extends StatePair> choosePairs() {
				return s.pairscores.chooseStatePairs();
			}
		});
	}
	
	private final void testChooseStatePairsInternal(DirectedSparseGraph g, String [] initialReds, String [][] expectedReds, List<PairScore> expectedPairs,InterfaceChooserToTest chooser)
	{
		for(String red:initialReds)
		{
			Vertex v = findVertex(JUConstants.LABEL, red, g);v.removeUserDatum(JUConstants.COLOUR);v.addUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
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
		for(Vertex red:findVertices(JUConstants.COLOUR, JUConstants.RED, g))
				finalReds.add((String)red.getUserDatum(JUConstants.LABEL));
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
			doneEdges = new HashSet();
			DeterministicVertex q = RPNIBlueFringeLearner.findVertexNamed(elem.getQ().getName(), g);
			DeterministicVertex r = RPNIBlueFringeLearner.findVertexNamed(elem.getR().getName(), g);
			int currentScore = computeScore(g, new OrigStatePair(q,r));
			PairScore elA = new PairScore(elem.getQ(),elem.getR(),currentScore, currentScore);
			PairScore elB = new PairScore(elem.getR(),elem.getQ(),currentScore, currentScore);
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
		pairsAndScores.add(constructPairScore("A2", "A", 0));
		pairsAndScores.add(constructPairScore("U1", "A", 2));
		pairsAndScores.add(constructPairScore("S1", "A", 3));
		pairsAndScores.add(constructPairScore("R1", "A", 2));
		pairsAndScores.add(constructPairScore("Q1", "A", 1));
		pairsAndScores.add(constructPairScore("P2", "A", 0));
		for(String state:new String[]{"A2","U1","S1","R1","Q1","P2"})
			pairsAndScores.add(constructPairScore(state, "P1", 0));

		testChooseStatePairs(
				"A-a-#Arej\nA-d->A2-c->A3-c->A4-c->A5\n"+
				"A-p->P1-a->P2\n"+
				"A-s->S1-d->S2-c->S3-c->S4\n"+
				"A-r->R1-d->R2-c->R3\n"+
				"A-u->U1-d->U2-c->U3\n"+
				"A-q->Q1-d->Q2",
				new String[]{"A"},
				new String[][] {new String[]{"A","P1","Arej"}},
				pairsAndScores);
	}
	
	@Test
	public final void testNewchooseStatePairs2()
	{
		List<PairScore> pairsAndScores = new LinkedList<PairScore>();
		pairsAndScores.add(constructPairScore("A2", "A", 0));
		pairsAndScores.add(constructPairScore("U1", "A", 2));
		pairsAndScores.add(constructPairScore("S1", "A", 3));
		pairsAndScores.add(constructPairScore("R1", "A", 2));
		pairsAndScores.add(constructPairScore("Q1", "A", 1));
		pairsAndScores.add(constructPairScore("P2", "A", 0));
		for(String state:new String[]{"A2","U1","S1","R1","Q1","P2"})
			pairsAndScores.add(constructPairScore(state, "P1", 0));
		pairsAndScores.add(constructPairScore("P3", "Arej", 0));

		testChooseStatePairs(
				"A-a-#Arej\nA-d->A2-c->A3-c->A4-c->A5\n"+
				"A-p->P1-a->P2\n"+"P1-b-#P3\n"+
				"A-s->S1-d->S2-c->S3-c->S4\n"+
				"A-r->R1-d->R2-c->R3\n"+
				"A-u->U1-d->U2-c->U3\n"+
				"A-q->Q1-d->Q2",
				new String[]{"A"},
				new String[][] {new String[]{"A","P1","Arej"},new String[]{"A","P1","P3"}},
				pairsAndScores);
	}
	
	@Test
	public final void testNewchooseStatePairs3()
	{
		List<PairScore> pairsAndScores = new LinkedList<PairScore>();
		pairsAndScores.add(constructPairScore("REJ1", "REJ2", 0));
		pairsAndScores.add(constructPairScore("A1", "A", 3));

		testChooseStatePairs(
				"A-a->B1-a-#REJ1\nB1-b-#REJ2\n"+
				"A-b->A1-a->B2\n"+
				"A1-a->B2\n"+
				"A1-b->A2-a->B3\n",
				new String[]{"A"},
				new String[][] {new String[]{"A","B1","REJ2"},new String[]{"A","B1","REJ1"}},
				pairsAndScores);
	}

	/** Checks that scores are correctly computed on complex graphs, taking the ability of pairCompatibilityScore
	 * to verify compatibility.
	 * 
	 * @param fsm the graph to operate
	 * @param expectedComputeScore the expected score
	 * @param pairCompatibility the expected pair compatibility score
	 */
	private void testScoreAndCompatibilityComputation(String fsm, int expectedComputedScore, int pairCompatibility)
	{
		DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsm, "testPairCompatible");
		OrigStatePair pairOrig = new OrigStatePair(
				RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g),
				RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "A", g));
		StatePair pairNew = new StatePair(
				RPNIBlueFringeLearner.findVertexNamed("B", g),
				RPNIBlueFringeLearner.findVertexNamed("A", g));
		LearnerGraph s = new LearnerGraph(g);

		doneEdges = new HashSet();
		int origScore = computeScore(g, pairOrig),
			newScoreA = s.pairscores.computeStateScore(pairNew),
			newScoreB = s.pairscores.computePairCompatibilityScore(pairNew);
		assertEquals(expectedComputedScore, origScore); 
		assertEquals(expectedComputedScore, newScoreA); 
		assertEquals(pairCompatibility,newScoreB); 
	}
	
	@Test
	public final void testPairCompatible1()
	{
		testScoreAndCompatibilityComputation(
				"A-a->B-a->C-b->D\n"+
				"A-b->E",
				1,0);
	}

	@Test
	public final void testPairCompatible2()
	{
		testScoreAndCompatibilityComputation(
				"A-a->B-a->C-b->D\n"+
				"A-b-#E",
				1,-1);
	}

	@Test
	public final void testPairCompatible3()
	{
		testScoreAndCompatibilityComputation(largeGraph1_invalid1,11,-1);
	}

	@Test
	public final void testPairCompatible4()
	{
		testScoreAndCompatibilityComputation(largeGraph1_invalid2,11,-1);
	}

	@Test
	public final void testPairCompatible5()
	{
		testScoreAndCompatibilityComputation(largeGraph1_invalid3,-1,-1);
	}

	@Test
	public final void testPairCompatible6()
	{
		testScoreAndCompatibilityComputation(largeGraph1_invalid4,-1,-1);
	}

	@Test
	public final void testPairCompatible7()
	{
		testScoreAndCompatibilityComputation(largeGraph1_invalid5,11,-1);
	}

	@Test
	public final void testPairCompatible2_1()
	{
		testScoreAndCompatibilityComputation(largeGraph2,5,2);
	}

	@Test
	public final void testPairCompatible2_2()
	{
		testScoreAndCompatibilityComputation(largeGraph3,5,2);
	}

	@Test
	public final void testPairCompatible2_3()
	{
		testScoreAndCompatibilityComputation(largeGraph2_invalid1,5,-1);
	}

	@Test
	public final void testPairCompatible2_4()
	{
		testScoreAndCompatibilityComputation(largeGraph4_invalid1,5,-1);
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{
		LearnerGraph.testMode = true;
		Visualiser.disposeFrame();
	}
	
	@AfterClass
	public static void cleanUp()
	{
		Visualiser.disposeFrame();
	}
}
