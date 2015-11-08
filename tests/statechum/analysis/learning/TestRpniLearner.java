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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.Stack;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.ParameterizedWithName;

import statechum.Configuration;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.Configuration.QuestionGeneratorKind;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.FsmParser;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.TestEquivalenceChecking;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.model.testset.PTASequenceSet;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;
import static statechum.analysis.learning.rpnicore.TestGraphBasicAlgorithms.constructPairScore;

@RunWith(ParameterizedWithName.class)
public class TestRpniLearner extends Test_Orig_RPNIBlueFringeLearnerTestComponent
{
	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		return Configuration.configurationsForTesting();
	}
	
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
	
	public TestRpniLearner(Configuration conf) 
	{
		super(null,conf, conf.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?new Transform.InternStringLabel():null);
		mainConfiguration = conf;
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
		final LearnerGraph expected = FsmParser.buildLearnerGraph(fsmString, name,testConfig,getLabelConverter());
		
		// now sanity checking on the plus and minus sets
		for(String [] path:plus)
			assert AbstractOracle.USER_ACCEPTED == expected.paths.tracePathPrefixClosed(AbstractLearnerGraph.buildList(Arrays.asList(path),config,getLabelConverter()));
		for(String [] path:minus)
			assert AbstractOracle.USER_ACCEPTED != expected.paths.tracePathPrefixClosed(AbstractLearnerGraph.buildList(Arrays.asList(path),config,getLabelConverter()));
		// Visualiser.getVisualiser()
		Learner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,testConfig,null,null))
		{
			@Override
			public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
			{
				// Check that compatibility score computation gives the same response as if we did merge and computed a difference between the number of states.
				ScoreMode origScore = original.config.getLearnerScoreMode();original.config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
				long compatibilityScore = original.pairscores.computePairCompatibilityScore(pair);
				original.config.setLearnerScoreMode(origScore);
				LearnerGraph outcome = super.MergeAndDeterminize(original, pair);
				Assert.assertEquals(compatibilityScore+1,original.getStateNumber()-outcome.getStateNumber());
				return outcome;
			}

			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					List<Label> question, @SuppressWarnings("unused") int valueForNoRestart,
					@SuppressWarnings("unused") List<Boolean> acceptedElements,
					@SuppressWarnings("unused") PairScore pairBeingMerged,
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				Pair<Integer,String> oracleAnswer = new Pair<Integer,String>(expected.paths.tracePathPrefixClosed(question),null);
				return oracleAnswer;				
			}
			
		};
		config.setDebugMode(false);
		//l.setPairsMergedPerHypothesis(0);
		//l.setGeneralisationThreshold(1);
		//l.setCertaintyThreshold(5);
		testConfig.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);
		LearnerGraph learntStructureA = new LearnerGraph(l.learnMachine(buildSet(plus,testConfig,getLabelConverter()), buildSet(minus,testConfig,getLabelConverter())),expected.config);
		// Now do the same with ptasets instead of real sets
		PTASequenceSet plusPTA = new PTASequenceSet();plusPTA.addAll(buildSet(plus,testConfig,getLabelConverter()));PTASequenceSet minusPTA = new PTASequenceSet();minusPTA.addAll(buildSet(minus,testConfig,getLabelConverter()));
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
		checkLearner("A-a->B\nA-b->A","testLearner1",
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
		LearnerGraph s = FsmParser.buildLearnerGraph(fsm, name,testConfig,getLabelConverter());
		DirectedSparseGraph g = s.pathroutines.getGraph();
		OrigStatePair pairOrig = new OrigStatePair(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("B"), g),
				DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("A"), g));
		StatePair pairNew = new StatePair(s.findVertex(VertexID.parseID("B")),s.findVertex(VertexID.parseID("A")));
		doneEdges = new HashSet<DirectedSparseEdge>();
		long origScore = computeScore(g, pairOrig),
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
		LearnerGraph s = FsmParser.buildLearnerGraph(fsm, learnerName,testConfig,getLabelConverter());
		DirectedSparseGraph g = s.pathroutines.getGraph();
		OrigStatePair pairOrig = 
			new OrigStatePair(
					DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("B"), g),
					DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("A"), g));
		StatePair pairNew1 = new StatePair(s.findVertex(VertexID.parseID("B")),s.findVertex(VertexID.parseID("A")));
		DirectedSparseGraph 
			temp = mergeAndDeterminize((Graph)g.copy(), pairOrig),
			tempB = MergeStates.mergeAndDeterminize(g, pairNew1,testConfig);
		
		// Now check that ComputeStateScores properly does  mergeAndDeterminize 
		// (on the test data we are dealing with in these tests, there are separate tests for mergeAndDeterminize)
		LearnerGraph tempG = new LearnerGraph(temp,testConfig), tempBG = new LearnerGraph(tempB,testConfig);
		Assert.assertEquals(false, tempG.pathroutines.checkUnreachableStates());Assert.assertEquals(false, tempBG.pathroutines.checkUnreachableStates());
		Assert.assertNull(WMethod.checkM(tempG, tempBG));
		
		
		doneEdges = new HashSet<DirectedSparseEdge>();
		long origScore = computeScore(g, pairOrig),
			newScoreA = s.pairscores.computeStateScore(pairNew1),
			newScoreB = s.pairscores.computePairCompatibilityScore(pairNew1),
			newScoreC = s.pairscores.computePairCompatibilityScore_general(pairNew1,null, new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>());

		LearnerGraph learner2 = new LearnerGraph(g, testConfig);
		StatePair pairNew2 = new StatePair(learner2.findVertex(VertexID.parseID("B")),learner2.findVertex(VertexID.parseID("A")));
		//Visualiser.updateFrame(g, MergeStates.mergeAndDeterminize_general(learner2, pairNew2).pathroutines.getGraph(learnerName));Visualiser.waitForKey();
		Collection<List<Label>> 
			// Since computeQS assumes that red names remain unchanged in the merged version, I have to use a specific merging procedure
			questionsB = ComputeQuestions.computeQS_orig(pairNew2, learner2,MergeStates.mergeAndDeterminize(learner2, pairNew2)),
			questionsC = ComputeQuestions.computeQS_orig(pairNew2, learner2,MergeStates.mergeAndDeterminize_general(learner2, pairNew2)),
			questionsD = ComputeQuestions.computeQS_general(pairNew2, learner2, MergeStates.mergeAndDeterminize_general(learner2, pairNew2), 
					new ComputeQuestions.QuestionGeneratorQSMLikeWithLoops()).getData();
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
	
	@Test
	public final void testQSMvsLoopsQuestionGenerator1()
	{
		LearnerGraph s = FsmParser.buildLearnerGraph("A-a->A3-b->R-r->T / A-c->A3 / A -b->A2-c->R / A-d->A4-a->A6-b->R / A-e->A5-a->B-q->C","testQSMvsLoopsQuestionGenerator1",testConfig,getLabelConverter());
		for(CmpVertex v:s.transitionMatrix.keySet())
			if (v.getStringId().equals("A5") || v.getStringId().equals("B") || v.getStringId().equals("C"))
				v.setColour(JUConstants.BLUE);
			else
				if (v.getStringId().equals("T"))
					v.setColour(null);
				else
					v.setColour(JUConstants.RED);
		CmpVertex R = s.findVertex(VertexID.parseID("R")),B=s.findVertex(VertexID.parseID("B"));
		
		StatePair pair = new StatePair(B,R);
		LearnerGraph merged = MergeStates.mergeAndDeterminize_general(s,pair);
		Assert.assertEquals(9, merged.getAcceptStateNumber());
		for(CmpVertex v:merged.transitionMatrix.keySet())
			if (v.getStringId().equals("A5") || v.getStringId().equals("C"))
				Assert.assertEquals(JUConstants.BLUE,v.getColour());
			else
				if (v.getStringId().equals("T"))
					Assert.assertNull(v.getColour());
				else
					Assert.assertEquals(JUConstants.RED,v.getColour());
		
		// now check questions
		List<List<Label>> questionsCompatible = ComputeQuestions.computeQS_general(pair, s, merged, new ComputeQuestions.QuestionGeneratorQSMLikeWithLoops()).getData();
		Assert.assertEquals("[[b, c, q], [a, b, q], [c, b, q], [e, a, r]]",questionsCompatible.toString());// this one takes account of the extra path ea into the merged vertex during question generation.
		List<List<Label>> questionsQSM = ComputeQuestions.computeQS_general(pair, s, merged, new ComputeQuestions.QuestionGeneratorQSM()).getData();
		Assert.assertEquals("[[b, c, q], [a, b, q], [c, b, q]]",questionsQSM.toString());
	}
	
	@Test
	public final void testQSMvsLoopsQuestionGenerator2()
	{
		LearnerGraph s = FsmParser.buildLearnerGraph("A-a->A3-b->R-r->T / A-c->A3 / A -b->A2-c->R / A-d->A4-a->A6-b->R / A-e->A5-a->B-q->C / R-w->B","testQSMvsLoopsQuestionGenerator2",testConfig,getLabelConverter());
		for(CmpVertex v:s.transitionMatrix.keySet())
			if (v.getStringId().equals("A5") || v.getStringId().equals("B") || v.getStringId().equals("C"))
				v.setColour(JUConstants.BLUE);
			else
				if (v.getStringId().equals("T"))
					v.setColour(null);
				else
					v.setColour(JUConstants.RED);
		CmpVertex R = s.findVertex(VertexID.parseID("R")),B=s.findVertex(VertexID.parseID("B"));
		
		StatePair pair = new StatePair(B,R);
		LearnerGraph merged = MergeStates.mergeAndDeterminize_general(s,pair);
		Assert.assertEquals(9, merged.getAcceptStateNumber());
		for(CmpVertex v:merged.transitionMatrix.keySet())
			if (v.getStringId().equals("A5") || v.getStringId().equals("C"))
				Assert.assertEquals(JUConstants.BLUE,v.getColour());
			else
				if (v.getStringId().equals("T"))
					Assert.assertNull(v.getColour());
				else
					Assert.assertEquals(JUConstants.RED,v.getColour());
		
		// now check questions
		List<List<Label>> questionsCompatible = ComputeQuestions.computeQS_general(pair, s, merged, new ComputeQuestions.QuestionGeneratorQSMLikeWithLoops()).getData();
		Assert.assertEquals("[[b, c, w, r], [b, c, w, w], [b, c, q], [a, b, w, r], [a, b, w, w], [a, b, q], [c, b, w, r], [c, b, w, w], [c, b, q], [e, a, w, q], [e, a, w, r], [e, a, w, w], [e, a, r]]",questionsCompatible.toString());// this one takes account of the extra path ea into the merged vertex during question generation.
		List<List<Label>> questionsQSM = ComputeQuestions.computeQS_general(pair, s, merged, new ComputeQuestions.QuestionGeneratorQSM()).getData();
		Assert.assertEquals("[[b, c, q], [a, b, q], [c, b, q]]",questionsQSM.toString());
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
		testNewLearnerIncompatible("A-a->A1-a-#RJ\nA1-b-#A2"+PTA1,"testNewLearner5");
	}

	@Test
	public final void testNewLearner6()
	{
		testNewLearnerIncompatible("A-a->A1-a-#RJ\nA1-b->A2-b-#A3"+PTA1,"testNewLearner6");
	}
	
	@Test
	public final void testNewLearner7()
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-b->A2-b->A3"+PTA1,4,"testNewLearner7");
	}
	
	public static final String PTA2 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2\nB1-b->B3-b->B4\nB1-c->BB1-c->BB2\n";
	
	@Test
	public final void testNewLearner8()
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-b->A2\nA1-c->A1"+PTA2,5,"testNewLearner8");
	}
	
	@Test
	public final void testNewLearner9()
	{
		testNewLearnerIncompatible("A-a->A1-a-#RJ\nA1-b->A2\nA1-c-#A3"+PTA2,"testNewLearner9");
	}
	
	@Test
	public final void testNewLearner10()
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-b->A2\nA1-c->A3"+PTA2,4,"testNewLearner10");
	}
	
	@Test
	public final void testNewLearner11()
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-b->A1\nA1-c->A3"+PTA2,5,"testNewLearner11");
	}
	
	protected static final String PTA_4 = "\nB1-d->Z1-d->Z2-c->Z3-c->Z4\nZ1-c->Y1-c->Y2-c->Y3\nY4-d->B4d-c->Y5-c->Y6\nB1-c->Y4-c->Y7-c->Y8-c->Y9\n";
	public static final String PTA3 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2"+PTA_4;
	
	@Test
	public final void testNewLearner_2_1()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#RJ\nA1-d->A2\nA1-c->A1"+PTA3,8,"testNewLearner_2_1");
	}
	
	@Test
	public final void testNewLearner_2_2()
	{
		testNewLearnerIncompatible("S-a->S1-b->"+"A-a->A1-a-#RJ\nA1-d->A2\nA1-c-#A3"+PTA3,"testNewLearner_2_2");
	}
	
	@Test
	public final void testNewLearner_2_3()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#RJ\nA1-d->A2\nA1-c->A3"+PTA3,4,"testNewLearner_2_3");
	}
	
	@Test
	public final void testNewLearner_2_4()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#RJ\nA1-d->A1\nA1-c->A3"+PTA3,7,"testNewLearner_2_4");
	}
	
	@Test
	public final void testNewLearner_2_6()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#RJ\nA1-d->A1\nA1-c->A1"+PTA3,16,"testNewLearner_2_6");
	}
	
	@Test
	public final void testNewLearner_2_7()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#RJ\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+PTA3,8,"testNewLearner_2_7");
	}

	@Test
	public final void testNewLearner_3_1()
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-d->A2\nA1-c->A1"+PTA3,8,"testNewLearner_3_1");
	}
	
	@Test
	public final void testNewLearner_3_2()
	{
		testNewLearnerIncompatible("A-a->A1-a-#RJ\nA1-d->A2\nA1-c-#A3"+PTA3,"testNewLearner_3_2");
	}
	
	@Test
	public final void testNewLearner_3_3()
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-d->A2\nA1-c->A3"+PTA3,4,"testNewLearner_3_3");
	}
	
	@Test
	public final void testNewLearner_3_4()
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-d->A1\nA1-c->A3"+PTA3,7,"testNewLearner_3_4");
	}
	
	@Test
	public final void testNewLearner_3_5a()
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-d->A1\nA1-c->A3-c->A3"+PTA3,13,"testNewLearner_3_5a");
	}
	
	@Test
	public final void testNewLearner_3_5b()
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-d->A1\nA1-c->A3-c->A4"+PTA3,10,"testNewLearner_3_5b");
	}
	
	@Test
	public final void testNewLearner_3_6()
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-d->A1\nA1-c->A1"+PTA3,16,"testNewLearner_3_6");
	}
	
	@Test
	public final void testNewLearner_3_7()
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+PTA3,8,"testNewLearner_3_7");
	}

	@Test
	public final void testNewLearner_4_1() // red and blue are adjacent
	{
		testNewLearnerQuestions("A-a->A1-a-#RJ\nA1-d->A2\nA1-c->A1"+"\nA-p->B"+"\nB-a->B1-a-#B2"+PTA_4,8,"testNewLearner_4_1");
	}
	
	@Test
	public final void testNewLearner_4_2() // blue node has no access successors
	{
		testNewLearnerQuestions("A-d->A1\nA1-d->A2\nA1-c->A1"+"\nA-p->At-q->B"+"\nB2#-c-B-a-#B1\n",0,"testNewLearner_4_2");
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
		testNewLearnerQuestions("S-n->A-a->A1-c->A2\nA-b->A1-d->A2\nA-n->An-n->B\nAn-k->A\nA-j->At-j->B\nA-v->P-l->P1\nA-u->P\nAn-m->B\nB-a->B1-c->B3\nB-b->B2-d->B4-p->B5",4,"testNewLearner_4_8b");
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
	
	@SuppressWarnings("unchecked")
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
		DirectedSparseGraph a=FsmParser.buildLearnerGraph("A-a->B", "testGetTempRed1 model",config,getLabelConverter()).pathroutines.getGraph(),
			temp=FsmParser.buildLearnerGraph("C-d->Q", "testGetTempRed1 temp",config,getLabelConverter()).pathroutines.getGraph();
		Vertex foundA = getTempRed_DijkstraShortestPath(a, DeterministicDirectedSparseGraph.findInitial(a), temp);
		Vertex foundB =Test_Orig_RPNIBlueFringeLearnerTestComponent.getTempRed(a, DeterministicDirectedSparseGraph.findInitial(a), temp);
		Assert.assertTrue(DeterministicDirectedSparseGraph.findInitial(temp).equals(foundA));
		Assert.assertTrue(DeterministicDirectedSparseGraph.findInitial(temp).equals(foundB));
	}
	
	@Test
	public final void testGetTempRed2()
	{
		DirectedSparseGraph a=FsmParser.buildLearnerGraph("A-a->B-a->B-c->C-c->D", "testGetTempRed1 model",config,getLabelConverter()).pathroutines.getGraph(),
			temp=FsmParser.buildLearnerGraph("C-a->Q-a->Q-c->Q", "testGetTempRed1 temp",config,getLabelConverter()).pathroutines.getGraph();
		Vertex foundA = getTempRed_DijkstraShortestPath(a, DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("D"), a), temp);
		Vertex foundB = Test_Orig_RPNIBlueFringeLearnerTestComponent.getTempRed(a, DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("D"), a), temp);
		Assert.assertTrue(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("Q"), temp).equals(foundA));
		Assert.assertTrue(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("Q"), temp).equals(foundB));
	}
	
	@Test
	public final void findMergeablePair1()
	{
		DirectedSparseGraph g=FsmParser.buildLearnerGraph("A-a->B\nA-b->B\nA-c->C\nA-d->D", "findMergeablePair1",config,getLabelConverter()).pathroutines.getGraph();
		Assert.assertNull(Test_Orig_RPNIBlueFringeLearner.findMergablePair(g));
	}
	
	@Test
	public final void findMergeablePair2()
	{
		DirectedSparseGraph g=FsmParser.buildLearnerGraphND("A-a->B\nA-b->B\nA-c->D\nA-b->D\nA-d->E", "findMergeablePair2",config,getLabelConverter()).pathroutines.getGraph();
		Vertex 
			b = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("B"), g),
			d = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("D"), g);
		Set<Vertex> expected = new HashSet<Vertex>();expected.add(d);expected.add(b);
		Set<Vertex> actualA = new HashSet<Vertex>();
		OrigStatePair value = Test_Orig_RPNIBlueFringeLearner.findMergablePair(g);actualA.add(value.getQ());actualA.add(value.getR());
		Assert.assertTrue("expected: B, D in either order got: "+actualA,expected.equals(actualA));
	}
	
	@Test
	public final void findMergeablePair3a()
	{
		DirectedSparseGraph g=FsmParser.buildLearnerGraphND("S-p->A-a->S\nA-b->S\nA-c->D\nA-b->D\nA-d->E", "findMergeablePair3a",config,getLabelConverter()).pathroutines.getGraph();
		Vertex 
			s = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("S"), g),
			d = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("D"), g);
		OrigStatePair expected = new OrigStatePair(d,s),
		actualA = Test_Orig_RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertTrue("expected: "+expected+" got: "+actualA,expected.equals(actualA));
	}

	@Test
	public final void findMergeablePair3b()
	{
		DirectedSparseGraph g=FsmParser.buildLearnerGraphND("S-p->A-a->B\nA-b->B\nA-c->S\nA-b->S\nA-d->E", "findMergeablePair3b",config,getLabelConverter()).pathroutines.getGraph();
		Vertex 
			b = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("B"), g),
			s = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("S"), g);
		OrigStatePair expected = new OrigStatePair(b,s),
		actualA = Test_Orig_RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertTrue("expected: "+expected+" got: "+actualA,expected.equals(actualA));
	}

	@Test
	public final void findMergeablePair4()
	{
		DirectedSparseGraph g=FsmParser.buildLearnerGraphND("S-p->A-a->B\nA-b->B\nA-c-#D\nA-b-#D\nA-d->E", "findMergeablePair4",config,getLabelConverter()).pathroutines.getGraph();
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
		DirectedSparseGraph g=FsmParser.buildLearnerGraph(machineToMerge, graphName,config,getLabelConverter()).pathroutines.getGraph(),
			g2=(DirectedSparseGraph)g.copy();
		Vertex 
			a = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID(stateRed), g),
			b = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID(stateBlue), g);
				
		Assert.assertNotNull("state "+stateRed+" was not found", a);
		Assert.assertNotNull("state "+stateBlue+" was not found", b);
		
		OrigStatePair pairOrig = new OrigStatePair(b,a);
		StatePair pairNew1 = new StatePair(DeterministicDirectedSparseGraph.findVertexNamed(VertexID.parseID(stateBlue), g),DeterministicDirectedSparseGraph.findVertexNamed(VertexID.parseID(stateRed), g));
		LearnerGraph l = new LearnerGraph(g, testConfig);
		StatePair pairNew2 = new StatePair(l.findVertex(VertexID.parseID(stateBlue)),l.findVertex(VertexID.parseID(stateRed)));
		LearnerGraph 
			mergeResultA = new LearnerGraph(Test_Orig_RPNIBlueFringeLearner.mergeAndDeterminize(g, pairOrig),testConfig), 
			mergeResultB = new LearnerGraph(MergeStates.mergeAndDeterminize(g2, pairNew1,testConfig),testConfig),
			mergeResultC = new LearnerGraph(MergeStates.mergeAndDeterminize(l, pairNew2).pathroutines.getGraph(),testConfig),
			mergeResultD = new LearnerGraph(MergeStates.mergeAndDeterminize_general(l, pairNew2).pathroutines.getGraph(),testConfig),
			expectedMachine = buildLearnerGraph(expectedFSM, "expected machine",testConfig,getLabelConverter());

		TestEquivalenceChecking.checkM(machineToMerge, new LearnerGraph(g2,testConfig), testConfig, getLabelConverter());
		
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
		DirectedSparseGraph g=FsmParser.buildLearnerGraphND("S-p->A-a->S\nA-b->S\nA-c->D\nA-b->D\nA-d->E\nS-n->U", "testMerge1a",config,getLabelConverter()).pathroutines.getGraph();
		Vertex 
			s = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("S"), g),
			d = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("U"), g);
		OrigStatePair pair = new OrigStatePair(d,s);
		LearnerGraph 
			mergeResultA = new LearnerGraph(Test_Orig_RPNIBlueFringeLearner.mergeAndDeterminize(g, pair),testConfig),
			expectedResult = buildLearnerGraph("S-p->A-a->S\nA-b->S\nA-c->S\nA-d->E\nS-n->S", "expected",testConfig,getLabelConverter());
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
		DirectedSparseGraph g=FsmParser.buildLearnerGraph(largeGraph1_invalid5,"testMerge_fail1",config,getLabelConverter()).pathroutines.getGraph();
		CmpVertex 
			a = DeterministicDirectedSparseGraph.findVertexNamed(VertexID.parseID("A"), g),
			b = DeterministicDirectedSparseGraph.findVertexNamed(VertexID.parseID("B"), g);
		StatePair pair = new StatePair(b,a);// A is red
		MergeStates.mergeAndDeterminize(g, pair,testConfig);
	}
		
	@Test(expected = IllegalArgumentException.class)
	public final void testMerge_fail2()
	{
		LearnerGraph l=buildLearnerGraph(largeGraph1_invalid5,"testMerge_fail1",testConfig,getLabelConverter());
		CmpVertex 
			a = l.findVertex(VertexID.parseID("A")),
			b = l.findVertex(VertexID.parseID("B"));
		StatePair pair = new StatePair(b,a);// A is red
		MergeStates.mergeAndDeterminize(l, pair);
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testMerge_fail3()
	{
		LearnerGraph l=buildLearnerGraph(largeGraph1_invalid5,"testMerge_fail1",testConfig,getLabelConverter());
		CmpVertex 
			a = l.findVertex(VertexID.parseID("A")),
			b = l.findVertex(VertexID.parseID("B"));
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
		final LearnerGraph fsmAsLearnerGraph = FsmParser.buildLearnerGraph(fsm, graphName,config,getLabelConverter());
		final DirectedSparseGraph gB = fsmAsLearnerGraph.pathroutines.getGraph();
		for(PairScore pair:expectedPairs) 
		{
			Assert.assertNotNull("vertex "+pair.getQ()+" is missing in the graph",fsmAsLearnerGraph.findVertex(pair.getQ()));
			Assert.assertNotNull("vertex "+pair.getR()+" is missing in the graph",fsmAsLearnerGraph.findVertex(pair.getR()));
		}
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

		final DirectedSparseGraph gA = FsmParser.buildLearnerGraph(fsm, graphName,config,getLabelConverter()).pathroutines.getGraph();
		// check how the revised pair selection function performs
		final LearnerGraph s = new LearnerGraph(gA, testConfig);
		testChooseStatePairsInternal(gA,s, initialReds, expectedReds, expectedPairs, new InterfaceChooserToTest() {
			public @Override Stack<? extends StatePair> choosePairs() {
				return s.pairscores.chooseStatePairs(null);
			}
		});
	}
	
	private final void testChooseStatePairsInternal(DirectedSparseGraph g,LearnerGraph l, String [] initialReds, String [][] expectedReds, 
			List<PairScore> expectedPairs,InterfaceChooserToTest chooser)
	{
		for(String red:initialReds)
		{
			CmpVertex v = l.findVertex(VertexID.parseID(red));v.setColour(JUConstants.RED);
		}
		Stack<? extends StatePair> pairs = chooser.choosePairs();

		Map<Long,Set<PairScore>> distribution = new HashMap<Long,Set<PairScore>>();// maps scores to sets of states which should correspond to them. The aim is to verify the contents of the stack regardless of the order in which elements with the same score are arranged.

		Set<Set<String>> expectedRedsAsSet = new HashSet<Set<String>>();
		for(int i=0;i<expectedReds.length;++i) 
		{
			Set<String> possibleReds = new HashSet<String>();possibleReds.addAll(Arrays.asList(expectedReds[i]));
			expectedRedsAsSet.add(possibleReds);
		}
		Set<String> finalReds = new HashSet<String>();
		DirectedSparseGraph grf = l.pathroutines.getGraph();
		for(Vertex red:DeterministicDirectedSparseGraph.findVertices(JUConstants.COLOUR, JUConstants.RED, grf))
				finalReds.add(((VertID)red.getUserDatum(JUConstants.LABEL)).getStringId());
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
		long lastScore = -1;
		for(StatePair elem:pairs)
		{
			doneEdges = new HashSet<DirectedSparseEdge>();
			DeterministicVertex origBlue = DeterministicDirectedSparseGraph.findVertexNamed(elem.getQ(), g);
			DeterministicVertex origRed = DeterministicDirectedSparseGraph.findVertexNamed(elem.getR(), g);
			long currentScore = computeScore(g, new OrigStatePair(origBlue,origRed));// This one returns vertices from g, but elem may easily contain StringVertices and such, hence convert elem to Vertex-pair.
			PairScore elA = constructPairScore(elem.getQ().getStringId(),elem.getR().getStringId(),currentScore, testConfig);
			PairScore elB = constructPairScore(elem.getR().getStringId(),elem.getQ().getStringId(),currentScore, testConfig);
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
	
	/** The distance from the initial state is factored into decisions to compare pairs, in order to 
	 * keep the spirit of BlueFringe where we look at red states closest to the initial state first. 
	 */
	@Test
	public final void testVertexOrdering()
	{
		LearnerGraph gr = new LearnerGraph(config);
		Label a=AbstractLearnerGraph.generateNewLabel("a", config,getLabelConverter()),b=AbstractLearnerGraph.generateNewLabel("b", config,getLabelConverter());
		gr.paths.augmentPTA(Arrays.asList(new Label[]{a,a}),true,false,null);
		gr.paths.augmentPTA(Arrays.asList(new Label[]{a,b}),true,false,null);
		gr.paths.augmentPTA(Arrays.asList(new Label[]{b}),true,false,null);
		
		CmpVertex A=gr.paths.getVertex(Arrays.asList(new Label[]{})),
				B=gr.paths.getVertex(Arrays.asList(new Label[]{a})),
				C=gr.paths.getVertex(Arrays.asList(new Label[]{a,a})),
				D=gr.paths.getVertex(Arrays.asList(new Label[]{a,b})),
				E=gr.paths.getVertex(Arrays.asList(new Label[]{b}));
						
		// The following names are in the order of Red,Blue, but constructor of PairScore expects them in the opposite order 
		PairScore AE=new PairScore(E,A,0,0),AB=new PairScore(B,A,0,0),
				AD=new PairScore(D,A,0,0),AC=new PairScore(C,A,0,0),
				BC=new PairScore(C,B,0,0),EC=new PairScore(C,E,0,0),
				DC=new PairScore(C,D,0,0),CC=new PairScore(C,C,0,0);
		
		PairScore orderedArray[]=new PairScore[]{AE,AB,AD,AC,EC,BC,DC,CC};
		for(int i=0;i<orderedArray.length;++i)
			for(int j=0;j<orderedArray.length;++j)
			{
				int actual = orderedArray[i].compareInTermsOfDepth(orderedArray[j]);if (actual > 0) actual=1;else if (actual < 0) actual=-1;
				int expected = new Integer(j).compareTo(i);// the first one is the highest
				Assert.assertEquals(orderedArray[i]+".compareto "+orderedArray[j]+" (="+actual+") != "+expected,expected,actual);
			}
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
				"A-a-#R\nA-d->A2-c->A3-c->A4-c->A5\n"+
				"A-p->P1-a->P2\n"+
				"A-s->S1-d->S2-c->S3-c->S4\n"+
				"A-r->R1-d->R2-c->R3\n"+
				"A-u->U1-d->U2-c->U3\n"+
				"A-q->Q1-d->Q2",
				new String[]{"A"},
				new String[][] {new String[]{"A","P1","R"}},
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
		pairsAndScores.add(constructPairScore("P3", "R", 0,testConfig));

		testChooseStatePairs(
				"A-a-#R\nA-d->A2-c->A3-c->A4-c->A5\n"+
				"A-p->P1-a->P2\n"+"P1-b-#P3\n"+
				"A-s->S1-d->S2-c->S3-c->S4\n"+
				"A-r->R1-d->R2-c->R3\n"+
				"A-u->U1-d->U2-c->U3\n"+
				"A-q->Q1-d->Q2",
				new String[]{"A"},
				new String[][] {new String[]{"A","P1","R"},new String[]{"A","P1","P3"}},
				pairsAndScores,"testNewchooseStatePairs2");
	}
	
	@Test
	public final void testNewchooseStatePairs3()
	{
		List<PairScore> pairsAndScores = new LinkedList<PairScore>();
		pairsAndScores.add(constructPairScore("R1", "R2", 0,testConfig));
		pairsAndScores.add(constructPairScore("A1", "A", 3,testConfig));

		testChooseStatePairs(
				"A-a->B1-a-#R1\nB1-b-#R2\n"+
				"A-b->A1\n"+
				"A1-a->B2\n"+
				"A1-b->A2-a->B3\n",
				new String[]{"A"},
				new String[][] {new String[]{"A","B1","R2"},new String[]{"A","B1","R1"}},
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
		
		DirectedSparseGraph g = FsmParser.buildLearnerGraph(fsm, graphName,config,getLabelConverter()).pathroutines.getGraph();
		OrigStatePair pairOrig = new OrigStatePair(
				DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("B"), g),
				DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("A"), g));
		
		LearnerGraph s = new LearnerGraph(g, testConfig);
		StatePair pairNew = new StatePair(
				s.findVertex(VertexID.parseID("B")),
				s.findVertex(VertexID.parseID("A")));
		doneEdges = new HashSet<DirectedSparseEdge>();
		s.config.setLearnerScoreMode(Configuration.ScoreMode.CONVENTIONAL);s.setMaxScore(maxScoreConstant-1);
		long origScore = computeScore(g, pairOrig),
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
			for(CmpVertex vert:eqClass.getStates()) oneOfTheSets.add(vert.getStringId());
			Assert.assertTrue("received an unexpected set "+oneOfTheSets,expectedSets.contains(oneOfTheSets));expectedSets.remove(oneOfTheSets);
		}
		Assert.assertEquals(0, expectedSets.size());		
	}
	
	private void testGeneralPairScoreComputation(String machine, String graphName, int expectedScore,
			String[][] expectedSrc,String [][]incompatibles)
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph(machine, graphName,mainConfiguration,getLabelConverter());

		if (incompatibles != null)
			for(String [] incompatibleRow:incompatibles)
			{
				assert incompatibleRow.length == 2;
				fsm.addToCompatibility(fsm.findVertex(incompatibleRow[0]), fsm.findVertex(incompatibleRow[1]),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
			}
		Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> result = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		int score = -2;
		score = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex(VertexID.parseID("A")),fsm.findVertex(VertexID.parseID("B"))),null,result);
		//Visualiser.updateFrame(g, result);Visualiser.waitForKey();
		Assert.assertEquals(expectedScore, score);
		if (score >=0)
			matchCollectionsOfVertices(result, expectedSrc);
		
		result.clear();score = -2;
		score = fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex(VertexID.parseID("B")),fsm.findVertex(VertexID.parseID("A"))),null,result);
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
	
	@Test
	public final void testPairCompatible_general_E()
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph("I-d->A1-a->A2-b->A3-c->A4 / I-b->B1-a->B2-b->B3 / I-c->C1-a->C2-b->C3", "testPairCompatible_general_Ea",config,getLabelConverter());
		Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		fsm.pairscores.computePairCompatibilityScore_general(null,Arrays.asList(new StatePair[]{
				new StatePair(fsm.findVertex("I"),fsm.findVertex("A1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("B1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("C1"))
				}), verticesToMerge);
		LearnerGraph mergeOutcome =  MergeStates.mergeCollectionOfVertices(fsm, null, verticesToMerge);
		LearnerGraph expected = FsmParser.buildLearnerGraph("I-d->I-b->I-c->I / I-a->B2-b->B3-c->C1", "testPairCompatible_general_Eb",config,getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, mergeOutcome);
		if (diffEx != null)
			throw diffEx;
		
		// Now test the "lite" version of state merging routine.
		LearnerGraph merge2 = MergeStates.mergeCollectionOfVerticesNoUpdateOfAuxiliaryInformation(fsm, verticesToMerge);
		diffEx = WMethod.checkM(expected, merge2);
		if (diffEx != null)
			throw diffEx;
	}

	@Test
	public final void testPairCompatible_general_F()
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph("I-a->A1-a->A2-b->A3-c->A4 / I-b->B1-a->B2-b->B3 / I-c->C1-a->C2-b->C3", "testPairCompatible_general_Fa",config,getLabelConverter());
		Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		fsm.pairscores.computePairCompatibilityScore_general(null,Arrays.asList(new StatePair[]{
				new StatePair(fsm.findVertex("I"),fsm.findVertex("A1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("B1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("C1"))
				}), verticesToMerge);
		LearnerGraph mergeOutcome =  MergeStates.mergeCollectionOfVertices(fsm, null, verticesToMerge);
		LearnerGraph expected = FsmParser.buildLearnerGraph("I-b->I-c->I / I-a->I", "testPairCompatible_general_Fb",config,getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, mergeOutcome);
		if (diffEx != null)
			throw diffEx;

		// Now test the "lite" version of state merging routine.
		LearnerGraph merge2 = MergeStates.mergeCollectionOfVerticesNoUpdateOfAuxiliaryInformation(fsm, verticesToMerge);
		diffEx = WMethod.checkM(expected, merge2);
		if (diffEx != null)
			throw diffEx;
	}

	@Test
	public final void testPairCompatible_general_G()
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph("I-d->A1-a->A2-b->A3-c->A4 / I-b->B1-a->B2-b->B3 / I-c->C1-a->C2-b->C3 / A1-b->T1-c->T1 / A1-c->T2-a->A2 / B1-b->T2-e->B2 / C1-c->T3-a->T4", "testPairCompatible_general_Ga",config,getLabelConverter());
		Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		fsm.pairscores.computePairCompatibilityScore_general(null,Arrays.asList(new StatePair[]{
				new StatePair(fsm.findVertex("I"),fsm.findVertex("A1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("B1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("C1"))
				}), verticesToMerge);
		LearnerGraph mergeOutcome =  MergeStates.mergeCollectionOfVertices(fsm, null, verticesToMerge);
		LearnerGraph expected = FsmParser.buildLearnerGraph("I-d->I-b->I-c->I / I-a->B2-b->B3-c->C1 / I-e->B2", "testPairCompatible_general_Gb",config,getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, mergeOutcome);
		if (diffEx != null)
			throw diffEx;

		// Now test the "lite" version of state merging routine.
		LearnerGraph merge2 = MergeStates.mergeCollectionOfVerticesNoUpdateOfAuxiliaryInformation(fsm, verticesToMerge);
		diffEx = WMethod.checkM(expected, merge2);
		if (diffEx != null)
			throw diffEx;
	}

	@Test
	public final void testPairCompatible_general_H()
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph("I-d->A1-a->A2-b->A3-c->A4 / I-b->B1-a->B2-b->B3 / I-c->C1-a->C2-b->C3 / A1-b->T1-c->T1 / A1-c->T2-a->A2 / B1-b->T2-d->B2 / C1-c->T3-a->T4", "testPairCompatible_general_Ha",config,getLabelConverter());
		Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		fsm.pairscores.computePairCompatibilityScore_general(null,Arrays.asList(new StatePair[]{
				new StatePair(fsm.findVertex("I"),fsm.findVertex("A1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("B1")),new StatePair(fsm.findVertex("I"),fsm.findVertex("C1"))
				}), verticesToMerge);
		LearnerGraph mergeOutcome =  MergeStates.mergeCollectionOfVertices(fsm, null, verticesToMerge);
		LearnerGraph expected = FsmParser.buildLearnerGraph("I-b->I-c->I / I-a->I-d->I", "testPairCompatible_general_Hb",config,getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, mergeOutcome);
		if (diffEx != null)
			throw diffEx;

		// Now test the "lite" version of state merging routine.
		LearnerGraph merge2 = MergeStates.mergeCollectionOfVerticesNoUpdateOfAuxiliaryInformation(fsm, verticesToMerge);
		diffEx = WMethod.checkM(expected, merge2);
		if (diffEx != null)
			throw diffEx;
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
		
		
		// This one generates a number of random machines; for every pair of vertices in those machines that can be merged, it constructs the set of vertices to merge and then
		// merges some of them (randomly) in one go. Such a merge should generate the same number of states and give the same score.
		@Test 
		public final void testRandomFSMMergers() throws IncompatibleStatesException
		{
			final int states = 50;
			MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
			LearnerGraph referenceGraph = mg.nextMachine(states/2,fsmNumber, config,getLabelConverter()).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, in the mergers below we can still attempt to merge arbitrary subsets of states.

			for(CmpVertex a:referenceGraph.transitionMatrix.keySet())
				for(CmpVertex b:referenceGraph.transitionMatrix.keySet())
				{
					Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					int score = referenceGraph.pairscores.computePairCompatibilityScore_general(new StatePair(a,b),null,verticesToMerge);
					if (score >= 0)
					{
						Set<Set<CmpVertex>> origVertexPairs = new HashSet<Set<CmpVertex>>();
						for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> cls:verticesToMerge)
							origVertexPairs.add(cls.getStates());
						Random rnd = new Random(fsmNumber);
						for(int experiment=0;experiment < 10;++experiment)
						{
							List<StatePair> pairs = new ArrayList<StatePair>(1000);
							for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> cls:verticesToMerge)
							{
								origVertexPairs.add(cls.getStates());
								CmpVertex bufferOfVertices[] = cls.getStates().toArray(new CmpVertex[]{});
								for(int i=0;i<10;++i)
									pairs.add(new StatePair(bufferOfVertices[rnd.nextInt(bufferOfVertices.length)],bufferOfVertices[rnd.nextInt(bufferOfVertices.length)]));
									
							}
							Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> newVerticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
							int newScore = referenceGraph.pairscores.computePairCompatibilityScore_general(new StatePair(a,b),pairs,newVerticesToMerge);
							/*
							System.out.println("scores: "+score+" "+newScore);
							if (score != newScore)
							{
								System.out.println("State pair: "+(new StatePair(a,b))+"states originally merged: "+verticesToMerge+" states to be merged: "+newVerticesToMerge);
								Visualiser.updateFrame(referenceGraph, null);
								LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> new2VerticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
								int anotherScore = referenceGraph.pairscores.computePairCompatibilityScore_general(new StatePair(a,b),pairs,new2VerticesToMerge);
								
							}*/
							Assert.assertEquals(score,newScore);
							Set<Set<CmpVertex>> newVertexPairs = new HashSet<Set<CmpVertex>>();for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> cls:newVerticesToMerge) newVertexPairs.add(cls.getStates());
							Assert.assertEquals(origVertexPairs,newVertexPairs);
						}
					}
				}
		}
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
	
	@Test
	public final void testKtails1()
	{
		testConfig.setKlimit(0);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-a-#N / U-a-#Q", "testKtails1",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("N")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("N")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("N"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("N"),fsm.findVertex("M")), false));
		
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("H")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("H")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("H"),fsm.findVertex("M")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("H"),fsm.findVertex("M")), false));
		
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("N"),fsm.findVertex("Q")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("N"),fsm.findVertex("Q")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("Q"),fsm.findVertex("N")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("Q"),fsm.findVertex("N")), false));
	}
	
	/** Similar to above, k limit is 1. */
	@Test
	public final void testKtails2()
	{
		testConfig.setKlimit(1);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-a-#N / U-b-#Q", "testKtails2",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("N")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("N")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("N"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("N"),fsm.findVertex("M")), false));
		
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("U")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("U")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("U"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("U"),fsm.findVertex("M")), false));
		
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("N"),fsm.findVertex("Q")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("N"),fsm.findVertex("Q")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("Q"),fsm.findVertex("N")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("Q"),fsm.findVertex("N")), false));
		
		// the following are all reject because C and N have different accept-conditions.
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("U")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("U")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("U"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("U"),fsm.findVertex("B")), false));
		
		// now we label C with the same condition as Q, things should improve 
		fsm.findVertex("C").setAccept(fsm.findVertex("Q").isAccept());
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("U")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("U")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("U"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("U"),fsm.findVertex("B")), false));
		
		fsm.findVertex("B").setAccept(false);
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("U")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("U")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("U"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("U"),fsm.findVertex("B")), false));

		fsm.findVertex("U").setAccept(false);
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("U")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("U")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("U"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("U"),fsm.findVertex("B")), false));
	}
	
	// both b and c transitions match
	@Test
	public final void testKtails3a()
	{
		testConfig.setKlimit(1);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N / M-c->P", "testKtails3a",testConfig,getLabelConverter());
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}	
	
	// only the first step matches, nothing is found on the second step.
	@Test
	public final void testKtails3b()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N / M-c->P", "testKtails3b",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}	
	
	// some transitions lead to conflicting states
	@Test
	public final void testKtails3c()
	{
		testConfig.setKlimit(1);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N / M-c-#P", "testKtails3c",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}
	
	// some transitions lead to conflicting states
	@Test
	public final void testKtails3d()
	{
		testConfig.setKlimit(1);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / B-e->H / M-b->N / M-c->P / M-e-#Z", "testKtails3d",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}

	@Test
	public final void testKtails4()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-b->E / B-c->F-a->G-b->H / M-b->N ", "testKtails4",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));

		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("C"),fsm.findVertex("F")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("C"),fsm.findVertex("F")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("F"),fsm.findVertex("C")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("F"),fsm.findVertex("C")), false));

		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A"),fsm.findVertex("F")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A"),fsm.findVertex("F")), false));// B->F does not match anything
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("F"),fsm.findVertex("A")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("F"),fsm.findVertex("A")), false));// B->F does not match anything
	}	
	
	@Test
	public final void testKtails5()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-c->P-a->A", "testKtails5",testConfig,getLabelConverter());
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}	
	
	/** A match. */
	@Test
	public final void testKtails6()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a->Q / M-c->P-a->S", "testKtails6",testConfig,getLabelConverter());
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}	
	
	/** Only matches for any path. */
	@Test
	public final void testKtails7()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a->Q / M-c->P-a->S / N-b->R", "testKtails7",testConfig,getLabelConverter());
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}	

	/** Only matches for any path. */
	@Test
	public final void testKtails8()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a->Q / M-c->P-a->S / P-b->R", "testKtails8",testConfig,getLabelConverter());
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}	

	/** Only matches for any path. */
	@Test
	public final void testKtails9()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b-#N / M-c->P-a->S", "testKtails9",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}	
	
	/** Only matches for any path. */
	@Test
	public final void testKtails10()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a-#Q / M-c->P-a->S / P-b->R", "testKtails10",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}	

	/** Only matches for any path. */
	@Test
	public final void testKtails11()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a->Q / M-c-#P", "testKtails11",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}	

	/** Only matches for any path. */
	@Test
	public final void testKtails12()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a->Q / M-c->P-a-#S", "testKtails12",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}
	
	/** Matches both any and all paths. */
	@Test
	public final void testKtails13()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->F-a->G-a->H / A-b->C-a->D-a->E", "testKtails13",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("H"),fsm.findVertex("E")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("H"),fsm.findVertex("E")), false));

		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("D"),fsm.findVertex("G")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("D"),fsm.findVertex("G")), false));

		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("C"),fsm.findVertex("F")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("C"),fsm.findVertex("F")), false));

		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));// -1 because b does not match anything
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A"),fsm.findVertex("C")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A"),fsm.findVertex("C")), false));// -1 because b does not match anything
	}

	/** Matches both any and all paths. */
	@Test
	public final void testKtails14()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->F-a->G-a->H / T-a->C-a->D-a->E", "testKtails14",testConfig,getLabelConverter());
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A"),fsm.findVertex("T")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A"),fsm.findVertex("T")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("T"),fsm.findVertex("A")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("T"),fsm.findVertex("A")), false));
	}
	
	/** Matches both any and all paths. */
	@Test
	public final void testKtails15a()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A1-a->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-a->F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15a",testConfig,getLabelConverter());
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), false));
		
		fsm.findVertex("C1").setAccept(false);fsm.findVertex("C2").setAccept(false);
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), false));

		fsm.findVertex("D1").setAccept(false);fsm.findVertex("D2").setAccept(false);
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), false));
	}
	
	/** Same as testKtails15 but F1/F2 are incompatible. */
	@Test
	public final void testKtails15b()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A1-a->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-a-#F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15b",testConfig,getLabelConverter());

		fsm.findVertex("C1").setAccept(false);fsm.findVertex("C2").setAccept(false);
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), false));
		
	}
	
	/** Same as testKtails15 but F1/F2 are incompatible. */
	@Test
	public final void testKtails15c()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A1-z->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-z-#F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15c",testConfig,getLabelConverter());

		fsm.findVertex("C1").setAccept(false);fsm.findVertex("C2").setAccept(false);
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), false));
	}
	
	/** Same as testKtails15 but B1/B2 are incompatible. */
	@Test
	public final void testKtails15d()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A1-a->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-a->F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15a",testConfig,getLabelConverter());

		fsm.findVertex("C1").setAccept(false);fsm.findVertex("C2").setAccept(false);
		
		fsm.findVertex("B1").setAccept(false);
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), false));
	}
	
	/** Same as testKtails15 but D1/D2 are incompatible. */
	@Test
	public final void testKtails15e()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A1-a->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-a->F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15a",testConfig,getLabelConverter());

		fsm.findVertex("C1").setAccept(false);fsm.findVertex("C2").setAccept(false);
		
		fsm.findVertex("D1").setAccept(false);
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), false));
	}
	
	/** Same as testKtails15 but D1/D2 are incompatible, however the depth of exploration is lower and hence this goes unnoticed. */
	@Test
	public final void testKtails15f()
	{
		testConfig.setKlimit(1);
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A1-a->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-a->F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15a",testConfig,getLabelConverter());

		fsm.findVertex("C1").setAccept(false);fsm.findVertex("C2").setAccept(false);
		
		fsm.findVertex("D1").setAccept(false);
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A1"),fsm.findVertex("A2")), false));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), true));
		Assert.assertEquals(0,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("A2"),fsm.findVertex("A1")), false));
	}
	
	@Test
	public final void testSiccoScoring0()
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c-#F", "testSiccoScoring0",testConfig,getLabelConverter());
		for(CmpVertex v:fsm.transitionMatrix.keySet()) v.setColour(JUConstants.RED);
		Assert.assertEquals(-1,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));
	}

	@Test
	public final void testSiccoScoring1()
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E", "testSiccoScoring1",testConfig,getLabelConverter());
		for(CmpVertex v:fsm.transitionMatrix.keySet()) v.setColour(JUConstants.RED);
		Assert.assertEquals(2,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(2,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));
	}
	
	@Test
	public final void testSiccoScoring2()
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c->F / E-a->G / C-b->H", "testSiccoScoring2",testConfig,getLabelConverter());
		for(CmpVertex v:fsm.transitionMatrix.keySet()) v.setColour(JUConstants.RED);
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));
	}
	
	@Test
	public final void testSiccoScoring3a()
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c->F / E-a->G / C-b->H / G-b->I", "testSiccoScoring3",testConfig,getLabelConverter());
		for(CmpVertex v:fsm.transitionMatrix.keySet()) v.setColour(JUConstants.RED);
		Assert.assertEquals(-1,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));
	}
	
	// Same as above but states not red
	@Test
	public final void testSiccoScoring3b()
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c->F / E-a->G / C-b->H / G-b->I", "testSiccoScoring3",testConfig,getLabelConverter());
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));
	}

	@Test
	public final void testSiccoScoring4a()
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c->F / E-a->G / C-b->H / C-c->I", "testSiccoScoring3",testConfig,getLabelConverter());
		for(CmpVertex v:fsm.transitionMatrix.keySet()) v.setColour(JUConstants.RED);
		Assert.assertEquals(-1,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));
	}

	// Same as above but states not red
	@Test
	public final void testSiccoScoring4b()
	{
		LearnerGraph fsm = FsmParser.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c->F / E-a->G / C-b->H / C-c->I", "testSiccoScoring3",testConfig,getLabelConverter());
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));
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
