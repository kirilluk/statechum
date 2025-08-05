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

import java.util.*;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import junit_runners.ParameterizedWithName;

import statechum.*;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.Configuration.IDMode;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.FsmParserStatechum;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PairScoreComputation.SiccoGeneralScoring;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.old_generalised_merge_routines.OldPairScoreComputation;
import statechum.model.testset.PTASequenceSet;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import static statechum.TestHelper.checkForCorrectException;
import static statechum.analysis.learning.rpnicore.PairScoreComputation.evaluateProgressProperty;
import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;
import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildList;
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
	@junit_runners.ParameterizedWithName.ParametersToString
	public static String parametersToString(Configuration config)
	{
		return TestConfiguration.parametersToString(config);
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
	Configuration testConfig = null;
	
	/** Each test starts with this configuration. */
	private Configuration mainConfiguration = null;
	
	protected void checkLearner(String fsmString, String name,String [][] plus, String [][] minus)
	{
		final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph(fsmString, name,testConfig,getLabelConverter());
		
		// now sanity checking on the plus and minus sets
		for(String [] path:plus)
			assert AbstractOracle.USER_ACCEPTED == expected.paths.tracePathPrefixClosed(AbstractLearnerGraph.buildList(Arrays.asList(path),config,getLabelConverter()));
		for(String [] path:minus)
			assert AbstractOracle.USER_ACCEPTED != expected.paths.tracePathPrefixClosed(AbstractLearnerGraph.buildList(Arrays.asList(path),config,getLabelConverter()));

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
				return new Pair<>(expected.paths.tracePathPrefixClosed(question), null);
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

//	DeterministicVertex p = AbstractLearnerGraph.generateNewJungVertex("P"), q= AbstractLearnerGraph.generateNewJungVertex("Q");
	/** Checks that both the old and the new algorithm reports a pair of states as incompatible. */
	public final void testNewLearnerIncompatible(String fsm, String name)
	{
		LearnerGraph s = FsmParserStatechum.buildLearnerGraph(fsm, name,testConfig,getLabelConverter());
		DirectedSparseGraph g = s.pathroutines.getGraph();
		OrigStatePair pairOrig = new OrigStatePair(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("B"), g),
				DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("A"), g));
		StatePair pairNew = new StatePair(s.findVertex(VertexID.parseID("B")),s.findVertex(VertexID.parseID("A")));
		doneEdges = new HashSet<>();
		long origScore = computeScore(g, pairOrig),
			newScoreA = s.pairscores.computeStateScore(pairNew);
		Assert.assertEquals(-1, origScore);
		Assert.assertEquals(-1, newScoreA);
	}
	
	/** Checks that both the old and the two new algorithms report the same score for a pair of states and ask the same questions.
	 * States being merged are called "A" and "B".
	 */
	public final void testNewLearnerQuestions(String fsm, int expectedScore, String learnerName)
	{
		LearnerGraph s = FsmParserStatechum.buildLearnerGraph(fsm, learnerName,testConfig,getLabelConverter());
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
		Assert.assertFalse(tempG.pathroutines.checkUnreachableStates());
		Assert.assertFalse(tempBG.pathroutines.checkUnreachableStates());
		Assert.assertNull(WMethod.checkM(tempG, tempBG));
		
		
		doneEdges = new HashSet<>();
		long origScore = computeScore(g, pairOrig),
			newScoreA = s.pairscores.computeStateScore(pairNew1),
			newScoreB = s.pairscores.computePairCompatibilityScore(pairNew1),
			newScoreC = s.pairscores.computePairCompatibilityScore_general(pairNew1,null, new LinkedList<>(), true),
			newScoreS = s.pairscores.computePairCompatibilityScore_general(pairNew1,null, new LinkedList<>(), false),
			newScoreT = new OldPairScoreComputation(s).computePairCompatibilityScore_general(pairNew1,null, new LinkedList<>());

		LearnerGraph learner2 = new LearnerGraph(g, testConfig);
		StatePair pairNew2 = new StatePair(learner2.findVertex(VertexID.parseID("B")),learner2.findVertex(VertexID.parseID("A")));

		Collection<List<Label>> 
			// Since computeQS assumes that red names remain unchanged in the merged version, I have to use a specific merging procedure
			questionsB = ComputeQuestions.computeQS_orig(pairNew2, learner2,MergeStates.mergeAndDeterminize(learner2, pairNew2)),
			questionsC = ComputeQuestions.computeQS_orig(pairNew2, learner2,MergeStates.mergeAndDeterminize_general(learner2, pairNew2)),
			questionsD = ComputeQuestions.computeQS_general(pairNew2, learner2, MergeStates.mergeAndDeterminize_general(learner2, pairNew2), 
					new ComputeQuestions.QuestionGeneratorQSMLikeWithLoops()).getData(),
			questionsT = ComputeQuestions.computeQS_general(pairNew2, learner2, OldPairScoreComputation.mergeAndDeterminize_general(learner2, pairNew2),
					new ComputeQuestions.QuestionGeneratorQSMLikeWithLoops()).getData();
		Assert.assertTrue("these states should be compatible - correct test data",origScore >= 0);
		Assert.assertEquals(expectedScore, origScore);
		Assert.assertEquals(expectedScore, newScoreA);
		Assert.assertTrue( expectedScore < 0? (newScoreB < 0):(newScoreB >= 0));
		Assert.assertTrue( expectedScore < 0? (newScoreC < 0):(newScoreC >= 0));
		Assert.assertEquals(newScoreC, newScoreS);
		Assert.assertEquals(newScoreC, newScoreT);
		if (expectedScore != -1)
		{
			Set<List<Label>> oldQuestions = new HashSet<>(generateQuestions(g, temp, pairOrig));
			//Assert.assertTrue(oldQuestions.size() > 0);
			Set<List<Label>> newQuestionsB = new HashSet<>(questionsB);
			Set<List<Label>> newQuestionsC = new HashSet<>(questionsC);
			Set<List<Label>> newQuestionsD = new HashSet<>(questionsD);
			Set<List<Label>> newQuestionsT = new HashSet<>(questionsT);
			Assert.assertEquals("different questions: old " + oldQuestions + ", new " + questionsB, oldQuestions, newQuestionsB);
			Assert.assertEquals("different questions: old " + oldQuestions + ", new " + questionsC, oldQuestions, newQuestionsC);
			Assert.assertEquals("different questions: old " + oldQuestions + ", new " + questionsD, oldQuestions, newQuestionsD);
			Assert.assertEquals("different questions: old " + oldQuestions + ", new " + questionsT, oldQuestions, newQuestionsT);
		}
	}
	
	@Test
	public final void testQSMvsLoopsQuestionGenerator1()
	{
		LearnerGraph s = FsmParserStatechum.buildLearnerGraph("A-a->A3-b->R-r->T / A-c->A3 / A -b->A2-c->R / A-d->A4-a->A6-b->R / A-e->A5-a->B-q->C","testQSMvsLoopsQuestionGenerator1",testConfig,getLabelConverter());
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
		LearnerGraph s = FsmParserStatechum.buildLearnerGraph("A-a->A3-b->R-r->T / A-c->A3 / A -b->A2-c->R / A-d->A4-a->A6-b->R / A-e->A5-a->B-q->C / R-w->B","testQSMvsLoopsQuestionGenerator2",testConfig,getLabelConverter());
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
		ComputeQuestions.computeQS_orig(new StatePair(null,AbstractLearnerGraph.generateNewJungVertex("non-existing")), new LearnerGraph(testConfig), new LearnerGraph(testConfig));
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
		Vertex tempRed;
		if(!pathToRed.isEmpty()){
			List<Label> pathToRedString = new LinkedList<>();
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
		DirectedSparseGraph a= FsmParserStatechum.buildLearnerGraph("A-a->B", "testGetTempRed1 model",config,getLabelConverter()).pathroutines.getGraph(),
			temp= FsmParserStatechum.buildLearnerGraph("C-d->Q", "testGetTempRed1 temp",config,getLabelConverter()).pathroutines.getGraph();
		Vertex foundA = getTempRed_DijkstraShortestPath(a, DeterministicDirectedSparseGraph.findInitial(a), temp);
		Vertex foundB =Test_Orig_RPNIBlueFringeLearnerTestComponent.getTempRed(a, DeterministicDirectedSparseGraph.findInitial(a), temp);
		Assert.assertEquals(DeterministicDirectedSparseGraph.findInitial(temp), foundA);
		Assert.assertEquals(DeterministicDirectedSparseGraph.findInitial(temp), foundB);
	}
	
	@Test
	public final void testGetTempRed2()
	{
		DirectedSparseGraph a= FsmParserStatechum.buildLearnerGraph("A-a->B-a->B-c->C-c->D", "testGetTempRed1 model",config,getLabelConverter()).pathroutines.getGraph(),
			temp= FsmParserStatechum.buildLearnerGraph("C-a->Q-a->Q-c->Q", "testGetTempRed1 temp",config,getLabelConverter()).pathroutines.getGraph();
		Vertex foundA = getTempRed_DijkstraShortestPath(a, DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("D"), a), temp);
		Vertex foundB = Test_Orig_RPNIBlueFringeLearnerTestComponent.getTempRed(a, DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("D"), a), temp);
		Assert.assertEquals(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("Q"), temp), foundA);
		Assert.assertEquals(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("Q"), temp), foundB);
	}
	
	@Test
	public final void findMergeablePair1()
	{
		DirectedSparseGraph g= FsmParserStatechum.buildLearnerGraph("A-a->B\nA-b->B\nA-c->C\nA-d->D", "findMergeablePair1",config,getLabelConverter()).pathroutines.getGraph();
		Assert.assertNull(Test_Orig_RPNIBlueFringeLearner.findMergablePair(g));
	}
	
	@Test
	public final void findMergeablePair2()
	{
		DirectedSparseGraph g= FsmParserStatechum.buildLearnerGraphND("A-a->B\nA-b->B\nA-c->D\nA-b->D\nA-d->E", "findMergeablePair2",config,getLabelConverter()).pathroutines.getGraph();
		Vertex 
			b = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("B"), g),
			d = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("D"), g);
		Set<Vertex> expected = new HashSet<>();expected.add(d);expected.add(b);
		Set<Vertex> actualA = new HashSet<>();
		OrigStatePair value = Test_Orig_RPNIBlueFringeLearner.findMergablePair(g);actualA.add(value.getQ());actualA.add(value.getR());
		Assert.assertEquals("expected: B, D in either order got: " + actualA, expected, actualA);
	}
	
	@Test
	public final void findMergeablePair3a()
	{
		DirectedSparseGraph g= FsmParserStatechum.buildLearnerGraphND("S-p->A-a->S\nA-b->S\nA-c->D\nA-b->D\nA-d->E", "findMergeablePair3a",config,getLabelConverter()).pathroutines.getGraph();
		DeterministicVertex 
			s = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("S"), g),
			d = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("D"), g);
		OrigStatePair expected = new OrigStatePair(d,s),
		actualA = Test_Orig_RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertEquals("expected: " + expected + " got: " + actualA, expected, actualA);
	}

	@Test
	public final void findMergeablePair3b()
	{
		DirectedSparseGraph g= FsmParserStatechum.buildLearnerGraphND("S-p->A-a->B\nA-b->B\nA-c->S\nA-b->S\nA-d->E", "findMergeablePair3b",config,getLabelConverter()).pathroutines.getGraph();
		DeterministicVertex 
			b = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("B"), g),
			s = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, VertexID.parseID("S"), g);
		OrigStatePair expected = new OrigStatePair(b,s),
		actualA = Test_Orig_RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertEquals("expected: " + expected + " got: " + actualA, expected, actualA);
	}

	@Test
	public final void findMergeablePair4()
	{
		DirectedSparseGraph g= FsmParserStatechum.buildLearnerGraphND("S-p->A-a->B\nA-b->B\nA-c-#D\nA-b-#D\nA-d->E", "findMergeablePair4",config,getLabelConverter()).pathroutines.getGraph();
		OrigStatePair actualA = Test_Orig_RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertNull(actualA);
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
		final LearnerGraph fsmAsLearnerGraph = FsmParserStatechum.buildLearnerGraph(fsm, graphName,config,getLabelConverter());
		final DirectedSparseGraph gB = fsmAsLearnerGraph.pathroutines.getGraph();
		for(PairScore pair:expectedPairs) 
		{
			Assert.assertNotNull("vertex "+pair.getQ()+" is missing in the graph",fsmAsLearnerGraph.findVertex(pair.getQ()));
			Assert.assertNotNull("vertex "+pair.getR()+" is missing in the graph",fsmAsLearnerGraph.findVertex(pair.getR()));
		}

		// check how the reference pair selection function performs
		Configuration conf = testConfig.copy();conf.setLearnerUseStrings(false);conf.setLearnerCloneGraph(false);
		testChooseStatePairsInternal(gB,new LearnerGraph(gB, conf), initialReds, expectedReds, expectedPairs,
				() -> {// Here I need to convert the old type of pairs to the new one.
					Stack<OrigStatePair> pairs = chooseStatePairs(gB, new HashSet<>(), new HashSet<>());
					Stack<StatePair> result = new Stack<>();
					for(OrigStatePair pair:pairs) result.add(new StatePair(pair.getQ(), pair.getR()));
					return result;
		});

		final DirectedSparseGraph gA = FsmParserStatechum.buildLearnerGraph(fsm, graphName,config,getLabelConverter()).pathroutines.getGraph();
		// check how the revised pair selection function performs
		final LearnerGraph s = new LearnerGraph(gA, testConfig);
		testChooseStatePairsInternal(gA,s, initialReds, expectedReds, expectedPairs, () -> s.pairscores.chooseStatePairs(null));
	}
	
	private void testChooseStatePairsInternal(DirectedSparseGraph g, LearnerGraph l, String [] initialReds, String [][] expectedReds,
											  List<PairScore> expectedPairs, InterfaceChooserToTest chooser)
	{
		for(String red:initialReds)
		{
			CmpVertex v = l.findVertex(VertexID.parseID(red));v.setColour(JUConstants.RED);
		}
		Stack<? extends StatePair> pairs = chooser.choosePairs();

		Map<Long,Set<PairScore>> distribution = new HashMap<>();// maps scores to sets of states which should correspond to them. The aim is to verify the contents of the stack regardless of the order in which elements with the same score are arranged.

		Set<Set<String>> expectedRedsAsSet = new HashSet<>();
		for (String[] expectedRed : expectedReds) {
			Set<String> possibleReds = new HashSet<>(Arrays.asList(expectedRed));
			expectedRedsAsSet.add(possibleReds);
		}
		Set<String> finalReds = new HashSet<>();
		DirectedSparseGraph grf = l.pathroutines.getGraph();
		for(Vertex red:DeterministicDirectedSparseGraph.findVertices(JUConstants.COLOUR, JUConstants.RED, grf))
				finalReds.add(((VertID)red.getUserDatum(JUConstants.LABEL)).getStringId());
		Assert.assertTrue("expected red states, any of: "+expectedRedsAsSet+" actual : "+finalReds,expectedRedsAsSet.contains(finalReds));
		for(PairScore ps:expectedPairs)
		{
			Set<PairScore> currScore = distribution.computeIfAbsent(ps.getScore(), k -> new HashSet<>());
			currScore.add(ps);
		}
		long lastScore = -1;
		for(StatePair elem:pairs)
		{
			doneEdges = new HashSet<>();
			DeterministicVertex origBlue = DeterministicDirectedSparseGraph.findVertexNamed(elem.getQ(), g);
			DeterministicVertex origRed = DeterministicDirectedSparseGraph.findVertexNamed(elem.getR(), g);
			long currentScore = computeScore(g, new OrigStatePair(origBlue,origRed));// This one returns vertices from g, but elem may easily contain StringVertices and such, hence convert elem to Vertex-pair.
			PairScore elA = constructPairScore(elem.getQ().getStringId(),elem.getR().getStringId(),currentScore, testConfig);
			PairScore elB = constructPairScore(elem.getR().getStringId(),elem.getQ().getStringId(),currentScore, testConfig);
			Assert.assertSame(JUConstants.RED, elem.getR().getColour());
			Assert.assertSame(JUConstants.BLUE, elem.getQ().getColour());
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
		gr.paths.augmentPTA(Arrays.asList(a,a),true,false,null);
		gr.paths.augmentPTA(Arrays.asList(a,b),true,false,null);
		gr.paths.augmentPTA(Collections.singletonList(b),true,false,null);
		
		CmpVertex A=gr.paths.getVertex(Collections.emptyList()),
				B=gr.paths.getVertex(Collections.singletonList(a)),
				C=gr.paths.getVertex(Arrays.asList(a,a)),
				D=gr.paths.getVertex(Arrays.asList(a,b)),
				E=gr.paths.getVertex(Collections.singletonList(b));
						
		// The following names are in the order of Red,Blue, but constructor of PairScore expects them in the opposite order 
		PairScore AE=new PairScore(E,A,0,0),AB=new PairScore(B,A,0,0),
				AD=new PairScore(D,A,0,0),AC=new PairScore(C,A,0,0),
				BC=new PairScore(C,B,0,0),EC=new PairScore(C,E,0,0),
				DC=new PairScore(C,D,0,0),CC=new PairScore(C,C,0,0);
		
		PairScore[] orderedArray =new PairScore[]{AE,AB,AD,AC,EC,BC,DC,CC};
		for(int i=0;i<orderedArray.length;++i)
			for(int j=0;j<orderedArray.length;++j)
			{
				int actual = orderedArray[i].compareInTermsOfDepth(orderedArray[j]);if (actual > 0) actual=1;else if (actual < 0) actual=-1;
				int expected = Integer.compare(j, i);// the first one is the highest
				Assert.assertEquals(orderedArray[i]+".compareto "+orderedArray[j]+" (="+actual+") != "+expected,expected,actual);
			}
	}
	
	@Test
	public final void testNewchooseStatePairs1()
	{
		List<PairScore> pairsAndScores = new LinkedList<>();
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
		List<PairScore> pairsAndScores = new LinkedList<>();
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
		List<PairScore> pairsAndScores = new LinkedList<>();
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
	
	@Test
	public final void testKtails1()
	{
		testConfig.setKlimit(0);
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-a-#N / U-a-#Q", "testKtails1",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-a-#N / U-b-#Q", "testKtails2",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N / M-c->P", "testKtails3a",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N / M-c->P", "testKtails3b",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N / M-c-#P", "testKtails3c",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / B-e->H / M-b->N / M-c->P / M-e-#Z", "testKtails3d",testConfig,getLabelConverter());
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("M"),fsm.findVertex("B")), false));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeStateScoreKTails(new StatePair(fsm.findVertex("B"),fsm.findVertex("M")), false));
	}

	@Test
	public final void testKtails4()
	{
		testConfig.setKlimit(2);
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-b->E / B-c->F-a->G-b->H / M-b->N ", "testKtails4",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-c->P-a->A", "testKtails5",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a->Q / M-c->P-a->S", "testKtails6",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a->Q / M-c->P-a->S / N-b->R", "testKtails7",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a->Q / M-c->P-a->S / P-b->R", "testKtails8",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b-#N / M-c->P-a->S", "testKtails9",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a-#Q / M-c->P-a->S / P-b->R", "testKtails10",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a->Q / M-c-#P", "testKtails11",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-a->D-d->E / B-c->F-a->G-e->H / M-b->N-a->Q / M-c->P-a-#S", "testKtails12",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->F-a->G-a->H / A-b->C-a->D-a->E", "testKtails13",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->F-a->G-a->H / T-a->C-a->D-a->E", "testKtails14",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A1-a->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-a->F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15a",testConfig,getLabelConverter());
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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A1-a->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-a-#F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15b",testConfig,getLabelConverter());

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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A1-z->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-z-#F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15c",testConfig,getLabelConverter());

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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A1-a->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-a->F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15a",testConfig,getLabelConverter());

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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A1-a->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-a->F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15a",testConfig,getLabelConverter());

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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A1-a->F1 / A1-b->C1-b->D1 / A1-c->B1-a->E1 / A2-a->F2 / A2-b->C2-b->D2 / A2-c->B2-a->E2", "testKtails15a",testConfig,getLabelConverter());

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
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c-#F", "testSiccoScoring0",testConfig,getLabelConverter());
		for(CmpVertex v:fsm.transitionMatrix.keySet()) v.setColour(JUConstants.RED);
		Assert.assertEquals(-1,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));
		
		// does not report a negative because all states are red.
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}

	@Test
	public final void testSiccoScoring1()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E", "testSiccoScoring1",testConfig,getLabelConverter());
		for(CmpVertex v:fsm.transitionMatrix.keySet()) v.setColour(JUConstants.RED);
		Assert.assertEquals(2,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(2,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));

		// does not report a negative because all states are red.
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}
	
	@Test
	public final void testSiccoScoring2()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c->F / E-a->G / C-b->H", "testSiccoScoring2",testConfig,getLabelConverter());
		for(CmpVertex v:fsm.transitionMatrix.keySet()) v.setColour(JUConstants.RED);
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));

		// does not report a negative because all states are red.
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}
	
	@Test
	public final void testSiccoScoring3a()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c->F / E-a->G / C-b->H / G-b->I", "testSiccoScoring3",testConfig,getLabelConverter());
		for(CmpVertex v:fsm.transitionMatrix.keySet()) v.setColour(JUConstants.RED);
		Assert.assertEquals(-1,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));

		// does not report a negative because all states are red.
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}
	
	// Same as above but states not red
	@Test
	public final void testSiccoScoring3b()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c->F / E-a->G / C-b->H / G-b->I", "testSiccoScoring3",testConfig,getLabelConverter());
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));

		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}

	@Test
	public final void testSiccoScoring4a()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c->F / E-a->G / C-b->H / C-c->I", "testSiccoScoring3",testConfig,getLabelConverter());
		for(CmpVertex v:fsm.transitionMatrix.keySet()) v.setColour(JUConstants.RED);
		Assert.assertEquals(-1,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(-1,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));

		// does not report a negative because all states are red.
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}

	// Same as above but states not red
	@Test
	public final void testSiccoScoring4b()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->B-c->B / A-b->C-a->D-a->E-c->F / E-a->G / C-b->H / C-c->I", "testSiccoScoring3",testConfig,getLabelConverter());
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), true));
		Assert.assertEquals(4,fsm.pairscores.computeScoreSicco(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), false));

		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}

	// This test aims to check the logic of the Sicco checking, hence red-blue marking is not the one that may happen in reality
	@Test
	public final void testSiccoScoring5()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-e->P-a->C-d->E / B-b->B / P-c->D / C-b->F", "testSiccoScoring5",testConfig,getLabelConverter());
		fsm.findVertex("A").setColour(JUConstants.RED);fsm.findVertex("B").setColour(JUConstants.RED);
		fsm.findVertex("D").setColour(JUConstants.BLUE);fsm.findVertex("P").setColour(JUConstants.BLUE);
		
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("P"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("P"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(-2,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("P"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("P"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("P"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}

	@Test
	public final void testSiccoScoring6()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-e->P-a->C-d->E / B-b->B / P-c->D / C-b->F-b->G-b->H-d->E", "testSiccoScoring6",testConfig,getLabelConverter());
		fsm.findVertex("A").setColour(JUConstants.RED);fsm.findVertex("B").setColour(JUConstants.RED);
		fsm.findVertex("D").setColour(JUConstants.BLUE);fsm.findVertex("P").setColour(JUConstants.BLUE);
		
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("P"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("P"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		// c from A and d from B.
		Assert.assertEquals(-2,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("P"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("P"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("P"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}

	@Test
	public final void testSiccoScoring7()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-e->P-a->C-d->E / B-b->B / P-c->D / C-b->F", "testSiccoScoring7",testConfig,getLabelConverter());
		fsm.findVertex("A").setColour(JUConstants.RED);fsm.findVertex("B").setColour(JUConstants.RED);
		fsm.findVertex("C").setColour(JUConstants.BLUE);fsm.findVertex("D").setColour(JUConstants.BLUE);fsm.findVertex("P").setColour(JUConstants.BLUE);
		
		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(-2,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(-2,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("C"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}

	// Here J-e->K-a->L-a->Q-a->R branch will be partly merged into B-e->P-a->C branch and the two 'a' at the end are not merged but ignored because all these states are not coloured.
	// The H-d-E-b->U-b->V branch has 'd' which should be merged into I-D->M-a->S-a->T. The d part is new to A and should be flagged for all the three merger types;
	// the rest of it (starting from a and starting from b are not coloured and hence should be ignored.
	// I will be merged into A and therefore f will be a new one that should be flagged as inconsistent by the S_RED policy but not by S_RED_BLUE or S_ONEPAIR because I is not labelled.
	@Test
	public final void testSiccoScoring8()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-e->P-a->C / B-b->B / P-c->D / B-a->H-a->H-d->E / H-b->F-b->I / I-f->N / I-d->M-a->S-a->T / I-b->J-e->K-a->L-a->Q-a->R", "testSiccoScoring8",testConfig,getLabelConverter());
		fsm.findVertex("A").setColour(JUConstants.RED);fsm.findVertex("B").setColour(JUConstants.RED);
		fsm.findVertex("P").setColour(JUConstants.BLUE);fsm.findVertex("H").setColour(JUConstants.BLUE);

		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(-2,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(-2,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}

	// same as testSiccoScoring8 but H is now now labelled, therefore ignored by S_RED_BLUE.
	@Test
	public final void testSiccoScoring9()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-e->P-a->C / B-b->B / P-c->D / B-a->H-a->H-d->E / H-b->F-b->I / I-f->N / I-d->M-a->S-a->T / I-b->J-e->K-a->L-a->Q-a->R", "testSiccoScoring8",testConfig,getLabelConverter());
		fsm.findVertex("A").setColour(JUConstants.RED);fsm.findVertex("B").setColour(JUConstants.RED);
		fsm.findVertex("P").setColour(JUConstants.BLUE);

		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(-2,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(-2,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}

	// same as testSiccoScoring8 but H is now now labelled, therefore ignored by S_RED_BLUE.
	@Test
	public final void testSiccoScoring10()
	{
		LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-e->P-a->C / B-b->B / B-a->H-a->H-b->F-e->K-c->L / K-d->M/ H-e->E-a->G-a->I / P-b->E-c->J", "testSiccoScoring10",testConfig,getLabelConverter());
		fsm.findVertex("A").setColour(JUConstants.RED);fsm.findVertex("B").setColour(JUConstants.RED);fsm.findVertex("P").setColour(JUConstants.RED);
		fsm.findVertex("H").setColour(JUConstants.BLUE);fsm.findVertex("E").setColour(JUConstants.BLUE);

		Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(-2,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(-1,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}

	@Test
	public final void testSiccoScoring11()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-e->P-a->C / B-b->B / B-a->H-a->H-b->F-e->K-c->L / K-d->M/ H-e->E-a->G-a->I / P-b->E-c->J", "testSiccoScoring10",testConfig,getLabelConverter());
		final Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);

		checkForCorrectException(() -> fsm.pairscores.computeSiccoRejectScoreGeneral(null, collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR),IllegalArgumentException.class,"when looking for a score from a single pair");

		checkForCorrectException(() -> fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(null, collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR),IllegalArgumentException.class,"when looking for a score from a single pair");
	}
	@Test
	public final void testSiccoScoring12()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-e->P-a->C / B-b->B / B-a->H-a->H-b->F-e->K-c->L / K-d->M/ H-e->E-a->G-a->I / P-b->E-c->J", "testSiccoScoring10",testConfig,getLabelConverter());
		final Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		fsm.pairscores.computePairCompatibilityScore_general(new StatePair(fsm.findVertex("H"),fsm.findVertex("A")),null,collectionOfVerticesToMerge, true);

		checkForCorrectException(() -> fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("J"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR),IllegalArgumentException.class,"invalid merge: pair");

		checkForCorrectException(() -> fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("J"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR),IllegalArgumentException.class,"invalid merge: pair");
	}
	
	// testing reject-vertex handling
	@Test
	public final void testSiccoScoring13()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C / D-a->E-b-#F", "testSiccoScoring13",testConfig,getLabelConverter());
		final Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> collectionOfVerticesToMerge = new ArrayList<>();
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("D"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral(new StatePair(fsm.findVertex("D"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));

		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("D"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_ONEPAIR));
		Assert.assertEquals(0,fsm.pairscores.computeSiccoRejectScoreGeneral_fastreturn(new StatePair(fsm.findVertex("D"),fsm.findVertex("A")), collectionOfVerticesToMerge, SiccoGeneralScoring.S_RED));
	}
	
	@SuppressWarnings("static-method")
	@Test
	public final void testcheckMatch1()
	{
		checkForCorrectException(() -> LearningAlgorithms.checkMatch(null, null, null, -1),IllegalArgumentException.class,"k has to be 0 or above");
	}
	
	@Test
	public final void testcheckMatch2a()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B / C-b->D", "testcheckMatch2a",testConfig,getLabelConverter());
		Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("C"), 0));
	}
	
	@Test
	public final void testcheckMatch2b()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B / C-b->D", "testcheckMatch2a",testConfig,getLabelConverter());
		Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("B"), fsm.findVertex("D"), 0));
	}
	
	@Test
	public final void testcheckMatch2c()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a-#T / C-a->D-a->C", "testcheckMatch8",testConfig,getLabelConverter());
		Assert.assertFalse(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("T"), fsm.findVertex("C"), 0));
		Assert.assertFalse(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("B"), fsm.findVertex("D"), 1));
	}		
	
	@Test
	public final void testcheckMatch2d()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a-#T / C-a->D-a->C", "testcheckMatch8",testConfig,getLabelConverter());
		Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("C"), 1));
		Assert.assertFalse(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("C"), 2));
	}
	
	@Test
	public final void testcheckMatch3()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->B-b->C-a->C", "testcheckMatch3",testConfig,getLabelConverter());
		checkForCorrectException(() -> LearningAlgorithms.checkMatch(fsm, fsm.findVertex("B"), fsm.findVertex("A"), 1),IllegalArgumentException.class,"the graph should have traces in it with no branches");
	}
	
	@Test
	public final void testcheckMatch4()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C", "testcheckMatch4",testConfig,getLabelConverter());
		Assert.assertFalse(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("B"), 1));
		Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("A"), 1));
		Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("A"), 2));
		Assert.assertFalse(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("A"), 3));
	}
	
	@Test
	public final void testcheckMatch5()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->B / T-b->C-a->C", "testcheckMatch5",testConfig,getLabelConverter());
		for(int i=0;i<100;++i)
		{
			Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("B"), fsm.findVertex("C"), i));
			Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("C"), i));
			Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("B"), i));
		}
		Assert.assertFalse(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("T"), 1));
		Assert.assertFalse(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("B"), fsm.findVertex("T"), 1));
	}
	@Test
	public final void testcheckMatch6()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->T / C-a->D-a->C", "testcheckMatch6",testConfig,getLabelConverter());
		Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("C"), 1));
		Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("C"), 2));
		Assert.assertFalse(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("C"), 3));
	}		
	
	@Test
	public final void testcheckMatch7()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->T-b->U / C-a->D-a->C / D-b->E", "testcheckMatch7",testConfig,getLabelConverter());
		Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("C"), 1));
		Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("C"), 2));
		Assert.assertFalse(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("C"), 3));
		Assert.assertTrue(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("D"), 3));
		Assert.assertFalse(LearningAlgorithms.checkMatch(fsm, fsm.findVertex("A"), fsm.findVertex("D"), 4));
	}		

	@Test
	public final void testTraditionalKTails1()
	{
		checkForCorrectException(() -> LearningAlgorithms.traditionalKtails(buildSet(new String[][]{},testConfig,getLabelConverter()),buildSet(new String[][]{new String[]{}},testConfig,getLabelConverter()),1, testConfig),IllegalArgumentException.class,"graphs with initial state reject-stat");
	}

	@Test
	public final void testIncrementalKTails1()
	{
		checkForCorrectException(() -> LearningAlgorithms.incrementalKtails(buildSet(new String[][]{},testConfig,getLabelConverter()),buildSet(new String[][]{new String[]{}},testConfig,getLabelConverter()),1, testConfig),IllegalArgumentException.class,"graphs with initial state reject-stat");
	}

	@Test
	public final void testTraditionalKTails2()
	{
		LearnerGraph actual = LearningAlgorithms.traditionalKtails(buildSet(new String[][]{},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1, testConfig);
		Assert.assertEquals(1, actual.getStateNumber());
		Assert.assertEquals(0, actual.pathroutines.countEdges());
	}
	@Test
	public final void testIncrementalKTails2()
	{
		LearnerGraph actual = LearningAlgorithms.incrementalKtails(buildSet(new String[][]{},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1, testConfig);
		Assert.assertEquals(1, actual.getStateNumber());
		Assert.assertEquals(0, actual.pathroutines.countEdges());
	}

	@Test
	public final void testPTAKTails2()
	{
		LearnerGraph actual = LearningAlgorithms.ptaKtails(buildSet(new String[][]{},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1, testConfig);
		Assert.assertEquals(1, actual.getStateNumber());
		Assert.assertEquals(0, actual.pathroutines.countEdges());
	}

	/** How many threads to run in testing of concurrent implementation of ktails. */
	protected final int threadNumber = 4;
	
	@Test
	public final void testConcurrentKTails2()
	{
		LearnerGraph actual = LearningAlgorithms.ptaConcurrentKtails(buildSet(new String[][]{},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1, testConfig,threadNumber,null);
		Assert.assertEquals(1, actual.getStateNumber());
		Assert.assertEquals(0, actual.pathroutines.countEdges());
	}
	
	@Test
	public final void testTraditionalKTails3()
	{
		LearnerGraph actual = LearningAlgorithms.traditionalKtails(buildSet(new String[][]{new String[]{"a","b"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C", "testTraditionalKTails1",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testIncrementalKTails3()
	{
		LearnerGraph actual = LearningAlgorithms.incrementalKtails(buildSet(new String[][]{new String[]{"a","b"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C", "testTraditionalKTails1",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testPTAKTails3()
	{
		LearnerGraph actual = LearningAlgorithms.ptaKtails(buildSet(new String[][]{new String[]{"a","b"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C", "testTraditionalKTails1",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}

	@Test
	public final void testConcurrentKTails3()
	{
		LearnerGraph actual = LearningAlgorithms.ptaConcurrentKtails(buildSet(new String[][]{new String[]{"a","b"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1,testConfig,threadNumber,null);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C", "testTraditionalKTails1",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}

	@Test
	public final void testTraditionalKTails4()
	{
		LearnerGraph actual = LearningAlgorithms.traditionalKtails(buildSet(new String[][]{new String[]{"a","a"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->A", "testTraditionalKTails4",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testPTAKTails4()
	{
		LearnerGraph actual = LearningAlgorithms.ptaKtails(buildSet(new String[][]{new String[]{"a","a"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->A", "testTraditionalKTails4",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testConcurrentKTails4()
	{
		LearnerGraph actual = LearningAlgorithms.ptaConcurrentKtails(buildSet(new String[][]{new String[]{"a","a"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1,testConfig,threadNumber,null);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->A", "testTraditionalKTails4",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testIncrementalKTails4()
	{
		LearnerGraph actual = LearningAlgorithms.incrementalKtails(buildSet(new String[][]{new String[]{"a","a"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),1,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->A", "testTraditionalKTails4",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	// Same traces as testTraditionalKTails4 but no mergers because k is too high. 
	@Test
	public final void testTraditionalKTails5()
	{
		LearnerGraph actual = LearningAlgorithms.traditionalKtails(buildSet(new String[][]{new String[]{"a","a"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),2,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C", "testTraditionalKTails4",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testPTAKTails5()
	{
		LearnerGraph actual = LearningAlgorithms.ptaKtails(buildSet(new String[][]{new String[]{"a","a"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),2,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C", "testTraditionalKTails4",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testConcurrentKTails5()
	{
		LearnerGraph actual = LearningAlgorithms.ptaConcurrentKtails(buildSet(new String[][]{new String[]{"a","a"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),2,testConfig,threadNumber,null);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C", "testTraditionalKTails4",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testIncrementalKTails5()
	{
		LearnerGraph actual = LearningAlgorithms.incrementalKtails(buildSet(new String[][]{new String[]{"a","a"}},testConfig,getLabelConverter()),buildSet(new String[][]{},testConfig,getLabelConverter()),2,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C", "testTraditionalKTails4",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	Collection<List<Label>> traces6 = buildList(new String[][]{
		new String[]{"a","b"},
		new String[]{"a","b"},
		new String[]{"a","a","b","a","b"},
		new String[]{"b","b","a","b"}
	},Configuration.getDefaultConfiguration(),getLabelConverter());
	
	@Test
	public final void testTraditionalKTails6()
	{
		LearnerGraph actual = LearningAlgorithms.traditionalKtails(traces6,buildList(new String[][]{},testConfig,getLabelConverter()),2,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->C-b->A-a->AB-a->AB / AB-b->AC-b->AC-a->AB", "testTraditionalKTails6",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
		
	}
	
	@Test
	public final void testPTAKTails6()
	{
		LearnerGraph actual = LearningAlgorithms.ptaKtails(traces6,buildList(new String[][]{},testConfig,getLabelConverter()),2,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->C-b->A-a->AB-a->AB / AB-b->AC-b->AC-a->AB", "testTraditionalKTails6",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
		
	}
	
	@Test
	public final void testPTAConcurrentKTails6()
	{
		LearnerGraph actual = LearningAlgorithms.ptaConcurrentKtails(traces6,buildList(new String[][]{},testConfig,getLabelConverter()),2,testConfig,threadNumber,null);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->C-b->A-a->AB-a->AB / AB-b->AC-b->AC-a->AB", "testTraditionalKTails6",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
		
	}
	
	@Test
	public final void testIncrementalKTails6()
	{
		LearnerGraph actual = LearningAlgorithms.incrementalKtails(traces6,buildList(new String[][]{},testConfig,getLabelConverter()),2,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->A-a->AP-a->AP-b->QA-a->AP / QA-b->A", "testTraditionalKTails6",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	Collection<List<Label>> traces7 = buildList(new String[][]{
		new String[]{"a","b"},
		new String[]{"a","b"},
		new String[]{"a","a","b","a","b"},
		new String[]{"b","b","a","b"}
	},Configuration.getDefaultConfiguration(),getLabelConverter());
	
	@Test
	public final void testTraditionalKTails7()
	{
		LearnerGraph actual = LearningAlgorithms.traditionalKtails(traces7,buildList(new String[][]{},testConfig,getLabelConverter()),1,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->A-a->A", "testTraditionalKTails7",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testPTAKTails7()
	{
		LearnerGraph actual = LearningAlgorithms.ptaKtails(traces7,buildList(new String[][]{},testConfig,getLabelConverter()),1,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->A-a->A", "testTraditionalKTails7",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testConcurrentKTails7()
	{
		LearnerGraph actual = LearningAlgorithms.ptaConcurrentKtails(traces7,buildList(new String[][]{},testConfig,getLabelConverter()),1,testConfig,threadNumber,null);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->A-a->A", "testTraditionalKTails7",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testIncrementalKTails7()
	{
		LearnerGraph actual = LearningAlgorithms.incrementalKtails(traces7,buildList(new String[][]{},testConfig,getLabelConverter()),1,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->A-a->A", "testTraditionalKTails7",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	Collection<List<Label>> traces8 = buildList(new String[][]{
		new String[]{"a","b"},
		new String[]{"a","b"},
		new String[]{"a","a","b","a","b"},
		new String[]{"b","b","a","b"}
	},Configuration.getDefaultConfiguration(),getLabelConverter());
	
	@Test
	public final void testTraditionalKTails8()
	{
		LearnerGraph actual = LearningAlgorithms.traditionalKtails(traces8,buildList(new String[][]{},testConfig,getLabelConverter()),0,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->A-a->A", "testTraditionalKTails7",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testPTAKTails8()
	{
		LearnerGraph actual = LearningAlgorithms.ptaKtails(traces8,buildList(new String[][]{},testConfig,getLabelConverter()),0,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->A-a->A", "testTraditionalKTails7",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testConcurrentKTails8()
	{
		LearnerGraph actual = LearningAlgorithms.ptaConcurrentKtails(traces8,buildList(new String[][]{},testConfig,getLabelConverter()),0,testConfig,threadNumber,null);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->A-a->A", "testTraditionalKTails7",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	@Test
	public final void testIncrementalKTails8()
	{
		LearnerGraph actual = LearningAlgorithms.incrementalKtails(traces8,buildList(new String[][]{},testConfig,getLabelConverter()),0,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-b->A-a->A", "testTraditionalKTails7",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}
	
	Collection<List<Label>> traces9 = buildList(new String[][]{
		new String[]{"a","b"},
		new String[]{"a","b"},
		new String[]{"a","a","b","a","b"},
		new String[]{"b","b","a","b"}
	},Configuration.getDefaultConfiguration(),getLabelConverter());
	
	@Test
	public final void testTraditionalKTails9()
	{
		LearnerGraph actual = LearningAlgorithms.traditionalKtails(traces9,buildList(new String[][]{},testConfig,getLabelConverter()),3,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->BC-b->E / BC-a->D / A-b->D-b->F-a->G-b->H", "testTraditionalKTails9",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}

	@Test
	public final void testPTAKTails9()
	{
		LearnerGraph actual = LearningAlgorithms.ptaKtails(traces9,buildList(new String[][]{},testConfig,getLabelConverter()),3,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->BC-b->E / BC-a->D / A-b->D-b->F-a->G-b->H", "testTraditionalKTails9",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}

	@Test
	public final void testConcurrentKTails9()
	{
		LearnerGraph actual = LearningAlgorithms.ptaConcurrentKtails(traces9,buildList(new String[][]{},testConfig,getLabelConverter()),3,testConfig,threadNumber,null);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->BC-b->E / BC-a->D / A-b->D-b->F-a->G-b->H", "testTraditionalKTails9",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
	}

	@Test
	public final void testIncrementalKTails9()
	{
		LearnerGraph actual = LearningAlgorithms.incrementalKtails(traces9,buildList(new String[][]{},testConfig,getLabelConverter()),3,testConfig);
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->BC-b->E / BC-a->D / A-b->D-b->F-a->G-b->H", "testTraditionalKTails9",testConfig,getLabelConverter());
		DifferentFSMException ex = WMethod.checkM(fsm,actual);
		Assert.assertNull(ex);
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
	
	@Test
	public final void testComputeInfeasiblePairs1()
	{
		Assert.assertTrue(LearningSupportRoutines.computeInfeasiblePairs(new LearnerGraph(testConfig)).isEmpty());
	}

	@Test
	public final void testComputeInfeasiblePairs2()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->A", "testComputeInfeasiblePairs2",testConfig,getLabelConverter());
		Map<Label,Set<Label>> outcome = LearningSupportRoutines.computeInfeasiblePairs(fsm);
		String outcomeAsText = outcome.toString();
		Assert.assertEquals("{a=[]}",outcomeAsText);
	}

	@Test
	public final void testComputeInfeasiblePairs3()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a-#B", "testComputeInfeasiblePairs3",testConfig,getLabelConverter());
		Map<Label,Set<Label>> outcome = LearningSupportRoutines.computeInfeasiblePairs(fsm);
		String outcomeAsText = outcome.toString();
		Assert.assertEquals("{a=[a]}",outcomeAsText);
	}

	@Test
	public final void testComputeInfeasiblePairs4()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a-#C", "testComputeInfeasiblePairs4",testConfig,getLabelConverter());
		Map<Label,Set<Label>> outcome = LearningSupportRoutines.computeInfeasiblePairs(fsm);
		String outcomeAsText = outcome.toString();
		Assert.assertEquals("{a=[]}",outcomeAsText);
	}

	@Test
	public final void testComputeInfeasiblePairs5()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-a->C", "testComputeInfeasiblePairs4",testConfig,getLabelConverter());
		Map<Label,Set<Label>> outcome = LearningSupportRoutines.computeInfeasiblePairs(fsm);
		String outcomeAsText = outcome.toString();
		Assert.assertEquals("{a=[]}",outcomeAsText);
	}

	@Test
	public final void testComputeInfeasiblePairs6()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b-#C", "testComputeInfeasiblePairs5",testConfig,getLabelConverter());
		Map<Label,Set<Label>> outcome = LearningSupportRoutines.computeInfeasiblePairs(fsm);
		String outcomeAsText = outcome.toString();
		Assert.assertEquals("{a=[a], b=[a, b]}",outcomeAsText);// this outcome means that both "a" and "b" cannot follow themselves and b cannot follow a. 
	}
	
	@Test
	public final void testComputeInfeasiblePairs7()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b-#C / A-b->A", "testComputeInfeasiblePairs5",testConfig,getLabelConverter());
		Map<Label,Set<Label>> outcome = LearningSupportRoutines.computeInfeasiblePairs(fsm);
		String outcomeAsText = outcome.toString();
		Assert.assertEquals("{a=[a], b=[]}",outcomeAsText);// this outcome means that both "a" and "b" cannot follow themselves and b cannot follow a. 
	}

	@Test
	public final void testEvaluateProgressProperty1()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty1",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty1",testConfig,getLabelConverter());
		Assert.assertTrue(evaluateProgressProperty(fsm,pta));
	}

	@Test
	public final void testEvaluateProgressProperty2()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty1",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B", "testEvaluateProgressProperty2",testConfig,getLabelConverter());
		Assert.assertTrue(evaluateProgressProperty(fsm,pta));
	}

	@Test
	public final void testEvaluateProgressProperty3()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->E", "testEvaluateProgressProperty3",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty2",testConfig,getLabelConverter());
		Assert.assertTrue(evaluateProgressProperty(fsm,pta));
	}

	@Test
	public final void testEvaluateProgressProperty4()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->E-b->E", "testEvaluateProgressProperty4",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty2",testConfig,getLabelConverter());
		Assert.assertTrue(evaluateProgressProperty(fsm,pta));
	}

	@Test
	public final void testEvaluateProgressProperty5()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->E-b->E-c->G", "testEvaluateProgressProperty5",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-b->F", "testEvaluateProgressProperty2",testConfig,getLabelConverter());
		Assert.assertTrue(evaluateProgressProperty(fsm,pta));
	}
	@Test
	public final void testEvaluateProgressProperty6()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->E-b->E-c->G", "testEvaluateProgressProperty6",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty2",testConfig,getLabelConverter());
		Assert.assertTrue(evaluateProgressProperty(fsm,pta));
	}
	@Test
	public final void testEvaluateProgressProperty7()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->E-b->E-c->G", "testEvaluateProgressProperty7",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-b->F-a->G", "testEvaluateProgressProperty2",testConfig,getLabelConverter());
		Assert.assertFalse(evaluateProgressProperty(fsm,pta));
	}
	@Test
	public final void testEvaluateProgressProperty8()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->E-b->E-c->G", "testEvaluateProgressProperty8a",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F-b->G", "testEvaluateProgressProperty8b",testConfig,getLabelConverter());
		Assert.assertFalse(evaluateProgressProperty(fsm,pta));
	}
	@Test
	public final void testEvaluateProgressProperty9()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->E-b->E-c->G-b->G", "testEvaluateProgressProperty9a",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F-c->G-b->H-b->I", "testEvaluateProgressProperty9b",testConfig,getLabelConverter());
		Assert.assertTrue(evaluateProgressProperty(fsm,pta));
	}
	@Test
	public final void testEvaluateProgressProperty10()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->D-a->F", "testEvaluateProgressProperty10",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty2",testConfig,getLabelConverter());
		Assert.assertTrue(evaluateProgressProperty(fsm,pta));
	}

	@Test
	public final void testEvaluateProgressProperty11()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->D-b->D-a->E", "testEvaluateProgressProperty11",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty2",testConfig,getLabelConverter());
		Assert.assertTrue(evaluateProgressProperty(fsm,pta));
	}

	@Test
	public final void testEvaluateProgressProperty12()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->D-b->D-a->E", "testEvaluateProgressProperty12a",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty12b",testConfig,getLabelConverter());
		Assert.assertTrue(evaluateProgressProperty(fsm,pta));
	}

	@Test
	public final void testEvaluateProgressProperty13()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->D-b->D-a->D", "testEvaluateProgressProperty13a",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty13b",testConfig,getLabelConverter());
		Assert.assertFalse(evaluateProgressProperty(fsm,pta));
	}

	@Test
	public final void testEvaluateProgressProperty14()
	{
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->A-b->A", "testEvaluateProgressProperty14",testConfig,getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty2",testConfig,getLabelConverter());
		Assert.assertFalse(evaluateProgressProperty(fsm,pta));
	}

	@Test
	public final void testEvaluateProgressProperty15() {
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E", "testEvaluateProgressProperty15", testConfig, getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E-a->F", "testEvaluateProgressProperty1", testConfig, getLabelConverter());
		checkForCorrectException(() -> evaluateProgressProperty(fsm,pta), IllegalArgumentException.class, "is not a subset of the graph being learnt");
	}

	@Test
	public final void testEvaluateProgressProperty16() {
		final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->E", "testEvaluateProgressProperty15", testConfig, getLabelConverter());
		final LearnerGraph pta = FsmParserStatechum.buildLearnerGraph("A-a->B-b->C-b->D-b->A", "testEvaluateProgressProperty1", testConfig, getLabelConverter());
		checkForCorrectException(() -> evaluateProgressProperty(fsm,pta), IllegalArgumentException.class, "contains a loop");
	}

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
