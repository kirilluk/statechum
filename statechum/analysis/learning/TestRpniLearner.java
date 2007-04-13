package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static statechum.analysis.learning.TestFSMAlgo.buildSet;

import java.awt.Point;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import javax.swing.SwingUtilities;

import junit.framework.AssertionFailedError;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.computeStateScores.PairScore;
import statechum.xmachine.model.testset.WMethod;
import sun.reflect.generics.scope.Scope;

import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class TestRpniLearner extends RPNIBlueFringeLearnerTestComponent
{
	public TestRpniLearner() {
		super(null);
	}

	protected int checkPath(FSMStructure expected, DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
	{
		int answer = WMethod.tracePath(expected, question);
		//System.out.println("questions: "+question+" answer "+answer);
		return answer;
	}

	protected void checkLearner(String fsmString, String [][] plus, String [][] minus)
	{
		final DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsmString, "sample FSM");
		final DirectedSparseGraph completedGraph = (DirectedSparseGraph)g.copy();TestFSMAlgo.completeGraph(completedGraph, "REJECT");
		final FSMStructure expected = WMethod.getGraphData(g);
		
		updateFrame(g, g);

		// now sanity checking on the plus and minus sets
		for(String [] path:plus)
			assert RPNIBlueFringeLearner.USER_ACCEPTED == WMethod.tracePath(expected, Arrays.asList(path));
		for(String [] path:minus)
			assert RPNIBlueFringeLearner.USER_ACCEPTED != WMethod.tracePath(expected, Arrays.asList(path));
		
		RPNIBlueFringeLearnerTestComponent l = new RPNIBlueFringeLearnerTestComponentOpt(visFrame)
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
		l.addObserver(visFrame);
		try{
			DirectedSparseGraph learningOutcome = l.learnMachine(RPNIBlueFringeLearner.initialise(), buildSet(plus), buildSet(minus));
			updateFrame(learningOutcome,g);
			FSMStructure learntStructure = WMethod.getGraphData(learningOutcome);
			System.out.println(l.getQuestionCounter());
			//TestFSMAlgo.checkM(learntStructure,completedGraph,learntStructure.init,expected.init);
		}
		catch(InterruptedException e){
			AssertionFailedError th = new AssertionFailedError("interrupted exception received");th.initCause(e);throw th;
		};
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
	
	/** Holds the JFrame to see the graphs being dealt with. Usage:
	 * <pre>
	 * 		updateFrame(g);// a public method
	 * </pre>
	 * where <i>g</i> is the graph to be displayed.
	 */
	protected static Visualiser visFrame = null;
	
	/** Displays twos graphs passed as arguments in the Jung window.
	 * @param g the graph to display 
	 * @param lowerGraph the graph to display below it
	 */
	public void updateFrame(final DirectedSparseGraph g,final DirectedSparseGraph lowerGraph)
	{
		if (!debugMode)
			return;		
		
		visFrame.update(null, g);
		if (lowerGraph != null)
		{
			try {// I'm assuming here that Swing has only one queue of threads to run on the AWT thread, hence the
				// thread scheduled by invokeLater will be run to completion before the next one (below) runs and hence
				// I rely on the results of execution of the above thread below in order to position the window.
				SwingUtilities.invokeAndWait(new Runnable() 
				{
					public void run()
					{
						Visualiser v=new Visualiser();v.update(null, lowerGraph);
						Point newLoc = visFrame.getLocation();newLoc.move(0, visFrame.getHeight());v.setLocation(newLoc);
					}
				});
			} catch (InterruptedException e) {
				// cannot do much about this
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// cannot do much about this
				e.printStackTrace();
			}
		}
	}
	
	/** Checks if the supplied vertex is an accept one or not.
	 * 
	 * @param v vertex to check
	 * @return true if the vertex is an accept-vertex
	 */
	public final static boolean isAccept(final Vertex v)
	{
		if (v.getUserDatum(JUConstants.ACCEPTED).toString().equalsIgnoreCase("true"))
			return true;
		else
			return false;
	}

	static protected PairScore constructPairScore(String a,String b, int score)
	{
		DirectedSparseVertex aV = new DirectedSparseVertex(), bV = new DirectedSparseVertex();
		aV.addUserDatum(JUConstants.LABEL, a, UserData.SHARED);
		bV.addUserDatum(JUConstants.LABEL, b, UserData.SHARED);
		return new computeStateScores.PairScore(aV,bV, score);
	}

	static protected void checkLess(String a, String b, int abS, String c, String d, int cdS)
	{
		StatePair p = constructPairScore(a,b,abS), q=constructPairScore(c,d,cdS);
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}
	
	@Test
	public void testPairScoreEquality()
	{
		StatePair p = constructPairScore("a","b",4), q=constructPairScore("a","b",4);
		assertTrue(p.equals(p));
		assertTrue(p.equals(q));
		assertFalse(p.equals(null));
		assertFalse(p.equals("test"));
		assertFalse(p.equals(constructPairScore("a","c",4)));
		assertFalse(p.equals(constructPairScore("a","b",6)));
		assertFalse(p.equals(constructPairScore("b","b",4)));
	}
	
	@Test
	public void testStatePairScoreComparison()
	{
		checkLess("a","b",4,"a","b",6);
		checkLess("z","z",4,"a","b",6);
		checkLess("a","b",4,"z","z",6);

		checkLess("a","b",4,"c","d",4);
		checkLess("a","b",4,"a","c",4);
		checkLess("a","b",4,"c","b",4);
	}

	/** Checks that both the old and the new algorithm reports a pair of states as incompatible. */
	public final void testNewLearnerIncompatible(String fsm)
	{
		DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsm, "testNewLearner");
		computeStateScores s = new computeStateScores(g,"SINK");
		StatePair pair = new StatePair(findVertex(JUConstants.LABEL, "B", g),findVertex(JUConstants.LABEL, "A", g));
		doneEdges = new HashSet();
		int origScore = computeScore(g, pair),
			newScoreA = s.computeStateScore(pair);
		Assert.assertEquals(-1, origScore);
		Assert.assertEquals(-1, newScoreA);
	}
	
	/** Checks that both the old and the new algorithm report the same score for a pair of states and ask the same questions. */
	public final void testNewLearnerQuestions(String fsm, int expectedScore)
	{
		DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsm, "testNewLearner");
		computeStateScores 	s = new computeStateScores(g,"SINK");
		StatePair pair = new StatePair(findVertex(JUConstants.LABEL, "B", g),findVertex(JUConstants.LABEL, "A", g));
		DirectedSparseGraph temp = mergeAndDeterminize((Graph)g.copy(), pair),
			tempB = computeStateScores.mergeAndDeterminize(g, pair);
		
		//setDebugMode(true);updateFrame(g, temp);

		// Now check that computeStateScores properly does  mergeAndDeterminize 
		// (on the test data we are dealing with in these tests, there are separate tests for mergeAndDeterminize)
		FSMStructure tempG = WMethod.getGraphData(temp), tempBG = WMethod.getGraphData(tempB);
		Assert.assertEquals(false, WMethod.checkUnreachableStates(tempG));Assert.assertEquals(false, WMethod.checkUnreachableStates(tempBG));
		Assert.assertEquals(true, TestFSMAlgo.checkMBoolean(tempG, tempBG, tempG.init, tempBG.init));
		
		doneEdges = new HashSet();
		int origScore = computeScore(g, pair),
			newScoreA = s.computeStateScore(pair),
			newScoreB = s.computePairCompatibilityScore(pair);
		Collection<List<String>> 
			questionsA = s.computeQS(pair,temp),
			// Since computeQS assumes that red names remain unchanged in the merged version, I have to use a specific merging procedure
			questionsB = s.computeQS(pair, computeStateScores.mergeAndDeterminize(new computeStateScores(g,"SINK"), pair));
				
		Assert.assertTrue("these states should be compatible - correct test data",origScore >= 0);
		//System.out.println("computed scores, orig: "+origScore+" and the new one is "+newScoreA);
		Assert.assertEquals(expectedScore, origScore);
		Assert.assertEquals(expectedScore, newScoreA);
		Assert.assertTrue( expectedScore < 0? (newScoreB < 0):(newScoreB >= 0));
		if (expectedScore != -1)
		{
			
			Set<List<String>> oldQuestions = new HashSet<List<String>>();oldQuestions.addAll(generateQuestions(g,temp, pair));
			//Assert.assertTrue(oldQuestions.size() > 0);
			//Set<List<String>> newQuestionsA = new HashSet<List<String>>();newQuestionsA.addAll(questionsA);
			Set<List<String>> newQuestionsB = new HashSet<List<String>>();newQuestionsB.addAll(questionsB);
			//Assert.assertTrue("different questions: old "+oldQuestions+", new "+questionsA,oldQuestions.equals(newQuestionsA));
			Assert.assertTrue("different questions: old "+oldQuestions+", new "+questionsB,oldQuestions.equals(newQuestionsB));
		}
	}
	
	public static final String PTA1 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2\nB1-b->B3-b->B4\n";

	@Test
	public final void testFindVertex0()
	{
		computeStateScores s = new computeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testFindVertex"),"SINK");
		Assert.assertNull(s.findVertex("Z"));
		Assert.assertEquals("A", s.findVertex("A").getUserDatum(JUConstants.LABEL));
		Assert.assertEquals("C", s.findVertex("C").getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public final void testFindVertex1()
	{
		computeStateScores s = new computeStateScores();
		Assert.assertNull(s.findVertex("Z"));
		Assert.assertEquals("Init", s.findVertex("Init").getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public final void testComputePathsToRed0a()
	{
		computeStateScores s = new computeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testComputePathsToRed1"),"SINK");
		Set<List<String>> expected = buildSet(new String[][] {
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(new DirectedSparseVertex()));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void testComputePathsToRed0b()
	{
		computeStateScores s = new computeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testComputePathsToRed1"),"SINK");
		Set<List<String>> expected = buildSet(new String[][] {
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(null));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void testComputePathsToRed1()
	{
		computeStateScores s = new computeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testComputePathsToRed1"),"SINK");
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(s.findVertex("A")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void testComputePathsToRed2()
	{
		computeStateScores s = new computeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\n", "testComputePathsToRed1"),"SINK");
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {"a","b"}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(s.findVertex("C")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void testComputePathsToRed3()
	{
		computeStateScores s = new computeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C", "testComputePathsToRed1"),"SINK");
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {"a","b"},
				new String[] {"a","d"},
				new String[] {"c","b"},
				new String[] {"c","d"}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(s.findVertex("C")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test
	public final void testComputePathsToRed4()
	{
		computeStateScores s = new computeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-a->A\nA-c->B-d->C\nA-p->C\nA-q->C", "testComputePathsToRed1"),"SINK");
		Set<List<String>> expected = buildSet(new String[][] {
				new String[] {"p"},
				new String[] {"q"}
			}), 
			actual = new HashSet<List<String>>();actual.addAll(s.computePathsToRed(s.findVertex("C")));
			
		Assert.assertTrue("expected: "+expected+", actual: "+actual, expected.equals(actual));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testLearnerFailsWhenRedNotFound()
	{
		new computeStateScores().computeQS(new StatePair(null,new DirectedSparseVertex()), new computeStateScores());
	}
	
	@Test
	public final void testNewLearner0()
	{
		testNewLearnerIncompatible("A-a->A1\nA-p->P1-q-#B");
	}
	
	@Test
	public final void testNewLearner1a()
	{
		testNewLearnerIncompatible("A-a->A"+PTA1);
	}

	@Test
	public final void testNewLearner1b()
	{
		testNewLearnerQuestions("A-a->A"+"\nA-p->I-q->B"+"\nB-a->B1-a->B2\nB1-b->B3-b->B4\n",2);
	}
	
	@Test
	public final void testNewLearner2()
	{
		testNewLearnerIncompatible("A-a->A1-a->A2"+PTA1);
	}

	@Test
	public final void testNewLearner3()
	{
		testNewLearnerQuestions("A-a->A1-b->A1"+PTA1,3);
	}
	
	@Test
	public final void testNewLearner4()
	{
		testNewLearnerQuestions("A-a->A1-b->A1-a-#A2"+PTA1,4);
	}
	
	@Test
	public final void testNewLearner5()
	{
		testNewLearnerIncompatible("A-a->A1-a-#ARej\nA1-b-#A2"+PTA1);
	}

	@Test
	public final void testNewLearner6()
	{
		testNewLearnerIncompatible("A-a->A1-a-#ARej\nA1-b->A2-b-#A3"+PTA1);
	}
	
	@Test
	public final void testNewLearner7()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-b->A2-b->A3"+PTA1,4);
	}
	
	public static final String PTA2 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2\nB1-b->B3-b->B4\nB1-c->BB1-c->BB2\n";
	
	@Test
	public final void testNewLearner8()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-b->A2\nA1-c->A1"+PTA2,5);
	}
	
	@Test
	public final void testNewLearner9()
	{
		testNewLearnerIncompatible("A-a->A1-a-#ARej\nA1-b->A2\nA1-c-#A3"+PTA2);
	}
	
	@Test
	public final void testNewLearner10()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-b->A2\nA1-c->A3"+PTA2,4);
	}
	
	@Test
	public final void testNewLearner11()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-b->A1\nA1-c->A3"+PTA2,5);
	}
	
	protected static final String PTA_4 = "\nB1-d->B3a-d->B4a-c->B5a-c->B6a\nB3a-c->B4c-c->B5c-c->B6c\nB3b-d->B4d-c->B5d-c->B6d\nB1-c->B3b-c->B4b-c->B5b-c->B6b\n";
	public static final String PTA3 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2"+PTA_4;
	
	@Test
	public final void testNewLearner_2_1()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2\nA1-c->A1"+PTA3,8);
	}
	
	@Test
	public final void testNewLearner_2_2()
	{
		testNewLearnerIncompatible("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2\nA1-c-#A3"+PTA3);
	}
	
	@Test
	public final void testNewLearner_2_3()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2\nA1-c->A3"+PTA3,4);
	}
	
	@Test
	public final void testNewLearner_2_4()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A3"+PTA3,7);
	}
	
	@Test
	public final void testNewLearner_2_6()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A1"+PTA3,16);
	}
	
	@Test
	public final void testNewLearner_2_7()
	{
		testNewLearnerQuestions("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+PTA3,8);
	}

	@Test
	public final void testNewLearner_3_1()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A2\nA1-c->A1"+PTA3,8);
	}
	
	@Test
	public final void testNewLearner_3_2()
	{
		testNewLearnerIncompatible("A-a->A1-a-#ARej\nA1-d->A2\nA1-c-#A3"+PTA3);
	}
	
	@Test
	public final void testNewLearner_3_3()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A2\nA1-c->A3"+PTA3,4);
	}
	
	@Test
	public final void testNewLearner_3_4()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A3"+PTA3,7);
	}
	
	@Test
	public final void testNewLearner_3_5a()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A3-c->A3"+PTA3,13);
	}
	
	@Test
	public final void testNewLearner_3_5b()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A3-c->A4"+PTA3,10);
	}
	
	@Test
	public final void testNewLearner_3_6()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A1\nA1-c->A1"+PTA3,16);
	}
	
	@Test
	public final void testNewLearner_3_7()
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+PTA3,8);
	}

	@Test
	public final void testNewLearner_4_1() // red and blue are adjacent
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A2\nA1-c->A1"+"\nA-p->B"+"\nB-a->B1-a-#B2"+PTA_4,8);
	}
	
	@Test
	public final void testNewLearner_4_2() // blue node has no access successors
	{
		testNewLearnerQuestions("A-d->A1\nA1-d->A2\nA1-c->A1"+"\nA-p->Atmp-q->B"+"\nB2#-c-B-a-#B1\n",0);
	}
	
	@Test
	public final void testNewLearner_4_3() // testing of folding of a long path into a complex machine
	{
		testNewLearnerQuestions("S-d->A-a->S\nA-b->B-a->D-a->E\nS-a->P-b->Q-b->P-t->R",2);
	}

	@Test
	public final void testNewLearner_4_4() // testing that loops around the red states are being dealt with correctly
	{
		testNewLearnerQuestions("S-m->A\nS-n->A-d->A-a->A1\nA-b->A2\nA-c->B-c->B1-p->B2",1);
	}

	@Test
	public final void testNewLearner_4_4_simple() // a simplified version of test 4_4
	{
		testNewLearnerQuestions("S-m->A\nA-d->A-a->A1\nA-b->A2\nA-c->B-c->B1-p->B2",1);
	}

	@Test
	public final void testNewLearner_4_5() // testing that shortest paths actually work
	{
		testNewLearnerQuestions("A-a->A1-c->A2\nA-b->A2\nA-n->B-a->B1-c->B2-p->B3-f->B4-g->B5",2);
	}

	@Test
	public final void testNewLearner_4_6() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-c->A2\nA-b->A1-d->A2\nA-n->B\nB-a->B1-c->B3\nB-b->B2-d->B4-p->B5\nB1-d->B6-f->B7",5);
	}

	@Test
	public final void testNewLearner_4_7() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-c->A2\nA-b->A1-d->A2\nA-n->An-n->B\nAn-m->B\nB-a->B1-c->B3\nB-b->B2-d->B4-p->B5",4);
	}

	@Test
	public final void testNewLearner_4_8a() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-c->A2\nA-b->A1-d->A2\nA-n->An-n->B\nAn-k->A\nA-j->B\nA-v->P-l->P1\nA-u->P\nAn-m->B\nB-a->B1-c->B3\nB-b->B2-d->B4-p->B5",4);
	}

	@Test
	public final void testNewLearner_4_8b() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-c->A2\nA-b->A1-d->A2\nA-n->An-n->B\nAn-k->A\nA-j->Atmp-j->B\nA-v->P-l->P1\nA-u->P\nAn-m->B\nB-a->B1-c->B3\nB-b->B2-d->B4-p->B5",4);
	}

	@Test
	public final void testNewLearner_4_9() // testing that different paths through a PTA which correspond to the same path through a merged machine are handled correctly
	{
		testNewLearnerQuestions("S-n->A-a->A1-c->A2\nA-b->A1-d->A2\nA-n->B\nB-a->B1-c->B3\nB-b->B2-d->B4-p->B5\nA2-r->A3-r->A4-i->A5",4);
	}
	
	@Test
	public final void testGetTempRed1()
	{
		DirectedSparseGraph a=TestFSMAlgo.buildGraph("A-a->B", "testGetTempRed1 model"),
			temp=TestFSMAlgo.buildGraph("C-d->Q", "testGetTempRed1 temp");
		Vertex foundA = computeStateScores.getTempRed_DijkstraShortestPath(a, findVertex(JUConstants.PROPERTY, JUConstants.INIT, a), temp);
		Vertex foundB =RPNIBlueFringeLearnerTestComponent.getTempRed(a, findVertex(JUConstants.PROPERTY, JUConstants.INIT, a), temp);
		Vertex foundC = new computeStateScores(a,"SINK").getTempRed_internal(findVertex(JUConstants.PROPERTY, JUConstants.INIT, a), temp);
		Assert.assertTrue(findVertex(JUConstants.PROPERTY, JUConstants.INIT, temp).equals(foundA));
		Assert.assertTrue(findVertex(JUConstants.PROPERTY, JUConstants.INIT, temp).equals(foundB));
		Assert.assertTrue(findVertex(JUConstants.PROPERTY, JUConstants.INIT, temp).equals(foundC));
	}
	
	@Test
	public final void testGetTempRed2()
	{
		DirectedSparseGraph a=TestFSMAlgo.buildGraph("A-a->B-a->B-c->C-c->D", "testGetTempRed1 model"),
			temp=TestFSMAlgo.buildGraph("C-a->Q-a->Q-c->Q", "testGetTempRed1 temp");
		Vertex foundA = computeStateScores.getTempRed_DijkstraShortestPath(a, findVertex(JUConstants.LABEL, "D", a), temp);
		Vertex foundB = RPNIBlueFringeLearnerTestComponent.getTempRed(a, findVertex(JUConstants.LABEL, "D", a), temp);
		Vertex foundC = new computeStateScores(a,"SINK").getTempRed_internal( findVertex(JUConstants.LABEL, "D", a), temp);
		Assert.assertTrue(findVertex(JUConstants.LABEL, "Q", temp).equals(foundA));
		Assert.assertTrue(findVertex(JUConstants.LABEL, "Q", temp).equals(foundB));
		Assert.assertTrue(findVertex(JUConstants.LABEL, "Q", temp).equals(foundC));
	}
	
	@Test
	public final void testCopyGraph0()
	{
		DirectedSparseGraph g=new DirectedSparseGraph();
		g.addVertex(new DirectedSparseVertex());
		g.addVertex(new DirectedSparseVertex());
		DirectedSparseGraph copy = computeStateScores.copy(g);
		Assert.assertTrue(copy.getEdges().isEmpty() && copy.getVertices().isEmpty());
	}
	
	// TODO to test FSMStructure's equals
	@Test
	public final void testCopyGraph1()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-a->S1", "testCopyGraph");
		DirectedSparseGraph copy=computeStateScores.copy(g);
		FSMStructure gS = WMethod.getGraphData(g),gC = WMethod.getGraphData(copy);
		
		Assert.assertTrue(gS.equals(gC));
	}
	
	@Test
	public final void testCopyGraph2()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+PTA3, "testCopyGraph");
		DirectedSparseGraph copy=computeStateScores.copy(g);
		FSMStructure gS = WMethod.getGraphData(g),gCopy = WMethod.getGraphData(copy);
		
		Assert.assertTrue(gS.equals(gCopy));
		
		// now test if all clones are faithful
		for(Edge e:(Set<Edge>)g.getEdges())
			((Set<String>)e.getUserDatum(JUConstants.LABEL)).add("junk");
		
		FSMStructure gS_Modified = WMethod.getGraphData(copy);
		
		Assert.assertTrue(gS_Modified.equals(gCopy));
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
		StatePair expected = new StatePair(b,d),
			actualA = RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertTrue("expected: "+expected+" got: "+actualA,expected.equals(actualA));
	}
	
	@Test
	public final void findMergablePair3a()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-p->A-a->S\nA-b->S\nA-c->D\nA-b->D\nA-d->E", "findMergablePair3a");
		Vertex 
			s = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "S", g),
			d = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "D", g);
		StatePair expected = new StatePair(d,s),
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
		StatePair expected = new StatePair(b,s),
		actualA = RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertTrue("expected: "+expected+" got: "+actualA,expected.equals(actualA));
	}

	@Test
	public final void findMergablePair4()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-p->A-a->B\nA-b->B\nA-c-#D\nA-b-#D\nA-d->E", "findMergablePair4");
		StatePair actualA = RPNIBlueFringeLearner.findMergablePair(g);
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
		
		StatePair pair = new StatePair(b,a);
		
		FSMStructure 
			mergeResultA = WMethod.getGraphData(new RPNIBlueFringeLearner(null).mergeAndDeterminize(g, pair)), 
			mergeResultB = WMethod.getGraphData(computeStateScores.mergeAndDeterminize(g2, pair)),
			mergeResultC = WMethod.getGraphData(computeStateScores.mergeAndDeterminize(new computeStateScores(g,"SINK"), pair).getGraph()),
			expectedMachine = WMethod.getGraphData(TestFSMAlgo.buildGraph(expectedFSM, "expected machine"));

		TestFSMAlgo.checkM(g2, machineToMerge);
		
		Assert.assertFalse("unreachable states - original",WMethod.checkUnreachableStates(mergeResultA));
		Assert.assertFalse("unreachable states",WMethod.checkUnreachableStates(mergeResultB));
		Assert.assertFalse("unreachable states",WMethod.checkUnreachableStates(mergeResultC));
		Assert.assertFalse("unreachable states",WMethod.checkUnreachableStates(expectedMachine));
		
		if (checkWithEquals)
		{
			Assert.assertTrue("incorrect merging - original",expectedMachine.equals(mergeResultA));
			Assert.assertTrue("incorrect merging",expectedMachine.equals(mergeResultB));
			Assert.assertTrue("incorrect merging",expectedMachine.equals(mergeResultC));
		}
		else
		{
			Assert.assertTrue("incorrect merging - original",TestFSMAlgo.checkMBoolean(mergeResultA, expectedMachine, mergeResultA.init, expectedMachine.init));
			Assert.assertTrue("incorrect merging - first improved",TestFSMAlgo.checkMBoolean(mergeResultB, expectedMachine, mergeResultB.init, expectedMachine.init));
			Assert.assertTrue("incorrect merging - most improved",TestFSMAlgo.checkMBoolean(mergeResultC, expectedMachine, mergeResultC.init, expectedMachine.init));
		}
	}

	@Test
	public final void testMerge1a()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-p->A-a->S\nA-b->S\nA-c->D\nA-b->D\nA-d->E\nS-n->U", "testMerge");
		Vertex 
			s = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "S", g),
			d = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "U", g);
		StatePair pair = new StatePair(d,s);
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

	@Test(expected = IllegalArgumentException.class)
	public final void testMerge_fail1()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph(largeGraph1_invalid5,"testMerge10");
		Vertex 
		a = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "A", g),
		b = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g);
		StatePair pair = new StatePair(b,a);// A is red
		computeStateScores.mergeAndDeterminize(g, pair);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testMerge_fail2()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph(largeGraph1_invalid5,"testMerge10");
		Vertex 
		a = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "A", g),
		b = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g);
		StatePair pair = new StatePair(b,a);// A is red
		computeStateScores.mergeAndDeterminize(new computeStateScores(g,"SINK"), pair);
	}

	protected interface InterfaceChooserToTest {
		Stack<StatePair> choosePairs();
	}

	/**
	 *  Tests the pair selection function of both the original and the optimized implementation.
	 *  <p>
	 * Important: the improved and the original versions of score computation are only compatible if 
	 * the machine has no parallel edges - these are the only allowed machines in testss of
	 * the choice of states.
	 * 
	 * @param fsm the graph to choose states in
	 * @param initialReds the initial set of reds
	 * @param expectedReds the expected reds
	 * @param expectedPairs a set of pairs which has to be returned
	 */
	public final void testChooseStatePairs(String fsm, String [] initialReds, String [] expectedReds, List<PairScore> expectedPairs)
	{
		final DirectedSparseGraph gB = TestFSMAlgo.buildGraph(fsm, "testChooseStatePairs_Ref");
		// check how the reference pair selection function performs
		testChooseStatePairsInternal(gB, initialReds, expectedReds, expectedPairs, new InterfaceChooserToTest() {
			public Stack<StatePair> choosePairs() {
				return chooseStatePairs(gB, new HashSet<List<String>>(), new HashSet<List<String>>());
			}
		});

		final DirectedSparseGraph gA = TestFSMAlgo.buildGraph(fsm, "testChooseStatePairs_Opt");

		// check how the revised pair selection function performs
		final computeStateScores s = new computeStateScores(gA,"SINK");
		testChooseStatePairsInternal(gA, initialReds, expectedReds, expectedPairs, new InterfaceChooserToTest() {
			public Stack<StatePair> choosePairs() {
				return s.chooseStatePairs();
			}
		});
	}
	
	private final void testChooseStatePairsInternal(DirectedSparseGraph g, String [] initialReds, String [] expectedReds, List<PairScore> expectedPairs,InterfaceChooserToTest chooser)
	{
		for(String red:initialReds)
		{
			Vertex v = findVertex(JUConstants.LABEL, red, g);v.removeUserDatum("colour");v.addUserDatum("colour", "red", UserData.SHARED);
		}
		Stack<StatePair> pairs = chooser.choosePairs();
		Map<Integer,Set<PairScore>> distribution = new HashMap<Integer,Set<PairScore>>();// maps scores to sets of states which should correspond to them. The aim is to verify the contents of the stack regardless of the order in which elements with the same score are arranged.

		Set<String> expectedRedsAsSet = new HashSet<String>();expectedRedsAsSet.addAll(Arrays.asList(expectedReds));
		Set<String> finalReds = new HashSet<String>();
		for(Vertex red:findVertices("colour", "red", g))
				finalReds.add((String)red.getUserDatum(JUConstants.LABEL));
		Assert.assertTrue("expected red states: "+expectedRedsAsSet+" actual : "+finalReds,expectedRedsAsSet.equals(finalReds));
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
			int currentScore = computeScore(g, elem);
			PairScore elA = new PairScore(elem.getQ(),elem.getR(),currentScore);
			PairScore elB = new PairScore(elem.getR(),elem.getQ(),currentScore);
			Assert.assertTrue(elem.getR().getUserDatum("colour").equals("red"));
			Assert.assertTrue(elem.getQ().getUserDatum("colour").equals("blue"));
			Assert.assertTrue(currentScore >= 0);
			Assert.assertTrue(distribution.containsKey(currentScore));
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
				"A-a-#Arej\nA-d->A2-c->A2\n"+
				"A-p->P1-a->P2\n"+
				"A-s->S1-d->S2-c->S3-c->S4\n"+
				"A-r->R1-d->R2-c->R3\n"+
				"A-u->U1-d->U2-c->U3\n"+
				"A-q->Q1-d->Q2",
				new String[]{"A"},
				new String[]{"A","P1","Arej"},
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
				"A-a-#Arej\nA-d->A2-c->A2\n"+
				"A-p->P1-a->P2\n"+"P1-b-#P3\n"+
				"A-s->S1-d->S2-c->S3-c->S4\n"+
				"A-r->R1-d->R2-c->R3\n"+
				"A-u->U1-d->U2-c->U3\n"+
				"A-q->Q1-d->Q2",
				new String[]{"A"},
				new String[]{"A","P1","Arej"},
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
				new String[]{"A","B1","REJ2"},
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
		StatePair pair = new StatePair(
				RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g),
				RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "A", g));
		computeStateScores s = new computeStateScores(g,"SINK");

		doneEdges = new HashSet();
		int origScore = computeScore(g, pair),
			newScoreA = s.computeStateScore(pair),
			newScoreB = s.computePairCompatibilityScore(pair);
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

	@Test
	public final void testGetVertex1()
	{
		computeStateScores score = new computeStateScores(TestFSMAlgo.buildGraph("A-a->B-a->C-b->D\n","testFindVertex1"),"SINK");
		Assert.assertTrue(score.getVertex(new LinkedList<String>()).getUserDatum(JUConstants.LABEL).equals("A"));
	}

	@Test
	public final void testGetVertex2()
	{
		computeStateScores score = new computeStateScores(TestFSMAlgo.buildGraph("A-a->B-b->C-b->D\n","testFindVertex2"),"SINK");
		Assert.assertTrue(score.getVertex(Arrays.asList(new String[]{"a","b"})).getUserDatum(JUConstants.LABEL).equals("C"));
	}

	@Test
	public final void testGetVertex3()
	{
		computeStateScores score = new computeStateScores(TestFSMAlgo.buildGraph("A-a->B-a->C-b->D\n","testFindVertex3"),"SINK");
		Assert.assertNull(score.getVertex(Arrays.asList(new String[]{"a","d"})));
	}

	@Ignore("only used occasionally")
	@Test 
	public final void testPerformanceOfCross()
	{
		List<List<String>> partA = new LinkedList<List<String>>(), partB = new LinkedList<List<String>>();
		for(int i=0;i < 1000;++i)
		{
			List<String> seqA = new LinkedList<String>(),seqB = new LinkedList<String>();
			for(int j=0;j < 2;++j)
			{
				seqA.add("A"+i+"_"+j);seqB.add("B"+i+"_"+j);
			}
			partA.add(seqA);partB.add(seqB);
		}
		WMethod.cross(partA, partB);
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{
		visFrame = new Visualiser();
	}
	
	@AfterClass
	public static void cleanUp()
	{
		try {
			SwingUtilities.invokeAndWait(new Runnable() 
			{
				public void run()
				{
					visFrame.setVisible(false);
					visFrame.dispose();
				}
			});
		} catch (InterruptedException e) {
			// cannot do anything with this
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// cannot do anything with this
			e.printStackTrace();
		}
	}
}
