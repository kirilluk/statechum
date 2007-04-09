package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static statechum.analysis.learning.TestFSMAlgo.buildSet;

import java.awt.Point;
import java.io.IOException;
import java.io.StringReader;
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
import statechum.analysis.learning.computeStateScores.IncompatibleMergeException;
import statechum.analysis.learning.computeStateScores.PairScore;
import statechum.xmachine.model.testset.WMethod;

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

	@Test
	public void testPTAconstruction1() // only two traces, both accept
	{
		RPNIBlueFringeLearner l=new RPNIBlueFringeLearner(null);
		Set<List<String>> plusStrings = buildSet(new String[][] { new String[] {"a","b","c"},new String[]{"a","d","c"} });
		DirectedSparseGraph g = l.augmentPTA(RPNIBlueFringeLearner.initialise(), plusStrings, true);
		RPNIBlueFringeLearner.numberVertices(g);
		//updateFrame(g,null);
		TestFSMAlgo.checkM(g,"A-a->B--b->C-c->End1\nB--d->C2-c->End2");
	}

	@Test
	public void testPTAconstruction2()// two accept traces and one reject one
	{
		RPNIBlueFringeLearner l=new RPNIBlueFringeLearner(null);
		Set<List<String>> plusStrings = buildSet(new String[][] { new String[]{"a","b","c"}, new String[]{"a","d","c"}});
		Set<List<String>> minusStrings = buildSet(new String[][] { new String[]{"a","b","c","d"} });
		DirectedSparseGraph g = l.createAugmentedPTA(RPNIBlueFringeLearner.initialise(), plusStrings, minusStrings);
		RPNIBlueFringeLearner.numberVertices(g);
		//updateFrame(g,null);
		TestFSMAlgo.checkM(g,"A-a->B--b->C-c->End1-d-#REJ\nB--d->C2-c->End2");
	}

	protected int checkPath(FSMStructure expected, DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
	{
		int answer = WMethod.tracePath(expected, question);
		System.out.print(question+" answer is "+answer);
		int lastElement = 0;
		while(lastElement < question.size() &&
			RPNIBlueFringeLearner.getVertex(model, question.subList(0, lastElement+1)) != null)
			lastElement++;
		RPNIBlueFringeLearner.getVertex(model, question);
		if ((answer >= 0 && answer != lastElement) ||
				answer < 0 && lastElement != question.size())
			System.out.println(" ERROR");
		else
			System.out.println();
		return answer;//answer>0?answer-1:answer;
	}

	protected void checkLearner(String fsmString, String [][] plus, String [][] minus)
	{
		final DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsmString, "sample FSM");
		final DirectedSparseGraph completedGraph = (DirectedSparseGraph)g.copy();TestFSMAlgo.completeGraph(completedGraph, "REJECT");
		final FSMStructure expected = WMethod.getGraphData(g);

		checkPath(expected, g, Arrays.asList(new String[]{"text", "set_position", "set_dimensions"}), null);
		
		debugMode = true;updateFrame(g, g);

		// now sanity checking on the plus and minus sets
		for(String [] path:plus)
			assert RPNIBlueFringeLearner.USER_ACCEPTED == WMethod.tracePath(expected, Arrays.asList(path));
		for(String [] path:minus)
			assert RPNIBlueFringeLearner.USER_ACCEPTED != WMethod.tracePath(expected, Arrays.asList(path));
		
		RPNIBlueFringeLearnerTestComponent l = new RPNIBlueFringeLearnerTestComponent(visFrame)
		{
			protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
			{
				return checkPath(expected, g, question, moreOptions);
			}
		};
		l.setDebugMode(true);
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

	@Test
	public void testLoadAnswers1() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		Assert.assertEquals(0,sa.getCount());
	}
	
	@Test
	public void testLoadAnswers2() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(""));
		Assert.assertEquals(0,sa.getCount());
	}
	
	@Test
	public void testLoadAnswers3() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(""));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_CANCELLED,sa.getAnswer(Arrays.asList(new String[]{})));
	}

	@Test
	public void testLoadAnswers4() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("[test] <yes>"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}
	
	@Test
	public void testLoadAnswers5A() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(" \t\t    [test] <no> at position 5, junk"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}

	@Test
	public void testLoadAnswers5B() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("\n\n[test] <no> at position 5, junk\n"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}

	@Test
	public void testLoadAnswers5C() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("  "+RPNIBlueFringeLearner.QUESTION_AUTO+" [test] <no> at position 5, junk"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail1() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("junk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail2() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] junk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail3() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no>"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail4() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no> at position "));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail5() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no> at position 6"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail6() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no> at position 7,\njunk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail7() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader(" junk\n\n[valid string] <no> at position 7,\n"));
	}
	
	
	@Test
	public void testLoadAnswers6() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("[test] <no> at position 5, junk\n "
				+RPNIBlueFringeLearner.QUESTION_AUTO+" [some text, more of it] <yes> whatever\n\n\n"
				+"[teststr, another, more] <no> at position 0, junk\n"				
				+RPNIBlueFringeLearner.QUESTION_AUTO+"[teststr, a, more] <no> at position 2, junk\n"				
				+"[teststr, p, more] <yes> junk\n"				
		));
		Assert.assertEquals(5,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa.getAnswer(Arrays.asList(new String[]{"some text","more of it"})));
		Assert.assertEquals(0, sa.getAnswer(Arrays.asList(new String[]{"teststr","another", "more"})));
		Assert.assertEquals(2, sa.getAnswer(Arrays.asList(new String[]{"teststr","a", "more"})));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa.getAnswer(Arrays.asList(new String[]{"teststr","p", "more"})));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_CANCELLED, sa.getAnswer(Arrays.asList(new String[]{"unknown","p", "more"})));
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
		computeStateScores s = new computeStateScores(g,"SINK");
		StatePair pair = new StatePair(findVertex(JUConstants.LABEL, "B", g),findVertex(JUConstants.LABEL, "A", g));
		DirectedSparseGraph temp = mergeAndDeterminize((Graph)g.copy(), pair);
		doneEdges = new HashSet();
		int origScore = computeScore(g, pair),
			newScoreA = s.computeStateScore(pair);
		updateFrame(g,temp);
		Collection<List<String>> questions = s.computeQS(pair,temp);
		Assert.assertTrue("these states should be compatible - correct test data",origScore >= 0);
		//System.out.println("computed scores, orig: "+origScore+" and the new one is "+newScoreA);
		Assert.assertEquals(expectedScore, newScoreA);
		Assert.assertEquals(expectedScore, origScore);
		if (origScore != -1)
		{
			Set<List<String>> oldQuestions = new HashSet<List<String>>();oldQuestions.addAll(generateQuestions(g,temp, pair));
			Set<List<String>> newQuestions = new HashSet<List<String>>();newQuestions.addAll(questions);
			Assert.assertTrue("different questions: old "+oldQuestions+", new "+questions,oldQuestions.equals(newQuestions));
		}
	}
	
	public static final String PTA1 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2\nB1-b->B3-b->B4\n";

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
	public final void testGetTempRed1()
	{
		DirectedSparseGraph a=TestFSMAlgo.buildGraph("A-a->B", "testGetTempRed1 model"),
			temp=TestFSMAlgo.buildGraph("C-d->Q", "testGetTempRed1 temp");
		Vertex foundA = computeStateScores.getTempRed(a, findVertex(JUConstants.PROPERTY, JUConstants.INIT, a), temp);
		Vertex foundB =RPNIBlueFringeLearnerTestComponent.getTempRed(a, findVertex(JUConstants.PROPERTY, JUConstants.INIT, a), temp);
		Assert.assertTrue(findVertex(JUConstants.PROPERTY, JUConstants.INIT, temp).equals(foundA));
		Assert.assertTrue(findVertex(JUConstants.PROPERTY, JUConstants.INIT, temp).equals(foundB));
	}
	
	@Test
	public final void testGetTempRed2()
	{
		DirectedSparseGraph a=TestFSMAlgo.buildGraph("A-a->B-a->B-c->C-c->D", "testGetTempRed1 model"),
			temp=TestFSMAlgo.buildGraph("C-a->Q-a->Q-c->Q", "testGetTempRed1 temp");
		Vertex foundA = computeStateScores.getTempRed(a, findVertex(JUConstants.LABEL, "D", a), temp);
		Vertex foundB = RPNIBlueFringeLearnerTestComponent.getTempRed(a, findVertex(JUConstants.LABEL, "D", a), temp);
		Assert.assertTrue(findVertex(JUConstants.LABEL, "Q", temp).equals(foundA));
		Assert.assertTrue(findVertex(JUConstants.LABEL, "Q", temp).equals(foundB));
	}
	
	@Test
	public final void findMergablePair1() throws IncompatibleMergeException
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("A-a->B\nA-b->B\nA-c->C\nA-d->D", "findMergablePair1");
		Assert.assertNull(RPNIBlueFringeLearner.findMergablePair(g));
		Assert.assertNull(computeStateScores.findMergablePair(g));		
	}
	
	@Test
	public final void findMergablePair2() throws IncompatibleMergeException
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("A-a->B\nA-b->B\nA-c->D\nA-b->D\nA-d->E", "findMergablePair2");
		Vertex 
			b = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g),
			d = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "D", g);
		StatePair expected = new StatePair(b,d),
			actualA = RPNIBlueFringeLearner.findMergablePair(g),
			actualB = computeStateScores.findMergablePair(g);
		Assert.assertTrue("expected: "+expected+" got: "+actualA,expected.equals(actualA));
		Assert.assertTrue("expected: "+expected+" got: "+actualB,expected.equals(actualB));
	}
	
	@Test
	public final void findMergablePair3a() throws IncompatibleMergeException
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-p->A-a->S\nA-b->S\nA-c->D\nA-b->D\nA-d->E", "findMergablePair3a");
		Vertex 
			s = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "S", g),
			d = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "D", g);
		StatePair expected = new StatePair(d,s),
		actualA = RPNIBlueFringeLearner.findMergablePair(g),
		actualB = computeStateScores.findMergablePair(g);
	Assert.assertTrue("expected: "+expected+" got: "+actualA,expected.equals(actualA));
	Assert.assertTrue("expected: "+expected+" got: "+actualB,expected.equals(actualB));
	}

	@Test
	public final void findMergablePair3b() throws IncompatibleMergeException
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-p->A-a->B\nA-b->B\nA-c->S\nA-b->S\nA-d->E", "findMergablePair3b");
		Vertex 
			b = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g),
			s = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "S", g);
		StatePair expected = new StatePair(b,s),
		actualA = RPNIBlueFringeLearner.findMergablePair(g),
		actualB = computeStateScores.findMergablePair(g);
	Assert.assertTrue("expected: "+expected+" got: "+actualA,expected.equals(actualA));
	Assert.assertTrue("expected: "+expected+" got: "+actualB,expected.equals(actualB));
	}

	@Test
	public final void findMergablePair4a() throws IncompatibleMergeException
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-p->A-a->B\nA-b->B\nA-c-#D\nA-b-#D\nA-d->E", "findMergablePair4a");
		Vertex 
			b = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g),
			d = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "D", g);
		StatePair expected = new StatePair(d,b),
		actualA = RPNIBlueFringeLearner.findMergablePair(g);
		Assert.assertNull(actualA);
	}

	@Test(expected=IncompatibleMergeException.class)
	public final void findMergablePair4b() throws IncompatibleMergeException
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-p->A-a->B\nA-b->B\nA-c-#D\nA-b-#D\nA-d->E", "findMergablePair4b");
		Vertex 
			b = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g),
			d = RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "D", g);
		computeStateScores.findMergablePair(g);
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
			findVertex(JUConstants.LABEL, red, g).addUserDatum("colour", "red", UserData.SHARED);
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

	@Test
	public final void testPairCompatible1()
	{
		DirectedSparseGraph g = TestFSMAlgo.buildGraph(
				"A-a->B-a->C-b->D\n"+
				"A-b->E", "testPairCompatible1");
		assertTrue(computeStateScores.pairCompatible(g, 
				new StatePair(
						RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g),
						RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "A", g))
						));
	}

	@Test
	public final void testPairCompatible2()
	{
		DirectedSparseGraph g = TestFSMAlgo.buildGraph(
				"A-a->B-a->C-b->D\n"+
				"A-b-#E", "testPairCompatible2");
		assertFalse(computeStateScores.pairCompatible(g, 
				new StatePair(
						RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "B", g),
						RPNIBlueFringeLearner.findVertex(JUConstants.LABEL, "A", g))
						));
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
