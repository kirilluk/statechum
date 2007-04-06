package statechum.analysis.learning;

import static statechum.analysis.learning.TestFSMAlgo.buildSet;

import java.awt.Frame;
import java.awt.Point;
import java.io.IOException;
import java.io.StringReader;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Map.Entry;

import javax.swing.SwingUtilities;

import junit.framework.AssertionFailedError;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import com.sun.corba.se.spi.legacy.connection.GetEndPointInfoAgainException;

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.TestFSMAlgo.StringPair;
import statechum.xmachine.model.testset.WMethod;

import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class TestRpniLearner extends RPNIBlueFringeLearner
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
		updateFrame(g,null);
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
		updateFrame(g,null);
		TestFSMAlgo.checkM(g,"A-a->B--b->C-c->End1-d-#REJ\nB--d->C2-c->End2");
	}

	protected void checkLearner(String fsmString, String [][] plus, String [][] minus)
	{
		final DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsmString, "sample FSM");TestFSMAlgo.completeGraph(g, "REJECT");
		final FSMStructure expected = WMethod.getGraphData(g);

		updateFrame(g, g);

		// now sanity checking on the plus and minus sets
		for(String [] path:plus)
			assert RPNIBlueFringeLearner.USER_ACCEPTED == WMethod.tracePath(expected, Arrays.asList(path));
		for(String [] path:minus)
			assert RPNIBlueFringeLearner.USER_ACCEPTED != WMethod.tracePath(expected, Arrays.asList(path));
		
		RPNIBlueFringeLearnerTestComponent l = new RPNIBlueFringeLearnerTestComponent(visFrame)
		{
			protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
			{
				return WMethod.tracePath(expected, question);
			}
		};
		l.setPairsMergedPerHypothesis(0);
		//l.setGeneralisationThreshold(1);
		l.addObserver(visFrame);
		try{
			DirectedSparseGraph learningOutcome = l.learnMachine(RPNIBlueFringeLearner.initialise(), buildSet(plus), buildSet(minus));
			updateFrame(learningOutcome,g);
			FSMStructure learntStructure = WMethod.getGraphData(learningOutcome);
			TestFSMAlgo.checkM(learntStructure,expected,learntStructure.init,expected.init);
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
	
	@Ignore("this one does not work yet")
	@Test
	public void testLearner2()
	{
		checkLearner("A-a->B<-a-C-b->A\nA-b->C\nC-c->C\n",new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b","c"}}, new String[][]{new String[]{"c"}});
	}

	@Ignore("this one does not work yet")
	@Test
	public void testLearner2b()
	{
		checkLearner("A-a->B<-a-C-b->A\nA-b->C\nC-c->C\n",new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b","c"}}, new String[][]{new String[]{"c"},new String[]{"b","b","c"}	});
	}

	@Ignore("this one does not work yet")
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

	public static class computeStateScores {
		private final Vertex sinkVertex = new DirectedSparseVertex();
		private final Set<String> alphabet;
		private final DirectedSparseGraph graph;
		private final DijkstraShortestPath shortestPath;

		private final Map<Vertex,Map<String,Vertex>> transitionMatrix = new HashMap<Vertex,Map<String,Vertex>>();
		
		/** Adds the target state and the input associated with the given edge, to the appropriate entry of the transition matrix.
		 * 
		 * @param e the edge to add to the transition matrix.
		 */
		private void populateTransitionMatrixWithVertex(DirectedSparseEdge e)
		{
			Map<String,Vertex> outgoing = transitionMatrix.get(e.getSource());
			if (outgoing == null)
			{
				outgoing = new HashMap<String,Vertex>();transitionMatrix.put(e.getSource(),outgoing);
			}
			outgoing.put((String)e.getUserDatum(JUConstants.LABEL), e.getDest());			
		}
		
		/** Initialises the class used to compute scores between states.
		 * 
		 * @param g the graph it will be used on 
		 * @param sinkVertexName the name for a sink vertex, to be different from names of all vertices on the graph
		 */
		public computeStateScores(DirectedSparseGraph g,String sinkVertexName)
		{
			graph = g;shortestPath = new DijkstraShortestPath(graph);
			Iterator<Vertex> vIt = g.getVertices().iterator();
			while(vIt.hasNext())
			{
				Vertex v = vIt.next(); 
				assert !sinkVertexName.equals(v);
				transitionMatrix.put(v,new HashMap<String,Vertex>());
			}
			alphabet = WMethod.computeAlphabet(g);
			sinkVertex.addUserDatum(JUConstants.LABEL, sinkVertexName, UserData.SHARED);
			sinkVertex.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
			Map<String,Vertex> outgoingSink = new HashMap<String,Vertex>();
			for(String input:alphabet)
				outgoingSink.put(input, sinkVertex);
			transitionMatrix.put(sinkVertex, outgoingSink);
			
			Iterator<DirectedSparseEdge> edgeIter = g.getEdges().iterator();
			while(edgeIter.hasNext())
			{
				DirectedSparseEdge e = edgeIter.next();
				Map<String,Vertex> outgoing = transitionMatrix.get(e.getSource());
				for(String label:(HashSet<String>)e.getUserDatum(JUConstants.LABEL))
					outgoing.put(label, e.getDest());			
			}
		}
		
		public Set<List<String>> computeQuestionPrefix(StatePair pair)
		{
			Vertex init = findVertex(JUConstants.PROPERTY, JUConstants.INIT, graph);
			Set<List<String>> result = new HashSet<List<String>>();
			List<String> sp = new LinkedList<String>();

			for(Vertex e: (List<Vertex>)shortestPath.getPath(init, pair.getR()))
			{
				HashSet<String> labels = (HashSet<String>)e.getUserDatum(JUConstants.LABEL);
				sp.add(labels.iterator().next());
			}
			
			// A1
			if ( !hasAcceptedNeighbours(pair.getQ()) )
			{
				List<String> newSequence = new LinkedList<String>();newSequence.addAll(sp);
				for(Vertex v: (List<Vertex>)shortestPath.getPath(pair.getR(),pair.getQ()))
					sp.add((String)v.getUserDatum(JUConstants.LABEL));
				result.add(newSequence);result.add(newSequence);
			}

			// A2
			Edge connectingEdge = findEdge(pair.getR(),pair.getQ());
			if ( connectingEdge != null )
			{
				List<String> newSequence = new LinkedList<String>();newSequence.addAll(sp);
				HashSet<String> labels = (HashSet<String>)connectingEdge.getUserDatum(JUConstants.LABEL);
				newSequence.addAll(labels);result.add(newSequence);
			}

			// B
			Edge loopEdge = findEdge(pair.getR(),pair.getR());
			if ( loopEdge != null )
			{
				List<String> newSequence = new LinkedList<String>();newSequence.addAll(sp);
				HashSet<String> labels = (HashSet<String>)loopEdge.getUserDatum(JUConstants.LABEL);
				newSequence.addAll(labels);result.add(newSequence);
			}

			
			result.add(new LinkedList<String>());// a singleton sequence
			return result;
		}
		
		public int computeStateScoreAndQuestions(StatePair pair, Set<List<String>> questions)
		{
			int score = 0;
			
			questions.clear();
			assert pair.getQ() != pair.getR();
			assert pair.getQ().getGraph() == graph && pair.getR().getGraph() == graph; 
			
			Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
			Queue<List<String>> questionQueue = new LinkedList<List<String>>(); // partial sequences, associated with traversed state pairs. These grow whenever the two states make a transition and when a terminal state is reached by the blue part, the resulting sequences are questions.
			Set<StatePair> statesAddedToBoundary = new HashSet<StatePair>();
			currentExplorationBoundary.add(pair);statesAddedToBoundary.add(pair);questionQueue.add(new LinkedList<String>());
			
			while(!currentExplorationBoundary.isEmpty())
			{
				StatePair currentPair = currentExplorationBoundary.remove();List<String> currentQuestion = questionQueue.remove();
				Map<String,Vertex> targetRed = transitionMatrix.get(currentPair.getR()),
					targetBlue = transitionMatrix.get(currentPair.getQ());
				
				for(Entry<String,Vertex> redEntry:targetRed.entrySet())
				{
					Vertex nextBlueState = targetBlue.get(redEntry.getKey());
					if (nextBlueState != null)
					{// both states can make a transition

						if (redEntry.getValue() != sinkVertex)
						{// if the red side is currently in the sink vertex, i.e. we are effectively calculating a set of questions, do not report inconsistency or increment the score
							if (isAccept(redEntry.getValue()) != isAccept(nextBlueState))
								return -1;// incompatible states
						
							++score;
						}
						
						List<String> newQuestion = new LinkedList<String>(currentQuestion);
						newQuestion.add(redEntry.getKey());

						if (((DirectedSparseVertex)nextBlueState).numSuccessors() == 0)
						{// at this point, either the red state is the sink one or the next-state red and blue states are compatible.
							if (redEntry.getValue() == sinkVertex)
								questions.add(newQuestion);// reached the end of PTA on the blue, return the constructed question
						}
						else
						{
							StatePair nextStatePair = new StatePair(nextBlueState,redEntry.getValue());
							if (!statesAddedToBoundary.contains(nextStatePair))
							{
								currentExplorationBoundary.offer(nextStatePair);questionQueue.add(newQuestion);
								statesAddedToBoundary.add(nextStatePair);
							}
						}
					}
					// if the red can make a move, but the blue one cannot, ignore this case.
				}

				for(Entry<String,Vertex> blueEntry:targetBlue.entrySet())
					if (!targetRed.containsKey(blueEntry.getKey()))
					{// the current transition from the blue vertex does not have an associated red one, hence make a transition to the sink vertex on the red side
						
						List<String> newQuestion = new LinkedList<String>(currentQuestion);
						newQuestion.add(blueEntry.getKey());

						if (((DirectedSparseVertex)blueEntry.getValue()).numSuccessors() == 0)
							questions.add(newQuestion);// reached the end of PTA, return the constructed question
						else
						{
							StatePair nextStatePair = new StatePair(blueEntry.getValue(),sinkVertex);
							if (!statesAddedToBoundary.contains(nextStatePair))
							{
								currentExplorationBoundary.offer(nextStatePair);questionQueue.add(newQuestion);
								statesAddedToBoundary.add(nextStatePair);
							}
						}
					}// we've already handled the case when both states can make a move.

			}		
			return score;
		}
	}
		
	public final void testNewLearner(String fsm)
	{
		DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsm, "testNewLearner");
		computeStateScores s = new computeStateScores(g,"SINK");
		StatePair pair = new StatePair(findVertex(JUConstants.LABEL, "B", g),findVertex(JUConstants.LABEL, "A", g));
		Set<List<String>> questions = new HashSet<List<String>>();
		doneEdges = new HashSet();
		int origScore = computeScore(g, pair),
			newScore = s.computeStateScoreAndQuestions(pair, questions);
		Assert.assertEquals(origScore, newScore);
		if (origScore != -1)
		{
			Set<List<String>> newQuestions = WMethod.cross(s.computeQuestionPrefix(pair), questions);
			Set<List<String>> q = new HashSet<List<String>>();q.addAll(generateQuestions(g, pair));
			Assert.assertTrue(q.equals(newQuestions));
		}
	}
	
	public static final String PTA1 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2\nB1-b->B3-b->B4\n";

	@Test
	public final void testNewLearner1a()
	{
		testNewLearner("A-a->A"+PTA1);
	}
	
	@Test
	public final void testNewLearner1b()
	{
		testNewLearner("A-a->A"+"\nA-p->I-q->B"+"\nB-a->B1-a->B2\nB1-b->B3-b->B4\n");
	}
	
	@Test
	public final void testNewLearner2()
	{
		testNewLearner("A-a->A1-a->A2"+PTA1);
	}
	
	@Test
	public final void testNewLearner3()
	{
		testNewLearner("A-a->A1-b->A1"+PTA1);
	}
	
	@Test
	public final void testNewLearner4()
	{
		testNewLearner("A-a->A1-b->A1-a-#A2"+PTA1);
	}
	
	@Test
	public final void testNewLearner5()
	{
		testNewLearner("A-a->A1-b->A1"+PTA1);
	}
	
	@Test
	public final void testNewLearner6()
	{
		testNewLearner("A-a->A1-a-#A1\nA1-b-#A2"+PTA1);
	}

	@Test
	public final void testNewLearner7()
	{
		testNewLearner("A-a->A1-a-#A1\nA1-b->A2-b-#A3"+PTA1);
	}
	
	@Test
	public final void testNewLearner8()
	{
		testNewLearner("A-a->A1-a-#A1\nA1-b->A2-b->A3"+PTA1);
	}
	
	public static final String PTA2 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2\nB1-b->B3-b->B4\nB1-c->BB1-c->BB2\n";
	
	@Test
	public final void testNewLearner9()
	{
		testNewLearner("A-a->A1-a-#A1\nA1-b->A2\nA1-c->A1"+PTA2);
	}
	
	@Test
	public final void testNewLearner10()
	{
		testNewLearner("A-a->A1-a-#A1\nA1-b->A2\nA1-c-#A3"+PTA2);
	}
	
	@Test
	public final void testNewLearner11()
	{
		testNewLearner("A-a->A1-a-#A1\nA1-b->A2\nA1-c->A3"+PTA2);
	}
	
	@Test
	public final void testNewLearner12()
	{
		testNewLearner("A-a->A1-a-#A1\nA1-b->A1\nA1-c->A3"+PTA2);
	}
	
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
