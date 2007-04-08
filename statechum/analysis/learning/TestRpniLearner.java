package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static statechum.analysis.learning.TestFSMAlgo.buildSet;

import java.awt.Point;
import java.io.IOException;
import java.io.StringReader;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import javax.swing.SwingUtilities;

import junit.framework.AssertionFailedError;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.TestFSMAlgo.StringPair;
import statechum.analysis.learning.TestRpniLearner.computeStateScores.PairScore;
import statechum.xmachine.model.testset.WMethod;

import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.algorithms.shortestpath.ShortestPathUtils;
import edu.uci.ics.jung.algorithms.shortestpath.UnweightedShortestPath;
import edu.uci.ics.jung.graph.Edge;
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
		
		RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(visFrame)
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
		private final UnweightedShortestPath shortestPath;
		private final DijkstraShortestPath shortestPathDijkstra;
		/** The initial vertex. */
		private final Vertex init;
		
		private final Map<Vertex,Map<String,Vertex>> transitionMatrix = new LinkedHashMap<Vertex,Map<String,Vertex>>();
				
		/** Initialises the class used to compute scores between states.
		 * 
		 * @param g the graph it will be used on 
		 * @param sinkVertexName the name for a sink vertex, to be different from names of all vertices on the graph
		 */
		public computeStateScores(DirectedSparseGraph g,String sinkVertexName)
		{
			graph = g;
			shortestPath = new UnweightedShortestPath(graph);shortestPathDijkstra = new DijkstraShortestPath(graph);
			init = findVertex(JUConstants.PROPERTY, JUConstants.INIT, graph);
			
			Set<Vertex> graphVertices = graph.getVertices();
			pairsAndScores = new ArrayList<PairScore>(graphVertices.size()*graphVertices.size());
			for(Vertex v:graphVertices)
			{
				assert !sinkVertexName.equals(v);
				transitionMatrix.put(v,new TreeMap<String,Vertex>());// using TreeMap makes everything more predictable
			}
			alphabet = WMethod.computeAlphabet(g);
			sinkVertex.addUserDatum(JUConstants.LABEL, sinkVertexName, UserData.SHARED);
			sinkVertex.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
			Map<String,Vertex> outgoingSink = new TreeMap<String,Vertex>();
			for(String input:alphabet)
				outgoingSink.put(input, sinkVertex);
			transitionMatrix.put(sinkVertex, outgoingSink);
			
			Iterator<DirectedSparseEdge> edgeIter = g.getEdges().iterator();
			while(edgeIter.hasNext())
			{	
				DirectedSparseEdge e = edgeIter.next();
				Map<String,Vertex> outgoing = transitionMatrix.get(e.getSource());
				// The line below aims to ensure that inputs are evaluated by computeStateScore in a specific order, which in conjunction with the visited set of computeStateScore permits emulating a bug in computeScore
				for(String label:(HashSet<String>)e.getUserDatum(JUConstants.LABEL))
					outgoing.put(label, e.getDest());			
			}
		}


		
		public List<List<String>> computeQS(StatePair pair)
		{
			List<List<String>> questions = new LinkedList<List<String>>();
			List<List<String>> suffixes = new LinkedList<List<String>>();
			int score = computeStateQuestions(pair,suffixes);
			assert score >= 0;
			
			List<String> sp = new LinkedList<String>();
			// A1
			// in this case, a path to Q is appended to outgoing edges from R; 
			// note that due to determinism the considered path to Q enters exactly the same state
			// and since there are no accept-states on edges from Q, any  
			if ( !hasAcceptedNeighbours(pair.getQ()) )
			{
				for(Edge v: (List<Edge>)ShortestPathUtils.getPath(shortestPath,init,pair.getQ()))
					sp.add( ((Set<String>)v.getUserDatum(JUConstants.LABEL)).iterator().next());
				for(Entry<String,Vertex> entry:transitionMatrix.get(pair.getR()).entrySet())
				{
					List<String> sequence = new LinkedList<String>();
					sequence.addAll(sp);sequence.add(entry.getKey());questions.add(sequence);
				}
			}
			else
			{// normal processing
				for(Vertex e: (List<Vertex>)shortestPathDijkstra.getPath(init, pair.getR()))
				{
					HashSet<String> labels = (HashSet<String>)e.getUserDatum(JUConstants.LABEL);
					sp.add(labels.iterator().next());
				}
				questions.add(sp);

				// A2 - does not really work since any prefix added in this way would cause suffixes to be executable right now, not only after merging
		/*		Edge connectingEdge = findEdge(pair.getR(),pair.getQ());
				if ( connectingEdge != null )
				{
					List<String> newSequence = new LinkedList<String>();newSequence.addAll(sp);
					HashSet<String> labels = (HashSet<String>)connectingEdge.getUserDatum(JUConstants.LABEL);
					newSequence.addAll(labels);result.add(newSequence);
				}
	*/
				// B
				Edge loopEdge = findEdge(pair.getR(),pair.getR());
				if ( loopEdge != null )
				{
					List<List<String>> prefs = WMethod.makeSingleton( (HashSet<String>)loopEdge.getUserDatum(JUConstants.LABEL) );
					prefs.add(new LinkedList<String>());// a singleton sequence
					questions = WMethod.cross(questions, prefs);
				}
				questions = WMethod.cross(questions, suffixes);				
			}
			
			return questions;
		}
		
		protected int computeStateQuestions(StatePair pair, Collection<List<String>> questions)
		{
			int score = 0;
			
			questions.clear();
			assert pair.getQ() != pair.getR();
			assert pair.getQ().getGraph() == graph && pair.getR().getGraph() == graph; 

			if (isAccept(pair.getR()) != isAccept(pair.getQ()))
				return -1;

			Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
			Queue<List<String>> questionQueue = new LinkedList<List<String>>(); // partial sequences, associated with traversed state pairs. These grow whenever the two states make a transition and when a terminal state is reached by the blue part, the resulting sequences are questions.
			currentExplorationBoundary.add(pair);questionQueue.add(new LinkedList<String>());
			
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
							currentExplorationBoundary.offer(nextStatePair);questionQueue.add(newQuestion);
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
							currentExplorationBoundary.offer(nextStatePair);questionQueue.add(newQuestion);
						}
					}// we've already handled the case when both states can make a move.

			}		
			return score;
		}

		// a heavily-reduced version of the above, tailured for score computation only
		protected int computeStateScore(StatePair pair)
		{
			if (isAccept(pair.getR()) != isAccept(pair.getQ()))
				return -1;
			//System.out.println("computing scores for "+pair);
			int score = 0;
			
			assert pair.getQ() != pair.getR();
			assert pair.getQ().getGraph() == graph && pair.getR().getGraph() == graph; 
			
			Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
			//Map<Vertex,Set<String>> visited = new HashMap<Vertex,Set<String>>();
			currentExplorationBoundary.add(pair);
			
			while(!currentExplorationBoundary.isEmpty())
			{
				StatePair currentPair = currentExplorationBoundary.remove();
				Map<String,Vertex> targetRed = transitionMatrix.get(currentPair.getR()),
					targetBlue = transitionMatrix.get(currentPair.getQ());
				//System.out.println("processing state pair "+currentPair);
				//Set<String> visitedSet = visited.get(currentPair.getQ());
				//if (visitedSet == null) { visitedSet = new HashSet<String>();visited.put(currentPair.getQ(), visitedSet); } 
				for(Entry<String,Vertex> redEntry:targetRed.entrySet())
				{
					Vertex nextBlueState = targetBlue.get(redEntry.getKey());
					if (nextBlueState != null)
							//&& !visitedSet.contains(redEntry.getKey()))
					{// both states can make a transition
						// if the red side is currently in the sink vertex, i.e. we are effectively calculating a set of questions, do not report inconsistency or increment the score
							if (isAccept(redEntry.getValue()) != isAccept(nextBlueState))
								return -1;// incompatible states
						
						++score;
						//System.out.println("\t "+redEntry.getKey()+"->"+nextBlueState.getUserDatum(JUConstants.LABEL)+" score "+score+" descend = "+(((DirectedSparseVertex)nextBlueState).numSuccessors() > 0));
						//visitedSet.add(redEntry.getKey());// KIRR: this (and the whole visitedSet thing) is actually an emulation of a bug in computeScore, but not a full-blown emulation since the two evaluate machines in a different order and hence some tests comparing the two fail.

						if (((DirectedSparseVertex)nextBlueState).numSuccessors() > 0)
						{
							StatePair nextStatePair = new StatePair(nextBlueState,redEntry.getValue());
							currentExplorationBoundary.offer(nextStatePair);
						}
					}
					// if the red can make a move, but the blue one cannot, ignore this case.
				}
			}		
			return score;
		}
		
		public static class PairScore extends StatePair implements Comparable
		{
			protected final int score;

			public PairScore(Vertex q, Vertex r, int sc) {
				super(q, r);
				score = sc;
			}
			
			public int getScore() {
				return score;
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#hashCode()
			 */
			@Override
			public int hashCode() {
				final int PRIME = 31;
				int result = super.hashCode();
				result = PRIME * result + score;
				return result;
			}

			public int compareTo(Object b){
				PairScore pB = (PairScore)b;
				if (score != pB.score)
					return score < pB.score? -1:1;
				return super.compareTo(b);
			}

			/* (non-Javadoc)
			 * @see java.lang.Object#equals(java.lang.Object)
			 */
			@Override
			public boolean equals(Object obj) {
				if (this == obj)
					return true;
				if (!super.equals(obj))
					return false;
				if (getClass() != obj.getClass())
					return false;
				final PairScore other = (PairScore) obj;
				if (score != other.score)
					return false;
				return true;
			}
			
			public String toString(){
				return "[ "+getQ().getUserDatum(JUConstants.LABEL)+", "+getR().getUserDatum(JUConstants.LABEL)+" : "+score+" ]";
			}
		}
		
		/** Stores all red-blue pairs; has to be backed by array for the optimal performance of the sort function. */
		protected final List<PairScore> pairsAndScores;
		
		protected int generalisationThreshold;
		protected int pairsMergedPerHypothesis;
		
		protected Stack<StatePair> chooseStatePairs()
		{
			pairsAndScores.clear();
			Set<Vertex> reds = new LinkedHashSet<Vertex>();reds.addAll(RPNIBlueFringeLearner.findVertices("colour", "red", graph));
			Queue<Vertex> currentExplorationBoundary = new LinkedList<Vertex>();// FIFO queue
			currentExplorationBoundary.addAll(reds);
			List<Vertex> BlueStatesConsideredSoFar = new LinkedList<Vertex>();
			while(!currentExplorationBoundary.isEmpty())
			{
				Vertex currentRed = currentExplorationBoundary.remove();
				for(Entry<String,Vertex> BlueEntry:transitionMatrix.get(currentRed).entrySet())
					if (!BlueEntry.getValue().containsUserDatumKey("colour") || BlueEntry.getValue().getUserDatum("colour").equals("blue"))
					{// the next vertex is not marked red, hence it has to become blue
						
						Vertex currentBlueState = BlueEntry.getValue();
						int numberOfCompatiblePairs = 0;
						for(Vertex oldRed:reds)
						{
							PairScore pair = new PairScore(currentBlueState,oldRed,computeStateScore(new StatePair(currentBlueState,oldRed)));
							if (pair.getScore() >= generalisationThreshold)
							{
								pairsAndScores.add(pair);
								++numberOfCompatiblePairs;
							}
						}
						if (numberOfCompatiblePairs == 0)
						{// mark this blue node as red. 
							currentBlueState.setUserDatum("colour", "red", UserData.SHARED);
							reds.add(BlueEntry.getValue());currentExplorationBoundary.add(BlueEntry.getValue());

							// All future blue nodes will use this revised set of red states; the fact that
							// it is added to the exploration boundary ensures that it is considered when looking for more blue states.
							// Note that previously-considered blue states were not compared to this one,
							// however previously-introduced red were - we're using the up-to-date reds set above.
							// For this reason, all we have to do is iterate over the old blue states and compare them to the
							// current one; none of those states may become red as a consequence since they are not red already, i.e. there is an entry about them in PairsAndScores
							for(Vertex oldBlue:BlueStatesConsideredSoFar)
							{
								PairScore pair = new PairScore(currentBlueState, oldBlue,computeStateScore(new StatePair(currentBlueState,oldBlue)));
								if (pair.getScore() >= generalisationThreshold)
									pairsAndScores.add(pair);
							}
						}
						else
						{
							BlueStatesConsideredSoFar.add(BlueEntry.getValue());// add a blue one
							currentBlueState.setUserDatum("colour", "blue", UserData.SHARED);
						}							
					}
			}
			Collections.sort(pairsAndScores);// there is no point maintaining a sorted collection as we go since a single quicksort at the end will do the job
					//, new Comparator(){	public int compare(Object o1, Object o2) { return ((PairScore)o2).compareTo(o1); }
			Stack<StatePair> result = new Stack<StatePair>();
			if (pairsMergedPerHypothesis > 0)
			{
				int numberOfElements = Math.min(pairsAndScores.size(),pairsMergedPerHypothesis);
				result.addAll(pairsAndScores.subList(0, numberOfElements));
			}
			else result.addAll(pairsAndScores);
			return result;
		}		
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
			newScoreA = s.computeStateScore(pair),
			newScoreB = s.computeStateQuestions(pair, new HashSet<List<String>>());
		Assert.assertEquals(-1, origScore);
		Assert.assertEquals(-1, newScoreA);
		Assert.assertEquals(-1, newScoreB);
	}
	
	/** Checks that both the old and the new algorithm report the same score for a pair of states and ask the same questions. */
	public final void testNewLearnerQuestions(String fsm, int expectedScore)
	{
		DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsm, "testNewLearner");
		computeStateScores s = new computeStateScores(g,"SINK");
		StatePair pair = new StatePair(findVertex(JUConstants.LABEL, "B", g),findVertex(JUConstants.LABEL, "A", g));
		doneEdges = new HashSet();
		int origScore = computeScore(g, pair),
			newScoreA = s.computeStateScore(pair);
		Collection<List<String>> questions = s.computeQS(pair);
		int newScoreB = s.computeStateQuestions(pair,new LinkedList<List<String>>());
		//updateFrame(g,null);
		Assert.assertTrue("these states should be compatible - correct test data",origScore >= 0);
		//System.out.println("computed scores, orig: "+origScore+" and the new one is "+newScoreA);
		Assert.assertEquals(expectedScore, newScoreA);
		Assert.assertEquals(expectedScore, newScoreB);
		if (origScore != -1)
		{
			Set<List<String>> oldQuestions = new HashSet<List<String>>();oldQuestions.addAll(generateQuestions(g, pair));
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

	@Ignore("BUG in generatequestions")
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
	
	public static final String PTA3 = "\nA-p->I-q->B"+"\nB-a->B1-a-#B2\nB1-d->B3-d->B4\nB1-c->B3-c->B4-c->B5-c->B6\n";
	
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

	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{
		visFrame = new Visualiser();
	}
	
	@Test
	public final void testNewLearner_4_1() // red and blue are adjacent
	{
		testNewLearnerQuestions("A-a->A1-a-#ARej\nA1-d->A2\nA1-c->A1"+"\nA-p->B"+"\nB-a->B1-a-#B2\nB1-d->B3-d->B4\nB1-c->B3-c->B4-c->B5-c->B6\n",8);
	}
	
	@Test
	public final void testNewLearner_4_2() // blue node has no access successors
	{
		testNewLearnerQuestions("A-d->A1\nA1-d->A2\nA1-c->A1"+"\nA-p->Atmp-q->B"+"\nB2#-c-B-a-#B1\n",0);
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
