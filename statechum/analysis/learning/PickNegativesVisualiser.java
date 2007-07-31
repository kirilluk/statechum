package statechum.analysis.learning;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import javax.swing.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.algorithms.shortestpath.*;

import statechum.analysis.learning.profileStringExtractor.SplitFrame;
import statechum.xmachine.model.testset.PTASequenceSet;
import statechum.*;

public class PickNegativesVisualiser extends Visualiser{
    /**
	 *  ID for serialization.
	 */
	private static final long serialVersionUID = 4842027206398108774L;
	
	RPNIBlueFringeLearner l =  null;
	
	protected SplitFrame split = null;
	protected StoredAnswers ans = null;
	
	public PickNegativesVisualiser()
	{
		super();
	}
	
	
	public PickNegativesVisualiser(SplitFrame frm, StoredAnswers an)
	{
		super();
		split = frm;ans = an;
	}
	
	/** The learner thread. */
	Thread learnerThread = null;
	
	public interface ThreadStartedInterface {
		public void threadStarted();
	}
	
	/** Starts the learning thread with the supplied sets of positive and negative examples.
	 * 
	 * @param sPlus positives
	 * @param sMinus negatives
	 * @param whomToNotify this one is called just before learning commences.
	 */
	public void construct(final Collection<List<String>> sPlus, final Collection<List<String>> sMinus,final ThreadStartedInterface whomToNotify, final boolean active)
    {
	   	learnerThread = new Thread(new Runnable()
		{
			public void run()
			{
					if (split != null) {
		        		l = new RPNIBlueFringeLearnerTestComponent(PickNegativesVisualiser.this);
		        		
		        		//l.setPairsMergedPerHypothesis(2);
		        		
		        	}
		        	else
		        		l = new RPNIBlueFringeLearnerTestComponentOpt(PickNegativesVisualiser.this);
		        	if(!active)
		        		l.setMinCertaintyThreshold(400000);
					l.setDebugMode(true);
		        	l.addObserver(PickNegativesVisualiser.this);
		        	l.setAnswers(ans);
		        	if (whomToNotify != null) whomToNotify.threadStarted();
	        		l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);
			}
		},"RPNI learner thread");
	   	learnerThread.start();
		
    }
	
	public class LearnerRestarter implements Runnable, ThreadStartedInterface
	{
		/** Set to true by the callback from the learner thread. */
		private boolean learnerStarted = false;
		
		final List<String> negatives;
		
		public LearnerRestarter(List<String> negs)
		{
			negatives = negs;
		}
		
		public void run() {
			synchronized (PickNegativesVisualiser.this) { 
				// to make sure that even if a user clicks on the edge multiple times, 
				// only one learner terminator will be active.
				
				final Collection<List<String>> sPlus = l.getSPlus();
				final Collection<List<String>> sMinus = l.getSMinus();
				try
				{
					learnerThread.join();
					sMinus.add(negatives);
					boolean active = true;
					if(l.getMinCertaintyThreshold()>200000)
						active = false;
					construct(sPlus, sMinus, this, active);
					synchronized (this) {
						while(!learnerStarted)
							wait();// here we wait for the learner thread to start - if we do not wait for this, 
							// multiple thread terminators might try to start learner at the same time - I'm not 
							// sure what join does on a thread which was created and started with start() 
							// but which did not yet start running.
					}
				}
				catch(InterruptedException ex)
				{// cannot stop a worker thread
					ex.printStackTrace();
				}				
			}
		}

		public synchronized void threadStarted() {
			learnerStarted = true;
			notify();
		}
		
	}
	
	public void mouseReleased(MouseEvent e) {
		final Set edges = viewer.getPickedState().getPickedEdges();
		if(edges.size() != 1)
			return;
		else {
			final List<String> negatives = pickNegativeStrings((Edge)edges.iterator().next());
			l.terminateLearner();
			// I'm on AWT thread now; once the dialog is closed, it needs its cleanup to be done on the AWT thread too.
			// For this reason, I launch another thread to wait for a cleanup and subsequently relaunch
			new Thread(new LearnerRestarter(negatives),"learner restarter").start();
		}
		
		
	}
	
	private List<String> pickNegativeStrings(Edge selected){
		DirectedSparseEdge e = (DirectedSparseEdge) selected;
		DirectedSparseGraph g = (DirectedSparseGraph)selected.getGraph();
		Set<List<String>> questions = new HashSet<List<String>>();
		Vertex init = RPNIBlueFringeLearner.findVertex("property", "init", g);
		DijkstraShortestPath p = new DijkstraShortestPath(g);
		List<Edge> shortPrefix = p.getPath(init, e.getSource());
		Set<List<String>> prefixStrings = RPNIBlueFringeLearner.getPaths(shortPrefix);
		List<Edge> picked = new ArrayList<Edge>();
		picked.add(e);
		Set<List<String>> successors = RPNIBlueFringeLearnerTestComponent.computeSuffixes(e.getDest(), g);
		
		questions.addAll(RPNIBlueFringeLearnerTestComponent.mergePrefixWithSuffixes(prefixStrings, (Collection<String>)e.getUserDatum(JUConstants.LABEL), successors));
		
		 Object[] possibleValues = questions.toArray();
		 Object selectedValue = JOptionPane.showInputDialog(null,

		             "Choose one", "Add Negative String",

		             JOptionPane.INFORMATION_MESSAGE, null,

		             possibleValues, possibleValues[0]);
		System.out.println(selectedValue);
		return (List<String>)selectedValue;
	}
	

}
