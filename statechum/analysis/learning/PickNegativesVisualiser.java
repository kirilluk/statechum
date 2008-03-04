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

import java.awt.event.MouseEvent;
import java.util.*;
import javax.swing.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.algorithms.shortestpath.*;

import statechum.analysis.learning.profileStringExtractor.SplitFrame;
import statechum.*;
import statechum.analysis.learning.spin.*;

public class PickNegativesVisualiser extends Visualiser {
    /**
	 *  ID for serialization.
	 */
	private static final long serialVersionUID = 4842027206398108774L;
	
	RPNIBlueFringeLearner l =  null;
	
	protected SplitFrame split = null;
	protected AbstractOracle ans = null;
	
	public PickNegativesVisualiser()
	{
		super(VIZ_PROPERTIES.UPPER);
	}
	
	
	public PickNegativesVisualiser(SplitFrame frm, AbstractOracle an)
	{
		super();
		split = frm;ans = an;
	}
	
	public PickNegativesVisualiser(AbstractOracle an){
		super();
		ans = an;
	}
	
	/** The learner thread. */
	Thread learnerThread = null;
	
	public interface ThreadStartedInterface {
		public void threadStarted();
	}

	public static void setSimpleConfiguration(Configuration config,final boolean active, final int k)
	{
		if(!active){
			config.setKlimit(k);
			config.setAskQuestions(false); 
		}
		else
			config.setKlimit(-1);
		config.setDebugMode(true);
	}
	
	/** Collections of positive and negative samples. */
	private Collection<List<String>> sPlus, sMinus;
	
	/** The ltl formula to check against. */
	private Set<String> ltlFormulae = null;
	
	/** Configuration for learners. */
	private Configuration config = null; 
	
	/** Starts the learning thread with the supplied sets of positive and negative examples.
	 * 
	 * @param sPlus positives
	 * @param sMinus negatives
	 * @param whomToNotify this one is called just before learning commences.
	 */
	public void construct(final Collection<List<String>> plus, final Collection<List<String>> minus,final Set<String> ltl, final Configuration conf)
    {
		sPlus=plus;sMinus=minus;ltlFormulae=ltl;config=conf;
    }
	public void startLearner(final ThreadStartedInterface whomToNotify)
    {
	   	learnerThread = new Thread(new Runnable()
		{
			public void run()
			{
				if (ltlFormulae != null)
					l = new BlueFringeSpinLearner(PickNegativesVisualiser.this, ltlFormulae,config);
				else
					if (split != null) {
		        		l = new RPNIBlueFringeLearnerTestComponent(PickNegativesVisualiser.this, config);
		        	}
		        	else
		        		l = new RPNIBlueFringeLearnerTestComponentOpt(PickNegativesVisualiser.this, config);
				
	        	l.addObserver(PickNegativesVisualiser.this);
	        	l.setAnswers(ans);
	        	if (whomToNotify != null) whomToNotify.threadStarted();
	        	l.init(sPlus, sMinus);
        		l.learnMachine();
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
				
				try
				{
					learnerThread.join();
					sMinus.add(negatives);
					Configuration config = Configuration.getDefaultConfiguration();
					boolean active = true;
					if(l.getConfig().getMinCertaintyThreshold()>200000)
						active = false;
					setSimpleConfiguration(config, active, l.getConfig().getMinCertaintyThreshold());
					startLearner(this);
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
		final List<String> negatives = pickNegativeStrings((Edge)edges.iterator().next());
		l.terminateLearner();
		// I'm on AWT thread now; once the dialog is closed, it needs its cleanup to be done on the AWT thread too.
		// For this reason, I launch another thread to wait for a cleanup and subsequently relaunch the learner.
		new Thread(new LearnerRestarter(negatives),"learner restarter").start();
	}
	
	private List<String> pickNegativeStrings(Edge selected){
		DirectedSparseEdge e = (DirectedSparseEdge) selected;
		DirectedSparseGraph g = (DirectedSparseGraph)selected.getGraph();
		Set<List<String>> questions = new HashSet<List<String>>();
		Vertex init = DeterministicDirectedSparseGraph.findInitial(g);
		DijkstraShortestPath p = new DijkstraShortestPath(g);
		List<Edge> shortPrefix = p.getPath(init, e.getSource());
		Set<List<String>> prefixStrings = RPNIBlueFringeLearnerOrig.getPaths(shortPrefix);
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
