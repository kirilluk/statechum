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

import statechum.analysis.learning.observers.AutoAnswers;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.profileStringExtractor.SplitFrame;
import statechum.*;
import statechum.analysis.learning.util.*;

public class PickNegativesVisualiser extends Visualiser {
    /**
	 *  ID for serialisation.
	 */
	private static final long serialVersionUID = 4842027206398108774L;
	
	/** This one does the learning. */
	RPNILearner innerLearner =  null;
	
	/** This is a decorator. */
	Learner autoAnswersDecorator = null;
	
	protected SplitFrame split = null;
	
	public PickNegativesVisualiser()
	{
		super(statechum.GlobalConfiguration.G_PROPERTIES.UPPER);
	}
	
	
	public PickNegativesVisualiser(SplitFrame frm)
	{
		super();
		split = frm;
	}
	
	/** The learner thread. */
	Thread learnerThread = null;
	
	public interface ThreadStartedInterface {
		public void threadStarted();
	}

	/** Collections of positive and negative samples. */
	Collection<List<String>> sPlus, sMinus;
	
	/** Configuration for learners. */
	LearnerEvaluationConfiguration conf = null; 
	
	/** Starts the learning thread with the supplied sets of positive and negative examples.
	 * 
	 * @param sPlus positives
	 * @param sMinus negatives
	 * @param whomToNotify this one is called just before learning commences.
	 */
	public void construct(final Collection<List<String>> plus, final Collection<List<String>> minus,final LearnerEvaluationConfiguration evalCnf)
    {
		sPlus=plus;sMinus=minus;conf=evalCnf;
    }
	
	public void startLearner(final ThreadStartedInterface whomToNotify)
    {
	   	learnerThread = new Thread(new Runnable()
		{
			public void run()
			{
				if (conf.ltlSequences != null)
					innerLearner = new RPNIUniversalLearner(PickNegativesVisualiser.this, conf);
				else
					if (split != null) {
						innerLearner = new Test_Orig_RPNIBlueFringeLearnerTestComponent(PickNegativesVisualiser.this, conf.config);
		        	}
		        	else
		        		innerLearner = new RPNIUniversalLearner(PickNegativesVisualiser.this, conf);// at this point ltlSequences will always be null.
				
				innerLearner.addObserver(PickNegativesVisualiser.this);
				autoAnswersDecorator=new AutoAnswers(innerLearner);
	        	if (whomToNotify != null) whomToNotify.threadStarted();
        		DirectedSparseGraph learnt = autoAnswersDecorator.learnMachine(sPlus, sMinus).paths.getGraph();
        		if(conf.config.isGenerateTextOutput())
        			OutputUtil.generateTextOutput(learnt);
        		if(conf.config.isGenerateDotOutput())
        			OutputUtil.generateDotOutput(learnt);
        		
			}
		},"RPNI learner thread");
	   	learnerThread.start();
		
    }
	
	public class LearnerRestarter implements Runnable, ThreadStartedInterface
	{
		/** Set to true by the callback from the learner thread. */
		private boolean learnerStarted = false;
		
		List<String> negatives = null;
		String negLTL = null;
		
		public LearnerRestarter(List<String> negs)
		{
			negatives = negs;
		}
		
		public LearnerRestarter(String argNegLTL)
		{
			this.negLTL = argNegLTL;
		}
		
		public void run() {
			synchronized (PickNegativesVisualiser.this) { 
				// to make sure that even if a user clicks on the edge multiple times, 
				// only one learner terminator will be active.
				
				try
				{
					learnerThread.join();
					if(negatives!=null)
						sMinus.add(negatives);
					if(negLTL!=null)
						conf.ltlSequences.add(negLTL);

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
	
	@Override
	public void mouseReleased(@SuppressWarnings("unused") MouseEvent e) {
		final Set edges = viewer.getPickedState().getPickedEdges();
		if(edges.size() != 1)
			return;
		if(conf.ltlSequences != null){
			String newLTL = JOptionPane.showInputDialog("New LTL formula:");
			innerLearner.terminateUserDialogueFrame();
			new Thread(new LearnerRestarter(newLTL),"learner restarter").start();
		}else{
			final List<String> negatives = pickNegativeStrings((Edge)edges.iterator().next());
			innerLearner.terminateUserDialogueFrame();
			// I'm on AWT thread now; once the dialog is closed, it needs its cleanup to be done on the AWT thread too.
			// For this reason, I launch another thread to wait for a cleanup and subsequently relaunch the learner.
			new Thread(new LearnerRestarter(negatives),"learner restarter").start();
		}
	}
	
	private List<String> pickNegativeStrings(Edge selected){
		DirectedSparseEdge e = (DirectedSparseEdge) selected;
		DirectedSparseGraph g = (DirectedSparseGraph)selected.getGraph();
		Set<List<String>> questions = new HashSet<List<String>>();
		Vertex init = DeterministicDirectedSparseGraph.findInitial(g);
		DijkstraShortestPath p = new DijkstraShortestPath(g);
		List<Edge> shortPrefix = p.getPath(init, e.getSource());
		Set<List<String>> prefixStrings = Test_Orig_RPNIBlueFringeLearner.getPaths(shortPrefix);
		List<Edge> picked = new ArrayList<Edge>();
		picked.add(e);
		Set<List<String>> successors = Test_Orig_RPNIBlueFringeLearnerTestComponent.computeSuffixes(e.getDest(), g);
		
		questions.addAll(Test_Orig_RPNIBlueFringeLearnerTestComponent.mergePrefixWithSuffixes(prefixStrings, (Collection<String>)e.getUserDatum(JUConstants.LABEL), successors));
		
		 Object[] possibleValues = questions.toArray();
		 Object selectedValue = JOptionPane.showInputDialog(null,
		             "Choose one", "Add Negative String",
		             JOptionPane.INFORMATION_MESSAGE, null,
		             possibleValues, possibleValues[0]);
		System.out.println(selectedValue);
		return (List<String>)selectedValue;
	}
	
	private List<String> pickNegativeStringsOrLTL(Edge selected){
		DirectedSparseEdge e = (DirectedSparseEdge) selected;
		DirectedSparseGraph g = (DirectedSparseGraph)selected.getGraph();
		Set<List<String>> questions = new HashSet<List<String>>();
		Vertex init = DeterministicDirectedSparseGraph.findInitial(g);
		DijkstraShortestPath p = new DijkstraShortestPath(g);
		List<Edge> shortPrefix = p.getPath(init, e.getSource());
		Set<List<String>> prefixStrings = Test_Orig_RPNIBlueFringeLearner.getPaths(shortPrefix);
		List<Edge> picked = new ArrayList<Edge>();
		picked.add(e);
		Set<List<String>> successors = Test_Orig_RPNIBlueFringeLearnerTestComponent.computeSuffixes(e.getDest(), g);
		
		questions.addAll(Test_Orig_RPNIBlueFringeLearnerTestComponent.mergePrefixWithSuffixes(prefixStrings, (Collection<String>)e.getUserDatum(JUConstants.LABEL), successors));
		
		 Object[] possibleValues = questions.toArray();
		 Object selectedValue = JOptionPane.showInputDialog(null,
		             "Choose one", "Add Negative String",
		             JOptionPane.INFORMATION_MESSAGE, null,
		             possibleValues, possibleValues[0]);
		System.out.println(selectedValue);
		return (List<String>)selectedValue;
	}
	

}
