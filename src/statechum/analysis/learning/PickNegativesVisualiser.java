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

import java.awt.event.MouseEvent;
import java.util.*;
import javax.swing.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;

import statechum.analysis.learning.observers.AutoAnswers;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.smt.SmtLearnerDecorator;
import statechum.*;
import statechum.analysis.learning.util.*;

public class PickNegativesVisualiser extends Visualiser {
    /**
	 *  ID for serialisation.
	 */
	private static final long serialVersionUID = 4842027206398108774L;
	
	/** This one does the learning. */
	protected RPNILearner innerLearner =  null;
		
	public PickNegativesVisualiser()
	{
		super(0);
	}
	
	/** The learner thread. */
	Thread learnerThread = null;
	
	public interface ThreadStartedInterface {
		public void threadStarted();
	}

	/** Collections of positive and negative samples. */
	Collection<List<Label>> sPlus, sMinus;
	
	/** Configuration for learners. */
	LearnerEvaluationConfiguration conf = null; 
	
	/** Starts the learning thread with the supplied sets of positive and negative examples.
	 * 
	 * @param sPlus positives
	 * @param sMinus negatives
	 * @param whomToNotify this one is called just before learning commences.
	 */
	public void construct(final Collection<List<Label>> plus, final Collection<List<Label>> minus,final LearnerEvaluationConfiguration evalCnf)
    {
		sPlus=plus;sMinus=minus;conf=evalCnf;
    }
	
	public void startLearner(final ThreadStartedInterface whomToNotify)
    {
	   	learnerThread = new Thread(new Runnable()
		{
	   		@Override 
			public void run()
			{
				if (conf.ifthenSequences != null)
					innerLearner = new RPNIUniversalLearner(PickNegativesVisualiser.this, conf);
				else
		        	innerLearner = new RPNIUniversalLearner(PickNegativesVisualiser.this, conf);// at this point ifthenSequences will always be null.
				
				innerLearner.addObserver(PickNegativesVisualiser.this);
				Learner mainDecorator = new AutoAnswers(innerLearner);
				if (conf.labelDetails != null)
					mainDecorator = new SmtLearnerDecorator(mainDecorator,conf.labelDetails);
	        	if (whomToNotify != null) whomToNotify.threadStarted();
	        	LearnerGraph graph = mainDecorator.learnMachine(sPlus, sMinus);
	        	if (graph != null)
	        	{
		        	DirectedSparseGraph learnt = graph.pathroutines.getGraph();
	        		if(conf.config.isGenerateTextOutput())
	        			OutputUtil.generateTextOutput(learnt,"textOutput.txt");
	        		if(conf.config.isGenerateDotOutput())
	        			OutputUtil.generateDotOutput(learnt, "dotOutput.dot");
	        	}        		
			}
		},"RPNI learner thread");
	   	learnerThread.start();
		
    }
	
	public class LearnerRestarter implements Runnable, ThreadStartedInterface
	{
		/** Set to true by the callback from the learner thread. */
		private boolean learnerStarted = false;
		
		List<Label> negatives = null;
		String negLTL = null;
		
		public LearnerRestarter(List<Label> negs)
		{
			negatives = negs;
		}
		
		public LearnerRestarter(String argNegLTL)
		{
			this.negLTL = argNegLTL;
		}
		
		@Override 
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
						conf.ifthenSequences.add(negLTL);

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

		@Override 
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
		if(conf.ifthenSequences != null){
			String newLTL = JOptionPane.showInputDialog("New LTL formula:");
			innerLearner.terminateUserDialogueFrame();
			new Thread(new LearnerRestarter(newLTL),"learner restarter").start();
		}else{
			final List<Label> negatives = pickNegativeStrings((Edge)edges.iterator().next());
			innerLearner.terminateUserDialogueFrame();
			// I'm on AWT thread now; once the dialog is closed, it needs its cleanup to be done on the AWT thread too.
			// For this reason, I launch another thread to wait for a cleanup and subsequently relaunch the learner.
			new Thread(new LearnerRestarter(negatives),"learner restarter").start();
		}
	}
	private List<Label> pickNegativeStrings(@SuppressWarnings("unused") Edge selected){
		throw new UnsupportedOperationException("this one made not to call legacy code");
	}
	/*
	private List<Label> pickNegativeStrings(Edge selected){
		DirectedSparseEdge e = (DirectedSparseEdge) selected;
		DirectedSparseGraph g = (DirectedSparseGraph)selected.getGraph();
		Set<List<Label>> questions = new LinkedHashSet<List<Label>>();
		Vertex init = DeterministicDirectedSparseGraph.findInitial(g);
		DijkstraShortestPath p = new DijkstraShortestPath(g);
		List<Edge> shortPrefix = p.getPath(init, e.getSource());
		Set<List<Label>> prefixStrings = Test_Orig_RPNIBlueFringeLearner.getPaths(shortPrefix);
		List<Edge> picked = new ArrayList<Edge>();
		picked.add(e);
		Set<List<Label>> successors = Test_Orig_RPNIBlueFringeLearnerTestComponent.computeSuffixes(e.getDest(), g);
		
		questions.addAll(Test_Orig_RPNIBlueFringeLearnerTestComponent.mergePrefixWithSuffixes(prefixStrings, (Collection<Label>)e.getUserDatum(JUConstants.LABEL), successors));
		
		 Object[] possibleValues = questions.toArray();
		 Object selectedValue = JOptionPane.showInputDialog(null,
		             "Choose one", "Add Negative String",
		             JOptionPane.INFORMATION_MESSAGE, null,
		             possibleValues, possibleValues[0]);
		System.out.println(selectedValue);
		return (List<Label>)selectedValue;
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
	*/

}
