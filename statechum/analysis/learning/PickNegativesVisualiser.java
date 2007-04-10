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
	
	//"Hypothesis Machine"
	public void construct(final Set<List<String>> sPlus, final Set<List<String>> sMinus)
    {
	   	learnerThread = new Thread(new Runnable()
		{
			public void run()
			{
					if (split != null) {
		        		l = new RPNIBlueFringeLearnerTestComponent(PickNegativesVisualiser.this);
		        		l.setDebugMode(true);
		        		//l.setPairsMergedPerHypothesis(2);
		        		
		        	}
		        	else
		        		l = new RPNIBlueFringeLearner(PickNegativesVisualiser.this);
		        		
		        	l.addObserver(PickNegativesVisualiser.this);
		        	l.setAnswers(ans);
		        	try{
		        		l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);
		        	}
		        	catch (InterruptedException e){return;};
			}
		},"RPNI learner thread");
	   	learnerThread.start();
		
    }
	
	public void mouseReleased(MouseEvent e) {
		final Set edges = viewer.getPickedState().getPickedEdges();
		if(edges.size() != 1)
			return;
		else {
				final Set<List<String>> sPlus = l.getSPlus();
				final Set<List<String>> sMinus = l.getSMinus();
				l.terminateLearner();
				new Thread(new Runnable() {// I'm on AWT thread now; once the dialog is closed, it needs its cleanup to be done on the AWT thread too.
					// For this reason, I launch another thread to wait for a cleanup and subsequently relaunch
					public void run()
					{
						try
						{
							learnerThread.join();
							sMinus.add(pickNegativeStrings((Edge)edges.toArray()[0]));
							construct(sPlus, sMinus);
						}
						catch(InterruptedException ex)
						{// cannot stop a worker thread
							ex.printStackTrace();
						}
					}
				},"learner restarter").start();
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
