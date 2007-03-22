package statechum.analysis.learning;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.*;
import javax.swing.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.algorithms.shortestpath.*;

import statechum.analysis.learning.profileStringExtractor.SplitFrame;
import statechum.*;

public class LearningVisualiser extends Visualiser{
    /**
	 *  ID for serialization.
	 */
	private static final long serialVersionUID = 4842027206398108774L;
	
	Thread learnerThread;
	RPNIBlueFringeLearner l =  null;
	SplitFrame split = null;
	
	//"Hypothesis Machine"
	public void construct(final Set<List<String>> sPlus, final Set<List<String>> sMinus, final SplitFrame split)
    {
		if(split!=null)
			this.split = split;
	   	learnerThread = new Thread(new Runnable()
		{
			public void run()
			{
					if (split != null) {
		        		l = new RPNIBlueFringeLearnerTestComponent(LearningVisualiser.this);
		        	}
		        	else
		        		l = new RPNIBlueFringeLearner(LearningVisualiser.this);
		        		
		        	l.addObserver(LearningVisualiser.this);
		        	try{
		        		l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus, 0);
		        	}
		        	catch (InterruptedException e){return;};
			}
		},"RPNI learner thread");
	   	learnerThread.start();
		
    }
	
	public void mouseReleased(MouseEvent e) {
		Set edges = viewer.getPickedState().getPickedEdges();
		if(edges.size() != 1)
			return;
		else {
				Set<List<String>> sPlus = l.getSPlus();
				Set<List<String>> sMinus = l.getSMinus();
				sMinus.add(pickNegativeStrings((Edge)edges.toArray()[0]));
				learnerThread.interrupt();
				construct(sPlus, sMinus, split);
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
