package statechum.xmachine.model.testset;

import java.util.*;
import java.awt.*;

import javax.swing.*;

import statechum.JUConstants;


import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.algorithms.shortestpath.*;

public class WMethodFrame extends JFrame {
	
	private DirectedSparseGraph machineGraph;
	private int numberOfExtraStates=0;
	
	public WMethodFrame(DirectedSparseGraph g){
		super();
		this.machineGraph = g;
		this.setTitle("Test set information");
		this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		numberOfExtraStates = Integer.valueOf(JOptionPane.showInputDialog(
                this.getParent(),
                "Number of extra states","0")).intValue();
		setUpJTabbedPane();
		this.setSize(600, 400);
		this.setVisible(true);
	}
	
	private void setUpJTabbedPane(){
		JTabbedPane pane = new JTabbedPane();
		Vector transitionCover = cross(computeTransitionCover(),getStimuli());
		transitionCover.addAll(getStimuli());
		transitionCover.add(new Vector());
		Vector characterisationSet = computeCharacterisationSet();
		Vector zSet = getExtraStateSequences(characterisationSet);
		Vector fullTestSet = cross(transitionCover, zSet);
		JComponent transitionCoverPanel = makeTextPanel(transitionCover);
		JComponent characteisationPanel = makeTextPanel(characterisationSet);
		JComponent fullTestSetPanel = makeTextPanel(fullTestSet);
		JComponent statsTab = makeStatsTab(transitionCover, characterisationSet, fullTestSet);
		pane.addTab("Test Set Statistics", statsTab);
		pane.addTab("Transition Cover", transitionCoverPanel);
		pane.addTab("Characerisation Set", characteisationPanel);
		pane.addTab("Full Test Set", fullTestSetPanel);
		add(pane);
		pane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
	}
	
	private Vertex findStartVertex(){
		Iterator vertexIt = machineGraph.getVertices().iterator();
		while(vertexIt.hasNext()){
			Vertex v = (Vertex)vertexIt.next();
			String start = String.valueOf(v.getUserDatum("startOrTerminal"));
			if(start.trim().equals("start"))
				return v;
		}
		return null;
	}
	
	
	
	private Vector cross(Collection a, Collection b){
		Vector returnVect = new Vector();
		Iterator transIt = a.iterator();
		while(transIt.hasNext()){
			java.util.List path = (java.util.List)transIt.next();
			Iterator wIt = b.iterator();
			while(wIt.hasNext()){
				java.util.List w = (java.util.List)wIt.next();
				ArrayList cross = new ArrayList();
				cross.addAll(path);
				cross.addAll(w);
				returnVect.add(cross);
			}
		}
		return returnVect;
	}
	
	private Vector getExtraStateSequences(java.util.List wSet){
		java.util.List inputs = getStimuli();
		ArrayList empty = new ArrayList();
		Vector tempInputs = new Vector();
		tempInputs.add(empty);
		Vector z = new Vector();
		for(int i=0;i<this.numberOfExtraStates;i++){
			tempInputs=cross(inputs,tempInputs);
		}
		
		z=cross(tempInputs,wSet);
		return z;
	}
	
	private java.util.List getStimuli(){
		Iterator edgeIt = machineGraph.getEdges().iterator();
		HashSet stimuli = new HashSet();
		while(edgeIt.hasNext()){
			Edge e = (Edge)edgeIt.next();
			String stimulus = (String)e.getUserDatum(JUConstants.LABEL);
			stimuli.add(stimulus);
		}
		Object[] stimArray = stimuli.toArray();
		Vector list = new Vector();
		for(int i=0;i<stimArray.length;i++){
			java.util.List path = new ArrayList();
			path.add(stimArray[i]);
			list.add(path);
		}
		return list;
	}
	
	private JComponent makeStatsTab(Vector transitionCover, Vector wSet, Vector fullTestSet){
		JTextArea area = new JTextArea();
		JScrollPane scrollPane = new JScrollPane(area);
		area.insert("Transition Cover:\n"+getStats(transitionCover)+"\n", area.getCaretPosition());
		area.insert("Characterisation Set:\n"+getStats(wSet)+"\n", area.getCaretPosition());
		area.insert("Full Test Set:\n"+getStats(fullTestSet)+"\n", area.getCaretPosition());
		JPanel panel = new JPanel(false);
		panel.setLayout(new GridLayout(1,1));
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}
	
	private String getStats(Vector paths){
		String returnString = new String();
		returnString = "size: "+paths.size()+"\n";
		Iterator pathIt = paths.iterator();
		int totalInputs = 0;
		while(pathIt.hasNext()){
			Collection c = (Collection)pathIt.next();
			totalInputs = totalInputs+c.size();
		}
		returnString = returnString + "total number of inputs: "+totalInputs+"\n";
		return returnString;
	}
	
	private JComponent makeTextPanel(Vector paths){
		JTextArea area = new JTextArea();
		JScrollPane scrollPane = new JScrollPane(area);
		Iterator pathIt = paths.iterator();
		while(pathIt.hasNext()){
			java.util.List l = (java.util.List)pathIt.next();
			area.insert(listPath(l)+"\n", area.getCaretPosition());
		}
		JPanel panel = new JPanel(false);
		panel.setLayout(new GridLayout(1,1));
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}
	
	private Vector computeTransitionCover(){
		DijkstraShortestPath dsp = new DijkstraShortestPath(machineGraph);
		Vertex start = findStartVertex();
		Iterator transitionIt = machineGraph.getEdges().iterator();
		Vector paths = new Vector();
		while(transitionIt.hasNext()){
			DirectedSparseEdge e = (DirectedSparseEdge)transitionIt.next();
			Vertex target = e.getSource();
			java.util.List list = dsp.getPath(start, target);
			list.add(e);
			paths.add(list);
		}
		return paths;
	}
	
	private Vector computeCharacterisationSet(){
		DijkstraShortestPath dsp = new DijkstraShortestPath(machineGraph);
		Vector stateStack = new Vector();
		stateStack.addAll(machineGraph.getVertices());
		HashMap vertexStrings = getAllVerticesToStrings(dsp);
		Vector paths = new Vector();
		Iterator stateIt = stateStack.iterator();
		while(stateIt.hasNext()){
			Vertex currentVertex = (Vertex)stateIt.next();
			HashSet strings = (HashSet)vertexStrings.get(currentVertex);
			for(int i=1;i<=machineGraph.numEdges();i++){
				Iterator stringIt = strings.iterator();
				while(stringIt.hasNext()){
					java.util.List string = (java.util.List)stringIt.next();
					if(string.size()!=i)
						continue;
					if(isUnique(string, currentVertex, vertexStrings)){
						paths.add(string);
						i=machineGraph.numEdges()+1;
						break;
					}
				}
			}
			
		}
		
		return paths;
	}
	
	private boolean isUnique(java.util.List string, Vertex v, HashMap vertexStrings){
		Iterator vertexIt = machineGraph.getVertices().iterator();
		while(vertexIt.hasNext()){
			Vertex current = (Vertex)vertexIt.next();
			if(v.getUserDatum("name").equals(current.getUserDatum("name")))
				continue;
			HashSet strings = (HashSet)vertexStrings.get(current);
			if(collectionContainsList(strings, string))
				return false;
		}
		return true;
	}
	
	private HashMap getAllVerticesToStrings(DijkstraShortestPath dsp){
		Iterator stateIterator = machineGraph.getVertices().iterator();
		HashMap statesToStrings = new HashMap();
		while(stateIterator.hasNext()){
			Vertex currentState = (Vertex)stateIterator.next();
			HashSet paths = new HashSet();
			Iterator transitionIt = machineGraph.getEdges().iterator();
			while(transitionIt.hasNext()){
				DirectedSparseEdge e = (DirectedSparseEdge)transitionIt.next();
				Vertex target = e.getSource();
				java.util.List list;
				if(target.getUserDatum("name").equals(currentState.getUserDatum("name"))){
					list = new ArrayList();
					list.add(e);
				}
				else{
					list = dsp.getPath(currentState, target);
					if(!list.isEmpty()){
						paths.add(list);
						list.add(e);
					}
				}
				if(!list.isEmpty());
				paths.add(list);
			}
			statesToStrings.put(currentState, paths);
		}
		return statesToStrings;
	}
	
	private String listPath(java.util.List list){
		Iterator listIt = list.iterator();
		String path = new String();
		while(listIt.hasNext()){
			Object next = listIt.next();
			if(next instanceof Edge){
				Edge e = (Edge)next;
				path = path.concat(String.valueOf(e.getUserDatum(JUConstants.LABEL))+", ");
			}
			else{
				String s = (String)next;
				path = path.concat(s+", ");
			}
		}
		return path.trim();
	}
	
	private boolean collectionContainsList(Collection c, java.util.List list){
		Iterator listIterator = c.iterator();
		while(listIterator.hasNext()){
			java.util.List current = (java.util.List)listIterator.next();
			if(current.size()!=list.size())
				continue;
			if(listPath(current).equals(listPath(list)))
				return true;
		}
		return false;
	}
	

}
