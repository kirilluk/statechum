package statechum.xmachine.model.testset;

import java.util.*;

import statechum.JUConstants;

import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;



public class WMethod {

	private DirectedSparseGraph machineGraph;
	private int numberOfExtraStates;
	private Vector fullTestSet, transitionCover, characterisationSet;
	
	public WMethod(DirectedSparseGraph g, int numberOfExtraStates){
		this.machineGraph = g;
		this.numberOfExtraStates = numberOfExtraStates;
		computeTestSets();
	}
	
	public Vector getFullTestSet(){
		return fullTestSet;
	}
	
	public Set<List<String>> getFullTestSetStrings(){
		Iterator<Collection> testSetIt = fullTestSet.iterator();
		HashSet<List<String>> testSet = new HashSet<List<String>>();
		while(testSetIt.hasNext()){
			Collection test = testSetIt.next();
			ArrayList<String> string = new ArrayList<String>();
			for(Object element:test){
				if(element instanceof Edge){
					Edge e = (Edge)element;
					Set labelSet = (Set)e.getUserDatum(JUConstants.LABEL);
					string.add(labelSet.toArray()[0].toString());
				}
				else 
					string.add((String)element);
			}
			testSet.add(string);
		}
		return testSet;
	}
	
	public Vector getCharacterisationSet() {
		return characterisationSet;
	}

	public Vector getTransitionCover() {
		return transitionCover;
	}

	private void computeTestSets(){
		transitionCover = cross(computeTransitionCover(),getStimuli());
		transitionCover.addAll(getStimuli());
		transitionCover.add(new Vector());
		characterisationSet = computeCharacterisationSet();
		Vector zSet = getExtraStateSequences(characterisationSet);
		fullTestSet = cross(transitionCover, zSet);
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
			String stimulus = (String)e.getUserDatum("EDGE");
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
			if(v.getUserDatum("VERTEX").equals(current.getUserDatum("VERTEX")))
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
				if(target.getUserDatum("VERTEX").equals(currentState.getUserDatum("VERTEX"))){
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
				path = path.concat(String.valueOf(e.getUserDatum("EDGE"))+", ");
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
