package statechum.analysis.learning;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.statistics.*;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraDistance;

import java.util.*;

import statechum.JUConstants;
import statechum.xmachine.model.testset.*;

public class RandomPathGenerator {
	
	protected DirectedSparseGraph g;
	protected Set<List<String>> sPlus;
	
	public RandomPathGenerator(DirectedSparseGraph baseGraph) {
		sPlus = new HashSet<List<String>>();
		g = baseGraph;
		//addNegativeSinkState();
		DijkstraDistance dd = new DijkstraDistance(baseGraph);
		Collection<Double> distances = dd.getDistanceMap(RPNIBlueFringeLearner.findVertex(JUConstants.PROPERTY, "init", g)).values();
		ArrayList<Double> distancesList = new ArrayList<Double>(distances);
		Collections.sort(distancesList);
		int diameter = distancesList.get(distancesList.size()-1).intValue();
		this.populateRandomWalks((int)Math.pow(g.getVertices().size(),2), diameter+5);
	}
	
	private void addNegativeSinkState(){
		Vertex sink = new DirectedSparseVertex();
		sink.setUserDatum(JUConstants.ACCEPTED, "false", UserData.SHARED);
		g.addVertex(sink);
		HashSet<String> alphabet = WMethod.computeAlphabet(g);
		Iterator<Vertex> vertexIt = g.getVertices().iterator();
		while (vertexIt.hasNext()) {
			Vertex v = vertexIt.next();
			Set<String> outgoingSymbols = getOutgoingSymbols(v);
			Set<String> rejectLabels = alphabet;
			rejectLabels.removeAll(outgoingSymbols);
			if(!rejectLabels.isEmpty()){
				DirectedSparseEdge e = new DirectedSparseEdge(v, sink);
				e.setUserDatum(JUConstants.LABEL, rejectLabels, UserData.SHARED);
				g.addEdge(e);
			}
		}
	}
	
	private Set<String> getOutgoingSymbols(Vertex v){
		Set<String> outSymbols = new HashSet<String>();
		Iterator<Edge> outEdges = v.getOutEdges().iterator();
		while (outEdges.hasNext()) {
			Edge e = outEdges.next();
			Set<String> labels = (Set<String>)e.getUserDatum(JUConstants.LABEL);
			outSymbols.addAll(labels);
		}
		return outSymbols;
	}
	
	private void populateRandomWalks(int number, int maxLength){
		int counter = 0;
		Set<String> doneStrings = new HashSet<String>();
		Vertex init = RPNIBlueFringeLearner.findVertex(JUConstants.PROPERTY, "init", g);
		while(counter<number){
			List<String> path = new ArrayList<String>();
			String s = "";
			Vertex current = init;
			for(int i=0;i<maxLength;i++){
				if(current.outDegree()==0)
					break;
				String currentString= pickRandom(current);
				s = s.concat(currentString);
				path.add(currentString);
				Vertex exists = RPNIBlueFringeLearner.getVertex(g, path);
				if(!doneStrings.contains(s)){
					sPlus.add(new ArrayList(path));
					doneStrings.add(s);
					counter++;
				}
				current = exists;
			}
		}
	}
	
	private String pickRandom(Vertex v){
		Set<String> labels;
		Edge e = (Edge)pickRandom(v.getOutEdges());
		labels = (Set<String>)e.getUserDatum(JUConstants.LABEL);
		return pickRandom(labels).toString();
	}
	
	private Object pickRandom(Collection c){
		Object[] array = c.toArray();
		Random r = new Random();
		if(array.length==1)
			return array[0];
		else{
			int random = r.nextInt(array.length);
			return array[random];
		}
	}

	public Set<List<String>> getSPlus() {
		return sPlus;
	}
	
	public Set<List<String>> getAllPaths(){
		Set<List<String>> allPaths = new HashSet<List<String>>();
		allPaths.addAll(sPlus);
		return allPaths;
	}
	
	

}
