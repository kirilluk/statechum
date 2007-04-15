package statechum.analysis.learning;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.statistics.*;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraDistance;

import java.util.*;

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.*;

public class RandomPathGenerator {
	
	protected DirectedSparseGraph g;
	protected Collection<List<String>> sPlus;
	
	public RandomPathGenerator(DirectedSparseGraph baseGraph) {
		sPlus = new LinkedList<List<String>>();
		g = baseGraph;
		DijkstraDistance dd = new DijkstraDistance(baseGraph);
		Collection<Double> distances = dd.getDistanceMap(RPNIBlueFringeLearner.findVertex(JUConstants.PROPERTY, "init", g)).values();
		ArrayList<Double> distancesList = new ArrayList<Double>(distances);
		Collections.sort(distancesList);
		int diameter = distancesList.get(distancesList.size()-1).intValue();
		this.populateRandomWalksC((int)Math.pow(g.getVertices().size(),2), diameter+5);
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
			List<String> path = new LinkedList<String>();
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

	private void populateRandomWalksC(int number, int maxLength){
		int counter = 0;
		Set<String> doneStrings = new HashSet<String>();
		FSMStructure fsm = WMethod.getGraphData(g);
		while(counter<number){
			List<String> path = new LinkedList<String>();
			String s = "";
			String current = fsm.init;
			for(int i=0;i<maxLength;i++){
				Map<String,String> row = fsm.trans.get(current);
				if(row.isEmpty())
					break;
				String nextInput= (String)pickRandom(row.keySet());
				s = s.concat(nextInput);
				path.add(nextInput);
				
				if(!doneStrings.contains(s)){
					sPlus.add(new ArrayList(path));
					doneStrings.add(s);
					counter++;
				}
				current = row.get(nextInput);
			}
		}
	}

	private String pickRandom(Vertex v){
		Set<String> labels;
		Edge e = (Edge)pickRandom(v.getOutEdges());
		labels = (Set<String>)e.getUserDatum(JUConstants.LABEL);
		return pickRandom(labels).toString();
	}

	public static final Random pathRandomNumberGenerator = new Random(0); 
	
	private Object pickRandom(Collection c){
		Object[] array = c.toArray();
		if(array.length==1)
			return array[0];
		else{
			int random = pathRandomNumberGenerator.nextInt(array.length);
			return array[random];
		}
	}

	public Collection<List<String>> getSPlus() {
		return sPlus;
	}
	
	public Collection<List<String>> getAllPaths(){
		Collection<List<String>> allPaths = new LinkedList<List<String>>();
		allPaths.addAll(sPlus);
		return allPaths;
	}
	
	

}
