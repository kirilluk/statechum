package statechum.analysis.learning.experiments;

import java.util.Formatter;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import cern.jet.random.engine.MersenneTwister;

import statechum.JUConstants;

import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;
/**
 * Adds an alphabet of a specific size, ensuring that the final machine is deterministic.
 * Also adds the potential of loops to the same state
 * @author nw
 *
 */
public class ForestFireLabelledStateMachineGenerator extends
		ForestFireStateMachineGenerator {
	
	Set<String> alphabet;

	public ForestFireLabelledStateMachineGenerator(double forwards, double backwards, int alphabetSize, int seed) throws Exception{
		super(forwards, backwards);
		this.generator = new MersenneTwister(seed);
		this.alphabet=getAlphabet(alphabetSize);
	}
	
	public ForestFireLabelledStateMachineGenerator(double forwards, double backwards, int alphabetSize) throws Exception{
		super(forwards, backwards);
		this.alphabet=getAlphabet(alphabetSize);
	}
	
	public ForestFireLabelledStateMachineGenerator(double forwards, double backwards, Set<String> alphabet) throws Exception{
		super(forwards, backwards);
		this.alphabet=alphabet;
	}
	
	protected DirectedSparseGraph buildMachine(int size) {
		for(int i=0;i<size-1;i++){
			DirectedSparseVertex v = (DirectedSparseVertex) machine.addVertex(new DirectedSparseVertex());
			//visited.add(v);
			HashSet tried = new HashSet<DirectedSparseVertex>();
			DirectedSparseVertex random = selectRandom();
			tried.add(random);
			while(!addEdge(random,v)){
				random = selectRandom(tried);
				tried.add(random);
				if(random == null){
					System.out.println("Could not construct complete machine");
					machine.removeVertex(v);
					return machine;
				}
			}
			visited.add(random);
			spread(v,random);
			vertices.add(v);
			visited.clear();
		}
		return machine;
	}
	
	@Override
	protected boolean addEdge(DirectedSparseVertex v, DirectedSparseVertex random){
		Set<String> vertexAlphabet = new HashSet<String>();
		for (Object e : random.getOutEdges()) {
			DirectedSparseEdge edge = (DirectedSparseEdge)e;
			Set<String>labels = (Set<String>)edge.getUserDatum(JUConstants.LABEL);
			if(labels!=null)
				vertexAlphabet.addAll(labels);
		}
		Set<String> possibles = alphabet;
		possibles.removeAll(vertexAlphabet);
		String label = null;
		if(possibles.size()==0)
			return false;
		else if(possibles.size()==1)
			label = (String) possibles.toArray()[0];
		else
			label = pickRandom(possibles);
		Set<String> labelSet = new HashSet<String>();
		labelSet.add(label);
		DirectedSparseEdge e = new DirectedSparseEdge(v,random);
		e.addUserDatum(JUConstants.LABEL, labelSet, UserData.SHARED);
		machine.addEdge(e);
		return true;
	}
	
	protected DirectedSparseVertex selectRandom(Set<DirectedSparseVertex> blocked){
		List<DirectedSparseVertex> available = vertices;
		available.removeAll(blocked);
		int size = available.size();
		if(size ==0)
			return null;
		else if(size == 1)
			return (DirectedSparseVertex) available.toArray()[0];
		else{
			Random r = new Random();
			int index = r.nextInt(available.size()-1);
			return (DirectedSparseVertex)available.get(index);
		}
	}

	private String pickRandom(Set<String> possibles) {
		Random index = new Random();
		int size = possibles.size();
		int random = index.nextInt(size);
		return String.valueOf(random);
	}

	private static Set<String> getAlphabet(int number){
		Set<String> alphabet = new HashSet<String>();
		for (int i=0;i<number;i++){
			String next = new Formatter().format("%05d", i).toString();
			alphabet.add(next);
		}
		return alphabet;
	}
	
}
