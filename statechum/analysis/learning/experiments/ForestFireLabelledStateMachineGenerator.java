package statechum.analysis.learning.experiments;

import java.util.Collection;
import java.util.Formatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Set;

import cern.jet.random.engine.MersenneTwister;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.LearnerGraph.FSMImplementation;
import statechum.analysis.learning.rpnicore.WMethod.EquivalentStatesException;

import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;
import edu.uci.ics.jung.utils.UserDataContainer.CopyAction;
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
	
	protected LearnerGraph buildMachine(int size) throws Exception {
		labelmap = new HashMap<Object,DirectedSparseVertex>();
		for(int i=0;i<size-1;i++){
			DirectedSparseVertex v =  new DirectedSparseVertex();
			//visited.add(v);
			String label = String.valueOf(i+1);
			v.setUserDatum(JUConstants.LABEL, label, UserData.SHARED);
			machine.addVertex(v);
			this.labelmap.put(label, v);
			HashSet tried = new HashSet<DirectedSparseVertex>();
			DirectedSparseVertex random = selectRandom();
			tried.add(random);
			while(!addEdge(random,v)){
				random = selectRandom(tried);
				tried.add(random);
				if(random == null){
					throw new Exception("Could not construct machine");
				}
			}
			visited.add(random);
			spread(v,random);
			vertices.add(v);
			visited.clear();
		}
		
		Configuration conf = Configuration.getDefaultConfiguration();
		conf.setAllowedToCloneNonCmpVertex(true);
		return new LearnerGraph(machine,conf).paths.reduce();
	}
	
	private boolean differentiate(AMEquivalenceClass<CmpVertex, LearnerGraphCachedData> equivalenceClass){
		Iterator<CmpVertex> vertices = equivalenceClass.getStates().iterator();
		while(vertices.hasNext()) {
			CmpVertex rep = vertices.next();
			HashSet tried = new HashSet<DirectedSparseVertex>();
			DirectedSparseVertex random = selectRandom(tried);
			tried.add(random);
			String label = String.valueOf(rep.getID());
			DirectedSparseVertex old = labelmap.get(label);
			while(!addEdge(old,random)){
				random = selectRandom(tried);
				tried.add(random);
				if(random == null){
					System.out.println("Could not construct minimal machine");
					return false;
				}
			}
		}
		return true;
	}
	
	@Override
	protected boolean addEdge(DirectedSparseVertex v, DirectedSparseVertex random){
		Set<String> vertexAlphabet = new HashSet<String>();
		for (Object e : v.getOutEdges()) {
			DirectedSparseEdge edge = (DirectedSparseEdge)e;
			Set<String>labels = (Set<String>)edge.getUserDatum(JUConstants.LABEL);
			if(labels!=null)
				vertexAlphabet.addAll(labels);
		}
		Set<String> possibles = new HashSet<String>();
		possibles.addAll(alphabet);
		possibles.removeAll(vertexAlphabet);
		String label = null;
		if(possibles.size()==0)
			return false;
		else
			label = (String)pickRandom(possibles.toArray());
		Set<String> labelSet = new HashSet<String>();
		labelSet.add(label);
		DirectedSparseEdge e = new DirectedSparseEdge(v,random);
		e.addUserDatum(JUConstants.LABEL, labelSet, UserData.SHARED);
		try{
			machine.addEdge(e);
		}
		catch(edu.uci.ics.jung.exceptions.ConstraintViolationException e1){
			System.out.println("poor constraints from"+v+random);
			return false;
		}
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

	private Object pickRandom(Object[] possibles) {
		Random index = new Random();
		int size = possibles.length;
		int random = index.nextInt(size);
		return possibles[random];
	}

	private static Set<String> getAlphabet(int number){
		Set<String> alphabet = new HashSet<String>();
		for (int i=0;i<number;i++){
			String next = String.valueOf(i);
			alphabet.add(next);
		}
		return alphabet;
	}
	
}
