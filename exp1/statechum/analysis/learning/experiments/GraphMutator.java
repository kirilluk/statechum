package statechum.analysis.learning.experiments;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.GD.LearnerGraphMutator;
import statechum.analysis.learning.rpnicore.GD.PatchGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;

public class GraphMutator<TARGET_A_TYPE,CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>>  {

	private LearnerGraphND mutating;
	private LearnerGraphMutator<TARGET_A_TYPE,CACHE_A_TYPE> mutator;
	private Set<Transition> added = new HashSet<Transition>();
	private Set<Transition> removed = new HashSet<Transition>();
	private Map<CmpVertex, Map<String, List<CmpVertex>>> preds = new HashMap<CmpVertex,Map<String,List<CmpVertex>>>();
	private Random r;
	private int addedStates = 0;
	
	public GraphMutator(LearnerGraphND mutating2, Random r){
		this.r = r;
		mutating = (LearnerGraphND) mutating2.copy(Configuration.getDefaultConfiguration());
		Configuration conf = Configuration.getDefaultConfiguration();
		mutator = new LearnerGraphMutator(mutating2.copy(conf), conf, null);
		preds = computePreds(mutating.getTransitionMatrix());
	}
	
	public void changeStateLabels(){
		for (CmpVertex vertex : mutating.getTransitionMatrix().keySet()) {
			VertexID id = vertex.getID();
			vertex.setID(new VertexID(id.getStringId()+"mut"));
		}
	}
	
	
	private void mutate(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> result){
		mutator.removeDanglingStates();
		mutator.relabel(result);
	}
	
	public LearnerGraphND getMutated(){
		return mutating;
	}
	
	public Set<Transition>getDiff() {
		Set<Transition> returnSet = added;
		returnSet.addAll(removed);
		return returnSet;
	}
	
	
	
	public void mutate(int mutations) {
		try{
			for(int i = 0; i< mutations;i++){
				int choice = r.nextInt(4);
				if(choice == 0)
					addEdgeBetweenExistingStates();
				else if(choice == 1)
					addEdgeToNewState();
				else if(choice == 2)
					removeEdge();
				else if(choice == 3)
					removeState();
				update();
			}
			
		}
		catch(Exception e){
			e.printStackTrace();
		}
	}
	
	private void update() {
		mutate((AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE>) mutating);
		preds = computePreds(mutating.getTransitionMatrix());
	}


	protected CmpVertex selectRandomState(){
		Set<CmpVertex> selectFrom = mutating.getTransitionMatrix().keySet();
		return (CmpVertex)randomFromCollection(selectFrom);
	}
	
	protected CmpVertex selectRandomStateWithOutEdges() throws Exception{
		boolean found = false;
		CmpVertex returnState = null;
		Set<CmpVertex> states = new HashSet<CmpVertex>();
		states.addAll(mutating.getTransitionMatrix().keySet());
		while(found == false){
			
			returnState = (CmpVertex)randomFromCollection(states);
			if(!mutating.getTransitionMatrix().get(returnState).isEmpty())
				found = true;
			else{
				states.remove(returnState);
				if(states.isEmpty())
					throw new Exception("Could not find state with outgoing edge, graph must be corrupted");
			}
			
		}
		return returnState;
	}
	
	protected CmpVertex selectRandomStateNotInit(){
		Set<CmpVertex> selectFrom = new HashSet<CmpVertex>();
		selectFrom.addAll(mutating.getTransitionMatrix().keySet());
		selectFrom.remove(mutating.getInit());
		return (CmpVertex)randomFromCollection(selectFrom);
	}
	
	

	/*
	 * Will select random alphabet element, but will not return an element if it could
	 * lead to non-determinism
	 */
	protected String randomLabel(Set<String> avoid){
		Set<String> alphabet = mutating.pathroutines.computeAlphabet();
		alphabet.removeAll(avoid);
		return (String)randomFromCollection(alphabet);
	}
	
	/*
	 * Will select random alphabet element, could lead to nondeterminism.
	 */
	protected String randomLabel(){
		Set<String> alphabet = mutating.pathroutines.computeAlphabet();
		return (String)randomFromCollection(alphabet);
	}
	
	protected String randomDeterministicLabel(CmpVertex v){
		Set<String> labs = new HashSet<String>();
		labs.addAll(mutating.pathroutines.computeAlphabet());
		labs.removeAll(buildAvoidSet(v));
		return randomLabel(labs);
	}
	
	protected Object randomFromCollection(Collection<?> set){
		int size = set.size();
		Object[] setArray = set.toArray();
		return setArray[r.nextInt(size)];
	}
	
	
	
	protected Set<String> buildAvoidSet(CmpVertex from){
		HashSet<String> avoid = new HashSet<String>();
		avoid.addAll(mutating.getTransitionMatrix().get(from).keySet());
		return avoid;
	}
	
	
	protected boolean addEdgeBetweenExistingStates(){
		CmpVertex from = selectRandomState();
		CmpVertex to = selectRandomState();
		try{
			String label = null;
			label = randomDeterministicLabel(from);
			mutator.addTransition(from, label, to);
			//mutating.addTransition(mutating.getTransitionMatrix().get(from), label, to);
			added.add(new Transition(from, to, label));
		}
		catch(Exception e){
			return false;
		}
		return true;
	}
	
	
	protected boolean addEdgeToNewState(){
		CmpVertex from = selectRandomState();
		CmpVertex to = selectRandomState();
		try{
			String label = null;
			label = randomDeterministicLabel(from);
			CmpVertex newV = AbstractLearnerGraph.generateNewCmpVertex(new VertexID("added"+addedStates++), Configuration.getDefaultConfiguration());
			mutator.addVertex(newV);
			String newLabel = randomLabel();
			mutator.addTransition(from, newLabel, newV);
			//mutating.addTransition(mutating.getTransitionMatrix().get(newV), newLabel, to);
			
			added.add(new Transition(from, newV, label));
			added.add(new Transition(newV, to, newLabel));
		}
		catch(Exception e){
			return false;
		}
		return true;
	}
	
	protected boolean removeEdge() throws Exception{
		CmpVertex from = selectRandomStateWithOutEdges();
		Map<String, List<CmpVertex>> row = mutating.getTransitionMatrix().get(from);
		if(!row.keySet().isEmpty()){
			String label = (String)randomFromCollection(row.keySet());
			List<CmpVertex> dests = row.get(label);
			CmpVertex dest = (CmpVertex)randomFromCollection(dests);
			mutator.removeTransition(from, label, dest);
			//mutating.removeTransition(row, label, to);
			removed.add(new Transition(from, dest, label));
			return true;
		}
		else
			return false;
	}
	
	protected boolean removeState(){
		
		int initSize = mutating.getStateNumber();
		if(initSize<3){
			return false;
		}
		CmpVertex remove = selectRandomStateNotInit();
		Map<String, List<CmpVertex>> outgoing = mutating.getTransitionMatrix().get(remove);
		if(outgoing!=null){
			removeTransitions(constructOutgoingSet(remove,outgoing));
		}
		Map<String,List<CmpVertex>> incoming = preds.get(remove);
		if(incoming!=null){
			removeTransitions(constructIncomingSet(remove,incoming));
		}
		//mutating.getTransitionMatrix().remove(remove);
		return true;
	}
	
	

	private Set<Transition> constructIncomingSet(CmpVertex remove, Map<String, List<CmpVertex>> outgoing) {
		Set<Transition> removeSet = new HashSet<Transition>();
		for (String label : outgoing.keySet()) {
			List<CmpVertex> others = outgoing.get(label);
			for (CmpVertex cmpVertex : others) {
				removeSet.add(new Transition(cmpVertex,remove,label));
				removed.add(new Transition(cmpVertex,remove,label));
			}
				
		}
		return removeSet;
	}
	
	private Set<Transition> constructOutgoingSet(CmpVertex remove,Map<String, List<CmpVertex>> outgoing) {
		Set<Transition> removeSet = new HashSet<Transition>();
		for (String label : outgoing.keySet()) {
			List<CmpVertex> others = outgoing.get(label);
			for (CmpVertex cmpVertex : others) {
				if(cmpVertex.equals(remove))
					continue; //otherwise we will end up removing the same transition twice
				removeSet.add(new Transition(remove,cmpVertex,label));
				removed.add(new Transition(remove,cmpVertex,label));
			}
				
		}
		return removeSet;
	}
	
	
	private void removeTransitions(Set<Transition> rmTransitions) {
		for (Transition cmpTrans : rmTransitions) {
			CmpVertex from = cmpTrans.getFrom();
			CmpVertex to = cmpTrans.getTo();
			String label = cmpTrans.getLabel();	
			mutator.removeTransition(from, label, to);
			//mutating.removeTransition(mutating.getTransitionMatrix().get(from), label, to);
		}
		
	}
	
	
	
	

	private Map<CmpVertex, Map<String,List<CmpVertex>>> computePreds(Map<CmpVertex, Map<String, List<CmpVertex>>> map) {
		Map<CmpVertex, Map<String,List<CmpVertex>>> preds = new HashMap<CmpVertex,Map<String,List<CmpVertex>>>();
		Set<CmpVertex> keys = map.keySet();
		for (CmpVertex cmpVertex : keys) { //iterate through all vertices
			Map<String, List<CmpVertex>> dests = map.get(cmpVertex);
			Set<String> labels = dests.keySet();
			for (String string : labels) { //for each vertex v iterate through its outgoing alphabet a
				
				List<CmpVertex> destinations = dests.get(string);
				for (CmpVertex destination : destinations) { //for each alphabet member get destination d
					Map<String,List<CmpVertex>> pred;
					if(preds.get(destination)==null){ //if destination d does not have any stored predecessors
						pred = new HashMap<String,List<CmpVertex>>();
						pred.put(string, newListWithVertex(cmpVertex)); //add v, with label a
					}
					else{ //if destination d does have stored predecessors
						pred = preds.get(destination); //get the existing list of predecessors pred
						if(pred.get(string) == null){ //if there is no predecessor that reaches d with a
							pred.put(string, newListWithVertex(cmpVertex)); // add it in its own list
						}
						else{ //otherwise
							pred.get(string).add(cmpVertex); //add it to the existing list list
						}
					}
					preds.put(destination, pred);
				}
			}
		}
		return preds;
	}

	private List<CmpVertex> newListWithVertex(CmpVertex cmpVertex) {
		List<CmpVertex> vertices = new ArrayList<CmpVertex>();
		vertices.add(cmpVertex);
		return vertices;
	}
	
}
