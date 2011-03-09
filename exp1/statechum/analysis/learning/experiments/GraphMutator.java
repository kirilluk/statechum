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
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;

public class GraphMutator {
	
	

	private LearnerGraphND mutating;
	private boolean allowNonDeterminism = false;
	private Set<Transition> added = new HashSet<Transition>();
	private Set<Transition> removed = new HashSet<Transition>();
	private Map<CmpVertex, Map<String, List<CmpVertex>>> preds = new HashMap<CmpVertex,Map<String,List<CmpVertex>>>();
	private Random r = new Random();
	
	public LearnerGraphND getMutated(){
		return mutating;
	}
	
	public Set<Transition>getDiff() {
		Set<Transition> returnSet = added;
		returnSet.addAll(removed);
		return returnSet;
	}
	
	public GraphMutator(LearnerGraphND mutating2, boolean nondet){
		mutating = mutating2;
		preds = computePreds(mutating.getTransitionMatrix());
		this.allowNonDeterminism = nondet;
	}
	
	public void mutate(int mutations) {
		for(int i = 0; i< mutations;i++){
			int choice = r.nextInt(4);
			boolean success = false;
			int attempts = 0;
			while(!success&&attempts<20){
				attempts++;
				if(choice == 0)
					success = addEdgeBetweenExistingStates();
				else if(choice == 1)
					success = addEdgeToNewState();
				else if(choice == 2)
					success = removeEdge();
				else if(choice == 3)
					success = removeState();
			}
			if(attempts == 100)
				System.out.println("MUTATIONS FAILED");
		}
	}
	
	protected CmpVertex selectRandomState(){
		Set<CmpVertex> selectFrom = mutating.getTransitionMatrix().keySet();
		return (CmpVertex)randomFromCollection(selectFrom);
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
	
	protected Object randomFromCollection(Collection set){
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
			if(allowNonDeterminism)
				label = randomLabel();
			else
				label = randomDeterministicLabel(from);
			mutating.addTransition(mutating.getTransitionMatrix().get(from), label, to);
			added.add(new Transition(from.getID().getStringId(), to.getID().getStringId(), label));
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
			if(allowNonDeterminism)
				label = randomLabel();
			else
				label = randomDeterministicLabel(from);
			CmpVertex newV = mutating.addVertex(from, true, label);
			String newLabel = randomLabel();
			mutating.addTransition(mutating.getTransitionMatrix().get(newV), newLabel, to);
			added.add(new Transition(from.getID().getStringId(), newV.getID().getStringId(), label));
			added.add(new Transition(newV.getID().getStringId(), to.getID().getStringId(), newLabel));
		}
		catch(Exception e){
			return false;
		}
		return true;
	}
	
	protected boolean removeEdge(){
		CmpVertex from = selectRandomState();
		Map<String, List<CmpVertex>> row = mutating.getTransitionMatrix().get(from);
		if(!row.keySet().isEmpty()){
			String label = (String)randomFromCollection(row.keySet());
			CmpVertex to = (CmpVertex)randomFromCollection(row.get(label));
			mutating.removeTransition(row, label, to);
			removed.add(new Transition(from.getID().getStringId(), to.getID().getStringId(), label));
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
		Map<String,List<CmpVertex>> outgoing = mutating.getTransitionMatrix().get(remove);
		if(outgoing!=null){
			removeTransitions(buildRemoveSet(remove,removed,outgoing, true));
		}
		Map<String,List<CmpVertex>> incoming = preds.get(remove);
		if(incoming!=null){
			removeTransitions(buildRemoveSet(remove,removed,incoming, false));
		}
		mutating.getTransitionMatrix().remove(remove);
		return true;
	}
	
	

	private Set<CmpTransition> buildRemoveSet(CmpVertex remove, Set<Transition> removed2,
			Map<String, List<CmpVertex>> transitions, boolean out) {
		Set<CmpTransition> removeSet = new HashSet<CmpTransition>();
		for (String label : transitions.keySet()) {
			List<CmpVertex> others = transitions.get(label);
			for (CmpVertex cmpVertex : others) {
				if(out){
					removeSet.add(new CmpTransition(remove,cmpVertex,label));
					removed.add(new Transition(remove.getID().getStringId(),cmpVertex.getID().getStringId(),label));
				}
				else{
					removeSet.add(new CmpTransition(cmpVertex,remove,label));
					removed.add(new Transition(cmpVertex.getID().getStringId(),remove.getID().getStringId(),label));
				}
			}
				
		}
		return removeSet;
	}
	
	
	private void removeTransitions(Set<CmpTransition> rmTransitions) {
		for (CmpTransition cmpTrans : rmTransitions) {
			CmpVertex from = cmpTrans.getFrom();
			CmpVertex to = cmpTrans.getTo();
			String label = cmpTrans.getLabel();
			mutating.removeTransition(mutating.getTransitionMatrix().get(from), label, to);
		}
		preds = computePreds(mutating.getTransitionMatrix());
		
	}
	
	
	
	

	private Map<CmpVertex, Map<String,List<CmpVertex>>> computePreds(Map<CmpVertex, Map<String,List<CmpVertex>>> matrix) {
		Map<CmpVertex, Map<String,List<CmpVertex>>> preds = new HashMap<CmpVertex,Map<String,List<CmpVertex>>>();
		Set<CmpVertex> keys = matrix.keySet();
		for (CmpVertex cmpVertex : keys) {
			Map<String,List<CmpVertex>> dests = matrix.get(cmpVertex);
			Set<String> labels = dests.keySet();
			for (String string : labels) {
				List<CmpVertex> destinations = dests.get(string);
				for (CmpVertex destination : destinations) {
					Map<String,List<CmpVertex>> pred;
					if(preds.get(destination)==null){
						pred = new HashMap<String,List<CmpVertex>>();
						pred.put(string, newListWithVertex(cmpVertex));
						
					}
					else{
						pred = preds.get(destination);
						if(pred.get(string) == null){
							pred.put(string, newListWithVertex(cmpVertex));
						}
						else{
							pred.get(string).add(cmpVertex);
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
	
	public class CmpTransition {
		private CmpVertex from, to;
		private String label;
		
		
		public CmpTransition(CmpVertex from, CmpVertex to, String label) {
			super();
			this.from = from;
			this.to = to;
			this.label = label;
		}
		public CmpVertex getFrom() {
			return from;
		}
		public CmpVertex getTo() {
			return to;
		}
		public String getLabel() {
			return label;
		}
	}

}
