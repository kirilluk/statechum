package statechum.analysis.learning.experiments;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.Map.Entry;
import java.util.TreeSet;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.GD.LearnerGraphMutator;
import statechum.analysis.learning.rpnicore.GD.PatchGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;

public class GraphMutator<TARGET_A_TYPE,CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>>  {

	private AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> mutating;
	private LearnerGraphMutator<TARGET_A_TYPE,CACHE_A_TYPE> mutator;
	private Random r;
	private int addedStates = 0;
	private ChangesRecorderAsCollectionOfTransitions changesMade = new ChangesRecorderAsCollectionOfTransitions(null);
	
	/** This class displays the requested changes.
	 */
	public static final class ChangesRecorderAsCollectionOfTransitions implements PatchGraph
	{
		private Set<Transition> diff = new HashSet<Transition>();
		
		/** Next instance of PatchGraph in a stack of observers. */
		private final PatchGraph next;

		public ChangesRecorderAsCollectionOfTransitions(PatchGraph nextInStack)
		{
			next = nextInStack;
		}
		
		@Override
		public void addTransition(CmpVertex from, String label, CmpVertex to) {
			if (next != null) next.addTransition(from, label, to);
			diff.add(new Transition(from,to,label));
		}

		@Override
		public void removeTransition(CmpVertex from, String label, CmpVertex to) {
			Transition trToRemove = new Transition(from,to,label);
			if (diff.contains(trToRemove))
				throw new FailureToMutateException("newly-added transition cannot be removed by another mutation");
			if (next != null) next.removeTransition(from, label, to);
			
			diff.add(new Transition(from,to,label));
		}
		
		@Override
		public String toString()
		{
			return diff.toString();
		}
		
		@Override
		public void setInitial(CmpVertex vertex) {
			if (next != null) next.setInitial(vertex);
		}

		@Override
		public void addToCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
			if (next != null) next.addToCompatibility(a,b,value);
		}

		@Override
		public void addVertex(CmpVertex vertex) {
			if (next != null) next.addVertex(vertex);
		}

		@Override
		public void removeFromCompatibility(CmpVertex a, CmpVertex b) {
			if (next != null) next.removeFromCompatibility(a,b);
		}

		@Override
		public void addRelabelling(VertexID a, VertexID b) {
			if (next != null) next.addRelabelling(a, b);
		}
		
		public Set<Transition>getDiff(){
			return diff;
		}
	}

	
	public GraphMutator(AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> mutating2, Random rArg){
		this.r = rArg;
		mutating = mutating2.copy(mutating2.config.copy());
		// it is important that the mutator above is asked first about the changes - 
		// it should be able to veto removal of transitions which have previously been added
		// or addition of those which were previously removed. 
		mutator = new LearnerGraphMutator<TARGET_A_TYPE,CACHE_A_TYPE>(mutating, mutating2.config.copy(), changesMade);
	}
	
	public AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> getMutated(){
		return mutating;
	}
	
	public Set<Transition>getDiff() {
		return changesMade.getDiff();
	}
	
	public void mutate(int mutations) 
	{
		int mutationsDone = 0;
		while(mutationsDone < mutations)
		{
			int choice = r.nextInt(4);
			try
			{
				switch(choice)
				{
					case 0:addEdgeBetweenExistingStates();break;
					case 1:addEdgeToNewState();break;
					case 2:removeEdge();break;
					case 3:removeState();break;
					default:throw new IllegalArgumentException("loop index out of range for possible mutations");
				}
				mutating.pathroutines.checkConsistency(mutating);
				mutationsDone++;// mutation successful
			}
			catch(FailureToMutateException e)
			{// failed to mutate, the next iteration thru the loop will make another attempt.
			}
		}
	}
	
	protected CmpVertex selectRandomState(){
		Set<CmpVertex> selectFrom = mutating.getTransitionMatrix().keySet();
		return randomFromCollection(selectFrom);
	}
	
	protected CmpVertex selectRandomStateWithOutEdges() 
	{
		Set<CmpVertex> states = new TreeSet<CmpVertex>();
		for(CmpVertex vert:mutating.getTransitionMatrix().keySet())
			if (!mutating.getTransitionMatrix().get(vert).isEmpty())
				states.add(vert);
		return randomFromCollection(states);
	}
	
	protected CmpVertex selectRandomStateNotInit(){
		Set<CmpVertex> selectFrom = new TreeSet<CmpVertex>();
		selectFrom.addAll(mutating.getTransitionMatrix().keySet());
		selectFrom.remove(mutating.getInit());
		return randomFromCollection(selectFrom);
	}
	
	/**
	 * Will select random alphabet element, could lead to nondeterminism.
	 */
	protected String randomLabel(){
		Set<String> alphabet = mutating.pathroutines.computeAlphabet();
		return randomFromCollection(alphabet);
	}
	
	protected String randomDeterministicLabel(CmpVertex v){
		Set<String> labs = new TreeSet<String>();
		labs.addAll(mutating.pathroutines.computeAlphabet());
		labs.removeAll(buildAvoidSet(v));
		return randomFromCollection(labs);
	}

	public static class FailureToMutateException extends IllegalArgumentException
	{
		/**
		 * serial ID
		 */
		private static final long serialVersionUID = -6536925107162966396L;
		
		public FailureToMutateException(String text)
		{
			super(text);
		}
	}
	
	
	@SuppressWarnings("unchecked")
	protected <ELEM> ELEM randomFromCollection(Collection<ELEM> set)
	{
		int size = set.size();if (size < 1) throw new FailureToMutateException("collection to choose from is empty");
		Object[] setArray = set.toArray();
		return (ELEM)setArray[r.nextInt(size)];
	}

	protected Set<String> buildAvoidSet(CmpVertex from)
	{
		return mutating.getTransitionMatrix().get(from).keySet();
	}
	
	
	protected void addEdgeBetweenExistingStates()
	{
		CmpVertex from = selectRandomState();
		CmpVertex to = selectRandomState();
		String label = randomDeterministicLabel(from);
		mutator.addTransition(from, label, to);
	}
	
	
	protected void addEdgeToNewState(){
		CmpVertex from = selectRandomState();
		CmpVertex newV = AbstractLearnerGraph.generateNewCmpVertex(new VertexID("added_"+addedStates++), Configuration.getDefaultConfiguration());
		mutator.addVertex(newV);
		String newLabel = randomLabel();
		mutator.addTransition(from, newLabel, newV);
	}
	
	protected void removeEdge()
	{
		CmpVertex from = selectRandomStateWithOutEdges();// there are always transitions from the "from" state by construction.
		Map<String, TARGET_A_TYPE> row = mutating.getTransitionMatrix().get(from);
		String label = randomFromCollection(row.keySet());
		Collection<CmpVertex> dests = mutating.getTargets(row.get(label));
		CmpVertex dest = randomFromCollection(dests);
		mutator.removeTransition(from, label, dest);
		mutator.removeDanglingStates();// useful if we ended up removing all transitions from a state
	}
	
	protected void removeState(){
		
		int initSize = mutating.getStateNumber();
		if(initSize<3) throw new FailureToMutateException("the number of states is less than 3");
		
		CmpVertex stateToRemove = selectRandomStateNotInit();
		List<Transition> transitionsToRemove = new LinkedList<Transition>();
		for(Entry<CmpVertex,Map<String,TARGET_A_TYPE>> entry:mutating.getTransitionMatrix().entrySet())
		{
			for(Entry<String,TARGET_A_TYPE> rowEntry:entry.getValue().entrySet())
			{
				for(CmpVertex targetVertex:mutating.getTargets(rowEntry.getValue()))
					if (entry.getKey() == stateToRemove || targetVertex == stateToRemove) transitionsToRemove.add(new Transition(entry.getKey(), targetVertex, rowEntry.getKey()));
			}
		}
		for(Transition tr:transitionsToRemove)
			mutator.removeTransition(tr.getFrom(), tr.getLabel(), tr.getTo());
		mutator.removeDanglingStates();
		//mutating.getTransitionMatrix().remove(stateToRemove);
	}
	
}
