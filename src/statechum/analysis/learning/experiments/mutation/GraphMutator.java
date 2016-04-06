package statechum.analysis.learning.experiments.mutation;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.TreeSet;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.JUConstants;
import statechum.Label;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.linear.GD.LearnerGraphMutator;
import statechum.analysis.learning.linear.GD.PatchGraph;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;

public class GraphMutator<TARGET_A_TYPE,CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>>  {

	private AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> mutating;
	private LearnerGraphMutator<TARGET_A_TYPE,CACHE_A_TYPE> mutator;
	private Random r;
	private int addedStates = 0;
	private ChangesRecorderAsCollectionOfTransitions changesMade = new ChangesRecorderAsCollectionOfTransitions(null,true);
	
	/** This class displays the requested changes.
	 */
	public static final class ChangesRecorderAsCollectionOfTransitions implements PatchGraph
	{
		
		private Set<Transition> diff = new HashSet<Transition>();
		
		/** Whether seemingly duplicate mutations should be banned. */
		private boolean checkForInvalidMutations;
		
		/** Next instance of PatchGraph in a stack of observers. */
		private final PatchGraph next;

		public ChangesRecorderAsCollectionOfTransitions(PatchGraph nextInStack, boolean checkForInvalidMutationsArg)
		{
			next = nextInStack;checkForInvalidMutations = checkForInvalidMutationsArg;
		}
		
		public void checkSimilarTransition(CmpVertex from, Label label, CmpVertex to)
		{
			if (!checkForInvalidMutations)
				return;
			Transition t = new Transition(from,to,label);
			if (diff.contains(t))
				throw new FailureToMutateException("mutation is too similar to an earlier one, requested "+t);
		}
		
		@Override
		public void addTransition(CmpVertex from, Label label, CmpVertex to) {
			checkSimilarTransition(from, label, to);
			if (next != null) next.addTransition(from, label, to);

			diff.add(new Transition(from,to,label));
		}

		@Override
		public void removeTransition(CmpVertex from, Label label, CmpVertex to) {
			checkSimilarTransition(from, label, to);
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
		public void removeFromCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
			if (next != null) next.removeFromCompatibility(a,b,value);
		}

		@Override
		public void addRelabelling(VertID a, VertID b) {
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
					case 0:addEdgeBetweenExistingStates();mutationsDone++;break;
					case 1:mutationsDone+=addEdgeToNewState(mutations - mutationsDone);break;
					case 2:removeEdge();mutationsDone++;break;
					case 3:mutationsDone+=removeState(mutations - mutationsDone);break;
					default:throw new IllegalArgumentException("loop index out of range for possible mutations");
				}
				mutating.pathroutines.checkConsistency(mutating);
				// mutation successful
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
	protected Label randomLabel(){
		Set<Label> alphabet = mutating.pathroutines.computeAlphabet();
		return randomFromCollection(alphabet);
	}
	
	protected Label randomLabel(@SuppressWarnings("unused") CmpVertex v){
		Set<Label> labs = new TreeSet<Label>();
		labs.addAll(mutating.pathroutines.computeAlphabet());
		//labs.removeAll(buildAvoidSet(v));
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

	protected Set<Label> buildAvoidSet(CmpVertex from)
	{
		return mutating.getTransitionMatrix().get(from).keySet();
	}
	
	
	protected void addEdgeBetweenExistingStates()
	{
		CmpVertex from = selectRandomState();
		CmpVertex to = selectRandomState();
		Label label = randomLabel(from);
		Map<Label,TARGET_A_TYPE> targets = mutating.getTransitionMatrix().get(from);
		
		if (targets.containsKey(label))
		{
			for(CmpVertex tgt:mutating.getTargets(targets.get(label)))
				if (to == tgt)
					throw new FailureToMutateException("duplicate transition");
		}
		mutator.addTransition(from, label, to);
	}
	
	
	protected int addEdgeToNewState(int mutationsWeCanAccommodate){
		if (mutationsWeCanAccommodate < 2)
			throw new FailureToMutateException("in order to add an edge, we have to be able to manage two mutations, only one is available");
		CmpVertex from = selectRandomState();
		CmpVertex newV = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("added_"+addedStates++), Configuration.getDefaultConfiguration());
		mutator.addVertex(newV);
		Label newLabel = randomLabel();
		mutator.addTransition(from, newLabel, newV);
		return 2;
	}
	
	protected void removeEdge()
	{
		CmpVertex from = selectRandomStateWithOutEdges();// there are always transitions from the "from" state by construction.
		Map<Label, TARGET_A_TYPE> row = mutating.getTransitionMatrix().get(from);
		Label label = randomFromCollection(row.keySet());
		Collection<CmpVertex> dests = mutating.getTargets(row.get(label));
		CmpVertex dest = randomFromCollection(dests);
		mutator.removeTransition(from, label, dest);
		mutator.removeDanglingStates();// useful if we ended up removing all transitions from a state
	}
	
	protected List<Transition> computeWhichTransitionsToRemoveFor(CmpVertex stateToRemove)
	{
		List<Transition> transitionsToRemove = new LinkedList<Transition>();
		for(Entry<CmpVertex,Map<Label,TARGET_A_TYPE>> entry:mutating.getTransitionMatrix().entrySet())
		{
			for(Entry<Label,TARGET_A_TYPE> rowEntry:entry.getValue().entrySet())
			{
				for(CmpVertex targetVertex:mutating.getTargets(rowEntry.getValue()))
					if (entry.getKey() == stateToRemove || targetVertex == stateToRemove) transitionsToRemove.add(new Transition(entry.getKey(), targetVertex, rowEntry.getKey()));
			}
		}
		return transitionsToRemove;
	}
	
	protected int removeState(int maxMutationsWeCanAccommodate){
		
		int initSize = mutating.getStateNumber();
		if(initSize<3) throw new FailureToMutateException("the number of states is less than 3");
		
		Map<CmpVertex,List<Transition>> candidatesForRemoval = new TreeMap<CmpVertex,List<Transition>>();
		for(CmpVertex vert:mutating.getTransitionMatrix().keySet())
		{
			List<Transition> mut = computeWhichTransitionsToRemoveFor(vert);
			if (vert != mutating.getInit() && mut.size() < maxMutationsWeCanAccommodate)
				candidatesForRemoval.put(vert,mut);
		}
		CmpVertex stateToRemove = randomFromCollection(candidatesForRemoval.keySet());
		for(Transition tr:candidatesForRemoval.get(stateToRemove))
			mutator.removeTransition(tr.getFrom(), tr.getLabel(), tr.getTo());
		mutator.removeDanglingStates();
		return candidatesForRemoval.get(stateToRemove).size()+1;
	}
	
}
