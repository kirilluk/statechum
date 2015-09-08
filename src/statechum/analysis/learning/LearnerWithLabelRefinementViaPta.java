package statechum.analysis.learning;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.StringLabel;
import statechum.analysis.learning.ASE2014.EDSM_MarkovLearner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;

public class LearnerWithLabelRefinementViaPta extends ASE2014.EDSM_MarkovLearner 
{
	
	public static class PrevVertexAndOutgoingLabel
	{
		public PrevVertexAndOutgoingLabel(CmpVertex prevState, AbstractLabel lastInput) {
			vertPTA = prevState;label = lastInput;
		}
		public final AbstractLabel label;
		public final CmpVertex vertPTA;
	}

	final Map<Label,AbstractLabel> initialLabelToAbstractLabel = new TreeMap<Label,AbstractLabel>();
	
	/** Describes an abstract version of a label that has a string representation appearing like a real label and methods to manipulate abstract labels. */
	public static class AbstractLabel extends StringLabel
	{
		/**
		 * ID for serialisation.
		 */
		@SuppressWarnings("unused")
		private static final long serialVersionUID = -8106697725431488206L;			

		private final int abstractionInstance;
		private int lastChildNumber;
		
		private final AbstractLabel labelThisOneWasSplitFrom;
		private final Map<Label,AbstractLabel> initialLabelToAbstractLabel;
		
		//protected Map<Label> labelToAbstractLabel;
		
		/** Typically used when labels are passed from Erlang to Java. All subsequent labels are obtained by splitting existing ones. 
		 */
		public AbstractLabel(Map<Label,AbstractLabel> initialLabelToAbstract, String l, List<Label> ptaLabels) 
		{
			super(l);
			abstractionInstance = 0;// the parent of all possible abstractions.
			labelThisOneWasSplitFrom = null;// the top-level label
			labelsThisoneabstracts = new TreeSet<Label>(ptaLabels);// creates a copy.
			initialLabelToAbstractLabel = initialLabelToAbstract;
			updateInitialToAbstract();
		}

		/** Used to split labels. */
		protected AbstractLabel(Map<Label,AbstractLabel> initialLabelToAbstract, AbstractLabel parent, int abstractionNum) 
		{
			super(parent.getTopLevelName()+","+abstractionNum);
			assert abstractionNum > 0;
			abstractionInstance = abstractionNum;labelThisOneWasSplitFrom = parent;
			initialLabelToAbstractLabel = initialLabelToAbstract;
		}

		/** Creates a new label based on splitting out the labels provided from those this particular label contains. */
		public AbstractLabel splitLabel(Collection<Label> labels)
		{
			int nextID = getNextAbstractionInstance();
			AbstractLabel outcome = new AbstractLabel(initialLabelToAbstractLabel,this,nextID);
			
			assert labelsThisoneabstracts.containsAll(labels);
			
			outcome.labelsThisoneabstracts.removeAll(labels);// we do not change labels of the current abstract label, relabelling will be done when core graph is rebuilt following splitting of states.
			outcome.updateInitialToAbstract();// redirect changed labels to the outcome of splitting.
			return outcome;
		}

		public boolean isEmpty()
		{
			return labelsThisoneabstracts.isEmpty();
		}
		
		public void updateInitialToAbstract()
		{
			for(Label l:labelsThisoneabstracts)
				initialLabelToAbstractLabel.put(l,this);
		}
		
		public String getTopLevelName()
		{
			if (abstractionInstance != 0)
				return labelThisOneWasSplitFrom.getTopLevelName();
			
			return label;
			
		}
		
		public int getNextAbstractionInstance()
		{
			if (abstractionInstance != 0)
				return labelThisOneWasSplitFrom.getNextAbstractionInstance();
			
			return ++lastChildNumber;
		}
		
		/** Low-level labels abstracted by this label. */ 
		private Set<Label> labelsThisoneabstracts = null;

		@Override
		public int compareTo(Label o) 
		{
			if (!(o instanceof AbstractLabel))
				throw new IllegalArgumentException("Cannot compare an abstract label with a non-abstract label");
			return super.compareTo(o);// we can delegate this to a parent because we simply need the labels to be ordered, we do not really need the order to be related to the set of labels this label abstracts. 
		}

		@Override
		public boolean equals(Object obj) 
		{
			if (!(obj instanceof AbstractLabel))
				throw new IllegalArgumentException("Cannot compare an abstract label with a non-abstract label");
			return super.equals(obj) && labelsThisoneabstracts.equals(((AbstractLabel)obj).labelsThisoneabstracts);
		}
		
		@Override
		public String toString()
		{
			return String.format("Abstract(%1$d,{%2$s}",abstractionInstance,labelThisOneWasSplitFrom);
		}
	}
	
	
	public LearnerWithLabelRefinementViaPta(LearnerEvaluationConfiguration evalCnf, LearnerGraph argInitialPTA, int threshold) 
	{
		super(evalCnf, argInitialPTA, threshold);
	}

	protected LearnerGraphND coreInverse;
	
	public static class PrevAndLabel
	{
		final public VertID prev;
		final public Label label;
		
		public PrevAndLabel(VertID p, Label l)
		{
			prev = p;label = l;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((label == null) ? 0 : label.hashCode());
			result = prime * result + ((prev == null) ? 0 : prev.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			PrevAndLabel other = (PrevAndLabel) obj;
			if (label == null) {
				if (other.label != null)
					return false;
			} else if (!label.equals(other.label))
				return false;
			if (prev == null) {
				if (other.prev != null)
					return false;
			} else if (!prev.equals(other.prev))
				return false;
			return true;
		}
	}

	protected Map<VertID,PrevAndLabel> initialInverse;// for a PTA, an inverse is a deterministic graph.
	
	/** Creates a set of vertex identifiers, useful where I may choose to use a different underlying collection class such as to account for a large number of states. */ 
	protected static Set<VertID> createSetOfVertID(Collection<VertID> verticesToInclude)
	{
		if (verticesToInclude == null)
			return new TreeSet<VertID>();
		else
			return new TreeSet<VertID>(verticesToInclude);
	}
	
	/** Given a collection of negatives traces, marks states in the core graph as conflict ones where the same abstract state contains both positive and negative PTA (low-level) states. */ 
	public void constructMapOfInconsistentStates_fromTraces(Collection<List<Label>> concreteNegativeTraces)
	{
		final Configuration shallowCopy = getTentativeAutomaton().config.copy();shallowCopy.setLearnerCloneGraph(true);// since we are going to tweak t
		ptaHardFacts = new LearnerGraph(shallowCopy);// this is now cloned to eliminate counter-examples added to ptaSoftFacts by Spin

		Set<VertID> negatives = createSetOfVertID(null);
		for(List<Label> trace:concreteNegativeTraces)
		{
			//List<Label> abstractTrace = new ArrayList<Label>(trace.size());for(Label l:trace) abstractTrace.add(initialLabelToAbstractLabel.get(l));
			negatives.add(initialPTA.getVertex(trace)); 
		}
		constructMapOfInconsistentStates_fromRejectStates(negatives);
	}
	
	/** Describes a pair of PTA states that are in conflict. */
	public static class VertIDPair
	{
		public final VertID a,b;
		
		public VertIDPair(VertID argA,VertID argB)
		{
			a=argA;b=argB;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((a == null) ? 0 : a.hashCode());
			result = prime * result + ((b == null) ? 0 : b.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			VertIDPair other = (VertIDPair) obj;
			if (a == null) {
				if (other.a != null)
					return false;
			} else if (!a.equals(other.a))
				return false;
			if (b == null) {
				if (other.b != null)
					return false;
			} else if (!b.equals(other.b))
				return false;
			return true;
		}
	}
	
	public static class IncompatiblePTALabels
	{
		public final Label a,b;
		
		public IncompatiblePTALabels(Label argA, Label argB)
		{
			a=argA;b=argB;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((a == null) ? 0 : a.hashCode());
			result = prime * result + ((b == null) ? 0 : b.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			IncompatiblePTALabels other = (IncompatiblePTALabels) obj;
			if (a == null) {
				if (other.a != null)
					return false;
			} else if (!a.equals(other.a))
				return false;
			if (b == null) {
				if (other.b != null)
					return false;
			} else if (!b.equals(other.b))
				return false;
			return true;
		}
		
	}
	
	/** Since higher-level states partition PTA states, it is enough to simply have an incompatibility map between PTA vertices. 
	 * Initially, maps a positive to a negative, but subsequently additional pairs to split are added. 
	 * It is not reflexive because I need to iterate through it and I do not want to explore pairs that were visited earlier. 
	 */
	protected List<VertIDPair> incompatibleVertices = new ArrayList<VertIDPair>();
	
	/** Contains details of incompatible PTA labels, induced by state splitting. */
	protected Map<AbstractLabel,List<IncompatiblePTALabels>> incompatibleLabelsForAbstractLabel = new TreeMap<AbstractLabel,List<IncompatiblePTALabels>>();
	
	/** Associates an abstract state with every encountered incompatible PTA vertex. 
	 * Computed on the fly since we do not want to populate it with all the PTA vertices that are not participating in split states. 
	 */
	protected Map<VertID,CmpVertex> ptaStateToCoreState = new TreeMap<VertID,CmpVertex>();
	
	/** Used to add all low-level states corresponding to the supplied vertex of the core graph, to the ptaStateToCoreState map. 
	 * We do not do it for absolutely all vertices of the core graph since this is a lot of work and only some of the lower-level states are going to be conflicting. 
	 */
	public void updatePtaStateToCoreState(CmpVertex vert)
	{
		for(VertID v: coregraph.getCache().getMergedToHardFacts().get(vert))
			ptaStateToCoreState.put(v,vert);
	}
	
	/** Used to add all low-level states corresponding to the supplied vertex of the core graph, to the ptaStateToCoreState map. 
	 * We do not do it for absolutely all vertices of the core graph since this is a lot of work and only some of the lower-level states are going to be conflicting. 
	 */
	public void updatePtaStateToCoreState(Collection<CmpVertex> vertices)
	{
		for(CmpVertex vert:vertices) updatePtaStateToCoreState(vert);
	}

	private void updateInconsistentPTALabelMap(PrevAndLabel lblToA, PrevAndLabel lblToB)
	{
		AbstractLabel abstractLabel = initialLabelToAbstractLabel.get(lblToA.label);
		if (abstractLabel == initialLabelToAbstractLabel.get(lblToB.label))
		{
			List<IncompatiblePTALabels> il = incompatibleLabelsForAbstractLabel.get(abstractLabel);
			if (il == null)
			{
				il = new ArrayList<IncompatiblePTALabels>();incompatibleLabelsForAbstractLabel.put(abstractLabel,il);
			}
			il.add(new IncompatiblePTALabels(lblToA.label, lblToB.label));
		}
	}
	
	public void constructMapOfInconsistentStates_fromRejectStates(Set<VertID> ptaNegativeStates)
	{
		incompatibleVertices.clear();ptaStateToCoreState.clear();
		
		for(CmpVertex vert:coregraph.transitionMatrix.keySet())
		{
			Collection<VertID> ptaVertices = coregraph.getCache().getMergedToHardFacts().get(vert);
			Set<VertID> negatives = createSetOfVertID(ptaVertices);
			negatives.retainAll(ptaNegativeStates);
			if (!negatives.isEmpty())
			{// need to split this state.
				updatePtaStateToCoreState(vert);// low-level states for this state will be of interest.
				
				for(VertID pos:ptaVertices)
				{
					PrevAndLabel lblToPos = initialInverse.get(pos);
					if (!negatives.contains(pos))
						for(VertID v:negatives)
						{
							incompatibleVertices.add(new VertIDPair(pos, v));
							updateInconsistentPTALabelMap(lblToPos,initialInverse.get(v));
						}
				}
			}
		}
	}
	
	
	/** Constructs an inverse of the supplied PTA. */
	public static Map<VertID,PrevAndLabel> constructInitialInverse(LearnerGraph graph)
	{
		Map<VertID,PrevAndLabel> outcome = new TreeMap<VertID,PrevAndLabel>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
		{
			for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
				outcome.put(transition.getValue(),new PrevAndLabel(transition.getValue(), transition.getKey()));
		}
		return outcome;
	}
	
	/** Starting with a global set of incompatible vertices, extends it recursively where the high-level graph has transitions to the conflicting states from the same state. */ 
	public boolean split()
	{
		List<VertIDPair> 
			newIncompatibleVertices = incompatibleVertices,
			currentIncompatibleVertices = null;
		do
		{
			incompatibleVertices.addAll(newIncompatibleVertices);
			currentIncompatibleVertices = newIncompatibleVertices;newIncompatibleVertices = new ArrayList<VertIDPair>();
			for(VertIDPair ab:currentIncompatibleVertices)
			{
				PrevAndLabel lblToA = initialInverse.get(ab.a);
				PrevAndLabel lblToB = initialInverse.get(ab.b);
				
				if (lblToA.equals(lblToB))
					return false;// both previous states and labels match, hence the inconsistency cannot be resolved by splitting labels, therefore the last merge is invalid.
				
				if (lblToA.label.equals(lblToB.label))
				{// Same label, leading to more split states if there is an abstract state from which an abstract label leads to the current abstract state.

					Label abstractLabel = initialLabelToAbstractLabel.get(lblToA.label);
					CmpVertex highLevelState = ptaStateToCoreState.get(ab.a);
					assert highLevelState == ptaStateToCoreState.get(ab.b);
					
					VertID coreTargetA = ptaStateToCoreState.get(lblToA.prev), coreTargetB = ptaStateToCoreState.get(lblToB.prev);
					if (coreTargetA == null || coreTargetB == null)
						updatePtaStateToCoreState(coreInverse.transitionMatrix.get(highLevelState).get(abstractLabel));

					if (coreTargetA == null)
					{
						coreTargetA = ptaStateToCoreState.get(lblToA.prev);
						assert coreTargetA != null;
					}
					
					if (coreTargetB == null)
					{
						coreTargetB = ptaStateToCoreState.get(lblToB.prev);
						assert coreTargetB != null;
					}
					
					if (coreTargetA == coreTargetB)
					{// abstract labels lead to the same high-level state, hence this state has to be split.
						newIncompatibleVertices.add(new VertIDPair(lblToA.prev, lblToB.prev));
					}
				}
				else
				// the conflict can be resolved by marking the current pair of PTA labels inconsistent. 
					updateInconsistentPTALabelMap(lblToA, lblToB);
			}			
		}
		while(!newIncompatibleVertices.isEmpty());
		
		return true;
	}
	
	// Based on https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
	public static<T> List<T> BronKerbosch(final Map<T,Set<T>> labelToNeighbours, List<T> R, List<T> P, List<T> X)
	{
		List<T> outcome = null;

		if (!P.isEmpty() || !X.isEmpty())
		{
			int maxLabelNeighbours = -1;T maxLabel = null;
			for(T l: R)
				if (labelToNeighbours.get(l).size() > maxLabelNeighbours)
				{
					maxLabelNeighbours = labelToNeighbours.get(l).size();maxLabel = l;
				}
			
			assert maxLabel != null;
			// we'll use maxLabel as a pivot vertex.
			Set<T> adjacentToPivot = new TreeSet<T>(labelToNeighbours.get(maxLabel));
			
			for(T l:R)
				if (!adjacentToPivot.contains(l))
				{
					List<T> newR = new LinkedList<T>(R);newR.add(l);
					List<T> newP = new LinkedList<T>(P);newP.removeAll(labelToNeighbours.get(l));
					List<T> newX = new LinkedList<T>(X);newP.removeAll(labelToNeighbours.get(l));
					List<T> tentativeResponse = BronKerbosch(labelToNeighbours,newR,newP,newX);
					if (null != tentativeResponse && (outcome == null || outcome.size() < tentativeResponse.size()))
						outcome = tentativeResponse;
				}
		}
		else
			outcome = R;

		return outcome;
	}
	
	/** Iterates through the lists of states to split, identifying components of labels that are pairwise compatible. 
	 * Those that are not touched can be clustered with any of those, probably based on how often they are used from 
	 * the same state as the split ones. Presently, those not touched are gathered together as a separate transition. 
	 */
	public void constructCompatiblePartsOfLabels()
	{
		for(Entry<AbstractLabel,List<IncompatiblePTALabels>> il:incompatibleLabelsForAbstractLabel.entrySet())
		{
			// convert the list of pairs that are not compatible into a mapping from a pair to those it is compatible with.
			final Set<Label> allLabelsUsed = new TreeSet<Label>();for(IncompatiblePTALabels l:il.getValue()) { allLabelsUsed.add(l.a);allLabelsUsed.add(l.b); }
			final Map<Label,Set<Label>> labelToNeighbours = new TreeMap<Label,Set<Label>>();
			for(Label l:allLabelsUsed) { Set<Label> compatibleLabels = new TreeSet<Label>();compatibleLabels.addAll(allLabelsUsed);labelToNeighbours.put(l, compatibleLabels); }
			for(IncompatiblePTALabels l:il.getValue())
			{
				labelToNeighbours.get(l.a).remove(l.b);labelToNeighbours.get(l.b).remove(l.a);
			}
			
			// Now labelToNeighbours maps each pta label to a list of compatible ones. 
			List<AbstractLabel> newLabelsForCurrentOne = new LinkedList<AbstractLabel>();
			if (allLabelsUsed.isEmpty())
				newLabelsForCurrentOne.add(il.getKey()); // no conflicting labels, hence no splitting. Record the abstract transition as an outcome of a split.
			else
			{
				newLabelsForCurrentOne.add(il.getKey().splitLabel(allLabelsUsed));// the first label contains those pta labels that are compatible with all others.
				
				// Iterate through the list of labels, with the intention of finding groups of compatible ones.
				while(!allLabelsUsed.isEmpty())
				{
					List<Label> maxPartition = BronKerbosch(labelToNeighbours, new LinkedList<Label>(), new LinkedList<Label>(allLabelsUsed), new LinkedList<Label>());
					assert maxPartition != null;
					assert !maxPartition.isEmpty();
					newLabelsForCurrentOne.add(il.getKey().splitLabel(maxPartition));
					
					allLabelsUsed.removeAll(maxPartition);// remove the labels used in the current partition from the list of labels, therefore expecting to find next-large partition.
					for(Label l:allLabelsUsed)
						labelToNeighbours.get(l).retainAll(allLabelsUsed);// restrict the neighbours to labels that are available. 
				}
			}
		}
/*		
		// Now we need to compute partitions of split states, based on the partitions of transition labels into each state (it is the transitions into a state that motivated an inverse.
		final Map<Label,Set<Label>> labelToNeighbours = new TreeMap<Label,Set<Label>>();
		fianl Map<AbstractLabel,>
		for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:coreInverse.transitionMatrix.entrySet())
		{
			Map<VertID,CmpVertex> ptaStateToNewCoreState = new TreeMap<VertID,CmpVertex>();
			for(Entry<Label,List<CmpVertex>> transitionInto:entry.getValue().entrySet())
			{
				Collection<VertID> ptaStates = coregraph.getCache().getMergedToHardFacts().get(entry.getKey());
				for(VertID v:ptaStates)
				{
					PrevAndLabel prev = initialInverse.get(v);
					if (labelToNeighbours)
			}
		}
		*/
	}
}
