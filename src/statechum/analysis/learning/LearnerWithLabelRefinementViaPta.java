/* Copyright (c) 2015 The University of Sheffield
 * 
 * This file is part of StateChum.
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */

package statechum.analysis.learning;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.StringLabel;
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

	protected Map<Label,AbstractLabel> initialLabelToAbstractLabel = null;
	
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
	protected List<VertIDPair> incompatibleVertices = null;
	
	/** Contains details of incompatible PTA labels, induced by state splitting. */
	protected Map<AbstractLabel,List<IncompatiblePTALabels>> incompatibleLabelsForAbstractLabel = null;
	
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
							updateMergeInfo(pos,v);
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

	public static class PartitioningOfvertices
	{
		/** Describes what should be merged where to resolve non-determinism associated with multiple pta labels mapped to the same high-level label. */
		public final Map<VertID,List<VertID>> forcedMergers = new TreeMap<VertID,List<VertID>>();
		
		/** Associates each vertex with a list of compatible vertices, in order to cluster them. */
		public Map<VertID,Set<VertID>> stateCompatibility = null;
	}

	/** Constructs a compatibility map between states, for subsequent use of BronKerbosch. */ 
	public void updateMergeInfo(VertID a, VertID b)
	{
		//stateCompatibility = new TreeMap<VertID,Set<VertID>>();
		CmpVertex abstractVert = ptaStateToCoreState.get(a);assert abstractVert == ptaStateToCoreState.get(b);
		PartitioningOfvertices p = ptaMergers.get(abstractVert);
		if (p == null)
		{
			p = new PartitioningOfvertices();ptaMergers.put(abstractVert,p);
			Collection<VertID> ptaStates = coregraph.getCache().getMergedToHardFacts().get(abstractVert);
			for(VertID v:ptaStates)
			{
				boolean accept = initialPTA.findVertex(v).isAccept();
				Set<VertID> possibleStates = createSetOfVertID(ptaStates);possibleStates.remove(v);
				for(VertID other:possibleStates)
					if (initialPTA.findVertex(other).isAccept() == accept)
						p.stateCompatibility.put(v,possibleStates);
			}
			ptaMergers.put(abstractVert, p);
		}
		p.stateCompatibility.get(a).remove(b);p.stateCompatibility.get(b).remove(a);
	}
	
	/** If a pair of transitions leaving a pta state has a non-deterministic choice, target states need to be merged (if they are inconsistent, this would have been taken into account at the label-splitting stage). 
	 * Maps existing high-level state to a map associating states to be forcefully merged with some other states. The expectation is that that other state will either be merged somewhere else 
	 * or be the representative of the state of interest in BronKerbosch across the states that comprise the considered high-level state.
	 */  
	protected Map<CmpVertex,PartitioningOfvertices> ptaMergers = null; 
	
	/** Vertices to evaluate in identification of forced mergers. */
	protected Set<VertID> ptaStatesForCheckOfNondeterminism = null;
	
	/** Starting with a global set of incompatible vertices, extends it recursively where the high-level graph has transitions to the conflicting states from the same state. */ 
	public boolean splitIncompatibleStates()
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
				
				CmpVertex highLevelState = ptaStateToCoreState.get(ab.a);
				assert highLevelState == ptaStateToCoreState.get(ab.b);

				VertID corePreviousA = ptaStateToCoreState.get(lblToA.prev), corePreviousB = ptaStateToCoreState.get(lblToB.prev);
				AbstractLabel abstractLabelA = initialLabelToAbstractLabel.get(lblToA.label);
				AbstractLabel abstractLabelB = initialLabelToAbstractLabel.get(lblToB.label);
				if (corePreviousA == null)
					updatePtaStateToCoreState(coreInverse.transitionMatrix.get(highLevelState).get(abstractLabelA));
				if (corePreviousB == null)
					updatePtaStateToCoreState(coreInverse.transitionMatrix.get(highLevelState).get(abstractLabelB));

				if (corePreviousA == null)
				{
					corePreviousA = ptaStateToCoreState.get(lblToA.prev);
					assert corePreviousA != null;
				}
				
				if (corePreviousB == null)
				{
					corePreviousB = ptaStateToCoreState.get(lblToB.prev);
					assert corePreviousB != null;
				}

				if (corePreviousA == corePreviousB && lblToA.label.equals(lblToB.label))
				{
					// Same concrete label, leading to more split states if there is an abstract state from which an abstract label leads to the current abstract state.
					newIncompatibleVertices.add(new VertIDPair(lblToA.prev, lblToB.prev));
					updateMergeInfo(lblToA.prev, lblToB.prev);

					ptaStatesForCheckOfNondeterminism.add(lblToA.prev);ptaStatesForCheckOfNondeterminism.add(lblToB.prev);
					
					PrevAndLabel Aprev = initialInverse.get(lblToA.prev), Bprev = initialInverse.get(lblToB.prev);
					updatePtaStateToCoreState(coreInverse.transitionMatrix.get(highLevelState).get(initialLabelToAbstractLabel.get(Aprev.label)));
					updatePtaStateToCoreState(coreInverse.transitionMatrix.get(highLevelState).get(initialLabelToAbstractLabel.get(Bprev.label)));
					ptaStatesForCheckOfNondeterminism.add(Aprev.prev);ptaStatesForCheckOfNondeterminism.add(Bprev.prev);
				}
				
				if (lblToA.prev == lblToB.prev && abstractLabelA == abstractLabelB)
					// the conflict can be resolved by marking the current pair of PTA labels inconsistent. In terms of determinism, we are looking at a pta state with two targets that are not compatible, hence labels absolutely have to be split. 
					updateInconsistentPTALabelMap(lblToA, lblToB);
			}			
		}
		while(!newIncompatibleVertices.isEmpty());
		
		return true;
	}

	/** Non-determinism is resolved by merging target states, identified here. */
	protected void identifyForcedMergers()
	{
		Map<AbstractLabel,VertID> forcedMergersForPTAstate = new TreeMap<AbstractLabel,VertID>();
		List<VertID> verticesToEliminate = new LinkedList<VertID>();
		
		for(VertID vert:ptaStatesForCheckOfNondeterminism)
		{// check for the outgoing transitions with the same label, record one of them as a force merge (if any is ptaStatesWithForCheckOfNondeterminism, this needs to be noted in order to avoid merging such a state into an incompatible one). 
			forcedMergersForPTAstate.clear();
			PartitioningOfvertices p = ptaMergers.get(ptaStateToCoreState.get(vert));
			
			// First part: find a representative state for all the forced mergers, making sure it is one of ptaStatesWithForCheckOfNondeterminism
			for(Entry<Label,CmpVertex> transition:initialPTA.transitionMatrix.get(vert).entrySet())
			{
				AbstractLabel lbl = initialLabelToAbstractLabel.get(transition.getKey());
				VertID target = forcedMergersForPTAstate.get(lbl);
				if (target != null)
				{// targets and the current target are to be forcefully merged.
					assert transition.getValue().isAccept() == initialPTA.findVertex(target).isAccept();
					if (ptaStatesForCheckOfNondeterminism.contains(transition.getValue()))
						forcedMergersForPTAstate.put(lbl, transition.getValue());
				}
				else
					forcedMergersForPTAstate.put(lbl, transition.getValue());
			}

			// Now associate states that are supposed to be merged, with the chosen representative state.
			for(Entry<Label,CmpVertex> transition:initialPTA.transitionMatrix.get(vert).entrySet())
			{
				AbstractLabel lbl = initialLabelToAbstractLabel.get(transition.getKey());
				VertID target = forcedMergersForPTAstate.get(lbl);
				if (target != transition.getValue())
				{// targets and the current target are to be forcefully merged.
					List<VertID> vertsToMerge = p.forcedMergers.get(target);if (vertsToMerge == null) { vertsToMerge = new ArrayList<VertID>();p.forcedMergers.put(target,vertsToMerge); }
					p.forcedMergers.get(target).add(transition.getValue());
					if (ptaStatesForCheckOfNondeterminism.contains(transition.getValue()))
						// this is where we are merging multiple compatible states, which is why they should not appear in the list of compatible ones - if they stay there, they may be clustered with different states.
						verticesToEliminate.add(transition.getValue());
				}
			}
		}
		
		// Now eliminate vertices that were marked for deletion in verticesToEliminate.
		for(PartitioningOfvertices p:ptaMergers.values())
			eliminateElementsFromCompatibilityMap(p.stateCompatibility,verticesToEliminate);
	}

	protected void constructTransitionsForState(LearnerGraph outcome, Map<VertID,CmpVertex> ptaToNewState, CmpVertex vert)
	{
		Map<VertID,Collection<VertID>> mergedToPta = outcome.getCache().getMergedToHardFacts();
		Map<Label, CmpVertex> transitions = outcome.transitionMatrix.get(vert);
		for(VertID v:mergedToPta.get(vert))
		{
			for(Entry<Label,CmpVertex> outgoing:initialPTA.transitionMatrix.get(v).entrySet())
			{
				CmpVertex target = ptaToNewState.get(outgoing.getValue());
				if (target == null) 
					target=ptaStateToCoreState.get(outgoing.getValue());
				CmpVertex origTarget = transitions.put(initialLabelToAbstractLabel.get(outgoing.getKey()),target);
				assert origTarget == null || origTarget == target;// otherwise a non-deterministic choice.
			}
		}
		
	}

	/** Constructs an updated transition diagram and returns it. */
	protected LearnerGraph partitionVerticesThatArePartOfAbstractStates()
	{
		final Configuration shallowCopy = getTentativeAutomaton().config.copy();shallowCopy.setLearnerCloneGraph(false);// since we are not going to change acceptance conditions of states.
		LearnerGraph outcome = new LearnerGraph(shallowCopy);
		Map<VertID,CmpVertex> ptaToNewState = new TreeMap<VertID,CmpVertex>();
		List<CmpVertex> newStates = new ArrayList<CmpVertex>();
		Map<VertID,Collection<VertID>> mergedToPta = outcome.getCache().getMergedToHardFacts();
		
		for(Entry<CmpVertex,PartitioningOfvertices> entry:ptaMergers.entrySet())
		{
			Map<VertID,Set<VertID>> vertToNeighbours = entry.getValue().stateCompatibility;
			while(!vertToNeighbours.isEmpty())
			{
				List<VertID> maxPartition = BronKerbosch(vertToNeighbours, new LinkedList<VertID>(), new LinkedList<VertID>(vertToNeighbours.keySet()), new LinkedList<VertID>());
				assert maxPartition != null;
				assert !maxPartition.isEmpty();
				
				eliminateElementsFromCompatibilityMap(vertToNeighbours,maxPartition);// restrict the neighbours to vertices that are available.
				
				// Now maxPartition is a set of states that we split the current state into.
				boolean isAccept = initialPTA.findVertex(maxPartition.iterator().next()).isAccept();
				CmpVertex newVertex = AbstractLearnerGraph.generateNewCmpVertex(outcome.nextID(isAccept),shallowCopy);
				outcome.transitionMatrix.put(newVertex, outcome.createNewRow());
				outcome.transitionMatrix.remove(entry.getKey());
				Collection<VertID> verticesForNewVertex = createSetOfVertID(maxPartition);
				
				// New high-level state created, add states that have been merged into any state among those added to this high-level state
				for(VertID v:maxPartition)
				{
					List<VertID> statesToMerge = entry.getValue().forcedMergers.get(v);
					if (statesToMerge != null) verticesForNewVertex.addAll(statesToMerge);
				}
				newStates.add(newVertex);
				for(VertID v:verticesForNewVertex) 
					ptaToNewState.put(v, newVertex);
				mergedToPta.put(newVertex,verticesForNewVertex);
			} 
		}
		
		// All new states created, now construct transitions to/from those in keyset of ptaMergers. This is done by 
		for(CmpVertex vert:newStates)
			constructTransitionsForState(outcome,ptaToNewState,vert);
		
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
			if (!ptaMergers.containsKey(entry.getKey()))
			{
				boolean targetInMergedStates = false;
				for(CmpVertex v:entry.getValue().values())
					if (ptaMergers.containsKey(v))
					{
						targetInMergedStates = true;break;
					}
				
				if (targetInMergedStates)
					constructTransitionsForState(outcome,ptaToNewState,entry.getKey());
			}
		
		// Now remove all the original states
		for(CmpVertex v:ptaMergers.keySet())
			outcome.transitionMatrix.remove(v);
		
		return outcome;
	}
	
	/** Adjacency map should be reflexive but not idempotent. */
	public static <T> void checkReflexivity(final Map<T,Set<T>> labelToNeighbours)
	{
		for(Entry<T,Set<T>> entry:labelToNeighbours.entrySet())
		{
			if (entry.getValue().contains(entry.getKey()))
				throw new IllegalArgumentException("Adjacency map should not have a vertex mapped to itself, got: pair "+entry.getKey()+"-"+entry.getValue());
			for(T value:entry.getValue())
			{
				Set<T> entriesForValue = labelToNeighbours.get(value);
				if (entriesForValue == null)
					throw new IllegalArgumentException("Pair "+entry.getKey()+"-"+value+" does not have "+value+" mapped anywhere");
				if (!entriesForValue.contains(entry.getKey()))
					throw new IllegalArgumentException("Pair "+entry.getKey()+"-"+value+" does not have map("+value+") mapped to "+entry.getKey());
			}
		}
	}
	
	/** Computes a set of partitions based on compatibility provided with the supplied map.  Based on https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
	 */ 
	public static<T> List<T> BronKerbosch(final Map<T,Set<T>> adjacencyMap, List<T> R, List<T> P, List<T> X)
	{
		checkReflexivity(adjacencyMap);
		List<T> outcome = null;

		if (!P.isEmpty() || !X.isEmpty())
		{
			int maxLabelNeighbours = -1;T maxLabel = null;
			for(T l: P)
				if (adjacencyMap.get(l).size() > maxLabelNeighbours)
				{
					maxLabelNeighbours = adjacencyMap.get(l).size();maxLabel = l;
				}
			
			assert maxLabel != null;
			// we'll use maxLabel as a pivot vertex.
			Set<T> adjacentToPivot = new TreeSet<T>(adjacencyMap.get(maxLabel));
			
			for(T l:P)
				if (!adjacentToPivot.contains(l))
				{
					List<T> newR = new LinkedList<T>(R);newR.add(l);
					List<T> newP = new LinkedList<T>(P);newP.retainAll(adjacencyMap.get(l));
					List<T> newX = new LinkedList<T>(X);newP.retainAll(adjacencyMap.get(l));
					List<T> tentativeResponse = BronKerbosch(adjacencyMap,newR,newP,newX);
					if (null != tentativeResponse && (outcome == null || outcome.size() < tentativeResponse.size()))
						outcome = tentativeResponse;
				}
		}
		else
			outcome = R;

		return outcome;
	}
	
	/** Eliminates the supplied entries from the map. If called after BronKerbosch, this permitting repeated calls to BronKerbosch to report maximum clusters that are both disjoint and of decreasing size. */
	public static <T> void eliminateElementsFromCompatibilityMap(final Map<T,Set<T>> labelToNeighbours, List<T> whatToEliminate)
	{
		for(T elem:whatToEliminate)
			labelToNeighbours.remove(elem);
		for(Entry<T,Set<T>> entry:labelToNeighbours.entrySet())
			entry.getValue().retainAll(whatToEliminate);// restrict the neighbours to labels that are available.
		
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
			//List<AbstractLabel> newLabelsForCurrentOne = new LinkedList<AbstractLabel>();
			if (!allLabelsUsed.isEmpty())
			//	newLabelsForCurrentOne.add(il.getKey()); // no conflicting labels, hence no splitting. Record the abstract transition as an outcome of a split.
			//else
			{
				//newLabelsForCurrentOne.add(il.getKey().splitLabel(allLabelsUsed));// the first label contains those pta labels that are compatible with all others.
				
				// Iterate through the list of labels, with the intention of finding groups of compatible ones.
				while(!labelToNeighbours.isEmpty())
				{
					List<Label> maxPartition = BronKerbosch(labelToNeighbours, new LinkedList<Label>(), new LinkedList<Label>(labelToNeighbours.keySet()), new LinkedList<Label>());
					assert maxPartition != null;
					assert !maxPartition.isEmpty();
					il.getKey().splitLabel(maxPartition); // populates initialLabelToAbstractLabel 
					//newLabelsForCurrentOne.add(il.getKey().splitLabel(maxPartition));
					
					//allLabelsUsed.removeAll(maxPartition);// remove the labels used in the current partition from the list of labels, therefore expecting to find next-large partition.
					eliminateElementsFromCompatibilityMap(labelToNeighbours,maxPartition); // restrict the neighbours to labels that are available.
				}
			}
		}
	}
	
	/** Initialisation of all the data structures used in splitting states. Useful if the process of splitting is re-run. */
	public void init()
	{
		ptaStatesForCheckOfNondeterminism = new TreeSet<VertID>();
		initialLabelToAbstractLabel = new TreeMap<Label,AbstractLabel>();
		incompatibleVertices = new ArrayList<VertIDPair>();
		incompatibleLabelsForAbstractLabel = new TreeMap<AbstractLabel,List<IncompatiblePTALabels>>();
		
		initialInverse = constructInitialInverse(initialPTA);	
	}
	
	/** Identifies where states and/or labels have to be split if the supplied positive and negative traces are added. Returns a new transition diagram. */
	public LearnerGraph refineGraph(Collection<List<Label>> concreteNegativeTraces)
	{
		init();
		constructMapOfInconsistentStates_fromTraces(concreteNegativeTraces);
		splitIncompatibleStates();
		constructCompatiblePartsOfLabels();
		identifyForcedMergers();
		return partitionVerticesThatArePartOfAbstractStates();
	}
}
