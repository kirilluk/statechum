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
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import java.util.Queue;

import statechum.Configuration;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.StringLabel;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.Transform;

public class LearnerWithLabelRefinementViaPta extends MarkovEDSM.EDSM_MarkovLearner 
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
		
		/** This is a reference to a map that is part of the {@link LearnerWithLabelRefinementViaPta} instance, in order to avoid creating a non-static class. */
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
			wasSplit = true;

			AbstractLabel outcome = new AbstractLabel(initialLabelToAbstractLabel,this,nextID);
			
			assert labelsThisoneabstracts.containsAll(labels);
			outcome.labelsThisoneabstracts = new TreeSet<Label>(labelsThisoneabstracts);
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
			if (labelThisOneWasSplitFrom == null)
				return String.format("Abstract(%1$d,%2$s)",abstractionInstance,labelsThisoneabstracts);
			return String.format("Abstract(%1$d,%2$s,{%3$s})",abstractionInstance,labelsThisoneabstracts,getTopLevelName());
		}
		
		/** Abstracted graphs cannot be directly compared to ordinary ones because abstract labels resent comparing to any other labels. This transform turns an abstract graph into a typical graph, returning the result. */
		public static LearnerGraph convertAbstractGraphToTextGraph(LearnerGraph orig)
		{
			final LearnerGraph outcome = new LearnerGraph(orig.config);
			AbstractLearnerGraph.interpretLabelsOnGraph(orig,outcome,
					new Transform.ConvertLabel(new Transform.ConvertALabel() {
	
					@Override
					public Label convertLabelToLabel(Label label) {
						return new StringLabel( ((AbstractLabel)label).label );
					}
				}));
			return outcome;
		}
		
		/** Contains details of incompatible PTA labels, induced by state splitting. */
		protected List<IncompatiblePTALabels> incompatibleLabelsForAbstractLabel = null;
		
		/** If this label was split (hence mandating changes to states with transitions labelled by it), this flag is set. */
		protected boolean wasSplit = false;
		
		public boolean wasSplit()
		{
			return wasSplit;
		}
		
		/** Given a collection of components of this label that are in conflict, constructs a compatibility map. */
		public Map<Label,Set<Label>> constructCompatibilityMap()
		{
			if (incompatibleLabelsForAbstractLabel == null)
				return null;
			
			final Map<Label,Set<Label>> labelToNeighbours = new TreeMap<Label,Set<Label>>();
			for(Label l:labelsThisoneabstracts) { Set<Label> compatibleLabels = new TreeSet<Label>();compatibleLabels.addAll(labelsThisoneabstracts);compatibleLabels.remove(l);labelToNeighbours.put(l, compatibleLabels); }
			for(IncompatiblePTALabels l:incompatibleLabelsForAbstractLabel)
			{
				labelToNeighbours.get(l.a).remove(l.b);labelToNeighbours.get(l.b).remove(l.a);
			}
			return labelToNeighbours;
		}
		
		/** Undoes the effects of recording of incompatible labels. */
		public void reset()
		{
			wasSplit = false;
			incompatibleLabelsForAbstractLabel = null;
		}
		
		/** Given a pair of labels that this abstract label abstracts from, records them as incompatible, eventually forcing a split of this label 
		 * (the split happens when all incompatible elements are found and we have clustered compatible ones). 
		 */
		public void recordLabelsAsIncompatible(Label label1, Label label2) 
		{
			if (!labelsThisoneabstracts.contains(label1))
				throw new IllegalArgumentException("Label "+label1+" does not belong to "+toString());
			if (!labelsThisoneabstracts.contains(label2))
				throw new IllegalArgumentException("Label "+label2+" does not belong to "+toString());
			if (incompatibleLabelsForAbstractLabel == null)
				incompatibleLabelsForAbstractLabel = new ArrayList<IncompatiblePTALabels>();
			incompatibleLabelsForAbstractLabel.add(new IncompatiblePTALabels(label1,label2));
		}
	}
	
	
	public LearnerWithLabelRefinementViaPta(LearnerEvaluationConfiguration evalCnf, LearnerGraph argInitialPTA, int threshold) 
	{
		super(evalCnf, argInitialPTA, threshold);
	}

	protected LearnerGraphND coreInverse = null;
	
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
		
		@Override
		public String toString()
		{
			return "Prev: "+prev+","+label;
		}
	}

	protected Map<VertID,PrevAndLabel> initialInverse;// for a PTA, an inverse is a deterministic graph.
	
	/** Creates a set of vertex identifiers, useful where I may choose to use a different underlying collection class such as to account for a large number of states. */ 
	protected static Set<VertID> createSetOfVertID(Collection<VertID> verticesToInclude)
	{
		if (verticesToInclude == null)
			return new TreeSet<VertID>();
		
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
		
		@Override
		public String toString()
		{
			return "! "+a+","+b+" !";
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
		
		@Override
		public String toString()
		{
			return "^ "+a+","+b+" ^";
		}
	}
	
	/** Since higher-level states partition PTA states, it is enough to simply have an incompatibility map between PTA vertices. 
	 * Initially, maps a positive to a negative, but subsequently additional pairs to split are added. 
	 * It is not reflexive because I need to iterate through it and I do not want to explore pairs that were visited earlier. 
	 */
	protected Collection<VertIDPair> incompatibleVertices = null;
	
	/** Associates an abstract state with every encountered incompatible PTA vertex. 
	 * Computed on the fly since we do not want to populate it with all the PTA vertices that are not participating in split states. 
	 */
	protected Map<VertID,CmpVertex> ptaStateToCoreState = null;
	
	/** Records where low-level states have been added to the map for this high-level state. */
	protected Set<CmpVertex> coreStateDetailsAddedToPtaStateToCoreState = null;
	
	/** Used to add all low-level states corresponding to the supplied vertex of the core graph, to the ptaStateToCoreState map. 
	 * We do not do it for absolutely all vertices of the core graph since this is a lot of work and only some of the lower-level states are going to be conflicting. 
	 */
	public void updatePtaStateToCoreState(CmpVertex vert)
	{
		if (!coreStateDetailsAddedToPtaStateToCoreState.contains(vert))
		{// for performance reasons, we cache vertices which details have been added to ptaStateToCoreState
			for(VertID v: coregraph.getCache().getMergedToHardFacts().get(vert))
				ptaStateToCoreState.put(v,vert);
			coreStateDetailsAddedToPtaStateToCoreState.add(vert);
		}
	}

	/** Used to add all low-level states corresponding to the supplied vertices of the core graph, to the {@link LearnerWithLabelRefinementViaPta#ptaStateToCoreState} map. 
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
			abstractLabel.recordLabelsAsIncompatible(lblToA.label,lblToB.label);
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
					if (!negatives.contains(pos)) // is true if pos is ID of a positive vertex 
						for(VertID v:negatives)
						{
							markPairAsIncompatible(pos,v);
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
			{
				if (outcome.get(transition.getValue()) != null)
					throw new IllegalArgumentException("graph "+graph.toString()+" is not a PTA: state "+transition.getValue()+" has more than one incoming transition, one with label "+transition.getKey()+", another with label "+outcome.get(transition.getValue()).label);
				outcome.put(transition.getValue(),new PrevAndLabel(entry.getKey(), transition.getKey()));
			}
		}
		return outcome;
	}

	/** Describes what should be merged where to resolve non-determinism associated with multiple pta labels mapped to the same high-level label. */
	public Map<VertID,List<VertID>> forcedMergers = null;

	public static class PartitioningOfvertices
	{
		/** Associates each vertex with a list of compatible vertices, in order to cluster them. */
		public Map<VertID,Set<VertID>> stateCompatibility = null;
		
		/** When an instance of {@link LearnerWithLabelRefinementViaPta.PartitioningOfvertices} is built, all states are marked as compatible and allCompatible is set to true, indicating that a state does not need to be split. If any vertices are not compatible, state needs splitting and this boolean is set to false. */
		//public boolean allCompatible = true;
		
		@Override
		public String toString()
		{
			return "PartitioningOfvertices["+stateCompatibility.toString()+"]";
		}
	}

	/** Builds a new entry for {@link LearnerWithLabelRefinementViaPta#ptaMergers}, using the supplied vertex as an argument.
	 * 
	 * @param a vertex to add an entry for
	 */
	public PartitioningOfvertices constructCompatibilityMappingForState(CmpVertex abstractVert)
	{
		PartitioningOfvertices p = ptaMergers.get(abstractVert);
		if (p == null)
		{
			p = new PartitioningOfvertices();ptaMergers.put(abstractVert,p);p.stateCompatibility = new TreeMap<VertID,Set<VertID>>();
			Collection<VertID> ptaStates = coregraph.getCache().getMergedToHardFacts().get(abstractVert);
			for(VertID v:ptaStates)
			{
				boolean accept = initialPTA.findVertex(v).isAccept();
				Set<VertID> possibleStates = createSetOfVertID(ptaStates);possibleStates.remove(v);
				for(VertID other:possibleStates)
					if (initialPTA.findVertex(other).isAccept() == accept)
						p.stateCompatibility.put(v,possibleStates);
			}
		}
		return p;
	}
	
	/** Constructs a compatibility map between states, for subsequent use of BronKerbosch. The two vertices supplied are marked as incompatible. */ 
	public void markPairAsIncompatible(VertID a, VertID b)
	{
		CmpVertex abstractVert = ptaStateToCoreState.get(a);assert abstractVert == ptaStateToCoreState.get(b);
		PartitioningOfvertices p = constructCompatibilityMappingForState(abstractVert);
		p.stateCompatibility.get(a).remove(b);p.stateCompatibility.get(b).remove(a);
		incompatibleVertices.add(new VertIDPair(a, b));
	}

	/** If a pair of transitions leaving a pta state has a non-deterministic choice, target states need to be merged (if they are inconsistent, this would have been taken into account at the label-splitting stage). 
	 * Maps existing high-level state to a map associating states to be forcefully merged with some other states. The expectation is that that other state will either be merged somewhere else 
	 * or be the representative of the state of interest in BronKerbosch across the states that comprise the considered high-level state.
	 */  
	protected Map<CmpVertex,PartitioningOfvertices> ptaMergers = null; 
	
	/** Vertices to evaluate in identification of forced mergers. */
	protected Set<VertID> ptaStatesForCheckOfNondeterminism = null;
	
	/** Starting with a global set of incompatible vertices, extends it recursively where the high-level graph has transitions to the conflicting states from the same state. */ 
	public void splitIncompatibleStates()
	{
		Collection<VertIDPair> newIncompatibleVertices = new ArrayList<VertIDPair>(incompatibleVertices),currentIncompatibleVertices = null;
		do
		{
			currentIncompatibleVertices = newIncompatibleVertices;newIncompatibleVertices = new ArrayList<VertIDPair>();
			for(VertIDPair ab:currentIncompatibleVertices)
			{
				PrevAndLabel lblToA = initialInverse.get(ab.a);
				PrevAndLabel lblToB = initialInverse.get(ab.b);
				
				assert !lblToA.equals(lblToB); // both previous states and labels match, cannot happen if the initial PTA is a determinstic tree.
				
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
					markPairAsIncompatible(lblToA.prev, lblToB.prev);// adds the pair to incompatibleVertices collection.

					ptaStatesForCheckOfNondeterminism.add(lblToA.prev);ptaStatesForCheckOfNondeterminism.add(lblToB.prev);
					
					PrevAndLabel Aprev = initialInverse.get(lblToA.prev), Bprev = initialInverse.get(lblToB.prev);
					Map<Label,List<CmpVertex>> incoming = coreInverse.transitionMatrix.get(corePreviousA);
					updatePtaStateToCoreState(incoming.get(initialLabelToAbstractLabel.get(Aprev.label)));
					updatePtaStateToCoreState(incoming.get(initialLabelToAbstractLabel.get(Bprev.label)));
					//constructCompatibilityMappingForState(ptaStateToCoreState.get(Aprev.prev));constructCompatibilityMappingForState(ptaStateToCoreState.get(Bprev.prev));
					ptaStatesForCheckOfNondeterminism.add(Aprev.prev);ptaStatesForCheckOfNondeterminism.add(Bprev.prev);
				}
				
				if (lblToA.prev == lblToB.prev && abstractLabelA == abstractLabelB)
					// the conflict can be resolved by marking the current pair of PTA labels inconsistent. In terms of determinism, we are looking at a pta state with two targets that are not compatible, hence labels absolutely have to be split. 
					updateInconsistentPTALabelMap(lblToA, lblToB);
			}			
		}
		while(!newIncompatibleVertices.isEmpty());
	}

	/** Given a pta vertex and a record describing what needs to be done for the abstract state currently associated with this vertex, 
	 * updates this information to record vertices that need to be merged to ensure that the outcome is deterministic.
	 * 
	 * @param vertex pta (low-level) vertex
	 * @param p record describing what needs to be done for the abstract state currently associated with vertex.
	 * @param forcedMergersForPTAstate map from abstract labels to the main state associated with them. This is a parameter to allow multiple calls to this routine to accumulate data.
	 * @param verticesToEliminate vertices mentioned in ptaStatesForCheckOfNondeterminism that have to be merged into some other vertices. These should not participate in BronKerbosch to avoid merging them with unrelated states. 
	 */
	protected void constructVerticesThatNeedMergingForAbstractState(VertID vertex,Map<AbstractLabel,VertID> forcedMergersForPTAstate, List<VertID> verticesToEliminate)
	{
		// First part: find a representative state for all the forced mergers, making sure it is one of ptaStatesWithForCheckOfNondeterminism
		for(Entry<Label,CmpVertex> transition:initialPTA.transitionMatrix.get(vertex).entrySet())
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
		for(Entry<Label,CmpVertex> transition:initialPTA.transitionMatrix.get(vertex).entrySet())
		{
			AbstractLabel lbl = initialLabelToAbstractLabel.get(transition.getKey());
			VertID target = forcedMergersForPTAstate.get(lbl);
			if (target != transition.getValue())
			{// targets and the current target are to be forcefully merged.
				List<VertID> vertsToMerge = forcedMergers.get(target);if (vertsToMerge == null) { vertsToMerge = new ArrayList<VertID>();forcedMergers.put(target,vertsToMerge); }
				vertsToMerge.add(transition.getValue());
				if (ptaStatesForCheckOfNondeterminism.contains(transition.getValue()) && verticesToEliminate != null)
					// this is where we are merging multiple compatible states, which is why they should not appear in the list of compatible ones - if they stay there, they may get clustered with different states.
					verticesToEliminate.add(transition.getValue());
			}
		}
	}
	
	/** Non-determinism is resolved by merging target states, identified here. */
	protected void identifyForcedMergers()
	{
		List<VertID> verticesToEliminate = new LinkedList<VertID>();
		
		Map<AbstractLabel,VertID> forcedMergersForPTAstate = new TreeMap<AbstractLabel,VertID>();
		for(VertID vert:ptaStatesForCheckOfNondeterminism)
		{// check for the outgoing transitions with the same label, record one of them as a force merge (if any is ptaStatesWithForCheckOfNondeterminism, this needs to be noted in order to avoid merging such a state into an incompatible one). 
			forcedMergersForPTAstate.clear();
			constructVerticesThatNeedMergingForAbstractState(vert,forcedMergersForPTAstate,verticesToEliminate);
		}
		
		// Now eliminate vertices that were marked for deletion in verticesToEliminate.
		for(PartitioningOfvertices p:ptaMergers.values())
			eliminateElementsFromCompatibilityMap(p.stateCompatibility,verticesToEliminate);
	}

	/** Adds outgoing transitions for a split state vert. 
	 * 
	 * @param outcome graph where to add the transitions 
	 * @param ptaToNewState map of pta vertices to new states (this routine is called for all of those new states).
	 * @param abstractVert vertex which transitions to add.
	 * @param ptaStatesForVert low-level transitions for this vertex (an inverse of ptaToNewState applied to this vertex).
	 */
	protected void constructTransitionsForState(LearnerGraph outcome, Map<VertID,CmpVertex> ptaToNewState, CmpVertex abstractVert, Collection<VertID> ptaStatesForVert)
	{
		Map<Label, CmpVertex> transitions = outcome.transitionMatrix.get(abstractVert);
		updatePtaStateToCoreState(transitions.values());transitions.clear();
		for(VertID v:ptaStatesForVert)
		{// add a transition for each low-level transition. 
			for(Entry<Label,CmpVertex> outgoing:initialPTA.transitionMatrix.get(v).entrySet())
			{
				AbstractLabel abstractLabel = initialLabelToAbstractLabel.get(outgoing.getKey());
				CmpVertex target = ptaToNewState.get(outgoing.getValue()); // if this is not null, the target is a new vertex ...
				if (target == null)
					target=ptaStateToCoreState.get(outgoing.getValue());// since there is nothing in ptaToNewState, the target is the original vertex.
				
				CmpVertex origTarget = transitions.put(abstractLabel,target);
				if (!(origTarget == null || origTarget == target))// otherwise a non-deterministic choice.
					assert false : "non-deterministic choice";
			}
		}
		
	}

	/** Constructs an updated transition diagram and returns it. */
	protected LearnerGraph partitionVerticesThatArePartOfAbstractStates()
	{
		final Configuration shallowCopy = getTentativeAutomaton().config.copy();shallowCopy.setLearnerCloneGraph(false);// since we are not going to change acceptance conditions of states.
		LearnerGraph outcome = new LearnerGraph(shallowCopy);AbstractLearnerGraph.copyGraphs(coregraph, outcome);
		Map<VertID,CmpVertex> ptaToNewState = new TreeMap<VertID,CmpVertex>();
		List<CmpVertex> newStates = new ArrayList<CmpVertex>();
		Map<VertID,Collection<VertID>> mergedToPta = new TreeMap<VertID,Collection<VertID>>();//outcome.getCache().getMergedToHardFacts();
		
		for(Entry<CmpVertex,PartitioningOfvertices> entry:ptaMergers.entrySet())
		{
			updatePtaStateToCoreState(coregraph.transitionMatrix.get(entry.getKey()).values());
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
					List<VertID> statesToMerge = forcedMergers.get(v);
					if (statesToMerge != null) verticesForNewVertex.addAll(statesToMerge);
				}
				newStates.add(newVertex);
				boolean isInitial = false;
				for(VertID v:verticesForNewVertex)
				{
					ptaToNewState.put(v, newVertex);
					if (v == initialPTA.getInit())
						isInitial = true;
				}
				if (isInitial)
					outcome.setInit(newVertex);
				mergedToPta.put(newVertex,verticesForNewVertex);
			} 
		}
		
		// All new states created, now construct transitions to/from those in keyset of ptaMergers. This is done by 
		for(CmpVertex vert:newStates)
			constructTransitionsForState(outcome,ptaToNewState,vert,mergedToPta.get(vert));
		
		// Copy the rest of the mergedToHardFacts
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
			if (!ptaMergers.containsKey(entry.getKey()))
				mergedToPta.put(entry.getKey(),createSetOfVertID(coregraph.getCache().getMergedToHardFacts().get(entry.getKey())));

		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
			if (!ptaMergers.containsKey(entry.getKey()))
			{
				boolean transitionsNeedRebuilding = false;
				for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
					if ( ((AbstractLabel)transition.getKey()).wasSplit() || ptaMergers.containsKey(transition.getValue()))
					{
						transitionsNeedRebuilding = true;break;// since one of the target states may have been split, it is possible that we need to redirect the current transition. A separate is is where this state has an outgoing transition with a label that was split,.
					}
				
				if (transitionsNeedRebuilding)
					constructTransitionsForState(outcome,ptaToNewState,entry.getKey(),mergedToPta.get(entry.getKey()));
			}
		
		// Now remove all the original states
		for(CmpVertex v:ptaMergers.keySet())
			outcome.transitionMatrix.remove(v);
		
		outcome.getCache().setMergedToHardFacts(mergedToPta);
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
			entry.getValue().removeAll(whatToEliminate);// restrict the neighbours to labels that are available.
		
	}
	
	/** Iterates through the lists of states to split, identifying components of labels that are pairwise compatible. 
	 * Those that are not touched can be clustered with any of those, probably based on how often they are used from 
	 * the same state as the split ones. Presently, those not touched are gathered together as a separate transition. 
	 */
	public void constructCompatiblePartsOfLabels()
	{
		for(Label lbl:coregraph.learnerCache.getAlphabet())
		{
			// convert the list of pairs that are not compatible into a mapping from a pair to those it is compatible with.
			final Map<Label,Set<Label>> labelToNeighbours = ((AbstractLabel)lbl).constructCompatibilityMap();
			if (labelToNeighbours != null)
			{
				// Iterate through the list of labels, with the intention of finding groups of compatible ones.
				while(!labelToNeighbours.isEmpty())
				{
					List<Label> maxPartition = BronKerbosch(labelToNeighbours, new LinkedList<Label>(), new LinkedList<Label>(labelToNeighbours.keySet()), new LinkedList<Label>());
					assert maxPartition != null;
					assert !maxPartition.isEmpty();
					((AbstractLabel)lbl).splitLabel(maxPartition); // populates initialLabelToAbstractLabel 
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
		incompatibleVertices = new LinkedHashSet<VertIDPair>();
		coreStateDetailsAddedToPtaStateToCoreState = new TreeSet<CmpVertex>();
		ptaStateToCoreState = new TreeMap<VertID,CmpVertex>();
		forcedMergers = new TreeMap<VertID,List<VertID>>();
		coreInverse = null;
		
		if (coregraph  != null)
			for(Label lbl:coregraph.learnerCache.getAlphabet()) ((AbstractLabel)lbl).reset();
		ptaMergers = new TreeMap<CmpVertex,PartitioningOfvertices>();
		
		initialInverse = constructInitialInverse(initialPTA);	
	}
	
	
	/** Uses a simple heuristic to construct abstract labels for the provided PTA. In effect, anything that precedes a supplied character becomes split out. 
	 * This is an effective way to cluster all that precedes an opening bracket. 
	 */
	@SuppressWarnings("unused")
	public void constructAbstractLabelsForInitialPta(char whereToSplit)
	{
		Map<String,List<Label>> abstractToConcrete = new TreeMap<String,List<Label>>();
		for(Label l:initialPTA.learnerCache.getAlphabet())
		{
			String origLabel = l.toErlangTerm();int splitPosition = origLabel.indexOf(whereToSplit);if (splitPosition <= 0) throw new IllegalArgumentException("label "+origLabel+" cannot be abstracted using '"+whereToSplit+"'");
			String abstractText = origLabel.substring(0, splitPosition);
			List<Label> concrete = abstractToConcrete.get(abstractText);if (concrete == null) { concrete = new ArrayList<Label>();abstractToConcrete.put(abstractText, concrete); }
			concrete.add(l); 
		}
		for(Entry<String,List<Label>> entry:abstractToConcrete.entrySet())
			new AbstractLabel(initialLabelToAbstractLabel, entry.getKey(), entry.getValue());// I do not need to store it anywhere because construction of such a label adds appropriate entries to the initialLabelToAbstractLabel map.
	}
	
	/** Assuming that the initial graph contains the low-level PTA, builds an abstract core graph using the current set of abstract labels. */ 
	public LearnerGraph abstractInitialGraph(char whereToSplit)
	{
		init();
		constructAbstractLabelsForInitialPta(whereToSplit);
		LearnerGraph outcome = new LearnerGraph(initialPTA.config);AbstractLearnerGraph.copyGraphs(initialPTA, outcome);outcome.getCache().setMergedToHardFacts(new TreeMap<VertID,Collection<VertID>>());
		Queue<CmpVertex> statesWhereTransitionsNeedAbstracting = new LinkedList<CmpVertex>();
		statesWhereTransitionsNeedAbstracting.add(outcome.getInit());
		Collection<VertID> initToHardFact = new ArrayList<VertID>();initToHardFact.add(initialPTA.getInit());
		outcome.getCache().getMergedToHardFacts().put(outcome.getInit(),initToHardFact);
		
		while(!statesWhereTransitionsNeedAbstracting.isEmpty())
		{
			CmpVertex currentState = statesWhereTransitionsNeedAbstracting.remove();
			Map<Label,CmpVertex> transitions = outcome.transitionMatrix.get(currentState);transitions.clear();
			Map<AbstractLabel,VertID> forcedMergersForPTAstate = new TreeMap<AbstractLabel,VertID>();
			for(VertID v: outcome.getCache().getMergedToHardFacts().get(currentState))
				constructVerticesThatNeedMergingForAbstractState(v,forcedMergersForPTAstate,null);
			for(Entry<AbstractLabel,VertID> merger:forcedMergersForPTAstate.entrySet())
			{
				CmpVertex target = outcome.findVertex(merger.getValue());
				Collection<VertID> mergedToHardFacts = new ArrayList<VertID>();outcome.getCache().getMergedToHardFacts().put(target,mergedToHardFacts);
				mergedToHardFacts.add(merger.getValue());
				List<VertID> otherVerts = forcedMergers.get(merger.getValue());
				if (otherVerts != null)
				{
					mergedToHardFacts.addAll(otherVerts);
					for(VertID v:otherVerts)
						outcome.transitionMatrix.remove(v);// remove all merged vertices, should have probably done outcome.findVertex(v) instead but v should be find since it is looking for the same object in both cases.
				}
				transitions.put(merger.getKey(),target);
				statesWhereTransitionsNeedAbstracting.add(target);
			}
		}
	
		return outcome;
	}
	

	/** Identifies where states and/or labels have to be split if the supplied positive and negative traces are added. Returns a new transition diagram. */
	public LearnerGraph refineGraph()
	{
		coreInverse = MarkovClassifier.computeInverseGraph(coregraph);
		splitIncompatibleStates();
		constructCompatiblePartsOfLabels();
		identifyForcedMergers();
		return partitionVerticesThatArePartOfAbstractStates();
	}
}
