/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of Statechum.
 * 
 * Statechum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Statechum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Statechum.  If not, see <http://www.gnu.org/licenses/>.
 */
package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.PairScoreComputation.StringVertexPair;

public class AMEquivalenceClass<TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
	implements Comparable<AMEquivalenceClass<TARGET_TYPE,CACHE_TYPE>>
{
	/** The list of outgoing transitions from this equivalence class. */ 
	private TreeSet<StringVertexPair> outgoingTransitions = new TreeSet<StringVertexPair>(),
		newOutgoingTransitions = null;
	
	/**	Vertices in the original graph corresponding to the merged vertex. 
	 * A tree is used to permit easy comparison between equivalence classes. 
	 * <p> 
	 * When merging multiple equivalence classes, I need to change entries 
	 * for all states belonging to those classes.
	 * This set makes it possible to do this.
	 */
	private Set<CmpVertex> states = new TreeSet<CmpVertex>();

	/** Accumulates the states which are not compatible to states in this equivalence class. */
	final Set<CmpVertex> incompatibleStates = new TreeSet<CmpVertex>();
	
	/** Whether the collection of states in this equivalence class contains all-accept or all-reject states. */
	private boolean accept;
	
	/** The ID of this equivalence class - not really necessary but handy. */
	private final int ClassNumber;
	
	/** The graph which states we shall store here (except for <em>mergedVertex</em> which will belong to another graph. */
	private final AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> coregraph;
	
	public AMEquivalenceClass(int number, AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph)
	{
		ClassNumber=number;coregraph=graph;
	}
	
	public int getNumber()
	{
		return ClassNumber;
	}
	
	/** Returns transitions leaving states contained in this equivalence class. */ 
	public TreeSet<StringVertexPair> getOutgoing()
	{
		return outgoingTransitions;
	}
	
	/** Returns transitions leaving states contained in this equivalence class. */ 
	public TreeSet<StringVertexPair> getNewOutgoing()
	{
		return newOutgoingTransitions;
	}
	
	public Set<CmpVertex> getStates()
	{
		return states;
	}

	/** A representative of this equivalence class. */
	private CmpVertex representative = null;
	
	/** Returns the current representative. */
	public CmpVertex getRepresentative()
	{
		return representative;
	}
	
	/** From the current representative and the supplied vertex, this method chooses 
	 * the one which is better as a representative. 
	 */
	private void updateRep(CmpVertex vert)
	{
		assert vert != null;
		if (representative == null) representative = vert;
		else
		{
			int vertDepth = vert.getDepth();
			int currentDepth = representative.getDepth();
			if (vert == coregraph.getInit())
				representative = vert;// the special case for graphs not built from PTAs where depth labelling would ensure that the init state is chosen as representative. 
			else
			if (coregraph.config.isIgnoreDepthInTheChoiceOfRepresentatives())
			{ 
				if (representative.compareTo(vert) > 0)
					representative = vert;// emulate lexicographical comparison if told to do so
			}
			else
			{// not emulating lexicographical order
				if (currentDepth == JUConstants.intUNKNOWN)
				{
					if (vertDepth != JUConstants.intUNKNOWN || representative.compareTo(vert) > 0)
						representative = vert;
				}
				else
					if (vertDepth != JUConstants.intUNKNOWN)
					{
						if (currentDepth > vertDepth || 
								(currentDepth == vertDepth && representative.compareTo(vert) > 0))// emulate lexicographical comparison when depth is the same
							representative = vert;
					}
			}	
		}
	}
	
	/** Adds a state to the collection of states in this equivalence class.
	 * 
	 * @param from transitions to add from.
	 * @return false if the new state is not compatible with any state in this equivalence class
	 * @throws IncompatibleStatesException if the state to be added is incompatible with any state in the equivalence class.
	 */
	private boolean addState(CmpVertex vert) throws IncompatibleStatesException
	{
		if (!states.isEmpty() &&
				(accept != vert.isAccept() || incompatibleStates.contains(vert)))
			 throw new IncompatibleStatesException("cannot add state "+vert+" to "+states);
		accept = vert.isAccept();
		Map<CmpVertex,JUConstants.PAIRCOMPATIBILITY> compatibility = coregraph.pairCompatibility.compatibility.get(vert);
		if (compatibility != null) 
			for(Entry<CmpVertex,JUConstants.PAIRCOMPATIBILITY> entry:compatibility.entrySet())
				if (entry.getValue() == JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE) incompatibleStates.add(entry.getKey());
		updateColour(vert.getColour());

		states.add(vert);updateRep(vert);return true;		
	}
	
	public static class IncompatibleStatesException extends Exception
	{

		public IncompatibleStatesException(String string) {
			super(string);
		}

		/**
		 * ID for serialisation.
		 */
		private static final long serialVersionUID = 1792082290362715752L;
	}
	
	/** Adds transitions from the supplied collection.
	 * 
	 * @param from transitions to add from.
	 * @throws IncompatibleStatesException if vertex is not compatible with any vertices in the collection.
	 */
	public void addFrom(CmpVertex vert,Collection<Entry<String,CmpVertex>> from) throws IncompatibleStatesException
	{
		addState(vert);
		if (from != null)
			for(Entry<String,CmpVertex> entry:from)
				outgoingTransitions.add(new StringVertexPair(entry.getKey(),entry.getValue()));
	}
	
	/** Adds transitions from the supplied collection.
	 * 
	 * @param from transitions to add from.
	 * @throws IncompatibleStatesException if vertex is not compatible with any vertices in the collection.
	 */
	public void mergeWith(CmpVertex vert,Collection<Entry<String,CmpVertex>> from) throws IncompatibleStatesException
	{
		addState(vert);

		newOutgoingTransitions = new TreeSet<StringVertexPair>();
		for(Entry<String,CmpVertex> entry:from)
			newOutgoingTransitions.add(new StringVertexPair(entry.getKey(),entry.getValue()));
	}
	
	/** Adds the contents of the supplied argument to outgoing 
	 * transitions of this class.
	 * 
	 * @param to the equivalence class to merge with
	 * @throws IncompatibleStatesException if vertex is not compatible with any vertices in the collection.
	 */
	public void mergeWith(AMEquivalenceClass<TARGET_TYPE,CACHE_TYPE> to) throws IncompatibleStatesException
	{
		if (!states.isEmpty())
		{ 
			if (accept != to.accept)
				throw new IncompatibleStatesException("incompatible equivalence classes");
			{// TODO: to test this case
				Set<CmpVertex> incomps = new TreeSet<CmpVertex>();incomps.addAll(incompatibleStates);incomps.retainAll(to.states);
				if (!incomps.isEmpty()) // we check that none of the states we add are incompatible with this state
					throw new IncompatibleStatesException("incompatible equivalence classes");
			}
			
			{// TODO: to test this case
				Set<CmpVertex> incomps = new TreeSet<CmpVertex>();incomps.addAll(to.incompatibleStates);incomps.retainAll(states);
				if (!incomps.isEmpty()) // we check that none of the states we add are incompatible with this state
					throw new IncompatibleStatesException("incompatible equivalence classes");
			}
		}
		accept = to.accept;incompatibleStates.addAll(to.incompatibleStates);

		newOutgoingTransitions = to.outgoingTransitions;updateRep(to.getRepresentative());
		if (!to.getStates().isEmpty()) updateColour(to.currentColour);
		states.addAll(to.states);
	}

	/** When equivalence classes are updated, I do not update "real" data, just store the changes.
	 * This is necessary to avoid scanning all pairs of old outgoing transitions, if there is 1000
	 * of them it takes a while. This method is invoked when such a scanning is finished to update the data.
	 */
	public void populate()
	{
		outgoingTransitions.addAll(newOutgoingTransitions);newOutgoingTransitions = null;
	}
	
	public int compareTo(AMEquivalenceClass<TARGET_TYPE,CACHE_TYPE> o) {
		return ClassNumber - o.ClassNumber;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		// representative is not used here because it can be derived from the states in this collection.
		result = prime * result + (mergedVertex == null? 0:mergedVertex.hashCode());
		result = prime * result + states.hashCode();
		result = prime * result + ClassNumber;
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof AMEquivalenceClass))
			return false;
		final AMEquivalenceClass other = (AMEquivalenceClass) obj;
		if (mergedVertex == null)
		{
			if (other.mergedVertex != null)
				return false;
		}
		else
			if (!mergedVertex.equals(other.mergedVertex))
				return false;
		
		// representative is not used here because it can be derived from the states in this collection.
		
		if (ClassNumber != other.ClassNumber)
			return false;
		return states.equals(other.states);
	}
	
	/** The merged vertex. */
	private CmpVertex mergedVertex;
	
	public CmpVertex getMergedVertex()
	{
		return mergedVertex;
	}
	
	private static final Map<JUConstants,Map<JUConstants,JUConstants>> ColourPriorities = new TreeMap<JUConstants,Map<JUConstants,JUConstants>>();
	
	static
	{
		JUConstants [][]priorities = new JUConstants[][]{
				new JUConstants[]{JUConstants.AMBER,JUConstants.GRAY,JUConstants.INF_AMBER},
				new JUConstants[]{JUConstants.NONE},
				new JUConstants[]{JUConstants.BLUE,JUConstants.RED }
				};
	
		for(int priority=0;priority<priorities.length;++priority)
		{
			for(JUConstants col:priorities[priority])
			{
				final Map<JUConstants,JUConstants> transformation = new TreeMap<JUConstants,JUConstants>();
				ColourPriorities.put(col,transformation);
				
				// lower priority and the current priority
				for(int other=0;other<=priority;++other)
					for(JUConstants lowerP:priorities[other])
						transformation.put(lowerP,col);
				
				// higher priority
				for(int other=priority+1;other<priorities.length;++other)
					for(JUConstants higherP:priorities[other])
						transformation.put(higherP,higherP);
			}
		}
		

	}
	
	/** When merging states, it is often necessary to preserve the colour of vertices.
	 * For instance, PTA states do not have colour assigned while the rest of a graph
	 * does. When performing QSM merging, we'd like to give a merged vertex 
	 * the colour of the one from the main graph all the other vertices have been
	 * merged into. 
	 */
	private void updateColour(JUConstants vertColour)
	{
		JUConstants colour = vertColour == null?JUConstants.NONE:vertColour;
		if (states.isEmpty())
			currentColour = colour;// the first state in this equivalence class
		else
		{
			assert ColourPriorities.containsKey(currentColour): "unknown colour "+colour+", current colour is "+currentColour;
			Map<JUConstants,JUConstants> otherToAnswer = ColourPriorities.get(currentColour);
			assert otherToAnswer.containsKey(colour): "unknown colour "+colour+", current colour is "+currentColour;
			currentColour = otherToAnswer.get(colour);
		}
	}
	
	private JUConstants currentColour = null;
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		String mergedDescr = mergedVertex == null?"":mergedVertex.getID().toString()+"->";
		StringBuffer result = new StringBuffer("["+mergedDescr+"{");
		Iterator<CmpVertex> vertIter = states.iterator();
		result.append(vertIter.next().getID().toString());
		while(vertIter.hasNext()) result.append(',').append(vertIter.next().getID().toString());
		result.append("}]");
		return result.toString();
	}

	/** Generates a vertex representing the representative vertex. If <em>useDifferentName</em> is false, 
	 * a true clone of a representative vertex is made, with the same ID; otherwise  
	 * a new name is chosen.
	 * 
	 * @param graph the graph which will be used to store the generated vertex
	 * @param useDifferentNameIfAlreadyExist whether to retain the ID of a representative vertex in the merged one or 
	 * check if the representative vertex already exists in the graph and if so create a new one.
	 * @param setOrigState whether to set the original state of a merged vertex to the ID of the representative state.
	 */
	public <TARGET_C_TYPE,CACHE_C_TYPE extends CachedData<TARGET_C_TYPE,CACHE_C_TYPE>>
		void constructMergedVertex(AbstractLearnerGraph<TARGET_C_TYPE,CACHE_C_TYPE> graph,
				boolean useDifferentNameIfAlreadyExist, boolean setOrigState) 
	{
		if (useDifferentNameIfAlreadyExist && graph.transitionMatrix.containsKey(representative))
			mergedVertex=graph.copyVertexUnderDifferentName(representative);
		else
		{// we have to clone a vertex regardless whether configuration is for it or not - since we'll later change colour,
		 // and perhaps even origState, we do have to make a copy here.
			mergedVertex=AbstractLearnerGraph.generateNewCmpVertex(representative.getID(), graph.config);graph.updateIDWith(mergedVertex);
			DeterministicDirectedSparseGraph.copyVertexData(representative, mergedVertex);
			graph.transitionMatrix.put(mergedVertex, graph.createNewRow());
		}
		mergedVertex.setColour(currentColour == JUConstants.NONE?null:currentColour);
		if (setOrigState)
			mergedVertex.setOrigState(representative.getID());
		
	}
	
	/**	When a merged vertex is built, we need to record states it is not compatible with, 
	 * based on the states in an equivalence class. This routine performs pairwise
	 * compatibility check between states of a graph based on equivalence classes and populates
	 * the incompatibles array.
	 * 
	 * @param graph the graph to update
	 * @param eqClasses the collection of equivalence classes out of which this graph has been built.
	 * @param ThreadNumber the number of threads to use. 
	 */
	public static <TARGET_IN_TYPE,TARGET_OUT_TYPE,
		CACHE_IN_TYPE extends CachedData<TARGET_IN_TYPE,CACHE_IN_TYPE>,
		CACHE_OUT_TYPE extends CachedData<TARGET_OUT_TYPE,CACHE_OUT_TYPE>> 
		void populateCompatible(final AbstractLearnerGraph<TARGET_OUT_TYPE,CACHE_OUT_TYPE> graph, 
				Collection<AMEquivalenceClass<TARGET_IN_TYPE,CACHE_IN_TYPE>> eqClasses)
	{
		final Map<CmpVertex, List<AMEquivalenceClass<TARGET_IN_TYPE,CACHE_IN_TYPE>>> vertexToEqClassesContainingIt = 
			new HashMap<CmpVertex, List<AMEquivalenceClass<TARGET_IN_TYPE,CACHE_IN_TYPE>>>();
		for(AMEquivalenceClass<TARGET_IN_TYPE,CACHE_IN_TYPE> eqClass:eqClasses)
			for(CmpVertex vertex:eqClass.getStates())
			{
				List<AMEquivalenceClass<TARGET_IN_TYPE,CACHE_IN_TYPE>> classes = vertexToEqClassesContainingIt.get(vertex);
				if (classes == null)
				{
					classes = new LinkedList<AMEquivalenceClass<TARGET_IN_TYPE,CACHE_IN_TYPE>>();vertexToEqClassesContainingIt.put(vertex,classes);
				}
				classes.add(eqClass);
			}
		
		for(AMEquivalenceClass<TARGET_IN_TYPE,CACHE_IN_TYPE> eqClass:eqClasses)
			for(CmpVertex vertex:eqClass.incompatibleStates)
			{
				List<AMEquivalenceClass<TARGET_IN_TYPE,CACHE_IN_TYPE>> classes = vertexToEqClassesContainingIt.get(vertex);
				if (classes != null)
					for(AMEquivalenceClass<TARGET_IN_TYPE,CACHE_IN_TYPE> incompClass:classes)
						graph.addToCompatibility(eqClass.getMergedVertex(), incompClass.getMergedVertex(),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
			}
		
	}
	
}
