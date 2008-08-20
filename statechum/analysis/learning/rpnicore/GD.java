/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/
package statechum.analysis.learning.rpnicore;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.Visualiser.VIZ_PROPERTIES;
import statechum.analysis.learning.rpnicore.LearnerGraph.StatesToConsider;
import statechum.analysis.learning.rpnicore.LearnerGraphND.DDRH_default;
import statechum.analysis.learning.rpnicore.LearnerGraphND.HandleRow;

/**
 * @author kirill
 *
 */
public class GD {
	/** Gives numbering to pairs to be considered in Linear and marks 
	 * pairs to be ignored (those part of the state space of the combined
	 * machine which are not member of the cross-product of states of 
	 * machines A and B.
	 */ 
	int [] pairScores;
	
	/** Compatibility scores for states obtained by scanning forwards and backwards. */
	double []scoresForward=null, scoresInverse=null;
	
	/** The coefficient to multiply scores by before converting to int in order to
	 * fit them into <em>PairScore</em>.
	 */
	final double multiplier = 10;

	/** The front wave. */
	List<PairScore> frontWave = null;
	
	/** The front wave. */
	List<PairScore> currentWave = null;
	
	/** A collection of states which have already been used in key pairs and hence cannot 
	 * be used in any other key pairs.
	 */
	Set<CmpVertex> statesInKeyPairs = null;
	
	/** In order to run linear and others, I need to combine the two graphs into a single one,
	 * this is the combined graph. It is easy to find out which states of this graph belong to 
	 * the first or the second graph, see <em>stateToNumber</em> and <em>statesOfB</em>.
	 */
	LearnerGraph grCombined = null;
	
	/** Collection of states in the combined graph which belong to the first graph. */
	Set<CmpVertex> statesOfA = null;

	/** Collection of states in the combined graph which belong to the second graph. */
	Set<CmpVertex> statesOfB = null;
	
	/** The initial state of A in the combined graph. */
	CmpVertex combined_initA = null;
	
	/** The initial state of B in the combined graph. */
	CmpVertex combined_initB = null;

	/** Used to compute the part of A which would remain after all transitions-to-remove have been removed. */
	LearnerGraph copyOfA = null;
	
	/** Maps vertices of the right-hand side of key pairs in the combined graph to vertices of the B graph. */
	Map<CmpVertex,CmpVertex> newBToOrig = null;
	
	/** Forward matrix for exploration of <em>grCombined</em>. */
	LearnerGraphND forward = null;
	
	/** Inverse matrix for exploration of <em>grCombined</em>. */
	LearnerGraphND inverse = null;

	/** Number of threads to use in a computation. */
	int ThreadNumber = 0;
	
	/** Compares the supplied two graphs.
	 * 
	 * @param a first graph
	 * @param b second graph
	 * @param threads the number of threads to use
	 * @param doc XML document used to create nodes
	 * @param observer this one receives the difference.
	 * @return XML node with GD.
	 */
	public Element computeGDToXML(LearnerGraph a,LearnerGraph b, int threads, Document doc, PatchGraph observer)
	{
		ChangesRecorder patcher = new ChangesRecorder(observer);
		computeGD(a, b, threads, patcher);
		return patcher.writeGD(doc);
	}
	
	/** Compares the supplied two graphs.
	 * 
	 * @param a first graph
	 * @param b second graph
	 * @param threads the number of threads to use
	 * @param patcher where to store changes.
	 */
	public void computeGD(LearnerGraph a,LearnerGraph b, int threads, PatchGraph patcher)
	{
		init(a, b, threads);
		identifyKeyPairs();
		makeSteps(patcher,null);
	}
	
	/** Describes primitive mutations which can be carried out on a graph. */
	public interface PatchGraph
	{
		/** Adds a transition between the specified states.
		 * Throws if transition already exists.
		 * 
		 * @param from source state
		 * @param label label of a transition
		 * @param to target state
		 */
		public void addTransition(CmpVertex from,String label, CmpVertex to);

		/** Removes a transition between the specified states. Throws
		 * if a transition does not exist.
		 * 
		 * @param from source state
		 * @param label label of a transition
		 * @param to target state
		 */
		public void removeTransition(CmpVertex from,String label, CmpVertex to);
		
		/** Sets the initial state to the requested state.
		 * @param vertex the state to become the initial state. 
		 */
		public void setInitial(CmpVertex vertex);
	}
	
	protected void printIncoming(String name)
	{
		CmpVertex vert = grCombined.findVertex(name);
		System.out.println("incoming to "+vert+" are "+inverse.matrixForward.get(vert));
	}
	
	/** Expands the set of key pairs and computes the outcome. 
	 * 
	 * @param graphToPatch this will be provided with changes necessary to transform the first graph
	 * to the second one. It can then be stored in XML if necessary.
	 * @param allKeyPairs if not null, this collection will be filled 
	 * with all of the key pairs for the graphs. Used for testing.
	 */
	protected void makeSteps(final PatchGraph graphToPatch, List<PairScore> allKeyPairs)
	{
		// Now we make steps. Data used:
		//
		// currentWave is what we'll populate with candidates for key pairs 
		// (the main criterion is that these states are not part of existing
		// key pairs, i.e. not in statesInKeyPairs collection.
		//
		// frontWave is the wavefront from which we are exploring grCombined in
		// search of these candidates for key pairs.
		final Map<CmpVertex,CmpVertex> aTOb = new TreeMap<CmpVertex,CmpVertex>();
		final Map<CmpVertex,CmpVertex> newToOrig = new TreeMap<CmpVertex,CmpVertex>();
		do
		{
			currentWave.clear();
			populateCurrentWave(forward.matrixForward);
			populateCurrentWave(inverse.matrixForward);
			sortWave(currentWave);
			if (allKeyPairs != null) allKeyPairs.addAll(frontWave);
			for(PairScore pair:frontWave) 
			{
				newToOrig.put(pair.getR(),pair.getQ());// since we now know 
				// which state of A pair.getQ() of combined corresponds to, change the mapping.
				// addTransitions(grCombined,statesOfB,added,cloneConfig) relies on this change.
				assert pair.getQ().isAccept() == pair.getR().isAccept();
				aTOb.put(pair.getQ(),pair.getR());
			}
			assert aTOb.size() == aTOb.values().size() : " duplicate right-hand side in key pairs";
			frontWave.clear();
			for(PairScore pair:currentWave)
				if (!statesInKeyPairs.contains(pair.getQ()) && !statesInKeyPairs.contains(pair.getR()) &&  // we can only consider a new pair if it does not share any states with existing key pairs
						pair.getQ().isAccept() == pair.getR().isAccept()) // we should not merge incompatible pairs
				{// this is the one for the front line
					frontWave.add(pair);statesInKeyPairs.add(pair.getQ());statesInKeyPairs.add(pair.getR());
				}
		}
		while(!frontWave.isEmpty());

		// Now find out which states need adding to A and check that we can actually do this
		// without name clashes.
		// The problem is that newBtoOrig maps states of the B part of grCombined
		// to the whole set of original states in A and B. The old version of GD was simply
		// using all new states if there was any intersection between states of A and B.
		// It is not hard to find out which states of the B part of grCombined clash with 
		// states of A which remain after removing transitions. We could then use grCombine's 
		// vertices. The next problem is that these vertices may clash with vertices of B,
		// so that when mapping to the original vertices via newBtoA, we have to check 
		// for clashes both with remaining states of A and states of B portion of grCombined.
		// This is resolved below by ensuring that grCombined's B-vertex space does not intersect
		// with that of the B graph by construction of grCombined.
		final LearnerGraphMutator remover = new LearnerGraphMutator(copyOfA,grCombined.config,null);
		final Set<VertexID> newToA = new TreeSet<VertexID>();// the original states of B which have to be added to A
		final Set<CmpVertex> retainedInA = new TreeSet<CmpVertex>();// vertices participating in transitions which start from A's vertices and lead to vertices of B.
		new DCollector()
		{
			@Override
			CmpVertex getOrig(CmpVertex vertex) {
				CmpVertex keyVertex = newToOrig.get(vertex);
				if (keyVertex != null) return keyVertex;
				assert newBToOrig.get(vertex) != null;
				newToA.add(newBToOrig.get(vertex).getID());
				return vertex;
			}

			public void addTransition(CmpVertex from, @SuppressWarnings("unused")	String label, 
					CmpVertex to) {
				if (statesOfA.contains(from))
					retainedInA.add(from);
				if (statesOfA.contains(to))
					retainedInA.add(to);
			}

			public void removeTransition(CmpVertex from, String label, CmpVertex to) {
				remover.removeTransition(from, label, to);
			}

			public void setInitial(CmpVertex vertex) 
			{
				if (statesOfA.contains(vertex))
					retainedInA.add(vertex);
			}
			
		}.computeGD(aTOb);
		
		remover.removeDanglingStates();
		final Set<VertexID> duplicates = new TreeSet<VertexID>();
		for(CmpVertex vertex:copyOfA.transitionMatrix.keySet()) // iterate through states remaining in A
			if (newToA.contains(vertex.getID()))
			{
				duplicates.add(vertex.getID());// we should not use this vertex
			}
		// now iterate through states are source and target states 
		for(CmpVertex vertex:retainedInA) 
			if (newToA.contains(vertex.getID()))
			{
				duplicates.add(vertex.getID());// we should not use this vertex
			}
		for(CmpVertex vertex:statesOfB) assert !newToA.contains(vertex.getID());// verify that ids of states of B grCombined do not clash with IDs of states of B. 
		
		if (!duplicates.isEmpty())
		{// duplicates hence use the unique names they were given in grCombined
			if (grCombined.config.getGdFailOnDuplicateNames()) throw new IllegalArgumentException("names of states "+duplicates+" are shared between A and B");
		}
		
		// Now actually record the changes.
		new DCollector()
		{

			@Override
			CmpVertex getOrig(CmpVertex vertex) {
				CmpVertex keyVertex = newToOrig.get(vertex);
				if (keyVertex != null) return keyVertex;
				
				CmpVertex result = newBToOrig.get(vertex);
				if (duplicates.contains(result.getID()))
					result = vertex;// duplicate vertices retain their new identifiers

				return result;
			}

			public void addTransition(CmpVertex from, String label, CmpVertex to) {
				graphToPatch.addTransition(from, label, to);// propagate changes
			}

			public void removeTransition(CmpVertex from, String label, CmpVertex to) {
				graphToPatch.removeTransition(from, label, to);
			}

			public void setInitial(CmpVertex vertex) {
				graphToPatch.setInitial(vertex);
			}
			
		}.computeGD(aTOb);
	}

	/** We need to do two things,
	 * <ul>
	 * <li>Iterate through the transitions of the combined graph in order to find out which 
	 * of them need removing and which new transitions to add.
	 * </li>
	 * <li>Collect the states of B to be added to A, these correspond to those which 
	 * were not matched to any states of A. This has to be done in advance because
	 * we'd like to ensure the set of these states does not intersect with states already
	 * in A, but without the above traversal it cannot be done since we need to know which
	 * states have been removed from A. The only problem is that without updating an existing
	 * transition matrix we cannot find out which states will remain after removing 
	 * and elimination of dangling states.
	 * </li></ul>
	 * For this reason, we start by doing a dummy run aimed at identification of states 
	 * which are to be removed and those to be added. The former is done by manipulating 
	 * a clone of grCombined and the latter
	 */
	protected abstract class DCollector implements PatchGraph
	{
		/** Called when we need to map a vertex in the B-part of grCombined to 
		 * an original vertex of B.
		 * 
		 * @param vertex vertex to map
		 */
		abstract CmpVertex getOrig(CmpVertex vertex);
		
		public void computeGD(Map<CmpVertex,CmpVertex> aTOb)
		{
			// Pick all transitions which have been added/removed.
			for(Entry<CmpVertex,CmpVertex> entry:aTOb.entrySet())
			{
				for(Entry<String,CmpVertex> transitionA:grCombined.transitionMatrix.get(entry.getKey()).entrySet())
				{
					CmpVertex targetA = transitionA.getValue();
					CmpVertex targetB = grCombined.transitionMatrix.get(entry.getValue()).get(transitionA.getKey());

					if (targetB == null) // this transition does not exist in B
						removeTransition(entry.getKey(), transitionA.getKey(),transitionA.getValue());
					else
						if (aTOb.get(targetA) != targetB) // it is not enough to check if both targetA and targetB are 
							// key states, but the two have to be part of the same key state. 
							// Otherwise, we risk making mistakes (see <em>testComputeGD6()</em> for an illustration).
						{// In other words, transition leads to a state which is not key in either of the two machines or both are parts of different key states.
							removeTransition(entry.getKey(), transitionA.getKey(),transitionA.getValue());
							addTransition(entry.getKey(), transitionA.getKey(),getOrig(targetB));
						}
				}
				
				for(Entry<String,CmpVertex> transitionB:grCombined.transitionMatrix.get(entry.getValue()).entrySet())
				{
					CmpVertex targetA = grCombined.transitionMatrix.get(entry.getKey()).get(transitionB.getKey());
					if (targetA == null) // a transition unique to B
						addTransition(getOrig(entry.getValue()), transitionB.getKey(),getOrig(transitionB.getValue()));
				}			
			}

			// now we just need to go through states which are not key states
			for(CmpVertex vertex:statesOfA)
				if (!statesInKeyPairs.contains(vertex))
				{
					for(Entry<String,CmpVertex> target:grCombined.transitionMatrix.get(vertex).entrySet())
						// transition not matched because some states are not known hence remove it.
							removeTransition(vertex, target.getKey(),target.getValue());
				}
		
			for(CmpVertex vertex:statesOfB)
				if (!statesInKeyPairs.contains(vertex))
				{
					CmpVertex origSource = getOrig(vertex);
					for(Entry<String,CmpVertex> target:grCombined.transitionMatrix.get(vertex).entrySet())
						// transition not matched because some states are not known hence append it.
							addTransition(origSource, target.getKey(),getOrig(target.getValue()));
				}
		
			// The initial state should be either combined_initB (which is the initial state of graph B)
			// or a key state of graph A which corresponds to this state.
			CmpVertex initialState = getOrig(combined_initB);
			Iterator<Entry<CmpVertex,CmpVertex>> entryIter = aTOb.entrySet().iterator();
			while(entryIter.hasNext())
			{// a linear search through not very many states
				Entry<CmpVertex,CmpVertex> pair = entryIter.next();
				if (pair.getValue() == initialState)
				{
					initialState = getOrig(pair.getKey());break;// found a match
				}
			}

			// if a match is found, it is appropriate to set the initial state to the corresponding state of A,
			// if not, this means that initial state of B will be the new state, we hence add it and set as 
			// initial. Many tests among TestGD_Multithreaded explore both possibilities.
			//if (initialState != combined_initA) setInitial(initialState);// only set the initial state if it has changed.
			setInitial(initialState);
		}
	}
	
	
	/** Sorts waves in place, in the order of descending scores.
	 * 
	 * @param wave wave to sort
	 */
	public static void sortWave(List<PairScore> wave)
	{
		Collections.sort(wave, new Comparator<PairScore>() {

			public int compare(PairScore o1, PairScore o2) {
				return o2.compareTo(o1);// in reverse order
			}
			
		});
	}
	
	/** Makes it possible to modify graphs by adding/removing transitions. */
	public static class LearnerGraphMutator implements PatchGraph
	{
		/** Graph to manipulate. */
		private final LearnerGraph graph;
		/** Configuration to use for cloning if necessary. */
		private Configuration cloneConfig;
		
		/** Next instance of PatchGraph in a stack of observers. */
		private final PatchGraph next;
		
		/** Constructs an instance of the mutator
		 * 
		 * @param gr graph to modify
		 * @param cloneConfig config to use
		 */
		public LearnerGraphMutator(LearnerGraph gr, Configuration config,PatchGraph nextInStack)
		{
			graph = gr;cloneConfig = config;next = nextInStack;
		}
		
		/** Adds the vertex to the graph, cloning this vertex if 
		 * (a) it is not already in the graph,
		 * (b) user requested it to be cloned in the configuration.
		 *  
		 * @param vert vertex to add
		 * @return added vertex
		 */
		protected CmpVertex addNewVertex(CmpVertex vert)
		{
			CmpVertex fromVert = graph.findVertex(vert.getID());
			if (fromVert == null)
			{// vertex with this ID is not already known
				fromVert = LearnerGraph.cloneCmpVertex(vert, cloneConfig);
				graph.transitionMatrix.put(fromVert,new TreeMap<String,CmpVertex>());
			}
			else
				if (fromVert.isAccept() != vert.isAccept()) // it is known but with a different accept condition
					throw new IllegalArgumentException("vertex "+vert+" has a different accept condition to the one in graph "+graph);
			return fromVert;
		}
		
		/** Adds transition to a graph. For each of the source and target states,
		 * if states with the same IDs already present, existing states are used.
		 * If those states do not exist, the respective states are cloned using
		 * the supplied configuration.
		 * 
		 * @param from source state
		 * @param input input
		 * @param to target state
		 */
		public void addTransition(CmpVertex from, String input,CmpVertex to)
		{
			if (next != null) next.addTransition(from, input, to);
			
			CmpVertex fromVert = addNewVertex(from);
			Map<String,CmpVertex> entry = graph.transitionMatrix.get(fromVert);
			
			if (entry.containsKey(input))
				throw new IllegalArgumentException("duplicate transition from state "+from+" with input \""+input+"\" in graph "+graph);
	
			CmpVertex toVert = addNewVertex(to);
			entry.put(input, toVert);
			graph.updateIDWith(fromVert);graph.updateIDWith(toVert);
		}
	
		/** Removes a transition from a graph. It does not matter which graph owns the vertices
		 * supplied - these vertices are not touched while a corresponding transition
		 * in the supplied graph is removed.
		 * 
		 * @param from source state
		 * @param input input
		 * @param to expected target state. This is not really necessary but useful to ensure that whoever removes transitions knows what he/she is doing. 
		 */
		public void removeTransition(CmpVertex from, String input,CmpVertex to)
		{
			if (next != null) next.removeTransition(from, input, to);
			
			Map<String,CmpVertex> entry = graph.transitionMatrix.get(from);
			if (entry == null) throw new IllegalArgumentException("state "+from+" was not found in graph "+graph);
			if (!entry.containsKey(input))
				throw new IllegalArgumentException("there is no transition from state "+from+" with input "+input+" in graph "+graph);
			if (!entry.get(input).equals(to))
				throw new IllegalArgumentException("there is no transition to state "+to+" from state "+from+" with input "+input+"in graph "+graph);
			entry.remove(input);
		}
	
		/** Removes all states which have no outgoing transitions and no incoming transitions,
		 * making sure that the initial state is not removed.
		 */
		public void removeDanglingStates()
		{
			Set<CmpVertex> statesInGraph = new TreeSet<CmpVertex>();
			for(Entry<CmpVertex,Map<String,CmpVertex>> entry:graph.transitionMatrix.entrySet())
				if (entry.getValue().isEmpty()) statesInGraph.add(entry.getKey()); // add those with no outgoing
			for(Entry<CmpVertex,Map<String,CmpVertex>> entry:graph.transitionMatrix.entrySet())
				statesInGraph.removeAll(entry.getValue().values());// and remove those used as targets
			statesInGraph.remove(graph.init);// initial state should stay
			graph.transitionMatrix.keySet().removeAll(statesInGraph);
		}

		/** Sets the initial state to an existing state. Throws if state is not known. */
		public void setInitial(CmpVertex vertex) {
			if (next != null) next.setInitial(vertex);
			
			graph.init = addNewVertex(vertex); // assuming that addNewVertex has been tested as a part of integration testing of addTransition :)
		}
	}

	/** This class displays the requested changes.
	 */
	public static class ChangesDisplay implements PatchGraph
	{
		private final StringBuffer result = new StringBuffer();
		
		/** Next instance of PatchGraph in a stack of observers. */
		private final PatchGraph next;

		public ChangesDisplay(PatchGraph nextInStack)
		{
			next = nextInStack;
		}
		
		protected void appendTransition(CmpVertex from, String label, CmpVertex to)
		{
			result.append(from);result.append(" - ");result.append(label);result.append(" -> ");result.append(to);result.append("\n");
		}
		public void addTransition(CmpVertex from, String label, CmpVertex to) {
			if (next != null) next.addTransition(from, label, to);
			result.append("added  : ");appendTransition(from, label, to);
		}

		public void removeTransition(CmpVertex from, String label, CmpVertex to) {
			if (next != null) next.removeTransition(from, label, to);
			result.append("removed: ");appendTransition(from, label, to);
		}
		
		@Override
		public String toString()
		{
			return result.toString();
		}
		
		public void setInitial(CmpVertex vertex) {
			if (next != null) next.setInitial(vertex);
			result.append("initial : ");result.append(vertex);result.append("\n");
		}
	}
	
	/** This class counts requested changes.
	 */
	public static class ChangesCounter implements PatchGraph
	{
		private int added = 0, removed = 0;
		private final int transitionsInB;
		private final String nameA, nameB;

		/** Next instance of PatchGraph in a stack of observers. */
		private final PatchGraph next;

		public ChangesCounter(LearnerGraph a,LearnerGraph b, PatchGraph nextInStack)
		{
			transitionsInB=b.countEdges();nameA = a.getNameNotNull();nameB=b.getNameNotNull();
			next = nextInStack;
		}
		
		public void addTransition(CmpVertex from, String label,	CmpVertex to) 
		{
			if (next != null) next.addTransition(from, label, to);
			
			++added;
		}

		public void removeTransition(CmpVertex from, String label,	CmpVertex to) 
		{
			if (next != null) next.removeTransition(from, label, to);
			
			++removed;
		}
		
		public int getAdded()
		{
			return added;
		}
		
		public int getRemoved()
		{
			return removed;
		}
		
		/** Returns the estimated compression rate. */
		public double getCompressionRate()
		{
			double result = 0;
			if (transitionsInB > 0) result = ((double)added+removed)/transitionsInB;
			return result;
		}
		
		@Override
		public String toString()
		{
			return "diff of "+nameB+" to "+nameA+" is "+(int)(100.*getCompressionRate())+"% of "+nameB;
		}

		public void setInitial(CmpVertex vertex) {
			if (next != null) next.setInitial(vertex);
		}
	}
	
	/** This class records requested changes and is capable of returning a 
	 * collection of them in a form of XML which can then be applied to a graph.
	 */
	public static class ChangesRecorder implements PatchGraph
	{
		/** Vertices removed from A. */
		private final LearnerGraph removed;

		/** Vertices added by B. */
		private final LearnerGraph added;
		
		private final PatchGraph addedPatcher, removedPatcher;
		
		/** Next instance of PatchGraph in a stack of observers. */
		private final PatchGraph next;

		public ChangesRecorder(PatchGraph nextInStack)
		{
			next = nextInStack;
			Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			added = new LearnerGraph(config);removed = new LearnerGraph(config);
			added.init = null;added.transitionMatrix.clear();// to make sure we can handle an assignment of a reject-state to an initial state
			addedPatcher = new LearnerGraphMutator(added, config,null);removedPatcher = new LearnerGraphMutator(removed,config,null);
		}
		
		/** Used for testing. */
		protected ChangesRecorder(LearnerGraph r,LearnerGraph a,PatchGraph nextInStack)
		{
			next = nextInStack;
			Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			removed = r;added = a;
			addedPatcher = new LearnerGraphMutator(added, config,null);removedPatcher = new LearnerGraphMutator(removed,config,null);
		}
		
		public void addTransition(CmpVertex from, String label, CmpVertex to) {
			if (next != null) next.addTransition(from, label, to);
			addedPatcher.addTransition(from, label, to);
		}

		public void removeTransition(CmpVertex from, String label, CmpVertex to) {
			if (next != null) next.removeTransition(from, label, to);
			removedPatcher.addTransition(from, label, to);
		}
		
		/** GD tags. */
		public static final String gdGD = "GD", gdAdded="gdAdded", gdRemoved = "gdRemoved";
		
		/** Writes the recorded changes in a form of an XML tag. */
		protected Element writeGD(Document doc)
		{
			if (added.init == null) throw new IllegalArgumentException("init state is was not defined");
			Element gd = doc.createElement(gdGD), addedNode = doc.createElement(gdAdded), removedNode = doc.createElement(gdRemoved);
			addedNode.appendChild(added.transform.createGraphMLNode(doc));removedNode.appendChild(removed.transform.createGraphMLNode(doc));
			gd.appendChild(removedNode);gd.appendChild(addedNode);
			return gd;
		}

		/** Given an element containing a number of elements, this one picks the one
		 * with the right tag and returns its first element-child.
		 * 
		 * @param elem
		 * @param name the tag to look for.
		 * @return
		 */
		static public Element getGraphElement(Element elem, String name)
		{
			if (!elem.getNodeName().equals(gdGD))
				throw new IllegalArgumentException("unexpected element, expected "+gdGD+", got "+elem.getNodeName());
			
			NodeList graphlist_List = elem.getElementsByTagName(name);
			int i=0;
			for(;i< graphlist_List.getLength() && graphlist_List.item(i).getNodeType() != Node.ELEMENT_NODE;++i);
			if (i == graphlist_List.getLength()) throw new IllegalArgumentException("no element "+name);
			
			NodeList graphs = graphlist_List.item(i).getChildNodes();
			int gr=0;
			for(;gr<graphs.getLength() && graphs.item(gr).getNodeType() != Node.ELEMENT_NODE;++gr);
			if (gr == graphs.getLength()) throw new IllegalArgumentException("no graph in the "+name+" entry");
			
			Element result = (Element)graphs.item(gr);
			
			for(++gr;gr<graphs.getLength() && graphs.item(gr).getNodeType() != Node.ELEMENT_NODE;++gr);
			if (gr != graphs.getLength()) throw new IllegalArgumentException("more than one graph in the "+name+" entry");
			for(++i;i< graphlist_List.getLength() && graphlist_List.item(i).getNodeType() != Node.ELEMENT_NODE;++i);
			if (i != graphlist_List.getLength()) throw new IllegalArgumentException("duplicate holder "+name);
			
			return result;
		}

		/** Applies GD to the supplied graph. This is a part of GD because it only
		 * handles GD and not general-purpose stuff which would be included in 
		 * <em>Transform</em>. 
		 * 
		 * @param graph graph to transform
		 * @param elem element containing the difference.
		 */
		static public void applyGD(LearnerGraph graph, Element elem)
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			LearnerGraphMutator graphPatcher = new LearnerGraphMutator(graph,config,null);
			loadDiff(graphPatcher, elem);
			graphPatcher.removeDanglingStates();
			//graph.setIDNumbers();			
			//System.out.println("Patch: "+graph.transitionMatrix+" "+graph.vertPositiveID+" "+graph.vertNegativeID+" "+graph.wmethod.checkGraphNumeric());
		}
		
		/** Loads diff from XML. This is a part of GD because it only
		 * handles GD and not general-purpose stuff which would be included in 
		 * <em>Transform</em>. 
		 * 
		 * @param patcher graph to transform
		 * @param elem element containing the difference.
		 */
		static public void loadDiff(PatchGraph graphPatcher, Element elem)
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			LearnerGraph gr = LearnerGraph.loadGraph(getGraphElement(elem, gdRemoved),config);
			for(Entry<CmpVertex,Map<String,CmpVertex>> entry:gr.transitionMatrix.entrySet())
				for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
					graphPatcher.removeTransition(entry.getKey(), transition.getKey(), transition.getValue());

			gr = LearnerGraph.loadGraph(getGraphElement(elem, gdAdded),config);
			for(Entry<CmpVertex,Map<String,CmpVertex>> entry:gr.transitionMatrix.entrySet())
				for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
					graphPatcher.addTransition(entry.getKey(), transition.getKey(), transition.getValue());
			graphPatcher.setInitial(gr.init);
		}

		public void setInitial(CmpVertex vertex) 
		{
			if (next != null) next.setInitial(vertex);
			addedPatcher.setInitial(vertex);			
		}
	}
	
	/** Builds the data structures subsequently used in traversal.
	 * 
	 * @param a the first graph
	 * @param b the second graph
	 * @param threads how many threads to use
	 * @param testValueOfNewToOrig if not null, this one receives a value of newToOrig. Used for testing.
	 */ 
	protected void init(LearnerGraph a,LearnerGraph b,int threads)
	{
		ThreadNumber = threads;
		grCombined = a.copy(a.config);// I cannot simply do Transform.addToGraph here because patch has to be relative to graph A.
		copyOfA = a.copy(a.config);
		grCombined.vertNegativeID=Math.max(grCombined.vertNegativeID, b.vertNegativeID);// we aim for new vertices in grCombined to have ids different from all vertices in B. 
		grCombined.vertPositiveID=Math.max(grCombined.vertPositiveID, b.vertPositiveID);
		combined_initA = grCombined.init;
		Map<CmpVertex,CmpVertex> origToNewB = new TreeMap<CmpVertex,CmpVertex>();
		statesOfA = new TreeSet<CmpVertex>();statesOfA.addAll(grCombined.transitionMatrix.keySet());
		//Transform.addToGraph(grCombined, a, origToNewA);
		// In the past, graph A could have textual vertices so when our new numerical IDs are converted to Strings for comparisons, IDs would overlap.
		// The current graph loading approach via VertexID.parseID generates numerical vertex IDs. Moreover, assertion statements will check for this.
		combined_initB = Transform.addToGraph(grCombined, b,origToNewB);
		
		grCombined.learnerCache.invalidate();
		statesOfB = new TreeSet<CmpVertex>();statesOfB.addAll(origToNewB.values());
		assert statesOfA.size() == a.getStateNumber();
		assert statesOfB.size() == origToNewB.size();assert statesOfB.size() == b.getStateNumber();
		assert statesOfA.size() + statesOfB.size() == grCombined.getStateNumber(): " added "+statesOfB.size()+" states but the outcome is only "+(grCombined.getStateNumber()-statesOfA.size())+" states larger";
		newBToOrig = new TreeMap<CmpVertex,CmpVertex>();
		for(Entry<CmpVertex,CmpVertex> entry:origToNewB.entrySet()) newBToOrig.put(entry.getValue(),entry.getKey());
		
		forward = new LearnerGraphND(grCombined,LearnerGraphND.ignoreNone,false);
		inverse = new LearnerGraphND(grCombined,LearnerGraphND.ignoreNone,true);
		pairScores = new int[forward.getPairNumber()];Arrays.fill(pairScores, LearnerGraphND.PAIR_INCOMPATIBLE);
		// states to be ignored are those where each element of a pair belongs to a different automaton, we fill in the rest.
		List<HandleRow<CmpVertex>> handlerList = new LinkedList<HandleRow<CmpVertex>>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)// this is not doing workload balancing because it should iterate over currently-used left-hand sides, not just all possible ones. 
			handlerList.add(new HandleRow<CmpVertex>()
			{
				public void init(@SuppressWarnings("unused") int threadNo) {}

				public void handleEntry(Entry<CmpVertex, Map<String, CmpVertex>> entryA, @SuppressWarnings("unused") int threadNo) 
				{
					// Now iterate through states
					for(CmpVertex stateB:statesOfB)
					{
						assert pairScores[forward.vertexToIntNR(stateB,entryA.getKey())]==LearnerGraphND.PAIR_INCOMPATIBLE:
							"duplicate number "+forward.vertexToIntNR(stateB,entryA.getKey())+" for states "+
							forward.getStatesToNumber().get(stateB)+","+forward.getStatesToNumber().get(entryA.getKey());
						pairScores[forward.vertexToIntNR(stateB,entryA.getKey())]=
							LearnerGraphND.PAIR_OK;// caching is likely to lower down my performance a lot here
					}
					
					// Perhaps I should be numbering states directly here instead of using numberNonNegativeElements afterwards,
					// but this is not simple to do: I have to give numbers in the order in which triangular traversal visits states.
				}
			});
		LearnerGraphND.performRowTasks(handlerList, ThreadNumber, grCombined.transitionMatrix,new StatesToConsider() {
			public boolean stateToConsider(CmpVertex vert) {
				return statesOfA.contains(vert);
			}
		}, LearnerGraphND.partitionWorkLoadLinear(ThreadNumber,statesOfA.size()));
		final int numberOfPairs = LearnerGraphND.numberNonNegativeElements(pairScores);
		assert numberOfPairs == statesOfA.size()*statesOfB.size();
		
		{
			LSolver solverForward = forward.buildMatrix_internal(pairScores, numberOfPairs, ThreadNumber,DDRH_default.class);
			//System.out.println(inverse.dumpEquations(solverForward, pairScores, newToOrig));
			solverForward.solve();
			solverForward.freeAllButResult();// deallocate memory before creating a large array.
			scoresForward = solverForward.j_x;
		}

		{
			LSolver solverInverse = inverse.buildMatrix_internal(pairScores, numberOfPairs, ThreadNumber,DDRH_default.class);
			solverInverse.solve();
			solverInverse.freeAllButResult();// deallocate memory before creating a large array.
			scoresInverse = solverInverse.j_x;
		}		
	}
	
	/** Goes through the result of linear and identifies candidates for key state pairs.
	 * @return true if everything is ok, false if no perfect set of candidates was found.
	 */
	protected boolean identifyKeyPairs()
	{
		currentWave = new ArrayList<PairScore>(java.lang.Math.max(statesOfA.size(),statesOfB.size()));
		List<HandleRow<CmpVertex>> handlerList = new LinkedList<HandleRow<CmpVertex>>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)// this is not doing workload balancing because it should iterate over currently-used left-hand sides, not just all possible ones. 
			handlerList.add(new HandleRow<CmpVertex>()
			{
				public void init(@SuppressWarnings("unused") int threadNo) {}

				public void handleEntry(Entry<CmpVertex, Map<String, CmpVertex>> entryA, @SuppressWarnings("unused") int threadNo) 
				{
					double scoreHigh = -Double.MAX_VALUE,scoreLow = -Double.MAX_VALUE;
					CmpVertex highState = null;
					// Now iterate through states
					for(CmpVertex stateB:statesOfB)
					{
						int scorePosition = pairScores[forward.vertexToIntNR(stateB,entryA.getKey())];
						double scoreForward = scoresForward[scorePosition],scoreBackward=scoresInverse[scorePosition];
						double score = scoreForward+scoreBackward;
						if (scoreForward < 0)// if a pair is incompatible, ensure the score is negative
							score = Math.min(scoreForward, scoreBackward);
						if (score > scoreHigh)
						{
							scoreLow = scoreHigh;scoreHigh = score;highState = stateB;
						}
						else
							if (score > scoreLow) scoreLow = score;
					}
					assert highState != null;
					currentWave.add(new PairScore(entryA.getKey(),highState,(int)(multiplier*scoreHigh),(int)(multiplier*scoreLow)));
				}
			});
		LearnerGraphND.performRowTasks(handlerList, ThreadNumber, grCombined.transitionMatrix,new StatesToConsider() {
			public boolean stateToConsider(CmpVertex vert) {
				return statesOfA.contains(vert);
			}
		}, LearnerGraphND.partitionWorkLoadLinear(ThreadNumber,statesOfA.size()));

		// now we find so many percent of top values.
		int topScore = 0;// to make sure that if we only get negative pairs, no key states will be detected.
		sortWave(currentWave);
		PairScore topPair = null;
		if (!currentWave.isEmpty() && currentWave.iterator().next().getScore() > topScore)
		{
			topPair = currentWave.iterator().next();
			topScore = topPair.getScore(); // this is done here to avoid cache problems when updating the same variable on multiple threads.
		}
		statesInKeyPairs = new HashSet<CmpVertex>();frontWave = new LinkedList<PairScore>();
		final int threshold = (int)(topScore*(1.-grCombined.config.getGdKeyPairThreshold()));
		// Key pairs added to the collection.
		for(PairScore pair:currentWave)
			if (pair.getScore() > 0 && pair.getScore() >= threshold && // top score good enough
					(pair.getAnotherScore() <= 0 || pair.getAnotherScore() <= pair.getScore()*grCombined.config.getGdLowToHighRatio()) && // and high-low ratio is ok
					!statesInKeyPairs.contains(pair.secondElem) && // and the target state has not already been used in another key pair
					pair.getQ().isAccept() == pair.getR().isAccept() // make sure we do not consider an incompatible pair as a key pair, regardless of the score 
					)
			{
				frontWave.add(pair);statesInKeyPairs.add(pair.getQ());statesInKeyPairs.add(pair.getR());
			}
		boolean result = true;
		// We have to be careful if none is found this way.
		if (frontWave.isEmpty())
		{
			if (topPair != null)
			{// at least we've got a pair with a score over zero.
				if (Boolean.valueOf(Visualiser.getProperty(VIZ_PROPERTIES.LINEARWARNINGS, "false")))
					System.out.println("Linear failed to find perfect candidiates for an initial set of key pairs, using "+topPair);
				frontWave.add(topPair);statesInKeyPairs.add(topPair.getQ());statesInKeyPairs.add(topPair.getR());
			}
			else
			{// nothing of use detected, the difference will contain a union of all transitions in graphs A and B.
				if (Boolean.valueOf(Visualiser.getProperty(VIZ_PROPERTIES.LINEARWARNINGS, "false")))
					System.out.println("Linear failed to find any pairs with positive scores, the diff is the union of A and B");
			}
			result = false;
		}
		
		return result;
	}
	
	/** Reaches out from the front wave to unexplored frontiers matched by the two machines.
	 * In other words, for each pair on the front line, it looks for matched transitions
	 * by the two machines and add pairs of target states to currentWave, as long as none
	 * of these target states are contained in statesInKeyPairs.
	 * 
	 * @param matrixA the first (non-deterministic) matrix
	 * @param matrixB the second (non-deterministic) matrix.
	 */
	protected void populateCurrentWave(Map<CmpVertex,Map<String,List<CmpVertex>>> matrix) 
	{
		for(PairScore pair:frontWave)
		{
			for(Entry<String,List<CmpVertex>> targetA:matrix.get(pair.getQ()).entrySet())
			{
				List<CmpVertex> targetB = matrix.get(pair.getR()).get(targetA.getKey());
				if (targetB != null)
				{// matched pair, now iterate over target states
					for(CmpVertex targetStateA:targetA.getValue())
						for(CmpVertex targetStateB:targetB)
							if (!statesInKeyPairs.contains(targetStateA) && !statesInKeyPairs.contains(targetStateB))
							{
								int scorePosition = pairScores[forward.vertexToIntNR(targetStateA,targetStateB)];
								double score = scoresForward[scorePosition] + scoresInverse[scorePosition];
								currentWave.add(new PairScore(targetStateA,targetStateB,(int)(multiplier*score),0));
							}
				}
			}
		}
	}
}	
	
