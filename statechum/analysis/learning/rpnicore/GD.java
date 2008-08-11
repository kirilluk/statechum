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
import java.util.HashSet;
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
	
	/** Maps vertices of the combined graph to those of the original graphs. */
	Map<CmpVertex,CmpVertex> newToOrig = null;

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
	 * @return XML node with GD.
	 */
	public Element computeGDToXML(LearnerGraph a,LearnerGraph b, int threads, Document doc)
	{
		ChangesRecorder patcher = new ChangesRecorder();
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
	}
	
	/** Expands the set of key pairs and computes the outcome. 
	 * 
	 * @param graphToPatch this will be provided with changes necessary to transform the first graph
	 * to the second one. It can then be stored in XML if necessary.
	 * @param allKeyPairs if not null, this collection will be filled 
	 * with all of the key pairs for the graphs. Used for testing.
	 */
	protected void makeSteps(PatchGraph graphToPatch, List<PairScore> allKeyPairs)
	{
		// Now we make steps. Data used:
		//
		// currentWave is what we'll populate with candidates for key pairs 
		// (the main criterion is that these states are not part of existing
		// key pairs, i.e. not in statesInKeyPairs collection.
		//
		// frontWave is the wavefront from which we are exploring grCombined in
		// search of these candidates for key pairs.
		if (allKeyPairs == null) allKeyPairs = new LinkedList<PairScore>();
		do
		{
			currentWave.clear();
			populateCurrentWave(forward.matrixForward);
			populateCurrentWave(inverse.matrixForward);
			Collections.sort(currentWave);
			allKeyPairs.addAll(frontWave);
			for(PairScore pair:frontWave) newToOrig.put(pair.getR(),newToOrig.get(pair.getQ()));// since we now know 
				// which state of A pair.getQ() of combined corresponds to, change the mapping.
				// addTransitions(grCombined,statesOfB,added,cloneConfig) relies on this change.
			frontWave.clear();
			for(PairScore pair:currentWave)
				if (!statesInKeyPairs.contains(pair.getQ()) && !statesInKeyPairs.contains(pair.getR()))
				{// this is the one for the front line
					frontWave.add(pair);statesInKeyPairs.add(pair.getQ());statesInKeyPairs.add(pair.getR());
				}
		}
		while(!frontWave.isEmpty());

		// Explored everything, now pick all transitions which have been added/removed.
		for(PairScore pair:allKeyPairs)
		{
			for(Entry<String,CmpVertex> transitionA:grCombined.transitionMatrix.get(pair.getQ()).entrySet())
			{
				CmpVertex targetA = transitionA.getValue();
				CmpVertex targetB = grCombined.transitionMatrix.get(pair.getR()).get(transitionA.getKey());

				if (targetB == null) // this transition does not exist in B
					graphToPatch.removeTransition(newToOrig.get(pair.getQ()), transitionA.getKey(),newToOrig.get(transitionA.getValue()));
				else
					if (!statesInKeyPairs.contains(targetB) || !statesInKeyPairs.contains(targetA))
					{// transition leads to a state which is not key in either of the two machines
						graphToPatch.removeTransition(newToOrig.get(pair.getQ()), transitionA.getKey(),newToOrig.get(transitionA.getValue()));
						graphToPatch.addTransition(newToOrig.get(pair.getQ()), transitionA.getKey(),newToOrig.get(targetB));
					}
			}
			
			for(Entry<String,CmpVertex> transitionB:grCombined.transitionMatrix.get(pair.getR()).entrySet())
			{
				CmpVertex targetA = grCombined.transitionMatrix.get(pair.getQ()).get(transitionB.getKey());
				if (targetA == null) // a transition unique to B
					graphToPatch.addTransition(newToOrig.get(pair.getR()), transitionB.getKey(),newToOrig.get(transitionB.getValue()));
			}			
		}

		// now we just need to go through states which are not key states
		addTransitions(grCombined,statesOfA,graphToPatch,false);
		addTransitions(grCombined,statesOfB,graphToPatch,true);
	}
	
	/** Makes it possible to modify graphs by adding/removing transitions. */
	public static class LearnerGraphMutator implements PatchGraph
	{
		/** Graph to manipulate. */
		private final LearnerGraph graph;
		/** Configuration to use for cloning if necessary. */
		private Configuration cloneConfig;
		
		/** Constructs an instance of the mutator
		 * 
		 * @param gr graph to modify
		 * @param cloneConfig config to use
		 */
		public LearnerGraphMutator(LearnerGraph gr, Configuration config)
		{
			graph = gr;cloneConfig = config;
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
			CmpVertex fromVert = graph.findVertex(from.getID());
			Map<String,CmpVertex> entry = null;
			if (fromVert == null)
			{
				fromVert = LearnerGraph.cloneCmpVertex(from, cloneConfig);
				entry = new TreeMap<String,CmpVertex>();graph.transitionMatrix.put(fromVert,entry);
			}
			else
				if (fromVert.isAccept() != from.isAccept())
					throw new IllegalArgumentException("vertex "+from+" has a different accept condition to the one in graph "+graph);
				else
					entry = graph.transitionMatrix.get(fromVert);
			
			if (entry.containsKey(input))
				throw new IllegalArgumentException("duplicate transition from state "+from+" with input "+input+"in graph "+graph);
	
			CmpVertex toVert = graph.findVertex(to.getID());
			if (toVert == null)
				toVert = LearnerGraph.cloneCmpVertex(to,cloneConfig);
			else
				if (toVert.isAccept() != to.isAccept())
					throw new IllegalArgumentException("vertex "+to+" has a different accept condition to the one in graph "+graph);
				
			entry.put(input, toVert);
			if (!graph.transitionMatrix.containsKey(toVert))
				graph.transitionMatrix.put(toVert, new TreeMap<String,CmpVertex>());
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
	}

	/** This class displays the requested changes.
	 */
	public static class ChangesDisplay implements PatchGraph
	{
		private final StringBuffer result = new StringBuffer();
		
		protected void appendTransition(CmpVertex from, String label, CmpVertex to)
		{
			result.append(from);result.append(" - ");result.append(label);result.append(" -> ");result.append(to);result.append("\n");
		}
		public void addTransition(CmpVertex from, String label, CmpVertex to) {
			result.append("added  : ");appendTransition(from, label, to);
		}

		public void removeTransition(CmpVertex from, String label, CmpVertex to) {
			result.append("removed: ");appendTransition(from, label, to);
		}
		
		@Override
		public String toString()
		{
			return result.toString();
		}
	}
	
	/** This class counts requested changes.
	 */
	public static class ChangesCounter implements PatchGraph
	{
		private int added = 0, removed = 0;
		private final int transitionsInB;
		private final String nameA, nameB;

		public ChangesCounter(LearnerGraph a,LearnerGraph b)
		{
			transitionsInB=b.countEdges();nameA = a.getNameNotNull();nameB=b.getNameNotNull();
		}
		
		public void addTransition(
				@SuppressWarnings("unused") CmpVertex from, 
				@SuppressWarnings("unused")	String label, 
				@SuppressWarnings("unused")	CmpVertex to) 
		{
			++added;
		}

		public void removeTransition(
				@SuppressWarnings("unused")	CmpVertex from, 
				@SuppressWarnings("unused")	String label, 
				@SuppressWarnings("unused")	CmpVertex to) 
		{
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
		
		@Override
		public String toString()
		{
			return "diff of "+nameB+" to "+nameA+" : "+(transitionsInB == 0?"NONE":(int)(100.*((double)added+removed)/transitionsInB)+"%");
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
		
		public ChangesRecorder()
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			added = new LearnerGraph(config);removed = new LearnerGraph(config);
			addedPatcher = new LearnerGraphMutator(added, config);removedPatcher = new LearnerGraphMutator(removed,config);
		}
		
		/** Used for testing. */
		protected ChangesRecorder(LearnerGraph r,LearnerGraph a)
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			removed = r;added = a;
			addedPatcher = new LearnerGraphMutator(added, config);removedPatcher = new LearnerGraphMutator(removed,config);
		}
		
		public void addTransition(CmpVertex from, String label, CmpVertex to) {
			addedPatcher.addTransition(from, label, to);
		}

		public void removeTransition(CmpVertex from, String label, CmpVertex to) {
			removedPatcher.addTransition(from, label, to);
		}
		
		/** GD tags. */
		protected static final String gdGD = "GD", gdAdded="gdAdded", gdRemoved = "gdRemoved";
		
		/** Writes the recorded changes in a form of an XML tag. */
		protected Element writeGD(Document doc)
		{
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
			LearnerGraphMutator graphPatcher = new LearnerGraphMutator(graph,config);
			loadDiff(graphPatcher, elem);
			graphPatcher.removeDanglingStates();			
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
		}
	}
	
	/** Iterates through states and adds transitions leading from states which 
	 * were not matched to the supplied collection.
	 * @param graph graph to process
	 * @param verticesForGraph vertices to iterate through. 
	 * @param toAdd if true will add, otherwise remove
	 * @param whereToAdd where to add the result
	 */
	protected void addTransitions(LearnerGraph graph, Set<CmpVertex> verticesForGraph, PatchGraph patcher, boolean toAdd)
	{
		for(CmpVertex vertex:verticesForGraph)
			if (!statesInKeyPairs.contains(vertex))
			{
				CmpVertex origSource = newToOrig.get(vertex);
				for(Entry<String,CmpVertex> target:graph.transitionMatrix.get(vertex).entrySet())
					// transition not matched because some states are not known hence append it.
				if (toAdd)
					patcher.addTransition(origSource, target.getKey(),newToOrig.get(target.getValue()));
				else
					patcher.removeTransition(origSource, target.getKey(),newToOrig.get(target.getValue()));
			}
	}

	/** Builds the data structures subsequently used in traversal. */
	protected void init(LearnerGraph a,LearnerGraph b,int threads)
	{
		ThreadNumber = threads;
		grCombined = a.copy(a.config.copy());
		//stateToNumber = grCombined.learnerCache.getStateToNumber();
		statesOfA = new TreeSet<CmpVertex>();statesOfA.addAll(grCombined.transitionMatrix.keySet());
		Map<CmpVertex,CmpVertex> origToNew = new TreeMap<CmpVertex,CmpVertex>();
		Transform.addToGraph(grCombined, b,origToNew);
		grCombined.learnerCache.invalidate();// even though we've reset the cache, our 
		// stateToNumber map above is still useful since it provides both (1) a way to find
		// out whether a state is a part of the first graph (we call it A) and 
		// (2) a numbering of those states so when I traverse a subset of a 
		// transition matrix with rows from A I know which state-pair numbers to assign.
		statesOfB = new TreeSet<CmpVertex>();statesOfB.addAll(origToNew.values());

		newToOrig = new TreeMap<CmpVertex,CmpVertex>();
		for(Entry<CmpVertex,CmpVertex> entry:origToNew.entrySet()) newToOrig.put(entry.getValue(),entry.getKey());
		for(CmpVertex vert:a.transitionMatrix.keySet()) newToOrig.put(vert, vert);// mapping for vertices of A is a tautology due to cloning.
		
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
						pairScores[forward.vertexToIntNR(stateB,entryA.getKey())]=
							LearnerGraphND.PAIR_OK;// caching is likely to lower down my performance a lot here
					
					// Perhaps I should be numbering states directly here instead of using numberNonNegativeElements afterwards.
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
						double score = scoresForward[scorePosition] 
							+ scoresInverse[scorePosition];
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
//TestGD.printListOfPairs(this, currentWave);
		// now we find so many percent of top values.
		int topScore = -1;
		PairScore topPair = null;
		for(PairScore pair:currentWave)
			if (pair.getScore() > topScore)
			{
				topScore = pair.getScore(); // this is done here to avoid cache problems when updating the same variable on multiple threads.
				topPair = pair;
			}
		assert topPair != null;
		
		statesInKeyPairs = new HashSet<CmpVertex>();frontWave = new LinkedList<PairScore>();
		final int threshold = (int)(topScore*(1.-grCombined.config.getGdKeyPairThreshold()));
		// Key pairs added to the collection.
		for(PairScore pair:currentWave)
			if (pair.getScore() > 0 && pair.getScore() >= threshold &&
					(pair.getAnotherScore() <= 0 || pair.getAnotherScore() <= pair.getScore()*grCombined.config.getGdLowToHighRatio()))
			{
				frontWave.add(pair);statesInKeyPairs.add(pair.getQ());statesInKeyPairs.add(pair.getR());
			}
		boolean result = true;
		// We have to be careful if none is found this way.
		if (frontWave.isEmpty())
		{
			if (Boolean.valueOf(Visualiser.getProperty(VIZ_PROPERTIES.LINEARWARNINGS, "false")))
				System.out.println("Linear failed to find perfect candidiates for an initial set of key pairs");
			result = false;
			frontWave.add(topPair);statesInKeyPairs.add(topPair.getQ());statesInKeyPairs.add(topPair.getR());
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
	
