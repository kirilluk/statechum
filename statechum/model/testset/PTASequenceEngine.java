/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
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

package statechum.model.testset;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.ArrayOperations;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;

public class PTASequenceEngine 
{
	/** The machine used to match all paths supplied against */
	FSMAbstraction fsm = null;
	
	/** The transition diagram of the pta stored in this object. Each node is an integer, negatives for reject, non-negatives for accept. */
	protected final Map<PTASequenceEngine.Node,Map<String,PTASequenceEngine.Node>> pta = new HashMap<PTASequenceEngine.Node,Map<String,PTASequenceEngine.Node>>(); 
	
	/** The global "counter" of nodes; this is not static to avoid racing problems associated with multiple threads
	 * creating nodes, so that the same thread may end up with multiple nodes bearing the same ID. This may
	 * have been causing random failures when question generation would produce sequences not possible in 
	 * the merged machine, running it multiple times would sometimes produce such sequences, sometimes not,
	 * but each produced sequence was different from another one (i.e. one failed, another few passed, next failing
	 * one different from the first one, all with the same transition matrix and even before we consider
	 * loops in the blue state).
	 */
	protected int positiveNodeID = 1;
	protected int negativeNodeID = -1;
	protected PTASequenceEngine.Node rejectNode = new Node();

	public FSMAbstraction getFSM()
	{
		return fsm;
	}
	
	/** Represents elements of the PTA. */
	public class Node  
	{
		
		/** Constructor for reject nodes. */
		Node() 
		{ 
			ID = negativeNodeID;fsmState=null;
		}

		/** Constructor for accept nodes. */
		public Node(Object state)
		{
			if (state == null)
				throw new IllegalArgumentException("state name cannot be null");
			ID = positiveNodeID++;fsmState = state;
		}
		
		public int getID()
		{
			return ID;
		}
		
		public boolean isAccept()
		{
			return ID >= 0;
		}

		public Object getState()
		{
			return fsmState;
		}
				
		/** The ID of this node, positive for accept nodes, negative for reject ones. */
		private final int ID;
		/** The FSM state this object corresponds. */
		private final Object fsmState;

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			return ID;
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
			if (getClass() != obj.getClass())
				return false;
			final PTASequenceEngine.Node other = (PTASequenceEngine.Node) obj;
			return ID == other.ID;// Since every two different instances of Node have different IDs, this comparison is always false, but I decided to keep it in case this changes in future. 
		}
		
		@Override
		public String toString()
		{
			if (!isAccept())
				return "REJECT";

			return ""+ID+"("+fsmState+")";
		}
	}
	
	/** The initial node of the pta */
	protected PTASequenceEngine.Node init = null; 
	
	/** I cannot simply use LearnerGraph here because LearnerGraph is a normal data 
	 * structure while what we need is a programmatical extraction of FSM. This 
	 * permits a lazy generation of those machines on the fly, potentially introducing
	 * infinite machines like what is possible with infinite lists in Haskell 
	 * and the like. 
	 */
	public interface FSMAbstraction 
	{
		/** The next-state function.
		 * Important: this function is only used to get new state which has not 
		 * yet been visited; for states which we've seen earlier, PTATestSequenceEngine will
		 * store the appropriate entries in its map.
		 */
		public Object getNextState(Object currentState, String input); 
		/** returns the initial state */
		public Object getInitState();
		/** returns true if the given state is an accept-state. */
		public boolean isAccept(Object currentState);
		/** If this abstraction is used to extend FSM, such as when getNextState performs an extension,
		 * this method can be used to set accept/reject conditions on vertices.
		 */
		public void setAccept(Object currentState, boolean value);
		/** Whether a sequence ending at a given vertex should be returned as a result of getData(). */
		public boolean shouldBeReturned(Object elem);
	}
	
	
	public PTASequenceEngine() {}
	
	/** Initialises this PTA engine with an underlying machine. There is no method 
	 * to swap a machine for a different one afterwards since states of the original
	 * machine used in PTA directly. Perhaps there has to be a mechanism to substitute
	 * a machine and relabel the existing nodes in a PTA with those of the new machine,
	 * expecting equals on them to define the expected (total) injection (orig->new). 
	 * 
	 * @param machine
	 */
	public void init(FSMAbstraction machine)
	{
		fsm = machine;
		pta.clear();// this is needed if I override the default constructor which also calls Init; without this clear, I end up with two supposedly init nodes
		if (machine.isAccept(machine.getInitState()))
			init =  new Node(machine.getInitState());
		else
			init = rejectNode;
		
		pta.put(init,new LinkedHashMap<String,PTASequenceEngine.Node>());
		pta.put(rejectNode,new LinkedHashMap<String,PTASequenceEngine.Node>());
	}
	
	/** Represents a set of sequences using a PTA, backed by an underlying state machine, passed in at initialisation. */
	public class SequenceSet 
	{
		private List<PTASequenceEngine.Node> ptaNodes;
		
		public SequenceSet()
		{
			ptaNodes = new LinkedList<PTASequenceEngine.Node>();
		}
		
		public void setIdentity()
		{
			ptaNodes.clear();ptaNodes.add(init);
		}
		
		private PTASequenceEngine getEnclosingObject()
		{
			return PTASequenceEngine.this;
		}
		
		public boolean isEmpty()
		{
			return ptaNodes.isEmpty();
		}
		
		/** Limits the number of nodes in the set to the specified number, 
		 * by throwing away the rest of the elements.  
		 *
		 * @param number the number of nodes to limit this set to. -1 means include all elements.
		 */
		public void limitTo(int number)
		{
			if (number >= 0 && number<ptaNodes.size())
				ptaNodes = ptaNodes.subList(0, number);
		}
		
		/** Unites the given set of sequence with the supplied one.
		 * Important: cannot cope well with duplicate nodes, they will be preserved.
		 * 
		 * @param with a sequence set to unite with
		 */
		public void unite(SequenceSet with)
		{
			if (getEnclosingObject() != with.getEnclosingObject())
				throw new IllegalArgumentException("unite with an argument from a different PTA machine");
			ptaNodes.addAll(with.ptaNodes);
		}
		
		/** Appends elements from the supplied set to those stored in this sequenceSet.
		 * 
		 * @param inputs set with elements to append
		 * @return
		 */
		public SequenceSet crossWithSet(Collection<String> inputs)
		{
			SequenceSet result = new SequenceSet();
			for(PTASequenceEngine.Node node:ptaNodes)
			{
				for(String input:inputs)
				{
					Node newNode = followToNextNode(node, input);
					if (newNode.isAccept()) // successfully extended
						result.ptaNodes.add(newNode);
				}
			}
			return result;
		}

		public SequenceSet crossWithMap(Map<String, CmpVertex> map) {
			SequenceSet result = new SequenceSet();
			for(PTASequenceEngine.Node node:ptaNodes)
			{
				for(Entry<String,CmpVertex> entry:map.entrySet())
				{
					Node newNode = followToNextNode(node, entry.getKey());
					if (newNode.isAccept()) // successfully extended
					{
						result.ptaNodes.add(newNode);
						fsm.setAccept(newNode.fsmState,entry.getValue().isAccept());
					}
				}
			}
			return result;
		}

		public SequenceSet cross(Collection<List<String>> inputSequences)
		{
			SequenceSet result = new SequenceSet();
			
			for(List<String> inputSequence:inputSequences)
				for(PTASequenceEngine.Node node:ptaNodes)
				{
						PTASequenceEngine.Node currentNode = node;
						Iterator<String> seqIt = inputSequence.iterator();
						while(seqIt.hasNext() && currentNode.isAccept())
							currentNode=followToNextNode(currentNode, seqIt.next());
						
						if (currentNode.isAccept())
							result.ptaNodes.add(currentNode);
				}
			return result;
		}

		public SequenceSet crossWithSequence(List<String> inputSequence)
		{
			SequenceSet result = new SequenceSet();
			
			for(PTASequenceEngine.Node node:ptaNodes)
			{
					PTASequenceEngine.Node currentNode = node;
					Iterator<String> seqIt = inputSequence.iterator();
					while(seqIt.hasNext() && currentNode.isAccept())
						currentNode=followToNextNode(currentNode, seqIt.next());
					
					if (currentNode.isAccept())
						result.ptaNodes.add(currentNode);
			}
			
			return result;
		}
		
		/** Given a node, this method determines whether that node belongs to this set of nodes.
		 * 
		 * @param currentVertex node to look for
		 * @return true if the supplied node is a part of this set of nodes.
		 */
		boolean contains(Node someNode) {
			return ptaNodes.contains(someNode);
		}
		
		public String getDebugData()
		{
			return PTASequenceEngine.this.getDebugData(this);
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((ptaNodes == null) ? 0 : ptaNodes.hashCode());
			result = prime * result + getEnclosingObject().hashCode();
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
			if (!(obj instanceof SequenceSet))
				return false;
			
			final SequenceSet other = (SequenceSet) obj;
			if (getEnclosingObject() != other.getEnclosingObject())
				return false;
			if (ptaNodes == null) {
				if (other.ptaNodes != null)
					return false;
			} else if (!ptaNodes.equals(other.ptaNodes))
				return false;
			return true;
		}
	}
	
	protected PTASequenceEngine.Node followToNextNode(PTASequenceEngine.Node currentNode, String input)
	{
		Map<String,PTASequenceEngine.Node> row = pta.get(currentNode);
		PTASequenceEngine.Node nextCurrentNode = null;

		if (row.containsKey(input))
			nextCurrentNode = row.get(input); // the next node is an accept one
		else
		{// No transition in the pta with the given input, 
		 // hence we have to extend the pta by adding a new transition
			Object newState = fsm.getNextState(currentNode.getState(), input); 
			if (newState == null || !fsm.isAccept(newState))
			{
				row.put(input, rejectNode);// next node is the reject one
				nextCurrentNode = rejectNode;
			}
			else
			{
				PTASequenceEngine.Node nextNode = new Node(newState);
				row.put(input, nextNode);pta.put(nextNode, new HashMap<String,PTASequenceEngine.Node>());
				nextCurrentNode = nextNode;
			}
		}
		
		return nextCurrentNode;
	}

	/** Checks whether the supplied sequence is contained in this PTA. 
	 * 
	 * @param inputSequence the sequence to check the existence of.
	 */
	public boolean containsSequence(List<String> inputSequence)
	{
		return containsSequence(inputSequence,false);
	}
	
	/** Checks whether the supplied sequence is contained in this PTA. 
	 * Returns true if the current sequence leads to a leaf in this PTA.
	 * 
	 * @param inputSequence the sequence to check the existence of.
	 */
	public boolean containsAsLeaf(List<String> inputSequence)
	{
		return containsSequence(inputSequence,true);
	}

	/** Checks whether the supplied sequence is contained in this PTA. 
	 * @param inputSequence the sequence to check the existence of
	 * @param checkLeaf if false, only checks whether the current sequence exists,
	 * if true, returns true if the current sequence leads to a leaf in this PTA. 
	 */
	private boolean containsSequence(List<String> inputSequence, boolean checkLeaf)
	{
		PTASequenceEngine.Node currentNode = init;if (!currentNode.isAccept()) throw new IllegalArgumentException("untested on empty graphs");
		Iterator<String> seqIt = inputSequence.iterator();
		while(seqIt.hasNext() && currentNode.isAccept())
		{
			Map<String,PTASequenceEngine.Node> row = pta.get(currentNode);
			String input = seqIt.next();
			if (row.containsKey(input))
				currentNode = row.get(input);
			else
				return false;// no transition with the current input
		}
		if (seqIt.hasNext())
			return false;// reached a reject state but not the end of the sequence

		return !checkLeaf || 
			pta.get(currentNode).isEmpty();
	}

	/** Turns this PTA into a set of sequences and returns this set. */
	public Collection<List<String>> getDataORIG()
	{
		Collection<List<String>> result = new LinkedList<List<String>>();
		Queue<Node> currentExplorationBoundary = new LinkedList<Node>();// FIFO queue
		Queue<List<String>> currentExplorationSequence = new LinkedList<List<String>>();// FIFO queue
		currentExplorationBoundary.add(init);currentExplorationSequence.add(new LinkedList<String>());
		while(!currentExplorationBoundary.isEmpty())
		{
			Node currentVertex = currentExplorationBoundary.remove();List<String> currentSequence = currentExplorationSequence.remove();
			Map<String,Node> row = pta.get(currentVertex);
			if (row.isEmpty())
			{
				// the current node is the last on a path, hence we simply add the current sequence to the result
				if (fsm.shouldBeReturned(currentVertex.getState())) 
					result.add(currentSequence);
			}
			else
				for(Entry<String,Node> entry:row.entrySet())
				{
					List<String> newSeq = new LinkedList<String>();newSeq.addAll(currentSequence);newSeq.add(entry.getKey());
					currentExplorationBoundary.offer(entry.getValue());currentExplorationSequence.offer(newSeq);
				}
		}
		
		return result;
	}
	
	public List<List<String>> getData()
	{
		return getData(null);
	}
	
	/** Returns the data from the PTA where only paths ending at nodes marked as 
	 * true by the supplied predicate are returned. 
	 * If null, uses the internal predicate of the fsm.
	 * 
	 * @param predicate determines which paths are returned.
	 * @return the collection of paths for which the predicate holds.
	 */ 
	public List<List<String>> getData(final FilterPredicate predicate)
	{
		final List<List<String>> result = new LinkedList<List<String>>();
		PTAExploration<Boolean> exploration = new PTAExploration<Boolean>(PTASequenceEngine.this) {
			@Override
			public Boolean newUserObject() {
				return new Boolean(true);
			}

			@Override
			public void nodeEntered(@SuppressWarnings("unused") PTAExplorationNode currentNode, @SuppressWarnings("unused") LinkedList<PTAExplorationNode> pathToInit) {
				// this is needed to implement an interface, but we only care if a leaf is entered.
			}

			@Override
			public void leafEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				if ((predicate == null && currentNode.shouldBeReturned()) ||
						(predicate != null && predicate.shouldBeReturned(currentNode.ptaNode.getState()))
						)
				{
					LinkedList<String> newSeq = new LinkedList<String>();
					for(PTAExplorationNode elem:pathToInit)	newSeq.addFirst(elem.inputFromThisNode);
					result.add(newSeq);
				}
			}

			@Override
			public void nodeLeft(
						@SuppressWarnings("unused") PTAExplorationNode currentNode,
						@SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit) 
			{
				// this is needed to implement an interface, but we only care if a leaf is entered.
			}
			
		};
		exploration.walkThroughAllPaths();
		return result;
	}
	
	/** Returns a textual representation of nodes held in the supplied set.
	 * Important: do not change the returned data unless you are prepared to modify tests
	 * relying on it, such as testComputePathsSBetween1.
	 * 
	 * @param targetNodes nodes to "display"
	 * @return textual representation of nodes in the set.
	 */
	public String getDebugData(SequenceSet targetNodes)
	{
		StringBuffer result = new StringBuffer();
		for(Entry<String,String> elem:getDebugDataMapDepth(targetNodes).entrySet())
		{
			result.append('[');result.append(elem.getKey());result.append(']');result.append(elem.getValue());result.append('\n');
		}
		return result.toString();
	}
	
	public static enum DebugDataValues { 
		LEAF("leaf"), INNER("inner"), sequenceReturned("returned"), sequenceTrashed("trashed");
		
		private final String text;
		DebugDataValues(String representation)
		{
			text = representation;
		}
		
		@Override
		public String toString()
		{
			return text;
		}
		
		public static String booleanToString(boolean leaf, boolean returned)
		{
			StringBuffer result = new StringBuffer();
			if (leaf) result.append(DebugDataValues.LEAF);else result.append(DebugDataValues.INNER);result.append(ArrayOperations.separator);
			if (returned) result.append(DebugDataValues.sequenceReturned);else result.append(DebugDataValues.sequenceTrashed);
			return result.toString();
		}
	}
		
	/** Returns a representation of nodes held in the supplied set.
	 * Important: do not change the returned data unless you are prepared to modify tests
	 * relying on it, such as testComputePathsSBetween1.
	 * 
	 * @param targetNodes nodes to "display"
	 * @return a map from a textual representation of nodes in the set to whether this is accept/reject/leaf/inner/returned/trashed node.
	 */
	public Map<String,String> getDebugDataMapDepth(final SequenceSet targetNodes)
	{
		final Map<String,String> setToBeReturned = new HashMap<String,String>();
		PTAExploration<Boolean> exploration = new PTAExploration<Boolean>(PTASequenceEngine.this) {
			@Override
			public Boolean newUserObject() {
				return new Boolean(true);
			}

			@Override
			public void nodeEntered(PTAExplorationNode currentNode, LinkedList<PTAExplorationNode> pathToInit) {
				if (targetNodes != null && targetNodes.contains(currentNode.ptaNode))
					addPath(currentNode, pathToInit);
			}

			@Override
			public void leafEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				if (targetNodes == null || targetNodes.contains(currentNode.ptaNode)) addPath(currentNode,pathToInit);
			}

			@Override
			public void nodeLeft(
						@SuppressWarnings("unused") PTAExplorationNode currentNode,
						@SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit) 
			{
				// this is needed to implement an interface, but we only care if a node (or a leaf) is entered.
			}
			
			private void addPath(PTAExplorationNode currentNode,LinkedList<PTAExplorationNode> pathToInit)
			{
				LinkedList<String> newSeq = new LinkedList<String>();
				for(PTAExplorationNode elem:pathToInit)	newSeq.addFirst(elem.inputFromThisNode);
				setToBeReturned.put(ArrayOperations.seqToString(
						newSeq),DebugDataValues.booleanToString(currentNode.isTail(), fsm.shouldBeReturned(currentNode.ptaNode.getState())));
			}
		};
		exploration.walkThroughAllPaths();
		return setToBeReturned;
	}

	/** Returns a representation of nodes held in the supplied set.
	 * Important: do not change the returned data unless you are prepared to modify tests
	 * relying on it, such as testComputePathsSBetween1.
	 * Note: this one is only used for consistency checking of getDebugDataMapDepth (the higher-order version of this routine).  
	 * 
	 * @param targetNodes nodes to "display"
	 * @return a map from a textual representation of nodes in the set to whether this is accept/reject/leaf/inner/returned/trashed node.
	 */
	public Map<String,String> getDebugDataMapBreadth(final SequenceSet targetNodes)
	{
		final Map<String,String> setToBeReturned = new HashMap<String,String>();
		Queue<Node> currentExplorationBoundary = new LinkedList<Node>();// FIFO queue
		Queue<List<String>> currentExplorationSequence = new LinkedList<List<String>>();// FIFO queue
		currentExplorationBoundary.add(init);currentExplorationSequence.add(new LinkedList<String>());
		
		while(!currentExplorationBoundary.isEmpty())
		{
			Node currentVertex = currentExplorationBoundary.remove();List<String> currentSequence = currentExplorationSequence.remove();
			Map<String,Node> row = pta.get(currentVertex);
			if ( (targetNodes == null && row.isEmpty()) ||
					(targetNodes != null && targetNodes.contains(currentVertex)))
			{// the current node is the last on a path, hence we simply add the current sequence to the result
					setToBeReturned.put(ArrayOperations.seqToString(currentSequence),DebugDataValues.booleanToString(row.isEmpty(), fsm.shouldBeReturned(currentVertex.getState())));
			}
			
			if (!row.isEmpty()) // continue exploring if we can
				for(Entry<String,Node> entry:row.entrySet())
				{
					List<String> newSeq = new LinkedList<String>();newSeq.addAll(currentSequence);newSeq.add(entry.getKey());
					currentExplorationBoundary.offer(entry.getValue());currentExplorationSequence.offer(newSeq);
				}
		}
		
		return setToBeReturned;
	}

	/** Returns the number of tail accept nodes in this PTA. */
	public int numberOfLeafNodes() {
		int result =0;
		boolean rejectReturned = fsm.shouldBeReturned(rejectNode.getState());
		
		if (init == rejectNode)
			return rejectReturned?1:0;
		
		for(Entry<Node, Map<String,Node>> entry:pta.entrySet())
			if (entry.getKey() != rejectNode)
			{
				if (entry.getValue().isEmpty())
				{
					if (fsm.shouldBeReturned(entry.getKey().getState()))
						++result;
				}
				else
					if (rejectReturned)
					{
						for(Entry<String,Node> out:entry.getValue().entrySet())
							if (out.getValue() == rejectNode)
								++result;
					}
			}	
		
		return result;
	}

	public interface FilterPredicate 
	{
		/** Whether a node with a specified name should be returned. Used during filtering. */
		public boolean shouldBeReturned(Object name);
	}
	
	/** A predicate which always returns true. */
	public static final FilterPredicate truePred = new FilterPredicate()
	{
		public boolean shouldBeReturned(@SuppressWarnings("unused") Object name) {
			return true;
		}
	};
	
	/** Returned a filter predicate determined by the underlying fsm. */
	public FilterPredicate getFSM_filterPredicate()
	{
		return new FilterPredicate()
		{
			public boolean shouldBeReturned(Object name) {
				return fsm.shouldBeReturned(name);
			}
		};
	}
	
	/** Returns a subset of this PTA, with all paths leading to rejected nodes removed.
	 * Nodes to be rejected are determined via filterPredicate.shouldBeReturned(). 
	 * Important: no nodes are cloned, in the expectation of them to be immutable.
	 * 
	 *  @param filterPredicate determines which nodes are to be retained.
	 */
	public PTASequenceEngine filter(final FilterPredicate filterPredicate)
	{
		PTASequenceEngine result = new PTASequenceEngine();
		result.init = init;result.rejectNode = rejectNode;result.fsm=fsm;
		result.negativeNodeID = negativeNodeID;result.positiveNodeID=positiveNodeID;
		final List<Node> nodesToTrash = new LinkedList<Node>();
			// all paths from these nodes are to be removed. The idea is to scan the PTA
			// looking for the lowest nodes (aka closest to the root) such that 
			// all paths from them lead to tail nodes which are reject-nodes.
			// Subsequently, I can scan the PTA again and remove all transitions to
			// such lowest nodes as well as rows corresponding to them and those they
			// point to. 
			// The above can be done more easily by recording all nodes such that
			// all paths from them lead to reject tail-nodes. I can then remove all rows
			// associated with them and all transitions from the remaining rows leading
			// to such states.
		
		/* The user object here is a boolean indicating
		 * whether all paths from this state lead to reject nodes. Default 
		 * value is true (set in the newUserObject method).
		 * The value is set to false if we've discovered that not 
		 * all paths from the current node lead to reject nodes. 
		 */ 
		PTAExploration<Boolean> exploration = new PTAExploration<Boolean>(PTASequenceEngine.this) {
			@Override
			public Boolean newUserObject() {
				return new Boolean(true);
			}

			@Override
			public void nodeEntered(
					@SuppressWarnings("unused")	PTAExplorationNode currentNode,
					@SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit) {
				// nothing to do here.
			}

			@Override
			public void leafEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				currentNode.userObject = !filterPredicate.shouldBeReturned(currentNode.ptaNode.getState());
				handleAcceptCondition(currentNode, pathToInit);
			}

			@Override
			public void nodeLeft(PTAExplorationNode currentNode,LinkedList<PTAExplorationNode> pathToInit) 
			{
				handleAcceptCondition(currentNode, pathToInit);
			}
			
			private void handleAcceptCondition(PTAExplorationNode currentNode,LinkedList<PTAExplorationNode> pathToInit)
			{
				if (!currentNode.userObject)
				{// go through the whole path from the current leaf to the initial state, clearing the "all paths to reject" flags.
					Iterator<PTAExplorationNode> previousOnStack = pathToInit.iterator();
					while(previousOnStack.hasNext())
					{
						PTAExplorationNode previous = previousOnStack.next();
						if (previous.userObject)
							previous.userObject = false;
						else
							break;// since nodes satisfying (allPathToReject == false) are 
								// prefix-closed by construction, once we found one which 
								// is already marked false, this means that all earlier 
								// ones are already marked false. 
					}
				}
				else
				// this node and all its children lead to reject states
					nodesToTrash.add(currentNode.ptaNode);
			}
		};
		exploration.walkThroughAllPaths();

		// Now, only need to copy the part of the original graph which does not refer to the reject-nodes.
		for(Entry<Node, Map<String,Node>> entry:pta.entrySet())
			if (!nodesToTrash.contains(entry.getKey()) || entry.getKey() == init) // never throw away the init state
			{// this node is not the one to be removed, hence add it, but filter the row to exclude elements referring to nodes to be removed.
				Map<String,Node> row = new LinkedHashMap<String,Node>();
				for(Entry<String,Node> rowEntry:entry.getValue().entrySet())
					if (!nodesToTrash.contains(rowEntry.getValue()))
						row.put(rowEntry.getKey(),rowEntry.getValue());
				result.pta.put(entry.getKey(), row);
			}
		
		return result;
	}
	
	/** This one takes a PTA and returns the sum of the length of all paths. */
	public static int ORIGstringCollectionSize(PTASequenceEngine pta)
	{
		int size = 0;
		for (List<String> list : pta.getData()) {
			size = size + list.size();
		}
		return size;
	}
	
	/** This one takes a PTA and returns (1) the sum of the length of all paths. 
	 * and (2) the number of nodes.
	 */
	public static Pair<Integer,Integer> stringCollectionSize(PTASequenceEngine pta)
	{
		final AtomicInteger counterCompressed = new AtomicInteger(-1), counterUncompressed = new AtomicInteger(0);
		
		PTAExploration<Boolean> exploration = new PTAExploration<Boolean>(pta) {
			@Override
			public Boolean newUserObject() {
				return null;
			}

			@Override
			public void nodeEntered(
					@SuppressWarnings("unused")	PTAExplorationNode currentNode,
					@SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit) {
				counterCompressed.addAndGet(1);
			}

			@Override
			public void leafEntered(@SuppressWarnings("unused")	PTAExplorationNode currentNode,	
					LinkedList<PTAExplorationNode> pathToInit) 
			{
				counterCompressed.addAndGet(1);
				counterUncompressed.addAndGet(pathToInit.size());
			}

			@Override
			public void nodeLeft(@SuppressWarnings("unused")	PTAExplorationNode currentNode,
					@SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit) {
				// nothing to do here.
			}

		};
		exploration.walkThroughAllPaths();
		
		return new Pair<Integer,Integer>(counterUncompressed.get(),counterCompressed.get());
	}
}

