/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of statechum.

statechum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Foobar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.xmachine.model.testset;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Map.Entry;

import statechum.ArrayOperations;

public class PTATestSequenceEngine {
	/** The machine used to match all paths supplied against */
	private FSMAbstraction fsm = null;

	/**
	 * The transition diagram of the pta stored in this object. Each node is an
	 * integer, negatives for reject, non-negatives for accept.
	 */
	protected final Map<PTATestSequenceEngine.Node, Map<String, PTATestSequenceEngine.Node>> pta = new HashMap<PTATestSequenceEngine.Node, Map<String, PTATestSequenceEngine.Node>>();

	/**
	 * The global "counter" of nodes; this is not static to avoid racing
	 * problems associated with multiple threads creating nodes, so that the
	 * same thread may end up with multiple nodes bearing the same ID. This may
	 * have been causing random failures when question generation would produce
	 * sequences not possible in the merged machine, running it multiple times
	 * would sometimes produce such sequences, sometimes not, but each produced
	 * sequence was different from another one (i.e. one failed, another few
	 * passed, next failing one different from the first one, all with the same
	 * transition matrix and even before we consider loops in the blue state).
	 */
	protected int positiveNodeID = 1;

	protected int negativeNodeID = -1;

	public final PTATestSequenceEngine.Node rejectNode = new Node();

	/** Represents elements of the PTA. */
	public class Node {

		/** Constructor for reject nodes. */
		private Node() {
			ID = negativeNodeID;
			fsmState = null;
		}

		/** Constructor for accept nodes. */
		public Node(Object state) {
			if (state == null)
				throw new IllegalArgumentException("state name cannot be null");
			ID = positiveNodeID++;
			fsmState = state;
		}

		public int getID() {
			return ID;
		}

		public boolean isAccept() {
			return ID >= 0;
		}

		public Object getState() {
			return fsmState;
		}

		/**
		 * The ID of this node, positive for accept nodes, negative for reject
		 * ones.
		 */
		private final int ID;

		/** The FSM state this object corresponds. */
		private final Object fsmState;

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			return ID;
		}

		/*
		 * (non-Javadoc)
		 * 
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
			final PTATestSequenceEngine.Node other = (PTATestSequenceEngine.Node) obj;
			if (ID != other.ID)
				return false;
			return true;
		}

		@Override
		public String toString() {
			if (!isAccept())
				return "REJECT";
			else
				return "" + ID + "(" + fsmState + ")";
		}
	}

	/** The initial node of the pta */
	protected PTATestSequenceEngine.Node init = null;

	public interface FSMAbstraction {
		/** the next-state function, returning a map from inputs to next states. */
		public Object getNextState(Object currentState, String input);

		/** returns the initial state */
		public Object getInitState();

		/** returns true if the given state is an accept-state. */
		public boolean isAccept(Object currentState);

		/**
		 * Whether a sequence ending at a given vertex should be returned as a
		 * result of getData().
		 */
		public boolean shouldBeReturned(Object elem);
	}

	public PTATestSequenceEngine() {
	}

	/**
	 * Initialises this PTA engine with an underlying machine. There is no
	 * method to swap a machine for a different one afterwards since states of
	 * the original machine used in PTA directly. Perhaps there has to be a
	 * mechanism to substitute a machine and relabel the existing nodes in a PTA
	 * with those of the new machine, expecting equals on them to define the
	 * expected (total) injection (orig->new).
	 * 
	 * @param machine
	 */
	public void init(FSMAbstraction machine) {
		fsm = machine;
		if (machine.isAccept(machine.getInitState()))
			init = new Node(machine.getInitState());
		else
			init = rejectNode;

		pta.put(init, new LinkedHashMap<String, PTATestSequenceEngine.Node>());
		pta.put(rejectNode,
				new LinkedHashMap<String, PTATestSequenceEngine.Node>());
	}

	/**
	 * Represents a set of sequences using a PTA, backed by an underlying state
	 * machine, passed in at initialization.
	 */
	public class sequenceSet {
		private List<PTATestSequenceEngine.Node> ptaNodes;

		public sequenceSet() {
			ptaNodes = new LinkedList<PTATestSequenceEngine.Node>();
		}

		public void setIdentity() {
			ptaNodes.clear();
			ptaNodes.add(init);
		}

		private PTATestSequenceEngine getEnclosingObject() {
			return PTATestSequenceEngine.this;
		}

		/**
		 * Unites the given set of sequence with the supplied one.
		 * 
		 * @param with
		 *            a sequence set to unite with
		 */
		public void unite(sequenceSet with) {// TODO to test that exception
												// gets thrown.
			if (getEnclosingObject() != with.getEnclosingObject())
				throw new IllegalArgumentException(
						"unite with an argument from a different PTA machine");
			ptaNodes.addAll(with.ptaNodes);
		}

		public sequenceSet crossWithSet(Collection<String> inputs) {
			sequenceSet result = new sequenceSet();

			for (PTATestSequenceEngine.Node node : ptaNodes)
				for (String input : inputs) {
					Node newNode = followToNextNode(node, input);
					if (newNode.isAccept())
						result.ptaNodes.add(newNode);
				}

			return result;
		}

		public sequenceSet cross(Collection<List<String>> inputSequences) {
			sequenceSet result = new sequenceSet();

			for (List<String> inputSequence : inputSequences)
				for (PTATestSequenceEngine.Node node : ptaNodes) {
					PTATestSequenceEngine.Node currentNode = node;
					Iterator<String> seqIt = inputSequence.iterator();
					while (seqIt.hasNext() && currentNode.isAccept())
						currentNode = followToNextNode(currentNode, seqIt
								.next());

					if (currentNode.isAccept())
						result.ptaNodes.add(currentNode);
				}
			return result;
		}

		public sequenceSet crossWithSequence(List<String> inputSequence) {
			sequenceSet result = new sequenceSet();

			for (PTATestSequenceEngine.Node node : ptaNodes) {
				PTATestSequenceEngine.Node currentNode = node;
				Iterator<String> seqIt = inputSequence.iterator();
				while (seqIt.hasNext() && currentNode.isAccept())
					currentNode = followToNextNode(currentNode, seqIt.next());

				if (currentNode.isAccept())
					result.ptaNodes.add(currentNode);
			}

			return result;
		}

		/**
		 * Given a node, this method determines whether that node belongs to
		 * this set of nodes.
		 * 
		 * @param currentVertex
		 *            node to look for
		 * @return true if the supplied node is a part of this set of nodes.
		 */
		boolean contains(Node someNode) {
			return ptaNodes.contains(someNode);
		}

		public String getDebugData() {
			return PTATestSequenceEngine.this.getDebugData(this);
		}
	}

	protected PTATestSequenceEngine.Node followToNextNode(
			PTATestSequenceEngine.Node currentNode, String input) {
		Map<String, PTATestSequenceEngine.Node> row = pta.get(currentNode);
		PTATestSequenceEngine.Node nextCurrentNode = null;

		if (row.containsKey(input))
			nextCurrentNode = row.get(input); // the next node is an accept
												// one
		else {// No transition in the pta with the given input,
			// hence we have to extend the pta by adding a new transition
			Object newState = fsm.getNextState(currentNode.getState(), input);
			if (newState == null || !fsm.isAccept(newState)) {
				row.put(input, rejectNode);// next node is the reject one
				nextCurrentNode = rejectNode;
			} else {
				PTATestSequenceEngine.Node nextNode = new Node(newState);
				row.put(input, nextNode);
				pta.put(nextNode,
						new HashMap<String, PTATestSequenceEngine.Node>());
				nextCurrentNode = nextNode;
			}
		}

		return nextCurrentNode;
	}

	public Collection<List<String>> getData() {
		Collection<List<String>> result = new LinkedList<List<String>>();
		Queue<Node> currentExplorationBoundary = new LinkedList<Node>();// FIFO
																		// queue
		Queue<List<String>> currentExplorationSequence = new LinkedList<List<String>>();// FIFO
																						// queue
		currentExplorationBoundary.add(init);
		currentExplorationSequence.add(new LinkedList<String>());
		while (!currentExplorationBoundary.isEmpty()) {
			Node currentVertex = currentExplorationBoundary.remove();
			List<String> currentSequence = currentExplorationSequence.remove();
			Map<String, Node> row = pta.get(currentVertex);
			if (row.isEmpty()) {
				// the current node is the last on a path, hence we simply add
				// the current sequence to the result
				if (fsm.shouldBeReturned(currentVertex.getState()))
					result.add(currentSequence);
			} else
				for (Entry<String, Node> entry : row.entrySet()) {
					List<String> newSeq = new LinkedList<String>();
					newSeq.addAll(currentSequence);
					newSeq.add(entry.getKey());
					currentExplorationBoundary.offer(entry.getValue());
					currentExplorationSequence.offer(newSeq);
				}
		}

		return result;
	}

	/**
	 * Returns a textual representation of nodes held in the supplied set.
	 * Important: do not change the returned data unless you are prepared to
	 * modify tests relying on it, such as testComputePathsSBetween1.
	 * 
	 * @param targetNodes
	 *            nodes to "display"
	 * @return textual representation of nodes in the set.
	 */
	public String getDebugData(sequenceSet targetNodes) {
		StringBuffer result = new StringBuffer();
		for (Entry<String, String> elem : getDebugDataMap(targetNodes)
				.entrySet()) {
			result.append('[');
			result.append(elem.getKey());
			result.append(']');
			result.append(elem.getValue());
			result.append('\n');
		}
		return result.toString();
	}

	public static enum DebugDataValues {
		LEAF("leaf"), INNER("inner"), sequenceReturned("returned"), sequenceTrashed(
				"trashed");

		private final String text;

		DebugDataValues(String representation) {
			text = representation;
		}

		@Override
		public String toString() {
			return text;
		}

		public static String booleanToString(boolean leaf, boolean returned) {
			StringBuffer result = new StringBuffer();
			if (leaf)
				result.append(DebugDataValues.LEAF);
			else
				result.append(DebugDataValues.INNER);
			result.append(ArrayOperations.separator);
			if (returned)
				result.append(DebugDataValues.sequenceReturned);
			else
				result.append(DebugDataValues.sequenceTrashed);
			return result.toString();
		}
	};

	/**
	 * Returns a representation of nodes held in the supplied set. Important: do
	 * not change the returned data unless you are prepared to modify tests
	 * relying on it, such as testComputePathsSBetween1.
	 * 
	 * @param targetNodes
	 *            nodes to "display"
	 * @return a map from a textual representation of nodes in the set to
	 *         whether this is accept/reject/leaf/inner/returned/trashed node.
	 */
	public Map<String, String> getDebugDataMap(sequenceSet targetNodes) {
		Map<String, String> setToBeReturned = new HashMap<String, String>();
		Queue<Node> currentExplorationBoundary = new LinkedList<Node>();// FIFO
																		// queue
		Queue<List<String>> currentExplorationSequence = new LinkedList<List<String>>();// FIFO
																						// queue
		currentExplorationBoundary.add(init);
		currentExplorationSequence.add(new LinkedList<String>());

		while (!currentExplorationBoundary.isEmpty()) {
			Node currentVertex = currentExplorationBoundary.remove();
			List<String> currentSequence = currentExplorationSequence.remove();
			Map<String, Node> row = pta.get(currentVertex);
			if ((targetNodes == null && row.isEmpty())
					|| (targetNodes != null && targetNodes
							.contains(currentVertex))) {// the current node is
														// the last on a path,
														// hence we simply add
														// the current sequence
														// to the result
				setToBeReturned.put(ArrayOperations
						.seqToString(currentSequence), DebugDataValues
						.booleanToString(row.isEmpty(), fsm
								.shouldBeReturned(currentVertex.getState())));
			}

			if (!row.isEmpty()) // continue exploring if we can
				for (Entry<String, Node> entry : row.entrySet()) {
					List<String> newSeq = new LinkedList<String>();
					newSeq.addAll(currentSequence);
					newSeq.add(entry.getKey());
					currentExplorationBoundary.offer(entry.getValue());
					currentExplorationSequence.offer(newSeq);
				}
		}

		return setToBeReturned;
	}

	public int treeSize() {
		int result = 0;
		Queue<Node> currentExplorationBoundary = new LinkedList<Node>();// FIFO
																		// queue
		currentExplorationBoundary.add(init);
		while (!currentExplorationBoundary.isEmpty()) {
			Node currentVertex = currentExplorationBoundary.remove();
			Map<String, Node> row = pta.get(currentVertex);
			if (!row.isEmpty())
				for (Entry<String, Node> entry : row.entrySet()) {
					result++;
					currentExplorationBoundary.offer(entry.getValue());
				}
		}

		return result;
	}
}
