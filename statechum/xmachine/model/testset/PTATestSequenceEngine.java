/**
 * 
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

public class PTATestSequenceEngine 
{
	/** The machine used to match all paths supplied against */
	private FSMAbstraction fsm = null;
	
	/** The transition diagram of the pta stored in this object. Each node is an integer, negatives for reject, non-negatives for accept. */
	private final Map<PTATestSequenceEngine.Node,Map<String,PTATestSequenceEngine.Node>> pta = new HashMap<PTATestSequenceEngine.Node,Map<String,PTATestSequenceEngine.Node>>(); 
	
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
	public final PTATestSequenceEngine.Node rejectNode = new Node();

	/** Represents elements of the PTA. */
	public class Node  
	{
		
		/** Constructor for reject nodes. */
		private Node() 
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
			final PTATestSequenceEngine.Node other = (PTATestSequenceEngine.Node) obj;
			if (ID != other.ID)
				return false;
			return true;
		}
		
		@Override
		public String toString()
		{
			if (!isAccept())
				return "REJECT";
			else
				return ""+ID+"("+fsmState+")";
		}
	}
	
	/** The initial node of the pta */
	private PTATestSequenceEngine.Node init = null; 
	
	public interface FSMAbstraction 
	{
		/** the next-state function, returning a map from inputs to next states. */
		public Object getNextState(Object currentState, String input); 
		/** returns the initial state */
		public Object getInitState();
		/** returns true if the given state is an accept-state. */
		public boolean isAccept(Object currentState);
		/** Whether a sequence ending at a given vertex should be returned as a result of getData(). */
		public boolean shouldBeReturned(Object elem);
	}
	
	
	public PTATestSequenceEngine() {}
	
	public void init(FSMAbstraction machine)
	{
		fsm = machine;
		if (machine.isAccept(machine.getInitState()))
			init =  new Node(machine.getInitState());
		else
			init = rejectNode;
		
		pta.put(init,new LinkedHashMap<String,PTATestSequenceEngine.Node>());
		pta.put(rejectNode,new LinkedHashMap<String,PTATestSequenceEngine.Node>());
	}
	
	/** Represents a set of sequences using a PTA. */
	public class sequenceSet 
	{
		private List<PTATestSequenceEngine.Node> ptaNodes;
		
		public sequenceSet()
		{
			ptaNodes = new LinkedList<PTATestSequenceEngine.Node>();
		}
		
		public void setIdentity()
		{
			ptaNodes.clear();ptaNodes.add(init);
		}
		
		public sequenceSet crossWithSet(Collection<String> inputs)
		{
			sequenceSet result = new sequenceSet();
			
			for(PTATestSequenceEngine.Node node:ptaNodes)
				for(String input:inputs)
				{
					Node newNode = followToNextNode(node, input);
					if (newNode.isAccept())
						result.ptaNodes.add(newNode);
				}
			
			return result;
		}
		
		public sequenceSet cross(Collection<List<String>> inputSequences)
		{
			sequenceSet result = new sequenceSet();
			
			for(PTATestSequenceEngine.Node node:ptaNodes)
			{
				for(List<String> inputSequence:inputSequences)
				{
					PTATestSequenceEngine.Node currentNode = node;
					Iterator<String> seqIt = inputSequence.iterator();
					while(seqIt.hasNext() && currentNode.isAccept())
						currentNode=followToNextNode(currentNode, seqIt.next());
					
					if (currentNode.isAccept())
						result.ptaNodes.add(currentNode);
				}
			}
			
			return result;
		}

		private PTATestSequenceEngine.Node followToNextNode(PTATestSequenceEngine.Node currentNode, String input)
		{
			Map<String,PTATestSequenceEngine.Node> row = pta.get(currentNode);
			PTATestSequenceEngine.Node nextCurrentNode = null;

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
					PTATestSequenceEngine.Node nextNode = new Node(newState);
					row.put(input, nextNode);pta.put(nextNode, new HashMap<String,PTATestSequenceEngine.Node>());
					nextCurrentNode = nextNode;
				}
			}
			
			return nextCurrentNode;
		}
	}
	
	public Collection<List<String>> getData()
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
}

