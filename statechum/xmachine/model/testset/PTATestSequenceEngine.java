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

import statechum.analysis.learning.TestFSMAlgo.FSMStructure;

public class PTATestSequenceEngine 
{
	/** The machine used to match all paths supplied against */
	private final FSMStructure fsm;
	
	/** The transition diagram of the pta stored in this object. Each node is an integer, negatives for reject, non-negatives for accept. */
	private final Map<PTATestSequenceEngine.Node,Map<String,PTATestSequenceEngine.Node>> pta = new HashMap<PTATestSequenceEngine.Node,Map<String,PTATestSequenceEngine.Node>>(); 
	
	/** Represents elements of the PTA. */
	public static class Node  
	{
		private static int positiveID = 1;
		private static int negativeID = -1;
		
		/** Constructor for reject nodes. */
		private Node() 
		{ 
			ID = negativeID;fsmState=null;
		}

		/** Constructor for accept nodes. */
		public Node(String state)
		{
			if (state == null)
				throw new IllegalArgumentException("state name cannot be null");
			ID = positiveID++;fsmState = state;
		}
		
		public int getID()
		{
			return ID;
		}
		
		public boolean isAccept()
		{
			return ID >= 0;
		}

		public String getState()
		{
			return fsmState;
		}
		
		public static final PTATestSequenceEngine.Node rejectNode = new Node();
		
		/** The ID of this node, positive for accept nodes, negative for reject ones. */
		private final int ID;
		/** The FSM state this object corresponds. */
		private final String fsmState;

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
	private final PTATestSequenceEngine.Node init; 
	
	public PTATestSequenceEngine(FSMStructure machine)
	{
		fsm = machine;
		if (machine.accept.get(machine.init))
			init =  new Node(machine.init);
		else
			init = Node.rejectNode;
		
		pta.put(init,new LinkedHashMap<String,PTATestSequenceEngine.Node>());
		pta.put(Node.rejectNode,new LinkedHashMap<String,PTATestSequenceEngine.Node>());
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
				String newState = null;
				Map<String,String> fsmRow = fsm.trans.get(currentNode.getState()); 
				if (fsmRow != null)
					newState = fsmRow.get(input);
				if (newState == null || !fsm.accept.get(newState))
				{
					row.put(input, Node.rejectNode);// next node is the reject one
					nextCurrentNode = Node.rejectNode;
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
				// the current node is the last on a path, hence we simply add the current sequence to the result
					result.add(currentSequence);
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

