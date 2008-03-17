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

package statechum.xmachine.model.testset;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map.Entry;

import statechum.xmachine.model.testset.PTASequenceEngine.Node;

/** Performs a depth-first walk through all nodes in a PTA, calling the user-supplied callback routine. */
public abstract class PTAExploration<USEROBJECT>
{
	protected final PTASequenceEngine engine;
	
	/** In order to implement a recursive routing without using recursion
	 * (Java is not a functional programming language, hence I do not feel
	 * like relying on a fat stack to cope with functional style.
	 */
	public class PTAExplorationNode
	{
		/** whether this is a tail node. */
		private boolean tailNode = false;
		
		public USEROBJECT userObject = null;
		
		/** The current node. */
		final Node ptaNode;
		
		public boolean shouldBeReturned()
		{
			return engine.fsm.shouldBeReturned(ptaNode.getState());			
		}
		
		/** In order to determine whether a node leads to all-reject tail nodes, I need to 
		 * go through all its children, recursively invoking the search. 
		 * This iterator store the current position in the list of children.
		 */
		final Iterator<Entry<String,Node>> entryIter;
		
		protected String inputFromThisNode = null;
		
		public String getInput()
		{
			return inputFromThisNode;
		}
		
		public PTAExplorationNode(Node currNode)
		{
			ptaNode = currNode;userObject = newUserObject();
			entryIter = engine.pta.get(currNode).entrySet().iterator();
			tailNode = !entryIter.hasNext();
		}
		
		@Override
		public String toString()
		{
			return getInput();
		}
		
		/** Returns true if this node is a tail node in a PTA. */
		public boolean isTail()
		{
			return tailNode;
		}
	}
	
	public PTAExploration(PTASequenceEngine en)
	{
		engine=en;
	}
	
	/** Performs a depth-first exploration of the PTA, calling supplied 
	 * (by the virtue of extending this class) callbacks for every node.
	 * In a sense, this is what SAX is doing for XML. 
	 * 
	 * @param whomToCall the callback to receive notifications.
	 */
	public void walkThroughAllPaths()
	{
		LinkedList<PTAExplorationNode> currentExplorationBoundary = new LinkedList<PTAExplorationNode>();// FIFO queue containing vertices to be explored
		PTAExplorationNode currentVert = new PTAExplorationNode(engine.init);
		if (currentVert.isTail())
			leafEntered(currentVert, currentExplorationBoundary);
		else
			nodeEntered(currentVert, currentExplorationBoundary);
		

		while(currentVert != null)
		{// depth-first exploration
			if (currentVert.entryIter.hasNext())
			{// regardless if all paths from this state lead to reject-nodes or some lead to accept-ones, 
				// we have to explore all paths in order to identify all nodes which need pruning.
				currentExplorationBoundary.addFirst( currentVert );
				Entry<String,Node> nextStringNode = currentVert.entryIter.next();
				currentVert.inputFromThisNode=nextStringNode.getKey();
				PTAExplorationNode newNode = new PTAExplorationNode(nextStringNode.getValue());
				currentVert = newNode;
				if (currentVert.isTail())
					leafEntered(currentVert, currentExplorationBoundary);
				else
					nodeEntered(currentVert, currentExplorationBoundary);
			}
			else
			{// either we've gone through all children, or this is a tail node.
				if (!currentVert.isTail())
					nodeLeft(currentVert, currentExplorationBoundary);
				if (currentExplorationBoundary.isEmpty())
					currentVert = null;
				else
					currentVert = currentExplorationBoundary.removeFirst();
			}
		}
	}

	/** Called when every node is entered, before 
	 * children of this node are processed. Not called for tail nodes.
	 * 
	 * @param currentNode the current node
	 * @param pathFromInit the current call stack. Does not include the current element (hence can be empty).
	 */
	public abstract void nodeEntered(PTAExplorationNode currentNode, LinkedList<PTAExplorationNode> pathToInit);

	/** Called when every node is entered, before 
	 * children of this node are processed. Only called for tail nodes.
	 * 
	 * @param currentNode the current node
	 * @param pathFromInit the current call stack. Does not include the current element (hence can be empty).
	 */
	public abstract void leafEntered(PTAExplorationNode currentNode, LinkedList<PTAExplorationNode> pathToInit);

	/** Called after children of this node have been processed. Not called for tail nodes.
	 * 
	 * @param currentNode the current node
	 * @param pathFromInit the current call stack. Does not include the current element (hence can be empty).
	 */
	public abstract void nodeLeft(PTAExplorationNode currentNode, LinkedList<PTAExplorationNode> pathToInit);

	/** Creates a new instance of a user object. */
	public abstract USEROBJECT newUserObject();
}
