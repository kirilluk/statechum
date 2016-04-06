/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

package statechum.model.testset;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Map.Entry;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;
import statechum.analysis.learning.PrecisionRecall.PosNegPrecisionRecall;
import statechum.analysis.learning.rpnicore.LearnerGraph;

/** Computes precision and recall by tracing a given pta structure through this machine. 
 * In order to exclude specific paths, it is possible to multiply this one by their pta. 
 */
public class PTA_computePrecisionRecall extends PTA_FSMStructure {

	/** Constructs a tree for the supplied machine, using the given state as an initial state.
	 * 
	 * @param machine machine to use
	 * @param initState the initial state to use
	 */
	public PTA_computePrecisionRecall(LearnerGraph machine, CmpVertex initState) 
	{
		super(machine,initState);
	}

	/** Constructs a tree for the supplied machine.
	 * 
	 * @param machine machine to use
	 */
	public PTA_computePrecisionRecall(LearnerGraph machine) 
	{
		super(machine,null);
	}

	int pos_Ret, pos_Rel, neg_Ret, neg_Rel;
	int resultTN,resultTP, resultFP, resultFN;
	
	protected static class PosNegPrecisionRecallNum extends PosNegPrecisionRecall
	{
// FIXME: strange: if we do not expect any negative (or any positive) sequences, the final precision is always zero.
		public PosNegPrecisionRecallNum(int pos_Ret, int pos_Rel, int pos_relret, int neg_Ret, int neg_Rel, int neg_relret) {
			super(new HashSet<List<Label>>(),new HashSet<List<Label>>(), 
					new HashSet<List<Label>>(), new HashSet<List<Label>>());
			
			negprecision = neg_Ret>0?(double)neg_relret/(double)neg_Ret:0;
			posprecision = pos_Ret>0?(double)pos_relret/(double)pos_Ret:0;
			negrecall = neg_Rel>0?(double)neg_relret/(double)neg_Rel:0;
			posrecall = pos_Rel>0?(double)pos_relret/(double)pos_Rel:0;
			precision = computeMean(negprecision, posprecision);
			recall = computeMean(posrecall, negrecall);
			fMeasure = computeMean(precision, recall);
		}
	}

	private void updateCounters(Node walkNode, Node ptaNode)
	{
		if (!walkNode.isAccept() && ptaNode.getState() == null) // ptaNode.getState() == null means it is a reject node
		{// True Positive
			resultTN++;neg_Rel++;neg_Ret++;
		}
		if (!walkNode.isAccept() && ptaNode.getState() != null)
		{// False Positive
			resultFP++;neg_Rel++;pos_Ret++;
		}
		if (walkNode.isAccept() && ptaNode.getState() == null)
		{// False Negative
			resultFN++;pos_Rel++;neg_Ret++;
		}
		if (walkNode.isAccept() && ptaNode.getState() != null)
		{// True Negative
			resultTP++;pos_Rel++;pos_Ret++;
		}		
	}
	
	/** Computes BCR. */
	public double getBCR()
	{
		double sensitivity = 0.;if (resultTP+resultFN > 0) sensitivity=(double)resultTP/(double)(resultTP+resultFN);
		double specificity = 0.;if (resultTN+resultFP > 0) specificity=(double)resultTN/(double)(resultTN+resultFP);
		return (sensitivity+specificity)/2.;
	}
	
	/** Resets all counters. */
	private void reset()
	{
		pos_Ret = 0;pos_Rel = 0;resultTN = 0;neg_Ret = 0;neg_Rel = 0;resultTP = 0;resultFP=0;resultFN=0;
	}
	
	/** Calculates precision/recall when supplied with a specific set of sequences (such as a test set, backed by an FSM).
	 * 
	 * When we record sequences, non-existing transitions at the ends are recorded too, however if we have a long
	 * non-existing path, only the first non-existing transition is being recorded. This is in compliance with
	 * the training data which only has one non-existing transition at the end of every non-existing path
	 * (by construction, we do not build long paths with multiple non-existing transitions).
	 * 
	 * @param pc the set of sequences.
	 * 
	 */
	public void crossWith(PTASequenceEngine pc)
	{
		reset();
		crossWithNoReset(pc);
	}

	/** Similar to <em>followToNextNode</em> except that instead of a single reject-node, 
	 * we keep adding reject nodes. */
	protected PTASequenceEngine.Node followWithRejectNodes(PTASequenceEngine.Node currentNode, Label input)
	{
		Map<Label,PTASequenceEngine.Node> row = pta.get(currentNode);
		PTASequenceEngine.Node nextCurrentNode = null;

		if (row.containsKey(input))
			nextCurrentNode = row.get(input); // the next node is an accept one
		else
		{// No transition in the pta with the given input, 
		 // hence we have to extend the pta by adding a new transition
			PTASequenceEngine.Node nextNode = null;

			if (currentNode.getState() == null)
				nextNode = new Node();// from a reject-node, we can only enter a reject one
			else
			{
				Object newState = fsm.getNextState(currentNode.getState(), input);
				if (newState == null || !fsm.isAccept(newState)) //TODO here it is assumed that automata are prefix-closed. 
					nextNode = new Node();
				else
					nextNode = new Node(newState);
			}
			row.put(input, nextNode);pta.put(nextNode, new HashMap<Label,PTASequenceEngine.Node>());
			nextCurrentNode = nextNode;
		}
		
		return nextCurrentNode;
	}

	/** Calculates precision/recall when supplied with a specific set of sequences (such as a test set, backed by an FSM).
	 * 
	 * When we record sequences, non-existing transitions at the ends are recorded too, however if we have a long
	 * non-existing path, only the first non-existing transition is being recorded. This is in compliance with
	 * the training data which only has one non-existing transition at the end of every non-existing path
	 * (by construction, we do not build long paths with multiple non-existing transitions).
	 * 
	 * @param walk the set of sequences.
	 * 
	 */
	public void crossWithNoReset(PTASequenceEngine walk)
	{
		Queue<Node> testExplorationBoundary = new LinkedList<Node>(), ptaExplorationBoundary = new LinkedList<Node>();// FIFO queue
		testExplorationBoundary.add(walk.init);ptaExplorationBoundary.add(init);
		
		while(!testExplorationBoundary.isEmpty()) // we explore all of the pc supplied
		{
			Node walkCurrentNode = testExplorationBoundary.remove(), ptaCurrentNode = ptaExplorationBoundary.remove();
			assert ptaCurrentNode != rejectNode;
			Map<Label,Node> walkRow = walk.pta.get(walkCurrentNode), ptaRow = pta.get(ptaCurrentNode);

			for(Entry<Label,Node> walkNextInputAndState:walkRow.entrySet())
			{// for each outgoing transition of a test, we need to do something with the current transition of a machine
				Node nextPtaNode=ptaRow.get(walkNextInputAndState.getKey());
				if (nextPtaNode == null)
				{// Since next PTA node is null, this means that we've not seen this already. This is important where we do a cross
				 // with set including training data which has to be filtered out. In this case, we first do cross with training and
			     // then only new nodes are considered for the purpose of performance evaluation.
				 // Note that we also end up here if the current node is a reject node.

					nextPtaNode = followWithRejectNodes(ptaCurrentNode,walkNextInputAndState.getKey());
					
					// if we reach a reject node of a PTA, it does not mean that we can stop: there could be numerous walks
					// which are not finished yet. Some of them may be accept-walks, others reject-walks, hence we have to follow
					// them through to their ends. The PTA will stay in its rejectNode and we will just unwind walks until they reach
					// their ends (walk.pta.get(walkNextInputAndState.getValue()).isEmpty())
					if (walk.pta.get(walkNextInputAndState.getValue()).isEmpty())
					{// The walk is finished.
						updateCounters(walkNextInputAndState.getValue(),nextPtaNode);
					}
				}
				else
					if (walk.pta.get(walkNextInputAndState.getValue()).isEmpty() && !pta.get(nextPtaNode).isEmpty())
					{// The walk is finished.
						updateCounters(walkNextInputAndState.getValue(),nextPtaNode);
					}

					
				testExplorationBoundary.offer(walkNextInputAndState.getValue());ptaExplorationBoundary.offer(nextPtaNode);
			}
		}
		
	}
	
	/** Converts counters stored in this object to precision/recall values.
	 * 
	 * @return precision/recall values 
	 */
	public PosNegPrecisionRecallNum getPosNegPrecisionRecallNum()
	{
		return new PosNegPrecisionRecallNum(pos_Ret, pos_Rel, resultTP, neg_Ret, neg_Rel, resultTN);
	}
}
