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

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Map.Entry;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
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
			super(new HashSet<List<String>>(),new HashSet<List<String>>(), 
					new HashSet<List<String>>(), new HashSet<List<String>>());
			
			negprecision = neg_Ret>0?(double)neg_relret/(double)neg_Ret:0;
			posprecision = pos_Ret>0?(double)pos_relret/(double)pos_Ret:0;
			negrecall = neg_Rel>0?(double)neg_relret/(double)neg_Rel:0;
			posrecall = pos_Rel>0?(double)pos_relret/(double)pos_Rel:0;
			precision = computeMean(negprecision, posprecision);
			recall = computeMean(posrecall, negrecall);
			fMeasure = computeMean(precision, recall);
		}
	}

	private void updateCounters(Node testNode, Node ptaNode, Node testNodeReject)
	{
		if (testNode == testNodeReject && ptaNode == rejectNode)
		{// True Positive
			resultTP++;neg_Rel++;neg_Ret++;
		}
		if (testNode == testNodeReject && ptaNode != rejectNode)
		{// False Positive
			resultFP++;neg_Rel++;pos_Ret++;
		}
		if (testNode != testNodeReject && ptaNode == rejectNode)
		{// False Negative
			resultFN++;pos_Rel++;neg_Ret++;
		}
		if (testNode != testNodeReject && ptaNode != rejectNode)
		{// True Negative
			resultTN++;pos_Rel++;pos_Ret++;
		}		
	}
	
	/** Computes BCR. */
	public double getBCR()
	{
		return ( (double)resultTP/(double)(resultTP+resultFN)+(double)resultTN/(double)(resultTN+resultFP) )/2.;
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
		
		Queue<Node> testExplorationBoundary = new LinkedList<Node>(), ptaExplorationBoundary = new LinkedList<Node>();// FIFO queue
		testExplorationBoundary.add(pc.init);ptaExplorationBoundary.add(init);
		
		while(!testExplorationBoundary.isEmpty()) // we explore all of the pc supplied
		{
			Node testCurrentNode = testExplorationBoundary.remove(), ptaCurrentNode = ptaExplorationBoundary.remove();
			Map<String,Node> testRow = pc.pta.get(testCurrentNode), ptaRow = pta.get(ptaCurrentNode);

			if (!testRow.isEmpty())
				for(Entry<String,Node> testNextInput:testRow.entrySet())
				{// for each outgoing transition of a test, we need to do something with the current transition of a machine
					Node nextPtaExplorationNode=ptaRow.get(testNextInput.getKey());
					if (nextPtaExplorationNode == null)
					{// if we've not seen this entry already, proceed (below). Otherwise, ignore.
					 // Note that we also end up here if the current node is a reject node.

						if (ptaCurrentNode != rejectNode) 
							// Unlike methods cross..., we do not really have to update a tree here - it is enough to simply 
							// track the current state in the learnt automata. However, updating makes it possible to filter
							// out training sequences and only keep results for test sequences. 
							nextPtaExplorationNode = followToNextNode(ptaCurrentNode,testNextInput.getKey());
						else
							nextPtaExplorationNode=rejectNode;

						updateCounters(testNextInput.getValue(),nextPtaExplorationNode,pc.rejectNode);
					}
					
					testExplorationBoundary.offer(testNextInput.getValue());ptaExplorationBoundary.offer(nextPtaExplorationNode);
				}
		}
		
	}
	
	/** Converts counters stored in this object to precision/recall values.
	 * 
	 * @return precision/recall values 
	 */
	public PosNegPrecisionRecallNum getPosNegPrecisionRecallNum()
	{
		return new PosNegPrecisionRecallNum(pos_Ret, pos_Rel, (int)resultTN, neg_Ret, neg_Rel, (int)resultTP);
	}
}
