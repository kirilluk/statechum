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

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.Map.Entry;

import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.experiments.PosNegPrecisionRecall;

/**
 * Computes precision and recall by tracing a given pta structure through this
 * machine. In order to exclude specific paths, it is possible to multiply this
 * one by their pta.
 */
public class PTA_computePrecisionRecall extends PTA_FSMStructure {

	public PTA_computePrecisionRecall(FSMStructure machine) {
		super(machine);
	}

	int pos_Ret, pos_Rel, pos_relret, neg_Ret, neg_Rel, neg_relret;

	protected static class PosNegPrecisionRecallNum extends
			PosNegPrecisionRecall {
		// Very strange: if we do not expect any negative (or any positive)
		// sequences, the final precision is always zero.
		public PosNegPrecisionRecallNum(int pos_Ret, int pos_Rel,
				int pos_relret, int neg_Ret, int neg_Rel, int neg_relret) {
			super(new HashSet(), new HashSet(), new HashSet(), new HashSet());

			negprecision = neg_Ret > 0 ? (double) neg_relret / (double) neg_Ret
					: 0;
			posprecision = pos_Ret > 0 ? (double) pos_relret / (double) pos_Ret
					: 0;
			negrecall = neg_Rel > 0 ? (double) neg_relret / (double) neg_Rel
					: 0;
			posrecall = pos_Rel > 0 ? (double) pos_relret / (double) pos_Rel
					: 0;
			precision = computeMean(negprecision, posprecision);
			recall = computeMean(posrecall, negrecall);
			fMeasure = computeMean(precision, recall);
		}
	}

	private void updateCounters(Node currentExplorationNode,
			Node currentLearntNode, PTATestSequenceEngine pc) {
		// System.out.println("matching "+currentExplorationNode+" and
		// "+currentLearntNode);
		/* First, we process a pair of the two nodes being considered. */
		if (currentExplorationNode == pc.rejectNode
				&& currentLearntNode == rejectNode) {
			neg_relret++;
			neg_Rel++;
			neg_Ret++;
		}
		if (currentExplorationNode == pc.rejectNode
				&& currentLearntNode != rejectNode) {
			neg_Rel++;
			pos_Ret++;
		}
		if (currentExplorationNode != pc.rejectNode
				&& currentLearntNode == rejectNode) {
			pos_Rel++;
			neg_Ret++;
		}
		if (currentExplorationNode != pc.rejectNode
				&& currentLearntNode != rejectNode) {
			pos_relret++;
			pos_Rel++;
			pos_Ret++;
		}
	}

	/**
	 * Calculates precision/recall when supplied with a specific set of
	 * sequences (such as a test set, backed by an FSM).
	 * 
	 * When we record sequences, non-existing transitions at the ends are
	 * recorded too, however if we have a long non-existing path, only the first
	 * non-existing transition is being recorded. This is in compliance with the
	 * training data which only has one non-existing transition at the end of
	 * every non-existing path (by construction, we do not build long paths with
	 * multiple non-existing transitions).
	 * 
	 * @param pc
	 *            the set of sequences.
	 * @return precision/recall values
	 */
	public PosNegPrecisionRecall crossWith(PTATestSequenceEngine pc) {
		pos_Ret = 0;
		pos_Rel = 0;
		pos_relret = 0;
		neg_Ret = 0;
		neg_Rel = 0;
		neg_relret = 0;

		Queue<Node> currentExplorationBoundary = new LinkedList<Node>(), currentLearntBoundary = new LinkedList<Node>();// FIFO
																														// queue
		currentExplorationBoundary.add(pc.init);
		currentLearntBoundary.add(init);

		while (!currentExplorationBoundary.isEmpty()) // we explore all of the
														// pc supplied
		{
			Node currentExplorationNode = currentExplorationBoundary.remove(), currentLearntNode = currentLearntBoundary
					.remove();
			Map<String, Node> explorationRow = pc.pta
					.get(currentExplorationNode), learntRow = pta
					.get(currentLearntNode);

			if (!explorationRow.isEmpty())
				for (Entry<String, Node> entry : explorationRow.entrySet()) {// for
																				// each
																				// outgoing
																				// transition
																				// of a
																				// test,
																				// we
																				// need
																				// to
																				// do
																				// something
																				// with
																				// the
																				// current
																				// transition
																				// of a
																				// machine
					Node nextLearntNode = learntRow.get(entry.getKey());
					if (nextLearntNode == null) {// if we've not seen this
													// entry already, proceed
													// (below). Otherwise,
													// ignore.
						// Note that we also end up here if the current node is
						// a reject node.

						if (currentLearntNode != rejectNode)
							// Unlike methods cross..., we do not really have to
							// update a tree here - it is enough to simply
							// track the current state in the learnt automata.
							// However, updating makes it possible to implement
							// precision/recall analysis where I first identify
							// states and subsequently perform analysis
							// of their behaviour compared to the original
							// automaton.
							nextLearntNode = followToNextNode(
									currentLearntNode, entry.getKey());
						else
							nextLearntNode = rejectNode;

						updateCounters(entry.getValue(), nextLearntNode, pc);
					}

					currentExplorationBoundary.offer(entry.getValue());
					currentLearntBoundary.offer(nextLearntNode);
				}
		}

		return new PosNegPrecisionRecallNum(pos_Ret, pos_Rel, pos_relret,
				neg_Ret, neg_Rel, neg_relret);
	}
}
