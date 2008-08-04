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

package statechum.analysis.learning;

import java.util.Collection;
import java.util.List;
import java.util.Stack;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public interface Learner 
{
	public enum RestartLearningEnum { restartNONE, restartHARD, restartSOFT };
	
	/** Learns the machine. When different learning events occur, the learner
	 * calls its listeners (which are the decorators); these listeners are given
	 * a chance to inspect the arguments, do something and pass the call 
	 * further or not pass. At the bottom level, there is default processing 
	 * performed by the learner itself.
	 * 
	 * @param topLevelDecorator the first listener in a chain of them.
	 */
	public DirectedSparseGraph learnMachine(Learner topLevelListener);
	
	/** Learns the machine. */
	public DirectedSparseGraph learnMachine();
	
	/** Returns statistics reflecting the learning. 
	 */
	public String getResult();
	
	/** Initialises the learner.
	 * 
	 * @param plus positive strings
	 * @param minus negative strings
	 */
	public DirectedSparseGraph init(Collection<List<String>> plus, Collection<List<String>> minus);
	
	/** Identifies a collection of states to merge, sorted in the order of scores. */
	public Stack<computeStateScores.PairScore> ChooseStatePairs(computeStateScores graph);
	
	/** Given a graph, merges a pair of states from it and returns the result. */
	public computeStateScores MergeAndDeterminize(computeStateScores original, StatePair pair);
	
	/** Given a pair of graphs, computes the set of questions to validate the merge which 
	 * resulted in the second graph
	 * 
	 * @param original the original graph
	 * @param temp the merged graph
	 * @param pair the pair of states merged in the original graph
	 */
	public Collection<List<String>> ComputeQuestions(computeStateScores original, computeStateScores temp, computeStateScores.PairScore pair);
	
	/** Displays a tentative graph and asks user a supplied question. 
	 * Options are to be shown as choices in addition to yes/element_not_accepted. 
	 */
	public int CheckWithEndUser(computeStateScores graph, List<String> question, Object [] options);
	
	/** Indicates that a restart has taken place.
	 * 
	 * @param mode determines whether this is a hard or a soft restart for 
	 * LTL learner and can be used to denote the extent to which one needs 
	 * to reset the learner. 
	 */
	public void Restart(RestartLearningEnum mode);
}
