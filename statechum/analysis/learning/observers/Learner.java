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

package statechum.analysis.learning.observers;

import java.util.Collection;
import java.util.List;
import java.util.Stack;

import statechum.JUConstants;
import statechum.Pair;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.model.testset.PTASequenceEngine;

public interface Learner 
{
	public enum RestartLearningEnum { restartNONE, restartHARD, restartSOFT };
	
	/** Learns the machine. When different learning events occur, the learner
	 * calls its listeners (which are the decorators); these listeners are given
	 * a chance to inspect the arguments, do something and pass the call 
	 * further or not pass. At the bottom level, there is default processing 
	 * performed by the learner itself.
	 * <p>
	 * For internal use only. 
	 * 
	 * @param topLevelDecorator the first listener in a chain of them.
	 */
	public LearnerGraph learnMachine();
	
	/** Sets the highest listener to receive notification calls; 
	 * it is expected that listeners will propagate calls down the chain
	 * so that this learner eventually gets to execute its own methods.
	 * 
	 * @param top new top of the stack of listeners.
	 */
	public void setTopLevelListener(Learner top);
	
	/** Learns the machine.
	 * 
	 * @param engine positive/negative samples
	 * @param plusSize the number of positives
	 * @param minusSize the number of negatives
	 * @return the learnt automaton.
	 */
	public LearnerGraph learnMachine(PTASequenceEngine engine, int plusSize, int minusSize);

	/** Learns the machine.
	 * 
	 * @param plus positive strings
	 * @param minus negative strings
	 */
	public LearnerGraph learnMachine(Collection<List<String>> plus, Collection<List<String>> minus);
	
	/** Returns statistics reflecting the learning. 
	 */
	public String getResult();
	
	/** Initialises the learner. Used only for testing.
	 * 
	 * @param plus positive strings
	 * @param minus negative strings
	 */
	public LearnerGraph init(Collection<List<String>> plus, Collection<List<String>> minus);
	
	/** Initialises the learner. The value returned is the corresponding graph. 
	 * 
	 * @param engine the collection of positive and negative strings.
	 * @param plusSize the number of positive strings in the collection <em>en</em>.
	 * @param minusSize the number of negative strings in the collection <em>en</em>.
	 */
	public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize);

	/** Identifies a collection of states to merge, sorted in the order of scores. 
	 * <p>
	 * Important: decorators use this one to observe changes to graph. Last time this one
	 * will be called for a learnt automaton. 
	 */
	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph);
	
	/** Given a graph, merges a pair of states from it and returns the result. */
	public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair);
	
	/** Given a pair of graphs, computes the set of questions to validate the merge which 
	 * resulted in the second graph
	 * 
	 * @param original the original graph
	 * @param temp the merged graph
	 * @param pair the pair of states merged in the original graph
	 */
	public List<List<String>> ComputeQuestions(PairScore pair,LearnerGraph original, LearnerGraph temp);
	
	/** Displays a tentative graph and asks user a supplied question. 
	 * Options are to be shown as choices in addition to yes/element_not_accepted. 
	 */
	public Pair<Integer,String> CheckWithEndUser(LearnerGraph graph, List<String> question, Object [] options);
	
	/** Indicates that a restart has taken place.
	 * 
	 * @param mode determines whether this is a hard or a soft restart for 
	 * LTL learner and can be used to denote the extent to which one needs 
	 * to reset the learner. 
	 */
	public void Restart(RestartLearningEnum mode);
	
	/** Used to update the data from which a PTA is built.
	 * 
	 * @param pta what to update
	 * @param ptaKind reflects the kind of database to update.
	 * @param sequence what to add to the database.
	 * @param accepted whether the sequence is accept or reject.
	 * @param newColour
	 */
	public void AugmentPTA(LearnerGraph pta,RestartLearningEnum ptaKind,List<String> sequence, boolean accepted, JUConstants newColour);
}
