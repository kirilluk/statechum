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

import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.model.testset.PTASequenceEngine;

public interface Learner 
{
	/** The process of learning can accumulate tentative data such as counter-examples from a model-checker
	 * which have not been explicitly confirmed by a user a true traces. Such data can be useful
	 * to rule out erroneous mergers but cannot be ultimately trusted. For this reason, when we
	 * roll back, it is possible to decide whether to retain such data or not. If traces offered
	 * by a user contradict such a tentative information, we have to discard all of it and restart
	 * with only user input. This is called <em>restartHARD</em>. When we do not discard anything,
	 * we do a <em>restartSOFT</em>. Finally, on occasion it may turn out to be impossible to merge
	 * a pair of states but no sensible counter-example may exist. This is the case when a counter-
	 * example is not prefix-closed; in the absence of explicit information from a user, this is not
	 * possible to determine. If such counter-examples are added to a collection of traces, they become
	 * just such semi-trusted tentative data; an alternative is to mark the pair of state to be 
	 * unmergeable and pick a different pair. This is called <em>restartRECOMPUTEPAIRS</em>.  
	 * 
	 * restartRECOMPUTEQUESTIONS is used in two cases, 
	 * <ul>
	 * <li>when a user felt like adding an LTL constraint but eventually decided not to and</li>
	 * <li>when using constraints -  each question changes our PTA so we need to re-augment in order
	 * to take the new hard fact into account in order to reduce the amount of questions being asked. 
	 * </li></ul>
	 */
	public enum RestartLearningEnum { restartNONE, restartHARD, restartSOFT,restartRECOMPUTEPAIRS, restartRECOMPUTEQUESTIONS }
	
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
	public LearnerGraph learnMachine(Collection<List<Label>> plus, Collection<List<Label>> minus);
	
	/** Returns statistics reflecting the learning. 
	 */
	public String getResult();
	
	/** Initialises the learner. Used only for testing.
	 * 
	 * @param plus positive strings
	 * @param minus negative strings
	 */
	public LearnerGraph init(Collection<List<Label>> plus, Collection<List<Label>> minus);
	
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
	public List<List<Label>> ComputeQuestions(PairScore pair,LearnerGraph original, LearnerGraph temp);
	
	/** Given a pair of graphs, rebuilds a set of questions to validate the merge which 
	 * resulted in the second graph. This one retains a Pta of questions previously asked
	 * and hence can be used to dynamically update the collection of questions using if-then automata.
	 * 
	 * @param original the original graph
	 * @param temp the merged graph
	 * @param pair the pair of states merged in the original graph
	 */
	public List<List<Label>> RecomputeQuestions(PairScore pair,LearnerGraph original, LearnerGraph temp);

	/** Displays a tentative graph and asks user a supplied question. 
	 * Options are to be shown as choices in addition to yes/element_not_accepted.
	 * 
	 *  @param expectedAccept if a response from a user is equals to this value, no restart will be needed. 
	 *  	If set to USER_CANCELLED, this means that no value is provided.
	 *  @param acceptedElements describes which elements of a path are supposed to be accepted/rejected by 
	 *  hard facts. Any inconsistency will cause the IncompatibleStateLabelling to be thrown by AugmentPTA.
	 *  In order to check for this, use <em>PathRoutines.verifyPrefixClosedness()</em> method.
	 *  <br/>
	 *  A value of null means that no check is performed.
	 */
	public Pair<Integer,String> CheckWithEndUser(LearnerGraph graph, List<Label> question, int expectedAccept,List<Boolean> acceptedElements,PairScore pairBeingMerged, Object [] options);
	
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
	public void AugmentPTA(LearnerGraph pta,RestartLearningEnum ptaKind,List<Label> sequence, boolean accepted, JUConstants newColour);
	
	/** Given a PTA, this method adds constraints to it, this could be reject states determined
	 * by Soot or an automaton obtained from LTL.
	 * <p>
	 * @param graph PTA to augment, this one is untouched by transformations. The reason for this is that
	 * one would usually only find out that augmentation went wrong in the process of doing it, hence
	 * the only way to revert back (and not merge the pair leading to the graph to augment) is to
	 * keep the result of augmentation separate from the source. 
	 * @param resultHolder the graph to store the result of augmentation.
	 * @param counterExampleHolder where the counter-example will be stored upon failure.
	 * @return null if the construction was successful,
	 */ 
	public boolean AddConstraints(LearnerGraph graph, LearnerGraph resultHolder, StringBuffer counterExampleHolder);
	
	/** Just about any experiment with large graphs involves interning strings. This is to be centralised
	 * at the learner level rather than at graph level, because if-then expects both if-then automata and the
	 * graphs that are being augmented to work off the same set of labels. It is worth pointing out that GD
	 * avoids this problem completely by making a copy of the graphs (and it will not work with large graphs anyway).
	 */
	public ConvertALabel getLabelConverter();
}
