/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
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

package statechum.analysis.learning;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.analysis.learning.spin.SpinResult;
import statechum.analysis.learning.spin.SpinUtil;
import statechum.apps.QSMTool;
import statechum.model.testset.PTASequenceEngine;

import java.awt.Frame;
import java.util.*;

import javax.swing.JOptionPane;

public class RPNIUniversalLearner extends RPNILearner 
{

	private Collection<String> ifthenAutomataAsText;
	
	public RPNIUniversalLearner(Frame parent, LearnerEvaluationConfiguration evalCnf) 
	{
		super(parent, evalCnf.config);
		ifthenAutomataAsText = evalCnf.ifthenSequences;
		if(ifthenAutomataAsText == null)
			ifthenAutomataAsText = new HashSet<String>();
		tentativeAutomaton = new LearnerGraph(evalCnf.config);
	}

	protected LearnerGraph tentativeAutomaton = null;

	@Override
	public LearnerGraph init(Collection<List<String>> plus, Collection<List<String>> minus)
	{// Given that we may have a graph with a single reject-state, we'd like to start by adding
	 // reject-sequences first.
		tentativeAutomaton.initPTA();		
		tentativeAutomaton.paths.augmentPTA(minus, false,false);
		tentativeAutomaton.paths.augmentPTA(plus, true,false);
		return tentativeAutomaton;
	}

	@Override
	public LearnerGraph init(PTASequenceEngine en, 
			@SuppressWarnings("unused") int plusSize, @SuppressWarnings("unused") int minusSize)
	{
		tentativeAutomaton.initPTA();
		tentativeAutomaton.paths.augmentPTA(en);

		return tentativeAutomaton;
	}
	
	/** Returns statistics reflecting the learning. 
	 */
	public String getResult()
	{
		return null;
	}

	/** Identifies a collection of states to merge, sorted in the order of scores. */
	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
	{
		return graph.pairscores.chooseStatePairs();
	}
	
	/** Given a graph, merges a pair of states from it and returns the result. */
	public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair)
	{
		return MergeStates.mergeAndDeterminize_general(original, pair);		
	}

	/** A graph representing constraints to be folded into PTA before learning commences and 
	 * upon every restart.
	 */
	protected LearnerGraph [] ifthenAutomata = null;
	
	public boolean AddConstraints(LearnerGraph pta, LearnerGraph outcome, StringBuffer counterExampleHolder)
	{
		if (ifthenAutomata == null) ifthenAutomata = Transform.buildIfThenAutomata(ifthenAutomataAsText, pta, config).toArray(new LearnerGraph[0]);
		boolean result = true;
		AbstractLearnerGraph.copyGraphs(pta, outcome);
		try {
			Transform.augmentFromIfThenAutomaton(outcome, null, ifthenAutomata, config.getHowManyStatesToAddFromIFTHEN());
		} catch (AugmentFromIfThenAutomatonException e) {
			// merge failed because the constraints disallowed it, hence return a failure
			result = false;e.getFailureLocation(counterExampleHolder);
		}
		return result;
	}
	
	/** Given a pair of graphs, computes the set of questions to validate the merge which 
	 * resulted in the second graph
	 * 
	 * @param original the original graph
	 * @param tempNew the merged graph
	 * @param pair the pair of states merged in the original graph
	 */
	public List<List<String>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph tempNew)
	{
		if (ifthenAutomata == null && config.isUseConstraints()) ifthenAutomata = Transform.buildIfThenAutomata(ifthenAutomataAsText, original, config).toArray(new LearnerGraph[0]);
		return ComputeQuestions.computeQS(pair, tentativeAutomaton,tempNew, ifthenAutomata);
	}

	/** Given a pair of graphs, rebuilds a set of questions to validate the merge which 
	 * resulted in the second graph. This one retains a Pta of questions previously asked
	 * and hence can be used to dynamically update the collection of questions using if-then automata.
	 * 
	 * @param original the original graph
	 * @param temp the merged graph
	 * @param pair the pair of states merged in the original graph
	 */
	public List<List<String>> RecomputeQuestions(PairScore pair,LearnerGraph original, LearnerGraph temp)
	{
		if (ifthenAutomata == null && config.isUseConstraints()) ifthenAutomata = Transform.buildIfThenAutomata(ifthenAutomataAsText, original, config).toArray(new LearnerGraph[0]);
		return ComputeQuestions.RecomputeQS(pair, tentativeAutomaton,temp, ifthenAutomata);
	}
	
	public void AugmentPTA(LearnerGraph pta, @SuppressWarnings("unused") RestartLearningEnum ptaKind,
			List<String> sequence, boolean accepted, JUConstants newColour) {
		pta.paths.augmentPTA(sequence, accepted, false, newColour);
	}

	public void AugumentPTA_and_QuestionPTA(LearnerGraph pta, RestartLearningEnum ptaKind,
			List<String> sequence, boolean accepted, JUConstants newColour)
	{
		topLevelListener.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
		tentativeAutomaton.transform.AugmentNonExistingMatrixWith(sequence, accepted);// rule out a question.
		// Note that since we've attempted to augment our new tentative automaton (right after 
		// merging and reached no contradiction, we can add new paths one-by one here 
		// and expect no contradiction.
	}
	
	protected String learntGraphName = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP)+"/beinglearnt";
	
	public LearnerGraph learnMachine()
	{
		final Configuration shallowCopy = tentativeAutomaton.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph ptaHardFacts = new LearnerGraph(tentativeAutomaton,shallowCopy);// this is cloned to eliminate counter-examples added to ptaSoftFacts by Spin
		LearnerGraph ptaSoftFacts = tentativeAutomaton;
		setChanged();tentativeAutomaton.setName(learntGraphName+"_init");
		
		if (config.isUseConstraints()) 
		{
			LearnerGraph updatedTentativeAutomaton = new LearnerGraph(shallowCopy);
			StringBuffer counterExampleHolder = new StringBuffer();
			if (!topLevelListener.AddConstraints(tentativeAutomaton,updatedTentativeAutomaton,counterExampleHolder))
				throw new IllegalArgumentException(getHardFactsContradictionErrorMessage(ifthenAutomataAsText, counterExampleHolder.toString()));
			tentativeAutomaton = updatedTentativeAutomaton;
		}
		if (tentativeAutomaton.config.getUseLTL() && tentativeAutomaton.config.getUseSpin() && !ifthenAutomataAsText.isEmpty()){
			SpinResult sr = SpinUtil.check(ptaHardFacts, ifthenAutomataAsText);
			if(!sr.isPass())
				throw new IllegalArgumentException(getHardFactsContradictionErrorMessage(ifthenAutomataAsText, sr.getCounters()));
		}
		tentativeAutomaton.merger.buildVertexToEqClassMap(null);// construct the initial version of the 
		// map associating vertices with those these vertices were built from; this map is subsequently 
		// updated when a merged automaton is built.
		
		Stack<PairScore> possibleMerges = topLevelListener.ChooseStatePairs(tentativeAutomaton);
		int iterations = 0, currentNonAmber = ptaHardFacts.getStateNumber()-ptaHardFacts.getAmberStateNumber();
		JUConstants colourToAugmentWith = tentativeAutomaton.config.getUseAmber()? JUConstants.AMBER:null;
		updateGraph(tentativeAutomaton);
		while (!possibleMerges.isEmpty()) 
		{
			iterations++;
			PairScore pair = possibleMerges.pop();

			LearnerGraph temp = topLevelListener.MergeAndDeterminize(tentativeAutomaton, pair);
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();
			RestartLearningEnum restartLearning = RestartLearningEnum.restartNONE;// whether we need to rebuild a PTA
											// and restart learning.

			//updateGraph(temp.paths.getGraph(learntGraphName+"_"+counterRestarted+"_"+iterations));
			if (tentativeAutomaton.config.getUseLTL() && tentativeAutomaton.config.getUseSpin() && !ifthenAutomataAsText.isEmpty()){

				Collection<List<String>> counterExamples = SpinUtil.check(temp, tentativeAutomaton, ifthenAutomataAsText).getCounters();
				Iterator<List<String>> counterExampleIt = counterExamples.iterator();
				while(counterExampleIt.hasNext())
				{
					List<String> counterExample = counterExampleIt.next();
					topLevelListener.AugmentPTA(ptaSoftFacts, RestartLearningEnum.restartSOFT, counterExample, false,colourToAugmentWith);
					System.out.println("<temp> "+counterExample);
					
				}
				if(counterExamples.size()>0)
					restartLearning = RestartLearningEnum.restartSOFT;
			}
			
			if (config.isUseConstraints()) 
			{
				LearnerGraph updatedTentativeAutomaton = new LearnerGraph(shallowCopy);
				StringBuffer counterExampleHolder = new StringBuffer();
				if (!topLevelListener.AddConstraints(temp,updatedTentativeAutomaton,counterExampleHolder))
				{
					tentativeAutomaton.addToCompatibility(pair.firstElem, pair.secondElem, PAIRCOMPATIBILITY.INCOMPATIBLE);
					restartLearning = RestartLearningEnum.restartRECOMPUTEPAIRS;
					System.out.println("<info> pair "+pair+" contradicts constraints, hence recorded as incompatible");
				}
				// since we still need the outcome of merging to ask questions, 
				// we delay actually performing augmentation until the time we are 
				// finished with questions.
			}

			Iterator<List<String>> questionIt = null;
			
			if (restartLearning == RestartLearningEnum.restartNONE && shouldAskQuestions(score)) 
			{
				temp.setName(learntGraphName+"_"+iterations);
				//LearnerGraph updatedGraphActual = ComputeQuestions.constructGraphWithQuestions(pair, tentativeAutomaton, temp);
				//updatedGraphActual.setName("questions "+iterations);setChanged();updateGraph(updatedGraphActual);

				questions = topLevelListener.ComputeQuestions(pair, tentativeAutomaton, temp);// all answers are considered "hard", hence we have to ask questions based on hard facts in order to avoid prefixes which are not valid in hard facts
				questionIt = questions.iterator();
				if (questionIt.hasNext())
				{
					pair.firstElem.setHighlight(true);pair.secondElem.setHighlight(true);
					updateGraph(tentativeAutomaton);pair.firstElem.setHighlight(false);pair.secondElem.setHighlight(false);
				}
			}
			
			while (restartLearning == RestartLearningEnum.restartNONE && questionIt != null && questionIt.hasNext()) 
			{
				List<String> question = questionIt.next();
				
				boolean accepted = pair.getQ().isAccept();
				Pair<Integer,String> answer = null;
				if (tentativeAutomaton.config.getUseLTL() && tentativeAutomaton.config.getUseSpin() && !ifthenAutomataAsText.isEmpty())
					answer = new Pair<Integer,String>(checkWithSPIN(question),null);
				
				CmpVertex tempVertex = temp.getVertex(question);
				boolean answerFromSpin = false;
				if(answer != null && answer.firstElem >= 0) 
					answerFromSpin = true;
				else
				{
					if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.ASSERT)))
						if (ptaHardFacts.paths.tracePath(question) == AbstractOracle.USER_ACCEPTED)
							throw new IllegalArgumentException("question "+ question+ " has already been answered");
					answer = topLevelListener.CheckWithEndUser(tentativeAutomaton, question, tempVertex.isAccept()?AbstractOracle.USER_ACCEPTED:question.size() - 1,ptaHardFacts.paths.tracePath(question), new Object[] { "LTL","IFTHEN","IGNORE QUESTION","MARK AS INCOMPATIBLE"});
				}
				if (answer.firstElem == AbstractOracle.USER_CANCELLED) 
				{
					System.err.println("CANCELLED");
					return null;
				}
				else
				if (answer.firstElem == AbstractOracle.USER_IGNORED)
				{// do nothing
					restartLearning = RestartLearningEnum.restartNONE;
					System.err.println("<ignore> "+question);
				}
				else
				if (answer.firstElem == AbstractOracle.USER_INCOMPATIBLE)
				{
					tentativeAutomaton.addToCompatibility(pair.firstElem, pair.secondElem, PAIRCOMPATIBILITY.INCOMPATIBLE);
					restartLearning = RestartLearningEnum.restartRECOMPUTEPAIRS;
				}
				else
				if (answer.firstElem == AbstractOracle.USER_ACCEPTED) 
				{
					if(!answerFromSpin) // only add to hard facts when obtained directly from a user or from autofile
						AugumentPTA_and_QuestionPTA(ptaHardFacts,RestartLearningEnum.restartHARD,question, true,colourToAugmentWith);
					
					if (tentativeAutomaton.config.getUseLTL() && tentativeAutomaton.config.getUseSpin()) topLevelListener.AugmentPTA(ptaSoftFacts,RestartLearningEnum.restartSOFT,question, true,colourToAugmentWith);

					if (!tempVertex.isAccept()) 
					{// contradiction with the result of merging
						if(!answerFromSpin)
							restartLearning = RestartLearningEnum.restartHARD;
						else
							restartLearning = RestartLearningEnum.restartSOFT;
						
					}
				} else 
				if (answer.firstElem >= 0) 
				{// The sequence has been rejected by a user
					assert answer.firstElem < question.size();
					LinkedList<String> subAnswer = new LinkedList<String>();
					subAnswer.addAll(question.subList(0, answer.firstElem + 1));
					if(!answerFromSpin) // only add to hard facts when obtained directly from a user or from autofile
						AugumentPTA_and_QuestionPTA(ptaHardFacts, RestartLearningEnum.restartHARD,subAnswer, false,colourToAugmentWith);

					if (tentativeAutomaton.config.getUseLTL() && tentativeAutomaton.config.getUseSpin()) topLevelListener.AugmentPTA(ptaSoftFacts,RestartLearningEnum.restartSOFT,subAnswer, false,colourToAugmentWith);
					// important: since vertex IDs is
					// only unique for each instance of ComputeStateScores, only
					// one instance should ever receive calls to augmentPTA

					if ((answer.firstElem < question.size() - 1) || tempVertex.isAccept()) 
					{// contradiction with the result of merging
						assert accepted == true;
						if(!answerFromSpin)
							restartLearning = RestartLearningEnum.restartHARD;
						else
							restartLearning = RestartLearningEnum.restartSOFT;
						
					}
				} 
				else 
					if(answer.firstElem == AbstractOracle.USER_LTL || answer.firstElem == AbstractOracle.USER_IFTHEN)
					{
						String answerType = null;
						if (answer.firstElem == AbstractOracle.USER_LTL)
							answerType = QSMTool.cmdLTL;
						else
							if (answer.firstElem == AbstractOracle.USER_IFTHEN)
								answerType = QSMTool.cmdIFTHENAUTOMATON;
							else
								throw new IllegalArgumentException("unexpected user choice kind "+answer.firstElem);

						restartLearning = RestartLearningEnum.restartRECOMPUTEQUESTIONS;
						String addedConstraint = answer.secondElem;
						boolean obtainedLTLViaAuto = addedConstraint != null;
						if (addedConstraint == null) addedConstraint = JOptionPane.showInputDialog("New "+answerType+" formula:");
						if(addedConstraint != null && addedConstraint.length() != 0)
						{
							if (!obtainedLTLViaAuto) System.out.println(QUESTION_USER+" "+question.toString()+ " <"+answerType+"> "+addedConstraint);
							Set<String> tmpLtl = new HashSet<String>();tmpLtl.addAll(ifthenAutomataAsText);tmpLtl.add(answerType+" "+addedConstraint);
							Collection<List<String>> counters = SpinUtil.check(ptaHardFacts, tmpLtl).getCounters();
							if (counters.size()>0)
							{
								String errorMessage = getHardFactsContradictionErrorMessage(tmpLtl, counters);
								if (obtainedLTLViaAuto) // cannot recover from autosetting, otherwise warn a user
									throw new IllegalArgumentException(errorMessage);
								
								// if not obtained via auto, complain
								System.out.println(errorMessage);
							}
							if (config.isUseConstraints()) 
							{
								LearnerGraph updatedTentativeAutomaton = new LearnerGraph(shallowCopy);
								StringBuffer counterExampleHolder = new StringBuffer();
								if (!topLevelListener.AddConstraints(tentativeAutomaton,updatedTentativeAutomaton,counterExampleHolder))
								{
									String errorMessage = getHardFactsContradictionErrorMessage(ifthenAutomataAsText, counterExampleHolder.toString());
									
									if (obtainedLTLViaAuto) // cannot recover from autosetting, otherwise warn a user
										throw new IllegalArgumentException(errorMessage);
									
									// if not obtained via auto, complain
									System.out.println(errorMessage);
								}
							}

							// the current set of constraints does not contradict hard facts, update them and restart learning.
							ifthenAutomataAsText.add(answerType+" "+addedConstraint);
							ifthenAutomata = null;// make sure constraints are rebuilt if in use
							restartLearning = RestartLearningEnum.restartHARD;
						}
						// no formula was entered, do not set the <em>questionAnswered</em> to answered, hence 
					    // when we get to the top of the loop, we'll re-pop the previous question.
					}
					else
						throw new IllegalArgumentException("unexpected user choice "+answer);
				
				if ( (config.isUseConstraints() && restartLearning == RestartLearningEnum.restartNONE) || restartLearning == RestartLearningEnum.restartRECOMPUTEQUESTIONS)
				{
					questions = topLevelListener.RecomputeQuestions(pair, tentativeAutomaton, temp);// all answers are considered "hard", hence we have to ask questions based on hard facts in order to avoid prefixes which are not valid in hard facts
					questionIt = questions.iterator();restartLearning = RestartLearningEnum.restartNONE;
				}
			} // loop of questions

			if (restartLearning == RestartLearningEnum.restartHARD || 
					restartLearning == RestartLearningEnum.restartSOFT) 
			{// restart learning
				
				if (restartLearning == RestartLearningEnum.restartHARD)
				{
					if (config.isSpeculativeQuestionAsking())
						if (speculativeGraphUpdate(possibleMerges, ptaHardFacts))
							return null;// this is the case when a user cancels the learning process when presented by "speculative" questions.
					AbstractLearnerGraph.copyGraphs(ptaHardFacts,ptaSoftFacts);// this is cloned to eliminate counter-examples added to ptaSoftFacts by Spin
				}
				tentativeAutomaton = ptaSoftFacts;// no need to clone - this is the job of mergeAndDeterminize anyway
				tentativeAutomaton.clearColoursButAmber();// this one will clear all colours if amber mode is not set.

				setChanged();
			} 
			else 
			if (restartLearning == RestartLearningEnum.restartNONE)
			{
				// At this point, tentativeAutomaton may have been modified because
				// it may point to
				// the original PTA which will be modified as a result of new
				// sequences being added to it.
				// temp is different too, hence there is no way for me to
				// compute compatibility score here.
				// This is hence computed inside the obtainPair method.

				// keep going with the existing model
				tentativeAutomaton = temp;
			}
			// if restartLearning == RestartLearningEnum.restartRECOMPUTEPAIRS, we do nothing, i.e. attempt to get state pairs again.

			if (restartLearning != RestartLearningEnum.restartRECOMPUTEPAIRS && config.isUseConstraints()) 
			{// Augmentation from IF-THEN does not use incompatibility constraints in a tentative automaton hence no point in re-augmenting (especially given that I do not have an automaton from before augmentation preserved).
				LearnerGraph updatedTentativeAutomaton = new LearnerGraph(shallowCopy);
				StringBuffer counterExampleHolder = new StringBuffer();
				if (!topLevelListener.AddConstraints(tentativeAutomaton,updatedTentativeAutomaton,counterExampleHolder))
					throw new IllegalArgumentException(getHardFactsContradictionErrorMessage(ifthenAutomataAsText, counterExampleHolder.toString()));
				tentativeAutomaton = updatedTentativeAutomaton;
			}

			topLevelListener.Restart(restartLearning);
			//System.out.println("<info> restart: "+restartLearning);

			possibleMerges = topLevelListener.ChooseStatePairs(tentativeAutomaton);
		}
		
		assert !config.getUseAmber() || currentNonAmber == ptaHardFacts.getStateNumber()-ptaHardFacts.getAmberStateNumber();
		updateGraph(tentativeAutomaton);
		return tentativeAutomaton;
	}

	protected String getHardFactsContradictionErrorMessage(Collection<String> tmpLtl, Collection<List<String>> counters)
	{
		StringBuffer errString = new StringBuffer();
		Iterator<List<String>> counterIt = counters.iterator();
		while(counterIt.hasNext()){
			errString.append(counterIt.next());errString.append('\n');
		}
		for(String elem:tmpLtl) errString.append(elem+"\n");
		return getHardFactsContradictionErrorMessage(tmpLtl,errString.toString());
	}
	
	protected String getHardFactsContradictionErrorMessage(Collection<String> tmpLtl, String counterExample)
	{
		String errString = "LTL formula or IFTHEN automata "+tmpLtl+" contradict hard facts\n"+counterExample;
		return errString;
	}
	
	protected int checkWithSPIN (List<String> question){
		return SpinUtil.check(question, ifthenAutomataAsText);
	}
	
	/** We might be doing a restart, but it never hurts to go through the existing 
	 * collection of vertices to merge and see if we can update the graph.
	 * FIXME: there is no support for LTL/IFTHEN/IGNORE in this method.
	 * @return true if question answering has been cancelled by a user.
	 */
	boolean speculativeGraphUpdate(Stack<PairScore> possibleMerges, LearnerGraph newPTA)
	{
		JUConstants colourToAugmentWith = tentativeAutomaton.config.getUseAmber()? JUConstants.AMBER:null;

		while(!possibleMerges.isEmpty())
		{
			PairScore pair = possibleMerges.pop();
			int score = pair.getScore();

			if(shouldAskQuestions(score))
			{
				LearnerGraph tempNew = null;
				try
				{
					tempNew = topLevelListener.MergeAndDeterminize(newPTA, pair);
				}
				catch(IllegalArgumentException ex)
				{// ignore - tempNew is null anyway					
				}
				
				if (tempNew != null) // merge successful - it would fail if our updates to newPTA have modified tentativeAutomaton (the two are often the same graph)
				{					
					for(List<String> question:topLevelListener.ComputeQuestions(pair, newPTA, tempNew))
					{
						Pair<Integer,String> answer = topLevelListener.CheckWithEndUser(tentativeAutomaton,question, tempNew.getVertex(question).isAccept()?AbstractOracle.USER_ACCEPTED:question.size() - 1,newPTA.paths.tracePath(question),new Object [] {"Test"});
						if (answer.firstElem == AbstractOracle.USER_CANCELLED)
						{
							System.out.println("CANCELLED");
							return true;
						}
						
						if(answer.firstElem == AbstractOracle.USER_ACCEPTED)
						{
							topLevelListener.AugmentPTA(newPTA,RestartLearningEnum.restartHARD,question, true,colourToAugmentWith);
						}
						else 
							if(answer.firstElem >= 0)
							{// The sequence has been rejected by a user
								assert answer.firstElem < question.size();
								LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer.firstElem+1));
								topLevelListener.AugmentPTA(newPTA,RestartLearningEnum.restartHARD,subAnswer, false,colourToAugmentWith);
							}
					}
				}
			}
		}
		
		return false;
	}
}
