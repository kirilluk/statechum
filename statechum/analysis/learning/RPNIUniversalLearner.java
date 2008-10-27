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

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LTL_to_ba;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.spin.SpinUtil;
import statechum.model.testset.PTASequenceEngine;

import java.awt.Frame;
import java.util.*;

import javax.swing.JOptionPane;

public class RPNIUniversalLearner extends RPNILearner {

	private Set<String> ltl;

	public RPNIUniversalLearner(Frame parent, Set<String> ltlFormulae, Configuration conf) 
	{
		super(parent, conf);
		ltl = ltlFormulae;
		scoreComputer = new LearnerGraph(conf);
	}

	protected LearnerGraph scoreComputer = null;

	@Override
	public LearnerGraph init(Collection<List<String>> plus, Collection<List<String>> minus)
	{
		scoreComputer.initPTA();
		scoreComputer.paths.augmentPTA(minus, false);
		scoreComputer.paths.augmentPTA(plus, true);
		return scoreComputer;
	}

	@Override
	public LearnerGraph init(PTASequenceEngine en, 
			@SuppressWarnings("unused") int plusSize, @SuppressWarnings("unused") int minusSize)
	{
		scoreComputer.initPTA();
		scoreComputer.paths.augmentPTA(en);

		return scoreComputer;
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
	protected LTL_to_ba constraints = null;
	
	public LearnerGraph AddConstraints(LearnerGraph pta)
	{
		if (constraints == null)
		{
			constraints = new LTL_to_ba(config);
			constraints.ltlToBA(ltl, pta);
		}
		LearnerGraph result = constraints.augmentGraph(pta);
		//Visualiser.updateFrame(result, null);
		return result;
	}
	
	/** Given a pair of graphs, computes the set of questions to validate the merge which 
	 * resulted in the second graph
	 * 
	 * @param original the original graph
	 * @param tempNew the merged graph
	 * @param pair the pair of states merged in the original graph
	 */
	public List<List<String>> ComputeQuestions(PairScore pair, @SuppressWarnings("unused") LearnerGraph original, LearnerGraph tempNew)
	{
		return ComputeQuestions.computeQS(pair, scoreComputer,tempNew);
	}

	public void AugmentPTA(LearnerGraph pta, @SuppressWarnings("unused") RestartLearningEnum ptaKind,
			List<String> sequence, boolean accepted, JUConstants newColour) {
		pta.paths.augmentPTA(sequence, accepted, newColour);
	}

	protected String learntGraphName = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP)+"/beinglearnt";
	
	public LearnerGraph learnMachine()
	{
		final Configuration shallowCopy = scoreComputer.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph ptaHardFacts = scoreComputer.copy(shallowCopy);// this is cloned to eliminate counter-examples added to ptaSoftFacts by Spin
		LearnerGraph ptaSoftFacts = scoreComputer;
		if (config.isUseConstraints()) scoreComputer = topLevelListener.AddConstraints(scoreComputer);
		if (scoreComputer.config.getUseSpin()){
			Collection<List<String>> counters = SpinUtil.check(ptaHardFacts, ltl);
			if(counters.size()>0)
				throw new IllegalArgumentException(getHardFactsContradictionErrorMessage(ltl, counters));
		}
		setChanged();scoreComputer.setName(learntGraphName+"_init");
		Stack<PairScore> possibleMerges = topLevelListener.ChooseStatePairs(scoreComputer);
		int iterations = 0, currentNonAmber = ptaHardFacts.getStateNumber()-ptaHardFacts.getAmberStateNumber();
		JUConstants colourToAugmentWith = scoreComputer.config.getUseAmber()? JUConstants.AMBER:null;
		
		while (!possibleMerges.isEmpty()) 
		{

			iterations++;
			PairScore pair = possibleMerges.pop();
			
			LearnerGraph temp = topLevelListener.MergeAndDeterminize(scoreComputer, pair);
			setChanged();
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();
			RestartLearningEnum restartLearning = RestartLearningEnum.restartNONE;// whether we need to rebuild a PTA
											// and restart learning.

			//Visualiser.updateFrame(scoreComputer.paths.getGraph(learntGraphName+"_"+iterations)
			//updateGraph(temp.paths.getGraph(learntGraphName+"_"+counterRestarted+"_"+iterations));
			updateGraph(temp);
			if (scoreComputer.config.getUseSpin()){

				Collection<List<String>> counterExamples = SpinUtil.check(temp, scoreComputer, ltl);
				Iterator<List<String>> counterExampleIt = counterExamples.iterator();
				while(counterExampleIt.hasNext()){
					List<String> counterExample = counterExampleIt.next();
					topLevelListener.AugmentPTA(ptaSoftFacts, RestartLearningEnum.restartSOFT, counterExample, false,colourToAugmentWith);
					System.out.println("<temp> "+counterExample);
					
				}
				if(counterExamples.size()>0)
					restartLearning = RestartLearningEnum.restartSOFT;
			}
			
			if (shouldAskQuestions(score) && restartLearning == RestartLearningEnum.restartNONE) 
			{
				temp.setName(learntGraphName+"_"+iterations);
				updateGraph(temp);
				questions = topLevelListener.ComputeQuestions(pair, scoreComputer, temp);// all answers are considered "hard", hence we have to ask questions based on hard facts in order to avoid prefixes which are not valid in hard facts
			}
			
			Iterator<List<String>> questionIt = questions.iterator();
			boolean questionAnswered = true;
			List<String> question = null;
			while (questionIt.hasNext() && restartLearning == RestartLearningEnum.restartNONE) 
			{
				if (questionAnswered) question = questionIt.next();// only pick next question if we've got an answer to the previous one
				questionAnswered = false;
				
				boolean accepted = pair.getQ().isAccept();
				Pair<Integer,String> answer = null;
				if (scoreComputer.config.getUseSpin())
					answer = new Pair<Integer,String>(checkWithSPIN(question),null);
				
				boolean answerFromSpin = false;
				if(answer != null && answer.firstElem >= 0) 
					answerFromSpin = true;
				else{
					//System.out.println("<question> "+question);
					answer = topLevelListener.CheckWithEndUser(scoreComputer, question, new Object[] { "LTL"});
				}
				
				if (answer.firstElem == AbstractOracle.USER_CANCELLED) {
					System.out.println("CANCELLED");
					return null;
				}
				
				CmpVertex tempVertex = temp.getVertex(question);

				if (answer.firstElem == AbstractOracle.USER_ACCEPTED) {
					if(!answerFromSpin) // only add to hard facts when obtained directly from a user or from autofile
						topLevelListener.AugmentPTA(ptaHardFacts,RestartLearningEnum.restartHARD,question, true,colourToAugmentWith);
					if (scoreComputer.config.getUseSpin()) topLevelListener.AugmentPTA(ptaSoftFacts,RestartLearningEnum.restartSOFT,question, true,colourToAugmentWith);

					questionAnswered = true;
					if (!tempVertex.isAccept()) 
					{// contradiction with the result of merging
						if(!answerFromSpin)
							restartLearning = RestartLearningEnum.restartHARD;
						else
							restartLearning = RestartLearningEnum.restartSOFT;
						break;
					}
				} else if (answer.firstElem >= 0) {// The sequence has been rejected by a user
					assert answer.firstElem < question.size();
					questionAnswered = true;
					LinkedList<String> subAnswer = new LinkedList<String>();
					subAnswer.addAll(question.subList(0, answer.firstElem + 1));
					if(!answerFromSpin) // only add to hard facts when obtained directly from a user or from autofile
						topLevelListener.AugmentPTA(ptaHardFacts, RestartLearningEnum.restartHARD,subAnswer, false,colourToAugmentWith);
					if (scoreComputer.config.getUseSpin()) topLevelListener.AugmentPTA(ptaSoftFacts,RestartLearningEnum.restartSOFT,subAnswer, false,colourToAugmentWith);
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
						break;
					}
				} else 
					if(answer.firstElem == AbstractOracle.USER_LTL)
					{
						String newLtl = answer.secondElem;
						boolean obtainedLTLViaAuto = newLtl != null;
						if (newLtl == null) newLtl = JOptionPane.showInputDialog("New LTL formula:");
						if(newLtl != null && newLtl.length() != 0)
						{
							if (!obtainedLTLViaAuto) System.out.println(QUESTION_USER+" "+question.toString()+ " <ltl> "+newLtl);
							Set<String> tmpLtl = new HashSet<String>();tmpLtl.addAll(ltl);tmpLtl.add(newLtl);
							Collection<List<String>> counters = SpinUtil.check(ptaHardFacts, tmpLtl);
							if (counters.size()>0)
							{
								if (obtainedLTLViaAuto) // cannot recover from autosetting, otherwise warn a user
									throw new IllegalArgumentException(getHardFactsContradictionErrorMessage(tmpLtl, counters));
								
								System.out.println(getHardFactsContradictionErrorMessage(tmpLtl, counters));
							}
							else 
							{// LTL does not contradict hard facts, update them and restart learning.
								ltl.add(newLtl);
								constraints = null;// make sure constraints are rebuilt if in use
								restartLearning = RestartLearningEnum.restartHARD;
								break;
							}
						}
						// no formula was entered, do not set the <em>questionAnswered</em> to answered, hence 
					    // when we get to the top of the loop, we'll re-pop the previous question.
					}
					else
						throw new IllegalArgumentException("unexpected user choice "+answer);
			}

			if (restartLearning != RestartLearningEnum.restartNONE) {// restart learning
				if (restartLearning == RestartLearningEnum.restartHARD)
				{
					if (config.isSpeculativeQuestionAsking())
						if (speculativeGraphUpdate(possibleMerges, ptaHardFacts))
							return null;// this is the case when a user cancels the learning process when presented by "speculative" questions.
					ptaSoftFacts = ptaHardFacts.copy(shallowCopy);// this is cloned to eliminate counter-examples added to ptaSoftFacts by Spin
				}
				scoreComputer = ptaSoftFacts;// no need to clone - this is the job of mergeAndDeterminize anyway
				if (config.isUseConstraints()) scoreComputer = topLevelListener.AddConstraints(scoreComputer);
				scoreComputer.clearColoursButAmber();// this one will clear all colours if amber mode is not set.

				setChanged();
				topLevelListener.Restart(restartLearning);
			} else {
				// At this point, scoreComputer may have been modified because
				// it may point to
				// the original PTA which will be modified as a result of new
				// sequences being added to it.
				// temp is different too, hence there is no way for me to
				// compute compatibility score here.
				// This is hence computed inside the obtainPair method.

				// keep going with the existing model
				scoreComputer = temp;
				topLevelListener.Restart(RestartLearningEnum.restartNONE);
			}
			possibleMerges = topLevelListener.ChooseStatePairs(scoreComputer);
		}
		
		assert !config.getUseAmber() || currentNonAmber == ptaHardFacts.getStateNumber()-ptaHardFacts.getAmberStateNumber();
		updateGraph(scoreComputer);
		return scoreComputer;
	}

	protected String getHardFactsContradictionErrorMessage(Set<String> tmpLtl, Collection<List<String>> counters)
	{
		String errString = "LTL formula contradicts hard facts\n";
		Iterator<List<String>> counterIt = counters.iterator();
		while(counterIt.hasNext()){
			errString.concat(counterIt.next()+"\n");
		}
		for(String elem:tmpLtl) errString+=elem+"\n";
		return errString;
	}
	
	protected int checkWithSPIN (List<String> question){
		return SpinUtil.check(question, ltl);
	}
	
	/** We might be doing a restart, but it never hurts to go through the existing 
	 * collection of vertices to merge and see if we can update the graph.
	 *  
	 * @return true if question answering has been cancelled by a user.
	 */
	boolean speculativeGraphUpdate(Stack<PairScore> possibleMerges, LearnerGraph newPTA)
	{
		JUConstants colourToAugmentWith = scoreComputer.config.getUseAmber()? JUConstants.AMBER:null;

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
				
				if (tempNew != null) // merge successful - it would fail if our updates to newPTA have modified scoreComputer (the two are often the same graph)
				{					
					for(List<String> question:topLevelListener.ComputeQuestions(pair, newPTA, tempNew))
					{
						Pair<Integer,String> answer = topLevelListener.CheckWithEndUser(scoreComputer,question, new Object [] {"Test"});
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
