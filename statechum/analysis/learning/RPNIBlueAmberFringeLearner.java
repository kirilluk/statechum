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

import java.awt.Frame;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.Configuration.QuestionGeneratorKind;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSet;

public class RPNIBlueAmberFringeLearner extends RPNILearnerInstrumented {
	
	public RPNIBlueAmberFringeLearner(Frame parent, Configuration c) {
		super(parent,c);
		scoreComputer = new LearnerGraph(c);
	}
	
	protected LearnerGraph scoreComputer = null;

	
	/** The size of the initial plus/minus sets. */
	protected int origPlusSize, origMinusSize;
	
	@Override
	public LearnerGraph init(Collection<List<String>> plus, Collection<List<String>> minus)
	{
		scoreComputer.initPTA();
		scoreComputer.paths.augmentPTA(minus, false);
		scoreComputer.paths.augmentPTA(plus, true);
		origMinusSize = plus.size();origMinusSize = minus.size();
		return scoreComputer;
	}
	
	@Override
	public LearnerGraph init(PTASequenceEngine en, int plus, int minus)
	{
		scoreComputer.initPTA();
		scoreComputer.paths.augmentPTA(en);

		origMinusSize = plus;origMinusSize = minus;
		return scoreComputer;
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

	public String DifferenceBetweenPairOfSets(String prefix, Collection<List<String>> seqOrig,Collection<List<String>> seqNew)
	{
		Set<List<String>> newInQS = new HashSet<List<String>>();newInQS.addAll(seqNew);newInQS.removeAll(seqOrig); 
		Set<List<String>> newInOrig = new HashSet<List<String>>();newInOrig.addAll(seqOrig);newInOrig.removeAll(seqNew);
		return prefix+": new in QS:\n"+newInQS+"\n"+prefix+": new In Orig:\n"+newInOrig;
	}
	
	int plusSize = 0, minusSize = 0;
	
	/** Does 
	 * <pre>
	 * MergeStates.mergeAndDeterminize_general(scoreComputer, pair);
	 * </pre>
	 * but additionally checks for consistency.
	 */
	protected LearnerGraph MergeAndDeterminize(LearnerGraph graph,PairScore pair)
	{
		int nonAmberA = 0;
		if (scoreComputer.config.isConsistencyCheckMode()) nonAmberA = graph.getStateNumber()-graph.getAmberStateNumber();
		LearnerGraph tempNew = MergeStates.mergeAndDeterminize_general(scoreComputer, pair);
		if (scoreComputer.config.isConsistencyCheckMode()) assert (graph.getStateNumber()-graph.getAmberStateNumber()) == nonAmberA;

		if (scoreComputer.config.isConsistencyCheckMode())
		{
			LearnerGraph tempOrig = MergeStates.mergeAndDeterminize(scoreComputer, pair);
			assert null == MergeStates.checkM_and_colours(tempOrig, tempNew);
		}
		return tempNew;
	}
	
	/** Returns a collection of questions, but also checks them for consistency.
	 * 
	 * @param tempNew the graph after merge.
	 * @param pair pair of states to consider.
	 * @return questions to ask.
	 */
	public List<List<String>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph tempNew)
	{
		List<List<String>> questions = ComputeQuestions.computeQS(pair, scoreComputer,tempNew);
		if (scoreComputer.config.isConsistencyCheckMode()) 
		{// checking that all the old questions are included in the new ones
			assert scoreComputer.config.getQuestionGenerator() == QuestionGeneratorKind.CONVENTIONAL;
			assert scoreComputer.config.getQuestionPathUnionLimit() < 0;
			
			Collection<List<String>> questionsOrigA = ComputeQuestions.computeQS_orig(pair, scoreComputer,MergeStates.mergeAndDeterminize(scoreComputer, pair));
			CmpVertex Rnew = tempNew.getStateLearnt();
			assert Rnew == tempNew.getVertex(scoreComputer.wmethod.computeShortPathsToAllStates().get(pair.getR()));
			Collection<List<String>> questionsOrigB = ComputeQuestions.computeQS_orig(new StatePair(Rnew,Rnew), scoreComputer,tempNew);
			PTASequenceSet newQuestions =new PTASequenceSet();newQuestions.addAll(questions);
			assert newQuestions.containsAll(questionsOrigA);
			assert newQuestions.containsAll(questionsOrigB);
		}
		
		return questions;
	}
	
	public LearnerGraph learnMachine() 
	{
		setAutoOracle();
		LearnerGraph newPTA = scoreComputer;// no need to clone - this is the job of mergeAndDeterminize anyway
		setChanged();
		newPTA.setName("merge_debug"+0);
		updateGraph(newPTA);
		
		Stack<PairScore> possibleMerges = topLevelListener.ChooseStatePairs(scoreComputer);
		plusSize = origPlusSize;minusSize = origMinusSize;
		int iterations = 0, currentNonAmber = newPTA.getStateNumber()-newPTA.getAmberStateNumber();
		counterRestarted = 0;
		while(!possibleMerges.isEmpty())
		{
			iterations++;
			PairScore pair = possibleMerges.pop();
			LearnerGraph temp = topLevelListener.MergeAndDeterminize(scoreComputer,pair);
			// TODO: this was earlier using newPTA instead of scoreComputer - not sure why it was happily converging.
			
			//System.out.println("considering "+pair+" non-amber: "+(newPTA.getStateNumber()-newPTA.getAmberStateNumber()));
			//Visualiser.updateFrame(scoreComputer.paths.getGraph(), temp.paths.getGraph());Visualiser.waitForKey();
			setChanged();temp.setName("merge_debug"+iterations);
			//updateGraph(temp);
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();
			if(shouldAskQuestions(score))
				questions = topLevelListener.ComputeQuestions(pair, scoreComputer, temp);

			boolean restartLearning = false;// whether we need to rebuild a PTA and restart learning.
			
			//System.out.println(Thread.currentThread()+ " "+pair + " "+questions);
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<String> question = questionIt.next();
				boolean accepted = pair.getQ().isAccept();
				Pair<Integer,String> answer = topLevelListener.CheckWithEndUser(scoreComputer,question, new Object [] {"Test"});
				this.questionCounter++;
				if (answer.firstElem == AbstractOracle.USER_CANCELLED)
				{
					System.out.println("CANCELLED");
					return null;
				}
				
				CmpVertex tempVertex = temp.getVertex(question);
				
				if(answer.firstElem == AbstractOracle.USER_ACCEPTED)
				{
					//sPlus.add(question);
					topLevelListener.AugmentPTA(newPTA,RestartLearningEnum.restartHARD,question, true,JUConstants.AMBER);++plusSize;
					if (ans != null) System.out.println(howAnswerWasObtained+question.toString()+ " <yes>");
					if(!tempVertex.isAccept())
					{
						restartLearning = true;break;
					}
				}
				else 
					if(answer.firstElem >= 0)
					{// The sequence has been rejected by a user
						assert answer.firstElem < question.size();
						LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer.firstElem+1));
						//sMinus.add(subAnswer);
						topLevelListener.AugmentPTA(newPTA,RestartLearningEnum.restartHARD,subAnswer, false,JUConstants.AMBER);++minusSize;
						// important: since vertex IDs are 
						// only unique for each instance of ComputeStateScores, only once 
						// instance should ever receive calls to augmentPTA
						if (ans != null) System.out.println(howAnswerWasObtained+question.toString()+ " <no> at position "+answer.firstElem+", element "+question.get(answer.firstElem));
						if( (answer.firstElem < question.size()-1) || tempVertex.isAccept())
						{
							assert accepted == true;
							restartLearning = true;break;
						}
					}
					else 
						throw new IllegalArgumentException("unexpected user choice");
				
			}
			
			if (restartLearning)
			{// restart learning
				//ComputeStateScores expected = createAugmentedPTA(sPlus, sMinus);// KIRR: node labelling is done by createAugmentedPTA
				//System.out.println("restart at pair "+pair+", currently "+scoreComputer.getStateNumber()+" states, "+(scoreComputer.getStateNumber()-scoreComputer.getAmberStateNumber())+" non-amber");
				if (config.isSpeculativeQuestionAsking())
					if (speculativeGraphUpdate(possibleMerges, newPTA))
						return null;// this is the case when a user cancels the learning process when presented by "speculative" questions.
				
				scoreComputer = newPTA;// no need to clone - this is the job of mergeAndDeterminize anyway
				scoreComputer.clearColoursButAmber();
				//System.out.println("finished with speculative update, currently "+scoreComputer.getStateNumber()+" states, "+(scoreComputer.getStateNumber()-scoreComputer.getAmberStateNumber())+" non-amber");
				iterations = 0;counterRestarted++;
				topLevelListener.Restart(RestartLearningEnum.restartHARD);
			}
			else
			{
				// At this point, scoreComputer may have been modified because it may point to 
				// the original PTA which will be modified as a result of new sequences being added to it.
				// temp is different too, hence there is no way for me to compute compatibility score here.
				// This is hence computed inside the obtainPair method.
				
				// keep going with the existing model
				scoreComputer = temp;
				topLevelListener.Restart(RestartLearningEnum.restartNONE);
			}
			
			possibleMerges = topLevelListener.ChooseStatePairs(scoreComputer);
		}
		assert currentNonAmber == newPTA.getStateNumber()-newPTA.getAmberStateNumber();
		updateGraph(scoreComputer);
		return scoreComputer;
	}
	
	/** We might be doing a restart, but it never hurts to go through the existing 
	 * collection of vertices to merge and see if we can update the graph.
	 *  
	 * @return true if question answering has been cancelled by a user.
	 */
	boolean speculativeGraphUpdate(Stack<PairScore> possibleMerges, LearnerGraph newPTA)
	{
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
						Pair<Integer,String> answer = CheckWithEndUser(scoreComputer,question, new Object [] {"Test"});
						this.questionCounter++;
						if (answer.firstElem == AbstractOracle.USER_CANCELLED)
						{
							System.out.println("CANCELLED");
							return true;
						}
						
						if(answer.firstElem == AbstractOracle.USER_ACCEPTED)
						{
							topLevelListener.AugmentPTA(newPTA,RestartLearningEnum.restartHARD,question, true,JUConstants.AMBER);++plusSize;
						}
						else 
							if(answer.firstElem >= 0)
							{// The sequence has been rejected by a user
								assert answer.firstElem < question.size();
								LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer.firstElem+1));
								topLevelListener.AugmentPTA(newPTA,RestartLearningEnum.restartHARD,subAnswer, false,JUConstants.AMBER);++minusSize;
							}
					}
				}
			}
		}
		
		return false;
	}

	public String getResult() {
		return null;
	}

	public void AugmentPTA(LearnerGraph pta, @SuppressWarnings("unused") RestartLearningEnum ptaKind,
			List<String> sequence, boolean accepted, JUConstants newColour) {
		pta.paths.augmentPTA(sequence, accepted, newColour);
	}
}
