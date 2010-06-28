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

package statechum.analysis.learning.oracles;

import java.awt.Frame;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.*;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;

public class RPNIBlueFringeSootLearner extends	RPNIUniversalLearner {
	

	
	public RPNIBlueFringeSootLearner(Frame parent, LearnerEvaluationConfiguration cnf){
		super(parent,cnf);
	}
	
	@Override
	public LearnerGraph learnMachine()
	{
		SootCallGraphOracle oracle = new SootCallGraphOracle();
		Map<Integer, AtomicInteger> whichScoresWereUsedForMerging = new HashMap<Integer,AtomicInteger>(),
			restartScoreDistribution = new HashMap<Integer,AtomicInteger>();
		Map<PairScore, Integer> scoresToIterations = new HashMap<PairScore, Integer>();
		Map<PairScore, Integer> restartsToIterations = new HashMap<PairScore, Integer>();
		LearnerGraph newPTA = tentativeAutomaton;// no need to clone - this is the job of mergeAndDeterminize anyway
		setChanged();
		Stack<PairScore> possibleMerges = topLevelListener.ChooseStatePairs(tentativeAutomaton);
		int iterations = 0;
		while(!possibleMerges.isEmpty()){
			iterations++;
			PairScore pair = possibleMerges.pop();
			LearnerGraph temp = topLevelListener.MergeAndDeterminize(tentativeAutomaton, pair);
			setChanged();
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();
			if(shouldAskQuestions(score))
			{
				questions = topLevelListener.ComputeQuestions(pair, tentativeAutomaton, temp);
			} 
			boolean restartLearning = false;
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<String> question = questionIt.next();
				CmpVertex tempVertex = temp.getVertex(question);
				Pair<Integer,String> answer = CheckWithEndUser(tentativeAutomaton,question, temp.getVertex(question).isAccept()?AbstractOracle.USER_ACCEPTED:question.size()-1,
							newPTA.paths.mapPathToConfirmedElements(question), new Object [] {"Test"});
				if(answer.firstElem>=0){
					String from = oracle.getFrom();
					String to = question.get(answer.firstElem);
					newPTA.sootsupport.augmentPairs(new StringPair(from, to), false);
					if( (answer.firstElem < question.size()-1) || tempVertex.isAccept()){
						restartLearning = true;break;
					}
				}
				else if(answer.firstElem == AbstractOracle.USER_ACCEPTED)
				{
					topLevelListener.AugmentPTA(newPTA,RestartLearningEnum.restartHARD,question, true,null);
				}
				else 
						throw new IllegalArgumentException("unexpected user choice");
				
			}
			
			if (restartLearning)
			{
				tentativeAutomaton = newPTA;// no need to clone - this is the job of mergeAndDeterminize anyway
				
				tentativeAutomaton.clearColours();
				setChanged();
				AtomicInteger count = restartScoreDistribution.get(pair.getScore());
				if (count == null)
				{
					count = new AtomicInteger();restartScoreDistribution.put(pair.getScore(),count);
				}
				count.incrementAndGet();
				restartsToIterations.put(pair, iterations);
				iterations = 0;
				topLevelListener.Restart(RestartLearningEnum.restartHARD);
			}
			else
			{
				// At this point, tentativeAutomaton may have been modified because it may point to 
				// the original PTA which will be modified as a result of new sequences being added to it.
				// temp is different too, hence there is no way for me to compute compatibility score here.
				// This is hence computed inside the obtainPair method.
				
				// keep going with the existing model
				tentativeAutomaton = temp;
				// now update the statistics
				AtomicInteger count = whichScoresWereUsedForMerging.get(pair.getScore());
				if (count == null)
				{
					count = new AtomicInteger();whichScoresWereUsedForMerging.put(pair.getScore(),count);
				}
				count.incrementAndGet();
				scoresToIterations.put(pair, iterations);
				topLevelListener.Restart(RestartLearningEnum.restartNONE);
			}
			
			possibleMerges = topLevelListener.ChooseStatePairs(tentativeAutomaton);
		}
		//DirectedSparseGraph result = tentativeAutomaton.paths.getGraph();result.addUserDatum(JUConstants.STATS, report.toString(), UserData.SHARED);
		updateGraph(tentativeAutomaton);
		return tentativeAutomaton;
	}
}
