package statechum.analysis.learning;

import java.awt.Frame;
import java.io.StringWriter;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicInteger;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.Configuration.QuestionGeneratorKind;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.model.testset.PTASequenceSet;

public class RPNIBlueFringeLinearLearner extends
		RPNIBlueFringeLearnerTestComponentOpt {
	
	
	
	public RPNIBlueFringeLinearLearner(Frame parent, Configuration c) {
		super(parent, c);
	}

	public DirectedSparseGraph learnMachine() {
		setAutoOracle();
		Map<Integer, AtomicInteger> whichScoresWereUsedForMerging = new HashMap<Integer,AtomicInteger>(),
			restartScoreDistribution = new HashMap<Integer,AtomicInteger>();
		Map<PairScore, Integer> scoresToIterations = new HashMap<PairScore, Integer>();
		Map<PairScore, Integer> restartsToIterations = new HashMap<PairScore, Integer>();
		LearnerGraph newPTA = scoreComputer;// no need to clone - this is the job of mergeAndDeterminize anyway
		StringWriter report = new StringWriter();
		counterAccepted =0;counterRejected =0;counterRestarted = 0;counterEmptyQuestions = 0;report.write("\n[ PTA: "+scoreComputer.paths.getStatistics(false)+" ] ");
		setChanged();
		newPTA.setName("merge_debug"+0);
		updateGraph(newPTA);
		
		Stack<PairScore> possibleMerges = scoreComputer.linear.chooseStatePairs(0, 10, 1, null);
		int plusSize = origPlusSize, minusSize = origMinusSize, iterations = 0;
		final int restartOfInterest = -21;
		while(!possibleMerges.isEmpty()&&iterations<20)
		{
			iterations++;
			//populateScores(possibleMerges,possibleMergeScoreDistribution);
			PairScore pair = possibleMerges.pop();
			if (counterRestarted == restartOfInterest) System.out.println("merging "+pair);
			LearnerGraph tempOrig= null;
			LearnerGraph tempNew = null;
			
			//tempOrig = MergeStates.mergeAndDeterminize(scoreComputer, pair);
			tempNew = MergeStates.mergeAndDeterminize_general(scoreComputer, pair);
			LearnerGraph temp=tempNew;
			if (scoreComputer.config.isConsistencyCheckMode())
			{
				tempOrig = MergeStates.mergeAndDeterminize(scoreComputer, pair);
				WMethod.checkM(tempNew, tempOrig);
				MergeStates.verifySameMergeResults(tempOrig, tempNew);
			}
			
			setChanged();temp.setName("merge_debug"+iterations);
			debugAction(temp, iterations);
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();

			if(shouldAskQuestions(score))
			{
				//questions = ArrayOperations.sort(ComputeQuestions.computeQS_origReduced(pair, scoreComputer,tempOrig));
				questions = ComputeQuestions.computeQS(pair, scoreComputer,tempNew);
				if (scoreComputer.config.isConsistencyCheckMode()) 
				{// checking that all the old questions are included in the new ones
					assert scoreComputer.config.getQuestionGenerator() == QuestionGeneratorKind.CONVENTIONAL;
					assert scoreComputer.config.getQuestionPathUnionLimit() < 0;
					
					Collection<List<String>> questionsOrigA = ComputeQuestions.computeQS_orig(pair, scoreComputer,tempOrig);
					//CmpVertex Rnew = tempNew.getVertex(scoreComputer.wmethod.computeShortPathsToAllStates().get(pair.getR()));
					CmpVertex Rnew = tempNew.getStateLearnt();
					assert Rnew == tempNew.getVertex(scoreComputer.wmethod.computeShortPathsToAllStates().get(pair.getR()));
					Collection<List<String>> questionsOrigB = ComputeQuestions.computeQS_orig(new StatePair(Rnew,Rnew), scoreComputer,tempNew);
					PTASequenceSet newQuestions =new PTASequenceSet();newQuestions.addAll(questions);
					assert newQuestions.containsAll(questionsOrigA);
					assert newQuestions.containsAll(questionsOrigB);
				}
				
				if (questions.isEmpty())
					++counterEmptyQuestions;

			} 
			boolean restartLearning = false;// whether we need to rebuild a PTA and restart learning.
			
			//System.out.println(Thread.currentThread()+ " "+pair + " "+questions);
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<String> question = questionIt.next();
				boolean accepted = pair.getQ().isAccept();
				Pair<Integer,String> answer = checkWithEndUser(scoreComputer,question, new Object [] {"Test"});
				this.questionCounter++;
				if (answer.firstElem == AbstractOracle.USER_CANCELLED)
				{
					System.out.println("CANCELLED");
					return null;
				}
				
				CmpVertex tempVertex = temp.getVertex(question);
				
				if(answer.firstElem == AbstractOracle.USER_ACCEPTED)
				{
					++counterAccepted;
					//sPlus.add(question);
					newPTA.paths.augmentPTA(question, true, null);++plusSize;
					if (ans != null) System.out.println(howAnswerWasObtained+question.toString()+ " <yes>");
					if (counterRestarted == restartOfInterest) System.out.println(question.toString()+ " <yes>");
					if(!tempVertex.isAccept())
					{
						restartLearning = true;break;
					}
				}
				else 
					if(answer.firstElem >= 0)
					{// The sequence has been rejected by a user
						assert answer.firstElem < question.size();
						++counterRejected;
						LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer.firstElem+1));
						//sMinus.add(subAnswer);
						newPTA.paths.augmentPTA(subAnswer, false, null);++minusSize ;// important: since vertex IDs are 
						// only unique for each instance of ComputeStateScores, only once 
						// instance should ever receive calls to augmentPTA
						if (ans != null) System.out.println(howAnswerWasObtained+question.toString()+ " <no> at position "+answer.firstElem+", element "+question.get(answer.firstElem));
						if (counterRestarted == restartOfInterest) System.out.println(question.toString()+ " <no> at position "+answer.firstElem+", element "+question.get(answer.firstElem));						
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
				scoreComputer = newPTA;// no need to clone - this is the job of mergeAndDeterminize anyway
				scoreComputer.clearColours();
				++counterRestarted;
				//System.out.println("restarts - "+counterRestarted+" questions: "+(counterAccepted+counterRejected)+" states in PTA: "+newPTA.getStateNumber());
				//dumpPTA(scoreComputer,"/tmp/new_restart"+counterRestarted);
				AtomicInteger count = restartScoreDistribution.get(pair.getScore());
				if (count == null)
				{
					count = new AtomicInteger();restartScoreDistribution.put(pair.getScore(),count);
				}
				count.incrementAndGet();
				restartsToIterations.put(pair, iterations);
				iterations = 0;
			}
			else
			{
				// At this point, scoreComputer may have been modified because it may point to 
				// the original PTA which will be modified as a result of new sequences being added to it.
				// temp is different too, hence there is no way for me to compute compatibility score here.
				// This is hence computed inside the obtainPair method.
				
				// keep going with the existing model
				scoreComputer = temp;
				// now update the statistics
				AtomicInteger count = whichScoresWereUsedForMerging.get(pair.getScore());
				if (count == null)
				{
					count = new AtomicInteger();whichScoresWereUsedForMerging.put(pair.getScore(),count);
				}
				count.incrementAndGet();
				scoresToIterations.put(pair, iterations);
			}
			
			possibleMerges = scoreComputer.linear.chooseStatePairs(0, 10, 1, null);
			//System.out.println(possibleMerges);
		}
		DirectedSparseGraph result = scoreComputer.paths.getGraph();result.addUserDatum(JUConstants.STATS, report.toString(), UserData.SHARED);
		if(config.getDebugMode())
			updateGraph(scoreComputer);
		return result;
	}

}
