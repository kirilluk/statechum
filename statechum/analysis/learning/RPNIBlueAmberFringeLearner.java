package statechum.analysis.learning;

import java.awt.Frame;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.Configuration.QuestionGeneratorKind;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSet;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class RPNIBlueAmberFringeLearner extends RPNIBlueFringeLearner {
	
	public RPNIBlueAmberFringeLearner(Frame parent, Configuration c) {
		super(parent,c);
		scoreComputer = new LearnerGraph(c);
	}
	
	protected LearnerGraph scoreComputer = null;

	
	/** The size of the initial plus/minus sets. */
	protected int origPlusSize, origMinusSize;
	
	@Override
	public void init(Collection<List<String>> plus, Collection<List<String>> minus)
	{
		scoreComputer.initPTA();
		scoreComputer.paths.augmentPTA(minus, false);
		scoreComputer.paths.augmentPTA(plus, true);
		origMinusSize = plus.size();origMinusSize = minus.size();
	}
	
	public void init(PTASequenceEngine en, int plusSize, int minusSize)
	{
		scoreComputer.initPTA();
		scoreComputer.paths.augmentPTA(en);

		origMinusSize = plusSize;origMinusSize = minusSize;
	}

	public String DifferenceBetweenPairOfSets(String prefix, Collection<List<String>> seqOrig,Collection<List<String>> seqNew)
	{
		Set<List<String>> newInQS = new HashSet<List<String>>();newInQS.addAll(seqNew);newInQS.removeAll(seqOrig); 
		Set<List<String>> newInOrig = new HashSet<List<String>>();newInOrig.addAll(seqOrig);newInOrig.removeAll(seqNew);
		return prefix+": new in QS:\n"+newInQS+"\n"+prefix+": new In Orig:\n"+newInOrig;
	}
	
	@Override
	public DirectedSparseGraph learnMachine() {
		setAutoOracle();
		Map<Integer, AtomicInteger> whichScoresWereUsedForMerging = new HashMap<Integer,AtomicInteger>(),
			restartScoreDistribution = new HashMap<Integer,AtomicInteger>();
		Map<PairScore, Integer> scoresToIterations = new HashMap<PairScore, Integer>();
		Map<PairScore, Integer> restartsToIterations = new HashMap<PairScore, Integer>();
		LearnerGraph newPTA = scoreComputer;// no need to clone - this is the job of mergeAndDeterminize anyway
		String pairsMerged = "";
		setChanged();
		newPTA.setName("merge_debug"+0);
		updateGraph(newPTA);
		
		Stack<PairScore> possibleMerges = scoreComputer.pairscores.chooseStatePairs();
		int plusSize = origPlusSize, minusSize = origMinusSize, iterations = 0, currentNonAmber = newPTA.getStateNumber()-newPTA.getAmberStateNumber();
		while(!possibleMerges.isEmpty())
		{
			iterations++;
			//populateScores(possibleMerges,possibleMergeScoreDistribution);
			PairScore pair = possibleMerges.pop();
			LearnerGraph tempOrig= null;
			LearnerGraph tempNew = null;
			//System.out.println("considering "+pair+" non-amber: "+(newPTA.getStateNumber()-newPTA.getAmberStateNumber()));
			int nonAmberA = (newPTA.getStateNumber()-newPTA.getAmberStateNumber());
			tempNew = MergeStates.mergeAndDeterminize_general(scoreComputer, pair);
			LearnerGraph temp=tempNew;
			if ((newPTA.getStateNumber()-newPTA.getAmberStateNumber()) > nonAmberA) System.out.println("added nonamber");
			if (scoreComputer.config.isConsistencyCheckMode())
			{
				tempOrig = MergeStates.mergeAndDeterminize(scoreComputer, pair);
				MergeStates.verifySameMergeResults(tempOrig, tempNew);
			}
			//Visualiser.updateFrame(scoreComputer.paths.getGraph(), temp.paths.getGraph());Visualiser.waitForKey();
			setChanged();temp.setName("merge_debug"+iterations);
			updateGraph(temp);
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();

			if(shouldAskQuestions(score))
			{/*
				PTASequenceEngine engine = new PTASequenceEngine();
				SequenceSet paths=engine.new SequenceSet();paths.setIdentity();
				scoreComputer.paths.computePathsSBetween(pair.getR(), pair.getQ(), paths, engine.new SequenceSet());*/
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
				
			} 
			boolean restartLearning = false;// whether we need to rebuild a PTA and restart learning.
			
			//System.out.println(Thread.currentThread()+ " "+pair + " "+questions);
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<String> question = questionIt.next();
				boolean accepted = pair.getQ().isAccept();
				Pair<Integer,String> answer = checkWithEndUser(scoreComputer,question, new Object [] {"Test"});
				this.questionCounter++;
				if (answer.firstElem == USER_CANCELLED)
				{
					System.out.println("CANCELLED");
					return null;
				}
				
				CmpVertex tempVertex = temp.getVertex(question);
				
				if(answer.firstElem == USER_ACCEPTED)
				{
					//sPlus.add(question);
					newPTA.paths.augmentPTA(question, true,JUConstants.AMBER);++plusSize;
					if (ans != null) System.out.println(howAnswerWasObtained+question.toString()+ " <yes>");
					if(!tempVertex.isAccept())
					{
						pairsMerged=pairsMerged+"ABOUT TO RESTART due to acceptance of a reject vertex for a pair "+pair+" ========\n";
						restartLearning = true;break;
					}
				}
				else 
					if(answer.firstElem >= 0)
					{// The sequence has been rejected by a user
						assert answer.firstElem < question.size();
						LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer.firstElem+1));
						//sMinus.add(subAnswer);
						newPTA.paths.augmentPTA(subAnswer, false,JUConstants.AMBER);++minusSize;
						// important: since vertex IDs are 
						// only unique for each instance of ComputeStateScores, only once 
						// instance should ever receive calls to augmentPTA
						if (ans != null) System.out.println(howAnswerWasObtained+question.toString()+ " <no> at position "+answer.firstElem+", element "+question.get(answer.firstElem));
						if( (answer.firstElem < question.size()-1) || tempVertex.isAccept())
						{
							assert accepted == true;
							pairsMerged=pairsMerged+"ABOUT TO RESTART because accept vertex was rejected for a pair "+pair+" ========\n";
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
				scoreComputer.clearColoursButAmber();
				//System.out.println("restart, currently "+scoreComputer.getStateNumber()+" states, "+(scoreComputer.getStateNumber()-scoreComputer.getAmberStateNumber())+" non-amber");
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
				pairsMerged=pairsMerged+pair+" questions: "+questions.size()+"\n";
				
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
			
			possibleMerges = scoreComputer.pairscores.chooseStatePairs();
		}
		assert currentNonAmber == newPTA.getStateNumber()-newPTA.getAmberStateNumber();
		DirectedSparseGraph result = scoreComputer.paths.getGraph();
		if(config.getDebugMode())
			updateGraph(scoreComputer);
		return result;
	}
}
