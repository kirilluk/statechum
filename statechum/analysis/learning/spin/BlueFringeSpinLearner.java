package statechum.analysis.learning.spin;

import statechum.JUConstants;
import statechum.analysis.learning.*;

import java.awt.Frame;
import java.io.StringWriter;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

public class BlueFringeSpinLearner extends RPNIBlueFringeLearnerTestComponentOpt {
	
	private Set<String> ltl;
	
	public BlueFringeSpinLearner(Frame parentFrame, Set<String> ltlFormulae){
		super(parentFrame);
		ltl = ltlFormulae;
	}
	
	public DirectedSparseGraph learnMachine() {
		Map<Integer, AtomicInteger> whichScoresWereUsedForMerging = new HashMap<Integer,AtomicInteger>(),
			restartScoreDistribution = new HashMap<Integer,AtomicInteger>();
		Map<ComputeStateScores.PairScore, Integer> scoresToIterations = new HashMap<ComputeStateScores.PairScore, Integer>();
		Map<ComputeStateScores.PairScore, Integer> restartsToIterations = new HashMap<ComputeStateScores.PairScore, Integer>();
		ComputeStateScores newPTA = scoreComputer;// no need to clone - this is the job of mergeAndDeterminize anyway
		String pairsMerged = "";
		StringWriter report = new StringWriter();
		counterAccepted =0;counterRejected =0;counterRestarted = 0;counterEmptyQuestions = 0;report.write("\n[ PTA: "+scoreComputer.getStatistics(false)+" ] ");
		setChanged();
		
		Stack<ComputeStateScores.PairScore> possibleMerges = scoreComputer.chooseStatePairs();
		int plusSize = sPlus.size(), minusSize = sMinus.size(), iterations = 0;
		while(!possibleMerges.isEmpty()){
			
			iterations++;
			//populateScores(possibleMerges,possibleMergeScoreDistribution);
			ComputeStateScores.PairScore pair = possibleMerges.pop();
			ComputeStateScores temp = ComputeStateScores.mergeAndDeterminize(scoreComputer, pair);
			setChanged();
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();
			
			boolean restartLearning = false;// whether we need to rebuild a PTA and restart learning.
			
			//System.out.println(Thread.currentThread()+ " "+pair + " "+questions);
			
			if(!SpinUtil.check(temp.getGraph(), ltl)){
				List<String> counterexample = new LinkedList<String>();
				counterexample.addAll(SpinUtil.getCurrentCounterExample());
				newPTA.augmentPTA(counterexample, false);
				
				System.out.println(counterexample);
				++minusSize ;
				restartLearning = true;
			}
			if((score <this.certaintyThreshold&&score>minCertaintyThreshold)&&!restartLearning)
			{
				questions = scoreComputer.computeQS(pair, temp);
				if (questions.isEmpty())
					++counterEmptyQuestions;
			} 
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()&& !restartLearning){
				
				List<String> question = questionIt.next();
				boolean accepted = isAccept(pair.getQ());
				int answer = checkWithEndUser(scoreComputer.getGraph(),question, new Object [] {"Test"});
				this.questionCounter++;
				if (answer == USER_CANCELLED)
				{
					System.out.println("CANCELLED");
					return null;
				}
				
				Vertex tempVertex = temp.getVertex(question);
				if (tempVertex == null)
					System.out.println();
				
				if(answer == USER_ACCEPTED)
				{
					++counterAccepted;
					//sPlus.add(question);
					newPTA.augmentPTA(question, true);++plusSize;
					//System.out.println(setByAuto+question.toString()+ " <yes>");
					
					if(!isAccept(tempVertex))
					{
						pairsMerged=pairsMerged+"ABOUT TO RESTART due to acceptance of a reject vertex for a pair "+pair+" ========\n";
						restartLearning = true;break;
					}
				}
				else 
					if(answer >= 0)
					{// The sequence has been rejected by a user
						assert answer < question.size();
						++counterRejected;
						LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer+1));
						//sMinus.add(subAnswer);
						newPTA.augmentPTA(subAnswer, false);++minusSize ;// important: since vertex IDs is 
						// only unique for each instance of ComputeStateScores, only once 
						// instance should ever receive calls to augmentPTA
						
						//System.out.println(setByAuto+question.toString()+ " <no> at position "+answer+", element "+question.get(answer));
						if( (answer < question.size()-1) || isAccept(tempVertex))
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
				scoreComputer.clearColours();
				setChanged();++counterRestarted;
				pairsMerged=pairsMerged+"========== RESTART "+counterRestarted+" ==========\n";
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
			
			possibleMerges = scoreComputer.chooseStatePairs();
		}
		report.write("\n[ Questions: "+counterAccepted+" accepted "+counterRejected+" rejected resulting in "+counterRestarted+ " restarts; "+counterEmptyQuestions+" empty sets of questions ]\n[ Learned automaton: "+scoreComputer.getStatistics(true)+" ] ");
		report.write("\n[ final sets of questions, plus: "+plusSize+" minus: "+minusSize+" ] ");
		report.write("\n[ Pair scores to iteration numbers:"+pairScoresAndIterations(scoresToIterations,"MERGED-ITERATIONS"));
		report.write("\n[ Restart scores to iteration numbers:"+pairScoresAndIterations(restartsToIterations,"RESTART-ITERATIONS"));
		report.write("\n[ Pairs merged (score-number of times):"+HistogramToSeries(whichScoresWereUsedForMerging,"MERGED"));
		report.write("\n[ Pairs restarted (score-number of times):"+HistogramToSeries(restartScoreDistribution,"RESTARTED"));
		report.write("\n Pair merge details: \n"+pairsMerged);
		DirectedSparseGraph result = scoreComputer.getGraph();result.addUserDatum(JUConstants.STATS, report.toString(), UserData.SHARED);
		updateGraph(result);
		return result;
	}

}
