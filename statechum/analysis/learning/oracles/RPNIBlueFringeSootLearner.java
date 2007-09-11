package statechum.analysis.learning.oracles;

import static statechum.analysis.learning.TestRpniLearner.isAccept;

import java.awt.Frame;
import java.io.StringWriter;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

import statechum.JUConstants;
import statechum.analysis.learning.*;

public class RPNIBlueFringeSootLearner extends
		RPNIBlueFringeLearnerTestComponentOpt {
	

	
	public RPNIBlueFringeSootLearner(Frame parentFrame){
		super(parentFrame);
	}
	
	private String getFromMethod(int i, List<String> question){
		List<String> l = new ArrayList<String>();
		l.addAll(question.subList(0, i));
		i--;
		String prev = l.get(i);
		while(prev.equals("ret")){
			i--;
			prev = question.get(i);
		}
		return prev;
	}
	
	public DirectedSparseGraph learnMachine() {
		SootCallGraphOracle oracle = (SootCallGraphOracle)ans;
		Map<Integer, AtomicInteger> whichScoresWereUsedForMerging = new HashMap<Integer,AtomicInteger>(),
			restartScoreDistribution = new HashMap<Integer,AtomicInteger>();
		Map<computeStateScores.PairScore, Integer> scoresToIterations = new HashMap<computeStateScores.PairScore, Integer>();
		Map<computeStateScores.PairScore, Integer> restartsToIterations = new HashMap<computeStateScores.PairScore, Integer>();
		Set<StringPair> impossiblePairs = new HashSet<StringPair>();
		computeStateScores newPTA = scoreComputer;// no need to clone - this is the job of mergeAndDeterminize anyway
		String pairsMerged = "";
		StringWriter report = new StringWriter();
		counterAccepted =0;counterRejected =0;counterRestarted = 0;counterEmptyQuestions = 0;report.write("\n[ PTA: "+scoreComputer.getStatistics(false)+" ] ");
		setChanged();

		Stack<computeStateScores.PairScore> possibleMerges = scoreComputer.chooseStatePairs();
		int plusSize = sPlus.size(), minusSize = sMinus.size(), iterations = 0;
		while(!possibleMerges.isEmpty()){
			iterations++;
			//populateScores(possibleMerges,possibleMergeScoreDistribution);
			computeStateScores.PairScore pair = possibleMerges.pop();
			computeStateScores temp = computeStateScores.mergeAndDeterminize(scoreComputer, pair);
			setChanged();
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();
			if(score <this.certaintyThreshold&&score>minCertaintyThreshold)
			{
				questions = scoreComputer.computeQS(pair, temp);
				if (questions.isEmpty())
					++counterEmptyQuestions;
			} 
			
			boolean restartLearning = false;// whether we need to rebuild a PTA and restart learning.
			
			//System.out.println(Thread.currentThread()+ " "+pair + " "+questions);
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<String> question = questionIt.next();
				Vertex tempVertex = temp.getVertex(question);
				int answer = checkWithEndUser(scoreComputer.getGraph(),question, new Object [] {"Test"});
				this.questionCounter++;
				if(answer>=0){
					String from = oracle.getFrom();
					String to = question.get(answer);
					impossiblePairs.add(new StringPair(from, to));
					//LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer+1));
					//sMinus.add(subAnswer);
					++counterRejected;
					if( (answer < question.size()-1) || isAccept(tempVertex)){
						pairsMerged=pairsMerged+"ABOUT TO RESTART because accept vertex was rejected for a pair "+pair+" ========\n";
						restartLearning = true;break;
					}
				}
				else if(answer == USER_ACCEPTED)
				{
					++counterAccepted;
					newPTA.augmentPTA(question, true);++plusSize;
					//sPlus.add(question);
				}
				else 
						throw new IllegalArgumentException("unexpected user choice");
				
			}
			
			if (restartLearning)
			{// restart learning
				//computeStateScores expected = createAugmentedPTA(sPlus, sMinus);// KIRR: node labelling is done by createAugmentedPTA
				
				scoreComputer = newPTA;// no need to clone - this is the job of mergeAndDeterminize anyway
				Iterator<StringPair> impossiblePairIt = impossiblePairs.iterator();
				Set<List<String>> impossibleStrings = new HashSet<List<String>>();
				while(impossiblePairIt.hasNext()){
					StringPair p = impossiblePairIt.next();
					impossibleStrings.addAll(scoreComputer.getNegativeStrings(p.from , p.to));
				}
				scoreComputer.augmentPTA(impossibleStrings, false);
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
	
	
	
	class StringPair{
		String from, to;
		public StringPair(String a, String b){
			from = a; to = b;
		}
	}
}
