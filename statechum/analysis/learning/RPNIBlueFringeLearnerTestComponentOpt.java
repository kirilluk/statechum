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
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import java.beans.XMLEncoder;
import java.io.BufferedOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.StringWriter;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.model.testset.PTASequenceEngine;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

public class RPNIBlueFringeLearnerTestComponentOpt extends RPNIBlueFringeLearner {
	
	public RPNIBlueFringeLearnerTestComponentOpt(Frame parent, Configuration c) {
		super(parent,c);
		scoreComputer = new LearnerGraph(c);
	}
	
	protected void update(StatePair pair)
	{
		pair.getQ().setHighlight(true);
		pair.getR().setHighlight(true);// since this copy of the graph will really not be used, changes to it are immaterial at this stage
	}
	
	protected LearnerGraph scoreComputer = null;

	protected int counterAccepted =0, counterRejected =0, counterRestarted = 0, counterEmptyQuestions = 0;

	/** Takes the candidates for merging and computes the number of times different scores are encountered. */
	public static void populateScores(Collection<PairScore> data, Map<Integer,AtomicInteger> histogram)
	{
		for(PairScore pair:data)
		{
		int pairScore = pair.getScore();
			AtomicInteger count = histogram.get(pairScore);
			if (count == null)
			{
				count = new AtomicInteger();histogram.put(pairScore,count);
			}
			count.incrementAndGet();
		}
	}
	
	/** Takes the candidates for merging and computes the number of times different scores (increments of 10) are encountered. */
	public static void populateHistogram(Collection<PairScore> data, Map<Integer,AtomicInteger> histogram)
	{
		for(PairScore pair:data)
		{
		int pairScore = pair.getScore()>= 200? pair.getScore()-pair.getScore() % 100: pair.getScore()>=10? pair.getScore()-pair.getScore()%10: pair.getScore()>0?1:0;
			AtomicInteger count = histogram.get(pairScore);
			if (count == null)
			{
				count = new AtomicInteger();histogram.put(pairScore,count);
			}
			count.incrementAndGet();
		}
	}
	
	public static String HistogramToString(Map<Integer,AtomicInteger> histogram, String Name)
	{
		final String FS=",";
		String result="\n"+Name;
		Map<Integer, AtomicInteger> tmp = new TreeMap<Integer,AtomicInteger>();
		tmp.putAll(histogram);
		for(Entry<Integer,AtomicInteger> sc:tmp.entrySet())
			result = result+FS+sc.getValue();
		result=result+"\n"+Name;
		for(Entry<Integer,AtomicInteger> sc:tmp.entrySet())
			result = result+FS+sc.getKey();

		return result+"\n";
	}
	
	public static String HistogramToSeries(Map<Integer,AtomicInteger> histogram, String Name)
	{
		final String FS=",";
		String result="\n"+Name;
		Map<Integer, AtomicInteger> tmp = new TreeMap<Integer,AtomicInteger>();
		tmp.putAll(histogram);
		int limit = 0;
		for(Entry<Integer,AtomicInteger> sc:tmp.entrySet()){
			limit = sc.getValue().get();
			for(int i = 0;i<limit;i++){
				result = result+FS+sc.getKey();
			}
		}

		return result+"\n";
	}
	
	public static String pairScoresAndIterations(Map<PairScore,Integer> map, String name){
		final String FS=",";
		String result="\n"+name+"-score"+FS;
		for(PairScore score:map.keySet())
			result=result+score.getScore()+FS;
		result = result+"\n"+name+"-iteration"+FS;
		for(Integer i:map.values())
			result = result+i+FS;
		return result;
	}

	
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

	@Override
	public DirectedSparseGraph learnMachine() {
		setAutoOracle();
		Map<Integer, AtomicInteger> whichScoresWereUsedForMerging = new HashMap<Integer,AtomicInteger>(),
			restartScoreDistribution = new HashMap<Integer,AtomicInteger>();
		Map<PairScore, Integer> scoresToIterations = new HashMap<PairScore, Integer>();
		Map<PairScore, Integer> restartsToIterations = new HashMap<PairScore, Integer>();
		LearnerGraph newPTA = scoreComputer;// no need to clone - this is the job of mergeAndDeterminize anyway
		String pairsMerged = "";
		StringWriter report = new StringWriter();
		counterAccepted =0;counterRejected =0;counterRestarted = 0;counterEmptyQuestions = 0;report.write("\n[ PTA: "+scoreComputer.paths.getStatistics(false)+" ] ");
		setChanged();
		if(config.getDebugMode())
			updateGraph(newPTA.paths.getGraph("merge_debug"+0));

		Stack<PairScore> possibleMerges = scoreComputer.pairscores.chooseStatePairs();
		int plusSize = origPlusSize, minusSize = origMinusSize, iterations = 0;
		while(!possibleMerges.isEmpty()){
			iterations++;
			//populateScores(possibleMerges,possibleMergeScoreDistribution);
			PairScore pair = possibleMerges.pop();
			LearnerGraph temp = MergeStates.mergeAndDeterminize(scoreComputer, pair);
			setChanged();
			if(config.getDebugMode())
				updateGraph(temp.paths.getGraph("merge_debug"+iterations));
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();

			if(shouldAskQuestions(score))
			{
				questions = ComputeQuestions.computeQS(pair, scoreComputer,temp);
				if (questions.isEmpty())
					++counterEmptyQuestions;
			} 

			boolean restartLearning = false;// whether we need to rebuild a PTA and restart learning.
			
			//System.out.println(Thread.currentThread()+ " "+pair + " "+questions);
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<String> question = questionIt.next();
				boolean accepted = pair.getQ().isAccept();
				Pair<Integer,String> answer = checkWithEndUser(scoreComputer.paths.getGraph(),question, new Object [] {"Test"});
				this.questionCounter++;
				if (answer.firstElem == USER_CANCELLED)
				{
					System.out.println("CANCELLED");
					return null;
				}
				
				CmpVertex tempVertex = temp.getVertex(question);
				
				if(answer.firstElem == USER_ACCEPTED)
				{
					++counterAccepted;
					//sPlus.add(question);
					newPTA.paths.augmentPTA(question, true);++plusSize;
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
						++counterRejected;
						LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer.firstElem+1));
						//sMinus.add(subAnswer);
						newPTA.paths.augmentPTA(subAnswer, false);++minusSize ;// important: since vertex IDs are 
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
				scoreComputer.clearColours();
				++counterRestarted;
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
			
			possibleMerges = scoreComputer.pairscores.chooseStatePairs();
		}
		report.write("\n[ Questions: "+counterAccepted+" accepted "+counterRejected+" rejected resulting in "+counterRestarted+ " restarts; "+counterEmptyQuestions+" empty sets of questions ]\n[ Learned automaton: "+scoreComputer.paths.getStatistics(true)+" ] ");
		report.write("\n[ final sets of questions, plus: "+plusSize+" minus: "+minusSize+" ] ");
		report.write("\n[ Pair scores to iteration numbers:"+pairScoresAndIterations(scoresToIterations,"MERGED-ITERATIONS"));
		report.write("\n[ Restart scores to iteration numbers:"+pairScoresAndIterations(restartsToIterations,"RESTART-ITERATIONS"));
		report.write("\n[ Pairs merged (score-number of times):"+HistogramToSeries(whichScoresWereUsedForMerging,"MERGED"));
		report.write("\n[ Pairs restarted (score-number of times):"+HistogramToSeries(restartScoreDistribution,"RESTARTED"));
		report.write("\n Pair merge details: \n"+pairsMerged);
		DirectedSparseGraph result = scoreComputer.paths.getGraph();result.addUserDatum(JUConstants.STATS, report.toString(), UserData.SHARED);
		if(config.getDebugMode())
			updateGraph(result);
		return result;
	}
	
	protected static void dumpSets(String output, Collection<List<String>> sPlus, Collection<List<String>> sMinus)
	{	
		try
		{
			System.out.println("dumping sets");
			XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(new FileOutputStream(output)));
			encoder.writeObject(sPlus);
			encoder.writeObject(sMinus);
			encoder.close();
			throw new IllegalArgumentException("finished");
		}
		catch(FileNotFoundException e)
		{
			IllegalArgumentException ex = new IllegalArgumentException("failed to write output file");
			ex.initCause(e);throw ex;
		}		
	}
}
