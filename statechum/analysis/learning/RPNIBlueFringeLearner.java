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
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.Map.Entry;
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

public class RPNIBlueFringeLearner extends RPNILearnerInstrumented {
	
	public RPNIBlueFringeLearner(Frame parent, Configuration c) {
		super(parent,c);
		scoreComputer = new LearnerGraph(c);
	}
	
	protected void update(StatePair pair)
	{
		pair.getQ().setHighlight(true);
		pair.getR().setHighlight(true);// since this copy of the graph will really not be used, changes to it are immaterial at this stage
	}
	
	protected LearnerGraph scoreComputer = null;

	protected int counterAccepted =0, counterRejected =0, counterEmptyQuestions = 0;

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
	public LearnerGraph init(Collection<List<String>> plus, Collection<List<String>> minus)
	{
		scoreComputer.initPTA();
		scoreComputer.paths.augmentPTA(minus, false);
		scoreComputer.paths.augmentPTA(plus, true);
		origMinusSize = plus.size();origMinusSize = minus.size();
		return scoreComputer;
	}

	@Override
	public LearnerGraph init(PTASequenceEngine en, int plusSize, int minusSize)
	{
		scoreComputer.initPTA();
		scoreComputer.paths.augmentPTA(en);

		origMinusSize = plusSize;origMinusSize = minusSize;
		return scoreComputer;
	}
	
	public static String DifferenceBetweenPairOfSets(String prefix, Collection<List<String>> seqOrig,Collection<List<String>> seqNew)
	{
		Set<List<String>> newInQS = new HashSet<List<String>>();newInQS.addAll(seqNew);newInQS.removeAll(seqOrig); 
		Set<List<String>> newInOrig = new HashSet<List<String>>();newInOrig.addAll(seqOrig);newInOrig.removeAll(seqNew);
		return prefix+": new in QS:\n"+newInQS+"\n"+prefix+": new In Orig:\n"+newInOrig;
	}
	
	
	/* Note: in order to get the same results from learning as in modified Dec 2007 version 
	 * on the appropriate branch, the following has to be done:
	 * 1. DeterministicDirectedSparseGraph.VertexID.comparisonKind = DeterministicDirectedSparseGraph.VertexID.ComparisonKind.COMPARISON_LEXICOGRAPHIC_ORIG;
	 * 2. load the initial PTA from _mt files (dumped by Dec 2007 version).
	 * 3. merge using tempOrig = MergeStates.mergeAndDeterminize(scoreComputer, pair);
	 * 4. generate questions using questions = ArrayOperations.sort(ComputeQuestions.computeQS_origReduced(pair, scoreComputer,tempOrig));
	 */
	
	private String getName(){
		String name = "machine";
		if(this.origMinusSize>0)
			name = name.concat("neg");
		if(this.config.getAskQuestions())
			name = name.concat("active");
		return name;
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

	/** Given a pair of graphs, computes the set of questions to validate the merge which 
	 * resulted in the second graph
	 * 
	 * @param original the original graph
	 * @param tempNew the merged graph
	 * @param pair the pair of states merged in the original graph
	 */
	public List<List<String>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph tempNew)
	{
		return ComputeQuestions.computeQS(pair, scoreComputer,tempNew);
	}

	public LearnerGraph learnMachine() 
	{
		setAutoOracle();
		Map<Integer, AtomicInteger> whichScoresWereUsedForMerging = new HashMap<Integer,AtomicInteger>(),
			restartScoreDistribution = new HashMap<Integer,AtomicInteger>();
		Map<PairScore, Integer> scoresToIterations = new HashMap<PairScore, Integer>();
		Map<PairScore, Integer> restartsToIterations = new HashMap<PairScore, Integer>();
		LearnerGraph newPTA = scoreComputer;// no need to clone - this is the job of mergeAndDeterminize anyway
		//StringWriter report = new StringWriter();
		//counterAccepted =0;counterRejected =0;counterRestarted = 0;counterEmptyQuestions = 0;report.write("\n[ PTA: "+scoreComputer.paths.getStatistics(false)+" ] ");
		setChanged();
		newPTA.setName(getName()+0);
		updateGraph(newPTA);
		
		Stack<PairScore> possibleMerges = topLevelListener.ChooseStatePairs(scoreComputer);
		int plusSize = origPlusSize, minusSize = origMinusSize,iterations=0;
		final int restartOfInterest = -21;
		while(!possibleMerges.isEmpty())
		{
			iterations++;scoreComputer.setName("current "+iterations);updateGraph(scoreComputer);
			//populateScores(possibleMerges,possibleMergeScoreDistribution);
			PairScore pair = possibleMerges.pop();
			if (counterRestarted == restartOfInterest) System.out.println("merging "+pair);
			LearnerGraph tempOrig= null;
			LearnerGraph tempNew = null;
			
			//tempOrig = MergeStates.mergeAndDeterminize(scoreComputer, pair);
			tempNew = topLevelListener.MergeAndDeterminize(scoreComputer, pair);
			LearnerGraph temp=tempNew;
			if (scoreComputer.config.isConsistencyCheckMode())
			{// TODO: to do this via decorators
				tempOrig = MergeStates.mergeAndDeterminize(scoreComputer, pair);
				WMethod.checkM(tempNew, tempOrig);
				MergeStates.verifySameMergeResults(tempOrig, tempNew);
			}
			
			setChanged();temp.setName(getName()+iterations);
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();

			if(shouldAskQuestions(score))
			{
				//questions = ArrayOperations.sort(ComputeQuestions.computeQS_origReduced(pair, scoreComputer,tempOrig));
				questions = topLevelListener.ComputeQuestions(pair, scoreComputer,tempNew);
				if (scoreComputer.config.isConsistencyCheckMode()) 
				{// checking that all the old questions are included in the new ones
					// TODO: to do this via decorators
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
			
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext())
			{
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
					++counterAccepted;
					//sPlus.add(question);
					topLevelListener.AugmentPTA(newPTA, RestartLearningEnum.restartHARD, question, true, null);
					++plusSize;
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
						topLevelListener.AugmentPTA(newPTA, RestartLearningEnum.restartHARD, subAnswer, false, null);
						++minusSize ;// important: since vertex IDs are 
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
				topLevelListener.Restart(RestartLearningEnum.restartNONE);
			}
			
			possibleMerges = topLevelListener.ChooseStatePairs(scoreComputer);
		}
		updateGraph(scoreComputer);
		return scoreComputer;
	}

	public void AugmentPTA(LearnerGraph pta, @SuppressWarnings("unused") RestartLearningEnum ptaKind,
			List<String> sequence, boolean accepted, JUConstants newColour) {
		pta.paths.augmentPTA(sequence, accepted, newColour);
	}

}
