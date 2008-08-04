package statechum.analysis.learning;

import java.awt.Frame;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import java.io.StringWriter;

import statechum.JUConstants;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

import static statechum.analysis.learning.TestRpniLearner.isAccept;

public class RPNIBlueFringeLearnerTestComponentOpt extends
		RPNIBlueFringeLearnerTestComponent implements Learner
{

	public RPNIBlueFringeLearnerTestComponentOpt(Frame parent) {
		super(parent);
	}
	
	protected void update(StatePair pair)
	{
		pair.getQ().setUserDatum("pair", pair, UserData.SHARED);
		pair.getR().setUserDatum("pair", pair, UserData.SHARED);// since this copy of the graph will really not be used, changes to it are immaterial at this stage
		updateGraph(scoreComputer.getGraph());
	}
	
	//protected computeStateScores scoreComputer = new computeStateScores(0);

	protected int counterAccepted =0, counterRejected =0, counterRestarted = 0, counterEmptyQuestions = 0;

	/** Takes the candidates for merging and computes the number of times different scores are encountered. */
	public static void populateScores(Collection<computeStateScores.PairScore> data, Map<Integer,AtomicInteger> histogram)
	{
		for(computeStateScores.PairScore pair:data)
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
	public static void populateHistogram(Collection<computeStateScores.PairScore> data, Map<Integer,AtomicInteger> histogram)
	{
		for(computeStateScores.PairScore pair:data)
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
	
	public static String pairScoresAndIterations(Map<computeStateScores.PairScore,Integer> map, String name){
		final String FS=",";
		String result="\n"+name+"-score"+FS;
		for(computeStateScores.PairScore score:map.keySet())
			result=result+score.getScore()+FS;
		result = result+"\n"+name+"-iteration"+FS;
		for(Integer i:map.values())
			result = result+i+FS;
		return result;
	}

	public DirectedSparseGraph init(Collection<List<String>> plus, Collection<List<String>> minus)
	{
		sPlus = plus;sMinus = minus;
		scoreComputer.initPTA();
		scoreComputer.augmentPTA(sMinus, false);
		scoreComputer.augmentPTA(sPlus, true);
		return scoreComputer.getGraph();
	}
	
	public computeStateScores getScoreComputer()
	{
		return scoreComputer;
	}
	
	@Override
	public DirectedSparseGraph learnMachine(
			@SuppressWarnings("unused")	DirectedSparseGraph model, 
			@SuppressWarnings("unused") Collection<List<String>> sP, 
			@SuppressWarnings("unused")	Collection<List<String>> sM) {
		//init(sPlus, sMinus);
		//return learnMachine();
		throw new UnsupportedOperationException();
	}

	public String getStats()
	{
		return counterRestarted+ ", "+ scoreComputer.getExtentOfCompleteness();
	}

	/** Returns statistics reflecting the learning. 
	 */
	public String getResult()
	{
		return null;
	}

	/** Identifies a collection of states to merge, sorted in the order of scores. */
	public Stack<computeStateScores.PairScore> ChooseStatePairs(computeStateScores graph)
	{
		return graph.chooseStatePairs();
	}
	
	/** Given a graph, merges a pair of states from it and returns the result. */
	public computeStateScores MergeAndDeterminize(computeStateScores original, StatePair pair)
	{
		return computeStateScores.mergeAndDeterminize(original, pair);		
	}

	/** Given a pair of graphs, computes the set of questions to validate the merge which 
	 * resulted in the second graph
	 * 
	 * @param original the original graph
	 * @param temp the merged graph
	 * @param pair the pair of states merged in the original graph
	 */
	public Collection<List<String>> ComputeQuestions(computeStateScores original, computeStateScores temp, computeStateScores.PairScore pair)
	{
		Collection<List<String>> questions = new LinkedList<List<String>>();
		int score = pair.getScore();
		if(score <this.certaintyThreshold&&score>minCertaintyThreshold)
		{
			questions = sort(original.computeQS(pair, temp));
			if (questions.isEmpty())
				++counterEmptyQuestions;
		} 
		return questions;
	}

	/** Displays a tentative graph and asks user a supplied question. 
	 * Options are to be shown as choices in addition to yes/element_not_accepted. 
	 */
	public int CheckWithEndUser(computeStateScores graph, List<String> question, Object [] options)
	{
		return checkWithEndUser(graph.getGraph(),question, options);
	}

	public void Restart(@SuppressWarnings("unused") RestartLearningEnum mode) 
	{
	}
	
	protected computeStateScores scoreComputer = new computeStateScores(0);
	
	public DirectedSparseGraph learnMachine() 
	{
		return learnMachine(this);
	}
	
	public DirectedSparseGraph learnMachine(Learner topLevelListener) 
	{
		Map<Integer, AtomicInteger> whichScoresWereUsedForMerging = new HashMap<Integer,AtomicInteger>(),
			restartScoreDistribution = new HashMap<Integer,AtomicInteger>();
		Map<computeStateScores.PairScore, Integer> scoresToIterations = new HashMap<computeStateScores.PairScore, Integer>();
		Map<computeStateScores.PairScore, Integer> restartsToIterations = new HashMap<computeStateScores.PairScore, Integer>();
		computeStateScores newPTA = scoreComputer;// no need to clone - this is the job of mergeAndDeterminize anyway
		String pairsMerged = "";
		StringWriter report = new StringWriter();
		counterAccepted =0;counterRejected =0;counterRestarted = 0;counterEmptyQuestions = 0;report.write("\n[ PTA: "+scoreComputer.getStatistics(false)+" ] ");

		setChanged();
		/*dumpPTA(scoreComputer, "/tmp/initial_pta.xml");
		 */
		Stack<computeStateScores.PairScore> possibleMerges = topLevelListener.ChooseStatePairs(scoreComputer);
		int plusSize = sPlus.size(), minusSize = sMinus.size(), iterations = 0;
		final int restartOfInterest = -21;
		
		while(!possibleMerges.isEmpty()){
			iterations++;
			//populateScores(possibleMerges,possibleMergeScoreDistribution);
			computeStateScores.PairScore pair = possibleMerges.pop();
			if (counterRestarted == restartOfInterest) System.out.println("merging "+pair);
			computeStateScores temp = topLevelListener.MergeAndDeterminize(scoreComputer,pair);
			setChanged();
			Collection<List<String>> questions = topLevelListener.ComputeQuestions(scoreComputer,temp,pair);
			
			boolean restartLearning = false;// whether we need to rebuild a PTA and restart learning.

			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<String> question = questionIt.next();
				String accepted = pair.getQ().getUserDatum(JUConstants.ACCEPTED).toString();
				int answer = topLevelListener.CheckWithEndUser(scoreComputer,question,new Object[] {"Test"});
				
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
					if (counterRestarted == restartOfInterest) System.out.println(question.toString()+ " <yes>");
					
					if(!TestRpniLearner.isAccept(tempVertex))
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
						// only unique for each instance of computeStateScores, only once 
						// instance should ever receive calls to augmentPTA
						if (counterRestarted == restartOfInterest) System.out.println(question.toString()+ " <no> at position "+answer+", element "+question.get(answer));
						//System.out.println(setByAuto+question.toString()+ " <no> at position "+answer+", element "+question.get(answer));
						if( (answer < question.size()-1) || isAccept(tempVertex))
						{
							assert accepted.equals("true");
							pairsMerged=pairsMerged+"ABOUT TO RESTART because accept vertex was rejected for a pair "+pair+" ========\n";
							restartLearning = true;break;
						}
					}
					else 
						throw new IllegalArgumentException("unexpected user choice");
				
			}

			if (restartLearning)
			{// restart learning
				//computeStateScores expected = createAugmentedPTA(sPlus, sMinus);// KIRR: node labelling is done by createAugmentedPTA
				scoreComputer = newPTA;// no need to clone - this is the job of mergeAndDeterminize anyway
				scoreComputer.clearColours();
				setChanged();++counterRestarted;
				//System.out.println("restarts - "+counterRestarted+" questions: "+(counterAccepted+counterRejected)+" states in PTA: "+newPTA.getStateNumber());
				//dumpPTA(scoreComputer,"/tmp/orig_restart"+counterRestarted);
				pairsMerged=pairsMerged+"========== RESTART "+counterRestarted+" ==========\n";
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
				topLevelListener.Restart(RestartLearningEnum.restartNONE);
			}
			
			possibleMerges = topLevelListener.ChooseStatePairs(scoreComputer);
			//System.out.println(possibleMerges);
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
/*	
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
*/
	@Override
	protected List<List<String>> generateQuestions(
			@SuppressWarnings("unused") DirectedSparseGraph model,
			@SuppressWarnings("unused") DirectedSparseGraph temp, 
			@SuppressWarnings("unused")	StatePair pair) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected Stack chooseStatePairs(
			@SuppressWarnings("unused")	DirectedSparseGraph g,
			@SuppressWarnings("unused")	Collection<List<String>> plus, 
			@SuppressWarnings("unused")	Collection<List<String>> minus) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected int computeScore(
			@SuppressWarnings("unused")	DirectedSparseGraph original, 
			@SuppressWarnings("unused")	StatePair blueRed) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected DirectedSparseGraph createAugmentedPTA(
			@SuppressWarnings("unused")	DirectedSparseGraph model,
			@SuppressWarnings("unused")	Collection<List<String>> plus, 
			@SuppressWarnings("unused")	Collection<List<String>> minus) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected List<List<String>> generateQuestions(
			@SuppressWarnings("unused")	DirectedSparseGraph model,
			@SuppressWarnings("unused")	StatePair pair) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected DirectedSparseGraph mergeAndDeterminize(
			@SuppressWarnings("unused")	Graph model,
			@SuppressWarnings("unused")	StatePair pair) {
		throw new UnsupportedOperationException();
	}
	
    public static Collection<List<String>> sort(Collection<List<String>> data)
    {
    	LinkedList<List<String>> result = new LinkedList<List<String>>();result.addAll(data);
    	Collections.sort(result, new Comparator<List<String>>() {

			public int compare(List<String> o1, List<String> o2) {
				int len1 = o1.size(),len2 = o2.size();
				if (len1 < len2) return -1;else if (len1 > len2) return 1;
				Iterator<String> it1 = o1.iterator(),it2 = o2.iterator();
				while(it1.hasNext())
				{
					int cmpResult = it1.next().compareTo(it2.next());
					if (cmpResult != 0) return cmpResult;
				}
				return 0;
			}});
    	return result;
    }
}
