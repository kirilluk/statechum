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

import statechum.JUConstants;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

import static statechum.analysis.learning.TestRpniLearner.isAccept;

public class RPNIBlueFringeLearnerTestComponentOpt extends
		RPNIBlueFringeLearnerTestComponent {

	public RPNIBlueFringeLearnerTestComponentOpt(Frame parentFrame) {
		super(parentFrame);
	}
	
	protected void update(StatePair pair)
	{
		pair.getQ().setUserDatum("pair", pair, UserData.SHARED);
		pair.getR().setUserDatum("pair", pair, UserData.SHARED);// since this copy of the graph will really not be used, changes to it are immaterial at this stage
		updateGraph(scoreComputer.getGraph());
	}
	
	computeStateScores scoreComputer = new computeStateScores(generalisationThreshold,pairsMergedPerHypothesis);

	private int counterAccepted =0, counterRejected =0, counterRestarted = 0, counterEmptyQuestions = 0;

	/** Takes the candidates for merging and computes the number of times different scores are encountered. */
	public static void populateHistogram(Collection<computeStateScores.PairScore> data, Map<Integer,AtomicInteger> histogram)
	{
		for(computeStateScores.PairScore pair:data)
		{
			AtomicInteger count = histogram.get(pair.getScore());
			if (count == null)
			{
				count = new AtomicInteger();histogram.put(pair.getScore(),count);
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

	public void init(Collection<List<String>> plus, Collection<List<String>> minus)
	{
		sPlus = plus;sMinus = minus;
		scoreComputer.initPTA();
		scoreComputer.augmentPTA(sMinus, false);
		scoreComputer.augmentPTA(sPlus, true);
	}
	
	public computeStateScores getScoreComputer()
	{
		return scoreComputer;
	}
	
	public DirectedSparseGraph learnMachine() {
		Map<Integer, AtomicInteger> whichScoresWereUsedForMerging = new HashMap<Integer,AtomicInteger>(),
			scoreDistribution = new HashMap<Integer,AtomicInteger>();
		
		computeStateScores newPTA = scoreComputer;// no need to clone - this is the job of mergeAndDeterminize anyway
		String pairsMerged = "";
		StringWriter report = new StringWriter();
		counterAccepted =0;counterRejected =0;counterRestarted = 0;counterEmptyQuestions = 0;report.write("\n[ PTA: "+scoreComputer.getStatistics(false)+" ] ");
		setChanged();

		Stack<computeStateScores.PairScore> possibleMerges = scoreComputer.chooseStatePairs();
		int plusSize = sPlus.size(), minusSize = sMinus.size();
		while(!possibleMerges.isEmpty()){
			populateHistogram(possibleMerges,scoreDistribution);
			computeStateScores.PairScore pair = possibleMerges.pop();
			computeStateScores temp = computeStateScores.mergeAndDeterminize(scoreComputer, pair);
			setChanged();
			Collection<List<String>> questions = new LinkedList<List<String>>();
			if(pair.getScore() <this.certaintyThreshold)
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
				String accepted = pair.getQ().getUserDatum(JUConstants.ACCEPTED).toString();
				int answer = checkWithEndUser(null,question, new Object [] {"Test"});
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
					
					if(!TestRpniLearner.isAccept(tempVertex))
					{
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
						
						//System.out.println(setByAuto+question.toString()+ " <no> at position "+answer+", element "+question.get(answer));
						if( (answer < question.size()-1) || isAccept(tempVertex))
						{
							assert accepted.equals("true");
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
				pairsMerged=pairsMerged+"========== RESTART "+counterRestarted+" ==========\n";
				//System.out.println(pairsMerged);
			}
			else
			{
				pairsMerged=pairsMerged+pair+" compatibility score : "+scoreComputer.computePairCompatibilityScore(pair)+" questions: "+questions.size()+"\n";

				// keep going with the existing model
				scoreComputer = temp;
				// now update the statistics
				AtomicInteger count = whichScoresWereUsedForMerging.get(pair.getScore());
				if (count == null)
				{
					count = new AtomicInteger();whichScoresWereUsedForMerging.put(pair.getScore(),count);
				}
				count.incrementAndGet();
			}
			
			possibleMerges = scoreComputer.chooseStatePairs();
		}
		report.write("\n[ Questions: "+counterAccepted+" accepted "+counterRejected+" rejected resulting in "+counterRestarted+ " restarts; "+counterEmptyQuestions+" empty sets of questions ]\n[ Learned automaton: "+scoreComputer.getStatistics(true)+" ] ");
		report.write("\n[ final sets of questions, plus: "+plusSize+" minus: "+minusSize+" ] ");
		report.write("\n[ Distribution of scores (score-count):"+HistogramToString(scoreDistribution,"DISTRIBUTION"));
		report.write("\n[ Pairs merged (score-number of times):"+HistogramToString(whichScoresWereUsedForMerging,"MERGED"));
		report.write("\n Pair merge details: \n"+pairsMerged);
		DirectedSparseGraph result = scoreComputer.getGraph();result.addUserDatum(JUConstants.STATS, report.toString(), UserData.SHARED);
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

	@Override
	protected List<List<String>> generateQuestions(DirectedSparseGraph model,
			DirectedSparseGraph temp, StatePair pair) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected Stack chooseStatePairs(DirectedSparseGraph g,
			Collection<List<String>> plus, Collection<List<String>> minus) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected int computeScore(DirectedSparseGraph original, StatePair blueRed) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected DirectedSparseGraph createAugmentedPTA(DirectedSparseGraph model,
			Collection<List<String>> plus, Collection<List<String>> minus) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected List<List<String>> generateQuestions(DirectedSparseGraph model,
			StatePair pair) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected DirectedSparseGraph mergeAndDeterminize(Graph model,
			StatePair pair) {
		throw new UnsupportedOperationException();
	}	
}
