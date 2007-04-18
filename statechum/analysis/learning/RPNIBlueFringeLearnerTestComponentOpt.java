package statechum.analysis.learning;

import java.awt.Frame;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

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
	
	protected int runCount = 1000;
	
	protected void update(StatePair pair)
	{
		pair.getQ().setUserDatum("pair", pair, UserData.SHARED);
		pair.getR().setUserDatum("pair", pair, UserData.SHARED);// since this copy of the graph will really not be used, changes to it are immaterial at this stage
		updateGraph(scoreComputer.getGraph());
	}
	
	computeStateScores scoreComputer = null;

	private int counterAccepted =0, counterRejected =0, counterRestarted = 0, counterEmptyQuestions = 0;
	
	/* (non-Javadoc)
	 * @see statechum.analysis.learning.RPNIBlueFringeLearnerTestComponent#learnMachine(edu.uci.ics.jung.graph.impl.DirectedSparseGraph, java.util.Set, java.util.Set)
	 */
	@Override
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model, Collection<List<String>> plus, Collection<List<String>> minus) {
		this.sPlus = plus;
		this.sMinus = minus;
		createAugmentedPTA(plus,minus);
		
		StringWriter report = new StringWriter();
		counterAccepted =0;counterRejected =0;counterRestarted = 0;counterEmptyQuestions = 0;report.write("\n[ PTA: "+scoreComputer.getStatistics(false)+" ] ");
		setChanged();

		Stack<StatePair> possibleMerges = scoreComputer.chooseStatePairs();
		while(!possibleMerges.isEmpty()){
			StatePair pair = (StatePair)possibleMerges.pop();
			computeStateScores temp = computeStateScores.mergeAndDeterminize(scoreComputer, pair);
			setChanged();
			List<List<String>> questions = new LinkedList<List<String>>();
			if(scoreComputer.computeStateScore(pair)<this.certaintyThreshold)
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
				int answer = checkWithEndUser(model,question, new Object [] {"Test"});
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
					sPlus.add(question);
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
						LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer+1));sMinus.add(subAnswer);
	
						//System.out.println(setByAuto+question.toString()+ " <no> at position "+answer+", element "+question.get(answer));
						if( (answer < question.size()-1) || isAccept(tempVertex))
						{
							assert accepted.equals("true");
							restartLearning = true;break;
						}
					}
					else if (answer == USER_ACCEPTED-1){
						// sPlus = this.parentFrame.addTest(sPlus);
						if(sPlus == null)
							return model;
						//if(!containsSubString(sPlus, question))
						//	return learnMachine(initialise(), sPlus, sMinus);
					}
				
			}
			
			if (restartLearning)
			{// restart learning
				createAugmentedPTA(sPlus, sMinus);// KIRR: node labelling is done by createAugmentedPTA 
				setChanged();++counterRestarted;		
			}
			else
				// keep going with the existing model
				scoreComputer = temp;
			
			possibleMerges = scoreComputer.chooseStatePairs();
		}
		report.write("\n[ Questions: "+counterAccepted+" accepted "+counterRejected+" rejected resulting in "+counterRestarted+ " restarts; "+counterEmptyQuestions+" empty sets of questions ]\n[ Learned automaton: "+scoreComputer.getStatistics(true)+" ] ");
		DirectedSparseGraph result = scoreComputer.getGraph();result.addUserDatum("STATS", report.toString(), UserData.SHARED);
		return result;
	}
		
	protected computeStateScores createAugmentedPTA(Collection<List<String>> sPlus, Collection<List<String>> sMinus) {
		scoreComputer = new computeStateScores(generalisationThreshold,pairsMergedPerHypothesis);
		scoreComputer.augmentPTA(sMinus, false);
		scoreComputer.augmentPTA(sPlus, true);
		return scoreComputer;
	}
	
	protected void dumpSets(String output, Collection<List<String>> sPlus, Collection<List<String>> sMinus)
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
