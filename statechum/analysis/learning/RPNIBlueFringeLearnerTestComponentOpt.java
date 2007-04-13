package statechum.analysis.learning;

import java.awt.Frame;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import java.beans.XMLEncoder;
import java.io.BufferedOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;

import statechum.JUConstants;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

public class RPNIBlueFringeLearnerTestComponentOpt extends
		RPNIBlueFringeLearnerTestComponent {

	public RPNIBlueFringeLearnerTestComponentOpt(Frame parentFrame) {
		super(parentFrame);
	}
	
	protected int runCount = 1000;
	
	computeStateScores scoreComputer = null;

	private int counterAccepted =0, counterRejected =0, counterRestarted = 0;
	
	/* (non-Javadoc)
	 * @see statechum.analysis.learning.RPNIBlueFringeLearnerTestComponent#learnMachine(edu.uci.ics.jung.graph.impl.DirectedSparseGraph, java.util.Set, java.util.Set)
	 */
	@Override
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model, Collection<List<String>> plus, Collection<List<String>> minus) throws InterruptedException {
		this.sPlus = plus;
		this.sMinus = minus;
		createAugmentedPTA(plus,minus);counterAccepted =0;counterRejected =0;counterRestarted = 0;System.out.print("\n[ PTA: "+scoreComputer.getStatistics()+" ] ");
		setChanged();

		Stack possibleMerges = scoreComputer.chooseStatePairs();
		while(!possibleMerges.isEmpty()){
			StatePair pair = (StatePair)possibleMerges.pop();
			computeStateScores temp = computeStateScores.mergeAndDeterminize(scoreComputer, pair);
			pair.getQ().setUserDatum("pair", pair, UserData.SHARED);
			pair.getR().setUserDatum("pair", pair, UserData.SHARED);// since this copy of the graph will really not be used, changes to it are immaterial at this stage 
			setChanged();
			List<List<String>> questions = new LinkedList<List<String>>();
			if(scoreComputer.computeStateScore(pair)<this.certaintyThreshold){
				questions = scoreComputer.computeQS(pair, temp);
				// questions = trimSet(questions); // KIRR: unnecessary by construction of questions
				//System.out.println("trying to merge "+pair);
			}
			
			boolean restartLearning = false;// whether we need to rebuild a PTA and restart learning.
			
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
				
				if(answer == USER_ACCEPTED)
				{
					++counterAccepted;
					sPlus.add(question);
					//System.out.println(setByAuto+question.toString()+ " <yes>");
					
					if(tempVertex.getUserDatum(JUConstants.ACCEPTED).toString().equals("false"))
					{
							restartLearning = true;break;
					}
				}
				else if(answer >= 0){
					assert answer < question.size();
					++counterRejected;
					LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer+1));sMinus.add(subAnswer);
					// sMinus.add(question.subList(0, answer+1)); // KIRR: without a `proper' collection in the set, I cannot serialise the sets into XML

					//System.out.println(setByAuto+question.toString()+ " <no> at position "+answer+", element "+question.get(answer));
					if((answer==question.size()-1)&&tempVertex.getUserDatum(JUConstants.ACCEPTED).toString().equals("false"))
						continue;
					else{
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
		System.out.print(counterAccepted+" accepts "+counterRejected+" rejects, "+counterRestarted+ " restarst \n[ Result: "+scoreComputer.getStatistics()+" ] ");
		return scoreComputer.getGraph();
	}
		
	protected computeStateScores createAugmentedPTA(Collection<List<String>> sPlus, Collection<List<String>> sMinus) {
		scoreComputer = new computeStateScores();
		scoreComputer.augmentPTA(sPlus, true);
		scoreComputer.augmentPTA(sMinus, false);
		return scoreComputer;
	}
	
	protected DirectedSparseGraph createAugmentedPTA(DirectedSparseGraph model, Collection<List<String>> sPlus, Collection<List<String>> sMinus) {
		//return super.createAugmentedPTA(model, sPlus, sMinus);
		computeStateScores.augmentPTA(model, sPlus, true);
		computeStateScores.augmentPTA(model, sMinus, false);

		
		//System.out.println("vertices: "+model.numVertices()+" edges: "+model.numEdges());
		/*
		if (model.getEdges().size() > 4000)
		{
			try
			{
				System.out.println("dumping sets");
				XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(new FileOutputStream("strings.xml")));
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
			
		}*/
		return model;
	}
}
