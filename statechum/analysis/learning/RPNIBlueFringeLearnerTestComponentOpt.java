package statechum.analysis.learning;

import java.awt.Frame;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import java.beans.XMLEncoder;
import java.io.BufferedOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class RPNIBlueFringeLearnerTestComponentOpt extends
		RPNIBlueFringeLearnerTestComponent {

	public RPNIBlueFringeLearnerTestComponentOpt(Frame parentFrame) {
		super(parentFrame);
	}

	@Override
	protected int computeScore(DirectedSparseGraph original, StatePair blueRed)
	{
		//return super.computeScore(original, blueRed);
		return scoreComputer.computeStateScore(blueRed);
	}
	
	protected int runCount = 1000;
	
	/* (non-Javadoc)
	 * @see statechum.analysis.learning.RPNIBlueFringeLearnerTestComponent#chooseStatePairs(edu.uci.ics.jung.graph.impl.DirectedSparseGraph, java.util.Set, java.util.Set)
	 */
	@Override
	protected Stack chooseStatePairs(DirectedSparseGraph g,
			Collection<List<String>> plus, Collection<List<String>> minus) {
		
		Stack result = null;
		scoreComputer = new computeStateScores(g,"SINK");
		//return super.chooseStatePairs(g, plus, minus);

		scoreComputer.generalisationThreshold = generalisationThreshold;
		scoreComputer.pairsMergedPerHypothesis = pairsMergedPerHypothesis;

		result = scoreComputer.chooseStatePairs();
		return result;
	}

	/* (non-Javadoc)
	 * @see statechum.analysis.learning.RPNIBlueFringeLearnerTestComponent#generateQuestions(edu.uci.ics.jung.graph.impl.DirectedSparseGraph, edu.uci.ics.jung.graph.impl.DirectedSparseGraph, statechum.analysis.learning.StatePair)
	 */
	@Override
	protected List<List<String>> generateQuestions(DirectedSparseGraph model,
			DirectedSparseGraph temp, StatePair pair) {
		scoreComputer.generalisationThreshold = generalisationThreshold;
		scoreComputer.pairsMergedPerHypothesis = pairsMergedPerHypothesis;
		return scoreComputer.computeQS(pair,temp);
		
		//return super.generateQuestions(model, temp, pair);
	}

	computeStateScores scoreComputer = null;

	/* (non-Javadoc)
	 * @see statechum.analysis.learning.RPNIBlueFringeLearnerTestComponent#learnMachine(edu.uci.ics.jung.graph.impl.DirectedSparseGraph, java.util.Set, java.util.Set)
	 */
	@Override
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model,
			Collection<List<String>> plus, Collection<List<String>> minus)
			throws InterruptedException {
		scoreComputer = null;// to rebuild the transition diagram each time learnMachine is called (the rebuild is done when chooseStatePairs is called.
		return super.learnMachine(model, plus, minus);
	}
	
	protected int checkpointNumber = 0;
	protected int iterationNumber = 0;
		
	@Override
	protected DirectedSparseGraph mergeAndDeterminize(Graph model, StatePair pair) {
		return computeStateScores.mergeAndDeterminize(model, pair);
	}

	@Override
	protected DirectedSparseGraph createAugmentedPTA(DirectedSparseGraph model, Collection<List<String>> sPlus, Collection<List<String>> sMinus) {
		//return super.createAugmentedPTA(model, sPlus, sMinus);
		computeStateScores.augmentPTA(model, sPlus, true);
		computeStateScores.augmentPTA(model, sMinus, false);

		System.out.println("vertices: "+model.numVertices()+" edges: "+model.numEdges());
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
