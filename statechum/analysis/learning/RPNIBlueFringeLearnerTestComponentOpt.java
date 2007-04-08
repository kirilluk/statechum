package statechum.analysis.learning;

import java.awt.Frame;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class RPNIBlueFringeLearnerTestComponentOpt extends
		RPNIBlueFringeLearnerTestComponent {

	public RPNIBlueFringeLearnerTestComponentOpt(Frame parentFrame) {
		super(parentFrame);
	}

	@Override
	protected int computeScore(DirectedSparseGraph original, StatePair blueRed)
	{
		return scoreComputer.computeStateScore(blueRed);
	}
	
	/* (non-Javadoc)
	 * @see statechum.analysis.learning.RPNIBlueFringeLearnerTestComponent#chooseStatePairs(edu.uci.ics.jung.graph.impl.DirectedSparseGraph, java.util.Set, java.util.Set)
	 */
	@Override
	protected Stack chooseStatePairs(DirectedSparseGraph g,
			Set<List<String>> plus, Set<List<String>> minus) {
		
		if (scoreComputer == null)
		{
			scoreComputer = new TestRpniLearner.computeStateScores(g,"SINK");
			scoreComputer.generalisationThreshold = generalisationThreshold;
			scoreComputer.pairsMergedPerHypothesis = pairsMergedPerHypothesis;
		}
		return scoreComputer.chooseStatePairs();
	}

	/* (non-Javadoc)
	 * @see statechum.analysis.learning.RPNIBlueFringeLearnerTestComponent#generateQuestions(edu.uci.ics.jung.graph.impl.DirectedSparseGraph, edu.uci.ics.jung.graph.impl.DirectedSparseGraph, statechum.analysis.learning.StatePair)
	 */
	@Override
	protected List<List<String>> generateQuestions(DirectedSparseGraph model,
			DirectedSparseGraph temp, StatePair pair) {
/*		setChanged();updateGraph(temp);
		TestRpniLearner.computeStateScores tmpScoreComputer = new TestRpniLearner.computeStateScores(temp,"SINK");
		tmpScoreComputer.generalisationThreshold = generalisationThreshold;
		tmpScoreComputer.pairsMergedPerHypothesis = pairsMergedPerHypothesis;
		return tmpScoreComputer.computeQS(pair);
		-*/
		return super.generateQuestions(model, temp, pair);
	}

	protected TestRpniLearner.computeStateScores scoreComputer = null;

	/* (non-Javadoc)
	 * @see statechum.analysis.learning.RPNIBlueFringeLearnerTestComponent#learnMachine(edu.uci.ics.jung.graph.impl.DirectedSparseGraph, java.util.Set, java.util.Set)
	 */
	@Override
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model,
			Set<List<String>> plus, Set<List<String>> minus)
			throws InterruptedException {
		scoreComputer = null;// to rebuild the transition diagram each time learnMachine is called (the rebuild is done when chooseStatePairs is called.
		return super.learnMachine(model, plus, minus);
	}

	
}
