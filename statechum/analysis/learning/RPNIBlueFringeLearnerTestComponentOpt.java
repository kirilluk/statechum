package statechum.analysis.learning;

import java.awt.Frame;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;


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
	
	protected int runCount = 40;
	
	/* (non-Javadoc)
	 * @see statechum.analysis.learning.RPNIBlueFringeLearnerTestComponent#chooseStatePairs(edu.uci.ics.jung.graph.impl.DirectedSparseGraph, java.util.Set, java.util.Set)
	 */
	@Override
	protected Stack chooseStatePairs(DirectedSparseGraph g,
			Set<List<String>> plus, Set<List<String>> minus) {
/*		
		if (runCount -- < 0) // used for profiling
		{
			System.out.println("FORCED TERMINATION");
			return new Stack();
		}
*/		
		System.out.println("vertices: "+g.numVertices()+" edges: "+g.numEdges());
		Stack result = null;
		scoreComputer = new computeStateScores(g,"SINK");
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

	protected computeStateScores scoreComputer = null;

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
