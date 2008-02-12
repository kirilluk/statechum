package statechum.analysis.learning.spin;

import static statechum.analysis.learning.RPNIBlueFringeLearner.isAccept;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.ComputeStateScores;
import statechum.analysis.learning.StatePair;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;
import java.util.Map.Entry;

import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.utils.UserData;

/**
 * 
 * @author nw
 *
 */

public class ComputeSpinStateScores extends ComputeStateScores {
	
	Set<String> ltl;

	public ComputeSpinStateScores(int x, Set<String> ltlFormulae){
		super(x);
		ltl = ltlFormulae;
	}
	
	public int computePairCompatibilityScore(StatePair origPair)
	{
		int score = super.computePairCompatibilityScore(origPair);
		if(score>=0){
			score = checkPropertiesOfMerge(origPair, score); //This is obviously going to be too slow, and needs to be improved
		}
		return score;
	}
	
	protected int checkPropertiesOfMerge(StatePair pair, int score){
		try{
			ComputeStateScores temp = (ComputeStateScores)this.clone();
			ComputeStateScores.mergeAndDeterminize(temp, pair);
			if(!SpinUtil.check(temp.getGraph(), ltl))
				return -1;
		} catch(Exception e){e.printStackTrace();}
		return score;
		
	}
	
}
