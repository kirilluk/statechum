package statechum.apps;

/*
 * Prints out the test set for a state machine as computed by Chow's W-Method (see Chow TSE article 1978). 
 * 
 * First argument - path to state machine file (as graphml - see examples in resources/TestGraphs)
 * Second argument (optional) - number of extra states you want to test for - default is zero.
 */

import statechum.Configuration;
import statechum.analysis.learning.rpnicore.LearnerGraph;

public class WMethodApp {
	
	public static void main(String[] args){
		LearnerGraph g = LearnerGraph.loadGraph(args[0], Configuration.getDefaultConfiguration());
		int k = 0;
		if(args[1]!=null)
			k = Integer.parseInt(args[1]);
		System.out.println(g.wmethod.getFullTestSet(k));
	}

}
