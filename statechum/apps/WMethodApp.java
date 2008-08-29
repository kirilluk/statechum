package statechum.apps;

/*
 * Prints out the test set for a state machine as computed by Chow's W-Method (see Chow TSE article 1978). 
 * 
 * First argument - path to state machine file (as graphml - see examples in resources/TestGraphs)
 * Second argument (optional) - number of extra states you want to test for - default is zero.
 */

import statechum.Configuration;

import java.util.*;

import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.rpnicore.*;

public class WMethodApp {
	
	public static void main(String[] args){
		LearnerGraph g = LearnerGraph.loadGraph(args[0], Configuration.getDefaultConfiguration());
		int k = 0;
		if(args.length>1)
			k = Integer.parseInt(args[1]);
		displayTests(g.wmethod.getFullTestSet(k), g);
	}
	
	private static void displayTests(Collection<List<String>> tests, LearnerGraph g){
		Iterator<List<String>> testIt = tests.iterator();
		int count = 0;
		while(testIt.hasNext()){
			count++;
			List<String> test = testIt.next();
			int accept = g.paths.tracePath(test);
			System.out.print("#"+count+" ");
			if(accept == AbstractOracle.USER_ACCEPTED)
				System.out.println("ACCEPTED: " + test);
			else
				formatRejectString(test, accept);
		}
	}
	
	private static void formatRejectString(List<String> test, int rejectPoint){
		System.out.println("ACCEPT: "+ test.subList(0, rejectPoint));
		System.out.println("\tREJECT: "+test.get(rejectPoint));
	}

}
