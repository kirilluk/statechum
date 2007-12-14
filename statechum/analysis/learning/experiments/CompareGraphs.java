package statechum.analysis.learning.experiments;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import statechum.JUConstants;
import statechum.analysis.learning.*;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import statechum.xmachine.model.testset.WMethod;
/**
 * Compares two graphs (fed as string inputs using Kirill's notation) and provides feedback
 * as Precision and recall, computed using the W-Method.
 * @author nw
 *
 */

public class CompareGraphs {

	/**
	 * @param hypothesis followed by actual graph
	 */
	public static void main(String[] args) {
		DirectedSparseGraph specGraph = TestFSMAlgo.buildGraph("A-a->B-b->C\nA-b->C", "specGraph");
		DirectedSparseGraph impGraph = TestFSMAlgo.buildGraph("A-a->B-b->C\nA-b->C", "impGraph");
		WMethod wm = new WMethod(impGraph,1);
		Collection testset = wm.getFullTestSet();
		testset.addAll(wm.getTransitionCover());
		PrecisionRecall pr = computePrecisionRecall(specGraph, impGraph, testset);
		System.out.println("precision: "+pr.getPrecision()+", recall: "+pr.getRecall());
		printTests(wm.getFullTestSet());
		
	}
	
	private static void printTests(Collection<List<String>> tests){
		for (List<String> list : tests) {
			System.out.println(list);
		}
	}

	public static PosNegPrecisionRecall computePrecisionRecall(DirectedSparseGraph learned, 
			DirectedSparseGraph correct, Collection<List<String>> tests){
		Collection<List<String>> retneg = new HashSet<List<String>>();
		Collection<List<String>> relneg = new HashSet<List<String>>();
		Collection<List<String>> retpos = new HashSet<List<String>>();
		Collection<List<String>> relpos = new HashSet<List<String>>();
		for (List<String> list : tests) {
			Vertex hypVertex = RPNIBlueFringeLearner.getVertex(learned, list);
			Vertex correctVertex = RPNIBlueFringeLearner.getVertex(correct, list);
			if((hypVertex == null)&(correctVertex == null)){
				relneg.add(list);
				retneg.add(list);
			}
			else if((hypVertex == null)&(correctVertex != null)){
				if(correctVertex.getUserDatum(JUConstants.ACCEPTED).equals("true")){
					relpos.add(list);
					retneg.add(list);
				}
				else if(correctVertex.getUserDatum(JUConstants.ACCEPTED).equals("false")){
					relneg.add(list);
					retneg.add(list);
				}
			}
			else if(hypVertex !=null & correctVertex!=null){
				if(hypVertex.getUserDatum(JUConstants.ACCEPTED).toString().equals("true")&&correctVertex.getUserDatum(JUConstants.ACCEPTED).equals("false")){
					retpos.add(list);
					relneg.add(list);
				}
				else if(hypVertex.getUserDatum(JUConstants.ACCEPTED).toString().equals("false")&&correctVertex.getUserDatum(JUConstants.ACCEPTED).equals("true")){
					retneg.add(list);
					relpos.add(list);
				}
				else if(hypVertex.getUserDatum(JUConstants.ACCEPTED).toString().equals("false")&&correctVertex.getUserDatum(JUConstants.ACCEPTED).equals("false")){
					retneg.add(list);
					relneg.add(list);
				}
				else if(hypVertex.getUserDatum(JUConstants.ACCEPTED).toString().equals("true")&&correctVertex.getUserDatum(JUConstants.ACCEPTED).equals("true")){ 
					retpos.add(list);
					relpos.add(list);
				}
			}
			else if(hypVertex!=null & correctVertex == null){
				if(hypVertex.getUserDatum(JUConstants.ACCEPTED).equals("true")){
					retpos.add(list);
					relneg.add(list);
				}
				else if(hypVertex.getUserDatum(JUConstants.ACCEPTED).equals("false")){
					retneg.add(list);
					relneg.add(list);
				}
			}
				
		}
		return new PosNegPrecisionRecall(retpos,relpos, retneg, relneg);
	}

}
