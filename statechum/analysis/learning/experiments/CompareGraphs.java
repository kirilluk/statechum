/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.analysis.learning.experiments;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

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
		LearnerGraph wm = new LearnerGraph(impGraph,Configuration.getDefaultConfiguration());
		final int extraStateNumber = 1;
		Collection<List<String>> testset = wm.wmethod.getFullTestSet(extraStateNumber);
		testset.addAll(wm.wmethod.getTransitionCover());
		PrecisionRecall pr = computePrecisionRecall(specGraph, impGraph, testset);
		System.out.println("precision: "+pr.getPrecision()+", recall: "+pr.getRecall());
		printTests(wm.wmethod.getFullTestSet(extraStateNumber));
		
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
			Vertex hypVertex = RPNIBlueFringeLearnerOrig.getVertex(learned, list);
			Vertex correctVertex = RPNIBlueFringeLearnerOrig.getVertex(correct, list);
			if((hypVertex == null)&(correctVertex == null)){
				relneg.add(list);
				retneg.add(list);
			}
			else if((hypVertex == null)&(correctVertex != null)){
				if(DeterministicDirectedSparseGraph.isAccept(correctVertex)){
					relpos.add(list);
					retneg.add(list);
				}
				else if(!DeterministicDirectedSparseGraph.isAccept(correctVertex)){
					relneg.add(list);
					retneg.add(list);
				}
			}
			else if(hypVertex !=null & correctVertex!=null){
				if(DeterministicDirectedSparseGraph.isAccept(hypVertex)&&!DeterministicDirectedSparseGraph.isAccept(correctVertex)){
					retpos.add(list);
					relneg.add(list);
				}
				else if(!DeterministicDirectedSparseGraph.isAccept(hypVertex)&&DeterministicDirectedSparseGraph.isAccept(correctVertex)){
					retneg.add(list);
					relpos.add(list);
				}
				else if(!DeterministicDirectedSparseGraph.isAccept(hypVertex)&&!DeterministicDirectedSparseGraph.isAccept(correctVertex)){
					retneg.add(list);
					relneg.add(list);
				}
				else if(DeterministicDirectedSparseGraph.isAccept(hypVertex)&&DeterministicDirectedSparseGraph.isAccept(correctVertex)){ 
					retpos.add(list);
					relpos.add(list);
				}
			}
			else if(hypVertex!=null & correctVertex == null){
				if(DeterministicDirectedSparseGraph.isAccept(hypVertex)){
					retpos.add(list);
					relneg.add(list);
				}
				else if(!DeterministicDirectedSparseGraph.isAccept(hypVertex)){
					retneg.add(list);
					relneg.add(list);
				}
			}
				
		}
		return new PosNegPrecisionRecall(retpos,relpos, retneg, relneg);
	}

}
