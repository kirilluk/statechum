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
import statechum.xmachine.model.testset.PTASequenceEngine;
import statechum.xmachine.model.testset.PTA_FSMStructure;
import statechum.xmachine.model.testset.PTA_computePrecisionRecall;
import statechum.xmachine.model.testset.PTASequenceEngine.SequenceSet;
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
		DirectedSparseGraph specGraph = TestFSMAlgo.buildGraph(args[0], "specGraph");
		DirectedSparseGraph impGraph = TestFSMAlgo.buildGraph(args[1], "impGraph");
		compare(specGraph, impGraph);
	}
	
	public static void compare(String spec, DirectedSparseGraph imp){
		DirectedSparseGraph specGraph = TestFSMAlgo.buildGraph(spec, "specGraph");
		compare(specGraph, imp);
	}
	
	public static void compare(DirectedSparseGraph spec, DirectedSparseGraph imp){
		LearnerGraph specfsm =new LearnerGraph(spec, Configuration.getDefaultConfiguration()); 
		LearnerGraph wm = new LearnerGraph(imp,Configuration.getDefaultConfiguration());
		PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(wm);
		PTASequenceEngine engine = new PTA_FSMStructure(specfsm);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(specfsm.wmethod.getFullTestSet(1));
		PosNegPrecisionRecall pr =  precRec.crossWith(engine);
		System.out.println(pr.getPosprecision()+", "+pr.getPosrecall()+", "+pr.getNegprecision()+", "+pr.getNegrecall());
	}
	
	private static void printTests(Collection<List<String>> tests){
		for (List<String> list : tests) {
			System.out.println(list);
		}
	}
/*  //I believe this can be removed
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
*/
}
