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
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTA_FSMStructure;
import statechum.model.testset.PTA_computePrecisionRecall;
import statechum.model.testset.PTASequenceEngine.SequenceSet;
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
		DirectedSparseGraph specGraph = TestFSMAlgo.buildGraph("q0-initialise->q1-connect->q2-login->q3-setfiletype->q4-rename->q6-storefile->q5-setfiletype->q4-storefile->q7-appendfile->q5-setfiletype->q4\nq3-makedir->q8-makedir->q8-logout->q16-disconnect->q17\nq3-changedir->q9-listnames->q10-delete->q10-changedir->q9\nq10-appendfile->q11-logout->q16\nq3-storefile->q11\nq3-listfiles->q13-retrievefile->q13-logout->q16\nq13-changedir->q14-listfiles->q13\nq7-logout->q16\nq6-logout->q16", "specgraph");
		DirectedSparseGraph impGraph = TestFSMAlgo.buildGraph("q0-initialise->q1-connect->q2-login->q3-storefile->q9-logout->q12-disconnect->q13\nq3-makedir->q8-makedir->q8-logout->q12\nq3-setfiletype->q4-storefile->q5-appendfile->q6-setfiletype->q4-rename->q7-storefile->q6\nq7-logout->q12\nq3-listfiles->q11-retrievefile->q11-changedirectory->q10-listfiles->q11-logout->q12\nq3-changedirectory->q17-listnames->q16-changedirectory->q17\nq16-delete->q14-delete->q15-delete->q16\nq14-changedirectory->q13\nq14-appendfile->q7", "impGraph");
		compare(specGraph, impGraph);
	}
	
	public static void compare(String spec, DirectedSparseGraph imp){
		DirectedSparseGraph specGraph = TestFSMAlgo.buildGraph(spec, "specGraph");
		compare(specGraph, imp);
	}
	
	public static void compare(DirectedSparseGraph spec, DirectedSparseGraph imp){
		LearnerGraph specfsm =new LearnerGraph(spec, Configuration.getDefaultConfiguration()); 
		Visualiser v = new Visualiser(Visualiser.VIZ_PROPERTIES.UPPER);
		v.construct(specfsm.paths.getGraph());
		LearnerGraph wm = new LearnerGraph(imp,Configuration.getDefaultConfiguration());
		PosNegPrecisionRecall pr = compare(specfsm, wm);
		System.out.println(pr.getPosprecision()+", "+pr.getPosrecall()+", "+pr.getNegprecision()+", "+pr.getNegrecall());
	}
	
	public static PosNegPrecisionRecall compare(LearnerGraph specfsm, LearnerGraph imp){
		PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(imp);
		PTASequenceEngine engine = new PTA_FSMStructure(specfsm);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(specfsm.wmethod.getFullTestSet(1));
		return precRec.crossWith(engine);
	}
	
	private static void printTests(Collection<List<String>> tests){
		for (List<String> list : tests) {
			System.out.println(list);
		}
	}
	
	public static double computeAccuracy(LearnerGraph learned, LearnerGraph correct, Collection<List<String>> tests){
		int failed = 0;
		for (List<String> list : tests) {
			if(learned.paths.tracePath(list)!=correct.paths.tracePath(list))
				failed++;
		}
		double accuracy = 1-((double)failed/(double)tests.size());
		return accuracy;
	}
}
