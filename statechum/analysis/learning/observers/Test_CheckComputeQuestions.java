/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
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
package statechum.analysis.learning.observers;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildGraph;

import java.io.FileNotFoundException;
import java.util.HashMap;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.DeterministicDirectedSparseGraph.VertexID.ComparisonKind;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.WMethod;

/**
 * @author kirill
 *
 */
public class Test_CheckComputeQuestions {
	public static void main(@SuppressWarnings("unused") String [] args)
	{
		LearnerGraph fsm = new LearnerGraph(buildGraph("A-a->A-b->B", "testTestGeneration4"),Configuration.getDefaultConfiguration());
		System.out.println("W set is "+WMethod.computeWSet_reducedmemory(fsm));
		
			HashMap<String,Integer> map = new HashMap<String,Integer>();
			map.put("b", 1);
			map.put("a", 0);
			map.put("c", 3);
			System.out.println("a".hashCode()+" "+"b".hashCode());
			System.out.println("b,a map is : "+map);
		/*
		String A="A",B="B";
		
		System.out.println(A.hashCode()+" "+B.hashCode());
		
		HashMap<String,String> m = new HashMap<String,String>();
		m.put(A, A);m.put(B, B);
		System.out.println(m);
		*/
	}
	public static void mainA(@SuppressWarnings("unused") String [] args)
	{
		Configuration evalData = Configuration.getDefaultConfiguration().copy();
		LearnerGraph original = LearnerGraph.loadGraph("00000010_ELEM_MERGEANDDETERMINIZE", evalData); 
		System.out.println(original);
	}
	
	public static void mainB(@SuppressWarnings("unused") String [] args)
	{
		VertexID.comparisonKind = ComparisonKind.COMPARISON_LEXICOGRAPHIC_ORIG;

		LearnerSimulator simulator;
		try {
			simulator = new LearnerSimulator(new java.io.FileInputStream("/home/kirill/workspace/XMachineTool_j6/resources/nonsvn/logs/3_25Inputs_75_12.xml_LEARNER_BLUEFRINGE_MAY2008_log-100.xml"),true);
			final LearnerEvaluationConfiguration evalData = simulator.readLearnerConstructionData();
			evalData.config.setInitialIDvalue(1);
			evalData.config.setUseAmber(false);evalData.config.setUseLTL(false);
			evalData.config.setSpeculativeQuestionAsking(false);
			evalData.config.setDefaultInitialPTAName("Init");
			
			LearnerGraph original = LearnerGraph.loadGraph("/home/kirill/workspace/XMachineTool_j6/original.xml", evalData.config); 
			LearnerGraph merged = LearnerGraph.loadGraph("/home/kirill/workspace/XMachineTool_j6/transformed.xml", evalData.config);
			
			System.out.println(ComputeQuestions.computeQS(new PairScore(original.findVertex("P91"), original.findVertex("P38"),19,0), original, merged));
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
