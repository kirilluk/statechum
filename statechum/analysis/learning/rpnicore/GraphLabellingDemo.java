/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */
package statechum.analysis.learning.rpnicore;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildGraph;

import java.awt.Color;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.ExperimentRunner;

/** Demonstrates how vertices and edges can be labelled.
 * 
 * @author kirr
 *
 */
public class GraphLabellingDemo 
{
	public static void main(String str[])
	{// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A---a--->B-a->C-b->C-c->B\nA-d->C\nC-d-#D", "simple_graph"),config);
		LearnerGraph graph2 = new LearnerGraph(TestFSMAlgo.buildGraph("A---a--->B", "simple_graph2"),config);
		LearnerGraph graph3 = new LearnerGraph(TestFSMAlgo.buildGraph("A---a--->A", "simple_graph3"),config);
		
		Map<String,Map<String,Color>> transitionsUsedInC = new TreeMap<String,Map<String,Color>>();
		for(CmpVertex v:graph.transitionMatrix.keySet())
			transitionsUsedInC.put(v.toString(),new TreeMap<String,Color>());
		Map<String,String> vertexExtraLabel = new TreeMap<String,String>();
		Collection<List<String>> wSet = WMethod.computeWSet_reducedmemory(graph);
		for(Entry<CmpVertex,LinkedList<String>> entry:graph.pathroutines.computeShortPathsToAllStates().entrySet())
		{
			CmpVertex v = graph.init;
			for(String input:entry.getValue())
			{
				transitionsUsedInC.get(v.toString()).put(input,Color.BLUE);v=graph.transitionMatrix.get(v).get(input);
			}
		}
		
		for(CmpVertex vert:graph.transitionMatrix.keySet())
		{
			int counter = 0;
			for(List<String> seq:wSet)
				if (graph.paths.tracePath(seq,vert) == AbstractOracle.USER_ACCEPTED) ++counter;
			
			vertexExtraLabel.put(vert.toString(), "\n"+counter+" seq");
		}
		//System.out.println(transitionsUsedInC+"\n###################\n"+vertexExtraLabel);
		
		DirectedSparseGraph gr = graph.pathroutines.getGraph();
		gr.addUserDatum(JUConstants.VERTEX, vertexExtraLabel, UserData.CLONE);
		gr.addUserDatum(JUConstants.EDGE, transitionsUsedInC, UserData.CLONE);
		
		GD gd = new GD();
		int counter = 0;

		DirectedSparseGraph aGraph = gd.showGD(
				new LearnerGraph(TestFSMAlgo.buildGraph(TestGD_Multithreaded.A6, "labellingDemo_A_"+counter),config),
				TestGD_Multithreaded.convertToNumerical(new LearnerGraph(buildGraph(TestGD_Multithreaded.A6,"testComputeGD5a"),config)),
				ExperimentRunner.getCpuNumber());
			++counter;
			Visualiser.updateFrame(gr, aGraph);
/*
		for(Pair<String,String> pair:new Pair[] {
				new Pair<String,String>("A-a->B-a->C\nB-b->D","A-a->C-c->E-c->D-c->F"),
				new Pair<String,String>("A-a->B-a->C\nB-b->D\nA-d->T-d->S","A-a->C-c->E-c->D-c->F-s->T\nA-d->T-d->T"),
//				new Pair<String,String>("A-a->B-a->C\nB-b->D\nA-d->T-d->T","A-a->C-c->E-c->D-c->F-s->T\nA-d->U-d->U"),
				})
		{
			DirectedSparseGraph dGraph = gd.showGD(
				new LearnerGraph(TestFSMAlgo.buildGraph(pair.firstElem, "labellingDemo_A_"+counter),config),
				new LearnerGraph(TestFSMAlgo.buildGraph(pair.secondElem, "labellingDemo_B_"+counter),config),
				ExperimentRunner.getCpuNumber());
			++counter;
			Visualiser.updateFrame(gr, dGraph);
		}
		*/
		//Visualiser.updateFrame(graph2, graph3);
	}
}
