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
package statechum.apps;

import static statechum.analysis.learning.rpnicore.FsmParser.buildGraph;

import java.util.List;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.rpnicore.GD;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.analysis.learning.rpnicore.TestGD_Multithreaded;

/** Demonstrates how vertices and edges can be labelled.
 * 
 * @author kirr
 *
 */
public class GDVisualDemo 
{
	public static DirectedSparseGraph obtainDifferenceGraph(String graphA, String graphB,int counter)
	{
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = 
			new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		Configuration config = Configuration.getDefaultConfiguration();
		return gd.showGD(
				new LearnerGraphND(buildGraph(graphA, "labellingDemo_A_"+counter),config),
				new LearnerGraphND(buildGraph(graphB, "labellingDemo_B_"+counter),config),
				ExperimentRunner.getCpuNumber());
	}
	
	public static void main(String str[])
	{// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
		
		int counter = 0;
		for(Pair<String,String> pair:new Pair[] {
				new Pair<String,String>("A-a->B-a->C\nB-b->D","A-a->C-c->E-c->D-c->F"),
				new Pair<String,String>("A-a->B-a->C\nB-b->D\nA-d->T-d->S","A-a->C-c->E-c->D-c->F-s->T\nA-d->T-d->T"),
				new Pair<String,String>("A-a->B-a->C\nB-b->D\nA-d->T-d->T","A-a->C-c->E-c->D-c->F-s->T\nA-d->U-d->U"),
				new Pair<String,String>(TestGD_Multithreaded.A6,TestGD_Multithreaded.B6)
				})
		{
			DirectedSparseGraph dGraph = obtainDifferenceGraph(pair.firstElem, pair.secondElem, counter);
			++counter;
			Visualiser.updateFrame(dGraph, null);
		}
		
	}
}
