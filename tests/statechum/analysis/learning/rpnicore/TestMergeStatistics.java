/* Copyright (c) The University of Sheffield
 * 
 * This file is part of StateChum
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

import static org.junit.Assert.assertEquals;
import static statechum.analysis.learning.rpnicore.FsmParserStatechum.buildLearnerGraph;

import java.util.ArrayList;
import java.util.Collection;

import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.StateMergingStatistics;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

public class TestMergeStatistics {
	public static final Configuration config = Configuration.getDefaultConfiguration().copy();
	/** Label converter to use. */
	public final ConvertALabel converter = new Transform.InternStringLabel();

	@Test
	public final void computeStatistics1()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C-a->D / B-b->B", "computeStatistics1",config,converter);
		StateMergingStatistics evaluator = new ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown(graph,true);
		assertEquals(0.0, evaluator.reportInvalidMergers(), Configuration.fpAccuracy);
		assertEquals(0.0, evaluator.reportMissedMergers(), Configuration.fpAccuracy);
	}

	@Test
	public final void computeStatistics2a()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C-a->D-a->A / B-b->B", "computeStatistics1graph",config,converter);
		LearnerGraph pta = buildLearnerGraph("A-a->B-a->C-a->D-a->E / B-b->B", "computeStatistics1pta",config,converter);
		StateMergingStatistics evaluator = new ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown(graph,true);
		Collection<CmpVertex> reds = new ArrayList<CmpVertex>();for(String name:new String[] {"A","B","C"}) reds.add(pta.findVertex(VertexID.parseID(name)));
		evaluator.stateSelectedAsRed(pta, pta.findVertex(VertexID.parseID("E")), reds);
		assertEquals(0.0, evaluator.reportInvalidMergers(), Configuration.fpAccuracy);
		assertEquals(1.0, evaluator.reportMissedMergers(), Configuration.fpAccuracy);
		assertEquals(3.0, evaluator.countRedsKnowingTheCorrectSolution(), Configuration.fpAccuracy);// here we account for missed mergers, hence an attempt to label E red does not increase the number of red states.
	}

	@Test
	public final void computeStatistics2b()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C-a->D-a->A / B-b->B", "computeStatistics1graph",config,converter);
		LearnerGraph pta = buildLearnerGraph("A-a->B-a->C-a->D-a->E / B-b->B", "computeStatistics1pta",config,converter);
		StateMergingStatistics evaluator = new ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown(graph,false);
		Collection<CmpVertex> reds = new ArrayList<CmpVertex>();for(String name:new String[] {"A","B","C"}) reds.add(pta.findVertex(VertexID.parseID(name)));
		evaluator.stateSelectedAsRed(pta, pta.findVertex(VertexID.parseID("E")), reds);
		assertEquals(0.0, evaluator.reportInvalidMergers(), Configuration.fpAccuracy);
		assertEquals(1.0, evaluator.reportMissedMergers(), Configuration.fpAccuracy);
		assertEquals(4.0, evaluator.countRedsKnowingTheCorrectSolution(), Configuration.fpAccuracy);// since we do not count missed mergers, all red states are reported.
	}


	@Test
	public final void computeStatistics3()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C-a->D-a->A / B-b->B", "computeStatistics1graph",config,converter);
		LearnerGraph pta = buildLearnerGraph("A-a->B-a->C-a->D-a->E / B-b->B", "computeStatistics1pta",config,converter);
		StateMergingStatistics evaluator = new ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown(graph,true);
		Collection<CmpVertex> reds = new ArrayList<CmpVertex>();for(String name:new String[] {"A","B"}) reds.add(pta.findVertex(VertexID.parseID(name)));
		evaluator.stateSelectedAsRed(pta, pta.findVertex(VertexID.parseID("C")), reds);
		assertEquals(0.0, evaluator.reportInvalidMergers(), Configuration.fpAccuracy);
		assertEquals(0.0, evaluator.reportMissedMergers(), Configuration.fpAccuracy);
	}

	@Test
	public final void computeStatistics4()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C-a->D-a->A / B-b->B", "computeStatistics1graph",config,converter);
		LearnerGraph pta = buildLearnerGraph("A-a->B-a->C-a->D-a->E / B-b->B", "computeStatistics1pta",config,converter);
		StateMergingStatistics evaluator = new ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown(graph,true);
		evaluator.pairSelectedForMerger(pta, new StatePair(pta.findVertex(VertexID.parseID("A")),pta.findVertex(VertexID.parseID("B"))));
		assertEquals(1.0, evaluator.reportInvalidMergers(), Configuration.fpAccuracy);
		assertEquals(0.0, evaluator.reportMissedMergers(), Configuration.fpAccuracy);
	}

	@Test
	public final void computeStatistics5()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C-a->D-a->A / B-b->B", "computeStatistics1graph",config,converter);
		LearnerGraph pta = buildLearnerGraph("A-a->B-a->C-a->D-a->E / B-b->B", "computeStatistics1pta",config,converter);
		StateMergingStatistics evaluator = new ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown(graph,true);
		Collection<CmpVertex> reds = new ArrayList<CmpVertex>();for(String name:new String[] {"A","B","C"}) reds.add(pta.findVertex(VertexID.parseID(name)));
		evaluator.stateSelectedAsRed(pta, pta.findVertex(VertexID.parseID("E")), reds);
		evaluator.pairSelectedForMerger(pta, new StatePair(pta.findVertex(VertexID.parseID("A")),pta.findVertex(VertexID.parseID("B"))));
		assertEquals(0.5, evaluator.reportInvalidMergers(), Configuration.fpAccuracy);
		assertEquals(0.5, evaluator.reportMissedMergers(), Configuration.fpAccuracy);
	}
}
