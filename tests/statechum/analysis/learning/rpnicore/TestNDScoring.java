/* Copyright (c) 2016 The University of Sheffield
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

import static org.junit.Assert.*;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.Test;

import statechum.Configuration;
import statechum.Label;
import statechum.StringLabel;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

public class TestNDScoring 
{
	final Configuration config = Configuration.getDefaultConfiguration().copy();
	final ConvertALabel converter = new ConvertALabel() {
		final Map<Label,StringLabel> map = new TreeMap<Label,StringLabel>();
		
		@Override
		public Label convertLabelToLabel(final Label lbl) {
			StringLabel outcome = map.get(lbl);
			if (outcome == null)
			{
				outcome = new StringLabel(lbl.toErlangTerm()){

					@Override
					public int compareTo(Label o) {
						return super.compareTo(o);
					}

					@Override
					public int toInt() {
						String text=toErlangTerm();
						if (text.length() != 1)
							throw new IllegalArgumentException("label "+text+" should have a length of 1");
						return text.codePointAt(0);
					}

					@Override
					public String toErlangTerm() {
						return super.toErlangTerm();
					}};
				
				map.put(lbl, outcome);
			}
			return outcome;
		}
	};


	@Test
	public void testNDScore1()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / D-a->G","testNDScore1",config, converter);
		LearnerGraphND ndGraph = MarkovClassifier.computeInverseGraph(graph);
		AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData> cache = new AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData>(ndGraph);
		assertEquals(0, ndGraph.pathroutines.computeScore(ndGraph.findVertex("A"), ndGraph.findVertex("D"), cache));		
	}

	@Test
	public void testNDScore2()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / A-c->B / D-a->G","testNDScore1",config, converter);
		LearnerGraphND ndGraph = MarkovClassifier.computeInverseGraph(graph);
		AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData> cache = new AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData>(ndGraph);
		assertEquals(1, ndGraph.pathroutines.computeScore(ndGraph.findVertex("B"), ndGraph.findVertex("G"), cache));		
	}

	@Test
	public void testNDScore3()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / C-a->B / D-a->G","testNDScore1",config, converter);
		LearnerGraphND ndGraph = MarkovClassifier.computeInverseGraph(graph);
		AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData> cache = new AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData>(ndGraph);
		assertEquals(1, ndGraph.pathroutines.computeScore(ndGraph.findVertex("B"), ndGraph.findVertex("G"), cache));		
	}
	
	@Test
	public void testNDScore4()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / C-a->B / C2-b->B / D-a->G","testNDScore4",config, converter);
		LearnerGraphND ndGraph = MarkovClassifier.computeInverseGraph(graph);
		AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData> cache = new AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData>(ndGraph);
		assertEquals(1, ndGraph.pathroutines.computeScore(ndGraph.findVertex("B"), ndGraph.findVertex("G"), cache));		
	}
	
	@Test
	public void testNDScore5()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / C-a->B / C2-b->B / D-a->G / E-b->G","testNDScore5",config, converter);
		LearnerGraphND ndGraph = MarkovClassifier.computeInverseGraph(graph);
		AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData> cache = new AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData>(ndGraph);
		assertEquals(2, ndGraph.pathroutines.computeScore(ndGraph.findVertex("B"), ndGraph.findVertex("G"), cache));		
	}
	
	@Test
	public void testNDScore6()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B / C-a->B / A-b->B / D-a->G / D-b->G","testNDScore6",config, converter);
		LearnerGraphND ndGraph = MarkovClassifier.computeInverseGraph(graph);
		AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData> cache = new AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData>(ndGraph);
		assertEquals(2, ndGraph.pathroutines.computeScore(ndGraph.findVertex("B"), ndGraph.findVertex("G"), cache));		
	}
	
	@Test
	public void testNDScore7()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("P-a->A-a->B / C-a->B / A-b->B / D-a->G / D-b->G","testNDScore7",config, converter);
		LearnerGraphND ndGraph = MarkovClassifier.computeInverseGraph(graph);
		Label lblA = ndGraph.transitionMatrix.get(ndGraph.findVertex("A")).keySet().iterator().next();
		ndGraph.addTransition(ndGraph.transitionMatrix.get(ndGraph.findVertex("D")),lblA,ndGraph.findVertex("D"));// creates a loop around D
		AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData> cache = new AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData>(ndGraph);
		assertEquals(3, ndGraph.pathroutines.computeScore(ndGraph.findVertex("B"), ndGraph.findVertex("G"), cache));		
	}
	
	@Test
	public void testNDScore8()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("P-a->A-a->B / C-a->B / A-b->B / D-a->G / D-b->G","testNDScore8",config, converter);
		LearnerGraphND ndGraph = MarkovClassifier.computeInverseGraph(graph);
		Label lblA = ndGraph.transitionMatrix.get(ndGraph.findVertex("A")).keySet().iterator().next();
		ndGraph.addTransition(ndGraph.transitionMatrix.get(ndGraph.findVertex("D")),lblA,ndGraph.findVertex("D"));// creates a loop around D
		ndGraph.addTransition(ndGraph.transitionMatrix.get(ndGraph.findVertex("P")),lblA,ndGraph.findVertex("B"));// creates a transition from P to B
		AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData> cache = new AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData>(ndGraph);
		assertEquals(4, ndGraph.pathroutines.computeScore(ndGraph.findVertex("B"), ndGraph.findVertex("G"), cache));		
	}
	
	@Test
	public void testNDScore9()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C-a->C / B-b->C-b->C / E-a->E-b->F-a->F-b->G-a->G","testNDScore9",config, converter);
		LearnerGraphND ndGraph = new LearnerGraphND(graph,config);
		Label lblA = ndGraph.transitionMatrix.get(ndGraph.findVertex("A")).keySet().iterator().next();
		Iterator<Label> iter = ndGraph.transitionMatrix.get(ndGraph.findVertex("B")).keySet().iterator();iter.next();
		Label lblB = iter.next();
		assert lblA != lblB;
		ndGraph.addTransition(ndGraph.transitionMatrix.get(ndGraph.findVertex("B")),lblA,ndGraph.findVertex("B"));// creates a loop around B
		ndGraph.addTransition(ndGraph.transitionMatrix.get(ndGraph.findVertex("B")),lblB,ndGraph.findVertex("B"));// creates a loop around B
		AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData> cache = new AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData>(ndGraph);
		assertEquals(4, ndGraph.pathroutines.computeScore(ndGraph.findVertex("A"), ndGraph.findVertex("E"), cache));		
	}
	
	@Test
	public void testNDScore10()
	{
		final LearnerGraph graph = FsmParser.buildLearnerGraph("A-a->B-a->C-a->C / C-b->C / E-a->E-b->F-a->F-b->G-a->G","testNDScore10",config, converter);
		LearnerGraphND ndGraph = new LearnerGraphND(graph,config);
		Label lblA = ndGraph.transitionMatrix.get(ndGraph.findVertex("A")).keySet().iterator().next();
		Iterator<Label> iter = ndGraph.transitionMatrix.get(ndGraph.findVertex("C")).keySet().iterator();iter.next();
		Label lblB = iter.next();
		assert lblA != lblB;
		ndGraph.addTransition(ndGraph.transitionMatrix.get(ndGraph.findVertex("B")),lblA,ndGraph.findVertex("B"));// creates a loop around B
		ndGraph.addTransition(ndGraph.transitionMatrix.get(ndGraph.findVertex("B")),lblB,ndGraph.findVertex("B"));// creates a loop around B
		AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData> cache = new AbstractPathRoutines.CacheOfStateGroups<List<CmpVertex>,LearnerGraphNDCachedData>(ndGraph);
		assertEquals(6, ndGraph.pathroutines.computeScore(ndGraph.findVertex("A"), ndGraph.findVertex("E"), cache));		
	}
	

}
