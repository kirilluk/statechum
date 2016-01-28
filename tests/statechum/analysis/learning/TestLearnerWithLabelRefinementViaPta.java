/* Copyright (c) 2015 The University of Sheffield
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
package statechum.analysis.learning;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;

public class TestLearnerWithLabelRefinementViaPta {

	@SuppressWarnings("unchecked")
	protected static <T> Map<T,Set<T>> constructMap(Object []data)
	{
		Map<T,Set<T>> adjacency = new TreeMap<T,Set<T>>();
		Iterator<Object> elemIter = Arrays.asList(data).iterator();
		while(elemIter.hasNext())
		{
			T key = (T)elemIter.next();Set<T> value = new TreeSet<T>(Arrays.asList((T[])elemIter.next()));
			adjacency.put(key, value);
		}
		
		return adjacency;
	}
	
	@Test
	public void testReflexivityCheck1()
	{
		LearnerWithLabelRefinementViaPta.checkReflexivity(constructMap(new Object[]{}));
	}

	@Test(expected = IllegalArgumentException.class)	
	public void testReflexivityCheck2()
	{
		LearnerWithLabelRefinementViaPta.checkReflexivity(constructMap(new Object[]{"A",new String[]{"B","C"},"B",new String[]{"C","A"}}));
	}
	
	@Test(expected = IllegalArgumentException.class)	
	public void testReflexivityCheck3()
	{
		LearnerWithLabelRefinementViaPta.checkReflexivity(constructMap(new Object[]{"A",new String[]{"B","C"},"B",new String[]{"C","A"},"C",new String[]{"A"}}));
	}
	
	@Test
	public void testReflexivityCheck4()
	{
		LearnerWithLabelRefinementViaPta.checkReflexivity(constructMap(new Object[]{"A",new String[]{"B","C"},"B",new String[]{"C","A"},"C",new String[]{"B","A"}}));
	}

	@Test(expected = IllegalArgumentException.class)	
	public void testReflexivityCheck5()
	{
		LearnerWithLabelRefinementViaPta.checkReflexivity(constructMap(new Object[]{"A",new String[]{"B","C"},"B",new String[]{"C","A"},"C",new String[]{"B","C","A"}}));
	}

	@Test
	public void testBronKerbosh0()
	{
		List<String> result = LearnerWithLabelRefinementViaPta.BronKerbosch(TestLearnerWithLabelRefinementViaPta.<String>constructMap(new Object[]{}),// thanks to http://stackoverflow.com/questions/450807/how-do-i-make-the-method-return-type-generic
				new LinkedList<String>(),Arrays.asList(new String[]{}), new LinkedList<String>());
		Assert.assertTrue(result.isEmpty());		
	}

	@Test
	public void testBronKerbosh1()
	{
		List<String> result = LearnerWithLabelRefinementViaPta.BronKerbosch(TestLearnerWithLabelRefinementViaPta.<String>constructMap(new Object[]{"A",new String[]{"B","C"},"B",new String[]{"C","A"},"C",new String[]{"B","A"}}),// thanks to http://stackoverflow.com/questions/450807/how-do-i-make-the-method-return-type-generic
				new LinkedList<String>(),Arrays.asList(new String[]{"A","B","C"}), new LinkedList<String>());
		Assert.assertEquals(Arrays.asList(new String[]{"A","B","C"}),result);
	}

	@Test
	public void testBronKerbosh2()
	{
		List<String> result = LearnerWithLabelRefinementViaPta.BronKerbosch(TestLearnerWithLabelRefinementViaPta.<String>constructMap(new Object[]{"A",new String[]{"B","C"},"B",new String[]{"C","A"},"C",new String[]{"B","A"}}),// thanks to http://stackoverflow.com/questions/450807/how-do-i-make-the-method-return-type-generic
				new LinkedList<String>(),Arrays.asList(new String[]{"B","C"}), new LinkedList<String>());
		Assert.assertEquals(Arrays.asList(new String[]{"B","C"}),result);
	}

	@Test
	public void testBronKerbosh3()
	{
		List<String> result = LearnerWithLabelRefinementViaPta.BronKerbosch(TestLearnerWithLabelRefinementViaPta.<String>constructMap(new Object[]{"A",new String[]{"B","C"},"B",new String[]{"C","A"},"C",new String[]{"B","A"}}),// thanks to http://stackoverflow.com/questions/450807/how-do-i-make-the-method-return-type-generic
				new LinkedList<String>(),Arrays.asList(new String[]{}), new LinkedList<String>());
		Assert.assertEquals(Arrays.asList(new String[]{}),result);
	}

	@Test
	public void testBronKerbosh4()
	{
		List<String> result = LearnerWithLabelRefinementViaPta.BronKerbosch(TestLearnerWithLabelRefinementViaPta.<String>constructMap(new Object[]{"A",new String[]{"B",},"B",new String[]{"A"},"C",new String[]{}}),// thanks to http://stackoverflow.com/questions/450807/how-do-i-make-the-method-return-type-generic
				new LinkedList<String>(),Arrays.asList(new String[]{"A","B","C"}), new LinkedList<String>());
		Assert.assertEquals(Arrays.asList(new String[]{"A","B"}),result);
	}

	/** Here the second part of the graph does not have all-to-all associations. */
	@Test
	public void testBronKerbosh5()
	{
		List<String> result = LearnerWithLabelRefinementViaPta.BronKerbosch(TestLearnerWithLabelRefinementViaPta.<String>constructMap(new Object[]{"A",new String[]{"B"},"B",new String[]{"A"},
				"C",new String[]{"D"},"D",new String[]{"C","E"},"E",new String[]{"D","F"},"F",new String[]{"E"}}),// thanks to http://stackoverflow.com/questions/450807/how-do-i-make-the-method-return-type-generic
				new LinkedList<String>(),Arrays.asList(new String[]{"A","B","C","D","E","F"}), new LinkedList<String>());
		Assert.assertEquals(Arrays.asList(new String[]{"A","B"}),result);
	}

	/** Nearly all-to-all in the second part. Of the part that is 1-to-1, C/E/D is the largest compatible set and hence is returned. */
	@Test
	public void testBronKerbosh6()
	{
		List<String> result = LearnerWithLabelRefinementViaPta.BronKerbosch(TestLearnerWithLabelRefinementViaPta.<String>constructMap(new Object[]{"A",new String[]{"B"},"B",new String[]{"A"},
				"C",new String[]{"D","E","F"},"D",new String[]{"C","E"},"E",new String[]{"F","C","D"},"F",new String[]{"E","C"}}),// thanks to http://stackoverflow.com/questions/450807/how-do-i-make-the-method-return-type-generic
				new LinkedList<String>(),Arrays.asList(new String[]{"A","B","C","D","E","F"}), new LinkedList<String>());
		Assert.assertEquals(Arrays.asList(new String[]{"C","E","D"}),result);
	}

	/** Nearly all-to-all in the second part, for the time being, the first part is returned. */
	@Test
	public void testBronKerbosh7()
	{
		List<String> result = LearnerWithLabelRefinementViaPta.BronKerbosch(TestLearnerWithLabelRefinementViaPta.<String>constructMap(new Object[]{"A",new String[]{"B"},"B",new String[]{"A"},
				"C",new String[]{"D","E","F"},"D",new String[]{"C","E","F"},"E",new String[]{"D","F","C"},"F",new String[]{"E","C","D"}}),// thanks to http://stackoverflow.com/questions/450807/how-do-i-make-the-method-return-type-generic
				new LinkedList<String>(),Arrays.asList(new String[]{"A","B","C","D","E","F"}), new LinkedList<String>());
		Assert.assertEquals(Arrays.asList(new String[]{"C","D","E","F"}),result);
	}

	/** Empty graph. */
	@Test
	public void testAbstractInitialPta0()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerGraph initialPta = new LearnerGraph(evalConfig.config);initialPta.initPTA();
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,initialPta,0);
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(learner.abstractInitialGraph('('));
		LearnerGraph expected = new LearnerGraph(evalConfig.config);expected.initPTA();
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}
	
	/** A few states. */
	@Test
	public void testAbstractInitialPta1()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,FsmParser.buildLearnerGraph("A-a()->B-a()->C","testAbstractInitialPta1",evalConfig.config,evalConfig.getLabelConverter()),0);
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(learner.abstractInitialGraph('('));
		LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-a->C","testAbstractInitialPta1",evalConfig.config,evalConfig.getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}
	
	/** Parallel transitions, all abstracting into a single one. */
	@Test
	public void testAbstractInitialPta2()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,FsmParser.buildLearnerGraph("A-a()->B-a()->C / A-a(2)->D / A-a(3)->B1","testAbstractInitialPta2",evalConfig.config,evalConfig.getLabelConverter()),0);
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(learner.abstractInitialGraph('('));
		LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-a->C","testAbstractInitialPta1",evalConfig.config,evalConfig.getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}

	/** Parallel transitions, all abstracting into a single one. */
	@Test
	public void testAbstractInitialPta3()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,FsmParser.buildLearnerGraph("A-a()->B-a()->C / A-a(2)->D / A-a(3)->B1 / A-b(0)->B2","testAbstractInitialPta3",evalConfig.config,evalConfig.getLabelConverter()),0);
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(learner.abstractInitialGraph('('));
		LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-a->C / A-b->B2","testAbstractInitialPta3",evalConfig.config,evalConfig.getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}

	/** Parallel transitions, all abstracting into a single one. Here state D is merged into B. */
	@Test
	public void testAbstractInitialPta4()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,FsmParser.buildLearnerGraph("A-a()->B-a()->C / A-a(2)->D / A-a(3)->B1 / A-b(0)->B2 / A-b(3)->B3","testAbstractInitialPta4",evalConfig.config,evalConfig.getLabelConverter()),0);
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(learner.abstractInitialGraph('('));
		LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-a->C / A-b->B3","testAbstractInitialPta3",evalConfig.config,evalConfig.getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}
	
	/** Parallel transitions, all abstracting into a single one. */
	@Test
	public void testAbstractInitialPta5()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,FsmParser.buildLearnerGraph("A-a()->B-a()->C / A-a(2)->D / A-a(3)->B1 / A-b(0)->B2 / A-b(3)->B3 / A-c(0)->E / A-c(2)->E1","testAbstractInitialPta5",evalConfig.config,evalConfig.getLabelConverter()),0);
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(learner.abstractInitialGraph('('));
		LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-a->C / A-b->D / A-c->E","testAbstractInitialPta5",evalConfig.config,evalConfig.getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}
	
	/** Parallel transitions, leading to a merge of a few states. */
	@Test
	public void testAbstractInitialPta6()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,FsmParser.buildLearnerGraph("A-a()->B-a()->C / A-a(2)->B1 / A-a(3)->B2 / A-b(0)->B3 / A-b(3)->B4 / A-c(3)->D / D-a(4)->C1 / D-b(3)->C2","testAbstractInitialPta6",evalConfig.config,evalConfig.getLabelConverter()),0);
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(learner.abstractInitialGraph('('));
		LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-a->C / A-b->E / A-c->D / D-a->C1 / D-b->C2","testAbstractInitialPta6",evalConfig.config,evalConfig.getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}
	
	/** Parallel transitions, leading to a merge of a few states. B3 and B4 end up getting merged. */
	@Test
	public void testAbstractInitialPta7()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,FsmParser.buildLearnerGraph("A-a()->B-a()->C / A-a(2)->B1 / A-a(3)->B2 / A-b(0)->B3-a(1)->T1 / B3-a(2)->T2 / B3-b(1)->G1-c(0)->I-a(0)->J / A-b(3)->B4-a(3)->T3 / B4-a(5)->T4 / B4-b(2)->G2-a(1)->H / A-c(3)->D / D-a(4)->C1 / D-b(3)->C2","testAbstractInitialPta6",evalConfig.config,evalConfig.getLabelConverter()),0);
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(learner.abstractInitialGraph('('));
		LearnerGraph expected = FsmParser.buildLearnerGraph("A-a->B-a->C / A-b->E-a->T / E-b->G-a->H / G-c->I-a->J / A-c->D / D-a->C1 / D-b->C2","testAbstractInitialPta6",evalConfig.config,evalConfig.getLabelConverter());
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}

	
	/** Label that cannot be abstracted. */
	@Test(expected = IllegalArgumentException.class)	
	public void testAbstractInitialPta_fail1()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,FsmParser.buildLearnerGraph("A-a()->B-a()->C / A-a(2)->D / A-a(3)->B / A-b(0)->B / A-b->B","testAbstractInitialPta3",evalConfig.config,evalConfig.getLabelConverter()),0);
		learner.abstractInitialGraph('(');
	}

	@Test
	public void testSplitLabels1()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerGraph initialPta = FsmParser.buildLearnerGraph("A-a()->B-a()->C / A-a(2)->B1 / A-a(3)->B2 / A-b(0)->B3-a(1)->T1 / B3-a(2)->T2 / B3-b(1)->G1-c(0)->I-a(0)->J / A-b(3)->B4-a(3)->T3 / B4-a(5)->T4 / B4-b(2)->G2-a(1)->H / A-c(3)->D / D-a(4)->C1 / D-b(3)->C2","testAbstractInitialPta6",evalConfig.config,evalConfig.getLabelConverter());
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,initialPta,0);
		learner.coregraph = learner.abstractInitialGraph('(');
		learner.constructMapOfInconsistentStates_fromRejectStates(LearnerWithLabelRefinementViaPta.createSetOfVertID(Arrays.asList(new VertID[]{VertexID.parseID("T1")})));
		LearnerGraph refinedGraph = learner.refineGraph();
		LearnerGraph expected = FsmParser.buildLearnerGraph("A-a,2->B-a,2->C / A-b->E-a,1->T1 / E-a,2->T2 / E-b->G-a,1->H / G-c->I-a,2->J / A-c->D / D-a,2->C1 / D-b->C2","testSplitLabels1",evalConfig.config,evalConfig.getLabelConverter());
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(refinedGraph);
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}

	@Test
	public void testSplitLabels2()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerGraph initialPta = FsmParser.buildLearnerGraph("A-a()->B-a()->C / A-a(2)->B1 / A-a(3)->B2 / A-b(0)->B3-a(1)->T1 / B3-a(2)->T2 / B3-b(1)->G1-c(0)->I-a(0)->J / A-b(3)->B4-a(3)->T3 / B4-a(5)->T4 / B4-b(2)->G2-a(1)->H / A-c(3)->D / D-a(4)->C1 / D-b(3)->C2","testSplitLabels2_pta",evalConfig.config,evalConfig.getLabelConverter());
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,initialPta,0);
		learner.coregraph = learner.abstractInitialGraph('(');
		learner.coregraph = MergeStates.mergeAndDeterminize_general(learner.coregraph, new StatePair(learner.coregraph.findVertex(VertexID.parseID("G1")),learner.coregraph.getInit()));learner.coregraph.setName("testSplitLabels2_refined");
		learner.constructMapOfInconsistentStates_fromRejectStates(LearnerWithLabelRefinementViaPta.createSetOfVertID(Arrays.asList(new VertID[]{VertexID.parseID("T1")})));
		LearnerGraph refinedGraph = learner.refineGraph();
		LearnerGraph expected = FsmParser.buildLearnerGraph("A-a,2->B-a,2->C / A-a,1->B / A-b->B3-b->A / B3-a,1->T1 / B3-a,2->T2 / A-c->D-a,2->J / D-b->C2","testSplitLabels2_expected",evalConfig.config,evalConfig.getLabelConverter());
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(refinedGraph);
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}
	
	@Test
	public void testSplitLabels3()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerGraph initialPta = FsmParser.buildLearnerGraph("A-a()->B-a()->C / A-a(2)->B1 / A-a(3)->B2 / A-b(0)->B3-a(1)->T1 / B3-a(2)->T2 / B3-b(1)->G1-c(0)->I-a(0)->J / A-b(3)->B4-a(1)->T3 / B4-a(5)->T4 / B4-b(2)->G2-a(1)->H / A-c(3)->D / D-a(4)->C1 / D-b(3)->C2","testSplitLabels3_pta",evalConfig.config,evalConfig.getLabelConverter());
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,initialPta,0);
		learner.coregraph = learner.abstractInitialGraph('(');
		learner.coregraph = MergeStates.mergeAndDeterminize_general(learner.coregraph, new StatePair(learner.coregraph.findVertex(VertexID.parseID("G1")),learner.coregraph.getInit()));learner.coregraph.setName("testSplitLabels3_refined");
		learner.constructMapOfInconsistentStates_fromRejectStates(LearnerWithLabelRefinementViaPta.createSetOfVertID(Arrays.asList(new VertID[]{VertexID.parseID("T1")})));
		LearnerGraph refinedGraph = learner.refineGraph();
		LearnerGraph expected = FsmParser.buildLearnerGraph("A-a,2->B-a,2->C / A-a,1->B / A-b,2->B3-b,2->A / B3-a,1->T2 / B3-a,2->T1 / A-b,1->B4-b,2->A / B4-a,1->T1 / B4-a,2->T1 / A-c->D-a,2->J / D-b,1->C2","testSplitLabels3_expected",evalConfig.config,evalConfig.getLabelConverter());
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(refinedGraph);
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}
	
	/** Integration test of forced mergers. */
	@Test
	public void testSplitLabels4()
	{
		LearnerEvaluationConfiguration evalConfig = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
		LearnerGraph initialPta = new LearnerGraph(evalConfig.config);initialPta.initPTA();initialPta.setName("testSplitLabels4_pta");
		Set<VertID> rejectStates = LearnerWithLabelRefinementViaPta.createSetOfVertID(null);
		for(int i=1;i<=4;++i)
		{
			CmpVertex bVertex = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"+i),initialPta.config);
			initialPta.transitionMatrix.put(bVertex, initialPta.createNewRow());
			initialPta.transitionMatrix.get(initialPta.getInit()).put(AbstractLearnerGraph.generateNewLabel("c("+i+")", initialPta.config, evalConfig.getLabelConverter()), bVertex);
			
			for(int j=1;j<=4;++j)
			{
				CmpVertex cVertex = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("C"+i+"_"+j),initialPta.config);
				initialPta.transitionMatrix.put(cVertex, initialPta.createNewRow());
				initialPta.transitionMatrix.get(bVertex).put(AbstractLearnerGraph.generateNewLabel("b("+j+")", initialPta.config, evalConfig.getLabelConverter()), cVertex);
				
				CmpVertex tailVertex = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("T"+i+"_"+j),initialPta.config);
				initialPta.transitionMatrix.put(tailVertex, initialPta.createNewRow());
				initialPta.transitionMatrix.get(cVertex).put(AbstractLearnerGraph.generateNewLabel("a("+0+")", initialPta.config, evalConfig.getLabelConverter()), tailVertex);
				if (j ==1 || j == 3)
					rejectStates.add(tailVertex);
			}
		}
				
		LearnerWithLabelRefinementViaPta learner = new LearnerWithLabelRefinementViaPta(evalConfig,initialPta,0);
		learner.coregraph = learner.abstractInitialGraph('(');
		learner.constructMapOfInconsistentStates_fromRejectStates(rejectStates);
		LearnerGraph refinedGraph = learner.refineGraph();refinedGraph.setName("testSplitLabels4_refined");
//		Visualiser.updateFrame(initialPta, refinedGraph);
//		Visualiser.waitForKey();
		LearnerGraph expected = FsmParser.buildLearnerGraph("A-c->P-b,1->B3-a->T5 / P-b,2->B2-a->T4","testSplitLabels4_expected",evalConfig.config,evalConfig.getLabelConverter());
		LearnerGraph abstractGraph = LearnerWithLabelRefinementViaPta.AbstractLabel.convertAbstractGraphToTextGraph(refinedGraph);
		DifferentFSMException diffEx = WMethod.checkM(expected, abstractGraph);
		if (diffEx != null)
			throw diffEx;
	}
	
	
}
