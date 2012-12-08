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

package statechum.analysis.learning.linear;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraphND;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.linear.GD.ChangesCounter;
import statechum.analysis.learning.linear.GD.ChangesDisplay;
import statechum.analysis.learning.linear.GD.ChangesRecorder;
import statechum.analysis.learning.linear.GD.LearnerGraphMutator;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;


/**
 * @author kirill
 *
 */
@RunWith(Parameterized.class)
public class TestGD_Multithreaded {
	protected java.util.Map<CmpVertex,CmpVertex> newToOrig = null;

	/** Number of threads to use. */
	protected final int threadNumber;

	@Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		for(int i=1;i<8;++i)
			result.add(new Object[]{new Integer(i)});
		
		return result;
	}

	public static String parametersToString(Integer threads)
	{
		return ""+threads+" threads";
	}
	
	/** Creates the test class with the number of threads to create as an argument. */
	public TestGD_Multithreaded(int th)
	{
		threadNumber = th;
	}
	
	@Before
	public final void beforeTest()
	{
		newToOrig = new java.util.TreeMap<CmpVertex,CmpVertex>();
	}
	
	@Test
	public final void testInit()
	{
		LearnerGraph graphA = buildLearnerGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C","testFindKeyPairs1A",Configuration.getDefaultConfiguration());
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C","testFindKeyPairs1B",Configuration.getDefaultConfiguration());

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(graphA, graphB, threadNumber,Configuration.getDefaultConfiguration());
		Assert.assertEquals(graphA.transitionMatrix.size(),gd.statesOfA.size());
		Assert.assertEquals(graphB.transitionMatrix.size(),gd.statesOfB.size());
		Assert.assertEquals(graphA.transitionMatrix.size()+graphB.transitionMatrix.size(),gd.grCombined.getStateNumber());
		for(CmpVertex v:gd.statesOfA) Assert.assertTrue(graphA.transitionMatrix.containsKey(v));
		for(CmpVertex v:gd.statesOfB) Assert.assertTrue(graphB.transitionMatrix.containsKey(gd.newBToOrig.get(v)));
	}

	@Test
	public final void testFindKeyPairs1()
	{

		LearnerGraph graphA = buildLearnerGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C","testFindKeyPairs1A",Configuration.getDefaultConfiguration());
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C","testFindKeyPairs1B",Configuration.getDefaultConfiguration());

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(graphA, graphB, threadNumber,Configuration.getDefaultConfiguration());
		Assert.assertTrue(gd.identifyKeyPairs());
		//printListOfPairs(gd,gd.currentWave);
		for(PairScore pair:gd.frontWave)
		{
			CmpVertex A=pair.getQ(), B=gd.newBToOrig.get(pair.getR());
			Assert.assertTrue(B.getID().toString().startsWith("@"));
			Assert.assertEquals(B.getID().toString(),"@"+A.getID().toString());
		}
		//printListOfPairs(gd,gd.currentWave);
		//printListOfPairs(gd,gd.frontWave);
	}
	
	@Test
	public final void testFindKeyPairs2()
	{
		LearnerGraph graphA = buildLearnerGraph("A-a->B-a->C-a->D-a->A","testFindKeyPairs2A",Configuration.getDefaultConfiguration());
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B-a->@C-a->@A","testFindKeyPairs2B",Configuration.getDefaultConfiguration());

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(graphA, graphB, threadNumber,Configuration.getDefaultConfiguration());
		Assert.assertFalse(gd.identifyKeyPairs());
		//printListOfPairs(gd,gd.frontWave);
	}

	@Test
	public final void testMakeSteps1()
	{
		Configuration configGraph = Configuration.getDefaultConfiguration().copy();
		LearnerGraph graphA = buildLearnerGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\n"+
				"B-c->B-d->B","testMakeSteps1A",configGraph);
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C\n"+
				"@B-c->@B-d->@B","testMakeSteps1B",configGraph);

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		Configuration config = configGraph.copy();config.setGdPropagateDet(true);
		gd.init(graphA, graphB, threadNumber,config);
		Assert.assertTrue(gd.identifyKeyPairs());
		TestGD.printListOfPairs(gd.frontWave, gd.newToOrig);
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.makeSteps();gd.computeDifference(recorder);
		//printListOfPairs(gd,allKeyPairs);
		for(Entry<CmpVertex,CmpVertex> pair:gd.aTOb.entrySet())
		{
			CmpVertex A=pair.getKey(), B=gd.newBToOrig.get(pair.getValue());
			Assert.assertEquals(B.getID().toString(),"@"+A.getID().toString());
		}
	}

	/** Same as above but without deterministic propagation. */
	@Test
	public final void testMakeSteps2()
	{
		LearnerGraph graphA = buildLearnerGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\n"+
				"B-c->B-d->B","testMakeSteps1A",Configuration.getDefaultConfiguration());
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C\n"+
				"@B-c->@B-d->@B","testMakeSteps1B",Configuration.getDefaultConfiguration());

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdPropagateDet(false);
		gd.init(graphA, graphB, threadNumber,config);
		Assert.assertTrue(gd.identifyKeyPairs());
		//TestGD.printListOfPairs(gd.frontWave, gd.newToOrig);
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.makeSteps();gd.computeDifference(recorder);
		//printListOfPairs(gd,allKeyPairs);
		for(Entry<CmpVertex,CmpVertex> pair:gd.aTOb.entrySet())
		{
			CmpVertex A=pair.getKey(), B=gd.newBToOrig.get(pair.getValue());
			Assert.assertEquals(B.getID().toString(),"@"+A.getID().toString());
		}
	}

	/** Same as above, with deterministic propagation but much fewer key pairs - I only keep one of interest and 
	 * deterministic propagation should generate all the remaining pairs. */
	@Test
	public final void testMakeSteps3()
	{
		LearnerGraph graphA = buildLearnerGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\n"+
				"B-c->B-d->B","testMakeSteps1A",Configuration.getDefaultConfiguration());
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C\n"+
				"@B-c->@B-d->@B","testMakeSteps1B",Configuration.getDefaultConfiguration());

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdPropagateDet(true);
		gd.init(graphA, graphB, threadNumber,config);
		Assert.assertTrue(gd.identifyKeyPairs());
		gd.currentWave.clear();gd.statesInKeyPairs.clear();
		for(PairScore pair:gd.frontWave) 
			if (pair.getQ().getID().equals(VertexID.parseID("B"))) 
			{ 
				gd.currentWave.add(pair);gd.statesInKeyPairs.add(pair.getQ());gd.statesInKeyPairs.add(pair.getR());
			}
		gd.frontWave.clear();gd.frontWave.addAll(gd.currentWave);gd.currentWave.clear();
		Assert.assertEquals(1,gd.frontWave.size());// we should only have one pair

		gd.propagateDet(gd.forward.matrixForward,gd.inverse.matrixForward);
		Assert.assertEquals(5,gd.frontWave.size());// now all states of the first graph should be matched
		Assert.assertEquals(5,graphA.getStateNumber());
		for(PairScore pair:gd.frontWave)
		{
			CmpVertex A=pair.getQ(), B=gd.newBToOrig.get(pair.getR());
			Assert.assertEquals(B.getID().toString(),"@"+A.getID().toString());
		}
		
	}

	
	/** Tests GD on the supplied two graphs
	 * 
	 * @param graphA first graph
	 * @param graphB second graph
	 * @param name prefix of the names to give to graphs. 
	 * @param expectedMatchedPairs the number of pairs of states which are expected to be matched
	 * @param config configuration to use, default will be used if null.
	 */
	final void testComputeGD(String graphA,String graphB,String name, int expectedMatchedPairs, Configuration config)
	{
		testComputeGD_oneway(graphA, graphB, name+"1", expectedMatchedPairs,config);
		testComputeGD_oneway(graphB, graphA, name+"2", expectedMatchedPairs,config);
		
		testNesting(graphA, graphB, name, config);
		testNesting(graphB, graphA, name, config);
	}
	
	/** Tests GD on the supplied two graphs
	 * 
	 * @param graphA first graph
	 * @param graphB second graph
	 * @param name prefix of the names to give to graphs. 
	 * @param expectedMatchedPairs the number of pairs of states which are expected to be matched
	 * @param argConfig configuration to use, default will be used if null.
	 */
	private final void testComputeGD_oneway(String graphA,String graphB,String name, int expectedMatchedPairs, Configuration argConfig)
	{
		Configuration config = argConfig;
		if (config == null) config = Configuration.getDefaultConfiguration();
		LearnerGraph grA = buildLearnerGraph(graphA,name+"A",config);
		LearnerGraph grB = buildLearnerGraph(graphB,name+"B",config);

		grA.pathroutines.checkConsistency(grA);grB.pathroutines.checkConsistency(grB);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,config);
		gd.identifyKeyPairs();
		//printListOfPairs(gd, gd.currentWave);
		//printListOfPairs(gd, gd.frontWave);
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.makeSteps();gd.computeDifference(recorder);
		//printListOfPairs(gd, allKeyPairs);
		Assert.assertEquals(expectedMatchedPairs,gd.aTOb.size());
		LearnerGraph graph = buildLearnerGraph(graphA,name+"A",config);
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	private final void testNesting(String graphA,String graphB,String name, Configuration argConfig)
	{
		Configuration config = argConfig;if (config == null) config = Configuration.getDefaultConfiguration();
		LearnerGraph grA = buildLearnerGraph(graphA,name+"A",config);
		LearnerGraph grB = buildLearnerGraph(graphB,name+"B",config);

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		ChangesRecorder rec1 = 	new ChangesRecorder(null);
		ChangesDisplay  rec2 = new ChangesDisplay(rec1);
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> rec3 = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,rec2);
		ChangesRecorder rec4 = 	new ChangesRecorder(rec3);
		ChangesDisplay  rec5 = new ChangesDisplay(rec4);
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>  rec6 = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,rec5);
		ChangesRecorder rec7 = 	new ChangesRecorder(rec6);
		ChangesDisplay  rec8 = new ChangesDisplay(rec7);
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>  rec9 = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,rec8);
		{// compute GD and check that changes recorded by rec9 are correct.
			LearnerGraph graph1 = buildLearnerGraph(graphA,name+"A",config);
			ChangesRecorder.applyGD(graph1, gd.computeGDToXML(grA, grB, threadNumber, TestGD.createDoc(), rec9,Configuration.getDefaultConfiguration()));
			Assert.assertNull(WMethod.checkM(graph1, grB));Assert.assertEquals(grB.getStateNumber(),graph1.getStateNumber());
		}
		{// check that changes propagated to rec7 are correct.
			LearnerGraph graph2 = buildLearnerGraph(graphA,name+"A",config);
			ChangesRecorder.applyGD(graph2, rec7.writeGD(TestGD.createDoc()));
			Assert.assertNull(WMethod.checkM(graph2, grB));Assert.assertEquals(grB.getStateNumber(),graph2.getStateNumber());
		}
		{// check that changes propagated to rec4 are correct.
			LearnerGraph graph3 = buildLearnerGraph(graphA,name+"A",config);
			ChangesRecorder.applyGD(graph3, rec4.writeGD(TestGD.createDoc()));
			Assert.assertNull(WMethod.checkM(graph3, grB));Assert.assertEquals(grB.getStateNumber(),graph3.getStateNumber());
		}
		{// check that changes propagated to rec1 are correct.
			LearnerGraph graph4 = buildLearnerGraph(graphA,name+"A",config);
			ChangesRecorder.applyGD(graph4, rec1.writeGD(TestGD.createDoc()));
			Assert.assertNull(WMethod.checkM(graph4, grB));Assert.assertEquals(grB.getStateNumber(),graph4.getStateNumber());
		}
		String displayResult = rec8.toString();
		Assert.assertEquals(displayResult,rec5.toString());Assert.assertEquals(displayResult, rec2.toString());
		
		int added = rec9.getAdded(),removed = rec9.getRemoved();String result = rec9.toString();
		Assert.assertEquals(added,rec6.getAdded());Assert.assertEquals(removed,rec6.getRemoved());Assert.assertEquals(result,rec6.toString());
		Assert.assertEquals(added,rec3.getAdded());Assert.assertEquals(removed,rec3.getRemoved());Assert.assertEquals(result,rec3.toString());
	}
	
	@Test
	public final void testComputeGD0()
	{
		testComputeGD(
				"A-a->B",
				"@A-a->@B","testMakeSteps0",2,null);
	}

	@Test
	public final void testComputeGD1()
	{
		testComputeGD(
				"A-a->B\nA-b->B",
				"@A-a->@B\n@A-c->@B","testMakeSteps1",2,null);
	}

	/** Changes to the initial path. */ 
	@Test
	public final void testComputeGD9()
	{
		testComputeGD(
				"A-a->B\nA-b->B",
				"@Q-a->@E-a->@A-a->@B\n@A-c->@B","testMakeSteps9",2,null);
	}

	@Test
	public final void testComputeGD2()
	{
		testComputeGD(
				"A-a->B-a->C-a->D-a->A",
				"@A-a->@B-a->@C-a->@A","testComputeGD2",3,null);
	}
	
	@Test
	public final void testComputeGD3()
	{
		testComputeGD(
				"A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\n"+
				"B-c->B-d->B",
				"@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C\n"+
				"@B-c->@B-d->@B","testMakeSteps1",5,null);
	}

	@Test
	public final void testComputeGD4()
	{
		testComputeGD(
				"A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\nT-c-#C",
				"@A-a->@B-a-#@C\n@A-d-#@D\n@A-c->@A\n@B-b->@F-b->@G-c-#@C","testComputeGD4",6,null);
	}
	
	/** Trying to confuse key pair identification algorithm with large plateau (i.e. lots of similar states). */ 
	public static final String 
	A6=
		"A-a->B-a->C-a->D-a->A\n"+
		"A-b->E\nB-b->F\nC-b->G\nD-b->H\n"+
		"E-b->F-b->G-b->H-b->E\n"+
		"E-a->I\nF-a->J\nG-a->K\nH-a->L\n"+
		"I-a->J-a->K-a->L-a->I\n";

	public static final String B6=
		"A@-a->B@-a->C@-a->A@\n"+
		"A@-b->E@\nB@-b->F@\nC@-b->G@\n"+
		"E@-b->F@-b->G@-b->E@\n"+
		"E@-a->I@\nF@-a->J@\nG@-a->K@\n"+
		"I@-a->J@-a->K@-a->I@\n", C6=
		"@A-a->@B-a->@C-a->@A\n"+
		"@A-b->@E\n@B-b->@F\n@C-b->@G\n"+
		"@E-b->@F-b->@G-b->@E\n"+
		"@E-a->@I\n@F-a->@J\n@G-a->@K\n"+
		"@I-a->@J-a->@K-a->@I\n", D6=
		"A-a->B-a->C-a->A\n"+
		"A-b->E\nB-b->F\nC-b->G\n"+
		"E-b->F-b->G-b->E\n"+
		"E-a->I\nF-a->J\nG-a->K\n"+
		"I-a->J-a->K-a->I\n";

	public static LearnerGraph convertToNumerical(LearnerGraph graph)
	{
		LearnerGraph result = new LearnerGraph(graph.config);AbstractPathRoutines.convertToNumerical(graph, result);
		return result;
	}
	
	/** Same graph. */
	@Test
	public final void testComputeGD5a()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = buildLearnerGraph(A6,"testComputeGD5a",config);
		LearnerGraph grB = convertToNumerical(buildLearnerGraph(A6,"testComputeGD5a",config));
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		//Visualiser.updateFrame(grA, grB);Visualiser.waitForKey();
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, counter,config);
		Assert.assertEquals(0,counter.getRemoved());
		Assert.assertEquals(0,counter.getAdded());
	}
	
	/** empty graph, with a single accept state (accept,accept). */
	@Test
	public final void testComputeGD5b_AA()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(config);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(config));
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, counter,config);
		Assert.assertEquals(0,counter.getRemoved());
		Assert.assertEquals(0,counter.getAdded());
	}
	
	/** empty graph, with a single reject state (reject,accept). */
	@Test
	public final void testComputeGD5b_RA()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(config);grA.getInit().setAccept(false);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(config));
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,config);
		LearnerGraph graph = new LearnerGraph(config);graph.setInit(null);graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single reject state (accept,reject). */
	@Test
	public final void testComputeGD5b_AR()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(config);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(config));grB.getInit().setAccept(false);
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,config);
		LearnerGraph graph = new LearnerGraph(config);graph.setInit(null);graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single reject state (accept,reject). */
	@Test
	public final void testComputeGD5_RR()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(config);grA.getInit().setAccept(false);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(config));grB.getInit().setAccept(false);
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,config);
		LearnerGraph graph = new LearnerGraph(config);graph.setInit(null);graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single accept state. */
	@Test
	public final void testComputeGD5d_AA()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(config);
		LearnerGraph grB = convertToNumerical(buildLearnerGraph(A6,"testComputeGD5b",config));
		int transitionsCount = grB.pathroutines.countEdges();
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, counter,config);
		Assert.assertEquals(0,counter.getRemoved());
		Assert.assertEquals(transitionsCount,counter.getAdded());
	}
	
	/** empty graph, with a single reject state (reject,accept). */
	@Test
	public final void testComputeGD5d_RR()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(config);grA.getInit().setAccept(false);
		LearnerGraph grB = convertToNumerical(buildLearnerGraph(A6,"testComputeGD5b",config));
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,config);
		LearnerGraph graph = new LearnerGraph(config);graph.setInit(null);graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single accept state. */
	@Test
	public final void testComputeGD5e_AA()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = buildLearnerGraph(A6,"testComputeGD5b",config);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(config));
		int transitionsCount = grA.pathroutines.countEdges();
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, counter,config);
		Assert.assertEquals(transitionsCount,counter.getRemoved());
		Assert.assertEquals(0,counter.getAdded());
	}
	
	/** empty graph, with a single reject state (accept,reject). */
	@Test
	public final void testComputeGD5e_RR()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = buildLearnerGraph(A6,"testComputeGD5b",config);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(config));grB.getInit().setAccept(false);
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,config);
		LearnerGraph graph = buildLearnerGraph(A6,"testComputeGD5b",config);
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	@Test
	public final void testComputeGD6a()
	{
		final String cd = "\nF-c->F-d->F\n";
		final String A=A6+cd, B = B6+cd.replace("F", "@F@").replace("U", "@U@");//B6+cd.replace("F", "F@").replace("U", "U@");
		testComputeGD(A, B, "testComputeGD6a", 9,null);
/*		
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph grA = buildLearnerGraph(A,"testComputeGD6A",config);
		LearnerGraph grB = buildLearnerGraph(B,"testComputeGD6B",config);
		List<PairScore> allKeyPairs = new LinkedList<PairScore>();
		ChangesDisplay recorder = new ChangesDisplay();
		GD gd = new GD();//gd.computeGD(grA, grB, threadNumber, recorder);
		gd.init(grB, grA, threadNumber,newToOrig);
		gd.identifyKeyPairs();
		gd.makeSteps(recorder,allKeyPairs);
		printListOfPairs(gd, allKeyPairs);
		System.out.println(recorder);
*/
	}
		
	@Test
	public final void testComputeGD6b()
	{
		final String cd = "\nF-c->F-d->F\n";
		final String A=A6+cd, B = B6+cd.replace("F", "F@").replace("U", "U@");
		testComputeGD(A, B, "testComputeGD6b", 9,null);
	}
	
	@Test
	public final void testComputeGD7a()
	{
		testComputeGD(A6, B6, "testComputeGD7a", 9,null);
	}
		
	@Test
	public final void testComputeGD7b()
	{
		testComputeGD(A6, C6, "testComputeGD7b", 9,null);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testComputeGD8a()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		config.setGdFailOnDuplicateNames(false);
		testComputeGD(A6, D6, "testComputeGD8a_", 9,null);
	}
	
	@Test
	public final void testComputeGD8b()
	{
		final Configuration config = Configuration.getDefaultConfiguration().copy();
		config.setGdFailOnDuplicateNames(true);
		statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() { public @Override void run() {
			testComputeGD(A6, D6, "testComputeGD8b_", 9,null);
		}},IllegalArgumentException.class,"are shared between A and B");
	}
	
	/** Generates a long sequence of transitions with the supplied between the supplied states.
	 * 
	 * @param from source state
	 * @param to target state
	 * @param label label to use
	 * @param howMany how many transitions to add
	 * @return machine corresponding to those two states.
	 */ 
	protected final String generateLine(String from, String to, String label, int howMany)
	{
		StringBuffer result = new StringBuffer();String transition = "\n"+from+"-"+label+"->"+to+"\n";
		for(int i=0;i< howMany;++i) 
			result.append(transition);
		return result.toString();
	}
	
	protected final static String additionA = "\nT-a-#U\nT-b->S-c->T\nS-a-#P", additionB=additionA.replace("T", "T@").replace("U", "U@").replace("S", "S@").replace("P", "P@");

	/** A very pathological case: A6 and B6 do not have easily identifiable key pairs, hence
	 * if these are the only available states, GD picks anything it finds useful and keeps going.
	 * In this case, additions do have easily-identifiable key states and hence no states
	 * in A6/B6 are matched. Not sure if I should look for disconnected parts of a graph.
	 */
	@Test
	public final void testComputeGD_big1()
	{
		testComputeGD(A6+additionA, B6+additionB, "testComputeGD9", 4,null);
		final Configuration config = Configuration.getDefaultConfiguration();
		final String name = "testComputeGD9";
		final int expectedMatchedPairs = 4;
		for(String fourStrings []:new String[][]{
				new String[]{A6+additionA, B6+additionB,"T","T@"},
				new String[]{B6+additionB,A6+additionA,"T@","T"}
		})
		{
			assert fourStrings.length == 4;
			final String graphA = fourStrings[0], graphB = fourStrings[1],
				otherA=fourStrings[2],otherB=fourStrings[3];
			LearnerGraph grA = buildLearnerGraph(graphA,name+"A",config);
			LearnerGraph grB = buildLearnerGraph(graphB,name+"B",config);
	
			GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
			gd.init(grA, grB, threadNumber,config);
			gd.identifyKeyPairs();
			ChangesRecorder recorder = new ChangesRecorder(null);
			gd.makeSteps();gd.computeDifference(recorder);
			Assert.assertEquals(expectedMatchedPairs,gd.aTOb.size());
			LearnerGraph graph = buildLearnerGraph(graphA,name+"A",config);
			ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
			Assert.assertNull(WMethod.checkM(graph, grB));
			Assert.assertNull(WMethod.checkM(graph, graph.findVertex(otherA), grB, grB.findVertex(otherB),WMethod.VERTEX_COMPARISON_KIND.NONE));
		}
	}

	@Test
	public final void testComputeGD_big2()
	{
		String fromMainToExtraA = generateLine("G", "T", "c",10)+generateLine("S","I","b",10)+additionA,
			fromMainToExtraB = generateLine("G@", "T@", "c",10)+generateLine("S@","I@","b",10)+additionB;
		testComputeGD(A6+fromMainToExtraA, B6+fromMainToExtraB, "testComputeGD9", 13,null);
	}

	@Test
	public final void testComputeGD_big3()
	{
		String A = generateLine("G", "T", "c",10)+generateLine("S","I","b",10)+additionA,
			B = generateLine("G@", "T@", "c",20)+generateLine("S@","I@","b",20)+additionB;
		testComputeGD(A, B, "testComputeGD_big3", 6,null);
	}

	@Test
	public final void testComputeGD_big4()
	{
		Configuration lotsOfKeyPairsConfig = Configuration.getDefaultConfiguration().copy();
		lotsOfKeyPairsConfig.setGdLowToHighRatio(0.3);// have to tune this - this test was originally written for a slightly different score computation routine.
		
		String A = A6+additionA,
			B = B6+generateLine("G@", "T@", "c",20)+generateLine("S@","I@","b",20)+additionB;
		testComputeGD(A, B, "testComputeGD_big4", 9,lotsOfKeyPairsConfig);
	}
	
	/** Tests emergency fallback. */
	@Test
	public final void testComputeGD_big4_fallback()
	{
		String A = A6+additionA,
			B = B6+generateLine("G@", "T@", "c",20)+generateLine("S@","I@","b",20)+additionB;
		Configuration fallbackConfig = Configuration.getDefaultConfiguration().copy();
		fallbackConfig.setGdMaxNumberOfStatesInCrossProduct(10);
		fallbackConfig.setGdLowToHighRatio(0.3);
		testComputeGD(A, B, "testComputeGD_big4", 9,fallbackConfig);
	}
	
	@Test
	public final void testComputeGD5b_RA_fallback()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		config.setGdMaxNumberOfStatesInCrossProduct(0);
		LearnerGraph grA = new LearnerGraph(config);grA.getInit().setAccept(false);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(config));
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,config);
		LearnerGraph graph = new LearnerGraph(config);graph.setInit(null);graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}

	@Test 
	public final void testComputeGD_small1()
	{
		Configuration niceConfig = Configuration.getDefaultConfiguration().copy();niceConfig.setGdLowToHighRatio(0.65);
		testComputeGD("A4-a->B4-a->C4-a-#D4", "A1-a->A1-b->B1-a->C1","testComputeGD_small1",1,niceConfig);
/*		
		String A="A4-a->B4-a->C4-a-#D4", B="A1-a->A1-b->B1-a->C1";
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph grA = buildLearnerGraph(A,"testComputeGD6A",config);
		LearnerGraph grB = buildLearnerGraph(B,"testComputeGD6B",config);
		List<PairScore> allKeyPairs = new LinkedList<PairScore>();
		ChangesDisplay recorder = new ChangesDisplay(null);
		GD gd = new GD();//gd.computeGD(grA, grB, 1, recorder);
		gd.init(grB, grA, 1,newToOrig);
		gd.identifyKeyPairs();
		gd.makeSteps(recorder,allKeyPairs);
		TestGD.printListOfPairs(gd, allKeyPairs,newToOrig);
*/	
	}
	
	@Test 
	public final void testComputeGD_small2()
	{
		Configuration niceConfig = Configuration.getDefaultConfiguration().copy();niceConfig.setGdLowToHighRatio(0.65);
		testComputeGD("A3-a->D3-b->D3-a->C3","A4-a->B4-a->C4-a-#D4", "testComputeGD_small2",3,null);
	}

	private LearnerGraphND checkDiffBetweenND(LearnerGraphND grA,LearnerGraphND grB,
			int expectedSizeOfATOb,int expectedSizeOfDuplicates,
			Configuration conf)
	{
		Configuration config = conf.copy();
		Configuration cloneConfig = config.copy();cloneConfig.setLearnerCloneGraph(true);
		AbstractLearnerGraph<List<CmpVertex>,LearnerGraphNDCachedData> copyOfA = grA.copy(cloneConfig), copyOfB = grB.copy(cloneConfig);
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		gd.init(grA, grB, threadNumber,config);gd.identifyKeyPairs();
		//TestGD.printListOfPairs(gd.frontWave, gd.newBToOrig);
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.makeSteps();gd.computeDifference(recorder);
		//Visualiser.updateFrame(grA, grB);
		Assert.assertEquals(expectedSizeOfATOb,gd.aTOb.size());
		Assert.assertEquals(expectedSizeOfDuplicates,gd.duplicates.size()); 
		LearnerGraphND graph = new LearnerGraphND(cloneConfig);AbstractLearnerGraph.copyGraphs(grA, graph);
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(grB, graph));
		
		// Now do the same as above, but renumber states to match grB
		AbstractLearnerGraph.copyGraphs(grA, graph);
		Configuration configMut = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> graphPatcher = new LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(graph,configMut,null);
		ChangesRecorder.loadDiff(graphPatcher, recorder.writeGD(TestGD.createDoc()));
		graphPatcher.removeDanglingStates();
		LearnerGraphND result = new LearnerGraphND(configMut);
		graphPatcher.relabel(result);
		Assert.assertNull(WMethod.checkM_and_colours(grB, result,VERTEX_COMPARISON_KIND.DEEP));
		
		// Now check that the original graphs are unchanged - earlier versions of constructMergedVertex did not clone vertices
		Assert.assertNull(WMethod.checkM_and_colours(copyOfA, grA,VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertNull(WMethod.checkM_and_colours(copyOfB, grB,VERTEX_COMPARISON_KIND.DEEP));
		return result;
	}
	
	/** A non-deterministic graph with itself. */
	@Test
	public final void testComputeGD_ND1()
	{
		final String name = "testComputeGD_ND1";
		Configuration config = Configuration.getDefaultConfiguration().copy();
		Configuration cloneConfig = config.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\n"+common,name+"A",config);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-s->G\nA-a->H-v->H\n"+common,name+"B",config);
		checkDiffBetweenND(grA, grB, 6,0,config);
	}
	
	/** A non-deterministic graph with a slightly different graph where not all states match exactly. */
	@Test
	public final void testComputeGD_ND2()
	{
		final String name = "testComputeGD_ND2";
		Configuration config = Configuration.getDefaultConfiguration().copy();
		Configuration cloneConfig = config.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\n"+common,name+"A",config);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\n"+common,name+"B",config);
		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		
		LearnerGraphND result = checkDiffBetweenND(grA, grB, 4,0,config);
/*
		ChangesDisplay disp = new ChangesDisplay(null);
		gd.computeGD(grA, grB, threadNumber, disp,config);System.out.println(disp.toString());*/
		Assert.assertEquals(JUConstants.GRAY,result.findVertex("B").getColour());
		Assert.assertEquals(origID,result.findVertex("B").getOrigState());
	}

	final static String nameC = "#", nameD = "@";

	/** Constructs a pair of graphs and runs a test. Used to check that key states which are not connected
	 * anywhere are added correctly.
	 */
	private GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>
		runTestCompute_ND3(Configuration config, int expectedKeyPairs)
	{
		final String name = "testComputeGD_ND2";
		Configuration cloneConfig = config.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\n"+common,name+"A",config);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		CmpVertex newStateA = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("testA"), config),
			newStateB = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("testB"), config);
		newStateA.setHighlight(true);newStateB.setDepth(45);
		grA.transitionMatrix.put(newStateA,grA.createNewRow());grA.transitionMatrix.put(newStateB,grA.createNewRow());

		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\n"+common,name+"B",config);
		CmpVertex newStateC = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(nameC), config),
		newStateD = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(nameD), config);
		newStateC.setColour(JUConstants.BLUE);newStateD.setOrigState(VertexID.parseID("P609"));
		grB.transitionMatrix.put(newStateC,grB.createNewRow());grB.transitionMatrix.put(newStateD,grB.createNewRow());

		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		
		LearnerGraphND result = checkDiffBetweenND(grA, grB,expectedKeyPairs,0,config);
		Assert.assertEquals(JUConstants.GRAY,result.findVertex("B").getColour());
		Assert.assertEquals(origID,result.findVertex("B").getOrigState());
		Assert.assertNull(result.findVertex("testA"));
		Assert.assertNull(result.findVertex("testB"));
		Assert.assertNotNull(result.findVertex(nameC));
		Assert.assertNotNull(result.findVertex(nameD));
		Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(newStateC,result.findVertex(nameC)));
		Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(newStateD,result.findVertex(nameD)));

		// The last check: ensure that disconnected states are or are not key pairs.
		// This chunk of code simply returns GD, the checking is performed by the caller of this method. 
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		gd.init(grA, grB, threadNumber,config);gd.identifyKeyPairs();
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.makeSteps();gd.computeDifference(recorder);
		return gd;
	}
	
	/** A non-deterministic graph with a slightly different graph where not all states match exactly
	 * and there are disconnected states in both of the two graphs. 
	 */
	@Test
	public final void testComputeGD_ND3a()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		config.setGdKeyPairThreshold(1);config.setGdLowToHighRatio(1);
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = runTestCompute_ND3(config,5);
		boolean foundC = false, foundB = false;
		for(Entry<CmpVertex,CmpVertex> entry:gd.aTOb.entrySet()) 
		{
			Assert.assertFalse(entry.getKey().getID().equals(VertexID.parseID("testA")));
			if (entry.getKey().getID().equals(VertexID.parseID("testB"))) foundB = true;
			if (gd.newBToOrig.get(entry.getValue()).getID().equals(VertexID.parseID(nameC))) foundC = true;
			Assert.assertFalse(gd.newBToOrig.get(entry.getValue()).getID().equals(VertexID.parseID(nameD)));
		}
		Assert.assertTrue(foundB);
		Assert.assertTrue(foundC);
	}

	/** A modified version of testComputeGD_ND3a where attributes on a disconnected key pair are unchanged. */
	@Test
	public final void testComputeGD_ND3a_modified()
	{
		final String name = "testComputeGD_ND2";
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		config.setGdKeyPairThreshold(1);config.setGdLowToHighRatio(1);
		Configuration cloneConfig = config.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\n"+common,name+"A",config);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		CmpVertex newStateA = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("testA"), config);
		newStateA.setHighlight(true);
		grA.transitionMatrix.put(newStateA,grA.createNewRow());

		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\n"+common,name+"B",config);
		CmpVertex newStateC = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(nameC), config);
		DeterministicDirectedSparseGraph.copyVertexData(newStateA, newStateC);
		grB.transitionMatrix.put(newStateC,grB.createNewRow());

		LearnerGraphND result = checkDiffBetweenND(grA, grB,5,0,config);
		Assert.assertNull(result.findVertex("testA"));
		Assert.assertNotNull(result.findVertex(nameC));
		Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(newStateC,result.findVertex(nameC)));

		// The last check: ensure that disconnected states are or are not key pairs.
		// This chunk of code simply returns GD, the checking is performed by the caller of this method. 
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		gd.init(grA, grB, threadNumber,config);gd.identifyKeyPairs();
		ChangesDisplay display = new ChangesDisplay(null);
		ChangesRecorder recorder = new ChangesRecorder(display);
		gd.makeSteps();gd.computeDifference(recorder);
		
		boolean foundA = false;
		for(Entry<CmpVertex,CmpVertex> entry:gd.aTOb.entrySet()) 
		{
			if (entry.getKey().getID().equals(VertexID.parseID("testA"))) 
			{
				foundA = true;
				Assert.assertTrue(gd.newBToOrig.get(entry.getValue()).getID().equals(VertexID.parseID(nameC)));
			}
		}
		Assert.assertTrue(foundA);
		Assert.assertTrue(display.toString().contains("added vertex:testA"));// this has to be done because testA is disconnected and will be killed by removeDisconnected
		
		LearnerGraphND graph = new LearnerGraphND(config);AbstractLearnerGraph.copyGraphs(grA, graph);
		Configuration configMut = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> graphPatcher = new LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(graph,configMut,null);
		ChangesRecorder.loadDiff(graphPatcher, recorder.writeGD(TestGD.createDoc()));
		graphPatcher.removeDanglingStates();
		LearnerGraphND outcome = new LearnerGraphND(configMut);
		graphPatcher.relabel(outcome);
		
		Assert.assertTrue(DeterministicDirectedSparseGraph.nonIDAttributesEquals(outcome.findVertex(nameC),grA.findVertex("testA")));
	}
	
	/** A non-deterministic graph with a slightly different graph where not all states match exactly
	 * and there are disconnected states in both of the two graphs. 
	 */
	@Test
	public final void testComputeGD_ND3b()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = runTestCompute_ND3(config,4);
		for(Entry<CmpVertex,CmpVertex> entry:gd.aTOb.entrySet()) 
		{
			Assert.assertFalse(entry.getKey().getID().equals(VertexID.parseID("testA")));
			Assert.assertFalse(entry.getKey().getID().equals(VertexID.parseID("testB")));
			Assert.assertFalse(gd.newBToOrig.get(entry.getValue()).getID().equals(VertexID.parseID(nameC)));
			Assert.assertFalse(gd.newBToOrig.get(entry.getValue()).getID().equals(VertexID.parseID(nameD)));
		}
	}

	
	/** A non-deterministic graph with a slightly different graph where not all states match exactly and
	 * there are some incompatible vertices.
	 */
	@Test
	public final void testComputeGD_ND4()
	{
		final String name = "testComputeGD_ND2";
		Configuration config = Configuration.getDefaultConfiguration().copy();
		Configuration cloneConfig = config.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\nC-p->S\n"+common,name+"A",config);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		grA.addToCompatibility(grA.findVertex("D"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\nC-q->T\n"+common,name+"B",config);
		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		grB.addToCompatibility(grB.findVertex("B"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		
		LearnerGraphND result = checkDiffBetweenND(grA, grB, 3,0,config);
/*
		ChangesDisplay disp = new ChangesDisplay(null);
		gd.computeGD(grA, grB, threadNumber, disp,config);System.out.println(disp.toString());*/
		Assert.assertEquals(JUConstants.GRAY,result.findVertex("B").getColour());
		Assert.assertEquals(origID,result.findVertex("B").getOrigState());
		Assert.assertTrue(result.pairCompatibility.compatibility.get(result.findVertex("T")).get(result.findVertex("B")) == JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
	}

	/** A non-deterministic graph with a slightly different graph where not all states match exactly and
	 * there are some incompatible vertices.
	 * @throws IncompatibleStatesException 
	 */
	@Test
	public final void testComputeGD_ND5()
	{
		final String name = "testComputeGD_ND2";
		Configuration config = Configuration.getDefaultConfiguration().copy();
		Configuration cloneConfig = config.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D\nU-a->U-b->R";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\nC-p->S\n"+common,name+"A",config);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		
		grA.addToCompatibility(grA.findVertex("D"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grA.addToCompatibility(grA.findVertex("U"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grA.addToCompatibility(grA.findVertex("U"), grA.findVertex("R"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\nC-q->T\n"+common,name+"B",config);
		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		grB.addToCompatibility(grB.findVertex("D"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grB.addToCompatibility(grB.findVertex("U"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
	
		LearnerGraphND result = checkDiffBetweenND(grA, grB, 5,0,config);
		Assert.assertEquals(JUConstants.GRAY,result.findVertex("B").getColour());
		Assert.assertEquals(origID,result.findVertex("B").getOrigState());
		Assert.assertTrue(result.pairCompatibility.compatibility.get(result.findVertex("T")).get(result.findVertex("D")) == JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertTrue(result.pairCompatibility.compatibility.get(result.findVertex("U")).get(result.findVertex("T")) == JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
	}

	/** A non-deterministic graph with a slightly different graph where not all states match exactly and
	 * there are some incompatible vertices.
	 */
	@Test
	public final void testComputeGD_ND6()
	{
		final String name = "testComputeGD_ND2";
		Configuration config = Configuration.getDefaultConfiguration().copy();
		Configuration cloneConfig = config.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D\nU-a->U-b->R";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\nC-p->S\n"+common,name+"A",config);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		grA.addToCompatibility(grA.findVertex("D"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grA.addToCompatibility(grA.findVertex("U"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grA.addToCompatibility(grA.findVertex("U"), grA.findVertex("R"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\nC-q->T\n"+common,name+"B",config);
		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		grB.addToCompatibility(grB.findVertex("D"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grB.addToCompatibility(grB.findVertex("U"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grB.addToCompatibility(grB.findVertex("U"), grB.findVertex("R"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
	
		LearnerGraphND result = checkDiffBetweenND(grA, grB, 5,0,config);
		Assert.assertEquals(JUConstants.GRAY,result.findVertex("B").getColour());
		Assert.assertEquals(origID,result.findVertex("B").getOrigState());
		Assert.assertTrue(result.pairCompatibility.compatibility.get(result.findVertex("T")).get(result.findVertex("D")) == JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertTrue(result.pairCompatibility.compatibility.get(result.findVertex("U")).get(result.findVertex("T")) == JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertTrue(result.pairCompatibility.compatibility.get(result.findVertex("U")).get(result.findVertex("R")) == JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
	}

	/** A non-deterministic graph with a slightly different graph where not all states match exactly and
	 * there are some incompatible vertices.
	 */
	@Test
	public final void testComputeGD_ND7()
	{
		final String name = "testComputeGD_ND2";
		Configuration config = Configuration.getDefaultConfiguration().copy();
		Configuration cloneConfig = config.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D\nU-a->U-b->R";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\nC-p->S\n"+common,name+"A",config);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		grA.addToCompatibility(grA.findVertex("D"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grA.addToCompatibility(grA.findVertex("U"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\nC-q->T\n"+common,name+"B",config);
		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		grB.addToCompatibility(grB.findVertex("D"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grB.addToCompatibility(grB.findVertex("U"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grB.addToCompatibility(grB.findVertex("U"), grB.findVertex("R"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
	
		LearnerGraphND result = checkDiffBetweenND(grA, grB, 5,0,config);
		Assert.assertEquals(JUConstants.GRAY,result.findVertex("B").getColour());
		Assert.assertEquals(origID,result.findVertex("B").getOrigState());
		Assert.assertTrue(result.pairCompatibility.compatibility.get(result.findVertex("T")).get(result.findVertex("D")) == JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertTrue(result.pairCompatibility.compatibility.get(result.findVertex("U")).get(result.findVertex("T")) == JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		Assert.assertTrue(result.pairCompatibility.compatibility.get(result.findVertex("U")).get(result.findVertex("R")) == JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
	}

	/*
	@Test
	public final void testComputeGD_big5()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = LearnerGraph.loadGraph("resources/LargeGraphs/experiment_500", config);
		LearnerGraph grB = LearnerGraph.loadGraph("resources/LargeGraphs/experiment_501", config);
		GD gd = new GD();
		LearnerGraph graph = LearnerGraph.loadGraph("resources/LargeGraphs/experiment_500", config);
		System.out.println("loaded");
		ChangesRecorder.applyGD(graph, gd.computeGDToXML(grA, grB, 1, createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
*/
	@Test
	public final void testCounter()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph grA = buildLearnerGraph("A-a->B\nA-b->B","testCounterA",config);
		LearnerGraph grB = buildLearnerGraph("@A-a->@B\n@A-c->@B","testCounterB",config);
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, counter,config);
		Assert.assertEquals(1,counter.getRemoved());
		Assert.assertEquals(1,counter.getAdded());
		Assert.assertEquals("diff of testCounterB to testCounterA is 100% of testCounterB",counter.toString());
	}

	/** Tests ChangesDisplay and nesting of change observers. */ 
	@Test
	public final void testDisplay()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph grA = buildLearnerGraph("A-a->B\nA-b->B","testCounterA",config);
		LearnerGraph grB = buildLearnerGraph("@A-a->@B\n@A-c->@B","testCounterB",config);
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,config);
		Assert.assertEquals("removed: A - b -> B\n"+
				"added  : A - c -> B\nmapping: A - @A\nmapping: B - @B\ninitial : A\n",recorder.toString());
		Assert.assertEquals(1,counter.getAdded());Assert.assertEquals(1,counter.getRemoved());
	}
}
