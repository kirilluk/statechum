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
import org.junit.runners.ParameterizedWithName;
import org.junit.runners.ParameterizedWithName.ParametersToString;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.linear.GD.ChangesCounter;
import statechum.analysis.learning.linear.GD.ChangesDisplay;
import statechum.analysis.learning.linear.GD.ChangesRecorder;
import statechum.analysis.learning.linear.GD.LearnerGraphMutator;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;


/**
 * @author kirill
 *
 */
@RunWith(ParameterizedWithName.class)
public class TestGD_Multithreaded {
	protected java.util.Map<CmpVertex,CmpVertex> newToOrig = null;

	/** Number of threads to use. */
	protected final int threadNumber;

	/** Label converter to use. */
	protected final ConvertALabel converter;

	protected final Configuration config;
	
	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		for(int i=1;i<8;++i)
		{
			result.add(new Object[]{new Integer(i),false});
			result.add(new Object[]{new Integer(i),true});
		}
		return result;
	}

	@ParametersToString
	public static String parametersToString(Integer threads, Boolean useArrays)
	{
		return ""+threads+" threads, arrays="+useArrays;
	}
	
	/** Creates the test class with the number of threads to create as an argument. */
	public TestGD_Multithreaded(int th, boolean useArrays)
	{
		threadNumber = th;
		if (useArrays)
		{
			config = Configuration.getDefaultConfiguration().copy();config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
			converter = new Transform.InternStringLabel();
		}
		else
		{
			config = Configuration.getDefaultConfiguration().copy();
			converter = null;
		}
	}
	
	@Before
	public void beforeTest()
	{
		newToOrig = new java.util.TreeMap<CmpVertex,CmpVertex>();
	}
	
	@Test
	public final void testInit()
	{
		LearnerGraph graphA = buildLearnerGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C","testFindKeyPairs1A",config,converter);
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C","testFindKeyPairs1B",config,converter);

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(graphA, graphB, threadNumber,config);
		Assert.assertEquals(graphA.transitionMatrix.size(),gd.statesOfA.size());
		Assert.assertEquals(graphB.transitionMatrix.size(),gd.statesOfB.size());
		Assert.assertEquals(graphA.transitionMatrix.size()+graphB.transitionMatrix.size(),gd.grCombined.getStateNumber());
		for(CmpVertex v:gd.statesOfA) Assert.assertTrue(graphA.transitionMatrix.containsKey(v));
		for(CmpVertex v:gd.statesOfB) Assert.assertTrue(graphB.transitionMatrix.containsKey(gd.newBToOrig.get(v)));
	}

	@Test
	public final void testFindKeyPairs1()
	{

		LearnerGraph graphA = buildLearnerGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C","testFindKeyPairs1A",config,converter);
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C","testFindKeyPairs1B",config,converter);

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(graphA, graphB, threadNumber,config);
		Assert.assertTrue(gd.identifyKeyPairs());
		//printListOfPairs(gd,gd.currentWave);
		for(PairScore pair:gd.frontWave)
		{
			CmpVertex A=pair.getQ(), B=gd.newBToOrig.get(pair.getR());
			Assert.assertTrue(B.getStringId().startsWith("@"));
			Assert.assertEquals(B.getStringId(),"@"+A.getStringId());
		}
		//printListOfPairs(gd,gd.currentWave);
		//printListOfPairs(gd,gd.frontWave);
	}
	
	@Test
	public final void testFindKeyPairs2()
	{
		LearnerGraph graphA = buildLearnerGraph("A-a->B-a->C-a->D-a->A","testFindKeyPairs2A",config,converter);
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B-a->@C-a->@A","testFindKeyPairs2B",config,converter);

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(graphA, graphB, threadNumber,config);
		Assert.assertFalse(gd.identifyKeyPairs());
		//printListOfPairs(gd,gd.frontWave);
	}

	@Test
	public final void testMakeSteps1()
	{
		Configuration configGraph = config.copy();
		LearnerGraph graphA = buildLearnerGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\n"+
				"B-c->B-d->B","testMakeSteps1A",configGraph,converter);
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C\n"+
				"@B-c->@B-d->@B","testMakeSteps1B",configGraph,converter);

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		Configuration configMakeSteps = configGraph.copy();configMakeSteps.setGdPropagateDet(true);
		gd.init(graphA, graphB, threadNumber,configMakeSteps);
		Assert.assertTrue(gd.identifyKeyPairs());
		//TestGD.printListOfPairs(gd.frontWave, gd.newToOrig);
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.makeSteps();gd.computeDifference(recorder);
		//printListOfPairs(gd,allKeyPairs);
		for(Entry<CmpVertex,CmpVertex> pair:gd.aTOb.entrySet())
		{
			CmpVertex A=pair.getKey(), B=gd.newBToOrig.get(pair.getValue());
			Assert.assertEquals(B.getStringId(),"@"+A.getStringId());
		}
	}

	/** Same as above but without deterministic propagation. */
	@Test
	public final void testMakeSteps2()
	{
		LearnerGraph graphA = buildLearnerGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\n"+
				"B-c->B-d->B","testMakeSteps1A",config,converter);
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C\n"+
				"@B-c->@B-d->@B","testMakeSteps1B",config,converter);

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		Configuration configMakeSteps = config.copy();configMakeSteps.setGdPropagateDet(false);
		gd.init(graphA, graphB, threadNumber,configMakeSteps);
		Assert.assertTrue(gd.identifyKeyPairs());
		//TestGD.printListOfPairs(gd.frontWave, gd.newToOrig);
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.makeSteps();gd.computeDifference(recorder);
		//printListOfPairs(gd,allKeyPairs);
		for(Entry<CmpVertex,CmpVertex> pair:gd.aTOb.entrySet())
		{
			CmpVertex A=pair.getKey(), B=gd.newBToOrig.get(pair.getValue());
			Assert.assertEquals(B.getStringId(),"@"+A.getStringId());
		}
	}

	/** Same as above, with deterministic propagation but much fewer key pairs - I only keep one of interest and 
	 * deterministic propagation should generate all the remaining pairs. */
	@Test
	public final void testMakeSteps3()
	{
		LearnerGraph graphA = buildLearnerGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\n"+
				"B-c->B-d->B","testMakeSteps1A",config,converter);
		LearnerGraph graphB = buildLearnerGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C\n"+
				"@B-c->@B-d->@B","testMakeSteps1B",config,converter);

		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		Configuration configMakeSteps = config.copy();configMakeSteps.setGdPropagateDet(true);
		gd.init(graphA, graphB, threadNumber,configMakeSteps);
		Assert.assertTrue(gd.identifyKeyPairs());
		gd.currentWave.clear();gd.statesInKeyPairs.clear();
		for(PairScore pair:gd.frontWave) 
			if (pair.getQ().equals(VertexID.parseID("B"))) 
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
			Assert.assertEquals(B.getStringId(),"@"+A.getStringId());
		}
		
	}

	
	/** Tests GD on the supplied two graphs
	 * 
	 * @param graphA first graph
	 * @param graphB second graph
	 * @param name prefix of the names to give to graphs. 
	 * @param expectedMatchedPairs the number of pairs of states which are expected to be matched
	 * @param argConfig configuration to use, default will be used if null.
	 */
	final void testComputeGD(String graphA,String graphB,String name, int expectedMatchedPairs, Configuration argConfig)
	{
		testComputeGD_oneway(graphA, graphB, name+"1", expectedMatchedPairs,argConfig);
		testComputeGD_oneway(graphB, graphA, name+"2", expectedMatchedPairs,argConfig);
		
		testNesting(graphA, graphB, name, argConfig);
		testNesting(graphB, graphA, name, argConfig);
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
		Configuration configOneWay = argConfig;
		if (configOneWay == null) configOneWay = Configuration.getDefaultConfiguration();
		LearnerGraph grA = buildLearnerGraph(graphA,name+"A",configOneWay,converter);
		LearnerGraph grB = buildLearnerGraph(graphB,name+"B",configOneWay,converter);
		grA.setIDNumbers();grB.setIDNumbers();// if a vertex uses vertex names such as P1000, it will clash with numbered vertices generated by Linear. This is why we ensure absence of clashes by updating identifier numbers.
		
		grA.pathroutines.checkConsistency(grA);grB.pathroutines.checkConsistency(grB);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		
		//Visualiser.updateFrame(gd.showGD(grA, grB, 1), grB);
		//Visualiser.updateFrame(gd.showGD(grA, grB, 1), grA);
	
		gd.init(grA, grB, threadNumber,configOneWay);
		gd.identifyKeyPairs();
		//printListOfPairs(gd, gd.currentWave);
		//printListOfPairs(gd, gd.frontWave);
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.makeSteps();gd.computeDifference(recorder);
		//printListOfPairs(gd, allKeyPairs);
		Assert.assertEquals(expectedMatchedPairs,gd.aTOb.size());
		LearnerGraph graph = buildLearnerGraph(graphA,name+"A",configOneWay,converter);
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
		Assert.assertNull(WMethod.checkM(graph, grB));
		Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	private final void testNesting(String graphA,String graphB,String name, Configuration argConfig)
	{
		Configuration configNesting = argConfig;if (configNesting == null) configNesting = config;
		LearnerGraph grA = buildLearnerGraph(graphA,name+"A",configNesting,converter);
		LearnerGraph grB = buildLearnerGraph(graphB,name+"B",configNesting,converter);
		grA.setIDNumbers();grB.setIDNumbers();
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
			LearnerGraph graph1 = buildLearnerGraph(graphA,name+"A",configNesting,converter);
			ChangesRecorder.applyGD(graph1, gd.computeGDToXML(grA, grB, threadNumber, TestGD.createDoc(), rec9,configNesting), converter);
			Assert.assertNull(WMethod.checkM(graph1, grB));Assert.assertEquals(grB.getStateNumber(),graph1.getStateNumber());
		}
		{// check that changes propagated to rec7 are correct.
			LearnerGraph graph2 = buildLearnerGraph(graphA,name+"A",configNesting,converter);
			ChangesRecorder.applyGD(graph2, rec7.writeGD(TestGD.createDoc()), converter);
			Assert.assertNull(WMethod.checkM(graph2, grB));Assert.assertEquals(grB.getStateNumber(),graph2.getStateNumber());
		}
		{// check that changes propagated to rec4 are correct.
			LearnerGraph graph3 = buildLearnerGraph(graphA,name+"A",configNesting,converter);
			ChangesRecorder.applyGD(graph3, rec4.writeGD(TestGD.createDoc()), converter);
			Assert.assertNull(WMethod.checkM(graph3, grB));Assert.assertEquals(grB.getStateNumber(),graph3.getStateNumber());
		}
		{// check that changes propagated to rec1 are correct.
			LearnerGraph graph4 = buildLearnerGraph(graphA,name+"A",configNesting,converter);
			ChangesRecorder.applyGD(graph4, rec1.writeGD(TestGD.createDoc()), converter);
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
				"@A-a->@B","testMakeSteps0",2,config);
	}

	@Test
	public final void testComputeGD1()
	{
		testComputeGD(
				"A-a->B\nA-b->B",
				"@A-a->@B\n@A-c->@B","testMakeSteps1",2,config);
	}

	/** Changes to the initial path. */ 
	@Test
	public final void testComputeGD9()
	{
		testComputeGD(
				"A-a->B\nA-b->B",
				"@Q-a->@E-a->@A-a->@B\n@A-c->@B","testMakeSteps9",2,config);
	}

	@Test
	public final void testComputeGD2()
	{
		testComputeGD(
				"A-a->B-a->C-a->D-a->A",
				"@A-a->@B-a->@C-a->@A","testComputeGD2",3,config);
	}
	
	@Test
	public final void testComputeGD3()
	{
		testComputeGD(
				"A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\n"+
				"B-c->B-d->B",
				"@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C\n"+
				"@B-c->@B-d->@B","testMakeSteps1",5,config);
	}

	@Test
	public final void testComputeGD4()
	{
		testComputeGD(
				"A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\nT-c-#C",
				"@A-a->@B-a-#@C\n@A-d-#@D\n@A-c->@A\n@B-b->@F-b->@G-c-#@C","testComputeGD4",6,config);
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
		Configuration configGD5a = config.copy();
		LearnerGraph grA = buildLearnerGraph(A6,"testComputeGD5a",configGD5a,converter);
		LearnerGraph grB = convertToNumerical(buildLearnerGraph(A6,"testComputeGD5a",configGD5a,converter));
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		//Visualiser.updateFrame(grA, grB);Visualiser.waitForKey();
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, counter,configGD5a);
		Assert.assertEquals(0,counter.getRemoved());
		Assert.assertEquals(0,counter.getAdded());
	}
	
	/** empty graph, with a single accept state (accept,accept). */
	@Test
	public final void testComputeGD5b_AA()
	{
		Configuration configGD5b = config.copy();configGD5b.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(configGD5b);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(configGD5b));
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, counter,configGD5b);
		Assert.assertEquals(0,counter.getRemoved());
		Assert.assertEquals(0,counter.getAdded());
	}
	
	/** empty graph, with a single reject state (reject,accept). */
	@Test
	public final void testComputeGD5b_RA()
	{
		Configuration configGD5b = config.copy();configGD5b.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(configGD5b);grA.getInit().setAccept(false);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(configGD5b));
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,configGD5b);
		LearnerGraph graph = new LearnerGraph(configGD5b);graph.setInit(null);graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single reject state (accept,reject). */
	@Test
	public final void testComputeGD5b_AR()
	{
		Configuration configGD5b = config.copy();configGD5b.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(configGD5b);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(configGD5b));grB.getInit().setAccept(false);
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,configGD5b);
		LearnerGraph graph = new LearnerGraph(configGD5b);graph.setInit(null);graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single reject state (accept,reject). */
	@Test
	public final void testComputeGD5_RR()
	{
		Configuration configGD5 = config.copy();configGD5.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(configGD5);grA.getInit().setAccept(false);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(configGD5));grB.getInit().setAccept(false);
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,configGD5);
		LearnerGraph graph = new LearnerGraph(configGD5);graph.setInit(null);graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single accept state. */
	@Test
	public final void testComputeGD5d_AA()
	{
		Configuration configGD5d = config;configGD5d.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(configGD5d);
		LearnerGraph grB = convertToNumerical(buildLearnerGraph(A6,"testComputeGD5b",configGD5d,converter));
		int transitionsCount = grB.pathroutines.countEdges();
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, counter,configGD5d);
		Assert.assertEquals(0,counter.getRemoved());
		Assert.assertEquals(transitionsCount,counter.getAdded());
	}
	
	/** empty graph, with a single reject state (reject,accept). */
	@Test
	public final void testComputeGD5d_RR()
	{
		Configuration configGD5d = config.copy();configGD5d.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = new LearnerGraph(configGD5d);grA.getInit().setAccept(false);
		LearnerGraph grB = convertToNumerical(buildLearnerGraph(A6,"testComputeGD5b",configGD5d,converter));
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,configGD5d);
		LearnerGraph graph = new LearnerGraph(configGD5d);graph.setInit(null);graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single accept state. */
	@Test
	public final void testComputeGD5e_AA()
	{
		Configuration confiGD5e = config.copy();
		LearnerGraph grA = buildLearnerGraph(A6,"testComputeGD5b",confiGD5e,converter);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(confiGD5e));
		int transitionsCount = grA.pathroutines.countEdges();
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, counter,confiGD5e);
		Assert.assertEquals(transitionsCount,counter.getRemoved());
		Assert.assertEquals(0,counter.getAdded());
	}
	
	/** empty graph, with a single reject state (accept,reject). */
	@Test
	public final void testComputeGD5e_RR()
	{
		Configuration configGD5e = config.copy();
		LearnerGraph grA = buildLearnerGraph(A6,"testComputeGD5b",configGD5e,converter);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(configGD5e));grB.getInit().setAccept(false);
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,configGD5e);
		LearnerGraph graph = buildLearnerGraph(A6,"testComputeGD5b",configGD5e,converter);
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	@Test
	public final void testComputeGD6a()
	{
		final String cd = "\nF-c->F-d->F\n";
		final String A=A6+cd, B = B6+cd.replace("F", "@F@").replace("U", "@U@");//B6+cd.replace("F", "F@").replace("U", "U@");
		testComputeGD(A, B, "testComputeGD6a", 9,config);
/*		
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
		testComputeGD(A, B, "testComputeGD6b", 9,config);
	}
	
	@Test
	public final void testComputeGD7a()
	{
		testComputeGD(A6, B6, "testComputeGD7a", 9,config);
	}
		
	@Test
	public final void testComputeGD7b()
	{
		testComputeGD(A6, C6, "testComputeGD7b", 9,config);
	}
	
	public final void testComputeGD8a()
	{
		Configuration configGD8 = config.copy();
		configGD8.setGdFailOnDuplicateNames(false);
		testComputeGD(A6, D6, "testComputeGD8a_", 9,configGD8);
	}
	
	@Test
	public final void testComputeGD8b()
	{
		final Configuration configGD8 = config.copy();
		configGD8.setGdFailOnDuplicateNames(true);
		statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() { public @Override void run() {
			testComputeGD(A6, D6, "testComputeGD8b_", 9,configGD8);
		}},IllegalArgumentException.class,"are shared between A and B");
	}
	
	/** Tests the correct handling of unmatched states. */
	@Test
	public final void testComputeGD10()
	{
		final Configuration configDup = config.copy();
		configDup.setGdMaxNumberOfStatesInCrossProduct(0);// this forces a fallback on the initial pair as the only key pair
		
		// Now a detailed check on the data in the patch
		LearnerGraph grA = buildLearnerGraph("P1000-a->P1000 / P1000-b->P1001 / P1001-a->P1002","testComputeGD10_A",configDup,converter);
		LearnerGraph grB = buildLearnerGraph("P1000-a->P1001 / P1001-a->P1002 / P1002-a-#N1000","testComputeGD10_B",configDup,converter);
		grA.setIDNumbers();grB.setIDNumbers();// if a vertex uses vertex names such as P1000, it will clash with numbered vertices generated by Linear. This is why we ensure absence of clashes by updating identifier numbers.
		
		grA.pathroutines.checkConsistency(grA);grB.pathroutines.checkConsistency(grB);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.computeGD(grA, grB, threadNumber,recorder,configDup);
		LearnerGraph graph = new LearnerGraph(grA,configDup);
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
		
		DifferentFSMException ex = WMethod.checkM_and_colours(grB, graph, VERTEX_COMPARISON_KIND.DEEP);
		Assert.assertNull(ex);
	}
		
	/** Tests the correct handling of unmatched states. This time P1002 is blue. */
	@Test
	public final void testComputeGD11()
	{
		final Configuration configDup = config.copy();
		configDup.setGdMaxNumberOfStatesInCrossProduct(0);// this forces a fallback on the initial pair as the only key pair
		
		// Now a detailed check on the data in the patch
		LearnerGraph grA = buildLearnerGraph("P1000-a->P1000 / P1000-b->P1001 / P1001-a->P1002","testComputeGD10_A",configDup,converter);
		LearnerGraph grB = buildLearnerGraph("P1000-a->P1001 / P1001-a->P1002 / P1002-a-#N1000","testComputeGD10_B",configDup,converter);
		grA.setIDNumbers();grB.setIDNumbers();// if a vertex uses vertex names such as P1000, it will clash with numbered vertices generated by Linear. This is why we ensure absence of clashes by updating identifier numbers.
		grA.findVertex(VertexID.parseID("P1002")).setColour(JUConstants.BLUE);
		
		grA.pathroutines.checkConsistency(grA);grB.pathroutines.checkConsistency(grB);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.computeGD(grA, grB, threadNumber,recorder,configDup);
		LearnerGraph graph = new LearnerGraph(grA,configDup);
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
		
		DifferentFSMException ex = WMethod.checkM_and_colours(grB, graph, VERTEX_COMPARISON_KIND.DEEP);
		Assert.assertNull(ex);
	}
		
	/** Tests the correct handling of unmatched states. This time P1003 is blue. */
	@Test
	public final void testComputeGD12()
	{
		final Configuration configDup = config.copy();
		configDup.setGdMaxNumberOfStatesInCrossProduct(0);// this forces a fallback on the initial pair as the only key pair
		
		// Now a detailed check on the data in the patch
		LearnerGraph grA = buildLearnerGraph("P1000-a->P1000 / P1000-b->P1001 / P1001-a->P1002-a->P1003-a->P1004","testComputeGD12_A",configDup,converter);
		LearnerGraph grB = buildLearnerGraph("P1000-a->P1001 / P1001-a->P1002 / P1002-a->P1003-a->P1004-a-#N1000","testComputeGD12_B",configDup,converter);
		grA.setIDNumbers();grB.setIDNumbers();// if a vertex uses vertex names such as P1000, it will clash with numbered vertices generated by Linear. This is why we ensure absence of clashes by updating identifier numbers.
		grA.findVertex(VertexID.parseID("P1003")).setColour(JUConstants.BLUE); 
		
		grA.pathroutines.checkConsistency(grA);grB.pathroutines.checkConsistency(grB);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.computeGD(grA, grB, threadNumber,recorder,configDup);
		LearnerGraph graph = new LearnerGraph(grA,configDup);
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
		
		DifferentFSMException ex = WMethod.checkM_and_colours(grB, graph, VERTEX_COMPARISON_KIND.DEEP);
		Assert.assertNull(ex);
	}

	final private void checkPatch(String graphA, String graphB, String name, int keyPairs, String expectedPatch, Configuration configToUse)
	{
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configToUse,converter);
		LearnerGraph grB = buildLearnerGraph(graphB,name+"_B",configToUse,converter);
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,configToUse);Assert.assertEquals(keyPairs,gd.aTOb.size());
		Assert.assertEquals(expectedPatch,recorder.toString());
	}
	
	/** Tests that associations between states are preserved. */
	@Test
	public final void testComputeGD13_A()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("AA-a->AB-b->AC", "D-a->E-b->F-c->AA-a->AB-b->AC", "testComputeGD13_A", 3,
				"mapping: AA - D\n"+
				"mapping: AB - E\n"+
				"mapping: AC - F\n"+
				"mapping: P1003 - AA\n"+
				"mapping: P1004 - AB\n"+
				"mapping: P1005 - AC\n"+
				"initial : AA\n"+
				"added  : AC - c -> P1003\n"+
				"added  : P1003 - a -> P1004\n"+
				"added  : P1004 - b -> P1005\n"
				,configNonFail);
	}
	
	/** Tests that associations between states are preserved. */
	@Test
	public final void testComputeGD13_B()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("AA-a->AB-b->AC / AA==INCOMPATIBLE==AB", "D-a->E-b->F-c->AA-a->AB-b->AC", "testComputeGD13_B", 3,
				"mapping: AA - D\n"+
				"mapping: AB - E\n"+
				"mapping: AC - F\n"+
				"mapping: P1003 - AA\n"+
				"mapping: P1004 - AB\n"+
				"mapping: P1005 - AC\n"+
				"initial : AA\n"+
				"added  : AC - c -> P1003\n"+
				"added  : P1003 - a -> P1004\n"+
				"added  : P1004 - b -> P1005\n"+
				"removed incompatibles: AA,AB with value INCOMPATIBLE\n"
				,configNonFail);
	}

	/** Tests that associations between states are preserved. */
	@Test
	public final void testComputeGD13_C()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("AA-a->AB-b->AC ", "D-a->E-b->F-c->AA-a->AB-b->AC/ D==INCOMPATIBLE==E", "testComputeGD13_C", 3,
				"mapping: AA - D\n"+
				"mapping: AB - E\n"+
				"mapping: AC - F\n"+
				"mapping: P1003 - AA\n"+
				"mapping: P1004 - AB\n"+
				"mapping: P1005 - AC\n"+
				"initial : AA\n"+
				"added  : AC - c -> P1003\n"+
				"added  : P1003 - a -> P1004\n"+
				"added  : P1004 - b -> P1005\n"+
				"added incompatibles: AA,AB with value INCOMPATIBLE\n"
				,configNonFail);
	}

	/** Tests that associations between states are correctly handled. */
	@Test
	public final void testComputeGD13_D()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("AA-a->AB-b->AC-d->M / AA==INCOMPATIBLE==AB / AC==INCOMPATIBLE==M", "D-a->E-b->F-e->N / F-c->AA-a->AB-b->AC/ D==INCOMPATIBLE==E / F==INCOMPATIBLE==N", "testComputeGD13_D",3,
				"mapping: AA - D\n"+
				"mapping: AB - E\n"+
				"mapping: AC - F\n"+
				"mapping: P1004 - AA\n"+
				"mapping: P1005 - AB\n"+
				"mapping: P1006 - AC\n"+
				"initial : AA\n"+
				"removed: AC - d -> M\n"+
				"added  : AC - c -> P1004\n"+
				"added  : AC - e -> N\n"+
				"added  : P1004 - a -> P1005\n"+
				"added  : P1005 - b -> P1006\n"+
				"removed incompatibles: M,AC with value INCOMPATIBLE\n"+
				"added incompatibles: AC,N with value INCOMPATIBLE\n"
				,configNonFail);
	}

	/** Adds the supplied pair of states as a key pair. */
	private static final void addToKeyPairs(GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd, CmpVertex stateM, CmpVertex stateN)
	{
		gd.aTOb.put(stateM, gd.origToNewB.get(stateN));gd.newToOrig.put(gd.origToNewB.get(stateN),stateM);
		gd.statesInKeyPairs.add(stateM);gd.statesInKeyPairs.add(gd.origToNewB.get(stateN));
	}
	
	/** Tests that associations between states are correctly handled. Here we have a case where two different states are associated by virtue of being part of key pairs. */
	@Test
	public final void testComputeGD13_E1()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="AA-a->AB-b->AC-d->M / AA==INCOMPATIBLE==AB / AC==INCOMPATIBLE==M", graphB="D-a->E-b->F-e->N / F-c->AA-a->AB-b->AC/ D==INCOMPATIBLE==E / F==INCOMPATIBLE==N", name = "testComputeGD13_E";
		String expectedPatch = 
				"mapping: M colour=red - N\n"+
				"mapping: AA - D\n"+
				"mapping: AB - E\n"+
				"mapping: AC - F\n"+
				"mapping: P1004 - AA\n"+
				"mapping: P1005 - AB\n"+
				"mapping: P1006 - AC\n"+
				"initial : AA\n"+
				"removed: AC - d -> M\n"+
				"added  : AC - c -> P1004\n"+
				"added  : AC - e -> M colour=red\n"+
				"added  : P1004 - a -> P1005\n"+
				"added  : P1005 - b -> P1006\n";
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter);
		grA.findVertex(VertexID.parseID("M")).setColour(JUConstants.RED);// ignored because it features in incompatibles.
		LearnerGraph grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		// add M-N as a key pair.
		CmpVertex stateM = grA.findVertex(VertexID.parseID("M")), stateN = grB.findVertex(VertexID.parseID("N"));
		addToKeyPairs(gd,stateM,stateN);
		
		Assert.assertEquals(4,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/** Tests that associations between states are correctly handled. Here we have a case where two different states are not associated by virtue of being part of key pairs. Hence they are treated separately. */
	@Test
	public final void testComputeGD13_E2()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="AA-a->AB-b->AC-d->M / AA==INCOMPATIBLE==AB / AC==INCOMPATIBLE==M", graphB="D-a->E-b->F-e->N / F-c->AA-a->AB-b->AC/ D==INCOMPATIBLE==E / F==INCOMPATIBLE==N", name = "testComputeGD13_E";
		String expectedPatch = 
				"mapping: AA - D\n"+
				"mapping: AB - E\n"+
				"mapping: AC - F\n"+
				"mapping: P1004 - AA\n"+
				"mapping: P1005 - AB\n"+
				"mapping: P1006 - AC\n"+
				"initial : AA\n"+
				"removed: AC - d -> M colour=red\n"+
				"added  : AC - c -> P1004\n"+
				"added  : AC - e -> N\n"+
				"added  : P1004 - a -> P1005\n"+
				"added  : P1005 - b -> P1006\n"+
				"removed incompatibles: M colour=red,AC with value INCOMPATIBLE\n"+
				"added incompatibles: AC,N with value INCOMPATIBLE\n";
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter);
		grA.findVertex(VertexID.parseID("M")).setColour(JUConstants.RED);// ignored because it features in incompatibles.
		LearnerGraph grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		Assert.assertEquals(3,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/** Tests that associations between states are correctly handled. */
	@Test
	public final void testComputeGD13_F()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("AA-a->AB-b->AC-d->M / AA==INCOMPATIBLE==AB / AC==INCOMPATIBLE==M", "D-a->E-b->F-e->M / F-c->AA-a->AB-b->AC/ D==INCOMPATIBLE==E / F==INCOMPATIBLE==M", "testComputeGD13_F",3,
				"mapping: AA - D\n"+
				"mapping: AB - E\n"+
				"mapping: AC - F\n"+
				"mapping: P1004 - AA\n"+
				"mapping: P1005 - AB\n"+
				"mapping: P1006 - AC\n"+
				"initial : AA\n"+
				"removed: AC - d -> M\n"+
				"added  : AC - c -> P1004\n"+
				"added  : AC - e -> M\n"+
				"added  : P1004 - a -> P1005\n"+
				"added  : P1005 - b -> P1006\n"
				,configNonFail);
	}
	
	/** Tests that associations between states are correctly handled. Same as the _D but states M and N have the same name M. 
	 * This leads to the association not entered into a diff C==INCOMPATIBLE==M because otherwise it would have been both added and removed between the same pair of states. 
	 */
	@Test
	public final void testComputeGD13_G()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="AA-a->AB-b->AC-d->M / AA==INCOMPATIBLE==AB / AC==INCOMPATIBLE==M", graphB="D-a->E-b->F-e->M / F-c->AA-a->AB-b->AC/ D==INCOMPATIBLE==E / F==INCOMPATIBLE==M", name = "testComputeGD13_G";
		String expectedPatch = 
				"mapping: AA colour=red - D colour=red\n"+ // both states are coloured because attributes between the respective states are copied
				"mapping: AB - E\n"+
				"mapping: AC - F\n"+
				"mapping: P1004 - AA\n"+
				"mapping: P1005 - AB\n"+
				"mapping: P1006 colour=amber - AC colour=amber\n"+// attributes are copied between the respective states
				"initial : AA colour=red\n"+
				"removed: AC - d -> M\n"+
				"added  : AC - c -> P1004\n"+
				"added  : AC - e -> M\n"+
				"added  : P1004 - a -> P1005\n"+
				"added  : P1005 - b -> P1006 colour=amber\n"+
				"added vertex:AA colour=red\n"+
				"added vertex:AB\n";
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter);
		grA.findVertex(VertexID.parseID("AB")).setColour(JUConstants.BLUE);// added as a vertex to the diff
		grA.findVertex(VertexID.parseID("AC")).setColour(JUConstants.RED);// ignored because it features in added transitions
		LearnerGraph grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		grB.findVertex(VertexID.parseID("AC")).setColour(JUConstants.AMBER);// ignored because it features in added transitions
		grB.findVertex(VertexID.parseID("D")).setColour(JUConstants.RED);// included in diff
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		gd.aTOb.put(grA.findVertex(VertexID.parseID("M")), gd.origToNewB.get(grB.findVertex(VertexID.parseID("M"))));
		Assert.assertEquals(4,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/** Tests that associations between states are preserved. Most states are not in key pairs, implemented by adding an initial state disconnected from others. */
	@Test
	public final void testComputeGD14_A()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("T-z->T / "+"A-a->B-b->C", "T-z->T / "+"D-a->E-b->F-c->A-a->B-b->C", "testComputeGD14_A", 1,
				"initial : T\n"+
				"added  : D - a -> E\n"+
				"added  : E - b -> F\n"+
				"added  : F - c -> A\n"
				,configNonFail);
	}
	
	/** Tests that associations between states are preserved. Most states are not in key pairs, implemented by adding an initial state disconnected from others. 
	 * Associations added/removed between different pairs of states. 
	 */
	@Test
	public final void testComputeGD14_B()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("T-z->T / "+"A-a->B-b->C / A==INCOMPATIBLE==B", "T-z->T / "+"D-a->E-b->F-c->A-a->B-b->C", "testComputeGD14_B", 1,
				"initial : T\n"+
				"added  : D - a -> E\n"+
				"added  : E - b -> F\n"+
				"added  : F - c -> A\n"+
				"removed incompatibles: A,B with value INCOMPATIBLE\n"			
				,configNonFail);
	}

	/** Tests that associations between states are preserved. Most states are not in key pairs, implemented by adding an initial state disconnected from others. Associations added/removed between different pairs of states. */
	@Test
	public final void testComputeGD14_C()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("T-z->T / "+"A-a->B-b->C ", "T-z->T / "+"D-a->E-b->F-c->A-a->B-b->C/ D==INCOMPATIBLE==E", "testComputeGD14_C", 1,
				"initial : T\n"+
				"added  : D - a -> E\n"+
				"added  : E - b -> F\n"+
				"added  : F - c -> A\n"+
				"added incompatibles: D,E with value INCOMPATIBLE\n"			
				,configNonFail);
	}

	/** Tests that associations between states are preserved. Most states are not in key pairs, implemented by adding an initial state disconnected from others. Associations added/removed between different pairs of states. */
	@Test
	public final void testComputeGD14_D()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("T-z->T / "+"A-a->B-b->C / A==INCOMPATIBLE==B ", "T-z->T / "+"D-a->E-b->F-c->A-a->B-b->C/ D==INCOMPATIBLE==E", "testComputeGD14_D",1, 
				"initial : T\n"+
				"added  : D - a -> E\n"+
				"added  : E - b -> F\n"+
				"added  : F - c -> A\n"+
				"removed incompatibles: A,B with value INCOMPATIBLE\n"+
				"added incompatibles: D,E with value INCOMPATIBLE\n"			
				,configNonFail);
	}

	/** Tests that associations between states are preserved. Most states are not in key pairs, implemented by adding an initial state disconnected from others. Association added/removed between the same pair of states. */
	@Test
	public final void testComputeGD15_B()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("T-z->T / "+"A-a->B-b->C / A==INCOMPATIBLE==B", "T-z->T / "+"D-a->E-b->F-c->A-a->B-b->C", "testComputeGD15_B", 1,
				"initial : T\n"+
				"added  : D - a -> E\n"+
				"added  : E - b -> F\n"+
				"added  : F - c -> A\n"+
				"removed incompatibles: A,B with value INCOMPATIBLE\n"				
				,configNonFail);
	}

	/** Tests that associations between states are preserved. Most states are not in key pairs, implemented by adding an initial state disconnected from others. Association added/removed between the same pair of states. */
	@Test
	public final void testComputeGD15_C()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("T-z->T / "+"A-a->B-b->C ", "T-z->T / "+"D-a->E-b->F-c->A-a->B-b->C/ A==INCOMPATIBLE==B", "testComputeGD15_C", 1,
				"initial : T\n"+
				"added  : D - a -> E\n"+
				"added  : E - b -> F\n"+
				"added  : F - c -> A\n"+
				"added incompatibles: A,B with value INCOMPATIBLE\n"				
				,configNonFail);
	}

	/** Tests that associations between states are preserved. Most states are not in key pairs, implemented by adding an initial state disconnected from others. Association added/removed between the same pair of states. */
	@Test
	public final void testComputeGD15_D()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		checkPatch("T-z->T / "+"A-a->B-b->C / A==INCOMPATIBLE==B ", "T-z->T / "+"D-a->E-b->F-c->A-a->B-b->C/ A==INCOMPATIBLE==B", "testComputeGD15_D", 1,
				"initial : T\n"+
				"added  : D - a -> E\n"+
				"added  : E - b -> F\n"+
				"added  : F - c -> A\n"				
				,configNonFail);
	}

	/**
	 * Tests that disconnected states in key pairs are handled correctly.
	 */
	@Test
	public final void testComputeGD16()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T", graphB="T-z->T", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"added vertex:A\n"+
				"added vertex:B\n";
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail),
				A2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail)
				;
		grA.transitionMatrix.put(A1, grA.createNewRow());grA.transitionMatrix.put(B1, grA.createNewRow());
		grB.transitionMatrix.put(A2, grB.createNewRow());grB.transitionMatrix.put(B2, grB.createNewRow());

		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		addToKeyPairs(gd, A1, A2);addToKeyPairs(gd, B1, B2);
		
		Assert.assertEquals(3,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that disconnected states outside key pairs are handled correctly.
	 */
	@Test
	public final void testComputeGD17()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T", graphB="T-z->T", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"added vertex:A\n"+
				"added vertex:B\n";
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail),
				A2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail)
				;
		grA.transitionMatrix.put(A1, grA.createNewRow());grA.transitionMatrix.put(B1, grA.createNewRow());
		grB.transitionMatrix.put(A2, grB.createNewRow());grB.transitionMatrix.put(B2, grB.createNewRow());

		
		//grA.findVertex(VertexID.parseID("AB")).setColour(JUConstants.BLUE);// added as a vertex to the diff
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		Assert.assertEquals(1,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that disconnected states in key pairs with associations between them are handled correctly.
	 */
	@Test
	public final void testComputeGD18()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T", graphB="T-z->T", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"removed incompatibles: A,B with value THEN\n"+
				"added vertex:A\n"+
				"added vertex:B\n";
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail),
				A2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail)
				;
		grA.transitionMatrix.put(A1, grA.createNewRow());grA.transitionMatrix.put(B1, grA.createNewRow());
		grB.transitionMatrix.put(A2, grB.createNewRow());grB.transitionMatrix.put(B2, grB.createNewRow());
		
		grA.addToCompatibility(A1, B1, PAIRCOMPATIBILITY.THEN);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		addToKeyPairs(gd, A1, A2);addToKeyPairs(gd, B1, B2);// in key pairs

		Assert.assertEquals(3,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that disconnected states in key pairs with associations between them are handled correctly.
	 */
	@Test
	public final void testComputeGD19()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T", graphB="T-z->T", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"added incompatibles: A,B with value THEN\n";
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail),
				A2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail)
				;
		grA.transitionMatrix.put(A1, grA.createNewRow());grA.transitionMatrix.put(B1, grA.createNewRow());
		grB.transitionMatrix.put(A2, grB.createNewRow());grB.transitionMatrix.put(B2, grB.createNewRow());
		
		grB.addToCompatibility(A1, B1, PAIRCOMPATIBILITY.THEN);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		addToKeyPairs(gd, A1, A2);addToKeyPairs(gd, B1, B2);// in key pairs

		Assert.assertEquals(3,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that disconnected states in key pairs with associations between them are handled correctly.
	 */
	@Test
	public final void testComputeGD20()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T", graphB="T-z->T", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"removed incompatibles: A,B with value THEN\n"+
				"added vertex:A\n"+
				"added vertex:B\n";
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail),
				A2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail)
				;
		grA.transitionMatrix.put(A1, grA.createNewRow());grA.transitionMatrix.put(B1, grA.createNewRow());
		grB.transitionMatrix.put(A2, grB.createNewRow());grB.transitionMatrix.put(B2, grB.createNewRow());
		
		grA.addToCompatibility(A1, B1, PAIRCOMPATIBILITY.THEN);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		addToKeyPairs(gd, A1, A2);addToKeyPairs(gd, B1, B2);// in key pairs

		Assert.assertEquals(3,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that disconnected states in key pairs with associations between them are handled correctly.
	 */
	@Test
	public final void testComputeGD21()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T", graphB="T-z->T", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"added vertex:A\n"+// these are added because there is no association to add and the vertex would otherwise be lost.
				"added vertex:B\n"// these are added because there is no association to add and the vertex would otherwise be lost.
		;
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail),
				A2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail)
				;
		grA.transitionMatrix.put(A1, grA.createNewRow());grA.transitionMatrix.put(B1, grA.createNewRow());
		grB.transitionMatrix.put(A2, grB.createNewRow());grB.transitionMatrix.put(B2, grB.createNewRow());
		
		grA.addToCompatibility(A1, B1, PAIRCOMPATIBILITY.THEN);// this is the key part of this test
		grB.addToCompatibility(A2, B2, PAIRCOMPATIBILITY.THEN);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		addToKeyPairs(gd, A1, A2);addToKeyPairs(gd, B1, B2);// in key pairs

		Assert.assertEquals(3,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that disconnected states not in key pairs with associations between them are handled correctly.
	 */
	@Test
	public final void testComputeGD22()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T", graphB="T-z->T", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"added incompatibles: A,B with value THEN\n";
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail),
				A2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail)
				;
		grA.transitionMatrix.put(A1, grA.createNewRow());grA.transitionMatrix.put(B1, grA.createNewRow());
		grB.transitionMatrix.put(A2, grB.createNewRow());grB.transitionMatrix.put(B2, grB.createNewRow());
		
		grB.addToCompatibility(A1, B1, PAIRCOMPATIBILITY.THEN);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		Assert.assertEquals(1,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that disconnected states not in key pairs with associations between them are handled correctly.
	 */
	@Test
	public final void testComputeGD23()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T", graphB="T-z->T", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"removed incompatibles: A,B with value THEN\n"+
				"added vertex:A\n"+
				"added vertex:B\n"
						;
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B1=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail),
				A2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"),configNonFail), B2=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"),configNonFail)
				;
		grA.transitionMatrix.put(A1, grA.createNewRow());grA.transitionMatrix.put(B1, grA.createNewRow());
		grB.transitionMatrix.put(A2, grB.createNewRow());grB.transitionMatrix.put(B2, grB.createNewRow());
		
		grA.addToCompatibility(A1, B1, PAIRCOMPATIBILITY.THEN);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		Assert.assertEquals(1,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	
	/**
	 * Tests that states in key pairs with different attributes between them are handled correctly.
	 */
	@Test
	public final void testComputeGD24()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T / A-a->A", graphB="T-z->T / A-a->A", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"added vertex:A\n"
		;
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=grA.findVertex(VertexID.parseID("A")), A2=grB.findVertex(VertexID.parseID("A"));
		
		A1.setAccept(false);// this is the key part of this test
		A2.setAccept(true);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		addToKeyPairs(gd, A1, A2);// in key pairs

		Assert.assertEquals(2,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that states in key pairs with different attributes between them are handled correctly.
	 */
	@Test
	public final void testComputeGD25()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T / A-a->A", graphB="T-z->T / A-a->A", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"
		;
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=grA.findVertex(VertexID.parseID("A")), A2=grB.findVertex(VertexID.parseID("A"));
		
		A1.setAccept(false);// this is the key part of this test
		A2.setAccept(false);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		addToKeyPairs(gd, A1, A2);// in key pairs

		Assert.assertEquals(2,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that states not in key pairs with different attributes between them are handled correctly.
	 */
	@Test
	public final void testComputeGD26()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T / A-a->A", graphB="T-z->T / A-a->A", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"added vertex:A\n"
		;
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=grA.findVertex(VertexID.parseID("A")), A2=grB.findVertex(VertexID.parseID("A"));
		
		A1.setAccept(false);// this is the key part of this test
		A2.setAccept(true);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		Assert.assertEquals(1,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that states not in key pairs with different attributes between them are handled correctly.
	 */
	@Test
	public final void testComputeGD27()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T / A-a->A", graphB="T-z->T / A-a->A", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"
		;
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=grA.findVertex(VertexID.parseID("A")), A2=grB.findVertex(VertexID.parseID("A"));
		
		A1.setAccept(false);// this is the key part of this test
		A2.setAccept(false);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		Assert.assertEquals(1,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that states not in key pairs with different attributes between them are handled correctly. Here a new transition is being added.
	 */
	@Test
	public final void testComputeGD28()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T / A-b->A", graphB="T-z->T / A-b->A / A-a->A", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"added  : A - a -> A\n"
		;
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=grA.findVertex(VertexID.parseID("A")), A2=grB.findVertex(VertexID.parseID("A"));
		
		A1.setAccept(false);// this is the key part of this test
		A2.setAccept(true);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		Assert.assertEquals(1,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	/**
	 * Tests that states not in key pairs with different attributes between them are handled correctly. Here a transition is being removed.
	 */
	@Test
	public final void testComputeGD29()
	{
		Configuration configNonFail = config.copy();configNonFail.setGdFailOnDuplicateNames(false);configNonFail.setGdMaxNumberOfStatesInCrossProduct(0);
		String graphA="T-z->T / A-b->A / A-a->A", graphB="T-z->T / A-b->A", name = "testComputeGD16";
		String expectedPatch = 
				"initial : T\n"+
				"removed: A - a -> A\n"+
				"added vertex:A\n"
		;
		LearnerGraph grA = buildLearnerGraph(graphA,name+"_A",configNonFail,converter),grB = buildLearnerGraph(graphB,name+"_B",configNonFail,converter);
		CmpVertex A1=grA.findVertex(VertexID.parseID("A")), A2=grB.findVertex(VertexID.parseID("A"));
		
		A1.setAccept(false);// this is the key part of this test
		A2.setAccept(true);// this is the key part of this test
		
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.init(grA, grB, threadNumber,configNonFail);
		gd.identifyKeyPairs();
		gd.makeSteps();
		
		Assert.assertEquals(1,gd.aTOb.size());
		gd.computeDifference(recorder);
		Assert.assertEquals(expectedPatch,recorder.toString());
	}

	@Test
	public final void testKeyPairLeadingToNoOutgoingTransitions_with_attribute_change()
	{
		testKeyPairLeadingToNoOutgoingTransitions_with_attribute_change("C");
	}
	
	@Test
	public final void testKeyPairLeadingToNoOutgoingTransitions_without_attribute_change()
	{
		testKeyPairLeadingToNoOutgoingTransitions_with_attribute_change(null);
	}
	
	public final void testKeyPairLeadingToNoOutgoingTransitions_with_attribute_change(String vertexToChange)
	{
		String graphAsText = "A-a->A / C-b->D / C-a->E", graphName = "testKeyPairLeadingToNoOutgoingTransitions_with_attribute_change_a";
		LearnerGraph grA = buildLearnerGraph(graphAsText,graphName,config,converter);
		if (vertexToChange != null) grA.findVertex(VertexID.parseID(vertexToChange)).setColour(JUConstants.AMBER);
		LearnerGraph graph = buildLearnerGraph(graphAsText,graphName,config,converter);
		if (vertexToChange != null) graph.findVertex(VertexID.parseID(vertexToChange)).setColour(JUConstants.AMBER);
		LearnerGraph grB = buildLearnerGraph("B-a->B","testKeyPairLeadingToNoOutgoingTransitions_with_attribute_change_b",config,converter);
		CmpVertex F=AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("F"), config);
		grB.transitionMatrix.put(F,grB.createNewRow());
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		LearnerGraph outcome = new LearnerGraph(config);
		ChangesRecorder patcher = new ChangesRecorder(null);
		
		gd.init(grA, grB, 1,config);
		gd.identifyKeyPairs();
		Assert.assertFalse(gd.fallbackToInitialPair);
		CmpVertex newF = gd.origToNewB.get(F);
		CmpVertex C = gd.grCombined.findVertex(VertexID.parseID("C"));

		// add C-newF as a new initial key pair.
		gd.frontWave.add(new PairScore(C,newF,1,0));
		gd.statesInKeyPairs.add(C);gd.statesInKeyPairs.add(newF);

		gd.makeSteps();
		ChangesDisplay recorder = new ChangesDisplay(patcher);
		gd.computeDifference(recorder);
		Assert.assertEquals(
				"mapping: A - B\n"+
				"mapping: C - F\n"+
				"initial : A\n"+
				"removed: C - a -> E\n"+
				"removed: C - b -> D\n"+
				"added vertex:C\n",
				recorder.toString());
		ChangesRecorder.applyGD_WithRelabelling(graph, patcher.writeGD(TestGD.createDoc()), converter,outcome);
		Assert.assertNull(WMethod.checkM(grB,graph));
		Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
		if (vertexToChange != null) Assert.assertEquals(JUConstants.AMBER,grA.findVertex(VertexID.parseID(vertexToChange)).getColour());
	}

	@Test
	public final void testCheckClash()
	{
		final LearnerGraph gr = buildLearnerGraph("P1000-a->P1001","testCheckClash",config,converter);
		final GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() { public @Override void run() {
			gd.init(gr, gr, 1,config);
		}},IllegalArgumentException.class,"duplicate vertex with ID P1000");
	}

	
	/** Generates a transition with the supplied between the supplied states.
	 * 
	 * @param from source state
	 * @param to target state
	 * @param label label to use
	 * @return machine corresponding to those two states.
	 */ 
	protected final String generateLine(String from, String to, String label)
	{
		return "\n"+from+"-"+label+"->"+to+"\n";
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
		testComputeGD(A6+additionA, B6+additionB, "testComputeGD9", 4,config);
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
			LearnerGraph grA = buildLearnerGraph(graphA,name+"A",config,converter);
			LearnerGraph grB = buildLearnerGraph(graphB,name+"B",config,converter);
	
			GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
			gd.init(grA, grB, threadNumber,config);
			gd.identifyKeyPairs();
			ChangesRecorder recorder = new ChangesRecorder(null);
			gd.makeSteps();gd.computeDifference(recorder);
			Assert.assertEquals(expectedMatchedPairs,gd.aTOb.size());
			LearnerGraph graph = buildLearnerGraph(graphA,name+"A",config,converter);
			ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
			Assert.assertNull(WMethod.checkM(graph, grB));
			Assert.assertNull(WMethod.checkM(graph, graph.findVertex(otherA), grB, grB.findVertex(otherB),WMethod.VERTEX_COMPARISON_KIND.NONE, true));
		}
	}

	@Test
	public final void testComputeGD_big2()
	{
		String fromMainToExtraA = generateLine("G", "T", "c")+generateLine("S","I","b")+additionA,
			fromMainToExtraB = generateLine("G@", "T@", "c")+generateLine("S@","I@","b")+additionB;
		testComputeGD(A6+fromMainToExtraA, B6+fromMainToExtraB, "testComputeGD9", 13,config);
	}

	@Test
	public final void testComputeGD_big3()
	{
		String A = generateLine("G", "T", "c")+generateLine("S","I","b")+additionA,
			B = generateLine("G@", "T@", "c")+generateLine("S@","I@","b")+additionB;
		testComputeGD(A, B, "testComputeGD_big3", 6,config);
	}

	@Test
	public final void testComputeGD_big4()
	{
		Configuration lotsOfKeyPairsConfig = config.copy();
		lotsOfKeyPairsConfig.setGdLowToHighRatio(0.3);// have to tune this - this test was originally written for a slightly different score computation routine.
		
		String A = A6+additionA,
			B = B6+generateLine("G@", "T@", "c")+generateLine("S@","I@","b")+additionB;
		testComputeGD(A, B, "testComputeGD_big4", 9,lotsOfKeyPairsConfig);
	}
	
	/** Tests emergency fallback. */
	@Test
	public final void testComputeGD_big4_fallback()
	{
		String A = A6+additionA,
			B = B6+generateLine("G@", "T@", "c")+generateLine("S@","I@","b")+additionB;
		Configuration fallbackConfig = config.copy();
		fallbackConfig.setGdMaxNumberOfStatesInCrossProduct(10);
		fallbackConfig.setGdLowToHighRatio(0.3);
		testComputeGD(A, B, "testComputeGD_big4", 9,fallbackConfig);
	}
	
	@Test
	public final void testComputeGD5b_RA_fallback()
	{
		Configuration configGD5b = config.copy();configGD5b.setGdFailOnDuplicateNames(false);
		configGD5b.setGdMaxNumberOfStatesInCrossProduct(0);
		LearnerGraph grA = new LearnerGraph(configGD5b);grA.getInit().setAccept(false);
		LearnerGraph grB = convertToNumerical(new LearnerGraph(configGD5b));
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,configGD5b);
		LearnerGraph graph = new LearnerGraph(configGD5b);graph.setInit(null);graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}

	@Test 
	public final void testComputeGD_small1()
	{
		Configuration niceConfig = config.copy();niceConfig.setGdLowToHighRatio(0.65);
		testComputeGD("A4-a->B4-a->C4-a-#D4", "A1-a->A1-b->B1-a->C1","testComputeGD_small1",1,niceConfig);
/*		
		String A="A4-a->B4-a->C4-a-#D4", B="A1-a->A1-b->B1-a->C1";
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
		Configuration niceConfig = config.copy();niceConfig.setGdLowToHighRatio(0.65);
		testComputeGD("A3-a->D3-b->D3-a->C3","A4-a->B4-a->C4-a-#D4", "testComputeGD_small2",3,config);
	}

	static LearnerGraphND checkDiffBetweenND(LearnerGraphND grA,LearnerGraphND grB, int expectedSizeOfATOb,int expectedSizeOfDuplicates,
			int threadNumber, Configuration conf, final ConvertALabel converter)
	{
		Configuration configCheckDiffBetweenND = conf.copy();
		Configuration cloneConfig = configCheckDiffBetweenND.copy();cloneConfig.setLearnerCloneGraph(true);
		AbstractLearnerGraph<List<CmpVertex>,LearnerGraphNDCachedData> copyOfA = grA.copy(cloneConfig), copyOfB = grB.copy(cloneConfig);
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		gd.init(grA, grB, threadNumber,configCheckDiffBetweenND);gd.identifyKeyPairs();
		//TestGD.printListOfPairs(gd.currentWave, gd.newBToOrig);
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.makeSteps();gd.computeDifference(recorder);
		//Visualiser.updateFrame(gd.showGD(grA, grB, 1), null);
		//Visualiser.updateFrame(grA, grB);
		Assert.assertEquals(expectedSizeOfATOb,gd.aTOb.size());
		Assert.assertEquals(expectedSizeOfDuplicates,gd.duplicates.size()); 
		LearnerGraphND graph = new LearnerGraphND(cloneConfig);AbstractLearnerGraph.copyGraphs(grA, graph);
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()), converter);
		Assert.assertNull(WMethod.checkM(grB, graph));
		
		// Now do the same as above, but renumber states to match grB
		AbstractLearnerGraph.copyGraphs(grA, graph);
		Configuration configMut = conf.copy();configCheckDiffBetweenND.setLearnerCloneGraph(false);
		LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> graphPatcher = new LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(graph,configMut,null);
		ChangesRecorder.loadDiff(graphPatcher, recorder.writeGD(TestGD.createDoc()), converter);
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
		Configuration configND1 = config.copy();
		Configuration cloneConfig = configND1.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\n"+common,name+"A",configND1, converter);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-s->G\nA-a->H-v->H\n"+common,name+"B",configND1, converter);
		checkDiffBetweenND(grA, grB, 6,0,threadNumber,configND1,converter);
	}
	
	/** A non-deterministic graph with a slightly different graph where not all states match exactly. */
	@Test
	public final void testComputeGD_ND2()
	{
		final String name = "testComputeGD_ND2";
		Configuration configND2 = config.copy();
		Configuration cloneConfig = configND2.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\n"+common,name+"A",configND2, converter);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\n"+common,name+"B",configND2, converter);
		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		
		LearnerGraphND result = checkDiffBetweenND(grA, grB, 6,0,threadNumber,configND2,converter);
/*
		ChangesDisplay disp = new ChangesDisplay(null);
		gd.computeGD(grA, grB, threadNumber, disp,config);System.out.println(disp.toString());*/
		Assert.assertEquals(JUConstants.GRAY,result.findVertex("B").getColour());
		Assert.assertEquals(origID,result.findVertex("B").getOrigState());
	}

	final static String nameC = "#", nameD = "@", testA="TA", testB="TB";

	/** Constructs a pair of graphs and runs a test. Used to check that key states which are not connected
	 * anywhere are added correctly.
	 */
	protected static GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>
		runTestCompute_ND3(Configuration argConfig, int expectedKeyPairs, int threadNumber, final ConvertALabel converter)
	{
		final String name = "testComputeGD_ND2";
		Configuration cloneConfig = argConfig.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\n"+common,name+"A",argConfig, converter);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		CmpVertex newStateA = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(testA), argConfig),
			newStateB = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(testB), argConfig);
		newStateA.setHighlight(true);newStateB.setDepth(45);
		grA.transitionMatrix.put(newStateA,grA.createNewRow());grA.transitionMatrix.put(newStateB,grA.createNewRow());

		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\n"+common,name+"B",argConfig, converter);
		CmpVertex newStateC = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(nameC), argConfig),
		newStateD = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(nameD), argConfig);
		newStateC.setColour(JUConstants.BLUE);newStateD.setOrigState(VertexID.parseID("P609"));
		grB.transitionMatrix.put(newStateC,grB.createNewRow());grB.transitionMatrix.put(newStateD,grB.createNewRow());

		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		
		LearnerGraphND result = checkDiffBetweenND(grA, grB,expectedKeyPairs,0,threadNumber,argConfig,converter);
		Assert.assertEquals(JUConstants.GRAY,result.findVertex("B").getColour());
		Assert.assertEquals(origID,result.findVertex("B").getOrigState());
		Assert.assertNull(result.findVertex(testA));
		Assert.assertNull(result.findVertex(testB));
		Assert.assertNotNull(result.findVertex(nameC));
		Assert.assertNotNull(result.findVertex(nameD));
		Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(newStateC,result.findVertex(nameC)));
		Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(newStateD,result.findVertex(nameD)));

		// The last check: ensure that disconnected states are or are not key pairs.
		// This chunk of code simply returns GD, the checking is performed by the caller of this method. 
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		gd.init(grA, grB, threadNumber,argConfig);gd.identifyKeyPairs();
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.makeSteps();gd.computeDifference(recorder);
		return gd;
	}
	
	
	/** A non-deterministic graph with a slightly different graph where not all states match exactly
	 * and there are disconnected states in both of the two graphs. 
	 */
	@Test
	public final void testComputeGD_ND3b()
	{
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = runTestCompute_ND3(config,6,threadNumber,converter);
		for(Entry<CmpVertex,CmpVertex> entry:gd.aTOb.entrySet()) 
		{
			Assert.assertFalse(entry.getKey().equals(VertexID.parseID(testA)));
			Assert.assertFalse(entry.getKey().equals(VertexID.parseID(testB)));
			Assert.assertFalse(gd.newBToOrig.get(entry.getValue()).equals(VertexID.parseID(nameC)));
			Assert.assertFalse(gd.newBToOrig.get(entry.getValue()).equals(VertexID.parseID(nameD)));
		}
	}

	
	/** A non-deterministic graph with a slightly different graph where not all states match exactly and
	 * there are some incompatible vertices.
	 */
	@Test
	public final void testComputeGD_ND4()
	{
		final String name = "testComputeGD_ND2";
		Configuration configND4 = config.copy();
		Configuration cloneConfig = configND4.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\nC-p->S\n"+common,name+"A",configND4, converter);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		grA.addToCompatibility(grA.findVertex("D"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\nC-q->T\n"+common,name+"B",configND4, converter);
		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		grB.addToCompatibility(grB.findVertex("B"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		
		LearnerGraphND result = checkDiffBetweenND(grA, grB, 6,0,threadNumber,configND4,converter);
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
		Configuration configND5 = config.copy();
		Configuration cloneConfig = configND5.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D\nU-a->U-b->R";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\nC-p->S\n"+common,name+"A",configND5, converter);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		
		grA.addToCompatibility(grA.findVertex("D"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grA.addToCompatibility(grA.findVertex("U"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grA.addToCompatibility(grA.findVertex("U"), grA.findVertex("R"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\nC-q->T\n"+common,name+"B",configND5, converter);
		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		grB.addToCompatibility(grB.findVertex("D"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grB.addToCompatibility(grB.findVertex("U"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
	
		LearnerGraphND result = checkDiffBetweenND(grA, grB, 8,0,threadNumber,configND5,converter);
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
		Configuration configND6 = config.copy();
		Configuration cloneConfig = configND6.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D\nU-a->U-b->R";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\nC-p->S\n"+common,name+"A",configND6, converter);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		grA.addToCompatibility(grA.findVertex("D"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grA.addToCompatibility(grA.findVertex("U"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grA.addToCompatibility(grA.findVertex("U"), grA.findVertex("R"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\nC-q->T\n"+common,name+"B",configND6, converter);
		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		grB.addToCompatibility(grB.findVertex("D"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grB.addToCompatibility(grB.findVertex("U"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grB.addToCompatibility(grB.findVertex("U"), grB.findVertex("R"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
	
		LearnerGraphND result = checkDiffBetweenND(grA, grB, 8,0,threadNumber,configND6,converter);
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
		Configuration configND7 = config.copy();
		Configuration cloneConfig = configND7.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D\nU-a->U-b->R";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\nC-p->S\n"+common,name+"A",configND7, converter);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		grA.addToCompatibility(grA.findVertex("D"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grA.addToCompatibility(grA.findVertex("U"), grA.findVertex("S"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\nC-q->T\n"+common,name+"B",configND7, converter);
		VertexID origID = VertexID.parseID("test orig");
		grB.findVertex("B").setColour(JUConstants.GRAY);grB.findVertex("B").setOrigState(origID);grB.findVertex("B").setDepth(3);
		grB.addToCompatibility(grB.findVertex("D"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grB.addToCompatibility(grB.findVertex("U"), grB.findVertex("T"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		grB.addToCompatibility(grB.findVertex("U"), grB.findVertex("R"),JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
	
		LearnerGraphND result = checkDiffBetweenND(grA, grB, 8,0,threadNumber,configND7,converter);
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
		LearnerGraph grA = LearnerGraph.loadGraph(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.RESOURCES)+File.separator+"LargeGraphs/experiment_500", config);
		LearnerGraph grB = LearnerGraph.loadGraph(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.RESOURCES)+File.separator+"LargeGraphs/experiment_501", config);
		GD gd = new GD();
		LearnerGraph graph = LearnerGraph.loadGraph(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.RESOURCES)+File.separator+"LargeGraphs/experiment_500", config);
		System.out.println("loaded");
		ChangesRecorder.applyGD(graph, gd.computeGDToXML(grA, grB, 1, createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
*/
	@Test
	public final void testCounter()
	{
		LearnerGraph grA = buildLearnerGraph("A-a->B\nA-b->B","testCounterA",config,converter);
		LearnerGraph grB = buildLearnerGraph("@A-a->@B\n@A-c->@B","testCounterB",config,converter);
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
		LearnerGraph grA = buildLearnerGraph("A-a->B\nA-b->B","testCounterA",config,converter);
		LearnerGraph grB = buildLearnerGraph("@A-a->@B\n@A-c->@B","testCounterB",config,converter);
		ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		gd.computeGD(grA, grB, threadNumber, recorder,config);
		Assert.assertEquals("mapping: A - @A\n"+
			"mapping: B - @B\n"+
			"initial : A\n"+
			"removed: A - b -> B\n"+
			"added  : A - c -> B\n",recorder.toString());
		Assert.assertEquals(1,counter.getAdded());Assert.assertEquals(1,counter.getRemoved());
	}
}
