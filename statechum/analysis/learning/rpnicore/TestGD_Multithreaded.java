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

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.rpnicore.GD.ChangesCounter;
import statechum.analysis.learning.rpnicore.GD.ChangesDisplay;
import statechum.analysis.learning.rpnicore.GD.ChangesRecorder;


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
		LearnerGraph graphA = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C","testFindKeyPairs1A"),Configuration.getDefaultConfiguration());
		LearnerGraph graphB = new LearnerGraph(buildGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C","testFindKeyPairs1B"),Configuration.getDefaultConfiguration());

		GD gd = new GD();
		gd.init(graphA, graphB, threadNumber);
		Assert.assertEquals(graphA.transitionMatrix.size(),gd.statesOfA.size());
		Assert.assertEquals(graphA.transitionMatrix.size(),gd.copyOfA.getStateNumber());
		Assert.assertEquals(graphB.transitionMatrix.size(),gd.statesOfB.size());
		Assert.assertEquals(graphA.transitionMatrix.size()+graphB.transitionMatrix.size(),gd.grCombined.getStateNumber());
		for(CmpVertex v:gd.statesOfA) Assert.assertTrue(graphA.transitionMatrix.containsKey(v));
		for(CmpVertex v:gd.statesOfB) Assert.assertTrue(graphB.transitionMatrix.containsKey(gd.newBToOrig.get(v)));
	}

	@Test
	public final void testFindKeyPairs1()
	{

		LearnerGraph graphA = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C","testFindKeyPairs1A"),Configuration.getDefaultConfiguration());
		LearnerGraph graphB = new LearnerGraph(buildGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C","testFindKeyPairs1B"),Configuration.getDefaultConfiguration());

		GD gd = new GD();
		gd.init(graphA, graphB, threadNumber);
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
		LearnerGraph graphA = new LearnerGraph(buildGraph("A-a->B-a->C-a->D-a->A","testFindKeyPairs2A"),Configuration.getDefaultConfiguration());
		LearnerGraph graphB = new LearnerGraph(buildGraph("@A-a->@B-a->@C-a->@A","testFindKeyPairs2B"),Configuration.getDefaultConfiguration());

		GD gd = new GD();
		gd.init(graphA, graphB, threadNumber);
		Assert.assertFalse(gd.identifyKeyPairs());
		//printListOfPairs(gd,gd.frontWave);
	}

	@Test
	public final void testMakeSteps1()
	{
		LearnerGraph graphA = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\n"+
				"B-c->B-d->B","testMakeSteps1A"),Configuration.getDefaultConfiguration());
		LearnerGraph graphB = new LearnerGraph(buildGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C\n"+
				"@B-c->@B-d->@B","testMakeSteps1B"),Configuration.getDefaultConfiguration());

		GD gd = new GD();
		gd.init(graphA, graphB, threadNumber);
		Assert.assertTrue(gd.identifyKeyPairs());
		List<PairScore> allKeyPairs = new LinkedList<PairScore>();
		ChangesRecorder recorder = new ChangesRecorder(null);
		gd.makeSteps(recorder,allKeyPairs);
		//printListOfPairs(gd,allKeyPairs);
		for(PairScore pair:allKeyPairs)
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
		testComputeGD_oneway(graphA, graphB, name, expectedMatchedPairs,config);
		testComputeGD_oneway(graphB, graphA, name, expectedMatchedPairs,config);
		
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
		LearnerGraph grA = new LearnerGraph(buildGraph(graphA,name+"A"),config);
		LearnerGraph grB = new LearnerGraph(buildGraph(graphB,name+"B"),config);

		GD gd = new GD();
		gd.init(grA, grB, threadNumber);
		gd.identifyKeyPairs();
		//printListOfPairs(gd, gd.currentWave);
		//printListOfPairs(gd, gd.frontWave);
		ChangesRecorder recorder = new ChangesRecorder(null);
		List<PairScore> allKeyPairs = new LinkedList<PairScore>();
		gd.makeSteps(recorder,allKeyPairs);
		//printListOfPairs(gd, allKeyPairs);
		Assert.assertEquals(expectedMatchedPairs,allKeyPairs.size());
		LearnerGraph graph = new LearnerGraph(buildGraph(graphA,name+"A"),config);
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	private final void testNesting(String graphA,String graphB,String name, Configuration argConfig)
	{
		Configuration config = argConfig;if (config == null) config = Configuration.getDefaultConfiguration();
		LearnerGraph grA = new LearnerGraph(buildGraph(graphA,name+"A"),config);
		LearnerGraph grB = new LearnerGraph(buildGraph(graphB,name+"B"),config);

		GD gd = new GD();
		ChangesRecorder rec1 = 	new ChangesRecorder(null);
		ChangesDisplay  rec2 = new ChangesDisplay(rec1);
		ChangesCounter  rec3 = new ChangesCounter(grA,grB,rec2);
		ChangesRecorder rec4 = 	new ChangesRecorder(rec3);
		ChangesDisplay  rec5 = new ChangesDisplay(rec4);
		ChangesCounter  rec6 = new ChangesCounter(grA,grB,rec5);
		ChangesRecorder rec7 = 	new ChangesRecorder(rec6);
		ChangesDisplay  rec8 = new ChangesDisplay(rec7);
		ChangesCounter  rec9 = new ChangesCounter(grA,grB,rec8);
		{// compute GD and check that changes recorded by rec9 are correct.
			LearnerGraph graph1 = new LearnerGraph(buildGraph(graphA,name+"A"),config);
			ChangesRecorder.applyGD(graph1, gd.computeGDToXML(grA, grB, threadNumber, TestGD.createDoc(), rec9));
			Assert.assertNull(WMethod.checkM(graph1, grB));Assert.assertEquals(grB.getStateNumber(),graph1.getStateNumber());
		}
		{// check that changes propagated to rec7 are correct.
			LearnerGraph graph2 = new LearnerGraph(buildGraph(graphA,name+"A"),config);
			ChangesRecorder.applyGD(graph2, rec7.writeGD(TestGD.createDoc()));
			Assert.assertNull(WMethod.checkM(graph2, grB));Assert.assertEquals(grB.getStateNumber(),graph2.getStateNumber());
		}
		{// check that changes propagated to rec4 are correct.
			LearnerGraph graph3 = new LearnerGraph(buildGraph(graphA,name+"A"),config);
			ChangesRecorder.applyGD(graph3, rec4.writeGD(TestGD.createDoc()));
			Assert.assertNull(WMethod.checkM(graph3, grB));Assert.assertEquals(grB.getStateNumber(),graph3.getStateNumber());
		}
		{// check that changes propagated to rec1 are correct.
			LearnerGraph graph4 = new LearnerGraph(buildGraph(graphA,name+"A"),config);
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

	/** Changes to initial path. */ 
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
	private static final String 
	A6=
		"A-a->B-a->C-a->D-a->A\n"+
		"A-b->E\nB-b->F\nC-b->G\nD-b->H\n"+
		"E-b->F-b->G-b->H-b->E\n"+
		"E-a->I\nF-a->J\nG-a->K\nH-a->L\n"+
		"I-a->J-a->K-a->L-a->I\n",
		B6=
			"A@-a->B@-a->C@-a->A@\n"+
			"A@-b->E@\nB@-b->F@\nC@-b->G@\n"+
			"E@-b->F@-b->G@-b->E@\n"+
			"E@-a->I@\nF@-a->J@\nG@-a->K@\n"+
			"I@-a->J@-a->K@-a->I@\n",
			C6=
				"@A-a->@B-a->@C-a->@A\n"+
				"@A-b->@E\n@B-b->@F\n@C-b->@G\n"+
				"@E-b->@F-b->@G-b->@E\n"+
				"@E-a->@I\n@F-a->@J\n@G-a->@K\n"+
				"@I-a->@J-a->@K-a->@I\n",
			D6=
				"A-a->B-a->C-a->A\n"+
				"A-b->E\nB-b->F\nC-b->G\n"+
				"E-b->F-b->G-b->E\n"+
				"E-a->I\nF-a->J\nG-a->K\n"+
				"I-a->J-a->K-a->I\n";

	/** Same graph. */
	@Test
	public final void testComputeGD5a()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = new LearnerGraph(buildGraph(A6,"testComputeGD5a"),config);
		LearnerGraph grB = Transform.convertToNumerical(new LearnerGraph(buildGraph(A6,"testComputeGD5a"),config));
		ChangesCounter counter = new ChangesCounter(grA,grB,null);
		GD gd = new GD();gd.computeGD(grA, grB, 1, counter);
		Assert.assertEquals(0,counter.getRemoved());
		Assert.assertEquals(0,counter.getAdded());
	}
	
	/** empty graph, with a single accept state (accept,accept). */
	@Test
	public final void testComputeGD5b_AA()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = new LearnerGraph(config);
		LearnerGraph grB = Transform.convertToNumerical(new LearnerGraph(config));
		ChangesCounter counter = new ChangesCounter(grA,grB,null);
		GD gd = new GD();gd.computeGD(grA, grB, 1, counter);
		Assert.assertEquals(0,counter.getRemoved());
		Assert.assertEquals(0,counter.getAdded());
	}
	
	/** empty graph, with a single reject state (reject,accept). */
	@Test
	public final void testComputeGD5b_RA()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = new LearnerGraph(config);grA.init.setAccept(false);
		LearnerGraph grB = Transform.convertToNumerical(new LearnerGraph(config));
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD gd = new GD();gd.computeGD(grA, grB, 1, recorder);
		LearnerGraph graph = new LearnerGraph(config);graph.init = null;graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single reject state (accept,reject). */
	@Test
	public final void testComputeGD5b_AR()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = new LearnerGraph(config);
		LearnerGraph grB = Transform.convertToNumerical(new LearnerGraph(config));grB.init.setAccept(false);
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD gd = new GD();gd.computeGD(grA, grB, 1, recorder);
		LearnerGraph graph = new LearnerGraph(config);graph.init = null;graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single reject state (accept,reject). */
	@Test
	public final void testComputeGD5_RR()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = new LearnerGraph(config);grA.init.setAccept(false);
		LearnerGraph grB = Transform.convertToNumerical(new LearnerGraph(config));grB.init.setAccept(false);
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD gd = new GD();gd.computeGD(grA, grB, 1, recorder);
		LearnerGraph graph = new LearnerGraph(config);graph.init = null;graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single accept state. */
	@Test
	public final void testComputeGD5d_AA()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = new LearnerGraph(config);
		LearnerGraph grB = Transform.convertToNumerical(new LearnerGraph(buildGraph(A6,"testComputeGD5b"),config));
		int transitionsCount = grB.countEdges();
		ChangesCounter counter = new ChangesCounter(grA,grB,null);
		GD gd = new GD();gd.computeGD(grA, grB, threadNumber, counter);
		Assert.assertEquals(0,counter.getRemoved());
		Assert.assertEquals(transitionsCount,counter.getAdded());
	}
	
	/** empty graph, with a single reject state (reject,accept). */
	@Test
	public final void testComputeGD5d_RR()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = new LearnerGraph(config);grA.init.setAccept(false);
		LearnerGraph grB = Transform.convertToNumerical(new LearnerGraph(buildGraph(A6,"testComputeGD5b"),config));
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD gd = new GD();gd.computeGD(grA, grB, 1, recorder);
		LearnerGraph graph = new LearnerGraph(config);graph.init = null;graph.transitionMatrix.clear();
		ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
		Assert.assertNull(WMethod.checkM(graph, grB));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
	/** empty graph, with a single accept state. */
	@Test
	public final void testComputeGD5e_AA()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = new LearnerGraph(buildGraph(A6,"testComputeGD5b"),config);
		LearnerGraph grB = Transform.convertToNumerical(new LearnerGraph(config));
		int transitionsCount = grA.countEdges();
		ChangesCounter counter = new ChangesCounter(grA,grB,null);
		GD gd = new GD();gd.computeGD(grA, grB, 1, counter);
		Assert.assertEquals(transitionsCount,counter.getRemoved());
		Assert.assertEquals(0,counter.getAdded());
	}
	
	/** empty graph, with a single reject state (accept,reject). */
	@Test
	public final void testComputeGD5e_RR()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		LearnerGraph grA = new LearnerGraph(buildGraph(A6,"testComputeGD5b"),config);
		LearnerGraph grB = Transform.convertToNumerical(new LearnerGraph(config));grB.init.setAccept(false);
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD gd = new GD();gd.computeGD(grA, grB, 1, recorder);
		LearnerGraph graph = new LearnerGraph(buildGraph(A6,"testComputeGD5b"),config);
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
		LearnerGraph grA = new LearnerGraph(buildGraph(A,"testComputeGD6A"),config);
		LearnerGraph grB = new LearnerGraph(buildGraph(B,"testComputeGD6B"),config);
		List<PairScore> allKeyPairs = new LinkedList<PairScore>();
		ChangesDisplay recorder = new ChangesDisplay();
		GD gd = new GD();//gd.computeGD(grA, grB, 1, recorder);
		gd.init(grB, grA, 1,newToOrig);
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
		statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() { public void run() {
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
			LearnerGraph grA = new LearnerGraph(buildGraph(graphA,name+"A"),config);
			LearnerGraph grB = new LearnerGraph(buildGraph(graphB,name+"B"),config);
	
			GD gd = new GD();
			gd.init(grA, grB, threadNumber);
			gd.identifyKeyPairs();
			ChangesRecorder recorder = new ChangesRecorder(null);
			List<PairScore> allKeyPairs = new LinkedList<PairScore>();
			gd.makeSteps(recorder,allKeyPairs);
			Assert.assertEquals(expectedMatchedPairs,allKeyPairs.size());
			LearnerGraph graph = new LearnerGraph(buildGraph(graphA,name+"A"),config);
			ChangesRecorder.applyGD(graph, recorder.writeGD(TestGD.createDoc()));
			Assert.assertNull(WMethod.checkM(graph, grB));
			Assert.assertNull(WMethod.checkM(graph, graph.findVertex(otherA), grB, grB.findVertex(otherB)));
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
		String A = A6+additionA,
			B = B6+generateLine("G@", "T@", "c",20)+generateLine("S@","I@","b",20)+additionB;
		testComputeGD(A, B, "testComputeGD_big4", 9,null);
	}
	
	/** Tests emergency fallback. */
	@Test
	public final void testComputeGD_big4_fallback()
	{
		String A = A6+additionA,
			B = B6+generateLine("G@", "T@", "c",20)+generateLine("S@","I@","b",20)+additionB;
		Configuration fallbackConfig = Configuration.getDefaultConfiguration().copy();
		fallbackConfig.setGdMaxNumberOfStatesInCrossProduct(10);
		testComputeGD(A, B, "testComputeGD_big4", 9,fallbackConfig);
	}
	
	@Test
	public final void testComputeGD5b_RA_fallback()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		config.setGdMaxNumberOfStatesInCrossProduct(0);
		LearnerGraph grA = new LearnerGraph(config);grA.init.setAccept(false);
		LearnerGraph grB = Transform.convertToNumerical(new LearnerGraph(config));
		ChangesRecorder recorder = new ChangesRecorder(null);
		GD gd = new GD();gd.computeGD(grA, grB, 1, recorder);
		LearnerGraph graph = new LearnerGraph(config);graph.init = null;graph.transitionMatrix.clear();
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
		LearnerGraph grA = new LearnerGraph(buildGraph(A,"testComputeGD6A"),config);
		LearnerGraph grB = new LearnerGraph(buildGraph(B,"testComputeGD6B"),config);
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
		WMethod.checkM(graph, grB);Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
*/
	@Test
	public final void testCounter()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph grA = new LearnerGraph(buildGraph("A-a->B\nA-b->B","testCounterA"),config);
		LearnerGraph grB = new LearnerGraph(buildGraph("@A-a->@B\n@A-c->@B","testCounterB"),config);
		ChangesCounter counter = new ChangesCounter(grA,grB,null);
		GD gd = new GD();gd.computeGD(grA, grB, threadNumber, counter);
		Assert.assertEquals(1,counter.getRemoved());
		Assert.assertEquals(1,counter.getAdded());
		Assert.assertEquals("diff of testCounterB to testCounterA is 100% of testCounterB",counter.toString());
	}

	/** Tests ChangesDisplay and nesting of change observers. */ 
	@Test
	public final void testDisplay()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph grA = new LearnerGraph(buildGraph("A-a->B\nA-b->B","testCounterA"),config);
		LearnerGraph grB = new LearnerGraph(buildGraph("@A-a->@B\n@A-c->@B","testCounterB"),config);
		ChangesCounter counter = new ChangesCounter(grA,grB,null);
		ChangesDisplay recorder = new ChangesDisplay(counter);
		GD gd = new GD();gd.computeGD(grA, grB, threadNumber, recorder);
		Assert.assertEquals("removed: A - b -> B\n"+
				"added  : A - c -> B\ninitial : A\n",recorder.toString());
		Assert.assertEquals(1,counter.getAdded());Assert.assertEquals(1,counter.getRemoved());
	}
}
