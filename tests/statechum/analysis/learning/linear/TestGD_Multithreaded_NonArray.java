/**
 * 
 */
package statechum.analysis.learning.linear;

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

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.linear.GD.ChangesDisplay;
import statechum.analysis.learning.linear.GD.ChangesRecorder;
import statechum.analysis.learning.linear.GD.LearnerGraphMutator;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

/**
 * @author kirill
 *
 */
@RunWith(ParameterizedWithName.class)
public class TestGD_Multithreaded_NonArray {
	protected java.util.Map<CmpVertex,CmpVertex> newToOrig = null;

	/** Number of threads to use. */
	protected final int threadNumber;

	/** Label converter to use. */
	protected final ConvertALabel converter;

	protected final Configuration config;

	public TestGD_Multithreaded_NonArray(int th) {
		threadNumber = th;
		config = Configuration.getDefaultConfiguration().copy();
		converter = null;
	}
	
	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		for(int i=1;i<8;++i)
			result.add(new Object[]{new Integer(i)});
		return result;
	}

	@org.junit.runners.ParameterizedWithName.ParametersToString
	public static String parametersToString(Integer threads)
	{
		return ""+threads+" threads";
	}
	
	@Before
	public void beforeTest()
	{
		newToOrig = new java.util.TreeMap<CmpVertex,CmpVertex>();
	}

	
	/** A non-deterministic graph with a slightly different graph where not all states match exactly
	 * and there are disconnected states in both of the two graphs. 
	 */
	@Test
	public final void testComputeGD_ND3a()
	{
		Configuration configND3 = config.copy();configND3.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		configND3.setGdKeyPairThreshold(1);configND3.setGdLowToHighRatio(1);
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = TestGD_Multithreaded.runTestCompute_ND3(configND3,7,threadNumber,converter);
		boolean foundC = false, foundB = false;
		for(Entry<CmpVertex,CmpVertex> entry:gd.aTOb.entrySet()) 
		{
			Assert.assertFalse(entry.getKey().equals(VertexID.parseID(TestGD_Multithreaded.testA)));
			if (entry.getKey().equals(VertexID.parseID(TestGD_Multithreaded.testB))) foundB = true;
			if (gd.newBToOrig.get(entry.getValue()).equals(VertexID.parseID(TestGD_Multithreaded.nameC))) foundC = true;
			Assert.assertFalse(gd.newBToOrig.get(entry.getValue()).equals(VertexID.parseID(TestGD_Multithreaded.nameD)));
		}
		Assert.assertTrue(foundB);
		Assert.assertTrue(foundC);
	}

	/** A modified version of testComputeGD_ND3a where attributes on a disconnected key pair are unchanged. */
	@Test
	public final void testComputeGD_ND3a_modified()
	{
		final String name = "testComputeGD_ND2";
		Configuration configND3 = config.copy();configND3.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		configND3.setGdKeyPairThreshold(1);configND3.setGdLowToHighRatio(1);
		Configuration cloneConfig = configND3.copy();cloneConfig.setLearnerCloneGraph(true);
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D";
		LearnerGraphND grA = buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\n"+common,name+"A",configND3, converter);
		grA.findVertex("F").setColour(JUConstants.RED);grA.findVertex("B").setColour(JUConstants.AMBER);grA.findVertex("B").setDepth(3);
		CmpVertex newStateA = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("testA"), configND3);
		newStateA.setHighlight(true);
		grA.transitionMatrix.put(newStateA,grA.createNewRow());

		LearnerGraphND grB = buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\n"+common,name+"B",configND3, converter);
		CmpVertex newStateC = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(TestGD_Multithreaded.nameC), configND3);
		DeterministicDirectedSparseGraph.copyVertexData(newStateA, newStateC);
		grB.transitionMatrix.put(newStateC,grB.createNewRow());

		LearnerGraphND result = TestGD_Multithreaded.checkDiffBetweenND(grA, grB,7,0,threadNumber,configND3,converter);
		Assert.assertNull(result.findVertex("testA"));
		Assert.assertNotNull(result.findVertex(TestGD_Multithreaded.nameC));
		Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(newStateC,result.findVertex(TestGD_Multithreaded.nameC)));

		// The last check: ensure that disconnected states are or are not key pairs.
		// This chunk of code simply returns GD, the checking is performed by the caller of this method. 
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		gd.init(grA, grB, threadNumber,configND3);gd.identifyKeyPairs();
		ChangesDisplay display = new ChangesDisplay(null);
		ChangesRecorder recorder = new ChangesRecorder(display);
		gd.makeSteps();gd.computeDifference(recorder);
		
		boolean foundA = false;
		for(Entry<CmpVertex,CmpVertex> entry:gd.aTOb.entrySet()) 
		{
			if (entry.getKey().equals(VertexID.parseID("testA"))) 
			{
				foundA = true;
				Assert.assertTrue(gd.newBToOrig.get(entry.getValue()).equals(VertexID.parseID(TestGD_Multithreaded.nameC)));
			}
		}
		Assert.assertTrue(foundA);
		Assert.assertTrue(display.toString().contains("added vertex:testA"));// this has to be done because testA is disconnected and will be killed by removeDisconnected
		
		LearnerGraphND graph = new LearnerGraphND(configND3);AbstractLearnerGraph.copyGraphs(grA, graph);
		Configuration configMut = config.copy();configND3.setLearnerCloneGraph(false);
		LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> graphPatcher = new LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(graph,configMut,null);
		ChangesRecorder.loadDiff(graphPatcher, recorder.writeGD(TestGD.createDoc()), converter);
		graphPatcher.removeDanglingStates();
		LearnerGraphND outcome = new LearnerGraphND(configMut);
		graphPatcher.relabel(outcome);
		
		Assert.assertTrue(DeterministicDirectedSparseGraph.nonIDAttributesEquals(outcome.findVertex(TestGD_Multithreaded.nameC),grA.findVertex("testA")));
	}
	
}
