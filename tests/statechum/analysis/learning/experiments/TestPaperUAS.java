package statechum.analysis.learning.experiments;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

import java.io.ByteArrayOutputStream;
import java.io.Reader;
import java.io.StringReader;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.Helper;
import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.StatechumXML;
import statechum.analysis.learning.PairOfPaths;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.experiments.PaperUAS;
import statechum.analysis.learning.experiments.PaperUAS.TracesForSeed;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.TestWithMultipleConfigurations;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

@RunWith(Parameterized.class)
public class TestPaperUAS extends TestWithMultipleConfigurations 
{

	protected PaperUAS paper;
	
	@Parameters
	public static Collection<Object[]> data() 
	{
		return TestWithMultipleConfigurations.data();
	}
	
	public static String parametersToString(Configuration config)
	{
		return TestWithMultipleConfigurations.parametersToString(config);
	}
	
	public TestPaperUAS(Configuration conf)
	{
		super(conf);
	}
	
	@Before
	public void BeforeTests()
	{
		paper = new PaperUAS();paper.learnerInitConfiguration.config = mainConfiguration;
		paper.labelConverter = paper.learnerInitConfiguration.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?new Transform.InternStringLabel():null;
	}
	
	/** Empty traces */
	@Test
	public void testLoad1a()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		paper.loadData(new StringReader(""));
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		
		Assert.assertEquals(-1,paper.getMaxFrame(new Reader[]{}));
		Assert.assertEquals(-1,paper.getMaxFrame(new Reader[]{new StringReader("")}));
	}

	/** Empty line */
	@Test
	public void testLoad1b()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("\n\n"));
		}},IllegalArgumentException.class,"invalid match");
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
	}

	
	static Map<Integer,Set<List<Label>>> constructCollectionOfTraces(final Map<Integer,PTASequenceEngine> frameToEngine,final boolean positive)
	{
		Map<Integer,Set<List<Label>>> frameToTraces = new TreeMap<Integer,Set<List<Label>>>();
		for(Entry<Integer,PTASequenceEngine> entry:frameToEngine.entrySet())
		{
			final FilterPredicate existingPredicate = entry.getValue().getFSM_filterPredicate();
			
			Set<List<Label>> tracesForFrame = new HashSet<List<Label>>();
			tracesForFrame.addAll(entry.getValue().getData(new FilterPredicate()
			{
	
				@Override
				public boolean shouldBeReturned(Object name) {
					return existingPredicate.shouldBeReturned(name) == positive;
				}
				
			}));
			frameToTraces.put(entry.getKey(),tracesForFrame);
		}
		return frameToTraces;
	}
	
	/** A single trace, with zero (initial) timestamp. */
	@Test
	public void testLoad2()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		paper.loadData(new StringReader("0,UAV3,4, + [[aa]]"));
		Map<Integer,Set<List<Label>>> uav3Positive = constructCollectionOfTraces(paper.collectionOfTraces.get("4").tracesForUAVandFrame.get("UAV3"),true);
		Map<Integer,Set<List<Label>>> uav3Negative = constructCollectionOfTraces(paper.collectionOfTraces.get("4").tracesForUAVandFrame.get("UAV3"),false);
		Assert.assertEquals(1,uav3Positive.size());
		Assert.assertEquals(1,uav3Negative.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, mainConfiguration,converter).equals(
				uav3Positive.get(0)));
		Assert.assertTrue(uav3Negative.get(0).isEmpty());
	}

	/** A single trace, with negative initial timestamp that does not make it past the parser. */
	@Test
	public void testLoad3a1()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("-1,UAV3,4, + [[aa]]"));
		}},IllegalArgumentException.class,"failed to lex");
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
	}
	
	/** A single trace, with non-zero initial timestamp. */
	@Test
	public void testLoad3a2()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		paper.loadData(new StringReader("1,UAV3,4, + [[aa]]"));
		Assert.assertFalse(paper.collectionOfTraces.isEmpty());
	}
	

	/** A single trace, with zero initial timestamp but invalid subsequent timestamp. */
/*	@Test
	public void testLoad3a2()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,UAV3,4, + [[aa]]\n"+
					"0,UAV66,4, + [[aa]]\n"+
					"2,UAV3,4, + [[aa]]\n"
					
			));
		}},IllegalArgumentException.class,"current frame number");
	}
	*/
	/** A single trace, with zero initial timestamp but invalid subsequent timestamp. */
	@Test
	public void testLoad3a3()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,UAV3,4, + [[aa]]\n"+
					"0,UAV66,4, + [[aa]]\n"+
					"1,UAV3,4, + [[aa]]\n"+
					"0,UAV3,4, + [[aa]]\n"
					
			));
		}},IllegalArgumentException.class,"current frame number");
	}
	
	/** Positives and negatives, multiple UAVs, interleaved. */
	@Test
	public void testLoad3a4()
	{
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
		paper.loadData(new StringReader("0,UAV3,4, + [[aa],[bb,cc]] - [[zz]]\n"+
				"0,UAV55,4, - [[Faa],[bb,Fcc]]\n"+
				"1,UAV3,4, + [[Taa],[bb,cc]] - [[Tzz]]\n"+
				"0,UAV55,4, + [[qq]]\n"+
				"2,UAV3,SEED2, + [[anotherOne]]\n"+
				"0,UAV3,4, + [[oo]]"
		));
		}},IllegalArgumentException.class,"current frame number");
	}
	
	/** A single trace, invalid UAV name. */
	@Test
	public void testLoad3b1()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,"+PaperUAS.UAVAll+",4, + [[aa]]"));
		}},IllegalArgumentException.class,"UAV name");
	}
	
	/** A single trace, invalid UAV name. */
	@Test
	public void testLoad3b2()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,"+PaperUAS.UAVAllSeeds+",4, + [[aa]]"));
		}},IllegalArgumentException.class,"UAV name");
	}
	
	/** A single trace, invalid seed name. */
	@Test
	public void testLoad3b3()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,valid,"+PaperUAS.UAVAllSeeds+", + [[aa]]"));
		}},IllegalArgumentException.class,"seed name");
	}

	/** A single trace, cannot parse. */
	@Test
	public void testLoad3c()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,,4, + [[aa]]"));
		}},IllegalArgumentException.class,"failed to lex");
	}
	
	/** An empty trace. */
	@Test
	public void testLoad3d()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		paper.loadData(new StringReader("0,AA,4,  "));
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		
		// two special UAVs, collecting all traces in a specific seed and the one collecting traces across seeds.
		Assert.assertEquals(3,tr.tracesForUAVandFrame.size());
		Assert.assertEquals(3,tr.tracesForUAVandFrame.size());
		Assert.assertEquals(1,constructCollectionOfTraces(tr.tracesForUAVandFrame.get("AA"),true).size());
		Assert.assertEquals(1,constructCollectionOfTraces(tr.tracesForUAVandFrame.get("AA"),false).size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{}}, mainConfiguration,converter).equals(
				constructCollectionOfTraces(tr.tracesForUAVandFrame.get("AA"),true).get(0)));// empty trace corresponds to a single accept state in a PTA
		Assert.assertTrue(constructCollectionOfTraces(tr.tracesForUAVandFrame.get("AA"),false).get(0).isEmpty());// nothing among reject states, an empty trace here would mean that the initial state is the reject one 

		Assert.assertEquals(1,constructCollectionOfTraces(tr.tracesForUAVandFrame.get(PaperUAS.UAVAll),true).size());
		Assert.assertEquals(1,constructCollectionOfTraces(tr.tracesForUAVandFrame.get(PaperUAS.UAVAll),false).size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{}}, mainConfiguration,converter).equals(
				constructCollectionOfTraces(tr.tracesForUAVandFrame.get(PaperUAS.UAVAll),true).get(0)));// empty trace corresponds to a single accept state in a PTA
		Assert.assertTrue(constructCollectionOfTraces(tr.tracesForUAVandFrame.get(PaperUAS.UAVAll),false).get(0).isEmpty());
	}
	
	/** A single trace, cannot parse invalid trace. */
	@Test
	public void testLoad3e()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,AA,4,  u"));
		}},IllegalArgumentException.class,"a collection of traces");
	}
	
	/** A single trace, invalid frame number. */
	@Test
	public void testLoad3f()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("pp,AA,4,  u"));
		}},NumberFormatException.class,"pp");
	}
	
	/** A single trace, with zero initial timestamp. */
	@Test
	public void testLoad3g()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		paper.loadData(new StringReader("0,UAV3,4, + [[aa]]"));
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav3Positive = constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV3"),true);
		Assert.assertEquals(1,uav3Positive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, mainConfiguration,converter).equals(
				uav3Positive.get(0)));
		Map<Integer,Set<List<Label>>> uav3Negative= constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV3"),false);
		Assert.assertEquals(1,uav3Negative.size());
		Assert.assertTrue(uav3Negative.get(0).isEmpty());
		Map<Integer,Set<List<Label>>> uav3PositiveAll = constructCollectionOfTraces(tr.tracesForUAVandFrame.get(PaperUAS.UAVAll),true);
		Assert.assertEquals(1,uav3PositiveAll.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, mainConfiguration,converter).equals(
				uav3PositiveAll.get(0)));
		Map<Integer,Set<List<Label>>> uav3NegativeAll = constructCollectionOfTraces(tr.tracesForUAVandFrame.get(PaperUAS.UAVAll),false);
		Assert.assertEquals(1,uav3NegativeAll.size());
		Assert.assertTrue(uav3NegativeAll.get(0).isEmpty());
	}
	
	/** Both positive and negative traces. */
	@Test
	public void testLoad4()
	{
		paper.loadData(new StringReader("0,UAV3,4, + [[aa],[bb,cc]] - [[zz]]"));
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav3Positive = constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV3"),true);
		Assert.assertEquals(1,uav3Positive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"bb","cc"}}, mainConfiguration,converter).equals(
				uav3Positive.get(0)));
		Map<Integer,Set<List<Label>>> uav3Negative = constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV3"),false);
		Assert.assertEquals(1,uav3Negative.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"zz"}}, mainConfiguration,converter).equals(
				uav3Negative.get(0)));
	}
	
	/** Positives and negatives, multiple UAVs, interleaved. */
	@Test
	public void testLoad5()
	{
		paper.loadData(new StringReader("0,UAV3,4, + [[aa],[bb,cc]] - [[zz]]\n"+
				"0,UAV55,4, - [[Faa],[bb,Fcc]]\n"+
				"1,UAV3,4, + [[Taa],[bb,cc]] - [[Tzz]]\n"
		));
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav3Positive = constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV3"),true);
		Assert.assertEquals(2,uav3Positive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"bb","cc"}}, mainConfiguration,converter).equals(
				uav3Positive.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"Taa"},new String[]{"bb","cc"}}, mainConfiguration,converter).equals(
				uav3Positive.get(1)));
		Map<Integer,Set<List<Label>>> uav3Negative =constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV3"),false);
		Assert.assertEquals(2,uav3Negative.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"zz"}}, mainConfiguration,converter).equals(
				uav3Negative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"zz"},new String[]{"Tzz"}}, mainConfiguration,converter).equals(
				uav3Negative.get(1)));
		
		Map<Integer,Set<List<Label>>> uav55Positive = constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV55"),true);
		Map<Integer,Set<List<Label>>> uav55Negative = constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV55"),false);
		Assert.assertEquals(2,uav55Negative.size());
		Assert.assertEquals(2,uav55Positive.size());
		Assert.assertTrue(uav55Positive.get(0).isEmpty());Assert.assertTrue(uav55Positive.get(1).isEmpty());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, mainConfiguration,converter).equals(
				uav55Negative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, mainConfiguration,converter).equals(
				uav55Negative.get(1)));
		
	}
	
	/** Positives and negatives, multiple UAVs, interleaved. */
	@Test
	public void testLoad6()
	{
		paper.loadData(new StringReader("0,UAV3,4, + [[aa],[bb,cc]] - [[zz]]\n"+
				"0,UAV55,4, - [[Faa],[bb,Fcc]]\n"+
				"1,UAV3,4, + [[Taa],[bb,cc]] - [[Tzz]]\n"+
				"0,UAV55,4, + [[qq]]\n"
		));
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav55Positive = constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV55"),true);
		Map<Integer,Set<List<Label>>> uav55Negative = constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV55"),false);
		Assert.assertEquals(2,uav55Negative.size());
		Assert.assertEquals(2,uav55Positive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, mainConfiguration,converter).equals(
				uav55Positive.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, mainConfiguration,converter).equals(
				uav55Positive.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, mainConfiguration,converter).equals(
				uav55Negative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, mainConfiguration,converter).equals(
				uav55Negative.get(1)));

		Map<Integer,Set<List<Label>>> uavAllPositive = constructCollectionOfTraces(tr.tracesForUAVandFrame.get(PaperUAS.UAVAll),true);
		Assert.assertEquals(2,uavAllPositive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"qq"},new String[]{"bb","cc"}}, mainConfiguration,converter).equals(
				uavAllPositive.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"qq"},new String[]{"Taa"},new String[]{"bb","cc"}}, mainConfiguration,converter).equals(
				uavAllPositive.get(1)));
		Map<Integer,Set<List<Label>>> uavAllNegative = constructCollectionOfTraces(tr.tracesForUAVandFrame.get(PaperUAS.UAVAll),false);
		Assert.assertEquals(2,uavAllNegative.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"},
				new String[]{"zz"}}, mainConfiguration,converter).equals(
				uavAllNegative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"},
				new String[]{"zz"}, new String[]{"Tzz"}}, mainConfiguration,converter).equals(
				uavAllNegative.get(1)));
	}

	/** Positives and negatives, multiple UAVs, interleaved. */
	@Test
	public void testLoad7()
	{
		paper.loadData(new StringReader("0,UAV3,4, + [[aa],[bb,cc]] - [[zz]]\n"+
				"0,UAV55,4, - [[Faa],[bb,Fcc]]\n"+
				"1,UAV3,4, + [[Taa],[bb,cc]] - [[Tzz]]\n"+
				"0,UAV55,4, + [[qq]]\n"+
				"2,UAV3,4, + [[anotherOne]]"
		));
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav55Positive = constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV55"),true);
		Map<Integer,Set<List<Label>>> uav55Negative = constructCollectionOfTraces(tr.tracesForUAVandFrame.get("UAV55"),false);
		Assert.assertEquals(3,uav55Negative.size());
		Assert.assertEquals(3,uav55Positive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, mainConfiguration,converter).equals(
				uav55Positive.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, mainConfiguration,converter).equals(
				uav55Positive.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, mainConfiguration,converter).equals(
				uav55Positive.get(2)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, mainConfiguration,converter).equals(
				uav55Negative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, mainConfiguration,converter).equals(
				uav55Negative.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, mainConfiguration,converter).equals(
				uav55Negative.get(2)));

		Map<Integer,Set<List<Label>>> uavAllPositive = constructCollectionOfTraces(tr.tracesForUAVandFrame.get(PaperUAS.UAVAll),true);
		Assert.assertEquals(3,uavAllPositive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"qq"},new String[]{"bb","cc"}}, mainConfiguration,converter).equals(
				uavAllPositive.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"qq"},new String[]{"Taa"},new String[]{"bb","cc"}}, mainConfiguration,converter).equals(
				uavAllPositive.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"anotherOne"},new String[]{"aa"},new String[]{"qq"},new String[]{"Taa"},new String[]{"bb","cc"}}, mainConfiguration,converter).equals(
				uavAllPositive.get(2)));
		Map<Integer,Set<List<Label>>> uavAllNegative = constructCollectionOfTraces(tr.tracesForUAVandFrame.get(PaperUAS.UAVAll),false);
		Assert.assertEquals(3,uavAllNegative.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"},
				new String[]{"zz"}}, mainConfiguration,converter).equals(
				uavAllNegative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"},
				new String[]{"zz"}, new String[]{"Tzz"}}, mainConfiguration,converter).equals(
				uavAllNegative.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"},
				new String[]{"zz"}, new String[]{"Tzz"}}, mainConfiguration,converter).equals(
				uavAllNegative.get(2)));
	}

	/** Positives and negatives, multiple UAVs, interleaved. */
	@Test
	public void testLoad8()
	{
		paper.loadData(new StringReader("0,UAV3,4, + [[aa]]\n"+
				"0,UAV55,4, - [[Faa],[bb]]\n"+
				"0,UAV3,SEED2, + [[Taa]]\n"+
				"1,UAV55,4, + [[qq]]\n"+
				"1,UAV3,SEED2, + [[anotherOne]]"
		));
		TracesForSeed tr4 = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav3Positive4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV3"),true);
		Map<Integer,Set<List<Label>>> uav3Negative4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV3"),false);
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, mainConfiguration,converter).equals(
				uav3Positive4.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, mainConfiguration,converter).equals(
				uav3Positive4.get(1)));
		Assert.assertTrue(uav3Negative4.get(0).isEmpty());
		Assert.assertTrue(uav3Negative4.get(1).isEmpty());
		Map<Integer,Set<List<Label>>> uav55Positive4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV55"),true);
		Map<Integer,Set<List<Label>>> uav55Negative4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV55"),false);
		Assert.assertTrue(uav55Positive4.get(0).isEmpty());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, mainConfiguration,converter).equals(
				uav55Positive4.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb"}}, mainConfiguration,converter).equals(
				uav55Negative4.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb"}}, mainConfiguration,converter).equals(
				uav55Negative4.get(1)));
	
		TracesForSeed tr2 = paper.collectionOfTraces.get("SEED2");
		Map<Integer,Set<List<Label>>> uav3Positive2 = constructCollectionOfTraces(tr2.tracesForUAVandFrame.get("UAV3"),true);
		Map<Integer,Set<List<Label>>> uav3Negative2 = constructCollectionOfTraces(tr2.tracesForUAVandFrame.get("UAV3"),false);
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Taa"}}, mainConfiguration,converter).equals(
				uav3Positive2.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Taa"},new String[]{"anotherOne"}}, mainConfiguration,converter).equals(
				uav3Positive2.get(1)));
		Assert.assertTrue(uav3Negative2.get(0).isEmpty());
		Assert.assertTrue(uav3Negative2.get(1).isEmpty());
		
		Map<Integer,Set<List<Label>>> uav55Positive2 = constructCollectionOfTraces(tr2.tracesForUAVandFrame.get("UAV55"),true);
		Map<Integer,Set<List<Label>>> uav55Negative2 = constructCollectionOfTraces(tr2.tracesForUAVandFrame.get("UAV55"),false);
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{}}, mainConfiguration,converter).equals(uav55Positive2.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{}}, mainConfiguration,converter).equals(uav55Positive2.get(1)));
		Assert.assertTrue(uav55Negative2.get(0).isEmpty());
		Assert.assertTrue(uav55Negative2.get(1).isEmpty());
		
		TracesForSeed trAll = paper.collectionOfTraces.get(PaperUAS.UAVAllSeeds);
		Assert.assertEquals(4,trAll.tracesForUAVandFrame.size());
		Map<Integer,Set<List<Label>>> uavAllPositiveAll = constructCollectionOfTraces(trAll.tracesForUAVandFrame.get(PaperUAS.UAVAllSeeds),true);
		Map<Integer,Set<List<Label>>> uavAllNegativeAll = constructCollectionOfTraces(trAll.tracesForUAVandFrame.get(PaperUAS.UAVAllSeeds),false);
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"Taa"}}, mainConfiguration,converter).equals(
				uavAllPositiveAll.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"Taa"},new String[]{"qq"},new String[]{"anotherOne"}}, mainConfiguration,converter).equals(
				uavAllPositiveAll.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb"}}, mainConfiguration,converter).equals(
				uavAllNegativeAll.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb"}}, mainConfiguration,converter).equals(
				uavAllNegativeAll.get(1)));
		for(Entry<String,Map<Integer,PTASequenceEngine>> entry:trAll.tracesForUAVandFrame.entrySet())
			if (!entry.getKey().equals(PaperUAS.UAVAllSeeds))
			{
				for(Entry<Integer,Set<List<Label>>> pair:constructCollectionOfTraces(entry.getValue(),true).entrySet())
					Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{}}, mainConfiguration,converter).equals(pair.getValue()));
			}
		
	}
	
	
	@Test
	public void testGetMaxFrame()
	{
		Assert.assertEquals(0,paper.getMaxFrame(new Reader[]{new StringReader("0,UAV3,4, + [[aa]]")}));
		Assert.assertEquals(10,paper.getMaxFrame(new Reader[]{new StringReader("10,UAV3,4, + [[aa]]")}));
		Assert.assertEquals(10,paper.getMaxFrame(new Reader[]{new StringReader("10,UAV3,40, + [[aa]]\n" + "0,UAV3,4, + [[aa]]")}));		
	}
	
	@Test
	public void testLoadByConcatenationFail1()
	{
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadDataByConcatenation(new Reader[]{new StringReader("0,UAV3,4, + [[aa]]\n"
			)});
		}},IllegalArgumentException.class,"should contain at least two");
	}
	
	/** not same as the starting element of a the same trace */
	@Test
	public void testLoadByConcatenationFail2()
	{
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadDataByConcatenation(new Reader[]{new StringReader("0,UAV3,4, + [[aa,bb]]\n"
			)});
		}},IllegalArgumentException.class,"each positive trace");
	}
	
	/** not same as the starting element of a the same trace */
	@Test
	public void testLoadByConcatenationFail3()
	{
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadDataByConcatenation(new Reader[]{new StringReader("0,UAV3,4, + [[aa,cc,bb]]\n"
			)});
		}},IllegalArgumentException.class,"each positive trace");
	}
	
	/** not same as the starting element of an existing positive trace */
	@Test
	public void testLoadByConcatenationFail4()
	{
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadDataByConcatenation(new Reader[]{new StringReader("0,UAV3,4, + [[aa,cc,aa]]\n"+"0,UAV3,4, + [[bb,cc,bb]]\n"
			)});
		}},IllegalArgumentException.class,"last positive trace");
	}
	
	/** not same as the starting element of a the same trace */
	@Test
	public void testLoadByConcatenationFail5()
	{
		paper.loadDataByConcatenation(new Reader[]{new StringReader("0,UAV55,4, - [[Faa],[bb]]\n"
			)});
		TracesForSeed tr4 = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav55Positive4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV55"),true);
		Map<Integer,Set<List<Label>>> uav55Negative4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV55"),false);
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(
				uav55Positive4.get(0)));// since there are no positive traces at all, this set is empty. It would usually contain an empty sequence. 
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb"}}, mainConfiguration,converter).equals(
				uav55Negative4.get(0)));
	}
	
	@Test
	public void testLoadByConcatenation1()
	{
		paper.loadDataByConcatenation(new Reader[]{new StringReader("0,UAV3,4, + [[aa,aa]]\n"+
				"0,UAV55,4, - [[Faa],[bb]]\n"+
				"0,UAV3,SEED2, + [[aa,bb,cc,aa]]\n"+
				"1,UAV55,4, + [[qq,aa,qq]]\n"+
				"1,UAV3,SEED2, + [[aa,gg,aa]]"
		)});
		TracesForSeed tr4 = paper.collectionOfTraces.get("4");
		{
			Map<Integer,Set<List<Label>>> uav3Positive4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV3"),true);
			Map<Integer,Set<List<Label>>> uav3Negative4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV3"),false);
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, mainConfiguration,converter).equals(
					uav3Positive4.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, mainConfiguration,converter).equals(
					uav3Positive4.get(1)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(
					uav3Negative4.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(
					uav3Negative4.get(1)));
		}
		{
			Map<Integer,Set<List<Label>>> uav55Positive4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV55"),true);
			Map<Integer,Set<List<Label>>> uav55Negative4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV55"),false);
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(
					uav55Positive4.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq","aa"}}, mainConfiguration,converter).equals(
					uav55Positive4.get(1)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"}, new String[]{"bb"}}, mainConfiguration,converter).equals(
					uav55Negative4.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"}, new String[]{"bb"}}, mainConfiguration,converter).equals(
					uav55Negative4.get(1)));
		}
		
		TracesForSeed tr2 = paper.collectionOfTraces.get("SEED2");
		{
			Map<Integer,Set<List<Label>>> uav3Positive2 = constructCollectionOfTraces(tr2.tracesForUAVandFrame.get("UAV3"),true);
			Map<Integer,Set<List<Label>>> uav3Negative2 = constructCollectionOfTraces(tr2.tracesForUAVandFrame.get("UAV3"),false);
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa","bb","cc"}}, mainConfiguration,converter).equals(
					uav3Positive2.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa","bb","cc","aa","gg"}}, mainConfiguration,converter).equals(
					uav3Positive2.get(1)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(
					uav3Negative2.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(
					uav3Negative2.get(1)));
		}		
		
		{
			Map<Integer,Set<List<Label>>> uav55Positive2 = constructCollectionOfTraces(tr2.tracesForUAVandFrame.get("UAV55"),true);
			Map<Integer,Set<List<Label>>> uav55Negative2 = constructCollectionOfTraces(tr2.tracesForUAVandFrame.get("UAV55"),false);
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{}}, mainConfiguration,converter).equals(
					uav55Positive2.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{}}, mainConfiguration,converter).equals(
					uav55Positive2.get(1)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(
					uav55Negative2.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(
					uav55Negative2.get(1)));
		}		
		
	}
	
	@Test
	public void testLoadByConcatenation2()
	{
		paper.loadDataByConcatenation(new Reader[]{new StringReader("0,UAV3,4, + [[aa,aa]]\n"+
				"0,UAV3,4, + [[aa,bb,cc,aa]]\n"+
				"0,UAV3,4, - [[nn,rr]]\n"+
				"1,UAV3,4, + [[aa,gg,aa]]\n"+
				"1,UAV3,4, - [[Rnn,Rrr]]\n"
		)});
		TracesForSeed tr4 = paper.collectionOfTraces.get("4");
		{
			Map<Integer,Set<List<Label>>> uav3Positive4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV3"),true);
			Map<Integer,Set<List<Label>>> uav3Negative4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV3"),false);
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(// all positive traces are prefixes of negatives and are thus not included
					uav3Positive4.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(// all positive traces are prefixes of negatives and are thus not included
					uav3Positive4.get(1)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa","aa","bb","cc","nn","rr"}}, mainConfiguration,converter).equals(
					uav3Negative4.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa","aa","bb","cc","nn","rr"}, new String[]{"aa","aa","bb","cc","aa","gg","Rnn","Rrr"}}, mainConfiguration,converter).equals(
					uav3Negative4.get(1)));
		}
	}

	@Test
	public void testLoadByConcatenation3()
	{
		paper.loadDataByConcatenation(new Reader[]{new StringReader("0,UAV3,4, + [[aa,aa]]\n"+
				"0,UAV3,4, - [[zz]]\n"+
				"0,UAV3,4, + [[aa,bb,cc,aa]]\n"+
				"0,UAV3,4, - [[nn,rr]]\n"+
				"1,UAV3,4, + [[aa,gg,aa]]\n"+
				"1,UAV3,4, - [[Rnn,Rrr]]\n"
		)});
		TracesForSeed tr4 = paper.collectionOfTraces.get("4");
		{
			Map<Integer,Set<List<Label>>> uav3Positive4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV3"),true);
			Map<Integer,Set<List<Label>>> uav3Negative4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV3"),false);
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(// all positive traces are prefixes of negatives and are thus not included
					uav3Positive4.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(
					uav3Positive4.get(1)));// all positive traces are prefixes of negatives and are thus not included
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa","aa","bb","cc","nn","rr"},new String[]{"aa","zz"}}, mainConfiguration,converter).equals(
					uav3Negative4.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa","aa","bb","cc","nn","rr"},new String[]{"aa","zz"}, new String[]{"aa","aa","bb","cc","aa","gg","Rnn","Rrr"}}, mainConfiguration,converter).equals(
					uav3Negative4.get(1)));
		}
	}
	
	/** Similar to above but contains a single positive trace. */
	@Test
	public void testLoadByConcatenation4()
	{
		paper.loadDataByConcatenation(new Reader[]{new StringReader("0,UAV3,4, + [[aa,aa]]\n"+
				"0,UAV3,4, - [[zz]]\n"+
				"0,UAV3,4, + [[aa,bb,cc,aa]]\n"+
				"0,UAV3,4, - [[nn,rr]]\n"+
				"1,UAV3,4, + [[aa,gg,aa]]\n"
		)});
		TracesForSeed tr4 = paper.collectionOfTraces.get("4");
		{
			Map<Integer,Set<List<Label>>> uav3Positive4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV3"),true);
			Map<Integer,Set<List<Label>>> uav3Negative4 = constructCollectionOfTraces(tr4.tracesForUAVandFrame.get("UAV3"),false);
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{}, mainConfiguration,converter).equals(// all positive traces are prefixes of negatives and are thus not included
					uav3Positive4.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa","aa","bb","cc","aa","gg"}}, mainConfiguration,converter).equals(
					uav3Positive4.get(1)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa","aa","bb","cc","nn","rr"},new String[]{"aa","zz"}}, mainConfiguration,converter).equals(
					uav3Negative4.get(0)));
			Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa","aa","bb","cc","nn","rr"},new String[]{"aa","zz"}}, mainConfiguration,converter).equals(
					uav3Negative4.get(1)));
		}
	}
	
	@Test
	public void testWritePairsToXMLFail1()
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C\nD-a->D", "testWritePairsToXMLFail1",mainConfiguration,converter);
		Helper.checkForCorrectException(new whatToRun() { @SuppressWarnings("unused") public @Override void run() {
			new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("D")), gr.findVertex(VertexID.parseID("C")),1,2));
		}},IllegalArgumentException.class,"failed to find paths");
	}
	
	@Test
	public void testWritePairsToXML1a()
	{
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		PairOfPaths pair = new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		List<PairOfPaths> list = new LinkedList<PairOfPaths>();list.add(pair);
		PairOfPaths.writePairs(list, mainConfiguration, outputStream);
		
		// Now load this.
		List<PairOfPaths> loaded = PairOfPaths.readPairs(new StringReader(outputStream.toString()), mainConfiguration,converter);
		Assert.assertEquals(1,loaded.size());
		
		PairOfPaths r=loaded.get(0);
		Assert.assertEquals("A",gr.getVertex(r.getQ()).getStringId());Assert.assertEquals("C",gr.getVertex(r.getR()).getStringId());
	}
	
	/** A pair with only one element. */
	@Test
	public void testWritePairsToXML1b()
	{
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		PairOfPaths pair = new PairOfPaths(gr,new PairScore(null, gr.findVertex(VertexID.parseID("C")),1,2));
		List<PairOfPaths> list = new LinkedList<PairOfPaths>();list.add(pair);
		PairOfPaths.writePairs(list, mainConfiguration, outputStream);
		
		// Now load this.
		List<PairOfPaths> loaded = PairOfPaths.readPairs(new StringReader(outputStream.toString()), mainConfiguration,converter);
		Assert.assertEquals(1,loaded.size());
		
		PairOfPaths r=loaded.get(0);
		Assert.assertNull(r.getQ());Assert.assertEquals("C",gr.getVertex(r.getR()).getStringId());
	}
	
	@Test
	public void testLoadFromXML1a()
	{
		String xmlString = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><"+PairOfPaths.pairCollectionElement+">\n"+
			"<"+StatechumXML.ELEM_SEQ+" "+StatechumXML.ATTR_SEQ+"=\""+PairOfPaths.pairElement+"\">[['b','c'],\n"+
			"[]]</"+StatechumXML.ELEM_SEQ+"></"+PairOfPaths.pairCollectionElement+">";
		
		LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		List<PairOfPaths> loaded = PairOfPaths.readPairs(new StringReader(xmlString), mainConfiguration,converter);
		Assert.assertEquals(1,loaded.size());
		
		PairOfPaths r=loaded.get(0);
		Assert.assertEquals("A",gr.getVertex(r.getQ()).getStringId());Assert.assertEquals("C",gr.getVertex(r.getR()).getStringId());
	}
	
	/** A pair with only one element. */
	@Test
	public void testLoadFromXML1b()
	{
		String xmlString = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><"+PairOfPaths.pairCollectionElement+">\n"+
			"<"+StatechumXML.ELEM_SEQ+" "+StatechumXML.ATTR_SEQ+"=\""+PairOfPaths.pairElement+"\">[['b','c']]</"+StatechumXML.ELEM_SEQ+"></"+PairOfPaths.pairCollectionElement+">";
		
		LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		List<PairOfPaths> loaded = PairOfPaths.readPairs(new StringReader(xmlString), mainConfiguration,converter);
		Assert.assertEquals(1,loaded.size());
		
		PairOfPaths r=loaded.get(0);
		Assert.assertNull(r.getQ());Assert.assertEquals("C",gr.getVertex(r.getR()).getStringId());
	}
	
	@Test
	public void testLoadFromXML2()
	{
		String xmlString = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><"+PairOfPaths.pairCollectionElement+">\n"+
			"</"+PairOfPaths.pairCollectionElement+">";
		
		List<PairOfPaths> loaded = PairOfPaths.readPairs(new StringReader(xmlString), mainConfiguration,converter);
		Assert.assertEquals(0,loaded.size());
	}
	
	@Test
	public void testWritePairsToFail2()
	{
		final String xmlString = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><AA>\n"+
			"<"+StatechumXML.ELEM_SEQ+" "+StatechumXML.ATTR_SEQ+"=\""+PairOfPaths.pairElement+"\">[['b','c'],\n"+
			"[]]</"+StatechumXML.ELEM_SEQ+"></AA>";
		
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			 PairOfPaths.readPairs(new StringReader(xmlString), mainConfiguration,converter);
		}},IllegalArgumentException.class,"invalid child element");
	}
	
	@Test
	public void testWritePairsToFail3()
	{
		final String xmlString = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><AA>\n"+
			"[]]</"+StatechumXML.ELEM_SEQ+"></AA>";
		
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			 PairOfPaths.readPairs(new StringReader(xmlString), mainConfiguration,converter);
		}},IllegalArgumentException.class,"failed to construct/load");
	}
	
	
	@Test
	public void testWritePairsToXML2()
	{
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		PairOfPaths pair1 = new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2)),
				pair2 = new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("C")),1,2));
		List<PairOfPaths> list = new LinkedList<PairOfPaths>();list.add(pair1);list.add(pair2);
		PairOfPaths.writePairs(list, mainConfiguration, outputStream);
		
		// Now load this.
		List<PairOfPaths> loaded = PairOfPaths.readPairs(new StringReader(outputStream.toString()), mainConfiguration,converter);
		Assert.assertEquals(2,loaded.size());
		
		PairOfPaths r=loaded.get(0);
		Assert.assertEquals("A",gr.getVertex(r.getQ()).getStringId());Assert.assertEquals("C",gr.getVertex(r.getR()).getStringId());
		r=loaded.get(1);
		Assert.assertEquals("B",gr.getVertex(r.getQ()).getStringId());Assert.assertEquals("C",gr.getVertex(r.getR()).getStringId());
	}
	
	/** Same as above but with one single-state element. */
	@Test
	public void testWritePairsToXML3()
	{
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		PairOfPaths pair1 = new PairOfPaths(gr,new PairScore(null, gr.findVertex(VertexID.parseID("C")),1,2)),
				pair2 = new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("C")),1,2));
		List<PairOfPaths> list = new LinkedList<PairOfPaths>();list.add(pair1);list.add(pair2);
		PairOfPaths.writePairs(list, mainConfiguration, outputStream);
		
		// Now load this.
		List<PairOfPaths> loaded = PairOfPaths.readPairs(new StringReader(outputStream.toString()), mainConfiguration,converter);
		Assert.assertEquals(2,loaded.size());
		
		PairOfPaths r=loaded.get(0);
		Assert.assertNull(r.getQ());Assert.assertEquals("C",gr.getVertex(r.getR()).getStringId());
		r=loaded.get(1);
		Assert.assertEquals("B",gr.getVertex(r.getQ()).getStringId());Assert.assertEquals("C",gr.getVertex(r.getR()).getStringId());
	}
	
	@Test
	public void testRebuildStack1()
	{
		LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		PairOfPaths pair1 = new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		Stack<PairScore> stack=new Stack<PairScore>();
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		pair1.rebuildStack(gr, stack);
		Assert.assertEquals(1,stack.size());
		PairScore r = stack.pop();
		Assert.assertEquals("A",r.getQ().getStringId());Assert.assertEquals("C",r.getR().getStringId());
	}	
	
	@Test
	public void testRebuildStack2()
	{
		LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		PairOfPaths pair1 = new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		Stack<PairScore> stack=new Stack<PairScore>();
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("A")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("C")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		pair1.rebuildStack(gr, stack);
		Assert.assertEquals(1,stack.size());
		PairScore r = stack.pop();
		Assert.assertEquals("A",r.getQ().getStringId());Assert.assertEquals("C",r.getR().getStringId());
	}
	
	@Test
	public void testRebuildStack3()
	{
		LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		PairOfPaths pair1 = new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		Stack<PairScore> stack=new Stack<PairScore>();
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("C")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("A")),1,2));
		pair1.rebuildStack(gr, stack);
		Assert.assertEquals(1,stack.size());
		PairScore r = stack.pop();
		Assert.assertEquals("A",r.getQ().getStringId());Assert.assertEquals("C",r.getR().getStringId());
	}	
	
	@Test
	public void testRebuildStackFail1()
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		final PairOfPaths pair1 = new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		final Stack<PairScore> stack=new Stack<PairScore>();
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			pair1.rebuildStack(gr, stack);
		}},IllegalArgumentException.class,"pair not found");
	}
	
	@Test
	public void testRebuildStackFail2()
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		final PairOfPaths pair1 = new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		final Stack<PairScore> stack=new Stack<PairScore>();
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("A")),2,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("C")),2,2));
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			pair1.rebuildStack(gr, stack);
		}},IllegalArgumentException.class,"pair not found");
	}
	
	@Test
	public void testRebuildStackFail3()
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		final PairOfPaths pair1 = new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		final Stack<PairScore> stack=new Stack<PairScore>();
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("C")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("B")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("A")),2,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("C")),2,2));
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			pair1.rebuildStack(gr, stack);
		}},IllegalArgumentException.class,"pair not found");
	}
	
	@Test
	public void testRebuildStackFail4()
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-c->C", "testWritePairsToXML1",mainConfiguration,converter);
		final PairOfPaths pair1 = new PairOfPaths(gr,new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("C")),1,2));
		final Stack<PairScore> stack=new Stack<PairScore>();
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("C")),2,2));
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			pair1.rebuildStack(gr, stack);
		}},IllegalArgumentException.class,"pair not found");
	}
	
	
	@Test
	public void testChoices1a()
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-a->C", "testChoices1a",mainConfiguration,converter);
		final Stack<PairScore> stack=new Stack<PairScore>();
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("C")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("B")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("A")),2,2));// same scores, same red states but different blue ones.
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("A")),2,2));
		Assert.assertEquals(2,PaperUAS.countChoices(stack));
		
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("A")),2,2), 
				PaperUAS.selectPairMinMax(gr, stack, PaperUAS.pairchoiceMAX));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("A")),2,2), 
				PaperUAS.selectPairMinMax(gr, stack, PaperUAS.pairchoiceMIN));
		
		Random seedSel = new Random(0);
		
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("A")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("A")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("A")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
	}
	
	/** Same scores but different red states. */
	@Test
	public void testChoices1b()
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-a->C", "testChoices1a",mainConfiguration,converter);
		final Stack<PairScore> stack=new Stack<PairScore>();
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("C")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("A")), gr.findVertex(VertexID.parseID("A")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("A")),2,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2));
		Assert.assertEquals(1,PaperUAS.countChoices(stack));
	}
	
	@Test
	public void testChoices1c()
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-a->C", "testChoices1a",mainConfiguration,converter);
		final Stack<PairScore> stack=new Stack<PairScore>();
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("A")),2,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("A")),2,2));
		Assert.assertEquals(2,PaperUAS.countChoices(stack));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("A")),2,2), 
				PaperUAS.selectPairMinMax(gr, stack, PaperUAS.pairchoiceMAX));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("A")),2,2), 
				PaperUAS.selectPairMinMax(gr, stack, PaperUAS.pairchoiceMIN));
		
		Random seedSel = new Random(0);
		
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("A")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("A")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("A")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
	}
	
	@Test
	public void testCountChoices2a()
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-a->C", "testChoices1a",mainConfiguration,converter);
		final Stack<PairScore> stack=new Stack<PairScore>();
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("C")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("B")), gr.findVertex(VertexID.parseID("A")),1,2));
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2));
		Assert.assertEquals(1,PaperUAS.countChoices(stack));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2), 
				PaperUAS.selectPairMinMax(gr, stack, PaperUAS.pairchoiceMAX));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2), 
				PaperUAS.selectPairMinMax(gr, stack, PaperUAS.pairchoiceMIN));
		
		Random seedSel = new Random(0);
		
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
	}
	
	@Test
	public void testCountChoices2b()
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->A-b->B-a->C", "testChoices1a",mainConfiguration,converter);
		final Stack<PairScore> stack=new Stack<PairScore>();
		stack.push(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2));
		Assert.assertEquals(1,PaperUAS.countChoices(stack));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2), 
				PaperUAS.selectPairMinMax(gr, stack, PaperUAS.pairchoiceMAX));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2), 
				PaperUAS.selectPairMinMax(gr, stack, PaperUAS.pairchoiceMIN));
		
		Random seedSel = new Random(0);
		
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
		Assert.assertEquals(new PairScore(gr.findVertex(VertexID.parseID("C")), gr.findVertex(VertexID.parseID("B")),2,2), 
				PaperUAS.selectPairAtRandom(stack, new Random(seedSel.nextLong())));
	}
	
	@Test
	public void testCountChoices3()
	{
		final Stack<PairScore> stack=new Stack<PairScore>();
		Assert.assertEquals(0,PaperUAS.countChoices(stack));
	}
	
	@Test
	public void testLearnIfThen1()
	{
		Assert.assertTrue(PaperUAS.learnIfThen(new LearnerGraph(mainConfiguration), 0,0).isEmpty());
	}
	
	@Test
	public void testLearnIfThen2()
	{
		Assert.assertTrue(PaperUAS.learnIfThen(FsmParser.buildLearnerGraph("A-a->B-a->C-b->D", "testLearnIfThen2", mainConfiguration,converter), 0,0.5).isEmpty());
	}
	
	@Test
	public void testLearnIfThen3()
	{
		Map<Label,Map<Label,Double>> outcome = PaperUAS.learnIfThen(FsmParser.buildLearnerGraph("A-a->B-a->C-b->D", "testLearnIfThen2", mainConfiguration,converter), 0,0);
		Assert.assertEquals(1,outcome.size());
		Label a= AbstractLearnerGraph.generateNewLabel("a", mainConfiguration);
		Map<Label,Double> entry = outcome.get(a);Assert.assertEquals(0.5, entry.get(a),Configuration.fpAccuracy);
	}
	
	@Test
	public void testLearnIfThen4()
	{
		Map<Label,Map<Label,Double>> outcome = PaperUAS.learnIfThen(FsmParser.buildLearnerGraph("A-a->B-a->C-a->D-a->E", "testLearnIfThen4", mainConfiguration,converter), 0,0);
		Assert.assertEquals(1,outcome.size());
		Label a= AbstractLearnerGraph.generateNewLabel("a", mainConfiguration);
		Map<Label,Double> entry = outcome.get(a);Assert.assertEquals(0.75, entry.get(a),Configuration.fpAccuracy);
	}
	
	@Test
	public void testLearnIfThen5()
	{
		Assert.assertTrue(PaperUAS.learnIfThen(FsmParser.buildLearnerGraph("A-a->B-a->C-b->D", "testLearnIfThen2", mainConfiguration,converter), 1,0).isEmpty());
	}
	
	@Test
	public void testLearnIfThen6()
	{
		Assert.assertTrue(PaperUAS.learnIfThen(FsmParser.buildLearnerGraph("A-a->B-a->C-a->D-a-#E", "testLearnIfThen5", mainConfiguration,converter), 0,0).isEmpty());
	}
	
	@Test
	public void testLearnIfThen7()
	{
		Map<Label,Map<Label,Double>> outcome = PaperUAS.learnIfThen(FsmParser.buildLearnerGraph("A-a->B-a->C-a->D-a->E-a->A-b-#F", "testLearnIfThen6", mainConfiguration,converter), 0,0);
		Assert.assertEquals(1,outcome.size());
		Label a= AbstractLearnerGraph.generateNewLabel("a", mainConfiguration),b= AbstractLearnerGraph.generateNewLabel("b", mainConfiguration);
		Map<Label,Double> entryA = outcome.get(a);Assert.assertEquals(1, entryA.get(a),Configuration.fpAccuracy);Assert.assertEquals(-0.2, entryA.get(b),Configuration.fpAccuracy);
	}
	
}