package statechum.analysis.learning.experiments;

import java.awt.print.Paper;
import java.io.StringReader;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.Label;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.experiments.PaperUAS;
import statechum.analysis.learning.experiments.PaperUAS.TracesForSeed;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;

public class TestPaperUAS {

	protected Configuration config;
	protected PaperUAS paper;
	
	@Before
	public void BeforeTests()
	{
		config = Configuration.getDefaultConfiguration().copy();paper = new PaperUAS();
	}
	
	/** Empty traces */
	@Test
	public void testLoad1a()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		paper.loadData(new StringReader(""), config);
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
	}

	/** Empty line */
	@Test
	public void testLoad1b()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("\n\n"), config);
		}},IllegalArgumentException.class,"invalid match");
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
	}

	/** A single trace, with zero (initial) timestamp. */
	@Test
	public void testLoad2()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		paper.loadData(new StringReader("0,UAV3,4, + [[aa]]"), config);
		Map<Integer,Set<List<Label>>> uav3Positive = paper.collectionOfTraces.get("4").collectionOfPositiveTraces.get("UAV3");
		Map<Integer,Set<List<Label>>> uav3Negative = paper.collectionOfTraces.get("4").collectionOfNegativeTraces.get("UAV3");
		Assert.assertEquals(1,uav3Positive.size());
		Assert.assertEquals(1,uav3Negative.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, config).equals(
				uav3Positive.get(0)));
		Assert.assertTrue(uav3Negative.get(0).isEmpty());
	}

	/** A single trace, with non-zero initial timestamp. */
	@Test
	public void testLoad3a1()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		paper.loadData(new StringReader("1,UAV3,4, + [[aa]]"), config);
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
					
			), config);
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
					
			), config);
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
		), config);
		}},IllegalArgumentException.class,"current frame number");
	}
	
	/** A single trace, invalid UAV name. */
	@Test
	public void testLoad3b1()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,"+PaperUAS.UAVAll+",4, + [[aa]]"), config);
		}},IllegalArgumentException.class,"UAV name");
	}
	
	/** A single trace, invalid UAV name. */
	@Test
	public void testLoad3b2()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,"+PaperUAS.UAVAllSeeds+",4, + [[aa]]"), config);
		}},IllegalArgumentException.class,"UAV name");
	}
	
	/** A single trace, invalid seed name. */
	@Test
	public void testLoad3b3()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,valid,"+PaperUAS.UAVAllSeeds+", + [[aa]]"), config);
		}},IllegalArgumentException.class,"seed name");
	}

	/** A single trace, cannot parse. */
	@Test
	public void testLoad3c()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,,4, + [[aa]]"), config);
		}},IllegalArgumentException.class,"failed to lex");
	}
	
	/** An empty trace. */
	@Test
	public void testLoad3d()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		paper.loadData(new StringReader("0,AA,4,  "), config);
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		
		// two special UAVs, collecting all traces in a specific seed and the one collecting traces across seeds.
		Assert.assertEquals(3,tr.collectionOfPositiveTraces.size());
		Assert.assertEquals(3,tr.collectionOfNegativeTraces.size());
		Assert.assertEquals(1,tr.collectionOfPositiveTraces.get("AA").size());
		Assert.assertEquals(1,tr.collectionOfNegativeTraces.get("AA").size());
		Assert.assertTrue(tr.collectionOfPositiveTraces.get("AA").get(0).isEmpty());
		Assert.assertTrue(tr.collectionOfNegativeTraces.get("AA").get(0).isEmpty());
		
		Assert.assertEquals(1,tr.collectionOfPositiveTraces.get(PaperUAS.UAVAll).size());
		Assert.assertEquals(1,tr.collectionOfNegativeTraces.get(PaperUAS.UAVAll).size());
		Assert.assertTrue(tr.collectionOfPositiveTraces.get(PaperUAS.UAVAll).get(0).isEmpty());
		Assert.assertTrue(tr.collectionOfNegativeTraces.get(PaperUAS.UAVAll).get(0).isEmpty());
	}
	
	/** A single trace, cannot parse invalid trace. */
	@Test
	public void testLoad3e()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("0,AA,4,  u"), config);
		}},IllegalArgumentException.class,"a collection of traces");
	}
	
	/** A single trace, invalid frame number. */
	@Test
	public void testLoad3f()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			paper.loadData(new StringReader("pp,AA,4,  u"), config);
		}},NumberFormatException.class,"pp");
	}
	
	/** A single trace, with zero initial timestamp. */
	@Test
	public void testLoad3g()
	{
		Assert.assertTrue(paper.collectionOfTraces.isEmpty());
		paper.loadData(new StringReader("0,UAV3,4, + [[aa]]"), config);
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav3Positive = tr.collectionOfPositiveTraces.get("UAV3");
		Assert.assertEquals(1,uav3Positive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, config).equals(
				uav3Positive.get(0)));
		Map<Integer,Set<List<Label>>> uav3Negative= tr.collectionOfNegativeTraces.get("UAV3");
		Assert.assertEquals(1,uav3Negative.size());
		Assert.assertTrue(uav3Negative.get(0).isEmpty());
		Map<Integer,Set<List<Label>>> uav3PositiveAll = tr.collectionOfPositiveTraces.get(PaperUAS.UAVAll);
		Assert.assertEquals(1,uav3PositiveAll.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, config).equals(
				uav3PositiveAll.get(0)));
		Map<Integer,Set<List<Label>>> uav3NegativeAll = tr.collectionOfNegativeTraces.get(PaperUAS.UAVAll);
		Assert.assertEquals(1,uav3NegativeAll.size());
		Assert.assertTrue(uav3NegativeAll.get(0).isEmpty());
	}
	
	/** Both positive and negative traces. */
	@Test
	public void testLoad4()
	{
		paper.loadData(new StringReader("0,UAV3,4, + [[aa],[bb,cc]] - [[zz]]"), config);
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav3Positive = tr.collectionOfPositiveTraces.get("UAV3");
		Assert.assertEquals(1,uav3Positive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"bb","cc"}}, config).equals(
				uav3Positive.get(0)));
		Map<Integer,Set<List<Label>>> uav3Negative = tr.collectionOfNegativeTraces.get("UAV3");
		Assert.assertEquals(1,uav3Negative.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"zz"}}, config).equals(
				uav3Negative.get(0)));
	}
	
	/** Positives and negatives, multiple UAVs, interleaved. */
	@Test
	public void testLoad5()
	{
		paper.loadData(new StringReader("0,UAV3,4, + [[aa],[bb,cc]] - [[zz]]\n"+
				"0,UAV55,4, - [[Faa],[bb,Fcc]]\n"+
				"1,UAV3,4, + [[Taa],[bb,cc]] - [[Tzz]]\n"
		), config);
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav3Positive = tr.collectionOfPositiveTraces.get("UAV3");
		Assert.assertEquals(2,uav3Positive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"bb","cc"}}, config).equals(
				uav3Positive.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"Taa"},new String[]{"bb","cc"}}, config).equals(
				uav3Positive.get(1)));
		Map<Integer,Set<List<Label>>> uav3Negative = tr.collectionOfNegativeTraces.get("UAV3");
		Assert.assertEquals(2,uav3Negative.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"zz"}}, config).equals(
				uav3Negative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"zz"},new String[]{"Tzz"}}, config).equals(
				uav3Negative.get(1)));
		
		Map<Integer,Set<List<Label>>> uav55Positive = tr.collectionOfPositiveTraces.get("UAV55");
		Map<Integer,Set<List<Label>>> uav55Negative = tr.collectionOfNegativeTraces.get("UAV55");
		Assert.assertEquals(2,uav55Negative.size());
		Assert.assertEquals(2,uav55Positive.size());
		Assert.assertTrue(uav55Positive.get(0).isEmpty());Assert.assertTrue(uav55Positive.get(1).isEmpty());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, config).equals(
				uav55Negative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, config).equals(
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
		), config);
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav55Positive = tr.collectionOfPositiveTraces.get("UAV55");
		Map<Integer,Set<List<Label>>> uav55Negative = tr.collectionOfNegativeTraces.get("UAV55");
		Assert.assertEquals(2,uav55Negative.size());
		Assert.assertEquals(2,uav55Positive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, config).equals(
				uav55Positive.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, config).equals(
				uav55Positive.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, config).equals(
				uav55Negative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, config).equals(
				uav55Negative.get(1)));

		Map<Integer,Set<List<Label>>> uavAllPositive = tr.collectionOfPositiveTraces.get(PaperUAS.UAVAll);
		Assert.assertEquals(2,uavAllPositive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"qq"},new String[]{"bb","cc"}}, config).equals(
				uavAllPositive.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"qq"},new String[]{"Taa"},new String[]{"bb","cc"}}, config).equals(
				uavAllPositive.get(1)));
		Map<Integer,Set<List<Label>>> uavAllNegative = tr.collectionOfNegativeTraces.get(PaperUAS.UAVAll);
		Assert.assertEquals(2,uavAllNegative.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"},
				new String[]{"zz"}}, config).equals(
				uavAllNegative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"},
				new String[]{"zz"}, new String[]{"Tzz"}}, config).equals(
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
		), config);
		TracesForSeed tr = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav55Positive = tr.collectionOfPositiveTraces.get("UAV55");
		Map<Integer,Set<List<Label>>> uav55Negative = tr.collectionOfNegativeTraces.get("UAV55");
		Assert.assertEquals(3,uav55Negative.size());
		Assert.assertEquals(3,uav55Positive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, config).equals(
				uav55Positive.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, config).equals(
				uav55Positive.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, config).equals(
				uav55Positive.get(2)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, config).equals(
				uav55Negative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, config).equals(
				uav55Negative.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"}}, config).equals(
				uav55Negative.get(2)));

		Map<Integer,Set<List<Label>>> uavAllPositive = tr.collectionOfPositiveTraces.get(PaperUAS.UAVAll);
		Assert.assertEquals(3,uavAllPositive.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"qq"},new String[]{"bb","cc"}}, config).equals(
				uavAllPositive.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"qq"},new String[]{"Taa"},new String[]{"bb","cc"}}, config).equals(
				uavAllPositive.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"anotherOne"},new String[]{"aa"},new String[]{"qq"},new String[]{"Taa"},new String[]{"bb","cc"}}, config).equals(
				uavAllPositive.get(2)));
		Map<Integer,Set<List<Label>>> uavAllNegative = tr.collectionOfNegativeTraces.get(PaperUAS.UAVAll);
		Assert.assertEquals(3,uavAllNegative.size());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"},
				new String[]{"zz"}}, config).equals(
				uavAllNegative.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"},
				new String[]{"zz"}, new String[]{"Tzz"}}, config).equals(
				uavAllNegative.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb","Fcc"},
				new String[]{"zz"}, new String[]{"Tzz"}}, config).equals(
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
		), config);
		TracesForSeed tr4 = paper.collectionOfTraces.get("4");
		Map<Integer,Set<List<Label>>> uav3Positive4 = tr4.collectionOfPositiveTraces.get("UAV3");
		Map<Integer,Set<List<Label>>> uav3Negative4 = tr4.collectionOfNegativeTraces.get("UAV3");
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, config).equals(
				uav3Positive4.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"}}, config).equals(
				uav3Positive4.get(1)));
		Assert.assertTrue(uav3Negative4.get(0).isEmpty());
		Assert.assertTrue(uav3Negative4.get(1).isEmpty());
		Map<Integer,Set<List<Label>>> uav55Positive4 = tr4.collectionOfPositiveTraces.get("UAV55");
		Map<Integer,Set<List<Label>>> uav55Negative4 = tr4.collectionOfNegativeTraces.get("UAV55");
		Assert.assertTrue(uav55Positive4.get(0).isEmpty());
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"qq"}}, config).equals(
				uav55Positive4.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb"}}, config).equals(
				uav55Negative4.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb"}}, config).equals(
				uav55Negative4.get(1)));
	
		TracesForSeed tr2 = paper.collectionOfTraces.get("SEED2");
		Map<Integer,Set<List<Label>>> uav3Positive2 = tr2.collectionOfPositiveTraces.get("UAV3");
		Map<Integer,Set<List<Label>>> uav3Negative2 = tr2.collectionOfNegativeTraces.get("UAV3");
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Taa"}}, config).equals(
				uav3Positive2.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Taa"},new String[]{"anotherOne"}}, config).equals(
				uav3Positive2.get(1)));
		Assert.assertTrue(uav3Negative2.get(0).isEmpty());
		Assert.assertTrue(uav3Negative2.get(1).isEmpty());
		
		Map<Integer,Set<List<Label>>> uav55Positive2 = tr2.collectionOfPositiveTraces.get("UAV55");
		Map<Integer,Set<List<Label>>> uav55Negative2 = tr2.collectionOfNegativeTraces.get("UAV55");
		Assert.assertTrue(uav55Positive2.get(0).isEmpty());
		Assert.assertTrue(uav55Positive2.get(1).isEmpty());
		Assert.assertTrue(uav55Negative2.get(0).isEmpty());
		Assert.assertTrue(uav55Negative2.get(1).isEmpty());
		
		TracesForSeed trAll = paper.collectionOfTraces.get(PaperUAS.UAVAllSeeds);
		Assert.assertEquals(4,trAll.collectionOfPositiveTraces.size());
		Assert.assertEquals(4,trAll.collectionOfNegativeTraces.size());
		Map<Integer,Set<List<Label>>> uavAllPositiveAll = trAll.collectionOfPositiveTraces.get(PaperUAS.UAVAllSeeds);
		Map<Integer,Set<List<Label>>> uavAllNegativeAll = trAll.collectionOfNegativeTraces.get(PaperUAS.UAVAllSeeds);
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"Taa"}}, config).equals(
				uavAllPositiveAll.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"aa"},new String[]{"Taa"},new String[]{"qq"},new String[]{"anotherOne"}}, config).equals(
				uavAllPositiveAll.get(1)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb"}}, config).equals(
				uavAllNegativeAll.get(0)));
		Assert.assertTrue(TestFSMAlgo.buildSet(new String[][]{new String[]{"Faa"},new String[]{"bb"}}, config).equals(
				uavAllNegativeAll.get(1)));
		for(Entry<String,Map<Integer,Set<List<Label>>>> entry:trAll.collectionOfPositiveTraces.entrySet())
			if (!entry.getKey().equals(PaperUAS.UAVAllSeeds))
			{
				for(Entry<Integer,Set<List<Label>>> pair:entry.getValue().entrySet())
					Assert.assertTrue(pair.getValue().isEmpty());
			}
		
	}
}
