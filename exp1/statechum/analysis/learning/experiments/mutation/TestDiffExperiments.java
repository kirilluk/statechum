package statechum.analysis.learning.experiments.mutation;

import static statechum.analysis.learning.rpnicore.FsmParser.buildGraph;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;

public class TestDiffExperiments {
	@Test
	public void testClassify1()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph from = new LearnerGraph(buildGraph("A-a->A-b->B / A-c-#C","testClassify1a"),config),
		to=new LearnerGraph(buildGraph("A-b->A-a->B","testClassify1b"),config);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"a"}
		}), from, to);
		Assert.assertEquals(1.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(1.,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(1.,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0.5,matrix.BCR(),Configuration.fpAccuracy);
	}

	@Test
	public void testClassify2()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph from = new LearnerGraph(buildGraph("A-a->A-b->B / A-c-#C","testClassify1a"),config),
		to=new LearnerGraph(buildGraph("A-b->A-a->B","testClassify1b"),config);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"notransition"}
		}), from, to);
		Assert.assertEquals(0.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(1.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0.5,matrix.BCR(),Configuration.fpAccuracy);
	}

	@Test
	public void testClassify3()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph from = new LearnerGraph(buildGraph("A-a->A-b->B / A-c-#C","testClassify1a"),config),
		to=new LearnerGraph(buildGraph("A-b->A-a->B","testClassify1b"),config);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"c"}
		}), from, to);
		Assert.assertEquals(0.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(1.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0.5,matrix.BCR(),Configuration.fpAccuracy);
	}
	
	@Test
	public void testClassify4()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph from = new LearnerGraph(buildGraph("A-a->A-b->B / A-c-#C","testClassify1a"),config),
		to=new LearnerGraph(buildGraph("A-b->A-a->B","testClassify1b"),config);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"a","a"} // FN
		}), from, to);
		Assert.assertEquals(0.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0,matrix.BCR(),Configuration.fpAccuracy);
	}

	
	@Test
	public void testClassify5()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph from = new LearnerGraph(buildGraph("A-a->A-b->B / A-c-#C","testClassify1a"),config),
		to=new LearnerGraph(buildGraph("A-b->A-a->B","testClassify1b"),config);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"a","a"} // FN
				,new String[]{"b"} // TP
		}), from, to);
		Assert.assertEquals(1.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(0.5,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.66666666,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0.25,matrix.BCR(),Configuration.fpAccuracy);
	}

	@Test
	public void testClassify6()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph from = new LearnerGraph(buildGraph("A-a->A-b->B / A-c-#C","testClassify1a"),config),
		to=new LearnerGraph(buildGraph("A-b->A-a->B","testClassify1b"),config);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"a","a"} // FN
				,new String[]{"b","b"} // FP
		}), from, to);
		Assert.assertEquals(0.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(0,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0,matrix.BCR(),Configuration.fpAccuracy);
	}


	@Test
	public void testClassify7()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph from = new LearnerGraph(buildGraph("A-a->A-b->B / A-c-#C","testClassify1a"),config),
		to=new LearnerGraph(buildGraph("A-b->A-a->B","testClassify1b"),config);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"a","a"} // FN
				,new String[]{"b","b"} // FP
				,new String[]{"c"} // TN
		}), from, to);
		Assert.assertEquals(0.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(0,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(0.5,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0.25,matrix.BCR(),Configuration.fpAccuracy);
	}
	
	/* More sequences. */
	@Test
	public void testClassify8()
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph from = new LearnerGraph(buildGraph("A-a->A-b->B / A-c-#C","testClassify1a"),config),
		to=new LearnerGraph(buildGraph("A-b->A-a->B","testClassify1b"),config);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[] {"a"} // TP
				,new String[] {"b"} // TP
				,new String[]{"b","b"} // FP
				,new String[]{"c"} // TN
		}), from, to);
		Assert.assertEquals(2./3.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(1,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.8,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(0.5,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0.75,matrix.BCR(),Configuration.fpAccuracy);
	}
	
}
