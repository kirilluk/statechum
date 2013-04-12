package statechum.analysis.learning.experiments.mutation;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

import java.util.Collection;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.TestWithMultipleConfigurations;

@RunWith(Parameterized.class)
public class TestDiffExperiments extends TestWithMultipleConfigurations
{
	@Parameters
	public static Collection<Object[]> data() 
	{
		return TestWithMultipleConfigurations.data();
	}
	
	public static String parametersToString(Configuration config)
	{
		return TestWithMultipleConfigurations.parametersToString(config);
	}
	
	public TestDiffExperiments(Configuration conf)
	{
		super(conf);
	}

	@Test
	public void testClassify1()
	{
		LearnerGraph from = buildLearnerGraph("A-a->A-b->B / A-c-#C","testClassify1a",mainConfiguration,converter),
		to= buildLearnerGraph("A-b->A-a->B","testClassify1b",mainConfiguration,converter);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"a"}
		},mainConfiguration,converter), from, to);
		Assert.assertEquals(1.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(1.,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(1.,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0.5,matrix.BCR(),Configuration.fpAccuracy);
	}

	@Test
	public void testClassify2()
	{
		LearnerGraph from = buildLearnerGraph("A-a->A-b->B / A-c-#C","testClassify1a",mainConfiguration,converter),
		to=buildLearnerGraph("A-b->A-a->B","testClassify1b",mainConfiguration,converter);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"notransition"}
		},mainConfiguration,converter), from, to);
		Assert.assertEquals(0.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(1.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0.5,matrix.BCR(),Configuration.fpAccuracy);
	}

	@Test
	public void testClassify3()
	{
		LearnerGraph from = buildLearnerGraph("A-a->A-b->B / A-c-#C","testClassify1a",mainConfiguration,converter),
		to=buildLearnerGraph("A-b->A-a->B","testClassify1b",mainConfiguration,converter);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"c"}
		},mainConfiguration,converter), from, to);
		Assert.assertEquals(0.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(1.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0.5,matrix.BCR(),Configuration.fpAccuracy);
	}
	
	@Test
	public void testClassify4()
	{
		LearnerGraph from = buildLearnerGraph("A-a->A-b->B / A-c-#C","testClassify1a",mainConfiguration,converter),
		to=buildLearnerGraph("A-b->A-a->B","testClassify1b",mainConfiguration,converter);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"a","a"} // FN
		},mainConfiguration,converter), from, to);
		Assert.assertEquals(0.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0,matrix.BCR(),Configuration.fpAccuracy);
	}

	
	@Test
	public void testClassify5()
	{
		LearnerGraph from = buildLearnerGraph("A-a->A-b->B / A-c-#C","testClassify1a",mainConfiguration,converter),
		to=buildLearnerGraph("A-b->A-a->B","testClassify1b",mainConfiguration,converter);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"a","a"} // FN
				,new String[]{"b"} // TP
		},mainConfiguration,converter), from, to);
		Assert.assertEquals(1.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(0.5,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.66666666,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0.25,matrix.BCR(),Configuration.fpAccuracy);
	}

	@Test
	public void testClassify6()
	{
		LearnerGraph from = buildLearnerGraph("A-a->A-b->B / A-c-#C","testClassify1a",mainConfiguration,converter),
		to=buildLearnerGraph("A-b->A-a->B","testClassify1b",mainConfiguration,converter);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"a","a"} // FN
				,new String[]{"b","b"} // FP
		},mainConfiguration,converter), from, to);
		Assert.assertEquals(0.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(0,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(0.,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0,matrix.BCR(),Configuration.fpAccuracy);
	}


	@Test
	public void testClassify7()
	{
		LearnerGraph from = buildLearnerGraph("A-a->A-b->B / A-c-#C","testClassify1a",mainConfiguration,converter),
		to=buildLearnerGraph("A-b->A-a->B","testClassify1b",mainConfiguration,converter);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[]{"a","a"} // FN
				,new String[]{"b","b"} // FP
				,new String[]{"c"} // TN
		},mainConfiguration,converter), from, to);
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
		LearnerGraph from = buildLearnerGraph("A-a->A-b->B / A-c-#C","testClassify1a",mainConfiguration,converter),
		to=buildLearnerGraph("A-b->A-a->B","testClassify1b",mainConfiguration,converter);
		
		ConfusionMatrix matrix = DiffExperiments.classify(TestFSMAlgo.buildSet(new String[][]{
				new String[] {"a"} // TP
				,new String[] {"b"} // TP
				,new String[]{"b","b"} // FP
				,new String[]{"c"} // TN
		},mainConfiguration,converter), from, to);
		Assert.assertEquals(2./3.,matrix.getPrecision(),Configuration.fpAccuracy);
		Assert.assertEquals(1,matrix.getRecall(),Configuration.fpAccuracy);
		Assert.assertEquals(0.8,matrix.fMeasure(),Configuration.fpAccuracy);
		Assert.assertEquals(0.5,matrix.getSpecificity(),Configuration.fpAccuracy);
		Assert.assertEquals(0.75,matrix.BCR(),Configuration.fpAccuracy);
	}
	
}
