package statechum.analysis.learning;

import static java.lang.Math.abs;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static statechum.TestHelper.checkForCorrectException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringWriter;
import java.util.*;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.TestHelper;
import statechum.analysis.learning.DrawGraphs.RGraph;
import statechum.analysis.learning.DrawGraphs.ScatterPlot;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.RBagPlot;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.DrawGraphs.StatisticalTestResult;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;

import static statechum.analysis.learning.DrawGraphs.RGraph.PLOT_X_LABELS.*;
import static statechum.analysis.learning.DrawGraphs.buildStringMapFromStringPairs;
import static statechum.analysis.learning.DrawGraphs.constructPredictiveCoefficientsString;

public class TestDrawGraphs {

	@Test
	public void testVectorToRFail()
	{
		checkForCorrectException(() -> DrawGraphs.vectorToR(new LinkedList<String>(),false),IllegalArgumentException.class,"empty");
	}

	@Test
	public void testVectorToR1()
	{
		Assert.assertEquals("c(1.0)", DrawGraphs.vectorToR(Collections.singletonList(1.0),false));
	}
	
	@Test
	public void testVectorToR2()
	{
		Assert.assertEquals("c(\"1.0\")", DrawGraphs.vectorToR(Collections.singletonList(1.0),true));
	}

	@Test
	public void testVectorToR3()
	{
		Assert.assertEquals("c(\"1.0\",\"6.0\")", DrawGraphs.vectorToR(Arrays.asList(1.0,6.0),true));
	}

	@Test
	public void testVectorToR4()
	{
		Assert.assertEquals("c(\"nameA\",\"nameB\",\"nameC\")", DrawGraphs.vectorToR(Arrays.asList("nameA","nameB","nameC"),true));
	}

	@Test
	public void testBoxPlotToStringFail1()
	{
		checkForCorrectException(() -> DrawGraphs.boxPlotToString(new LinkedList<>(), new LinkedList<>(), XLABELS_TEXT_MANUAL, new LinkedList<>(),null),IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testBoxPlotToStringFail2()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.,5.));
		data.add(Arrays.asList(4.,5.,5.));
		checkForCorrectException(() -> DrawGraphs.boxPlotToString(data, new LinkedList<>(), XLABELS_TEXT_MANUAL,new LinkedList<>(),null),IllegalArgumentException.class,"mismatch");
	}

	@Test
	public void testBoxPlotToStringFail3()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.,5.));
		checkForCorrectException(() -> DrawGraphs.boxPlotToString(data, new LinkedList<>(), XLABELS_TEXT_MANUAL, new LinkedList<>(),null),IllegalArgumentException.class,"not used");
	}

	@Test
	public void testBoxPlotToString1a1()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.,5.));
		data.add(Arrays.asList(7.,8.,3.));
		String colour = DrawGraphs.defaultColour;
		Assert.assertEquals("boxplot(yaxt=\"n\",xaxt=\"n\",c(4.0,5.0,5.0),c(7.0,8.0,3.0),names=c(\"graphA\",\"graphB\"),col=c(\"green\",\"green\"))",
				DrawGraphs.boxPlotToString(data, Arrays.asList("graphA","graphB"),XLABELS_TEXT_MANUAL,Arrays.asList(colour,colour),null));
	}

    @Test
    public void testBoxPlotToString1a2()
    {
        final List<List<Double>> data = new LinkedList<>();
        data.add(Arrays.asList(4.,5.,5.));
        data.add(Arrays.asList(7.,8.,3.));
        String colour = DrawGraphs.defaultColour;
        Assert.assertEquals("boxplot(yaxt=\"n\",c(4.0,5.0,5.0),c(7.0,8.0,3.0),names=c(\"graphA\",\"graphB\"),col=c(\"green\",\"green\"))",
                DrawGraphs.boxPlotToString(data, Arrays.asList("graphA","graphB"),XLABELS_R,Arrays.asList(colour,colour),null));
    }
	/** Same colours. */
	@Test
	public void testBoxPlotToString1b()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.,5.));
		data.add(Arrays.asList(7.,8.,3.));
		String colour = DrawGraphs.defaultColour;
		Assert.assertEquals("boxplot(yaxt=\"n\",xaxt=\"n\",c(4.0,5.0,5.0),c(7.0,8.0,3.0),names=c(\"graphA\",\"graphB\"),col=c(\"green\",\"green\"),someOther attrs)",
				DrawGraphs.boxPlotToString(data, Arrays.asList("graphA","graphB"),XLABELS_TEXT_MANUAL,Arrays.asList(colour,colour),Collections.singletonList("someOther attrs")).toString());
	}
	
	/** Same as above but different colours. */
	@Test
	public void testBoxPlotToString1c()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.,5.));
		data.add(Arrays.asList(7.,8.,3.));
		Assert.assertEquals("boxplot(yaxt=\"n\",xaxt=\"n\",c(4.0,5.0,5.0),c(7.0,8.0,3.0),names=c(\"graphA\",\"graphB\"),col=c(\"red\",\"blue\"),someOther attrs)",
				DrawGraphs.boxPlotToString(data, Arrays.asList("graphA","graphB"),XLABELS_TEXT_MANUAL,Arrays.asList("red","blue"),Collections.singletonList("someOther attrs")).toString());
	}
	
	/** As above but without labels. */
	@Test
	public void testBoxPlotToString2()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.,5.));
		data.add(Arrays.asList(7.,8.,3.));
		Assert.assertEquals("boxplot(yaxt=\"n\",xaxt=\"n\",c(4.0,5.0,5.0),c(7.0,8.0,3.0),col=c(\"green\",\"green\"))",
				DrawGraphs.boxPlotToString(data, null,XLABELS_TEXT_MANUAL, null,null).toString());
	}
	
	/** As above but one vector and without labels. */
	@Test
	public void testBoxPlotToString3a()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.,5.));
		Assert.assertEquals("boxplot(yaxt=\"n\",xaxt=\"n\",c(4.0,5.0,5.0),col=c(\"green\"))",
				DrawGraphs.boxPlotToString(data, null,XLABELS_TEXT_MANUAL,null,null));
	}
	/** As above but one vector and without labels. */
	@Test
	public void testBoxPlotToString3b()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.,5.));
		Assert.assertEquals("boxplot(yaxt=\"n\",xaxt=\"n\",c(4.0,5.0,5.0),col=c(\"green\"),other attrs)",
				DrawGraphs.boxPlotToString(data, null,XLABELS_TEXT_MANUAL,null,Collections.singletonList("other attrs")).toString());
	}
	
	public static final File tmpDir = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP));
	public static final File testDir = new File(tmpDir,"__TestDrawGraphs__");

	@Test
	public void testBagPlotToStringFail1()
	{
		checkForCorrectException(() -> DrawGraphs.datasetToString("bagplot", new LinkedList<>(), new LinkedList<>(),null),IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testBagPlotToStringFail2()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.,5.));
		data.add(Arrays.asList(4.,5.,5.));
		checkForCorrectException(() -> DrawGraphs.datasetToString("bagplot",data, Collections.singletonList(6.7),null),IllegalArgumentException.class,"mismatch");
	}

	@Test
	public void testBagPlotToString1a()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.));
		data.add(Arrays.asList(7.,8.,3.));
		Assert.assertEquals("bagplot(c(7.0,7.0,8.3,8.3,8.3),c(4.0,5.0,7.0,8.0,3.0))",
				DrawGraphs.datasetToString("bagplot",data, Arrays.asList(7.,8.3),null));
	}

	@Test
	public void testBagPlotToString1b()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.));
		data.add(Arrays.asList(7.,8.,3.));
		Assert.assertEquals("bagplot(c(7.0,7.0,8.3,8.3,8.3),c(4.0,5.0,7.0,8.0,3.0),someOther attrs)",
				DrawGraphs.datasetToString("bagplot",data, Arrays.asList(7.,8.3),Collections.singletonList("someOther attrs")));
	}

	
	@Test
	public void testWilcoxonTestToStringFail1()
	{
		checkForCorrectException(() -> new DrawGraphs.WilcoxonPairedTest(new File("test")).getDrawingCommand(),IllegalArgumentException.class,"empty");
	}
	
	// here we are only testing for lists valuesA and valuesB being empty because they cannot be null by construction due to being final and a non-null initialisation.
	
	@Test
	public void testWilcoxonTestToStringFail2()
	{
		final DrawGraphs.WilcoxonPairedTest w = new DrawGraphs.WilcoxonPairedTest(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 6.);
		w.valuesB.clear();
		
		checkForCorrectException(w::getDrawingCommand,IllegalArgumentException.class,"empty");
	}

	@Test
	public void testWilcoxonTestToStringFail3()
	{
		final DrawGraphs.WilcoxonPairedTest w = new DrawGraphs.WilcoxonPairedTest(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 6.);
		w.valuesA.clear();
		
		checkForCorrectException(w::getDrawingCommand,IllegalArgumentException.class,"empty");
	}

	@Test
	public void testWilcoxonTestToStringFail4()
	{
		final DrawGraphs.WilcoxonPairedTest w = new DrawGraphs.WilcoxonPairedTest(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 6.);w.valuesB.remove(2);
		
		checkForCorrectException(w::getDrawingCommand,IllegalArgumentException.class," 'x' and 'y' must have the same length");
	}
	
	@Test
	public void testWilcoxonTestToString()
	{
		final DrawGraphs.WilcoxonPairedTest w = new DrawGraphs.WilcoxonPairedTest(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 3.);
		Assert.assertEquals("[m=wilcox.test(c(4.0,5.0,5.0),c(7.0,8.0,3.0),paired=TRUE)]",
				w.getDrawingCommand().toString());
	}
	
	@Test
	public void testWilcoxonTest() throws IOException
	{
		@SuppressWarnings("unused")
		DrawGraphs gr = new DrawGraphs();// loads the R library
		final DrawGraphs.WilcoxonPairedTest w = new DrawGraphs.WilcoxonPairedTest(new File("test"));
		w.add(1., 7.);w.add(5., 8.);w.add(5., 3.);

		StringWriter s=new StringWriter();
		StatisticalTestResult result = w.obtainResultFromR(false);w.writetofile(result,s);
		Assert.assertTrue(result.valueValid);
		Assert.assertEquals("Method,Statistic,P-value\nWilcoxon signed rank test,1.0,0.5000000000000001\n",s.toString());
	}
	
	@Test
	public void testMann_Whitney_U_TestToString()
	{
		final DrawGraphs.Mann_Whitney_U_Test w = new DrawGraphs.Mann_Whitney_U_Test(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 3.);
		Assert.assertEquals("[m=wilcox.test(c(4.0,5.0,5.0),c(7.0,8.0,3.0))]",
				w.getDrawingCommand().toString());
	}
	
	@Test
	public void testMann_Whitney_U_Test() throws IOException
	{
		@SuppressWarnings("unused")
		DrawGraphs gr = new DrawGraphs();// loads the R library
		final DrawGraphs.Mann_Whitney_U_Test w = new DrawGraphs.Mann_Whitney_U_Test(new File("test"));
		w.add(1., 7.);w.add(5., 8.);w.add(5., 3.);

		StringWriter s=new StringWriter();
		StatisticalTestResult result = w.obtainResultFromR(false);w.writetofile(result,s);
		Assert.assertTrue(result.valueValid);
		Assert.assertEquals("Method,Statistic,P-value\nWilcoxon rank sum test,2.0,0.3758250874886983\n",s.toString());
	}

	@Test
	public void testVarghaDelaney_TestToString1()
	{
		final DrawGraphs.A_VarghaDelaney w = new DrawGraphs.A_VarghaDelaney(new File("test"),0);
		w.add(4., 7.);w.add(5., 8.);w.add(5., 3.);
		Assert.assertEquals("[m=ufs::A_VarghaDelaney(c(4.0,5.0,5.0),c(7.0,8.0,3.0))]",
				w.getDrawingCommand().toString());
	}

	@Test
	public void testVarghaDelaney_TestToString2()
	{
		final DrawGraphs.A_VarghaDelaney w = new DrawGraphs.A_VarghaDelaney(new File("test"),100);
		w.add(4., 7.);w.add(5., 8.);w.add(5., 3.);
		Assert.assertEquals("[m=ufs::A_VarghaDelaney(c(4.0,5.0,5.0),c(7.0,8.0,3.0),100)]",
				w.getDrawingCommand().toString());
	}
	@Test
	public void testVarghaDelaney_Test0() throws IOException
	{
		@SuppressWarnings("unused")
		DrawGraphs gr = new DrawGraphs();// loads the R library
		checkForCorrectException(() -> {
			new DrawGraphs.A_VarghaDelaney(new File("test"),-1);
		},IllegalArgumentException.class,"has to be non-negative");
	}
	@Test
	public void testVarghaDelaney_Test1() throws IOException
	{
		@SuppressWarnings("unused")
		DrawGraphs gr = new DrawGraphs();// loads the R library
		final DrawGraphs.A_VarghaDelaney w = new DrawGraphs.A_VarghaDelaney(new File("test"),0);
		w.add(1., 7.);w.add(5., 8.);w.add(5., 3.);

		StringWriter s=new StringWriter();
		StatisticalTestResult result = w.obtainResultFromR(false);w.writetofile(result,s);
		Assert.assertTrue(result.valueValid);
		Assert.assertEquals("Method,Statistic\nA_VarghaDelaney (A12) test,0.7777777777777778\n",s.toString());
	}
	@Test
	public void testVarghaDelaney_Test2() throws IOException
	{
		@SuppressWarnings("unused")
		DrawGraphs gr = new DrawGraphs();// loads the R library
		final DrawGraphs.A_VarghaDelaney w = new DrawGraphs.A_VarghaDelaney(new File("test"),100);
		Random rnd = new Random(0);
		for(double cnt=0;cnt<20;cnt++)
			w.add(rnd.nextDouble(), rnd.nextDouble()+0.2);

		w.resetRandomSeed();
		StringWriter s=new StringWriter();
		StatisticalTestResult result = w.obtainResultFromR(false);w.writetofile(result,s);
		Assert.assertTrue(result.valueValid);
		String expectedFixedPart = "Method,Statistic,confidence_lo,confidence_high\nA_VarghaDelaney (A12) test (100),0.6825,";
		Assert.assertEquals(expectedFixedPart,s.toString().substring(0,expectedFixedPart.length()));
		String[] confidence_interval = s.toString().substring(expectedFixedPart.length()).split(",");
		Assert.assertEquals(2,confidence_interval.length);
		Assert.assertTrue(abs(Double.parseDouble(confidence_interval[0])-1) < 0.01);// || Double.parseDouble(confidence_interval[0]) < 0.01);// result is either a 0 or a 1 (depending on a random seed in R as it selects different subsets of values).
		Assert.assertTrue(abs(Double.parseDouble(confidence_interval[1])-1) < 0.01);// || Double.parseDouble(confidence_interval[0]) < 0.01);// result is either a 0 or a 1 (depending on a random seed in R as it selects different subsets of values).
	}

	@Test
	public void testVarghaDelaney_Test3a() throws IOException
	{
		@SuppressWarnings("unused")
		DrawGraphs gr = new DrawGraphs();// loads the R library
		final DrawGraphs.A_VarghaDelaney w = new DrawGraphs.A_VarghaDelaney(new File("test"),10000);
		w.add(1., 7.);w.add(5., 8.);w.add(5., 3.);

		checkForCorrectException(() -> {
			w.obtainResultFromR(false);
		},IllegalArgumentException.class,"not enough");// error from R
	}
	@Test
	public void testVarghaDelaney_Test3b() throws IOException
	{
		@SuppressWarnings("unused")
		DrawGraphs gr = new DrawGraphs();// loads the R library
		final DrawGraphs.A_VarghaDelaney w = new DrawGraphs.A_VarghaDelaney(new File("test"),10000);
		w.add(1., 7.);w.add(5., 8.);w.add(5., 3.);

		StatisticalTestResult result = w.obtainResultFromR(true);
		Assert.assertFalse(result.valueValid);
	}
	@Test
	public void testKruskal_Wallis_TestToString()
	{
		final DrawGraphs.Kruskal_Wallis w = new DrawGraphs.Kruskal_Wallis(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 3.);
		Assert.assertEquals("[m=kruskal.test(c(4.0,5.0,5.0),c(7.0,8.0,3.0))]",
				w.getDrawingCommand().toString());
	}

	@Test
	public void testKruskal_Wallis_Test() throws IOException
	{
		@SuppressWarnings("unused")
		DrawGraphs gr = new DrawGraphs();// loads the R library
		final DrawGraphs.Kruskal_Wallis w = new DrawGraphs.Kruskal_Wallis(new File("test"));
		w.add(1., 7.);w.add(5., 8.);w.add(5., 3.);
		StringWriter s=new StringWriter();
		
		StatisticalTestResult result = w.obtainResultFromR(false);w.writetofile(result,s);
		Assert.assertTrue(result.valueValid);
		Assert.assertEquals(2.0,result.statistic,Configuration.fpAccuracy);// values obtained by recording the results rather than attempting to determine what should be returned.
		Assert.assertEquals(0.36787944117144233,result.pvalue,Configuration.fpAccuracy);Assert.assertEquals(2.0,result.parameter,Configuration.fpAccuracy);
		Assert.assertNull(result.alternative);
		
		Assert.assertEquals("Method,Statistic,P-value,parameter\nKruskal-Wallis rank sum test,2.0,0.36787944117144233,2.0\n",s.toString());
		//System.out.println(result.statistic+" "+result.pvalue+" "+result.alternative+" "+result.parameter+" ");
	}

	@Test
	public void testLogisticRegressionR0() {
		checkForCorrectException(() -> constructPredictiveCoefficientsString(new LinkedList<>(),"text","a"),IllegalArgumentException.class,"no data to learn from");
	}
	@Test
	public void testLogisticRegressionR1() {
		List<PairQualityLearner.PairScoreValue> values = Arrays.asList(
				new PairQualityLearner.PairScoreValue(true,10,1),
				new PairQualityLearner.PairScoreValue(false,1,10)
				);
		Assert.assertEquals(Arrays.asList("datavalues=data.frame(validity=c(1,0),score=c(10,1),inconsistency=c(1,10))","fit3=speedglm::speedglm(formula = validity ~ score + inconsistency,family = binomial(),data=datavalues)"),constructPredictiveCoefficientsString(values,"fit3","datavalues"));
	}

	@Test
	public void testLogisticRegression1() {
		new DrawGraphs();// loads the R library
		List<PairQualityLearner.PairScoreValue> values = Arrays.asList(
				new PairQualityLearner.PairScoreValue(true,10,1),
				new PairQualityLearner.PairScoreValue(false,1,10)
		);
		DrawGraphs.LogisticRegression regression = new DrawGraphs.LogisticRegression(values,"fit","pairscores");
		Assert.assertEquals("-28.803,5.237,NaN",regression.reportCoefficients());
		Assert.assertTrue(regression.evaluate(100,3));
		Assert.assertFalse(regression.evaluate(2,300));
	}

	@Test
	public void testLogisticRegression2() {
		new DrawGraphs();// loads the R library
		List<PairQualityLearner.PairScoreValue> values = Arrays.asList(
				new PairQualityLearner.PairScoreValue(true,10,1),
				new PairQualityLearner.PairScoreValue(true,20,5),
				new PairQualityLearner.PairScoreValue(false,1,10),
				new PairQualityLearner.PairScoreValue(false,0,100),
				new PairQualityLearner.PairScoreValue(false,5,20)
		);
		DrawGraphs.LogisticRegression regression = new DrawGraphs.LogisticRegression(values,"fit","pairvalues");
		Assert.assertEquals("-17.732,4.224,-1.322",regression.reportCoefficients());
		Assert.assertTrue(regression.evaluate(100,3));
		Assert.assertFalse(regression.evaluate(2,300));
		Assert.assertFalse(regression.evaluate(8,300));


		{
			List<PairQualityLearner.PairScoreValue> evaluationSet = Arrays.asList(
					new PairQualityLearner.PairScoreValue(true, 7, 1),
					new PairQualityLearner.PairScoreValue(true, 200, 5),
					new PairQualityLearner.PairScoreValue(false, 1, 1),
					new PairQualityLearner.PairScoreValue(false, 0, 10),
					new PairQualityLearner.PairScoreValue(false, 1, 15)
			);
			ConfusionMatrix confusionComputedByRegression = regression.computeConfusionMatrix(evaluationSet);
			ConfusionMatrix confusionComputedByR = regression.confusionMatrixViaR(evaluationSet, "fit", "evaluationdata", "confusion");
			Assert.assertEquals(confusionComputedByRegression, confusionComputedByR);
		}

		{
			List<PairQualityLearner.PairScoreValue> evaluationSet = Arrays.asList(
					new PairQualityLearner.PairScoreValue(true, 2, 0),
					new PairQualityLearner.PairScoreValue(true, 3, 0),
					new PairQualityLearner.PairScoreValue(true, 3, 1),
					new PairQualityLearner.PairScoreValue(true, 60, 5),
					new PairQualityLearner.PairScoreValue(false, 1, 1),
					new PairQualityLearner.PairScoreValue(false, 0, 1),
					new PairQualityLearner.PairScoreValue(false, 10, 15)
			);
			ConfusionMatrix confusionComputedByRegression = regression.computeConfusionMatrix(evaluationSet);
			ConfusionMatrix confusionComputedByR = regression.confusionMatrixViaR(evaluationSet, "fit", "evaluationdata", "confusion");
			Assert.assertEquals(confusionComputedByRegression, confusionComputedByR);
		}

	}

	public static class TestParameters implements ThreadResultID
	{
		public String rowID, columnID;
		public String [] columnText, headerForCell;
		

		public TestParameters(String row, String column, String [] colText, String [] hForCell)
		{
			rowID = row;columnID = column; columnText = colText; headerForCell = hForCell;
		}
		
		@Override
		public String getRowID() {
			return rowID;
		}

		@Override
		public String[] getColumnText() {
			return columnText;
		}

		@Override
		public String getColumnID() {
			return columnID;
		}

		@Override
		public String[] headerValuesForEachCell() {
			return headerForCell;
		}

		@Override
		public String getSubExperimentName() {
			return "experiment";
		}

		@Override
		public int executionTimeInCell() {
			return -1;
		}
	}

	@Test
	public void testCSVwriteFile1() throws IOException
	{
		File output = new File(testDir,"out.csv");
		CSVExperimentResult w = new CSVExperimentResult(output);
		TestParameters par = new TestParameters(null,"Col",new String[]{"a"}, new String[]{"b"});
		par.rowID = "Row1";w.add(par,"line A");par.rowID = "Row2";w.add(par,"line B");w.reportResults(null);
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line;
		StringBuilder buffer = new StringBuilder();
		try
		{
			while((line=reader.readLine()) != null)
			{
				buffer.append('[');buffer.append(line);buffer.append(']');
			}
		}
		finally
		{
			reader.close();
		}
		Assert.assertEquals("[,a][experiment,b][Row1,line A][Row2,line B]", buffer.toString());
	}
	
	@Test
	public void testCSVwriteFile2() throws IOException
	{
		File output = new File(testDir,"out.csv");
		CSVExperimentResult w = new CSVExperimentResult(output);w.reportResults(null);
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line;
		StringBuilder buffer = new StringBuilder();
		try
		{
			while((line=reader.readLine()) != null)
			{
				buffer.append('[');buffer.append(line);buffer.append(']');
			}
		}
		finally
		{
			reader.close();
		}
		Assert.assertEquals("", buffer.toString());
	}
	
	@Test
	public void testCSVwriteFile3a() throws IOException
	{
		File output = new File(testDir,"out.csv");
		CSVExperimentResult w = new CSVExperimentResult(output);
		TestParameters par = new TestParameters(null,"Col",new String[]{"posNeg","reference"}, new String[]{"BCR","Diff","States"});
		par.rowID = "Row1";w.add(par,"A BCR, A Diff, A states");par.rowID = "Row2";w.add(par,"B BCR, B Diff, B PTA states");w.reportResults(null);
		
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line;
		StringBuilder buffer = new StringBuilder();
		try
		{
			while((line=reader.readLine()) != null)
			{
				buffer.append('[');buffer.append(line);buffer.append(']');
			}
		}
		finally
		{
			reader.close();
		}
		Assert.assertEquals("[,posNeg,posNeg,posNeg][,reference,reference,reference][experiment,BCR,Diff,States][Row1,A BCR,A Diff,A states][Row2,B BCR,B Diff,B PTA states]", buffer.toString());
	}
	
	@Test
	public void testCSVwriteFile3b() throws IOException
	{
		File output = new File(testDir,"out.csv");
		CSVExperimentResult w = new CSVExperimentResult(output);
		TestParameters par = new TestParameters(null,"Col",new String[]{"posNeg","reference"}, new String[]{"BCR","Diff","States"});
		par.rowID = "Row1";w.add(par,"A BCR, A Diff, A states");par.rowID = "Row2";w.add(par,"B BCR, B Diff, B PTA states");
		par = new TestParameters(null,"Col2",new String[]{"pos","ref"}, new String[]{"P","Q"});
		par.rowID = "Row1";w.add(par,"p1,q1");par.rowID = "Row2";w.add(par,"p2,q2");
		w.reportResults(null);
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line;
		StringBuilder buffer = new StringBuilder();
		try
		{
			while((line=reader.readLine()) != null)
			{
				buffer.append('[');buffer.append(line);buffer.append(']');
			}
		}
		finally
		{
			reader.close();
		}
		Assert.assertEquals("[,posNeg,posNeg,posNeg,pos,pos][,reference,reference,reference,ref,ref][experiment,BCR,Diff,States,P,Q][Row1,A BCR,A Diff,A states,p1,q1][Row2,B BCR,B Diff,B PTA states,p2,q2]", buffer.toString());
	}
	
	/** Same as the test above but entries are filled in a different order. */
	@Test
	public void testCSVwriteFile3c() throws IOException
	{
		File output = new File(testDir,"out.csv");
		CSVExperimentResult w = new CSVExperimentResult(output);
		TestParameters parA = new TestParameters(null,"Col",new String[]{"posNeg","reference"}, new String[]{"BCR","Diff","States"});
		TestParameters parB = new TestParameters(null,"Col2",new String[]{"pos","ref"}, new String[]{"P","Q"});
		
		parB.rowID = "Row2";w.add(parB,"p2,q2");
		parA.rowID = "Row1";w.add(parA,"A BCR, A Diff, A states");parA.rowID = "Row2";w.add(parA,"B BCR, B Diff, B PTA states");
		parB.rowID = "Row1";w.add(parB,"p1,q1");
		w.reportResults(null);
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line;
		StringBuilder buffer = new StringBuilder();
		try
		{
			while((line=reader.readLine()) != null)
			{
				buffer.append('[');buffer.append(line);buffer.append(']');
			}
		}
		finally
		{
			reader.close();
		}
		Assert.assertEquals("[,posNeg,posNeg,posNeg,pos,pos][,reference,reference,reference,ref,ref][experiment,BCR,Diff,States,P,Q][Row1,A BCR,A Diff,A states,p1,q1][Row2,B BCR,B Diff,B PTA states,p2,q2]", buffer.toString());
	}
	
	/** Same as the test above but entries are filled in a different order. */
	@Test
	public void testCSVwriteFile3d() throws IOException
	{
		File output = new File(testDir,"out.csv");
		CSVExperimentResult w = new CSVExperimentResult(output);w.setMissingValue("MISSING");
		TestParameters parA = new TestParameters(null,"Col",new String[]{"posNeg","reference"}, new String[]{"BCR","Diff","States"});
		TestParameters parB = new TestParameters(null,"Col2",new String[]{"pos","ref"}, new String[]{"P","Q"});
		
		parA.rowID = "Row1";w.add(parA,"A BCR, A Diff, A states");parA.rowID = "Row2";w.add(parA,"B BCR, B Diff, B PTA states");
		parB.rowID = "Row1";w.add(parB,"p1,q1");
		w.reportResults(null);
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line;
		StringBuilder buffer = new StringBuilder();
		try
		{
			while((line=reader.readLine()) != null)
			{
				buffer.append('[');buffer.append(line);buffer.append(']');
			}
		}
		finally
		{
			reader.close();
		}
		Assert.assertEquals("[,posNeg,posNeg,posNeg,pos,pos][,reference,reference,reference,ref,ref][experiment,BCR,Diff,States,P,Q][Row1,A BCR,A Diff,A states,p1,q1][Row2,B BCR,B Diff,B PTA states,MISSING,MISSING]", buffer.toString());
	}
	
	// Tests that a spreadsheet can be successfully iterated through.
	@Test
	public void testScanMatrix1()
	{
		CSVExperimentResult w = new CSVExperimentResult(new File(testDir,"out.csv"));w.setMissingValue("MISSING");
		TestParameters parA = new TestParameters(null,"Col",new String[]{"posNeg","reference"}, new String[]{"BCR","Diff","States"});
		TestParameters parB = new TestParameters(null,"Col2",new String[]{"pos","ref"}, new String[]{"P","Q"});
		
		parA.rowID = "Row1";w.add(parA,"A BCR, A Diff, A states");parA.rowID = "Row2";w.add(parA,"B BCR, B Diff, B PTA states");
		parB.rowID = "Row1";w.add(parB,"p1,q1");parB.rowID = "Row2";w.add(parB,"p2,q10");
		
		final List<String> valueA= new ArrayList<>(), valueB = new ArrayList<>();
		DrawGraphs.spreadsheetAsString((A, B) -> {
            valueA.add(A);valueB.add(B);
        },w,"Col",1,"Col2",0);
		Assert.assertEquals("[A Diff, B Diff]", valueA.toString());
		Assert.assertEquals("[p1, p2]", valueB.toString());
	}
	
	@Test
	public void testScanMatrix2()
	{
		CSVExperimentResult w = new CSVExperimentResult(new File(testDir,"out.csv"));w.setMissingValue("MISSING");
		TestParameters parA = new TestParameters(null,"Col",new String[]{"posNeg","reference"}, new String[]{"BCR","Diff","States"});
		TestParameters parB = new TestParameters(null,"Col2",new String[]{"pos","ref"}, new String[]{"P","Q"});
		
		parA.rowID = "Row1";w.add(parA,"A BCR, A Diff, A states");parA.rowID = "Row2";w.add(parA,"B BCR, B Diff, B PTA states");
		parB.rowID = "Row1";w.add(parB,"p1,q1");parB.rowID = "Row2";
		
		final List<String> valueA= new ArrayList<>(), valueB = new ArrayList<>();
		DrawGraphs.spreadsheetAsString((A, B) -> {
            valueA.add(A);valueB.add(B);
        },w,"Col",1,"Col2",0);
		Assert.assertEquals("[A Diff, B Diff]", valueA.toString());
		Assert.assertEquals("[p1, null]", valueB.toString());
	}
	
	
	@Test
	public void testScanMatrix3()
	{
		CSVExperimentResult w = new CSVExperimentResult(new File(testDir,"out.csv"));w.setMissingValue("MISSING");
		TestParameters parA = new TestParameters(null,"Col",new String[]{"posNeg","reference"}, new String[]{"BCR","Diff","States"});
		TestParameters parB = new TestParameters(null,"Col2",new String[]{"pos","ref"}, new String[]{"P","Q"});
		
		parA.rowID = "Row1";w.add(parA,"A BCR, 1.5, A states");parA.rowID = "Row2";w.add(parA,"B BCR, 2, B PTA states");
		parB.rowID = "Row1";w.add(parB,"0.01,q1");parB.rowID = "Row2";w.add(parB,"0.21,q1");
		
		final List<Double> valueA= new ArrayList<>(), valueB = new ArrayList<>();
		DrawGraphs.spreadsheetAsDouble((A, B) -> {
            valueA.add(A);valueB.add(B);
        },w,"Col",1,"Col2",0);
		Assert.assertEquals("[1.5, 2.0]", valueA.toString());
		Assert.assertEquals("[0.01, 0.21]", valueB.toString());
	}
	
	// here one of the pairs is missing and is therefore ignored.
	@Test
	public void testScanMatrix4()
	{
		CSVExperimentResult w = new CSVExperimentResult(new File(testDir,"out.csv"));w.setMissingValue("MISSING");
		TestParameters parA = new TestParameters(null,"Col",new String[]{"posNeg","reference"}, new String[]{"BCR","Diff","States"});
		TestParameters parB = new TestParameters(null,"Col2",new String[]{"pos","ref"}, new String[]{"P","Q"});
		
		parA.rowID = "Row1";w.add(parA,"A BCR, 1.5, A states");parA.rowID = "Row2";w.add(parA,"B BCR, 2, B PTA states");
		parB.rowID = "Row1";w.add(parB,"0.01,q1");
		
		final List<Double> valueA= new ArrayList<>(), valueB = new ArrayList<>();
		DrawGraphs.spreadsheetAsDouble((A, B) -> {
            valueA.add(A);valueB.add(B);
        },w,"Col",1,"Col2",0);
		Assert.assertEquals("[1.5]", valueA.toString());
		Assert.assertEquals("[0.01]", valueB.toString());
	}
	
	@Test
	public void testParseObject1()
	{
		Object obj = DrawGraphs.parseObject(DrawGraphs.objectAsText("a_string"));
		Assert.assertSame(String.class,obj.getClass());
		Assert.assertEquals("a_string", obj);
	}
	
	@Test
	public void testParseObject2()
	{
		Object obj = DrawGraphs.parseObject(DrawGraphs.objectAsText(4.5));
		Assert.assertSame(Double.class,obj.getClass());
		Assert.assertEquals("4.5", obj.toString());
	}
	
	@Test
	public void testParseObjectFail0()
	{
		checkForCorrectException(() -> DrawGraphs.objectAsText(new Object()),IllegalArgumentException.class,"failed to serialise");
	}
	
	@Test
	public void testParseObjectFail1()
	{
		checkForCorrectException(() -> DrawGraphs.parseObject("jj"),IllegalArgumentException.class,"invalid char");
	}
	
	@Test
	public void testParseObjectFail2()
	{
		checkForCorrectException(() -> DrawGraphs.parseObject("A"),IllegalArgumentException.class,"should be even");
	}
	
	@Test
	public void testParseObjectFail3()
	{
		checkForCorrectException(() -> DrawGraphs.parseObject("A0"),IllegalArgumentException.class,"failed to deserialise");
	}
	
	@SuppressWarnings("ResultOfMethodCallIgnored")
    @Test
	public void testParseObjectFail4()
	{
		checkForCorrectException(() -> DrawGraphs.charToHex(400),IllegalArgumentException.class,"invalid byte");
	}
	
	
	@Test
	public void testRemoveSpaces1()
	{
		Assert.assertEquals("", LearningSupportRoutines.removeSpaces(""));
	}
	@Test
	public void testRemoveSpaces2()
	{
		Assert.assertEquals("", LearningSupportRoutines.removeSpaces("   "));
	}
	@Test
	public void testRemoveSpaces3()
	{
		Assert.assertEquals("", LearningSupportRoutines.removeSpaces("  \t   "));
	}
	@Test
	public void testRemoveSpaces4()
	{
		Assert.assertEquals("a", LearningSupportRoutines.removeSpaces("a"));
	}
	@Test
	public void testRemoveSpaces5()
	{
		Assert.assertEquals("a", LearningSupportRoutines.removeSpaces("  a"));
	}
	@Test
	public void testRemoveSpaces6()
	{
		Assert.assertEquals("a", LearningSupportRoutines.removeSpaces("a  "));
	}
	@Test
	public void testRemoveSpaces7()
	{
		Assert.assertEquals("a", LearningSupportRoutines.removeSpaces("  a  "));
	}
	@Test
	public void testRemoveSpaces8()
	{
		Assert.assertEquals("a b", LearningSupportRoutines.removeSpaces("  a b "));
	}
	
	@Test
	public void testCSVwriteFileFail1a()
	{
		File output = new File(testDir,"out.csv");
		final CSVExperimentResult w = new CSVExperimentResult(output);
		TestHelper.checkForCorrectException(() -> w.add(new TestParameters("row","col",new String[]{},new String[]{"BCR","Diff","States","PTA states"}),"a,b,c,d"), IllegalArgumentException.class,"invalid column header");
	}
	
	@Test
	public void testCSVwriteFileFail1b()
	{
		File output = new File(testDir,"out.csv");
		final CSVExperimentResult w = new CSVExperimentResult(output);
		TestHelper.checkForCorrectException(() -> w.add(new TestParameters("row","col",new String[]{"BCR","Diff","States","PTA states"},new String[]{}),"a,b,c,d"), IllegalArgumentException.class,"invalid header values for cell");
	}
	
	/** Number of elements appended does not match the number of the element in supplemental headers. */
	@Test
	public void testCSVwriteFileFail1c()
	{
		File output = new File(testDir,"out.csv");
		final CSVExperimentResult w = new CSVExperimentResult(output);
		TestHelper.checkForCorrectException(() -> w.add(new TestParameters("row","col",new String[]{"descr"},new String[]{"BCR","Diff","States","PTA states"}),"a,b,c"), IllegalArgumentException.class,"the number of values (");
	}
	
	/** Number of elements appended is zero. */
	@Test
	public void testCSVwriteFileFail1d()
	{
		File output = new File(testDir,"out.csv");
		final CSVExperimentResult w = new CSVExperimentResult(output);
		TestHelper.checkForCorrectException(() -> w.add(new TestParameters("row","col",new String[]{"descr"},new String[]{})," "), IllegalArgumentException.class,"invalid header values");
	}
	
	/** Number of elements appended is zero. */
	@Test
	public void testCSVwriteFileFail1e()
	{
		File output = new File(testDir,"out.csv");
		final CSVExperimentResult w = new CSVExperimentResult(output);
		TestHelper.checkForCorrectException(() -> w.add(new TestParameters("row","col",new String[]{"descr"},new String[]{"a"})," "), IllegalArgumentException.class,"empty line added at");
	}
	
	/** The number of elements in a header between two writes does not match. */
	@Test
	public void testCSVwriteFileFail2()
	{
		File output = new File(testDir,"out.csv");
		final CSVExperimentResult w = new CSVExperimentResult(output);
		w.add(new TestParameters("row","col",new String[]{"descr"},new String[]{"BCR","Diff","States","PTA states"}),"a,b,c,d");
		TestHelper.checkForCorrectException(() ->
				w.add(new TestParameters("row2","col",new String[]{"descr"},new String[]{"BCR","Diff","States"}),"a,b,c"),
				IllegalArgumentException.class,"different values of cell headers");
	}
	
	/** Duplicate values in cell. */
	@Test
	public void testCSVwriteFileFail3()
	{
		File output = new File(testDir,"out.csv");
		final CSVExperimentResult w = new CSVExperimentResult(output);
		Assert.assertEquals(-1,w.getHeaderRowsNumber());
		w.add(new TestParameters("row","col",new String[]{"descr"},new String[]{"BCR","Diff","States","PTA states"}),"a,b,c,d");
		Assert.assertEquals(1,w.getHeaderRowsNumber());
		w.add(new TestParameters("row","col2",new String[]{"descr","descr2"},new String[]{"BCR","Diff","States","PTA states"}),"a,b,c,d");
		Assert.assertEquals(2,w.getHeaderRowsNumber());
	}
	
	@Test
	public void testCSVwriteFileFail4a()
	{
		File output = new File(testDir,"out.csv");
		final CSVExperimentResult w = new CSVExperimentResult(output);
		TestHelper.checkForCorrectException(() -> w.add(new TestParameters(null,"col",new String[]{"descr"},new String[]{"BCR","Diff","States","PTA states"}),"a,b,c,d"), IllegalArgumentException.class,"cannot add a cell without row id");
		TestHelper.checkForCorrectException(() -> w.add(new TestParameters("","col",new String[]{"descr"},new String[]{"BCR","Diff","States","PTA states"}),"a,b,c,d"), IllegalArgumentException.class,"cannot add a cell without row id");
	}
	
	@Test
	public void testCSVwriteFileFail4b()
	{
		File output = new File(testDir,"out.csv");
		final CSVExperimentResult w = new CSVExperimentResult(output);
		TestHelper.checkForCorrectException(() -> w.add(new TestParameters("row",null,new String[]{"descr"},new String[]{"BCR","Diff","States","PTA states"}),"a,b,c,d"), IllegalArgumentException.class,"cannot add a cell without column id");
		TestHelper.checkForCorrectException(() -> w.add(new TestParameters("row","",new String[]{"descr"},new String[]{"BCR","Diff","States","PTA states"}),"a,b,c,d"), IllegalArgumentException.class,"cannot add a cell without column id");
	}

	@Test
	public void testBagPlotToString1()
	{
		final List<List<Double>> data = new LinkedList<>();
		data.add(Arrays.asList(4.,5.));
		Assert.assertEquals("bagplot(c(7.0,7.0),c(4.0,5.0),someOther attrs)",
				DrawGraphs.datasetToString("bagplot",data, Collections.singletonList(7.),Collections.singletonList("someOther attrs")));
	}

	public static void mkDirRetryOnFail(File dir)
	{
		if (!dir.isDirectory()) 
		{
			if (!dir.mkdir())
			{
				try {
					Thread.sleep(500);
				} catch (InterruptedException e) {
					// can be safely ignored
				}
				Assert.assertTrue("could not create "+dir.getAbsolutePath(),dir.mkdir());
			}
		}
		
	}
	@Before
	public void before()
	{
		mkDirRetryOnFail(tmpDir);
		mkDirRetryOnFail(testDir);
	}

	@After
	public void after()
	{
		ExperimentRunner.zapDir(testDir);
	}
		
	@Test
	public void testRunRealPlot() throws IOException
	{
		final DrawGraphs gr = new DrawGraphs();
		File output = new File(testDir,"out.pdf");
		final RBoxPlot<String> graph = new RBoxPlot<>("X axis", "Y axis", output);
		for(double v:Arrays.asList(4.,5.,5.))
			graph.add("A",v);
		for(double v:Arrays.asList(7.,8.,3.))
			graph.add("B",v);
		graph.reportResults(gr);
//		gr.drawPlot(Collections.singletonList(DrawGraphs.boxPlotToString(data, Arrays.asList("graphA","graphB"),null,null)),7,7,output);
		
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line;
		List<String> stringsOfInterest = Arrays.asList("Title (R Graphics Output)", "X axis","Y axis");
		Map<String,Boolean> encounteredStrings = new TreeMap<>();
		StringBuilder buffer = new StringBuilder();
		try
		{
			while((line=reader.readLine()) != null)
			{
				buffer.append(line);
				for(String str:stringsOfInterest)
					if (line.contains(str)) encounteredStrings.put(str,true);
			}
		}
		finally
		{
			reader.close();
		}

		Assert.assertEquals("only found "+encounteredStrings+"\n"+ buffer,stringsOfInterest.size(),encounteredStrings.size());// ensure that we find all our strings
	}
	
	@Test
	public void testRunRealPlotWithLabelsAndColours() throws IOException
	{
		final DrawGraphs gr = new DrawGraphs();

		final String X="axisX", Y="axisY";
		File output = new File(testDir,"out.pdf");
		RGraph<String> g= new RBoxPlot<>(X, Y, output);
		g.add("one",34.,"cyan","lbl");
		g.add("one",34.);
		g.add("one",2.,"magenta",null);
		g.add("two",3.);
		g.add("three",4.,"blue","");
		g.add("three",5.);
		g.reportResults(gr);

		BufferedReader reader = new BufferedReader(new FileReader(output));
		StringBuilder buffer = new StringBuilder();
		String line;
		List<String> stringsOfInterest = Arrays.asList("Title (R Graphics Output)", X,Y,"(lb","0.000 0.000 1.000 ","1.000 0.000 1.000 ","0.000 1.000 0.000 ","0.000 0.000 0.000 ");
		Map<String,Boolean> encounteredStrings = new TreeMap<>();
		try
		{
			while((line=reader.readLine()) != null)
			{
				buffer.append(line);
				for(String str:stringsOfInterest)
					if (line.contains(str)) encounteredStrings.put(str,true);
			}
		}
		finally
		{
			reader.close();
		}
		Assert.assertEquals("only found "+encounteredStrings+"\n"+ buffer,stringsOfInterest.size(),encounteredStrings.size());// ensure that we find all our strings
	}
	
	@Test
	public void testPlotFail1()
	{
		final DrawGraphs gr = new DrawGraphs();
		final File output = new File(testDir,"out.pdf");
		checkForCorrectException(() -> gr.drawPlot(Collections.singletonList(""),0,1,output),IllegalArgumentException.class,"horizontal");
	}
	
	@Test
	public void testPlotFail2()
	{
		final DrawGraphs gr = new DrawGraphs();
		final File output = new File(testDir,"out.pdf");
		checkForCorrectException(() -> gr.drawPlot(Collections.singletonList(""),1,0,output),IllegalArgumentException.class,"vertical");
	}
	
	@Test
	public void testGenerateGraphFail1a()
	{
		final RGraph<Integer> g= new RBoxPlot<>("axisX", "axisY", new File("someName"));
		checkForCorrectException(g::getDrawingCommand,IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testGenerateGraphFail1b()
	{
		final RGraph<Double> g=new RBagPlot("axisX", "axisY", new File("someName"));
		checkForCorrectException(g::getDrawingCommand,IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testGenerateGraphFail2()
	{
		final DrawGraphs gr = new DrawGraphs();
		final RGraph<String> g= new RBoxPlot<>("axisX", "axisY", new File("someName"));
		checkForCorrectException(() -> g.drawInteractive(gr),IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testGenerateGraph1a1()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g= new RBoxPlot<>(X, Y, new File("someName"));
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.add("one",34.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(5.0,4.0,4.0,2.0)), boxplot(yaxt=\"n\",xaxt=\"n\",c(34.0),col=c(\"green\"),mar=c(5.0,4.0,4.0,2.0)), axis(side=1,mgp=c(3.0,1.0,0.0),las=1), axis(side=2,mgp=c(3.0,1.0,0.0),las=1), title(xlab=\"axisX\"), title(ylab=\"axisY\"), par(mar=curMar)]",g.getDrawingCommand().toString());
	}

    @Test
    public void testGenerateGraph1a2()
    {
        final String X="axisX", Y="axisY";
        RGraph<String> g= new RBoxPlot<>(X, Y, new File("someName"));
        g.setLabelsAuto(XLABELS_R);
        g.add("one",34.);
        Assert.assertEquals("[curMar=par()$mar, par(mar=c(5.0,4.0,4.0,2.0)), boxplot(yaxt=\"n\",c(34.0),col=c(\"green\"),mar=c(5.0,4.0,4.0,2.0)), axis(side=2,mgp=c(3.0,1.0,0.0),las=1), title(xlab=\"axisX\"), title(ylab=\"axisY\"), par(mar=curMar)]",g.getDrawingCommand().toString());
    }

	@Test
	public void testGenerateGraph1a3()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g= new RBoxPlot<>(X, Y, new File("someName"));
		g.setLabelsAuto(XLABELS_R);
		g.add("one",34.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(5.0,4.0,4.0,2.0)), boxplot(yaxt=\"n\",c(34.0),col=c(\"green\"),mar=c(5.0,4.0,4.0,2.0)), axis(side=2,mgp=c(3.0,1.0,0.0),las=1), title(xlab=\"axisX\"), title(ylab=\"axisY\"), par(mar=curMar)]",g.getDrawingCommand().toString());
	}

    @Test
	public void testGenerateGraph1b()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.add(4.5,34.);
		Assert.assertEquals(
				"[curMar=par()$mar, par(mar=c(3.0,3.0,0.2,0.2)), bagplot(c(4.5),c(34.0),xlab=\"\",ylab=\"\",xaxt=\"n\",yaxt=\"n\",mar=c(3.0,3.0,0.2,0.2)), axis(side=1,mgp=c(3.0,0.7,0.0),las=1), axis(side=2,mgp=c(3.0,0.7,0.0),las=1), title(xlab=\"axisX\",line=1.8), title(ylab=\"axisY\",line=2.0), par(mar=curMar)]",
				g.getDrawingCommand().toString());
	}
	
	@Test
	public void testGenerateGraph2()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g= new RBoxPlot<>(X, Y, new File("someName"));
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.add("one",34.);
		g.add("one",34.);
		g.add("one",2.);
		Assert.assertEquals(
				"[curMar=par()$mar, par(mar=c(5.0,4.0,4.0,2.0)), boxplot(yaxt=\"n\",xaxt=\"n\",c(34.0,34.0,2.0),col=c(\"green\"),mar=c(5.0,4.0,4.0,2.0)), axis(side=1,mgp=c(3.0,1.0,0.0),las=1), axis(side=2,mgp=c(3.0,1.0,0.0),las=1), title(xlab=\"axisX\"), title(ylab=\"axisY\"), par(mar=curMar)]",g.getDrawingCommand().toString());
	}


	@Test
	public void testGenerateGraph3()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g= new RBoxPlot<>(X, Y, new File("someName"));
		g.configureTextLabels(-0.15,1,0.5);
		g.setLabelsAuto(XLABELS_TEXT_AUTO);
		g.add("one",34.);
		g.add("one",134.);
		g.add("two",67.);
		g.add("two",87.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(5.0,4.0,4.0,2.0)), boxplot(yaxt=\"n\",xaxt=\"n\",c(34.0,134.0),c(67.0,87.0),names=c(\"one\",\"two\"),col=c(\"green\",\"green\"),mar=c(5.0,4.0,4.0,2.0)), axis(side=1,mgp=c(3.0,1.0,0.0),las=1,at=1:2,labels=FALSE), axis(side=2,mgp=c(3.0,1.0,0.0),las=1), text(x=1:2,y=19.0,labels=c(\"one\",\"two\"),xpd=NA,srt=1.0,adj=0.5), title(xlab=\"axisX\"), title(ylab=\"axisY\"), par(mar=curMar)]",g.getDrawingCommand().toString());
	}


	@Test
	public void testGenerateGraph4()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g= new RBoxPlot<>(X, Y, new File("someName"));
		g.configureTextLabels(-0.15,1,0.5);
		g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.add("one",34.);
		g.add("one",134.);
		g.add("two",67.);
		g.add("two",87.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(5.0,4.0,4.0,2.0)), boxplot(yaxt=\"n\",xaxt=\"n\",c(34.0,134.0),c(67.0,87.0),names=c(\"one\",\"two\"),col=c(\"green\",\"green\"),mar=c(5.0,4.0,4.0,2.0)), axis(side=1,mgp=c(3.0,1.0,0.0),las=1,at=1:2,labels=FALSE), axis(side=2,mgp=c(3.0,1.0,0.0),las=1), text(x=1:2,y=-0.15,labels=c(\"one\",\"two\"),xpd=NA,srt=1.0,adj=0.5), title(xlab=\"axisX\"), title(ylab=\"axisY\"), par(mar=curMar)]",g.getDrawingCommand().toString());
	}


	@Test
	public void testGenerateGraph5()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g= new RBoxPlot<>(X, Y, new File("someName"));
		g.configureTextLabels(-0.15,1,0.5);
		g.setLabelsAuto(XLABELS_R);
		g.add("one",34.);
		g.add("one",134.);
		g.add("two",67.);
		g.add("two",87.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(5.0,4.0,4.0,2.0)), boxplot(yaxt=\"n\",c(34.0,134.0),c(67.0,87.0),names=c(\"one\",\"two\"),col=c(\"green\",\"green\"),mar=c(5.0,4.0,4.0,2.0)), axis(side=2,mgp=c(3.0,1.0,0.0),las=1), title(xlab=\"axisX\"), title(ylab=\"axisY\"), par(mar=curMar)]",g.getDrawingCommand().toString());
	}

	/** Same as above, but with different colours. */
	@Test
	public void testGenerateGraphWithdifferentColours()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g= new RBoxPlot<>(X, Y, new File("someName"));
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.add("one",34.,"cyan",null);
		g.add("one",34.);
		g.add("one",2.,"magenta",null);
		g.add("two",3.);
		g.add("three",4.,"blue",null);
		Assert.assertEquals(
				"[curMar=par()$mar, par(mar=c(5.0,4.0,4.0,2.0)), boxplot(yaxt=\"n\",xaxt=\"n\",c(34.0,34.0,2.0),c(4.0),c(3.0),names=c(\"one\",\"three\",\"two\"),col=c(\"magenta\",\"blue\",\"green\"),mar=c(5.0,4.0,4.0,2.0)), axis(side=1,mgp=c(3.0,1.0,0.0),las=1,at=1:3,labels=FALSE), axis(side=2,mgp=c(3.0,1.0,0.0),las=1), text(x=1:3,y=0.0,labels=c(\"one\",\"three\",\"two\"),xpd=NA,srt=90.0,adj=1.0), title(xlab=\"axisX\"), title(ylab=\"axisY\"), par(mar=curMar)]",
				g.getDrawingCommand().toString());
	}
	
	/** Same as above, but with different colours and labels. */
	@Test
	public void testGenerateGraphWithdifferentColoursAndLabels()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g= new RBoxPlot<>(X, Y, new File("someName"));
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.add("one",34.,"cyan","lbl");
		g.add("one",34.);
		g.add("one",2.,"magenta",null);
		g.add("two",3.);
		g.add("three",4.,"blue","");
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(5.0,4.0,4.0,2.0)), boxplot(yaxt=\"n\",xaxt=\"n\",c(34.0,34.0,2.0),c(4.0),c(3.0),names=c(\"lbl\",\"\",\"two\"),col=c(\"magenta\",\"blue\",\"green\"),mar=c(5.0,4.0,4.0,2.0)), axis(side=1,mgp=c(3.0,1.0,0.0),las=1,at=1:3,labels=FALSE), axis(side=2,mgp=c(3.0,1.0,0.0),las=1), text(x=1:3,y=0.0,labels=c(\"lbl\",\"\",\"two\"),xpd=NA,srt=90.0,adj=1.0), title(xlab=\"axisX\"), title(ylab=\"axisY\"), par(mar=curMar)]",g.getDrawingCommand().toString());
	}

	/** This one is a bagplot. */
	@Test
	public void testGenerateGraph2c()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.add(5.5,34.);
		g.add(5.5,34.);
		g.add(5.5,2.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(3.0,3.0,0.2,0.2)), bagplot(c(5.5,5.5,5.5),c(34.0,34.0,2.0),xlab=\"\",ylab=\"\",xaxt=\"n\",yaxt=\"n\",mar=c(3.0,3.0,0.2,0.2)), axis(side=1,mgp=c(3.0,0.7,0.0),las=1), axis(side=2,mgp=c(3.0,0.7,0.0),las=1), title(xlab=\"axisX\",line=1.8), title(ylab=\"axisY\",line=2.0), par(mar=curMar)]",
				g.getDrawingCommand().toString());
	}
	
	@Test
	public void testGenerateGraph3a()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g= new RBoxPlot<>(X, Y, new File("someName"));
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.add("one",34.);
		g.add("one",34.);
		g.add("one",2.);
		g.add("two",2.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(5.0,4.0,4.0,2.0)), boxplot(yaxt=\"n\",xaxt=\"n\",c(34.0,34.0,2.0),c(2.0),names=c(\"one\",\"two\"),col=c(\"green\",\"green\"),mar=c(5.0,4.0,4.0,2.0)), axis(side=1,mgp=c(3.0,1.0,0.0),las=1,at=1:2,labels=FALSE), axis(side=2,mgp=c(3.0,1.0,0.0),las=1), text(x=1:2,y=0.0,labels=c(\"one\",\"two\"),xpd=NA,srt=90.0,adj=1.0), title(xlab=\"axisX\"), title(ylab=\"axisY\"), par(mar=curMar)]",
				g.getDrawingCommand().toString());
	}
	
	@Test
	public void testGenerateGraph3b()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.add(5.5,34.);
		g.add(5.5,34.);
		g.add(5.5,2.);
		g.add(7.5,2.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(3.0,3.0,0.2,0.2)), bagplot(c(5.5,5.5,5.5,7.5),c(34.0,34.0,2.0,2.0),xlab=\"\",ylab=\"\",xaxt=\"n\",yaxt=\"n\",mar=c(3.0,3.0,0.2,0.2)), axis(side=1,mgp=c(3.0,0.7,0.0),las=1), axis(side=2,mgp=c(3.0,0.7,0.0),las=1), title(xlab=\""+X+"\",line=1.8), title(ylab=\""+Y+"\",line=2.0), par(mar=curMar)]",
				g.getDrawingCommand().toString());
	}
	
	@Test
	public void testAttemptSingleDotBagPlot1()
	{
		final String X="axisX", Y="axisY";
		RBagPlot g=new RBagPlot(X,Y, new File("someName"));
		g.add(5.5,34.);
		g.add(5.5,34.);
		Assert.assertTrue(g.checkSingleDot());
	}
	
	@Test
	public void testAttemptSingleDotBagPlot2()
	{
		final String X="axisX", Y="axisY";
		RBagPlot g=new RBagPlot(X,Y, new File("someName"));
		Assert.assertTrue(g.checkSingleDot());
	}

	@Test
	public void testAttemptSingleDotBagPlot3()
	{
		final String X="axisX", Y="axisY";
		RBagPlot g=new RBagPlot(X,Y, new File("someName"));
		g.add(0.,1.);
		g.add(0.,1.);
		Assert.assertTrue(g.checkSingleDot());
	}
	
	@Test
	public void testAttemptSingleDotBagPlot4()
	{
		final String X="axisX", Y="axisY";
		RBagPlot g=new RBagPlot(X,Y, new File("someName"));
		g.add(0.,1.);
		g.add(0.,2.);
		Assert.assertFalse(g.checkSingleDot());
	}
	
	@Test
	public void testAttemptSingleDotBagPlot5()
	{
		final String X="axisX", Y="axisY";
		RBagPlot g=new RBagPlot(X,Y, new File("someName"));
		g.add(0.,1.);
		g.add(1.,1.);
		Assert.assertFalse(g.checkSingleDot());
	}
	
	@Test
	public void testBoundaries1()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.setXboundaries(5.5, 34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(3.0,3.0,0.2,0.2)), bagplot(c(5.5,5.5,5.5,7.5),c(34.0,34.0,2.0,2.0),xlab=\"\",ylab=\"\",xaxt=\"n\",yaxt=\"n\",mar=c(3.0,3.0,0.2,0.2)), axis(side=1,mgp=c(3.0,0.7,0.0),las=1), axis(side=2,mgp=c(3.0,0.7,0.0),las=1), title(xlab=\"axisX\",line=1.8), title(ylab=\"axisY\",line=2.0), par(mar=curMar)]",
				g.getDrawingCommand().toString());
	}
	
	@Test
	public void testBoundaries2()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.setXboundaries(5.6, 34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(3.0,3.0,0.2,0.2)), bagplot(c(7.5),c(2.0),xlab=\"\",ylab=\"\",xaxt=\"n\",yaxt=\"n\",mar=c(3.0,3.0,0.2,0.2)), axis(side=1,mgp=c(3.0,0.7,0.0),las=1), axis(side=2,mgp=c(3.0,0.7,0.0),las=1), title(xlab=\"axisX\",line=1.8), title(ylab=\"axisY\",line=2.0), par(mar=curMar)]",g.getDrawingCommand().toString());
	}
	
	@Test
	public void testBoundaries3()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.setYboundaries(5.5, 34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(3.0,3.0,0.2,0.2)), bagplot(c(5.5,5.5),c(34.0,34.0),xlab=\"\",ylab=\"\",xaxt=\"n\",yaxt=\"n\",mar=c(3.0,3.0,0.2,0.2)), axis(side=1,mgp=c(3.0,0.7,0.0),las=1), axis(side=2,mgp=c(3.0,0.7,0.0),las=1), title(xlab=\"axisX\",line=1.8), title(ylab=\"axisY\",line=2.0), par(mar=curMar)]",g.getDrawingCommand().toString());
	}
	
	@Test
	public void testBoundaries4()
	{
		final String X="axisX", Y="axisY";
		final RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.setXboundaries(5.5, -34.);
		g.setYboundaries(5.5, -34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		checkForCorrectException(g::getDrawingCommand,IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testComputeDiagonal1()
	{
		final String X="axisX", Y="axisY";
		final RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.setXboundaries(5.5, -34.);
		g.setYboundaries(5.5, -34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		checkForCorrectException(g::getDrawingCommand,IllegalArgumentException.class,"empty");
		
	}
	
	@Test
	public void testComputeDiagonal2()
	{
		final String X="axisX", Y="axisY";
		final RBagPlot g=new RBagPlot(X,Y, new File("someName"));
		g.add(5.5,34.);g.add(5.5,35.);
		checkForCorrectException(g::computeDiagonal,IllegalArgumentException.class,"width is too small");
		
	}	
	
	@Test
	public void testComputeDiagonal3()
	{
		final String X="axisX", Y="axisY";
		final RBagPlot g=new RBagPlot(X,Y, new File("someName"));
		g.add(5.6,35.);g.add(5.5,35.);
		checkForCorrectException(g::computeDiagonal,IllegalArgumentException.class,"height is too small");
		
	}
	
	@Test
	public void testComputeDiagonal4()
	{
		final String X="axisX", Y="axisY";
		final RBagPlot g=new RBagPlot(X,Y, new File("someName"));
		g.add(5.5,34.);g.add(5.7,32.);g.add(7.8,31.);
		Assert.assertEquals("abline(23.82608695652174,1.3043478260869565)",g.computeDiagonal());
	}
	
	@Test
	public void testDrawBagPlotWithDiagonal1()
	{
		final String X="axisX", Y="axisY";
		final SquareBagPlot g=new SquareBagPlot(X,Y, new File("someName"),2,40,true);
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.add(5.5,34.);g.add(5.7,32.);g.add(7.8,31.);
		Assert.assertEquals(
				"[curMar=par()$mar, par(mar=c(3.0,3.0,0.2,0.2)), bplot<-compute.bagplot(c(5.5,5.7,7.8),c(34.0,32.0,31.0)), plot(bplot,xlim=c(2.0,40.0), ylim=c(2.0,40.0), xlab=\"\",ylab=\"\",xaxt=\"n\",yaxt=\"n\"), abline(0,1), axis(side=1,mgp=c(3.0,0.7,0.0),las=1), axis(side=2,mgp=c(3.0,0.7,0.0),las=1), title(xlab=\""+X+"\",line=1.8), title(ylab=\""+Y+"\",line=2.0), par(mar=curMar)]",
				g.getDrawingCommand().toString());
	}
	
	@Test
	public void testDrawBagPlotWithDiagonal2()
	{
		final String X="axisX", Y="axisY";
		final SquareBagPlot g=new SquareBagPlot(X,Y, new File("someName"),2,40,false);
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.add(5.5,34.);g.add(5.7,32.);g.add(7.8,31.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(3.0,3.0,0.2,0.2)), bplot<-compute.bagplot(c(5.5,5.7,7.8),c(34.0,32.0,31.0)), plot(bplot,xlim=c(2.0,40.0), ylim=c(2.0,40.0), xlab=\"\",ylab=\"\",xaxt=\"n\",yaxt=\"n\"), axis(side=1,mgp=c(3.0,0.7,0.0),las=1), axis(side=2,mgp=c(3.0,0.7,0.0),las=1), title(xlab=\""+X+"\",line=1.8), title(ylab=\""+Y+"\",line=2.0), par(mar=curMar)]",
				g.getDrawingCommand().toString());
	}
	
	@Test
	public void testDrawBagPlotWithDiagonal3()
	{
		final String X="axisX", Y="axisY";
		final SquareBagPlot g=new SquareBagPlot(X,Y, new File("someName"),2,40,true);
        g.setLabelsAuto(XLABELS_TEXT_MANUAL);
		g.setLimit(30000);
		g.add(5.5,34.);g.add(5.7,32.);g.add(7.8,31.);
		Assert.assertEquals("[curMar=par()$mar, par(mar=c(3.0,3.0,0.2,0.2)), bplot<-compute.bagplot(c(5.5,5.7,7.8),c(34.0,32.0,31.0),approx.limit=30000), plot(bplot,xlim=c(2.0,40.0), ylim=c(2.0,40.0), xlab=\"\",ylab=\"\",xaxt=\"n\",yaxt=\"n\"), abline(0,1), axis(side=1,mgp=c(3.0,0.7,0.0),las=1), axis(side=2,mgp=c(3.0,0.7,0.0),las=1), title(xlab=\""+X+"\",line=1.8), title(ylab=\""+Y+"\",line=2.0), par(mar=curMar)]",
				g.getDrawingCommand().toString());
	}


	public static String arrayToString(List<String> a)
	{
		StringBuilder outcome = new StringBuilder();
		boolean first = true;
		for(String s:a)
		{
			if (first)
				first = false;
			else
				outcome.append(',');
			
			outcome.append(s);
		}
		return outcome.toString();
	}
	
	@Test
	public void testDrawingScatterPlot1()
	{
		final ScatterPlot plot = new ScatterPlot("x axis", "y axis", new File("plotName"));

		plot.add(0, 0, "red");plot.add(1, 0, "red");plot.add(0, 1, "red");plot.add(1, 1, "red");
		Assert.assertEquals(arrayToString(Collections.singletonList("plot(c(0.0,1.0,0.0,1.0),c(0.0,0.0,1.0,1.0),type = \"p\",col=\"red\",xlab=\"x axis\",ylab=\"y axis\",axes=FALSE, frame.plot=TRUE)")),arrayToString(plot.getDrawingCommand()));
	}
	
	@Test
	public void testDrawingScatterPlot2()
	{
		final ScatterPlot plot = new ScatterPlot("x axis", "y axis", new File("plotName"));
		
		plot.add(0, 0, "red");plot.add(1, 0, "red");plot.add(0, 1, "red");plot.add(1, 1, "red");
		plot.add(0, 0.5, "blue");plot.add(1.5, 0, "green");plot.add(0, 1.5, "blue");plot.add(1.2, 1, "red");
		Assert.assertEquals(arrayToString(Arrays.asList("plot(c(0.0,0.0),c(0.5,1.5),type = \"p\",col=\"blue\",xlab=\"x axis\",ylab=\"y axis\",axes=FALSE, frame.plot=TRUE)",
                "par(new=TRUE)",
                "plot(c(1.5),c(0.0),type = \"p\",col=\"green\",xlab=\"x axis\",ylab=\"y axis\",axes=FALSE, frame.plot=TRUE)",
                "par(new=TRUE)",
                "plot(c(0.0,1.0,0.0,1.0,1.2),c(0.0,0.0,1.0,1.0,1.0),type = \"p\",col=\"red\",xlab=\"x axis\",ylab=\"y axis\",axes=FALSE, frame.plot=TRUE)")),arrayToString(plot.getDrawingCommand()));
	}
	
	@Test
	public void testDrawingScatterFail()
	{
		final ScatterPlot plot = new ScatterPlot("x axis", "y axis", new File("plotName"));
		checkForCorrectException(plot::getDrawingCommand,IllegalArgumentException.class,"empty");
	}

	@Test
	public void testRunDrawingScatterPlot() throws IOException
	{
		final DrawGraphs gr = new DrawGraphs();

		final String X="axisX", Y="axisY";
		File output = new File(testDir,"out.pdf");
		final ScatterPlot plot = new ScatterPlot(X, Y, output);
		
		plot.add(0, 0, "red");plot.add(1, 0, "red");plot.add(0, 1, "red");plot.add(1, 1, "red");plot.add(0, 0.5, "blue");plot.add(1.5, 0, "green");plot.add(0, 1.5, "blue");plot.add(1.2, 1, "red");
		plot.reportResults(gr);
		
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line;
		List<String> stringsOfInterest = Arrays.asList("Title (R Graphics Output)", X,Y);
		Map<String,Boolean> encounteredStrings = new TreeMap<>();
		StringBuilder buffer = new StringBuilder();
		try
		{
			while((line=reader.readLine()) != null)
			{
				buffer.append(line);
				for(String str:stringsOfInterest)
					if (line.contains(str)) encounteredStrings.put(str,true);
			}
		}
		finally
		{
			reader.close();
		}

		Assert.assertEquals("only found "+encounteredStrings+"\n"+ buffer,stringsOfInterest.size(),encounteredStrings.size());// ensure that we find all our strings
	}


	@Test
	public final void testBuildStringMapFromPairs1()
	{
		assertTrue(buildStringMapFromStringPairs(new String[][]{}).isEmpty());
	}
	
	@Test
	public final void testBuildStringMapFromPairs2()
	{
		Map<String,String> expectedResult = new HashMap<>();
		expectedResult.put("a","value2");expectedResult.put("b","value3");

        assertEquals(expectedResult, buildStringMapFromStringPairs(new String[][]{
                new String[]{"a", "value2"},
                new String[]{"b", "value3"}
        }));
	}
	
	@Test
	public final void testBuildStringMapFromPairs3()
	{
		Map<String,String> expectedResult = new HashMap<>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");

        assertEquals(expectedResult, buildStringMapFromStringPairs(new String[][]{
                new String[]{"strC", "value2"},
                new String[]{"a", "value1"},
                new String[]{"b", "value3"}
        }));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMapFromPairs4()
	{
		Map<String,String> expectedResult = new HashMap<>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");

        assertEquals(expectedResult, buildStringMapFromStringPairs(new String[][]{
                new String[]{"strC", "value1"},
                new String[]{"a"},// an invalid sequence
                new String[]{"b", "value3"}
        }));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMapFromPairs5()
	{
		Map<String,String> expectedResult = new HashMap<>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");

        assertEquals(expectedResult, buildStringMapFromStringPairs(new String[][]{
                new String[]{"strC", "value1"},
                new String[]{},// an invalid sequence - too few elements
                new String[]{"b", "value3"}
        }));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMapFromPairs6()
	{
		Map<String,String> expectedResult = new HashMap<>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");

        assertEquals(expectedResult, buildStringMapFromStringPairs(new String[][]{
                new String[]{"strC", "value1"},
                new String[]{"a", "c", "d"},// an invalid sequence - too many elements
                new String[]{"b", "value3"}
        }));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMapFromPairs7()
	{
		Map<String,String> expectedResult = new HashMap<>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");

        assertEquals(expectedResult, buildStringMapFromStringPairs(new String[][]{
                new String[]{"strC", "value1"},
                new String[]{null, "value"},// an invalid sequence - null in the first element
                new String[]{"b", "value3"}
        }));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMapFromPairs8()
	{
		Map<String,String> expectedResult = new HashMap<>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");

        assertEquals(expectedResult, buildStringMapFromStringPairs(new String[][]{
                new String[]{"strC", "value1"},
                new String[]{"a", null},// an invalid sequence - null in the second element
                new String[]{"b", null}
        }));
	}


	@Test
	public final void testFormatTex1() {
		List<List<String>> data = new ArrayList<>();
		data.add(Arrays.asList("headerA","headerB","headerC"));
		data.add(Arrays.asList("dataA","dataB","dataC"));
		data.add(Arrays.asList("dataD","dataE","dataF"));
		String got = DrawGraphs.formatTEX(data,true);
		Assert.assertEquals("\\begin{tabular}{| l | c | c |}\\\\hline\n" +
				"headerA & headerB & headerC\\\\\\hline\\hline\n" +
				"dataA & dataB & dataC\\\\\n" +
				"dataD & dataE & dataF\\\\\\hline\n" +
				"\\end{tabular}\n",got);
	}

	@Test
	public final void testFormatTex2() {
		List<List<String>> data = new ArrayList<>();
		data.add(Arrays.asList("headerA","headerB","headerC"));
		data.add(Arrays.asList("dataA","dataB","dataC"));
		data.add(Arrays.asList("dataD","dataE","dataF"));
		String got = DrawGraphs.formatTEX(data,false);
		Assert.assertEquals("\\begin{tabular}{| c | c | c |}\\\\hline\n" +
				"headerA & headerB & headerC\\\\\\hline\\hline\n" +
				"dataA & dataB & dataC\\\\\n" +
				"dataD & dataE & dataF\\\\\\hline\n" +
				"\\end{tabular}\n",got);
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testFormatTex_err1()
	{
		List<List<String>> data = new ArrayList<>();
		data.add(Arrays.asList("headerA","headerB","headerC"));
		data.add(Arrays.asList("dataA","dataB"));
		data.add(Arrays.asList("dataD","dataE","dataF"));
		DrawGraphs.formatTEX(data,false);
	}
	@Test(expected = IllegalArgumentException.class)
	public final void testFormatTex_err2()
	{
		List<List<String>> data = new ArrayList<>();
		DrawGraphs.formatTEX(data,false);
	}
	@Test(expected = IllegalArgumentException.class)
	public final void testFormatTex_err3()
	{
		List<List<String>> data = new ArrayList<>();
		data.add(Arrays.asList("headerA","headerB","headerC"));
		DrawGraphs.formatTEX(data,false);
	}
}
