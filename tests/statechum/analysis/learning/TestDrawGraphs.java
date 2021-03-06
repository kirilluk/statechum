package statechum.analysis.learning;

import static statechum.Helper.checkForCorrectException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.DrawGraphs.RGraph;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.RBagPlot;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.DrawGraphs.StatisticalTestResult;
import statechum.analysis.learning.experiments.ExperimentRunner;

public class TestDrawGraphs {

	@Test
	public void testVectorToRFail()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			DrawGraphs.vectorToR(new LinkedList<String>(),false);
		}},IllegalArgumentException.class,"empty");
	}

	@Test
	public void testVectorToR1()
	{
		Assert.assertEquals("c(1.0)", DrawGraphs.vectorToR(Arrays.asList(new Double[]{1.0}),false));
	}
	
	@Test
	public void testVectorToR2()
	{
		Assert.assertEquals("c(\"1.0\")", DrawGraphs.vectorToR(Arrays.asList(new Double[]{1.0}),true));
	}

	@Test
	public void testVectorToR3()
	{
		Assert.assertEquals("c(\"1.0\",\"6.0\")", DrawGraphs.vectorToR(Arrays.asList(new Double[]{1.0,6.0}),true));
	}

	@Test
	public void testVectorToR4()
	{
		Assert.assertEquals("c(\"nameA\",\"nameB\",\"nameC\")", DrawGraphs.vectorToR(Arrays.asList(new String[]{"nameA","nameB","nameC"}),true));
	}

	@Test
	public void testBoxPlotToStringFail1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			DrawGraphs.boxPlotToString(new LinkedList<List<Double>>(), new LinkedList<String>(),new LinkedList<String>(),null);
		}},IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testBoxPlotToStringFail2()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			DrawGraphs.boxPlotToString(data, new LinkedList<String>(),new LinkedList<String>(),null);
		}},IllegalArgumentException.class,"mismatch");
	}

	@Test
	public void testBoxPlotToStringFail3()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			DrawGraphs.boxPlotToString(data, new LinkedList<String>(),new LinkedList<String>(),null);
		}},IllegalArgumentException.class,"not used");
	}

	@Test
	public void testBoxPlotToString1a()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		String colour = DrawGraphs.defaultColour;
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),c(7.0,8.0,3.0),names=c(\"graphA\",\"graphB\"),col=c(\""+colour+"\",\""+colour+"\"))",
				DrawGraphs.boxPlotToString(data, Arrays.asList(new String[]{"graphA","graphB"}),Arrays.asList(new String[]{colour,colour}),null));
	}
	
	/** Same colours. */
	@Test
	public void testBoxPlotToString1b()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		String colour = DrawGraphs.defaultColour;
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),c(7.0,8.0,3.0),names=c(\"graphA\",\"graphB\"),col=c(\""+colour+"\",\""+colour+"\"),someOther attrs)",
				DrawGraphs.boxPlotToString(data, Arrays.asList(new String[]{"graphA","graphB"}),Arrays.asList(new String[]{colour,colour}),"someOther attrs"));
	}
	
	/** Same as above but different colours. */
	@Test
	public void testBoxPlotToString1c()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),c(7.0,8.0,3.0),names=c(\"graphA\",\"graphB\"),col=c(\""+"red"+"\",\""+"blue"+"\"),someOther attrs)",
				DrawGraphs.boxPlotToString(data, Arrays.asList(new String[]{"graphA","graphB"}),Arrays.asList(new String[]{"red","blue"}),"someOther attrs"));
	}
	
	/** As above but without labels. */
	@Test
	public void testBoxPlotToString2()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),c(7.0,8.0,3.0),col=c(\""+DrawGraphs.defaultColour+"\",\""+DrawGraphs.defaultColour+"\"))",
				DrawGraphs.boxPlotToString(data, null,null,null));
	}
	
	/** As above but one vector and without labels. */
	@Test
	public void testBoxPlotToString3a()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),col=c(\""+DrawGraphs.defaultColour+"\"))",
				DrawGraphs.boxPlotToString(data, null,null,null));
	}
	/** As above but one vector and without labels. */
	@Test
	public void testBoxPlotToString3b()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),col=c(\""+DrawGraphs.defaultColour+"\"),other attrs)",
				DrawGraphs.boxPlotToString(data, null,null,"other attrs"));
	}
	
	public static final File tmpDir = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP));
	public static final File testDir = new File(tmpDir,"__TestDrawGraphs__");

	@Test
	public void testBagPlotToStringFail1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			DrawGraphs.datasetToString("bagplot",new LinkedList<List<Double>>(), new LinkedList<Double>(),null);
		}},IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testBagPlotToStringFail2()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			DrawGraphs.datasetToString("bagplot",data, Arrays.asList(new Double[]{6.7}),null);
		}},IllegalArgumentException.class,"mismatch");
	}

	@Test
	public void testBagPlotToString1a()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		Assert.assertEquals("bagplot(c(7.0,7.0,8.3,8.3,8.3),c(4.0,5.0,7.0,8.0,3.0))",
				DrawGraphs.datasetToString("bagplot",data, Arrays.asList(new Double[]{7.,8.3}),null));
	}

	@Test
	public void testBagPlotToString1b()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		Assert.assertEquals("bagplot(c(7.0,7.0,8.3,8.3,8.3),c(4.0,5.0,7.0,8.0,3.0),someOther attrs)",
				DrawGraphs.datasetToString("bagplot",data, Arrays.asList(new Double[]{7.,8.3}),"someOther attrs"));
	}

	
	@Test
	public void testWilcoxonTestToStringFail1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			new DrawGraphs.Wilcoxon(new File("test")).getDrawingCommand();
		}},IllegalArgumentException.class,"empty");
	}
	
	// here we only testing for lists valuesA and valuesB being empty because they cannot be null by construction due to being final and a non-null initialisation. 
	
	@Test
	public void testWilcoxonTestToStringFail2()
	{
		final DrawGraphs.Wilcoxon w = new DrawGraphs.Wilcoxon(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 6.);
		w.valuesB.clear();
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			w.getDrawingCommand();
		}},IllegalArgumentException.class,"empty");
	}

	@Test
	public void testWilcoxonTestToStringFail3()
	{
		final DrawGraphs.Wilcoxon w = new DrawGraphs.Wilcoxon(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 6.);
		w.valuesA.clear();
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			w.getDrawingCommand();
		}},IllegalArgumentException.class,"empty");
	}

	@Test
	public void testWilcoxonTestToStringFail4()
	{
		final DrawGraphs.Wilcoxon w = new DrawGraphs.Wilcoxon(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 6.);w.valuesB.remove(2);
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			w.getDrawingCommand();
		}},IllegalArgumentException.class," 'x' and 'y' must have the same length");
	}
	
	@Test
	public void testWilcoxonTestToString()
	{
		final DrawGraphs.Wilcoxon w = new DrawGraphs.Wilcoxon(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 3.);
		Assert.assertEquals("[m=wilcox.test(c(4.0,5.0,5.0),c(7.0,8.0,3.0),paired=TRUE)]",
				w.getDrawingCommand().toString());
	}
	
	@Test
	public void testWilcoxonTest() throws IOException
	{
		final DrawGraphs.Wilcoxon w = new DrawGraphs.Wilcoxon(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 3.);

		StringWriter s=new StringWriter();
		StatisticalTestResult result = w.obtainResultFromR();w.writetofile(result,s);
		Assert.assertEquals("Method,Statistic,P-value\nWilcoxon signed rank test,1.0,0.41421617824252505\n",s.toString());
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
		final DrawGraphs.Mann_Whitney_U_Test w = new DrawGraphs.Mann_Whitney_U_Test(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 3.);

		StringWriter s=new StringWriter();
		StatisticalTestResult result = w.obtainResultFromR();w.writetofile(result,s);
		Assert.assertEquals("Method,Statistic,P-value\nWilcoxon rank sum test,3.0,0.6579050194284821\n",s.toString());
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
		final DrawGraphs.Kruskal_Wallis w = new DrawGraphs.Kruskal_Wallis(new File("test"));
		w.add(4., 7.);w.add(5., 8.);w.add(5., 3.);
		StringWriter s=new StringWriter();
		
		StatisticalTestResult result = w.obtainResultFromR();w.writetofile(result,s);
		Assert.assertEquals(2.0,result.statistic,Configuration.fpAccuracy);// values obtained by recording the results rather than attempting to determine what should be returned.
		Assert.assertEquals(0.36787944117144233,result.pvalue,Configuration.fpAccuracy);Assert.assertEquals(2.0,result.parameter,Configuration.fpAccuracy);
		Assert.assertNull(result.alternative);
		
		Assert.assertEquals("Method,Statistic,P-value,parameter\nKruskal-Wallis rank sum test,2.0,0.36787944117144233,2.0\n",s.toString());
		//System.out.println(result.statistic+" "+result.pvalue+" "+result.alternative+" "+result.parameter+" ");
	}

	@Test
	public void testCSVwriteFile1() throws IOException
	{
		File output = new File(testDir,"out.csv");
		CSVExperimentResult w = new CSVExperimentResult(output);
		w.add("line A");w.add("line B");w.reportResults(null);
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line = null;
		StringBuffer buffer = new StringBuffer();
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
		Assert.assertEquals("[line A][line B]", buffer.toString());
	}
	
	@Test
	public void testCSVwriteFile2() throws IOException
	{
		File output = new File(testDir,"out.csv");
		CSVExperimentResult w = new CSVExperimentResult(output);w.reportResults(null);
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line = null;
		StringBuffer buffer = new StringBuffer();
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
	public void testCSVwriteFile3() throws IOException
	{
		File output = new File(testDir,"out.csv");
		CSVExperimentResult w = new CSVExperimentResult(output);
		w.appendToHeader(new String[]{"posNeg","reference"},new String[]{"BCR","Diff","States","PTA states"});
		w.add("line A");w.add("line B");w.reportResults(null);
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line = null;
		StringBuffer buffer = new StringBuffer();
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
		Assert.assertEquals("[posNeg,posNeg,posNeg,posNeg][reference,reference,reference,reference][BCR,Diff,States,PTA states][line A][line B]", buffer.toString());
	}
	
	@Test
	public void testCSVwriteFile4() throws IOException
	{
		File output = new File(testDir,"out.csv");
		CSVExperimentResult w = new CSVExperimentResult(output);
		w.appendToHeader(new String[]{"posNeg","reference"},new String[]{"BCR","Diff","States","PTA states"});
		w.appendToHeader(new String[]{"positive","reference2"},new String[]{"BCR","A","B"});
		w.add("line A");w.add("line B");w.reportResults(null);
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line = null;
		StringBuffer buffer = new StringBuffer();
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
		Assert.assertEquals("[posNeg,posNeg,posNeg,posNeg,positive,positive,positive][reference,reference,reference,reference,reference2,reference2,reference2][BCR,Diff,States,PTA states,BCR,A,B][line A][line B]", buffer.toString());
	}
	
	@Test
	public void testCSVwriteFileFail1()
	{
		File output = new File(testDir,"out.csv");
		final CSVExperimentResult w = new CSVExperimentResult(output);
		Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
			w.appendToHeader(new String[]{},new String[]{"BCR","Diff","States","PTA states"});
		}}, IllegalArgumentException.class,"cannot handle zero");
	}
	
	@Test
	public void testCSVwriteFileFail2()
	{
		File output = new File(testDir,"out.csv");
		final CSVExperimentResult w = new CSVExperimentResult(output);
		w.appendToHeader(new String[]{"posNeg","reference"},new String[]{"BCR","Diff","States","PTA states"});
		Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
			w.appendToHeader(new String[]{"positive"},new String[]{"BCR","Diff","States","PTA states"});
		}}, IllegalArgumentException.class,"cannot append 1");
	}
	
	@Test
	public void testBagPlotToString1()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.}));
		Assert.assertEquals("bagplot(c(7.0,7.0),c(4.0,5.0),someOther attrs)",
				DrawGraphs.datasetToString("bagplot",data, Arrays.asList(new Double[]{7.}),"someOther attrs"));
	}

	@Before
	public void before()
	{
		if (!tmpDir.isDirectory()) 
			Assert.assertTrue("could not create "+tmpDir.getAbsolutePath(),tmpDir.mkdir());
		if (!testDir.isDirectory()) 
			Assert.assertTrue("could not create "+testDir.getAbsolutePath(),testDir.mkdir());
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
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		File output = new File(testDir,"out.pdf");
		gr.drawPlot(Collections.singletonList(DrawGraphs.boxPlotToString(data, Arrays.asList(new String[]{"graphA","graphB"}),null,null)),7,7,output);
		
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line = null;
		List<String> stringsOfInterest = Arrays.asList(new String[]{"Title (R Graphics Output)", "aphA","aphB"});
		Map<String,Boolean> encounteredStrings = new TreeMap<String,Boolean>();
		StringBuffer buffer = new StringBuffer();
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

		Assert.assertEquals("only found "+encounteredStrings+"\n"+buffer.toString(),stringsOfInterest.size(),encounteredStrings.size());// ensure that we find all our strings
	}
	
	@Test
	public void testRunRealPlotWithLabelsAndColours() throws IOException
	{
		final DrawGraphs gr = new DrawGraphs();

		final String X="axisX", Y="axisY";
		File output = new File(testDir,"out.pdf");
		RGraph<String> g=new RBoxPlot<String>(X,Y, output);
		g.add("one",34.,"cyan","lbl");
		g.add("one",34.);
		g.add("one",2.,"magenta",null);
		g.add("two",3.);
		g.add("three",4.,"blue","");
		g.add("three",5.);
		g.reportResults(gr);

		BufferedReader reader = new BufferedReader(new FileReader(output));
		StringBuffer buffer = new StringBuffer();
		String line = null;
		List<String> stringsOfInterest = Arrays.asList(new String[]{"Title (R Graphics Output)", X,Y,"(lb","0.000 0.000 1.000 ","1.000 0.000 1.000 ","0.000 1.000 0.000 ","0.000 0.000 0.000 "});
		Map<String,Boolean> encounteredStrings = new TreeMap<String,Boolean>();
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
		Assert.assertEquals("only found "+encounteredStrings+"\n"+buffer.toString(),stringsOfInterest.size(),encounteredStrings.size());// ensure that we find all our strings	
	}
	
	@Test
	public void testPlotFail1()
	{
		final DrawGraphs gr = new DrawGraphs();
		final File output = new File(testDir,"out.pdf");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			gr.drawPlot(Collections.singletonList(""),0,1,output);
		}},IllegalArgumentException.class,"horizontal");
	}
	
	@Test
	public void testPlotFail2()
	{
		final DrawGraphs gr = new DrawGraphs();
		final File output = new File(testDir,"out.pdf");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			gr.drawPlot(Collections.singletonList(""),1,0,output);
		}},IllegalArgumentException.class,"vertical");
	}
	
	@Test
	public void testGenerateGraphFail1a()
	{
		final RGraph<Integer> g=new RBoxPlot<Integer>("axisX", "axisY", new File("someName"));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			g.getDrawingCommand();
		}},IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testGenerateGraphFail1b()
	{
		final RGraph<Double> g=new RBagPlot("axisX", "axisY", new File("someName"));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			g.getDrawingCommand();
		}},IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testGenerateGraphFail2()
	{
		final DrawGraphs gr = new DrawGraphs();
		final RGraph<String> g=new RBoxPlot<String>("axisX", "axisY", new File("someName"));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			g.drawInteractive(gr);
		}},IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testGenerateGraph1a()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g=new RBoxPlot<String>(X,Y, new File("someName"));
		g.add("one",34.);
		Assert.assertEquals(Collections.singletonList("boxplot(c(34.0),col=c(\""+DrawGraphs.defaultColour+"\"),xlab=\""+X+"\",ylab=\""+Y+"\")"),g.getDrawingCommand());
	}
	
	@Test
	public void testGenerateGraph1b()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.add(4.5,34.);
		Assert.assertEquals(Collections.singletonList("bagplot(c(4.5),c(34.0),xlab=\""+X+"\",ylab=\""+Y+"\")"),g.getDrawingCommand());
	}
	
	@Test
	public void testGenerateGraph2a()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g=new RBoxPlot<String>(X,Y, new File("someName"));
		g.add("one",34.);
		g.add("one",34.);
		g.add("one",2.);
		Assert.assertEquals(Collections.singletonList("boxplot(c(34.0,34.0,2.0),col=c(\""+DrawGraphs.defaultColour+"\"),xlab=\""+X+"\",ylab=\""+Y+"\")"),g.getDrawingCommand());
	}
	
	/** Same as above, but with different colours. */
	@Test
	public void testGenerateGraphWithdifferentColours()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g=new RBoxPlot<String>(X,Y, new File("someName"));
		g.add("one",34.,"cyan",null);
		g.add("one",34.);
		g.add("one",2.,"magenta",null);
		g.add("two",3.);
		g.add("three",4.,"blue",null);
		Assert.assertEquals(Collections.singletonList("boxplot(c(34.0,34.0,2.0),c(4.0),c(3.0),names=c(\"one\",\"three\",\"two\"),col=c(\"magenta\",\"blue\",\""+DrawGraphs.defaultColour+"\"),xlab=\""+X+"\",ylab=\""+Y+"\")"),g.getDrawingCommand());
	}
	
	/** Same as above, but with different colours and labels. */
	@Test
	public void testGenerateGraphWithdifferentColoursAndLabels()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g=new RBoxPlot<String>(X,Y, new File("someName"));
		g.add("one",34.,"cyan","lbl");
		g.add("one",34.);
		g.add("one",2.,"magenta",null);
		g.add("two",3.);
		g.add("three",4.,"blue","");
		Assert.assertEquals(Collections.singletonList("boxplot(c(34.0,34.0,2.0),c(4.0),c(3.0),names=c(\"lbl\",\"\",\"two\"),col=c(\"magenta\",\"blue\",\""+DrawGraphs.defaultColour+"\"),xlab=\""+X+"\",ylab=\""+Y+"\")"),g.getDrawingCommand());
	}

	/** This one is a bagplot. */
	@Test
	public void testGenerateGraph2c()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.add(5.5,34.);
		g.add(5.5,34.);
		g.add(5.5,2.);
		Assert.assertEquals(Collections.singletonList("bagplot(c(5.5,5.5,5.5),c(34.0,34.0,2.0),xlab=\""+X+"\",ylab=\""+Y+"\")"),g.getDrawingCommand());
	}
	
	@Test
	public void testGenerateGraph3a()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g=new RBoxPlot<String>(X,Y, new File("someName"));
		g.add("one",34.);
		g.add("one",34.);
		g.add("one",2.);
		g.add("two",2.);
		Assert.assertEquals(Collections.singletonList("boxplot(c(34.0,34.0,2.0),c(2.0),names=c(\"one\",\"two\"),col=c(\""+DrawGraphs.defaultColour+"\",\""+DrawGraphs.defaultColour+"\"),xlab=\""+X+"\",ylab=\""+Y+"\")"),g.getDrawingCommand());
	}
	
	@Test
	public void testGenerateGraph3b()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.add(5.5,34.);
		g.add(5.5,34.);
		g.add(5.5,2.);
		g.add(7.5,2.);
		Assert.assertEquals(Collections.singletonList("bagplot(c(5.5,5.5,5.5,7.5),c(34.0,34.0,2.0,2.0),xlab=\""+X+"\",ylab=\""+Y+"\")"),g.getDrawingCommand());
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
		g.setXboundaries(5.5, 34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		Assert.assertEquals(Collections.singletonList("bagplot(c(5.5,5.5,5.5,7.5),c(34.0,34.0,2.0,2.0),xlab=\""+X+"\",ylab=\""+Y+"\")"),g.getDrawingCommand());
	}
	
	@Test
	public void testBoundaries2()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.setXboundaries(5.6, 34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		Assert.assertEquals(Collections.singletonList("bagplot(c(7.5),c(2.0),xlab=\""+X+"\",ylab=\""+Y+"\")"),g.getDrawingCommand());
	}
	
	@Test
	public void testBoundaries3()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.setYboundaries(5.5, 34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		Assert.assertEquals(Collections.singletonList("bagplot(c(5.5,5.5),c(34.0,34.0),xlab=\""+X+"\",ylab=\""+Y+"\")"),g.getDrawingCommand());
	}
	
	@Test
	public void testBoundaries4()
	{
		final String X="axisX", Y="axisY";
		final RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.setXboundaries(5.5, -34.);
		g.setYboundaries(5.5, -34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			g.getDrawingCommand();
		}},IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testComputeDiagonal1()
	{
		final String X="axisX", Y="axisY";
		final RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.setXboundaries(5.5, -34.);
		g.setYboundaries(5.5, -34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			g.getDrawingCommand();
		}},IllegalArgumentException.class,"empty");
		
	}
	
	@Test
	public void testComputeDiagonal2()
	{
		final String X="axisX", Y="axisY";
		final RBagPlot g=new RBagPlot(X,Y, new File("someName"));
		g.add(5.5,34.);g.add(5.5,35.);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			g.computeDiagonal();
		}},IllegalArgumentException.class,"width is too small");
		
	}	
	
	@Test
	public void testComputeDiagonal3()
	{
		final String X="axisX", Y="axisY";
		final RBagPlot g=new RBagPlot(X,Y, new File("someName"));
		g.add(5.6,35.);g.add(5.5,35.);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			g.computeDiagonal();
		}},IllegalArgumentException.class,"height is too small");
		
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
		g.add(5.5,34.);g.add(5.7,32.);g.add(7.8,31.);
		Assert.assertEquals(Arrays.asList(new String[]{"bplot<-compute.bagplot(c(5.5,5.7,7.8),c(34.0,32.0,31.0))","plot(bplot,xlim=c(2.0,40.0), ylim=c(2.0,40.0),xlab=\"axisX\",ylab=\"axisY\")", "abline(0,1)"}),
				g.getDrawingCommand());
	}
	
	@Test
	public void testDrawBagPlotWithDiagonal2()
	{
		final String X="axisX", Y="axisY";
		final SquareBagPlot g=new SquareBagPlot(X,Y, new File("someName"),2,40,false);
		g.add(5.5,34.);g.add(5.7,32.);g.add(7.8,31.);
		Assert.assertEquals(Arrays.asList(new String[]{"bplot<-compute.bagplot(c(5.5,5.7,7.8),c(34.0,32.0,31.0))","plot(bplot,xlim=c(2.0,40.0), ylim=c(2.0,40.0),xlab=\"axisX\",ylab=\"axisY\")"}),
				g.getDrawingCommand());
	}
	
	@Test
	public void testDrawBagPlotWithDiagonal3()
	{
		final String X="axisX", Y="axisY";
		final SquareBagPlot g=new SquareBagPlot(X,Y, new File("someName"),2,40,true);
		g.setLimit(30000);
		g.add(5.5,34.);g.add(5.7,32.);g.add(7.8,31.);
		Assert.assertEquals(Arrays.asList(new String[]{"bplot<-compute.bagplot(c(5.5,5.7,7.8),c(34.0,32.0,31.0),approx.limit=30000)","plot(bplot,xlim=c(2.0,40.0), ylim=c(2.0,40.0),xlab=\"axisX\",ylab=\"axisY\")", "abline(0,1)"}),
				g.getDrawingCommand());
	}

	
}
