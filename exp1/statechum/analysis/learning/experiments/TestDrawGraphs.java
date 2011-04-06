package statechum.analysis.learning.experiments;

import static statechum.Helper.checkForCorrectException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Helper.whatToRun;
import statechum.analysis.learning.experiments.GraphMutator.FailureToMutateException;

public class TestDrawGraphs {

	@Test
	public void testBoxPlotToStringFail1()
	{
		final DrawGraphs gr = new DrawGraphs();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			gr.boxPlotToString(new LinkedList<List<Double>>(), new LinkedList<String>(),"green");
		}},IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testBoxPlotToStringFail2()
	{
		final DrawGraphs gr = new DrawGraphs();
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			gr.boxPlotToString(data, new LinkedList<String>(),"green");
		}},IllegalArgumentException.class,"mismatch");
	}

	@Test
	public void testBoxPlotToStringFail3()
	{
		final DrawGraphs gr = new DrawGraphs();
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			gr.boxPlotToString(data, new LinkedList<String>(),"green");
		}},IllegalArgumentException.class,"not used");
	}

	@Test
	public void testBoxPlotToString1()
	{
		final DrawGraphs gr = new DrawGraphs();
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		String colour = "green";
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),c(7.0,8.0,3.0),names=c(\"graphA\",\"graphB\"),col=c(\""+colour+"\",\""+colour+"\"))",
				gr.boxPlotToString(data, Arrays.asList(new String[]{"graphA","graphB"}),colour));
	}
	
	/** As above but without labels. */
	@Test
	public void testBoxPlotToString2()
	{
		final DrawGraphs gr = new DrawGraphs();
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		String colour = "green";
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),c(7.0,8.0,3.0),col=c(\""+colour+"\",\""+colour+"\"))",
				gr.boxPlotToString(data, null,colour));
	}
	
	/** As above but one vector and without labels. */
	@Test
	public void testBoxPlotToString3()
	{
		final DrawGraphs gr = new DrawGraphs();
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		String colour = "green";
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),col=c(\""+colour+"\"))",
				gr.boxPlotToString(data, null,colour));
	}
	public static final File testDir = new File("resources","__TestDrawGraphs__");

	@Before
	public void before()
	{
		if (!testDir.isDirectory()) 
			Assert.assertTrue(testDir.mkdir());
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
		// Slashes have to be the Unix-way - R simply terminates the DLL on WinXP otherwise.
		gr.drawBoxPlot(data, Arrays.asList(new String[]{"graphA","graphB"}),output.getAbsolutePath().replace(File.separatorChar, '/'));
		BufferedReader reader = new BufferedReader(new FileReader(output));
		String line = null;
		List<String> stringsOfInterest = Arrays.asList(new String[]{"Title (R Graphics Output)", "aphA","aphB"});
		Map<String,Boolean> encounteredStrings = new TreeMap<String,Boolean>();
		try
		{
			while((line=reader.readLine()) != null)
				for(String str:stringsOfInterest)
					if (line.contains(str)) encounteredStrings.put(str,true);
		}
		finally
		{
			reader.close();
		}
		Assert.assertEquals(stringsOfInterest.size(),encounteredStrings.size());// ensure that we find all our strings	
	}
	
}
