package statechum.analysis.learning;

import static statechum.Helper.checkForCorrectException;

import java.io.BufferedReader;
import java.io.File;
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
import statechum.analysis.learning.DrawGraphs.RGraph;
import statechum.analysis.learning.DrawGraphs.RBagPlot;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
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
			DrawGraphs.boxPlotToString(new LinkedList<List<Double>>(), new LinkedList<String>(),"green",null);
		}},IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testBoxPlotToStringFail2()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			DrawGraphs.boxPlotToString(data, new LinkedList<String>(),"green",null);
		}},IllegalArgumentException.class,"mismatch");
	}

	@Test
	public void testBoxPlotToStringFail3()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			DrawGraphs.boxPlotToString(data, new LinkedList<String>(),"green",null);
		}},IllegalArgumentException.class,"not used");
	}

	@Test
	public void testBoxPlotToString1a()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		String colour = "green";
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),c(7.0,8.0,3.0),names=c(\"graphA\",\"graphB\"),col=c(\""+colour+"\",\""+colour+"\"))",
				DrawGraphs.boxPlotToString(data, Arrays.asList(new String[]{"graphA","graphB"}),colour,null));
	}
	
	@Test
	public void testBoxPlotToString1b()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		String colour = "green";
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),c(7.0,8.0,3.0),names=c(\"graphA\",\"graphB\"),col=c(\""+colour+"\",\""+colour+"\"),someOther attrs)",
				DrawGraphs.boxPlotToString(data, Arrays.asList(new String[]{"graphA","graphB"}),colour,"someOther attrs"));
	}
	
	/** As above but without labels. */
	@Test
	public void testBoxPlotToString2()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		String colour = "green";
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),c(7.0,8.0,3.0),col=c(\""+colour+"\",\""+colour+"\"))",
				DrawGraphs.boxPlotToString(data, null,colour,null));
	}
	
	/** As above but one vector and without labels. */
	@Test
	public void testBoxPlotToString3a()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		String colour = "green";
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),col=c(\""+colour+"\"))",
				DrawGraphs.boxPlotToString(data, null,colour,null));
	}
	/** As above but one vector and without labels. */
	@Test
	public void testBoxPlotToString3b()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		String colour = "green";
		Assert.assertEquals("boxplot(c(4.0,5.0,5.0),col=c(\""+colour+"\"),other attrs)",
				DrawGraphs.boxPlotToString(data, null,colour,"other attrs"));
	}
	public static final File testDir = new File("resources","__TestDrawGraphs__");

	@Test
	public void testBagPlotToStringFail1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			DrawGraphs.bagPlotToString(new LinkedList<List<Double>>(), new LinkedList<Double>(),null);
		}},IllegalArgumentException.class,"empty");
	}
	
	@Test
	public void testBagPlotToStringFail2()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		data.add(Arrays.asList(new Double[]{4.,5.,5.}));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			DrawGraphs.bagPlotToString(data, Arrays.asList(new Double[]{6.7}),null);
		}},IllegalArgumentException.class,"mismatch");
	}

	@Test
	public void testBagPlotToString1a()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		Assert.assertEquals("bagplot(c(7.0,7.0,8.3,8.3,8.3),c(4.0,5.0,7.0,8.0,3.0))",
				DrawGraphs.bagPlotToString(data, Arrays.asList(new Double[]{7.,8.3}),null));
	}

	@Test
	public void testBagPlotToString1b()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.}));
		data.add(Arrays.asList(new Double[]{7.,8.,3.}));
		Assert.assertEquals("bagplot(c(7.0,7.0,8.3,8.3,8.3),c(4.0,5.0,7.0,8.0,3.0),someOther attrs)",
				DrawGraphs.bagPlotToString(data, Arrays.asList(new Double[]{7.,8.3}),"someOther attrs"));
	}

	@Test
	public void testBagPlotToString1()
	{
		final List<List<Double>> data = new LinkedList<List<Double>>();
		data.add(Arrays.asList(new Double[]{4.,5.}));
		Assert.assertEquals("bagplot(c(7.0,7.0),c(4.0,5.0),someOther attrs)",
				DrawGraphs.bagPlotToString(data, Arrays.asList(new Double[]{7.}),"someOther attrs"));
	}

	@Before
	public void before()
	{
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
		gr.drawPlot(DrawGraphs.boxPlotToString(data, Arrays.asList(new String[]{"graphA","graphB"}),"green",null),7,7,output);
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
	
	@Test
	public void testPlotFail1()
	{
		final DrawGraphs gr = new DrawGraphs();
		final File output = new File(testDir,"out.pdf");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			gr.drawPlot("",0,1,output);
		}},IllegalArgumentException.class,"horizontal");
	}
	
	@Test
	public void testPlotFail2()
	{
		final DrawGraphs gr = new DrawGraphs();
		final File output = new File(testDir,"out.pdf");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			gr.drawPlot("",1,0,output);
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
		Assert.assertEquals("boxplot(c(34.0),col=c(\"green\"),xlab=\""+X+"\",ylab=\""+Y+"\")",g.getDrawingCommand());
	}
	
	@Test
	public void testGenerateGraph1b()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.add(4.5,34.);
		Assert.assertEquals("bagplot(c(4.5),c(34.0),xlab=\""+X+"\",ylab=\""+Y+"\")",g.getDrawingCommand());
	}
	
	@Test
	public void testGenerateGraph2a()
	{
		final String X="axisX", Y="axisY";
		RGraph<String> g=new RBoxPlot<String>(X,Y, new File("someName"));
		g.add("one",34.);
		g.add("one",34.);
		g.add("one",2.);
		Assert.assertEquals("boxplot(c(34.0,34.0,2.0),col=c(\"green\"),xlab=\""+X+"\",ylab=\""+Y+"\")",g.getDrawingCommand());
	}
	
	@Test
	public void testGenerateGraph2b()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.add(5.5,34.);
		g.add(5.5,34.);
		g.add(5.5,2.);
		Assert.assertEquals("bagplot(c(5.5,5.5,5.5),c(34.0,34.0,2.0),xlab=\""+X+"\",ylab=\""+Y+"\")",g.getDrawingCommand());
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
		Assert.assertEquals("boxplot(c(34.0,34.0,2.0),c(2.0),names=c(\"one\",\"two\"),col=c(\"green\",\"green\"),xlab=\""+X+"\",ylab=\""+Y+"\")",g.getDrawingCommand());
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
		Assert.assertEquals("bagplot(c(5.5,5.5,5.5,7.5),c(34.0,34.0,2.0,2.0),xlab=\""+X+"\",ylab=\""+Y+"\")",g.getDrawingCommand());
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
		Assert.assertEquals("bagplot(c(5.5,5.5,5.5,7.5),c(34.0,34.0,2.0,2.0),xlab=\""+X+"\",ylab=\""+Y+"\")",g.getDrawingCommand());
	}
	
	@Test
	public void testBoundaries2()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.setXboundaries(5.6, 34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		Assert.assertEquals("bagplot(c(7.5),c(2.0),xlab=\""+X+"\",ylab=\""+Y+"\")",g.getDrawingCommand());
	}
	
	@Test
	public void testBoundaries3()
	{
		final String X="axisX", Y="axisY";
		RGraph<Double> g=new RBagPlot(X,Y, new File("someName"));
		g.setYboundaries(5.5, 34.);
		g.add(5.5,34.);g.add(5.5,34.);g.add(5.5,2.);g.add(7.5,2.);
		Assert.assertEquals("bagplot(c(5.5,5.5),c(34.0,34.0),xlab=\""+X+"\",ylab=\""+Y+"\")",g.getDrawingCommand());
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
}
