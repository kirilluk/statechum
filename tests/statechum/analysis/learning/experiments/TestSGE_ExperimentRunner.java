package statechum.analysis.learning.experiments;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.RGraph;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.DrawGraphs.TimeAndCorrection;
import statechum.analysis.learning.TestDrawGraphs;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.experiments.EvaluationOfLearners.SmallVsHuge;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;

public class TestSGE_ExperimentRunner
{
	public TestSGE_ExperimentRunner() {
	}

	String globalTimeScaling;
	
	@Before
	public void before()
	{
		File tmpDir = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP));
		globalTimeScaling = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING);
		if (!tmpDir.isDirectory())
		{
			Assert.assertTrue("could not create "+tmpDir.getAbsolutePath(),tmpDir.mkdir());
		}
		
		if (!ExperimentRunner.testDir.isDirectory())
		{
			for(int t=0;t< 5;++t)
				if (!ExperimentRunner.testDir.mkdir())
				{
					try {
						Thread.sleep(10);
					} catch (InterruptedException e) {
						break;// assume we need to proceed
					}
				}
			if (!ExperimentRunner.testDir.isDirectory())
				Assert.assertTrue("could not create "+ExperimentRunner.testDir.getAbsolutePath(),ExperimentRunner.testDir.mkdir());
		}
	}

	@After
	public void after()
	{
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING, globalTimeScaling);
		ExperimentRunner.zapDir(ExperimentRunner.testDir);
	}

	public static  class DummyExperiment<PAR extends ThreadResultID> extends UASExperiment<PAR,ExperimentResult<PAR>>
	{
		public DummyExperiment(PAR parameters, LearnerEvaluationConfiguration eval, String directoryNamePrefix) 
		{
			super(parameters, eval, directoryNamePrefix);
		}
		
		@Override
		public ExperimentResult<PAR> runexperiment() throws Exception 
		{
			return new ExperimentResult<PAR>(par);
		}
		
	}
	
	public static class MockPlot<ELEM extends Comparable<? super ELEM>> extends RGraph<ELEM>
	{
		private String data="";
		
		public MockPlot(String x, String y, File name) {
			super(x, y, name);
		}
		
		@Override
		public List<String> getDrawingCommand()
		{
			return Collections.emptyList();
		}

		@Override
		protected double computeHorizSize() {
			return 0;
		}

		@SuppressWarnings("unused")
		@Override
		public synchronized void add(ELEM el, Double value) {
			throw new UnsupportedOperationException("only four-argument add is supported");
		}

		@Override
		public synchronized void add(ELEM el, Double value, String colour,String label) {
			data+="["+el+","+value+","+(colour == null?"NULL":colour)+","+(label == null?"NULL":label)+"]";
		}
		
		public String getData()
		{
			return data;
		}
	}

	public static class MockCSV extends CSVExperimentResult
	{
		private String data="";

		@Override
		public void add(ThreadResultID id,String text) 
		{
			super.add(id,text);
			data=data+"[("+id.getRowID()+","+id.getColumnID()+") "+text+"]";
		}

		public MockCSV(File arg) {
			super(arg);
		}

		public String getData()
		{
			return data;
		}
	}
	
	final MockPlot<String> gr_StructuralDiff = new MockPlot<String>("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File("tmp/runA_struct.pdf"));
	final MockPlot<String> gr_BCR = new MockPlot<String>("BCR, Sicco","BCR, EDSM-Markov learner",new File("tmp/runA_BCR.pdf"));		
	final MockPlot<String> gr_a = new MockPlot<String>("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File("tmp/runA_a.pdf"));
	final MockPlot<String> gr_b = new MockPlot<String>("BCR, Sicco","BCR, EDSM-Markov learner",new File("tmp/runA_b.pdf"));		
	final MockCSV csvA = new MockCSV(new File(ExperimentRunner.testDir,"runCSV_A.csv"));
	final MockCSV csvB = new MockCSV(new File(ExperimentRunner.testDir,"runCSV_B.csv"));
			
	public int runA(String []args)
	{
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row_first",sample),null,ExperimentRunner.testSGEDirectory);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_StructuralDiff,new Double(result.parameters.value),new Double(result.parameters.value+1),null,null);
					experimentrunner.RecordR(gr_BCR,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,null);
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR};
				}
				
		});
		return experimentRunner.successfulTermination();
	}

	public int runcsv_A(String []args)
	{
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row_second",sample),null,ExperimentRunner.testSGEDirectory);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_StructuralDiff,new Double(result.parameters.value),new Double(result.parameters.value+1),null,null);
					experimentrunner.RecordCSV(csvA,new TestDrawGraphs.TestParameters(result.parameters.value+"_1","A",new String[]{"experiment"},new String[]{"value"}),"line A"+result.parameters.value);
					experimentrunner.RecordCSV(csvB,new TestDrawGraphs.TestParameters(result.parameters.value+"_2","B",new String[]{"experiment"},new String[]{"value"}),"line B"+result.parameters.value);					
				}
				
				@Override
				public SGEExperimentResult[] getGraphs() {
					return new SGEExperimentResult[]{gr_StructuralDiff,csvA,csvB};
				}
				
		});
		return experimentRunner.successfulTermination();
	}

	/** Same as runcsv_A but only csvA is populated with data. */
	public int runcsv_B(String []args)
	{
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row",sample),null,ExperimentRunner.testSGEDirectory);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_StructuralDiff,new Double(result.parameters.value),new Double(result.parameters.value+1),null,null);
					experimentrunner.RecordCSV(csvA,new TestDrawGraphs.TestParameters(result.parameters.value+"_1","A",new String[]{"experiment"},new String[]{"value"}),"line A1_"+result.parameters.value);
					experimentrunner.RecordCSV(csvA,new TestDrawGraphs.TestParameters(result.parameters.value+"_2","A",new String[]{"experiment"},new String[]{"value"}),"line A2_"+result.parameters.value);					
				}
				
				@Override
				public SGEExperimentResult[] getGraphs() {
					return new SGEExperimentResult[]{gr_StructuralDiff,csvA,csvB};
				}
				
		});
		return experimentRunner.successfulTermination();
	}

	// same as runA but with both labels and colours (as strings since this is what is expected by R)
	public int runB_both_labels_and_colours(String []args)
	{
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row",sample),null,ExperimentRunner.testSGEDirectory);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_StructuralDiff,new Double(result.parameters.value),new Double(result.parameters.value+1),"dd"+new Double(result.parameters.value+1),null);
					experimentrunner.RecordR(gr_BCR,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,"tt"+new Double(result.parameters.value+1));
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR};
				}
				
		});
		return experimentRunner.successfulTermination();
	}

	// same as runA but experiment fails for one of the samples
	public int runC_fails_in_one_of_the_samples(String []args)
	{
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row",sample),null,ExperimentRunner.testSGEDirectory){
				@Override
				public ExperimentResult<TestParameters> call() throws Exception 
				{
					if (par.value != 2)
						return new ExperimentResult<TestParameters>(par);
					
					throw new IllegalArgumentException("task failed");
				}
			};
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_StructuralDiff,new Double(result.parameters.value),new Double(result.parameters.value+1),"dd"+new Double(result.parameters.value+1),null);
					experimentrunner.RecordR(gr_BCR,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,"tt"+new Double(result.parameters.value+1));
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR};
				}
				
		});
		return experimentRunner.successfulTermination();
	}

	// same as runA but experiment returns null for one of the samples
	public int runD_null_for_one_of_the_samples(String []args)
	{
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		experimentRunner.setThrowOnTaskReturningNull(true);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row",sample),null,ExperimentRunner.testSGEDirectory){
				@Override
				public ExperimentResult<TestParameters> call() throws Exception 
				{
					if (par.value != 2)
						return new ExperimentResult<TestParameters>(par);
					
					return null;
				}
			};
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_StructuralDiff,new Double(result.parameters.value),new Double(result.parameters.value+1),"dd"+new Double(result.parameters.value+1),null);
					experimentrunner.RecordR(gr_BCR,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,"tt"+new Double(result.parameters.value+1));
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR};
				}
				
		});
		return experimentRunner.successfulTermination();
	}
	
	public int runE(final int timeMult,final MockCSV csvSpreadsheet, String []args)
	{
		final Random rnd = new Random(timeMult);
		final RunSubExperiment<TestParametersMultiCell,ExperimentResult<TestParametersMultiCell>> experimentRunner = new RunSubExperiment<TestParametersMultiCell,ExperimentResult<TestParametersMultiCell>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<6;++sample)
		{
			DummyExperiment<TestParametersMultiCell> learnerRunner = new DummyExperiment<TestParametersMultiCell>(new TestParametersMultiCell("row",sample),null,ExperimentRunner.testSGEDirectory){
				@Override
				public ExperimentResult<TestParametersMultiCell> call() throws Exception 
				{
					return new ExperimentResult<TestParametersMultiCell>(par);
				}
			};
			experimentRunner.submitTask(learnerRunner);
		}
		csvSpreadsheet.setMissingValue(SmallVsHuge.unknownValue);
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParametersMultiCell,ExperimentResult<TestParametersMultiCell>>() {

			@Override
			public void processSubResult(ExperimentResult<TestParametersMultiCell> result, RunSubExperiment<TestParametersMultiCell,ExperimentResult<TestParametersMultiCell>> experimentrunner) throws IOException 
			{
				StringBuffer csvLine = new StringBuffer();
				csvLine.append(result.parameters.value);
				CSVExperimentResult.addSeparator(csvLine);
				csvLine.append(result.parameters.value*timeMult+rnd.nextInt(4));// this value pretends to be the time
				CSVExperimentResult.addSeparator(csvLine);
				csvLine.append(result.parameters.value+20);
				experimentrunner.RecordCSV(csvSpreadsheet, result.parameters, csvLine.toString());
			}
			
			@Override
			public SGEExperimentResult[] getGraphs() {
				return new SGEExperimentResult[]{csvSpreadsheet};
			}
		});
		return experimentRunner.successfulTermination();
	}
	
	/** Almost the same as runE but computes different values for one of the cells. */
	public int runF(final int timeMult,final MockCSV csvSpreadsheet, String []args)
	{
		final Random rnd = new Random(timeMult);
		final RunSubExperiment<TestParametersMultiCell,ExperimentResult<TestParametersMultiCell>> experimentRunner = new RunSubExperiment<TestParametersMultiCell,ExperimentResult<TestParametersMultiCell>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<6;++sample)
		{
			DummyExperiment<TestParametersMultiCell> learnerRunner = new DummyExperiment<TestParametersMultiCell>(new TestParametersMultiCell("row",sample),null,ExperimentRunner.testSGEDirectory){
				@Override
				public ExperimentResult<TestParametersMultiCell> call() throws Exception 
				{
					return new ExperimentResult<TestParametersMultiCell>(par);
				}
			};
			experimentRunner.submitTask(learnerRunner);
		}
		csvSpreadsheet.setMissingValue(SmallVsHuge.unknownValue);
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParametersMultiCell,ExperimentResult<TestParametersMultiCell>>() {

			@Override
			public void processSubResult(ExperimentResult<TestParametersMultiCell> result, RunSubExperiment<TestParametersMultiCell,ExperimentResult<TestParametersMultiCell>> experimentrunner) throws IOException 
			{
				StringBuffer csvLine = new StringBuffer();
				csvLine.append(result.parameters.value);
				CSVExperimentResult.addSeparator(csvLine);
				csvLine.append(result.parameters.value*timeMult+rnd.nextInt(4));// this value pretends to be the time
				CSVExperimentResult.addSeparator(csvLine);
				if (result.parameters.value == 5)
					csvLine.append(result.parameters.value+221);
				else
					csvLine.append(result.parameters.value+20);
				experimentrunner.RecordCSV(csvSpreadsheet, result.parameters, csvLine.toString());
			}
			
			@Override
			public SGEExperimentResult[] getGraphs() {
				return new SGEExperimentResult[]{csvSpreadsheet};
			}
		});
		return experimentRunner.successfulTermination();
	}
		
	public static class TestParameters implements ThreadResultID
	{
		final int value;
		final String rowID;
		public TestParameters(String r,int v)
		{
			value = v;rowID=r;
		}
		
		@Override
		public String getRowID() 
		{
			return rowID;
		}

		@Override
		public String[] getColumnText() {
			return new String[]{"column_text"};
		}

		@Override
		public String getColumnID() {
			return Integer.toString(value);
		}

		@Override
		public String[] headerValuesForEachCell() {
			return new String[]{"cell_header"};
		}

		@Override
		public String getSubExperimentName() {
			return "experiment_name";
		}

		@Override
		public int executionTimeInCell() {
			return -1;
		}
		
	}

	public static class TestParametersMultiCell implements ThreadResultID
	{
		final int value;
		final String rowID;
		public TestParametersMultiCell(String r,int v)
		{
			value = v;rowID=r+(v/2);
		}
		
		@Override
		public String getRowID() 
		{
			return rowID;
		}

		@Override
		public String[] getColumnText() {
			return new String[]{"column_text"};
		}

		@Override
		public String getColumnID() {
			return Integer.toString(value % 2);
		}

		@Override
		public String[] headerValuesForEachCell() {
			return new String[]{"cell_header1","cell_time","cell_header2"};
		}

		@Override
		public String getSubExperimentName() {
			return "experiment_name";
		}

		@Override
		public int executionTimeInCell() {
			return 1;
		}
		
	}
	
	// same as runA but experiment places invalid data in the output file.
	public int runE_invalid_data_in_output_file(String []args)
	{
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row",sample),null,ExperimentRunner.testSGEDirectory);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					if (result.parameters.value >= 1)
						experimentrunner.RecordR(gr_StructuralDiff,new Double(result.parameters.value+1),new Double(result.parameters.value+1),"dd"+new Double(result.parameters.value+1),null);// invalid data is an instance of class file rather than an instance of
					else
						experimentrunner.RecordR(gr_StructuralDiff,new java.io.File("gg"+result),new Double(result.parameters.value+1),"dd"+new Double(result.parameters.value+1),null);
					experimentrunner.RecordR(gr_BCR,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,"tt"+new Double(result.parameters.value+1));
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR};
				}
				
		});
		return experimentRunner.successfulTermination();
	}
	
	// Multiple experiments in a single method
	public int runMultiple(String []args)
	{
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row_first",sample),null,ExperimentRunner.testSGEDirectory);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_StructuralDiff,new Double(result.parameters.value),new Double(result.parameters.value+1),"dd"+new Double(result.parameters.value+1),null);
					experimentrunner.RecordR(gr_BCR,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,"tt"+new Double(result.parameters.value+1));
				}
			
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR};
				}
				
		});
		for(int sample=0;sample<2;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row_second",sample),null,ExperimentRunner.testSGEDirectory);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_a,new Double(result.parameters.value),new Double(result.parameters.value+2),"aa"+new Double(result.parameters.value+1),"bb"+new Double(result.parameters.value+1));
					experimentrunner.RecordR(gr_b,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,null);
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_a,gr_b};
				}
				
		});
		return experimentRunner.successfulTermination();
	}
	
	// Multiple experiments in a single method, the second phase contains a failing experiment
	public int runMultipleFail2(String []args)
	{
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		experimentRunner.setThrowOnTaskReturningNull(true);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row_first",sample),null,ExperimentRunner.testSGEDirectory);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_StructuralDiff,new Double(result.parameters.value),new Double(result.parameters.value+1),"dd"+new Double(result.parameters.value+1),null);
					experimentrunner.RecordR(gr_BCR,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,"tt"+new Double(result.parameters.value+1));
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR};
				}
				
		});
		for(int sample=0;sample<2;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row_second",sample),null,ExperimentRunner.testSGEDirectory){
				@Override
				public ExperimentResult<TestParameters> call() throws Exception 
				{
					if (par.value != 1)
						return new ExperimentResult<TestParameters>(par);
					
					return null;
				}
			};
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_a,new Double(result.parameters.value),new Double(result.parameters.value+2),"aa"+new Double(result.parameters.value+1),"bb"+new Double(result.parameters.value+1));
					experimentrunner.RecordR(gr_b,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,null);
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_a,gr_b};
				}
				
			});
		return experimentRunner.successfulTermination();
	}
	
	public int runDuplicateFilenames(String []args)
	{
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row",sample),null,ExperimentRunner.testSGEDirectory);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_StructuralDiff,new Double(result.parameters.value),new Double(result.parameters.value+1),null,null);
					experimentrunner.RecordR(gr_BCR,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,null);
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR,gr_StructuralDiff};
				}
				
			});
		return experimentRunner.successfulTermination();
	}
	
	public int runUnknownGraphs(String []args)
	{
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row",sample),null,ExperimentRunner.testSGEDirectory);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr_StructuralDiff,new Double(result.parameters.value),new Double(result.parameters.value+1),null,null);
					experimentrunner.RecordR(gr_BCR,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,null);
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{};
				}
				
			});
		return experimentRunner.successfulTermination();
	}
	
	public int runInvalidFileName(String []args)
	{
		final MockPlot<String> gr = new MockPlot<String>("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File("tmp/|runA_struct.pdf"));
		RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentRunner = new RunSubExperiment<TestParameters,ExperimentResult<TestParameters>>(1,ExperimentRunner.testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment<TestParameters> learnerRunner = new DummyExperiment<TestParameters>(new TestParameters("row",sample),null,ExperimentRunner.testSGEDirectory);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<TestParameters,ExperimentResult<TestParameters>>() {

				@Override
				public void processSubResult(ExperimentResult<TestParameters> result, RunSubExperiment<TestParameters,ExperimentResult<TestParameters>> experimentrunner) throws IOException 
				{
					experimentrunner.RecordR(gr,new Double(result.parameters.value),new Double(result.parameters.value+1),null,null);
					experimentrunner.RecordR(gr_BCR,new Double(result.parameters.value+1),new Double(result.parameters.value-1),null,null);
				}
			
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr,gr_StructuralDiff,gr_BCR};
				}
				
			});
		return experimentRunner.successfulTermination();
	}
	
	@Test
	public void testCount1a() throws Exception
	{
		Assert.assertEquals(3,runA(new String[]{"COUNT_TASKS","3"}));
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());// until we actually run tasks, there should be nothing recorded.
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testCount1b() throws Exception
	{
		Assert.assertEquals(3,runD_null_for_one_of_the_samples(new String[]{"COUNT_TASKS","3"}));
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}
	
	@Test
	public void testCount1c() throws Exception
	{
		Assert.assertEquals(3,runcsv_A(new String[]{"COUNT_TASKS","3"}));
		Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());Assert.assertTrue(csvA.getData().isEmpty());Assert.assertTrue(csvB.getData().isEmpty());// until we actually run tasks, there should be nothing recorded.
	}

	@Test
	public void testCount2a() throws Exception
	{
		Assert.assertEquals(3,runMultiple(new String[]{"COUNT_TASKS","3"}));
		Assert.assertEquals("{1=[0], 2=[1, 2], 3=[3, 4]}", statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment.loadVirtTaskToReal(ExperimentRunner.testDir.getAbsolutePath()+File.separator).toString());
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testCount2b() throws Exception
	{
		Assert.assertEquals(1,runMultiple(new String[]{"COUNT_TASKS","1"}));
		Assert.assertEquals("{1=[0, 1, 2, 3, 4]}", statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment.loadVirtTaskToReal(ExperimentRunner.testDir.getAbsolutePath()+File.separator).toString());
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testCount2c() throws Exception
	{
		Assert.assertEquals(5,runMultiple(new String[]{"COUNT_TASKS","8"}));
		Assert.assertEquals("{1=[0], 2=[1], 3=[2], 4=[3], 5=[4]}", statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment.loadVirtTaskToReal(ExperimentRunner.testDir.getAbsolutePath()+File.separator).toString());
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testCount3() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {

			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				runMultiple(new String[]{"COUNT_TASKS"});
			}}, IllegalArgumentException.class, "the number of tasks per virtual task has to be provided");
		
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testCount4() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {

			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				runMultiple(new String[]{"COUNT_TASKS","3","4"});
			}}, IllegalArgumentException.class, "the number of tasks per virtual task has to be provided");
		
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testCount5() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {

			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				runMultiple(new String[]{"COUNT_TASKS","-3"});
			}}, IllegalArgumentException.class, "the number of real tasks to run");
		
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testCount6() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {

			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				runMultiple(new String[]{"COUNT_TASKS","a"});
			}}, IllegalArgumentException.class, "invalid number");
		
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun1a() throws Exception
	{
		Assert.assertEquals(0,runA(new String[]{}));// runs standalone
		Assert.assertEquals("[1.0,-1.0,NULL,NULL][2.0,0.0,NULL,NULL][3.0,1.0,NULL,NULL]",gr_BCR.getData());
		Assert.assertEquals("[0.0,1.0,NULL,NULL][1.0,2.0,NULL,NULL][2.0,3.0,NULL,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun1b() throws Exception
	{
		Assert.assertEquals(0,runcsv_A(new String[]{}));// runs standalone
		Assert.assertEquals("[0.0,1.0,NULL,NULL][1.0,2.0,NULL,NULL][2.0,3.0,NULL,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals("[(0_1,A) line A0][(1_1,A) line A1][(2_1,A) line A2]",csvA.getData());
		Assert.assertEquals("[(0_2,B) line B0][(1_2,B) line B1][(2_2,B) line B2]",csvB.getData());
	}

	@Test
	public void testRun1c() throws Exception
	{
		Assert.assertEquals(0,runcsv_B(new String[]{}));// runs standalone
		Assert.assertEquals("[0.0,1.0,NULL,NULL][1.0,2.0,NULL,NULL][2.0,3.0,NULL,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals("[(0_1,A) line A1_0][(0_2,A) line A2_0][(1_1,A) line A1_1][(1_2,A) line A2_1][(2_1,A) line A1_2][(2_2,A) line A2_2]",csvA.getData());
		Assert.assertEquals("",csvB.getData());
	}

	@Test
	public void testRun2() throws Exception
	{
		Assert.assertEquals(0,runB_both_labels_and_colours(new String[]{}));// implicit standalone
		Assert.assertEquals("[1.0,-1.0,NULL,tt1.0][2.0,0.0,NULL,tt2.0][3.0,1.0,NULL,tt3.0]",gr_BCR.getData());
		Assert.assertEquals("[0.0,1.0,dd1.0,NULL][1.0,2.0,dd2.0,NULL][2.0,3.0,dd3.0,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun3() throws Exception
	{
		Assert.assertEquals(0,runB_both_labels_and_colours(new String[]{"RUN_STANDALONE"}));// explicit standalone
		Assert.assertEquals("[1.0,-1.0,NULL,tt1.0][2.0,0.0,NULL,tt2.0][3.0,1.0,NULL,tt3.0]",gr_BCR.getData());
		Assert.assertEquals("[0.0,1.0,dd1.0,NULL][1.0,2.0,dd2.0,NULL][2.0,3.0,dd3.0,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun4a() throws Exception
	{
		Assert.assertEquals(3,runB_both_labels_and_colours(new String[]{"COUNT_TASKS","3"}));
		Assert.assertEquals(0,runB_both_labels_and_colours(new String[]{"RUN_TASK","1"}));
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals(0,runB_both_labels_and_colours(new String[]{"RUN_TASK","2"}));
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals(0,runB_both_labels_and_colours(new String[]{"RUN_TASK","3"}));
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals(0,runB_both_labels_and_colours(new String[]{"COLLECT_RESULTS"}));

		Assert.assertEquals("[1.0,-1.0,NULL,tt1.0][2.0,0.0,NULL,tt2.0][3.0,1.0,NULL,tt3.0]",gr_BCR.getData());
		Assert.assertEquals("[0.0,1.0,dd1.0,NULL][1.0,2.0,dd2.0,NULL][2.0,3.0,dd3.0,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}
	
	@Test
	public void testRun4b() throws Exception
	{
		Assert.assertEquals(3,runB_both_labels_and_colours(new String[]{"COUNT_TASKS","3"}));
		Assert.assertEquals(0,runcsv_A(new String[]{"RUN_TASK","1"}));
		Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());Assert.assertTrue(csvA.getData().isEmpty());Assert.assertTrue(csvB.getData().isEmpty());
		Assert.assertEquals(0,runcsv_A(new String[]{"RUN_TASK","2"}));
		Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());Assert.assertTrue(csvA.getData().isEmpty());Assert.assertTrue(csvB.getData().isEmpty());
		Assert.assertEquals(0,runcsv_A(new String[]{"RUN_TASK","3"}));
		Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());Assert.assertTrue(csvA.getData().isEmpty());Assert.assertTrue(csvB.getData().isEmpty());
		Assert.assertEquals(0,runcsv_A(new String[]{"COLLECT_RESULTS"}));
		Assert.assertEquals("[0.0,1.0,NULL,NULL][1.0,2.0,NULL,NULL][2.0,3.0,NULL,NULL]",gr_StructuralDiff.getData());
		Assert.assertEquals("[(0_1,A) line A0][(1_1,A) line A1][(2_1,A) line A2]",csvA.getData());
		Assert.assertEquals("[(0_2,B) line B0][(1_2,B) line B1][(2_2,B) line B2]",csvB.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun5a() throws Exception
	{
		int taskNumber = runMultiple(new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=taskNumber;++i)
			Assert.assertEquals(0,runMultiple(new String[]{"RUN_TASK",""+i}));
		Assert.assertEquals(0,runMultiple(new String[]{"COLLECT_RESULTS"}));

		Assert.assertEquals("[1.0,-1.0,NULL,tt1.0][2.0,0.0,NULL,tt2.0][3.0,1.0,NULL,tt3.0]",gr_BCR.getData());
		Assert.assertEquals("[0.0,1.0,dd1.0,NULL][1.0,2.0,dd2.0,NULL][2.0,3.0,dd3.0,NULL]",gr_StructuralDiff.getData());
		Assert.assertEquals("[0.0,2.0,aa1.0,bb1.0][1.0,3.0,aa2.0,bb2.0]",gr_a.getData());
		Assert.assertEquals("[1.0,-1.0,NULL,NULL][2.0,0.0,NULL,NULL]",gr_b.getData());
	}
	
	@Test
	public void testRun5b() throws Exception
	{
		int count = runcsv_A(new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			Assert.assertEquals(0,runcsv_A(new String[]{"RUN_TASK",""+i}));
		Assert.assertEquals(0,runcsv_A(new String[]{"COLLECT_RESULTS"}));

		Assert.assertEquals("[0.0,1.0,NULL,NULL][1.0,2.0,NULL,NULL][2.0,3.0,NULL,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals("[(0_1,A) line A0][(1_1,A) line A1][(2_1,A) line A2]",csvA.getData());
		Assert.assertEquals("[(0_2,B) line B0][(1_2,B) line B1][(2_2,B) line B2]",csvB.getData());
	}

	@Test
	public void testRun5c() throws Exception
	{
		int count = runcsv_B(new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			Assert.assertEquals(0,runcsv_B(new String[]{"RUN_TASK",""+i}));
		Assert.assertEquals(0,runcsv_B(new String[]{"COLLECT_RESULTS"}));

		Assert.assertEquals("[0.0,1.0,NULL,NULL][1.0,2.0,NULL,NULL][2.0,3.0,NULL,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals("[(0_1,A) line A1_0][(0_2,A) line A2_0][(1_1,A) line A1_1][(1_2,A) line A2_1][(2_1,A) line A1_2][(2_2,A) line A2_2]",csvA.getData());
		Assert.assertEquals("",csvB.getData());
	}

	@Test
	public void testRun5c_parallel1() throws Exception
	{
		int count = runcsv_B(new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			Assert.assertEquals(0,runcsv_B(new String[]{"RUN_PARALLEL",""+i}));
		Assert.assertEquals(0,runcsv_B(new String[]{"COLLECT_RESULTS"}));

		Assert.assertEquals("[0.0,1.0,NULL,NULL][1.0,2.0,NULL,NULL][2.0,3.0,NULL,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals("[(0_1,A) line A1_0][(0_2,A) line A2_0][(1_1,A) line A1_1][(1_2,A) line A2_1][(2_1,A) line A1_2][(2_2,A) line A2_2]",csvA.getData());
		Assert.assertEquals("",csvB.getData());
	}

	public static String readResultFile(CSVExperimentResult result)
	{
		StringBuffer buffer = new StringBuffer();
		try
		{
			BufferedReader reader = new BufferedReader(new FileReader(result.getAbsoluteFileName()));
			String line = null;
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
		}
		catch(IOException ex)
		{
			Helper.throwUnchecked("failed to read result file "+result.getAbsoluteFileName(), ex);
		}
		return buffer.toString();
	}
	
	/** Tests handling of missing rows. */
	@Test
	public void testRun5c_parallel2() throws Exception
	{
		int count = runE(10,csvA, new String[]{"COUNT_TASKS","3"});
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING,"1.32");

		for(int i=1;i<=count;++i)
			runE(10,csvA, new String[]{"RUN_PARALLEL",""+i});
		
		for(int i=2;i<4;++i)
		{
			TestParametersMultiCell p=new TestParametersMultiCell("row",2);
			File experimentResultFile = new File(ExperimentRunner.testDir,p.getSubExperimentName()+"-"+p.getRowID()+File.separator+(i % 2));
			Assert.assertTrue(experimentResultFile.canRead());
			experimentResultFile.delete();
		}
		Assert.assertEquals(0,runE(10,csvA, new String[]{"COLLECT_AVAILABLE"}));

		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals("[(row0,0) 0,2,20][(row0,1) 1,8,21][(row2,0) 4,32,24][(row2,1) 5,39,25]",csvA.getData());
		Assert.assertEquals("",csvB.getData());

		String dataInResultFile = readResultFile(csvA);
		Assert.assertEquals(
				"[,column_text,column_text,column_text,column_text,column_text,column_text]"+
				"[experiment,cell_header1,cell_time,cell_header2,cell_header1,cell_time,cell_header2]"+
				"[row0,0,2,20,1,8,21][row2,4,32,24,5,39,25]",
		dataInResultFile);
	}
	
	/** Tests handling of missing cells. */
	@Test
	public void testRun5c_parallel3() throws Exception
	{
		int count = runE(10,csvA, new String[]{"COUNT_TASKS","3"});
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING,"1.32");

		for(int i=1;i<=count;++i)
			runE(10,csvA, new String[]{"RUN_PARALLEL",""+i});
		for(int i=2;i<3;++i)
		{
			TestParametersMultiCell p=new TestParametersMultiCell("row",2);
			File experimentResultFile = new File(ExperimentRunner.testDir,p.getSubExperimentName()+"-"+p.getRowID()+File.separator+(i % 2));
			Assert.assertTrue(experimentResultFile.canRead());
			experimentResultFile.delete();
		}
		Assert.assertEquals(0,runE(10,csvA, new String[]{"COLLECT_AVAILABLE"}));

		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals("[(row0,0) 0,2,20][(row0,1) 1,8,21][(row1,1) 3,23,23][(row2,0) 4,32,24][(row2,1) 5,39,25]",csvA.getData());
		Assert.assertEquals("",csvB.getData());
		String dataInResultFile = readResultFile(csvA);
		Assert.assertEquals(
				"[,column_text,column_text,column_text,column_text,column_text,column_text]"+
				"[experiment,cell_header1,cell_time,cell_header2,cell_header1,cell_time,cell_header2]"+
				"[row0,0,2,20,1,8,21][row1,UNKNOWN,UNKNOWN,UNKNOWN,3,23,23][row2,4,32,24,5,39,25]",
		dataInResultFile);
	}
	
	@Test
	public void testRun5c_checkMatching1a() throws Exception
	{
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING,"1.0");
		int mult0 = 10;
		int count = runE(mult0,csvA, new String[]{"COUNT_TASKS","6"});
		for(int i=1;i<=count;++i)
			runE(mult0,csvA, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult0,csvA, new String[]{"COLLECT_RESULTS"}));
		
		int mult1 = 15;
		count = runE(mult1,csvB, new String[]{"COUNT_TASKS","3"});// although it may seem that we are re-running an experiment here, the spreadsheet csvB is different to the one above (csvA) which makes experimentRunner aware that it is an altogether different experiment and results are therefore replaced with new ones.
		for(int i=1;i<=count;++i)
			runE(mult1,csvB, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult1,csvB, new String[]{"COLLECT_RESULTS"}));
		
		TimeAndCorrection tc = DrawGraphs.computeTimeAndCorrection(csvA, csvB, new TestParametersMultiCell("row",0), -1,0, 1.0);
		Assert.assertEquals((double)mult1/(double)mult0, tc.average, 0.1);// the big discrepancy is due to randomness introduced in the runE method. Hence not using Configuration.fpAccuracy
		Assert.assertEquals(6, tc.count);
	}	
	
	/** Almost the same as testRun5c_checkMatching1a but has missing values. */
	@Test
	public void testRun5c_checkMatching1b() throws Exception
	{
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING,"1.0");
		int mult0 = 10;
		int count = runE(mult0,csvA, new String[]{"COUNT_TASKS","6"});
		for(int i=1;i<=count;++i)
			runE(mult0,csvA, new String[]{"RUN_PARALLEL",""+i});
		
		for(int i=3;i<6;++i)
		{
			TestParametersMultiCell p=new TestParametersMultiCell("row",i/2);
			File experimentResultFile = new File(ExperimentRunner.testDir,p.getSubExperimentName()+"-"+p.getRowID()+File.separator+(i % 2));
			Assert.assertTrue("missing file for cell "+i,experimentResultFile.canRead());
			experimentResultFile.delete();
		}
		Assert.assertEquals(0,runE(mult0,csvA, new String[]{"COLLECT_AVAILABLE"}));

		int mult1 = 15;
		count = runE(mult1,csvB, new String[]{"COUNT_TASKS","3"});// although it may seem that we are re-running an experiment here, the spreadsheet csvB is different to the one above (csvA) which makes experimentRunner aware that it is an altogether different experiment and results are therefore replaced with new ones.
		for(int i=1;i<=count;++i)
			runE(mult1,csvB, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult1,csvB, new String[]{"COLLECT_RESULTS"}));
		
		TimeAndCorrection tc = DrawGraphs.computeTimeAndCorrection(csvA, csvB, new TestParametersMultiCell("row",0), -1,0, 1.0);
		Assert.assertEquals((double)mult1/(double)mult0, tc.average, 0.2);// the big discrepancy is due to randomness introduced in the runE method. Hence not using Configuration.fpAccuracy
		Assert.assertEquals(3, tc.count);
	}
	
	@Test
	public void testRun5c_checkMatching2() throws Exception
	{
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING,"1.0");
		int mult0 = 15;
		int count = runE(mult0,csvA, new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			runE(mult0,csvA, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult0,csvA, new String[]{"COLLECT_RESULTS"}));
		
		int mult1 = 10;
		count = runE(mult1,csvB, new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			runE(mult1,csvB, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult1,csvB, new String[]{"COLLECT_RESULTS"}));// although it may seem that we are re-running an experiment here, the spreadsheet csvB is different to the one above (csvA) which makes experimentRunner aware that it is an altogether different experiment and results are therefore replaced with new ones.
		
		TimeAndCorrection tc = DrawGraphs.computeTimeAndCorrection(csvA, csvB, new TestParametersMultiCell("row",0), -1,0, 1.0);
		Assert.assertEquals((double)mult1/(double)mult0, tc.average, 0.1);// the big discrepancy is due to randomness introduced in the runE method. Hence not using Configuration.fpAccuracy
		Assert.assertEquals(6, tc.count);
	}	
	
	@Test
	public void testRun5c_checkMatching3() throws Exception
	{
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING,"1.0");
		int mult0 = 10;
		int count = runE(mult0,csvA, new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			runE(mult0,csvA, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult0,csvA, new String[]{"COLLECT_RESULTS"}));
		
		int mult1 = 10;
		count = runE(mult1,csvB, new String[]{"COUNT_TASKS","3"});// although it may seem that we are re-running an experiment here, the spreadsheet csvB is different to the one above (csvA) which makes experimentRunner aware that it is an altogether different experiment and results are therefore replaced with new ones.
		for(int i=1;i<=count;++i)
			runE(mult1,csvB, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult1,csvB, new String[]{"COLLECT_RESULTS"}));
		
		TimeAndCorrection tc = DrawGraphs.computeTimeAndCorrection(csvA, csvB, new TestParametersMultiCell("row",0), -1,0, 1.0);
		Assert.assertEquals((double)mult1/(double)mult0, tc.average, 0.1);// the big discrepancy is due to randomness introduced in the runE method. Hence not using Configuration.fpAccuracy
		Assert.assertEquals(6, tc.count);
	}
	
	@Test
	public void testRun5c_checkMatching3_timeouts1() throws Exception
	{
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING,"1.0");
		int mult0 = 10;
		int count = runE(mult0,csvA, new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			runE(mult0,csvA, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult0,csvA, new String[]{"COLLECT_RESULTS"}));
		
		int mult1 = 10;
		count = runE(mult1,csvB, new String[]{"COUNT_TASKS","3"});// although it may seem that we are re-running an experiment here, the spreadsheet csvB is different to the one above (csvA) which makes experimentRunner aware that it is an altogether different experiment and results are therefore replaced with new ones.
		for(int i=1;i<=count;++i)
			runE(mult1,csvB, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult1,csvB, new String[]{"COLLECT_RESULTS"}));
		
		TimeAndCorrection tc = DrawGraphs.computeTimeAndCorrection(csvA, csvB, new TestParametersMultiCell("row",0), 30,0, 1.0);
		Assert.assertEquals((double)mult1/(double)mult0, tc.average, 0.1);// the big discrepancy is due to randomness introduced in the runE method. Hence not using Configuration.fpAccuracy
		Assert.assertEquals(3, tc.count);
	}	

	/** here timeouts filter all the values. */
	@Test
	public void testRun5c_checkMatching3_timeouts2() throws Exception
	{
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING,"1.0");
		int mult0 = 10;
		int count = runE(mult0,csvA, new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			runE(mult0,csvA, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult0,csvA, new String[]{"COLLECT_RESULTS"}));
		
		int mult1 = 10;
		count = runE(mult1,csvB, new String[]{"COUNT_TASKS","3"});// although it may seem that we are re-running an experiment here, the spreadsheet csvB is different to the one above (csvA) which makes experimentRunner aware that it is an altogether different experiment and results are therefore replaced with new ones.
		for(int i=1;i<=count;++i)
			runE(mult1,csvB, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult1,csvB, new String[]{"COLLECT_RESULTS"}));
		
		TimeAndCorrection tc = DrawGraphs.computeTimeAndCorrection(csvA, csvB, new TestParametersMultiCell("row",0), 0,0, 1.0);
		Assert.assertEquals(0, tc.count);
	}	

	@Test
	public void testRun5c_checkMatching4() throws Exception
	{
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING,"1.0");
		int mult0 = 10;
		int count = runE(mult0,csvA, new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			runE(mult0,csvA, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult0,csvA, new String[]{"COLLECT_RESULTS"}));
		
		Helper.checkForCorrectException(new whatToRun() {

			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				DrawGraphs.computeTimeAndCorrection(csvA, csvB, new TestParametersMultiCell("row",0)
					{
						@Override
						public int executionTimeInCell() {
							return -1;
						}
					}
				, -1,0, 1.0);
		}}, IllegalArgumentException.class, "no time is present");
		
	}	

	@Test
	public void testRun5c_checkMatching5() throws Exception
	{
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING,"1.0");
		int mult0 = 10;
		int count = runE(mult0,csvA, new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			runE(mult0,csvA, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult0,csvA, new String[]{"COLLECT_RESULTS"}));
		
		Helper.checkForCorrectException(new whatToRun() {

			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				DrawGraphs.computeTimeAndCorrection(csvA, csvB, new TestParametersMultiCell("row",0)
					{
						@Override
						public int executionTimeInCell() {
							return 10;
						}
					}
				, -1,0, 1.0);
		}}, IllegalArgumentException.class, "value is too high");
		
	}	

	@Test
	public void testRun5c_checkMatching6() throws Exception
	{
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING,"1.0");
		int mult0 = 10;
		int count = runE(mult0,csvA, new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			runE(mult0,csvA, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runE(mult0,csvA, new String[]{"COLLECT_RESULTS"}));
		
		int mult1 = 10;
		count = runF(mult1,csvB, new String[]{"COUNT_TASKS","3"});// although it may seem that we are re-running an experiment here, the spreadsheet csvB is different to the one above (csvA) which makes experimentRunner aware that it is an altogether different experiment and results are therefore replaced with new ones.
		for(int i=1;i<=count;++i)
			runF(mult1,csvB, new String[]{"RUN_PARALLEL",""+i});
		Assert.assertEquals(0,runF(mult1,csvB, new String[]{"COLLECT_RESULTS"}));
		
		
		Helper.checkForCorrectException(new whatToRun() {

			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				DrawGraphs.computeTimeAndCorrection(csvA, csvB, new TestParametersMultiCell("row",0), -1,0, 1.0);
		}}, IllegalArgumentException.class, "Cell [row2,1] is different");
		
	}	
	
	
	/** Tests that a request to only report a specific plot is honoured: here we only request to render a plot that is non-empty. */
	@Test
	public void testRun5c_collect1a() throws Exception
	{
		int count = runcsv_B(new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			Assert.assertEquals(0,runcsv_B(new String[]{"RUN_TASK",""+i}));
		Assert.assertEquals(0,runcsv_B(new String[]{"COLLECT_RESULTS", "runCSV_A.csv"}));

		Assert.assertEquals("",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals("[(0_1,A) line A1_0][(0_2,A) line A2_0][(1_1,A) line A1_1][(1_2,A) line A2_1][(2_1,A) line A1_2][(2_2,A) line A2_2]",csvA.getData());
		Assert.assertEquals("",csvB.getData());
	}

	/** Tests that a request to only report a specific plot is honoured: here we only request to render a plot that is empty. */
	@Test
	public void testRun5c_collect1b() throws Exception
	{
		int count = runcsv_B(new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			Assert.assertEquals(0,runcsv_B(new String[]{"RUN_TASK",""+i}));
		Assert.assertEquals(0,runcsv_B(new String[]{"COLLECT_RESULTS", "runCSV_B.csv"}));

		Assert.assertEquals("",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
		Assert.assertEquals("",csvA.getData());
		Assert.assertEquals("",csvB.getData());
	}

	/** Tests that a request to only report a specific plot is honoured: here we request a plot that does not exist. */
	@Test
	public void testRun5c_collect2() throws Exception
	{
		int count = runcsv_B(new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=count;++i)
			Assert.assertEquals(0,runcsv_B(new String[]{"RUN_TASK",""+i}));
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
					runD_null_for_one_of_the_samples(new String[]{"COLLECT_RESULTS","AA"}); // will throw because experiment 2 did not complete
			}
		}, IllegalArgumentException.class, "invalid plot \"AA");	}


	@Test
	public void testRun5d() throws Exception
	{
		Assert.assertEquals(0,runD_null_for_one_of_the_samples(new String[]{"PROGRESS_INDICATOR"}));// 0% complete
		int counter = runD_null_for_one_of_the_samples(new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=counter-1;++i)
			Assert.assertEquals(0,runD_null_for_one_of_the_samples(new String[]{"RUN_TASK",""+i}));

		try
		{
			runD_null_for_one_of_the_samples(new String[]{"RUN_TASK",""+counter});
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			// ignore this - this experiment should fail
		}
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
					runD_null_for_one_of_the_samples(new String[]{"COLLECT_RESULTS"}); // will throw because experiment 2 did not complete
			}
		}, IllegalArgumentException.class, "experiment_name-row"+File.separator+"2");
		Assert.assertEquals(66,runD_null_for_one_of_the_samples(new String[]{"PROGRESS_INDICATOR"}));// 66% complete because one failed.
		Assert.assertEquals("[1.0,-1.0,NULL,tt1.0][2.0,0.0,NULL,tt2.0]",gr_BCR.getData());// only partial data is available due to failure, 
			// we cannot eliminate it completely because the failure is only detected part-way through. In reality, we'll not write .pdfs on a failure and thus no data will be available at all. 
		Assert.assertEquals("[0.0,1.0,dd1.0,NULL][1.0,2.0,dd2.0,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}
	
	@Test
	public void testRun5e() throws Exception
	{
		Assert.assertEquals(0,runE_invalid_data_in_output_file(new String[]{"PROGRESS_INDICATOR"}));// 0% complete
		int counter = runE_invalid_data_in_output_file(new String[]{"COUNT_TASKS","3"});
		for(int i=1;i<=counter;++i)
			Assert.assertEquals(0,runE_invalid_data_in_output_file(new String[]{"RUN_TASK",""+i}));

		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
					runE_invalid_data_in_output_file(new String[]{"COLLECT_RESULTS"}); // will throw because experiment 2 did not complete
			}
		}, IllegalArgumentException.class, "cannot load a value of type");// value of type File cannot be loaded.
		Assert.assertEquals(66,runE_invalid_data_in_output_file(new String[]{"PROGRESS_INDICATOR"}));// 66% complete because one failed.
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}
	
	@Test
	public void testRun5f() throws Exception
	{
		for(int i=1;i<=runMultipleFail2(new String[]{"COUNT_TASKS","3"})-1;++i)
			Assert.assertEquals(0,runMultipleFail2(new String[]{"RUN_TASK",""+i}));
		try
		{
			Assert.assertEquals(0,runMultipleFail2(new String[]{"RUN_TASK",""+runMultipleFail2(new String[]{"COUNT_TASKS","3"})}));// this particular task fails
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			// ignore this - this experiment should fail
		}
	}
	
	@Test
	public void testRun5g() throws Exception
	{
		int taskCount = runMultiple(new String[]{"COUNT_TASKS","5"});// this should be evaluated once, if done multiple times, it rebuilds a virtual-physical map, leading to skipped tasks.
		for(int i=1;i<=taskCount-1;++i)
			Assert.assertEquals(0,runMultiple(new String[]{"RUN_TASK",""+i}));
		// here we deliberately ignore one of the experiments
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runMultipleFail2(new String[]{"COLLECT_RESULTS"}); // will throw because experiment 2 did not complete
			}
		}, IllegalArgumentException.class, "experiment_name-row_second"+File.separator+"1");

		Assert.assertEquals("[1.0,-1.0,NULL,tt1.0][2.0,0.0,NULL,tt2.0][3.0,1.0,NULL,tt3.0]",gr_BCR.getData());
		Assert.assertEquals("[0.0,1.0,dd1.0,NULL][1.0,2.0,dd2.0,NULL][2.0,3.0,dd3.0,NULL]",gr_StructuralDiff.getData());
		Assert.assertEquals("[0.0,2.0,aa1.0,bb1.0]",gr_a.getData());
		Assert.assertEquals("[1.0,-1.0,NULL,NULL]",gr_b.getData());
	}
	
	@Test
	public void testRun5g_parallel1() throws Exception
	{
		int taskCount = runMultiple(new String[]{"COUNT_TASKS","5"});// this should be evaluated once, if done multiple times, it rebuilds a virtual-physical map, leading to skipped tasks.
		for(int i=1;i<=taskCount-1;++i)
			Assert.assertEquals(0,runMultiple(new String[]{"RUN_PARALLEL",""+i}));
		// here we deliberately ignore one of the experiments
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runMultipleFail2(new String[]{"COLLECT_RESULTS"}); // will throw because experiment 2 did not complete
			}
		}, IllegalArgumentException.class, "experiment_name-row_second"+File.separator+"1");

		Assert.assertEquals("[1.0,-1.0,NULL,tt1.0][2.0,0.0,NULL,tt2.0][3.0,1.0,NULL,tt3.0]",gr_BCR.getData());
		Assert.assertEquals("[0.0,1.0,dd1.0,NULL][1.0,2.0,dd2.0,NULL][2.0,3.0,dd3.0,NULL]",gr_StructuralDiff.getData());
		Assert.assertEquals("[0.0,2.0,aa1.0,bb1.0]",gr_a.getData());
		Assert.assertEquals("[1.0,-1.0,NULL,NULL]",gr_b.getData());
	}
	
	@Test
	public void testRun5g_parallel2() throws Exception
	{
		int taskCount = runMultiple(new String[]{"COUNT_TASKS","5"});// this should be evaluated once, if done multiple times, it rebuilds a virtual-physical map, leading to skipped tasks.
		for(int i=1;i<=taskCount-1;++i)
			Assert.assertEquals(0,runMultiple(new String[]{"RUN_PARALLEL",""+i}));
		// here we deliberately ignore one of the experiments
		runMultipleFail2(new String[]{"COLLECT_AVAILABLE"}); // will use undefined values for missing cells.

		Assert.assertEquals("[1.0,-1.0,NULL,tt1.0][2.0,0.0,NULL,tt2.0][3.0,1.0,NULL,tt3.0]",gr_BCR.getData());
		Assert.assertEquals("[0.0,1.0,dd1.0,NULL][1.0,2.0,dd2.0,NULL][2.0,3.0,dd3.0,NULL]",gr_StructuralDiff.getData());
		Assert.assertEquals("[0.0,2.0,aa1.0,bb1.0]",gr_a.getData());
		Assert.assertEquals("[1.0,-1.0,NULL,NULL]",gr_b.getData());
	}
	
	// Here we run an experiment, observe failure and then re-run it. This checks correct identification of successfully finished tasks.
	@Test
	public void testRun5h() throws Exception
	{
		Assert.assertEquals(0,runMultipleFail2(new String[]{"PROGRESS_INDICATOR"}));// 0% complete
		int taskCount = runMultipleFail2(new String[]{"COUNT_TASKS","5"});
		Assert.assertEquals(5, taskCount);
		for(int i=1;i<=taskCount-1;++i)
			Assert.assertEquals(0,runMultipleFail2(new String[]{"RUN_TASK",""+i}));
		try
		{
			Assert.assertEquals(0,runMultipleFail2(new String[]{"RUN_TASK",""+taskCount}));// this particular task fails
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			// ignore this - this experiment should fail
		}
		Assert.assertEquals(80,runMultipleFail2(new String[]{"PROGRESS_INDICATOR"}));// 80% complete because one failed.
		
		// Now try the same - we should only get 1 task reported.
		Assert.assertEquals(1,runMultipleFail2(new String[]{"COUNT_TASKS","5"}));
		try
		{
			runMultipleFail2(new String[]{"RUN_TASK","0"});// attempting the failing task again will fail.
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			// ignore this - this experiment should fail
		}
		// Now try the same for the second time - we should still only get 1 task reported.
		Assert.assertEquals(1,runMultipleFail2(new String[]{"COUNT_TASKS","5"}));
		Assert.assertEquals(0,runMultiple(new String[]{"RUN_TASK","1"}));// this one should be successful 
		Assert.assertEquals(100,runMultipleFail2(new String[]{"PROGRESS_INDICATOR"}));// all complete
	}
	
	@Test
	public void testRun5h_parallel1() throws Exception
	{
		int taskCount = runMultipleFail2(new String[]{"COUNT_TASKS","5"});
		Assert.assertEquals(5, taskCount);
		for(int i=1;i<=taskCount-1;++i)
			Assert.assertEquals(0,runMultipleFail2(new String[]{"RUN_PARALLEL",""+i}));
		try
		{
			Assert.assertEquals(0,runMultipleFail2(new String[]{"RUN_PARALLEL",""+taskCount}));// this particular task fails
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			// ignore this - this experiment should fail
		}
		
		// Now try the same - we should only get 1 task reported.
		Assert.assertEquals(1,runMultipleFail2(new String[]{"COUNT_TASKS","5"}));
		try
		{
			runMultipleFail2(new String[]{"RUN_PARALLEL","0"});// attempting the failing task again will fail.
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			// ignore this - this experiment should fail
		}
		// Now try the same for the second time - we should still only get 1 task reported.
		Assert.assertEquals(1,runMultipleFail2(new String[]{"COUNT_TASKS","5"}));
		Assert.assertEquals(0,runMultiple(new String[]{"RUN_PARALLEL","1"}));// this one should be successful 
	}
	
	@Test
	public void testRun5h_parallel2() throws Exception
	{
		int taskCount = runMultipleFail2(new String[]{"COUNT_TASKS","1"});
		Assert.assertEquals(1, taskCount);
		try
		{
			runMultipleFail2(new String[]{"RUN_PARALLEL","1"});// the last task fails
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			// ignore this - this experiment should fail
		}
		
		// Now try the same - we should only get 1 task reported.
		Assert.assertEquals(1,runMultipleFail2(new String[]{"COUNT_TASKS","5"}));
		try
		{
			runMultipleFail2(new String[]{"RUN_PARALLEL","0"});// attempting the failing task again will fail.
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			// ignore this - this experiment should fail
		}
		// Now try the same for the second time - we should still only get 1 task reported.
		Assert.assertEquals(1,runMultipleFail2(new String[]{"COUNT_TASKS","5"}));
		Assert.assertEquals(0,runMultiple(new String[]{"RUN_PARALLEL","1"}));// this one should be successful 
	}
	
	
	// if I run a task with too high an ID, it is ignored
	@Test
	public void testRun6a1() throws Exception
	{
		Assert.assertEquals(3,runMultiple(new String[]{"COUNT_TASKS","3"}));
		Assert.assertEquals(0,runB_both_labels_and_colours(new String[]{"RUN_TASK","100"}));
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	// if I run a task with too high an ID, it is ignored
	@Test
	public void testRun6a2() throws Exception
	{
		Assert.assertEquals(3,runMultiple(new String[]{"COUNT_TASKS","3"}));
		Assert.assertEquals(0,runB_both_labels_and_colours(new String[]{"RUN_PARALLEL","100"}));
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun6b1() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				runB_both_labels_and_colours(new String[]{"RUN_TASK"});
			}
		}, IllegalArgumentException.class, "should be");
		
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun6b2() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				runB_both_labels_and_colours(new String[]{"RUN_PARALLEL"});
			}
		}, IllegalArgumentException.class, "should be");
		
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun6c1() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				runB_both_labels_and_colours(new String[]{"RUN_TASK","-100"});
			}
		}, IllegalArgumentException.class, "should be positive");
		
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun6c2() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				runB_both_labels_and_colours(new String[]{"RUN_PARALLEL","-100"});
			}
		}, IllegalArgumentException.class, "should be positive");
		
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun6d1() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				runB_both_labels_and_colours(new String[]{"RUN_TASK","aa"});
			}
		}, IllegalArgumentException.class, "invalid number");
		
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun6d2() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws NumberFormatException, IOException, IncompatibleStatesException {
				runB_both_labels_and_colours(new String[]{"RUN_PARALLEL","aa"});
			}
		}, IllegalArgumentException.class, "invalid number");
		
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	// This one will fail in a specific experiment
	@Test
	public void testRun7() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runC_fails_in_one_of_the_samples(new String[]{"RUN_STANDALONE"});
			}
		}, IllegalArgumentException.class, "task failed");
	}

	// This one will fail in a specific experiment
	@Test
	public void testRun8() throws Exception
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runD_null_for_one_of_the_samples(new String[]{"RUN_STANDALONE"});
			}
		}, IllegalArgumentException.class, "returned null");
	}


	/** Checks that corrupt output files are detected as corrupt. */
	@Test
	public void testRun9() throws Exception
	{
		Assert.assertEquals(0,runMultiple(new String[]{"PROGRESS_INDICATOR"}));// 0% complete
		int taskCount = runMultiple(new String[]{"COUNT_TASKS","5"});// this should be evaluated once, if done multiple times, it rebuilds a virtual-physical map, leading to skipped tasks.
		for(int i=1;i<=taskCount;++i)
			Assert.assertEquals(0,runMultiple(new String[]{"RUN_TASK",""+i}));
		Assert.assertEquals(0,runMultiple(new String[]{"COLLECT_RESULTS"}));// checks everything is fine.
		// now we corrupt one of the output files
		
		
		BufferedWriter writer = null;
		try
		{
			writer = new BufferedWriter(new FileWriter(ExperimentRunner.testDir.getAbsolutePath()+File.separator+"experiment_name-row_second/0"));
			writer.append("junk");
		}
		finally
		{
			if (writer != null)
			{
				writer.close();writer = null;
			}
		}
		
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runMultiple(new String[]{"COLLECT_RESULTS"}); // will throw because experiment 2 did not complete
			}
		}, IllegalArgumentException.class, "experiment_name-row_second"+File.separator+"0");

		Assert.assertEquals(80,runMultiple(new String[]{"PROGRESS_INDICATOR"}));// 80% complete because one failed.
		runMultiple(new String[]{"RUN_TASK","4"});// physical task 3 corresponds to a virtual task 4
		Assert.assertEquals(100,runMultiple(new String[]{"PROGRESS_INDICATOR"}));// All done.
	}

	// invalid phase name
	@Test
	public void testInvalidPhase1()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runA(new String[]{"A"});
			}
		}, IllegalArgumentException.class, "No enum const");// it is const for java 6 and constant for 7.
	}
	
	@Test
	public void testInvalidPhase2()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runA(new String[]{"RUN_STANDALONE","21"});
			}
		}, IllegalArgumentException.class, "no arguments");
	}
	
	
	@Test
	public void testInvalidPhase3()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runA(new String[]{"COLLECT_RESULTS","21","22"});
			}
		}, IllegalArgumentException.class, "at most one argument");
	}
		
	@Test
	public void testInvalidPhase4()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runA(new String[]{"RUN_TASK","-21"}); 
			}
		}, IllegalArgumentException.class, "positive");
	}
	
	@Test
	public void testInvalidPhase5()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runA(new String[]{"RUN_TASK","0"}); 
			}
		}, IllegalArgumentException.class, "positive");
	}
	
	
	@Test
	public void testInvalidGraphs()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runDuplicateFilenames(new String[]{});
			}
		}, IllegalArgumentException.class, "duplicate");
	}
	
	@Test
	public void testUnknownGraphs()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runUnknownGraphs(new String[]{});
			}
		}, IllegalArgumentException.class, "unknown graph");
	}
	
	@Test
	public void testInvalidFileName()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runInvalidFileName(new String[]{});
			}
		}, IllegalArgumentException.class, "invalid file name");
	}

	@Test
	public void testGetCPUName1() throws IOException
	{
		Assert.assertNull(SGE_ExperimentRunner.getCpuFreqValue(new BufferedReader(new StringReader(""))));
	}
	
	@Test
	public void testGetCPUName2() throws IOException
	{
		Assert.assertEquals("AA",SGE_ExperimentRunner.getCpuFreqValue(new BufferedReader(new StringReader("text\n"+SGE_ExperimentRunner.cpuName+"\t AA\naa"))));
	}
	
	@Test
	public void testGetCorrection1() throws IOException
	{
		Assert.assertNull(SGE_ExperimentRunner.getCorrection(new BufferedReader(new StringReader("")),"AA"));
	}
	
	@Test
	public void testGetCorrection2() throws IOException
	{
		Assert.assertEquals("z",SGE_ExperimentRunner.getCorrection(new BufferedReader(new StringReader("text | t \n AA | z \n aa |n")),"AA"));
	}
	
	@Test
	public void testGetCorrection3()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run() throws IOException
			{
				SGE_ExperimentRunner.getCorrection(new BufferedReader(new StringReader("text | t | \n AA | z \n aa |n")),"AA");
			}
		}, IllegalArgumentException.class, "invalid file format");
		
	}
}
