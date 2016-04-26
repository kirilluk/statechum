package statechum.analysis.learning.experiments;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.DrawGraphs.RGraph;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;

public class TestSGE_ExperimentRunner {

	public static final File testDir = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP),"__Test_SGE__");

	public TestSGE_ExperimentRunner() {
	}

	@Before
	public void before()
	{
		File tmpDir = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP));
		if (!tmpDir.isDirectory())
		{
			Assert.assertTrue("could not create "+tmpDir.getAbsolutePath(),tmpDir.mkdir());
		}
		
		if (!testDir.isDirectory())
		{
			Assert.assertTrue("could not create "+testDir.getAbsolutePath(),testDir.mkdir());
		}
	}

	@After
	public void after()
	{
		ExperimentRunner.zapDir(testDir);
	}

	public static class DummyExperiment implements Callable<Integer>
	{
		protected final int value;
		
		public DummyExperiment(int a)
		{
			value = a;
		}
		
		@Override
		public Integer call() throws Exception 
		{
			return value;
		}
		
	}
	
	public static class MockPlot<ELEM extends Comparable<? super ELEM>> extends RGraph<ELEM>
	{
		public String data="";
		
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

	final MockPlot<String> gr_StructuralDiff = new MockPlot<String>("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File("tmp/runA_struct.pdf"));
	final MockPlot<String> gr_BCR = new MockPlot<String>("BCR, Sicco","BCR, EDSM-Markov learner",new File("tmp/runA_BCR.pdf"));		
	final MockPlot<String> gr_a = new MockPlot<String>("Structural score, Sicco","Structural Score, EDSM-Markov learner",new File("tmp/runA_a.pdf"));
	final MockPlot<String> gr_b = new MockPlot<String>("BCR, Sicco","BCR, EDSM-Markov learner",new File("tmp/runA_b.pdf"));		

	public int runA(String []args)
	{
		RunSubExperiment<Integer> experimentRunner = new RunSubExperiment<Integer>(1,testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr_StructuralDiff,new Double(result),new Double(result+1),null,null);
					experimentrunner.Record(gr_BCR,new Double(result+1),new Double(result-1),null,null);
				}

				@Override
				public String getSubExperimentName()
				{
					return "experiment runA";
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR};
				}
				
		});
		return experimentRunner.successfulTermination();
	}

	// same as runA but with both labels and colours (as strings since this is what is expected by R)
	public int runB_both_labels_and_colours(String []args)
	{
		RunSubExperiment<Integer> experimentRunner = new RunSubExperiment<Integer>(1,testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr_StructuralDiff,new Double(result),new Double(result+1),"dd"+new Double(result+1),null);
					experimentrunner.Record(gr_BCR,new Double(result+1),new Double(result-1),null,"tt"+new Double(result+1));
				}

				@Override
				public String getSubExperimentName()
				{
					return "tmp/experimentrunA";
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
		RunSubExperiment<Integer> experimentRunner = new RunSubExperiment<Integer>(1,testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample){
				@Override
				public Integer call() throws Exception 
				{
					if (value != 2)
						return value;
					
					throw new IllegalArgumentException("task failed");
				}
			};
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr_StructuralDiff,new Double(result),new Double(result+1),"dd"+new Double(result+1),null);
					experimentrunner.Record(gr_BCR,new Double(result+1),new Double(result-1),null,"tt"+new Double(result+1));
				}

				@Override
				public String getSubExperimentName()
				{
					return "tmp/experimentrunA";
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
		RunSubExperiment<Integer> experimentRunner = new RunSubExperiment<Integer>(1,testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample){
				@Override
				public Integer call() throws Exception 
				{
					if (value != 2)
						return value;
					
					return null;
				}
			};
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr_StructuralDiff,new Double(result),new Double(result+1),"dd"+new Double(result+1),null);
					experimentrunner.Record(gr_BCR,new Double(result+1),new Double(result-1),null,"tt"+new Double(result+1));
				}

				@Override
				public String getSubExperimentName()
				{
					return "tmp/experimentrunA";
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR};
				}
				
		});
		return experimentRunner.successfulTermination();
	}
	
	// same as runA but experiment places invalid data in the output file.
	public int runE_invalid_data_in_output_file(String []args)
	{
		RunSubExperiment<Integer> experimentRunner = new RunSubExperiment<Integer>(1,testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr_StructuralDiff,new java.io.File("gg"+result),new Double(result+1),"dd"+new Double(result+1),null);// invalid data is an instance of class file rather than an instance of 
					experimentrunner.Record(gr_BCR,new Double(result+1),new Double(result-1),null,"tt"+new Double(result+1));
				}

				@Override
				public String getSubExperimentName()
				{
					return "tmp/experimentrunA";
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
		RunSubExperiment<Integer> experimentRunner = new RunSubExperiment<Integer>(1,testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr_StructuralDiff,new Double(result),new Double(result+1),"dd"+new Double(result+1),null);
					experimentrunner.Record(gr_BCR,new Double(result+1),new Double(result-1),null,"tt"+new Double(result+1));
				}

				@Override
				public String getSubExperimentName()
				{
					return "tmp/experimentrunA";
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR};
				}
				
		});
		for(int sample=0;sample<2;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr_a,new Double(result),new Double(result+2),"aa"+new Double(result+1),"bb"+new Double(result+1));
					experimentrunner.Record(gr_b,new Double(result+1),new Double(result-1),null,null);
				}

				@Override
				public String getSubExperimentName()
				{
					return "tmp/experimentrunA";
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
		RunSubExperiment<Integer> experimentRunner = new RunSubExperiment<Integer>(1,testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr_StructuralDiff,new Double(result),new Double(result+1),"dd"+new Double(result+1),null);
					experimentrunner.Record(gr_BCR,new Double(result+1),new Double(result-1),null,"tt"+new Double(result+1));
				}

				@Override
				public String getSubExperimentName()
				{
					return "tmp/experimentrunA";
				}
				
				@SuppressWarnings("rawtypes")
				@Override
				public RGraph[] getGraphs() {
					return new RGraph[]{gr_StructuralDiff,gr_BCR};
				}
				
		});
		for(int sample=0;sample<2;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample){
				@Override
				public Integer call() throws Exception 
				{
					if (value != 1)
						return value;
					
					return null;
				}
			};
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr_a,new Double(result),new Double(result+2),"aa"+new Double(result+1),"bb"+new Double(result+1));
					experimentrunner.Record(gr_b,new Double(result+1),new Double(result-1),null,null);
				}

				@Override
				public String getSubExperimentName()
				{
					return "tmp/experimentrunA";
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
		RunSubExperiment<Integer> experimentRunner = new RunSubExperiment<Integer>(1,testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr_StructuralDiff,new Double(result),new Double(result+1),null,null);
					experimentrunner.Record(gr_BCR,new Double(result+1),new Double(result-1),null,null);
				}

				@Override
				public String getSubExperimentName()
				{
					return "experiment runA";
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
		RunSubExperiment<Integer> experimentRunner = new RunSubExperiment<Integer>(1,testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr_StructuralDiff,new Double(result),new Double(result+1),null,null);
					experimentrunner.Record(gr_BCR,new Double(result+1),new Double(result-1),null,null);
				}

				@Override
				public String getSubExperimentName()
				{
					return "experiment runA";
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
		RunSubExperiment<Integer> experimentRunner = new RunSubExperiment<Integer>(1,testDir.getAbsolutePath(),args);
		for(int sample=0;sample<3;++sample)
		{
			DummyExperiment learnerRunner = new DummyExperiment(sample);
			experimentRunner.submitTask(learnerRunner);
		}
		experimentRunner.collectOutcomeOfExperiments(new processSubExperimentResult<Integer>() {

				@Override
				public void processSubResult(Integer result, RunSubExperiment<Integer> experimentrunner) throws IOException 
				{
					experimentrunner.Record(gr,new Double(result),new Double(result+1),null,null);
					experimentrunner.Record(gr_BCR,new Double(result+1),new Double(result-1),null,null);
				}

				@Override
				public String getSubExperimentName()
				{
					return "experiment runA";
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
		Assert.assertEquals(3,runA(new String[]{"COUNT_TASKS"}));
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());// until we actually run tasks, there should be nothing recorded.
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testCount1b() throws Exception
	{
		Assert.assertEquals(3,runD_null_for_one_of_the_samples(new String[]{"COUNT_TASKS"}));
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}
	
	@Test
	public void testCount2() throws Exception
	{
		Assert.assertEquals(5,runMultiple(new String[]{"COUNT_TASKS"}));
		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}

	@Test
	public void testRun1() throws Exception
	{
		Assert.assertEquals(0,runA(new String[]{}));// runs standalone
		Assert.assertEquals("[1.0,-1.0,NULL,NULL][2.0,0.0,NULL,NULL][3.0,1.0,NULL,NULL]",gr_BCR.getData());
		Assert.assertEquals("[0.0,1.0,NULL,NULL][1.0,2.0,NULL,NULL][2.0,3.0,NULL,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
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
	public void testRun4() throws Exception
	{
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
	public void testRun5a() throws Exception
	{
		for(int i=1;i<=runMultiple(new String[]{"COUNT_TASKS"});++i)
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
		int counter = runD_null_for_one_of_the_samples(new String[]{"COUNT_TASKS"});
		for(int i=1;i<=counter-1;++i)
			Assert.assertEquals(0,runD_null_for_one_of_the_samples(new String[]{"RUN_TASK",""+i}));

		try
		{
			Assert.assertEquals(0,runD_null_for_one_of_the_samples(new String[]{"RUN_TASK",""+counter}));
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
		}, IllegalArgumentException.class, "experimentrunA-2");

		Assert.assertEquals("[1.0,-1.0,NULL,tt1.0][2.0,0.0,NULL,tt2.0]",gr_BCR.getData());// only partial data is available due to failure, 
			// we cannot eliminate it completely because the failure is only detected part-way through. In reality, we'll not write .pdfs on a failure and thus no data will be available at all. 
		Assert.assertEquals("[0.0,1.0,dd1.0,NULL][1.0,2.0,dd2.0,NULL]",gr_StructuralDiff.getData());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}
	
	@Test
	public void testRun5c() throws Exception
	{
		int counter = runE_invalid_data_in_output_file(new String[]{"COUNT_TASKS"});
		for(int i=1;i<=counter;++i)
			Assert.assertEquals(0,runE_invalid_data_in_output_file(new String[]{"RUN_TASK",""+i}));

		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
					runE_invalid_data_in_output_file(new String[]{"COLLECT_RESULTS"}); // will throw because experiment 2 did not complete
			}
		}, IllegalArgumentException.class, "cannot load a value of type");// value of type File cannot be loaded.

		Assert.assertTrue(gr_BCR.getData().isEmpty());Assert.assertTrue(gr_StructuralDiff.getData().isEmpty());
		Assert.assertTrue(gr_a.getData().isEmpty());Assert.assertTrue(gr_b.getData().isEmpty());
	}
	
	@Test
	public void testRun5d() throws Exception
	{
		for(int i=1;i<=runMultipleFail2(new String[]{"COUNT_TASKS"})-1;++i)
			Assert.assertEquals(0,runMultipleFail2(new String[]{"RUN_TASK",""+i}));
		try
		{
			Assert.assertEquals(0,runMultipleFail2(new String[]{"RUN_TASK",""+runMultipleFail2(new String[]{"COUNT_TASKS"})}));// this particular task fails
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			// ignore this - this experiment should fail
		}
	}
	
	@Test
	public void testRun5e() throws Exception
	{
		for(int i=1;i<=runMultiple(new String[]{"COUNT_TASKS"})-1;++i)
			Assert.assertEquals(0,runMultiple(new String[]{"RUN_TASK",""+i}));
		// here we deliberately ignore one of the experiments
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runMultipleFail2(new String[]{"COLLECT_RESULTS"}); // will throw because experiment 2 did not complete
			}
		}, IllegalArgumentException.class, "experimentrunA-4");

		Assert.assertEquals("[1.0,-1.0,NULL,tt1.0][2.0,0.0,NULL,tt2.0][3.0,1.0,NULL,tt3.0]",gr_BCR.getData());
		Assert.assertEquals("[0.0,1.0,dd1.0,NULL][1.0,2.0,dd2.0,NULL][2.0,3.0,dd3.0,NULL]",gr_StructuralDiff.getData());
		Assert.assertEquals("[0.0,2.0,aa1.0,bb1.0]",gr_a.getData());
		Assert.assertEquals("[1.0,-1.0,NULL,NULL]",gr_b.getData());
	}
	
	// if I run a task with too high an ID, it is ignored
	@Test
	public void testRun6() throws Exception
	{
		Assert.assertEquals(0,runB_both_labels_and_colours(new String[]{"RUN_TASK","100"}));
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
				runA(new String[]{"COUNT_TASKS","21"});
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
				runA(new String[]{"RUN_STANDALONE","21"});
			}
		}, IllegalArgumentException.class, "no arguments");
	}
	
	
	@Test
	public void testInvalidPhase4()
	{
		Helper.checkForCorrectException(new whatToRun() {
			
			@Override
			public void run()
			{
				runA(new String[]{"COLLECT_RESULTS","21"});
			}
		}, IllegalArgumentException.class, "no arguments");
	}
	
	@Test
	public void testInvalidPhase5()
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
	public void testInvalidPhase6()
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

	
	
}
