/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */

package statechum.analysis.learning.experiments;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;


import statechum.Configuration;
import statechum.analysis.learning.experiments.ExperimentRunner.GeneratorConfiguration;
import statechum.analysis.learning.experiments.ExperimentRunner.LearnerEvaluator;
import statechum.analysis.learning.experiments.ExperimentRunner.LearnerFailed;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod;
import static statechum.Helper.whatToRun;
import static statechum.Helper.checkForCorrectException;

public class TestExperimentRunner {
	protected static int directoryCounter = 0;
	
	public final File testDir = new File(statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)
				,"__TestAbstractExperiment__"+(directoryCounter++)),
		testGraphsDir = new File(testDir,"__graphs"), testOutputDir = new File(testDir,ExperimentRunner.outputDirNamePrefix+testGraphsDir.getName());

	protected final Configuration config = Configuration.getDefaultConfiguration();
	protected final ConvertALabel converter = null;
	
	protected final LearnerGraph Graphs[] = new LearnerGraph[]{
			buildLearnerGraph("A-a->A-b-#B", "testAbstractExperiment_graph1.xml",config,converter),
			buildLearnerGraph("A-a->A-b->B-b->C-a->A", "testAbstractExperiment_graph2.xml",config,converter),
			buildLearnerGraph("A-a->A-b->B-b->C-a->A-c->A", "testAbstractExperiment_graph3.xml",config,converter),
			buildLearnerGraph("A-a->A-b->B-b->C-a->A\nC-b->C", "testAbstractExperiment_graph4.xml",config,converter),
			buildLearnerGraph("A-a->A-b->B-c->B", "testAbstractExperiment_graph5.xml",config,converter)
	};
	protected Map<String,LearnerGraph> graphs = new TreeMap<String,LearnerGraph>();
	
	/** Clears the test space. */
	protected void deleteTestDirectories()
	{
		ExperimentRunner.zapDir(testGraphsDir);
		ExperimentRunner.zapDir(testOutputDir);
		ExperimentRunner.zapDir(testDir);
		Assert.assertFalse(testDir.isDirectory());Assert.assertFalse(testGraphsDir.isDirectory());Assert.assertFalse(testOutputDir.isDirectory());
	}
	
	protected final File fileList = new File(testDir,"fileList");
	
	protected void populateGraphs()
	{
		try
		{
			LearnerGraph emptyGraph = new LearnerGraph(config);String emptyGraphName = "testAbstractExperiment_graph0.xml"; // an empty graph 
			graphs.put(emptyGraphName,emptyGraph);emptyGraph.storage.writeGraphML(new File(testGraphsDir,emptyGraphName).getAbsolutePath());
			Writer fileListWriter = new FileWriter(fileList);fileListWriter.write(new File(testGraphsDir,emptyGraphName).getAbsolutePath()+"\n");
			for(LearnerGraph graph:Graphs)
			{
				String graphName = graph.getName();
				graphs.put(graphName,graph);graph.storage.writeGraphML(new File(testGraphsDir,graphName).getAbsolutePath());
				fileListWriter.write(new File(testGraphsDir,graphName).getAbsolutePath()+"\n");
			}
			fileListWriter.close();
		}
		catch(IOException ex)
		{
			Assert.fail("failed to write graphs");
		}
	}
	
	protected class w_evaluator extends LearnerEvaluator
	{
		public w_evaluator(String inputFile, int per, int inID,
				ExperimentRunner exp, Configuration cnf, String name) {
			super(inputFile, per, inID, exp, cnf, name);
		}

		@Override
		protected void runTheExperiment() {
			if (WMethod.checkM(recoveryGraph, graph) == null)
			{// this is the special diagnostic graph
				LearnerGraph emptyGraph = new LearnerGraph(config);String emptyGraphName = "testAbstractExperiment_graph0.xml"; // an empty graph 
				try {
					emptyGraph.storage.writeGraphML(new File(testGraphsDir,emptyGraphName).getAbsolutePath());
				} catch (IOException e) {
					// if we cannot write a new version of the graph, we shall keep aborting and hence the test will fail.
				}
				Runtime.getRuntime().halt(-1); // abort the jvm
			}
			else
				result = result + graph.pathroutines.countEdges()*percent + ExperimentRunner.FS + WMethod.computeWSet_reducedmemory(graph).size();
		}
	}
	
	protected static class a_evaluator extends LearnerEvaluator
	{

		public a_evaluator(String inputFile, int per, int inID,
				ExperimentRunner exp, Configuration cnf, String name) {
			super(inputFile, per, inID, exp, cnf, name);
		}
		
		@Override
		protected void runTheExperiment() {
			result = result + graph.pathroutines.countEdges() + ExperimentRunner.FS + graph.pathroutines.computeAlphabet().size()*percent;
		}
	}
	
	@Before
	public final void beforeTest()
	{
		// clean the directories
		testDir.deleteOnExit();
                deleteTestDirectories();
		
		Assert.assertFalse(testDir.isDirectory());
                Assert.assertFalse(testGraphsDir.isDirectory());
		Assert.assertTrue(testDir.mkdir());
                Assert.assertTrue(testGraphsDir.mkdir());
                Assert.assertTrue(testOutputDir.mkdir());
		populateGraphs();
		
		multiExp = new ExperimentRunner();multiExp.graphsPerRunner=4;multiExp.setTimeBetweenHearbeats(heartbeatTestValue);
		multiExp.setTimeBetweenHearbeats(heartbeatTestValue);// make things run fast.
		multiExp.setLearnerStages(new int[]{30,45,90,99});
		multiExp.addLearnerEvaluator(new GeneratorConfiguration(Configuration.getDefaultConfiguration(),
				w_evaluator.class, "learnerTransitions"));
		multiExp.addLearnerEvaluator(new GeneratorConfiguration(Configuration.getDefaultConfiguration(),
				a_evaluator.class, "learnerAlphabet"));

		List<String> result = new LinkedList<String>();
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
			for(int stage:new int[]{30,45,90,99})
				result.add(gr.getKey()+ExperimentRunner.FS+"learnerAlphabet"+ExperimentRunner.FS+stage+
						ExperimentRunner.FS+gr.getValue().pathroutines.countEdges()+
						ExperimentRunner.FS+gr.getValue().pathroutines.computeAlphabet().size()*stage);
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
			for(int stage:new int[]{30,45,90,99})
				result.add(gr.getKey()+ExperimentRunner.FS+"learnerTransitions"+ExperimentRunner.FS+stage+
						ExperimentRunner.FS+gr.getValue().pathroutines.countEdges()*stage+
						ExperimentRunner.FS+WMethod.computeWSet_reducedmemory(gr.getValue()).size());
		multiExpResult = result.toArray(new String[]{});
	}

	@After
	public final void afterTest()
	{
		// clean the directories
		deleteTestDirectories();
	}
	
	protected String [] multiExpResult = null;
	protected ExperimentRunner multiExp = null;

	/** Test value for the heartbeat - we'd like it all happen fast. */
	private final int heartbeatTestValue = 500;

	/** Checks that the csv file contains the expected data. */
	protected final void checkCSV(String []expectedData)
	{
		BufferedReader reader = null;
		try
		{
			reader = new BufferedReader(new FileReader(new File(testOutputDir,ExperimentRunner.resultName)));
			int pos=0;String line = reader.readLine();
			while(pos < expectedData.length)
			{
				Assert.assertEquals(expectedData[pos], line);
				++pos;line=reader.readLine();
			}
			Assert.assertNull(line);
		}
		catch(IOException ex)
		{
			Assert.fail(ex.getMessage());
		}
		finally
		{
			if (reader != null) { try { reader.close();reader=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
		}
	}

	protected static class countEdge_evaluator extends LearnerEvaluator
	{
		public countEdge_evaluator(String inputFile, int per, int inID,
				ExperimentRunner exp, Configuration cnf, String name) {
			super(inputFile, per, inID, exp, cnf, name);
		}

		@Override
		protected void runTheExperiment() {
			result = result + graph.pathroutines.countEdges();
		}
	}
	
	protected ExperimentRunner getSingleStageEvaluator()
	{
		ExperimentRunner experiment = new ExperimentRunner();experiment.graphsPerRunner=4;experiment.setTimeBetweenHearbeats(heartbeatTestValue );
		experiment.addLearnerEvaluator(new GeneratorConfiguration(Configuration.getDefaultConfiguration(),
				countEdge_evaluator.class,"testAllGraphsSingleStage"));
		return experiment;
	}
	
	
	@Test 
	public final void testAllGraphsSingleStageSingleEvaluator() throws NumberFormatException, IOException
	{
		getSingleStageEvaluator().runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
		List<String> result = new LinkedList<String>();
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
				result.add(gr.getKey()+ExperimentRunner.FS+"testAllGraphsSingleStage"+ExperimentRunner.FS+gr.getValue().pathroutines.countEdges());
		checkCSV(result.toArray(new String[]{}));
	}
	
	/** Verify that desktop mode builds the correct spreadsheet. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator1() throws NumberFormatException, IOException
	{
		multiExp.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
		checkCSV(multiExpResult);
	}

	/** Checks that an exception is thrown if evaluator name is invalid. Valid 
	 * cases are considered in integration tests.
	 */
	@Test
	public final void testInvalidLearnerEvaluatorName1()
	{
		final ExperimentRunner experiment = new ExperimentRunner();experiment.graphsPerRunner=4;experiment.setTimeBetweenHearbeats(heartbeatTestValue);
		experiment.addLearnerEvaluator(new GeneratorConfiguration(Configuration.getDefaultConfiguration(),
				w_evaluator.class, "invalid"+FS));
		checkForCorrectException(new whatToRun() { public @Override void run() throws NumberFormatException, IOException {
			experiment.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
		}},IllegalArgumentException.class,"invalid learner name");
	}
	
	/** Checks that an exception is thrown if evaluator name is invalid. Valid 
	 * cases are considered in integration tests.
	 */
	@Test
	public final void testInvalidLearnerEvaluatorName2()
	{
		final ExperimentRunner experiment = new ExperimentRunner();experiment.graphsPerRunner=4;experiment.setTimeBetweenHearbeats(heartbeatTestValue);
		experiment.addLearnerEvaluator(new GeneratorConfiguration(Configuration.getDefaultConfiguration(),
				w_evaluator.class, ""));
		checkForCorrectException(new whatToRun() { public @Override void run() throws NumberFormatException, IOException {
			experiment.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
		}},IllegalArgumentException.class,"invalid learner name");
	}
	
	/** Checks that an exception is thrown if evaluator name is invalid. Valid 
	 * cases are considered in integration tests.
	 */
	@Test
	public final void testInvalidLearnerEvaluatorName3()
	{
		final ExperimentRunner experiment = new ExperimentRunner();experiment.graphsPerRunner=4;experiment.setTimeBetweenHearbeats(heartbeatTestValue);
		experiment.addLearnerEvaluator(new GeneratorConfiguration(Configuration.getDefaultConfiguration(),
				w_evaluator.class, null));
		checkForCorrectException(new whatToRun() { public @Override void run() throws NumberFormatException, IOException {
			experiment.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
		}},IllegalArgumentException.class,"invalid learner name");
	}
	
	/** Verify that Grid counting mode returns the correct numbers. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator2() throws NumberFormatException, IOException
	{
		Assert.assertEquals(multiExpResult.length,multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+ExperimentRunner.argCMD_COUNT}));
	}
	
	/** Verify that the Grid mode builds the correct spreadsheet. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator3() throws NumberFormatException, IOException
	{
		for(int i=0;i < multiExpResult.length;++i)
			multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+i});
		multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+ExperimentRunner.argCMD_POSTPROCESS});
		checkCSV(multiExpResult);
	}

	/** Verify that robust runner builds the correct spreadsheet. 
	 * @throws InterruptedException */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator4a() throws NumberFormatException, IOException
	{
		multiExp.robustRunExperiment(testGraphsDir.getAbsolutePath(), testOutputDir.getAbsolutePath());
		checkCSV(multiExpResult);
	}
	
	/** Verify that robust runner builds the correct spreadsheet. 
	 * @throws InterruptedException */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator4b_slow() throws NumberFormatException, IOException
	{
		multiExp.graphsPerRunner=1;
		multiExp.robustRunExperiment(testGraphsDir.getAbsolutePath(), testOutputDir.getAbsolutePath());
		checkCSV(multiExpResult);
	}
	
	/** Verify that robust runner builds the correct spreadsheet. 
	 * @throws InterruptedException */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator4c() throws NumberFormatException, IOException
	{
		multiExp.graphsPerRunner=7;
		multiExp.robustRunExperiment(testGraphsDir.getAbsolutePath(), testOutputDir.getAbsolutePath());
		checkCSV(multiExpResult);
	}
	
	/** Verify that robust runner builds the correct spreadsheet. 
	 * @throws InterruptedException */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator4d() throws NumberFormatException, IOException
	{
		multiExp.graphsPerRunner=700;
		multiExp.robustRunExperiment(testGraphsDir.getAbsolutePath(), testOutputDir.getAbsolutePath());
		checkCSV(multiExpResult);
	}
	
	/** Verify that when asked to post-process an empty collection of files, the Grid mode throws an exception. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail1a() throws NumberFormatException
	{
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+ExperimentRunner.argCMD_POSTPROCESS});
			}
		},LearnerFailed.class,""+multiExpResult.length);
	}
	
	/** One file cannot be processed because it is not found. When run via the Grid experiment, the appropriate runner reports a failure. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail2_A() throws NumberFormatException
	{
		String fileToRemove = graphs.entrySet().iterator().next().getKey();
		Assert.assertTrue(new File(testGraphsDir,fileToRemove).delete());
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException, IOException {
				for(int i=0;i < multiExpResult.length;++i)
					multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+i});
			}
		},IOException.class,fileToRemove);
	}

	/** One file cannot be processed because it is not found. When run via the desktop interface, the corresponding file is ignored. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail2_B1() throws NumberFormatException, IOException
	{
		String fileToRemove = graphs.entrySet().iterator().next().getKey();
		Assert.assertTrue(new File(testGraphsDir,fileToRemove).delete());
		multiExp.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});

		List<String> result = new LinkedList<String>();
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
			if (gr.getKey() != fileToRemove)
				for(int stage:new int[]{30,45,90,99})
					result.add(gr.getKey()+ExperimentRunner.FS+"learnerAlphabet"+ExperimentRunner.FS+stage+
						ExperimentRunner.FS+gr.getValue().pathroutines.countEdges()+
						ExperimentRunner.FS+gr.getValue().pathroutines.computeAlphabet().size()*stage);
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
			if (gr.getKey() != fileToRemove)
				for(int stage:new int[]{30,45,90,99})
					result.add(gr.getKey()+ExperimentRunner.FS+"learnerTransitions"+ExperimentRunner.FS+stage+
						ExperimentRunner.FS+gr.getValue().pathroutines.countEdges()*stage+
						ExperimentRunner.FS+WMethod.computeWSet_reducedmemory(gr.getValue()).size());
		checkCSV(result.toArray(new String[]{}));
	}

	/** One file cannot be processed because it is not found. When run via the robust runner, the corresponding file is ignored. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail2_B2() throws NumberFormatException, IOException
	{
		String fileToRemove = graphs.entrySet().iterator().next().getKey();
		Assert.assertTrue(new File(testGraphsDir,fileToRemove).delete());
		multiExp.robustRunExperiment(testGraphsDir.getAbsolutePath(),testOutputDir.getAbsolutePath());

		List<String> result = new LinkedList<String>();
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
			if (gr.getKey() != fileToRemove)
				for(int stage:new int[]{30,45,90,99})
					result.add(gr.getKey()+ExperimentRunner.FS+"learnerAlphabet"+ExperimentRunner.FS+stage+
						ExperimentRunner.FS+gr.getValue().pathroutines.countEdges()+
						ExperimentRunner.FS+gr.getValue().pathroutines.computeAlphabet().size()*stage);
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
			if (gr.getKey() != fileToRemove)
				for(int stage:new int[]{30,45,90,99})
					result.add(gr.getKey()+ExperimentRunner.FS+"learnerTransitions"+ExperimentRunner.FS+stage+
						ExperimentRunner.FS+gr.getValue().pathroutines.countEdges()*stage+
						ExperimentRunner.FS+WMethod.computeWSet_reducedmemory(gr.getValue()).size());
		checkCSV(result.toArray(new String[]{}));
	}


	/** One file cannot be processed because it contains junk. Run via the Grid interface. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail3_A() throws NumberFormatException, IOException
	{
		String fileToBreak = graphs.entrySet().iterator().next().getKey();
		Writer fileWriter = new FileWriter(new File(testGraphsDir,fileToBreak));fileWriter.write("junk");fileWriter.close();
		for(int i=0;i < multiExpResult.length;++i)
			multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+i});
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+ExperimentRunner.argCMD_POSTPROCESS});
			}
		},LearnerFailed.class,""+2*4);
	}

	/** One file cannot be processed because it contains junk. Run via the desktop interface. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail3_B() throws NumberFormatException, IOException
	{
		String fileToBreak = graphs.entrySet().iterator().next().getKey();
		Writer fileWriter = new FileWriter(new File(testGraphsDir,fileToBreak));fileWriter.write("junk");fileWriter.close();
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
			}
		},LearnerFailed.class,""+2*4);
	}

	/** One file cannot be processed because it contains junk. Run via the robust runner interface. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail3_C() throws NumberFormatException, IOException
	{
		String fileToBreak = graphs.entrySet().iterator().next().getKey();
		Writer fileWriter = new FileWriter(new File(testGraphsDir,fileToBreak));fileWriter.write("junk");fileWriter.close();
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException, IOException {
				multiExp.robustRunExperiment(testGraphsDir.getAbsolutePath(),testOutputDir.getAbsolutePath());
			}
		},LearnerFailed.class,""+2*4);
	}

	
	protected ExperimentRunner getNonOverwriteExperiment()
	{
		ExperimentRunner result = new ExperimentRunner();result.graphsPerRunner=4;result.setTimeBetweenHearbeats(heartbeatTestValue);
		result.setLearnerStages(new int[]{30,45,90,99});
		Configuration cnf = Configuration.getDefaultConfiguration().copy();
		cnf.setLearnerOverwriteOutput(false);// make sure that existing files are preserved.
		result.addLearnerEvaluator(new GeneratorConfiguration(cnf,w_evaluator.class, "learnerTransitions"));
		result.addLearnerEvaluator(new GeneratorConfiguration(cnf,a_evaluator.class, "learnerAlphabet"));
		return result;
	}
	
	/** One file cannot be processed because it contains junk, but if it has already been
	 * processed, we shall not notice this. Run via the desktop interface. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_restart1() throws NumberFormatException, IOException
	{
		multiExp.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
		checkCSV(multiExpResult);

		
		final String fileToBreak = graphs.entrySet().iterator().next().getKey();
		Writer fileWriter = new FileWriter(new File(testGraphsDir,fileToBreak));fileWriter.write("junk");fileWriter.close();

		getNonOverwriteExperiment().runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
		checkCSV(multiExpResult);
	}

	/** One file cannot be processed because it contains junk, but if it has already been
	 * processed, we shall not notice this. Run via the Grid interface. */
	@Test
	public final void testAllGraphsMultiStageMultiEvaluator_restart2() throws NumberFormatException, IOException
	{
		for(int i=0;i < multiExpResult.length;++i)
			multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+i});
		multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+ExperimentRunner.argCMD_POSTPROCESS});
		checkCSV(multiExpResult);
		
		final String fileToBreak = graphs.entrySet().iterator().next().getKey();
		Writer fileWriter = new FileWriter(new File(testGraphsDir,fileToBreak));fileWriter.write("junk");fileWriter.close();

		ExperimentRunner nonOvExperiment = getNonOverwriteExperiment();
		for(int i=0;i < multiExpResult.length;++i)
			nonOvExperiment.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+i});
		nonOvExperiment.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+ExperimentRunner.argCMD_POSTPROCESS});
		checkCSV(multiExpResult);
	}
	
	/** One file cannot be processed because it contains junk, but if it has already been
	 * processed, we shall not notice this. Run via the robust runner interface. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_restart3() throws NumberFormatException, IOException
	{
		multiExp.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
		checkCSV(multiExpResult);

		
		final String fileToBreak = graphs.entrySet().iterator().next().getKey();
		Writer fileWriter = new FileWriter(new File(testGraphsDir,fileToBreak));fileWriter.write("junk");fileWriter.close();
		ExperimentRunner runner = getNonOverwriteExperiment();
		runner.zapOutputDir = false;// since we populated output directory, it seems a bad idea to clear it.
		runner.robustRunExperiment(testGraphsDir.getAbsolutePath(),testOutputDir.getAbsolutePath());
		checkCSV(multiExpResult);
	}
 
	/** The machine which indicates to the learner that it should kill the JVM, but before this
	 * it has to write the correct data into the file. Upon a restart, we can expect the file to be processed
	 * successfully. */
	public static final LearnerGraph recoveryGraph = buildLearnerGraph("A-a->B-a->C-a->D-a->E-a->F-b->F","TestAbstractExperiment", Configuration.getDefaultConfiguration(),null);
	
	/** One file contains a machine which indicates to the learner that it should kill the JVM, but before this
	 * it has to write the correct data into the file. Upon a restart, we can expect the file to be processed
	 * successfully. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_recovery1() throws NumberFormatException, IOException
	{
		final String fileToBreak = graphs.entrySet().iterator().next().getKey();
		recoveryGraph.storage.writeGraphML(new File(testGraphsDir,fileToBreak).getAbsolutePath());

		getNonOverwriteExperiment().robustRunExperiment(testGraphsDir.getAbsolutePath(),testOutputDir.getAbsolutePath());
		checkCSV(multiExpResult);
	}

	/** One file contains a machine which indicates to the learner that it should kill the JVM, but before this
	 * it has to write the correct data into the file. Upon a restart, we can expect the file to be processed
	 * successfully. This one checks that if we only have one attempt, processing fails. 
	 */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_recovery2() throws NumberFormatException, IOException
	{
		final String fileToBreak = graphs.entrySet().iterator().next().getKey();
		recoveryGraph.storage.writeGraphML(new File(testGraphsDir,fileToBreak).getAbsolutePath());
		final ExperimentRunner experiment = getNonOverwriteExperiment();experiment.restarts=1;
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException, IOException {
				experiment.robustRunExperiment(testGraphsDir.getAbsolutePath(),testOutputDir.getAbsolutePath());
		}},LearnerFailed.class,""+experiment.graphsPerRunner);// since we modify a file on the first run, the killed jvm corresponds to four files which were supposed to be processed, thus the number is 4.
	}

	/** One file cannot be processed because the learner throws an exception on it. 
	 * This test is run via the Grid interface. 
	 */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail4_A() throws NumberFormatException, IOException
	{
		String fileToBreak = graphs.entrySet().iterator().next().getKey();
		// The following graph has two equivalent states, B and C
		buildLearnerGraph("A-a->B\nA-b->C", "testAllGraphsMultiStageMultiEvaluator_fail4",config,converter).storage.writeGraphML(new File(testGraphsDir,fileToBreak).getAbsolutePath());
		for(int i=0;i < multiExpResult.length;++i)
			multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+i});
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+ExperimentRunner.argCMD_POSTPROCESS});
			}
		},LearnerFailed.class,""+4);
	}
	
	/** One file cannot be processed because the learner throws an exception on it.
	 * This file is run via the desktop interface. 
	 */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail4_B() throws NumberFormatException, IOException
	{
		String fileToBreak = graphs.entrySet().iterator().next().getKey();
		buildLearnerGraph("A-a->B\nA-b->C", "testAllGraphsMultiStageMultiEvaluator_fail4",config,converter).storage.writeGraphML(new File(testGraphsDir,fileToBreak).getAbsolutePath());
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
			}
		},LearnerFailed.class,""+4);
	}
	
	/** One file cannot be processed because the learner throws an exception on it.
	 * This file is run via the robust runner interface. 
	 */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail4_C() throws NumberFormatException, IOException
	{
		String fileToBreak = graphs.entrySet().iterator().next().getKey();
		buildLearnerGraph("A-a->B\nA-b->C", "testAllGraphsMultiStageMultiEvaluator_fail4",config,converter).storage.writeGraphML(new File(testGraphsDir,fileToBreak).getAbsolutePath());
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException, IOException {
				multiExp.robustRunExperiment(testGraphsDir.getAbsolutePath(),testOutputDir.getAbsolutePath());
			}
		},LearnerFailed.class,""+4);
	}
	
	/** Tests that it is possible to process many files by specifying their numbers on a command line. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator5() throws NumberFormatException, IOException
	{
		int num=0;
		for(int i=0;i < multiExpResult.length/graphs.size();++i)
		{
			String cmdLine[] = new String[2+graphs.size()];
			cmdLine[0]=fileList.getAbsolutePath();cmdLine[1]=testOutputDir.getAbsolutePath();
			for(int j=0;j<graphs.size();++j) cmdLine[j+2] = ""+num++;
			multiExp.runExperiment(cmdLine);
		}
		multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+ExperimentRunner.argCMD_POSTPROCESS});
		checkCSV(multiExpResult);
	}

	/** Test that desktop mode throws an exception when asked to run on a non-existing directory. */ 
	@Test
	public final void test_fail1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() throws NumberFormatException, IOException {
			getSingleStageEvaluator().runExperiment(
					new String[]{testGraphsDir.getAbsolutePath()+"non-existing"});
		}}, IllegalArgumentException.class,"invalid directory");		
	}

	/** Test that Grid mode throws an exception when asked to run on a non-existing directory. */
	@Test
	public final void test_fail_grid1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() throws NumberFormatException, IOException {
				getSingleStageEvaluator().runExperiment(
						new String[]{fileList.getAbsolutePath()+"non-existing",testOutputDir.getAbsolutePath()});
		}}, IllegalArgumentException.class,"grid mode");		
	}
	
	/** Test that robust runner mode throws an exception when asked to run on a non-existing directory. */
	@Test
	public final void test_fail_robust()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() throws NumberFormatException, IOException {
				getSingleStageEvaluator().robustRunExperiment(
						fileList.getAbsolutePath()+"non-existing",testOutputDir.getAbsolutePath());
		}}, FileNotFoundException.class,"");		
	}
	
	/** Constructs a list of files containing the provided string.
	 * 
	 * @param junk what to put into a list of files to process in the Grid mode.
	 * @throws IOException if something goes wrong.
	 */
	protected void putIntoFileList(String junk) throws IOException
	{
		FileWriter writer = null;
		try
		{
			writer = new FileWriter(fileList);
			if (junk != null)
				writer.write(junk);
			writer.close();writer = null;
		}
		finally
		{
			if (writer != null) { try { writer.close();writer=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
		}
	}
	
	/** Empty list of files. */
	@Test
	public final void test_grid_zero1() throws IOException
	{
		putIntoFileList(null);
		checkForCorrectException(new whatToRun() {

			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+ExperimentRunner.argCMD_COUNT});
			}
		},IllegalArgumentException.class,"no usable");
	}
	
	/** List of files containing non-existing files - counting gives an error. */
	@Test
	public final void test_grid_zero2() throws IOException
	{
		putIntoFileList("trash");
		checkForCorrectException(new whatToRun() {

			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+ExperimentRunner.argCMD_COUNT});
			}
			
		}, IOException.class,"cannot load file ");		
	}
	
	/** Empty list of files, robust runner. */
	@Test
	public final void test_grid_zero3() throws IOException
	{
		putIntoFileList(null);
		checkForCorrectException(new whatToRun() {

			public @Override void run() throws NumberFormatException, IOException {
				multiExp.robustRunExperiment(fileList.getAbsolutePath(),testOutputDir.getAbsolutePath());
			}
		},IllegalArgumentException.class,"no usable");
	}
	
	/** Grid mode cannot process a non-existing file. */
	@Test
	public final void test_fail_grid2a() throws IOException
	{
		putIntoFileList("junk");
		checkForCorrectException(new whatToRun() {

			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"1"});
			}
			
		}, IOException.class,"junk");		
	}
	
	/** An index provided is over the number of files in a list. */
	@Test
	public final void test_fail_grid3()
	{
		checkForCorrectException(new whatToRun() {

			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"100"});
			}
			
		}, IllegalArgumentException.class,"Array task number ");		
	}
	
	/** An unrecognised command provided to the Grid mode. */ 
	@Test
	public final void test_fail_grid4() throws IOException
	{
		putIntoFileList("junk");
		checkForCorrectException(new whatToRun() {

			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"-10"});
			}
			
		}, IllegalArgumentException.class,"invalid command");		
	}

	/** Cannot parse the command to the Grid mode. List of files contains junk. */
	@Test
	public final void test_fail_grid5() throws IOException
	{
		putIntoFileList("junk");
		checkForCorrectException(new whatToRun() {

			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"junk"});
			}
			
		}, NumberFormatException.class,"");		
	}
	
	
	/** Cannot parse the command to the Grid mode. List of files is reasonable. */
	@Test
	public final void test_fail_grid6()
	{
		checkForCorrectException(new whatToRun() {

			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"junk"});
			}
			
		}, NumberFormatException.class,"");		
	}

	/** The number of the next file to process is invalid. */
	@Test
	public final void test_fail_grid7()
	{
		checkForCorrectException(new whatToRun() {

			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"0","junk"});
			}
			
		}, NumberFormatException.class,"");		
	}
	
	/** An invalid file number. */ 
	@Test
	public final void test_fail_grid8()
	{
		checkForCorrectException(new whatToRun() {

			public @Override void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"0", "-10"});
			}
			
		}, IllegalArgumentException.class,"Array task number ");		
	}

	final protected String FS = ExperimentRunner.FS; 
	protected final String array = 
	 "A"+FS+"10"+FS+"4"+"\n"+
	 "A"+FS+"15"+FS+"6"+"\n"+
	 "B"+FS+"10"+FS+"8"+"\n"+
	 "B"+FS+"15"+FS+"7"+"\n"+
	 "A"+FS+"10"+FS+"3"+"\n"+
	 "A"+FS+"15"+FS+"5"+"\n";
	
	/** Tests the conversion of a result table into R-friendly format. 
	 * @throws IOException 
	 */
	@Test(expected=IllegalArgumentException.class)
	public final void testResultToR_fail1() throws IOException
	{
		
		StringWriter wr = new StringWriter();
		ExperimentRunner.postProcessIntoR(null,-1, true, 2, new BufferedReader(new StringReader(array)), wr);
	}
	
	/** Tests the conversion of a result table into R-friendly format. 
	 * @throws IOException 
	 */
	@Test(expected=IllegalArgumentException.class)
	public final void testResultToR_fail2() throws IOException
	{
		
		StringWriter wr = new StringWriter();
		ExperimentRunner.postProcessIntoR(null,1, true, -2, new BufferedReader(new StringReader(array)), wr);
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testResultToR_fail3() throws IOException
	{
		
		StringWriter wr = new StringWriter();
		ExperimentRunner.postProcessIntoR(null,1, true, 20, new BufferedReader(new StringReader(array)), wr);
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testResultToR_fail4() throws IOException
	{
		
		StringWriter wr = new StringWriter();
		ExperimentRunner.postProcessIntoR(null,10, true, 2, new BufferedReader(new StringReader(array)), wr);
	}

	/** Empty buffer. */
	@Test
	public final void testResultToR_fail_empty()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			StringWriter wr = new StringWriter();
			ExperimentRunner.postProcessIntoR(null,0, true,0, new BufferedReader(new StringReader("")), wr);
		}},IllegalArgumentException.class,"no data to dump");
	}

	/** Tests the conversion of a result table into R-friendly format. Uses integers.
	 * @throws IOException 
	 */
	@Test
	public final void testResultToR1a() throws IOException
	{
		
		StringWriter wr = new StringWriter();
		ExperimentRunner.postProcessIntoR(null,1, true, 2, new BufferedReader(new StringReader(array)), wr);
		Assert.assertEquals(
				"10"+FS+"15"+"\n"+
				 "4"+FS+"6"+"\n"+
				 "8"+FS+"7"+"\n"+
				 "3"+FS+"5"+"\n",
				 wr.toString());
	}
	
	/** Tests the conversion of a result table into R-friendly format. Uses doubles.
	 * @throws IOException 
	 */
	@Test
	public final void testResultToR1b() throws IOException
	{
		
		StringWriter wr = new StringWriter();
		ExperimentRunner.postProcessIntoR(null,1, true, 2, new BufferedReader(new StringReader(
				 "A"+FS+"10.56"+FS+"4"+"\n"+
				 "A"+FS+"15"+FS+"6"+"\n"+
				 "B"+FS+"10.56"+FS+"8"+"\n"+
				 "B"+FS+"15"+FS+"7"+"\n"+
				 "A"+FS+"10.56"+FS+"3"+"\n"+
				 "A"+FS+"15"+FS+"5"+"\n")), wr);
		Assert.assertEquals(
				"10.56"+FS+"15"+"\n"+
				 "4"+FS+"6"+"\n"+
				 "8"+FS+"7"+"\n"+
				 "3"+FS+"5"+"\n",
				 wr.toString());
	}
	
	/** Tests the conversion of a result table into R-friendly format. Uses doubles.
	 * @throws IOException 
	 */
	@Test
	public final void testResultToR1c() throws IOException
	{
		
		StringWriter wr = new StringWriter();
		ExperimentRunner.postProcessIntoR(null,1, true, 2, new BufferedReader(new StringReader(
				 "A"+FS+"10.56"+FS+"4"+"\n"+
				 "A"+FS+"15"+FS+"6.77"+"\n"+
				 "B"+FS+"10.56"+FS+"8"+"\n"+
				 "B"+FS+"15"+FS+"7"+"\n"+
				 "A"+FS+"10.56"+FS+"3"+"\n"+
				 "A"+FS+"15"+FS+"5"+"\n")), wr);
		Assert.assertEquals(
				"10.56"+FS+"15"+"\n"+
				 "4"+FS+"6.77"+"\n"+
				 "8"+FS+"7"+"\n"+
				 "3"+FS+"5"+"\n",
				 wr.toString());
	}
	
	/** Tests the conversion of a result table into R-friendly format. 
	 * No data to process.
	 * @throws IOException 
	 */
	@Test
	public final void testResultToR2_fail()
	{
		final StringWriter wr = new StringWriter();
		checkForCorrectException(new whatToRun() { public @Override void run () throws IOException {
		ExperimentRunner.postProcessIntoR(null,0,true, 2, new BufferedReader(new StringReader(array)), wr);
		}}, NumberFormatException.class,"");
	}
	
	
	/** Tests the conversion of a result table into R-friendly format. 
	 * No data to process due to filter.
	 * @throws IOException 
	 */
	@Test
	public final void testResultToR2_fail_filter()
	{
		final StringWriter wr = new StringWriter();
		checkForCorrectException(new whatToRun() { public @Override void run () throws IOException {
		ExperimentRunner.postProcessIntoR("Q",1,true, 2, new BufferedReader(new StringReader(array)), wr);
		}}, IllegalArgumentException.class,"no data to dump");
	}
	
	/** Tests the conversion of a result table into R-friendly format. 
	 * @throws IOException 
	 */
	@Test
	public final void testResultToR2() throws IOException
	{
		StringWriter wr = new StringWriter();
		ExperimentRunner.postProcessIntoR(null,0,false, 2, new BufferedReader(new StringReader(array)), wr);
		Assert.assertEquals(
				"A"+FS+"B"+"\n"+
				"4"+FS+"8"+"\n"+
				"6"+FS+"7"+"\n"+
				"3"+FS+"\n"+
				"5"+FS+"\n",
				 wr.toString());
	}
	
	/** Tests the conversion of a result table into R-friendly format with filtering on B. 
	 * @throws IOException 
	 */
	@Test
	public final void testResultToR2_filter() throws IOException
	{
		StringWriter wr = new StringWriter();
		ExperimentRunner.postProcessIntoR("10",0,false, 2, new BufferedReader(new StringReader(array)), wr);
		Assert.assertEquals(
				"A"+FS+"B"+"\n"+
				"4"+FS+"8"+"\n"+
				"3"+FS+"\n",
				 wr.toString());
	}
	
	/** Tests the conversion of a result table into R-friendly format. 
	 * @throws IOException 
	 */
	@Test
	public final void testResultToR3() throws IOException
	{
		StringWriter wr = new StringWriter();
		ExperimentRunner.postProcessIntoR(null,0,false, 1, new BufferedReader(new StringReader(array)), wr);
		Assert.assertEquals(
				"A"+FS+"B"+"\n"+
				"10"+FS+"10"+"\n"+
				"15"+FS+"15"+"\n"+
				"10"+FS+"\n"+
				"15"+FS+"\n",
				 wr.toString());
	}
}
