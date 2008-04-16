/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 
package statechum.analysis.learning.experiments;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.experiments.AbstractExperiment.LearnerEvaluator;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.WMethod;

public class TestAbstractExperiment {
	public static final File testDir = new File("resources","__TestAbstractExperiment__"),
		testGraphsDir = new File(testDir,"__graphs"), testOutputDir = new File(testDir,AbstractExperiment.outputDirName+testGraphsDir.getName());

	protected final Configuration config = Configuration.getDefaultConfiguration();
	
	protected final DirectedSparseGraph JungGraphs[] = new DirectedSparseGraph[]{
			TestFSMAlgo.buildGraph("A-a->A-b-#B", "testAbstractExperiment_graph1.xml"),
			TestFSMAlgo.buildGraph("A-a->A-b->B-b->C-a->A", "testAbstractExperiment_graph2.xml"),
			TestFSMAlgo.buildGraph("A-a->A-b->B-b->C-a->A-c->A", "testAbstractExperiment_graph3.xml"),
			TestFSMAlgo.buildGraph("A-a->A-b->B-b->C-a->A\nC-b->C", "testAbstractExperiment_graph4.xml"),
			TestFSMAlgo.buildGraph("A-a->A-b->B-c->B", "testAbstractExperiment_graph5.xml")
	};
	protected Map<String,LearnerGraph> graphs = new TreeMap<String,LearnerGraph>();

	/** Removes the directory and all its files. If the directory contains 
	 * other directories, aborts, but not before deleting some files. 
	 */
	protected void zapDir(File directory)
	{
		if (directory.isDirectory())
		{
			for(File f:directory.listFiles())
			{
				Assert.assertFalse(f.isDirectory());
				Assert.assertTrue("cannot delete file "+f,f.delete());
			}
			directory.delete();
		}
	}
	
	/** Clears the test space. */
	protected void deleteTestDirectories()
	{
		zapDir(testGraphsDir);
		zapDir(testOutputDir);
		zapDir(testDir);
		Assert.assertFalse(testDir.isDirectory());Assert.assertFalse(testGraphsDir.isDirectory());Assert.assertFalse(testOutputDir.isDirectory());
	}
	
	protected final File fileList = new File(testDir,"fileList");
	
	protected void populateGraphs()
	{
		try
		{
			LearnerGraph emptyGraph = new LearnerGraph(config);String emptyGraphName = "testAbstractExperiment_graph0.xml"; // an empty graph 
			graphs.put(emptyGraphName,emptyGraph);emptyGraph.transform.writeGraphML(new File(testGraphsDir,emptyGraphName).getAbsolutePath());
			Writer fileListWriter = new FileWriter(fileList);fileListWriter.write(new File(testGraphsDir,emptyGraphName).getAbsolutePath()+"\n");
			for(DirectedSparseGraph gr:JungGraphs)
			{
				LearnerGraph graph = new LearnerGraph(gr,config);String graphName = (String)gr.getUserDatum(JUConstants.TITLE);
				graphs.put(graphName,graph);graph.transform.writeGraphML(new File(testGraphsDir,graphName).getAbsolutePath());
				fileListWriter.write(new File(testGraphsDir,graphName).getAbsolutePath()+"\n");
			}
			fileListWriter.close();
		}
		catch(IOException ex)
		{
			Assert.fail("failed to write graphs");
		}
	}
	
	@Before
	public final void beforeTest()
	{
		// clean the directories
		testDir.deleteOnExit();deleteTestDirectories();
		
		Assert.assertFalse(testDir.isDirectory());Assert.assertFalse(testGraphsDir.isDirectory());
		Assert.assertTrue(testDir.mkdir());Assert.assertTrue(testGraphsDir.mkdir());Assert.assertTrue(testOutputDir.mkdir());
		populateGraphs();

		multiExp = new AbstractExperiment() {
			@Override
			public List<LearnerEvaluatorGenerator> getLearnerGenerators() {
				return Arrays.asList(new LearnerEvaluatorGenerator[]{new LearnerEvaluatorGenerator() {
					@Override
					LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, AbstractExperiment exp) {
						return new TestEvaluator(inputFile,percent,instanceID,exp) {
							@Override
							protected void runTheExperiment() {
								result = result + graph.countEdges()*percent + FS + WMethod.computeWSet_reducedmemory(graph).size();
							}
							@Override
							public String getLearnerName() { return "learnerTransitions";	}
						};
					}
					
				},new LearnerEvaluatorGenerator() {
					@Override
					LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, AbstractExperiment exp) {
						return new TestEvaluator(inputFile,percent,instanceID,exp) {
							@Override
							protected void runTheExperiment() {
								result = result + graph.countEdges() + FS + graph.wmethod.computeAlphabet().size()*percent;
							}
							@Override
							public String getLearnerName() { return "learnerAlphabet";	}
						};
					}
					
				}});
			}

			@Override
			public int [] getStages() {
				return new int[]{30,45,90,99};
			}
			
		};
		List<String> result = new LinkedList<String>();
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
			for(int stage:new int[]{30,45,90,99})
				result.add(gr.getKey()+AbstractExperiment.FS+"learnerAlphabet"+AbstractExperiment.FS+stage+
						AbstractExperiment.FS+gr.getValue().countEdges()+
						AbstractExperiment.FS+gr.getValue().wmethod.computeAlphabet().size()*stage);
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
			for(int stage:new int[]{30,45,90,99})
				result.add(gr.getKey()+AbstractExperiment.FS+"learnerTransitions"+AbstractExperiment.FS+stage+
						AbstractExperiment.FS+gr.getValue().countEdges()*stage+
						AbstractExperiment.FS+WMethod.computeWSet_reducedmemory(gr.getValue()).size());
		multiExpResult = result.toArray(new String[]{});
	}

	protected String [] multiExpResult = null;
	protected AbstractExperiment multiExp = null;

	/** This one is not static because it refers to the frame to display results. */
	public static abstract class TestEvaluator extends LearnerEvaluator
	{
		public TestEvaluator(String inputFile, int per, int instance, AbstractExperiment exp)
		{
			super(inputFile, per,instance, exp);			
		}

		@Override
		protected void changeParameters(Configuration c) {
			// not used
		}
	}

	/** Checks that the csv file contains the expected data. */
	protected final void checkCSV(String []expectedData)
	{
		BufferedReader reader = null;
		try
		{
			reader = new BufferedReader(new FileReader(new File(testOutputDir,AbstractExperiment.resultName)));
			int pos=0;String line = reader.readLine();
			while(pos < expectedData.length)
			{
				Assert.assertEquals(expectedData[pos], line);
				++pos;line=reader.readLine();
			}
			Assert.assertNull(line);
			reader.close();
		}
		catch(IOException ex)
		{
			Assert.fail(ex.getMessage());
		}
		finally
		{
			if (reader != null)
				try {
					reader.close();
				} catch (IOException e) {
					// ignore
				}
		}
	}

	public interface whatToRun
	{
		public void run() throws NumberFormatException, IOException;
	}
	
	public final void checkForCorrectException(whatToRun what, Class exceptionClass, String exceptionString)
	{
		try
		{
			what.run();
			Assert.fail("Exception not thrown");
		}
		catch(Exception ex)
		{
			StringWriter str = new StringWriter();ex.printStackTrace(new PrintWriter(str));
			Assert.assertEquals("wrong exception received "+str.toString(),exceptionClass,ex.getClass());
			Assert.assertTrue(ex.getMessage().contains(exceptionString));
		}
	}

	@Test 
	public final void testAllGraphsSingleStageSingleEvaluator() throws NumberFormatException, IOException
	{
		new AbstractExperiment() {
			@Override
			public List<LearnerEvaluatorGenerator> getLearnerGenerators() {
				return Arrays.asList(new LearnerEvaluatorGenerator[]{new LearnerEvaluatorGenerator() {
					@Override
					LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, AbstractExperiment exp) {
						return new TestEvaluator(inputFile,percent,instanceID,exp) {
							@Override
							protected void runTheExperiment() {
								result = result + graph.countEdges();
							}
							@Override
							public String getLearnerName() { return "testAllGraphsSingleStage";	}
						};
					}
					
				}});
			}

			@Override
			public int [] getStages() {	return null; }
			
		}.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
		List<String> result = new LinkedList<String>();
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
				result.add(gr.getKey()+AbstractExperiment.FS+"testAllGraphsSingleStage"+AbstractExperiment.FS+gr.getValue().countEdges());
		checkCSV(result.toArray(new String[]{}));
	}
	
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator() throws NumberFormatException, IOException
	{
		multiExp.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
		checkCSV(multiExpResult);
	}
	
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator1() throws NumberFormatException, IOException
	{
		Assert.assertEquals(multiExpResult.length,multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"-1"}));
	}
	
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator2() throws NumberFormatException, IOException
	{
		for(int i=0;i < multiExpResult.length;++i)
			multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+i});
		multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"-2"});
		checkCSV(multiExpResult);
	}

	/** No files could be processed. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail1() throws NumberFormatException, IOException
	{
		checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"-2"});
			}
		},IOException.class,""+multiExpResult.length);
	}
	
	/** One file cannot be processed because it is not found. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail2_A() throws NumberFormatException, IOException
	{
		String fileToRemove = graphs.entrySet().iterator().next().getKey();
		Assert.assertTrue(new File(testGraphsDir,fileToRemove).delete());
		checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				for(int i=0;i < multiExpResult.length;++i)
					multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+i});
			}
		},IOException.class,fileToRemove);
	}
	
	/** One file cannot be processed because it is not found. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail2_B() throws NumberFormatException, IOException
	{
		String fileToRemove = graphs.entrySet().iterator().next().getKey();
		Assert.assertTrue(new File(testGraphsDir,fileToRemove).delete());
		multiExp.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});

		List<String> result = new LinkedList<String>();
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
			if (gr.getKey() != fileToRemove)
				for(int stage:new int[]{30,45,90,99})
					result.add(gr.getKey()+AbstractExperiment.FS+"learnerAlphabet"+AbstractExperiment.FS+stage+
						AbstractExperiment.FS+gr.getValue().countEdges()+
						AbstractExperiment.FS+gr.getValue().wmethod.computeAlphabet().size()*stage);
		for(Entry<String,LearnerGraph> gr:graphs.entrySet())
			if (gr.getKey() != fileToRemove)
				for(int stage:new int[]{30,45,90,99})
					result.add(gr.getKey()+AbstractExperiment.FS+"learnerTransitions"+AbstractExperiment.FS+stage+
						AbstractExperiment.FS+gr.getValue().countEdges()*stage+
						AbstractExperiment.FS+WMethod.computeWSet_reducedmemory(gr.getValue()).size());
		checkCSV(result.toArray(new String[]{}));
	}


	/** One file cannot be processed because it contains junk. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail3_A() throws NumberFormatException, IOException
	{
		String fileToBreak = graphs.entrySet().iterator().next().getKey();
		Writer fileWriter = new FileWriter(new File(testGraphsDir,fileToBreak));fileWriter.write("junk");fileWriter.close();
		for(int i=0;i < multiExpResult.length;++i)
			multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+i});
		checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"-2"});
			}
		},IOException.class,""+2*4);
	}

	/** One file cannot be processed because it contains junk. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail3_B() throws NumberFormatException, IOException
	{
		String fileToBreak = graphs.entrySet().iterator().next().getKey();
		Writer fileWriter = new FileWriter(new File(testGraphsDir,fileToBreak));fileWriter.write("junk");fileWriter.close();
		checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
			}
		},IOException.class,""+2*4);
	}

	/** One file cannot be processed because the learner throws an exception on it. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail4_A() throws NumberFormatException, IOException
	{
		String fileToBreak = graphs.entrySet().iterator().next().getKey();
		new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B\nA-b->C", "testAllGraphsMultiStageMultiEvaluator_fail4"),config).transform.writeGraphML(new File(testGraphsDir,fileToBreak).getAbsolutePath());
		for(int i=0;i < multiExpResult.length;++i)
			multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),""+i});
		checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"-2"});
			}
		},IOException.class,""+4);
	}
	
	/** One file cannot be processed because the learner throws an exception on it. */
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator_fail4_B() throws NumberFormatException, IOException
	{
		String fileToBreak = graphs.entrySet().iterator().next().getKey();
		new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B\nA-b->C", "testAllGraphsMultiStageMultiEvaluator_fail4"),config).transform.writeGraphML(new File(testGraphsDir,fileToBreak).getAbsolutePath());
		checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{testGraphsDir.getAbsolutePath()});
			}
		},IOException.class,""+4);
	}
	
	@Test 
	public final void testAllGraphsMultiStageMultiEvaluator3() throws NumberFormatException, IOException
	{
		int num=0;
		for(int i=0;i < multiExpResult.length/graphs.size();++i)
		{
			String cmdLine[] = new String[2+graphs.size()];
			cmdLine[0]=fileList.getAbsolutePath();cmdLine[1]=testOutputDir.getAbsolutePath();
			for(int j=0;j<graphs.size();++j) cmdLine[j+2] = ""+num++;
			multiExp.runExperiment(cmdLine);
		}
		multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"-2"});
		checkCSV(multiExpResult);
	}

	@Test
	public final void test_fail1()
	{
		checkForCorrectException(new whatToRun() {

			public void run() throws NumberFormatException, IOException {
				new AbstractExperiment() {
					@Override
					public List<LearnerEvaluatorGenerator> getLearnerGenerators() {
						return Arrays.asList(new LearnerEvaluatorGenerator[]{new LearnerEvaluatorGenerator() {
							@Override
							LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, AbstractExperiment exp) {
								return new TestEvaluator(inputFile,percent,instanceID,exp) {
									@Override
									protected void runTheExperiment() {
										result = result + graph.countEdges();
									}
									@Override
									public String getLearnerName() { return "testAllGraphsSingleStage";	}							
								};
							}
						}});
					}
			
					@Override
					public int [] getStages() {	return null;}
					
				}.runExperiment(new String[]{testGraphsDir.getAbsolutePath()+"non-existing"});
			}
			
		}, IllegalArgumentException.class,"invalid directory");		
	}

	@Test
	public final void test_fail_grid1()
	{
		checkForCorrectException(new whatToRun() {

			public void run() throws NumberFormatException, IOException {
				new AbstractExperiment() {
					
					@Override
					public List<LearnerEvaluatorGenerator> getLearnerGenerators() {
						return Arrays.asList(new LearnerEvaluatorGenerator[]{new LearnerEvaluatorGenerator() {
							@Override
							LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, AbstractExperiment exp) {
								return new TestEvaluator(inputFile,percent,instanceID,exp) {
									@Override
									protected void runTheExperiment() {
										result = result + graph.countEdges();
									}
									@Override
									public String getLearnerName() { return "testAllGraphsSingleStage";	}							
								};
							}
						}});
					}
			
					@Override
					public int [] getStages() {	return null;}
					
				}.runExperiment(new String[]{fileList.getAbsolutePath()+"non-existing",testOutputDir.getAbsolutePath()});
			}
			
		}, IllegalArgumentException.class,"grid mode");		
	}
	
	protected void putIntoFileList(String junk) throws IOException
	{
		FileWriter writer = null;
		try
		{
			writer = new FileWriter(fileList);
			if (junk != null)
				writer.write(junk);
		}
		finally
		{
			if(writer != null)
				try {
					writer.close();
				} catch (IOException e) {
					// ignored.
				}
		}
	}
	
	@Test
	public final void test_grid_zero1() throws IOException
	{
		putIntoFileList(null);
		checkForCorrectException(new whatToRun() {

			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"-1"});
			}
		},IllegalArgumentException.class,"no usable");
	}
	
	@Test
	public final void test_grid_zero2() throws IOException
	{
		putIntoFileList("trash");
		checkForCorrectException(new whatToRun() {

			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"-1"});
			}
			
		}, IOException.class,"trash");		
	}
	
	@Test
	public final void test_fail_grid2() throws IOException
	{
		putIntoFileList("junk");
		checkForCorrectException(new whatToRun() {

			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"1"});
			}
			
		}, IOException.class,"junk");		
	}
	
	@Test
	public final void test_fail_grid3()
	{
		checkForCorrectException(new whatToRun() {

			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"100"});
			}
			
		}, IllegalArgumentException.class,"Array");		
	}
	
	@Test
	public final void test_fail_grid4() throws IOException
	{
		putIntoFileList("junk");
		checkForCorrectException(new whatToRun() {

			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"-10"});
			}
			
		}, IllegalArgumentException.class,"invalid command");		
	}

	@Test
	public final void test_fail_grid5() throws IOException
	{
		putIntoFileList("junk");
		checkForCorrectException(new whatToRun() {

			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"junk"});
			}
			
		}, NumberFormatException.class,"");		
	}
	
	@Test
	public final void test_fail_grid6()
	{
		checkForCorrectException(new whatToRun() {

			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"junk"});
			}
			
		}, NumberFormatException.class,"");		
	}
	
	@Test
	public final void test_fail_grid7()
	{
		checkForCorrectException(new whatToRun() {

			public void run() throws NumberFormatException, IOException {
				multiExp.runExperiment(new String[]{fileList.getAbsolutePath(),testOutputDir.getAbsolutePath(),"0","junk"});
			}
			
		}, NumberFormatException.class,"");		
	}
}
