/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
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

package statechum.analysis.learning.rpnicore;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.Helper;
import statechum.Configuration.GDScoreComputationAlgorithmEnum;
import statechum.Configuration.GDScoreComputationEnum;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.GD.ChangesRecorder;

/**
 * @author kirill
 *
 */
@RunWith(Parameterized.class)
public class TestGD_ExistingGraphsND {
	protected java.util.Map<CmpVertex,CmpVertex> newToOrig = null;

	/** Number of threads to use. */
	protected final int threadNumber, pairsToAdd;

	Configuration config = null;

	public static final String testFilePath = "resources/TestGraphs/75-6/";

	@Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		File path = new File(testFilePath);assert path.isDirectory();
		File files [] = path.listFiles(new FilenameFilter()
		{
			@Override 
			public boolean accept(@SuppressWarnings("unused") File dir, String name) 
			{
				return name.startsWith("N_");
		}});
		Arrays.sort(files);

		// N_1320.xml+N_502.xml v.s. N_2070.xml+N_2232.xml takes a long while.
		addFilesToCollection(new File(testFilePath+"N_1320.xml"),new File(testFilePath+"N_502.xml"),
				new File(testFilePath+"N_2070.xml"),new File(testFilePath+"N_2232.xml"),result);
		for(int fileNum = 0;fileNum < files.length;++fileNum)
		{
			File
				fileA1 = files[fileNum], 
				fileA2 = files[(fileNum+1)%files.length],
				fileB1 = files[(fileNum+2)%files.length],
				fileB2 = files[(fileNum+3)%files.length];
			
			addFilesToCollection(fileA1, fileA2, fileB1, fileB2, result);
		}
		return result;
	}

	static void addFilesToCollection(File fileA1, File fileA2, File fileB1, File fileB2, Collection<Object []> result)
	{
		for(int threadNo:new int[]{1,2,4,8})
		{
			boolean fallback = TestGD_ExistingGraphs.detectFallbackToInitialPair(fileA1, fileA2, fileB1, fileB2);
			Assert.assertFalse(fallback);// our test files are very small hence must fit in memory
			for(double ratio:new double[]{0.5,0.68,0.9})
				for(int pairs:new int[]{0,100})
				result.add(new Object[]{new Integer(threadNo), new Integer(pairs), ratio,
						fileA1,fileA2,fileB1,fileB2
					});
			result.add(new Object[]{new Integer(threadNo), new Integer(0),-1.,fileA1,fileA2,fileB1,fileB2});
		}
	}
	
	final File graphA,graphB,graphC,graphD;
	
	/** Positive value is the ratio of low-to-high above which key pairs are considered ok;
	 * negative value means that we set <em>setGdMaxNumberOfStatesInCrossProduct</em>
	 * to a low value and check that the outcome works.
	 */
	double low_to_high_ratio = -1;
	
	/** Creates the test class with the number of threads to create as an argument. */
	public TestGD_ExistingGraphsND(int th, int pairs, double ratio, File fileA, File fileB, File fileC, File fileD)
	{
		threadNumber = th;graphA=fileA;graphB=fileB;graphC=fileC;graphD=fileD;low_to_high_ratio=ratio;pairsToAdd=pairs;
	}
	
	public static String parametersToString(Integer th, Integer pairs, Double ratio, File fileA, File fileB, File fileC, File fileD)
	{
		return "threads: "+th+", extra pairs: "+pairs+" ratio: "+ratio+", "+fileA.getName()+"+"+fileB.getName()+" v.s. "+fileC.getName()+"+"+fileD.getName();
	}
	
	@Before
	public final void beforeTest()
	{
		newToOrig = new java.util.TreeMap<CmpVertex,CmpVertex>();config=TestGD_ExistingGraphs.computeConfig(low_to_high_ratio);
	}
	
	protected String testDetails()
	{
		return graphA+"+"+graphB+"-"+graphC+"+"+graphD+" ["+threadNumber+" threads] ";
	}
	
	/** This one assembles a non-deterministic version of the graphs and checks that things still work. */
	public final void runNDPatch(File fileA1, File fileA2, File fileB1, File fileB2)
	{
		try
		{
			LearnerGraphND grA = null, grB = null, graph = null;
			{
				LearnerGraphND loadedA1 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileA1, loadedA1);
				LearnerGraph loadedA2 = new LearnerGraph(config);AbstractPersistence.loadGraph(fileA2, loadedA2);
				grA = LearnerGraphND.UniteTransitionMatrices(loadedA1,loadedA2);TestGD_ExistingGraphs.addColourAndTransitionsRandomly(grA, new Random(0));
			}
			
			{
				LearnerGraphND loadedB1 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileB1, loadedB1);
				LearnerGraph loadedB2 = new LearnerGraph(config);AbstractPersistence.loadGraph(fileB2, loadedB2);
				grB = LearnerGraphND.UniteTransitionMatrices(loadedB1,loadedB2);TestGD_ExistingGraphs.addColourAndTransitionsRandomly(grB, new Random(1));
			}
			
			GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
			{
				LearnerGraphND loadedA1 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileA1, loadedA1);
				LearnerGraph loadedA2 = new LearnerGraph(config);AbstractPersistence.loadGraph(fileA2, loadedA2);
				graph = LearnerGraphND.UniteTransitionMatrices(loadedA1,loadedA2);TestGD_ExistingGraphs.addColourAndTransitionsRandomly(graph, new Random(0));
			}
			ChangesRecorder patcher = new ChangesRecorder(null);
			//gd.computeGD(grA, grB, threadNumber, patcher,config);
			gd.init(grA, grB, threadNumber,config);
			gd.identifyKeyPairs();
			if (!gd.fallbackToInitialPair) TestGD_ExistingGraphs.addPairsRandomly(gd,pairsToAdd);
			else Assert.assertEquals(-1.,low_to_high_ratio,Configuration.fpAccuracy);
			gd.makeSteps();
			gd.computeDifference(patcher);

			LearnerGraphND outcome = new LearnerGraphND(config);
			ChangesRecorder.applyGD_WithRelabelling(graph, patcher.writeGD(TestGD.createDoc()),outcome);
			Assert.assertNull(testDetails(),WMethod.checkM(grB,graph));
			Assert.assertEquals(testDetails(),grB.getStateNumber(),graph.getStateNumber());
			Assert.assertEquals(grB,outcome);
			
			// Cannot do checkM_and_colours here because merged vertices may change their colours and other attributes.
			//DifferentFSMException ex= WMethod.checkM_and_colours(grB,outcome,VERTEX_COMPARISON_KIND.DEEP);Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
			//Assert.assertNull(testDetails()+ex,ex);
		}
		catch(IOException ex)
		{
			Helper.throwUnchecked("failed to load a file", ex);
		}
	}

	@Test
	public final void testGD_AB_linearRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_LINEAR);
		runNDPatch(graphA, graphB, graphC, graphD);
	}
	
	@Test
	public final void testGD_BA_linearRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_LINEAR);
		runNDPatch(graphC, graphD, graphA, graphB);
	}
	
	@Test
	public final void testGD_AB_walkRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(100);
		runNDPatch(graphA, graphB, graphC, graphD);
	}
	
	@Test
	public final void testGD_BA_walkRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(100);
		runNDPatch(graphC, graphD, graphA, graphB);
	}
	
	@Test
	public final void testGD_AB_walk()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(100);
		runNDPatch(graphA, graphB, graphC, graphD);
	}
	
	@Test
	public final void testGD_BA_walk()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(100);
		runNDPatch(graphC, graphD, graphA, graphB);
	}
	
	@Test
	public final void testGD_AB_testsetRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		runNDPatch(graphA, graphB, graphC, graphD);
	}
	
	@Test
	public final void testGD_BA_testsetRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		runNDPatch(graphC, graphD, graphA, graphB);
	}
	
	@Test
	public final void testGD_AB_testset()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		runNDPatch(graphA, graphB, graphC, graphD);
	}
	
	@Test
	public final void testGD_BA_testset()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		runNDPatch(graphC, graphD, graphA, graphB);
	}

	
	public static void main(String aa[])
	{
		TestGD_ExistingGraphsND tester = new TestGD_ExistingGraphsND(2, 0, 0.68, new File(testFilePath+"N_1320.xml"),new File(testFilePath+"N_502.xml"),
				new File(testFilePath+"N_2070.xml"),new File(testFilePath+"N_2232.xml"));
		tester.beforeTest();
		tester.config.setEquivalentStatesAllowedForW(true);
		tester.testGD_AB_testset();
	}
}
