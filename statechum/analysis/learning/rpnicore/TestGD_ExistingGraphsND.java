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
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.Helper;
import statechum.JUConstants;
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

	@Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		final String testFilePath = "resources/TestGraphs/75-6/";
		File path = new File(testFilePath);assert path.isDirectory();
		File files [] = path.listFiles(new FilenameFilter()
		{
			@Override 
			public boolean accept(@SuppressWarnings("unused") File dir, String name) 
			{
				return name.startsWith("N_");
			}});
		
		for(int fileNum = 0;fileNum < files.length;++fileNum)
			for(int threadNo=1;threadNo<8;++threadNo)
				for(int pairs:new int[]{0,10,100})
					for(double ratio:new double[]{0.5,0.68,0.9,-1})
						result.add(new Object[]{new Integer(threadNo), new Integer(pairs), ratio,
							files[fileNum].getAbsolutePath(), 
							files[(fileNum+1)%files.length].getAbsolutePath(),
							files[(fileNum+2)%files.length].getAbsolutePath(),
							files[(fileNum+3)%files.length].getAbsolutePath()
							});
		
		return result;
	}

	final String fileNameA,fileNameB,fileNameC,fileNameD;
	
	/** Positive value is the ratio of low-to-high above which key pairs are considered ok;
	 * negative value means that we set <em>setGdMaxNumberOfStatesInCrossProduct</em>
	 * to a low value and check that the outcome works.
	 */
	double low_to_high_ratio = -1;
	
	/** Creates the test class with the number of threads to create as an argument. */
	public TestGD_ExistingGraphsND(int th, int pairs, double ratio, String fileA, String fileB, String fileC, String fileD)
	{
		threadNumber = th;fileNameA=fileA;fileNameB=fileB;fileNameC=fileC;fileNameD=fileD;low_to_high_ratio=ratio;pairsToAdd=pairs;
	}
	
	public static String parametersToString(Integer th, Integer pairs, Double ratio, String fileA, String fileB, String fileC, String fileD)
	{
		return "threads: "+th+", extra pairs: "+pairs+" ratio: "+ratio+", "+fileA+"+"+fileB+" v.s. "+fileC+"+"+fileD;
	}
	
	@Before
	public final void beforeTest()
	{
		newToOrig = new java.util.TreeMap<CmpVertex,CmpVertex>();
		config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		config.setGdKeyPairThreshold(.75);
		if (low_to_high_ratio > 0)
			config.setGdLowToHighRatio(low_to_high_ratio);
		else
		{
			config.setGdLowToHighRatio(0.5);
			config.setGdMaxNumberOfStatesInCrossProduct(10);
		}
		config.setAttenuationK(0.95);
	}
	
	protected String testDetails()
	{
		return fileNameA+"+"+fileNameB+"-"+fileNameC+"+"+fileNameD+" ["+threadNumber+" threads] ";
	}
	
	private void addColourAndTransitionsRandomly(LearnerGraphND gr,Random amberRnd)
	{
		for(int i=0;i<gr.getStateNumber()/3;++i)
		{
			gr.pathroutines.pickRandomState(amberRnd).setColour(JUConstants.AMBER);
			gr.pathroutines.pickRandomState(amberRnd).setColour(JUConstants.BLUE);
		}
		
		for(int i=0;i<gr.getStateNumber()/3;++i)
		{
			CmpVertex a = gr.pathroutines.pickRandomState(amberRnd), b = gr.pathroutines.pickRandomState(amberRnd);
			Map<String,List<CmpVertex>> targets = gr.transitionMatrix.get(a);
			if (targets != null)
			{
				Entry<String,List<CmpVertex>> entry=targets.entrySet().iterator().next();
				if (!gr.getTargets(entry.getValue()).contains(b)) gr.getTargets(entry.getValue()).add(b);
			}
		}
	}
	
	/** This one assembles a non-deterministic version of the graphs and checks that things still work. */
	public final void runNDPatch(String fileA1, String fileA2, String fileB1, String fileB2)
	{
		try
		{
			LearnerGraphND grA = null, grB = null, graph = null;
			{
				LearnerGraphND loadedA1 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileA1, loadedA1);
				LearnerGraph loadedA2 = new LearnerGraph(config);AbstractPersistence.loadGraph(fileA2, loadedA2);
				grA = LearnerGraphND.UniteTransitionMatrices(loadedA1,loadedA2);addColourAndTransitionsRandomly(grA, new Random(0));
			}
			
			{
				LearnerGraphND loadedB1 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileB1, loadedB1);
				LearnerGraph loadedB2 = new LearnerGraph(config);AbstractPersistence.loadGraph(fileB2, loadedB2);
				grB = LearnerGraphND.UniteTransitionMatrices(loadedB1,loadedB2);addColourAndTransitionsRandomly(grB, new Random(1));
			}
			
			GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
			{
				LearnerGraphND loadedA1 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileA1, loadedA1);
				LearnerGraph loadedA2 = new LearnerGraph(config);AbstractPersistence.loadGraph(fileA2, loadedA2);
				graph = LearnerGraphND.UniteTransitionMatrices(loadedA1,loadedA2);addColourAndTransitionsRandomly(graph, new Random(0));
			}
			ChangesRecorder patcher = new ChangesRecorder(null);
			//gd.computeGD(grA, grB, threadNumber, patcher,config);
			gd.init(grA, grB, threadNumber,config);
			gd.identifyKeyPairs();
			TestGD_ExistingGraphs.addPairsRandomly(gd,pairsToAdd);
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
		runNDPatch(fileNameA, fileNameB, fileNameC, fileNameD);
	}
	
	@Test
	public final void testGD_BA_linearRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_LINEAR);
		runNDPatch(fileNameC, fileNameD, fileNameA, fileNameB);
	}
	
	@Test
	public final void testGD_AB_walkRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(100);
		runNDPatch(fileNameA, fileNameB, fileNameC, fileNameD);
	}
	
	@Test
	public final void testGD_BA_walkRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(100);
		runNDPatch(fileNameC, fileNameD, fileNameA, fileNameB);
	}
	
	@Test
	public final void testGD_AB_walk()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(100);
		runNDPatch(fileNameA, fileNameB, fileNameC, fileNameD);
	}
	
	@Test
	public final void testGD_BA_walk()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(100);
		runNDPatch(fileNameC, fileNameD, fileNameA, fileNameB);
	}
	
	@Test
	public final void testGD_AB_testsetRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		runNDPatch(fileNameA, fileNameB, fileNameC, fileNameD);
	}
	
	@Test
	public final void testGD_BA_testsetRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		runNDPatch(fileNameC, fileNameD, fileNameA, fileNameB);
	}
	
	@Test
	public final void testGD_AB_testset()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		runNDPatch(fileNameA, fileNameB, fileNameC, fileNameD);
	}
	
	@Test
	public final void testGD_BA_testset()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		runNDPatch(fileNameC, fileNameD, fileNameA, fileNameB);
	}

}
