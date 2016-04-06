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

package statechum.analysis.learning.linear;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Random;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.ParameterizedWithName;
import org.junit.runners.ParameterizedWithName.ParametersToString;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Configuration.GDScoreComputationAlgorithmEnum;
import statechum.Configuration.GDScoreComputationEnum;
import statechum.Helper;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.ProgressIndicator;
import statechum.analysis.learning.linear.GD.ChangesRecorder;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;

/**
 * @author kirill
 *
 */
@RunWith(ParameterizedWithName.class)
public class TestGD_ExistingGraphsUsingTestSet {
	protected java.util.Map<CmpVertex,CmpVertex> newToOrig = null;

	/** Number of threads to use. */
	protected final int threadNumber, pairsToAdd;

	Configuration config = null;

	/** Label converter to use. */
	private ConvertALabel converter = null;

	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.ASSERT_ENABLED);// this dummy forces the load of configuration if not already loaded, hence progress indicator does not interleave with "configuration loaded" messages.
		Collection<Object []> result = new LinkedList<Object []>();
		final String testFilePath = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.RESOURCES)+File.separator+"TestGraphs/75-6/";
		File path = new File(testFilePath);assert path.isDirectory();
		File files [] = path.listFiles(new FilenameFilter()
		{
			@Override 
			public boolean accept(@SuppressWarnings("unused") File dir, String name) 
			{
				return name.startsWith("N_");
			}
		});
		Arrays.sort(files);
		int threads[] = new int[]{1,8};
		ProgressIndicator progress = new ProgressIndicator("eT:", files.length*threads.length);

		for(int fileNum = 0;fileNum < files.length;++fileNum)
		{
			File 
			fileA=files[fileNum], 
			fileB=files[(fileNum+1)%files.length];
			boolean fallback = TestGD_ExistingGraphs.detectFallbackToInitialPair(fileA, null, fileB, null);
			Assert.assertFalse(fallback);// our test files are very small hence must fit in memory

			for(int threadNo:threads)
			{
				for(double ratio:new double[]{0.3,0.9})
					for(int pairs:new int[]{0})
						result.add(new Object[]{new Integer(threadNo), new Integer(pairs),ratio,fileA,fileB});

				// -1. should be floating-point number otherwise it is turned into Integer and our parametersToString fails to match the resulting list of values.
				result.add(new Object[]{new Integer(threadNo), new Integer(0),-1.,fileA,fileB});
				progress.next();
			}
		}
		return result;
	}

	final File graphA,graphB;
	
	/** Positive value is the ratio of low-to-high above which key pairs are considered ok;
	 * negative value means that we set <em>setGdMaxNumberOfStatesInCrossProduct</em>
	 * to a low value and check that the outcome works.
	 */
	double low_to_high_ratio = -1;
	
	/** Creates the test class with the number of threads to create as an argument. */
	public TestGD_ExistingGraphsUsingTestSet(int th, int pairs,double ratio, File fileA, File fileB)
	{
		threadNumber = th;graphA=fileA;graphB=fileB;low_to_high_ratio=ratio;pairsToAdd=pairs;
	}
	
	@ParametersToString
	public static String parametersToString(Integer th, Integer pairs, Double ratio, File fileA, File fileB)
	{
		return "threads: "+th+", extra pairs: "+pairs+", ratio: "+ratio+", "+fileA.getName()+" v.s. "+fileB.getName();
	}
	
	/** Returns test configuration corresponding to the supplied arguments. 
	 * @param ratio the low to high ratio. The value of zero is special - it forces the out-of-memory fallback operation. 
	 */
	static Configuration computeConfig(double ratio)
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		config.setGdKeyPairThreshold(.75);
		if (ratio > 0)
			config.setGdLowToHighRatio(ratio);
		else
		{
			config.setGdLowToHighRatio(0.5);
			config.setGdMaxNumberOfStatesInCrossProduct(10);
		}
		config.setAttenuationK(0.95);config.setEquivalentStatesAllowedForW(true);
		return config;
	}
	
	@Before
	public final void beforeTest()
	{
		newToOrig = new java.util.TreeMap<CmpVertex,CmpVertex>();config=computeConfig(low_to_high_ratio);
	}
	
	protected String testDetails()
	{
		return graphA+"-"+graphB+" ["+threadNumber+" threads] ";
	}
	

	public final void runPatch(File fileA, File fileB)
	{
		try
		{
			LearnerGraph grA = new LearnerGraph(config);AbstractPersistence.loadGraph(fileA, grA, converter);TestGD_ExistingGraphs.addColourAndIncompatiblesRandomly(grA, new Random(0));
			LearnerGraph grB = new LearnerGraph(config);AbstractPersistence.loadGraph(fileB, grB, converter);TestGD_ExistingGraphs.addColourAndIncompatiblesRandomly(grB, new Random(1));
			GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
			LearnerGraph graph = new LearnerGraph(config);AbstractPersistence.loadGraph(fileA, graph, converter);TestGD_ExistingGraphs.addColourAndIncompatiblesRandomly(graph, new Random(0));
			LearnerGraph outcome = new LearnerGraph(config);
			ChangesRecorder patcher = new ChangesRecorder(null);
			//gd.computeGD(grA, grB, threadNumber, patcher,config);
			
			gd.init(grA, grB, threadNumber,config);
			gd.identifyKeyPairs();
			if (!gd.fallbackToInitialPair) TestGD_ExistingGraphs.addPairsRandomly(gd,pairsToAdd);
			else Assert.assertEquals(-1.,low_to_high_ratio,Configuration.fpAccuracy);
			gd.makeSteps();
			gd.computeDifference(patcher);

			ChangesRecorder.applyGD_WithRelabelling(graph, patcher.writeGD(TestGD.createDoc()), converter,outcome);
			Assert.assertNull(testDetails(),WMethod.checkM(grB,graph));
			Assert.assertEquals(testDetails(),grB.getStateNumber(),graph.getStateNumber());
			DifferentFSMException ex= WMethod.checkM_and_colours(grB,outcome,VERTEX_COMPARISON_KIND.DEEP);
			Assert.assertNull(testDetails()+ex,WMethod.checkM_and_colours(grB,outcome,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
			Assert.assertEquals(grB,outcome);
		}
		catch(IOException ex)
		{
			Helper.throwUnchecked("failed to load a file", ex);
		}
	}

	@Test
	public final void testGD_AB_testsetRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		runPatch(graphA, graphB);
	}
	
	@Test
	public final void testGD_AB_testset()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		runPatch(graphA, graphB);
	}
		
}
