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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.ParameterizedWithName;


import org.junit.runners.ParameterizedWithName.ParametersToString;

import statechum.Configuration;
import statechum.Configuration.GDScoreComputationAlgorithmEnum;
import statechum.Configuration.GDScoreComputationEnum;
import statechum.Configuration.STATETREE;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.ProgressIndicator;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.linear.GD.ChangesRecorder;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;

/**
 * @author kirill
 *
 */
@RunWith(ParameterizedWithName.class)
public class TestGD_ExistingGraphs 
{
	protected java.util.Map<CmpVertex,CmpVertex> newToOrig = null;

	/** Number of threads to use. */
	protected final int threadNumber, pairsToAdd;

	Configuration config = null;

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
		int threads[]=new int[]{1,8};
		ProgressIndicator progress = new ProgressIndicator("e:", files.length*threads.length);
		
		for(int fileNum = 0;fileNum < files.length;++fileNum)
		{
			File 
			fileA=files[fileNum], 
			fileB=files[(fileNum+1)%files.length];
			boolean fallback = detectFallbackToInitialPair(fileA, null, fileB, null);
			Assert.assertFalse(fallback);// our test files are very small hence must fit in memory

			for(int threadNo:threads)
			{
				for(double ratio:new double[]{0.6,0.9})
					for(int pairs:new int[]{0,40})
						result.add(new Object[]{Integer.valueOf(threadNo), Integer.valueOf(pairs),ratio,fileA,fileB});

				// -1. should be floating-point number otherwise it is turned into Integer and our parametersToString fails to match the resulting list of values.
				result.add(new Object[]{Integer.valueOf(threadNo), Integer.valueOf(0),-1.,fileA,fileB});
				
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
	public TestGD_ExistingGraphs(int th, int pairs,double ratio, File fileA, File fileB)
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
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
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
	
	protected static final <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void addColourAndTransitionsRandomly(AbstractLearnerGraph<TARGET_TYPE, CACHE_TYPE> gr,Random rnd)
	{
		gr.pathroutines.addColourRandomly(rnd, 3);
		gr.pathroutines.addTransitionsRandomly(rnd, 3);
	}

	static public final <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void addPairsRandomly(final GD<TARGET_TYPE,TARGET_TYPE,CACHE_TYPE,CACHE_TYPE> gd, int pairsToAdd)
	{
		Set<CmpVertex> usedA = new TreeSet<CmpVertex>(), usedB = new TreeSet<CmpVertex>();
		usedA.addAll(gd.statesOfA);usedB.addAll(gd.statesOfB);
		for(PairScore ps:gd.frontWave) // first, we remove all states already used as key pairs from the set of states we can choose from. 
		{
			usedA.remove(ps.firstElem);usedB.remove(ps.secondElem);
		}
		if (usedA.isEmpty() || usedB.isEmpty()) return;
		ArrayList<CmpVertex> usedA_array=new ArrayList<CmpVertex>(usedA),usedB_array=new ArrayList<CmpVertex>(usedB);
		int pairsPossible = Math.min(Math.min(usedA_array.size(),usedB_array.size()),pairsToAdd);
		Random rnd=new Random(0);
		Iterator<Integer> 
			itA=TestGD.chooseRandomly(rnd, usedA_array.size(), pairsPossible).iterator(),
			itB=TestGD.chooseRandomly(rnd, usedB_array.size(), pairsPossible).iterator();
		for(int cnt=0;cnt<pairsPossible;++cnt)
		{
			PairScore pair = new PairScore(usedA_array.get(itA.next()), usedB_array.get(itB.next()), 1, 0);
			gd.frontWave.add(pair);
			gd.statesInKeyPairs.add(pair.getQ());gd.statesInKeyPairs.add(pair.getR());
		}
	}
	
	/** Determines whether we have enough memory to compute a product of pairs, 
	 * this depends both on configuration (the type of analysis)
	 * and the amount of memory available to linear solver (in theory, in practice the amount of memory is determined by
	 * taking the number of states, otherwise out-of-memory on solver can easily crash jvm, perhaps I should fix this).
	 *  
	 * @param fileA1 first file to look at
	 * @param fileA2 the second half of the first one, null if not used.
	 * @param fileB1 second file
	 * @param fileB2 the second half of the second one, null if not used.
	 * @return whether a product of pairs will be considered
	 */
	static final boolean detectFallbackToInitialPair(File fileA1, File fileA2, File fileB1, File fileB2)
	{
		final GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		try
		{
			Configuration config=computeConfig(0.75);
			
			LearnerGraphND grA = null, grB = null;
			{
				LearnerGraphND loadedA1 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileA1, loadedA1, null);
				if (fileA2 != null)
				{
					LearnerGraphND loadedA2 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileA2, loadedA2, null);
					grA = LearnerGraphND.UniteTransitionMatrices(loadedA1,loadedA2);TestGD_ExistingGraphs.addColourAndTransitionsRandomly(grA, new Random(0));
				}
				else
					grA = loadedA1;
			}
			
			{
				LearnerGraphND loadedB1 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileB1, loadedB1, null);
				if (fileB2 != null)
				{
					LearnerGraphND loadedB2 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileB2, loadedB2, null);
					grB = LearnerGraphND.UniteTransitionMatrices(loadedB1,loadedB2);TestGD_ExistingGraphs.addColourAndTransitionsRandomly(grB, new Random(1));
				}
				else
					grB = loadedB1;
			}
			
			gd.init(grA, grB, ExperimentRunner.getCpuNumber(),config);
		}
		catch(IOException ex)
		{
			Helper.throwUnchecked("failed to load a file", ex);
		}
		return gd.fallbackToInitialPair;
	}
	
	protected static void addColourAndIncompatiblesRandomly(LearnerGraph gr, Random rnd)
	{
		gr.pathroutines.addColourRandomly(rnd,3);
		gr.pathroutines.addIncompatiblesRandomly(rnd,6);
		gr.pathroutines.addMergedRandomly(rnd,6);
	}

	static ScoresLogger scoresLogger = new ScoresLoggerChecker();

	/** Label converter to use. */
	private ConvertALabel converter = null;

	@BeforeClass
	public static void loadLog()
	{
		scoresLogger.loadMap();
	}
	
	@AfterClass
	public static void saveLogIfNeeded()
	{
		scoresLogger.saveMap();
	}
 
	public final void runPatch(File fileA, File fileB,boolean checkScores)
	{
		try
		{
			LearnerGraph grA = new LearnerGraph(config);AbstractPersistence.loadGraph(fileA, grA, converter);addColourAndIncompatiblesRandomly(grA, new Random(0));
			LearnerGraph grB = new LearnerGraph(config);AbstractPersistence.loadGraph(fileB, grB, converter);addColourAndIncompatiblesRandomly(grB, new Random(1));
			GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
			LearnerGraph graph = new LearnerGraph(config);AbstractPersistence.loadGraph(fileA, graph, converter);addColourAndIncompatiblesRandomly(graph, new Random(0));
			LearnerGraph outcome = new LearnerGraph(config);
			ChangesRecorder patcher = new ChangesRecorder(null);
			//gd.computeGD(grA, grB, threadNumber, patcher,config);
			
			gd.init(grA, grB, threadNumber,config);
			if (checkScores) scoresLogger.checkOrRecord(parametersToString(threadNumber,pairsToAdd,low_to_high_ratio,fileA,fileB), gd.serialiseScores());
			gd.identifyKeyPairs();
			if (!gd.fallbackToInitialPair) addPairsRandomly(gd,pairsToAdd);
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
	public final void testGD_AB_linearRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_LINEAR);
		runPatch(graphA, graphB,true);
	}
	
	@Test
	public final void testGD_BA_linearRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_LINEAR);
		runPatch(graphB, graphA,true);
	}
	
	@Test
	public final void testGD_BA_linearRH_noPropagate()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_LINEAR);
		config.setGdPropagateDet(false);
		runPatch(graphB, graphA,false);
	}
	
	final int NumberOfSequences=50,PathLength=10;
	
	@Test
	public final void testGD_AB_walkRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(NumberOfSequences);
		config.setGdScoreComputationAlgorithm_RandomWalk_PathLength(PathLength);
		runPatch(graphA, graphB,false);
	}
	
	@Test
	public final void testGD_AB_walkRH_noPropagate()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(NumberOfSequences);
		config.setGdScoreComputationAlgorithm_RandomWalk_PathLength(PathLength);
		config.setGdPropagateDet(false);
		runPatch(graphA, graphB,false);
	}
	
	@Test
	public final void testGD_BA_walkRH()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_RH);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(NumberOfSequences);
		config.setGdScoreComputationAlgorithm_RandomWalk_PathLength(PathLength);
		runPatch(graphB, graphA,false);
	}
	
	@Test
	public final void testGD_AB_walk()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(NumberOfSequences);
		config.setGdScoreComputationAlgorithm_RandomWalk_PathLength(PathLength);
		runPatch(graphA, graphB,false);
	}
	
	@Test
	public final void testGD_BA_walk()
	{
		config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		config.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(NumberOfSequences);
		config.setGdScoreComputationAlgorithm_RandomWalk_PathLength(PathLength);
		runPatch(graphB, graphA,false);
	}

	@Test
	public final void testGD_AA()
	{
		try
		{
			LearnerGraph grA = new LearnerGraph(config);AbstractPersistence.loadGraph(graphA,grA, converter);
			LearnerGraph grB = new LearnerGraph(config);AbstractPersistence.loadGraph(graphA,grB, converter);
			LearnerGraph graph = new LearnerGraph(config);AbstractPersistence.loadGraph(graphA,graph, converter);
			GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
			LearnerGraph outcome = new LearnerGraph(config);
			ChangesRecorder.applyGD_WithRelabelling(graph, gd.computeGDToXML(grA, grB, threadNumber, TestGD.createDoc(),null,config), converter,outcome);
			Assert.assertNull(testDetails(),WMethod.checkM(grB,graph));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
			Assert.assertNull(testDetails(),WMethod.checkM_and_colours(grB,outcome,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
		}
		catch(IOException ex)
		{
			Helper.throwUnchecked("failed to load a file", ex);
		}
	}
	
}
