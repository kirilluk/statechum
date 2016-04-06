package statechum.analysis.learning.linear;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Configuration.GDScoreComputationAlgorithmEnum;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;

public class PairQualityExperiment {
	
	public final static int threadNumber = ExperimentRunner.getCpuNumber();
	
	public static <TARGET_A_TYPE,TARGET_B_TYPE,
		CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
		CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>> void runExperiment(
				AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> grA,AbstractLearnerGraph<TARGET_B_TYPE,CACHE_B_TYPE> grB,
				GD<TARGET_A_TYPE,TARGET_B_TYPE,CACHE_A_TYPE,CACHE_B_TYPE> gd,
				GD.ChangesCounter<TARGET_A_TYPE,TARGET_B_TYPE,CACHE_A_TYPE,CACHE_B_TYPE> changesCounter,
				String name)
	{
		Configuration configA = Configuration.getDefaultConfiguration().copy(), configB = Configuration.getDefaultConfiguration().copy();
		configA.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_LINEAR);
		configA.setGdFailOnDuplicateNames(false);
		configA.setGdLowToHighRatio(0.9);
		configA.setAttenuationK(0.95);
		//configA.setGdPropagateDet(true);
		
		configB.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_RANDOMPATHS);
		configB.setGdFailOnDuplicateNames(false);
		configB.setGdScoreComputationAlgorithm_RandomWalk_NumberOfSequences(400);
		configB.setGdScoreComputationAlgorithm_RandomWalk_PathLength(10);
		configA.setAttenuationK(0.95);
		configB.setGdLowToHighRatio(0.9);
		//configB.setGdPropagateDet(true);
		
		System.out.print(name+" ");
		
		for(Configuration cnf:new Configuration[]{configA,configB})
		{
			gd.init(grA, grB, threadNumber,cnf);
			gd.identifyKeyPairs();int keyPairs = gd.frontWave.size();
			//TestGD.printListOfPairs(gd.frontWave, gd.newBToOrig);
			gd.makeSteps();
			changesCounter.reset();
			gd.computeDifference(changesCounter);
			System.out.print(keyPairs+" ("+changesCounter.detailsToString()+") ");
		}
		System.out.println();
	}
	
	protected static final <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void addColourAndTransitionsRandomly(AbstractLearnerGraph<TARGET_TYPE, CACHE_TYPE> gr,Random rnd)
	{
		gr.pathroutines.addColourRandomly(rnd, 3);
		gr.pathroutines.addTransitionsRandomly(rnd, 3);
	}
		
	public static final void main(String []args)
	{
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
		//TestGD.ProgressIndicator progress = new TestGD.ProgressIndicator("e:", files.length*threads.length);
		
		try
		{
			for(int fileNum = 0;fileNum < files.length;++fileNum)
			{
				{
					File 
						fileA=files[fileNum], 
						fileB=files[(fileNum+1)%files.length];
					Configuration config = Configuration.getDefaultConfiguration().copy();
					LearnerGraph grA = new LearnerGraph(config);AbstractPersistence.loadGraph(fileA, grA,null);
					LearnerGraph grB = new LearnerGraph(config);AbstractPersistence.loadGraph(fileB, grB,null);
					GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
					GD.ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new GD.ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(grA,grB,null);
					runExperiment(grA,grB,gd,counter,fileA.getName()+"-"+fileB.getName());
				}
				
				{
					
					Configuration config = Configuration.getDefaultConfiguration().copy();
					File
						fileA1 = files[fileNum], 
						fileA2 = files[(fileNum+1)%files.length],
						fileB1 = files[(fileNum+2)%files.length],
						fileB2 = files[(fileNum+3)%files.length];
				
					LearnerGraphND grA = null, grB = null;
					{
						LearnerGraphND loadedA1 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileA1, loadedA1,null);
						LearnerGraph loadedA2 = new LearnerGraph(config);AbstractPersistence.loadGraph(fileA2, loadedA2,null);
						grA = LearnerGraphND.UniteTransitionMatrices(loadedA1,loadedA2);
						addColourAndTransitionsRandomly(grA, new Random(0));
					}
					
					{
						LearnerGraphND loadedB1 = new LearnerGraphND(config);AbstractPersistence.loadGraph(fileB1, loadedB1,null);
						LearnerGraph loadedB2 = new LearnerGraph(config);AbstractPersistence.loadGraph(fileB2, loadedB2,null);
						grB = LearnerGraphND.UniteTransitionMatrices(loadedB1,loadedB2);
						addColourAndTransitionsRandomly(grB, new Random(1));
					}
					
					GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
					GD.ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> counter = new GD.ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>(grA,grB,null);
					runExperiment(grA,grB,gd,counter,fileA1.getName()+"+"+fileA2.getName()+"-"+fileB1.getName()+"+"+fileB2.getName());
	
				}
			}
		}
		catch(Exception ex)
		{
			ex.printStackTrace();
		}
	}
}
