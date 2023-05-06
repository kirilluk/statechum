package statechum.analysis.learning.experiments;


import java.io.IOException;
import java.util.Collection;


import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized.Parameters;
import junit_runners.ParameterizedWithName;

import statechum.Configuration;
import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.InitialConfigurationAndData;
import statechum.analysis.learning.experiments.PaperUAS.ExperimentPaperUAS;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestWithMultipleConfigurations;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;

@RunWith(ParameterizedWithName.class)
public class TestPaperUASLarge extends TestWithMultipleConfigurations
{

	protected ExperimentPaperUAS paper;
	
	@Parameters
	public static Collection<Object[]> data() 
	{
		return TestWithMultipleConfigurations.data();
	}
	
	public static String parametersToString(Configuration config)
	{
		return TestWithMultipleConfigurations.parametersToString(config);
	}
	
	public TestPaperUASLarge(Configuration conf)
	{
		super(conf);
	}
	
	@Before
	public void BeforeTests()
	{
		paper = new ExperimentPaperUAS();paper.learnerInitConfiguration.config = mainConfiguration;
		paper.learnerInitConfiguration.setLabelConverter( paper.learnerInitConfiguration.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?new Transform.InternStringLabel():null );
	}
	
	@Test
	public void testMergerOnLargePTA() throws IOException
	{
		Configuration configToLoadWith = mainConfiguration.copy();configToLoadWith.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
		ConvertALabel labelConverter = converter;if (labelConverter == null) labelConverter = new Transform.InternStringLabel();
		InitialConfigurationAndData initialConfigurationData = PairQualityLearner.loadInitialAndPopulateInitialConfiguration(PairQualityLearner.veryLargePTAFileName, configToLoadWith, labelConverter);
		LearnerGraph hugeGraph = new LearnerGraph(initialConfigurationData.initial.graph,mainConfiguration);LearnerGraph smallGraph = new LearnerGraph(initialConfigurationData.learnerInitConfiguration.graph,mainConfiguration);
		
		System.out.println("Huge: "+hugeGraph.getStateNumber()+" states, "+(hugeGraph.getStateNumber()-hugeGraph.getAcceptStateNumber())+" reject states");
		System.out.println("Small: "+smallGraph.getStateNumber()+" states, "+(smallGraph.getStateNumber()-smallGraph.getAcceptStateNumber())+" reject states");
		Label labelToMerge = AbstractLearnerGraph.generateNewLabel("Waypoint_Selected",mainConfiguration,converter);
		LearnerGraph mergedHuge = LearningSupportRoutines.mergeStatesForUnique(hugeGraph, labelToMerge);
		DifferentFSMException diffFSM = WMethod.checkM(smallGraph, mergedHuge);
		if (diffFSM != null)
			throw diffFSM;

		LearnerGraph mergedSmall = LearningSupportRoutines.mergeStatesForUnique(smallGraph,labelToMerge);
		diffFSM = WMethod.checkM(smallGraph,mergedSmall);
		if (diffFSM != null)
			throw diffFSM;
		
		LearnerGraph mergedHugeB=UASExperiment.mergePTA(hugeGraph, labelToMerge, true);// we do not particularly care what we get but there should be no exception generated.
		LearnerGraph mergedSmallB=UASExperiment.mergePTA(smallGraph, labelToMerge, true);// we do not particularly care what we get but there should be no exception generated.
		diffFSM = WMethod.checkM(mergedHugeB,mergedSmallB);// the two graphs should reduce to the same one since they were built out of the same data, one by concatenation and another one without, however transitions between elements that have been concatenated are merged by the merger above so the outcome should be the same. 
		if (diffFSM != null)
			throw diffFSM;

		// now do the same merge but this time without update of auxiliary information
		mergedHugeB=UASExperiment.mergePTA(hugeGraph, labelToMerge, false);
		diffFSM = WMethod.checkM(mergedHugeB,mergedSmallB); 
		if (diffFSM != null)
			throw diffFSM;
		
	}
		
}