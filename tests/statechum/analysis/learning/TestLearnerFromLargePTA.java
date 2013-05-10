/* Copyright (c) 2012 The University of Sheffield, UK.
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
package statechum.analysis.learning;

import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.analysis.learning.experiments.PaperUAS;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;

@RunWith(Parameterized.class)
public class TestLearnerFromLargePTA extends PaperUAS 
{
	/*
   	public void recordInitialConfiguration() throws IOException
    {
        PTASequenceEngine engine = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber);
        
        FileOutputStream log = new java.io.FileOutputStream(largePTAFileName);
        RPNIBlueFringe ourLearner = new RPNIBlueFringe(0,false,null,null,null);
        LearnerGraph automatonAfterInit = ourLearner.getLearner().init(engine,0,0);
        final Configuration shallowCopy = automatonAfterInit.config.copy();shallowCopy.setLearnerCloneGraph(false);
        LearnerGraph copyOfAutomaton = new LearnerGraph(shallowCopy);LearnerGraph.copyGraphs(automatonAfterInit, copyOfAutomaton);
        ourLearner.setInitPta(copyOfAutomaton);
        RecordProgressDecorator recorder = new RecordProgressDecorator(ourLearner.getLearner(),log,1,learnerInitConfiguration.config,true);
        learnerInitConfiguration.graph = new LearnerGraph(learnerInitConfiguration.config);learnerInitConfiguration.testSet = new LinkedList<List<statechum.Label>>();
		recorder.writeLearnerEvaluationData(learnerInitConfiguration);
		recorder.init(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		recorder.close();
		log.close();// double-close in fact (should already been closed by recorder) but it does not really matter.
    }
    /**
	 * @param args trace file to load.
     * @throws IOException 
	 *
	public static void main(String[] args) throws IOException {
		TestLearnerFromLargePTA paper = new TestLearnerFromLargePTA();
    	Configuration config = Configuration.getDefaultConfiguration().copy();config.setDebugMode(false);paper.learnerInitConfiguration.config = config;
    	paper.loadReducedConfigurationFile(args[0]);
    	
		int offset=1;
    	{
        	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
	    	int maxFrame = paper.getMaxFrame(inputFiles);
	    	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
    	}
    	
    	{
        	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
	    	//paper.loadDataByConcatenation(inputFiles);
	    	paper.recordInitialConfiguration();
	    	
    	}
	}
	*/

	/* Running learners gives the following numbers for an i7:
[running large PTA test Learning (true), listOpt.xml completed in 2439 sec
completed comparing.
..Learning (true), listGen.xml completed in 1986 sec
completed comparing.
..Learning (false), listOpt.xml completed in 6705 sec
completed comparing.
...Learning (false), listGen.xml completed in 9262 sec
completed comparing.
...] 

Total time: 20492 sec

*/
   	
   	public TestLearnerFromLargePTA(Boolean merger, String pairs,Configuration.STATETREE matrixType)
   	{
   		pairsToUse = pairs;mergerToUse = merger;matrixToUse = matrixType;
   	}
	
   	public static String mergerTypeToXml(boolean optimizedMerge)
   	{
   		if (optimizedMerge) return "listOpt.xml";
   		return "listGen.xml";
   	}
   	
	@Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		for(Configuration.STATETREE matrixType:new Configuration.STATETREE []{Configuration.STATETREE.STATETREE_LINKEDHASH, Configuration.STATETREE.STATETREE_ARRAY})
			for(boolean merger:new boolean[]{true,false})
				for(boolean pairsFromMerger:new boolean[]{true,false})
					result.add(new Object[]{merger,mergerTypeToXml(pairsFromMerger), matrixType});
		
		return result;
	}

	public static String parametersToString(Boolean merger, String pairsFile,Configuration.STATETREE matrixType)
	{
		return "merger:"+(merger?"fast":"general")+", "+matrixType+" , "+pairsFile;
	}

	/** Name of the file containing pairs to be chosen from those determined by ChooseStatePairs. */
	final String pairsToUse;
	/** Whether to use an old merger that relies on merging a PTA into an automaton or a general one that mergers an arbitrary pairs of states. */ 
	final boolean mergerToUse;
	/** The kind of transition matrix to create. */
	final Configuration.STATETREE matrixToUse;
	
	@Test
	public void runCompareTwoLearners() throws IOException
    {
		Transform.InternStringLabel converter = new Transform.InternStringLabel();
		PaperUAS paper = new PaperUAS();
		ProgressDecorator.InitialData initial = PairQualityLearner.loadInitialAndPopulateInitialConfiguration(paper, PairQualityLearner.largePTAFileName, converter);
		
		Configuration learnerConf = paper.learnerInitConfiguration.config.copy();learnerConf.setTransitionMatrixImplType(matrixToUse);
        FileReader listOptReader = new FileReader(PairQualityLearner.largePTALogsDir+pairsToUse);
        List<PairOfPaths> listOpt=PairOfPaths.readPairs(listOptReader, paper.learnerInitConfiguration.config,converter);
        listOptReader.close();

        long tmStarted = new Date().getTime();
        LearnerGraph graphD=paper.new RPNIBlueFringeTestVariability(learnerConf,mergerToUse,null,listOpt).learn(initial.graph);
        long tmFinished = new Date().getTime();
        System.out.println("Learning ("+mergerToUse+"), "+pairsToUse+" completed in "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
        String outcomeName = PairQualityLearner.largePTALogsDir+"outcome_"+pairsToUse;
        //graphD.storage.writeGraphML(outcomeName);
        LearnerGraph referenceA = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph(outcomeName, referenceA, converter);
        Assert.assertEquals(matrixToUse,graphD.config.getTransitionMatrixImplType());
        DifferentFSMException diff = WMethod.checkM(referenceA, graphD);
        if (diff != null)
        	throw diff;
    }
}
