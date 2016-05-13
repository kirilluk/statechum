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

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import org.junit.Test;

import statechum.Configuration;
import statechum.Label;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.InitialConfigurationAndData;
import statechum.analysis.learning.experiments.PaperUAS.ExperimentPaperUAS;
import statechum.analysis.learning.observers.RecordProgressDecorator;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RPNIBlueFringeVariability;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.model.testset.PTASequenceEngine;

/** Records the choices of pairs made during the learning of a large PTA. */
public class LargePTARecordPairs
{
	   /** Records the initial PTA. The Pta from the paper is recorded as the initial automaton and the automaton passed as an argument is recorded as the expected outcome of learning of the evaluation configuration. 
	    */
	   public void recordInitialConfiguration(String []args,LearnerGraph smallPTA) throws IOException
	   {
		   ExperimentPaperUAS paper = ExperimentPaperUAS.loadTraces(args, true);
			PTASequenceEngine engine = paper.getCollectionOfTraces().get(ExperimentPaperUAS.UAVAllSeeds).tracesForUAVandFrame.get(ExperimentPaperUAS.UAVAllSeeds).get(paper.getMaxFrameNumber());
	       
	       FileOutputStream log = new java.io.FileOutputStream("resources/largePTA/VeryLargePTA.zip");
	       RPNIBlueFringeVariability ourLearner = new RPNIBlueFringeVariability(paper.learnerInitConfiguration,true,null,null);
	       LearnerGraph automatonAfterInit = ourLearner.getLearner().init(engine,0,0);
	       final Configuration shallowCopy = automatonAfterInit.config.copy();shallowCopy.setLearnerCloneGraph(false);
	       LearnerGraph copyOfAutomaton = new LearnerGraph(shallowCopy);LearnerGraph.copyGraphs(automatonAfterInit, copyOfAutomaton);
	       ourLearner.setInitPta(copyOfAutomaton);
	       RecordProgressDecorator recorder = new RecordProgressDecorator(ourLearner.getLearner(),log,1,paper.learnerInitConfiguration.config,true);
	       paper.learnerInitConfiguration.graph = smallPTA;paper.learnerInitConfiguration.testSet = new LinkedList<List<statechum.Label>>();
			recorder.writeLearnerEvaluationData(paper.learnerInitConfiguration);
			recorder.init(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
			recorder.close();
			log.close();// double-close in fact (should already been closed by recorder) but it does not really matter.
	   }


	@Test
	public void recordPairs() throws IOException
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setTransitionMatrixImplType(Configuration.STATETREE.STATETREE_LINKEDHASH); 
		Transform.InternStringLabel converter = new Transform.InternStringLabel();
		InitialConfigurationAndData initialConfigAndData = PairQualityLearner.loadInitialAndPopulateInitialConfiguration(PairQualityLearner.largePTAFileName, config, converter);
		for(boolean merger:new Boolean[]{true,false})
		{
			List<PairOfPaths> listOfPairs = new java.util.ArrayList<PairOfPaths>(1000);
	        long tmStarted = new Date().getTime();
	        new RPNIBlueFringeVariability(initialConfigAndData.learnerInitConfiguration,merger,listOfPairs,null).learn(initialConfigAndData.initial.graph);
	        long tmFinished = new Date().getTime();
	        System.out.println("Learning ("+merger+"), "+merger+" completed in "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
	        java.io.FileOutputStream pairsStream = new java.io.FileOutputStream(PairQualityLearner.largePTALogsDir+TestLearnerFromLargePTA.mergerTypeToXml(merger));
	        PairOfPaths.writePairs(listOfPairs, config, pairsStream);
	        pairsStream.close();
		}
		
	}
}