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

import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.Collection;

import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.analysis.learning.*;
import statechum.analysis.learning.experiments.ExperimentRunner.GeneratorConfiguration;
import statechum.analysis.learning.experiments.ExperimentRunner.LearnerEvaluator;
import statechum.analysis.learning.observers.AccuracyTrackerDecorator;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.model.testset.PTASequenceEngine;

public class IterativeEvaluatorExperiment {	
	
	
	public static abstract class IterativeEvaluator extends LearnerEvaluator
	{
		PTASequenceEngine sPlus = null, sMinus = null;
		public IterativeEvaluator(String inputFile, int per, int instance, ExperimentRunner exp, Configuration cnf, String name)
		{
			super(inputFile, per, instance, exp,cnf,name);			
		}

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParameters(Configuration c);

		protected AtomicInteger questionNumber = new AtomicInteger(0);
		
		/** This method is executed on an executor thread. */
		@Override
		public void runTheExperiment()
		{
			int sampleSize = (graph.getStateNumber()*4);
			RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),5);// the seed for Random should be the same for each file
			rpg.generateRandomPosNeg(sampleSize, 2);  
			Collection<List<String>> tests = graph.wmethod.getFullTestSet(1);
			config.setDebugMode(true);
			config.setAskQuestions(true);
			config.setSpeculativeQuestionAsking(true);
			config.setMinCertaintyThreshold(2);
			//config.setAskQuestions(false);
			//config.setKlimit(2);
			//config.setLearnerScoreMode(Configuration.ScoreMode.KTAILS);
			config.setLearnerScoreMode(Configuration.ScoreMode.CONVENTIONAL);
			Learner l = new AccuracyTrackerDecorator(new RPNIUniversalLearner(null,null,config){
				@Override
				public Pair<Integer,String> CheckWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<String> question, 
						@SuppressWarnings("unused") final Object [] moreOptions)
				{
					questionNumber.addAndGet(1);
					return new Pair<Integer,String>(graph.paths.tracePath(question),null);
				}
			}
			, graph);
			sMinus = rpg.getAllSequencesPercentageInterval(1);

			learn(l,sMinus);
			result = result + l.getResult();
		}
		
		
		

		private LearnerGraph learn(Learner l, PTASequenceEngine pta)
		{
			LearnerGraph learningOutcome = null;
			changeParameters(config);
			int ptaSize = pta.numberOfLeafNodes();
			learningOutcome = l.learnMachine(pta, ptaSize,ptaSize);// our imaginary positives are prefixes of negatives.
			//l.setQuestionCounter(0);
			return learningOutcome;
		}
	}

	public static void main(String []args)
	{
		try {
			LearnerGraph.testMode=true;
			//Experiment consistencyExperiment = new Experiment();consistencyExperiment.setOutputDir("consistency_");consistencyExperiment.runExperiment(args);// Consistency check
			LearnerGraph.testMode=false;
			ExperimentRunner experiment = new ExperimentRunner();
			
			for(Configuration.QuestionGeneratorKind qk:new Configuration.QuestionGeneratorKind[]{
					Configuration.QuestionGeneratorKind.CONVENTIONAL,
					//Configuration.QuestionGeneratorKind.CONVENTIONAL_IMPROVED,
					//Configuration.QuestionGeneratorKind.SYMMETRIC
					})
				for(boolean speculative:new boolean[]{false})
					for(int limit:new int[]{-1,3,1})
					{
						String experimentDescription = "BLUE_"+qk+"_"+(limit<0?"all":limit)+(speculative?"_SPEC_":"");
						Configuration config= Configuration.getDefaultConfiguration().copy();
						config.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);						
						config.setQuestionGenerator(qk);
						config.setQuestionPathUnionLimit(limit);
						config.setSpeculativeQuestionAsking(speculative);
						experiment.addLearnerEvaluator(new GeneratorConfiguration(config,IterativeEvaluator.class,experimentDescription));
					}			
			
			//experiment.setOutputDir(experimentDescription+"_");
			experiment.runExperiment(args);
		} catch (Exception e1) {
			e1.printStackTrace();
			return;
		}
	}

}
