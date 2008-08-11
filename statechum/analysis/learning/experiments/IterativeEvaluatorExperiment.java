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

import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.Collection;

import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.model.testset.PTASequenceEngine;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public abstract class IterativeEvaluatorExperiment extends AbstractExperiment {	
	
	
	public static abstract class IterativeEvaluator extends LearnerEvaluator
	{
		PTASequenceEngine sPlus = null, sMinus = null;
		public IterativeEvaluator(String inputFile, int per, int instance, AbstractExperiment exp)
		{
			super(inputFile, per, instance, exp);			
		}

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParameters(Configuration c);

		protected AtomicInteger questionNumber = new AtomicInteger(0);
		
		/** This method is executed on an executor thread. */
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
			Learner l = new AccuracyTrackerDecorator(new RPNIBlueFringeLearnerTestComponentOpt(null,config){
				@Override
				protected Pair<Integer,String> checkWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<String> question, 
						@SuppressWarnings("unused") final Object [] moreOptions)
				{
					questionNumber.addAndGet(1);
					return new Pair<Integer,String>(graph.paths.tracePath(question),null);
				}
			}
			, graph, tests);
			sMinus = rpg.getAllSequencesPercentageInterval(1);

			LearnerGraph learned = learn(l,sMinus);
			
			result = result + l.getResult();

		}
		
		
		

		private LearnerGraph learn(Learner l, PTASequenceEngine pta)
		{
			DirectedSparseGraph learningOutcome = null;
			changeParameters(config);
			int ptaSize = pta.numberOfLeafNodes();
			l.init(pta, ptaSize,ptaSize);// our imaginary positives are prefixes of negatives.
			learningOutcome = l.learnMachine();
			//l.setQuestionCounter(0);
			return new LearnerGraph(learningOutcome,config);
		}
	}
	

	static class IterativeExperiment extends IterativeEvaluatorExperiment
	{
		protected final Configuration conf;
		
		/** Constructs an experiment class
		 * 
		 * @param qg the questioning strategy to use.
		 * @param limit the limit on the number of paths to choose when looking for paths between a pair of states.
		 * @param useSpeculative whether to use speculative question asking.
		 */
		public IterativeExperiment(Configuration.QuestionGeneratorKind qg, int limit, boolean useSpeculative)
		{
			super();conf=Configuration.getDefaultConfiguration().copy();
			conf.setQuestionGenerator(qg);conf.setQuestionPathUnionLimit(limit);conf.setSpeculativeQuestionAsking(useSpeculative);
		}

		/** Constructs an experiment class for checking whether the improved merger and
		 * question generator does the same thing as the old one.
		 * 
		 * @param qg the questioning strategy to use.
		 * @param limit the limit on the number of paths to choose when looking for paths between a pair of states.
		 */
		public IterativeExperiment()
		{
			super();conf=Configuration.getDefaultConfiguration().copy();
			conf.setQuestionGenerator(Configuration.QuestionGeneratorKind.CONVENTIONAL);
			conf.setSpeculativeQuestionAsking(true);
			conf.setQuestionPathUnionLimit(-1);conf.setConsistencyCheckMode(true);
		}

		public List<LearnerEvaluatorGenerator> getLearnerGenerators() {
			return Arrays.asList(new LearnerEvaluatorGenerator[] {
				new LearnerEvaluatorGenerator() {
					@Override
					LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, AbstractExperiment exp) {
						return new IterativeEvaluator(inputFile, percent, instanceID, exp)
						{
							@Override
							protected void changeParameters(Configuration c) 
							{
								c.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);						
								//c.setCertaintyThreshold(2);c.setGeneralisationThreshold(3);
								//c.setMinCertaintyThreshold(0); //question threshold
								//c.setKlimit(0);c.setLearnerScoreMode(Configuration.ScoreMode.KTAILS);
								c.setQuestionGenerator(conf.getQuestionGenerator());
								c.setQuestionPathUnionLimit(conf.getQuestionPathUnionLimit());
								c.setSpeculativeQuestionAsking(conf.isSpeculativeQuestionAsking());
							}

							@Override
							protected String getLearnerName() {
								return "Questions: "+conf.getQuestionGenerator()+"; union limited to "+conf.getQuestionPathUnionLimit()+"; speculative : "+conf.isSpeculativeQuestionAsking();
							}
						};
					}
				}
			});
		}
	}

	@Override
	public int[] getStages() {return new int[]{100};}
	
	public static void main(String []args)
	{
		try {
			LearnerGraph.testMode=true;
			//Experiment consistencyExperiment = new Experiment();consistencyExperiment.setOutputDir("consistency_");consistencyExperiment.runExperiment(args);// Consistency check
			LearnerGraph.testMode=false;
			
			for(Configuration.QuestionGeneratorKind qk:new Configuration.QuestionGeneratorKind[]{
					Configuration.QuestionGeneratorKind.CONVENTIONAL,
					//Configuration.QuestionGeneratorKind.CONVENTIONAL_IMPROVED,
					//Configuration.QuestionGeneratorKind.SYMMETRIC
					})
				for(boolean speculative:new boolean[]{false})
					for(int limit:new int[]{-1,3,1})
					{
						String experimentDescription = "BLUE_"+qk+"_"+(limit<0?"all":limit)+(speculative?"_SPEC_":"");
						AbstractExperiment experiment = new IterativeExperiment(qk,limit,speculative);experiment.setOutputDir(experimentDescription+"_");
						experiment.runExperiment(args);
					}			
			
		} catch (Exception e1) {
			e1.printStackTrace();
			return;
		}
	}

}
