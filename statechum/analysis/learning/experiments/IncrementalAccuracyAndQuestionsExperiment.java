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


import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import edu.uci.ics.jung.graph.impl.*;
import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Linear;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.model.testset.*;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

public abstract class IncrementalAccuracyAndQuestionsExperiment extends AbstractExperiment 
{	
	/** This one is not static because it refers to the frame to display results. */
	public static abstract class RPNIEvaluator extends LearnerEvaluator
	{
		PTASequenceEngine sPlus = null, sMinus = null;
		public RPNIEvaluator(String inputFile, int per, int instance, AbstractExperiment exp)
		{
			super(inputFile, per, instance, exp);			
		}

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParameters(Configuration c);

		protected AtomicInteger questionNumber = new AtomicInteger(0);
		
		/** This method is executed on an executor thread. */
		public void runTheExperiment()
		{
			int size = 4*graph.getStateNumber();
			RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),5);// the seed for Random should be the same for each file
			int percentPerChunk = 10;
			int nrPerChunk = size/(100/percentPerChunk);nrPerChunk+=nrPerChunk % 2;// make the number even
			rpg.generatePosNeg(2*nrPerChunk , 100/percentPerChunk);// 2* reflects the fact that nrPerChunk denotes the number of elements in both chunks (positive and negative) combined.  
			
			RPNIBlueFringeLearner l = new RPNIBlueFringeLearnerTestComponentOpt(null,config)
			{
				@Override
				protected Pair<Integer,String> checkWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<String> question, 
						@SuppressWarnings("unused") final Object [] moreOptions)
				{
					questionNumber.addAndGet(1);
					return new Pair<Integer,String>(graph.paths.tracePath(question),null);
				}
			};
			sPlus = rpg.getExtraSequences(percent/10-1);sMinus = rpg.getAllSequences(percent/10-1);

			LearnerGraph learned = learn(l,sMinus);
			PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(learned);
			PTASequenceEngine engine = new PTA_FSMStructure(graph);
			PosNegPrecisionRecall ptaPR = precRec.crossWith(sMinus);
			SequenceSet ptaTestSet = engine.new SequenceSet();ptaTestSet.setIdentity();
			ptaTestSet = ptaTestSet.cross(graph.wmethod.getFullTestSet(1));
			PosNegPrecisionRecall prNeg = precRec.crossWith(engine);
			
			// Columns 3 and 4
			result = result+prNeg.precision+FS+prNeg.recall;
			
			result = result + FS + questionNumber+ FS + // 5
				// Columns 6 and 7
				ptaPR.precision  + FS + ptaPR.recall + FS +
				"size:"+size+FS+ // 8
				"chunks: "+(100/percentPerChunk)+FS+ // 9
				"per chunk:"+nrPerChunk + // 10
				FS+percent+"%"+FS+ // 11
				"+:"+sPlus.getData().size()+FS+// 12
				"-:"+sMinus.getData(PTASequenceEngine.truePred).size(); // 13
			try
			{
				result = result + FS+"L"+// 14
				// 15 and 16
					FS+graph.linear.getSimilarity(learned, false, 1)+FS+graph.linear.getSimilarity(learned, true, 1);
				// 17
				result = result + FS + graph.linear.getSimilarityWithNegatives(learned, 1, Linear.DDRH_highlight.class);
				// 18
				result = result + FS + graph.linear.getSimilarityWithNegatives(learned, 1, Linear.DDRH_highlight_Neg.class);
			}
			catch(IllegalArgumentException ex)
			{
				StringWriter wr = new StringWriter();ex.printStackTrace(new PrintWriter(wr));
				result = result+"\n"+"exception from linear: "+ex+
					" on graph with "+learned.getStateNumber()+" and "+learned.getStateNumber()+" transitions" +
					"\n"+wr.getBuffer().toString();
			}
			
			// 19 and 20
			result = result + FS + graph.paths.getExtentOfCompleteness() + FS + learned.paths.getExtentOfCompleteness() + FS +
				l.getRestarts(); // 21
		}

		private LearnerGraph learn(RPNIBlueFringeLearner l, PTASequenceEngine pta)
		{
			DirectedSparseGraph learningOutcome = null;
			changeParameters(config);
			int ptaSize = pta.numberOfLeafNodes();
			l.init(pta, ptaSize,ptaSize);// our imaginary positives are prefixes of negatives.
			learningOutcome = l.learnMachine();
			l.setQuestionCounter(0);
			return new LearnerGraph(learningOutcome,config);
		}
	}
	
	public int [] getStages()
	{
		return new int[]{10,30,60,100};
	}
		
	static class Experiment extends IncrementalAccuracyAndQuestionsExperiment
	{
		protected final Configuration conf;
		
		/** Constructs an experiment class
		 * 
		 * @param qg the questioning strategy to use.
		 * @param limit the limit on the number of paths to choose when looking for paths between a pair of states.
		 * @param useSpeculative whether to use speculative question asking.
		 */
		public Experiment(Configuration.QuestionGeneratorKind qg, int limit, boolean useSpeculative)
		{
			super();conf=(Configuration)Configuration.getDefaultConfiguration().clone();
			conf.setQuestionGenerator(qg);conf.setQuestionPathUnionLimit(limit);conf.setSpeculativeQuestionAsking(useSpeculative);
		}

		/** Constructs an experiment class for checking whether the improved merger and
		 * question generator does the same thing as the old one.
		 * 
		 * @param qg the questioning strategy to use.
		 * @param limit the limit on the number of paths to choose when looking for paths between a pair of states.
		 */
		public Experiment()
		{
			super();conf=(Configuration)Configuration.getDefaultConfiguration().clone();
			conf.setQuestionGenerator(Configuration.QuestionGeneratorKind.CONVENTIONAL);
			conf.setSpeculativeQuestionAsking(true);
			conf.setQuestionPathUnionLimit(-1);conf.setConsistencyCheckMode(true);
		}

		public List<LearnerEvaluatorGenerator> getLearnerGenerators() {
			return Arrays.asList(new LearnerEvaluatorGenerator[] {
				new LearnerEvaluatorGenerator() {
					@Override
					LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, AbstractExperiment exp) {
						return new RPNIEvaluator(inputFile, percent, instanceID, exp)
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
						AbstractExperiment experiment = new Experiment(qk,limit,speculative);experiment.setOutputDir(experimentDescription+"_");
						experiment.runExperiment(args);
						String ending = experimentDescription+".csv";
						experiment.postProcessIntoR(2,true, 3, new File(experiment.getOutputDir(),"precision"+ending));
						experiment.postProcessIntoR(2,true, 4, new File(experiment.getOutputDir(),"recall"+ending));
						experiment.postProcessIntoR(2,true, 5, new File(experiment.getOutputDir(),"questionNumber"+ending));
						experiment.postProcessIntoR(2,true, 16, new File(experiment.getOutputDir(),"linearA"+ending));
						experiment.postProcessIntoR(2,true, 17, new File(experiment.getOutputDir(),"linearN"+ending));
						experiment.postProcessIntoR(2,true, 18, new File(experiment.getOutputDir(),"linearB"+ending));
						experiment.postProcessIntoR(2,true, 20, new File(experiment.getOutputDir(),"completeness"+ending));
						experiment.postProcessIntoR(2,true, 21, new File(experiment.getOutputDir(),"restarts"+ending));
					}			
			
		} catch (Exception e1) {
			e1.printStackTrace();
			return;
		}
	}
}
