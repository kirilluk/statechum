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
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import edu.uci.ics.jung.graph.impl.*;
import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
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

		protected String extraPart = "";
		protected AtomicInteger questionNumber = new AtomicInteger(0);
		
		/** This method is executed on an executor thread. */
		public void runTheExperiment()
		{
			int size = 4*graph.getStateNumber();
			RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),5);// the seed for Random should be the same for each file
			int percentPerChunk = 10;
			int nrPerChunk = size/(100/percentPerChunk);nrPerChunk+=nrPerChunk % 2;// make the number even
			rpg.generatePosNeg(nrPerChunk , 100/percentPerChunk);extraPart=extraPart+"size:"+size+FS+"chunks: "+(100/percentPerChunk)+FS+"per chunk:"+nrPerChunk;
			
			RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null,config)
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
			extraPart =extraPart+FS+percent+"%"+FS+"+:"+sPlus.getData().size()+FS+"-:"+sMinus.getData(PTASequenceEngine.truePred).size();
			computePR(l, sMinus);
		}

//		private void posNegExperiment(LearnerGraph fsm, RPNIBlueFringeLearnerTestComponentOpt l){
//			PosNegPrecisionRecall prNeg = computePR(fsm, l, sMinus);
//			PosNegPrecisionRecall pr= computePR(fsm, l, sPlus);
//			System.out.println(pr.getPosprecision()+", "+pr.getPosrecall()/*+", "+pr.getNegprecision()+", "+pr.getNegrecall()+", "+prNeg.getPosprecision()+", "+prNeg.getPosrecall()+", "+prNeg.getNegprecision()+", "+prNeg.getNegrecall()*/);
//		}
		
		private void computePR(RPNIBlueFringeLearnerTestComponentOpt l, 
				PTASequenceEngine ptaMinus)
		{
			LearnerGraph learned = learn(l,ptaMinus);
			PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(learned);
			PTASequenceEngine engine = new PTA_FSMStructure(graph);
			PosNegPrecisionRecall ptaPR = precRec.crossWith(ptaMinus);
			SequenceSet ptaTestSet = engine.new SequenceSet();ptaTestSet.setIdentity();
			ptaTestSet = ptaTestSet.cross(graph.wmethod.getFullTestSet(1));
			PosNegPrecisionRecall prNeg = precRec.crossWith(engine);
			
			// Columns 2 and 3
			result = result+prNeg.precision+FS+prNeg.recall;
			
			result = result + FS + questionNumber+ FS + 
				// Columns 5 and 6
				ptaPR.precision  + FS + ptaPR.recall + FS +extraPart + FS+"L"+FS+graph.linear.getSimilarity(learned, false, 1)+FS+graph.linear.getSimilarity(learned, true, 1);
			result = result + FS + graph.linear.getSimilarityWithNegatives(learned, 1, Linear.DDRH_highlight.class);
			result = result + FS + graph.linear.getSimilarityWithNegatives(learned, 1, Linear.DDRH_highlight_Neg.class);
		}

		private LearnerGraph learn(RPNIBlueFringeLearnerTestComponentOpt l, PTASequenceEngine pta)
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
		return new int[]{10,25,50,75,100};
	}
		
	static class Experiment extends IncrementalAccuracyAndQuestionsExperiment
	{
		protected final Configuration conf;
		
		/** Constructs an experiment class
		 * 
		 * @param qg the questioning strategy to use.
		 * @param limit the limit on the number of paths to choose when looking for paths between a pair of states.
		 */
		public Experiment(Configuration.QuestionGeneratorKind qg, int limit)
		{
			super();conf=(Configuration)Configuration.getDefaultConfiguration().clone();
			conf.setQuestionGenerator(qg);conf.setQuestionPathUnionLimit(limit);
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
								//c.setCertaintyThreshold(3);
								//c.setMinCertaintyThreshold(0); //question threshold
								
								c.setQuestionGenerator(conf.getQuestionGenerator());
								c.setQuestionPathUnionLimit(conf.getQuestionPathUnionLimit());
							}

							@Override
							protected String getLearnerName() {
								return "Questions: "+conf.getQuestionGenerator()+"; union limited to "+conf.getQuestionPathUnionLimit();
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
			Experiment consistencyExperiment = new Experiment();consistencyExperiment.setOutputDir("consistency_");consistencyExperiment.runExperiment(args);// Consistency check
			LearnerGraph.testMode=false;
			
			for(Configuration.QuestionGeneratorKind qk:new Configuration.QuestionGeneratorKind[]{
					Configuration.QuestionGeneratorKind.CONVENTIONAL,
					Configuration.QuestionGeneratorKind.CONVENTIONAL_IMPROVED,
					Configuration.QuestionGeneratorKind.SYMMETRIC
					})
				for(int limit:new int[]{-1,4,2,1})
				{
					String experimentDescription = "_"+qk+"_"+(limit<0?"all":limit);
					AbstractExperiment experiment = new Experiment(qk,limit);experiment.setOutputDir(experimentDescription+"_");
					experiment.runExperiment(args);
					String ending = experimentDescription+".csv";
					experiment.postProcessIntoR(2,true, 3, new File(experiment.getOutputDir(),"precision"+ending));
					experiment.postProcessIntoR(2,true, 4, new File(experiment.getOutputDir(),"recall"+ending));
					experiment.postProcessIntoR(2,true, 5, new File(experiment.getOutputDir(),"questionNumber"+ending));
					experiment.postProcessIntoR(2,true, 16, new File(experiment.getOutputDir(),"linearA"+ending));
					experiment.postProcessIntoR(2,true, 17, new File(experiment.getOutputDir(),"linearN"+ending));
					experiment.postProcessIntoR(2,true, 18, new File(experiment.getOutputDir(),"linearB"+ending));
				}			
		} catch (Exception e1) {
			e1.printStackTrace();
			return;
		}
	}
}
