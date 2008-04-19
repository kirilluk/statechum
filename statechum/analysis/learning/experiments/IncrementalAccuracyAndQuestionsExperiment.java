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
import edu.uci.ics.jung.graph.impl.*;
import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.rpnicore.LearnerGraph;
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

		/** This method is executed on an executor thread. */
		public void runTheExperiment()
		{
			int size = 4*graph.getStateNumber();
			RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),5);// the seed for Random should be the same for each file
			int nrPerChunk = size/experiment.getStageNumber();nrPerChunk+=nrPerChunk % 2;// make the number even
			rpg.generatePosNeg(nrPerChunk , experiment.getStageNumber());
			
			RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null,config)
			{
				protected Pair<Integer,String> checkWithEndUser(
						@SuppressWarnings("unused")	DirectedSparseGraph model,
						List<String> question, 
						@SuppressWarnings("unused") final Object [] moreOptions)
				{
					return new Pair<Integer,String>(graph.paths.tracePath(question),null);
				}
			};
			sPlus = rpg.getExtraSequences(percent);sMinus = rpg.getAllSequences(percent);
			computePR(graph, l, sMinus);
		}

//		private void posNegExperiment(LearnerGraph fsm, RPNIBlueFringeLearnerTestComponentOpt l){
//			PosNegPrecisionRecall prNeg = computePR(fsm, l, sMinus);
//			PosNegPrecisionRecall pr= computePR(fsm, l, sPlus);
//			System.out.println(pr.getPosprecision()+", "+pr.getPosrecall()/*+", "+pr.getNegprecision()+", "+pr.getNegrecall()+", "+prNeg.getPosprecision()+", "+prNeg.getPosrecall()+", "+prNeg.getNegprecision()+", "+prNeg.getNegrecall()*/);
//		}
		
		private void computePR(LearnerGraph fsm, RPNIBlueFringeLearnerTestComponentOpt l, 
				PTASequenceEngine ptaMinus)
		{
			LearnerGraph learned = learn(l,ptaMinus);
			PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(learned);
			PTASequenceEngine engine = new PTA_FSMStructure(fsm);
			PosNegPrecisionRecall ptaPR = precRec.crossWith(ptaMinus);
			SequenceSet ptaTestSet = engine.new SequenceSet();ptaTestSet.setIdentity();
			ptaTestSet = ptaTestSet.cross(fsm.wmethod.getFullTestSet(1));
			PosNegPrecisionRecall prNeg = precRec.crossWith(engine);
			
			// Columns 2 and 3
			result = result+prNeg.precision+FS+prNeg.recall;
			
			result = result + FS + "AUX"+ FS + 
				// Columns 5 and 6
				ptaPR.precision  + FS + ptaPR.recall;
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
		return new int[]{10,50,100};
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
								c.setCertaintyThreshold(3);
								c.setMinCertaintyThreshold(0); //question threshold
								
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
			for(Configuration.QuestionGeneratorKind qk:new Configuration.QuestionGeneratorKind[]{Configuration.QuestionGeneratorKind.CONVENTIONAL, Configuration.QuestionGeneratorKind.SYMMETRIC})
				for(int limit:new int[]{1,2,-1})
				{
					AbstractExperiment experiment = new Experiment(qk,limit);experiment.runExperiment(args);
					String ending = "_"+qk+"_"+(limit<0?"all":limit)+".csv";
					experiment.postProcessIntoR(2, 3, new File(experiment.getOutputDir(),"precision"+ending));
					experiment.postProcessIntoR(2, 4, new File(experiment.getOutputDir(),"recall"+ending));
				}			
		} catch (Exception e1) {
			e1.printStackTrace();
			return;
		}
	}
}
