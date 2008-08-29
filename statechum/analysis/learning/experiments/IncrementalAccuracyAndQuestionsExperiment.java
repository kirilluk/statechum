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


import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.analysis.learning.RPNILearner;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.PrecisionRecall.PosNegPrecisionRecall;
import statechum.analysis.learning.experiments.ExperimentRunner.GeneratorConfiguration;
import statechum.analysis.learning.experiments.ExperimentRunner.LearnerEvaluator;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.observers.QuestionAndRestartCounter;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Linear;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.model.testset.*;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

public class IncrementalAccuracyAndQuestionsExperiment
{	
	public static final String FS = ExperimentRunner.FS;

	/** This one is not static because it refers to the frame to display results. */
	public static class RPNIEvaluator extends LearnerEvaluator
	{
		PTASequenceEngine sPlus = null, sMinus = null;
		
		/** Constructs the experiment runner. 
		 * 
		 * @param inputFile input file to process
		 * @param per percentage
		 * @param instance a single number which can be used to identify this file/percentage/learner_kind combo.
		 * @param exp the enclosing instance of the experiment - a poor man's way to subclassing nested classes.
		 * @param cnf configuration to base this learner experiment on
		 * @param name the name to give to this learner.
		 */
		public RPNIEvaluator(String inputFile, int per, int instance, ExperimentRunner exp, Configuration cnf, String name)
		{
			super(inputFile, per, instance, exp, cnf, name);
		}

		protected AtomicInteger questionNumber = new AtomicInteger(0);
		
		/** This method is executed on an executor thread. */
		@Override
		public void runTheExperiment()
		{
			int size = 4*graph.getStateNumber();
			RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),5);// the seed for Random should be the same for each file
			int percentPerChunk = 10;
			int nrPerChunk = size/(100/percentPerChunk);nrPerChunk+=nrPerChunk % 2;// make the number even
			rpg.generatePosNeg(2*nrPerChunk , 100/percentPerChunk);// 2* reflects the fact that nrPerChunk denotes the number of elements in both chunks (positive and negative) combined.  
			RPNILearner learner = new RPNIBlueFringeLearner(null,config)
			{
				@Override
				public Pair<Integer,String> CheckWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<String> question, 
						@SuppressWarnings("unused") final Object [] moreOptions)
				{
					questionNumber.addAndGet(1);
					return new Pair<Integer,String>(graph.paths.tracePath(question),null);
				}
			};
			QuestionAndRestartCounter l = new QuestionAndRestartCounter(learner);
			sPlus = rpg.getExtraSequences(percent/10-1);sMinus = rpg.getAllSequences(percent/10-1);

			LearnerGraph learnt = learn(l,sMinus);
			PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(learnt);
			PTASequenceEngine engine = new PTA_FSMStructure(graph);
			PosNegPrecisionRecall ptaPR = precRec.crossWith(sMinus);
			SequenceSet ptaTestSet = engine.new SequenceSet();ptaTestSet.setIdentity();
			ptaTestSet = ptaTestSet.cross(graph.wmethod.getFullTestSet(1));
			PosNegPrecisionRecall prNeg = precRec.crossWith(engine);
			
			assert questionNumber.get() == l.getQuestionCounter();
			
			// Column 0 is the name of the learner. 
			// Columns 3 and 4
			result = result+prNeg.getPrecision()+FS+prNeg.getRecall();
			
			result = result + FS + questionNumber+ FS + // 5
				// Columns 6 and 7
				ptaPR.getPrecision()  + FS + ptaPR.getRecall() + FS +
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
					FS+Linear.getSimilarity(graph,learnt, false, 1)+FS+Linear.getSimilarity(graph, learnt, true, 1);
				// 17
				result = result + FS + Linear.getSimilarityGD(graph,learnt, 1).getCompressionRate();// + graph.linear.getSimilarityWithNegatives(learned, 1, LearnerGraphND.DDRH_default.class);
				// 18
				result = result + FS + Linear.getSimilarityGD_details(graph,learnt, 1);// + graph.linear.getSimilarityWithNegatives(learned, 1, LearnerGraphND.DDRH_default.class);
			}
			catch(IllegalArgumentException ex)
			{
				StringWriter wr = new StringWriter();ex.printStackTrace(new PrintWriter(wr));
				result = result+"\n"+"exception from linear: "+ex+
					" on graph with "+learnt.getStateNumber()+" and "+learnt.getStateNumber()+" transitions" +
					"\n"+wr.getBuffer().toString();
			}
			
			// 19 and 20
			result = result + FS + graph.paths.getExtentOfCompleteness() + FS + learnt.paths.getExtentOfCompleteness() + FS +
				l.getRestarts(); // 21
		}

		private LearnerGraph learn(Learner l, PTASequenceEngine pta)
		{
			int ptaSize = pta.numberOfLeafNodes();
			LearnerGraph learningOutcome  = l.learnMachine(pta, ptaSize,ptaSize);// our imaginary positives are prefixes of negatives.
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
			List<String> learnerNames= new LinkedList<String>();
			for(Configuration.QuestionGeneratorKind qk:new Configuration.QuestionGeneratorKind[]{
					Configuration.QuestionGeneratorKind.CONVENTIONAL,
					//Configuration.QuestionGeneratorKind.CONVENTIONAL_IMPROVED,
					//Configuration.QuestionGeneratorKind.SYMMETRIC
					})
				for(boolean speculative:new boolean[]{false})
					for(int limit:new int[]{-1,3,1})
					{
						Configuration config = Configuration.getDefaultConfiguration().copy();
						config.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);
						config.setGdFailOnDuplicateNames(false);
						config.setGdLowToHighRatio(0.65);
						//c.setCertaintyThreshold(2);c.setGeneralisationThreshold(3);
						//c.setMinCertaintyThreshold(0); //question threshold
						//c.setKlimit(0);c.setLearnerScoreMode(Configuration.ScoreMode.KTAILS);
						config.setQuestionGenerator(qk);
						config.setQuestionPathUnionLimit(limit);
						config.setSpeculativeQuestionAsking(speculative);

						String experimentDescription = "BLUE_"+qk+"_"+(limit<0?"all":limit)+(speculative?"_SPEC_":"");
						learnerNames.add(experimentDescription);
						experiment.addLearnerEvaluator(new GeneratorConfiguration(config,RPNIEvaluator.class,experimentDescription));
					}			
			
			//experiment.setOutputDir(experimentDescription+"_");
			//experiment.runExperiment(args);
			experiment.robustRunExperiment("resources/testfilelist.txt", "output/Incremental");
			for(String name:learnerNames)
			{
				String ending = ".csv";
				/*
				experiment.postProcessIntoR(name,2,true, 3, new File(experiment.getOutputDir(),name+"-precision"+ending));
				experiment.postProcessIntoR(name,2,true, 4, new File(experiment.getOutputDir(),name+"-recall"+ending));
				experiment.postProcessIntoR(name,2,true, 5, new File(experiment.getOutputDir(),name+"-questionNumber"+ending));
				experiment.postProcessIntoR(name,2,true, 15, new File(experiment.getOutputDir(),name+"-linear"+ending));
				experiment.postProcessIntoR(name,2,true, 16, new File(experiment.getOutputDir(),name+"-linearP"+ending));
				experiment.postProcessIntoR(name,2,true, 20, new File(experiment.getOutputDir(),name+"-completeness"+ending));
				experiment.postProcessIntoR(name,2,true, 21, new File(experiment.getOutputDir(),name+"-restarts"+ending));
				*/
			}
		} catch (Exception e1) {
			e1.printStackTrace();
			return;
		}
	}
}
