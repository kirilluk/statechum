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
/*
 * INCOMPLETE
 */

package statechum.analysis.learning.experiments;


import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.*;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.Configuration.IDMode;
import statechum.analysis.learning.RPNIBlueFringeLearnerOrig;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.xmachine.model.testset.PTASequenceEngine;

public class AccuracyAndQuestionsExperiment extends AbstractExperiment {

	/** This one is not static because it refers to the frame to display results. */
	public static abstract class RPNIEvaluator extends LearnerEvaluator
	{
		protected Collection<List<String>> tests = null;
		protected PTASequenceEngine pta = null;
		
		public RPNIEvaluator(String inputFile, int per, int instance, AbstractExperiment exp)
		{
			super(inputFile, per,instance, exp);			
		}

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParameters(Configuration c);

		protected void buildSetsHalfNegative()
		{
			loadGraph();
			int size = (graph.getStateNumber()*graph.getStateNumber())/2;
	    	RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),5);// the seed for Random should be the same for each file
	    	rpg.generateRandomPosNeg(size/(2*experiment.getStageNumber()), experiment.getStageNumber());
			pta = rpg.getAllSequences(percent);
			tests = new ArrayList<List<String>>();
			for(List<String> test:graph.wmethod.getFullTestSet(0))
				if (!pta.containsSequence(test))
					tests.add(test);
		}

		public String call()
		{
			//System.out.println(inputFileName+" (instance "+instanceID+"), learner "+this+", "+percent + "% started at "+Calendar.getInstance().getTime());
			OUTCOME currentOutcome = OUTCOME.FAILURE;
			String stdOutput = writeResult(currentOutcome,null);// record the failure result in case something fails later and we fail to update the file, such as if we are killed or run out of memory
			if (stdOutput != null) return stdOutput;
			
			buildSetsHalfNegative();
			
			RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null,config)
			{
				protected int checkWithEndUser(
						@SuppressWarnings("unused")	DirectedSparseGraph model,
						List<String> question, 
						@SuppressWarnings("unused")	final Object [] moreOptions)
				{
					return graph.paths.tracePath(question);
				}
			};
			//l.setCertaintyThreshold(10);
			config.setMinCertaintyThreshold(500000); //question threshold
			DirectedSparseGraph learningOutcome = null;
			int ptaElements = pta.numberOfLeafNodes();
			String result = "";
			String stats = "Instance: "+instanceID+", learner: "+this+", pta: "+ptaElements+" tests: "+tests.size()+ "\n";
			try
			{
				changeParameters(config);
				l.init(pta, ptaElements/2,ptaElements/2);

				learningOutcome = l.learnMachine();
				result = result+l.getQuestionCounter()+FS+computeAccuracy(learningOutcome, graph.paths.getGraph(),tests);
				if(this.percent == 10)
					System.out.println();
				System.out.print(computeAccuracy(learningOutcome, graph.paths.getGraph(),tests)+",");
				//System.out.println(instanceID+","+result);
				//updateFrame(g,learningOutcome);
				l.setQuestionCounter(0);
				if (learningOutcome != null)
					stats = stats+(learningOutcome.containsUserDatumKey(JUConstants.STATS)? "\n"+learningOutcome.getUserDatum(JUConstants.STATS).toString():"");
				//System.out.println(inputFileName+" (instance "+instanceID+"), learner "+this+", "+ percent+"% terminated at "+Calendar.getInstance().getTime());
				currentOutcome = OUTCOME.SUCCESS;
			}
			catch(Throwable th)
			{
				StringWriter writer = new StringWriter();
				th.printStackTrace();
				th.printStackTrace(new PrintWriter(writer));
				stats = stats+"\nFAILED\nSTACK: "+writer.toString();
			}
			
			// now record the result
			stdOutput = writeResult(currentOutcome, result + "\n"+ stats);
			if (stdOutput != null) return stdOutput;
			return inputFileName+FS+percent+FS+currentOutcome;
		}
	}
	
	static double computeAccuracy(DirectedSparseGraph learned, DirectedSparseGraph correct, Collection<List<String>> tests){
		int failed = 0;
		for (List<String> list : tests) {
			Vertex hypVertex = RPNIBlueFringeLearnerOrig.getVertex(learned, list);
			Vertex correctVertex = RPNIBlueFringeLearnerOrig.getVertex(correct, list);
			if((hypVertex == null)&(correctVertex != null)){
				if(DeterministicDirectedSparseGraph.isAccept(correctVertex)){
					//updateFrame(learned, correct);
					failed ++;
				}
			}
			else if(hypVertex !=null & correctVertex!=null){
				if(DeterministicDirectedSparseGraph.isAccept(hypVertex) != DeterministicDirectedSparseGraph.isAccept(correctVertex)){
					//updateFrame(learned, correct);
					failed ++;
				}
			}
			else if(hypVertex!=null & correctVertex == null){
				if(DeterministicDirectedSparseGraph.isAccept(hypVertex)){
					//updateFrame(learned, correct);
					failed++;
				}
			}
				
		}
		double accuracy = 1-((double)failed/(double)tests.size());
		return accuracy;
	}
		
	@Override
	public int getStageNumber() {
		return 10;
	};

	public List<LearnerEvaluatorGenerator> getLearnerGenerators()
	{
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
						}
	
						@Override
						public String toString()
						{
							return "RPNI, POSITIVE_NEGATIVE";
						}
					};
				}
			}
		});
		/*,
		new LearnerEvaluatorGenerator() {
			@Override
			LearnerEvaluator getLearnerEvaluator(String inputFile, String outputDir, int percent, int instanceID) {
				return new RPNIEvaluator(inputFile,outputDir, percent, instanceID)
				{
					@Override
					protected void changeParametersOnLearner(RPNIBlueFringeLearner l)
					{
					}

					@Override
					protected void changeParametersOnComputeStateScores(ComputeStateScores c) 
					{
						c.setMode(IDMode.POSITIVE_ONLY);						
					}

					@Override
					public String toString()
					{
						return "RPNI, POSITIVE_ONLY";
					}
				};
			}
		},
		new LearnerEvaluatorGenerator() {
			@Override
			LearnerEvaluator getLearnerEvaluator(String inputFile, String outputDir, int percent, int instanceID) {
				return new RPNIEvaluator(inputFile,outputDir, percent, instanceID)
				{
					protected void changeParametersOnLearner(RPNIBlueFringeLearner l)
					{
					}

					@Override
					protected void changeParametersOnComputeStateScores(ComputeStateScores c) 
					{
						c.bumpPositive();
						c.setMode(IDMode.POSITIVE_ONLY);
					}

					@Override
					public String toString()
					{
						return "RPNI, POSITIVE_ONLY with a bump of 1";
					}
				};
			}
		},
		new LearnerEvaluatorGenerator() {
			@Override
			LearnerEvaluator getLearnerEvaluator(String inputFile, String outputDir, int percent, int instanceID) {
				return new RPNIEvaluator(inputFile,outputDir, percent, instanceID)
				{
					protected void changeParametersOnLearner(RPNIBlueFringeLearner l)
					{
					}

					@Override
					protected void changeParametersOnComputeStateScores(ComputeStateScores c) 
					{
						c.bumpPositive();c.useCompatibilityScore();
						c.setMode(IDMode.POSITIVE_ONLY);
					}

					@Override
					public String toString()
					{
						return "RPNI, POSITIVE_ONLY with a bump of 1 and compatibility scores";
					}
				};
			}
		}*/
		// at this point, one may add the above learners with different arguments or completely different learners such as the Angluin's one
	}
		
}
