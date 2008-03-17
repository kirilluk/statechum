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


import java.util.*;
import edu.uci.ics.jung.graph.impl.*;
import statechum.Configuration;
import statechum.Configuration.IDMode;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.xmachine.model.testset.*;
import statechum.xmachine.model.testset.PTASequenceEngine.SequenceSet;

public class IncrementalAccuracyAndQuestionsExperiment extends AbstractExperiment 
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
		public String call()
		{
			//System.out.println(inputFileName+" (instance "+instanceID+"), learner "+this+", "+percent + "% started at "+Calendar.getInstance().getTime());
			OUTCOME currentOutcome = OUTCOME.FAILURE;
			String stdOutput = writeResult(currentOutcome,null);// record the failure result in case something fails later and we fail to update the file, such as if we are killed or run out of memory
			if (stdOutput != null) return stdOutput;
			loadGraph();
			int size = 4*graph.getStateNumber();
			RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),5);// the seed for Random should be the same for each file			
			rpg.generatePosNeg(size/experiment.getStageNumber(), experiment.getStageNumber());
			RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null,config)
			{
				protected int checkWithEndUser(
						@SuppressWarnings("unused")	DirectedSparseGraph model,
						List<String> question, 
						@SuppressWarnings("unused") final Object [] moreOptions)
				{
					return graph.paths.tracePath(question);
				}
			};
			sPlus = rpg.getExtraSequences(percent);sMinus = rpg.getAllSequences(percent);
			questionsExperiment(graph, l);
			
			return inputFileName+"success";
		}

		private void questionsExperiment(LearnerGraph fsm, RPNIBlueFringeLearnerTestComponentOpt l){
			PosNegPrecisionRecall prNeg = computePR(fsm, l, sMinus);
			System.out.println(prNeg.precision+", "+prNeg.recall);
		}
		
		private void posNegExperiment(LearnerGraph fsm, RPNIBlueFringeLearnerTestComponentOpt l){
			PosNegPrecisionRecall prNeg = computePR(fsm, l, sMinus);
			PosNegPrecisionRecall pr= computePR(fsm, l, sPlus);
			System.out.println(pr.getPosprecision()+", "+pr.getPosrecall()/*+", "+pr.getNegprecision()+", "+pr.getNegrecall()+", "+prNeg.getPosprecision()+", "+prNeg.getPosrecall()+", "+prNeg.getNegprecision()+", "+prNeg.getNegrecall()*/);
		}
		
		private PosNegPrecisionRecall computePR(LearnerGraph fsm, RPNIBlueFringeLearnerTestComponentOpt l, 
				PTASequenceEngine pta)
		{// FIXME: need to mark rpg splus/sminus elements as learnt via partialPTA.cross(sminus), since splus consists of prefixes of sminus
			LearnerGraph learned = learn(l,pta);
			PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(learned);
			PTASequenceEngine engine = new PTA_FSMStructure(fsm);
			SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
			partialPTA = partialPTA.cross(fsm.wmethod.getFullTestSet(1));
			return precRec.crossWith(engine);
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
	
	public int getStageNumber()
	{
		return 1;
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
	}

}
