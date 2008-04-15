/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

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

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.model.testset.PTASequenceEngine;

/**
 * @author kirill
 *
 */
public class WExperiment extends AbstractExperiment {
	public static ArrayList<LearnerGraph> graphs = new ArrayList<LearnerGraph>();
	
	/** This one is not static because it refers to the frame to display results. */
	public static abstract class WEvaluator extends LearnerEvaluator
	{
		protected Collection<List<String>> tests = null;
		protected PTASequenceEngine pta = null;
		
		public WEvaluator(String inputFile, int per, int instance, AbstractExperiment exp)
		{
			super(inputFile, per,instance, exp);			
		}

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParameters(Configuration c);

		public String call()
		{
			OUTCOME currentOutcome = OUTCOME.FAILURE;
			String stdOutput = writeResult(currentOutcome,null);// record the failure result in case something fails later and we fail to update the file, such as if we are killed or run out of memory
			if (stdOutput != null) return stdOutput;
			loadGraph();

			String result = "";
			String stats = "";//"Instance: "+instanceID+ "\n";
			try
			{
				changeParameters(config);
//				result = result+l.getQuestionCounter()+FS+computeAccuracy(learningOutcome, graph.paths.getGraph(),tests);
				synchronized (graphs) {
					graphs.add(graph);
				}
				//AddTransitions addTr = new AddTransitions(graph);
				//addTr.populatePairToNumber();
				result+="\n4,5,"+percent;
				//result += addTr.toOctaveMatrix();//addTr.checkWChanged()+"\n"+addTr.ComputeHamming(true);
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

	/* (non-Javadoc)
	 * @see statechum.analysis.learning.experiments.AbstractExperiment#getLearnerGenerators()
	 */
	@Override
	public List<LearnerEvaluatorGenerator> getLearnerGenerators() {
		return Arrays.asList(new LearnerEvaluatorGenerator[] {
				new LearnerEvaluatorGenerator() {
					@Override
					LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, AbstractExperiment exp) {
						return new WEvaluator(inputFile, percent, instanceID, exp)
						{
							@Override
							protected void changeParameters(@SuppressWarnings("unused")	Configuration c) 
							{
							}
		
							@Override
							public String toString()
							{
								return evaluatorName();
							}
						};
					}

					@Override
					String evaluatorName() {
						return "W_learner";
					}
				}});
	}

	/* (non-Javadoc)
	 * @see statechum.analysis.learning.experiments.AbstractExperiment#getStageNumber()
	 */
	@Override
	public int getStageNumber() {
		return 1;
	}

	public static void main(String []args)
	{
		new WExperiment().runExperiment(args);
		
		// at this point, we've got all the graphs, hence try to merge states in them.
		LearnerGraph result = graphs.get(0);
		Random rnd = new Random(0);
		int graphNumber = 1;
		for(LearnerGraph gr:graphs)
			if (gr != result)
			{
				Transform.relabel(gr, 13, "gr_"+graphNumber++);
				CmpVertex newInit = Transform.addToGraph(result, gr);
				int score = -1;
				do
				{
					CmpVertex vertResult = Transform.pickRandomState(result,rnd);
					StatePair whatToMerge = new StatePair(vertResult,newInit);
					LinkedList<Collection<CmpVertex>> collectionOfVerticesToMerge = new LinkedList<Collection<CmpVertex>>();
					score = result.pairscores.computePairCompatibilityScore_general(whatToMerge,collectionOfVerticesToMerge);
					if (score >= 0) result = MergeStates.mergeAndDeterminize_general(result, whatToMerge,collectionOfVerticesToMerge);
					else throw new IllegalArgumentException("failed to merge states");// no easy way to restart with a different pair since result has already been modified, should've cloned perhaps, but absence of negative states ensures that no failure is possible.
				}
				while(score < 0);
				System.out.println(result.toString());
			}
		System.out.println();
		System.out.println(result.toString());
		//Collection<List<String>> wset = WMethod.computeWSet(result);
		//System.out.println(" w set size: "+wset.size());
		
		// Now start to merge some of those states
		
		try {
			result.transform.writeGraphML("resources/tmp/experiment_tmpresult.xml");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}			
		//AddTransitions.writeGraphML(graph, "resources/tmp/"+new File(inputFileName).getName()+"_tmpresult.xml");
	}
}
