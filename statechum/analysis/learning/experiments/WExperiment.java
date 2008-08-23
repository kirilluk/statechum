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
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.experiments.ExperimentRunner.GeneratorConfiguration;
import statechum.analysis.learning.experiments.ExperimentRunner.LearnerEvaluator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.model.testset.PTASequenceEngine;

/**
 * @author kirill
 *
 */
public class WExperiment {
	public static ArrayList<LearnerGraph> graphs = new ArrayList<LearnerGraph>();
	
	/** This one is not static because it refers to the frame to display results. */
	public static abstract class WEvaluator extends LearnerEvaluator
	{
		protected Collection<List<String>> tests = null;
		protected PTASequenceEngine pta = null;
		
		public WEvaluator(String inputFile, int per, int instance, ExperimentRunner exp, Configuration cnf, String name)
		{
			super(inputFile, per,instance, exp, cnf, name);			
		}

		public void runTheExperiment()
		{
//				result = result+l.getQuestionCounter()+FS+computeAccuracy(learningOutcome, graph.paths.getGraph(),tests);
				synchronized (graphs) {
					graphs.add(graph);
				}
				//AddTransitions addTr = new AddTransitions(graph);
				//addTr.populatePairToNumber();
				result+="\n4,5,"+percent;
				//result += addTr.toOctaveMatrix();//addTr.checkWChanged()+"\n"+addTr.ComputeHamming(true);
		}
	}

	public static void main(String []args)
	{
		try {
			ExperimentRunner experiment = new ExperimentRunner();
			experiment.addLearnerEvaluator(new GeneratorConfiguration(Configuration.getDefaultConfiguration(),WEvaluator.class,"W_learner"));
			experiment.runExperiment(args);
		} catch (Exception e1) {
			e1.printStackTrace();
			return;
		}
		
		// at this point, we've got all the graphs, hence try to merge states in them.
		LearnerGraph result = graphs.get(0);
		Random rnd = new Random(0);
		int graphNumber = 1;
		for(LearnerGraph gr:graphs)
			if (gr != result)
			{
				Transform.relabel(gr, 1, "gr_"+graphNumber++);
				CmpVertex newInit = Transform.addToGraph(result, gr,null);
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
				
				//if (result.getStateNumber() > 1000) break;
			}
		System.out.println();
		System.out.println(result.toString());
		
		
		//long tmStarted = new Date().getTime();HashSet<List<String>> outcome = new HashSet<List<String>>();
		//result.linear.buildMatrix(AbstractExperiment.getCpuNumber());
		//Collection<List<String>> wset = WMethod.computeWSetOrig(result);outcome.clear();outcome.addAll(wset);
		//WMethod.computeWSet_reducedmemory(result);
		//long tmFinished = new Date().getTime();
		/*
		System.out.println(" w set size: "+outcome.size()+" time taken: "+(tmFinished-tmStarted)/1000);
		tmStarted = new Date().getTime();
		wset = WMethod.computeWSet_reducedmemory(result);outcome.clear();outcome.addAll(wset);
		tmFinished = new Date().getTime();
		System.out.println(" w set size: "+outcome.size()+" time taken: "+(tmFinished-tmStarted)/1000);
		// Now start to merge some of those states
		*/
		try {
			//Writer writer = new FileWriter("resources/tmp/experiment");
			//result.buildCachedData();
			//result.transform.toOctaveMatrix(writer);writer.close();
			
			//System.out.println("time taken: "+(tmFinished-tmStarted)/1000);
			result.transform.writeGraphML("../../W_experiment/experiment_5000.xml");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}			
		//AddTransitions.writeGraphML(graph, "resources/tmp/"+new File(inputFileName).getName()+"_tmpresult.xml");
	}
}
