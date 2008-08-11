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

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Random;

import statechum.Configuration;
import statechum.Pair;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.rpnicore.LSolver;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTA_FSMStructure;
import statechum.model.testset.PTA_computePrecisionRecall;
import statechum.model.testset.PTASequenceEngine.SequenceSet;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.io.GraphMLFile;

public class Benchmarklinear {
	static LearnerGraph graph = null;

	public static void main(String[] args)
	{
		long tmStarted = new Date().getTime(),tmFinished = 0;;
		int buffer[] = new int[10000000];
		Arrays.fill(buffer,1);
		tmFinished = new Date().getTime();
		System.out.println("filled: "+((double)tmFinished-tmStarted)/1000+" sec");tmStarted=tmFinished;
	}
	
	public static void maina(String[] args)
	{
		Configuration config = Configuration.getDefaultConfiguration();
		//config.setConsistencyCheckMode(true);
		//config.setQuestionGenerator(QuestionGeneratorKind.SYMMETRIC);
		synchronized (LearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
	    	graph = new LearnerGraph(graphmlFile.load(
	    			//"resources/pcodaExperiments/5StatesCrowded/5Inputs_15_13.xml"
	    			"resources/pcodaExperiments/25StatesCrowded/25Inputs_75_12.xml"
	    			),config);
		}
		
		int size = 4*graph.getStateNumber();
		RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),5);// the seed for Random should be the same for each file
		int percentPerChunk = 10;
		int nrPerChunk = size/(100/percentPerChunk);nrPerChunk+=nrPerChunk % 2;// make the number even
		rpg.generatePosNeg(nrPerChunk , 100/percentPerChunk);
		
		RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null,config)
		{
			@Override
			protected Pair<Integer,String> checkWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					List<String> question, 
					@SuppressWarnings("unused") final Object [] moreOptions)
			{
				return new Pair<Integer,String>(graph.paths.tracePath(question),null);
			}
		};
		PTASequenceEngine sPlus = null, sMinus = null;int percent = 10;
		sPlus = rpg.getExtraSequences(percent/10-1);sMinus = rpg.getAllSequences(percent/10-1);

		int ptaSize = sMinus.numberOfLeafNodes();
		l.init(sMinus, ptaSize,ptaSize);// our imaginary positives are prefixes of negatives.
		DirectedSparseGraph learningOutcome = l.learnMachine();
		l.setQuestionCounter(0);
		LearnerGraph learned = new LearnerGraph(learningOutcome,config);
		PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(learned);
		PTASequenceEngine engine = new PTA_FSMStructure(graph);
		precRec.crossWith(sMinus);
		SequenceSet ptaTestSet = engine.new SequenceSet();ptaTestSet.setIdentity();
		ptaTestSet = ptaTestSet.cross(graph.wmethod.getFullTestSet(1));
		PosNegPrecisionRecall prNeg = precRec.crossWith(engine);
		System.out.println("pre: "+prNeg.getPrecision()+" rec: "+prNeg.getRecall());
		/*
		LearnerGraph.testMode=true;
		StatePair p = new StatePair(graph.findVertex("P1062"),graph.findVertex("P1153"));
		System.out.println("score: "+graph.pairscores.computePairCompatibilityScore(p));
		graph.merger.mergeAndDeterminize(graph, p);
		//System.out.println("score: "+graph.pairscores.computePairCompatibilityScore(new StatePair(graph.findVertex("P1153"),graph.findVertex("P1062"))));
		Collection<Collection<CmpVertex>> mergedVertices = new LinkedList<Collection<CmpVertex>>();
		System.out.println("scoreG: "+graph.pairscores.computePairCompatibilityScore_general(p, mergedVertices));
		System.out.println("scoreG: "+graph.pairscores.computePairCompatibilityScore_general(new StatePair(graph.findVertex("P1153"),graph.findVertex("P1062")), mergedVertices));
		//System.out.println("scoreG: "+graph.pairscores.computePairCompatibilityScore_general(p, mergedVertices));
		//PairScore pair = new PairScore(graph.findVertex("P1153"),graph.findVertex("P1062"));
		 */
	}
	
	public static void mainA(@SuppressWarnings("unused") String[] args)
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = null;
		int ThreadNumber=AbstractExperiment.getCpuNumber();
		long tmStarted = new Date().getTime(),tmFinished = 0;;
		synchronized (LearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
	    	graph = new LearnerGraph(graphmlFile.load(
	    			//"../../W_experiment/experiment_4300.xml"
	    			//"../W_experiment/3000/N_3000_1.xml"
	    			"resources/LargeGraphs/experiment_1000.xml"
	    			),config);
		}
		tmFinished = new Date().getTime();
		System.out.println("graph loaded: "+((double)tmFinished-tmStarted)/1000+" sec");tmStarted=tmFinished;
		Random rnd = new Random(0);
		for(int i=0;i<1000;++i) graph.transform.addRejectStateRandomly(rnd);
		//for(int i=0;i<5000;++i) graph.transform.addRejectStateRandomly(rnd);
		tmFinished = new Date().getTime();
		System.out.println("random reject vertices added: "+((double)tmFinished-tmStarted)/1000+" sec");tmStarted=tmFinished;
		LearnerGraphND ndGraph = new LearnerGraphND(graph,LearnerGraphND.ignoreRejectStates,false);
		LSolver sl  = ndGraph.buildMatrix(ThreadNumber);
		//Collection<List<String>> wset = WMethod.computeWSetOrig(result);outcome.clear();outcome.addAll(wset);
		//WMethod.computeWSet_reducedmemory(result);
		tmFinished = new Date().getTime();
		System.out.println("matrix built: "+((double)tmFinished-tmStarted)/1000+" sec");tmStarted=tmFinished;
		System.out.println("size: "+(sl.j_Ap.length-1)+" non-zeroes: "+sl.j_Ap[sl.j_Ap.length-1]);
		tmStarted = new Date().getTime();
		sl.solve();
		tmFinished = new Date().getTime();
		System.out.println("solver time taken: "+((double)tmFinished-tmStarted)/1000+" sec");
	}
}
