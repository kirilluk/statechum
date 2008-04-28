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

import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.Random;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.LSolver;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import edu.uci.ics.jung.io.GraphMLFile;

public class Benchmarklinear {
	public static void main(String[] args)
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = null;
		synchronized (LearnerGraph.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
	    	graph = new LearnerGraph(graphmlFile.load(
	    			"resources/failedmerge_P1153_P1062.xml"
	    			),config);
		}
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
	}
	
	public static void mainA(String[] args)
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
		LSolver sl  = graph.linear.buildMatrix(ThreadNumber);
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
