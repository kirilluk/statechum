/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
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
package statechum.analysis.learning;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.io.GraphMLFile;

import statechum.JUConstants;
import statechum.analysis.learning.experiments.ExperimentGraphMLHandler;

/**
 * @author kirill
 *
 */
public class CheckComputeQuestions {
	public static DirectedSparseGraph loadGraph(String fileName)
	{
		DirectedSparseGraph graph = null;
		synchronized (computeStateScores.syncObj) 
		{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
	    	graph = new DirectedSparseGraph();
	    	graph.getEdgeConstraints().clear();
	    	graph = (DirectedSparseGraph)graphmlFile.load(fileName);
		}
		return graph;
	}
	
	public static void dumpQuestionsFrom(String A,String B)
	{
		computeStateScores original = new computeStateScores(loadGraph(A),"JUNK"); 
		computeStateScores merged = new computeStateScores(loadGraph(B),"JUNK");
		computeStateScores.PairScore pair = new computeStateScores.PairScore(RPNIBlueFringeLearner.findVertex(JUConstants.LABEL,"P91",original.getGraph()),
				RPNIBlueFringeLearner.findVertex(JUConstants.LABEL,"P38",original.getGraph()),7,0);
		System.out.println(RPNIBlueFringeLearnerTestComponentOpt.sort(original.computeQS(pair, merged)));		
	}
	
	public static void main(String [] args)
	{
		//dumpQuestionsFrom("/home/kirill/workspace/XMachineTool/original.xml","/home/kirill/workspace/XMachineTool/transformed.xml");
		
		dumpQuestionsFrom("orig_1.xml","temp_1.xml");
	}
}
