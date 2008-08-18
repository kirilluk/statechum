/**
 * 
 */
package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.Visualiser;

/**
 * @author kirr
 *
 */
public class TestChangePrediction 
{
	public static void main(String str[])
	{// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("A---a--->B-a->C-b->C-c->B\nA-d->C", "simple_graph"),config);
		LearnerGraph graph2 = new LearnerGraph(TestFSMAlgo.buildGraph("A---a--->B", "simple_graph2"),config);
		LearnerGraph graph3 = new LearnerGraph(TestFSMAlgo.buildGraph("A---a--->A", "simple_graph3"),config);
		
		Map<String,Set<String>> transitionsUsedInC = new TreeMap<String,Set<String>>();
		for(CmpVertex v:graph.transitionMatrix.keySet())
			transitionsUsedInC.put(v.toString(),new TreeSet<String>());
		Map<String,String> vertexExtraLabel = new TreeMap<String,String>();
		Collection<List<String>> wSet = WMethod.computeWSet_reducedmemory(graph);
		for(Entry<CmpVertex,LinkedList<String>> entry:graph.wmethod.computeShortPathsToAllStates().entrySet())
		{
			CmpVertex v = graph.init;
			for(String input:entry.getValue())
			{
				transitionsUsedInC.get(v.toString()).add(input);v=graph.transitionMatrix.get(v).get(input);
			}
		}
		
		for(CmpVertex vert:graph.transitionMatrix.keySet())
		{
			int counter = 0;;
			for(List<String> seq:wSet)
				if (graph.paths.tracePath(seq,vert) == AbstractOracle.USER_ACCEPTED) ++counter;
			
			vertexExtraLabel.put(vert.toString(), "\n"+counter+" seq");
		}
		System.out.println(transitionsUsedInC+"\n###################\n"+vertexExtraLabel);
		
		DirectedSparseGraph gr = graph.paths.getGraph();
		gr.addUserDatum("VERTEX", vertexExtraLabel, UserData.CLONE);
		gr.addUserDatum("EDGE", transitionsUsedInC, UserData.CLONE);
		Visualiser.updateFrame(gr, null);
		Visualiser.updateFrame(graph2, graph3);
	}
}
