package statechum.analysis.learning.experiments;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.util.OutputUtil;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;
import edu.uci.ics.jung.visualization.VisualizationViewer;

public class ForestFireIntermediateNegativesGenerator extends
		ForestFireLabelledStateMachineGenerator {

	public ForestFireIntermediateNegativesGenerator(double forwards,
			double backwards, int alphabetSize, int seed) throws Exception {
		super(forwards, backwards, alphabetSize, seed);
	}

	protected LearnerGraph buildMachine(int size) throws Exception{
		boolean accept = true;
		labelmap = new HashMap<Object,DirectedSparseVertex>();
		for(int i=0;i<size-1;i++){
			DirectedSparseVertex v =  new DirectedSparseVertex();
			//visited.add(v);  COMMENTED OUT TO ENABLE LOOPS
			String label = String.valueOf(i+1);
			v.setUserDatum(JUConstants.LABEL, label, UserData.SHARED);
			v.setUserDatum(JUConstants.ACCEPTED, accept, UserData.SHARED);
			machine.addVertex(v);
			this.labelmap.put(label, v);
			HashSet tried = new HashSet<DirectedSparseVertex>();
			DirectedSparseVertex random = selectRandom();
			tried.add(random);
			while(!addEdge(random,v)){
				random = selectRandom(tried);
				tried.add(random);
				if(random == null){
					System.out.println("Could not construct complete machine");
					machine.removeVertex(v);
					return new LearnerGraph(machine,Configuration.getDefaultConfiguration());
				}
			}
			visited.add(random);
			spread(v,random);
			vertices.add(v);
			visited.clear();
			accept = !accept;
		}
		
		Configuration conf = Configuration.getDefaultConfiguration();
		conf.setAllowedToCloneNonCmpVertex(true);
		
		LearnerGraph l = new LearnerGraph(machine,conf);
		l = l.paths.reduce();
		System.out.println(l.pathroutines.computeAlphabet().size() + ", "+l.getStateNumber());
		Visualiser.updateFrame(l, null);
		return l;
	}

}
