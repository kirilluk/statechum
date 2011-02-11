package statechum.analysis.learning.rpnicore;

import static statechum.analysis.learning.rpnicore.FsmParser.buildGraph;
import static statechum.analysis.learning.rpnicore.GDLearnerGraph.PAIR_OK;

import statechum.Configuration;

public class DumpEq {

	/**
	 * @param args
	 */
	public static void main(String[] args) 
	{
		Configuration config = Configuration.getDefaultConfiguration();
		GDLearnerGraph ndGraph = new GDLearnerGraph(new LearnerGraph(buildGraph("A-a->B-a->B-b->A / B-c->C / E-a->F-a->F-d->F-b->E-c->F",	"testBuildMatrix1"),config),LearnerGraphND.ignoreRejectStates, false);
		final int [] incompatiblePairs = new int[ndGraph.getPairNumber()];
		for(int i=0;i<incompatiblePairs.length;++i) incompatiblePairs[i]=PAIR_OK;
		
		final int pairsNumber = ndGraph.findIncompatiblePairs(incompatiblePairs,1);
		LSolver solver = ndGraph.buildMatrix_internal(incompatiblePairs, pairsNumber, 1,null);

		String outcome = ndGraph.dumpEquations(solver, incompatiblePairs, null).toString();
		System.out.println(outcome);
	}

}
