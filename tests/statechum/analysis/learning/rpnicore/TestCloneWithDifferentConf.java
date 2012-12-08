package statechum.analysis.learning.rpnicore;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.analysis.learning.TestRpniLearner;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

/** Tests that cloning and comparison of both deterministic and non-deterministic graphs works. */
@RunWith(Parameterized.class)
public final class TestCloneWithDifferentConf 
{

	@SuppressWarnings("rawtypes")
	private final AbstractLearnerGraph graph;
	@SuppressWarnings("rawtypes")
	private final List<AbstractLearnerGraph> sameA,sameB,different;
	
	@SuppressWarnings("rawtypes")
	public TestCloneWithDifferentConf(AbstractLearnerGraph argGraph, 
			List<AbstractLearnerGraph> argSameA,List<AbstractLearnerGraph> argSameB,List<AbstractLearnerGraph> argDifferent) {
		graph = argGraph;sameA=argSameA;sameB=argSameB;different=argDifferent;
	}
	
		
	// AbstractLearnerGraph is polymorphic in this context: I can use both deterministic and non-deterministic graphs.
	@SuppressWarnings("rawtypes")
	@Parameters
	public static Collection<Object[]> data() 
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();//config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		DirectedSparseGraph graphD=FsmParser.buildGraph("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+TestRpniLearner.PTA3, "testCopyGraph2",config);
		DirectedSparseGraph graphND=FsmParser.buildGraph("S-a->S\nA1-a->A2\nS-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+TestRpniLearner.PTA3, "testCopyGraph4",config);

		Configuration confJung = config.copy();confJung.setLearnerUseStrings(false);confJung.setLearnerCloneGraph(true);confJung.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);
		Configuration confString = config.copy();confString.setLearnerUseStrings(true);confString.setLearnerCloneGraph(true);confString.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);
		Configuration confSame = config.copy();confSame.setLearnerUseStrings(false);confSame.setLearnerCloneGraph(false);confSame.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);
		Configuration confStringCompat = config.copy();confStringCompat.setLearnerUseStrings(true);confStringCompat.setLearnerCloneGraph(true);confStringCompat.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);

		Configuration configsOfInterest[] = new Configuration[]{confJung,confSame,confString,confStringCompat}; 

		List<AbstractLearnerGraph> sameGraphsA = new LinkedList<AbstractLearnerGraph>(),sameGraphsB = new LinkedList<AbstractLearnerGraph>();
		for(int configAidx=0;configAidx < configsOfInterest.length;++configAidx)
		{
			Configuration configA = configsOfInterest[configAidx];
			LearnerGraph origD = new LearnerGraph(graphD,configA);origD.setName("origD"+configAidx);
			LearnerGraphND origND = new LearnerGraphND(graphND,configA);origND.setName("origND"+configAidx);
			sameGraphsA.add(origD);sameGraphsB.add(origND);
			
			for(int configBidx=0;configBidx < configsOfInterest.length;++configBidx)
			{
				Configuration configB = configsOfInterest[configBidx];
				LearnerGraph copyD = new LearnerGraph(origD,configB);copyD.setName("copyD"+configAidx+configBidx);
				LearnerGraphND copyND = new LearnerGraphND(origND,configB);copyND.setName("copyND"+configAidx+configBidx);
				sameGraphsA.add(copyD);sameGraphsB.add(copyND);
		
				for(int configCidx=0;configCidx < configsOfInterest.length;++configCidx)
				{
					Configuration configC = configsOfInterest[configCidx];
					LearnerGraph ggD = new LearnerGraph(origD.pathroutines.getGraph(),configC),
						ggCopyD = new LearnerGraph(copyD.pathroutines.getGraph(),configC);
					ggD.setName("origD:"+configAidx+","+configBidx+","+configCidx+".getGraph");ggCopyD.setName("copyD:"+configAidx+","+configBidx+","+configCidx+".getGraph");
					LearnerGraphND ggND = new LearnerGraphND(origND.pathroutines.getGraph(),configC),
						ggCopyND = new LearnerGraphND(copyND.pathroutines.getGraph(),configC);
					ggND.setName("origND:"+configAidx+","+configBidx+","+configCidx+".getGraph");ggCopyND.setName("copyND:"+configAidx+","+configBidx+","+configCidx+".getGraph");
					sameGraphsA.add(ggD);sameGraphsB.add(ggND);
					sameGraphsA.add(ggCopyD);sameGraphsB.add(ggCopyND);
				}
			}
		}
		

		LearnerGraph differentGraphA = new LearnerGraph(graphD,config);differentGraphA.transitionMatrix.get(differentGraphA.findVertex("A2"))
			.put(AbstractLearnerGraph.generateNewLabel("t",differentGraphA.config),
					differentGraphA.findVertex("A3"));
		differentGraphA.setName("differentA");
		LearnerGraphND differentGraphB = new LearnerGraphND(graphND,config);differentGraphB.transitionMatrix.get(differentGraphB.findVertex("A2"))
			.get(AbstractLearnerGraph.generateNewLabel("c",differentGraphB.config))
			.add(differentGraphB.findVertex("S"));
		differentGraphB.setName("differentB");
		
		List<AbstractLearnerGraph> different = new LinkedList<AbstractLearnerGraph>();different.addAll(Arrays.asList(new AbstractLearnerGraph[]{
				differentGraphA,differentGraphB
		}));

		Collection<Object[]> result = new LinkedList<Object []>();
		
		for(AbstractLearnerGraph A:sameGraphsA)
			result.add(new Object[]{A,sameGraphsA,sameGraphsB,different});
			/*
			for(AbstractLearnerGraph B:sameGraphsA)
				for(AbstractLearnerGraph diffA:sameGraphsB)
					for(AbstractLearnerGraph diffB:different)
						TestEqualityComparisonAndHashCode.equalityTestingHelper(A,B,diffA,diffB);
						*/

		for(AbstractLearnerGraph A:sameGraphsB)
			result.add(new Object[]{A,sameGraphsB,sameGraphsA,different});
			
			/*
			for(AbstractLearnerGraph B:sameGraphsB)
				for(AbstractLearnerGraph diffA:sameGraphsA)
					for(AbstractLearnerGraph diffB:different)
						TestEqualityComparisonAndHashCode.equalityTestingHelper(A,B,diffA,diffB);
						*/
		
		return result;
	}

	/** Given a test configuration, returns a textual description of its purpose. 
	 * 
	 * @param config configuration to consider
	 * @return description.
	 */ 
	@SuppressWarnings("rawtypes")
	public static String parametersToString(LearnerGraph argGraph, 
			LinkedList<AbstractLearnerGraph> argSameA,LinkedList<AbstractLearnerGraph> argSameB,LinkedList<AbstractLearnerGraph> argDifferent)
	{
		return argGraph.getName()+" with "+argSameA.size()*argSameB.size()*argDifferent.size()+" graphs";
	}
	
	/** Given a test configuration, returns a textual description of its purpose. 
	 * 
	 * @param config configuration to consider
	 * @return description.
	 */ 
	@SuppressWarnings("rawtypes")
	public static String parametersToString(LearnerGraphND argGraph, 
			LinkedList<AbstractLearnerGraph> argSameA,LinkedList<AbstractLearnerGraph> argSameB,LinkedList<AbstractLearnerGraph> argDifferent)
	{
		return argGraph.getName()+" with "+argSameA.size()*argSameB.size()*argDifferent.size()+" graphs";
	}
	
	@SuppressWarnings("rawtypes")
	@Test
	public final void testCopyGraph() // this one tests that clone works
	{
			for(AbstractLearnerGraph B:sameA)
				for(AbstractLearnerGraph diffA:sameB)
					for(AbstractLearnerGraph diffB:different)
						TestEqualityComparisonAndHashCode.equalityTestingHelper(graph,B,diffA,diffB, true);
	}	
}
