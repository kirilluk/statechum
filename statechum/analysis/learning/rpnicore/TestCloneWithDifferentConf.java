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
import statechum.analysis.learning.TestRpniLearner;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

/** Tests that cloning and comparison of both deterministic and non-deterministic graphs works. */
@RunWith(Parameterized.class)
@SuppressWarnings("unchecked")
public final class TestCloneWithDifferentConf 
{

	private final AbstractLearnerGraph graph;
	private final List<AbstractLearnerGraph> sameA,sameB,different;
	
	public TestCloneWithDifferentConf(AbstractLearnerGraph argGraph, 
			List<AbstractLearnerGraph> argSameA,List<AbstractLearnerGraph> argSameB,List<AbstractLearnerGraph> argDifferent) {
		graph = argGraph;sameA=argSameA;sameB=argSameB;different=argDifferent;
	}
	
	@Parameters
	public static Collection<Object[]> data() 
	{
		DirectedSparseGraph graphD=FsmParser.buildGraph("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+TestRpniLearner.PTA3, "testCopyGraph2");
		DirectedSparseGraph graphND=FsmParser.buildGraph("S-a->S\nA1-a->A2\nS-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+TestRpniLearner.PTA3, "testCopyGraph4");
		
		Configuration config = Configuration.getDefaultConfiguration().copy();

		Configuration confJung = config.copy();confJung.setLearnerUseStrings(false);confJung.setLearnerCloneGraph(true);
		Configuration confString = config.copy();confString.setLearnerUseStrings(true);confString.setLearnerCloneGraph(true);
		Configuration confSame = config.copy();confSame.setLearnerUseStrings(false);confSame.setLearnerCloneGraph(false);

		List<AbstractLearnerGraph> sameGraphsA = new LinkedList<AbstractLearnerGraph>(),sameGraphsB = new LinkedList<AbstractLearnerGraph>();
		for(Configuration configA:new Configuration[]{confJung,confSame,confString})
		{
			LearnerGraph origD = new LearnerGraph(graphD,configA);origD.setName("origD");
			LearnerGraphND origND = new LearnerGraphND(graphND,configA);origND.setName("origND");
			sameGraphsA.add(origD);sameGraphsB.add(origND);
			
			for(Configuration configB:new Configuration[]{confJung,confSame,confString})
			{
				LearnerGraph copyD = new LearnerGraph(origD,configB);copyD.setName("copyD");
				LearnerGraphND copyND = new LearnerGraphND(origND,configB);copyND.setName("copyND");
				sameGraphsA.add(copyD);sameGraphsB.add(copyND);
		
				for(Configuration configC:new Configuration[]{confJung,confSame,confString})
				{
					LearnerGraph ggD = new LearnerGraph(origD.pathroutines.getGraph(),configC),
						ggCopyD = new LearnerGraph(copyD.pathroutines.getGraph(),configC);
					ggD.setName("origD.getGraph");ggCopyD.setName("copyD.getGraph");
					LearnerGraphND ggND = new LearnerGraphND(origND.pathroutines.getGraph(),configC),
						ggCopyND = new LearnerGraphND(copyND.pathroutines.getGraph(),configC);
					ggND.setName("origND.getGraph");ggCopyND.setName("copyND.getGraph");
					sameGraphsA.add(ggD);sameGraphsB.add(ggND);
					sameGraphsA.add(ggCopyD);sameGraphsB.add(ggCopyND);
/*			
					for(Configuration configD:new Configuration[]{confJung,confSame,confString})
					{
						LearnerGraphND a=new LearnerGraphND(origD,configD);a.setName("origD.copy");
						LearnerGraphND b=new LearnerGraphND(copyD,configD);b.setName("copyD.copy");
						LearnerGraphND c=new LearnerGraphND(ggD,configD);c.setName("ggD.copy");
						LearnerGraphND d=new LearnerGraphND(ggCopyD,configD);d.setName("ggCopyD.copy");
						sameGraphsA.add(a);sameGraphsA.add(b);sameGraphsA.add(c);sameGraphsA.add(d);
					}
*/					
				}
			}
		}
		
/*
		List<AbstractLearnerGraph> sameGraphsA = Arrays.asList(new AbstractLearnerGraph[]{
				origD,copyD,ggD,ggCopyD,
				a,b,c,d
		});
		
		List<AbstractLearnerGraph> sameGraphsB = Arrays.asList(new AbstractLearnerGraph[]{
				origND,copyND,ggND,ggCopyND
		});
*/		
		LearnerGraph differentGraphA = new LearnerGraph(graphD,config);differentGraphA.transitionMatrix.get(differentGraphA.findVertex("A2")).put("t", differentGraphA.findVertex("A3"));
		differentGraphA.setName("differentA");
		LearnerGraphND differentGraphB = new LearnerGraphND(graphND,config);differentGraphB.transitionMatrix.get(differentGraphB.findVertex("A2")).get("c").add(differentGraphB.findVertex("S"));
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
	public static String parametersToString(LearnerGraphND argGraph, 
			LinkedList<AbstractLearnerGraph> argSameA,LinkedList<AbstractLearnerGraph> argSameB,LinkedList<AbstractLearnerGraph> argDifferent)
	{
		return argGraph.getName()+" with "+argSameA.size()*argSameB.size()*argDifferent.size()+" graphs";
	}
	
	@Test
	public final void testCopyGraph() // this one tests that clone works
	{
			for(AbstractLearnerGraph B:sameA)
				for(AbstractLearnerGraph diffA:sameB)
					for(AbstractLearnerGraph diffB:different)
						TestEqualityComparisonAndHashCode.equalityTestingHelper(graph,B,diffA,diffB);
	}	
}
