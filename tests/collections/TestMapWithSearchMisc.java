package collections;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import junit.framework.Assert;

import org.junit.Test;

import collections.TestMapWithSearch.CInteger;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.collections.ArrayMapWithSearchPos;


/** Tests a few methods that are unique to some of the collections. */
public class TestMapWithSearchMisc {
	
	@Test
	public void testCannotAddNegativesToPosMap()
	{
		final ArrayMapWithSearchPos<CInteger, Long> ourMap = new ArrayMapWithSearchPos<CInteger, Long>();
		statechum.Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			ourMap.put(new CInteger(-2), 5L);
		}},IllegalArgumentException.class,"negative indices are not supported");
	}
	
	@Test
	public void testFindByID1()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);
		LearnerGraph g = buildLearnerGraph("A-a->A-b->B",	"testToBooleans",config,null);
		Assert.assertEquals("A",g.findVertex(VertexID.parseID("A")).getStringId());
		Assert.assertEquals("B",g.findVertex(VertexID.parseID("B")).getStringId());
		Assert.assertNull(g.findVertex(VertexID.parseID("AB")));
	}

}
 