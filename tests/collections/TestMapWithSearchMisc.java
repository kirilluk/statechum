package collections;

import collections.TestMapWithSearch.CInteger;
import org.junit.Assert;
import org.junit.Test;
import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.collections.ArrayMapWithSearchPos;

import static statechum.analysis.learning.rpnicore.FsmParserStatechum.buildLearnerGraph;


/** Tests a few methods that are unique to some of the collections. */
public class TestMapWithSearchMisc {
	
	@SuppressWarnings("MismatchedQueryAndUpdateOfCollection")
	@Test
	public void testCannotAddNegativesToPosMap()
	{
		final ArrayMapWithSearchPos<CInteger,CInteger, Long> ourMap = new ArrayMapWithSearchPos<>();
		statechum.TestHelper.checkForCorrectException(() -> ourMap.put(new CInteger(-2), 5L),
				IllegalArgumentException.class,"negative indices are not supported");
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
 