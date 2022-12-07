/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */ 

package statechum.analysis.learning.rpnicore;

import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;
import harmony.collections.HashMapWithSearch;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import statechum.*;
import statechum.AttributeMutator.MethodAndArgs;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.CmpVertex.IllegalUserDataException;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.DeterministicDirectedSparseGraph.VertID.VertKind;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.TestRpniLearner;
import statechum.analysis.learning.Test_Orig_RPNIBlueFringeLearner.OrigStatePair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph.PairCompatibility;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

import java.util.*;
import java.util.Map.Entry;

import static org.junit.Assert.*;
import static statechum.DeterministicDirectedSparseGraph.deepEquals;
import static statechum.Helper.checkForCorrectException;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraphND;

public class TestEqualityComparisonAndHashCode {
	public TestEqualityComparisonAndHashCode()
	{
		mainConfiguration = Configuration.getDefaultConfiguration();
		converter = mainConfiguration.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?new Transform.InternStringLabel():null;
	}
	
	/** Graph used in tests of cloning. */
	private LearnerGraph testGraph = null;

	/** The configuration to use when running tests. */
	Configuration config = null;

	final ConvertALabel converter;
	
	private Configuration mainConfiguration = null;
	
	/** Configuration settings used to test creation/cloning of graphs. */
	private Configuration confJung,confString,confSame;
	
	/** Used as arguments to equalityTestingHelper. */
	private LearnerGraph differentA = null, differentB = null;

	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public void beforeTest()
	{
		config = mainConfiguration.copy();config.setUseOrderedEntrySet(true);
		
		differentA = buildLearnerGraph("Q-a->A-b->B", "testFSMStructureEquals2",config,converter);
		differentB = buildLearnerGraph("A-b->A\nB-b->B", "testFSMStructureEquals2",config,converter);

		confJung = config.copy();confJung.setLearnerUseStrings(false);confJung.setLearnerCloneGraph(true);
		confString = config.copy();confString.setLearnerUseStrings(true);confString.setLearnerCloneGraph(true);
		confSame = config.copy();confSame.setLearnerUseStrings(false);confSame.setLearnerCloneGraph(false);
		testGraph = buildLearnerGraph("A-a->A-b->B-c->B\nA-c-#C\nB-b->B", "testFSMStructureClone",config,converter);
		testGraphJung=new LearnerGraph(testGraph,confJung.copy());// clone here makes it possible to change the configuration later without affecting objects in this object
		testGraphString=new LearnerGraph(testGraph,confString.copy());
		testGraphSame=new LearnerGraph(testGraph,confSame.copy());
	}
	
	/** FSMs used in testing of cloning. */
	private LearnerGraph testGraphJung,testGraphString,testGraphSame;

	static protected OrigStatePair constructOrigPair(String a, String b)
	{
		DirectedSparseVertex aV = new DirectedSparseVertex(), bV = new DirectedSparseVertex();
		aV.addUserDatum(JUConstants.LABEL, a, UserData.SHARED);
		bV.addUserDatum(JUConstants.LABEL, b, UserData.SHARED);
		return new OrigStatePair(aV,bV);
	}

	/** Used to check that equality checking is implemented correctly. 
	 * The first two arguments are supposed to be independently constructed and they 
	 * are checked for equality.
	 * The last two arguments are supposed to be different from any of the first two.
	 * 
	 * @param checkHashCodesDifferent whether to verify that hash codes are different. 
	 * They do not actually have to be for different objects, so where tests expects them to be we would like to check and not check otherwise.
	 */
	static public void equalityTestingHelper(Object p, Object q, Object differentA, Object differentB, boolean checkHashCodesDifferent)
	{
		assertEquals(p, p);
		assertEquals(q, q);
		if (!p.equals(q))
			//noinspection ResultOfMethodCallIgnored
			p.equals(q);// this line is to set a breakpoint on when debugging equality
		assertEquals("graphs differ: " + p + " and " + q, p, q);
		assertEquals("graphs differ: " + p + " and " + q, q, p);
		if (p.hashCode() != q.hashCode())
		{
			//noinspection ResultOfMethodCallIgnored
			p.hashCode();// this line is to set a breakpoint on when debugging hash code computation
			//noinspection ResultOfMethodCallIgnored
			q.hashCode();
		}
		assertEquals("hash codes differ", p.hashCode(), q.hashCode());
		assertTrue("p has a zero hash code",p.hashCode() != 0);assertTrue("p has a zero hash code",q.hashCode() != 0);
		assertNotEquals(null, p);
		assertNotEquals(null, q);

		assertNotEquals("test", p);
		assertNotEquals("test", q);
		assertNotEquals("test", p);
		assertNotEquals("test", q);
		
		Object obj = new Object();
		assertEquals(obj, obj);
		assertNotEquals(p, obj);
		assertNotEquals(q, obj);
		assertNotEquals(obj, p);
		assertNotEquals(obj, q);

		assertNotEquals(p, differentA);
		assertNotEquals(q, differentA);
		if (differentA != null)
		{
			assertNotEquals("graphs should not be equal: " + differentA + " and " + p, differentA, p);
			assertNotEquals("graphs should not be equal: " + differentA + " and " + q, differentA, q);
			if (checkHashCodesDifferent)
			{
				assertNotEquals(p.hashCode(), differentA.hashCode());
				assertNotEquals(q.hashCode(), differentA.hashCode());
			}
		}
		if (differentB != null)
		{
			assertNotEquals("graphs should not be equal: " + differentB + " and " + p, differentB, p);
			assertNotEquals("graphs should not be equal: " + differentB + " and " + q, differentB, q);
			if (checkHashCodesDifferent)
			{
				assertNotEquals(p.hashCode(), differentB.hashCode());
				assertNotEquals(q.hashCode(), differentB.hashCode());
			}
		}
	}
	
	/** Used to check that compareTo method works well.
	 * The first argument should be less that the second one. 
	 */ 
	@SuppressWarnings({"unchecked", "rawtypes", "EqualsWithItself"})
	static private void checkLessHelper(Comparable p, Comparable q)
	{
		assertNotEquals(p, q);
		assertNotEquals(p.hashCode(), q.hashCode());
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertNotEquals(p.hashCode(), q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}

	/** Tests that text IDs can be automatically converted into numeric ones. */
	@Test
	public final void testParseID1()
	{
		VertexID id = VertexID.parseID("this is a test");
		Assert.assertEquals(VertKind.NONE,id.getKind());
		Assert.assertEquals("this is a test",id.toString());
	}
	
	@Test
	public final void testParseID2()
	{
		VertexID id = VertexID.parseID("Pthis is a test");
		Assert.assertEquals(VertKind.NONE,id.getKind());
		Assert.assertEquals("Pthis is a test",id.toString());
	}
	
	@Test
	public final void testParseID3()
	{
		VertexID id = VertexID.parseID("P2this is a test");
		Assert.assertEquals(VertKind.NONE,id.getKind());
		Assert.assertEquals("P2this is a test",id.toString());
	}
	
	@Test
	public final void testParseID4()
	{
		VertexID id = VertexID.parseID("");
		Assert.assertEquals(VertKind.NONE,id.getKind());
		Assert.assertEquals("",id.toString());
	}
	
	@Test
	public final void testParseID5()
	{
		VertexID id = VertexID.parseID("P00");
		Assert.assertEquals(VertKind.POSITIVE,id.getKind());
		Assert.assertEquals("P0",id.toString());
		Assert.assertEquals(0, id.getIntegerID());
	}
	
	@Test
	public final void testParseID6()
	{
		VertexID id = VertexID.parseID("P100789");
		Assert.assertEquals(VertKind.POSITIVE,id.getKind());
		Assert.assertEquals("P100789",id.toString());
		Assert.assertEquals(100789, id.getIntegerID());
		Assert.assertEquals(100789, id.toInt());
	}
	
	@Test
	public final void testParseID7()
	{
		VertexID id = VertexID.parseID("N100789");
		Assert.assertEquals(VertKind.NEGATIVE,id.getKind());
		Assert.assertEquals("N100789",id.toString());
		Assert.assertEquals(100789, id.getIntegerID());
		Assert.assertEquals(-100789, id.toInt());
	}
	
	
	/** Tests that it is not possible to create an invalid VertexID. */
	@SuppressWarnings("unused")
	@Test(expected=IllegalArgumentException.class)
	public final void testCannotCreateNoneVertexID1()
	{
		new VertexID(VertKind.NONE,3);
	}
	
	/** Tests that it is not possible to create an invalid VertexID. */
	@SuppressWarnings({"unused", "ConstantConditions"})
	@Test(expected=IllegalArgumentException.class)
	public final void testCannotCreateNoneVertexID2a()
	{
		new VertexID((String)null);
	}
	
	/** Tests that it is not possible to create an invalid VertexID. */
	@SuppressWarnings({"unused", "ConstantConditions"})
	@Test(expected=IllegalArgumentException.class)
	public final void testCannotCreateNoneVertexID2b()
	{
		new VertexID((VertexID)null);
	}
	
	/** Tests equality for VertexIDs. */
	@Test
	public final void testVertexIDEquals1a()
	{
		equalityTestingHelper(VertexID.parseID("A"), VertexID.parseID("A"), VertexID.parseID("B"), VertexID.parseID("C"),true);
	}

	/** Tests equality for VertexIDs. */
	@Test
	public final void testVertexIDEquals1b()
	{
		equalityTestingHelper(new VertexID(VertexID.parseID("A")), VertexID.parseID("A"), VertexID.parseID("B"), VertexID.parseID("C"),true);
	}

	/** Tests equality for VertexIDs. */
	@Test
	public final void testVertexIDEquals2a()
	{
		equalityTestingHelper(new VertexID(VertKind.POSITIVE,5), new VertexID(VertKind.POSITIVE,5), new VertexID(VertKind.NEGATIVE,9), new VertexID(VertKind.NEUTRAL,9),true);
	}

	/** Tests equality for VertexIDs. */
	@Test
	public final void testVertexIDEquals2b()
	{
		equalityTestingHelper(new VertexID(VertKind.POSITIVE,5), new VertexID(new VertexID(VertKind.POSITIVE,5)), new VertexID(VertKind.NEGATIVE,9), new VertexID(new VertexID(VertKind.NEUTRAL,9)),true);
	}

	@Test
	public final void testParseString1()
	{
		VertexID id = VertexID.parseID("A");Assert.assertEquals(VertKind.NEUTRAL,id.getKind());Assert.assertEquals(65,id.getIntegerID());Assert.assertEquals("A",id.getStringId());
	}
	
	@Test
	public final void testParseString2()
	{
		final VertexID id = VertexID.parseID("");Assert.assertEquals(VertKind.NONE,id.getKind());Assert.assertEquals("",id.getStringId());
		checkForCorrectException(
				id::getIntegerID,
				IllegalArgumentException.class,"ID without an integer");
	}

	@Test
	public final void testParseString3()
	{
		VertexID id = VertexID.parseID("ABCD");Assert.assertEquals(VertKind.NEUTRAL,id.getKind());Assert.assertEquals(65+(66 *96)+(67*96*96)+(68 *96*96*96),id.getIntegerID());Assert.assertEquals("ABCD",id.getStringId());
	}

	@Test
	public final void testParseString4()
	{
		final VertexID id = VertexID.parseID("ABCDE");Assert.assertEquals(VertKind.NONE,id.getKind());Assert.assertEquals("ABCDE",id.getStringId());
		checkForCorrectException(
				id::getIntegerID,
				IllegalArgumentException.class,"ID without an integer");
	}

	public final static String 
		idP5 = new VertexID(VertKind.POSITIVE,5).getStringId(),
		idN5 = new VertexID(VertKind.NEGATIVE,5).getStringId(),
		idP6 = new VertexID(VertKind.POSITIVE,6).getStringId();
	
	/** Tests equality for VertexIDs. */
	@Test
	public final void testVertexIDEquals3()
	{
		equalityTestingHelper(new VertexID(VertKind.NEGATIVE,5), new VertexID(VertKind.NEGATIVE,5), new VertexID(VertKind.POSITIVE,5), new VertexID(VertKind.NEUTRAL,5),true);
	}
	
	/** Tests equality for VertexIDs with string and numerical IDs. */
	@Test
	public final void testVertexIDEquals4()
	{
		equalityTestingHelper(new VertexID(VertKind.POSITIVE,5), new VertexID(VertKind.POSITIVE,5), new VertexID(idN5), new VertexID(idP5),true);
	}

	/** Tests equality for VertexIDs with numerical IDs, across both Deterministic vertices and StringVertices. */
	@Test
	public final void testVertexIDEqualsCmpVertex1()
	{
		Configuration configToUse = Configuration.getDefaultConfiguration().copy();configToUse.setLearnerUseStrings(true);
		equalityTestingHelper(new VertexID(VertKind.POSITIVE,5),
				AbstractLearnerGraph.generateNewCmpVertex(new VertexID(VertKind.POSITIVE,5), configToUse), 
				new VertexID(idN5), new VertexID(idP5),true);
	}

	/** Tests equality for VertexIDs with numerical IDs, across both Deterministic vertices and StringVertices. */
	@Test
	public final void testVertexIDEqualsCmpVertex2()
	{
		Configuration configToUse = Configuration.getDefaultConfiguration().copy();configToUse.setLearnerUseStrings(false);
		equalityTestingHelper(new VertexID(VertKind.POSITIVE,5),
				AbstractLearnerGraph.generateNewCmpVertex(new VertexID(VertKind.POSITIVE,5), configToUse), 
				new VertexID(idN5), new VertexID(idP5),true);
	}

	/** Tests equality for VertexIDs with String IDs, across both Deterministic vertices and StringVertices. */
	@Test
	public final void testVertexIDEqualsCmpVertex3()
	{
		Configuration configToUse = Configuration.getDefaultConfiguration().copy();configToUse.setLearnerUseStrings(true);
		equalityTestingHelper(new VertexID(idP5),
				AbstractLearnerGraph.generateNewCmpVertex(new VertexID(idP5), configToUse), 
				new VertexID(idN5), new VertexID(VertKind.POSITIVE,5),true);
	}

	/** Tests equality for VertexIDs with String IDs, across both Deterministic vertices and StringVertices. */
	@Test
	public final void testVertexIDEqualsCmpVertex4()
	{
		Configuration configToUse = Configuration.getDefaultConfiguration().copy();configToUse.setLearnerUseStrings(false);
		equalityTestingHelper(new VertexID(idP5),
				AbstractLearnerGraph.generateNewCmpVertex(new VertexID(idP5), configToUse), 
				new VertexID(idN5), new VertexID(VertKind.POSITIVE,5),true);
	}

	/** Tests VertexID toString methods. */
	@Test
	public final void testVertexIDToString()
	{
		Assert.assertEquals("P5", new VertexID(idP5).toString());
		Assert.assertEquals("N5", new VertexID(idN5).toString());
		Assert.assertEquals("P5", new VertexID(VertKind.POSITIVE,5).toString());
		Assert.assertEquals("N5", new VertexID(VertKind.NEGATIVE,5).toString());

		Assert.assertEquals("JustAnything", VertexID.parseID("JustAnything").toString());
		Assert.assertEquals("V5", new VertexID(VertKind.NEUTRAL,5).toString());
	}
	
	@Test
	public final void testVertexIDLess1()
	{
		VertexID pA=new VertexID(VertKind.POSITIVE,5), pB=new VertexID(idP5),
			qA = new VertexID(VertKind.POSITIVE,6);
		checkLessHelper(pA,qA);
		checkLessHelper(qA,pB);// now qA has a cached representation of its string value.
		checkLessHelper(qA,pB);
		checkLessHelper(pA,qA);
	}
	
	@Test
	public final void testVertexIDLess2()
	{
		VertexID pA=new VertexID(idP5), pB=new VertexID(VertKind.POSITIVE,5),
			qA = new VertexID(idP6);
		checkLessHelper(pA,qA);
		checkLessHelper(pB,qA);// now pB has a cached representation of its string value.
		checkLessHelper(pB,qA);
		checkLessHelper(pA,qA);
	}
	
	@Test
	public final void testVertexIDLess3()
	{
		VertexID pA=new VertexID(VertKind.POSITIVE,5), pB=new VertexID(VertKind.POSITIVE,10),
			qA = new VertexID(VertKind.POSITIVE,6);
		checkLessHelper(pA,qA);
		checkLessHelper(pA,pB);
		checkLessHelper(qA,pB);

		//noinspection ConstantConditions
		Assert.assertTrue("P10".compareTo("P5") < 0);
		
		Assert.assertTrue(pB.compareTo(new VertexID(idP5)) < 0);
		
		checkLessHelper(pA,qA);
		checkLessHelper(pA,pB);
		checkLessHelper(qA,pB);
	}
	
	private final DeterministicVertex DvertA = new DeterministicVertex("a"),DvertB = new DeterministicVertex("a");
	private final DeterministicVertex DdifferentA = new DeterministicVertex("b");

	private final StringVertex SvertA = new StringVertex("a"),SvertB = new StringVertex("a");
	private final StringVertex SdifferentA = new StringVertex("b");

	/** Resets the attributes on vertices used for equality testing. */
	@Before
	public final void beforeTests()
	{
		DvertA.setAccept(true);DvertB.setAccept(true);DvertA.setColour(null);DvertB.setColour(null);DvertA.setHighlight(false);DvertB.setHighlight(false);
		SvertA.setAccept(true);SvertB.setAccept(true);SvertA.setColour(null);SvertB.setColour(null);SvertA.setHighlight(false);SvertB.setHighlight(false);
	}
	
	@Test
	public final void checkDEquality1()
	{
		DvertA.setAccept(true);DvertB.setAccept(true);
		equalityTestingHelper(DvertA,DvertA,DdifferentA,SdifferentA,true);equalityTestingHelper(DvertB,DvertB,DdifferentA,SdifferentA,true);
		equalityTestingHelper(DvertA,DvertB,DdifferentA,SdifferentA,true);
	}

	@Test
	public final void checkDEquality2()
	{
		DvertA.setAccept(false);DvertB.setAccept(false);
		equalityTestingHelper(DvertA,DvertA,DdifferentA,SdifferentA,true);equalityTestingHelper(DvertB,DvertB,DdifferentA,SdifferentA,true);
		equalityTestingHelper(DvertA,DvertB,DdifferentA,SdifferentA,true);
	}

	/** Checks that attributes other than accept, name and the like known to Statechum are ignored. */
	@Test
	public final void checkDEquality_ignoresAttrs1()
	{
		DvertA.setAccept(true);DvertB.setAccept(true);
		DvertA.setColour(JUConstants.RED);DvertA.setHighlight(true);DvertA.addUserDatum(JUConstants.INITIAL, "", UserData.SHARED);
		DvertA.addUserDatum(JUConstants.JUNKVERTEX, "a", UserData.SHARED);DvertA.addUserDatum(JUConstants.ORIGSTATE, VertexID.parseID("test"), UserData.SHARED);
		DvertB.setColour(JUConstants.BLUE);DvertB.setHighlight(true);DvertB.removeUserDatum(JUConstants.INITIAL);
		DvertB.addUserDatum(JUConstants.JUNKVERTEX, "b", UserData.SHARED);DvertB.addUserDatum(JUConstants.DEPTH, 3, UserData.SHARED);
		equalityTestingHelper(DvertA,DvertB,DdifferentA,SdifferentA,true);
		Assert.assertFalse(deepEquals(DvertA,DvertB));
		Assert.assertTrue(deepEquals(DvertA,DvertA));
		Assert.assertTrue(deepEquals(DvertB,DvertB));
	}
	
	/** Checks that name is ignored by <em>nonIDAttributesEquals</em>. */
	@Test
	public final void checkDEquality_ignoresAttrs2()
	{
		CmpVertex A = new DeterministicVertex("a"),B = new DeterministicVertex("b");
		A.setAccept(true);B.setAccept(true);
		A.setColour(JUConstants.RED);B.setColour(JUConstants.RED);
		Assert.assertTrue(DeterministicDirectedSparseGraph.nonIDAttributesEquals(A, B));
		Assert.assertFalse(DeterministicDirectedSparseGraph.deepEquals(A, B));
	}
	
	@Test
	public final void checkSEquality1()
	{
		SvertA.setAccept(true);SvertB.setAccept(true);
		equalityTestingHelper(SvertA,SvertA,SdifferentA,DdifferentA,true);equalityTestingHelper(SvertB,SvertB,SdifferentA,DdifferentA,true);
		equalityTestingHelper(SvertA,SvertB,SdifferentA,DdifferentA,true);
	}

	@Test
	public final void checkSEquality2()
	{
		SvertA.setAccept(false);SvertB.setAccept(false);
		equalityTestingHelper(SvertA,SvertA,SdifferentA,DdifferentA,true);equalityTestingHelper(SvertB,SvertB,SdifferentA,DdifferentA,true);
		equalityTestingHelper(SvertA,SvertB,SdifferentA,DdifferentA,true);
	}

	/** Checks that attributes other than name are ignored. */
	@Test
	public final void checkSEquality_ignoresAttrs()
	{
		SvertA.setAccept(true);SvertB.setAccept(true);
		SvertA.setColour(JUConstants.RED);SvertA.setHighlight(true);
		SvertB.setColour(JUConstants.BLUE);SvertB.setHighlight(true);
		SvertA.setOrigState(VertexID.parseID("A"));SvertB.setOrigState(VertexID.parseID("B"));
		SvertA.setDepth(1);SvertB.setDepth(2);
		equalityTestingHelper(SvertA,SvertB,SdifferentA,DdifferentA,true);
		Assert.assertFalse(deepEquals(SvertA, SvertB));
		Assert.assertTrue(deepEquals(SvertA,SvertA));
		Assert.assertTrue(deepEquals(SvertB,SvertB));
	}

	/* Checks that if CmpVertex implemented with different types
	 * (StringVertex v.s. DeterminisitcVertex), equals returns false.
	 * Right now, we allow comparisons between different types, hence the test is commented out. 
	@Test
	public final void checkSEquality4()
	{
		DvertA.setAccept(true);DvertB.setAccept(true);SvertA.setAccept(true);SvertB.setAccept(true);
		equalityTestingHelper(DvertA,DvertB,SvertA,SvertB);
	}	
	 */ 

	/** Checks that implementations of different types can be compared. 
	 */
	@Test
	public final void checkEquality_differentTypes()
	{
		equalityTestingHelper(SvertA,DvertA,SdifferentA,DdifferentA,true);
	}

	@Test
	public final void checkDComparison1()
	{
		checkLessHelper(DvertA, new DeterministicVertex("b"));
	}
	
	@Test
	public final void checkSComparison1()
	{
		checkLessHelper(SvertA, new StringVertex("b"));
	}
	
	@Test
	public final void checkComparison_differentTypes()
	{
		checkLessHelper(DvertA, new StringVertex("b"));
		checkLessHelper(SvertA, new DeterministicVertex("b"));
	}

	/** Checks that it is not possible to compare implementations of different types. 
	 * The restriction is useful to detect programming errors when I end up putting elements of unrelated graphs into the same collection.
	 * Decided to comment this out since the restriction makes life hard: if I have
	 * a collection of StringVertices and another one of Deterministic ones, they may
	 * actually be equal, but the way Java5 collections compare them makes it impossible
	 * to use equals without causing a comparison between vertices of the two types. 
	Test(expected=IllegalArgumentException.class)
	public final void checkComparison_fail1()
	{
		SvertA.compareTo(DvertA);
	}
		
	Test(expected=IllegalArgumentException.class)
	public final void checkComparison_fail2()
	{
		DvertA.compareTo(SvertA);
	}
	 */
	
	@SuppressWarnings("EqualsWithItself")
	@Test
	public final void testDeterministicVertexComparison1_old()
	{
		DeterministicVertex p = new DeterministicVertex("P"), q= new DeterministicVertex("Q");
		assertNotEquals(p, q);
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertNotEquals(p.hashCode(), q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}
		
	@SuppressWarnings("EqualsWithItself")
	@Test
	public final void testDeterministicVertexComparison2_old()
	{
		DeterministicVertex p = new DeterministicVertex("A"), q= new DeterministicVertex("B");
		assertNotEquals(p, q);
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertNotEquals(p.hashCode(), q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}

	@Test
	public final void testDeterministicVertexComparison3_old()
	{
		DeterministicVertex p = new DeterministicVertex("P"), q= new DeterministicVertex("P");
		assertEquals(p, q);
		assertEquals(0, p.compareTo(q));
	}

	@Test
	public final void testStatePairToString()
	{
		assertEquals("[ A, B ]", new StatePair(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"), config),AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("B"), config) ).toString());
		assertEquals("[ A, NULL ]", new StatePair(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"), config),null ).toString());
		assertEquals("[ NULL, A ]", new StatePair(null,AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("A"), config)).toString());
		assertEquals("[ NULL, NULL ]", new StatePair(null,null).toString());
	}
	
	private <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> EquivalenceClass<TARGET_TYPE, CACHE_TYPE>
		buildClass(EquivalenceClass<TARGET_TYPE, CACHE_TYPE> result, CmpVertex[] vertices)
	{
		//new AMEquivalenceClass(0,testGraphString);
		for(CmpVertex vert:vertices)
			try {
				result.mergeWith(vert, testGraphString.transitionMatrix.get(testGraphString.getInit()).entrySet());
			} catch (IncompatibleStatesException e) {
				Assert.fail(e.getMessage());
			}
		return result;
	}
	
	@Test
	public final void testEqClass_toString0()
	{
		Assert.assertEquals("[{A,B,D}]",
		buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
				new StringVertex("B"),new StringVertex("A"),new StringVertex("D")}).toString());		
	}
	
	/** Creating a merged vertex with the same id as the representative one. */ 
	@Test
	public final void testEqClass_toString1a()
	{
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = buildClass(new AMEquivalenceClass<>(0, testGraphString), new CmpVertex[]{
				new StringVertex("B"),new StringVertex("A"),new StringVertex("D")});
		eqClass.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertEquals("[A->{A,B,D}]", eqClass.toString());		
	}
	
	/** Creating a merged vertex with a the same ID as the representative one because the vertex is not present in the supplied graph. */ 
	@Test
	public final void testEqClass_toString1b()
	{
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = buildClass(new AMEquivalenceClass<>(0, testGraphString), new CmpVertex[]{
				new StringVertex("B"),new StringVertex("A"),new StringVertex("D")});
		eqClass.constructMergedVertex(new LearnerGraph(config),true,false);
		Assert.assertEquals("[A->{A,B,D}]", eqClass.toString());		
	}
	
	/** Creating a merged vertex with a different id as the representative one. */ 
	@Test
	public final void testEqClass_toString1c()
	{
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = buildClass(new AMEquivalenceClass<>(0, testGraphString), new CmpVertex[]{
				new StringVertex("B"),new StringVertex("A"),new StringVertex("D")});
		eqClass.constructMergedVertex(testGraphString,true,false);
		Assert.assertEquals("[P1000->{A,B,D}]", eqClass.toString());		
	}
	
	@Test
	public final void testEqClass_toString2()
	{
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = buildClass(new AMEquivalenceClass<>(0, testGraphString),
				new CmpVertex[]{ new StringVertex("B")});
		eqClass.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertEquals("[B->{B}]",eqClass.toString());		
	}

	/** Handling of depth using representatives. */ 
	@Test
	public final void testEqClassRepresentatives1()
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C=new DeterministicVertex("C"), D=new DeterministicVertex("D");
		B.setDepth(5);
		EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = buildClass(new AMEquivalenceClass<>(0, testGraphString),
				new CmpVertex[]{A,B,C,D});
		Assert.assertSame(B,eqClass.getRepresentative());
	}
	
	/** Handling of depth using representatives. */ 
	@Test
	public final void testEqClassRepresentatives2()
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C=new DeterministicVertex("C"), D=new DeterministicVertex("D");
		B.setDepth(5);C.setDepth(45);
		EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = buildClass(new AMEquivalenceClass<>(0, testGraphString),
				new CmpVertex[]{A,B,C,D});
		Assert.assertSame(B,eqClass.getRepresentative());
	}
	
	/** Handling of depth using representatives. */ 
	@Test
	public final void testEqClassRepresentatives3()
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C=new DeterministicVertex("C"), D=new DeterministicVertex("D");
		B.setDepth(5);C.setDepth(45);D.setDepth(4);
		EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{A,B,C,D});
		Assert.assertSame(D,eqClass.getRepresentative());
	}
	
	/** Handling of depth using representatives where multiple equivalence classes are merged. */ 
	@Test
	public final void testEqClassRepresentatives4() throws IncompatibleStatesException
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C=new DeterministicVertex("C"), D=new DeterministicVertex("D");
		B.setDepth(5);C.setDepth(45);D.setDepth(4);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> 
			eqClassA = buildClass(new AMEquivalenceClass<>(0, testGraphString),
				new CmpVertex[]{A,B}),eqClassB = buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{C,D});
		eqClassA.mergeWith(eqClassB);
		Assert.assertSame(D,eqClassA.getRepresentative());
	}
	
	/** Handling of depth using representatives where multiple equivalence classes are merged. */ 
	@Test
	public final void testEqClassRepresentatives5() throws IncompatibleStatesException
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C=new DeterministicVertex("C"), D=new DeterministicVertex("D");
		B.setDepth(5);C.setDepth(45);D.setDepth(4);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> 
			eqClassA = buildClass(new AMEquivalenceClass<>(0, testGraphString),
					new CmpVertex[]{A,B}),eqClassB = buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{C,D});
		eqClassB.mergeWith(eqClassA);
		Assert.assertSame(D,eqClassB.getRepresentative());
	}
	
	/** Handling of depth using representatives where multiple equivalence classes are merged. */ 
	@Test
	public final void testEqClassRepresentatives6() throws IncompatibleStatesException
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C=new DeterministicVertex("C"), D=new DeterministicVertex("D"),
			E = new DeterministicVertex("E");
		B.setDepth(5);C.setDepth(45);D.setDepth(4);E.setDepth(4);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> 
			eqClassA = buildClass(new AMEquivalenceClass<>(0, testGraphString),
					new CmpVertex[]{A,B}),
			eqClassB = buildClass(new AMEquivalenceClass<>(0, testGraphString),
					new CmpVertex[]{C,D}),eqClassC = buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{E});
		eqClassB.mergeWith(eqClassA);eqClassB.mergeWith(eqClassC);
		Assert.assertSame(D,eqClassB.getRepresentative());
	}
	
	/** Testing whether checking for incompatible vertices works correctly. */
	@Test
	public final void testIncompatChecking1()
	{
		CmpVertex A = new StringVertex("A"), B = new StringVertex("B"), C=new DeterministicVertex("C");
		C.setAccept(false);
		PairCompatibility<CmpVertex> incompatibles = new PairCompatibility<>(HashMapWithSearch.DEFAULT_INITIAL_CAPACITY / 2, HashMapWithSearch.DEFAULT_INITIAL_CAPACITY / 2);
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(A, B, incompatibles));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(B, A, incompatibles));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(A, A, incompatibles));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(B, B, incompatibles));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(C, C, incompatibles));

		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(A, C, incompatibles));
		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(C, A, incompatibles));
	}
	
	/** Testing whether checking for incompatible vertices works correctly. */
	@Test
	public final void testIncompatChecking2()
	{
		LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C", "testIncompatChecking2",config,converter);
		CmpVertex A = gr.findVertex("A"),B=gr.findVertex("B"),C=gr.findVertex("C");
		assert !C.isAccept();
		
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(A, B, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(B, A, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(A, A, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(B, B, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(C, C, gr.pairCompatibility));

		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(A, C, gr.pairCompatibility));
		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(C, A, gr.pairCompatibility));
	}

	/** Testing whether checking for incompatible vertices works correctly. */
	@Test
	public final void testIncompatChecking3()
	{
		LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D", "testIncompatChecking3",config,converter);
		CmpVertex A = gr.findVertex("A"),B=gr.findVertex("B"),C=gr.findVertex("C"),D=gr.findVertex("D");
		assert !C.isAccept();
		
		gr.addToCompatibility(A, B,JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		
		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(A, B, gr.pairCompatibility));
		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(B, A, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(A, D, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(D, A, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(B, D, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(D, B, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(A, A, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(B, B, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(C, C, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(D, D, gr.pairCompatibility));

		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(A, C, gr.pairCompatibility));
		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(C, A, gr.pairCompatibility));
	}
	
	/** Testing whether checking for incompatible vertices works correctly, both with 
	 * compatible and with incompatible vertices. */
	@Test
	public final void testIncompatChecking4()
	{
		LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E", "testIncompatChecking4",config,converter);
		CmpVertex A = gr.findVertex("A"),B=gr.findVertex("B"),C=gr.findVertex("C"),D=gr.findVertex("D"),E=gr.findVertex("E");
		assert !C.isAccept();
		
		gr.addToCompatibility(A, E,JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		gr.addToCompatibility(A, B,JUConstants.PAIRCOMPATIBILITY.MERGED);
		
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(A, B, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(B, A, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(A, D, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(D, A, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(B, D, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(D, B, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(A, A, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(B, B, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(C, C, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(D, D, gr.pairCompatibility));
		Assert.assertTrue(AbstractLearnerGraph.checkCompatible(E, E, gr.pairCompatibility));

		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(A, C, gr.pairCompatibility));
		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(C, A, gr.pairCompatibility));

		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(A, E, gr.pairCompatibility));
		Assert.assertFalse(AbstractLearnerGraph.checkCompatible(E, A, gr.pairCompatibility));
	}
	
	private List<CmpVertex> addOutgoing(Map<Label,Object> where, String label)
	{
		Label lbl = AbstractLearnerGraph.generateNewLabel(label, config,converter);
		if (!where.containsKey(lbl)) where.put(lbl, new ArrayList<CmpVertex>());
		else
		{
			if (where.get(lbl) instanceof CmpVertex)
			{// convert an instance of CmpVertex into a collection.
				List<CmpVertex> list = new ArrayList<>();list.add((CmpVertex)where.get(lbl));where.put(lbl, list);
			}
		}
		//noinspection unchecked
		return (List<CmpVertex>)where.get(lbl);
	}
	
	/** Checking whether vertices can be merged correctly.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassMerging1() throws IncompatibleStatesException
	{
		LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E\nA-d->A", "testEqClassMerging",config,converter);
		CmpVertex A = gr.findVertex("A"),B=gr.findVertex("B"),C=gr.findVertex("C"),D=gr.findVertex("D");
		
		EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = new AMEquivalenceClass<>(0, gr);Assert.assertEquals(0, eqClass.getNumber());
		eqClass.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		eqClass.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		eqClass.mergeWith(B, gr.transitionMatrix.get(B).entrySet());
		Assert.assertSame(A,eqClass.getRepresentative());
		Map<Label,Object> expectedTargets = new TreeMap<>();
		AMEquivalenceClass.addTransition(expectedTargets,AbstractLearnerGraph.generateNewLabel("a", config,converter), B,config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY);
		
		addOutgoing(expectedTargets,"b").add(C);
		addOutgoing(expectedTargets,"a").add(D);
		addOutgoing(expectedTargets,"d").add(A);
		
		Map<Label,ArrayList<CmpVertex>> actualTargetsAsSets = new TreeMap<>();
		Set<CmpVertex> vertices = new TreeSet<>();
		for(Entry<Label,Object> entry:eqClass.getOutgoing().entrySet())
		{
			if (entry.getValue() instanceof CmpVertex)
				vertices.add((CmpVertex)entry.getValue());
			else
				//noinspection unchecked
				vertices.addAll((List<CmpVertex>)entry.getValue());
			actualTargetsAsSets.put(entry.getKey(), new ArrayList<>(vertices));
			vertices.clear();
		}
		Assert.assertEquals(expectedTargets, actualTargetsAsSets);
	}
	
	/** Checking whether vertices can be merged correctly.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassMerging2() throws IncompatibleStatesException
	{
		LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E\nA-d->A", "testEqClassMerging",config,converter);
		CmpVertex A = gr.findVertex("A"),B=gr.findVertex("B");
		
		EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = new AMEquivalenceClass<>(10, gr);
		
		Assert.assertNull(eqClass.getRepresentative());Assert.assertEquals(10, eqClass.getNumber());
		eqClass.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		Assert.assertSame(A,eqClass.getRepresentative());Assert.assertEquals(10, eqClass.getNumber());
		if (config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY)
			Assert.assertEquals("{a=B, d=A}", eqClass.getOutgoing().toString());// using a string comparison in order to check that correct objects are stored in the AMEquivalenceClass collection. Where multiple data values it will be a list, otherwise a single object.
		else
			Assert.assertEquals("{a=[B], d=[A]}", eqClass.getOutgoing().toString());
		eqClass.mergeWith(B, gr.transitionMatrix.get(B).entrySet());
		Assert.assertSame(A,eqClass.getRepresentative());

		if (config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY)
			Assert.assertEquals("{a=[B, D], b=C, d=A}", eqClass.getOutgoing().toString());
		else
			Assert.assertEquals("{a=[B, D], b=[C], d=[A]}", eqClass.getOutgoing().toString());
	}
	
	/** Checking whether vertices can be merged correctly.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassMerging3() throws IncompatibleStatesException
	{
		LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E\nA-d->A", "testEqClassMerging",config,converter);
		CmpVertex A = gr.findVertex("A"),B=gr.findVertex("B"),C=gr.findVertex("C"),D=gr.findVertex("D");
		
		AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClassA = new AMEquivalenceClass<>(4, gr),eqClassB = new AMEquivalenceClass<>(1, gr);
		eqClassA.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		eqClassB.mergeWith(B, gr.transitionMatrix.get(B).entrySet());
		
		Map<Label,Object> expectedTargets = new TreeMap<>();
		addOutgoing(expectedTargets,"a").add(B);addOutgoing(expectedTargets,"d").add(A);
		if (config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY)
			Assert.assertEquals("{a=B, d=A}", eqClassA.getOutgoing().toString());
		else
			Assert.assertEquals("{a=[B], d=[A]}", eqClassA.getOutgoing().toString());

		eqClassA.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		eqClassB.mergeWith(B, gr.transitionMatrix.get(B).entrySet());
		Assert.assertSame(A,eqClassA.getRepresentative());Assert.assertSame(B,eqClassB.getRepresentative());
		Assert.assertEquals(4, eqClassA.getNumber());Assert.assertEquals(1, eqClassB.getNumber());
		
		eqClassA.mergeWith(eqClassB);
		
		addOutgoing(expectedTargets,"b").add(C);
		//noinspection unchecked
		((List<CmpVertex>)expectedTargets.get(AbstractLearnerGraph.generateNewLabel("a", config,converter))).add(D);
		Map<Label,ArrayList<CmpVertex>> actualTargetsAsSets = new TreeMap<>();
		Set<CmpVertex> vertices = new TreeSet<>();
		for(Entry<Label,Object> entry:eqClassA.getOutgoing().entrySet())
		{
			if (entry.getValue() instanceof CmpVertex)
				vertices.add((CmpVertex)entry.getValue());
			else
				//noinspection unchecked
				vertices.addAll((List<CmpVertex>)entry.getValue());
			actualTargetsAsSets.put(entry.getKey(), new ArrayList<>(vertices));
			vertices.clear();
		}
		Assert.assertEquals(expectedTargets, actualTargetsAsSets);
	}
	
	/** Checking whether incompatible vertices are correctly handled. This tests checks for adding of incompatible accept/reject vertices.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail1() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex A = gr.findVertex("A"),B=gr.findVertex("B"),C=gr.findVertex("C");
		
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = new AMEquivalenceClass<>(0, gr);
		eqClass.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		eqClass.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		eqClass.mergeWith(B, gr.transitionMatrix.get(B).entrySet());
		Helper.checkForCorrectException(
				() -> eqClass.mergeWith(C, gr.transitionMatrix.get(C).entrySet()),
				IncompatibleStatesException.class,"cannot");
	}
	
	/** Checking whether incompatible vertices are correctly handled. This tests checks for adding of incompatible accept/reject vertices.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail2() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex A = gr.findVertex("A"),B=gr.findVertex("B"),C=gr.findVertex("C");
		
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = new AMEquivalenceClass<>(0, gr);
		eqClass.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		eqClass.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		eqClass.mergeWith(B, gr.transitionMatrix.get(B).entrySet());
		Helper.checkForCorrectException(
				() -> eqClass.mergeWith(C, gr.transitionMatrix.get(C).entrySet()),
				IncompatibleStatesException.class,"cannot");
	}
	
	/** Checking whether vertices can be merged correctly.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassMerging4() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassMerging2",config,converter);
		final CmpVertex C=gr.findVertex("C"), F=gr.findVertex("F");
		
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = new AMEquivalenceClass<>(0, gr);
		eqClass.mergeWith(C, gr.transitionMatrix.get(C).entrySet());
		eqClass.mergeWith(C, gr.transitionMatrix.get(C).entrySet());
		eqClass.mergeWith(F, gr.transitionMatrix.get(F).entrySet());
		Assert.assertSame(C,eqClass.getRepresentative());
		Assert.assertTrue(eqClass.getOutgoing().isEmpty());
	}
	
	/** Checking whether incompatible vertices are correctly handled. 
	 * This tests checks for adding of incompatible accept/reject vertices.
	 * 
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail3() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex C=gr.findVertex("C"),D=gr.findVertex("D");
		
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = new AMEquivalenceClass<>(0, gr);
		eqClass.mergeWith(C, gr.transitionMatrix.get(C).entrySet());
		eqClass.mergeWith(C, gr.transitionMatrix.get(C).entrySet());
		Helper.checkForCorrectException(
				() -> eqClass.mergeWith(D, gr.transitionMatrix.get(D).entrySet()),
				IncompatibleStatesException.class,"cannot");
	}
	
	/** Checking whether incompatible vertices are correctly handled. This tests checks for adding of incompatible accept/reject vertices.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail4() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex C=gr.findVertex("C"),D=gr.findVertex("D");
		
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClass = new AMEquivalenceClass<>(0, gr);
		eqClass.mergeWith(C, gr.transitionMatrix.get(C).entrySet());
		Helper.checkForCorrectException(
				() -> eqClass.mergeWith(D, gr.transitionMatrix.get(D).entrySet()),
				IncompatibleStatesException.class,"cannot");
	}
	
	/** Checking whether incompatible vertices are correctly handled. This tests checks for adding of incompatible accept/reject vertices
	 * when one equivalence class is merged into another one.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail5() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex C=gr.findVertex("C"),D=gr.findVertex("D");
		
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClassA = new AMEquivalenceClass<>(0, gr);
		eqClassA.mergeWith(C, gr.transitionMatrix.get(C).entrySet());
		final AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClassB = new AMEquivalenceClass<>(1, gr);
		eqClassB.mergeWith(D, gr.transitionMatrix.get(D).entrySet());
		Helper.checkForCorrectException(
				() -> eqClassA.mergeWith(eqClassB),
				IncompatibleStatesException.class,"incompatible");
	}
	
	/** Checking whether incompatible vertices are correctly handled. This tests checks for adding of incompatible accept/reject vertices
	 * when one equivalence class is merged into another one.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail6() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex C=gr.findVertex("C"),D=gr.findVertex("D");
		
		final AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClassA = new AMEquivalenceClass<>(0, gr);
		eqClassA.mergeWith(C, gr.transitionMatrix.get(C).entrySet());
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClassB = new AMEquivalenceClass<>(1, gr);
		eqClassB.mergeWith(D, gr.transitionMatrix.get(D).entrySet());
		Helper.checkForCorrectException(
				() -> eqClassB.mergeWith(eqClassA),
				IncompatibleStatesException.class,"incompatible");
	}
	
	/** Checking whether incompatible vertices are correctly handled. This tests checks for adding of vertices
	 * recorded as incompatible  when one equivalence class is merged into another one.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail7a() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex A=gr.findVertex("A"),D=gr.findVertex("D");
		
		gr.addToCompatibility(A, D, JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClassA = new AMEquivalenceClass<>(0, gr);
		eqClassA.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		final AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClassB = new AMEquivalenceClass<>(1, gr);
		eqClassB.mergeWith(D, gr.transitionMatrix.get(D).entrySet());
		
		Helper.checkForCorrectException(
				() -> eqClassA.mergeWith(eqClassB),
				IncompatibleStatesException.class,"incompatible");
	}

	/** Checking whether incompatible vertices are correctly handled. This tests checks for adding of vertices
	 * recorded as incompatible  when one equivalence class is merged into another one.
	 * This test differs from {@link TestEqualityComparisonAndHashCode#testEqClassHandlingOfIncompatibleVertices_fail7a()} in that the we do mergeWith on B with A rather than the other way around.
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail7b() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex A=gr.findVertex("A"),D=gr.findVertex("D");
		
		gr.addToCompatibility(A, D, JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		final AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClassA = new AMEquivalenceClass<>(0, gr);
		eqClassA.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClassB = new AMEquivalenceClass<>(1, gr);
		eqClassB.mergeWith(D, gr.transitionMatrix.get(D).entrySet());
		
		Helper.checkForCorrectException(
				() -> eqClassB.mergeWith(eqClassA),
				IncompatibleStatesException.class,"incompatible");
	}

	/** Checking whether incompatible vertices are correctly handled. This tests checks for adding of vertices
	 * recorded as incompatible  when one equivalence class is merged into another one.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail8() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex A=gr.findVertex("A"),D=gr.findVertex("D");
		
		gr.addToCompatibility(A, D, JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		final AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClassA = new AMEquivalenceClass<>(0, gr);
		eqClassA.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClassB = new AMEquivalenceClass<>(1, gr);
		eqClassB.mergeWith(D, gr.transitionMatrix.get(D).entrySet());
		
		Helper.checkForCorrectException(
				() -> eqClassB.mergeWith(eqClassA),
				IncompatibleStatesException.class,"incompatible");
	}

	/** Checking whether incompatible vertices are correctly handled. This tests checks for adding of vertices
	 * recorded as incompatible to an equivalence class.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail9() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex A=gr.findVertex("A"),D=gr.findVertex("D");
		
		gr.addToCompatibility(A, D, JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClassA = new AMEquivalenceClass<>(0, gr);
		eqClassA.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		
		Helper.checkForCorrectException(
				() -> eqClassA.mergeWith(D,gr.transitionMatrix.get(D).entrySet()),
				IncompatibleStatesException.class,"cannot");
	}

	/** Checks that a pair of vertices recorded as something else other than incompatible will not
	 * cause a failure of an equivalence class merge.
	 * 
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_nofail9a() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex A=gr.findVertex("A"),D=gr.findVertex("D");
		
		gr.addToCompatibility(A, D, JUConstants.PAIRCOMPATIBILITY.MERGED);
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClassA = new AMEquivalenceClass<>(0, gr);
		eqClassA.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		
		eqClassA.mergeWith(D,gr.transitionMatrix.get(D).entrySet());// should be no exception
	}

	/** Checking whether incompatible vertices are correctly handled. This tests checks for adding of vertices
	 * recorded as incompatible to an equivalence class.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail10() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex A=gr.findVertex("A"),D=gr.findVertex("D");
		
		gr.addToCompatibility(A, D, JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClassA = new AMEquivalenceClass<>(0, gr);
		eqClassA.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		
		Helper.checkForCorrectException(
				() -> eqClassA.mergeWith(D,gr.transitionMatrix.get(D).entrySet()),
				IncompatibleStatesException.class,"cannot");
	}

	/** Checking whether incompatible vertices are correctly handled. This tests checks for adding of vertices
	 * recorded as incompatible  when one equivalence class is merged into another one.
	 * @throws IncompatibleStatesException if this test unexpectedly fails.  
	 */
	@Test
	public final void testEqClassHandlingOfIncompatibleVertices_fail11() throws IncompatibleStatesException
	{
		final LearnerGraph gr = buildLearnerGraph("A-a->B-b-#C\nB-a->D-a->E-a-#F", "testEqClassHandlingOfIncompatibleVertices1",config,converter);
		final CmpVertex A=gr.findVertex("A"),D=gr.findVertex("D"),E=gr.findVertex("E");
		
		gr.addToCompatibility(E, D, JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		final EquivalenceClass<CmpVertex, LearnerGraphCachedData> eqClassA = new AMEquivalenceClass<>(0, gr);
		eqClassA.mergeWith(A, gr.transitionMatrix.get(A).entrySet());
		final AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClassB = new AMEquivalenceClass<>(1, gr);
		eqClassB.mergeWith(D, gr.transitionMatrix.get(D).entrySet());
		final AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClassC = new AMEquivalenceClass<>(1, gr);
		eqClassC.mergeWith(E, gr.transitionMatrix.get(E).entrySet());
		eqClassA.mergeWith(eqClassB);
		
		Helper.checkForCorrectException(
				() -> eqClassA.mergeWith(eqClassC),
				IncompatibleStatesException.class,"incompatible");
	}

	/** Tests that equality does not depend on the order in which vertices are added. */
	@Test
	public final void testEqClassEquality1()
	{
		equalityTestingHelper(
				buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
						new StringVertex("B"),new StringVertex("A"),new StringVertex("C")}),
				
				buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
						new StringVertex("B"),new StringVertex("A"),new StringVertex("C")}),

				buildClass(new AMEquivalenceClass<>(1, testGraphString),new CmpVertex[]{
						new StringVertex("A"),new StringVertex("A"),new StringVertex("C")}),

				buildClass(new AMEquivalenceClass<>(2, testGraphString),new CmpVertex[]{
						new StringVertex("B"),new StringVertex("A"),new StringVertex("D")}),
				false
		);
	}
/*
	// Tests that equality does not depend on the order in which vertices are added.
	@Test
	public final void testEqClassEquality2()
	{
		equalityTestingHelper(
				buildClass(new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(0,testGraphString),new CmpVertex[]{
						new StringVertex("B")}),
				
				buildClass(new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(0,testGraphString),new CmpVertex[]{
						new StringVertex("B")}),

				buildClass(new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(0,testGraphString),new CmpVertex[]{
						new StringVertex("A"),new StringVertex("A"),new StringVertex("C")}),
		
				buildClass(new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(0,testGraphString),new CmpVertex[]{
						new StringVertex("B"),new StringVertex("A"),new StringVertex("D")}),
				false
		);
	}

	// Tests that equality does not depend on the order in which vertices are added. 
	@Test
	public final void testEqClassEquality3()
	{
		equalityTestingHelper(
				buildClass(new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(0,testGraphString),new CmpVertex[]{
						new StringVertex("A"),new StringVertex("B"),new StringVertex("C")}),
				
				buildClass(new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(0,testGraphString),new CmpVertex[]{
						new StringVertex("B"),new StringVertex("A"),new StringVertex("C")}),

				buildClass(new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(0,testGraphString),new CmpVertex[]{
						new StringVertex("A"),new StringVertex("B"),new StringVertex("D")}),
		
				buildClass(new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(0,testGraphString),new CmpVertex[]{
						new StringVertex("T"),new StringVertex("A"),new StringVertex("C")}),
				false
		);
	}
*/
	/** Checks that merged vertex and number of associated to the equivalence class are used in both
	 * equals() and hashCode() methods. 
	 */
	@Test
	public final void testEqClassEquality4()
	{
		CmpVertex [] vertices = new CmpVertex[]{
				new StringVertex("A"),new StringVertex("B"),new StringVertex("C")};
		
		AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClassA = new AMEquivalenceClass<>(10, testGraphString),
			eqClassB = new AMEquivalenceClass<>(0, testGraphString);
		for(CmpVertex vert:vertices)
			try {
				eqClassA.mergeWith(vert, testGraphString.transitionMatrix.get(testGraphString.getInit()).entrySet());
				eqClassB.mergeWith(vert, testGraphString.transitionMatrix.get(testGraphString.getInit()).entrySet());
			} catch (IncompatibleStatesException e) {
				Assert.fail(e.getMessage());
			}
		eqClassB.constructMergedVertex(new LearnerGraph(config),false,false);
		
		equalityTestingHelper(
				
				buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
						new StringVertex("B"),new StringVertex("A"),new StringVertex("C")}),

				buildClass(new AMEquivalenceClass<>(0, testGraphString),vertices),

				eqClassA, eqClassB, true
		);
	}

	/** Ensures that a representative vertex may be equal to one of those in a set of vertices. */
	@Test
	public final void testConstructionOfRepresentative1()
	{
		CmpVertex vert = new StringVertex("B");vert.setOrigState(VertexID.parseID("test"));
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = 
			buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{vert});
		eqClass.constructMergedVertex(testGraphString, false, false);
		Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(vert,eqClass.getMergedVertex()));
	}
	
	/** Ensures that a representative vertex may be equal to one of those in a set of vertices in all the attributes
	 * except the original state. */
	@Test
	public final void testConstructionOfRepresentative2()
	{
		CmpVertex vert = new StringVertex("B");vert.setOrigState(VertexID.parseID("test"));
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = 
			buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{vert});
		eqClass.constructMergedVertex(testGraphString, false, true);
		Assert.assertEquals(vert,eqClass.getMergedVertex().getOrigState());
		// now set the orig state of the merged vertex to the original value and compare the rest of the attributes.
		eqClass.getMergedVertex().setOrigState(vert.getOrigState());
		Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(vert,eqClass.getMergedVertex()));
	}
	
	/** Check that duplicate names are correctly detected, part 1. */
	@Test
	public final void testConstructionOfRepresentative3()
	{
		CmpVertex vert = new StringVertex("B");vert.setOrigState(VertexID.parseID("origVertex"));
		vert.setOrigState(VertexID.parseID("test"));
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = 
			buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{vert});
		eqClass.constructMergedVertex(testGraphString, true, false);
		Assert.assertTrue(DeterministicDirectedSparseGraph.nonIDAttributesEquals(eqClass.getMergedVertex(),vert));
		Assert.assertNotEquals(eqClass.getMergedVertex(), vert);
	}
	
	/** Check that duplicate names are correctly detected, part 2. */
	@Test
	public final void testConstructionOfRepresentative4()
	{
		CmpVertex vert = new StringVertex("B");vert.setColour(JUConstants.AMBER);vert.setOrigState(VertexID.parseID("test"));
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass = 
			buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{vert});
		eqClass.constructMergedVertex(testGraphString, true, true);
		// no point comparing all attributes here since origState will be set to a different value than that of vert
		Assert.assertEquals(JUConstants.AMBER, eqClass.getMergedVertex().getColour());
		Assert.assertEquals(vert,eqClass.getMergedVertex().getOrigState());
		Assert.assertNotEquals(eqClass.getMergedVertex(), vert);
	}

	@Test
	public final void testAM_colour1()
	{
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq =buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
				new StringVertex("A"),new StringVertex("B"),new StringVertex("C")});
		eq.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertNull(eq.getMergedVertex().getColour());
	}
	
	@Test
	public final void testAM_colour2a()
	{
		CmpVertex vertB = new StringVertex("B");vertB.setColour(JUConstants.RED);
		CmpVertex vertC = new StringVertex("C");
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq =buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
				new StringVertex("A"),vertB,vertC});
		eq.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertSame(eq.getMergedVertex().getColour(), JUConstants.RED);
	}

	@Test
	public final void testAM_colour2b()
	{
		CmpVertex vertB = new StringVertex("B");vertB.setColour(JUConstants.BLUE);
		CmpVertex vertC = new StringVertex("C");
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq =buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
				new StringVertex("A"),vertB,vertC});
		eq.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertSame(eq.getMergedVertex().getColour(), JUConstants.BLUE);
	}

	@Test
	public final void testAM_colour2c()
	{
		CmpVertex vertB = new StringVertex("B");vertB.setColour(JUConstants.AMBER);
		CmpVertex vertC = new StringVertex("C");vertC.setColour(JUConstants.BLUE);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq =buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
				new StringVertex("A"),vertB,vertC});
		eq.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertSame(eq.getMergedVertex().getColour(), JUConstants.BLUE);
	}

	@Test
	public final void testAM_colour2d()
	{
		CmpVertex vertA = new StringVertex("A");vertA.setColour(JUConstants.AMBER);
		CmpVertex vertB = new StringVertex("B");
		CmpVertex vertC = new StringVertex("C");vertC.setColour(JUConstants.BLUE);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq =buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
				vertA,vertB,vertC});
		eq.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertSame(eq.getMergedVertex().getColour(), JUConstants.BLUE);
	}

	@Test
	public final void testAM_colour2e()
	{
		CmpVertex vertA = new StringVertex("A");vertA.setColour(JUConstants.AMBER);
		CmpVertex vertB = new StringVertex("B");
		CmpVertex vertC = new StringVertex("C");
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq =buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
				vertA,vertB,vertC});
		eq.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertNull(eq.getMergedVertex().getColour());
	}

	@Test
	public final void testAM_colour2f()
	{
		CmpVertex vertA = new StringVertex("A");vertA.setColour(JUConstants.AMBER);
		CmpVertex vertB = new StringVertex("B");
		CmpVertex vertC = new StringVertex("C");vertC.setColour(JUConstants.AMBER);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq =buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
				vertA,vertB,vertC});
		eq.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertNull(eq.getMergedVertex().getColour());
	}

	@Test
	public final void testAM_colour2g()
	{
		CmpVertex vertA = new StringVertex("A");
		CmpVertex vertB = new StringVertex("B");vertB.setColour(JUConstants.AMBER);
		CmpVertex vertC = new StringVertex("C");vertC.setColour(JUConstants.AMBER);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq =buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
				vertA,vertB,vertC});
		eq.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertNull(eq.getMergedVertex().getColour());
	}

	@Test
	public final void testAM_colour2h()
	{
		CmpVertex vertA = new StringVertex("A");vertA.setColour(JUConstants.AMBER);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq =buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
				vertA});
		eq.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertSame(eq.getMergedVertex().getColour(), JUConstants.AMBER);
	}

	@Test
	public final void testAM_colour3()
	{
		CmpVertex vertB = new StringVertex("B");vertB.setColour(JUConstants.RED);
		CmpVertex vertC = new StringVertex("C");vertC.setColour(JUConstants.BLUE);
		EquivalenceClass<CmpVertex,LearnerGraphCachedData> eq =buildClass(new AMEquivalenceClass<>(0, testGraphString),new CmpVertex[]{
				new StringVertex("A"),vertB,vertC});
		eq.constructMergedVertex(new LearnerGraph(config),false,false);
		Assert.assertSame(eq.getMergedVertex().getColour(), JUConstants.RED);
	}

	/** Tests of comparison/equality of string/deterministic pairs of vertices
	 * (string-"a"/string "b"/det-"a"/det-"b")
	 * each pair of above, against another pair. 
	 */
	@Test
	public final void testStatePairEquality()
	{
		final Object[] samePairs = new StatePair[]{
				new StatePair(new StringVertex("a"), new StringVertex("b")),
				new StatePair(new DeterministicVertex("a"), new StringVertex("b")),
				new StatePair(new StringVertex("a"), new DeterministicVertex("b")),
				new StatePair(new DeterministicVertex("a"), new DeterministicVertex("b"))				
		},
		differentPairs = new Object[] {
				new StatePair(new StringVertex("a"), new StringVertex("c")),
				new StatePair(new StringVertex("d"), new StringVertex("b")),
				new StatePair(new StringVertex("d"), new StringVertex("e")),
				constructOrigPair("a", "b")
		};
		for (Object samePair : samePairs)
			for (Object pair : samePairs)
				for (int different = 0; different < differentPairs.length; ++different)
					equalityTestingHelper(samePair, pair, differentPairs[different], differentPairs[differentPairs.length - different - 1],
							true);

		equalityTestingHelper(constructOrigPair("a","b"), constructOrigPair("a","b"),
				constructOrigPair("a","c"),constructOrigPair("b","b"), true);
		for(int i=0;i<samePairs.length;++i)
			equalityTestingHelper(constructOrigPair("a","b"), constructOrigPair("a","b"),
					samePairs[i],samePairs[samePairs.length-i-1], true);
	}
	
	/** Tests that nulls are valid elements of state pairs. */
	@Test
	public final void testStatePairEqualityWithNulls1()
	{
		final Object[] samePairs = new StatePair[]{
				new StatePair(null, new StringVertex("b")),
				new StatePair(null, new DeterministicVertex("b"))			
		},
		differentPairs = new Object[] {
				new StatePair(new StringVertex("b"), new StringVertex("b")),
				new StatePair(new DeterministicVertex("b"), new StringVertex("b")),
				new StatePair(null, null),
				constructOrigPair("a", "b")
		};
		for (Object samePair : samePairs)
			for (Object pair : samePairs)
				for (int different = 0; different < differentPairs.length; ++different)
					equalityTestingHelper(samePair, pair, differentPairs[different], differentPairs[differentPairs.length - different - 1], true);
		
	}
	
	/** Tests that nulls are valid elements of state pairs. */
	@Test
	public final void testStatePairEqualityWithNulls2()
	{
		final Object[] samePairs = new StatePair[]{
				new StatePair(new StringVertex("b"), null),
				new StatePair(new DeterministicVertex("b"),null)
		},
		differentPairs = new Object[] {
				new StatePair(new StringVertex("b"), new StringVertex("b")),
				new StatePair(new DeterministicVertex("b"), new StringVertex("b")),
				new StatePair(null, null),
				constructOrigPair("a", "b")
		};
		for (Object samePair : samePairs)
			for (Object pair : samePairs)
				for (int different = 0; different < differentPairs.length; ++different)
					equalityTestingHelper(samePair, pair, differentPairs[different], differentPairs[differentPairs.length - different - 1], true);

	}
	
	/** Tests that nulls are valid elements of state pairs. */
	@Test
	public final void testStatePairEqualityWithNulls3()
	{
		final Object[] samePairs = new StatePair[]{
				new StatePair(null, null)
		},
		differentPairs = new Object[] {
				new StatePair(null, new StringVertex("b")),
				new StatePair(new DeterministicVertex("b"), null),
				constructOrigPair("a", "b")
		};
		for (Object samePair : samePairs)
			for (Object pair : samePairs)
				for (int different = 0; different < differentPairs.length; ++different)
					equalityTestingHelper(samePair, pair, differentPairs[different], differentPairs[differentPairs.length - different - 1], true);
		
	}
	
	@SuppressWarnings("SameParameterValue")
	private static void checkLess(String a, String b, String c, String d)
	{
		checkLessHelper(new StatePair(new StringVertex(a), new StringVertex(b)), new StatePair(new StringVertex(c), new StringVertex(d)));
		checkLessHelper(new StatePair(new DeterministicVertex(a), new StringVertex(b)), new StatePair(new DeterministicVertex(c), new StringVertex(d)));
		checkLessHelper(new StatePair(new StringVertex(a), new DeterministicVertex(b)), new StatePair(new StringVertex(c), new DeterministicVertex(d)));
	}
	
	@Test
	public final void testStatePairComparison()
	{
		checkLess("a","b","c","d");
		checkLess("a","b","a","c");
		checkLess("a","b","c","b");
	}
	
	@Test
	public final void testStatePairComparisonWithNull()
	{
		checkLessHelper(new StatePair(null,null),new StatePair(new StringVertex("a"),null)); 
		checkLessHelper(new StatePair(null,null),new StatePair(null,new StringVertex("a"))); 
		checkLessHelper(new StatePair(null,new StringVertex("a")),new StatePair(new StringVertex("a"),null)); 
		checkLessHelper(new StatePair(null,new StringVertex("a")),new StatePair(new StringVertex("a"),new StringVertex("b"))); 
		checkLessHelper(new StatePair(new StringVertex("a"),null),new StatePair(new StringVertex("a"),new StringVertex("b")));
		Assert.assertEquals(0,new StatePair(null,null).compareTo(new StatePair(null,null)));
	}

	
	/** Checking that attributes which are cleared are not added to a Jung vertex. */
	@Test
	public final void testVertexConstruction()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(true);
		DeterministicVertex vertex = new DeterministicVertex("testVertex");
		vertex.setAccept(false);vertex.setHighlight(true);vertex.setColour(JUConstants.AMBER);vertex.setOrigState(VertexID.parseID("id"));vertex.setDepth(34);
		Assert.assertTrue(vertex.containsUserDatumKey(JUConstants.LABEL));
		Assert.assertTrue(vertex.containsUserDatumKey(JUConstants.ACCEPTED));
		Assert.assertTrue(vertex.containsUserDatumKey(JUConstants.HIGHLIGHT));
		Assert.assertTrue(vertex.containsUserDatumKey(JUConstants.COLOUR));
		Assert.assertTrue(vertex.containsUserDatumKey(JUConstants.ORIGSTATE));
		Assert.assertTrue(vertex.containsUserDatumKey(JUConstants.DEPTH));
		
		vertex.setAccept(true);vertex.setHighlight(false);vertex.setColour(null);vertex.setOrigState(null);vertex.setDepth(JUConstants.intUNKNOWN);
		Assert.assertTrue(vertex.containsUserDatumKey(JUConstants.LABEL));
		Assert.assertTrue(vertex.containsUserDatumKey(JUConstants.ACCEPTED));
		Assert.assertFalse(vertex.containsUserDatumKey(JUConstants.HIGHLIGHT));
		Assert.assertFalse(vertex.containsUserDatumKey(JUConstants.COLOUR));
		Assert.assertFalse(vertex.containsUserDatumKey(JUConstants.ORIGSTATE));
		Assert.assertFalse(vertex.containsUserDatumKey(JUConstants.DEPTH));
	}
	
	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_name1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		
		// here I cannot add, only set since the ID has already been set.
		vA.setUserDatum(JUConstants.LABEL, VertexID.parseID("name"), UserData.SHARED);Assert.assertEquals("name", vA.getStringId());
		vA.setUserDatum(JUConstants.LABEL, VertexID.parseID("D"), UserData.SHARED);Assert.assertEquals("D", vA.getStringId());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum(JUConstants.LABEL, VertexID.parseID("name"), UserData.SHARED);Assert.assertEquals("name", vS.getStringId());
	}
	
	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_name2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");

		// here I cannot add, only set since the ID has already been set.
		vA.setUserDatum("lAbel", VertexID.parseID("name"), UserData.SHARED);Assert.assertEquals("name", vA.getStringId());
		vA.setUserDatum("labEl", VertexID.parseID("D"), UserData.SHARED);Assert.assertEquals("D", vA.getStringId());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum("laBel", VertexID.parseID("name"), UserData.SHARED);Assert.assertEquals("name", vS.getStringId());
	}
	
	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_accepted1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);Assert.assertTrue(vA.isAccept());
		vA.setUserDatum(JUConstants.ACCEPTED, "false", UserData.SHARED);Assert.assertFalse(vA.isAccept());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum(JUConstants.ACCEPTED, "false", UserData.SHARED);Assert.assertFalse(vS.isAccept());
	}
	
	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_accepted2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("acCepted", "truE", UserData.SHARED);Assert.assertTrue(vA.isAccept());
		vA.setUserDatum("accePted", "faLse", UserData.SHARED);Assert.assertFalse(vA.isAccept());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum("acceptEd", "fAlse", UserData.SHARED);Assert.assertFalse(vS.isAccept());
	}
	
	@Test(expected=IllegalUserDataException.class)
	public final void testAddUserData_accepted_fail1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("accePted", "junk", UserData.SHARED);
	}

	@Test(expected=IllegalUserDataException.class)
	public final void testAddUserData_accepted_fail2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("accePted", new Object(), UserData.SHARED);
	}

	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_highlight1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		Assert.assertFalse(vA.isHighlight());
		vA.addUserDatum(JUConstants.HIGHLIGHT, "true", UserData.SHARED);Assert.assertTrue(vA.isHighlight());
		vA.setUserDatum(JUConstants.HIGHLIGHT, "false", UserData.SHARED);
		Assert.assertFalse(vA.isHighlight());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum(JUConstants.HIGHLIGHT, "false", UserData.SHARED);Assert.assertFalse(vS.isHighlight());
	}
	
	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_highlight2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		Assert.assertFalse(vA.isHighlight());
		vA.addUserDatum("highlIght", "tRue", UserData.SHARED);Assert.assertTrue(vA.isHighlight());
		vA.setUserDatum("hiGhlight", "falsE", UserData.SHARED);Assert.assertFalse(vA.isHighlight());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum("higHlight", "faLse", UserData.SHARED);Assert.assertFalse(vS.isHighlight());
	}
	
	@Test(expected=IllegalUserDataException.class)
	public final void testAddUserData_highlight_fail1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("higHlight", "junk", UserData.SHARED);
	}

	@Test(expected=IllegalUserDataException.class)
	public final void testAddUserData_highlight_fail2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("higHlight", new Object(), UserData.SHARED);
	}

	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_colour1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum(JUConstants.COLOUR, "rEd", UserData.SHARED);
		Assert.assertSame(JUConstants.RED, vA.getColour());
		vA.setUserDatum(JUConstants.COLOUR, "bLue", UserData.SHARED);
		Assert.assertSame(JUConstants.BLUE, vA.getColour());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum(JUConstants.COLOUR, "bLue", UserData.SHARED);
		Assert.assertSame(JUConstants.BLUE, vA.getColour());
	}
	
	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_colour2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("cOlour", "rEd", UserData.SHARED);
		Assert.assertSame(JUConstants.RED, vA.getColour());
		vA.setUserDatum("cOloUr", "bLue", UserData.SHARED);
		Assert.assertSame(JUConstants.BLUE, vA.getColour());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum("cOlouR", "blUe", UserData.SHARED);
		Assert.assertSame(JUConstants.BLUE, vA.getColour());
	}
	
	@Test(expected=IllegalUserDataException.class)
	public final void testAddUserData_colour_fail1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("cOlour", "junk", UserData.SHARED);
	}

	@Test(expected=IllegalUserDataException.class)
	public final void testAddUserData_colour_fail2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("cOlour", new Object(), UserData.SHARED);
	}

	/** Setting and resetting orig state works. */
	@Test
	public final void testAddUserData_Orig1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		VertexID vertID = VertexID.parseID("id");
		vA.addUserDatum(JUConstants.ORIGSTATE,vertID, UserData.SHARED);
		assertEquals(vertID, vA.getOrigState());
		vA.setOrigState(null);Assert.assertNull(vA.getOrigState());Assert.assertNull(vA.getUserDatum(JUConstants.ORIGSTATE));
		vA.addUserDatum(JUConstants.ORIGSTATE,"id", UserData.SHARED);
		assertEquals(vertID, vA.getOrigState());
	}
	
	/** Setting and resetting orig state using different names. */
	@Test
	public final void testAddUserData_Orig2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("oRigStatE", VertexID.parseID("id"), UserData.SHARED);
		assertEquals(VertexID.parseID("id"), vA.getOrigState());
		vA.setUserDatum("origstAte", VertexID.parseID("DD"), UserData.SHARED);
		assertEquals(VertexID.parseID("DD"), vA.getOrigState());
		vA.setUserDatum("origstAte", "PP", UserData.SHARED);
		assertEquals(VertexID.parseID("PP"), vA.getOrigState());
		vA.setUserDatum("origstAtE", "N56", UserData.SHARED);
		assertEquals(VertexID.parseID("N56"), vA.getOrigState());
	}
	
	/** Cannot set an illegal vertex ID. */
	@Test(expected=IllegalUserDataException.class)
	public final void testAddUserData_Orig_fail1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("oRigStatE", new Object(), UserData.SHARED);
	}

	/** Cannot set an illegal vertex ID. */
	@Test(expected=IllegalUserDataException.class)
	public final void testAddUserData_Orig_fail2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("oRigStatE", 23, UserData.SHARED);
	}

	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_anything()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		Object obj = new Object();
		vA.addUserDatum(obj, "aa", UserData.SHARED);Assert.assertEquals("aa",vA.getUserDatum(obj));
		vA.setUserDatum(obj, false, UserData.SHARED);Assert.assertFalse((Boolean)vA.getUserDatum(obj));

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum(obj, false, UserData.SHARED);Assert.assertFalse((Boolean)vS.getUserDatum(obj));
	}
	
	/** Setting and resetting depth works. */
	@Test
	public final void testAddUserData_Depth1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum(JUConstants.DEPTH,12, UserData.SHARED);Assert.assertEquals(12,vA.getDepth());
		vA.setUserDatum(JUConstants.DEPTH, JUConstants.intUNKNOWN, UserData.SHARED);Assert.assertNull(vA.getOrigState());
	}
	
	/** Setting and resetting depth works even if names of the attribute differ. */
	@Test
	public final void testAddUserData_Depth2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("dEpTH",12, UserData.SHARED);Assert.assertEquals(12,vA.getDepth());
		vA.setUserDatum("depTh",14, UserData.SHARED);Assert.assertEquals(14,vA.getDepth());
		vA.setUserDatum("depTh","45", UserData.SHARED);Assert.assertEquals(45,vA.getDepth());
	}
	
	/** Cannot set an illegal vertex ID. */
	@Test(expected=IllegalUserDataException.class)
	public final void testAddUserData_Depth_fail1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum(JUConstants.DEPTH, new Object(), UserData.SHARED);
	}

	/** Cannot set an illegal vertex ID. */
	@Test(expected=IllegalUserDataException.class)
	public final void testAddUserData_Depth_fail2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum(JUConstants.DEPTH, "A34", UserData.SHARED);
	}

	@Test
	public final void testLearnerGraph_toString()
	{
		Assert.assertEquals("Graph testFSMStructureClone states: 3", new LearnerGraph(testGraph,confJung).toString());
		Assert.assertEquals("Graph testFSMStructureClone states: 3", new LearnerGraph(testGraph,confString).toString());
		Assert.assertEquals("Graph testFSMStructureClone states: 3", new LearnerGraph(testGraph,confSame).toString());
		Assert.assertEquals("Graph <UNKNOWN> states: 1", new LearnerGraph(confSame).toString());
	}
	
	/** Non graph vertex to copy. */
	@Test(expected = IllegalArgumentException.class) 
	public final void testVertexClone_fail1()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(true);
		AbstractLearnerGraph.cloneCmpVertex("junk", conf);
	}
	
	/** Non-CmpVertex copying denied. */
	@Test(expected = IllegalArgumentException.class) 
	public final void testVertexClone_fail2()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(false);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, "name", UserData.SHARED);
		AbstractLearnerGraph.cloneCmpVertex(vertex, conf);
	}
	
	/** Unlabelled copying denied. */
	@Test(expected = IllegalArgumentException.class) 
	public final void testVertexClone_fail3()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();
		AbstractLearnerGraph.cloneCmpVertex(vertex, conf);
	}
	
	/** Copying of a vertex with a label which is neither a string nor a VertexID is denied. */
	@Test(expected = IllegalArgumentException.class) 
	public final void testVertexClone_fail4()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, true, UserData.SHARED);
		AbstractLearnerGraph.cloneCmpVertex(vertex, conf);
	}
	
	
	/** Normal copying successful. */
	@Test
	public final void testVertexClone1a()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, "name", UserData.SHARED);
		CmpVertex result = AbstractLearnerGraph.cloneCmpVertex(vertex, conf);
		Assert.assertEquals("name", result.getStringId());
		Assert.assertTrue(result.isAccept());Assert.assertFalse(result.isHighlight());Assert.assertNull(result.getColour());
		Assert.assertNull(result.getOrigState());Assert.assertEquals(JUConstants.intUNKNOWN,result.getDepth());
	}
	
	/** Normal copying successful. */
	@Test
	public final void testVertexClone1b()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, VertexID.parseID("name"), UserData.SHARED);
		CmpVertex result = AbstractLearnerGraph.cloneCmpVertex(vertex, conf);
		Assert.assertEquals("name", result.getStringId());
		Assert.assertTrue(result.isAccept());Assert.assertFalse(result.isHighlight());Assert.assertNull(result.getColour());
		Assert.assertNull(result.getOrigState());Assert.assertEquals(JUConstants.intUNKNOWN,result.getDepth());
	}
	
	/** Checking that attributes are preserved. */
	@Test
	public final void testVertexClone2()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, "name", UserData.SHARED);
		vertex.addUserDatum(JUConstants.HIGHLIGHT, 1, UserData.SHARED);
		vertex.addUserDatum(JUConstants.COLOUR, JUConstants.BLUE, UserData.SHARED);
		vertex.addUserDatum(JUConstants.ACCEPTED, false, UserData.SHARED);
		vertex.addUserDatum(JUConstants.ORIGSTATE, VertexID.parseID("test"), UserData.SHARED);
		vertex.addUserDatum(JUConstants.DEPTH, 12, UserData.SHARED);
		
		CmpVertex result = AbstractLearnerGraph.cloneCmpVertex(vertex, conf);
		Assert.assertEquals("name", result.getStringId());
		Assert.assertEquals(JUConstants.BLUE, result.getColour());
		Assert.assertFalse(result.isAccept());Assert.assertTrue(result.isHighlight());
		assertEquals(VertexID.parseID("test"), result.getOrigState());Assert.assertEquals(12,result.getDepth());
		
		vertex.removeUserDatum(JUConstants.ACCEPTED);vertex.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
		Assert.assertFalse(result.isAccept());
	}
	
	/** Checking that cloning can return the same vertex regardless of the value of setLearnerUseStrings. */
	@Test
	public final void testVertexClone3()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerUseStrings(false);conf.setLearnerCloneGraph(false);
		CmpVertex vA = new DeterministicVertex("test vertex");vA.setColour(JUConstants.RED);
		Assert.assertSame(vA, AbstractLearnerGraph.cloneCmpVertex(vA, conf));
	}
	
	/** Checking that cloning can return the same vertex regardless of the value of setLearnerUseStrings. */
	@Test
	public final void testVertexClone4()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerUseStrings(true);conf.setLearnerCloneGraph(false);
		CmpVertex vA = new DeterministicVertex("test vertex");vA.setColour(JUConstants.RED);
		Assert.assertSame(vA, AbstractLearnerGraph.cloneCmpVertex(vA, conf));
	}
	
	@Test
	public final void testCopyVertex()
	{
		DirectedSparseGraph graphB = FsmParser.buildLearnerGraph("A-a->B-b->C","testCopyVertex",config,converter).pathroutines.getGraph();
		Map<VertID,DeterministicVertex> vertexMap = new TreeMap<>();

		DeterministicVertex vertD = new DeterministicVertex("D");
		DeterministicVertex copyOfD = DeterministicDirectedSparseGraph.copyVertex(vertexMap, graphB, vertD);
		Assert.assertNotSame(vertD,copyOfD);Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(vertD, copyOfD));
		Assert.assertEquals(1,vertexMap.size());
		DeterministicVertex anotherCopyOfD = DeterministicDirectedSparseGraph.copyVertex(vertexMap, graphB, vertD);
		Assert.assertSame(copyOfD,anotherCopyOfD);
		
		DirectedSparseGraph graphC = new DirectedSparseGraph();
		DeterministicVertex initial = new DeterministicVertex("init");
		initial.setUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		DeterministicVertex copyOfInitial = DeterministicDirectedSparseGraph.copyVertex(vertexMap, graphC, initial);
		Assert.assertNotSame(initial,copyOfInitial);Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(initial, copyOfInitial));
		Assert.assertEquals(2,vertexMap.size());
		LearnerGraph gr = new LearnerGraph(graphC,config);Assert.assertEquals(1,gr.getStateNumber());
		Assert.assertEquals("init",gr.getInit().getStringId());
	}
	
	private static void testColourHelper(CmpVertex vert)
	{
		Assert.assertNull(vert.getColour());
		vert.setColour(JUConstants.RED);Assert.assertSame(JUConstants.RED, vert.getColour());
		vert.setColour(JUConstants.BLUE);Assert.assertSame(JUConstants.BLUE, vert.getColour());
		vert.setColour(null);Assert.assertNull(vert.getColour());
		
		// Now check invalid colours
		try
		{
			vert.setColour(JUConstants.HIGHLIGHT);
			Assert.fail("exception expected here");
		}
		catch(IllegalUserDataException ex)
		{// exception is expected here, continue			
		}
		Assert.assertNull(vert.getColour());

		// check that settting of an invalid colour does not mess up the previous one.
		vert.setColour(JUConstants.BLUE);Assert.assertSame(JUConstants.BLUE, vert.getColour());
		try
		{
			vert.setColour(JUConstants.ACCEPTED);
			Assert.fail("exception expected here");
		}
		catch(IllegalUserDataException ex)
		{// exception is expected here, continue			
		}
		Assert.assertSame(JUConstants.BLUE, vert.getColour());
	}
	
	/** Tests that I can assign colours meaningfully on Deterministic and String vertices. */
	@Test
	public final void testColourSetting1()
	{
		testColourHelper(new DeterministicVertex("testA"));
	}
	
	/** Tests that I can assign colours meaningfully on Deterministic and String vertices. */
	@Test
	public final void testColourSetting2()
	{
		testColourHelper(new StringVertex("testB"));
	}
	
	private static void cloneTestHelper(CmpVertex vert, Configuration conf)
	{
		vert.setAccept(true);
		CmpVertex vert_clone = AbstractLearnerGraph.cloneCmpVertex(vert, conf);
		Assert.assertNotSame(vert, vert_clone);Assert.assertEquals("test vertex", vert_clone.getStringId());Assert.assertEquals(vert, vert_clone);
		Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(vert, vert_clone));

		vert.setAccept(false);
		Assert.assertEquals(JUConstants.RED, vert_clone.getColour());
		assertEquals(vert, vert_clone);// acceptance is ignored in vertex comparisons
		Assert.assertFalse(DeterministicDirectedSparseGraph.deepEquals(vert, vert_clone));// but considered in deep comparisons		
	}
	
	/** Checking that vertex cloning is faithful. */
	@Test
	public final void testVertexClone5()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerCloneGraph(true);
		
		conf.setLearnerUseStrings(true);
		CmpVertex vA = new DeterministicVertex("test vertex");vA.setColour(JUConstants.RED);
		cloneTestHelper(vA, conf);
	}
	
	/** Checking that vertex cloning is faithful. */
	@Test
	public final void testVertexClone6()
	{
		Configuration conf = Configuration.getDefaultConfiguration().copy();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerCloneGraph(true);
		
		conf.setLearnerUseStrings(false);
		CmpVertex vB = new DeterministicVertex("test vertex");vB.setColour(JUConstants.RED);
		cloneTestHelper(vB, conf);
	}

	@Test
	public final void updateDiagram_remove0()
	{
	// "A-a->A-b->B-c->B\nA-c-#C\nB-b->B"	
		assertEquals(testGraphSame, testGraphString);
		testGraphString.removeTransition(testGraphString.transitionMatrix.get(testGraphString.findVertex("B")),AbstractLearnerGraph.generateNewLabel("d",config,converter),testGraphString.findVertex("A"));
		assertEquals(testGraphSame, testGraphString);
	}

	/** Tests that in a deterministic case <em>removeTransition</em> disregards the target state.
	 */
	@Test
	public final void updateDiagram_remove1()
	{
		assertEquals(testGraphSame, testGraphString);
		testGraphString.removeTransition(testGraphString.transitionMatrix.get(testGraphString.findVertex("B")),AbstractLearnerGraph.generateNewLabel("c",config,converter),testGraphString.findVertex("A"));
		assertEquals(buildLearnerGraph("A-a->A-b->B\nA-c-#C\nB-b->B", "updateDiagram_add2", config, converter), testGraphString);
	}

	/** Tests that a transition diagram can be updated for both deterministic 
	 * and non-deterministic graphs using the same routines. 
	 */
	@Test
	public final void updateDiagram_remove2()
	{
	// "A-a->A-b->B-c->B\nA-c-#C\nB-b->B"	
		testGraphString.removeTransition(testGraphString.transitionMatrix.get(testGraphString.findVertex("B")),AbstractLearnerGraph.generateNewLabel("c",config,converter),testGraphString.findVertex("B"));
		assertEquals(buildLearnerGraph("A-a->A-b->B\nA-c-#C\nB-b->B", "updateDiagram_add2", config, converter), testGraphString);
	}

	/** Tests that a transition diagram can be updated for both deterministic 
	 * and non-deterministic graphs using the same routines. 
	 */
	@Test
	public final void updateDiagram_add1()
	{
	// "A-a->A-b->B-c->B\nA-c-#C\nB-b->B"	
		testGraphString.addTransition(testGraphString.transitionMatrix.get(testGraphString.findVertex("B")),AbstractLearnerGraph.generateNewLabel("d",config,converter),testGraphString.findVertex("A"));
		assertEquals(buildLearnerGraph("A-a->A-b->B-c->B\nA-c-#C\nB-b->B-d->A", "updateDiagram_add1", config, converter), testGraphString);
	}
	
	/** Tests that it is not possible to add a transition leading to a non-determinism.
	 */
	@Test
	public final void updateDiagram_fail1()
	{
		final LearnerGraph graph = testGraphString;
		Helper.checkForCorrectException(
				() -> graph.addTransition(graph.transitionMatrix.get(graph.findVertex("B")),
						AbstractLearnerGraph.generateNewLabel("c",config,converter),graph.findVertex("A")),
				IllegalArgumentException.class,"non-determinism");
	}
	
	/** Tests that it is not possible to add a duplicate transition.
	 */
	@Test
	public final void updateDiagram_fail2()
	{
		final LearnerGraph graph = testGraphString;
		Helper.checkForCorrectException(
				() -> graph.addTransition(graph.transitionMatrix.get(graph.findVertex("B")),
						AbstractLearnerGraph.generateNewLabel("c",config,converter),graph.findVertex("B")),
				IllegalArgumentException.class,"non-determinism");
	}
	
	/** Tests that a transition diagram can be updated for both deterministic 
	 * and non-deterministic graphs using the same routines. 
	 */
	@Test
	public final void updateDiagramND_remove1()
	{
		final LearnerGraphND graph = new LearnerGraphND(testGraph,config);
		graph.removeTransition(graph.transitionMatrix.get(graph.findVertex("B")),AbstractLearnerGraph.generateNewLabel("c",config,converter),graph.findVertex("A"));
		assertEquals(testGraphString, graph);
	}

	/** Tests that a transition diagram can be updated for both deterministic 
	 * and non-deterministic graphs using the same routines. 
	 */
	@Test
	public final void updateDiagramND_remove2()
	{
		final LearnerGraphND graph = new LearnerGraphND(testGraph,config);
		graph.removeTransition(graph.transitionMatrix.get(graph.findVertex("B")),AbstractLearnerGraph.generateNewLabel("c",config,converter),graph.findVertex("B"));
		Assert.assertNull(graph.transitionMatrix.get(graph.findVertex("B")).get(AbstractLearnerGraph.generateNewLabel("c",config,converter)));
		assertEquals(buildLearnerGraph("A-a->A-b->B\nA-c-#C\nB-b->B", "updateDiagram_add2", config, converter), graph);
	}

	/** Tests that a transition diagram can be updated for both deterministic 
	 * and non-deterministic graphs using the same routines.
	 */
	@Test
	public final void updateDiagramND_remove3()
	{
		final LearnerGraphND graph = buildLearnerGraphND("A-a->B\nA-a->C\nA-a-#D\nB-b->C\nA-c->C", "testbuildDeterministicGraph_fail2",config,converter);
		graph.removeTransition(graph.transitionMatrix.get(graph.findVertex("A")),AbstractLearnerGraph.generateNewLabel("a",config,converter),graph.findVertex("C"));
		Set<CmpVertex> targets = new TreeSet<>();
		targets.add(graph.findVertex("B"));targets.add(graph.findVertex("D"));
		Set<CmpVertex> actual = new TreeSet<>(graph.getTargets(graph.transitionMatrix.get(graph.getInit()).get(AbstractLearnerGraph.generateNewLabel("a", config, converter))));
		Assert.assertEquals(targets,actual);
	}

	/** Tests that a transition diagram can be updated for both deterministic 
	 * and non-deterministic graphs using the same routines. 
	 */
	@Test
	public final void updateDiagramND_add1()
	{
		final LearnerGraphND graph = new LearnerGraphND(testGraph,config);
		Helper.checkForCorrectException(() -> graph.addTransition(graph.transitionMatrix.get(graph.findVertex("B")),AbstractLearnerGraph.generateNewLabel("c",config,converter),graph.findVertex("B")),IllegalArgumentException.class,"duplicate transition");
	}

	/** Tests that a transition diagram can be updated for both deterministic 
	 * and non-deterministic graphs using the same routines. 
	 */
	@Test
	public final void updateDiagramND_add2()
	{
		final LearnerGraphND graph = new LearnerGraphND(testGraph,config);
		graph.addTransition(graph.transitionMatrix.get(graph.findVertex("B")),AbstractLearnerGraph.generateNewLabel("c",config,converter),graph.findVertex("A"));
		assertEquals(buildLearnerGraphND("A-a->A-b->B-c->B\nA-c-#C\nB-b->B\nB-c->A", "updateDiagramND_add2", config, converter), graph);
	}

	/** Tests that a transition diagram can be updated for both deterministic 
	 * and non-deterministic graphs using the same routines. 
	 */
	@Test
	public final void updateDiagramND_add3()
	{
		final LearnerGraphND graph = new LearnerGraphND(testGraph,config);
		graph.addTransition(graph.transitionMatrix.get(graph.findVertex("B")),AbstractLearnerGraph.generateNewLabel("c",config,converter),graph.findVertex("A"));
		graph.addTransition(graph.transitionMatrix.get(graph.findVertex("A")),AbstractLearnerGraph.generateNewLabel("b",config,converter),graph.findVertex("C"));
		assertEquals(buildLearnerGraphND("A-a->A-b->B-c->B\nA-c-#C\nB-b->B\nB-c->A-b-#C", "updateDiagramND_add2", config, converter), graph);
	}

	/** Tests that a transition diagram can be updated for both deterministic 
	 * and non-deterministic graphs using the same routines. 
	 */
	@Test
	public final void updateDiagram_add2()
	{
		final LearnerGraphND graph = new LearnerGraphND(testGraph,config);
		graph.addTransition(graph.transitionMatrix.get(graph.findVertex("B")),AbstractLearnerGraph.generateNewLabel("c",config,converter),graph.findVertex("A"));
		assertEquals(buildLearnerGraphND("A-a->A-b->B-c->A\nB-c->B\nA-c-#C\nB-b->B", "updateDiagram_add2", config, converter), graph);
	}
	
	/** Tests that access to target states is correctly handled, non-deterministic case. */
	@Test
	public final void testAccessToTargetStates1()
	{
		final LearnerGraphND graph = buildLearnerGraphND("A-a->B\nA-a->C\nA-a-#D\nB-b->C\nA-c->C", "testbuildDeterministicGraph_fail2",config,converter);
		Set<CmpVertex> targets = new TreeSet<>();
		targets.add(graph.findVertex("B"));targets.add(graph.findVertex("D"));targets.add(graph.findVertex("C"));
		Set<CmpVertex> actual = new TreeSet<>(graph.getTargets(graph.transitionMatrix.get(graph.getInit()).get(AbstractLearnerGraph.generateNewLabel("a", config, converter))));
		Assert.assertEquals(targets,actual);
	}
	
	/** Tests that access to target states is correctly handled, deterministic case. */
	@Test
	public final void testAccessToTargetStates2()
	{
		final LearnerGraph graph = buildLearnerGraph("A-a->B\nA-a2->C\nA-a3-#D\nB-b->C\nA-c->C", "testAccessToTargetStates2",config,converter);
		Collection<CmpVertex> targets = graph.getTargets(graph.transitionMatrix.get(graph.getInit()).get(AbstractLearnerGraph.generateNewLabel("a",config,converter)));
		Assert.assertFalse(targets.isEmpty());Assert.assertEquals(1,targets.size());
		Assert.assertTrue(targets.contains(graph.findVertex("B")));
		Iterator<CmpVertex> data = targets.iterator();
		Assert.assertTrue(data.hasNext());Assert.assertSame(graph.findVertex("B"),data.next());
		Assert.assertFalse(data.hasNext());
	}
	
	@Test
	public final void testFSMStructureEquals0()
	{
		LearnerGraph a=new LearnerGraph(config),b=new LearnerGraph(config);
		assertEquals(a, a);
		assertEquals(a, b);

		Assert.assertNotEquals(null, a);
		Assert.assertNotEquals("hello", a);
		b.setInit(new StringVertex("B"));
		Assert.assertNotEquals(a, b);
	}

	@Test
	public final void testFSMStructureEquals1()
	{
		LearnerGraph a=new LearnerGraph(config),b=new LearnerGraph(config);
		a.setInit(new StringVertex("A"));b.setInit(new StringVertex("A"));
		assertEquals(a, a);
		assertEquals(a, b);

		Assert.assertNotEquals(null, a);
		Assert.assertNotEquals("hello", a);
		b.setInit(new StringVertex("B"));
		Assert.assertNotEquals(a, b);
	}

	/** Same as above, but with something in a transition matrix. */
	@Test
	public final void testFSMStructureEquals2()
	{
		LearnerGraph a=new LearnerGraph(config),b=new LearnerGraph(config);
		a.setInit(new StringVertex("A"));b.setInit(new StringVertex("A"));
		a.transitionMatrix.put(a.getInit(),a.createNewRow());b.transitionMatrix.put(b.getInit(),b.createNewRow());
		assertEquals(a, a);
		assertEquals(a, b);

		Assert.assertNotEquals(null, a);
		Assert.assertNotEquals("hello", a);
		b.setInit(new StringVertex("B"));
		Assert.assertNotEquals(a, b);
	}

	@Test
	public final void testFSMStructureEquals3()
	{
		LearnerGraph a=new LearnerGraph(config),b=new LearnerGraph(config);
		a.setInit(new StringVertex("A"));b.setInit(new StringVertex("A"));
		a.transitionMatrix.put(a.getInit(),a.createNewRow());b.transitionMatrix.put(b.getInit(),b.createNewRow());
		assertEquals(a, a);
		assertEquals(a, b);

		b.getInit().setAccept(false);
		Assert.assertNotEquals(a, b);
	}

	/** Tests that graphs of different kind look the same. */
	@Test
	public final void testFSMStructureEquals4()
	{
		Assert.assertTrue(testGraphString.getInit() instanceof StringVertex);
		Assert.assertTrue(testGraphJung.getInit() instanceof DeterministicVertex);
		Assert.assertTrue(testGraphSame.getInit() instanceof StringVertex);
		equalityTestingHelper(testGraphJung,testGraphString,differentA,differentB, true);
		equalityTestingHelper(testGraphJung,testGraphSame,differentA,differentB, true);
		equalityTestingHelper(testGraphString,testGraphSame,differentA,differentB, true);
	}

	/** Tests the recorded state compatibility is taken into account. */ 
	@Test
	public final void testFSMStructureEquals5()
	{
		String graph = "A-a->B-a->C";
		LearnerGraph a=buildLearnerGraph(graph,"testFSMStructureEquals5a",config,converter),
			b=buildLearnerGraph(graph,"testFSMStructureEquals5b",config,converter);

		a.addToCompatibility(a.findVertex("A"), a.findVertex("C"), JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		equalityTestingHelper(a,a,b,differentA, true);
		
		a.removeFromIncompatibles(a.findVertex("A"), a.findVertex("C"));
		equalityTestingHelper(a,b,differentA,differentB, true);
		
		a.addToCompatibility(a.findVertex("A"), a.findVertex("C"), JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		b.addToCompatibility(b.findVertex("A"), b.findVertex("C"), JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		equalityTestingHelper(a,b,differentA,differentB, true);
	}

	/** Tests the recorded state compatibility is taken into account. */ 
	@Test
	public final void testFSMStructureEquals6()
	{
		String graph = "A-a->B-a->C";
		LearnerGraph a=buildLearnerGraph(graph,"testFSMStructureEquals5a",config,converter),
			b=buildLearnerGraph(graph,"testFSMStructureEquals5b",config,converter);

		a.addToCompatibility(a.findVertex("A"), a.findVertex("C"), JUConstants.PAIRCOMPATIBILITY.MERGED);
		b.addToCompatibility(b.findVertex("A"), b.findVertex("C"), JUConstants.PAIRCOMPATIBILITY.MERGED);
		equalityTestingHelper(a,b,differentA,differentB, true);

		a.addToCompatibility(a.findVertex("A"), a.findVertex("C"), JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		equalityTestingHelper(a,a,b,differentA, true);

		b.addToCompatibility(b.findVertex("A"), b.findVertex("C"), JUConstants.PAIRCOMPATIBILITY.INCOMPATIBLE);
		equalityTestingHelper(a,b,differentA,differentB, true);
	}

	@Test
	public final void testCopyGraph0()
	{
		DirectedSparseGraph g=new DirectedSparseGraph();
		g.addVertex(new DirectedSparseVertex());
		g.addVertex(new DirectedSparseVertex());
		DirectedSparseGraph copy = DeterministicDirectedSparseGraph.copy(g);
		Assert.assertTrue(copy.getEdges().isEmpty() && copy.getVertices().isEmpty());
	}
	
	/** Yet another test that copy works. */
	@Test
	public final void testCopyGraph1()
	{
		DirectedSparseGraph g=FsmParser.buildLearnerGraph("S-a->S1", "testCopyGraph",config,converter).pathroutines.getGraph();
		DirectedSparseGraph copy=DeterministicDirectedSparseGraph.copy(g);
		LearnerGraph gS = new LearnerGraph(g,config),gC = new LearnerGraph(copy,config);

		assertEquals(gS, gC);
	}
	
	/** Yet another test that copy works. */
	@SuppressWarnings("unchecked")
	@Test
	public final void testCopyGraph2()
	{
		DirectedSparseGraph g=FsmParser.buildLearnerGraph("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+TestRpniLearner.PTA3, "testCopyGraph2",config,converter).pathroutines.getGraph();
		DirectedSparseGraph copy=DeterministicDirectedSparseGraph.copy(g);
		LearnerGraph gS = new LearnerGraph(g,config),gCopy = new LearnerGraph(copy,config);

		assertEquals(gS, gCopy);
		
		// now test if all clones are faithful
		for(Edge e:(Set<Edge>)g.getEdges())
			((Set<Label>)e.getUserDatum(JUConstants.LABEL)).add(AbstractLearnerGraph.generateNewLabel("junk",config,converter));
		
		LearnerGraph gS_Modified = new LearnerGraph(copy,config);

		assertEquals(gS_Modified, gCopy);
	}
	
	/** Yet another test that copy works. */
	@Test
	public final void testCopyGraph3() // this one tests that clone works
	{
		DirectedSparseGraph g=FsmParser.buildLearnerGraph("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+TestRpniLearner.PTA3, "testCopyGraph2",config,converter).pathroutines.getGraph();
		LearnerGraph orig = new LearnerGraph(g,config);
		LearnerGraph copy = new LearnerGraph(orig,config);
		LearnerGraph gS = new LearnerGraph(orig.pathroutines.getGraph(),config),
			gCopy = new LearnerGraph(copy.pathroutines.getGraph(),config);

		assertEquals(gS, gCopy);
		
		// now test if all clones are faithful by modifying the first graph
		orig.findVertex("A3").setAccept(false);
		
		LearnerGraph gS_afterChange = new LearnerGraph(orig.pathroutines.getGraph(),config), 
			gCopy_afterChange = new LearnerGraph(copy.pathroutines.getGraph(),config);

		assertEquals(gCopy_afterChange, gCopy);
		assertEquals(gCopy_afterChange, gS);
		Assert.assertNotEquals(gS_afterChange, gCopy);
		Assert.assertNotEquals(gS_afterChange, gS);
	}

	/** Tests that it is possible to copy a deterministic graph into a non-deterministic one. */
	@Test
	public final void testGraphCopying1()
	{
		LearnerGraph graph = buildLearnerGraph("A-a->B-a->C-a->D\nB-b->D", "testGraphCopying1",config,converter);
		Configuration orderedConfig = Configuration.getDefaultConfiguration().copy();orderedConfig.setUseOrderedEntrySet(true);
		LearnerGraphND otherGraph = new LearnerGraphND(orderedConfig);
		AbstractLearnerGraph.copyGraphs(graph, otherGraph);
		assertEquals(otherGraph, graph);
	}
	
	/** Tests that it is not possible to copy a non-deterministic graph into a deterministic one. */
	@Test
	public final void testGraphCopying2()
	{
		final LearnerGraphND graph = buildLearnerGraphND("A-a->B-a->C-a->D\nB-b->D\nB-a->A", "testGraphCopying2",config,converter);
		final LearnerGraph otherGraph = new LearnerGraph(Configuration.getDefaultConfiguration());
		Helper.checkForCorrectException(() -> AbstractLearnerGraph.copyGraphs(graph, otherGraph),IllegalArgumentException.class, "non-determinism");
	}
	
	/** Tests that (1) it is possible to convert any type of a graph to a different type of graph
	 * and (2) getGraph does its job.
	 */
	@Test
	public final void testFSMStructureClone3()
	{
		ArrayList<LearnerGraph> origGraphs = new ArrayList<>(3);origGraphs.add(testGraphJung);origGraphs.add(testGraphSame);origGraphs.add(testGraphString);
		List<LearnerGraph> origAndCloned = new LinkedList<>();
		
		for(LearnerGraph g:origGraphs)
		{
			origAndCloned.add(g);
			Configuration copyConfig = g.config.copy();
			copyConfig.setLearnerUseStrings(false);copyConfig.setLearnerCloneGraph(true);
			LearnerGraph cloneJung = new LearnerGraph(g,copyConfig);
			Assert.assertTrue(cloneJung.getInit() instanceof DeterministicVertex);

			copyConfig.setLearnerUseStrings(true);copyConfig.setLearnerCloneGraph(true);
			LearnerGraph cloneStrings = new LearnerGraph(g,copyConfig);
			Assert.assertTrue(cloneStrings.getInit() instanceof StringVertex);

			copyConfig.setLearnerUseStrings(false);copyConfig.setLearnerCloneGraph(false);
			LearnerGraph cloneSame = new LearnerGraph(g,copyConfig);
			assertEquals(cloneSame.getInit().getClass(), g.getInit().getClass());
			Assert.assertSame(cloneSame.getInit(), g.getInit());

			origAndCloned.add(cloneJung);origAndCloned.add(cloneStrings);origAndCloned.add(cloneSame);
		}
		List<LearnerGraph> afterGetGraph = new LinkedList<>();
		
		// Now add results of getGraph, considering all combinations of "getGraph" configuration and "new LearnerGraph" configuration
		for(LearnerGraph g:origAndCloned)
			for(Configuration conf: new Configuration[]{confJung,confSame,confString})
			{
				g.config.setLearnerUseStrings(false);g.config.setLearnerCloneGraph(true);
				afterGetGraph.add(new LearnerGraph(g.pathroutines.getGraph(),conf));
				g.config.setLearnerUseStrings(true);g.config.setLearnerCloneGraph(true);
				afterGetGraph.add(new LearnerGraph(g.pathroutines.getGraph(),conf));
				
				DirectedSparseGraph someGraph = g.paths.OrigGetGraph("someName");
				Assert.assertEquals("someName", someGraph.getUserDatum(JUConstants.TITLE));
				afterGetGraph.add(new LearnerGraph(someGraph,conf));
				
				g.config.setLearnerUseStrings(false);g.config.setLearnerCloneGraph(false);
				DirectedSparseGraph almostOrigGraph = g.pathroutines.getGraph("new Name");
				Assert.assertEquals("new Name", almostOrigGraph.getUserDatum(JUConstants.TITLE));
				Assert.assertEquals(DeterministicDirectedSparseGraph.findInitial(almostOrigGraph), g.getInit());
				afterGetGraph.add(new LearnerGraph(almostOrigGraph,conf));
			}
		
		for(LearnerGraph gFirst:afterGetGraph)
			for(LearnerGraph gSecond:afterGetGraph)
				equalityTestingHelper(gFirst,gSecond,differentA,differentB, true);
	}
	
	@Test
	public void testStringVertexUsesAllItsAttributes()
	{	
		List<MethodAndArgs<StringVertex>> MethodsArgs = AttributeMutator.constructArgList(StringVertex.class);
		
		// Now check that hashCode and equals are affected by values of different fields.
		// ID is not included because it is declared final.
		for(MethodAndArgs<StringVertex> currentMethod:MethodsArgs)
		{
			StringVertex vertexA = new StringVertex("P"), vertexB = new StringVertex("P");
			for(MethodAndArgs<StringVertex> orig:MethodsArgs)
			{// resets vertexA and vertexB to the original values
				orig.assignA(vertexA);
				orig.assignA(vertexB);
			}
			Assert.assertEquals(vertexA, vertexB);
			Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(vertexA,vertexB));
			Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(vertexA,vertexA));
			Assert.assertTrue(DeterministicDirectedSparseGraph.deepEquals(vertexB,vertexB));
			
			currentMethod.assignB(vertexB);// mutates B's attribute
			String errMsg = "StringVertices differ: field "+currentMethod.getField()+" is/not in use for ";
			assertEquals(errMsg + "equals", vertexB, vertexA);
			assertEquals(errMsg + "equals", vertexA, vertexB);
			Assert.assertFalse(errMsg+"deepEquals",DeterministicDirectedSparseGraph.deepEquals(vertexA,vertexB));
			Assert.assertFalse(errMsg+"deepEquals",DeterministicDirectedSparseGraph.deepEquals(vertexB,vertexA));

			assertEquals(errMsg + "hashCode", vertexA.hashCode(), vertexB.hashCode());
		}
	}

}
