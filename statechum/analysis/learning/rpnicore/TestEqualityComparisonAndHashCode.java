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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildGraph;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.StringVertex;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.DeterministicDirectedSparseGraph.CmpVertex.IllegalUserDataException;
import statechum.DeterministicDirectedSparseGraph.VertexID.VertKind;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.TestRpniLearner;
import statechum.analysis.learning.Test_Orig_RPNIBlueFringeLearner.OrigStatePair;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class TestEqualityComparisonAndHashCode {
	public TestEqualityComparisonAndHashCode()
	{
		mainConfiguration = Configuration.getDefaultConfiguration();
	}
	
	/** Graph used in tests of cloning. */
	private DirectedSparseGraph testGraph = null;

	/** The configuration to use when running tests. */
	private Configuration config = null, mainConfiguration = null;
	
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
		
		config = mainConfiguration.copy();
		differentA = new LearnerGraph(buildGraph("Q-a->A-b->B", "testFSMStructureEquals2"),config);
		differentB = new LearnerGraph(buildGraph("A-b->A\nB-b->B", "testFSMStructureEquals2"),config);

		confJung = config.copy();confJung.setLearnerUseStrings(false);confJung.setLearnerCloneGraph(true);
		confString = config.copy();confString.setLearnerUseStrings(true);confString.setLearnerCloneGraph(true);
		confSame = config.copy();confSame.setLearnerUseStrings(false);confSame.setLearnerCloneGraph(false);
		testGraph = buildGraph("A-a->A-b->B-c->B\nA-c-#C\nB-b->B", "testFSMStructureClone");
		testGraphJung=new LearnerGraph(testGraph,confJung.copy());// clone here makes it possible to change the configuration later without affecting objects in this object
		testGraphString=new LearnerGraph(testGraph,confString.copy());
		testGraphSame=new LearnerGraph(testGraph,confSame.copy());
		LearnerGraph.testMode = true;		
	}
	/** FSMs used in testing of cloning. */
	private LearnerGraph testGraphJung,testGraphString,testGraphSame;

	
	static final protected OrigStatePair constructOrigPair(String a,String b)
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
	 */
	static final public void equalityTestingHelper(Object p, Object q, 
			Object differentA, Object differentB)
	{
		assertTrue(p.equals(p));assertTrue(q.equals(q));
		assertTrue(p.equals(q));assertTrue(q.equals(p));
		assertTrue(p.hashCode() == q.hashCode());assertTrue(p.hashCode() != 0);assertTrue(q.hashCode() != 0);
		assertFalse(p.equals(null));assertFalse(q.equals(null));
		
		assertFalse(p.equals("test"));assertFalse(q.equals("test"));
		assertFalse("test".equals(p));assertFalse("test".equals(q));
		
		Object obj = new Object();assertTrue(obj.equals(obj));
		assertFalse(p.equals(obj));assertFalse(q.equals(obj));
		assertFalse(obj.equals(p));assertFalse(obj.equals(q));
		
		assertFalse(p.equals(differentA));assertFalse(q.equals(differentA));
		if (differentA != null)
		{
			assertFalse(differentA.equals(p));
			assertFalse(differentA.equals(q));
			assertFalse(p.hashCode() == differentA.hashCode());
			assertFalse(q.hashCode() == differentA.hashCode());
		}
		if (differentB != null)
		{
			assertFalse(differentB.equals(p));
			assertFalse(differentB.equals(q));
			assertFalse(p.hashCode() == differentB.hashCode());
			assertFalse(q.hashCode() == differentB.hashCode());
		}
	}
	
	/** Used to check that compareTo method works well. */ 
	@SuppressWarnings("unchecked")
	static final private void checkLessHelper(Comparable p, Comparable q)
	{
		assertFalse(p.equals(q));assertFalse(p.hashCode() == q.hashCode());
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
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
		Assert.assertEquals(0, id.getIngegerID());
	}
	
	@Test
	public final void testParseID6()
	{
		VertexID id = VertexID.parseID("P100789");
		Assert.assertEquals(VertKind.POSITIVE,id.getKind());
		Assert.assertEquals("P100789",id.toString());
		Assert.assertEquals(100789, id.getIngegerID());
	}
	
	@Test
	public final void testParseID7()
	{
		VertexID id = VertexID.parseID("N100789");
		Assert.assertEquals(VertKind.NEGATIVE,id.getKind());
		Assert.assertEquals("N100789",id.toString());
		Assert.assertEquals(100789, id.getIngegerID());
	}
	
	
	/** Tests that it is not possible to create an invalid vertexid. */
	@Test(expected=IllegalArgumentException.class)
	public final void testCannotCreateNoneVertexID1()
	{
		new VertexID(VertKind.NONE,3);
	}
	
	/** Tests that it is not possible to create an invalid vertexid. */
	@Test(expected=IllegalArgumentException.class)
	public final void testCannotCreateNoneVertexID2()
	{
		new VertexID(null);
	}
	
	/** Tests equality for VertexIDs. */
	@Test
	public final void testVertexIDEquals1()
	{
		equalityTestingHelper(new VertexID("A"), new VertexID("A"), new VertexID("B"), new VertexID("C"));
	}

	/** Tests equality for VertexIDs. */
	@Test
	public final void testVertexIDEquals2()
	{
		equalityTestingHelper(new VertexID(VertKind.POSITIVE,5), new VertexID(VertKind.POSITIVE,5), new VertexID(VertKind.NEGATIVE,9), new VertexID(VertKind.NEUTRAL,9));
	}

	public final static String 
		idP5 = new VertexID(VertKind.POSITIVE,5).getStringId(),
		idN5 = new VertexID(VertKind.NEGATIVE,5).getStringId(),
		idP6 = new VertexID(VertKind.POSITIVE,6).getStringId();
	
	/** Tests equality for VertexIDs. */
	@Test
	public final void testVertexIDEquals3()
	{
		equalityTestingHelper(new VertexID(VertKind.NEGATIVE,5), new VertexID(VertKind.NEGATIVE,5), new VertexID(VertKind.POSITIVE,5), new VertexID(VertKind.NEUTRAL,5));
	}
	
	/** Tests equality for VertexIDs with string and numerical IDs. */
	@Test
	public final void testVertexIDEquals4()
	{
		equalityTestingHelper(new VertexID(VertKind.POSITIVE,5), new VertexID(VertKind.POSITIVE,5), new VertexID(idN5), new VertexID(idP5));
	}

	/** Tests VertexID toString methods. */
	@Test
	public final void testVertexIDToString()
	{
		Assert.assertEquals("P5", new VertexID(idP5).toString());
		Assert.assertEquals("N5", new VertexID(idN5).toString());
		Assert.assertEquals("P5", new VertexID(VertKind.POSITIVE,5).toString());
		Assert.assertEquals("N5", new VertexID(VertKind.NEGATIVE,5).toString());

		Assert.assertEquals("JustAnything", new VertexID("JustAnything").toString());
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
		equalityTestingHelper(DvertA,DvertA,DdifferentA,SdifferentA);equalityTestingHelper(DvertB,DvertB,DdifferentA,SdifferentA);
		equalityTestingHelper(DvertA,DvertB,DdifferentA,SdifferentA);
	}

	@Test
	public final void checkDEquality2()
	{
		DvertA.setAccept(false);DvertB.setAccept(false);
		equalityTestingHelper(DvertA,DvertA,DdifferentA,SdifferentA);equalityTestingHelper(DvertB,DvertB,DdifferentA,SdifferentA);
		equalityTestingHelper(DvertA,DvertB,DdifferentA,SdifferentA);
	}

	@Test
	public final void checkDEquality3()
	{
		DvertA.setAccept(true);DvertB.setAccept(false);
		equalityTestingHelper(DvertA,DvertA,DvertB,DdifferentA);equalityTestingHelper(DvertB,DvertB,DvertA,DdifferentA);
	}
	
	@Test
	/** Checks that attributes other than accept and name are ignored. */
	public final void checkDEquality_ignoresAttrs()
	{
		DvertA.setAccept(true);DvertB.setAccept(true);
		DvertA.setColour(JUConstants.RED);DvertA.setHighlight(true);DvertA.addUserDatum(JUConstants.INITIAL, "", UserData.SHARED);DvertA.addUserDatum(JUConstants.JUNKVERTEX, "a", UserData.SHARED);
		DvertB.setColour(JUConstants.BLUE);DvertB.setHighlight(true);DvertB.removeUserDatum(JUConstants.INITIAL);DvertB.addUserDatum(JUConstants.JUNKVERTEX, "b", UserData.SHARED);
		equalityTestingHelper(DvertA,DvertB,DdifferentA,SdifferentA);
	}
	
	@Test
	public final void checkSEquality1()
	{
		SvertA.setAccept(true);SvertB.setAccept(true);
		equalityTestingHelper(SvertA,SvertA,SdifferentA,DdifferentA);equalityTestingHelper(SvertB,SvertB,SdifferentA,DdifferentA);
		equalityTestingHelper(SvertA,SvertB,SdifferentA,DdifferentA);
	}

	@Test
	public final void checkSEquality2()
	{
		SvertA.setAccept(false);SvertB.setAccept(false);
		equalityTestingHelper(SvertA,SvertA,SdifferentA,DdifferentA);equalityTestingHelper(SvertB,SvertB,SdifferentA,DdifferentA);
		equalityTestingHelper(SvertA,SvertB,SdifferentA,DdifferentA);
	}

	@Test
	/** Checks that if one is accept and another one is reject, they are different. */ 
	public final void checkSEquality3()
	{
		SvertA.setAccept(true);SvertB.setAccept(false);
		equalityTestingHelper(SvertA,SvertA,SvertB,SdifferentA);
		equalityTestingHelper(SvertB,SvertB,SvertA,SdifferentA);
	}	

	@Test
	/** Checks that attributes other than accept and name are ignored. */
	public final void checkSEquality_ignoresAttrs()
	{
		SvertA.setAccept(true);SvertB.setAccept(true);
		SvertA.setColour(JUConstants.RED);SvertA.setHighlight(true);
		SvertB.setColour(JUConstants.BLUE);SvertB.setHighlight(true);
		equalityTestingHelper(SvertA,SvertB,SdifferentA,DdifferentA);
	}

	/** Checks that if CmpVertex implemented with different types 
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
		equalityTestingHelper(SvertA,DvertA,SdifferentA,DdifferentA);
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
	@Test(expected=IllegalArgumentException.class)
	public final void checkComparison_fail1()
	{
		SvertA.compareTo(DvertA);
	}
		
	@Test(expected=IllegalArgumentException.class)
	public final void checkComparison_fail2()
	{
		DvertA.compareTo(SvertA);
	}
	 */
	
	@Test
	public final void testDeterministicVertexComparison1_old()
	{
		DeterministicVertex p = new DeterministicVertex("P"), q= new DeterministicVertex("Q");
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}
		
	@Test
	public final void testDeterministicVertexComparison2_old()
	{
		DeterministicVertex p = new DeterministicVertex("A"), q= new DeterministicVertex("B");
		assertFalse(p.equals(q));
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}

	@Test
	public final void testDeterministicVertexComparison3_old()
	{
		DeterministicVertex p = new DeterministicVertex("P"), q= new DeterministicVertex("P");
		assertTrue(p.equals(q));
		assertTrue(p.compareTo(q)==0);
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testEqClassEquality_fail1()
	{
		new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{}));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testEqClassEquality_fail2()
	{
		new AMEquivalenceClass(null);
	}
	
	@Test
	public final void testEqClass_toString1()
	{
		Assert.assertEquals("[B->{B,A,D}]",
		new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				new StringVertex("B"),new StringVertex("A"),new StringVertex("D")})).toString());		
	}
	
	@Test
	public final void testEqClass_toString2()
	{
		Assert.assertEquals("[B->{B}]",
		new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				new StringVertex("B")})).toString());		
	}
	
	@Test
	public final void testEqClassEquality1()
	{
		equalityTestingHelper(
				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("B"),new StringVertex("A"),new StringVertex("C")})),
				
				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("B"),new StringVertex("A"),new StringVertex("C")})),

				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("A"),new StringVertex("A"),new StringVertex("C")})),
		
				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("B"),new StringVertex("A"),new StringVertex("D")}))
		);
	}

	@Test
	public final void testEqClassEquality2()
	{
		equalityTestingHelper(
				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("B")})),
				
				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("B")})),

				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("A"),new StringVertex("A"),new StringVertex("C")})),
		
				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("B"),new StringVertex("A"),new StringVertex("D")}))
		);
	}

	@Test
	public final void testEqClassEquality3()
	{
		equalityTestingHelper(
				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("A"),new StringVertex("B"),new StringVertex("C")})),
				
				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("A"),new StringVertex("B"),new StringVertex("C")})),

				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("A"),new StringVertex("B"),new StringVertex("D")})),
		
				new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
						new StringVertex("B"),new StringVertex("A"),new StringVertex("C")}))
		);
	}

	@Test
	public final void testAM_colour1()
	{
		AMEquivalenceClass eq =new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				new StringVertex("A"),new StringVertex("B"),new StringVertex("C")}));
		eq.computeMergedColour();
		Assert.assertNull(eq.getMergedVertex().getColour());
	}
	
	@Test
	public final void testAM_colour2a()
	{
		CmpVertex vertB = new StringVertex("B");vertB.setColour(JUConstants.RED);
		CmpVertex vertC = new StringVertex("C");
		AMEquivalenceClass eq =new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				new StringVertex("A"),vertB,vertC}));
		eq.computeMergedColour();
		Assert.assertTrue(eq.getMergedVertex().getColour() == JUConstants.RED);
	}

	@Test
	public final void testAM_colour2b()
	{
		CmpVertex vertB = new StringVertex("B");vertB.setColour(JUConstants.BLUE);
		CmpVertex vertC = new StringVertex("C");
		AMEquivalenceClass eq =new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				new StringVertex("A"),vertB,vertC}));
		eq.computeMergedColour();
		Assert.assertTrue(eq.getMergedVertex().getColour() == JUConstants.BLUE);
	}

	@Test
	public final void testAM_colour2c()
	{
		CmpVertex vertB = new StringVertex("B");vertB.setColour(JUConstants.AMBER);
		CmpVertex vertC = new StringVertex("C");vertC.setColour(JUConstants.BLUE);
		AMEquivalenceClass eq =new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				new StringVertex("A"),vertB,vertC}));
		eq.computeMergedColour();
		Assert.assertTrue(eq.getMergedVertex().getColour() == JUConstants.BLUE);
	}

	@Test
	public final void testAM_colour2d()
	{
		CmpVertex vertA = new StringVertex("A");vertA.setColour(JUConstants.AMBER);
		CmpVertex vertB = new StringVertex("B");
		CmpVertex vertC = new StringVertex("C");vertC.setColour(JUConstants.BLUE);
		AMEquivalenceClass eq =new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				vertA,vertB,vertC}));
		eq.computeMergedColour();
		Assert.assertTrue(eq.getMergedVertex().getColour() == JUConstants.BLUE);
	}

	@Test
	public final void testAM_colour2e()
	{
		CmpVertex vertA = new StringVertex("A");vertA.setColour(JUConstants.AMBER);
		CmpVertex vertB = new StringVertex("B");
		CmpVertex vertC = new StringVertex("C");
		AMEquivalenceClass eq =new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				vertA,vertB,vertC}));
		eq.computeMergedColour();
		Assert.assertNull(eq.getMergedVertex().getColour());
	}

	@Test
	public final void testAM_colour2f()
	{
		CmpVertex vertA = new StringVertex("A");vertA.setColour(JUConstants.AMBER);
		CmpVertex vertB = new StringVertex("B");
		CmpVertex vertC = new StringVertex("C");vertC.setColour(JUConstants.AMBER);
		AMEquivalenceClass eq =new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				vertA,vertB,vertC}));
		eq.computeMergedColour();
		Assert.assertNull(eq.getMergedVertex().getColour());
	}

	@Test
	public final void testAM_colour2g()
	{
		CmpVertex vertA = new StringVertex("A");
		CmpVertex vertB = new StringVertex("B");vertB.setColour(JUConstants.AMBER);
		CmpVertex vertC = new StringVertex("C");vertC.setColour(JUConstants.AMBER);
		AMEquivalenceClass eq =new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				vertA,vertB,vertC}));
		eq.computeMergedColour();
		Assert.assertNull(eq.getMergedVertex().getColour());
	}

	@Test
	public final void testAM_colour2h()
	{
		CmpVertex vertA = new StringVertex("A");vertA.setColour(JUConstants.AMBER);
		AMEquivalenceClass eq =new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				vertA}));
		eq.computeMergedColour();
		Assert.assertTrue(eq.getMergedVertex().getColour() == JUConstants.AMBER);
	}

	@Test
	public final void testAM_colour3()
	{
		CmpVertex vertB = new StringVertex("B");vertB.setColour(JUConstants.RED);
		CmpVertex vertC = new StringVertex("C");vertC.setColour(JUConstants.BLUE);
		AMEquivalenceClass eq =new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				new StringVertex("A"),vertB,vertC}));
		eq.computeMergedColour();
		Assert.assertTrue(eq.getMergedVertex().getColour() == JUConstants.RED);
	}

	/** Tests of comparison/equality of string/deterministic pairs of vertices
	 * (string-"a"/string "b"/det-"a"/det-"b")
	 * each pair of above, against another pair. 
	 */
	@Test
	public final void testStatePairEquality()
	{
		final Object samePairs[] = new StatePair[]{
				new StatePair(new StringVertex("a"), new StringVertex("b")),
				new StatePair(new DeterministicVertex("a"), new StringVertex("b")),
				new StatePair(new StringVertex("a"), new DeterministicVertex("b")),
				new StatePair(new DeterministicVertex("a"), new DeterministicVertex("b"))				
		},
		differentPairs[] = new Object[] {
				new StatePair(new StringVertex("a"), new StringVertex("c")),
				new StatePair(new StringVertex("d"), new StringVertex("b")),
				new StatePair(new StringVertex("d"), new StringVertex("e")),
				constructOrigPair("a", "b")
		};
		for(int sameFirst=0;sameFirst<samePairs.length;++sameFirst)
			for(int sameSecond=0;sameSecond<samePairs.length;++sameSecond)
				for(int different=0;different<differentPairs.length;++different)
					equalityTestingHelper(samePairs[sameFirst],samePairs[sameSecond],differentPairs[different],differentPairs[differentPairs.length-different-1]);

		equalityTestingHelper(constructOrigPair("a","b"), constructOrigPair("a","b"),
				constructOrigPair("a","c"),constructOrigPair("b","b"));
		for(int i=0;i<samePairs.length;++i)
			equalityTestingHelper(constructOrigPair("a","b"), constructOrigPair("a","b"),
					samePairs[i],samePairs[samePairs.length-i-1]);
	}
	
	/** Tests that nulls are valid elements of state pairs. */
	public final void testStatePairEqualityWithNulls1()
	{
		final Object samePairs[] = new StatePair[]{
				new StatePair(null, new StringVertex("b")),
				new StatePair(null, new DeterministicVertex("b"))			
		},
		differentPairs[] = new Object[] {
				new StatePair(new StringVertex("b"), new StringVertex("b")),
				new StatePair(new DeterministicVertex("b"), new StringVertex("b")),
				new StatePair(null, null),
				constructOrigPair("a", "b")
		};
		for(int sameFirst=0;sameFirst<samePairs.length;++sameFirst)
			for(int sameSecond=0;sameSecond<samePairs.length;++sameSecond)
				for(int different=0;different<differentPairs.length;++different)
					equalityTestingHelper(samePairs[sameFirst],samePairs[sameSecond],differentPairs[different],differentPairs[differentPairs.length-different-1]);
		
	}
	
	/** Tests that nulls are valid elements of state pairs. */
	public final void testStatePairEqualityWithNulls2()
	{
		final Object samePairs[] = new StatePair[]{
				new StatePair(new StringVertex("b"), null),
				new StatePair(new DeterministicVertex("b"),null)
		},
		differentPairs[] = new Object[] {
				new StatePair(new StringVertex("b"), new StringVertex("b")),
				new StatePair(new DeterministicVertex("b"), new StringVertex("b")),
				new StatePair(null, null),
				constructOrigPair("a", "b")
		};
		for(int sameFirst=0;sameFirst<samePairs.length;++sameFirst)
			for(int sameSecond=0;sameSecond<samePairs.length;++sameSecond)
				for(int different=0;different<differentPairs.length;++different)
					equalityTestingHelper(samePairs[sameFirst],samePairs[sameSecond],differentPairs[different],differentPairs[differentPairs.length-different-1]);
		
	}
	
	/** Tests that nulls are valid elements of state pairs. */
	public final void testStatePairEqualityWithNulls3()
	{
		final Object samePairs[] = new StatePair[]{
				new StatePair(null, null)
		},
		differentPairs[] = new Object[] {
				new StatePair(null, new StringVertex("b")),
				new StatePair(new DeterministicVertex("b"), null),
				constructOrigPair("a", "b")
		};
		for(int sameFirst=0;sameFirst<samePairs.length;++sameFirst)
			for(int sameSecond=0;sameSecond<samePairs.length;++sameSecond)
				for(int different=0;different<differentPairs.length;++different)
					equalityTestingHelper(samePairs[sameFirst],samePairs[sameSecond],differentPairs[different],differentPairs[differentPairs.length-different-1]);
		
	}
	
	private final static void checkLess(String a,String b,String c,String d)
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

	
	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_name1()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		
		// here I cannot add, only set since the ID has already been set.
		vA.setUserDatum(JUConstants.LABEL, new VertexID("name"), UserData.SHARED);Assert.assertEquals("name", vA.getID().toString());
		vA.setUserDatum(JUConstants.LABEL, new VertexID("D"), UserData.SHARED);Assert.assertEquals("D", vA.getID().toString());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum(JUConstants.LABEL, new VertexID("name"), UserData.SHARED);Assert.assertEquals("name", vS.getID().toString());
	}
	
	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_name2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");

		// here I cannot add, only set since the ID has already been set.
		vA.setUserDatum("lAbel", new VertexID("name"), UserData.SHARED);Assert.assertEquals("name", vA.getID().toString());
		vA.setUserDatum("labEl", new VertexID("D"), UserData.SHARED);Assert.assertEquals("D", vA.getID().toString());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum("laBel", new VertexID("name"), UserData.SHARED);Assert.assertEquals("name", vS.getID().toString());
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
		vA.addUserDatum(JUConstants.COLOUR, "rEd", UserData.SHARED);Assert.assertTrue(JUConstants.RED == vA.getColour());
		vA.setUserDatum(JUConstants.COLOUR, "bLue", UserData.SHARED);Assert.assertTrue(JUConstants.BLUE == vA.getColour());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum(JUConstants.COLOUR, "bLue", UserData.SHARED);Assert.assertTrue(JUConstants.BLUE == vA.getColour());
	}
	
	/** Tests that types are correctly converted. */
	@Test
	public final void testAddUserData_colour2()
	{
		DeterministicVertex vA=new DeterministicVertex("a");
		vA.addUserDatum("cOlour", "rEd", UserData.SHARED);Assert.assertTrue(JUConstants.RED == vA.getColour());
		vA.setUserDatum("cOloUr", "bLue", UserData.SHARED);Assert.assertTrue(JUConstants.BLUE == vA.getColour());

		DeterministicVertex vS=new DeterministicVertex("a");
		vS.setUserDatum("cOlouR", "blUe", UserData.SHARED);Assert.assertTrue(JUConstants.BLUE == vA.getColour());
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
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(true);
		LearnerGraph.cloneCmpVertex("junk", conf);
	}
	
	/** Non-CmpVertex copying denied. */
	@Test(expected = IllegalArgumentException.class) 
	public final void testVertexClone_fail2()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(false);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, "name", UserData.SHARED);
		LearnerGraph.cloneCmpVertex(vertex, conf);
	}
	
	/** Unlabelled copying denied. */
	@Test(expected = IllegalArgumentException.class) 
	public final void testVertexClone_fail3()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();
		LearnerGraph.cloneCmpVertex(vertex, conf);
	}
	
	/** Copying of a vertex with a label which is neither a string nor a VertexID is denied. */
	@Test(expected = IllegalArgumentException.class) 
	public final void testVertexClone_fail4()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, true, UserData.SHARED);
		LearnerGraph.cloneCmpVertex(vertex, conf);
	}
	
	
	/** Normal copying successful. */
	@Test
	public final void testVertexClone1a()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, "name", UserData.SHARED);
		CmpVertex result = LearnerGraph.cloneCmpVertex(vertex, conf);
		Assert.assertEquals("name", result.getID().toString());
		Assert.assertTrue(result.isAccept());Assert.assertFalse(result.isHighlight());Assert.assertNull(result.getColour());
	}
	
	/** Normal copying successful. */
	@Test
	public final void testVertexClone1b()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, new VertexID("name"), UserData.SHARED);
		CmpVertex result = LearnerGraph.cloneCmpVertex(vertex, conf);
		Assert.assertEquals("name", result.getID().toString());
		Assert.assertTrue(result.isAccept());Assert.assertFalse(result.isHighlight());Assert.assertNull(result.getColour());
	}
	
	/** Checking that attributes are preserved. */
	@Test
	public final void testVertexClone2()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(true);
		DirectedSparseVertex vertex = new DirectedSparseVertex();vertex.addUserDatum(JUConstants.LABEL, "name", UserData.SHARED);
		vertex.addUserDatum(JUConstants.HIGHLIGHT, 1, UserData.SHARED);
		vertex.addUserDatum(JUConstants.COLOUR, JUConstants.BLUE, UserData.SHARED);
		vertex.addUserDatum(JUConstants.ACCEPTED, false, UserData.SHARED);
		
		CmpVertex result = LearnerGraph.cloneCmpVertex(vertex, conf);
		Assert.assertEquals("name", result.getID().toString());
		Assert.assertEquals(JUConstants.BLUE, result.getColour());
		Assert.assertFalse(result.isAccept());Assert.assertTrue(result.isHighlight());
		
		vertex.removeUserDatum(JUConstants.ACCEPTED);vertex.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
		Assert.assertFalse(result.isAccept());
	}
	
	/** Checking that cloning can returns the same vertex regardless of the value of setLearnerUseStrings. */
	@Test
	public final void testVertexClone3()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerUseStrings(false);conf.setLearnerCloneGraph(false);
		CmpVertex vA = new DeterministicVertex("test vertex");vA.setColour(JUConstants.RED);
		Assert.assertSame(vA, LearnerGraph.cloneCmpVertex(vA, conf));
	}
	
	/** Checking that cloning can returns the same vertex regardless of the value of setLearnerUseStrings. */
	@Test
	public final void testVertexClone4()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerUseStrings(true);conf.setLearnerCloneGraph(false);
		CmpVertex vA = new DeterministicVertex("test vertex");vA.setColour(JUConstants.RED);
		Assert.assertSame(vA, LearnerGraph.cloneCmpVertex(vA, conf));
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
		CmpVertex vert_clone = LearnerGraph.cloneCmpVertex(vert, conf);
		Assert.assertNotSame(vert, vert_clone);Assert.assertEquals("test vertex", vert_clone.getID().toString());Assert.assertEquals(vert, vert_clone);
		
		vert.setAccept(false);
		Assert.assertEquals(JUConstants.RED, vert_clone.getColour());
		Assert.assertFalse(vert.equals(vert_clone));		
	}
	
	/** Checking that cloning is faithful. */
	@Test
	public final void testVertexClone5()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerCloneGraph(true);
		
		conf.setLearnerUseStrings(true);
		CmpVertex vA = new DeterministicVertex("test vertex");vA.setColour(JUConstants.RED);
		cloneTestHelper(vA, conf);
	}
	
	/** Checking that cloning is faithful. */
	@Test
	public final void testVertexClone6()
	{
		Configuration conf = Configuration.getDefaultConfiguration();conf.setAllowedToCloneNonCmpVertex(false);
		conf.setLearnerCloneGraph(true);
		
		conf.setLearnerUseStrings(false);
		CmpVertex vB = new DeterministicVertex("test vertex");vB.setColour(JUConstants.RED);
		cloneTestHelper(vB, conf);
	}

	@Test
	public final void testFSMStructureEquals1()
	{
		LearnerGraph a=new LearnerGraph(config),b=new LearnerGraph(config);
		a.init = new StringVertex("A");b.init = new StringVertex("A");
		Assert.assertTrue(a.equals(a));
		Assert.assertTrue(a.equals(b));

		Assert.assertFalse(a.equals(null));
		Assert.assertFalse(a.equals("hello"));
		b.init = new StringVertex("B");Assert.assertFalse(a.equals(b));
	}

	/** Tests that graphs of different kind look the same. */
	@Test
	public final void testFSMStructureEquals2()
	{
		Assert.assertTrue(testGraphString.init instanceof StringVertex);
		Assert.assertTrue(testGraphJung.init instanceof DeterministicVertex);
		Assert.assertTrue(testGraphSame.init instanceof DeterministicVertex);
		equalityTestingHelper(testGraphJung,testGraphString,differentA,differentB);
		equalityTestingHelper(testGraphJung,testGraphSame,differentA,differentB);
		equalityTestingHelper(testGraphString,testGraphSame,differentA,differentB);
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
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-a->S1", "testCopyGraph");
		DirectedSparseGraph copy=DeterministicDirectedSparseGraph.copy(g);
		LearnerGraph gS = new LearnerGraph(g,config),gC = new LearnerGraph(copy,config);
		
		Assert.assertTrue(gS.equals(gC));
	}
	
	/** Yet another test that copy works. */
	@Test
	public final void testCopyGraph2()
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+TestRpniLearner.PTA3, "testCopyGraph");
		DirectedSparseGraph copy=DeterministicDirectedSparseGraph.copy(g);
		LearnerGraph gS = new LearnerGraph(g,config),gCopy = new LearnerGraph(copy,config);
		
		Assert.assertTrue(gS.equals(gCopy));
		
		// now test if all clones are faithful
		for(Edge e:(Set<Edge>)g.getEdges())
			((Set<String>)e.getUserDatum(JUConstants.LABEL)).add("junk");
		
		LearnerGraph gS_Modified = new LearnerGraph(copy,config);
		
		Assert.assertTrue(gS_Modified.equals(gCopy));
	}
	
	/** Yet another test that copy works. */
	@Test
	public final void testCopyGraph3() // this one tests that clone works
	{
		DirectedSparseGraph g=TestFSMAlgo.buildGraph("S-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"+TestRpniLearner.PTA3, "testCopyGraph");
		LearnerGraph orig = new LearnerGraph(g,config);
		LearnerGraph copy;
		copy = orig.copy(orig.config);
		LearnerGraph gS = new LearnerGraph(orig.paths.getGraph(),config),
			gCopy = new LearnerGraph(copy.paths.getGraph(),config);
		
		Assert.assertTrue(gS.equals(gCopy));
		
		// now test if all clones are faithful by clearing the first graph.
		orig.initPTA();
		
		LearnerGraph gS_afterChange = new LearnerGraph(orig.paths.getGraph(),config), 
			gCopy_afterChange = new LearnerGraph(copy.paths.getGraph(),config);
		
		Assert.assertTrue(gCopy_afterChange.equals(gCopy));
		Assert.assertTrue(gCopy_afterChange.equals(gS));
		Assert.assertFalse(gS_afterChange.equals(gCopy));
		Assert.assertFalse(gS_afterChange.equals(gS));		
	}

	/** Tests that (1) it is possible to convert to a different type of graph 
	 * using clone and (2) getGraph does its job. 
	 */
	@Test
	public final void testFSMStructureClone3()
	{
		ArrayList<LearnerGraph> origGraphs = new ArrayList<LearnerGraph>(3);origGraphs.add(testGraphJung);origGraphs.add(testGraphSame);origGraphs.add(testGraphString);
		List<LearnerGraph> same = new LinkedList<LearnerGraph>();
		
		for(LearnerGraph g:origGraphs)
		{
			same.add(g);
			Configuration copyConfig = g.config.copy();
			copyConfig.setLearnerUseStrings(false);copyConfig.setLearnerCloneGraph(true);
			LearnerGraph cloneJung = g.copy(copyConfig);
			Assert.assertTrue(cloneJung.init instanceof DeterministicVertex);

			copyConfig.setLearnerUseStrings(true);copyConfig.setLearnerCloneGraph(true);
			LearnerGraph cloneStrings = g.copy(copyConfig);
			Assert.assertTrue(cloneStrings.init instanceof StringVertex);

			copyConfig.setLearnerUseStrings(false);copyConfig.setLearnerCloneGraph(false);
			LearnerGraph cloneSame = g.copy(copyConfig);
			Assert.assertTrue(cloneSame.init.getClass().equals(g.init.getClass()));
			Assert.assertSame(cloneSame.init, g.init);
			if (g.config == confSame) // if the graph we are playing with is the
				Assert.assertTrue(false);
				//Assert.assertSame(cloneSame.init, testGraphSame.init);// verify (of sorts) that the same vertices are being used.
			same.add(cloneJung);same.add(cloneStrings);same.add(cloneSame);
			
			// Now add results of getGraph, considering all combinations of "getGraph" configuration and "new LearnerGraph" configuration
			for(Configuration conf:Arrays.asList(new Configuration[]{confJung,confSame,confString}))
			{
				g.config.setLearnerUseStrings(false);g.config.setLearnerCloneGraph(true);
				same.add(new LearnerGraph(g.paths.getGraph(),conf));
				g.config.setLearnerUseStrings(true);g.config.setLearnerCloneGraph(true);
				same.add(new LearnerGraph(g.paths.getGraph(),conf));
				
				DirectedSparseGraph someGraph = g.paths.OrigGetGraph("someName");
				Assert.assertEquals("someName", someGraph.getUserDatum(JUConstants.TITLE));
				same.add(new LearnerGraph(someGraph,conf));
				
				g.config.setLearnerUseStrings(false);g.config.setLearnerCloneGraph(false);
				DirectedSparseGraph almostOrigGraph = g.paths.getGraph("new Name");
				Assert.assertEquals("new Name", almostOrigGraph.getUserDatum(JUConstants.TITLE));
				Assert.assertEquals(DeterministicDirectedSparseGraph.findInitial(almostOrigGraph), g.init);
				same.add(new LearnerGraph(almostOrigGraph,conf));
			}
		}
		
		for(LearnerGraph gFirst:origGraphs)
			for(LearnerGraph gSecond:origGraphs)
				equalityTestingHelper(gFirst,gSecond,differentA,differentB);
	}
}
