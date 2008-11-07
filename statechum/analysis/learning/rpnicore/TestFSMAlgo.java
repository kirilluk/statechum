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
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import junit.framework.Assert;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;
import org.junit.BeforeClass;

import statechum.ArrayOperations;
import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.StringVertex;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.DeterministicDirectedSparseGraph.VertexID.VertKind;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.Test_Orig_RPNIBlueFringeLearner.OrigStatePair;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import static statechum.Helper.whatToRun;

public class TestFSMAlgo {

	public TestFSMAlgo()
	{
		mainConfiguration = Configuration.getDefaultConfiguration();
		mainConfiguration.setAllowedToCloneNonCmpVertex(true);
	}
	
	org.w3c.dom.Document doc = null;
	
	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public final void beforeTest()
	{
		config = mainConfiguration.copy();
		LearnerGraph.testMode=true;

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		try
		{
			factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
		}
		catch(ParserConfigurationException e)
		{
			statechum.Helper.throwUnchecked("failed to construct DOM document",e);
		}
	}

	/** The configuration to use when running tests. */
	Configuration config = null, mainConfiguration = null;

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
	
	/** Given a textual representation of an fsm, builds a corresponding Jung graph
	 * 
	 * @param fsm the textual representation of an FSM
	 * @param name graph name, to be displayed as the caption of the Jung window.
	 * @return Jung graph for it
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public final static DirectedSparseGraph buildGraph(String fsm,String name)
	{
		final Map<String,DeterministicVertex> existingVertices = new HashMap<String,DeterministicVertex>();
		final Map<StatePair,DeterministicEdge> existingEdges = new HashMap<StatePair,DeterministicEdge>();
		
		final DirectedSparseGraph g = new DirectedSparseGraph();
		g.setUserDatum(JUConstants.TITLE, name,UserData.SHARED);

		new TestFSMParser.fsmParser(fsm).parse(new TestFSMParser.TransitionReceiver()
		{
			public void put(String from, String to, String label, boolean accept) {
				DeterministicVertex fromVertex = existingVertices.get(from), toVertex = existingVertices.get(to);
				
				if (fromVertex == null)
				{
					fromVertex = new DeterministicDirectedSparseGraph.DeterministicVertex(from);
					if (existingVertices.isEmpty())
						fromVertex.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
					fromVertex.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
					existingVertices.put(from, fromVertex);
					g.addVertex(fromVertex);
				}
				else
					if (!Boolean.valueOf(fromVertex.getUserDatum(JUConstants.ACCEPTED).toString()))
						throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+from);

				if (from.equals(to))
				{
					if (!accept) throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+to);
					toVertex = fromVertex;
				}
				else
					if (toVertex == null)
					{
						toVertex = new DeterministicDirectedSparseGraph.DeterministicVertex(to);
						toVertex.removeUserDatum(JUConstants.ACCEPTED); // in case we've got a reject loop in the same state
						toVertex.addUserDatum(JUConstants.ACCEPTED, accept, UserData.SHARED);
						existingVertices.put(to, toVertex);
						g.addVertex(toVertex);
					}
					else
						if (DeterministicDirectedSparseGraph.isAccept(toVertex) != accept)
							throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+to);
				
				StatePair pair = new StatePair(fromVertex,toVertex);
				DeterministicEdge edge = existingEdges.get(pair);
				if (edge == null)
				{
					edge = new DeterministicDirectedSparseGraph.DeterministicEdge(fromVertex,toVertex);
					edge.addUserDatum(JUConstants.LABEL, new HashSet<String>(), UserData.CLONE);
					g.addEdge(edge);existingEdges.put(pair,edge);
				}
				
				Set<String> labels = (Set<String>)edge.getUserDatum(JUConstants.LABEL);
				labels.add(label);
			}

			public void accept(String from, String to, String label) {
				put(from,to,label,true);
			}
			public void reject(String from, String to, String label) {
				put(from,to,label,false);
			}
		});

		if (GlobalConfiguration.getConfiguration().isGraphTransformationDebug(g))
		{
			Visualiser.updateFrame(g, null);System.out.println("******** PROCESSING "+name+" **********\n");
		}
		return g;
	}

	@Test
	public final void completeComputeAlphabet0()
	{
		Set<String> alphabet = DeterministicDirectedSparseGraph.computeAlphabet(new DirectedSparseGraph());
		Assert.assertTrue(alphabet.isEmpty());
	}

	/** Tests alphabet computation in the presence of unreachable states. */
	@Test
	public final void testComputeFSMAlphabet1()
	{
		Set<String> expected = new HashSet<String>();
		expected.add("p");
		DirectedSparseGraph g = buildGraph("A-p->A","testComputeFSMAlphabet1");
		Assert.assertEquals(expected, new LearnerGraph(g,config).wmethod.computeAlphabet());
		Assert.assertEquals(expected, DeterministicDirectedSparseGraph.computeAlphabet(g));
	}

	@Test
	public final void testComputeFSMAlphabet2()
	{
		DirectedSparseGraph g = buildGraph("A-a->A<-b-A", "completeComputeAlphabet3");
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {"a","b"}));
		Assert.assertEquals(expected, new LearnerGraph(g,config).wmethod.computeAlphabet());
		Assert.assertEquals(expected, DeterministicDirectedSparseGraph.computeAlphabet(g));				
	}
	
	/** Tests alphabet computation in the presence of unreachable states. */
	@Test
	public final void testComputeFSMAlphabet3()
	{
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[]{"p","d","b","c","a"}));
		DirectedSparseGraph g = buildGraph("A-p->A-b->B-c->B-a-#C\nQ-d->S-c->S","testComputeFSMAlphabet3");
		Assert.assertEquals(expected, new LearnerGraph(g,config).wmethod.computeAlphabet());
		Assert.assertEquals(expected, DeterministicDirectedSparseGraph.computeAlphabet(g));				
	}


	@Test
	public final void testComputeFSMAlphabet4() {
		DirectedSparseGraph g = buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S-a-#T","testComputeFSMAlphabet4");
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[]{"p","d","b","c","a"}));
		Assert.assertEquals(expected, new LearnerGraph(g,config).wmethod.computeAlphabet());
		Assert.assertEquals(expected, DeterministicDirectedSparseGraph.computeAlphabet(g));				
	}

	@Test
	public final void completeComputeAlphabet5()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-a->S\nA-c->A\nB-b->B\nC-a->C-b->C-c->C\nQ-b->Q-c->Q\nS-a->S-b->S-c->S", "completeComputeAlphabet5");
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {"a","b","c"}));
		Assert.assertEquals(expected, new LearnerGraph(g,config).wmethod.computeAlphabet());
		Assert.assertEquals(expected, DeterministicDirectedSparseGraph.computeAlphabet(g));				

		LearnerGraph clone = new LearnerGraph(g,config).copy(config);
		Assert.assertFalse( clone.transform.completeGraph(
				new VertexID("REJECT")));
		Assert.assertFalse(DeterministicDirectedSparseGraph.completeGraph(g,"REJECT"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testFindVertex0()
	{
		DeterministicDirectedSparseGraph.findVertex(JUConstants.JUNKVERTEX, null, new DirectedSparseGraph());
	}

	@Test
	public final void testFindVertex1()
	{
		Assert.assertNull(DeterministicDirectedSparseGraph.findVertex(JUConstants.JUNKVERTEX, "bb", new DirectedSparseGraph()));
	}
	
	@Test
	public final void testFindVertex2()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex2");
		//Visualiser.updateFrame(g, g);Visualiser.waitForKey();
		Assert.assertNull(DeterministicDirectedSparseGraph.findVertex(JUConstants.JUNKVERTEX, "bb", g));
	}
		
	@Test
	public final void testFindVertex3()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex3");
		//Visualiser.updateFrame(g, null);Visualiser.waitForKey();
		Assert.assertNull(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, "D", g));
	}

	@Test
	public final void testFindVertex4a()
	{
		Vertex v = DeterministicDirectedSparseGraph.findVertex(JUConstants.INITIAL, "anything", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex4a"));
		Assert.assertNull(v);
	}

	@Test
	public final void testFindVertex4b()
	{
		Vertex v =  DeterministicDirectedSparseGraph.findVertex(JUConstants.INITIAL, true, buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex4b"));
		Assert.assertEquals(new VertexID("A"), v.getUserDatum(JUConstants.LABEL));
	}

	@Test
	public final void testFindVertex5()
	{
		Vertex v =  DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("A"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex5"));
		Assert.assertEquals(new VertexID("A"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public final void testFindVertex6()
	{
		Vertex v =  DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("C"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex6"));
		Assert.assertEquals(new VertexID("C"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public final void testFindVertex7()
	{
		Vertex v = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("S"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex7"));
		Assert.assertEquals(new VertexID("S"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public final void testFindVertex8()
	{
		Vertex v = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("Q"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex8"));
		Assert.assertEquals(new VertexID("Q"), v.getUserDatum(JUConstants.LABEL));
	}

	
	@Test
	public final void testFindInitial1()
	{
		Vertex v = DeterministicDirectedSparseGraph.findInitial(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"));
		Assert.assertEquals(new VertexID("A"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public final void testFindInitial2()
	{
		Vertex v = DeterministicDirectedSparseGraph.findInitial(new DirectedSparseGraph());
		Assert.assertNull(v);
	}

	/** Adding A, B as incompatible states. */
	@Test
	public final void testAddToIncompatibles1()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		
		grf.addToIncompatibles(grf.findVertex("B"),grf.findVertex("A"));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("C"),grf.findVertex("A"))));
	}
	
	/** Adding B, A as incompatible states. */
	@Test
	public final void testAddToIncompatibles2()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		
		grf.addToIncompatibles(grf.findVertex("A"),grf.findVertex("B"));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("C"),grf.findVertex("A"))));
	}
	
	/** Adding B, A as incompatible states twice. */
	@Test
	public final void testAddToIncompatibles3()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		
		grf.addToIncompatibles(grf.findVertex("A"),grf.findVertex("B"));
		grf.addToIncompatibles(grf.findVertex("A"),grf.findVertex("B"));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("C"),grf.findVertex("A"))));
	}
	
	/** Adding B, A as incompatible states twice. */
	@Test
	public final void testAddToIncompatibles4()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		
		grf.addToIncompatibles(grf.findVertex("A"),grf.findVertex("B"));
		grf.addToIncompatibles(grf.findVertex("B"),grf.findVertex("A"));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("C"),grf.findVertex("A"))));
	}
	
	/** Adding B, A as incompatible states twice. */
	@Test
	public final void testAddToIncompatibles5()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		
		grf.addToIncompatibles(grf.findVertex("A"),grf.findVertex("B"));
		grf.addToIncompatibles(grf.findVertex("C"),grf.findVertex("A"));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("C"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("D"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("D"),grf.findVertex("A"))));
	}
	
	/** Checking that copying a graph clones the array. */
	@Test
	public final void testIncompatibles5()
	{
		LearnerGraph grf = new LearnerGraph(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"),Configuration.getDefaultConfiguration());
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		LearnerGraph graph2 = grf.copy(Configuration.getDefaultConfiguration());
		
		grf.addToIncompatibles(grf.findVertex("B"),grf.findVertex("A"));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("C"),grf.findVertex("A"))));

		Assert.assertFalse(graph2.checkIncompatible(new StatePair(graph2.findVertex("A"),graph2.findVertex("A"))));
		Assert.assertFalse(graph2.checkIncompatible(new StatePair(graph2.findVertex("A"),graph2.findVertex("B"))));
		Assert.assertFalse(graph2.checkIncompatible(new StatePair(graph2.findVertex("B"),graph2.findVertex("A"))));
		Assert.assertFalse(graph2.checkIncompatible(new StatePair(graph2.findVertex("A"),graph2.findVertex("C"))));
		Assert.assertFalse(graph2.checkIncompatible(new StatePair(graph2.findVertex("C"),graph2.findVertex("A"))));
		
		graph2.addToIncompatibles(grf.findVertex("C"),grf.findVertex("A"));
		
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("A"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("B"))));
		Assert.assertTrue(grf.checkIncompatible(new StatePair(grf.findVertex("B"),grf.findVertex("A"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("A"),grf.findVertex("C"))));
		Assert.assertFalse(grf.checkIncompatible(new StatePair(grf.findVertex("C"),grf.findVertex("A"))));

		Assert.assertFalse(graph2.checkIncompatible(new StatePair(graph2.findVertex("A"),graph2.findVertex("A"))));
		Assert.assertFalse(graph2.checkIncompatible(new StatePair(graph2.findVertex("A"),graph2.findVertex("B"))));
		Assert.assertFalse(graph2.checkIncompatible(new StatePair(graph2.findVertex("B"),graph2.findVertex("A"))));
		Assert.assertTrue(graph2.checkIncompatible(new StatePair(graph2.findVertex("A"),graph2.findVertex("C"))));
		Assert.assertTrue(graph2.checkIncompatible(new StatePair(graph2.findVertex("C"),graph2.findVertex("A"))));
	}
	
	/** Builds a set of sequences from a two-dimensional array, where each element corresponds to a sequence.
	 * 
	 * @param data source data
	 * @return a set of sequences to apply to an RPNI learner
	 */
	public static Set<List<String>> buildSet(String [][] data)
	{
		Set<List<String>> result = new HashSet<List<String>>();
		for(String []seq:data)
		{
			result.add(Arrays.asList(seq));
		}
		return result;
	}
	
	/** Builds a map from an array, where each element corresponds to a pair of a string array 
	 * (representing a sequence) and a string (representing flags associated with this sequence).
	 * 
	 * @param data source data
	 * @return a string->string map
	 */
	public static Map<String,String> buildStringMap(Object [][] data)
	{
		Map<String,String> result = new HashMap<String,String>();
		for(Object[] str:data)
		{
			if (str.length != 2)
				throw new IllegalArgumentException("more than two elements in sequence "+str);
			if (str[0] == null || str[1] == null || !(str[0] instanceof String[]) || !(str[1] instanceof String))
				throw new IllegalArgumentException("invalid data in array");
			result.put(ArrayOperations.seqToString(Arrays.asList((String[])str[0])),(String)str[1]);
		}
		return result;
	}
	
	/** Builds a set of sequences from a two-dimensional array, where each element corresponds to a sequence.
	 * 
	 * @param data source data
	 * @return a set of sequences to apply to an RPNI learner
	 */
	public static List<List<String>> buildList(String [][] data)
	{
		List<List<String>> result = new LinkedList<List<String>>();
		for(String []seq:data)
		{
			result.add(Arrays.asList(seq));
		}
		return result;
	}
	
	@Test
	public final void testBuildSet1()
	{
		assertTrue(buildSet(new String[] []{}).isEmpty());
	}

	@Test
	public final void testBuildSet2()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(new LinkedList<String>());
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{}})));
	}

	@Test
	public final void testBuildSet3A()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		expectedResult.add(new LinkedList<String>());
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{},new String[]{"a","b","c"}})));
	}

	@Test
	public final void testBuildSet3B()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{"a","b","c"}})));
	}

	@Test
	public final void testBuildSet4()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		expectedResult.add(new LinkedList<String>());
		expectedResult.add(Arrays.asList(new String[]{"g","t"}));
		expectedResult.add(Arrays.asList(new String[]{"h","q","i"}));
		assertTrue(expectedResult.equals(buildSet(new String[] []{
				new String[]{"a","b","c"},new String[]{"h","q","i"}, new String[] {},new String[]{"g","t"} })));
	}

	@Test
	public final void testBuildStringMap1()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
		})));
	}
	
	@Test
	public final void testBuildStringMap2()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"a"},"value2"},
				new Object[]{new String[]{"b"},"value3"}
		})));
	}
	
	@Test
	public final void testBuildStringMap3()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value2"},
				new Object[]{new String[]{"a"},"value1"},
				new Object[]{new String[]{"b"},"value3"}
		})));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap4()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"}},// an invalid sequence
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap5()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{},// an invalid sequence - too few elements
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap6()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"},"c","d"},// an invalid sequence - too many elements
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap7()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new Object(),"c"},// an invalid sequence - wrong type of the first element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap8()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{"text","c"},// an invalid sequence - wrong type of the first element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap9()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"},new Object()},// an invalid sequence - wrong type of the second element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap10()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{null,"value"},// an invalid sequence - null in the first element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public final void testBuildStringMap11()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"}, null},// an invalid sequence - null in the second element
				new Object[]{new String[]{"b"},null}
		})));
	}
	
	public final void checkForCorrectException(final int [][]tTable, final int []vFrom, String exceptionString)
	{
		statechum.Helper.checkForCorrectException(new whatToRun() { public void run() {
			LearnerGraph.convertTableToFSMStructure(tTable, vFrom, -1	,config);
		}}, IllegalArgumentException.class,exceptionString);
	}
	
	/** Zero-sized array. */
	@Test
	public final void testConvertTableToFSMStructure0()
	{
		int [][]table = new int[][] {
		};
		checkForCorrectException(table, new int[0], "array is zero-sized");
	}

	/** Zero-sized alphabet. */
	@Test
	public final void testConvertTableToFSMStructure1a()
	{
		int [][]table = new int[][] {
			{}, 
			{1,1}
		};
		checkForCorrectException(table, new int[]{0,1}, "alphabet is zero-sized");
	}
	
	/** "rows of inconsistent size" */
	@Test
	public final void testConvertTableToFSMStructure1b()
	{
		int [][]table = new int[][] {
			{}, 
			{1,1}
		};
		checkForCorrectException(table, new int[]{1,0}, "rows of inconsistent size");
	}
	
	/** "rows of inconsistent size" */
	@Test
	public final void testConvertTableToFSMStructure2()
	{
		int [][]table = new int[][] {
				{1,0,1,0}, 
				{0,1}
			};
		checkForCorrectException(table, new int[]{0,1}, "rows of inconsistent size");
	}
	
	/** Reject number in vfrom. */
	@Test
	public final void testConvertTableToFSMStructure3()
	{
		int [][]table = new int[][] {
				{1,0,1,0}, 
				{0,1,0,1}
			};
		checkForCorrectException(table, new int[]{0,-1}, "reject number in vFrom");
	}
	
	/** Transition to illegal state 6 */
	@Test
	public final void testConvertTableToFSMStructure4a()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	2}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		checkForCorrectException(table, new int[]{0,1,2,3}, "leads to an invalid state");
	}
	
	/** Transition to illegal state -4 */
	@Test
	public final void testConvertTableToFSMStructure4b()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	2}, 
			{0, 3,	0,	-1},
			{0,0,0,-4},
			{-1,-1,-1,-1}
		};
		checkForCorrectException(table, new int[]{0,1,2,3}, "leads to an invalid state");
	}

	@Test
	public final void testConvertTableToFSMStructure_missing_elements_in_vFrom()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		checkForCorrectException(table, new int[]{0,1}, "Some states in the transition table are not included in vFrom");
	}

	@Test
	public final void testConvertTableToFSMStructure5()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{0,1,3}, -1	,config);
		Assert.assertNull(WMethod.checkM(fsm, fsm.findVertex("S0"),new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure5"),config), fsm.findVertex("S0")));
	}
	
	@Test
	public final void testConvertTableToFSMStructure6()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{1,0,3}, -1	,config);
		Assert.assertNull(WMethod.checkM(fsm, fsm.findVertex("S0"), new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure6"),config), fsm.findVertex("S0")));
	}

	@Test
	public final void testConvertTableToFSMStructure7()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{3,0,1}, -1	,config);
		Assert.assertNull(WMethod.checkM(fsm, fsm.findVertex("S0"), new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure7"),config), fsm.findVertex("S0")));
	}
	
	@Test
	public final void testConvertTableToFSMStructure8()
	{
		int [][]table = new int[][] {
			{0,	1,	-1,	3}, 
			{0, 3,	0,	-1},
			{0,0,0,6},
			{-1,-1,-1,-1}
		};
		LearnerGraph fsm = LearnerGraph.convertTableToFSMStructure(table, new int[]{3,0,1,0,1,1}, -1	,config);
		Assert.assertNull(WMethod.checkM(fsm, fsm.findVertex("S0"), new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure8"),config), fsm.findVertex("S0")));
	}

	@Test
	public final void testGetNonRepeatingNumbers0()
	{
		int data[] = DeterministicDirectedSparseGraph.getNonRepeatingNumbers(0, 0); 
		Assert.assertEquals(0,data.length);
	}
	
	@Test
	public final void testGetNonRepeatingNumbers1()
	{
		int data[] = DeterministicDirectedSparseGraph.getNonRepeatingNumbers(1, 0); 
		Assert.assertEquals(1,data.length);Assert.assertEquals(0, data[0]);
	}
	
	@Test
	public final void testGetNonRepeatingNumbers2()
	{
		int data[] = DeterministicDirectedSparseGraph.getNonRepeatingNumbers(2, 0); 
		Assert.assertEquals(2,data.length);
		if (data[0] == 0)
			Assert.assertEquals(1, data[1]);
		else
		{
			Assert.assertEquals(1, data[0]);Assert.assertEquals(0, data[1]);
		}
	}
	
	@Test
	public final void testGetNonRepeatingNumbers3()
	{
		final int size = 200;
		int data[] = DeterministicDirectedSparseGraph.getNonRepeatingNumbers(size, 1); 
		Assert.assertEquals(size,data.length);
		boolean values[] = new boolean[size];
		for(int i=0;i<size;++i) { Assert.assertFalse(values[data[i]]);values[data[i]]=true; }
		//System.out.println(Arrays.toString(data));
	}
	
	@Test
	public final void assertsEnabled()
	{
		boolean assertsOn = false;
		assert assertsOn = true;
		
		Assert.assertTrue("asserts have to be enabled", assertsOn);
	}
		
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{		
		Visualiser.disposeFrame();
	}

	@AfterClass
	public static void cleanUp()
	{
		Visualiser.disposeFrame();
	}
}
