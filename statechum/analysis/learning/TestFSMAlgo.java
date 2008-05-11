/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.analysis.learning;

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
import statechum.JUConstants;
import statechum.StringVertex;
import statechum.Configuration.IDMode;
import statechum.Configuration.QuestionGeneratorKind;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.DeterministicDirectedSparseGraph.VertexID.VertKind;
import statechum.analysis.learning.RPNIBlueFringeLearnerOrig.OrigStatePair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static statechum.analysis.learning.Visualiser.isGraphTransformationDebug;

public class TestFSMAlgo {

	public TestFSMAlgo()
	{
		mainConfiguration = Configuration.getDefaultConfiguration();
		mainConfiguration.setAllowedToCloneNonCmpVertex(true);
	}
	
	/** Make sure that whatever changes a test have made to the 
	 * configuration, next test is not affected.
	 */
	@Before
	public void beforeTest()
	{
		config = (Configuration)mainConfiguration.clone();
		LearnerGraph.testMode=true;
	}

	/** The configuration to use when running tests. */
	private Configuration config = null, mainConfiguration = null;

	static protected OrigStatePair constructOrigPair(String a,String b)
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
	static public void equalityTestingHelper(Object p, Object q, 
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
	static private void checkLessHelper(Comparable p, Comparable q)
	{
		assertFalse(p.equals(q));assertFalse(p.hashCode() == q.hashCode());
		assertTrue(p.compareTo(q)<0);
		assertTrue(q.compareTo(p)>0);
		assertFalse(p.hashCode() == q.hashCode());
		assertEquals(0,p.compareTo(p));
		assertEquals(0,q.compareTo(q));
	}

	@Test
	public void testConfigurationEquals()
	{
		Configuration confA = new Configuration(), confB = new Configuration(),
			confC = new Configuration(), confD = new Configuration();
		confA.setBumpPositives(true);confA.setLearnerIdMode(Configuration.IDMode.NONE);confA.setDefaultInitialPTAName("test");
		confB.setBumpPositives(true);confB.setLearnerIdMode(Configuration.IDMode.NONE);confB.setDefaultInitialPTAName("test");
		confC.setBumpPositives(true);confC.setLearnerIdMode(Configuration.IDMode.NONE);confC.setDefaultInitialPTAName("a");
		confD.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
		equalityTestingHelper(confA, confB, confC, confD);
		confC.setDefaultInitialPTAName("test");confA.setDefaultInitialPTAName("b");
		equalityTestingHelper(confC, confB, confA, confD);
	}
	
	@Test
	public void testConfigurationClone()
	{
		Configuration confA = new Configuration(),
		confC = new Configuration(), confD = new Configuration();
		confA.setBumpPositives(true);confA.setLearnerIdMode(Configuration.IDMode.NONE);confA.setDefaultInitialPTAName("test");
		confC.setBumpPositives(true);confC.setLearnerIdMode(Configuration.IDMode.NONE);confC.setDefaultInitialPTAName("a");
		Configuration confClone = (Configuration)confA.clone();
		equalityTestingHelper(confA, confClone, confC, confD);
		
		confClone.setDefaultInitialPTAName("avalue");// mess up the clone
		equalityTestingHelper(confA, confA, confClone, confD);
		equalityTestingHelper(confA.clone(), confA, confClone, confD);
		
		confA.setDefaultInitialPTAName("avalue");// mess up the original the same way as the clone was messed up
		equalityTestingHelper(confA, confClone, confC, confD);
	}

	/** An obvious problem with Configuration is forgetting to include all 
	 * the necessary variables in equals and hashcode methods. This one
	 * checks that each change to instance variables affects the response from 
	 * those methods. This is accomplished by collecting all fields, identifying
	 * the corresponding setter methods and choosing two values for each
	 * of those fields. Subsequently, I can assign the first value to all fields
	 * and go through the fields setting the second one and checking that 
	 * this affects hashCode and equals. 
	 */ 
	@Test
	public void testConfigurationUsesAllItsVariables()
	{
		class MethodAndArgs {
			public MethodAndArgs(Method m, Field f, Object a, Object b)
			{
				method=m;Arg=a;AlternativeArg=b;field = f;
			}
			Field field;
			Method method;Object Arg, AlternativeArg;
		}

		List<MethodAndArgs> MethodsArgs=new LinkedList<MethodAndArgs>();
		for(Field var:Configuration.class.getDeclaredFields())
		{
			if (var.getType() != Configuration.class && 
					var.getName() != "$VRc")// added by eclemma (coverage analysis) 
			{
				String varName = var.getName();
				String setterName = "set"+(Character.toUpperCase(varName.charAt(0)))+varName.substring(1);
				Method setter = null;
				try {
					setter = Configuration.class.getMethod(setterName, new Class[]{var.getType()});
				} catch (Exception e) {
					Assert.fail("failed to obtain setter method "+setterName+" for field "+varName);
				}
				Object valueA = null, valueB = null;
				if (var.getType().equals(Boolean.class) || var.getType().equals(boolean.class))
				{
					valueA = new Boolean(true);valueB=new Boolean(false);
				}
				else
					if (var.getType().equals(Double.class) || var.getType().equals(double.class))
					{
						valueA = new Double(0.4);valueB=new Double(0.5);// note that we have to choose values which fall within the allowed range of values
					}
					else
					if (var.getType().equals(String.class))
					{
						valueA = varName+", value A";valueB=varName+", value B";
					}
					else
						if (var.getType().equals(IDMode.class))
						{
							valueA = IDMode.POSITIVE_NEGATIVE;valueB=IDMode.POSITIVE_ONLY;
						}
						else
							if (var.getType().equals(ScoreMode.class))
							{
								valueA = ScoreMode.KTAILS;valueB=ScoreMode.COMPATIBILITY;
							}
						else
							if (var.getType().equals(QuestionGeneratorKind.class))
							{
									valueA = QuestionGeneratorKind.CONVENTIONAL;valueB=QuestionGeneratorKind.SYMMETRIC;
							}
							else
							if (var.getType().equals(Integer.class) || var.getType().equals(int.class))
							{
								valueA = varName.hashCode();valueB=setterName.hashCode();// just some integers likely to be different from each other between different variables.
							}
							else
								throw new IllegalArgumentException("A field "+var+" of Configuration has an unsupported type "+var.getType());
				
				MethodsArgs.add(new MethodAndArgs(setter,var,valueA,valueB));
			}
		}
		
		try {
			// Now check that hashCode and equals are affected by values of different fields.
			for(MethodAndArgs currentMethod:MethodsArgs)
			{
				Configuration 
					configA = (Configuration)Configuration.getDefaultConfiguration().clone(),
					configB = (Configuration)Configuration.getDefaultConfiguration().clone();
				for(MethodAndArgs orig:MethodsArgs)
				{
					orig.method.invoke(configA, new Object[]{orig.Arg});
					orig.method.invoke(configB, new Object[]{orig.Arg});
				}
				Assert.assertEquals(configB, configA);
				currentMethod.method.invoke(configB, new Object[]{currentMethod.AlternativeArg});
				String errMsg = "configurations differ: field "+currentMethod.field+" is not in use for ";
				Assert.assertFalse(errMsg+"equals",configB.equals(configA));
				Assert.assertFalse(errMsg+"equals",configA.equals(configB));
				Assert.assertTrue(errMsg+"hashCode",configA.hashCode() != configB.hashCode());
			}
		} catch (Exception e) {
			Assert.fail(e.getMessage());
		}
	}
	
	/** Tests that it is not possible to create an invalid vertexid. */
	@Test(expected=IllegalArgumentException.class)
	public void testCannotCreateNoneVertexID1()
	{
		new VertexID(VertKind.NONE,3);
	}
	
	/** Tests that it is not possible to create an invalid vertexid. */
	@Test(expected=IllegalArgumentException.class)
	public void testCannotCreateNoneVertexID2()
	{
		new VertexID(null);
	}
	
	/** Tests equality for VertexIDs. */
	@Test
	public void testVertexIDEquals1()
	{
		equalityTestingHelper(new VertexID("A"), new VertexID("A"), new VertexID("B"), new VertexID("C"));
	}

	/** Tests equality for VertexIDs. */
	@Test
	public void testVertexIDEquals2()
	{
		equalityTestingHelper(new VertexID(VertKind.POSITIVE,5), new VertexID(VertKind.POSITIVE,5), new VertexID(VertKind.NEGATIVE,9), new VertexID(VertKind.INIT,9));
	}

	public final static String 
		idP5 = new VertexID(VertKind.POSITIVE,5).getStringId(),
		idN5 = new VertexID(VertKind.NEGATIVE,5).getStringId(),
		idP6 = new VertexID(VertKind.POSITIVE,6).getStringId();
	
	/** Tests equality for VertexIDs. */
	@Test
	public void testVertexIDEquals3()
	{
		equalityTestingHelper(new VertexID(VertKind.NEGATIVE,5), new VertexID(VertKind.NEGATIVE,5), new VertexID(VertKind.POSITIVE,5), new VertexID(VertKind.INIT,5));
	}
	
	/** Tests equality for VertexIDs with string and numerical IDs. */
	@Test
	public void testVertexIDEquals4()
	{
		equalityTestingHelper(new VertexID(VertKind.POSITIVE,5), new VertexID(idP5), new VertexID(idN5), new VertexID(VertKind.INIT,9));
	}

	/** Tests equality for VertexIDs with string and numerical IDs, checking that cached representation works. */
	@Test
	public void testVertexIDEquals_cached()
	{
		VertexID p=new VertexID(VertKind.POSITIVE,5), q=new VertexID(idP5), 
		differentA=new VertexID(VertKind.POSITIVE,6), differentB=new VertexID(VertKind.INIT,9);
		equalityTestingHelper(p, p, differentA, differentB);// at this point, numeric ID will have a textual representation added

		equalityTestingHelper(p, q, differentA, differentB);
		equalityTestingHelper(p, new VertexID(idP5), differentA, differentB);
		equalityTestingHelper(new VertexID(idP5), q, differentA, differentB);
		equalityTestingHelper(new VertexID(idP5), q, new VertexID(idP6), new VertexID(VertKind.POSITIVE,6));
		equalityTestingHelper(new VertexID(idP5), q, new VertexID(idN5), new VertexID(VertKind.POSITIVE,6));
	}

	/** Tests VertexID toString methods. */
	@Test
	public void testVertexIDToString()
	{
		Assert.assertEquals("P5", new VertexID(idP5).toString());
		Assert.assertEquals("N5", new VertexID(idN5).toString());
		Assert.assertEquals("P5", new VertexID(VertKind.POSITIVE,5).toString());
		Assert.assertEquals("N5", new VertexID(VertKind.NEGATIVE,5).toString());

		Assert.assertEquals("JustAnything", new VertexID("JustAnything").toString());
		Assert.assertEquals("V5", new VertexID(VertKind.NEUTRAL,5).toString());
	}
	
	@Test
	public void testVertexIDLess1()
	{
		VertexID pA=new VertexID(VertKind.POSITIVE,5), pB=new VertexID(idP5),
			qA = new VertexID(VertKind.POSITIVE,6);
		checkLessHelper(pA,qA);
		checkLessHelper(pB,qA);// now qA has a cached representation of its string value.
		checkLessHelper(pB,qA);
		checkLessHelper(pA,qA);
	}
	
	@Test
	public void testVertexIDLess2()
	{
		VertexID pA=new VertexID(idP5), pB=new VertexID(VertKind.POSITIVE,5),
			qA = new VertexID(idP6);
		checkLessHelper(pA,qA);
		checkLessHelper(pB,qA);// now pB has a cached representation of its string value.
		checkLessHelper(pB,qA);
		checkLessHelper(pA,qA);
	}
	
	private final DeterministicVertex DvertA = new DeterministicVertex("a"),DvertB = new DeterministicVertex("a");
	private final DeterministicVertex DdifferentA = new DeterministicVertex("b");

	private final StringVertex SvertA = new StringVertex("a"),SvertB = new StringVertex("a");
	private final StringVertex SdifferentA = new StringVertex("b");

	/** Resets the attributes on vertices used for equality testing. */
	@Before
	public void beforeTests()
	{
		DvertA.setAccept(true);DvertB.setAccept(true);DvertA.setColour(null);DvertB.setColour(null);DvertA.setHighlight(false);DvertB.setHighlight(false);
		SvertA.setAccept(true);SvertB.setAccept(true);SvertA.setColour(null);SvertB.setColour(null);SvertA.setHighlight(false);SvertB.setHighlight(false);
	}
	
	@Test
	public void checkDEquality1()
	{
		DvertA.setAccept(true);DvertB.setAccept(true);
		equalityTestingHelper(DvertA,DvertA,DdifferentA,SdifferentA);equalityTestingHelper(DvertB,DvertB,DdifferentA,SdifferentA);
		equalityTestingHelper(DvertA,DvertB,DdifferentA,SdifferentA);
	}

	@Test
	public void checkDEquality2()
	{
		DvertA.setAccept(false);DvertB.setAccept(false);
		equalityTestingHelper(DvertA,DvertA,DdifferentA,SdifferentA);equalityTestingHelper(DvertB,DvertB,DdifferentA,SdifferentA);
		equalityTestingHelper(DvertA,DvertB,DdifferentA,SdifferentA);
	}

	@Test
	public void checkDEquality3()
	{
		DvertA.setAccept(true);DvertB.setAccept(false);
		equalityTestingHelper(DvertA,DvertA,DvertB,DdifferentA);equalityTestingHelper(DvertB,DvertB,DvertA,DdifferentA);
	}
	
	@Test
	/** Checks that attributes other than accept and name are ignored. */
	public void checkDEquality_ignoresAttrs()
	{
		DvertA.setAccept(true);DvertB.setAccept(true);
		DvertA.setColour(JUConstants.RED);DvertA.setHighlight(true);DvertA.addUserDatum(JUConstants.INITIAL, "", UserData.SHARED);DvertA.addUserDatum(JUConstants.JUNKVERTEX, "a", UserData.SHARED);
		DvertB.setColour(JUConstants.BLUE);DvertB.setHighlight(true);DvertB.removeUserDatum(JUConstants.INITIAL);DvertB.addUserDatum(JUConstants.JUNKVERTEX, "b", UserData.SHARED);
		equalityTestingHelper(DvertA,DvertB,DdifferentA,SdifferentA);
	}
	
	@Test
	public void checkSEquality1()
	{
		SvertA.setAccept(true);SvertB.setAccept(true);
		equalityTestingHelper(SvertA,SvertA,SdifferentA,DdifferentA);equalityTestingHelper(SvertB,SvertB,SdifferentA,DdifferentA);
		equalityTestingHelper(SvertA,SvertB,SdifferentA,DdifferentA);
	}

	@Test
	public void checkSEquality2()
	{
		SvertA.setAccept(false);SvertB.setAccept(false);
		equalityTestingHelper(SvertA,SvertA,SdifferentA,DdifferentA);equalityTestingHelper(SvertB,SvertB,SdifferentA,DdifferentA);
		equalityTestingHelper(SvertA,SvertB,SdifferentA,DdifferentA);
	}

	@Test
	/** Checks that if one is accept and another one is reject, they are different. */ 
	public void checkSEquality3()
	{
		SvertA.setAccept(true);SvertB.setAccept(false);
		equalityTestingHelper(SvertA,SvertA,SvertB,SdifferentA);
		equalityTestingHelper(SvertB,SvertB,SvertA,SdifferentA);
	}	

	@Test
	/** Checks that attributes other than accept and name are ignored. */
	public void checkSEquality_ignoresAttrs()
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
	public void checkSEquality4()
	{
		DvertA.setAccept(true);DvertB.setAccept(true);SvertA.setAccept(true);SvertB.setAccept(true);
		equalityTestingHelper(DvertA,DvertB,SvertA,SvertB);
	}	
	 */ 

	/** Checks that implementations of different types can be compared. 
	 */
	@Test
	public void checkEquality_differentTypes()
	{
		equalityTestingHelper(SvertA,DvertA,SdifferentA,DdifferentA);
	}

	@Test
	public void checkDComparison1()
	{
		checkLessHelper(DvertA, new DeterministicVertex("b"));
	}
	
	@Test
	public void checkSComparison1()
	{
		checkLessHelper(SvertA, new StringVertex("b"));
	}
	
	@Test
	public void checkComparison_differentTypes()
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
	public void checkComparison_fail1()
	{
		SvertA.compareTo(DvertA);
	}
		
	@Test(expected=IllegalArgumentException.class)
	public void checkComparison_fail2()
	{
		DvertA.compareTo(SvertA);
	}
	 */
	
	@Test
	public void testDeterministicVertexComparison1_old()
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
	public void testDeterministicVertexComparison2_old()
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
	public void testDeterministicVertexComparison3_old()
	{
		DeterministicVertex p = new DeterministicVertex("P"), q= new DeterministicVertex("P");
		assertTrue(p.equals(q));
		assertTrue(p.compareTo(q)==0);
	}

	@Test(expected=IllegalArgumentException.class)
	public void testEqClassEquality_fail1()
	{
		new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{}));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testEqClassEquality_fail2()
	{
		new AMEquivalenceClass(null);
	}
	
	@Test
	public void testEqClass_toString1()
	{
		Assert.assertEquals("[B->{B,A,D}]",
		new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				new StringVertex("B"),new StringVertex("A"),new StringVertex("D")})).toString());		
	}
	
	@Test
	public void testEqClass_toString2()
	{
		Assert.assertEquals("[B->{B}]",
		new AMEquivalenceClass(Arrays.asList(new CmpVertex[]{
				new StringVertex("B")})).toString());		
	}
	
	@Test
	public void testEqClassEquality1()
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
	public void testEqClassEquality2()
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
	public void testEqClassEquality3()
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
	public void testStatePairEquality()
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
	
	private static void checkLess(String a,String b,String c,String d)
	{
		checkLessHelper(new StatePair(new StringVertex(a), new StringVertex(b)), new StatePair(new StringVertex(c), new StringVertex(d)));
		checkLessHelper(new StatePair(new DeterministicVertex(a), new StringVertex(b)), new StatePair(new DeterministicVertex(c), new StringVertex(d)));
		checkLessHelper(new StatePair(new StringVertex(a), new DeterministicVertex(b)), new StatePair(new StringVertex(c), new DeterministicVertex(d)));
	}
	
	@Test
	public void testStatePairComparison()
	{
		checkLess("a","b","c","d");
		checkLess("a","b","a","c");
		checkLess("a","b","c","b");
	}
	
	/** Given a textual representation of an fsm, builds a corresponding Jung graph
	 * 
	 * @param fsm the textual representation of an FSM
	 * @param name graph name, to be displayed as the caption of the Jung window.
	 * @return Jung graph for it
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public static DirectedSparseGraph buildGraph(String fsm,String name)
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

		if (isGraphTransformationDebug(g))
		{
			Visualiser.updateFrame(g, null);System.out.println("******** PROCESSING "+name+" **********\n");
		}
		return g;
	}

	/** Checks if the passed graph is isomorphic to the provided fsm
	 * 
	 * @param g graph to check
	 * @param fsm the string representation of the machine which the graph should be isomorphic to
	 */
	public void checkEq(DirectedSparseGraph g, String fsm)
	{
		DirectedSparseGraph expectedGraph = buildGraph(fsm,"expected graph");
		final LearnerGraph graph = new LearnerGraph(g,Configuration.getDefaultConfiguration());
		final LearnerGraph expected = new LearnerGraph(expectedGraph,Configuration.getDefaultConfiguration());
		
		assertEquals("incorrect data",true,expected.equals(graph));
	}

	@Test
	public void testCheckEq()
	{
		DirectedSparseGraph g=buildGraph("P-a->Q_State-b->P-c->P","testCheckEq");
		checkEq(g,"P-c->P<-b-Q_State<-a-P");
	}
	
	/** Verifies the equivalence of a supplied graph to the supplied machine. */
	public static void checkM(DirectedSparseGraph g,String fsm,Configuration conf)
	{
		final LearnerGraph graph = new LearnerGraph(g,conf);
		final DirectedSparseGraph expectedGraph = buildGraph(fsm,"expected graph");
		final LearnerGraph expected = new LearnerGraph(expectedGraph,conf);
		WMethod.checkM(graph,expected);
	}
		
	@Test
	public void testCheckM1()
	{
		checkM(buildGraph("A-a->B-b->C", "testCheck1"), "B-a->C-b->D",config);
	}
	
	@Test
	public void testCheckM2()
	{
		checkM(buildGraph("A-a->B-b->C-d-#F#-b-A", "testCheck2"), "B-a->C-b->D\nB-b-#REJ\nD-d-#REJ",config);
	}

	@Test
	public void testCheckM3()
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ";
		String expected = "A-a->B-b->C-b-#F#-d-A";
		checkM(buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheck3"), expected,config);
	}

	@Test
	public void testCheckM4() // multiple reject states
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ\nA-b-#REJ2\nB-a-#REJ2\nB-c-#REJ3";
		String expected = "A-a->B-b->C-b-#F#-d-A-b-#R\nB-a-#R\nU#-c-B";
		checkM(buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheck4"), expected,config);
	}

	@Test
	public void testCheckM5()
	{
		checkM(buildGraph("A-a->B-b->B-a->C", "testCheck5"), "S-a->U<-b-U\nQ<-a-U",config);
	}

	@Test
	public void testCheckM6()
	{
		final LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-b->B-a->C", "testCheck6"),config);
		final LearnerGraph expected = new LearnerGraph(buildGraph("U<-b-U\nQ<-a-U<-a-S","expected graph"),config);
		assertTrue(checkMBoolean(graph,expected,"A","S"));
		assertTrue(checkMBoolean(graph,expected,"B","U"));
		assertTrue(checkMBoolean(graph,expected,"C","Q"));
	}

	@Test
	public final void testCheckM_multipleEq1() // equivalent states
	{
		final LearnerGraph graph = new LearnerGraph(buildGraph("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3","testCheckM_multipleEq1"),config);
		assertTrue(checkMBoolean(graph,graph,"D","C2"));
		assertTrue(checkMBoolean(graph,graph,"C2","D"));
		
		assertTrue(checkMBoolean(graph,graph,"D1","D2"));
		assertTrue(checkMBoolean(graph,graph,"D2","D1"));

		assertTrue(checkMBoolean(graph,graph,"D2","K2"));
		assertTrue(checkMBoolean(graph,graph,"K2","D2"));

		assertFalse(checkMBoolean(graph,graph,"D2","A1"));
		assertFalse(checkMBoolean(graph,graph,"A1","D2"));

		assertFalse(checkMBoolean(graph,graph,"D2","F1"));
		assertFalse(checkMBoolean(graph,graph,"F1","D2"));
	}

	@Test
	public final void testCheckM_multipleEq2() // equivalent states
	{
		final DirectedSparseGraph g = buildGraph("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->N-b->N","testCheckM_multipleEq2");
		final LearnerGraph graph = new LearnerGraph(g,Configuration.getDefaultConfiguration());
		List<String> states = Arrays.asList(new String[]{"S","A","B","C","D","N"});
		for(String stA:states)
			for(String stB:states)
				assertTrue("states "+stA+"and "+stB+" should be equivalent",checkMBoolean(graph,graph,stA,stB));
	}
	
	@Test
	public final void testCheckM_multipleEq3() // equivalent states
	{
		final DirectedSparseGraph g = buildGraph("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->M-a->N\nN-b->M-b->N","testCheckM_multipleEq3");
		final LearnerGraph graph = new LearnerGraph(g,Configuration.getDefaultConfiguration());
		List<String> states = Arrays.asList(new String[]{"S","A","B","C","D","N","M"});
		for(String stA:states)
			for(String stB:states)
				assertTrue("states "+stA+"and "+stB+" should be equivalent",checkMBoolean(graph,graph,stA,stB));
	}
	
	@Test
	public final void testCheckM_multipleEq4() // non-equivalent states
	{
		final DirectedSparseGraph g = buildGraph("A-a->B-a->C-a->A-b->C-b->B","testCheckM_multipleEq4");
		final LearnerGraph graph = new LearnerGraph(g,Configuration.getDefaultConfiguration());
		List<String> states = Arrays.asList(new String[]{"A","B","C"});
		for(String stA:states)
			for(String stB:states)
				if (stA.equals(stB))
					assertTrue("states "+stA+" and "+stB+" should be equivalent",checkMBoolean(graph,graph,stA,stB));
				else
					assertFalse("states "+stA+" and "+stB+" should not be equivalent",checkMBoolean(graph,graph,stA,stB));
	}
	
	/** Same as checkM, but returns a boolean false instead of an exception. */
	public static boolean checkMBoolean(LearnerGraph graph, LearnerGraph expected, String stateGraph, String stateExpected)
	{
		try
		{
			WMethod.checkM(graph,expected,graph.findVertex(stateGraph),expected.findVertex(stateExpected));
		}
		catch(DifferentFSMException ex)
		{
			return false;
		}
		return true;
	}
	
	@Test
	public void testCheckM6_f1()
	{
		final LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-b->B-a->C", "testCheck6"), Configuration.getDefaultConfiguration());
		final LearnerGraph expected = new LearnerGraph(buildGraph("U<-b-U\nQ<-a-U<-a-S","expected graph"),Configuration.getDefaultConfiguration());
		Assert.assertTrue(checkMBoolean(graph,graph,"A","A"));
		Assert.assertTrue(checkMBoolean(graph,graph,"B","B"));
		Assert.assertTrue(checkMBoolean(graph,graph,"C","C"));
		Assert.assertTrue(checkMBoolean(expected,expected,"Q","Q"));
		Assert.assertTrue(checkMBoolean(expected,expected,"S","S"));
		
		Assert.assertFalse(checkMBoolean(graph,expected,"A","Q"));
		Assert.assertFalse(checkMBoolean(graph,expected,"A","U"));
		Assert.assertFalse(checkMBoolean(graph,expected,"B","Q"));
		Assert.assertFalse(checkMBoolean(graph,expected,"B","S"));
		Assert.assertFalse(checkMBoolean(graph,expected,"C","U"));
		Assert.assertFalse(checkMBoolean(graph,expected,"C","S"));
	}
	

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD1()
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD1"), "B-a->C-b->B",config);		
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD2() // different reject states
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD2"), "B-a->C-b-#D",config);
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD3() // missing transition
	{
		checkM(buildGraph("A-a->B-b->C\nA-b->B", "testCheckMD3"), "B-a->C-b->D",config);
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD4() // extra transition
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD4"), "B-a->C-b->D\nB-b->C",config);
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD5() // missing transition
	{
		checkM(buildGraph("A-a->B-b->C\nB-c->B", "testCheckMD5"), "B-a->C-b->D",config);
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD6() // extra transition
	{
		checkM(buildGraph("A-a->B-b->C", "testCheckMD6"), "B-a->C-b->D\nC-c->C",config);
	}

	@Test(expected = DifferentFSMException.class)
	public void testCheckMD7() // swapped transitions
	{
		String another  = "A-a->B-b->C\nC-b-#REJ\nA-d-#REJ";
		String expected = "A-a->B-b->C-d-#F#-b-A";
		checkM(buildGraph(another.replace('A', 'Q').replace('B', 'G').replace('C', 'A'), "testCheckMD7"), expected,config);
	}

	@Test
	public void completeComputeAlphabet0()
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
	public void testComputeFSMAlphabet2()
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
	public void completeComputeAlphabet5()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-a->S\nA-c->A\nB-b->B\nC-a->C-b->C-c->C\nQ-b->Q-c->Q\nS-a->S-b->S-c->S", "completeComputeAlphabet5");
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {"a","b","c"}));
		Assert.assertEquals(expected, new LearnerGraph(g,config).wmethod.computeAlphabet());
		Assert.assertEquals(expected, DeterministicDirectedSparseGraph.computeAlphabet(g));				

		LearnerGraph clone = new LearnerGraph(g,config).copy(config);
		Assert.assertFalse( clone.paths.completeGraph(
				new VertexID("REJECT")));
		Assert.assertFalse(DeterministicDirectedSparseGraph.completeGraph(g,"REJECT"));
	}
	
	/** Given a graph, this one calls completeGraph and checks that the returned value is whetherToBeCompleted
	 * and that the final graph is equivalent to the one provided. Note that this also checks that in case
	 * a graph does not need to be completed, the result of completion is the same graph.
	 * 
	 * @param originalGraph the graph to complete
	 * @param expectedOutcome the graph to be expected
	 * @param whetherToBeCompleted whether graph "machine" is incomplete.
	 * @param testName the name of the test to give to the above graph.
	 */
	private void completeGraphTestHelper(String originalGraph, String expectedOutcome, boolean whetherToBeCompleted, String testName)
	{
		DirectedSparseGraph g = buildGraph(originalGraph, testName);
		Assert.assertEquals(whetherToBeCompleted,DeterministicDirectedSparseGraph.completeGraph(g,"REJECT"));checkM(g, expectedOutcome,config);		
		LearnerGraph fsm = new LearnerGraph(buildGraph(originalGraph, testName),config);
		Assert.assertEquals(whetherToBeCompleted,fsm.paths.completeGraph(
				new VertexID("REJECT")));
		WMethod.checkM(fsm, new LearnerGraph(buildGraph(expectedOutcome,testName),config));				
	}
	
	/** Checks that passing a name of an existing state causes an exception to be thrown. */
	@Test(expected=IllegalArgumentException.class)
	public void complete_fail()
	{
		new LearnerGraph(buildGraph("A-a->A-b->B-c->B", "complete_fail"),config).paths.completeGraph(
				new VertexID("B"));
	}
	
	@Test
	public void completeGraphTest1()
	{
		completeGraphTestHelper("A-a->A", "A-a->A",false,"completeGraphTest1");
	}
	
	@Test
	public void completeGraphTest2()
	{
		completeGraphTestHelper("A-a->B-a->A", "A-a->A", false, "completeGraphTest2");
	}
	
	@Test
	public void completeGraphTest3()
	{
		completeGraphTestHelper("A-a->A<-b-A", "A-b->A-a->A", false, "completeGraphTest3");
	}
	
	@Test
	public void completeGraphTest4a()
	{
		completeGraphTestHelper("A-a->B-b->A", "A-a->B-b->A\nA-b-#REJECT#-a-B", true, "completeGraphTest4a");
	}
	
	@Test
	public void completeGraphTest4b()
	{
		completeGraphTestHelper("A-a->B-b->A-b->A", "A-a->B-b->A-b->A\nREJECT#-a-B", true, "completeGraphTest4b");
	}

	@Test
	public void completeGraphTest5()
	{
		completeGraphTestHelper("A-a->A-b->B-c->B", "A-a->A-b->B-c->B\nA-c-#REJECT#-a-B-b-#REJECT", true, "completeGraphTest5");
	}	
	
	@Test
	public void completeGraphTest6()
	{
		completeGraphTestHelper("A-a->A-b->B-c->B-a->C", "A-a->A-b->B-c->B-a->C\nA-c-#REJECT#-b-B\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT", true, "completeGraphTest6");
	}	
	
	@Test
	public void completeGraphTest7()
	{
		String fsmOrig = "A-a->A-b->B-c->B-a->C\nQ-d->S",
			fsmExpected = "A-a->A-b->B-c->B-a->C\nA-c-#REJECT\nA-d-#REJECT\nB-b-#REJECT\nB-d-#REJECT\nC-a-#REJECT\nC-b-#REJECT\nC-c-#REJECT\nC-d-#REJECT\nS-a-#REJECT\nS-b-#REJECT\nS-c-#REJECT\nS-d-#REJECT\nQ-a-#REJECT\nQ-b-#REJECT\nQ-c-#REJECT\nQ-d->S";
		completeGraphTestHelper(fsmOrig,fsmExpected,true,"completeGraphTest7");
		
		// Additional checking.
		DirectedSparseGraph g = buildGraph(fsmOrig, "completeGraphTest7");
		final LearnerGraph graph = new LearnerGraph(g,config);
		Assert.assertTrue(graph.paths.completeGraph(
				new DeterministicDirectedSparseGraph.VertexID("REJECT")));
		final LearnerGraph expected = new LearnerGraph(buildGraph(fsmExpected,"completeGraphTest7"),config);
		Assert.assertTrue(checkMBoolean(graph,expected,"A","A"));
		Assert.assertTrue(checkMBoolean(graph,expected,"B","B"));
		Assert.assertTrue(checkMBoolean(graph,expected,"Q","Q"));
		Assert.assertTrue(checkMBoolean(graph,expected,"S","S"));
		Assert.assertTrue(checkMBoolean(graph,expected,"REJECT","REJECT"));
	}	

	@Test(expected = IllegalArgumentException.class)
	public void testFindVertex0()
	{
		DeterministicDirectedSparseGraph.findVertex(JUConstants.JUNKVERTEX, null, new DirectedSparseGraph());
	}

	@Test
	public void testFindVertex1()
	{
		Assert.assertNull(DeterministicDirectedSparseGraph.findVertex(JUConstants.JUNKVERTEX, "bb", new DirectedSparseGraph()));
	}
	
	@Test
	public void testFindVertex2()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex2");
		//Visualiser.updateFrame(g, g);Visualiser.waitForKey();
		Assert.assertNull(DeterministicDirectedSparseGraph.findVertex(JUConstants.JUNKVERTEX, "bb", g));
	}
		
	@Test
	public void testFindVertex3()
	{
		DirectedSparseGraph g = buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex3");
		//Visualiser.updateFrame(g, null);Visualiser.waitForKey();
		Assert.assertNull(DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, "D", g));
	}

	@Test
	public void testFindVertex4a()
	{
		Vertex v = DeterministicDirectedSparseGraph.findVertex(JUConstants.INITIAL, "anything", buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex4a"));
		Assert.assertNull(v);
	}

	@Test
	public void testFindVertex4b()
	{
		Vertex v =  DeterministicDirectedSparseGraph.findVertex(JUConstants.INITIAL, true, buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex4b"));
		Assert.assertEquals(new VertexID("A"), v.getUserDatum(JUConstants.LABEL));
	}

	@Test
	public void testFindVertex5()
	{
		Vertex v =  DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("A"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex5"));
		Assert.assertEquals(new VertexID("A"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindVertex6()
	{
		Vertex v =  DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("C"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex6"));
		Assert.assertEquals(new VertexID("C"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindVertex7()
	{
		Vertex v = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("S"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex7"));
		Assert.assertEquals(new VertexID("S"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindVertex8()
	{
		Vertex v = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, new VertexID("Q"), buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindVertex8"));
		Assert.assertEquals(new VertexID("Q"), v.getUserDatum(JUConstants.LABEL));
	}

	
	@Test
	public void testFindInitial1()
	{
		Vertex v = DeterministicDirectedSparseGraph.findInitial(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S", "testFindInitial"));
		Assert.assertEquals(new VertexID("A"), v.getUserDatum(JUConstants.LABEL));
	}
	
	@Test
	public void testFindInitial2()
	{
		Vertex v = DeterministicDirectedSparseGraph.findInitial(new DirectedSparseGraph());
		Assert.assertNull(v);
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
				throw new IllegalArgumentException("invalid data in array");// TODO: to test that this exception is thrown.
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
		List<List<String>> result = new LinkedList<List<String>>();result.addAll(buildSet(data));
		return result;
	}
	
	@Test
	public void testBuildSet1()
	{
		assertTrue(buildSet(new String[] []{}).isEmpty());
	}

	@Test
	public void testBuildSet2()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(new LinkedList<String>());
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{}})));
	}

	@Test
	public void testBuildSet3A()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		expectedResult.add(new LinkedList<String>());
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{},new String[]{"a","b","c"}})));
	}

	@Test
	public void testBuildSet3B()
	{
		Set<List<String>> expectedResult = new HashSet<List<String>>();
		expectedResult.add(Arrays.asList(new String[]{"a","b","c"}));
		assertTrue(expectedResult.equals(buildSet(new String[] []{new String[]{"a","b","c"}})));
	}

	@Test
	public void testBuildSet4()
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
	public void testBuildStringMap1()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
		})));
	}
	
	@Test
	public void testBuildStringMap2()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"a"},"value2"},
				new Object[]{new String[]{"b"},"value3"}
		})));
	}
	
	@Test
	public void testBuildStringMap3()
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
	public void testBuildStringMap4()
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
	public void testBuildStringMap5()
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
	public void testBuildStringMap6()
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
	public void testBuildStringMap7()
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
	public void testBuildStringMap8()
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
	public void testBuildStringMap9()
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
	public void testBuildStringMap10()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{new String[]{"a"},new Object()},// an invalid sequence - null in the first element
				new Object[]{new String[]{"b"},"value3"}
		})));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testBuildStringMap11()
	{
		Map<String,String> expectedResult = new HashMap<String,String>();
		expectedResult.put("a","value1");expectedResult.put("strC","value2");expectedResult.put("b","value3");
		
		assertTrue(expectedResult.equals(buildStringMap(new Object[][]{
				new Object[]{new String[]{"strC"},"value1"},
				new Object[]{null, "a"},// an invalid sequence - null in the second element
				new Object[]{new String[]{"b"},null}
		})));
	}
	
	public final void checkForCorrectException(final int [][]tTable, final int []vFrom, String exceptionString)
	{
		try
		{
			LearnerGraph.convertTableToFSMStructure(tTable, vFrom, -1	,config);
			Assert.fail("Exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			Assert.assertTrue(ex.getMessage().contains(exceptionString));
		}
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
		WMethod.checkM(fsm, new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure5"),config), fsm.findVertex("S0"), fsm.findVertex("S0"));
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
		WMethod.checkM(fsm, new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure6"),config), fsm.findVertex("S0"), fsm.findVertex("S0"));
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
		WMethod.checkM(fsm, new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure7"),config), fsm.findVertex("S0"), fsm.findVertex("S0"));
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
		WMethod.checkM(fsm, new LearnerGraph(buildGraph("S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0", "testConvertTableToFSMStructure8"),config), fsm.findVertex("S0"), fsm.findVertex("S0"));
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
