package statechum.analysis.learning;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static statechum.analysis.learning.TestFSMAlgo.buildSet;

import java.awt.Point;
import java.io.IOException;
import java.io.StringReader;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import javax.swing.SwingUtilities;

import junit.framework.AssertionFailedError;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.computeStateScores.PairScore;
import statechum.xmachine.model.testset.WMethod;

import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class TestPTAConstruction
{
	public TestPTAConstruction() {
		
	}

	@Test
	public void testAugmentPTA() // only two traces, both accept
	{
		Set<List<String>> plusStrings = buildSet(new String[][] { new String[] {"a","b","c"},new String[]{"a","d","c"} });
		DirectedSparseGraph actualA = new RPNIBlueFringeLearner(null).augmentPTA(RPNIBlueFringeLearner.initialise(), plusStrings, true),
			actualB = computeStateScores.augmentPTA(RPNIBlueFringeLearner.initialise(), plusStrings, true),
			actualC = new computeStateScores().augmentPTA(plusStrings, true).getGraph();
		RPNIBlueFringeLearner.numberVertices(actualA);
		String expectedPTA = "A-a->B--b->C-c->End1\nB--d->C2-c->End2";
		TestFSMAlgo.checkM(actualA, expectedPTA);
		TestFSMAlgo.checkM(actualB, expectedPTA);
		TestFSMAlgo.checkM(actualC, expectedPTA);
	}

	private void checkEmptyPTA(String[][] arrayPlusStrings,String [][] arrayMinusStrings)
	{
		Set<List<String>> plusStrings = buildSet(arrayPlusStrings), minusStrings = buildSet(arrayMinusStrings);
		DirectedSparseGraph actualA = null, actualB =null, actualC = null;
		IllegalArgumentException eA = null, eB = null, eC = null;
		try
		{
			actualA = new RPNIBlueFringeLearner(null).createAugmentedPTA(RPNIBlueFringeLearner.initialise(), plusStrings, minusStrings);
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eA = e;
		}

		try
		{
			actualB = new RPNIBlueFringeLearnerTestComponentOpt(null).createAugmentedPTA(RPNIBlueFringeLearner.initialise(), plusStrings, minusStrings);
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eB = e;
		}
		

		try
		{
			actualC = new RPNIBlueFringeLearnerTestComponentOpt(null).createAugmentedPTA(plusStrings, minusStrings).getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eC = e;
		}

		if (eA != null)
		{
			Assert.assertNotNull(eB);
			Assert.assertNotNull(eC);
			throw eA;
		}
		else
			if (eB != null)
			{
				Assert.assertNotNull(eA);
				Assert.assertNotNull(eC);
				throw eA;
			}
			else
				if (eC != null)
				{
					Assert.assertNotNull(eA);
					Assert.assertNotNull(eB);
					throw eA;
				}
				
		Assert.assertEquals(1, actualA.getVertices().size());Assert.assertEquals(true, TestRpniLearner.isAccept( ((Vertex)actualA.getVertices().iterator().next()) )); 
		Assert.assertEquals(0, actualA.getEdges().size());
	
		Assert.assertEquals(1, actualB.getVertices().size());Assert.assertEquals(true, TestRpniLearner.isAccept( ((Vertex)actualB.getVertices().iterator().next()) )); 
		Assert.assertEquals(0, actualB.getEdges().size());

		Assert.assertEquals(1, actualC.getVertices().size());Assert.assertEquals(true, TestRpniLearner.isAccept( ((Vertex)actualC.getVertices().iterator().next()) )); 
		Assert.assertEquals(0, actualC.getEdges().size());
	}
	
	@Test
	public void testPTAconstruction1a()// an empty accept trace
	{
		checkEmptyPTA(
				new String[][] { new String[]{}},
				new String[][] { }
			);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction1b()// an empty reject trace
	{
		checkEmptyPTA(
				new String[][] { },
				new String[][] { new String[]{} }
			);
	}

	@Test
	public void testPTAconstruction1c()// empty traces
	{
		checkEmptyPTA(
				new String[][] {},
				new String[][] {}
			);
	}

	private void checkPTAconstruction(String[][] arrayPlusStrings,String [][] arrayMinusStrings, String expectedPTA)
	{
		Set<List<String>> plusStrings = buildSet(arrayPlusStrings), minusStrings = buildSet(arrayMinusStrings);
		DirectedSparseGraph actualA = null, actualB =null, actualC =null;
		IllegalArgumentException eA = null, eB = null, eC = null;
		try
		{
			actualA = new RPNIBlueFringeLearner(null).createAugmentedPTA(RPNIBlueFringeLearner.initialise(), plusStrings, minusStrings);
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eA = e;
		}

		try
		{
			actualB = new RPNIBlueFringeLearnerTestComponentOpt(null).createAugmentedPTA(RPNIBlueFringeLearner.initialise(), plusStrings, minusStrings);
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eB = e;
		}
		
		try
		{
			actualC = new RPNIBlueFringeLearnerTestComponentOpt(null).createAugmentedPTA(plusStrings, minusStrings).getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eC = e;
		}
		
		if (eA != null)
		{
			Assert.assertNotNull(eB);
			Assert.assertNotNull(eC);
			throw eA;
		}
		else
			if (eB != null)
			{
				Assert.assertNotNull(eA);
				Assert.assertNotNull(eC);
				throw eA;
			}
			else
				if (eC != null)
				{
					Assert.assertNotNull(eA);
					Assert.assertNotNull(eB);
					throw eA;
				}

	 //updateFrame(g,null);
		TestFSMAlgo.checkM(actualA, expectedPTA);
		TestFSMAlgo.checkM(actualB, expectedPTA);
		TestFSMAlgo.checkM(actualC, expectedPTA);
	}
	
	@Test
	public void testPTAconstruction2()// two accept traces and one reject one
	{
		checkPTAconstruction(
			new String[][] { new String[]{"a","b","c"}, new String[]{"a","d","c"}},
			new String[][] { new String[]{"a","b","c","d"} },
			"A-a->B--b->C-c->End1-d-#REJ\nB--d->C2-c->End2"
			);
	}

	@Test
	public void testPTAconstruction3()// two accept traces and two reject ones
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, new String[]{"a","u"} },
				"A-a->B--b->C-c->End1-d-#REJ\nB--d->C2-c->End2\nB-u-#A2");
	}

	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction4()// a trace goes through a reject-state
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, 
						new String[]{"a","b"} },
				"junk");
	}


	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction5()// a trace goes through a reject-state
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, 
						new String[]{"a","b","c"} },
				"junk");
	}


	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction6()// a trace goes through a reject-state
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, 
						new String[]{"a","b","c","d","e"} },
				"junk");
	}

}
