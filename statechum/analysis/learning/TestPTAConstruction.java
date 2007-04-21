package statechum.analysis.learning;

import static statechum.analysis.learning.TestFSMAlgo.buildSet;

import java.util.List;
import java.util.Set;

import junit.framework.JUnit4TestAdapter;

import org.junit.Assert;
import org.junit.Test;

import statechum.analysis.learning.computeStateScores.IDMode;

import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class TestPTAConstruction
{
	public TestPTAConstruction() {
		
	}

	@Test
	public void testAugmentPTA() // only two traces, both accept
	{
		Set<List<String>> plusStrings = buildSet(new String[][] { new String[] {"a","b","c"},new String[]{"a","d","c"} });
		DirectedSparseGraph actualA = new RPNIBlueFringeLearner(null).augmentPTA(RPNIBlueFringeLearner.initialise(), plusStrings, true),
			actualC = new computeStateScores(0,0).setMode(IDMode.POSITIVE_NEGATIVE).augmentPTA(plusStrings, true).getGraph();
		RPNIBlueFringeLearner.numberVertices(actualA);
		String expectedPTA = "A-a->B--b->C-c->End1\nB--d->C2-c->End2";
		TestFSMAlgo.checkM(actualA, expectedPTA);
		TestFSMAlgo.checkM(actualC, expectedPTA);
	}

	private void checkEmptyPTA(String[][] arrayPlusStrings,String [][] arrayMinusStrings)
	{
		Set<List<String>> plusStrings = buildSet(arrayPlusStrings), minusStrings = buildSet(arrayMinusStrings);
		DirectedSparseGraph actualA = null, actualC = null;
		IllegalArgumentException eA = null, eC = null;
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
			actualC = new RPNIBlueFringeLearnerTestComponentOpt(null).createAugmentedPTA(plusStrings, minusStrings).getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eC = e;
		}

		if (eA != null)
		{
			Assert.assertNotNull(eC);
			throw eA;
		}
		else
			if (eC != null)
			{
				Assert.assertNotNull(eA);
				Assert.assertNotNull(eC);
				throw eA;
			}
				
		Assert.assertEquals(1, actualA.getVertices().size());Assert.assertEquals(true, TestRpniLearner.isAccept( ((Vertex)actualA.getVertices().iterator().next()) )); 
		Assert.assertEquals(0, actualA.getEdges().size());

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
		DirectedSparseGraph actualA = null, actualC =null;
		IllegalArgumentException eA = null, eC = null;
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
			actualC = new RPNIBlueFringeLearnerTestComponentOpt(null).setMode(IDMode.POSITIVE_NEGATIVE).createAugmentedPTA(plusStrings, minusStrings).getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eC = e;
		}
		
		if (eA != null)
		{
			Assert.assertNotNull(eC);
			throw eA;
		}
		else
			if (eC != null)
			{
				Assert.assertNotNull(eA);
				throw eA;
			}

	 //updateFrame(g,null);
		TestFSMAlgo.checkM(actualA, expectedPTA);
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

	
	/** In order to be able to use old junit runner. */
	public static junit.framework.Test suite()
	{
		return new JUnit4TestAdapter(TestLoadAnswers.class);
	}
}
