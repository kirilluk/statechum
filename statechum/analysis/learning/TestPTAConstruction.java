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

import static statechum.analysis.learning.TestFSMAlgo.buildSet;

import java.util.List;
import java.util.Set;

import junit.framework.JUnit4TestAdapter;

import org.junit.Assert;
import org.junit.Test;

import statechum.analysis.learning.ComputeStateScores.IDMode;
import static statechum.analysis.learning.RPNIBlueFringeLearner.isAccept;

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
			actualC = new ComputeStateScores(0).setMode(IDMode.POSITIVE_NEGATIVE).augmentPTA(plusStrings, true).getGraph();
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
			RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null);
			l.getScoreComputer().setMode(IDMode.POSITIVE_NEGATIVE);
			l.init(plusStrings, minusStrings);
			actualC = l.getScoreComputer().getGraph();
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
				
		Assert.assertEquals(1, actualA.getVertices().size());Assert.assertEquals(true, isAccept( ((Vertex)actualA.getVertices().iterator().next()) )); 
		Assert.assertEquals(0, actualA.getEdges().size());

		Assert.assertEquals(1, actualC.getVertices().size());Assert.assertEquals(true, isAccept( ((Vertex)actualC.getVertices().iterator().next()) )); 
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

	/** Builds a PTA from the supplied arguments using two different methods. If any of them throws, checks that another one throws too and then rethrows the exception. 
	 * 
	 * @param arrayPlusStrings allowed sequences
	 * @param arrayMinusStrings sequences ending at a reject state
	 * @param expectedPTA a textual representation of a PTA which should be built.
	 */
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
			
			RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null);
			l.getScoreComputer().setMode(IDMode.POSITIVE_NEGATIVE);
			l.init(plusStrings, minusStrings);
			actualC = l.getScoreComputer().getGraph();
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
}
