/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
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
package statechum.analysis.learning.rpnicore;

import static statechum.analysis.learning.TestFSMAlgo.buildGraph;

import java.util.LinkedList;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PairScore;


/**
 * @author kirill
 *
 */
public class TestGD {

	protected Configuration cloneConfig = null;
	
	@Before
	public final void beforeTest()
	{
		 cloneConfig = Configuration.getDefaultConfiguration().copy();cloneConfig.setLearnerCloneGraph(false);		
	}
	
	/** Tests that supplied states are not cloned when I ask them not to be cloned. */
	@Test
	public final void testAddTransitions1()
	{
		LearnerGraph gr = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("\nB-d-#C","testAddTransitions0E"),Configuration.getDefaultConfiguration());
		GD.addTransitionToGraph(gr, grAnother.findVertex("B"), "c", grAnother.findVertex("C"),cloneConfig);
		Assert.assertEquals(3,gr.getStateNumber());// the first state is the default initial one.
		Assert.assertSame(gr.findVertex("B"),grAnother.findVertex("B"));
		Assert.assertEquals(gr.findVertex("B"),grAnother.findVertex("B"));
		Assert.assertSame(gr.findVertex("C"),grAnother.findVertex("C"));
		Assert.assertEquals(gr.findVertex("C"),grAnother.findVertex("C"));
		WMethod.checkM(grAnother, expected);
	}
	
	/** Tests that supplied states are not cloned when there is already a state with that ID. */
	@Test
	public final void testAddTransitions2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("B-a->D","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("\nB-d-#C","testAddTransitions0E"),Configuration.getDefaultConfiguration());
		GD.addTransitionToGraph(gr, grAnother.findVertex("B"), "c", grAnother.findVertex("C"),cloneConfig);
		Assert.assertEquals(3,gr.getStateNumber());
		Assert.assertNotSame(gr.findVertex("B"),grAnother.findVertex("B"));
		Assert.assertEquals(gr.findVertex("B"),grAnother.findVertex("B"));
		WMethod.checkM(grAnother, expected);
	}
	
	/** Tests that supplied states are not cloned when there is already a state with that ID. */
	@Test
	public final void testAddTransitions3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("\nB-d-#C","testAddTransitions0E"),Configuration.getDefaultConfiguration());
		GD.addTransitionToGraph(gr, grAnother.findVertex("B"), "c", grAnother.findVertex("C"),cloneConfig);
		Assert.assertEquals(3,gr.getStateNumber());
		Assert.assertNotSame(gr.findVertex("C"),grAnother.findVertex("C"));
		Assert.assertEquals(gr.findVertex("C"),grAnother.findVertex("C"));
		WMethod.checkM(grAnother, expected);
	}
	
	/** Tests that inconsistent acceptance conditions are detected. */
	@Test(expected=IllegalArgumentException.class)
	public final void testAddTransitions4()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d->C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		GD.addTransitionToGraph(gr, grAnother.findVertex("B"), "c", grAnother.findVertex("C"),cloneConfig);
	}
	
	/** Tests that inconsistent acceptance conditions are detected. */
	@Test(expected=IllegalArgumentException.class)
	public final void testAddTransitions5()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("C-d->A","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		GD.addTransitionToGraph(gr, grAnother.findVertex("C"), "c", grAnother.findVertex("A"),cloneConfig);
	}
	
	/** Tests that I cannot replace an existing transition. */
	@Test(expected=IllegalArgumentException.class)
	public final void testAddTransitions6()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		GD.addTransitionToGraph(gr, grAnother.findVertex("T"), "a", grAnother.findVertex("C"),cloneConfig);
	}
	
	/** Tests that supplied states are cloned when I ask them to be cloned. */
	@Test
	public final void testAddTransitions7()
	{
		LearnerGraph gr = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("\nB-d-#C","testAddTransitions0E"),Configuration.getDefaultConfiguration());
		cloneConfig.setLearnerCloneGraph(true);
		GD.addTransitionToGraph(gr, grAnother.findVertex("B"), "c", grAnother.findVertex("C"),cloneConfig);
		Assert.assertNotSame(gr.findVertex("B"),grAnother.findVertex("B"));
		Assert.assertEquals(gr.findVertex("B"),grAnother.findVertex("B"));
		Assert.assertNotSame(gr.findVertex("C"),grAnother.findVertex("C"));
		Assert.assertEquals(gr.findVertex("C"),grAnother.findVertex("C"));
		WMethod.checkM(grAnother, expected);
	}
	
	@Test
	public final void testAddTransitionsG1()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d-#C","testAddTransitionsG1A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D"+"\nB-d-#C","testAddTransitionsG1E"),Configuration.getDefaultConfiguration());
		GD.addTransitionToGraph(gr, grAnother.findVertex("B"), "d", grAnother.findVertex("C"),cloneConfig);
		WMethod.checkM(gr, expected);
	}

	/** Adding a transition from an initial state. */
	@Test
	public final void testAddTransitionsG2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#C","testAddTransitionsG2A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D"+"\nA-d-#C","testAddTransitionsG2E"),Configuration.getDefaultConfiguration());
		GD.addTransitionToGraph(gr, grAnother.findVertex("A"), "d", grAnother.findVertex("C"),cloneConfig);
		WMethod.checkM(gr, expected);
	}
	
	/** Adding a transition to an initial state. */
	@Test
	public final void testAddTransitionsG3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d->A","testAddTransitionsG3A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D"+"\nB-d->A","testAddTransitionsG3E"),Configuration.getDefaultConfiguration());
		GD.addTransitionToGraph(gr, grAnother.findVertex("B"), "d", grAnother.findVertex("A"),cloneConfig);
		WMethod.checkM(gr, expected);
	}
	
	@Test
	public final void testRemoveTransitions1()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B\nA-b-#D","testRemoveTransitions1E"),Configuration.getDefaultConfiguration());
		GD.removeTransitionFromGraph(gr, grAnother.findVertex("B"), "a", grAnother.findVertex("C"));
		WMethod.checkM(gr, expected);
	}
	
	/** remove a transition from an initial state. */
	@Test
	public final void testRemoveTransitions2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C","testRemoveTransitions2E"),Configuration.getDefaultConfiguration());
		GD.removeTransitionFromGraph(gr, grAnother.findVertex("A"), "b", grAnother.findVertex("D"));
		WMethod.checkM(gr, expected);
	}
	
	/** remove a transition to an initial state. */
	@Test
	public final void testRemoveTransitions3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->A","testAddTransitions3"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		GD.removeTransitionFromGraph(gr, grAnother.findVertex("B"), "b", grAnother.findVertex("A"));
		WMethod.checkM(gr, expected);
	}
	
	/** remove a loop around an initial state. */
	@Test
	public final void testRemoveTransitions4()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-u->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		GD.removeTransitionFromGraph(gr, grAnother.findVertex("A"), "u", grAnother.findVertex("A"));
		WMethod.checkM(gr, expected);
	}
	
	/** Tests that a transition with a non-existing source state cannot be removed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveTransitions5()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-b->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		GD.removeTransitionFromGraph(gr, grAnother.findVertex("T"), "b", grAnother.findVertex("A"));
	}

	/** Tests that a transition with a non-existing label cannot be removed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveTransitions6()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-b->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		GD.removeTransitionFromGraph(gr, grAnother.findVertex("A"), "c", grAnother.findVertex("A"));
	}

	/** Tests that a transition with a non-existing target state cannot be removed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveTransitions7()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-b->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		GD.removeTransitionFromGraph(gr, grAnother.findVertex("A"), "b", grAnother.findVertex("T"));
	}

	/** Tests that an empty graph cannot be compressed. */
	@Test
	public final void testRemoveDangling1()
	{
		LearnerGraph gr = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(Configuration.getDefaultConfiguration());
		GD.removeDanglingStates(gr);
		WMethod.checkM(gr, expected);
	}

	
	/** Tests that dangling states are compressed. */
	@Test
	public final void testRemoveDangling2a()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->T","testRemoveDangling1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C","testRemoveDangling2E"),Configuration.getDefaultConfiguration());
		GD.removeTransitionFromGraph(gr, gr.findVertex("B"), "b", gr.findVertex("T"));
		GD.removeTransitionFromGraph(gr, gr.findVertex("A"), "b", gr.findVertex("D"));
		GD.removeDanglingStates(gr);
		WMethod.checkM(gr, expected);
	}

	/** Tests that dangling states are compressed. */
	@Test
	public final void testRemoveDangling2b()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->T","testRemoveDangling1"),Configuration.getDefaultConfiguration());
		GD.removeTransitionFromGraph(gr, gr.findVertex("B"), "b", gr.findVertex("T"));
		GD.removeTransitionFromGraph(gr, gr.findVertex("A"), "a", gr.findVertex("B"));
		GD.removeTransitionFromGraph(gr, gr.findVertex("B"), "a", gr.findVertex("C"));
		GD.removeTransitionFromGraph(gr, gr.findVertex("A"), "b", gr.findVertex("D"));
		GD.removeDanglingStates(gr);
		Assert.assertEquals(1, gr.getStateNumber());
		Assert.assertNotNull(gr.findVertex("A"));// the initial state
		Assert.assertSame(gr.init, gr.findVertex("A"));
	}

	/** Tests that a state with a transition leading to it is not compressed. */
	@Test
	public final void testRemoveDangling3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->T","testRemoveDangling1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->T","testRemoveDangling1"),Configuration.getDefaultConfiguration());
		GD.removeDanglingStates(gr);
		WMethod.checkM(gr, expected);
	}
	
	private final Document createDoc()
	{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		Document doc = null;
		try
		{
			factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
		}
		catch(Exception ex)
		{
			Configuration.throwUnchecked("configuration exception: ",ex);
		}
		
		return doc;
	}
	
	@Test
	public final void testEmpty()
	{
		try
		{
			GD.getGraphElement(createDoc().createElement("junk"),"whatever");
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException e)
		{
			Assert.assertTrue(e.getMessage().contains("unexpected element"));
		}
	}

	@Test
	public final void testNoElem()
	{
		try
		{
			GD.getGraphElement(createDoc().createElement(GD.gdGD),"whatever");
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException e)
		{
			Assert.assertTrue(e.getMessage().contains("no element"));
		}
	}

	private static final String graphholder = "graphholder";
	
	@Test
	public final void testElemNoGraph1()
	{
		try
		{
			Document doc = createDoc();
			Element top = doc.createElement(GD.gdGD),subElement=doc.createElement(graphholder);
			top.appendChild(subElement);
			GD.getGraphElement(top,graphholder);
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException e)
		{
			Assert.assertTrue(e.getMessage().contains("no graph"));
		}
	}

	@Test
	public final void testElemNoGraph2()
	{
		try
		{
			Document doc = createDoc();
			Element top = doc.createElement(GD.gdGD),subElement=doc.createElement(graphholder);
			top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
			GD.getGraphElement(top,graphholder);
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException e)
		{
			Assert.assertTrue(e.getMessage().contains("no graph"));
		}
	}

	@Test
	public final void testElemGraph()
	{
		Document doc = createDoc();
		Element top = doc.createElement(GD.gdGD),subElement=doc.createElement(graphholder);
		top.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(doc.createTextNode("junk"));
		Element graphElem = doc.createElement("mygraph");
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(graphElem);
		Assert.assertSame(graphElem,GD.getGraphElement(top,graphholder));
	}

	@Test
	public final void testElemDuplicateGraph()
	{
		try
		{
			Document doc = createDoc();
			Element top = doc.createElement(GD.gdGD),subElement=doc.createElement(graphholder);
			Element graphElem = doc.createElement("mygraph");
			top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
			subElement.appendChild(graphElem);subElement.appendChild(doc.createTextNode("junk"));
			subElement.appendChild(doc.createElement("anotherelement"));
			subElement.appendChild(doc.createTextNode("junk"));
			GD.getGraphElement(top,graphholder);
		}
		catch(IllegalArgumentException e)
		{
			Assert.assertTrue(e.getMessage().contains("more than one graph"));
		}
	}

	@Test
	public final void testElemDuplicateHolder()
	{
		try
		{
			Document doc = createDoc();
			Element top = doc.createElement(GD.gdGD),subElement=doc.createElement(graphholder);
			Element graphElem = doc.createElement("mygraph");
			top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
			subElement.appendChild(graphElem);
			top.appendChild(doc.createElement("smth"));
			GD.getGraphElement(top,graphholder);
		}
		catch(IllegalArgumentException e)
		{
			Assert.assertTrue(e.getMessage().contains("duplicate holder"));
		}
	}

	@Test
	public final void testWriteAndLoad1()
	{
		GD gd = new GD();
		
		LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		gd.removed = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		gd.added = new LearnerGraph(buildGraph("A-d-#E\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());

		GD.applyGD(graph, gd.writeGD(createDoc()));
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#E\nA-c->A","testWriteAndLoad1"),Configuration.getDefaultConfiguration());
		WMethod.checkM(graph, expected);
	}

	@Test
	public final void testWriteAndLoad2()
	{
		GD gd = new GD();
		
		LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		gd.removed = new LearnerGraph(buildGraph("A-d-#D\nA-a->B\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		gd.added = new LearnerGraph(buildGraph("A-d-#E\nB-a-#C\nB-c->B\nA-q->B","testRemoveTransitions1"),Configuration.getDefaultConfiguration());

		GD.applyGD(graph, gd.writeGD(createDoc()));
		LearnerGraph expected = new LearnerGraph(buildGraph("A-q->B-a-#C\nA-d-#E\nA-c->A\nB-c->B","testWriteAndLoad1"),Configuration.getDefaultConfiguration());
		WMethod.checkM(graph, expected);
	}
	
	@Test
	public final void testInit()
	{
		LearnerGraph graphA = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C","testFindKeyPairs1A"),Configuration.getDefaultConfiguration());
		LearnerGraph graphB = new LearnerGraph(buildGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C","testFindKeyPairs1B"),Configuration.getDefaultConfiguration());

		GD gd = new GD();
		int threads = 1;
		gd.init(graphA, graphB, threads);
		Assert.assertEquals(graphA.transitionMatrix.size(),gd.statesOfA.size());
		Assert.assertEquals(graphB.transitionMatrix.size(),gd.statesOfB.size());
		Assert.assertEquals(graphA.transitionMatrix.size()+graphB.transitionMatrix.size(),gd.newToOrig.size());
		for(CmpVertex v:gd.statesOfB) Assert.assertTrue(graphB.transitionMatrix.containsKey(gd.newToOrig.get(v)));
	}
	
	/** Displays the supplied list of pairs, converting names of states
	 * from the notation of the combined graph to their original names. 
	 * 
	 * @param gd
	 * @param listOfPairs
	 */
	protected static void printListOfPairs(GD gd,List<PairScore> listOfPairs)
	{
		System.out.print("[ ");
		for(PairScore pair:listOfPairs) System.out.print(
				new PairScore(gd.newToOrig.get(pair.getQ()),gd.newToOrig.get(pair.getR()),pair.getScore(),pair.getAnotherScore())+" ");
		System.out.println("]");
	}
	
	@Test
	public final void testFindKeyPairs1()
	{// TODO: why does linear generate very high scores backwards?

		LearnerGraph graphA = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C","testFindKeyPairs1A"),Configuration.getDefaultConfiguration());
		LearnerGraph graphB = new LearnerGraph(buildGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C","testFindKeyPairs1B"),Configuration.getDefaultConfiguration());

		GD gd = new GD();
		int threads = 1;
		gd.init(graphA, graphB, threads);
		Assert.assertTrue(gd.identifyKeyPairs());
		for(PairScore pair:gd.frontWave)
		{
			CmpVertex A=gd.newToOrig.get(pair.getQ()), B=gd.newToOrig.get(pair.getR());
			Assert.assertTrue(B.getID().toString().startsWith("@"));
			Assert.assertEquals(B.getID().toString(),"@"+A.getID().toString());
		}
		//printListOfPairs(gd,gd.currentWave);
		//printListOfPairs(gd,gd.frontWave);
	}
	
	@Test
	public final void testFindKeyPairs2()
	{// TODO: why does A get paired with @B?
		LearnerGraph graphA = new LearnerGraph(buildGraph("A-a->B-a->C-a->D-a->A","testFindKeyPairs2A"),Configuration.getDefaultConfiguration());
		LearnerGraph graphB = new LearnerGraph(buildGraph("@A-a->@B-a->@C-a->@A","testFindKeyPairs2B"),Configuration.getDefaultConfiguration());

		GD gd = new GD();
		int threads = 1;
		gd.init(graphA, graphB, threads);
		Assert.assertFalse(gd.identifyKeyPairs());
	}

	@Test
	public final void testMakeSteps1()
	{
		LearnerGraph graphA = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\n"+
				"B-c->B-d->B","testMakeSteps1A"),Configuration.getDefaultConfiguration());
		LearnerGraph graphB = new LearnerGraph(buildGraph("@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C\n"+
				"@B-c->@B-d->@B","testMakeSteps1B"),Configuration.getDefaultConfiguration());

		GD gd = new GD();
		int threads = 1;
		gd.init(graphA, graphB, threads);
		Assert.assertTrue(gd.identifyKeyPairs());
		List<PairScore> allKeyPairs = new LinkedList<PairScore>();
		gd.makeSteps(allKeyPairs);
		//printListOfPairs(gd,allKeyPairs);
		for(PairScore pair:allKeyPairs)
		{
			CmpVertex A=gd.newToOrig.get(pair.getQ()), B=gd.newToOrig.get(pair.getR());
			Assert.assertEquals(B.getID().toString(),A.getID().toString());
		}
	}

	/** Tests GD on the supplied two graphs
	 * 
	 * @param graphA first graph
	 * @param graphB second graph
	 * @param name prefix of the names to give to graphs. 
	 * @param expectedMatchedPairs the number of pairs of states which are expected to be matched
	 */
	private final void testComputeGD(String graphA,String graphB,String name,int expectedMatchedPairs)
	{
		testComputeGD_oneway(graphA, graphB, name, expectedMatchedPairs);
		testComputeGD_oneway(graphB, graphA, name, expectedMatchedPairs);
	}
	
	/** Tests GD on the supplied two graphs
	 * 
	 * @param graphA first graph
	 * @param graphB second graph
	 * @param name prefix of the names to give to graphs. 
	 * @param expectedMatchedPairs the number of pairs of states which are expected to be matched
	 */
	private final void testComputeGD_oneway(String graphA,String graphB,String name, int expectedMatchedPairs)
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph grA = new LearnerGraph(buildGraph(graphA,name+"A"),config);
		LearnerGraph grB = new LearnerGraph(buildGraph(graphB,name+"B"),config);

		GD gd = new GD();
		int threads = 1;
		gd.init(grA, grB, threads);
		gd.identifyKeyPairs();
		List<PairScore> allKeyPairs = new LinkedList<PairScore>();
		gd.makeSteps(allKeyPairs);
		//printListOfPairs(gd, allKeyPairs);
		Assert.assertEquals(expectedMatchedPairs,allKeyPairs.size());
		LearnerGraph graph = new LearnerGraph(buildGraph(graphA,name+"A"),config);
		GD.applyGD(graph, gd.writeGD(createDoc()));
		WMethod.checkM(graph, grB);
	}
	
	@Test
	public final void testComputeGD0()
	{
		testComputeGD(
				"A-a->B",
				"@A-a->@B","testMakeSteps0",2);
	}

	@Test
	public final void testComputeGD1()
	{
		testComputeGD(
				"A-a->B\nA-b->B",
				"@A-a->@B\n@A-c->@B","testMakeSteps1",2);
	}
	@Test
	public final void testComputeGD2()
	{
		testComputeGD(
				"A-a->B-a->C-a->D-a->A",
				"@A-a->@B-a->@C-a->@A","testComputeGD2",3);
	}
	
	@Test
	public final void testComputeGD3()
	{
		testComputeGD(
				"A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\n"+
				"B-c->B-d->B",
				"@A-a->@B\n@A-d-#@D\n@A-c->@A\n@B-b->@E-a-#@C"+"\n@B-a->@F-b->@G-c-#@C\n"+
				"@B-c->@B-d->@B","testMakeSteps1",5);
	}

	@Test
	public final void testComputeGD4()
	{
		testComputeGD(
				"A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->E-a-#C\nT-c-#C",
				"A-a->B-a-#C\nA-d-#D\nA-c->A\nB-b->F-b->G-c-#C","testComputeGD4",6);
	}
}
