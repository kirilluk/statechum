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

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildGraph;

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
import statechum.analysis.learning.rpnicore.GD.ChangesRecorder;
import statechum.analysis.learning.rpnicore.GD.LearnerGraphMutator;
import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;

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
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "c", grAnother.findVertex("C"));
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
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "c", grAnother.findVertex("C"));
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
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "c", grAnother.findVertex("C"));
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
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "c", grAnother.findVertex("C"));
	}
	
	/** Tests that inconsistent acceptance conditions are detected. */
	@Test(expected=IllegalArgumentException.class)
	public final void testAddTransitions5()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("C-d->A","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("C"), "c", grAnother.findVertex("A"));
	}
	
	/** Tests that I cannot replace an existing transition. */
	@Test(expected=IllegalArgumentException.class)
	public final void testAddTransitions6()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("T"), "a", grAnother.findVertex("C"));
	}
	
	/** Tests that supplied states are cloned when I ask them to be cloned. */
	@Test
	public final void testAddTransitions7()
	{
		LearnerGraph gr = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d-#C","testAddTransitions0A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("\nB-d-#C","testAddTransitions0E"),Configuration.getDefaultConfiguration());
		cloneConfig.setLearnerCloneGraph(true);
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "c", grAnother.findVertex("C"));
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
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "d", grAnother.findVertex("C"));
		WMethod.checkM(gr, expected);
	}

	/** Adding a transition from an initial state. */
	@Test
	public final void testAddTransitionsG2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#C","testAddTransitionsG2A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D"+"\nA-d-#C","testAddTransitionsG2E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("A"), "d", grAnother.findVertex("C"));
		WMethod.checkM(gr, expected);
	}
	
	/** Adding a transition to an initial state. */
	@Test
	public final void testAddTransitionsG3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("B-d->A","testAddTransitionsG3A"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D"+"\nB-d->A","testAddTransitionsG3E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.addTransition(grAnother.findVertex("B"), "d", grAnother.findVertex("A"));
		WMethod.checkM(gr, expected);
	}
	
	@Test
	public final void testAddInit1()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grB = new LearnerGraph(buildGraph("C-b->T","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		gr.init = null;
		patcher.setInitial(grB.findVertex("T"));Assert.assertSame(gr.findVertex("T"),gr.init);
	}
	
	@Test
	public final void testAddInit2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grB = new LearnerGraph(buildGraph("T-u-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		gr.init = null;
		patcher.setInitial(grB.findVertex("C"));Assert.assertSame(gr.findVertex("C"),gr.init);
	}
	
	@Test
	public final void testAddInit3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grB = new LearnerGraph(buildGraph("C-b-#T\nC-c->Q","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		gr.init = null;
		patcher.setInitial(grB.findVertex("Q"));Assert.assertSame(gr.findVertex("Q"),gr.init);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testAddInit4()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("T-a-#C","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraph grB = new LearnerGraph(buildGraph("C-b-#T\nC-c->Q","testAddTransitions0B"), Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		gr.init = null;
		patcher.setInitial(grB.findVertex("T"));
	}
	
	@Test
	public final void testRemoveTransitions1()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B\nA-b-#D","testRemoveTransitions1E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("B"), "a", grAnother.findVertex("C"));
		WMethod.checkM(gr, expected);
	}
	
	/** remove a transition from an initial state. */
	@Test
	public final void testRemoveTransitions2()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C","testRemoveTransitions2E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("A"), "b", grAnother.findVertex("D"));
		WMethod.checkM(gr, expected);
	}
	
	/** remove a transition to an initial state. */
	@Test
	public final void testRemoveTransitions3()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->A","testAddTransitions3"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("B"), "b", grAnother.findVertex("A"));
		WMethod.checkM(gr, expected);
	}
	
	/** remove a loop around an initial state. */
	@Test
	public final void testRemoveTransitions4()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-u->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D","testAddTransitionsG1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("A"), "u", grAnother.findVertex("A"));
		WMethod.checkM(gr, expected);
	}
	
	/** Tests that a transition with a non-existing source state cannot be removed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveTransitions5()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-b->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("T"), "b", grAnother.findVertex("A"));
	}

	/** Tests that a transition with a non-existing label cannot be removed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveTransitions6()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-b->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("A"), "c", grAnother.findVertex("A"));
	}

	/** Tests that a transition with a non-existing target state cannot be removed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testRemoveTransitions7()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nA-b->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph grAnother = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.removeTransition(grAnother.findVertex("A"), "b", grAnother.findVertex("T"));
	}

	/** Tests that an empty graph cannot be compressed. */
	@Test
	public final void testRemoveDangling1()
	{
		LearnerGraph gr = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.removeDanglingStates();
		WMethod.checkM(gr, expected);
	}

	
	/** Tests that dangling states are compressed. */
	@Test
	public final void testRemoveDangling2a()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->T","testRemoveDangling1"),Configuration.getDefaultConfiguration());
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C","testRemoveDangling2E"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.removeTransition(gr.findVertex("B"), "b", gr.findVertex("T"));
		patcher.removeTransition(gr.findVertex("A"), "b", gr.findVertex("D"));
		patcher.removeDanglingStates();
		WMethod.checkM(gr, expected);
	}

	/** Tests that dangling states are compressed. */
	@Test
	public final void testRemoveDangling2b()
	{
		LearnerGraph gr = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-b-#D\nB-b->T","testRemoveDangling1"),Configuration.getDefaultConfiguration());
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.removeTransition(gr.findVertex("B"), "b", gr.findVertex("T"));
		patcher.removeTransition(gr.findVertex("A"), "a", gr.findVertex("B"));
		patcher.removeTransition(gr.findVertex("B"), "a", gr.findVertex("C"));
		patcher.removeTransition(gr.findVertex("A"), "b", gr.findVertex("D"));
		patcher.removeDanglingStates();
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
		LearnerGraphMutator patcher = new LearnerGraphMutator(gr, cloneConfig,null);
		patcher.removeDanglingStates();
		WMethod.checkM(gr, expected);
	}
	
	protected static final Document createDoc()
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
			statechum.Helper.throwUnchecked("configuration exception: ",ex);
		}
		
		return doc;
	}
	
	@Test
	public final void testEmpty()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			GD.ChangesRecorder.getGraphElement(createDoc().createElement("junk"),"whatever");
		}},IllegalArgumentException.class,"unexpected element");
	}

	@Test
	public final void testNoElem()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			GD.ChangesRecorder.getGraphElement(createDoc().createElement(GD.ChangesRecorder.gdGD),"whatever");
		}},IllegalArgumentException.class,"no element");
	}

	private static final String graphholder = "graphholder";
	
	@Test
	public final void testElemNoGraph1()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			Document doc = createDoc();
			Element top = doc.createElement(GD.ChangesRecorder.gdGD),subElement=doc.createElement(graphholder);
			top.appendChild(subElement);
			GD.ChangesRecorder.getGraphElement(top,graphholder);
		}},IllegalArgumentException.class,"no graph");
	}

	@Test
	public final void testElemNoGraph2()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			Document doc = createDoc();
			Element top = doc.createElement(GD.ChangesRecorder.gdGD),subElement=doc.createElement(graphholder);
			top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
			GD.ChangesRecorder.getGraphElement(top,graphholder);
		}},IllegalArgumentException.class,"no graph");
	}

	@Test
	public final void testElemGraph()
	{
		Document doc = createDoc();
		Element top = doc.createElement(GD.ChangesRecorder.gdGD),subElement=doc.createElement(graphholder);
		top.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(doc.createTextNode("junk"));
		Element graphElem = doc.createElement("mygraph");
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(graphElem);
		Assert.assertSame(graphElem,GD.ChangesRecorder.getGraphElement(top,graphholder));
	}

	@Test
	public final void testElemDuplicateGraph()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			Document doc = createDoc();
			Element top = doc.createElement(GD.ChangesRecorder.gdGD),subElement=doc.createElement(graphholder);
			Element graphElem = doc.createElement("mygraph");
			top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
			subElement.appendChild(graphElem);subElement.appendChild(doc.createTextNode("junk"));
			subElement.appendChild(doc.createElement("anotherelement"));
			subElement.appendChild(doc.createTextNode("junk"));
			GD.ChangesRecorder.getGraphElement(top,graphholder);
		}},IllegalArgumentException.class,"more than one graph");
	}

	@Test
	public final void testElemJunkElementInGD()
	{
		Document doc = createDoc();
		Element top = doc.createElement(GD.ChangesRecorder.gdGD),subElement=doc.createElement(graphholder);
		Element graphElem = doc.createElement("mygraph");
		top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
		subElement.appendChild(graphElem);
		top.appendChild(doc.createElement("smth"));
		Assert.assertSame(graphElem,GD.ChangesRecorder.getGraphElement(top,graphholder));
	}
	
	@Test
	public final void testElemDuplicateHolder()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			Document doc = createDoc();
			Element top = doc.createElement(GD.ChangesRecorder.gdGD),subElement=doc.createElement(graphholder);
			Element graphElem = doc.createElement("mygraph");
			top.appendChild(subElement);subElement.appendChild(doc.createTextNode("junk"));
			subElement.appendChild(graphElem);
			top.appendChild(doc.createElement(graphholder));
			GD.ChangesRecorder.getGraphElement(top,graphholder);
		}},IllegalArgumentException.class,"duplicate holder");
	}

	@Test
	public final void testWriteAndLoad1()
	{
		LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph removed = new LearnerGraph(buildGraph("A-d-#D\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph added = new LearnerGraph(buildGraph("A-d-#E\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		ChangesRecorder patcher = new ChangesRecorder(removed,added,null);

		ChangesRecorder.applyGD(graph, patcher.writeGD(createDoc()));
		LearnerGraph expected = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#E\nA-c->A","testWriteAndLoad1"),Configuration.getDefaultConfiguration());
		WMethod.checkM(graph, expected);
	}

	@Test
	public final void testWriteAndLoad2()
	{
		LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		LearnerGraph removed = new LearnerGraph(buildGraph("A-d-#D\nA-a->B\nB-a-#C","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		LearnerGraph added = new LearnerGraph(buildGraph("A-d-#E\nB-a-#C\nB-c->B\nA-q->B","testRemoveTransitions1"),Configuration.getDefaultConfiguration());
		ChangesRecorder patcher = new ChangesRecorder(removed,added,null);

		ChangesRecorder.applyGD(graph, patcher.writeGD(createDoc()));
		LearnerGraph expected = new LearnerGraph(buildGraph("A-q->B-a-#C\nA-d-#E\nA-c->A\nB-c->B","testWriteAndLoad1"),Configuration.getDefaultConfiguration());
		WMethod.checkM(graph, expected);
	}
	
	@Test
	public final void testWriteAndLoad3()
	{
		LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		ChangesRecorder patcher = new ChangesRecorder(null);
		patcher.addTransition(graph.findVertex("B"), "c", graph.findVertex("B"));
		patcher.removeTransition(graph.findVertex("A"), "a", graph.findVertex("B"));
		patcher.addTransition(graph.findVertex("A"), "q", graph.findVertex("B"));
		patcher.setInitial(graph.findVertex("A"));
		ChangesRecorder.applyGD(graph, patcher.writeGD(createDoc()));
		LearnerGraph expected = new LearnerGraph(buildGraph("A-q->B-a-#C\nA-d-#E\nA-c->A\nB-c->B","testWriteAndLoad1"),Configuration.getDefaultConfiguration());
		WMethod.checkM(graph, expected);
	}
	
	/** Tests that initial state has to be assigned. */
	@Test
	public final void testWriteAndLoad4()
	{
		LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		final ChangesRecorder patcher = new ChangesRecorder(null);
		patcher.addTransition(graph.findVertex("B"), "c", graph.findVertex("B"));
		patcher.removeTransition(graph.findVertex("A"), "a", graph.findVertex("B"));
		patcher.addTransition(graph.findVertex("A"), "q", graph.findVertex("B"));
		checkForCorrectException(new whatToRun() { public void run() {
			patcher.writeGD(createDoc());}},
		IllegalArgumentException.class,"init state is was not defined");
	}
	
	
	protected final java.util.Map<CmpVertex,CmpVertex> newToOrig = new java.util.TreeMap<CmpVertex,CmpVertex>();
	
	/** Displays the supplied list of pairs, converting names of states
	 * from the notation of the combined graph to their original names. 
	 * 
	 * @param gd
	 * @param listOfPairs
	 */
	protected static void printListOfPairs(GD gd,List<PairScore> listOfPairs,java.util.Map<CmpVertex,CmpVertex> newToOrig)
	{
		System.out.print("[ ");
		for(PairScore pair:listOfPairs) System.out.print(
				new PairScore(newToOrig.get(pair.getQ()),newToOrig.get(pair.getR()),pair.getScore(),pair.getAnotherScore())+" ");
		System.out.println("]");
	}
	
	@Test
	public final void testSortingOfWaves0()
	{
		List<PairScore> wave = java.util.Arrays.asList(new PairScore[]{}),
		expected= java.util.Arrays.asList(new PairScore[]{});
		GD.sortWave(wave);
		Assert.assertEquals(expected,wave);
	}
	
	@Test
	public final void testSortingOfWaves1()
	{
		LearnerGraph graph = new LearnerGraph(buildGraph("A-a->B-a-#C\nA-d-#D\nA-c->A","testAddTransitions4"),Configuration.getDefaultConfiguration());
		PairScore A=new PairScore(graph.findVertex("A"),graph.findVertex("B"),10,0),
			B=new PairScore(graph.findVertex("B"),graph.findVertex("A"),100,0),
			C=new PairScore(graph.findVertex("A"),graph.findVertex("B"),12,0),
			D=new PairScore(graph.findVertex("B"),graph.findVertex("A"),90,0),
			E=new PairScore(graph.findVertex("A"),graph.findVertex("B"),10,0);

		List<PairScore> wave = java.util.Arrays.asList(new PairScore[]{A,B,C,D,E}),
		expected= java.util.Arrays.asList(new PairScore[]{B,D,C,A,E});
		GD.sortWave(wave);
		Assert.assertEquals(expected,wave);
	}
/*
	@Test
	public final void testComputeGD_big5()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		final String path = "resources/LargeGraphs/";
		LearnerGraph grA = Transform.convertToNumerical(LearnerGraph.loadGraph(path+"experiment_500", config));
		LearnerGraph grB = Transform.convertToNumerical(LearnerGraph.loadGraph(path+"experiment_501", config));
		GD gd = new GD();
		LearnerGraph graph = Transform.convertToNumerical(LearnerGraph.loadGraph(path+"experiment_500", config));
		long tmStarted = new Date().getTime();
		System.out.println("loaded ");
		Element xml = gd.computeGDToXML(grA, grB, 2, createDoc());
		System.out.println("patch created "+ (new Date().getTime()-tmStarted)/1000);
		tmStarted = new Date().getTime();
		ChangesRecorder.applyGD(graph, xml);
		System.out.println("patch applied "+ (new Date().getTime()-tmStarted)/1000);
		WMethod.checkM(graph, grB);Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
*/
}
