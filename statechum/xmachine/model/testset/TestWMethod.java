/**
 * 
 */
package statechum.xmachine.model.testset;

import static org.junit.Assert.*;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import javax.swing.SwingUtilities;

import junit.framework.Assert;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import samples.preview_new_graphdraw.iter.UpdatableIterableLayout;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.WMethod.EquivalentStatesException;
import static statechum.analysis.learning.TestFSMAlgo.buildGraph;
import static statechum.analysis.learning.TestFSMAlgo.buildSet;
import static statechum.xmachine.model.testset.WMethod.getGraphData;
import static statechum.xmachine.model.testset.WMethod.isPrefix;
import static statechum.xmachine.model.testset.WMethod.cross;

/**
 * @author kirr
 *
 */
public class TestWMethod {


	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover1() {
		Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"b"},new String[]{"b","a"}}),
			actual = WMethod.computeStateCover(getGraphData(buildGraph("A-a->A-b->B-c->B-a->C\nQ-d->S","computeStateCover1")));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover2() {
		Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"b"},new String[]{"b","a"}}),
			actual = WMethod.computeStateCover(getGraphData(buildGraph("A-d->A-b->B-c->B-a->C\nQ-d->S","computeStateCover2")));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover3() {
		Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"b"},new String[]{"d"},new String[]{"b","a"}}),
			actual = WMethod.computeStateCover(getGraphData(buildGraph("A-a->A\nD<-d-A-b->B-c->B-a->C\nQ-d->S","computeStateCover3")));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getTransitionCover()}.
	 */
	@Test
	public final void computeStateCover4() {
		Set<List<String>> expected = TestFSMAlgo.buildSet(new String[][]{new String[]{},new String[]{"a"},new String[]{"b"},new String[]{"d"},new String[]{"b","a"},new String[]{"b","a","a"}}),
			actual = WMethod.computeStateCover(getGraphData(buildGraph("A-a->S\nD<-d-A-b->B-c->B-a->C-a->Q\nQ-d->S","computeStateCover3")));
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix0() {
		Assert.assertTrue(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{})));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix1() {
		Assert.assertTrue(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{"a"})));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix2() {
		Assert.assertFalse(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{"a","b","c","d"})));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#isPrefix(java.util.List, java.util.List)}.
	 */
	@Test
	public final void testIsPrefix3() {
		Assert.assertTrue(isPrefix(Arrays.asList(new String[]{"a","b","c"}),Arrays.asList(new String[]{"a","b","c"})));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross1a() {
		Set<List<String>> actual = cross(buildSet(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}), 
				buildSet(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}})),
				expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"},
						new String[]{"z","x","c","d"},
						new String[]{"z","x","q","w"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross1b() {
		Set<List<String>> actual = cross(buildSet(new String[][] {new String[]{},new String[]{"a","b"},new String[]{"z","x"}}), 
				buildSet(new String[][] {new String[]{},new String[]{"c","d"}, new String[]{"q","w"}})),
				expected = buildSet(new String[][] {
						new String[]{},
						new String[]{"a","b"},new String[]{"z","x"},
						new String[]{"c","d"},new String[]{"q","w"},
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"},
						new String[]{"z","x","c","d"},
						new String[]{"z","x","q","w"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross2() {
		Set<List<String>> actual = cross(buildSet(new String[][] {new String[]{"a","b"}}), 
				buildSet(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}})),
				expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"a","b","q","w"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross3() {
		Set<List<String>> actual = cross(buildSet(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}), 
				buildSet(new String[][] {new String[]{"c","d"}})),
				expected = buildSet(new String[][] {
						new String[]{"a","b","c","d"},
						new String[]{"z","x","c","d"}});
		Assert.assertTrue(expected.equals(actual));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross4() {
		Set<List<String>> actual = cross(buildSet(new String[][] {}), 
				buildSet(new String[][] {new String[]{"c","d"}, new String[]{"q","w"}}));
				
		Assert.assertTrue(actual.isEmpty());		
	}				

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#cross(java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testCross5() {
		Set<List<String>> actual = cross(buildSet(new String[][] {new String[]{"a","b"},new String[]{"z","x"}}), 
				buildSet(new String[][] {}));
				
		Assert.assertTrue(actual.isEmpty());		
	}
	
	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getStimuli}.
	 */
	@Test
	public final void testGetStimuli1() {
		Assert.assertTrue(WMethod.makeSingleton(WMethod.computeAlphabet(new DirectedSparseGraph())).isEmpty());
	}	

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#getStimuli}.
	 */
	@Test
	public final void testGetStimuli2() {
		Assert.assertTrue(WMethod.makeSingleton(WMethod.computeAlphabet(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testGetStimuli2")))
				.equals(buildSet(new String[][]{
						new String[]{"p"},
						new String[]{"d"},
						new String[]{"b"},
						new String[]{"c"},
						new String[]{"a"}
				})));
	}
	
	@Test
	public final void testTruncateSeq1()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq1"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","b"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","b"})));
	}

	@Test
	public final void testTruncateSeq2()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq2"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","c"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","b","c"})));
	}
	
	@Test
	public final void testTruncateSeq3()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq3"));
		Assert.assertEquals(Arrays.asList(new String[]{}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{})));
	}

	@Test
	public final void testTruncateSeq4()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq4"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","z"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","b","z","e"})));
	}

	@Test
	public final void testTruncateSeq5()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq5"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","d"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","b","d"})));
	}

	@Test
	public final void testTruncateSeq5b()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-f->S","testTruncateSeq5b"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","b","d"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","b","d"})));
	}

	@Test
	public final void testTruncateSeq6()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq6"));
		Assert.assertEquals(Arrays.asList(new String[]{"a"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"a"})));
	}
	
	@Test
	public final void testTruncateSeq7()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testTruncateSeq7"));
		Assert.assertEquals(Arrays.asList(new String[]{"p","p","p"}), WMethod.truncateSequence(fsm, Arrays.asList(new String[]{"p","p","p"})));
	}
	
	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence0() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence0"));
		Set<List<String>> sequences = buildSet(new String[][]{ 
			}),
		expected = buildSet(new String[][]{
				new String[]{}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{}));
		Assert.assertTrue(expected.equals(sequences));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence0b() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence0b"));
		Set<List<String>> sequences = Collections.unmodifiableSet(buildSet(new String[][]{ 
				new String[]{} 
			})),
		expected = buildSet(new String[][]{
				new String[]{}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{}));
		Assert.assertTrue(expected.equals(sequences));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence1() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence1"));
		Set<List<String>> sequences = Collections.unmodifiableSet(buildSet(new String[][]{ 
				new String[]{"p","b"} 
			})),
		expected = buildSet(new String[][]{
				new String[]{"p","b"}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{}));
		Assert.assertTrue(expected.equals(sequences));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence2() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence2"));
		Set<List<String>> sequences = Collections.unmodifiableSet(buildSet(new String[][]{ 
				new String[]{"p","b"} 
			})),
		expected = buildSet(new String[][]{
				new String[]{"p","b"}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p","b"}));
		Assert.assertTrue(expected.equals(sequences));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence3() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence3"));
		Set<List<String>> sequences = buildSet(new String[][]{ 
				new String[]{"p","b"} 
			}),
		expected = buildSet(new String[][]{
				new String[]{"p","b","c"}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p","b","c"}));
		Assert.assertTrue(expected.equals(sequences));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence4() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence4"));
		Set<List<String>> sequences = buildSet(new String[][]{ 
				new String[]{"p","b"} 
			}),
		expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"p","p"}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p","p"}));
		Assert.assertTrue(expected.equals(sequences));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendSequence(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.List)}.
	 */
	@Test
	public final void testAppendSequence5() 
	{
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendSequence5"));
		Set<List<String>> sequences = buildSet(new String[][]{ 
				new String[]{"p","b"} 
			}),
		expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"p","p","p"}
			});
		
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p"}));
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p","p"}));
		WMethod.appendSequence(fsm, sequences, Arrays.asList(new String[]{"p","p","p"}));
		Assert.assertTrue(expected.equals(sequences));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences0() {
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences0"));
		Set<List<String>> sequences = buildSet(new String[][]{ 
			}),
		expected = buildSet(new String[][]{
				new String[]{"p","p","p"},
				new String[]{"g"}
			});
		
		WMethod.appendAllSequences(fsm, sequences, buildSet(new String[][] {
				new String[]{"p"},new String[]{"p","p"},new String[] {}, new String [] {"g"},new String[]{"p","p","p"}}));
		Assert.assertTrue(expected.equals(sequences));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences0b() {
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences0b"));
		Set<List<String>> sequences = buildSet(new String[][]{ 
			}),
		expected = buildSet(new String[][]{
			});
		
		WMethod.appendAllSequences(fsm, sequences, buildSet(new String[][] {}));
		Assert.assertTrue(expected.equals(sequences));
	}

	/**
	 * Test method for {@link statechum.xmachine.model.testset.WMethod#appendAllSequences(statechum.analysis.learning.TestFSMAlgo.FSMStructure, java.util.Set, java.util.Set)}.
	 */
	@Test
	public final void testAppendAllSequences1() {
		FSMStructure fsm = getGraphData(buildGraph("A-p->A-b->B-c->B-a->C\nQ-d->S","testAppendAllSequences1"));
		Set<List<String>> sequences = buildSet(new String[][]{ 
				new String[]{"p","b"} 
			}),
		expected = buildSet(new String[][]{
				new String[]{"p","b"},
				new String[]{"g"},
				new String[]{"p","p","p"}
			});
		
		WMethod.appendAllSequences(fsm, sequences, buildSet(new String[][] {
				new String[]{"p"},new String[]{"p","p"},new String[] {}, new String [] {"g"},new String[]{"p","p","p"}}));
		Assert.assertTrue(expected.equals(sequences));
	}

	public static void testWsetconstruction(String machine)
	{
		DirectedSparseGraph g = buildGraph(machine,"testWset");
		FSMStructure fsm = getGraphData(g);visFrame.update(null, g);
		try
		{
			Set<List<String>> wset = WMethod.computeWSet(fsm);
			for(Entry<String,Boolean> stateA:fsm.accept.entrySet())
			{
				for(Entry<String,Boolean> stateB:fsm.accept.entrySet())
					if (!stateA.getKey().equals(stateB.getKey()))
					{
						boolean foundString = false;
						Iterator<List<String>> pathIt = wset.iterator();
						while(pathIt.hasNext() && !foundString)
						{
							List<String> path = pathIt.next();
							int aResult = WMethod.tracePath(fsm, path, stateA.getKey()),
								bResult = WMethod.tracePath(fsm, path, stateB.getKey());
							
							if ( (aResult == RPNIBlueFringeLearner.USER_ACCEPTED && bResult >= 0) ||
									(bResult == RPNIBlueFringeLearner.USER_ACCEPTED && aResult >= 0))
								foundString = true;
						}
						
						if (!foundString)
							fail("W set does not distinguish between "+stateA.getKey()+" and "+stateB.getKey());
					}
			}
		}
		catch(EquivalentStatesException e)
		{
			TestFSMAlgo.checkM(fsm,fsm,e.getA(),e.getB());
		}
		
	}
	
	@Test
	public final void testWset1()
	{
		testWsetconstruction("A-p->A-b->B-c->B-a->C");
	}
	
	@Test
	public final void testWset2()
	{
		testWsetconstruction("A-a->C-b->Q\nB-a->D-a->Q");
	}
	
	@Test
	public final void testWset3() // equivalent states
	{
		testWsetconstruction("A-a->C-b->Q\nB-a->D-b->Q");
	}

	@Test
	public final void testWset4()
	{
		FSMStructure fsm = getGraphData(buildGraph("A-a->A","testWset"));
		Assert.assertTrue(WMethod.computeWSet(fsm).isEmpty());
	}

	@Test
	public final void testWset5a() // equivalent states
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3");
	}
	
	@Test
	public final void testWset5b() // equivalent states
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3");
	}
	
	@Test
	public final void testWset6() // no equivalent states
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-m->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-z->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3");
	}
	
	/** Holds the JFrame to see the graphs being dealt with. Usage:
	 * <pre>
	 * 		updateFrame(g);// a public method
	 * </pre>
	 * where <i>g</i> is the graph to be displayed.
	 */
	protected static Visualiser visFrame = null;
	
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{
		visFrame = new Visualiser();
	}

	@AfterClass
	public static void cleanUp()
	{
		try {
			SwingUtilities.invokeAndWait(new Runnable() 
			{
				public void run()
				{
					visFrame.setVisible(false);
					visFrame.dispose();
				}
			});
		} catch (InterruptedException e) {
			// cannot do anything with this
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// cannot do anything with this
			e.printStackTrace();
		}
	}	
}