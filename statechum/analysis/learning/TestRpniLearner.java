package statechum.analysis.learning;

import static org.junit.Assert.assertTrue;

import java.awt.Point;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.SwingUtilities;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class TestRpniLearner 
{
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
	public void testPTAconstruction1() // only two traces, both accept
	{
		RPNIBlueFringeLearner l=new RPNIBlueFringeLearner(null);
		Set<List<String>> plusStrings = buildSet(new String[][] { new String[] {"a","b","c"},new String[]{"a","d","c"} });
		DirectedSparseGraph g = l.augmentPTA(RPNIBlueFringeLearner.initialise(), plusStrings, true);
		RPNIBlueFringeLearner.numberVertices(g);
		updateFrame(g);
		TestFSMAlgo.checkM(g,"A-a->B--b->C-c->End1\nB--d->C2-c->End2");
	}

	@Test
	public void testPTAconstruction2()// two accept traces and one reject one
	{
		RPNIBlueFringeLearner l=new RPNIBlueFringeLearner(null);
		Set<List<String>> plusStrings = buildSet(new String[][] { new String[]{"a","b","c"}, new String[]{"a","d","c"}});
		Set<List<String>> minusStrings = buildSet(new String[][] { new String[]{"a","b","c","d"} });
		DirectedSparseGraph g = l.createAugmentedPTA(RPNIBlueFringeLearner.initialise(), plusStrings, minusStrings);
		RPNIBlueFringeLearner.numberVertices(g);
		updateFrame(g);
		TestFSMAlgo.checkM(g,"A-a->B--b->C-c->End1-d-#REJ\nB--d->C2-c->End2");
	}

	protected static void checkLearner(String fsmString, String [][] plus, String [][] minus, int threshold)
	{
		final Map<String,Map<String,String>> expectedTrans = new HashMap<String,Map<String,String>>();
		final Map<String,Boolean> expectedAccept = new HashMap<String,Boolean>();
		DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsmString, "sample FSM");
		Visualiser v=new Visualiser();v.update(null, g);
		Point newLoc = visFrame.getLocation();newLoc.move(0, visFrame.getHeight());v.setLocation(newLoc);
		final String expectedInit = TestFSMAlgo.getGraphData(g, expectedTrans, expectedAccept);
		
		RPNIBlueFringeLearnerTestComponent l = new RPNIBlueFringeLearnerTestComponent(visFrame)
		{
			protected int checkWithEndUser(List<String> question, final Object [] moreOptions)
			{
				return TestFSMAlgo.tracePath(expectedInit, expectedTrans, expectedAccept, question);
			}
		};
		l.addObserver(visFrame);
		try{
		DirectedSparseGraph learningOutcome = l.learnMachine(RPNIBlueFringeLearner.initialise(), buildSet(plus), buildSet(minus), threshold);
		TestFSMAlgo.checkM(
				learningOutcome,
				fsmString);
		}
		catch(InterruptedException e){return;};
	}
	
	@Test
	public void testLearner1()
	{
		checkLearner("A-a->B<-a-C-b->A\nA-b->C",new String[][]{new String[]{"b","b","a"},new String[]{"b","a"}}, new String[][]{},1);
	}
	
	
	/** Holds the JFrame to see the graphs being dealt with. Usage:
	 * <pre>
	 * 		updateFrame(g);// a public method
	 * </pre>
	 * where <i>g</i> is the graph to be displayed.
	 */
	protected static Visualiser visFrame = null;
	
	/** Displays the graph passed as an argument in the Jung window.
	 * @param g the graph to display 
	 */
	public void updateFrame(DirectedSparseGraph g)
	{
		visFrame.update(null, g);
	}

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
