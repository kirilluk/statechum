package statechum.analysis.learning;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.BeforeClass;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class TestRpniLearner {

	@Test
	public void testPTAconstruction1() // only two traces, both accept
	{
		RPNIBlueFringeLearner l=new RPNIBlueFringeLearner(null);
		Set<List<String>> plusStrings = new HashSet<List<String>>();
		plusStrings.add(Arrays.asList(new String[]{"a","b","c"}));
		plusStrings.add(Arrays.asList(new String[]{"a","d","c"}));
		DirectedSparseGraph g = l.augmentPTA(RPNIBlueFringeLearner.initialise(), plusStrings, true);
		RPNIBlueFringeLearner.numberVertices(g);
		updateFrame(g);
		TestFSMAlgo.checkM(g,"A-a->B--b->C-c->End1\nB--d->C2-c->End2");
	}

	@Test
	public void testPTAconstruction2()// two accept traces and one reject one
	{
		RPNIBlueFringeLearner l=new RPNIBlueFringeLearner(null);
		Set<List<String>> plusStrings = new HashSet<List<String>>();
		plusStrings.add(Arrays.asList(new String[]{"a","b","c"}));
		plusStrings.add(Arrays.asList(new String[]{"a","d","c"}));
		Set<List<String>> minusStrings = new HashSet<List<String>>();
		minusStrings.add(Arrays.asList(new String[]{"a","b","c","d"}));
		DirectedSparseGraph g = l.createAugmentedPTA(RPNIBlueFringeLearner.initialise(), plusStrings, minusStrings);
		RPNIBlueFringeLearner.numberVertices(g);
		updateFrame(g);
		TestFSMAlgo.checkM(g,"A-a->B--b->C-c->End1-d-#REJ\nB--d->C2-c->End2");
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
}
