package statechum.analysis.learning;

import static org.junit.Assert.assertTrue;

import java.awt.Point;
import java.io.IOException;
import java.io.StringReader;
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
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import samples.preview_new_graphdraw.iter.UpdatableIterableLayout;

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
		updateFrame(g,null);
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
		updateFrame(g,null);
		TestFSMAlgo.checkM(g,"A-a->B--b->C-c->End1-d-#REJ\nB--d->C2-c->End2");
	}

	protected void checkLearner(String fsmString, String [][] plus, String [][] minus, int threshold)
	{
		final Map<String,Map<String,String>> expectedTrans = new HashMap<String,Map<String,String>>();
		final Map<String,Boolean> expectedAccept = new HashMap<String,Boolean>();
		final DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsmString, "sample FSM");
		final String expectedInit = TestFSMAlgo.getGraphData(g, expectedTrans, expectedAccept);
		
		// now sanity checking on the plus and minus sets
		for(String [] path:plus)
			assert RPNIBlueFringeLearner.USER_ACCEPTED == TestFSMAlgo.tracePath(expectedInit, expectedTrans, expectedAccept, Arrays.asList(path));
		for(String [] path:minus)
			assert RPNIBlueFringeLearner.USER_ACCEPTED != TestFSMAlgo.tracePath(expectedInit, expectedTrans, expectedAccept, Arrays.asList(path));
		
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
		updateFrame(learningOutcome,g);
		TestFSMAlgo.checkM(
				learningOutcome,
				fsmString);
		}
		catch(InterruptedException e){return;};
	}
	
	@Test
	public void testLearner1()
	{
		checkLearner("A-a->B<-a-A-b->A\nA-b->A",new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b"}}, new String[][]{},1);
	}
	
	//@Ignore("the learner does not work yet")
	@Test
	public void testLearner2()
	{
		checkLearner("A-a->B<-a-C-b->A\nA-b->C\nC-c->C\n",new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b","c"}}, new String[][]{new String[]{"c"}},1);
	}
	
	
	/** Holds the JFrame to see the graphs being dealt with. Usage:
	 * <pre>
	 * 		updateFrame(g);// a public method
	 * </pre>
	 * where <i>g</i> is the graph to be displayed.
	 */
	protected static Visualiser visFrame = null;
	
	/** Displays twos graphs passed as arguments in the Jung window.
	 * @param g the graph to display 
	 * @param lowerGraph the graph to display below it
	 */
	public void updateFrame(final DirectedSparseGraph g,final DirectedSparseGraph lowerGraph)
	{
		visFrame.update(null, g);
		if (lowerGraph != null)
		{
			try {// I'm assuming here that Swing has only one queue of threads to run on the AWT thread, hence the
				// thread scheduled by invokeLater will be run to completion before the next one (below) runs and hence
				// I rely on the results of execution of the above thread below in order to position the window.
				SwingUtilities.invokeAndWait(new Runnable() 
				{
					public void run()
					{
						Visualiser v=new Visualiser();v.update(null, lowerGraph);
						Point newLoc = visFrame.getLocation();newLoc.move(0, visFrame.getHeight());v.setLocation(newLoc);
					}
				});
			} catch (InterruptedException e) {
				// cannot do much about this
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// cannot do much about this
				e.printStackTrace();
			}
		}
	}


	@Test
	public void testLoadAnswers1() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		Assert.assertEquals(0,sa.getCount());
	}
	
	@Test
	public void testLoadAnswers2() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(""));
		Assert.assertEquals(0,sa.getCount());
	}
	
	@Test
	public void testLoadAnswers3() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(""));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_CANCELLED,sa.getAnswer(Arrays.asList(new String[]{})));
	}

	@Test
	public void testLoadAnswers4() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("[test] <yes>"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}
	
	@Test
	public void testLoadAnswers5A() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader(" \t\t    [test] <no> at position 5, junk"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}

	@Test
	public void testLoadAnswers5B() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("\n\n[test] <no> at position 5, junk\n"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}

	@Test
	public void testLoadAnswers5C() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("  "+RPNIBlueFringeLearner.QUESTION_AUTO+" [test] <no> at position 5, junk"));
		Assert.assertEquals(1,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail1() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("junk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail2() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] junk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail3() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no>"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail4() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no> at position "));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail5() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no> at position 6"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail6() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader("[valid string] <no> at position 7,\njunk"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testLoadAnswersFail7() throws IOException
	{
		new StoredAnswers().setAnswers(new StringReader(" junk\n\n[valid string] <no> at position 7,\n"));
	}
	
	
	@Test
	public void testLoadAnswers6() throws IOException
	{
		StoredAnswers sa = new StoredAnswers();
		sa.setAnswers(new StringReader("[test] <no> at position 5, junk\n "
				+RPNIBlueFringeLearner.QUESTION_AUTO+" [some text, more of it] <yes> whatever\n\n\n"
				+"[teststr, another, more] <no> at position 0, junk\n"				
				+RPNIBlueFringeLearner.QUESTION_AUTO+"[teststr, a, more] <no> at position 2, junk\n"				
				+"[teststr, p, more] <yes> junk\n"				
		));
		Assert.assertEquals(5,sa.getCount());
		Assert.assertEquals(5, sa.getAnswer(Arrays.asList(new String[]{"test"})));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa.getAnswer(Arrays.asList(new String[]{"some text","more of it"})));
		Assert.assertEquals(0, sa.getAnswer(Arrays.asList(new String[]{"teststr","another", "more"})));
		Assert.assertEquals(2, sa.getAnswer(Arrays.asList(new String[]{"teststr","a", "more"})));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_ACCEPTED, sa.getAnswer(Arrays.asList(new String[]{"teststr","p", "more"})));
		Assert.assertEquals(RPNIBlueFringeLearner.USER_CANCELLED, sa.getAnswer(Arrays.asList(new String[]{"unknown","p", "more"})));
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
