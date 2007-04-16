package statechum.analysis.learning;

import java.awt.Point;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.swing.SwingUtilities;

import junit.framework.JUnit4TestAdapter;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.Visualiser;
import statechum.xmachine.model.testset.TestWMethod;

import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class TestGraphGeneration {
	public static final String EDGE = " EDGE";
	public static final String VERTEX = "VERTEX";
	
	private static void addSingletonLabel(DirectedSparseEdge e, String label)
	{
		Set<String> labelSet = new HashSet<String>();labelSet.add(label);
		e.setUserDatum(JUConstants.LABEL, labelSet, UserData.SHARED);		
	}
	
	public static DirectedSparseGraph buildEVGraph(String graphString)
	{
		DirectedSparseGraph g = TestFSMAlgo.buildGraph(graphString, "simpleGraph");
		g.getEdgeConstraints().clear();
		List<Edge> newEdges = new LinkedList();
		for(DirectedSparseEdge e:(Set<DirectedSparseEdge>)g.getEdges())
		{
			Collection<String> labels = (Collection<String>)e.getUserDatum(JUConstants.LABEL);
			e.removeUserDatum(JUConstants.LABEL);
			Iterator<String> labelIt = labels.iterator(); 
			e.addUserDatum(EDGE, labelIt.next(), UserData.SHARED);
			while(labelIt.hasNext())
			{
				DirectedSparseEdge newEdge = new DirectedSparseEdge(e.getSource(),e.getDest());
				newEdge.setUserDatum(EDGE, labelIt.next(), UserData.SHARED);
				newEdges.add(newEdge);
			}
		}
		
		for(Edge e:newEdges)
			g.addEdge(e);
		
		for(DirectedSparseVertex v:(Set<DirectedSparseVertex>)g.getVertices())
		{
			 v.addUserDatum(VERTEX, v.getUserDatum(JUConstants.LABEL), UserData.SHARED);
			 v.removeUserDatum(JUConstants.LABEL);
		}
		
		return g;
	}

	public void checkEquivalence(DirectedSparseGraph g, String expected)
	{
		for(DirectedSparseEdge e:(Set<DirectedSparseEdge>)g.getEdges())
			addSingletonLabel(e, (String)e.getUserDatum(EDGE));
		
		for(DirectedSparseVertex v:(Set<DirectedSparseVertex>)g.getVertices())
		{
			 v.addUserDatum(JUConstants.LABEL, v.getUserDatum(VERTEX), UserData.SHARED);
			 v.removeUserDatum(VERTEX);
		}
		//updateFrame(TestFSMAlgo.buildGraph(expected,"expected"),g);
		TestFSMAlgo.checkM(g,expected);
	}
	
	@Test
	public final void testA()
	{
		String graphString = "S0-i0->S0-i1->S1\nS0-i3->S2\nS1-i0->S0\nS1-i1->S3\nS1-i2->S0";
		DirectedSparseGraph g=buildEVGraph(graphString);
		checkEquivalence(g,graphString);
	}
	
	@Test
	public final void testB()
	{
		String graphString = "A-a->B\nA-b->B-a->A-c->D";
		DirectedSparseGraph g=buildEVGraph(graphString);
		checkEquivalence(g,graphString);
	}

	/** Displays twos graphs passed as arguments in the Jung window.
	 * @param g the graph to display 
	 * @param lowerGraph the graph to display below it
	 */
	public void updateFrame(final DirectedSparseGraph g,final DirectedSparseGraph lowerGraph)
	{
		if (visFrame == null)
			visFrame = new Visualiser();
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

	
	/** Displays the graph passed as an argument in the Jung window.
	 * @param g the graph to display 
	 */
	public void updateFrame(DirectedSparseGraph g)
	{
		if (visFrame == null)
			visFrame = new Visualiser();
		visFrame.update(null, g);
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
		visFrame = null;
	}

	@AfterClass
	public static void cleanUp()
	{
		try {
			if (visFrame != null)
			{
				SwingUtilities.invokeAndWait(new Runnable() 
				{
					public void run()
					{
							visFrame.setVisible(false);
							visFrame.dispose();
					}
				});
			}
		} catch (InterruptedException e) {
			// cannot do anything with this
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// cannot do anything with this
			e.printStackTrace();
		}
	}	

	
	/** In order to be able to use old junit runner. */
	public static junit.framework.Test suite()
	{
		return new JUnit4TestAdapter(TestGraphGeneration.class);
	}
}
