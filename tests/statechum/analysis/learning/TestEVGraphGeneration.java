/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */ 

package statechum.analysis.learning;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.JUConstants;
import statechum.Label;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestEquivalenceChecking;

import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

public class TestEVGraphGeneration {
	public static final String EDGE = " EDGE";
	public static final String VERTEX = "VERTEX";
	
	private static void addSingletonLabel(DirectedSparseEdge e, Label label)
	{
		Set<Label> labelSet = new TreeSet<Label>();labelSet.add(label);
		e.setUserDatum(JUConstants.LABEL, labelSet, UserData.SHARED);		
	}
	
	@SuppressWarnings("unchecked")
	public static DirectedSparseGraph buildEVGraph(String graphString)
	{
		DirectedSparseGraph g = FsmParser.buildLearnerGraph(graphString, "simpleGraph",Configuration.getDefaultConfiguration().copy(),null).pathroutines.getGraph();
		g.getEdgeConstraints().clear();
		List<Edge> newEdges = new LinkedList<Edge>();
		for(DeterministicEdge e:(Set<DeterministicEdge>)g.getEdges())
		{
			Collection<Label> labels = (Collection<Label>)e.getUserDatum(JUConstants.LABEL);
			e.removeUserDatum(JUConstants.LABEL);
			Iterator<Label> labelIt = labels.iterator(); 
			e.addUserDatum(EDGE, labelIt.next(), UserData.SHARED);
			while(labelIt.hasNext())
			{
				DeterministicEdge newEdge = AbstractLearnerGraph.generateNewJungEdge((DeterministicVertex)e.getSource(),(DeterministicVertex)e.getDest());
				newEdge.setUserDatum(EDGE, labelIt.next(), UserData.SHARED);
				newEdges.add(newEdge);
			}
		}
		
		for(Edge e:newEdges)
			g.addEdge(e);
		
		for(DeterministicVertex v:(Set<DeterministicVertex>)g.getVertices())
		{
			 v.addUserDatum(VERTEX, v.getUserDatum(JUConstants.LABEL), UserData.SHARED);
			 v.removeUserDatum(JUConstants.LABEL);
		}
		
		return g;
	}

	public void checkEquivalence(DirectedSparseGraph g, String expected)
	{
		for(DeterministicEdge e:(Set<DeterministicEdge>)g.getEdges())
			addSingletonLabel(e, (Label)e.getUserDatum(EDGE));
		
		for(DeterministicVertex v:(Set<DeterministicVertex>)g.getVertices())
		{
			 v.addUserDatum(JUConstants.LABEL, v.getUserDatum(VERTEX), UserData.SHARED);
			 v.removeUserDatum(VERTEX);
		}
		TestEquivalenceChecking.checkM(expected, new LearnerGraph(g,Configuration.getDefaultConfiguration()), Configuration.getDefaultConfiguration(),null);
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
		Visualiser.disposeFrame();
	}

	@AfterClass
	public static void cleanUp()
	{
		Visualiser.disposeFrame();
	}	

}
