/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
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

package statechum.analysis.learning.observers;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import statechum.Configuration;
import statechum.analysis.learning.rpnicore.GD;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.GD.ChangesRecorder;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;

/** Rather often, one would want to be able to load and store a sequence of 
 * graphs, so that only differences between graphs are stored. This 
 * can be done using an instance of this class.
 * 
 * @author kirill
 *
 */
public class GraphSeries {
	/** The previous graph in the series. */
	protected LearnerGraph graph = null;
	
	/** Document to use when creating XML nodes. */
	protected final Document doc;
	
	/** The number of threads to use. */
	protected final int threadsNumber;
	
	protected final Configuration config;
	
	/** Creates the series for writing graphs.
	 * 
	 * @param d document to create XML nodes in
	 * @param threads number of threads to use during GD computation.
	 * @param cnf configuration to use. Particularly important if 
	 * <em>setGdFailOnDuplicateNames(false)</em> has to be set.
	 */ 
	public GraphSeries(Document d, int threads, Configuration cnf)
	{
		doc = d;threadsNumber = threads;config=cnf;reset();
	}
	
	/** Creates a read-only graph series. */
	public GraphSeries(Configuration cnf)
	{
		doc = null;threadsNumber = 1;config=cnf;reset();
	}
	
	/** Resets the series - the first graph to follow will be stored directly.
	 */
	public void reset()
	{
		graph = null;graphNumber=0;totalRate=0.;
	}
	
	/** Loads graph from XML element, which could be either complete graph
	 * or a difference between the previous and the new graph.
	 *  
	 * @param node XML element to load from
	 * @param config configuration to use when loading graph.
	 * @return loaded graph.
	 */
	public LearnerGraph readGraph(Node node)
	{
		if (doc != null) throw new IllegalArgumentException("write-only series");
		if (node.getNodeType() != Node.ELEMENT_NODE)
			throw new IllegalArgumentException("loadGraph was passed a non-element");
		Element element = (Element)node;
		if (element.getNodeName().equals(Transform.graphmlNodeNameNS))
		{
			graph=Transform.loadGraph(element, config);
		}
		else if (element.getNodeName().equals(GD.ChangesRecorder.gdGD))
		{
			graph = graph != null?graph.copy(config):new LearnerGraph(config);
			ChangesRecorder.applyGD(graph, element);
		}
		else throw new IllegalArgumentException("expected either graph or GD, got "+element.getNodeName());
		
		assert graph != null;
		return graph;
	}
	
	protected int graphNumber = 0;
	protected double totalRate = 0;
	
	/** Stores the difference between the current and the new graph.
	 * 
	 * @param newGraph graph to store
	 * @param doc XML document to create a node in.
	 * @return XML element corresponding to the graph or the differences.
	 */
	public Element writeGraph(LearnerGraph newGraph)
	{
		if (doc == null) throw new IllegalArgumentException("read-only series");
		Element result = null;
		if (graph == null || !config.getCompressLogs())
		{
			graph = newGraph.copy(config);// use the config passed in during construction
			// we need to keep a copy in case the original changes between now and when we get the 
			// next graph to compress.
			result = graph.transform.createGraphMLNode(doc);
		}
		else
		{
			result = new GD().computeGDToXML(graph, newGraph, threadsNumber, doc, null);
			GD.ChangesRecorder.applyGD(graph, result);// this ensures that state IDs are consistent with what we'll end up with when a series of graphs is sequentially reconstructed.
			boolean assertionsEnabled = false;assert assertionsEnabled = true;
			if (assertionsEnabled) 
			{
				// Cannot compare colours here because patches are structural, 
				// hence where there are no new transitions, colours are retained rather
				// than changed to new ones.
				DifferentFSMException ex = WMethod.checkM(newGraph, graph);
				if (ex != null) throw ex;
				assert graph.wmethod.checkUnreachableStates() == false; 
			}
			++graphNumber;
		}
		return result;
	}
}
