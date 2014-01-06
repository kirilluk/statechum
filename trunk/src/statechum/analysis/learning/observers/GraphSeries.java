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
import statechum.GlobalConfiguration;
import statechum.StatechumXML;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.linear.GD;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.linear.GD.ChangesRecorder;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;

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
	
	/** The label converter to be used. */
	protected final ConvertALabel converter;
	
	/** Creates the series for writing graphs.
	 * 
	 * @param d document to create XML nodes in
	 * @param threads number of threads to use during GD computation.
	 * @param cnf configuration to use. Particularly important if
	 * @param conv the converter to use when loading graphs 
	 * <em>setGdFailOnDuplicateNames(false)</em> has to be set.
	 */ 
	public GraphSeries(Document d, int threads, Configuration cnf, ConvertALabel conv)
	{
		doc = d;threadsNumber = threads;config=cnf;converter=conv;reset();
	}
	
	/** Creates a read-only graph series. */
	public GraphSeries(Configuration cnf, ConvertALabel conv)
	{
		doc = null;threadsNumber = 1;config=cnf;converter = conv;reset();
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
		if (element.getNodeName().equals(StatechumXML.graphmlNodeNameNS.toString()))
		{
			graph=new LearnerGraph(config);AbstractPersistence.loadGraph(element, graph, converter);
		}
		else if (element.getNodeName().equals(StatechumXML.gdGD.toString()))
		{
			LearnerGraph newGraph = new LearnerGraph(config);
			ChangesRecorder.applyGD_WithRelabelling(graph != null?new LearnerGraph(graph,config):new LearnerGraph(config), element,converter,newGraph);
			graph = newGraph;
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
			graph = new LearnerGraph(newGraph,config);// use the config passed in during construction
			// we need to keep a copy in case the original changes between now and when we get the 
			// next graph to compress.
			result = graph.storage.createGraphMLNode(doc);
		}
		else
		{
			result = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>().computeGDToXML(graph, newGraph, threadsNumber, doc, null,config);
			LearnerGraph whatShouldBeIdenticalToNewGraph = new LearnerGraph(config);
			GD.ChangesRecorder.applyGD_WithRelabelling(graph, result,converter,whatShouldBeIdenticalToNewGraph);
			// Note: if we simply do GD.ChangesRecorder.applyGD(graph,result) when loading graphs, this does not relabel vertices. 
			// For this reason, we have to do applyGD during saving in order to ensure that state IDs are consistent with 
			// what we'll end up with when a series of graphs is sequentially reconstructed, 
			// because we have to account for new IDs introduced for duplicate vertices.
			if (GlobalConfiguration.getConfiguration().isAssertEnabled()) 
			{
				// Cannot compare colours here because patches are structural, 
				// hence where there are no new transitions, colours are retained rather
				// than changed to new ones.
				DifferentFSMException ex = WMethod.checkM(newGraph, graph);
				if (ex != null) throw ex;
				ex = WMethod.checkM(newGraph, whatShouldBeIdenticalToNewGraph);
				if (ex != null) throw ex;
				ex = WMethod.checkM_and_colours(newGraph, whatShouldBeIdenticalToNewGraph,VERTEX_COMPARISON_KIND.DEEP);
				if (ex != null) throw ex;
				assert graph.pathroutines.checkUnreachableStates() == false; 
				assert whatShouldBeIdenticalToNewGraph.pathroutines.checkUnreachableStates() == false; 
			}
			graph = new LearnerGraph(newGraph,config);AbstractLearnerGraph.copyGraphs(newGraph, graph);

			// we have to make a copy of newGraph so that it will not suffer from the subsequent applyGD_WithRelabelling.
			// Although whatShouldBeIdenticalToNewGraph has the same structure as newGraph and state attributes, node numbers
			// can be different; in order to preserve them, we copy IDs too.
			++graphNumber;
		}
		return result;
	}
}
