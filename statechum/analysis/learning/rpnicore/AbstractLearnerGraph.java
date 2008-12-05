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

package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map.Entry;

import edu.uci.ics.jung.graph.Vertex;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.StringVertex;
import statechum.Configuration.IDMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.DeterministicDirectedSparseGraph.VertexID.VertKind;

abstract public class AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
{
	/** The initial vertex. */
	CmpVertex init;
	
	final public AbstractPathRoutines<TARGET_TYPE,CACHE_TYPE> pathroutines = new AbstractPathRoutines<TARGET_TYPE,CACHE_TYPE>(this);
	final public AbstractPersistence<TARGET_TYPE,CACHE_TYPE> storage = new AbstractPersistence<TARGET_TYPE,CACHE_TYPE>(this);

	/** Transition matrix. */
	Map<CmpVertex,Map<String,TARGET_TYPE>> transitionMatrix = createNewTransitionMatrix();

	final CACHE_TYPE learnerCache = createCache();
	
	/** Creates the cache. Should be overridden by subclasses to create instances of an appropriate type. */
	abstract public CACHE_TYPE createCache();
	
	/** The configuration stores parameters used by a variety methods
	 * involved in learning;
	 */
	public final Configuration config;
	
	/** The name of this graph to be displayed by visualiser. */
	protected String graphName = null;
	
	public void setName(String newGraphName)
	{
		graphName = newGraphName;
	}
	
	public String getName()
	{
		return graphName;
	}
	
	public static final String unknownName = "<UNKNOWN>";
	
	/** Returns a name if assigned and "Unknown" otherwise. 
	 * 
	 * @return name of this graph, never null.
	 */
	public String getNameNotNull()
	{
		String name = getName();return name == null?unknownName:name;
	}

	protected AbstractLearnerGraph(Configuration conf) {
		config = conf;
		initEmpty();
	}
	
	/** Given a set of labels and a target state, this method adds to a supplied map an association 
	 * of every label with the specified target state.
	 * 
	 * @param labels labels
	 * @param to target state
	 * @param map a map associating state <i>to</i> with each of the labels. If this is <i>null</i>, a new map is created.
	 * @return an updated map.
	 */ 
	public static Map<String,List<CmpVertex>> createLabelToStateMap(Collection<String> labels,CmpVertex to,Map<String,List<CmpVertex>> map)
	{
		Map<String,List<CmpVertex>> result = (map == null)? new LinkedHashMap<String,List<CmpVertex>>() : map;
		
		for(String label:labels)
		{
			List<CmpVertex> targets = result.get(label);
			if (targets == null) 
			{
				targets = new LinkedList<CmpVertex>();result.put(label,targets);
			}
			for(CmpVertex tgt:targets)
				if (tgt.equals(to))
					throw new IllegalArgumentException("duplicate transition with label "+label+" to state "+to);
			targets.add(to);
		}
		return result;
	}

	/** Resets all the colour labelling to the initial value. */
	public void clearColours()
	{
		for(CmpVertex v:transitionMatrix.keySet())
			v.setColour(null);
		init.setColour(JUConstants.RED);
	}
	
	/** Resets all the colour labelling to the initial value, keeping the Amber. */
	public void clearColoursButAmber()
	{
		for(CmpVertex v:transitionMatrix.keySet())
			if (!config.getUseAmber() || // clear if not using amber
					(v.getColour() != JUConstants.AMBER && v.getColour() != JUConstants.GRAY))// or colour is not AMBER or GREY 
				v.setColour(null);
		init.setColour(JUConstants.RED);
	}
	
	/** Returns the number of states in the state machine. */
	public int getStateNumber()
	{
		return transitionMatrix.size();
	}
	
	/** Returns the number of amber states in the state machine. */
	public int getAmberStateNumber()
	{
		int count = 0;
		for(CmpVertex vert:transitionMatrix.keySet()) 
			if (vert.getColour() == JUConstants.AMBER || vert.getColour() == JUConstants.GRAY) ++count;
		return count;
	}

	/** Returns the number of accept states. */
	public int getAcceptStateNumber()
	{
		int count = 0;
		for(CmpVertex vert:transitionMatrix.keySet()) if (vert.isAccept()) ++count;
		return count;
	}
	
	/** Identifies maximal values of currently used IDs and makes sure 
	 * that generation of IDs will not return any of the existing ones.
	 * <p>
	 * It is important to note that use of this method does not always 
	 * ensure that IDs are not duplicates because graphs can be loaded 
	 * with textual IDs which were originally numeric but were
	 * converted into text before graphs were written out. Since we do 
	 * not know this but both newly-generated numerical IDs and original
	 * text IDs share a namespace, it is possible that an existing ID
	 * the two will have the same string as the previously-loaded text.
	 * TODO: replace most assert statements with conditional checks, perhaps related to "underTest" variable of LearnerGraph or config, so I'll be able to test both with and without consistency checks. Best to run all tests this way and another way via ant
	 */
	protected void setIDNumbers()
	{
		for(CmpVertex vert:transitionMatrix.keySet())
			updateIDWith(vert);
	}
	
	public void updateIDWith(CmpVertex vert)
	{
		VertexID id = vert.getID();
		if ((id.getKind() == VertKind.NEUTRAL || id.getKind() == VertKind.POSITIVE)
			&& id.getIngegerID() >= vertPositiveID)
			vertPositiveID = id.getIngegerID()+1;
		if ((id.getKind() == VertKind.NEGATIVE)
				&& id.getIngegerID() >= vertNegativeID)
			vertNegativeID = id.getIngegerID()+1;
	}

	/** A very important object - this one is used when I wish to create new vertices or edges in a Jung graph.
	 * There are many threads which may wish to do that; the potential outcome is that a single thread may end up
	 * with multiple Vertices with the same ID, as repeatedly observed on 50-6. Holding a lock on this global object 
	 * when creating vertices/edges eliminates the potential of such racing, which occurs when public static int ID
	 * gets increased by Jung in the course of object creation.
	 */
	public static final Object syncObj = new Object();
	
	/** Important: when a graph is cloned, these should be cloned too in order to avoid creating duplicate vertices at some point in future. */
	protected int vertPositiveID = 0;
	protected int vertNegativeID = 0;

	/** Generates vertex IDs. Since it modifies instance ID-related variables, it has to be synchronized. */
	public synchronized VertexID nextID(boolean accepted)
	{
		VertexID result = null;
		if (config.getLearnerIdMode() == IDMode.POSITIVE_ONLY)
			result = new VertexID(VertKind.NEUTRAL,vertPositiveID++);
		else
			result = (accepted?new VertexID(VertKind.POSITIVE,vertPositiveID++):
					new VertexID(VertKind.NEGATIVE,vertNegativeID++));

		return result;
	}

	public synchronized VertexID getDefaultInitialPTAName()
	{
		if (config.getDefaultInitialPTAName().length() > 0)
			return new VertexID(config.getDefaultInitialPTAName());
		return new VertexID(VertKind.POSITIVE,vertPositiveID++);
		// Since the text ID of the initial vertex is "Init" which does not contain numerical ID,
		// I cannot adequately load graphs containing such vertices. The best solution is to abolish it.
		//new VertexID(VertKind.INIT,vertPositiveID++);
	}

	/** Creates a new vertex with the supplied name; the specific 
	 * type generated depends on the configuration supplied. 
	 * 
	 * @param name the name of the vertex to generate
	 * @param conf the configuration to use when deciding what to produce.
	 * @return the new vertex.
	 */
	public static CmpVertex generateNewCmpVertex(VertexID name,Configuration conf)
	{
		synchronized(syncObj)
		{
			return conf.isLearnerUseStrings()? 
					new StringVertex(name):
					new DeterministicVertex(name);			
		}		
	}
		
	/** This is not quite like a real clone - it clones depending on the 
	 * global configuration, so it is possible to turn a DeterministicVertex 
	 * into a StringVertex and the other way around. Moreover, cloning a 
	 * deterministic vertex requires holding a lock which is currently located in this class.
	 * <p>
	 * Important: if configuration specifies "do not clone", this one simply returns the 
	 * vertex it has been passed (with one exception). 
	 * There is an exception to this rule: if the supplied vertex
	 * is not a CmpVertex and the configuration permits handling of such vertices, 
	 * it will always be cloned. 
	 * 
	 * @param vertToClone vertex to clone.
	 * @param conf configuration to use when cloning.
	 * @return the result of cloning.
	 */
	public static CmpVertex cloneCmpVertex(Object vertToClone,Configuration conf)
	{
		CmpVertex result = null;
		if (vertToClone instanceof CmpVertex)
		{// normal processing
			CmpVertex vert = (CmpVertex)vertToClone;
			if (!conf.isLearnerCloneGraph())
				result = vert;
			else
			{
				result = generateNewCmpVertex(vert.getID(),conf);
				DeterministicDirectedSparseGraph.copyVertexData(vert, result);
			}
		}
		else
		{// we've been passed something which is not a CmpVertex
			if (!(vertToClone instanceof Vertex))
				throw new IllegalArgumentException("Cannot clone vertex which is neither a CmpVertex nor Vertex, what was passed is "+vertToClone);
			if (!conf.isAllowedToCloneNonCmpVertex())
				throw new IllegalArgumentException("Cannot clone a non-CmpVertex - prohibited using configuration");
			
			Vertex srcVert = (Vertex)vertToClone;
			if (!srcVert.containsUserDatumKey(JUConstants.LABEL))
				throw new IllegalArgumentException("vertex "+srcVert+" is not labelled");
			
			// Copying the attributes associated with the vertex
			Object label = srcVert.getUserDatum(JUConstants.LABEL);
			if (label instanceof VertexID)
				result = generateNewCmpVertex((VertexID)label, conf);
			else
				if (label instanceof String)
					result = generateNewCmpVertex(VertexID.parseID((String)label), conf);
				else
					throw new IllegalArgumentException("vertex with label "+label+" has an unsupported type");

			result.setAccept(DeterministicDirectedSparseGraph.isAccept(srcVert));
			if (srcVert.containsUserDatumKey(JUConstants.COLOUR))
				result.setColour((JUConstants)srcVert.getUserDatum(JUConstants.COLOUR));
			if (srcVert.containsUserDatumKey(JUConstants.HIGHLIGHT))
				result.setHighlight(true);
			if (srcVert.containsUserDatumKey(JUConstants.ORIGSTATE))
				result.setOrigState((VertexID)srcVert.getUserDatum(JUConstants.ORIGSTATE));
			if (srcVert.containsUserDatumKey(JUConstants.DEPTH))
				result.setDepth((Integer)srcVert.getUserDatum(JUConstants.DEPTH));
		}
		return result;
	}

	/** Used to determine which states to consider/ignore during linear.
	 * Conceptually similar to <em>FilterPredicate</em> but for a different
	 * purpose and an argument of a different type.
	  */ 
	public interface StatesToConsider
	{
		/** Returns true if the state is to be considered
		 * 
		 * @param vert state
		 * @param graph the graph in which this state is contained.
		 * @return whether state is to be considered
		 */
		public boolean stateToConsider(CmpVertex vert);
	}
	
	/** Builds a map from vertices to number, for use with <em>vertexToInt</em>.
	 * 
	 * @param whatToConsider to reject interface determining vertices to reject.
	 * null means all states are to be considered.
	 * @param vertToIntMap from vertices to numbers (an inverse of the map returned).
	 * @return map from vertex number to vertices 
	 */
	CmpVertex[] buildStateToIntegerMap(StatesToConsider whatToConsider, Map<CmpVertex,Integer> vertToIntMap)
	{
		int size=0;for(CmpVertex vert:transitionMatrix.keySet()) 
			if (whatToConsider == null || whatToConsider.stateToConsider(vert)) size++;
		CmpVertex [] intToVertexMap = new CmpVertex[size];
		int num=0;
		for(CmpVertex vert:transitionMatrix.keySet())
			if (whatToConsider == null || whatToConsider.stateToConsider(vert))
			{
				if (intToVertexMap != null) intToVertexMap[num]=vert;// populate an inverse map
				vertToIntMap.put(vert, num++);// populate the forward map
			}
		assert num == size;
		return intToVertexMap;
	}
	
	/** This one is similar to the above but does not add a vertex to the graph - I need this behaviour when
	 * concurrently processing graphs. 
	 *  
	 * @param prevState the state from which to start the new edge
	 * @param accepted whether the vertex to add should be an accept one
	 * @param input the label of the edge
	 * @return the new vertex.
	 */
	CmpVertex addVertex(CmpVertex prevState, boolean accepted, String input)
	{
		assert Thread.holdsLock(syncObj);
		CmpVertex newVertex = generateNewCmpVertex(nextID(accepted),config);
		assert !transitionMatrix.containsKey(newVertex);
		newVertex.setAccept(accepted);
		transitionMatrix.put(newVertex, createNewRow());
		addTransition(transitionMatrix.get(prevState),input,newVertex);
		return newVertex;
	}

	/** In many cases we have a vertex from some graph we'd like to add to another graph, but we cannot
	 * just do a clone because we may encounter a name clash. This method takes care of this by creating 
	 * an entirely new vertex and copies all the attributes of an existing vertex to the new one.
	 * 
	 * @param what what to add to the current graph
	 * @return the newly-added vertex.
	 */
	public CmpVertex copyVertexUnderDifferentName(CmpVertex what)
	{
		CmpVertex newVert = AbstractLearnerGraph.generateNewCmpVertex(nextID(what.isAccept()), config);
		if (GlobalConfiguration.getConfiguration().isAssertEnabled() && findVertex(newVert.getID()) != null) throw new IllegalArgumentException("duplicate vertex with ID "+newVert.getID()+" in graph "+toString());
		assert !transitionMatrix.containsKey(newVert) : "duplicate vertex "+newVert;
		DeterministicDirectedSparseGraph.copyVertexData(what, newVert);
		transitionMatrix.put(newVert,createNewRow());
		return newVert;
	}
	
	/** Clones this graph. */
	abstract public AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> copy(Configuration conf);
	
	/** Creates a new instance of this graph. */
	abstract public AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> newInstance(Configuration conf);
	
	/** Creates a new transition matrix of the correct type and backed by an appropriate map,
	 * such as a TreeMap. 
	 */
	abstract Map<CmpVertex,Map<String,TARGET_TYPE>> createNewTransitionMatrix();
	
	/** Given that we should be able to accommodate both deterministic and non-deterministic graphs,
	 * this method expected to be used when a new row for a transition matrix is to be created.
	 * The other reason for it to be introduced is that all rows are expected to be implemented with
	 * the same kind of a map, such as TreeMap, hence there has to be a mechanism to define what kind
	 * of map to use for specific kinds of graphs.  
	 *  
	 * @return new row
	 */
	abstract Map<String,TARGET_TYPE> createNewRow();
	
	/** Given that we should be able to accommodate both deterministic and non-deterministic graphs,
	 * this method expected to be used when a row for a transition matrix needs to be updated with
	 * a new label and a target state, it will add an input and/or a state to the row.
	 * 
	 * @param row the row to update
	 * @param input the input to use
	 * @param target the target state
	 */
	abstract void addTransition(Map<String,TARGET_TYPE> row,String input,CmpVertex target);

	/** Makes it possible to remove a transition from a row in a transition matrix.
	 * Does nothing if a transition does not exist.
	 * 
	 * @param row the row to modify.
	 * @param input the input to consider 
	 * @param target state to remove.
	 */
	abstract void removeTransition(Map<String,TARGET_TYPE> row,String input,CmpVertex target);	
	
	/** Given a collection of vertices or a single vertex, this method returns a collection
	 * representing all the vertices. This makes it possible to iterate through all target
	 * states regardless whether we are talking of a single vertex of a collection of them.
	 * 
	 * @param targ targets
	 * @return iterator for navigation through target states.
	 */
	abstract Collection<CmpVertex> getTargets(TARGET_TYPE targ);
	
	/** Initialises this graph with a single-state PTA. */
	public void initPTA()
	{
		initEmpty(); 
		init = generateNewCmpVertex(getDefaultInitialPTAName(),config);
		init.setAccept(true);init.setColour(JUConstants.RED);
		init.setDepth(0);

		transitionMatrix.put(init,createNewRow());
		learnerCache.invalidate();
	}

	/** Initialises this graph with a single-state PTA with ID of 1001 rather than 1000 as per normal initPTA(). */
	public void initPTA_1()
	{
		initEmpty();nextID(true);
		init = generateNewCmpVertex(getDefaultInitialPTAName(),config);
		init.setAccept(true);init.setColour(JUConstants.RED);
		init.setDepth(0);
	
		transitionMatrix.put(init,createNewRow());
		learnerCache.invalidate();
	}

	/** Initialises this graph with an empty graph, but IDs of vertices are unchanged. */
	public void initEmpty()
	{
		transitionMatrix.clear();init=null;incompatibles.clear();
		learnerCache.invalidate();
		vertNegativeID = config.getInitialIDvalue();vertPositiveID = config.getInitialIDvalue();
	}

	/** Finds a vertex with a supplied identifier in a transition matrix.
	 * <p>
	 * <b>Important</b>: do not change the acceptance condition on the returned vertex: 
	 * it will mess up the transition matrix since hash code is dependent on acceptance.
	 */
	public CmpVertex findVertex(String name)
	{
		if (name == null)
			return null;
		return findVertex(VertexID.parseID(name));
	}
	
	/** Finds a vertex with a supplied identifier in a transition matrix.
	 * <p>
	 * <b>Important</b>: do not change the acceptance condition on the returned vertex: 
	 * it will mess up the transition matrix since hash code is dependent on acceptance.
	 */
	public CmpVertex findVertex(VertexID name)
	{
		CmpVertex result = null;
		Iterator<Entry<CmpVertex,Map<String,TARGET_TYPE>>> entryIt = transitionMatrix.entrySet().iterator();
		while(entryIt.hasNext() && result == null)
		{
			CmpVertex currentVert = entryIt.next().getKey();
			VertexID vertName = currentVert.getID();
			if (vertName.equals(name))
				result = currentVert;
		}
		return result;
	}
	
	/** Given two graphs, empties the target graph and copies all of the <em>from</em> 
	 * graph into it. The reason this is better than cloning is that one can copy
	 * between graphs of completely different types, such as deterministic to a 
	 * non-deterministic. Vertex-cloning configuration is taken from the target graph.
	 * Hence the way to do old-fashioned clone is:
	 * <pre>
	 * LearnerGraph copy = new LearnerGraph(copyConfig);
	 * AbstractLearnerGraph.copyGraphs(from,copy);
	 * </pre>
	 * <p>
	 * Note: the copying process is not necessarily deep: the transition matrix is 
	 * cloned and so is the configuration, but states (vertices) are not 
	 * if the configuration does not specify cloning; they are cloned 
	 * otherwise.
	 * 
	 * @param from the source graph
	 * @param result the graph to copy into.
	 */
	public static <TARGET_A_TYPE,TARGET_B_TYPE,
		CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
		CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>> 
		void copyGraphs(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> from,
				AbstractLearnerGraph<TARGET_B_TYPE, CACHE_B_TYPE> result)
	{
		result.initEmpty();
		result.transitionMatrix = result.createNewTransitionMatrix();
		result.vertNegativeID = from.vertNegativeID;result.vertPositiveID=from.vertPositiveID;
		result.setName(from.getName());

		Map<CmpVertex,CmpVertex> oldToNew = new HashMap<CmpVertex,CmpVertex>();
		
		// First, clone vertices
		for(CmpVertex state:from.transitionMatrix.keySet())
			oldToNew.put(state, cloneCmpVertex(state, result.config));

		result.init = oldToNew.get(from.init);
		addAndRelabelGraphs(from, oldToNew, result);
	}

	/** Copies a transition matrix and incompatible sets between graphs, relabelling states as specified.
	 * The relabelling map has to be total; it is not checked for duplicate target states.
	 * 
	 * @param from graph to copy
	 * @param relabel map relabelling states
	 * @param result where to store result of copying
	 */
	public static <TARGET_A_TYPE,TARGET_B_TYPE,
		CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
		CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>> 
		void addAndRelabelGraphs(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> from,
				Map<CmpVertex,CmpVertex> oldToNew,
				AbstractLearnerGraph<TARGET_B_TYPE, CACHE_B_TYPE> result)
	{
		// Clone edges.
		for(Entry<CmpVertex,Map<String,TARGET_A_TYPE>> entry:from.transitionMatrix.entrySet())
		{
			Map<String,TARGET_B_TYPE> row = result.createNewRow();result.transitionMatrix.put(oldToNew.get(entry.getKey()),row);
			for(Entry<String,TARGET_A_TYPE> rowEntry:entry.getValue().entrySet())
			{
				for(CmpVertex vertex:from.getTargets(rowEntry.getValue()))
					result.addTransition(row, rowEntry.getKey(), oldToNew.get(vertex));
			}
		}
		
		for(Entry<CmpVertex,Set<CmpVertex>> entry:from.incompatibles.entrySet())
		{
			CmpVertex incompatibleVertex = oldToNew.get(entry.getKey());// TODO: to test that this part works.
			assert !result.incompatibles.containsKey(incompatibleVertex);
			Set<CmpVertex> incSet = result.createNewIncompatiblesRow(incompatibleVertex);
			for(CmpVertex vert:entry.getValue())
				incSet.add(oldToNew.get(vert));
		}
		
		result.learnerCache.invalidate();
	}
		
	@Override
	public String toString()
	{
		return "Graph "+getNameNotNull()+" states: "+transitionMatrix.size();//+" (hash "+transitionMatrix.hashCode()+")";
	}

	/** This one does not consider configuration or IDs - only states/transitions 
	 * are compared. I think this is best, however note that it is not a 
	 * congruence: doing an "augment pta" may hence add vertices with different
	 * numbers and hence previously identical graphs will become different.
	 * I think that despite this problem, it is important to be able to consider
	 * graphs with isomorphic transition diagrams to be the same.
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		//result = prime * result + ((config == null) ? 0 : config.hashCode());
		//result = prime * result + vertNegativeID;
		//result = prime * result + vertPositiveID;
		result = prime * result + (init == null?0:init.hashCode());
		//result = prime * result + transitionMatrix.hashCode();
		for(Entry<CmpVertex,Map<String,TARGET_TYPE>> entry:transitionMatrix.entrySet())
		{
			result = prime * result + entry.getKey().hashCode();
			result = prime * result + (entry.getKey().isAccept()?  1231 : 1237);
			result = prime * result + (entry.getKey().getColour() == null?0:entry.getKey().getColour().hashCode());
			result = prime * result + (entry.getKey().isHighlight()?  1231 : 1237);
			result = prime * result + (entry.getKey().getDepth());
			result = prime * result + (entry.getKey().getOrigState() == null? 0:entry.getKey().getOrigState().hashCode());
			for(Entry<String,TARGET_TYPE> rowEntry:entry.getValue().entrySet())
			{
				result = prime * result + rowEntry.getKey().hashCode();
				for(CmpVertex vertex:getTargets(rowEntry.getValue()))
					result = prime * result + vertex.hashCode();
			}
		}
		for(Entry<CmpVertex,Set<CmpVertex>> entry:incompatibles.entrySet())
			result = prime * result + entry.getValue().hashCode();

		return result;
	}

	/** A crude comparison of graphs, however makes it possible to compare
	 * deterministic and non-deterministic graphs.<p> 
	 * 
	 * This one does not compare vertex attributes, configurations or IDs - only states/transitions 
	 * are compared and even that is done by matching identifiers of vertices. 
	 * I think this is best, however note that it is not a congruence: 
	 * <ul>
	 * <li>doing an "augment pta" may hence add vertices with different
	 * numbers and hence previously identical graphs will become different.
	 * I think that despite this problem, it is important to be able to consider
	 * graphs with isomorphic transition diagrams to be the same.</li>
	 * <li>configurations are not compared, hence two graphs may behave differently 
	 * when I try to learn automata using them.</li>
	 * </ul>
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof AbstractLearnerGraph))
			return false;
		final AbstractLearnerGraph other = (AbstractLearnerGraph) obj;
		
		if (init == null)
		{
			if (other.init != null)
				return false;
		}
		else
			if (!init.equals(other.init))
				return false;

		if (!transitionMatrix.keySet().equals(other.transitionMatrix.keySet()))
			return false;
		if (!incompatibles.keySet().equals(other.incompatibles.keySet()))
			return false;
		
		final Set<CmpVertex> targetThis = new TreeSet<CmpVertex>(), targetOther = new TreeSet<CmpVertex>(); 
		for(Entry<CmpVertex,Map<String,TARGET_TYPE>> entry:transitionMatrix.entrySet())
		{
			Map<String,Object> row = (Map<String,Object>)other.transitionMatrix.get(entry.getKey());
			if (!DeterministicDirectedSparseGraph.deepEquals(entry.getKey(),other.findVertex(entry.getKey().getID())))
				return false;// different attributes or incompatible labelling
			
			if (!row.keySet().equals(entry.getValue().keySet()))
				return false;
			
			for(Entry<String,TARGET_TYPE> rowEntry:entry.getValue().entrySet())
			{
				targetThis.clear();targetOther.clear();
				targetThis.addAll(getTargets(rowEntry.getValue()));targetOther.addAll(other.getTargets(row.get(rowEntry.getKey())));
				if (!targetThis.equals(targetOther))
					return false;
			}
		}
		
		for(Entry<CmpVertex,Set<CmpVertex>> entry:incompatibles.entrySet())
			if (!entry.getValue().equals(other.incompatibles.get(entry.getKey())))
				return false;

		return true;
	}

	/** Verifies whether a supplied pair is either incompatible (one state is accept and another one - reject) 
	 * or recorded as incompatible.
	 *  
	 * @param pair what to check. It is assumed that the two states belong to the graph.
	 * @return false if a pair is incompatible, true otherwise.
	 */
	public static boolean checkCompatible(CmpVertex Q, CmpVertex R, Map<CmpVertex,Set<CmpVertex>> incompatibles)
	{
		if (Q.isAccept() != R.isAccept())
			return false;

		Set<CmpVertex> incSet = incompatibles.get(Q);
		return incSet == null || !incSet.contains(R);
	}
	
	
	/** Stores pairs of states which should either never be merged due to constraints which cannot 
	 * be easily expressed with counter-examples such as non-determinism related to intersection of
	 * domains of labels from them (those which can be expressed using labels should be added to the maximal
	 * automaton). 
	 */
	final protected Map<CmpVertex,Set<CmpVertex>> incompatibles = new HashMap<CmpVertex,Set<CmpVertex>>();

	/** Adds a supplied pair to the collection of incompatible elements. 
	 *  It is assumed that the two states belong to the graph; the two states should not be the same.
	 *  
	 * @param A one of the vertices to add
	 * @param B another vertex to add.
	 */
	public void addToIncompatibles(CmpVertex A, CmpVertex B)
	{
		assert !A.getID().equals(B.getID());
		addToIncompatibles_internal(A, B);
		addToIncompatibles_internal(B, A);
	}

	/** Removes a supplied pair from the collection of incompatible elements. 
	 *  It is assumed that the two states belong to the graph; the two states should not be the same.
	 *  If a pair is not found, it is ignored.
	 *  
	 * @param A one of the vertices to remove
	 * @param B another vertex to remove.
	 */
	public void removeFromIncompatibles(CmpVertex A, CmpVertex B)
	{
		assert !A.getID().equals(B.getID());
		removeFromIncompatibles_internal(A, B);
		removeFromIncompatibles_internal(B, A);
		
	}	
	
	protected Set<CmpVertex> createNewIncompatiblesRow(CmpVertex A)
	{
		Set<CmpVertex> incSet = new HashSet<CmpVertex>();incompatibles.put(A,incSet);return incSet;
	}
	
	private void addToIncompatibles_internal(CmpVertex A, CmpVertex B)
	{
		Set<CmpVertex> incSet = incompatibles.get(A);
		if (incSet == null) incSet = createNewIncompatiblesRow(A);
		incSet.add(B);
	}

	private void removeFromIncompatibles_internal(CmpVertex A, CmpVertex B)
	{
		Set<CmpVertex> remSet = incompatibles.get(A);
		if (remSet != null)	
		{
			remSet.remove(B);if (remSet.isEmpty()) incompatibles.remove(A);
		}
	}
}
