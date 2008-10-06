/*
 * Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */

package statechum;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class DeterministicDirectedSparseGraph {

	final public static class VertexID implements Comparable<VertexID>, Serializable
	{
		/**
		 * 
		 */
		private static final long serialVersionUID = -6367197525198958482L;

		/** A kind of a state. INIT does not really have to be a special kind but it is convenient to 
		 * be able to look at an ID and know exactly the sort of state one is looking at.
		 * <ul>
		 * <li>INIT was used to designate an initial state, not used any more.</li>
		 * <li>NONE is for states which only have string IDs.</li>
		 * <li>NEUTRAL is used if I wish to give uniform IDs to accept and reject states. 
		 * This is useful if I would like not to consider all accept states before reject
		 * states and such - this is useful to test how sensitive the learner is to such
		 * an ordering.</li>
		 * <li>NEGATIVE means that this is a reject-state.</li>
		 * <li>POSITIVE means that this is an accept-state.</li>
		 */
		public enum VertKind { NEUTRAL, NEGATIVE, POSITIVE, NONE };
		
		/** Textual representation of this ID, definite value if kind == VertKind.NONE
		 * and a cached version of a numerical ID if not.
		 */
		private String idString; 
		private final VertKind kind;
		private final int idInteger;
		
		/** Cached hash code. */
		private final int cachedHash;
		
		protected VertexID()
		{// default values to ensure failure of operations
			idString = null;kind = VertKind.NONE;idInteger=-1;cachedHash=0;
		}
		
		/** Returns the kind of this ID. */
		public VertKind getKind()
		{
			return kind;
		}
		
		public VertexID(String id)
		{
			if (id == null) throw new IllegalArgumentException("invalid id");
			idString = id;kind=VertKind.NONE;idInteger=0;
			cachedHash = ~idString.hashCode(); // aims to ensure that "P5" and positive,5 have different hashes since we now consider them different.
		}
		
		/** When XML files are loaded, IDs are always textual, however we'd like
		/* to convert them to numbers in order to prevent nextID from returning
		/* identifies which when converted to strings are the same as existing 
		/* identifiers.
		 */
		public static VertexID parseID(String text)
		{
			VertKind tentativeKind = VertKind.NONE;
			int id = -1;
			/*
			if (text.equals(initID))
			{// if this is an ID of an initial state
				id=LearnerGraph.initialIDvalue;tentativeKind = VertKind.INIT;
			}
			else*/
			if (text.length() > 1)
			{
				if (text.charAt(0) == 'P')
					tentativeKind = VertKind.POSITIVE;
				else if (text.charAt(0) == 'N')
					tentativeKind = VertKind.NEGATIVE;
				
				if (tentativeKind != VertKind.NONE)
					try
				{
						id = Integer.parseInt(text.substring(1));
				}
				catch(NumberFormatException ex)
				{// cannot parse, revert to text.
					tentativeKind = VertKind.NONE;
				}
			}
			
			if (tentativeKind == VertKind.NONE)
				return new VertexID(text);
			
			return new VertexID(tentativeKind,id);
		}
		
		public int getIngegerID()
		{
			assert kind != VertKind.NONE;
			return idInteger;
		}
		
		protected static final String initID = "Init"; 
		
		/** In order to enable comparison between vertex ID which are 
		 * represented by Strings and/or integer, 
		 * the two need need to be converted to a common form.
		 */
		@SuppressWarnings("incomplete-switch") // NONE action is impossible by construction.
		public String getStringId()
		{
			String result = null;
			switch(kind)
			{
/*			case INIT:
				result = initID;break;*/
			case NEGATIVE:
				result = "N"+idInteger;break;
			case POSITIVE:
				result = "P"+idInteger;break;
			case NEUTRAL:
				result = "V"+idInteger;break;
			}
			
			return result;
		}
		
		public VertexID(VertKind k, int i)
		{
			if (k == VertKind.NONE) throw new IllegalArgumentException("invalid id kind");
			idString = null;kind = k;idInteger=i;
			cachedHash = getStringId().hashCode();
			assignStringID_ifNeeded();
		}

		@Override
		public String toString()
		{
			assignStringID_ifNeeded();
			return idString;
		}
	
		protected void assignStringID_ifNeeded()
		{
			if (idString == null)
				idString = getStringId();
		}
		
		/** The following two kinds of comparisons are supported (this determines 
		 * the order of exploration of vertices, which influences the results.
		 * <p>
		 * The COMPARISON_LEXICOGRAPHIC_ORIG is used for checking the compatibility to 
		 * the Dec 2007 version.
		 */
		public enum ComparisonKind { COMPARISON_NORM, COMPARISON_LEXICOGRAPHIC_ORIG };
		
		/** Determines the behaviour of <em>compareTo</em>. This has to be static and sadly public.
		 * I could've passed an extra member from LearnerGraph or such which would determine this,
		 * but this way seems more appropriate for occasional experiments.
		 */
		public static ComparisonKind comparisonKind = ComparisonKind.COMPARISON_NORM;
		
		public int compareTo(VertexID o) {
			if (comparisonKind == ComparisonKind.COMPARISON_LEXICOGRAPHIC_ORIG) // Dec2007 compatibility mode
				return idString.compareTo(o.idString);
			
			// normal comparison mode
			if (kind != VertKind.NONE && o.kind != VertKind.NONE)
			{// if both this one and the other ID are numeric, use a numeric comparison.
				int kindDifference = kind.compareTo(o.kind);
				if (kindDifference != 0)
					return kindDifference;
				
				return idInteger - o.idInteger;				
			}
			
			int kindDifference = kind.compareTo(o.kind);
			if (kindDifference != 0) // we should never compare textual representation of numbers to the actual numbers, because 
				// it is easy to get into a nontransitive situation where init>P39>N1017>Init (Statechum verision 338)
				return kindDifference;

			int lenDifference = idString.length() - o.idString.length();
			if (lenDifference !=0)
				return lenDifference;
			return idString.compareTo(o.idString);
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			return cachedHash;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof VertexID))
				return false;
			final VertexID other = (VertexID) obj;
			
			if (kind != other.kind) // never compare integers with strings
				return false;

			if (kind != VertKind.NONE && other.kind != VertKind.NONE)
				return idInteger == other.idInteger;
			
			return idString.equals(other.idString);
		}
		
	}
	
	/** These are expected to be compared for equality using acceptance and IDs only,
	 * not highlight or colour.
	 */
	public interface CmpVertex extends Comparable<CmpVertex> {
		/** Returns an ID of this vertex. */
		VertexID getID();
		
		/** Returns true if this is an accept vertex and false for a reject one. */
		boolean isAccept();
		/** Makes this state an accept/reject one. */
		void setAccept(boolean accept);

		/** Returns the colour of this vertex. */
		JUConstants getColour();
		/** Sets the colour of this vertex. null removes the colour. */
		void setColour(JUConstants colour);

		//public static class IllegalBooleanException extends IllegalArgumentException {
		
		/** This exception is thrown when vertex user data for a property is out of range. */ 
		public static class IllegalUserDataException extends IllegalArgumentException {
			/**
			 * Serial ID.
			 */
			private static final long serialVersionUID = -464955935013454935L;

			public IllegalUserDataException(String name)
			{ 
				super("invalid colour "+name);
			}
		}
		
		/** Determines whether this vertex is to be highlighted. */
		boolean isHighlight();
		/** Sets the mark on this vertex. */
		void setHighlight(boolean hightlight);
	}

	/**
	 * The extension of the vertex where all operations are ID-based, for
	 * performance.
	 */
	public static class DeterministicVertex extends DirectedSparseVertex implements Comparable<CmpVertex>, CmpVertex {
		protected VertexID vertexID = null;

		protected int hashCode = super.hashCode();

		public DeterministicVertex(VertexID thisVertexID) {
			super();
			addUserDatum(JUConstants.LABEL, thisVertexID, UserData.SHARED);
		}

		public DeterministicVertex(String name)
		{
			super();
			addUserDatum(JUConstants.LABEL, 
					new DeterministicDirectedSparseGraph.VertexID(name), UserData.SHARED);
		}
		
		/** Used to hold two objects and nothing else. */
		public static class MiniPair
		{
			protected Object key,datum;
			
			public Object getKey()
			{ return key; }
			
			public Object getValue()
			{ return datum; }

			public MiniPair(Object k, Object d)
			{
				key=k;datum=d;
				String strKey = key.toString(), strDatum = datum.toString();
				
				if (key == JUConstants.LABEL || strKey.equalsIgnoreCase(JUConstants.LABEL.name())) 
				{
					key = JUConstants.LABEL;
				}
				else
				{
					if (key == JUConstants.COLOUR || strKey.equalsIgnoreCase(JUConstants.COLOUR.name()))
					{
						key = JUConstants.COLOUR;
						if (datum == JUConstants.RED || strDatum.equalsIgnoreCase(JUConstants.RED.name()))
							datum = JUConstants.RED;
						else
							if (datum == JUConstants.BLUE || strDatum.equalsIgnoreCase(JUConstants.BLUE.name()))
								datum = JUConstants.BLUE;
							else
								if (datum == JUConstants.AMBER || strDatum.equalsIgnoreCase(JUConstants.AMBER.name()))
									datum = JUConstants.AMBER;
								else
									if (datum == JUConstants.GRAY || strDatum.equalsIgnoreCase(JUConstants.GRAY.name()))
										datum = JUConstants.GRAY;
									else
										if (datum == JUConstants.INF_AMBER || strDatum.equalsIgnoreCase(JUConstants.INF_AMBER.name()))
											datum = JUConstants.INF_AMBER;
										else
											throw new DeterministicDirectedSparseGraph.CmpVertex.IllegalUserDataException(strDatum);
					}
					else
					if (key == JUConstants.ACCEPTED || strKey.equalsIgnoreCase(JUConstants.ACCEPTED.name()))
					{
						key = JUConstants.ACCEPTED;datum = getBoolean(datum);
					}
					else
						if (key == JUConstants.HIGHLIGHT || strKey.equalsIgnoreCase(JUConstants.HIGHLIGHT.name()))
						{
							key = JUConstants.HIGHLIGHT;datum = getBoolean(datum);
						}
						
				}
			}
		}
		
		/** An overridden version of Jung's setUserDatum which converts textual attributes
		 * to JUConstant-drive types.
		 */
		@Override
		public void addUserDatum(Object key, Object datum, CopyAction copyAct) {
			MiniPair p=new MiniPair(key,datum);
			if (p.key == JUConstants.LABEL)
			{
				vertexID = (VertexID) p.getValue();
				hashCode = p.getValue().hashCode();
			}
			super.addUserDatum(p.getKey(), p.getValue(), copyAct);
		}

		public static Boolean getBoolean(Object obj)
		{
			if (obj instanceof Boolean)
				return (Boolean)obj;
			
			if (obj.toString().equalsIgnoreCase("true"))
				return new Boolean(true);

			if (obj.toString().equalsIgnoreCase("false"))
				return new Boolean(false);
			
			throw new IllegalUserDataException(obj.toString());
		}
		
		/** Removes and re-adds the datum, in order to make use of the  
		 */
		@Override
		public void setUserDatum(Object key, Object datum, CopyAction copyAct) {
			MiniPair p=new MiniPair(key,datum);
			if (p.key == JUConstants.LABEL)
			{
				vertexID = (VertexID) p.datum;
				hashCode = p.datum.hashCode();
			}
			super.setUserDatum(p.key, p.datum, copyAct);
		}

		@Override
		public int hashCode() {
			int labelHashCode = hashCode;
			if (!isAccept())
				labelHashCode = ~labelHashCode;
			
			return labelHashCode;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see edu.uci.ics.jung.graph.impl.AbstractSparseVertex#toString()
		 */
		@Override
		public String toString() {
			if (vertexID != null)
				return vertexID.toString();

			return super.toString();
		}

		/** The ordering is based on names only ignoring whether this is an
		 * accept or a reject vertex. This is necessary if we wish to adjust
		 * an order of traversal in experiments. In cases where accepts or
		 * rejects should appear earlier/later, the <i>nextID</i> method
		 * will generate the appropriate number. 
		 */
		public int compareTo(CmpVertex o) {
			assert o != null;
/*			if (!(o instanceof CmpVertex))
				throw new IllegalArgumentException("an attempt to compare "
					+ toString() + " with a non-CmpVertex " + o.getName());*/
			CmpVertex v = o;
			if (this == v)
				return 0;
			return vertexID.compareTo(v.getID());
		}
		
		/** Compares this vertex with a different one, based on label alone.
		 */

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (!(obj instanceof CmpVertex))
				return false;
			
			final CmpVertex other = (CmpVertex) obj;
			if (isAccept() != other.isAccept())
				return false;
			if (vertexID == null)
				return other.getID() == null;
			
			return vertexID.equals(other.getID());
		}
		
		public VertexID getID() {
			return vertexID;
		}

		public boolean isAccept() {
			return DeterministicDirectedSparseGraph.isAccept(this);
		}

		public void setAccept(boolean accept) 
		{
			removeUserDatum(JUConstants.ACCEPTED);
			addUserDatum(JUConstants.ACCEPTED, accept, UserData.SHARED);
		}

		public JUConstants getColour() 
		{
			return (JUConstants)getUserDatum(JUConstants.COLOUR);
		}

		public void setColour(JUConstants c) 
		{
			if (c != null && c != JUConstants.RED && c != JUConstants.BLUE && c != JUConstants.AMBER && c != JUConstants.GRAY && c != JUConstants.INF_AMBER)
				throw new IllegalUserDataException("colour "+c+" is not a valid colour (vertex "+vertexID.toString()+")");

			removeUserDatum(JUConstants.COLOUR);
			if (c != null)
				addUserDatum(JUConstants.COLOUR, c, UserData.SHARED);
		}

		public void setHighlight(boolean hightlight) {
			removeUserDatum(JUConstants.HIGHLIGHT);
			if (hightlight)
				addUserDatum(JUConstants.HIGHLIGHT, true, UserData.SHARED);
		}

		public boolean isHighlight() {
			Object highlight = getUserDatum(JUConstants.HIGHLIGHT);
			if (highlight == null)
				return false;
			
			return (Boolean)highlight;
				
		}
	}

	public static class DeterministicEdge extends DirectedSparseEdge {

		public DeterministicEdge(DeterministicVertex from, DeterministicVertex to) {
			super(from, to);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see edu.uci.ics.jung.graph.impl.AbstractSparseVertex#toString()
		 */
		@Override
		public String toString() {
			return getSource().toString() + "-"
					+ getUserDatum(JUConstants.LABEL).toString() + "->"
					+ getDest().toString();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see edu.uci.ics.jung.graph.impl.AbstractElement#hashCode()
		 */
		@Override
		public int hashCode() {
			final int PRIME1 = 31;
			return getDest().hashCode() * PRIME1 + getSource().hashCode();
		}

	}

	/** Checks if the supplied vertex is an accept one or not. If the vertex is not annotated, returns true.
	 * 
	 * @param v vertex to check
	 * @return true if the vertex is an accept-vertex
	 */
	public final static boolean isAccept(final Vertex v)
	{
		if (v.containsUserDatumKey(JUConstants.ACCEPTED))
			return ((Boolean)v.getUserDatum(JUConstants.ACCEPTED)).booleanValue();
		
		return true;
	}

	/** Finds a vertex with a given name.
	 * 
	 * @param name the name of the node to look for.
	 * @param g the graph to search in
	 * @return vertex found.
	 */
	public static DeterministicVertex findVertexNamed(Object name,Graph g)
	{
		return (DeterministicVertex)DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL,name,g);
	}

	/** Given a graph, this method computes an alphabet of it. */
	public static Set<String> computeAlphabet(DirectedSparseGraph g)
	{
		Set<String> alphabet = new TreeSet<String>();
	
		for(Edge e:(Set<Edge>)g.getEdges())
				alphabet.addAll( (Set<String>)e.getUserDatum(JUConstants.LABEL) );
		return alphabet;
	}

	/** If the supplied vertex is already known (its label is stored in the map), the one from the map is returned;
	 * otherwise a reasonable copy is made, it is then both returned and stored in the map.
	 * 
	 * @param newVertices the map from labels to new vertices
	 * @param g the graph which will have the new vertex added to it
	 * @param origVertex the vertex to copy
	 * @return a copy of the vertex
	 */
	public static DeterministicVertex copyVertex(Map<VertexID,DeterministicVertex> newVertices, DirectedSparseGraph g,Vertex orig)
	{
		if (!(orig instanceof DeterministicVertex))
			throw new IllegalArgumentException("cannot copy a graph which is not known to be built out of deterministic elements");
		DeterministicVertex origVertex = (DeterministicVertex)orig;
		VertexID vertID = origVertex.getID();
		DeterministicVertex newVertex = newVertices.get(vertID);
		if (newVertex == null) { 
			newVertex = new DeterministicVertex(vertID);
			newVertex.addUserDatum(JUConstants.ACCEPTED, isAccept(origVertex), UserData.SHARED);
			if (DeterministicDirectedSparseGraph.isInitial(origVertex))
				newVertex.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
			newVertices.put(vertID,newVertex);g.addVertex(newVertex);
		}
		return newVertex;
	}

	/** Checks if the supplied vertex is an initial state.
	 * 
	 * @param v vertex to check
	 * @return true if the vertex is an initial state
	 */
	public final static boolean isInitial(final Vertex v)
	{
		return v.containsUserDatumKey(JUConstants.INITIAL);		
	}

	/** Creates a graph with a single accept-vertex. */
	public static DirectedSparseGraph initialise(){
		DirectedSparseGraph pta = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
		pta.setUserDatum(JUConstants.TITLE, "Hypothesis machine", UserData.SHARED);
		pta.addVertex(init);
		DeterministicDirectedSparseGraph.numberVertices(pta);
		return pta;
	}

	/** Computes an alphabet of a given graph and adds transitions to a 
	 * reject state from all states A and inputs a from which there is no B such that A-a->B
	 * (A-a-#REJECT) gets added. Note: such transitions are even added to reject vertices.
	 * The behaviour is not defined if reject vertex already exists.
	 * 
	 * @param g the graph to add transitions to
	 * @param reject the name of the reject state, to be added to the graph.
	 * @return true if any transitions have been added
	 */   
	public static boolean completeGraph(DirectedSparseGraph g, String reject)
	{
		DirectedSparseVertex rejectVertex = new DirectedSparseVertex();
		boolean transitionsToBeAdded = false;// whether and new transitions have to be added.
		rejectVertex.addUserDatum(JUConstants.ACCEPTED, false, UserData.SHARED);
		rejectVertex.addUserDatum(JUConstants.LABEL, reject, UserData.SHARED);
		
		// first pass - computing an alphabet
		Set<String> alphabet = computeAlphabet(g);
		
		// second pass - checking if any transitions need to be added.
		Set<String> outLabels = new HashSet<String>();
		Iterator<Vertex> vertexIt = (Iterator<Vertex>)g.getVertices().iterator();
		while(vertexIt.hasNext() && !transitionsToBeAdded)
		{
			Vertex v = vertexIt.next();
			outLabels.clear();
			Iterator<DirectedSparseEdge>outEdgeIt = v.getOutEdges().iterator();
			while(outEdgeIt.hasNext()){
				DirectedSparseEdge outEdge = outEdgeIt.next();
				outLabels.addAll( (Set<String>)outEdge.getUserDatum(JUConstants.LABEL) );
			}
			transitionsToBeAdded = !alphabet.equals(outLabels);
		}
		
		if (transitionsToBeAdded)
		{
			// third pass - adding transitions
			g.addVertex(rejectVertex);
			vertexIt = (Iterator<Vertex>)g.getVertices().iterator();
			while(vertexIt.hasNext())
			{
				Vertex v = vertexIt.next();
				if (v != rejectVertex)
				{// no transitions should start from the reject vertex
					Set<String> outgoingLabels = new TreeSet<String>();outgoingLabels.addAll(alphabet);
					
					Iterator<DirectedSparseEdge>outEdgeIt = v.getOutEdges().iterator();
					while(outEdgeIt.hasNext()){
						DirectedSparseEdge outEdge = outEdgeIt.next();
						outgoingLabels.removeAll( (Set<String>)outEdge.getUserDatum(JUConstants.LABEL) );
					}
					if (!outgoingLabels.isEmpty())
					{
						// add a transition
						DirectedSparseEdge edge = new DirectedSparseEdge(v,rejectVertex);
						edge.addUserDatum(JUConstants.LABEL, outgoingLabels, UserData.CLONE);
						g.addEdge(edge);
					}
				}
			}
		}
		
		return transitionsToBeAdded;
	}

	/**
	 * Labels vertices according to breadth-first search
	 * KIRR: this should label vertices according to their Jung ID instead, because those IDs 
	 * are shown in the debugger and it is pain to dig through to find labels in user-added data.
	 * 
	 * @param pta the graph to operate on.
	 */
	public static void numberVertices(DirectedSparseGraph pta){
		Iterator<Vertex> vertexIt = getBFSList(pta).iterator();
		while(vertexIt.hasNext()){
			Vertex v = vertexIt.next();
			v.removeUserDatum(JUConstants.LABEL);// since we'd like this method to run multiple times, once immediately after initialisation and subsequently when sPlus and sMinus are added.
			v.addUserDatum(JUConstants.LABEL, v.toString(), UserData.SHARED);
		}
	}

	public static List<Vertex> getBFSList(Graph g){
		List<Vertex> queue = new LinkedList<Vertex>();
		Vertex init = DeterministicDirectedSparseGraph.findInitial(g);
		queue.add(0,init);
		int i=0;
		int j= queue.size();
		Set<Vertex> done = new HashSet<Vertex>();
		while(i<j){
			DirectedSparseVertex v = (DirectedSparseVertex)queue.get(i);
			done.add(v);
			Iterator succIt = v.getSuccessors().iterator();
			while(succIt.hasNext()){
				Vertex succ = (Vertex)succIt.next();
				if(!done.contains(succ))
					queue.add(succ);
			}
			j = queue.size();
			i++;
		}
		return queue;
	}

	/** Finds a vertex with a given property set to a specified value. 
	 * 
	 * @param property property to search
	 * @param value what it has to be set to, cannot be null
	 * @param g the graph to search in
	 * @return vertex found.
	 */
	public static Vertex findVertex(JUConstants property, Object value, Graph g){
		if (value == null)
			throw new IllegalArgumentException("value to search for cannot be null");
		
		Iterator<Vertex> vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext()){
			Vertex v = vertexIt.next();
			if(v.getUserDatum(property) == null)
				continue;
			if(v.getUserDatum(property).equals(value))
				return v;
		}
		return null;
	}

	/** Finds an initial state in a graph. Returns null if the initial state was not found.
	 * 
	 * @param g graph to search for an initial state in.
	 * @return initial vertex, null if not found.
	 */
	public static Vertex findInitial(Graph g){
		Iterator<Vertex> vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext()){
			Vertex v = vertexIt.next();
			if (isInitial(v))
				return v;
		}
		return null;
	}

	public static Set<Vertex> findVertices(JUConstants property, Object value, Graph g){
		Set<Vertex> vertices = new HashSet<Vertex>();
		Iterator<Vertex> vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext()){
			Vertex v = vertexIt.next();
			if(v.getUserDatum(property) == null)
				continue;
			if(v.getUserDatum(property).equals(value))
				vertices.add(v);
		}
		return vertices;
	}

	/** A fast graph copy, which only copies labels and accept labelling. Transition labels are cloned.
	 * This one only copies vertices which participate in transitions. 
	 */
	@SuppressWarnings("unchecked")
	public static DirectedSparseGraph copy(Graph g)
	{
		DirectedSparseGraph result = new DirectedSparseGraph();
		Map<VertexID,DeterministicVertex> newVertices = new TreeMap<VertexID,DeterministicVertex>();
		for(DirectedSparseEdge e:(Set<DirectedSparseEdge>)g.getEdges())
		{
			DeterministicVertex newSrc = DeterministicDirectedSparseGraph.copyVertex(newVertices,result,e.getSource()),
				newDst = DeterministicDirectedSparseGraph.copyVertex(newVertices, result, e.getDest());
			DirectedSparseEdge newEdge = new DirectedSparseEdge(newSrc,newDst);
			newEdge.addUserDatum(JUConstants.LABEL, ((HashSet<String>)e.getUserDatum(JUConstants.LABEL)).clone(), UserData.SHARED);
			result.addEdge(newEdge);
		}
		return result;
	}

	/** Returns an array of sequential numbers 0..howMany, randomly permuted. 
	 * 
	 * @param howMany how many numbers to return (and hence the length of the array.
	 * @param seed the seed for the random generator
	 * @return the array of numbers.
	 */ 
	public static int [] getNonRepeatingNumbers(int howMany, int seed)
	{
    	int newNumbers[] = new int[howMany];
    	for(int i=0;i < howMany;++i) newNumbers[i]=i;
    	Random rnd = new Random(seed);
    	for(int i=0;i < howMany;++i) 
    	{ 
    		int a = rnd.nextInt(howMany),b=rnd.nextInt(howMany);
    		int value = newNumbers[a];newNumbers[a]=newNumbers[b];newNumbers[b]=value;
    	}
    	
    	return newNumbers;
	}
}
