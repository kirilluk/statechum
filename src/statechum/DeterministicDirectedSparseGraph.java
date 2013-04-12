/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.collections.ConvertibleToInt;

import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

final public class DeterministicDirectedSparseGraph {

	/** An identity for vertices. */
	public static interface VertID extends Comparable<VertID>, ConvertibleToInt
	{
		/** A kind of a state identifier - this is not the same as state label accept/reject/none; the two are different
		 * because I may choose to give uniform IDs to all states however some have to be accept and others - rejects.
		 * 
		 * <ul>
		 * <li>NONE is for states which only have string IDs.</li>
		 * <li>NEUTRAL is used if I wish to give uniform IDs to accept and reject states. 
		 * This is useful if I would like not to consider all accept states before reject
		 * states and such - this is useful to test how sensitive the learner is to such
		 * an ordering. Note that configuration has an option to add 1 to scores from non-reject state pairs
		 * which largely does the same thing. 
		 * Presently, NEUTRAL vertices are generated by the ID parser and integer identifiers are assigned to them
		 * if labels are shorter than 5 characters. The purpose of this is to handle such vertices in array-based
		 * transition matrices where each vertex & label has to have a unique integer ID.
		 * </li>
		 * <li>NEGATIVE means that this is a reject-state.</li>
		 * <li>POSITIVE means that this is an accept-state.</li>
		 * <li>NONEXISTING means that this is a state introduced in order to record paths from questions which
		 * do not exist in a tentative automaton.
		 * </ul>
		 */
		public enum VertKind { NEGATIVE, POSITIVE, NONEXISTING, NEUTRAL, NONE }
		
		/** Returns the kind of this ID. */
		public VertKind getKind();
		
		/** In order to enable comparison between vertex ID which are 
		 * represented by Strings and/or integer, 
		 * the two need need to be converted to a common form.
		 */
		public String getStringId();
		
		public int getIntegerID();
	}
	
	public static class VertexID implements VertID, Serializable
	{
		/**
		 * ID for serialization
		 */
		private static final long serialVersionUID = -6367197525198958482L;

		
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
			idString = null;kind = VertKind.NONE;idInteger=JUConstants.intUNKNOWN;cachedHash=0;
		}
		
		/** Returns the kind of this ID. */
		@Override
		public VertKind getKind()
		{
			return kind;
		}
		
		/** Computes the hash code, used during construction of the object. */
		private int computeHash()
		{
			if (kind == VertKind.NONE)
				return ~idString.hashCode();
			return
				hashFromID(kind, idInteger);
		}
		
		protected VertexID(String id, VertKind k, int intID)
		{
			if (id == null) throw new IllegalArgumentException("invalid id");
			idString = id;kind=k;idInteger=intID;
			cachedHash = computeHash(); // aims to ensure that "P5" and positive 5 have different hashes since we now consider them different.
		}
		
		public VertexID(String id)
		{
			this(id,VertKind.NONE,0);
		}
		
		public VertexID(VertID id)
		{
			if (id == null) throw new IllegalArgumentException("invalid id");
			kind = id.getKind();
			if (kind == VertKind.NONE)
				idInteger = 0;
			else
				idInteger=id.getIntegerID();
			idString=id.getStringId();
			cachedHash=computeHash();
		}
		
		/** When XML files are loaded, IDs are always textual, however we'd like
		/* to convert them to numbers in order to prevent nextID from returning
		/* identifies which when converted to strings are the same as existing 
		/* identifiers.
		 */
		public static VertexID parseID(String text)
		{
			VertKind tentativeKind = VertKind.NONE;
			int id = JUConstants.intUNKNOWN;
			VertexID outcome = null;
			
			if (text.length() > 1)
			{// Hash codes are assigned with the lowest bit not set. This permits clashes to be resolved with a limited amount of additional search, thus it is possible to mix string and integer IDs without a significant collision penalty.
				if (text.charAt(0) == 'P')
				{
					tentativeKind = VertKind.POSITIVE;
				}
				else if (text.charAt(0) == 'N')
				{
					tentativeKind = VertKind.NEGATIVE;
				}
				else if (text.charAt(0) == '+')
				{
					tentativeKind = VertKind.NONEXISTING;
				}
				else if (text.charAt(0) == 'V')
				{
					tentativeKind = VertKind.NEUTRAL;
				}
				
				if (tentativeKind != VertKind.NONE)
				{
					try
					{
							id = Integer.parseInt(text.substring(1));
							outcome = new VertexID(tentativeKind,id);
					}
					catch(NumberFormatException ex)
					{// cannot parse, revert to text.
						tentativeKind = VertKind.NONE;
					}
				}
			}

			if (outcome == null && text.length() > 0 && text.length() < 5)
			{// where the label is a short text, convert it to an integer, at most 4 digits because the outcome has to fit within 31 bit.
				int idInt = 0,step=1;
				final int zero = 32, max=128;
				for(int i=0;i<text.length();++i)
				{
					char ch = text.charAt(i);assert ch >= zero && ch < max;
					idInt+=ch*step;step*=max-zero;
				}
				outcome = new VertexID(text,VertKind.NEUTRAL,idInt);
			}
			
			if (outcome == null)
				outcome = new VertexID(text);
			return outcome;
		}
		
		/** Computes hash code from numerical ID. When vertices are added to a graph, their IDs 
		 * increase linearly, both positive and negative IDs. This method intended in order 
		 * to subvert a hash table to look like an array lookup of vertices by IDs.
		 * 
		 * @param kind
		 * @param id
		 * @return
		 */
		public static int hashFromID(VertKind kind, int id)
		{
			int value = 0;
			switch(kind)
			{
			case POSITIVE:value = 0;break;
			case NEGATIVE:value = 2;break;
			case NONEXISTING:value=4;break;
			default:value=6;
			}
			
			return value | (id << 3);
		}
		
		@Override
		public int getIntegerID()
		{
			if (kind == VertKind.NONE)
				throw new IllegalArgumentException("ID without an integer kind");
			return idInteger;
		}
		
		@Override
		public int toInt()
		{
			if (kind == VertKind.POSITIVE || kind == VertKind.NEUTRAL || kind == VertKind.NONEXISTING)
				return idInteger;
			else
				if (kind == VertKind.NEGATIVE)
					return -idInteger;
				
			throw new IllegalArgumentException("ID "+kind+" in vertex ID "+toString()+" is neither a positive nor a negative");
		}
		
		protected static final String initID = "Init"; 
		
		/** In order to enable comparison between vertex ID which are 
		 * represented by Strings and/or integer, 
		 * the two need need to be converted to a common form.
		 */
		@Override
		public String getStringId()
		{
			String result = null;
			switch(kind)
			{
			case NEGATIVE:
				result = "N"+idInteger;break;
			case POSITIVE:
				result = "P"+idInteger;break;
			case NEUTRAL:
				if (idString == null)
					result = "V"+idInteger;
				else
					result = idString;
				break;
			case NONEXISTING:
				result = "+"+idInteger;break;
			case NONE:
				result = idString;
			}
			
			return result;
		}
		
		
		/** This is a special case constructor to set not only ID details but also the hashCode (which is declared final). Used to ensure that where all vertices fit in an array, a hashcode
		 * can be used to map them to the appropriate slot. 
		 * 
		 * @param k type of vertex
		 * @param i identification
		 * @param hashCode hash code to use for this vertex, often set to i.
		 */
		@SuppressWarnings("unused")
		private VertexID(VertKind k, int i, int hashCode)
		{
			if (k == VertKind.NONE) throw new IllegalArgumentException("invalid id kind");
			idString = null;kind = k;idInteger=i;
			cachedHash = hashCode;
			assignStringID_ifNeeded();
		}

		public VertexID(VertKind k, int i)
		{
			if (k == VertKind.NONE) throw new IllegalArgumentException("invalid id kind");
			idString = null;kind = k;idInteger=i;
			assignStringID_ifNeeded();
			cachedHash = hashFromID(k, i);
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
		public enum ComparisonKind { COMPARISON_NORM, COMPARISON_LEXICOGRAPHIC_ORIG }
		
		/** Determines the behaviour of <em>compareTo</em>. This has to be static and sadly public.
		 * I could've passed an extra member from LearnerGraph or such which would determine this,
		 * but this way seems more appropriate for occasional experiments.
		 */
		public static ComparisonKind comparisonKind = ComparisonKind.COMPARISON_NORM;
		
		@Override
		public int compareTo(VertID vertID) {
			if (comparisonKind == ComparisonKind.COMPARISON_LEXICOGRAPHIC_ORIG) // Dec2007 compatibility mode
				return idString.compareTo(vertID.getStringId());
			
			// normal comparison mode
			if (kind != VertKind.NONE && vertID.getKind() != VertKind.NONE)
			{// if both this one and the other ID are numeric, use a numeric comparison.
				int kindDifference = kind.compareTo(vertID.getKind());
				if (kindDifference != 0)
					return kindDifference;
				
				if (idInteger < vertID.getIntegerID())
					return -1;
				if (idInteger > vertID.getIntegerID())
					return 1;
				return 0;				
			}
			
			int kindDifference = kind.compareTo(vertID.getKind());
			if (kindDifference != 0) // we should never compare textual representation of numbers to the actual numbers, because 
				// it is easy to get into a nontransitive situation where Init>P39>N1017>Init (Statechum version 338)
				return kindDifference;

			int lenDifference = idString.length() - vertID.getStringId().length();
			if (lenDifference !=0)
				return lenDifference;
			return idString.compareTo(vertID.getStringId());
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
			if (!(obj instanceof VertID))
				return false;
			final VertID other = (VertID) obj;
			
			if (kind != other.getKind()) // never compare integers with strings
				return false;

			if (kind != VertKind.NONE && other.getKind() != VertKind.NONE)
				return idInteger == other.getIntegerID();
			
			return idString.equals(other.getStringId());
		}
		
	}
	
	/** These are expected to be compared for equality using acceptance and IDs only,
	 * not highlight or colour. The {@link AbstractLearnerGraph#findVertex(VertexID)} method relies on this property to efficiently find a vertex with the supplied identifier. 
	 */
	public interface CmpVertex extends VertID
	{
		/** Returns true if this is an accept vertex and false for a reject one. */
		boolean isAccept();
		/** Makes this state an accept/reject one. */
		void setAccept(boolean accept);

		/** Returns the colour of this vertex. */
		JUConstants getColour();
		/** Sets the colour of this vertex. null removes the colour. */
		void setColour(JUConstants colour);

		/** Returns the name of a vertex in hard facts corresponding to this vertex. Since one
		 * would wish to avoid introducing dependencies between graphs, this is a {@link VertID} rather
		 * than a <em>CmpVertex</em>.<p> 
		 * Unknown if set to null. 
		 */
		VertID getOrigState();
		
		/** Sets the name of a vertex in hard facts corresponding to this vertex. Can be null.
		 * 
		 * @param newState new name.
		 */
		void setOrigState(VertID newState);
		
		/** Returns the distance from the root state in the state corresponding to this state in hard facts.
		 * An inverse of this value is used as a priority when deciding on a state to represent an equivalence class
		 * during state merging.
		 * <p>A value of <em>JUConstants.intUNKNOWN</em> means "unknown".
		 * 
		 * @return distance from an original state corresponding to this state in hard facts and the root state (in hard facts).
		 */
		int getDepth();
		
		/** Sets a new depth value. 
		 * 
		 * @param depth value to set.
		 */
		void setDepth(int depth);
			
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
	public final static class DeterministicVertex extends DirectedSparseVertex implements CmpVertex 
	{
		private VertID vertexID = null;

		private int hashCode = super.hashCode();

		public DeterministicVertex(VertID thisVertexID) {
			super();
			addUserDatum(JUConstants.LABEL, thisVertexID, UserData.SHARED);
		}

		public DeterministicVertex(String name)
		{
			super();
			addUserDatum(JUConstants.LABEL, 
					DeterministicDirectedSparseGraph.VertexID.parseID(name), UserData.SHARED);
		}
		
		/** Used to hold two objects and nothing else. */
		public static final class MiniPair
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
				else
				if (key == JUConstants.ORIGSTATE || strKey.equalsIgnoreCase(JUConstants.ORIGSTATE.name()))
				{
					key = JUConstants.ORIGSTATE;
					if (datum instanceof String) datum = VertexID.parseID(strDatum);
					else if (!(datum instanceof VertexID)) throw new DeterministicDirectedSparseGraph.CmpVertex.IllegalUserDataException(strDatum);
						
				}
				else
				if (key == JUConstants.DEPTH || strKey.equalsIgnoreCase(JUConstants.DEPTH.name()))
				{
					key = JUConstants.DEPTH;datum = getInteger(datum);
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
				vertexID = (VertID) p.getValue();
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
		
		public static Integer getInteger(Object obj)
		{
			if (obj instanceof Integer)
				return (Integer)obj;
			
			Integer result = null;
			try
			{ result = Integer.valueOf(obj.toString()); }
			catch(NumberFormatException ex)
			{
				throw new IllegalUserDataException("invalid number "+obj.toString());
			}
			return result;
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
			
			return labelHashCode;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see edu.uci.ics.jung.graph.impl.AbstractSparseVertex#toString()
		 */
		@Override
		public String toString() 
		{
			String origName = "";if (getOrigState() != null) origName=" <"+getOrigState()+">";
			String strDepth="";if(getDepth()!=JUConstants.intUNKNOWN) strDepth=" depth="+getDepth();
			String strColour="";if(getColour()!=null) strColour=" colour="+getColour();
			return vertexID == null?"NULL":vertexID.toString()+origName+strDepth+strColour;
		}

		/** The ordering is based on names only ignoring whether this is an
		 * accept or a reject vertex. This is necessary if we wish to adjust
		 * an order of traversal in experiments. In cases where accepts or
		 * rejects should appear earlier/later, the <i>nextID</i> method
		 * will generate the appropriate number. 
		 */
		@Override 
		public int compareTo(VertID otherID) {
			return vertexID.compareTo(otherID);
		}
		
		/** Compares this vertex with a different one, based on label alone.
		 */

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			return vertexID.equals(obj);
		}

		@Override 
		public boolean isAccept() {
			return DeterministicDirectedSparseGraph.isAccept(this);
		}

		@Override 
		public void setAccept(boolean accept) 
		{
			removeUserDatum(JUConstants.ACCEPTED);
			addUserDatum(JUConstants.ACCEPTED, accept, UserData.SHARED);
		}

		@Override 
		public JUConstants getColour() 
		{
			return (JUConstants)getUserDatum(JUConstants.COLOUR);
		}

		@Override 
		public void setColour(JUConstants c) 
		{
			if (c != null && c != JUConstants.RED && c != JUConstants.BLUE && c != JUConstants.AMBER && c != JUConstants.GRAY && c != JUConstants.INF_AMBER)
				throw new IllegalUserDataException("colour "+c+" is not a valid colour (vertex "+vertexID.toString()+")");

			removeUserDatum(JUConstants.COLOUR);
			if (c != null)
				addUserDatum(JUConstants.COLOUR, c, UserData.SHARED);
		}

		@Override 
		public void setHighlight(boolean hightlight) {
			removeUserDatum(JUConstants.HIGHLIGHT);
			if (hightlight)
				addUserDatum(JUConstants.HIGHLIGHT, true, UserData.SHARED);
		}

		@Override 
		public boolean isHighlight() {
			Object highlight = getUserDatum(JUConstants.HIGHLIGHT);
			if (highlight == null)
				return false;
			
			return (Boolean)highlight;
		}

		@Override 
		public int getDepth() {
			Object depthObject = getUserDatum(JUConstants.DEPTH);
			if (depthObject == null)
				return JUConstants.intUNKNOWN;
			return ((Integer)depthObject).intValue();
		}

		@Override 
		public VertexID getOrigState() {
			return (VertexID)getUserDatum(JUConstants.ORIGSTATE);
		}

		@Override 
		public void setDepth(int argDepth)
		{
			removeUserDatum(JUConstants.DEPTH);
			if (argDepth != JUConstants.intUNKNOWN)
				addUserDatum(JUConstants.DEPTH, argDepth, UserData.SHARED);
		}

		@Override 
		public void setOrigState(VertID newState) {
			removeUserDatum(JUConstants.ORIGSTATE);
			if (newState != null)
				addUserDatum(JUConstants.ORIGSTATE, new VertexID(newState), UserData.SHARED);
		}

		/** Delegates to the implementation of the ID. */
		@Override
		public VertKind getKind() {
			return vertexID.getKind();
		}

		/** Delegates to the implementation of the ID. */
		@Override
		public int getIntegerID() {
			return vertexID.getIntegerID();
		}

		/** Delegates to the implementation of the ID. */
		@Override
		public String getStringId() {
			return vertexID.getStringId();
		}

		@Override
		public int toInt() {
			return vertexID.toInt();
		}
	}

	public final static class DeterministicEdge extends DirectedSparseEdge {

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
	@SuppressWarnings("unchecked")
	public static Set<Label> computeAlphabet(DirectedSparseGraph g)
	{
		Set<Label> alphabet = new TreeSet<Label>();
	
		for(Edge e:(Set<Edge>)g.getEdges())
				alphabet.addAll( (Set<Label>)e.getUserDatum(JUConstants.LABEL) );
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
	public static DeterministicVertex copyVertex(Map<VertID,DeterministicVertex> newVertices, DirectedSparseGraph g,Vertex orig)
	{
		if (!(orig instanceof DeterministicVertex))
			throw new IllegalArgumentException("cannot copy a graph which is not known to be built out of deterministic elements");
		DeterministicVertex origVertex = (DeterministicVertex)orig;
		VertID vertID = origVertex;
		DeterministicVertex newVertex = newVertices.get(vertID);
		if (newVertex == null) { 
			newVertex = new DeterministicVertex(vertID);
			if (DeterministicDirectedSparseGraph.isInitial(origVertex))
				newVertex.addUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
			DeterministicDirectedSparseGraph.copyVertexData(origVertex, newVertex);
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
	@SuppressWarnings("unchecked")
	public static boolean completeGraph(DirectedSparseGraph g, String reject)
	{
		DirectedSparseVertex rejectVertex = new DirectedSparseVertex();
		boolean transitionsToBeAdded = false;// whether and new transitions have to be added.
		rejectVertex.addUserDatum(JUConstants.ACCEPTED, false, UserData.SHARED);
		rejectVertex.addUserDatum(JUConstants.LABEL, reject, UserData.SHARED);
		
		// first pass - computing an alphabet
		Set<Label> alphabet = computeAlphabet(g);
		
		// second pass - checking if any transitions need to be added.
		Set<Label> outLabels = new HashSet<Label>();
		Iterator<Vertex> vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext() && !transitionsToBeAdded)
		{
			Vertex v = vertexIt.next();
			outLabels.clear();
			Iterator<DirectedSparseEdge>outEdgeIt = v.getOutEdges().iterator();
			while(outEdgeIt.hasNext()){
				DirectedSparseEdge outEdge = outEdgeIt.next();
				outLabels.addAll( (Set<Label>)outEdge.getUserDatum(JUConstants.LABEL) );
			}
			transitionsToBeAdded = !alphabet.equals(outLabels);
		}
		
		if (transitionsToBeAdded)
		{
			// third pass - adding transitions
			g.addVertex(rejectVertex);
			vertexIt = g.getVertices().iterator();
			while(vertexIt.hasNext())
			{
				Vertex v = vertexIt.next();
				if (v != rejectVertex)
				{// no transitions should start from the reject vertex
					Set<Label> outgoingLabels = new TreeSet<Label>();outgoingLabels.addAll(alphabet);
					
					Iterator<DirectedSparseEdge>outEdgeIt = v.getOutEdges().iterator();
					while(outEdgeIt.hasNext()){
						DirectedSparseEdge outEdge = outEdgeIt.next();
						outgoingLabels.removeAll( (Set<Label>)outEdge.getUserDatum(JUConstants.LABEL) );
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
			@SuppressWarnings("unchecked")
			Iterator<Vertex> succIt = v.getSuccessors().iterator();
			while(succIt.hasNext()){
				Vertex succ = succIt.next();
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
		
		@SuppressWarnings("unchecked")
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
		@SuppressWarnings("unchecked")
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
		@SuppressWarnings("unchecked")
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
		Map<VertID,DeterministicVertex> newVertices = new TreeMap<VertID,DeterministicVertex>();
		for(DirectedSparseEdge e:(Set<DirectedSparseEdge>)g.getEdges())
		{
			DeterministicVertex newSrc = DeterministicDirectedSparseGraph.copyVertex(newVertices,result,e.getSource()),
				newDst = DeterministicDirectedSparseGraph.copyVertex(newVertices, result, e.getDest());
			DirectedSparseEdge newEdge = new DirectedSparseEdge(newSrc,newDst);
			Set<Label> origLabels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
			TreeSet<Label> copiedLabels = new TreeSet<Label>();copiedLabels.addAll(origLabels);
			newEdge.addUserDatum(JUConstants.LABEL, copiedLabels, UserData.SHARED);
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
	
	/** This equality operation compares all attributes except IDs. */
	public static boolean deepEquals(CmpVertex thisVertex, CmpVertex vert) 
	{
		if (!thisVertex.equals(vert))// shallow equals
			return false;
		
		return nonIDAttributesEquals(thisVertex, vert);
	}
	
	/** This equality operation compares not only IDs but also all attributes. */
	public static boolean nonIDAttributesEquals(CmpVertex thisVertex, CmpVertex vert) 
	{
		JUConstants colour = thisVertex.getColour();
		if (colour == null)
		{
			if (vert.getColour() != null)
				return false;
		}
		else
			if (colour != vert.getColour())
				return false;
		
		if (thisVertex.isAccept() != vert.isAccept()) return false;
		if (thisVertex.isHighlight() != vert.isHighlight()) return false;
		VertID origVert = thisVertex.getOrigState();
		if (origVert == null)
		{
			if (vert.getOrigState() != null)
				return false;
		}
		else
			if (!origVert.equals(vert.getOrigState()))
				return false;
		
		if (thisVertex.getDepth() != vert.getDepth()) return false;
		return true;
	}

	/** Copies all attributes from one vertex into another one.
	 * 
	 * @param from vertex to copy attributes from
	 * @param to vertex to assign attribute values to.
	 */
	public static void copyVertexData(CmpVertex from, CmpVertex to)
	{
		to.setColour(from.getColour());
		to.setAccept(from.isAccept());
		to.setHighlight(from.isHighlight());
		to.setOrigState(from.getOrigState());
		to.setDepth(from.getDepth());
	}
	
	public static Edge findEdge(Vertex from, Vertex to){
		@SuppressWarnings("unchecked")
		Iterator<DirectedSparseEdge> edgesOut = from.getOutEdges().iterator();
		while(edgesOut.hasNext()){
			DirectedSparseEdge current = edgesOut.next();
			if(current.getDest().equals(to))
				return current;
		}
		return null;
	}
}
