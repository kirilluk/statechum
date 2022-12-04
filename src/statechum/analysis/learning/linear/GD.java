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

package statechum.analysis.learning.linear;

import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import statechum.*;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Label;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.linear.GDLearnerGraph.*;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.*;
import statechum.analysis.learning.rpnicore.PathRoutines.EdgeAnnotation;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.collections.MapWithSearch;

import java.awt.*;
import java.text.DateFormat;
import java.util.List;
import java.util.*;
import java.util.Map.Entry;

/**
 * @author kirill
 *
 */
public class GD<TARGET_A_TYPE,TARGET_B_TYPE,
	CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
	CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>>
{
	/** Gives numbering to pairs to be considered in Linear and marks 
	 * pairs to be ignored (those part of the state space of the combined
	 * machine which are not member of the cross-product of states of 
	 * machines A and B.
	 */ 
	int [] pairScores;
	
	/** Compatibility scores for states obtained by scanning forwards and backwards. */
	double []scoresForward=null, scoresInverse=null;
	
	/** The coefficient to multiply scores by before converting to int in order to
	 * fit them into <em>PairScore</em>.
	 */
	final double multiplier = 10;

	/** The front wave. */
	List<PairScore> frontWave = null;
	
	/** The front wave. */
	List<PairScore> currentWave = null;

	/** A collection of states which have already been used in key pairs and hence cannot 
	 * be used in any other key pairs.
	 */
	Set<CmpVertex> statesInKeyPairs = null;
	
	/** In order to run linear and others, I need to combine the two graphs into a single one,
	 * this is the combined graph. It is easy to find out which states of this graph belong to 
	 * the first or the second graph, see <em>stateToNumber</em> and <em>statesOfB</em>.
	 */
	LearnerGraphND grCombined = null;
	
	/** Collection of states in the combined graph which belong to the first graph. */
	Set<CmpVertex> statesOfA = null;

	/** Collection of states in the combined graph which belong to the second graph. */
	Set<CmpVertex> statesOfB = null;
	
	/** The initial state of A in the combined graph. */
	CmpVertex combined_initA = null;
	
	/** The initial state of B in the combined graph. */
	CmpVertex combined_initB = null;

	/** Maps vertices of the right-hand side of key pairs in the combined graph to vertices of the B graph. */
	Map<CmpVertex,CmpVertex> newBToOrig = null;
	
	/** The inverse of newBToOrig. */
	Map<CmpVertex,CmpVertex> origToNewB = null;

	/** Forward matrix for exploration of <em>grCombined</em>. */
	GDLearnerGraph forward = null;
	
	/** Inverse matrix for exploration of <em>grCombined</em>. */
	GDLearnerGraph inverse = null;

	/** States which are not matched between the two graphs but have shared names between the two graphs
	 * and thus have to be renamed.
	 */
	Set<CmpVertex> duplicates = null;
	
	/** Maps key states of A to the corresponding ones in the B part of grCombined. */
	Map<CmpVertex,CmpVertex> aTOb = null;// this is a replica of the key pair waves, needed for aTOb.get()

	/** Number of threads to use in a computation. */
	int ThreadNumber = 0;
	
	/** Converts the supplied array to the corresponding Erlang representation. */
	private static OtpErlangList serialiseDoubleArray(double []array)
	{
		if (array == null)
			return new OtpErlangList();// empty list
		OtpErlangDouble[] convertedArray = new OtpErlangDouble[array.length];for(int i = 0; i<array.length; ++i) convertedArray[i]=new OtpErlangDouble(array[i]);
		return new OtpErlangList(convertedArray);
	}
	
	/** Returns a serializable representation of scores. */
	public OtpErlangTuple serialiseScores()
	{
		return new OtpErlangTuple(new OtpErlangObject[]{serialiseDoubleArray(scoresForward), serialiseDoubleArray(scoresInverse)});
	}
	
	/** Compares the supplied two graphs.
	 * 
	 * @param a first graph
	 * @param b second graph
	 * @param threads the number of threads to use
	 * @param doc XML document used to create nodes
	 * @param observer this one receives the difference.
	 * @param config configuration to use for computing a difference.
	 * @return XML node with GD.
	 */
	public Element computeGDToXML(AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> a,
			AbstractLearnerGraph<TARGET_B_TYPE,CACHE_B_TYPE> b, int threads, Document doc, PatchGraph observer, Configuration config)
	{
		ChangesRecorder patcher = new ChangesRecorder(observer);
		computeGD(a, b, threads, patcher,config);
		return patcher.writeGD(doc);
	}
	
	/** Compares the supplied two graphs.
	 * Important: the code in this method is also replicated in <em>runPatch</em> of <em>TestGD_ExistingGraphs</em> because 
	 * chosen pairs may have to be messed up for the purpose of testing. 
	 * 
	 * @param a first graph
	 * @param b second graph
	 * @param threads the number of threads to use
	 * @param patcher where to store changes.
	 * @param config configuration to use for computing a difference.
	 */
	public void computeGD(AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> a,
			AbstractLearnerGraph<TARGET_B_TYPE,CACHE_B_TYPE> b, int threads, PatchGraph patcher, Configuration config)
	{
		init(a, b, threads,config);
		identifyKeyPairs();
		makeSteps();computeDifference(patcher);
	}
	
	/** Describes primitive mutations which can be carried out on a graph. */
	public interface PatchGraph
	{
		/** By default, graphs are generated such that the outcome of applying a patch retains all 
		 * matched states of A and as many states of B (unmatched ones) as possible. This map
		 * permits one to relabel the target graph so that all target states carry the IDs they
		 * used to carry in B when the patch was constructed.
		 *   
		 * @param a ID of the vertex in the outcome of patching
		 * @param b the actual ID of the vertex.
		 */
		void addRelabelling(VertID a,VertID b);
		
		/** Adds a supplied vertex. Useful for adding vertices which are not connected anywhere or
		 * updating attributes on those which do exist.
		 * 
		 * @param vertex what to add or update.
		 */
		void addVertex(CmpVertex vertex);
		
		/** Adds a pair of compatible or incompatible states. See <em>AbstractLearnerGraph</em> for details.
		 *  
		 * @param a the first element of a pair to be added.
		 * @param b the second element of a pair.
		 * @param value the relation between <em>a</em> and <em>b</em>.
		 */
		void addToCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value);

		/** Removes a pair of compatible or incompatible states from the collection of incompatible states. 
		 * See <em>AbstractLearnerGraph</em> for details.
		 *  
		 * @param a the first element of a pair to be added.
		 * @param b the second element of a pair.
		 * @param value value to remove between <em>a</em> and <em>b</em>. This is useful where we wish for our adds to be cancelled out by removes.
		 */
		void removeFromCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value);

		/** Adds a transition between the specified states.
		 * Throws if transition already exists.<p>
		 * Important: adding transitions using a vertex with different attributes will not
		 * change attributes on an existing vertex, but adding a new vertex will use the attributes 
		 * from the supplied vertex. Use <em>addVertex</em> to update attributes.
		 * 
		 * @param from source state
		 * @param label label of a transition
		 * @param to target state
		 */
		void addTransition(CmpVertex from,Label label, CmpVertex to);

		/** Removes a transition between the specified states. Throws
		 * if a transition does not exist.
		 * 
		 * @param from source state
		 * @param label label of a transition
		 * @param to target state
		 */
		void removeTransition(CmpVertex from,Label label, CmpVertex to);
		
		/** Sets the initial state to the requested state.
		 * @param vertex the state to become the initial state. 
		 */
		void setInitial(CmpVertex vertex);
	}
	
	/** Contains an inverse of the <em>frontwave</em> map. */
	Map<CmpVertex,CmpVertex> newToOrig = null;
	
	public void printWave(Collection<PairScore> wave)
	{
		boolean initial = true;
		System.out.print("[");
		for(PairScore elem:wave) 
		{	
			if (!initial) System.out.print(", ");else initial=false;
			System.out.print(elem.getQ()+","+newBToOrig.get(elem.getR())+": "+elem.getScore());
		}
		System.out.println("]");
	}
	
	/** Expands the set of key pairs.
	 */
	protected void makeSteps()
	{
		// Now we make steps. Data used:
		//
		// currentWave is what we'll populate with candidates for key pairs 
		// (the main criterion is that these states are not part of existing
		// key pairs, i.e. not in statesInKeyPairs collection.
		//
		// frontWave is the wavefront from which we are exploring grCombined in
		// search of these candidates for key pairs.
		aTOb= new TreeMap<>();
		
		newToOrig = new TreeMap<>();
		do
		{
			// we start with a wave, see if determinism can be used to extend it
			if (grCombined.config.getGdPropagateDet())
				propagateDet(forward.matrixForward,inverse.matrixForward);

			// After extension via determinism, update the supporting collections
			// It is a rather wasteful to do it for a full collection here, would've been better
			// to do this every time new pair is added but in real terms, this part consumes little
			// time so it stays for the time being.
			for(PairScore pair:frontWave) 
			{
				CmpVertex origState=pair.getQ(),newState=pair.getR();

				newToOrig.put(newState,origState);// since we now know 
				// which state of A pair.getQ() of combined corresponds to, change the mapping.
				// addTransitions(grCombined,statesOfB,added,cloneConfig) relies on this change.
				assert AbstractLearnerGraph.checkCompatible(origState,newState,grCombined.pairCompatibility);

				aTOb.put(origState,newState);
				statesInKeyPairs.add(origState);statesInKeyPairs.add(newState);
				assert aTOb.size() == aTOb.values().size() : " duplicate right-hand side in key pairs";
			}
			
			// compute the new front from the existing out, the outcome is in currentWave
			currentWave.clear();
			populateCurrentWave(forward.matrixForward);
			if (!fallbackToInitialPair) populateCurrentWave(inverse.matrixForward);
			sortWave(currentWave);
			
			// now we need to walk through the tentative pairs in currentWave and select relevant ones to add to frontWave
			frontWave.clear();
			for(PairScore pair:currentWave)
				if (!statesInKeyPairs.contains(pair.getQ()) && !statesInKeyPairs.contains(pair.getR()) &&  // we can only consider a new pair if it does not share any states with existing key pairs
						AbstractLearnerGraph.checkCompatible(pair.getQ(), pair.getR(),grCombined.pairCompatibility)) // we should not merge incompatible pairs
				{// this is the one for the front line
					frontWave.add(pair);statesInKeyPairs.add(pair.getQ());statesInKeyPairs.add(pair.getR());
					/*
						if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LINEARWARNINGS)))
							for(PairScore similarPair:currentWave)
								if ((similarPair.getQ().equals(pair.getQ()) || similarPair.getR().equals(pair.getR())) && !similarPair.equals(pair))
								{
									int minScore = Math.min(similarPair.getScore(),pair.getScore());
									assert minScore >= 0;
									if (minScore == 0)
									System.out.println("Warning: for pairs "+pair+" and "+similarPair+" in the current wave min score is 0");
									else
									{
										if (Math.abs(similarPair.getScore()-pair.getScore()) < minScore/2)
											System.out.println("Warning: for pairs "+pair+" and "+similarPair+" there is very small difference in scores");
									}								
								}
					*/
				}
		}
		while(!frontWave.isEmpty());
	
	}

	/** Where we are adding a transition that has previously been marked for removal, the two requests should cancel each other. This one checks if a transition is in a specific graph.
	 * 
	 * @param graphToCheck graph to check for transition
	 * @param from source state
	 * @param label transition label
	 * @param to target state.
	 */
	public static boolean checkTransitionPresent(LearnerGraphND graphToCheck, CmpVertex from, Label label, CmpVertex to)
	{
		boolean transitionFound = false;
		Map<Label,List<CmpVertex>> rowInRemove = graphToCheck.transitionMatrix.get(from);
		if (rowInRemove != null)
		{
			List<CmpVertex> targets = rowInRemove.get(label);
			if (targets != null)
				// the check is not by identity because one of them could be a vertex of the left-hand 
				// side graph and the other - that of the right-hand one. They map to the same actual 
				// state but CmpVertex objects are different.
				{
					transitionFound = targets.contains(to);
				}
		}

		return transitionFound;
	}

	/** Where we are adding a compatibility annotation that has previously been marked for removal, the two requests should cancel each other. This one checks 
	 * if an annotation of a specific kind is present between the specific states.
	 * 
	 * @param graphToCheck graph to check for an annotation
	 * @param from source state
	 * @param to target state
	 * @param compat compatibility annotation.
	 */
	public static boolean checkCompatibilityAnnotationPresent(LearnerGraphND graphToCheck, CmpVertex from, CmpVertex to, PAIRCOMPATIBILITY compat)
	{
		boolean associationFound = false;
		Map<CmpVertex,JUConstants.PAIRCOMPATIBILITY> row = graphToCheck.pairCompatibility.compatibility.get(from);
		if (row != null)
		{
			PAIRCOMPATIBILITY annotation = row.get(to);
			associationFound = annotation == compat;
		}
		return associationFound;
	}
	
	/** Expands the set of key pairs and computes the outcome. 
	 * 
	 * @param graphToPatch this will be provided with changes necessary to transform the first graph
	 * to the second one. It can then be stored in XML if necessary.
	 */
	protected void computeDifference(final PatchGraph graphToPatch)
	{
		// Now find out which states need adding to A and check that we can actually do this
		// without name clashes.
		// The problem is that newBtoOrig maps states of the B part of grCombined
		// to the whole set of original states in A and B. The old version of GD was simply
		// using all newly-created states if there was any intersection detected between names of states in A and B.
		// It is not hard to find out which states of the B part of grCombined clash with 
		// states of A which remain after removing transitions. We could then use grCombine's 
		// vertices. The next problem is that these vertices may clash with vertices of B,
		// so that when mapping to the original vertices via newBtoA, we have to check 
		// for clashes both with remaining states of A and states of B portion of grCombined.
		// This is resolved below by ensuring that grCombined's B-vertex space does not intersect
		// with that of the B graph by construction of grCombined.

		final Set<CmpVertex> duplicatesAB = new TreeSet<>(aTOb.keySet());
		duplicatesAB.retainAll(newBToOrig.values());// throws away all states not in B, such as states in A's key pairs which are not in B and hence cannot be duplicates
		for(Entry<CmpVertex,CmpVertex> entry:aTOb.entrySet()) duplicatesAB.remove(newBToOrig.get(entry.getValue()));// throws all states of B which are B's key pairs - these are paired with A and hence cannot be conflicting.
		// now we got duplicate states in terms of states of A and B, but duplicates are in terms of renamed states of B in grCombined, hence convert it.
		duplicates = new TreeSet<>();for(CmpVertex vertex:duplicatesAB) duplicates.add(origToNewB.get(vertex));

		if (!duplicates.isEmpty())
		{// duplicates state names found, hence use the unique names the corresponding states were given in grCombined (given via addToGraph)
			if (grCombined.config.getGdFailOnDuplicateNames()) throw new IllegalArgumentException("names of states "+duplicates+" are shared between A and B");
		}
		
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
		final LearnerGraphND added = new LearnerGraphND(config),removed = new LearnerGraphND(config);
		added.initEmpty();removed.initEmpty();// to make sure we can handle an assignment of a reject-state to an initial state

		final Set<CmpVertex> extraVertices = new TreeSet<>();
		
		// Now collect the changes, where vertices to be added can cancel out with those to be removed. This is needed because 
		// there could be transitions between key pairs in A such that there are transitions between vertices with the same 
		// names in B that are part of other pairs. When we calculate which transitions between key pairs to add or remove, 
		// it is difficult to take this into account on the fly. This is why a separate pass is done. 
		new DCollector()
		{
			@Override
			public void addTransition(CmpVertex from, Label label, CmpVertex to) {
				super.addTransition(from, label, to);
				MapWithSearch<Label,Label,List<CmpVertex>> rowInAdd = added.transitionMatrix.get(from);
				if (rowInAdd == null)
				{
					rowInAdd = added.createNewRow();added.transitionMatrix.put(from, rowInAdd);
				}
				added.addTransition(rowInAdd, label, to);
			}

			@Override
			public void removeTransition(CmpVertex from, Label label, CmpVertex to) {
				super.removeTransition(from, label, to);
				MapWithSearch<Label,Label,List<CmpVertex>> rowInAdd = removed.transitionMatrix.get(from);
				if (rowInAdd == null)
				{
					rowInAdd = removed.createNewRow();removed.transitionMatrix.put(from, rowInAdd);
				}
				removed.addTransition(rowInAdd, label, to);
			}

			@Override
			public void setInitial(CmpVertex vertex) {
				graphToPatch.setInitial(vertex);
			}

			@Override
			public void addToCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
				super.addToCompatibility(a,b,value);
				added.addToCompatibility(a, b, value);
			}

			@Override
			public void addVertex(CmpVertex vertex) {
				extraVertices.add(vertex);
			}

			@Override
			public void removeFromCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
				super.removeFromCompatibility(a,b,value);
				removed.addToCompatibility(a, b, value);
			}

			@Override
			public void addRelabelling(VertID a, VertID b) {
				graphToPatch.addRelabelling(a, b);
			}
			
		}.computeGD();
		
		// Second phase - collect transitions that have not been cancelled out.
		// The vertex-removal phase keeps all states that are part of transitions (indirectly, 
		// those that are to be added or part of associations to be added). 
		// Hence we may only remove vertices from extraVertices for transitions and/or associations that are to be added.
		
		// The purpose of checkTransitionPresent and checkCompatibilityAnnotationPresent is to eliminate duplicate 
		// transitions (where a transition between the same pair of states is removed and then added). This happens 
		// in two cases, 
		// (a) where a transition between unmatched states of A is removed and subsequently added between states of B
		// (b) where a transition between unmatched states of A is removed but these states correspond to key states of B 
		// that contain a transition that is missing between the components of those keys states in A and hence is added.
		for(Entry<CmpVertex,MapWithSearch<Label,Label,List<CmpVertex>>> entry:removed.transitionMatrix.entrySet())
			for(Entry<Label,List<CmpVertex>> transition:entry.getValue().entrySet())
				for(CmpVertex target:removed.getTargets(transition.getValue()))
					if (!checkTransitionPresent(added, entry.getKey(), transition.getKey(), target))
						graphToPatch.removeTransition(entry.getKey(), transition.getKey(), target);

		for(Entry<CmpVertex,MapWithSearch<Label,Label,List<CmpVertex>>> entry:added.transitionMatrix.entrySet())
			for(Entry<Label,List<CmpVertex>> transition:entry.getValue().entrySet())
				for(CmpVertex target:added.getTargets(transition.getValue()))
					if (!checkTransitionPresent(removed, entry.getKey(), transition.getKey(), target))
					{
						graphToPatch.addTransition(entry.getKey(), transition.getKey(), target);
						extraVertices.remove(entry.getKey());
						extraVertices.remove(target);
					}

		// Now the same as above but for pairwise compatibility information
		Set<CmpVertex> verticesSeen = new TreeSet<>();
		for(Entry<CmpVertex,Map<CmpVertex,PAIRCOMPATIBILITY>> entry:removed.pairCompatibility.compatibility.entrySet())
		{
			for(Entry<CmpVertex,PAIRCOMPATIBILITY> vertexComp:entry.getValue().entrySet())
				if (!verticesSeen.contains(vertexComp.getKey()) && !checkCompatibilityAnnotationPresent(added, entry.getKey(), vertexComp.getKey(), vertexComp.getValue()))
					graphToPatch.removeFromCompatibility(entry.getKey(), vertexComp.getKey(), vertexComp.getValue());

			verticesSeen.add(entry.getKey());
		}
		
		verticesSeen.clear();
		for(Entry<CmpVertex,Map<CmpVertex,PAIRCOMPATIBILITY>> entry:added.pairCompatibility.compatibility.entrySet())
		{
			for(Entry<CmpVertex,PAIRCOMPATIBILITY> vertexComp:entry.getValue().entrySet())
				if (!verticesSeen.contains(vertexComp.getKey()) && !checkCompatibilityAnnotationPresent(removed, entry.getKey(), vertexComp.getKey(), vertexComp.getValue()))
				{
					extraVertices.remove(entry.getKey());extraVertices.remove(vertexComp.getKey());// we only process a single relation once due to symmetry hence both vertices have to be removed from a list of those to be explicitly added.
					graphToPatch.addToCompatibility(entry.getKey(), vertexComp.getKey(), vertexComp.getValue());
				}
			verticesSeen.add(entry.getKey());
		}
		
		// Third phase - record vertices that either changed attributes but did not feature in any added or removed transitions, or those that are disconnected and hence could not ever feature in them.
		for(CmpVertex vert:extraVertices)
			graphToPatch.addVertex(vert);
	}

	/** We need to do two things,
	 * <ul>
	 * <li>Iterate through the transitions of the combined graph in order to find out which 
	 * of them need removing and which new transitions to add.
	 * </li>
	 * <li>Collect the states of B to be added to A, these correspond to those which 
	 * were not matched to any states of A. This has to be done in advance because
	 * we'd like to ensure the set of these states does not intersect with states already
	 * in A, but without the above traversal it cannot be done since we need to know which
	 * states have been removed from A. The only problem is that without updating an existing
	 * transition matrix we cannot find out which states will remain after removing 
	 * and elimination of dangling states.
	 * </li></ul>
	 * For this reason, we start by doing a dummy run aimed at identification of states 
	 * which are to be removed and those to be added. The former is done by manipulating 
	 * a clone of grCombined and the latter
	 */
	protected abstract class DCollector implements PatchGraph
	{
		
		/** Called when we need to map a vertex in the B-part of grCombined to 
		 * an original vertex of A, if possible. Where the supplied vertex is uniquely new to B, its original name in B will be returned.
		 * 
		 * @param vertex vertex to map
		 */
		protected final CmpVertex getOrig(CmpVertex vertex) 
		{
			CmpVertex keyVertex = newToOrig.get(vertex);// reverse-lookup of the keypairs map.
			if (keyVertex != null) return keyVertex;
			
			if (duplicates.contains(vertex))
				return vertex;// duplicate vertices retain their new identifiers
			
			return newBToOrig.get(vertex);
		}
		
		public void computeGD()
		{
			/* States to be explicitly added are those that do not feature in any added or removed transitions and have to be added for two reasons,
			 * <ul>
			 * <li>attributes on these states may have changed or</li> 
			 * <li>these states feature no outgoing/incoming transitions
			 * and hence will be dropped from the original graph when we clean up the old states after patch
			 * application.</li>
			 * </ul>
			 * The case of attribute change applies to states of A that also exist in B. The two have the same names unless the corresponding vertex of A is part of a key pair
			 * in which case we have to apply aTob mapping to them. Usually, such states are part of transitions being added or removed, but where a state is unconnected or part of a key pair that is perfectly matched
			 * it will not feature in a diff and we have to ensure attribute change is recorded.
			 * 
			 * Where there are disconnected states (either part of key pairs or unmatched ones), these will be dropped after all unconnected vertices left after removal of transitions are eliminated.
			 * Such states also have to be explicitly added.
			 */
			
			// The initial state should be either combined_initB (which is the initial state of graph B)
			// or a key state of graph A which corresponds to this state. Method getOrig performs this computation.
			CmpVertex initialState = getOrig(combined_initB);
			// Now we have to copy attributes from vertices of B to the their replicas in A because when a key pair is matched and we add a "remove transition" entry,
			// it will otherwise use old attributes. In the absence of a subsequently "add transition" attributes will not be preserved.
			for(CmpVertex vertex:statesOfA)
			{
				if (aTOb.containsKey(vertex))
				{// we are considering a key state
					if (!DeterministicDirectedSparseGraph.nonIDAttributesEquals(aTOb.get(vertex), vertex))
					{
						addVertex(vertex);
						DeterministicDirectedSparseGraph.copyVertexData(aTOb.get(vertex), vertex);// make sure both vertices have the same attributes
					}
				}
				else
				{// There could be states of B that have the same name as states of A. In a number of cases these can be re-used (KEPT).
				 // In a similar way to the above, we make sure attributes of both are the same so that when transitions are removed or added,
				 // correct attributes will be entered. 
				 // It is important to point out that we ignore both vertices where there is no corresponding vertex in B and those where the corresponding B vertex is part of any key pair (because key pairs are "paired" and unpaired elements of A are removed completely). 
					if (origToNewB.containsKey(vertex) && !aTOb.containsValue(origToNewB.get(vertex)) && !DeterministicDirectedSparseGraph.nonIDAttributesEquals(vertex, origToNewB.get(vertex)))
					{
						addVertex(vertex);
						DeterministicDirectedSparseGraph.copyVertexData(origToNewB.get(vertex), vertex);
					}
				}
			}
			
			// Pick all transitions and incompatible pairs which have been added/removed from the matched states.
			for(Entry<CmpVertex,CmpVertex> entry:aTOb.entrySet())
			{
				// check records of compatible/incompatible pairs of states. This is based on what our compatibility matrix
				// says about states A (i.e. from the left-hand side of the aTOb set) and then matching it to what it says
				// about the right-hand side of it (the B part).
				{
					// targetsB are the states&compatibility values associated with the B state in the entry pair.
					Map<CmpVertex,JUConstants.PAIRCOMPATIBILITY> targetsB = grCombined.pairCompatibility.compatibility.get(entry.getValue());// this is a function - there is no potential for non-determinism unlike that of transitionMatrix
					Map<CmpVertex,JUConstants.PAIRCOMPATIBILITY> newTargetsForB= new TreeMap<>();if (targetsB != null) newTargetsForB.putAll(targetsB);
					if (grCombined.pairCompatibility.compatibility.containsKey(entry.getKey())) // we have some pairs recorded in A which may match those in B
						for(Entry<CmpVertex,JUConstants.PAIRCOMPATIBILITY> targetInA:grCombined.pairCompatibility.compatibility.get(entry.getKey()).entrySet())
							if (aTOb.containsKey(targetInA.getKey()))
							{// both the current state (entry.getKey()) and the other side (targetInA.getKey()) are part of key pairs,
							 // hence we have to check what happens to the corresponding side (entry.getValue(),targetInB.getKey())
								if (targetsB == null || // this state has no compatible/incompatible states recorded in B
										!targetsB.containsKey(aTOb.get(targetInA.getKey())) || // It is not enough to check if both targetA and targetB are 
										// key states, but the two have to be part of the same key state. 
										// Otherwise, we risk making mistakes (see <em>testComputeGD6()</em> for an illustration).
										
										targetsB.get(aTOb.get(targetInA.getKey())) != targetInA.getValue()) // different value of the relation
									removeFromCompatibility(entry.getKey(), targetInA.getKey(),targetInA.getValue());
								else
									newTargetsForB.remove(aTOb.get(targetInA.getKey()));// relations match (note that here I may easily ask to remove elements from an empty collection or remove a null element which is fine since there cannot be such elements in newTargetsForB)
							} 	
							// There is no "else" clause because if a target state is not a matched one, 
					 		// such a relation will be removed later on when we focus on removing transitions from/to unmatched states
					
					for(Entry<CmpVertex,JUConstants.PAIRCOMPATIBILITY> newTarget:newTargetsForB.entrySet())
						// the pair entry.getKey(),targetInA.getKey() are not related or
						// they in a different relation to entry.getValue(),targetInB.getKey() (such as INCOMPATIBLE v.s. IFTHEN)						
						addToCompatibility(entry.getKey(), getOrig(newTarget.getKey()),newTarget.getValue());
				}
				
				// Transitions from the A part. We only consider outgoing transitions because incoming ones are either from other key pairs or from unmatched states.
				// Those from other key pairs will be matched eventually, those from unmatched states are unconditionally removed. This may cause the same transition to be 
				// both removed and then added, but this is taken into account in a separate step of the algorithm.
				for(Entry<Label,List<CmpVertex>> transitionA:grCombined.transitionMatrix.get(entry.getKey()).entrySet())
				{
					List<CmpVertex> targetsInB = grCombined.transitionMatrix.get(entry.getValue()).get(transitionA.getKey());
					if (targetsInB == null) // this transition does not exist in B, record that all target states have to be removed
						for(CmpVertex targetA:grCombined.getTargets(transitionA.getValue()))
							removeTransition(entry.getKey(), transitionA.getKey(),targetA);
					else
					{
						Collection<CmpVertex> targetsA=grCombined.getTargets(transitionA.getValue()), targetsB=grCombined.getTargets(targetsInB);
						Set<CmpVertex> newTargetsForB = new TreeSet<>(targetsB);
						
						// It is not enough to check if both targetA and targetB are 
						// key states, but the two have to be part of the same key state. 
						// Otherwise, we risk making mistakes (see <em>testComputeGD6()</em> for an illustration).
						// Targets which are only in A should be removed, those only in B should be added and 
						// those shared but not from the same key pair should be updated (add/remove).
						for(CmpVertex targetA:targetsA)
						{
							CmpVertex targetB = aTOb.get(targetA);
							if (targetB == null)
								removeTransition(entry.getKey(), transitionA.getKey(),targetA);// target is not a key state
							else
							if (!targetsB.contains(targetB))
							// Transition leads to a state which is not key in either of the two machines or both are parts of different key states.
								removeTransition(entry.getKey(), transitionA.getKey(),targetA);
							else
								newTargetsForB.remove(targetB);
						}
						
						for(CmpVertex targetB:newTargetsForB)
							addTransition(entry.getKey(), transitionA.getKey(),getOrig(targetB));
					}
				}
				
				// transitions from the B part which were not covered above.
				for(Entry<Label,List<CmpVertex>> transitionB:grCombined.transitionMatrix.get(entry.getValue()).entrySet())
				{
					List<CmpVertex> targetsInA = grCombined.transitionMatrix.get(entry.getKey()).get(transitionB.getKey());
					if (targetsInA == null) // a transition unique to B
						for(CmpVertex targetB:grCombined.getTargets(transitionB.getValue()))
							addTransition(getOrig(entry.getValue()), transitionB.getKey(),getOrig(targetB));
				}
				
			}

			// now we just need to go through states which are not key states
			for(CmpVertex vertex:statesOfA)
				if (!statesInKeyPairs.contains(vertex))
				{
					for(Entry<Label,List<CmpVertex>> target:grCombined.transitionMatrix.get(vertex).entrySet())
						// transition not matched because some states are not known hence remove it.
						for(CmpVertex targetState:grCombined.getTargets(target.getValue()))
								// if either of the two vertices does not exist or the transition does not exist on the B's side
								removeTransition(vertex, target.getKey(),targetState);
					// incompatible pairs.
					if (grCombined.pairCompatibility.compatibility.containsKey(vertex))
						for(Entry<CmpVertex,PAIRCOMPATIBILITY> vertOther:grCombined.pairCompatibility.compatibility.get(vertex).entrySet())
							removeFromCompatibility(vertex, vertOther.getKey(),vertOther.getValue());// we are talking in terms of the original vertices hence no need to call getOrig here unlike below when we go through vertices of B
				}

			for(CmpVertex vertex:statesOfB)
			{
				CmpVertex vertexA = getOrig(vertex);
				if ( (grCombined.transitionMatrix.get(vertex).isEmpty() && inverse.matrixForward.transitionMatrix.get(vertex).isEmpty())) // disconnected vertex, regardless whether in a key pair or not.
					addVertex(vertexA);

				if (!statesInKeyPairs.contains(vertex))
				{

					for (Entry<Label, List<CmpVertex>> target : grCombined.transitionMatrix.get(vertex).entrySet()) {
						// transition not matched because some states are not known hence append it.
						for (CmpVertex targetState : grCombined.getTargets(target.getValue()))
							addTransition(vertexA, target.getKey(), getOrig(targetState));
					}

					// incompatible pairs.
					if (grCombined.pairCompatibility.compatibility.containsKey(vertex))
						for(Entry<CmpVertex,JUConstants.PAIRCOMPATIBILITY> vertEntry:grCombined.pairCompatibility.compatibility.get(vertex).entrySet())
							addToCompatibility(vertexA, getOrig(vertEntry.getKey()),vertEntry.getValue());
				}
			}
			// Add relabelling: first, aTOb , then duplicates. If this is done in a different order
			// we might relabel a vertex to the name already in use and relabel will choke.
			for(Entry<CmpVertex,CmpVertex> entry:aTOb.entrySet())
			{
				VertID from = entry.getKey(), to = newBToOrig.get(entry.getValue());
				if (!from.equals(to))
					addRelabelling(from,to);
			}

			for(CmpVertex vert:duplicates)
				addRelabelling(vert, newBToOrig.get(vert));
			//StringBuffer inTermsOfB = new StringBuffer("disconnected: "+disconnectedStatesInKeyPairs+" key pairs: "+aTOb+", that is: ");
			//for(Entry<CmpVertex,CmpVertex> entry:aTOb.entrySet()) inTermsOfB.append(entry.getKey()).append("=").append(newBToOrig.get(entry.getValue())).append(", ");
			//System.out.println(inTermsOfB);
			
			
			// If a match is found in the loop through aTOb, it is appropriate to set the initial state to the corresponding state of A,
			// if not, this means that initial state of B will be the new state, we hence add it and set as 
			// initial. Many tests among TestGD_Multithreaded explore both possibilities.
			setInitial(initialState);
		}

		/**
		 * @see GD.PatchGraph#addTransition(CmpVertex, Label, CmpVertex)
		 */
		@SuppressWarnings("unused")
		@Override
		public void addTransition(CmpVertex from, Label label, CmpVertex to) {
		}

		/**
		 * @see GD.PatchGraph#removeTransition(CmpVertex, Label, CmpVertex)
		 */
		@SuppressWarnings("unused")
		@Override
		public void removeTransition(CmpVertex from, Label label, CmpVertex to) {
		}

		/**
		 * @see GD.PatchGraph#addToCompatibility(CmpVertex, CmpVertex, PAIRCOMPATIBILITY)
		 */
		@SuppressWarnings("unused")
		@Override
		public void addToCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
		}

		/**
		 * @see GD.PatchGraph#removeFromCompatibility(CmpVertex, CmpVertex, PAIRCOMPATIBILITY)
		 */
		@SuppressWarnings("unused")
		@Override
		public void removeFromCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
		}
		
		
	}
	
	
	/** Sorts waves in place, in the order of descending scores.
	 * 
	 * @param wave wave to sort
	 */
	public static void sortWave(List<PairScore> wave)
	{
		wave.sort(Comparator.reverseOrder());
	}
	
	/** Makes it possible to modify graphs by adding/removing transitions. */
	public static class LearnerGraphMutator<TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> implements PatchGraph
	{
		/** Graph to manipulate. */
		private final AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph;
		/** Configuration to use for cloning if necessary. */
		private final Configuration cloneConfig;
		
		/** States which should be included in the target graph, even if they have no 
		 * outgoing or incoming transitions. One of such states is an initial state. 
		 */
		private final Set<CmpVertex> statesToInclude = new TreeSet<>();
		
		/** Next instance of PatchGraph in a stack of observers. */
		private final PatchGraph next;
		
		/** Relabelling of states, applied if necessary. */
		private final Map<VertID,VertID> relabelling = new TreeMap<>();
		
		/** Constructs an instance of the mutator
		 * 
		 * @param gr graph to modify
		 * @param config configuration to use
		 */
		public LearnerGraphMutator(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> gr, Configuration config,PatchGraph nextInStack)
		{
			graph = gr;cloneConfig = config;next = nextInStack;
		}
		
		/** Adds the vertex to the graph, cloning this vertex if 
		 * (a) it is not already in the graph,
		 * (b) user requested it to be cloned in the configuration.
		 *  
		 * @param vert vertex to add
		 * @return added vertex
		 */
		protected CmpVertex addNewVertex(CmpVertex vert)
		{
			CmpVertex fromVert = graph.findVertex(vert);
			if (fromVert == vert)
				return fromVert;
			
			if (fromVert == null)
			{// vertex with this ID is not already known
				fromVert = AbstractLearnerGraph.cloneCmpVertex(vert, cloneConfig);
				graph.transitionMatrix.put(fromVert,graph.createNewRow());
				graph.updateIDWith(fromVert);
			}
			else // vertex with the same ID exists
				if (!AbstractLearnerGraph.checkCompatible(fromVert,vert,graph.pairCompatibility)) // it is known but with a different accept condition
					throw new IllegalArgumentException("vertex "+vert+" is incompatible to the one in graph "+graph);// incompatibles cannot 
						// lead to this exception since this would mean that a state is not compatible with 
						// itself - such a contradiction cannot be added to a set of incompatibles.
				else
					DeterministicDirectedSparseGraph.copyVertexData(vert, fromVert);// update attributes

			return fromVert;
		}
		
		/** Adds transition to a graph. For each of the source and target states,
		 * if states with the same IDs already present, existing states are used.
		 * If those states do not exist, the respective states are cloned using
		 * the supplied configuration.
		 * 
		 * @param from source state
		 * @param input input
		 * @param to target state
		 */
		@Override
		public void addTransition(CmpVertex from, Label input,CmpVertex to)
		{
			if (next != null) next.addTransition(from, input, to);
			
			CmpVertex fromVert = addNewVertex(from);
			MapWithSearch<Label,Label,TARGET_TYPE> entry = graph.transitionMatrix.get(fromVert);
			
			CmpVertex toVert = addNewVertex(to);
			graph.addTransition(entry, input, toVert);
			graph.updateIDWith(fromVert);graph.updateIDWith(toVert);
		}
	
		/** Removes a transition from a graph. It does not matter which graph owns the vertices
		 * supplied - these vertices are not touched while a corresponding transition
		 * in the supplied graph is removed.
		 * 
		 * @param from source state
		 * @param input input
		 * @param to expected target state. This is not really necessary but useful to ensure that whoever removes transitions knows what he/she is doing. 
		 */
		@Override
		public void removeTransition(CmpVertex from, Label input,CmpVertex to)
		{
			if (next != null) next.removeTransition(from, input, to);
			CmpVertex fromVert = addNewVertex(from), toVert = addNewVertex(to);
			MapWithSearch<Label,Label,TARGET_TYPE> entry = graph.transitionMatrix.get(fromVert);
			if (!entry.containsKey(input))
				throw new IllegalArgumentException("there is no transition from state "+fromVert+" with input "+input+" in graph "+graph);
			if (!graph.getTargets(entry.get(input)).contains(toVert))
				throw new IllegalArgumentException("there is no transition to state "+toVert+" from state "+fromVert+" with input "+input+" in graph "+graph);
			graph.removeTransition(entry, input, toVert);
		}
	
		/** Removes all states which have no outgoing transitions and no incoming transitions,
		 * making sure that the initial state is not removed.
		 */
		public void removeDanglingStates()
		{
			Set<CmpVertex> statesInGraph = new TreeSet<>();
			for(Entry<CmpVertex,MapWithSearch<Label,Label,TARGET_TYPE>> entry:graph.transitionMatrix.entrySet())
				if (entry.getValue().isEmpty()) statesInGraph.add(entry.getKey()); // add those with no outgoing
			for(Entry<CmpVertex,MapWithSearch<Label,Label,TARGET_TYPE>> entry:graph.transitionMatrix.entrySet())
				for(TARGET_TYPE targets:entry.getValue().values())
					statesInGraph.removeAll(graph.getTargets(targets));// and remove those used as targets
			statesInGraph.remove(graph.getInit());// initial state should stay
			statesInGraph.removeAll(statesToInclude);// as should those which have been explicitly added.
			graph.transitionMatrix.keySet().removeAll(statesInGraph);
		}

		/** Sets the initial state to an existing state. Throws if state is not known. */
		@Override
		public void setInitial(CmpVertex vertex) {
			if (next != null) next.setInitial(vertex);
			
			graph.setInit(addNewVertex(vertex)); // assuming that addNewVertex has been tested as a part of integration testing of addTransition :)
		}

		@Override
		public void addVertex(CmpVertex vertex) {
			statesToInclude.add(addNewVertex(vertex));
		}

		@Override
		public void addToCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
			if (next != null) next.addToCompatibility(a,b,value);

			CmpVertex stateA = addNewVertex(a),stateB = addNewVertex(b);
/*			
			if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LINEARWARNINGS)))
			{
				if (graph.findVertex(a) == null) throw new IllegalArgumentException("vertex "+a+" does not exist");
				if (graph.findVertex(b) == null) throw new IllegalArgumentException("vertex "+b+" does not exist");
			}
*/			
			graph.addToCompatibility(stateA, stateB,value);statesToInclude.add(stateA);statesToInclude.add(stateB);
		}

		@Override
		public void removeFromCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
			if (next != null) next.removeFromCompatibility(a,b,value);
			
			CmpVertex stateA = addNewVertex(a),stateB = addNewVertex(b);
/*			
			if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LINEARWARNINGS)))
			{
				if (graph.findVertex(a.getID()) == null) throw new IllegalArgumentException("vertex "+a+" does not exist");
				if (graph.findVertex(b.getID()) == null) throw new IllegalArgumentException("vertex "+b+" does not exist");
			}
*/
			graph.removeFromIncompatibles(stateA, stateB);
		}

		@Override
		public void addRelabelling(VertID a, VertID b) {
			if (graph.findVertex(a) == null) throw new IllegalArgumentException("source vertex "+a+" does not exist");
			relabelling.put(a,b);
		}
		
		/** Relabels states in the graph as determined by the relabelling.
		 *   
		 * @param result where to store the outcome of relabelling. 
		 */
		public void relabel(final AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> result)
		{
			result.initEmpty();
			result.setName(graph.getName());

			Map<CmpVertex,CmpVertex> oldToNew = new HashMap<>();

			// First, clone vertices
			for(CmpVertex state:graph.transitionMatrix.keySet())
			{
				VertID newID = relabelling.get(state);
				if (newID == null)
					oldToNew.put(state, AbstractLearnerGraph.cloneCmpVertex(state, result.config)); // clone
				else // copy under a different name
				{
					CmpVertex newVertex = AbstractLearnerGraph.generateNewCmpVertex(newID, result.config);// rename
					DeterministicDirectedSparseGraph.copyVertexData(state, newVertex);
					oldToNew.put(state,newVertex);
				}
			}

			if (GlobalConfiguration.getConfiguration().isAssertEnabled()) 
			{
				Set<CmpVertex> encounteredNewVertices = new TreeSet<>();
				for(Entry<CmpVertex,CmpVertex> entry:oldToNew.entrySet())
				{
					if (encounteredNewVertices.contains(entry.getValue()))
						throw new IllegalArgumentException("duplicate vertex "+entry.getValue()+" after relabelling");
				
					encounteredNewVertices.add(entry.getValue());
				}
			}
			
			result.setInit(oldToNew.get(graph.getInit()));
			AbstractLearnerGraph.addAndRelabelGraphs(graph, oldToNew, result);
			result.setIDNumbers();// have to do this because we used different IDs than those used in the outcome of patching
		}
	}

	/** This class displays the requested changes.
	 */
	public static final class ChangesDisplay implements PatchGraph
	{
		private final StringBuffer result = new StringBuffer();
		
		/** Next instance of PatchGraph in a stack of observers. */
		private final PatchGraph next;

		public ChangesDisplay(PatchGraph nextInStack)
		{
			next = nextInStack;
		}
		
		/** Appends a newline to the result. */
		private void appendEndl()
		{
			result.append('\n');
		}
		
		private void appendTransition(CmpVertex from, Label label, CmpVertex to)
		{
			result.append(from);
                        result.append(" - ");
                        result.append(label);
                        result.append(" -> ");
                        result.append(to);
                        appendEndl();
		}
		@Override
		public void addTransition(CmpVertex from, Label label, CmpVertex to) {
			if (next != null) next.addTransition(from, label, to);
			result.append("added  : ");
                        appendTransition(from, label, to);
		}

		@Override
		public void removeTransition(CmpVertex from, Label label, CmpVertex to) {
			if (next != null) next.removeTransition(from, label, to);
			result.append("removed: ");
                        appendTransition(from, label, to);
		}
		
		@Override
		public String toString()
		{
			return result.toString();
		}
		
		@Override
		public void setInitial(CmpVertex vertex) {
			if (next != null) next.setInitial(vertex);
			result.append("initial : ");result.append(vertex);appendEndl();
		}

		@Override
		public void addToCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
			if (next != null) next.addToCompatibility(a,b,value);
			result.append("added incompatibles: ").append(a).append(",").append(b).append(" with value ").append(value);appendEndl();
		}

		@Override
		public void addVertex(CmpVertex vertex) {
			if (next != null) next.addVertex(vertex);
			result.append("added vertex:").append(vertex);appendEndl();
		}

		@Override
		public void removeFromCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
			if (next != null) next.removeFromCompatibility(a,b,value);
			result.append("removed incompatibles: ").append(a).append(",").append(b).append(" with value ").append(value);appendEndl();
		}

		@Override
		public void addRelabelling(VertID a, VertID b) {
			if (next != null) next.addRelabelling(a, b);
			result.append("mapping: ").append(a).append(" - ").append(b);appendEndl();
		}
	}

	/** This class counts the requested changes.
	 */
	public static class ChangesCounter<TARGET_A_TYPE,TARGET_B_TYPE,
		CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
		CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>>
		 implements PatchGraph
	{
		private int added = 0, removed = 0;
		private final int transitionsInA,transitionsInB;
		private final String nameA, nameB;

		public void reset()
		{
			added = 0;removed = 0;
		}
		
		/** Next instance of PatchGraph in a stack of observers. */
		private final PatchGraph next;

		public ChangesCounter(final AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> a,
				final AbstractLearnerGraph<TARGET_B_TYPE,CACHE_B_TYPE> b, PatchGraph nextInStack)
		{
			transitionsInA=a.pathroutines.countEdges();transitionsInB=b.pathroutines.countEdges();nameA = a.getNameNotNull();nameB=b.getNameNotNull();
			next = nextInStack;
		}
		
		@Override
		public void addTransition(CmpVertex from, Label label,	CmpVertex to)
		{
			if (next != null) next.addTransition(from, label, to);
			
			++added;
		}

		@Override
		public void removeTransition(CmpVertex from, Label label,	CmpVertex to)
		{
			if (next != null) next.removeTransition(from, label, to);
			
			++removed;
		}
		
		public int getAdded()
		{
			return added;
		}
		
		public int getRemoved()
		{
			return removed;
		}
		
		/** Returns the estimated compression rate. */
		public double getCompressionRate()
		{
			double result = 0;
			if (transitionsInB > 0) result = ((double)added+removed)/transitionsInB;
			return result;
		}
		
		@Override
		public String toString()
		{
			return "diff of "+nameB+" to "+nameA+" is "+(int)(100.*getCompressionRate())+"% of "+nameB;
		}

		public String detailsToString()
		{
			return transitionsInA+"+"+added+"-"+removed+"="+transitionsInB;	
		}
		
		@Override
		public void setInitial(CmpVertex vertex) {
			if (next != null) next.setInitial(vertex);
		}

		@Override
		public void addToCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
			if (next != null) next.addToCompatibility(a, b, value);
		}

		@Override
		public void addVertex(CmpVertex vertex) {
			if (next != null) next.addVertex(vertex);
		}

		@Override
		public void removeFromCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
			if (next != null) next.removeFromCompatibility(a, b, value);			
		}

		@Override
		public void addRelabelling(VertID a, VertID b) {
			if (next != null) next.addRelabelling(a, b);
		}
	}
	
	/** This class records requested changes and is capable of returning a 
	 * collection of them in a form of XML which can then be applied to a graph.
	 */
	public static class ChangesRecorder implements PatchGraph
	{
		/** Vertices removed from A. */
		protected final LearnerGraphND removed;

		/** Vertices added by B. */
		protected final LearnerGraphND added;
		
		private final PatchGraph addedPatcher, removedPatcher;
		
		protected final Map<VertID,VertID> relabelling = new TreeMap<>();
		
		/** Next instance of PatchGraph in a stack of observers. */
		private final PatchGraph next;

		public ChangesRecorder(PatchGraph nextInStack)
		{
			next = nextInStack;
			Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			added = new LearnerGraphND(config);removed = new LearnerGraphND(config);removed.initEmpty();
			added.setInit(null);added.initEmpty();// to make sure we can handle an assignment of a reject-state to an initial state
			addedPatcher = new LearnerGraphMutator<>(added, config, null);
			removedPatcher = new LearnerGraphMutator<>(removed, config, null);
		}
		
		/** Used for testing. */
		protected ChangesRecorder(LearnerGraphND r,LearnerGraphND a,PatchGraph nextInStack)
		{
			next = nextInStack;
			Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			removed = r;added = a;
			addedPatcher = new LearnerGraphMutator<>(added, config, null);
			removedPatcher = new LearnerGraphMutator<>(removed, config, null);
		}
		
		/** Used to check whether a transition is present and fail assertion. Used to check for the same
		 * transition being both added and removed.
		 * 
		 * @param graphToCheck graph to inspect
		 * @param from source state
		 * @param label transition label
		 * @param to target state.
		 */
		protected static void checkTransitionPresent(LearnerGraphND graphToCheck,CmpVertex from, Label label, CmpVertex to)
		{
			Map<Label,List<CmpVertex>> row = graphToCheck.transitionMatrix.get(from);
			if (row != null)
			{
				List<CmpVertex> targets = row.get(label);
				if (targets != null)
					for(CmpVertex vert:targets)
						// the check is not by identity because one of them could be a vertex of the left-hand 
						// side graph and the other - that of the right-hand one. They map to the same actual 
						// state but CmpVertex objects are different.
						if (vert.equals(to) && DeterministicDirectedSparseGraph.nonIDAttributesEquals(vert,to)) 
							throw new IllegalArgumentException
							("duplicate transition added "+from+"-"+label+"->"+to);
			}
		}
		
		
		@Override
		public void addTransition(CmpVertex from, Label label, CmpVertex to) {
			if (next != null) next.addTransition(from, label, to);
			if (Boolean.parseBoolean(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LINEARWARNINGS)))
				checkTransitionPresent(removed, from, label, to);
			addedPatcher.addTransition(from, label, to);
		}

		@Override
		public void removeTransition(CmpVertex from, Label label, CmpVertex to) {
			if (next != null) next.removeTransition(from, label, to);
			if (Boolean.parseBoolean(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LINEARWARNINGS)))
				checkTransitionPresent(added, from, label, to);
			removedPatcher.addTransition(from, label, to);
		}
		
		/** Writes the recorded changes in a form of an XML tag. */
		protected Element writeGD(Document doc)
		{
			if (added.getInit() == null) throw new IllegalArgumentException("init state is was not defined");
			Element gd = doc.createElement(StatechumXML.gdGD.toString()), addedNode = doc.createElement(StatechumXML.gdAdded.toString()), removedNode = doc.createElement(StatechumXML.gdRemoved.toString()), relabellingNode = doc.createElement(StatechumXML.gdRelabelling.toString());
			addedNode.appendChild(added.storage.createGraphMLNode(doc));removedNode.appendChild(removed.storage.createGraphMLNode(doc));
			for(Entry<VertID,VertID> entry:relabelling.entrySet())
				relabellingNode.appendChild(ProgressDecorator.writePair(
						new PairScore(AbstractLearnerGraph.generateNewCmpVertex(entry.getKey(),Configuration.getDefaultConfiguration()), 
								AbstractLearnerGraph.generateNewCmpVertex(entry.getValue(),Configuration.getDefaultConfiguration()),JUConstants.intUNKNOWN, JUConstants.intUNKNOWN),doc));
			gd.appendChild(removedNode);gd.appendChild(addedNode);gd.appendChild(relabellingNode);
			return gd;
		}

		/** Given an element containing a number of children, this one picks the one
		 * with the right tag and returns its first child.
		 * If <em>tolerateAbsentElements</em> is set, returns null if the element with the requested
		 * name was not found; otherwise, an exception is thrown.
		 * <p>
		 * If <em>checksingleChild</em> is set, checks that there is exactly one ELEMENT_NODE child of the found node
		 * and throws {@link IllegalArgumentException} if this is not so.
		 * 
		 * @param elem element within which to look for a node with the supplied tag.
		 * @param name the tag to look for.
		 * @param tolerateAbsentElements to throw an exception if an element was not found.
		 * @return element found.
		 */
		static public Element getGraphElement(Element elem, String name,boolean tolerateAbsentElements, boolean checksingleChild)
		{
			if (!elem.getNodeName().equals(StatechumXML.gdGD.toString()))
				throw new IllegalArgumentException("unexpected element, expected "+StatechumXML.gdGD+", got "+elem.getNodeName());
			
			NodeList graphlist_List = StatechumXML.getChildWithTag(elem,name);
			int i=0;
			while(i< graphlist_List.getLength() && graphlist_List.item(i).getNodeType() != Node.ELEMENT_NODE) ++i;
			if (i == graphlist_List.getLength()) throw new IllegalArgumentException("no element "+name);
			
			NodeList graphs = graphlist_List.item(i).getChildNodes();
			int gr=0;
			while(gr<graphs.getLength() && graphs.item(gr).getNodeType() != Node.ELEMENT_NODE) ++gr;
			if (gr == graphs.getLength())
			{
				if (tolerateAbsentElements)
					return null;
				throw new IllegalArgumentException("no nodes in the "+name+" entry");
			}
			
			Element result = (Element)graphs.item(gr);

			if (checksingleChild)
			{// check that there is only one node inside an element with name name.
				++gr;while(gr<graphs.getLength() && graphs.item(gr).getNodeType() != Node.ELEMENT_NODE) ++gr;
				if (gr != graphs.getLength()) throw new IllegalArgumentException("more than one node in the "+name+" entry");
				++i;while(i< graphlist_List.getLength() && graphlist_List.item(i).getNodeType() != Node.ELEMENT_NODE) ++i;
				if (i != graphlist_List.getLength()) throw new IllegalArgumentException("duplicate holder "+name);
			}
			return result;
		}

		/** Applies GD to the supplied graph. This is a part of GD because it only
		 * handles GD and not general-purpose stuff which would be included in 
		 * <em>Transform</em>. 
		 * 
		 * @param graph graph to transform
		 * @param elem element containing the difference.
		 * @param conv label converter, ignored if null.
		 */
		static public <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
			void applyGD(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph, Element elem,final ConvertALabel conv)
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			LearnerGraphMutator<TARGET_TYPE,CACHE_TYPE> graphPatcher = new LearnerGraphMutator<>(graph, config, null);
			loadDiff(graphPatcher, elem,conv);
			graphPatcher.removeDanglingStates();
		}
				
		/** Applies GD to the supplied graph. This is a part of GD because it only
		 * handles GD and not general-purpose stuff which would be included in 
		 * <em>Transform</em>. 
		 * 
		 * @param graph graph to transform
		 * @param elem element containing the difference.
		 * @param conv label converter, ignored if null.
		 */
		static public <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
			void applyGD_WithRelabelling(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph, Element elem,final ConvertALabel conv,
					AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> result)
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			LearnerGraphMutator<TARGET_TYPE,CACHE_TYPE> graphPatcher = new LearnerGraphMutator<>(graph, config, null);
			loadDiff(graphPatcher, elem,conv);
			graphPatcher.removeDanglingStates();
			graphPatcher.relabel(result);
		}
				
		/** Loads diff from XML. This is a part of GD because it only
		 * handles GD and not general-purpose stuff which would be included in 
		 * the likes of <em>Transform</em>. 
		 * 
		 * @param graphPatcher graph to transform
		 * @param elem element containing the difference.
		 * @param conv label converter, ignored if null.
		 */
		static public void loadDiff(PatchGraph graphPatcher, Element elem,final ConvertALabel conv)
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerCloneGraph(false);
			LearnerGraphND gr = new LearnerGraphND(config);AbstractPersistence.loadGraph(getGraphElement(elem, StatechumXML.gdRemoved.toString(),false,true),gr,conv);
			for(Entry<CmpVertex,MapWithSearch<Label,Label,List<CmpVertex>>> entry:gr.transitionMatrix.entrySet())
				for(Entry<Label,List<CmpVertex>> transition:entry.getValue().entrySet())
					for(CmpVertex target:gr.getTargets(transition.getValue()))
						graphPatcher.removeTransition(entry.getKey(), transition.getKey(), target);
					
			for(Entry<CmpVertex,Map<CmpVertex,JUConstants.PAIRCOMPATIBILITY>> incompatibles:gr.pairCompatibility.compatibility.entrySet())
				for(Entry<CmpVertex,PAIRCOMPATIBILITY> target:incompatibles.getValue().entrySet())
					graphPatcher.removeFromCompatibility(incompatibles.getKey(), target.getKey(), target.getValue());
			
			AbstractPersistence.loadGraph(getGraphElement(elem, StatechumXML.gdAdded.toString(),false,true),gr,conv);
			
			for(Entry<CmpVertex,MapWithSearch<Label,Label,List<CmpVertex>>> entry:gr.transitionMatrix.entrySet())
			{
				if (entry.getValue().isEmpty())
					graphPatcher.addVertex(entry.getKey());
				else
					for(Entry<Label,List<CmpVertex>> transition:entry.getValue().entrySet())
						for(CmpVertex target:gr.getTargets(transition.getValue()))
							graphPatcher.addTransition(entry.getKey(), transition.getKey(), target);
			}
			for(Entry<CmpVertex,Map<CmpVertex,JUConstants.PAIRCOMPATIBILITY>> incompatibles:gr.pairCompatibility.compatibility.entrySet())
				for(Entry<CmpVertex,JUConstants.PAIRCOMPATIBILITY> targetEntry:incompatibles.getValue().entrySet())
					graphPatcher.addToCompatibility(incompatibles.getKey(), targetEntry.getKey(), targetEntry.getValue());
			
			NodeList relabellingElementList=  StatechumXML.getChildWithTag(elem,StatechumXML.gdRelabelling.toString());
			if (relabellingElementList.getLength() > 0)
			{// load the relabelling if present.
				getGraphElement(elem, StatechumXML.gdRelabelling.toString(),true,false);// check that XML structure is consistent and there are pairs stored - will throw otherwise
				Element relabellingElement = (Element)relabellingElementList.item(0); 
				NodeList children = relabellingElement.getChildNodes();
				for(int childNum=0;childNum<children.getLength();++childNum)
					if (children.item(childNum).getNodeType() == Node.ELEMENT_NODE)
					{
						PairScore pair=ProgressDecorator.readPair(gr, (Element)children.item(childNum));
						graphPatcher.addRelabelling(pair.getQ(), pair.getR());
					}
			}
			graphPatcher.setInitial(gr.getInit());
		}

		@Override
		public void setInitial(CmpVertex vertex) 
		{
			if (next != null) next.setInitial(vertex);
			addedPatcher.setInitial(vertex);removedPatcher.setInitial(vertex);			
		}

		@Override
		public void addToCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
			if (next != null) next.addToCompatibility(a, b, value);
			addedPatcher.addVertex(a);addedPatcher.addVertex(b);
			addedPatcher.addToCompatibility(a, b, value);
		}

		@Override
		public void addRelabelling(VertID a, VertID b) {
			if (next != null) next.addRelabelling(a, b);
			relabelling.put(a,b);
		}

		@Override
		public void addVertex(CmpVertex vertex) {
			if (next != null) next.addVertex(vertex);
			addedPatcher.addVertex(vertex);
		}

		@Override
		public void removeFromCompatibility(CmpVertex a, CmpVertex b, JUConstants.PAIRCOMPATIBILITY value) {
			if (next != null) next.removeFromCompatibility(a, b, value);
			removedPatcher.addVertex(a);removedPatcher.addVertex(b);
			removedPatcher.addToCompatibility(a, b, value);// when a diff is 
				// applied via loadDiff, we remove all pairs mentioned in removedPatcher from a list of pairs in the graph being transformed. 
				// For this reason, it does not matter how the two elements are related, we are only interested to record pairs to remove. 
				// Earlier versions use INCOMPATIBLE, the current one uses the actual value passed.
		}
	}
		
	protected boolean fallbackToInitialPair = false;
	
	/** Builds the data structures subsequently used in traversal.
	 * 
	 * @param a the first graph
	 * @param b the second graph
	 * @param threads how many threads to use
	 * @param argConfig configuration to use
	 */ 
	protected void init(AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> a,AbstractLearnerGraph<TARGET_B_TYPE,CACHE_B_TYPE> b,
			int threads, Configuration argConfig)
	{
		a.pathroutines.checkConsistency(a);
		b.pathroutines.checkConsistency(b);

		ThreadNumber = threads;
		Configuration cloneConfig = argConfig.copy();cloneConfig.setLearnerCloneGraph(true);// we need to clone attributes here because after key pair identification, attributes from graph B will be copied into vertices of A, otherwise these attributes may not make it into the patch.
		grCombined = new LearnerGraphND(a,cloneConfig);// I cannot simply do Transform.addToGraph here because patch has to be relative to graph A.
		grCombined.config.setLearnerCloneGraph(false);//reset the clone attribute
		grCombined.vertNegativeID=Math.max(grCombined.vertNegativeID, b.vertNegativeID);// we aim for new vertices in grCombined to have ids different from all vertices in B. 
		grCombined.vertPositiveID=Math.max(grCombined.vertPositiveID, b.vertPositiveID);
		combined_initA = grCombined.getInit();
		origToNewB = new TreeMap<>();
		statesOfA = new TreeSet<>();statesOfA.addAll(grCombined.transitionMatrix.keySet());

		// In the past, graph A could have textual vertices so when our new numerical IDs are converted to Strings for comparisons, IDs would overlap.
		// The current graph loading approach via VertexID.parseID generates numerical vertex IDs. Moreover, assertion statements will check for this.
		combined_initB = AbstractPathRoutines.addToGraph(grCombined, b,origToNewB);
		grCombined.pathroutines.checkConsistency(grCombined);
		grCombined.learnerCache.invalidate();
		
		statesOfB = new TreeSet<>();statesOfB.addAll(origToNewB.values());
		assert statesOfA.size() == a.getStateNumber();
		assert statesOfB.size() == origToNewB.size();assert statesOfB.size() == b.getStateNumber();
		assert statesOfA.size() + statesOfB.size() == grCombined.getStateNumber(): " added "+statesOfB.size()+" states but the outcome is only "+(grCombined.getStateNumber()-statesOfA.size())+" states larger";
		newBToOrig = new TreeMap<>();
		for(Entry<CmpVertex,CmpVertex> entry:origToNewB.entrySet()) newBToOrig.put(entry.getValue(),entry.getKey());

		forward = new GDLearnerGraph(grCombined,LearnerGraphND.ignoreNone,false);
		inverse = new GDLearnerGraph(grCombined,LearnerGraphND.ignoreNone,true);

		if (grCombined.config.getGdMaxNumberOfStatesInCrossProduct() == 0 || 
					forward.getStateNumber() > grCombined.config.getGdMaxNumberOfStatesInCrossProduct())
				fallbackToInitialPair = true;

		Class<? extends DetermineDiagonalAndRightHandSideInterface> ddrh = null;
		switch(argConfig.getGdScoreComputationAlgorithm())
		{
		case SCORE_RANDOMPATHS:
		case SCORE_TESTSET:
			// build (1) deterministic machines for each state and (2) walks from each state. 
			int seed = 80;
			JConsole_Diagnostics.getDiagnostics().setStatus("started on walk forward "+DateFormat.getTimeInstance().format(new Date()));
			forward.computeWalkSequences(new StateBasedRandom(seed), threads);
			JConsole_Diagnostics.getDiagnostics().setStatus("started on walk inverse "+DateFormat.getTimeInstance().format(new Date()));
			inverse.computeWalkSequences(new StateBasedRandom(seed), threads);
			ddrh = DDRH_BCR.class;
			break;
		case SCORE_LINEAR:
			ddrh = DDRH_default.class;
			break;
		default:
			throw new IllegalArgumentException("computation algorithm "+argConfig.getGdScoreComputationAlgorithm()+" is not currently supported");
		}

		JConsole_Diagnostics.getDiagnostics().setStatus("finished building walks "+DateFormat.getTimeInstance().format(new Date()));

		if (fallbackToInitialPair)
		{// we are here only if the full matrix has to be built and it will be too big to solve it in the usual way
			if (grCombined.config.getGdMaxNumberOfStatesInCrossProduct() > 0 && // only warn if not forced.
					Boolean.parseBoolean(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LINEARWARNINGS)))
				System.out.println("Cannot use Linear since the number of states is "+
						((double)forward.getStateNumber()/grCombined.config.getGdMaxNumberOfStatesInCrossProduct())+
						" times over the limit");
		}
		else
		{// normal processing
			pairScores = new int[forward.getPairNumber()];Arrays.fill(pairScores, GDLearnerGraph.PAIR_INCOMPATIBLE);
			// states to be ignored are those where each element of a pair belongs to a different automaton, we fill in the rest.
			List<HandleRow<List<CmpVertex>>> handlerList = new LinkedList<>();
			for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)// this is not doing workload balancing because it should iterate over currently-used left-hand sides, not just all possible ones. 
				handlerList.add(new HandleRow<>() {
					@Override
					public void init(@SuppressWarnings("unused") int threadNo) {
						// No per-thread initialisation is needed.
					}

					@Override
					public void handleEntry(Entry<CmpVertex, MapWithSearch<Label, Label, List<CmpVertex>>> entryA, @SuppressWarnings("unused") int threadNo) {
						// Now iterate through states
						for (CmpVertex stateB : statesOfB) {
							int pairINT = forward.vertexToIntNR(stateB, entryA.getKey());
							assert pairScores[pairINT] == GDLearnerGraph.PAIR_INCOMPATIBLE :
									"duplicate number " + pairINT + " for states " +
											forward.getStatesToNumber().get(stateB) + "," + forward.getStatesToNumber().get(entryA.getKey());
							pairScores[pairINT] = GDLearnerGraph.PAIR_OK;// caching is likely to lower down my performance a lot here
						}

						// Perhaps I should be numbering states directly here instead of using numberNonNegativeElements afterwards,
						// but this is not simple to do: I have to give numbers in the order in which triangular traversal visits states.
					}
				});
			GDLearnerGraph.performRowTasks(handlerList, ThreadNumber, grCombined.transitionMatrix,
					vert -> statesOfA.contains(vert),
					GDLearnerGraph.partitionWorkLoadLinear(ThreadNumber,statesOfA.size()));
			final int numberOfPairs = GDLearnerGraph.numberNonNegativeElements(pairScores);
			assert numberOfPairs == statesOfA.size()*statesOfB.size();
			JConsole_Diagnostics.getDiagnostics().setStatus("started building matrix forward "+DateFormat.getTimeInstance().format(new Date()));

			// Now the system of equations will be built and solved. The only exception is where 
			// argConfig.getGdScoreComputation() == GDScoreComputationEnum.GD_DIRECT in which case
			// the solver returned will be a dummy with b[] part copied to the x one.
			{
				LSolver solverForward = forward.buildMatrix_internal(pairScores, numberOfPairs, ThreadNumber,ddrh);
				JConsole_Diagnostics.getDiagnostics().setStatus("finished building matrix forward "+DateFormat.getTimeInstance().format(new Date()));
				forward.stateToCorrespondingGraph = null;// deallocate memory
				//System.out.println(forward.dumpEquations(solverForward, pairScores, newBToOrig));
				solverForward.solve(threads);
				solverForward.freeAllButResult();// deallocate memory before creating a large array.
				scoresForward = solverForward.j_x;
			}

			JConsole_Diagnostics.getDiagnostics().setStatus("started building matrix inverse "+DateFormat.getTimeInstance().format(new Date()));
			{
				LSolver solverInverse = inverse.buildMatrix_internal(pairScores, numberOfPairs, ThreadNumber,ddrh);
				//System.out.println(inverse.dumpEquations(solverInverse, pairScores, newBToOrig));
				JConsole_Diagnostics.getDiagnostics().setStatus("finished building matrix inverse "+DateFormat.getTimeInstance().format(new Date()));
				inverse.stateToCorrespondingGraph = null;// deallocate memory
				solverInverse.solve(threads);
				solverInverse.freeAllButResult();// deallocate memory before creating a large array.
				scoresInverse = solverInverse.j_x;
			}
			JConsole_Diagnostics.getDiagnostics().setStatus("finished with the inverse solver"+DateFormat.getTimeInstance().format(new Date()));
		}

	}
	
	/** Goes through the result of linear and identifies candidates for key state pairs.
	 * @return true if everything is ok, false if no perfect set of candidates was found.
	 */
	protected boolean identifyKeyPairs()
	{
		currentWave = new ArrayList<>(java.lang.Math.max(statesOfA.size(), statesOfB.size()));
		statesInKeyPairs = new HashSet<>();
		frontWave = new LinkedList<>();
		PairScore topPair = null;
		int threshold=0;
		
		// If we have to fall back to a pair of initial states, there is no point doing any
		// of the computation below.
		if (fallbackToInitialPair)
		{
			if (AbstractLearnerGraph.checkCompatible(combined_initA, combined_initB,grCombined.pairCompatibility))
				topPair = new PairScore(combined_initA,combined_initB,0,0);
		}
		else
		{// normal processing
			List<HandleRow<List<CmpVertex>>> handlerList = new LinkedList<>();
			@SuppressWarnings("unchecked")
			final ArrayList<PairScore>[] wavePerThread = new ArrayList[ThreadNumber];
			for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)// this is not doing workload balancing because it should iterate over currently-used left-hand sides, not just all possible ones.
			{
				wavePerThread[threadCnt] = new ArrayList<>(java.lang.Math.max(statesOfA.size(), statesOfB.size()));
				handlerList.add(new HandleRow<>() {
					ArrayList<PairScore> wave = null;

					@Override
					public void init(int threadNo) {
						wave = wavePerThread[threadNo];
					}

					@Override
					public void handleEntry(Entry<CmpVertex, MapWithSearch<Label, Label, List<CmpVertex>>> entryA, @SuppressWarnings("unused") int threadNo) {
						double scoreHigh = -Double.MAX_VALUE, scoreLow = -Double.MAX_VALUE;
						CmpVertex highState = null;
						// Now iterate through states
						for (CmpVertex stateB : statesOfB) {
							int scorePosition = pairScores[forward.vertexToIntNR(stateB, entryA.getKey())];
							double scoreForward = scoresForward[scorePosition], scoreBackward = scoresInverse[scorePosition];
							double score = scoreForward + scoreBackward;
							if (scoreForward < 0)// if a pair is incompatible, ensure the score is negative
								score = Math.min(scoreForward, scoreBackward);
							if (score > scoreHigh) {
								scoreLow = scoreHigh;
								scoreHigh = score;
								highState = stateB;
							} else if (score > scoreLow) scoreLow = score;
						}
						assert highState != null;
						wave.add(new PairScore(entryA.getKey(), highState, (int) (multiplier * scoreHigh), (int) (multiplier * scoreLow)));
					}
				});
			}
			GDLearnerGraph.performRowTasks(handlerList, ThreadNumber, grCombined.transitionMatrix,
					vert -> statesOfA.contains(vert),
					GDLearnerGraph.partitionWorkLoadLinear(ThreadNumber,statesOfA.size()));
			
			// now collect the results of processing. It is worth noting that each state of A may possibly be matched with only one state of B, 
			// the one with which the compatibility score is the highest. Hence some states of B will not be matched with any of A and will
			// hence appear as added.
			for(int th=0;th<ThreadNumber;++th) currentWave.addAll(wavePerThread[th]);
			
			// now we find so many percent of top values.
			long topScore = 0;// to make sure that if we only get negative pairs, no key states will be detected.
			sortWave(currentWave);
			if (!currentWave.isEmpty() && currentWave.iterator().next().getScore() > topScore)
			{
				topPair = currentWave.iterator().next();
				topScore = topPair.getScore(); // this is done here to avoid cache problems when updating the same variable on multiple threads.
			}
			threshold = (int)(topScore*(1.-grCombined.config.getGdKeyPairThreshold()));
			//System.out.println("top score: "+topScore+", threshold "+threshold+" and mult is "+(1.-grCombined.config.getGdKeyPairThreshold()));

			// Key pairs added to the collection.
			for(PairScore pair:currentWave)
				if (pair.getScore() >= 0 && pair.getScore() >= threshold && // top score good enough
						(pair.getAnotherScore() <= 0 || pair.getAnotherScore() <= pair.getScore()*grCombined.config.getGdLowToHighRatio()) && // and high-low ratio is ok
						!statesInKeyPairs.contains(pair.secondElem) && // and the target state has not already been used in another key pair
						AbstractLearnerGraph.checkCompatible(pair.getQ(), pair.getR(),grCombined.pairCompatibility) // make sure we do not consider an incompatible pair as a key pair, regardless of the score 
						)
				{
					frontWave.add(pair);statesInKeyPairs.add(pair.getQ());statesInKeyPairs.add(pair.getR());
				}
		}	

		boolean result = true;
		// We have to be careful if none is found this way.
		if (frontWave.isEmpty())
		{
			if (topPair != null)
			{// at least we've got a pair with a score over zero.
				if (!fallbackToInitialPair &&
						Boolean.parseBoolean(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LINEARWARNINGS)))
					System.out.println("Linear failed to find perfect candidiates for an initial set of key pairs, scores in ["+topPair.getScore()+","+threshold+"], ratio "+grCombined.config.getGdLowToHighRatio()+", hence using "+topPair);
				frontWave.add(topPair);statesInKeyPairs.add(topPair.getQ());statesInKeyPairs.add(topPair.getR());
			}
			else
			{// nothing of use detected, the difference will contain a union of all transitions in graphs A and B.
				if (Boolean.parseBoolean(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LINEARWARNINGS)))
					System.out.println("Failed to find any pairs with positive scores, the diff is the union of A and B");
			}
			result = false;
		}
		return result;
	}
	
	/** Reaches out from the front wave to unexplored frontiers matched by the two machines.
	 * In other words, for each pair on the front line, it looks for matched transitions
	 * by the two machines and add pairs of target states to currentWave, as long as none
	 * of these target states are contained in statesInKeyPairs.
	 * 
	 * @param matrixND the (non-deterministic) matrix
	 */
	protected void populateCurrentWave(LearnerGraphND matrixND) 
	{
		for(PairScore pair:frontWave)
		{
			for(Entry<Label,List<CmpVertex>> targetCollectionA:matrixND.transitionMatrix.get(pair.getQ()).entrySet())
			{
				List<CmpVertex> targetCollectionB = matrixND.transitionMatrix.get(pair.getR()).get(targetCollectionA.getKey());
				if (targetCollectionB != null)
				{// matched pair, now iterate over target states
					for(CmpVertex targetStateA:targetCollectionA.getValue())
						for(CmpVertex targetStateB:targetCollectionB)
							if (!statesInKeyPairs.contains(targetStateA) && !statesInKeyPairs.contains(targetStateB))
							{
								double score = 0;
								if (!fallbackToInitialPair)
								{
									int scorePosition = pairScores[forward.vertexToIntNR(targetStateA,targetStateB)];
									score = scoresForward[scorePosition] + scoresInverse[scorePosition];
								}
								currentWave.add(new PairScore(targetStateA,targetStateB,(int)(multiplier*score),0));
								
							}
				}
			}
		}
	}

	/** Where two graphs are deterministic, whenever two states are matched, one does not have to look one step
	 *  ahead - propagation can be performed recursively and one would expect everything to match well. 
	 *  This is useful because backward match is usually non-deterministic
	 *  so we have to make guesses, hence a potential for error.
	 *
	 * @param matrixForward the (non-deterministic) matrix forward
	 * @param matrixInverse the (non-deterministic) matrix inverse
	 */
	protected void propagateDet(LearnerGraphND matrixForward,LearnerGraphND matrixInverse)
	{
		boolean stateAdded = false;
		
		// on each iteration, new pairs are generated and only these will need to be iterated over in the following iteration.
		List<PairScore> newWaveA = new LinkedList<>(), newWaveB = new LinkedList<>();
		
		// When we start, we have to iterate over all the existing pairs and in subsequent iterations - over all the newly-added ones.
		List<PairScore> waveToProcess = frontWave, waveToAppendTo = newWaveA;
		currentWave.clear();

		do
		{
			waveToAppendTo.clear();stateAdded = false;
			for(PairScore pair:waveToProcess)
			{
				// forward
				for(Entry<Label,List<CmpVertex>> targetCollectionA:matrixForward.transitionMatrix.get(pair.getQ()).entrySet())
					if (targetCollectionA.getValue().size() == 1)
					{
						CmpVertex targetA = targetCollectionA.getValue().get(0); 
						
						List<CmpVertex> targetCollectionB = matrixForward.transitionMatrix.get(pair.getR()).get(targetCollectionA.getKey());
						if (targetCollectionB != null && targetCollectionB.size() == 1)
						{// matched deterministic pair
							CmpVertex targetB = targetCollectionB.get(0);
							PairScore newPair = checkAndAddIfPairIsMatched(targetA, targetB);
							if (newPair != null) 
							{
								waveToAppendTo.add(newPair);
								stateAdded = true;// this means we need to iterate in order to recursively process the next pair 
							}
						}
					}
				// inverse
				for(Entry<Label,List<CmpVertex>> targetCollectionA:matrixInverse.transitionMatrix.get(pair.getQ()).entrySet())
					if (targetCollectionA.getValue().size() == 1)
					{
						CmpVertex targetA = targetCollectionA.getValue().get(0); 
						
						List<CmpVertex> targetCollectionB = matrixInverse.transitionMatrix.get(pair.getR()).get(targetCollectionA.getKey());
						if (targetCollectionB != null && targetCollectionB.size() == 1)
						{// matched deterministic pair
							CmpVertex targetB = targetCollectionB.get(0);
							PairScore newPair = checkAndAddIfPairIsMatched(targetA, targetB);
							if (newPair != null) 
							{
								waveToAppendTo.add(newPair);
								stateAdded = true;// this means we need to iterate in order to recursively process the next pair 
							}
						}
					}
			}
			if (waveToAppendTo == newWaveA)
			{
				waveToProcess = newWaveA;waveToAppendTo = newWaveB;
			}
			else
			{
				waveToProcess = newWaveB;waveToAppendTo = newWaveA;
			}
				
		}
		while(stateAdded);
		
		// at this point, currentWave contains all the new pairs, add them to the main wave
		frontWave.addAll(currentWave);currentWave.clear();
	}
	
	private PairScore checkAndAddIfPairIsMatched(CmpVertex targetA, CmpVertex targetB)
	{
		PairScore result = null;
		if (!statesInKeyPairs.contains(targetA) && !statesInKeyPairs.contains(targetB) &&
				AbstractLearnerGraph.checkCompatible(targetA, targetB,grCombined.pairCompatibility))
		{
			PairScore newPair = new PairScore(targetA,targetB,Integer.MAX_VALUE,0);
			statesInKeyPairs.add(targetA);statesInKeyPairs.add(targetB);
			currentWave.add(newPair);result = newPair;
		}
		return result;
	}
	
	/** Adds the supplied prefix to vertex ID provided. */
	static void renameVertex(VertID currID, String prefix,Map<VertID,VertID> oldVerticesToNew)
	{
		VertID currentVertex = oldVerticesToNew.get(currID);
		VertID newID = VertexID.parseID(prefix+(currentVertex==null?"":currentVertex.toString()));
		if (oldVerticesToNew.containsKey(newID) || oldVerticesToNew.containsKey(newID))
			throw new IllegalArgumentException("duplicate vertex "+newID+" in outcome");
		oldVerticesToNew.put(currID,newID);
	}

	public Map<CmpVertex,CmpVertex> getKeyPairs()
	{
		Map<CmpVertex,CmpVertex> result = new TreeMap<>();
		for(Entry<CmpVertex,CmpVertex> entry:aTOb.entrySet())
			result.put(entry.getKey(),newBToOrig.get(entry.getValue()));
		return result;
	}

	static class TransitionChanges
	{
		public int added,removed,orig;
		
		/** Obtains a colour that corresponds to the specific arrangement of changes. */
		public Color getColor(Configuration config)
		{
			Color outcome =  Color.BLACK;
			switch(config.getGDColourMode())
			{
			case GD_COL_REDUCED:
				if (added > 0 && removed > 0)
					outcome = Color.YELLOW;//.darker();
				else
					if (added > 0)
					{
						outcome = Color.GREEN;
						//if (orig > 0) outcome.darker().darker();
					}
					else
						if (removed > 0)
						{
							outcome = Color.RED;
							//if (removed < orig) outcome=outcome.darker().darker();
						}
				
				break;
			default:
				if (added > 0 && removed > 0)
					outcome = Color.YELLOW.darker();
				else
					if (added > 0)
					{
						outcome = Color.GREEN;
						if (orig > 0) outcome.darker().darker();
					}
					else
						if (removed > 0)
						{
							outcome = Color.RED;
							if (removed < orig) outcome=outcome.darker().darker();
						}
				
				break;
			}

			return outcome;
		}
		
	}
	
	/** This one is similar to applyGD but computes a union of the remove and added parts,
	 *  very useful if I wish to visualise the difference between two graphs.
	 *  <p>
	 *  Returns labelling of matching pairs of states (including key states)
	 *  and colouring of edges, used to indicate which transitions are to be removed
	 *  and which are to be added.
	 */
	public DirectedSparseGraph showGD(final AbstractLearnerGraph<TARGET_A_TYPE,CACHE_A_TYPE> a,
			final AbstractLearnerGraph<TARGET_B_TYPE,CACHE_B_TYPE> b, int threads)
	{
		Configuration gdConfig = a.config.copy();gdConfig.setGdFailOnDuplicateNames(false);gdConfig.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		init(a,b, threads,gdConfig);
		identifyKeyPairs();
		final List<PairScore> initialKeyPairs = new LinkedList<>(frontWave);
		final Map<VertID,VertID> oldVerticesToNew = new TreeMap<>();
		final LearnerGraphND outcome = new LearnerGraphND(a,gdConfig);outcome.setName(outcome.getName()+"_diff");
		final LearnerGraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> mutator =
				new LearnerGraphMutator<>(outcome, gdConfig, null);

		final EdgeAnnotation transitionAnnotation = new EdgeAnnotation();
		final Map<StatePair,TransitionChanges> pairToNumberOfChanges = new TreeMap<>();
		for(Entry<CmpVertex,MapWithSearch<Label,Label,TARGET_A_TYPE>> entry:a.transitionMatrix.entrySet())
			for(Entry<Label,TARGET_A_TYPE> transition:entry.getValue().entrySet())
				for(CmpVertex target:a.getTargets(transition.getValue()))
				{
					TransitionChanges changes = pairToNumberOfChanges.get(new StatePair(entry.getKey(), target));
					if (changes == null)
					{
						changes = new TransitionChanges();pairToNumberOfChanges.put(new StatePair(entry.getKey(), target),changes);
					}
					changes.orig++;// we populate our map with the existing transitions
					
				}
		makeSteps();
		computeDifference(new PatchGraph() {
			private Label copyVertexWithPrefix(String prefix, Label origLabel)
			{
				Label result = null;
				if (origLabel instanceof StringLabel)
					result = new StringLabel(prefix+origLabel);
				else
				if (origLabel instanceof ErlangLabel)
				{
					ErlangLabel oErl = (ErlangLabel)origLabel;
					result = new ErlangLabel(oErl.function,prefix+oErl.callName,
							oErl.input, oErl.expectedOutput);
				}
				else
					assert false;
				
				return result;
			}
			
			@Override
			public void addTransition(CmpVertex from, Label origLabel, CmpVertex to)
			{
				Label label = copyVertexWithPrefix("ADD_",origLabel);
				mutator.addTransition(from, label, to);
				TransitionChanges changes = pairToNumberOfChanges.get(new StatePair(from,to));
				if (changes == null)
				{
					changes = new TransitionChanges();pairToNumberOfChanges.put(new StatePair(from,to),changes);
				}
				changes.added++;
			}

			@Override
			public void removeTransition(CmpVertex from, Label origLabel, CmpVertex to)
			{
				Label label = copyVertexWithPrefix("REM_",origLabel);
				mutator.removeTransition(from, origLabel, to);// remove the original transition
				mutator.addTransition(from, label, to);// and add the renamed one
				TransitionChanges changes = pairToNumberOfChanges.get(new StatePair(from,to));
				if (changes == null)
				{
					changes = new TransitionChanges();pairToNumberOfChanges.put(new StatePair(from,to),changes);
				}
				changes.removed++;
			}

			@Override
			public void setInitial(CmpVertex vertex) 
			{
				mutator.setInitial(vertex);
			}

			@Override
			public void addToCompatibility(@SuppressWarnings("unused") CmpVertex astate, 
					@SuppressWarnings("unused") CmpVertex bstate, @SuppressWarnings("unused") JUConstants.PAIRCOMPATIBILITY value) {
				// does not do anything
			}

			@Override
			public void addRelabelling(VertID astate, VertID bstate) {
				renameVertex(astate, "["+bstate+"] ",oldVerticesToNew);
			}

			@Override
			public void addVertex(CmpVertex vertex) {
				mutator.addNewVertex(vertex);
			}

			@SuppressWarnings("unused")
			@Override
			public void removeFromCompatibility(CmpVertex astate, CmpVertex bstate, JUConstants.PAIRCOMPATIBILITY value) {
				// does not do anything
			}
		});

		for(Entry<StatePair,TransitionChanges> pc:pairToNumberOfChanges.entrySet())
		{
			CmpVertex from = pc.getKey().getQ(), to = pc.getKey().getR();
			Map<Label, Map<String, Color>> lbl = transitionAnnotation.computeIfAbsent(from.getStringId(), k -> new TreeMap<>());
			for(Entry<Label,List<CmpVertex>> transition:outcome.transitionMatrix.get(from).entrySet())
				for(CmpVertex target:outcome.getTargets(transition.getValue()))
					if (to.equals(target))
					{
						Map<String, Color> targetToColour = lbl.computeIfAbsent(transition.getKey(), k -> new TreeMap<>());
						// this is the first annotation for the specific target state
						targetToColour.put(to.getStringId(),pc.getValue().getColor(a.config));
					}
		}
		// There are a few kinds of states, those from the A graph which remain,
		// those which are removed and perhaps replaced by states from B with the same names
		// those from B which correspond to some states of A, these are ignored.
		// those from B which are new, these are added, as long as their names do not intersect 
		// names of existing states in A, in which case such new states are given unique names.

		final Set<CmpVertex> stateOfBOrig = new TreeSet<>(newBToOrig.values());
		
		for(Entry<CmpVertex,CmpVertex> entry:aTOb.entrySet()) 
			stateOfBOrig.remove(newBToOrig.get(entry.getValue()));// states of B which participate in key pairs do not correspond to vertices of A which will be kept.

		for(CmpVertex vertex:statesOfA)
			if (!aTOb.containsKey(vertex))
			{// not a key vertex, so it will either be reused if there is a vertex of the same name in B or throw away if there is not any.
				if (!stateOfBOrig.contains(vertex))
					renameVertex(vertex,"DEL",oldVerticesToNew);
				else
					// this vertex has one of the same name in B, hence it will be reused (unless it is a key vertex in which case A's vertex is used instead since we are aiming to express a diff in terms of states of A).
					renameVertex(vertex,"KEPT",oldVerticesToNew);
			}
		
		for(CmpVertex vertex:duplicates)
			renameVertex(vertex,"DUP",oldVerticesToNew);
		
		for(CmpVertex vertex:stateOfBOrig)
			if (!statesOfA.contains(vertex))
				renameVertex(vertex, "ADD",oldVerticesToNew);
		Map<CmpVertex,PairScore> initialKeyA = new TreeMap<>();
		for(PairScore pair:initialKeyPairs) initialKeyA.put(pair.getQ(),pair);
		
		for(Entry<CmpVertex,CmpVertex> pair:aTOb.entrySet()) 
		{
			if (initialKeyA.containsKey(pair.getKey()))
			{
				PairScore pairscore = initialKeyA.get(pair.getKey());
				renameVertex(pair.getKey(),"(K "+pairscore.getScore()+","+pairscore.getAnotherScore()+"="+newBToOrig.get(pair.getValue())+")",oldVerticesToNew);
			}
			else
			{
				if (pairScores != null)
				{
					int scorePosition = pairScores[forward.vertexToIntNR(pair.getKey(),pair.getValue())];
					double score = scoresForward[scorePosition] + scoresInverse[scorePosition];
					renameVertex(pair.getKey(),"(P="+newBToOrig.get(pair.getValue())+","+score+")",oldVerticesToNew);
				}
				else
					renameVertex(pair.getKey(),"(P)",oldVerticesToNew);
			}
		}
		
		/*
		for(CmpVertex stateA:statesOfA)
			for(CmpVertex stateB:statesOfB)
		{
			int scorePosition = pairScores[forward.vertexToIntNR(stateA,stateB)];double score = scoresForward[scorePosition] + scoresInverse[scorePosition];
			System.out.println(stateA.toString()+","+newBToOrig.get(stateB).toString()+" : "+score);
		}
		*/
		
		Map<String,String> labelling = new TreeMap<>();
		for(Entry<VertID,VertID> entry:oldVerticesToNew.entrySet())
			labelling.put(entry.getKey().toString(),entry.getValue().toString());
		DirectedSparseGraph gr = outcome.pathroutines.getGraph();
		
		gr.addUserDatum(JUConstants.VERTEX, labelling, UserData.SHARED);
		gr.addUserDatum(JUConstants.EDGE, transitionAnnotation, UserData.SHARED);
		return gr;
	}
}	
	
