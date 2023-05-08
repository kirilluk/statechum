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

package statechum.analysis.learning.rpnicore;

import static statechum.analysis.learning.rpnicore.FsmParserStatechum.buildLearnerGraph;

import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.collections.HashMapWithSearch;
import statechum.*;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.collections.MapWithSearch;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTA_FSMStructure;
import statechum.model.testset.PrefixFreeCollection;
import statechum.model.testset.SlowPrefixFreeCollection;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

@SuppressWarnings("Convert2Diamond")
public class WMethod
{
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	WMethod(LearnerGraph computeStateScores) 
	{
		coregraph = computeStateScores;
	}

	private Collection<List<Label>> fullTestSet;
	private Collection<List<Label>> transitionCover, characterisationSet;
	
	public Collection<List<Label>> getFullTestSet(int extraStates){
		if (fullTestSet == null)
			fullTestSet = computeNewTestSet(extraStates);
		return fullTestSet;
	}
	
	public Collection<List<Label>> getCharacterisationSet() {
		return characterisationSet;
	}

	public Collection<List<Label>> getTransitionCover() {
		return transitionCover;
	}

	/** Given a database of sequences and a test sequence, converts a given sequence into
	 * a fundamental test sequence and appends a the result to the <em>sequences</em> set 
	 * if the fundamental test sequence is not a prefix of an existing sequence in that set.  
	 */
	public void appendSequence(PrefixFreeCollection sequences, List<Label> path)
	{
		List<Label> seq = coregraph.paths.truncateSequence(path);
		sequences.addSequence(seq);
	}
	
	public void appendAllSequences(PrefixFreeCollection sequences, List<List<Label>> paths)
	{
		for(List<Label> path:paths)
			appendSequence(sequences, path);
	}
	
	public Collection<List<Label>> computeOldTestSet(int numberOfExtraStates)
	{
		Set<Label> alphabet =  coregraph.learnerCache.getAlphabet();
		List<List<Label>> partialSet = coregraph.pathroutines.computeStateCover(coregraph.getInit());
		characterisationSet = computeWSet_reducedmemory(coregraph);if (characterisationSet.isEmpty()) characterisationSet.add(Collections.emptyList());
		transitionCover = crossWithSet(partialSet,alphabet);transitionCover.addAll(partialSet);

		SlowPrefixFreeCollection testsequenceCollection = new SlowPrefixFreeCollection();
		
		appendAllSequences(testsequenceCollection, cross(partialSet,characterisationSet));
		for(int i=0;i<=numberOfExtraStates;i++)
		{
			partialSet=crossWithSet(partialSet,alphabet);
			appendAllSequences(testsequenceCollection, cross(partialSet,characterisationSet));
		}
		
		return testsequenceCollection.getData();
	}

	public PTASequenceEngine computeNewTestSet(CmpVertex initialState, int numberOfExtraStates)
	{
		Set<Label> alphabet =  coregraph.learnerCache.getAlphabet();
		List<List<Label>> stateCover = coregraph.pathroutines.computeStateCover(initialState);
		characterisationSet = computeWSet_reducedmemory(coregraph);if (characterisationSet.isEmpty()) characterisationSet.add(Collections.emptyList());
		transitionCover = crossWithSet(stateCover,alphabet);transitionCover.addAll(stateCover);

		PTASequenceEngine engine = new PTA_FSMStructure(coregraph,initialState);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(stateCover);
		
		partialPTA.cross(characterisationSet);
		for(int i=0;i<=numberOfExtraStates;i++)
		{
			partialPTA = partialPTA.crossWithSet(alphabet);
			partialPTA.cross(characterisationSet);
		}
		
		return engine;
	}
	
	public Collection<List<Label>> computeNewTestSet(int numberOfExtraStates)
	{
		return computeNewTestSet(coregraph.getInit(),numberOfExtraStates).getData();
	}
	
	/** Checks if the supplied FSM has equivalent states. */
	public static boolean checkEquivalentStates(LearnerGraph fsm)
	{
		try
		{
			computeWSet_reducedmemory(fsm);
		}
		catch(EquivalentStatesException e)
		{
			return true;
		}
		return false;
	}
	
	public static <ELEM> List<List<ELEM>> cross(Collection<List<ELEM>> a, Collection<List<ELEM>> b){
		List<List<ELEM>> returnVect = new LinkedList<List<ELEM>>();
		for(List<ELEM> elemA:a){
			for(List<ELEM> elemB:b) {
				List<ELEM> cross = new LinkedList<ELEM>();
				cross.addAll(elemA);
				cross.addAll(elemB);
				returnVect.add( cross );
			}
		}
		return returnVect;
	}

	/** Important: this one destroys the first operand. */
	public static <ELEM> List<List<ELEM>> crossWithSet_One(List<List<ELEM>> a, Collection<ELEM> b)
	{
		if (b.size() == 1)
		{
			ELEM element = b.iterator().next();
			for(List<ELEM> elemA:a)
				elemA.add(element);
			return a;
		}

		return crossWithSet(a, b);
	}
	
	public static <ELEM> List<List<ELEM>> crossWithSet(Collection<List<ELEM>> a, Collection<ELEM> b){
		List<List<ELEM>> returnVect = new LinkedList<List<ELEM>>();
		for(List<ELEM> elemA:a){
			for(ELEM elemB:b) {
				List<ELEM> cross = new LinkedList<ELEM>(elemA);
				cross.add(elemB);
				returnVect.add( cross );
			}
		}
		return returnVect;
	}
	
	/** Turns a collection of strings into a collection consisting of singleton lists, 
	 * each of which containing an element of <i>alphabet</i>.
	 * 
	 * @param alphabet the collection to turn into 
	 * @return a collection of singleton sequences.
	 */
	public static <ELEM> List<List<ELEM>> makeSingleton(Collection<ELEM> alphabet){
		List<List<ELEM>> functionList = new LinkedList<List<ELEM>>();
		for(ELEM stim:alphabet){
			List<ELEM> path = new LinkedList<ELEM>();
			path.add(stim);
			functionList.add(path);
		}
		return functionList;
	}
	
	public static class EquivalentStatesException extends Error 
	{
		/**
		 *  For serialization.
		 */
		private static final long serialVersionUID = 6988899034488999997L;
		
		private final Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> statesToReduce;
		
		public Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> getEquivalentStates()
		{
			Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> equivalentStates = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			for(EquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:getStatesToComputeReduction())
				if (eqClass.getStates().size() > 1)
					equivalentStates.add(eqClass);
			
			return equivalentStates;
		}
		
		/** Returns a collection which can be passed to <em>mergeCollectionOfVertices</em> in order to compute a reduced FSM. */
		public Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> getStatesToComputeReduction()
		{
			return statesToReduce;
		}
		
		public static Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> IdentifyEquivalentStates(
				final Map<CmpVertex,Integer> equivalenceClasses,LearnerGraph graph, CmpVertex sink)
		{
			Map<Integer,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> mapOfEquivalentStates = new TreeMap<Integer,EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> equivalentStates = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			int number =0;
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
				if (stateA.getKey() != sink)
				{
					EquivalenceClass<CmpVertex,LearnerGraphCachedData> equiv = mapOfEquivalentStates.get(stateA.getValue());
					if (equiv == null)
					{
						equiv = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(number++,graph);mapOfEquivalentStates.put(stateA.getValue(), equiv);
					}
					try {
						equiv.mergeWith(stateA.getKey(),graph.transitionMatrix.get(stateA.getKey()).entrySet());
					} catch (IncompatibleStatesException e) {
						Helper.throwUnchecked("equivalent states cannot be incompatible", e);
					}
				}
			
			boolean equivFound = false;
			for(Entry<Integer,EquivalenceClass<CmpVertex,LearnerGraphCachedData>> entry:mapOfEquivalentStates.entrySet())
			{
				equivalentStates.add(entry.getValue());
				if (entry.getValue().getStates().size() > 1) equivFound=true;
			}
			assert equivFound: "equivalent states were not found";
			return equivalentStates; 
		}
		
		public static EquivalentStatesException construct(final Map<CmpVertex,Integer> equivalenceClasses,LearnerGraph graph, CmpVertex sink)
		{
			return new EquivalentStatesException(IdentifyEquivalentStates(equivalenceClasses,graph,sink));
		}
		
		private EquivalentStatesException(Collection<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> argStatesToReduce)
		{
			statesToReduce=argStatesToReduce;
		}
		
		@Override
		public String toString()
		{
			Iterator<CmpVertex> eqStatesIterator = getEquivalentStates().iterator().next().getStates().iterator();
			return "There are "+getEquivalentStates().size()+" equivalent states, such as "+eqStatesIterator.next()+ " and "+eqStatesIterator.next();
		}
	}
	
	
	/** Maps transitions from a state to equivalence classes of the states those transitions lead to.
	 * Assumes that there are not many outgoing transitions from each state, otherwise {@link HashMapWithSearch} has to be used.
	 */
	@SuppressWarnings("Convert2Diamond")
	private static class TransitionRowEqClass extends HashMap<Label,Integer>
	{
		private final int origEqClass;
		
		public TransitionRowEqClass(int eqClass)
		{
			origEqClass = eqClass;
		}
		
		@Override
		public int hashCode()
		{
			int result = super.hashCode(); 
				/*
			for(Entry<String,Integer> entry:entrySet())
				if (entry.getValue() != sinkEqClass)
					result+=entry.getKey().hashCode()^entry.getValue().hashCode();
			*/
			return result*17+origEqClass;
		}		
		
		/** All non-existing transitions lead to a sink state, hence we filter all transitions
		 * to a sink equivalent class out during the computation.
		 */
		@Override
		public boolean equals(Object obj)
		{
			if (!(obj instanceof TransitionRowEqClass))
				return false;
			TransitionRowEqClass what = (TransitionRowEqClass) obj;
			
			if (origEqClass != what.origEqClass) return false;
/*
			Iterator<Entry<String,Integer>> mapAiter = entrySet().iterator();
			while(mapAiter.hasNext())
			{
				Entry<String,Integer> enA = mapAiter.next();
				Integer mapBvalue = what.get(enA.getKey());if (mapBvalue == null) mapBvalue = sinkEqClass;
				if (!enA.getValue().equals(mapBvalue)) // different equivalence classes
					return false;
			}
			
			Iterator<Entry<String,Integer>> mapBiter = what.entrySet().iterator();
			while(mapBiter.hasNext())
			{
				Entry<String,Integer> enB = mapBiter.next();
				Integer mapAvalue = get(enB.getKey());if (mapAvalue == null) mapAvalue = sinkEqClass;
				if (!enB.getValue().equals(mapAvalue))
					return false;
			}
			
			return true;
			*/
			
			return super.equals(what);
		}
		
		/**
		 * ID for serialisation
		 */
		private static final long serialVersionUID = 5657856564072318410L;

		/** Given two states and a map from them to equivalence classes, this method computes 
		 * a set of inputs which distinguishes between states.
		 *  
		 * @param what the row to compare with
		 * @return a set of distinguishing inputs 
		 */
		public Set<Label> computeDistinguishingLabel(TransitionRowEqClass what)
		{
			Set<Label> distInputs = new HashSet<Label>();

			for (Entry<Label, Integer> enA : entrySet()) {
				Integer mapBvalue = what.get(enA.getKey());
				if (!enA.getValue().equals(mapBvalue)) // different equivalence classes
					distInputs.add(enA.getKey());
			}

			for (Entry<Label, Integer> enB : what.entrySet()) {
				Integer mapAvalue = get(enB.getKey());
				if (!enB.getValue().equals(mapAvalue))
					distInputs.add(enB.getKey());
			}
			
			assert !distInputs.isEmpty():" cannot find a distinguishing label between "+this+" and "+what;
			return distInputs;
		}

		/** The only purpose of this method is to identify a row associated with the newly-added sink state. If there
		 * are a few states with that same row, they will always be lumped together. When we are finished splitting 
		 * states, this method makes it possible to count the number of states with the behaviour of the sink.
		 * (a circle of reject-states will be turned into a number of separate reject-states due to the filtering
		 * of transitions to the sink equivalence class.
		 * 
		 * @return true if the state appears to be like a sink state.
		 */
		public boolean looksLikeSink(int sinkEqClass)
		{
			return isEmpty() && origEqClass == sinkEqClass;
		}
	}
	
	/** Computes a characterising set, regardless of the presence of unreachable states.
	 * <br>
	 * IMPORTANT: this is not maintained particularly well and has no support for ignoring 
	 * pairs of states which are equivalent (except for those equivalent to the sink state). 
	 * <br>
	 * Since we do not know in advance whether there are any states behaviourally-equivalent to a sink state
	 * (and there could be a few of them, connected in a ring), a sink state is explicitly added.
	 * @param fsm the machine to process
	 * @return characterising set
	 */
	public static List<List<Label>> computeWSetOrig(LearnerGraph fsm) throws EquivalentStatesException
	{ 
		MapWithSearch<VertID,CmpVertex,Integer> equivalenceClasses = new HashMapWithSearch<VertID,CmpVertex,Integer>(fsm.getStateNumber()),
			newEquivClasses = new HashMapWithSearch<VertID,CmpVertex,Integer>(fsm.getStateNumber());
		Map<Map<Label,Integer>,Integer> sortedRows = new HashMap<Map<Label,Integer>,Integer>();
		Map<CmpVertex,Map<CmpVertex,List<Label>>> Wdata = new HashMap<CmpVertex,Map<CmpVertex,List<Label>>>(fsm.getStateNumber());
		CmpVertex sink = generateSinkState(fsm);
		for(CmpVertex state:fsm.transitionMatrix.keySet()) 
			equivalenceClasses.put(state, state.isAccept()?1:0);
		equivalenceClasses.put(sink, 0);
		
		for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
		{
			MapWithSearch<VertID,CmpVertex,List<Label>> row = new HashMapWithSearch<VertID,CmpVertex,List<Label>>(fsm.getStateNumber());
			Wdata.put(stateA.getKey(), row);

			for (Entry<CmpVertex, Integer> stateB : equivalenceClasses.entrySet()) {
				if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
				row.put(stateB.getKey(), new LinkedList<Label>());
			}
		}
		
		int equivalenceClassNumber = 0,oldEquivalenceClassNumber;
		int statesEquivalentToSink;
		do
		{
			oldEquivalenceClassNumber = equivalenceClassNumber;statesEquivalentToSink = 0;
			Map<CmpVertex,TransitionRowEqClass> newMap = new HashMap<CmpVertex,TransitionRowEqClass>();
			equivalenceClassNumber = 0;sortedRows.clear();newEquivClasses.clear();
			int sinkEqClass = equivalenceClasses.get(sink);
			for(CmpVertex stateA:equivalenceClasses.keySet())
			{
				TransitionRowEqClass map = new TransitionRowEqClass(equivalenceClasses.get(stateA));
				Map<Label,CmpVertex> labelNSmap = fsm.transitionMatrix.get(stateA);
				if (labelNSmap != null)
					for(Entry<Label,CmpVertex> labelstate:labelNSmap.entrySet())
					{
						int targetEqClass = equivalenceClasses.get(labelstate.getValue());
						if (targetEqClass != sinkEqClass) // filter out all transitions to sink
							map.put(labelstate.getKey(), targetEqClass);
					}
				newMap.put(stateA, map);
				if (map.looksLikeSink(sinkEqClass)) statesEquivalentToSink++;
				if (!sortedRows.containsKey(map))
				{
					equivalenceClassNumber++;
					sortedRows.put(map,equivalenceClassNumber);newEquivClasses.put(stateA, equivalenceClassNumber);
				}
				else
					newEquivClasses.put(stateA, sortedRows.get(map));
			}

			//System.out.println(newEquivClasses);
			//System.out.println("v163: "+fsm.transitionMatrix.get(fsm.findVertex("V163")));
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				for (Entry<CmpVertex, Integer> stateB : equivalenceClasses.entrySet()) {
					if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.

					if (stateA.getValue().equals(stateB.getValue()) &&
							!newEquivClasses.get(stateA.getKey()).equals(newEquivClasses.get(stateB.getKey()))) {// the two states used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.

						List<Label> Wsequence;
						if (Wdata.get(stateA.getKey()).containsKey(stateB.getKey()))
							Wsequence = Wdata.get(stateA.getKey()).get(stateB.getKey());// depending on the ordering in the matrix, either (A,B) or (B,A) should be defined.
						else
							Wsequence = Wdata.get(stateB.getKey()).get(stateA.getKey());
						assert Wsequence != null : "In states (" + stateA.getKey() + "," + stateB.getKey() + ") Wsequence is null";
						assert Wsequence.isEmpty() : "In states (" + stateA.getKey() + "," + stateB.getKey() + ") Wsequence is non-empty and contains " + Wsequence;

						// the two states used to be equivalent but not any more, find the different element
						Label label = newMap.get(stateA.getKey()).computeDistinguishingLabel(newMap.get(stateB.getKey())).iterator().next();
						//System.out.println(stateA.getKey()+" - "+stateB.getKey());
						/*if (
								stateA.getKey().getID().equals(VertexID.parseID("V148")))
							System.out.println("v148 v.s. "+stateB.getKey()+" label: "+label);*/
						CmpVertex toA = stateA.getKey() == sink ? sink : fsm.transitionMatrix.get(stateA.getKey()).get(label);
						if (toA == null) toA = sink;
						CmpVertex toB = stateB.getKey() == sink ? sink : fsm.transitionMatrix.get(stateB.getKey()).get(label);
						if (toB == null) toB = sink;
						Wsequence.add(label);
						List<Label> Wprevious;
						if (Wdata.get(toA).containsKey(toB))
							Wprevious = Wdata.get(toA).get(toB);// depending on the ordering in the matrix, either (A,B) or (B,A) should be defined.
						else
							Wprevious = Wdata.get(toB).get(toA);
						assert Wprevious != null : "In states (" + stateA.getKey() + "," + stateB.getKey() + ") previous pair (" + toA + "," + toB + ") has a null sequence";
						assert !Wprevious.isEmpty() || toA.isAccept() != toB.isAccept() : "In states (" + stateA.getKey() + "," + stateB.getKey() + "), separating input \"" + label + "\" previous pair (" + toA + "(" + toA.isAccept() + ")," + toB + "(" + toB.isAccept() + ") was not separated";

						Wsequence.addAll(Wprevious);
					}
				}			
			}			
			equivalenceClasses = newEquivClasses;newEquivClasses = new HashMapWithSearch<VertID,CmpVertex,Integer>(fsm.config.getMaxAcceptStateNumber());
		}
		while(equivalenceClassNumber > oldEquivalenceClassNumber);

		List<List<Label>> result = new LinkedList<List<Label>>();
		assert statesEquivalentToSink > 0: "missing equivalence class associated with sink state";
		if ( (statesEquivalentToSink <= 2 && oldEquivalenceClassNumber == fsm.transitionMatrix.size()+2-statesEquivalentToSink) 
			|| fsm.config.getEquivalentStatesAllowedForW()
			)
		{
			boolean sinkAsRealState =  !fsm.config.isPrefixClosed() && statesEquivalentToSink == 1;
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
				if (sinkAsRealState || stateA.getKey() != sink)
				{
					for (Entry<CmpVertex, Integer> stateB : equivalenceClasses.entrySet()) {
						if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
						if (sinkAsRealState || stateB.getKey() != sink) {
							List<Label> seq = Wdata.get(stateA.getKey()).get(stateB.getKey());
							assert seq != null : "equivalenceClasses uses a different state ordering to that of LearnerGraphs";
							result.add(seq);
						}
					}
				}			
		}
		else
			// report equivalent states
			throw EquivalentStatesException.construct(equivalenceClasses,fsm,sink);
		
		return result;
	}

	/** Given a map from pairs of states to sets of labels which distinguish between these pairs, 
	 * this method returns a label which will distinguish the largest number of pairs.
	 * 
	 * @param distinguishingLabels a map from pairs of states to sets of distinguishing labels.
	 * @return computes a label distinguishing the largest number of states
	 */ 
	public static Label computeTopLabel(Map<CmpVertex,Map<CmpVertex,Set<Label>>> distinguishingLabels)
	{
		Map<Label,AtomicInteger> distLabelUsage = new HashMap<Label,AtomicInteger>();// reset the histogram ...
		Label topLabel = null;int topLabelCounter = -1;// ... and the top element
		for(Entry<CmpVertex,Map<CmpVertex,Set<Label>>> stateAdist:distinguishingLabels.entrySet())
		{
			for(Entry<CmpVertex,Set<Label>> stateB:stateAdist.getValue().entrySet())
			{// the two states used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.
				for(Label label:stateB.getValue())
				{
					AtomicInteger counter = distLabelUsage.get(label);
					if (counter == null) counter = new AtomicInteger(1);else counter.addAndGet(1);
					distLabelUsage.put(label, counter);
					if (counter.get() > topLabelCounter)
					{
						topLabel = label;topLabelCounter = counter.get();// record which label is the top now
					}
				}
			}
			
		}
		return topLabel;
	}

	
	/** Given an FSM, this method generates a completely new sink state which will be the target of
	 * transitions for inputs where there is no existing transition.
	 * 
	 * @param fsm graph to look for a sink state in
	 * @return sink state
	 */
	public static CmpVertex generateSinkState(LearnerGraph fsm)
	{/*
		CmpVertex sink = null;
		for(Entry<CmpVertex,Map<String,CmpVertex>> state:fsm.transitionMatrix.entrySet()) 
			if (!state.getKey().isAccept() && state.getValue().isEmpty())
			{
				sink = state.getKey();break;
			}
		if (sink == null)
		*/
		CmpVertex sink = AbstractLearnerGraph.generateNewCmpVertex(fsm.nextID(JUConstants.VERTEXLABEL.REJECT,false), fsm.config);
		sink.setAccept(false);return sink;
	}

	static final int W_NOPREV=-1, W_INDIST=-2;

	/** Computes a characterising set, assuming that there are no unreachable states (with unreachable states,
	 * it will take a bit longer to perform the computation). 
	 * Additionally, it attempts to reduce the size of W and conserve computer memory. W is not reduced as much
	 * as per computeWSet_reducedw though because the distribution of labels is not recomputed each time we 
	 * separate a pair, but is performed once per step.
	 * <br>
	 * In order to perform W computation in the case when we are building a non-prefix-closed set, a sink state 
	 * has to be added because machines are typically incomplete and we have to verify that when a label is not 
	 * implemented, it leads to the sink state - this requires distinguishing between sink and all other states.
	 * Adding such a sink state means changing the graph, hence a copy is made and modified.
	 * @param fsmOrig the machine to process
	 * @return characterising set
	 */
	public static Collection<List<Label>> computeWSet_reducedmemory(LearnerGraph fsmOrig) throws EquivalentStatesException
	{
		Configuration copyConfig = fsmOrig.config.copy();copyConfig.setLearnerCloneGraph(false);
		LearnerGraph fsm = new LearnerGraph(fsmOrig,copyConfig);
		CmpVertex sink = generateSinkState(fsm);fsm.transitionMatrix.put(sink, fsm.createNewRow());
		Map<CmpVertex,Integer> equivalenceClasses = new HashMap<CmpVertex,Integer>(fsm.getStateNumber()),
				newEquivClasses = new HashMap<CmpVertex,Integer>(fsm.getStateNumber());
		
		// Since this one associates maps with numbers, make it Hash set so that fewer computations have to be performed.
		Map<Map<Label,Integer>,Integer> sortedRows = new HashMap<Map<Label,Integer>,Integer>();
		int[] WNext = new int[fsm.transitionMatrix.size()*(fsm.transitionMatrix.size()+1)/2];
		Label[] WChar = new Label[fsm.transitionMatrix.size()*(fsm.transitionMatrix.size()+1)/2];
		final Map<Label,AtomicInteger> distinguishingLabels = new HashMap<Label,AtomicInteger>();

		boolean mealy = fsm.config.getLabelKind() == Configuration.LABELKIND.LABEL_INPUT_OUTPUT;

		// It is important to iterate through the same collection because different collections
		// such as fsm.transitionMatrix and equivalenceClasses may have a different order of elements
		// and thus I would not be going through a triangular matrix of pairs.
		if (mealy) {
			constructMealyEquivalenceClasses(fsm, sink, equivalenceClasses);
			constructMealy_WNextWChar(fsm, equivalenceClasses, WNext, WChar);
		}
		else {
			constructDFAEquivalenceClasses(fsm, sink, equivalenceClasses);
			constructDFA_WNextWChar(fsm, equivalenceClasses, WNext, WChar);
		}

		int equivalenceClassNumber = 0,oldEquivalenceClassNumber;
		int statesEquivalentToSink;
		do
		{
			oldEquivalenceClassNumber = equivalenceClassNumber;statesEquivalentToSink = 0;
			MapWithSearch<VertID,CmpVertex,TransitionRowEqClass> newMap = new HashMapWithSearch<VertID,CmpVertex,TransitionRowEqClass>(fsm.getStateNumber());
			equivalenceClassNumber = 0;sortedRows.clear();newEquivClasses.clear();
			int sinkEqClass = equivalenceClasses.get(sink);
			for(Map.Entry<CmpVertex,Integer> stateA_entry:equivalenceClasses.entrySet())
			{
				CmpVertex stateA = stateA_entry.getKey();
				TransitionRowEqClass map = new TransitionRowEqClass(stateA_entry.getValue());
				Map<Label,CmpVertex> labelNSmap = fsm.transitionMatrix.get(stateA);
				if (labelNSmap != null)
					for(Entry<Label,CmpVertex> labelstate:labelNSmap.entrySet())
					{
						int targetEqClass = equivalenceClasses.get(labelstate.getValue());
						if (mealy || targetEqClass != sinkEqClass) // filter out all transitions to sink - this is important because otherwise
							// vectors for two reject-states with transitions to a sink state (equivalence class 0)
							// { a->0, b->0 } and { a->0 } look superficially different even though they
							// both denote states accepting an empty language.
							// Note that any real sink states would belong to the same equivalence class as our sink state.
							map.put(labelstate.getKey(), targetEqClass);
					}
				
				newMap.put(stateA, map);
				if ( (map.isEmpty() && mealy) || map.looksLikeSink(sinkEqClass))
					statesEquivalentToSink++;
				if (!sortedRows.containsKey(map))
				{
					sortedRows.put(map,equivalenceClassNumber);newEquivClasses.put(stateA, equivalenceClassNumber);
					equivalenceClassNumber++;
				}
				else
					newEquivClasses.put(stateA, sortedRows.get(map));
				
				//System.out.println("state "+stateA+" has a map of "+map+" and a number "+sortedRows.get(map));
			}


			distinguishingLabels.clear();// clear a map from pairs of states to sets of labels which distinguish between them
			
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				for (Entry<CmpVertex, Integer> stateB : equivalenceClasses.entrySet()) {
					if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.

					if (stateA.getValue().equals(stateB.getValue()) &&
							!newEquivClasses.get(stateA.getKey()).equals(newEquivClasses.get(stateB.getKey()))) {// the two states
						// used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.

						// the two states used to be equivalent but not any more, find inputs which 
						// distinguish between them and update the histogram to count the number
						// of inputs which can be used distinguish between states at this stage.
						for (Label distLabel : newMap.get(stateA.getKey()).computeDistinguishingLabel(newMap.get(stateB.getKey())))
							distinguishingLabels.computeIfAbsent(distLabel, k->new AtomicInteger(0)).addAndGet(1);
					}
				}
			}

			// distinguishingLabels contains all labels we may use; the choice of an optimal subset is NP, hence we simply pick
			// those which look best until we distinguish all states.
			ArrayList<Label> labelList = new ArrayList<Label>(distinguishingLabels.size());labelList.addAll(0, distinguishingLabels.keySet());
			labelList.sort(
					(o1, o2) -> {
				int diffInNumberOfdistStates = -distinguishingLabels.get(o1).get() + distinguishingLabels.get(o2).get();
				if (diffInNumberOfdistStates != 0) return diffInNumberOfdistStates;
				return o1.compareTo(o2);// otherwise, just compare strings.
			});
			
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				for (Entry<CmpVertex, Integer> stateB : equivalenceClasses.entrySet()) {
					if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
					//System.out.println("looking at "+stateA.getKey()+", "+stateB.getKey()+" eq classes: "+stateA.getValue()+", "+stateB.getValue());
					if (stateA.getValue().equals(stateB.getValue()) &&
							!newEquivClasses.get(stateA.getKey()).equals(newEquivClasses.get(stateB.getKey()))) {// the two states used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.
						Set<Label> distLabels = newMap.get(stateA.getKey()).computeDistinguishingLabel(newMap.get(stateB.getKey()));
						Label topLabel = null;
						Iterator<Label> topLabelIter = labelList.iterator();
						while (topLabel == null) {
							Label lbl = topLabelIter.next();
							if (distLabels.contains(lbl)) topLabel = lbl;
						}

						CmpVertex toA = stateA.getKey() == sink ? sink : fsm.transitionMatrix.get(stateA.getKey()).get(topLabel);
						if (toA == null) toA = sink;
						CmpVertex toB = stateB.getKey() == sink ? sink : fsm.transitionMatrix.get(stateB.getKey()).get(topLabel);
						if (toB == null) toB = sink;
						int index = fsm.wmethod.vertexToInt(stateA.getKey(), stateB.getKey());
						assert WChar[index] == null : "In states (" + stateA.getKey() + "," + stateB.getKey() + ") Wsequence is non-empty and contains " + WChar[index];
						WChar[index] = topLabel;
						int previous = fsm.wmethod.vertexToInt(toA, toB);
						assert toA.isAccept() != toB.isAccept() || WChar[previous] != null : "In states (" + stateA.getKey() + "," + stateB.getKey() + ") previous pair (" + toA + "," + toB + ") has a null sequence";
						WNext[index] = previous;
					}
				}			
			}
			
			equivalenceClasses = newEquivClasses;newEquivClasses = new HashMapWithSearch<VertID,CmpVertex,Integer>(fsm.getStateNumber());
		}
		while(equivalenceClassNumber > oldEquivalenceClassNumber);

		Collection<List<Label>> result = new HashSet<List<Label>>();
		assert statesEquivalentToSink > 0: "missing equivalence class associated with sink state";
		// If all states have been distinguished, statesEquivalentToSink can be either 1 (no existing state is like sink) or 2 (there is an existing sink-like state).
		// (to test where sink is partial)
		// If statesEquivalentToSink is 1, oldEquivalenceClassNumber == fsm.transitionMatrix.size()
		// for 2, oldEquivalenceClassNumber == fsm.transitionMatrix.size()-1
		
		if ((statesEquivalentToSink <= 2 && oldEquivalenceClassNumber == fsm.transitionMatrix.size()+1-statesEquivalentToSink )
				|| fsm.config.getEquivalentStatesAllowedForW())
		{
			// This one means that we only consider our artificial sink state as a real state
			// if there is no graph state which accepts an empty language.
			boolean sinkAsRealState =  !fsm.config.isPrefixClosed() && statesEquivalentToSink == 1;
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
				if (sinkAsRealState || stateA.getKey() != sink)
				{
					for (Entry<CmpVertex, Integer> stateB : equivalenceClasses.entrySet()) {
						if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
						if (sinkAsRealState || stateB.getKey() != sink) {
							LinkedList<Label> seq = new LinkedList<Label>();
							int index = fsm.wmethod.vertexToInt(stateA.getKey(), stateB.getKey());
							assert index >= 0;
							if (WNext[index] != W_INDIST) {// stateA and stateB have been separated.

								Label elementToAdd = WChar[index];
								while (elementToAdd != null) {
									seq.add(elementToAdd);
									if (WNext[index] == W_NOPREV) // this is the special case for the Mealy automata where the last character is not an epsilon.
										break;
									index = WNext[index];
									elementToAdd = WChar[index];
								}
								assert WNext[index] == W_NOPREV : "empty element not at end of sequence";
								result.add(seq);
							}
						}
					}
				}			
		}
		else
			// report equivalent states
			throw EquivalentStatesException.construct(equivalenceClasses,fsm,sink);

		return result;
	}

	/**
	 * Constructs equivalence classes based on accept/reject states.
	 *
	 * @param fsm                the automaton to generate equivalence classes for
	 * @param sinkState          the sink state (will be equivalence to an actual sink state if present in the graph).
	 * @param equivalenceClasses map from states to equivalence classes
	 */
	public static void constructDFAEquivalenceClasses(LearnerGraph fsm, CmpVertex sinkState, Map<CmpVertex,Integer> equivalenceClasses) {
		assert fsm.config.getLabelKind() != Configuration.LABELKIND.LABEL_INPUT_OUTPUT;

		for(CmpVertex state:fsm.transitionMatrix.keySet())
			equivalenceClasses.put(state, state.isAccept()?1:0);
		if (sinkState != null)
			equivalenceClasses.put(sinkState, 0);
	}

	/** Takes an automaton and equivalence classes and constructs a list giving prioritiy to
	 * labels that distinguish the most pairs of states.
	 *
	 * @param  fsm automaton
	 * @param equivalenceClasses equivalence classes of states
	 * @return list of labels. The first is most 'distinguishing'.
	 */

	private static List<String> constructMealyTopLabelsForFirstStep(LearnerGraph fsm, Map<CmpVertex, Integer> equivalenceClasses) {
		final Map<String,AtomicInteger> distinguishingLabels = new HashMap<String,AtomicInteger>();

		for(Entry<CmpVertex, Integer> stateA_entry : equivalenceClasses.entrySet()) {
			CmpVertex stateA = stateA_entry.getKey();
			for (Entry<CmpVertex, Integer> stateB_entry : equivalenceClasses.entrySet()) {
				if (stateB_entry.getKey().equals(stateA)) break; // we only process a triangular subset.
				CmpVertex stateB = stateB_entry.getKey();
				MapWithSearch<Label, Label, CmpVertex> mapB = fsm.transitionMatrix.get(stateB);
				if (!stateA_entry.getValue().equals(stateB_entry.getValue())) {
					for (Entry<Label, CmpVertex> enA : fsm.transitionMatrix.get(stateA).entrySet()) {
						LabelInputOutput keyA = (LabelInputOutput) enA.getKey();
						LabelInputOutput keyB = null;
						if (mapB != null)
							keyB = (LabelInputOutput) mapB.findKey(enA.getKey());
						if (keyB == null || !Objects.equals(keyA.output, keyB.output)) // either no defined transition or different outputs
							distinguishingLabels.computeIfAbsent(keyA.input, k -> new AtomicInteger(0)).addAndGet(1);
					}

					if (mapB != null)
						for (Entry<Label, CmpVertex> enB : mapB.entrySet()) {
							LabelInputOutput keyB = (LabelInputOutput) enB.getKey();
							MapWithSearch<Label, Label, CmpVertex> mapA = fsm.transitionMatrix.get(stateA);
							LabelInputOutput keyA = (LabelInputOutput) mapA.findKey(enB.getKey());
							if (keyA == null) // no defined transition
								distinguishingLabels.computeIfAbsent(keyB.input, k -> new AtomicInteger(0)).addAndGet(1);
						}
				}
			}
		}

		// distinguishingLabels contains all labels we may use; the choice of an optimal subset is NP, hence we simply pick
		// those which look best until we distinguish all states.
		ArrayList<String> labelList = new ArrayList<String>(distinguishingLabels.size());labelList.addAll(0, distinguishingLabels.keySet());
		labelList.sort(
				(o1, o2) -> {
					int diffInNumberOfdistStates = -distinguishingLabels.get(o1).get() + distinguishingLabels.get(o2).get();
					if (diffInNumberOfdistStates != 0) return diffInNumberOfdistStates;
					return o1.compareTo(o2);// otherwise, just compare strings.
				});
		return labelList;
	}

	private static void constructMealy_WNextWChar(LearnerGraph fsm, Map<CmpVertex, Integer> equivalenceClasses, int[] WNext, Label[] WChar) {
		List<String> labelList = constructMealyTopLabelsForFirstStep(fsm,equivalenceClasses);

		Set<String> distLabels = new TreeSet<>();
		for(Entry<CmpVertex, Integer> stateA_entry : equivalenceClasses.entrySet()) {
			CmpVertex stateA = stateA_entry.getKey();
			for (Entry<CmpVertex, Integer> stateB_entry : equivalenceClasses.entrySet()) {
				if (stateB_entry.getKey().equals(stateA)) break; // we only process a triangular subset.
				CmpVertex stateB = stateB_entry.getKey();
				int index = fsm.wmethod.vertexToInt(stateA, stateB);

				WChar[index] = null;

				if (!stateA_entry.getValue().equals(stateB_entry.getValue())) {
					distLabels.clear();
					for (Entry<Label, CmpVertex> enA : fsm.transitionMatrix.get(stateA).entrySet()) {
						LabelInputOutput keyA = (LabelInputOutput) enA.getKey();
						MapWithSearch<Label, Label, CmpVertex> mapB = fsm.transitionMatrix.get(stateB);
						LabelInputOutput keyB = null;
						if (mapB != null)
							keyB = (LabelInputOutput) mapB.findKey(enA.getKey());
						if (keyB == null || !Objects.equals(keyA.output, keyB.output)) // either no defined transition or different outputs
							distLabels.add(keyA.input);
					}

					for (Entry<Label, CmpVertex> enB : fsm.transitionMatrix.get(stateB).entrySet()) {
						LabelInputOutput keyB = (LabelInputOutput) enB.getKey();
						MapWithSearch<Label, Label, CmpVertex> mapA = fsm.transitionMatrix.get(stateA);
						LabelInputOutput keyA = (LabelInputOutput) mapA.findKey(enB.getKey());
						if (keyA == null) // no defined transition
							distLabels.add(keyB.input);
					}

					// now we have both the ordering of labels in labelList and the list of labels that can be distinguish stateA from stateB in distLabels.
					String topInput = null;
					Iterator<String> topLabelIter = labelList.iterator();
					while (topLabelIter.hasNext() && topInput == null) {
						String lbl = topLabelIter.next();
						if (distLabels.contains(lbl))
							topInput = lbl;
					}
					assert topInput != null : "Collection of all labels that can separate states no longer separates the pair " + stateA + " and " + stateB;
					WChar[index] = new LabelInputOutput(topInput, null);
					WNext[index] = W_NOPREV;
				}
				else
					WNext[index] = W_INDIST;

			}
		}
	}

	private static void constructMealy_NextChar(LearnerGraph fsm, Map<CmpVertex, Integer> equivalenceClasses, Map<CmpVertex,Map<CmpVertex,List<Label>>> Wdata) {
		List<String> labelList = constructMealyTopLabelsForFirstStep(fsm,equivalenceClasses);

		Set<String> distLabels = new TreeSet<>();
		for(Entry<CmpVertex, Integer> stateA_entry : equivalenceClasses.entrySet()) {
			CmpVertex stateA = stateA_entry.getKey();
			Map<CmpVertex,List<Label>> row = new HashMap<CmpVertex,List<Label>>(fsm.getStateNumber());
			Wdata.put(stateA, row);

			for (Entry<CmpVertex, Integer> stateB_entry : equivalenceClasses.entrySet()) {
				if (stateB_entry.getKey().equals(stateA)) break; // we only process a triangular subset.
				CmpVertex stateB = stateB_entry.getKey();
				List<Label> separatingLabelIfAny = new LinkedList<Label>();
				row.put(stateB, separatingLabelIfAny);

				MapWithSearch<Label, Label, CmpVertex> mapB = fsm.transitionMatrix.get(stateB);
				if (!stateA_entry.getValue().equals(stateB_entry.getValue())) {
					distLabels.clear();
					for (Entry<Label, CmpVertex> enA : fsm.transitionMatrix.get(stateA).entrySet()) {
						LabelInputOutput keyA = (LabelInputOutput) enA.getKey();
						LabelInputOutput keyB = null;
						if (mapB != null)
							keyB = (LabelInputOutput) mapB.findKey(enA.getKey());
						if (keyB == null || !Objects.equals(keyA.output, keyB.output)) // either no defined transition or different outputs
							distLabels.add(keyA.input);
					}

					if (mapB != null)
						for (Entry<Label, CmpVertex> enB : mapB.entrySet()) {
							LabelInputOutput keyB = (LabelInputOutput) enB.getKey();
							MapWithSearch<Label, Label, CmpVertex> mapA = fsm.transitionMatrix.get(stateA);
							LabelInputOutput keyA = (LabelInputOutput) mapA.findKey(enB.getKey());
							if (keyA == null) // no defined transition
								distLabels.add(keyB.input);
						}

					// now we have both the ordering of labels in labelList and the list of labels that can be distinguish stateA from stateB in distLabels.
					String topInput = null;
					Iterator<String> topLabelIter = labelList.iterator();
					while (topLabelIter.hasNext() && topInput == null) {
						String lbl = topLabelIter.next();
						if (distLabels.contains(lbl))
							topInput = lbl;
					}
					assert topInput != null : "Collection of all labels that can separate states no longer separates the pair " + stateA + " and " + stateB;
					separatingLabelIfAny.add(new LabelInputOutput(topInput,null));
				}
			}
		}
	}

	private static void  constructDFA_NextChar(LearnerGraph fsm, Map<CmpVertex,Integer> equivalenceClasses, Map<CmpVertex,Map<CmpVertex,List<Label>>> Wdata) {
		for (Entry<CmpVertex, Integer> stateA : equivalenceClasses.entrySet()) {
			Map<CmpVertex, List<Label>> row = new HashMap<CmpVertex, List<Label>>(fsm.getStateNumber());
			Wdata.put(stateA.getKey(), row);

			for (Entry<CmpVertex, Integer> stateB : equivalenceClasses.entrySet()) {
				if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
				row.put(stateB.getKey(), new LinkedList<Label>());
			}
		}
	}

	private static void constructDFA_WNextWChar(LearnerGraph fsm, Map<CmpVertex, Integer> equivalenceClasses, int[] WNext, Label[] WChar) {
		for(Entry<CmpVertex, Integer> stateA_entry : equivalenceClasses.entrySet()) {
			CmpVertex stateA = stateA_entry.getKey();
			boolean stateAaccept = stateA.isAccept();
			for (Entry<CmpVertex, Integer> stateB : equivalenceClasses.entrySet()) {
				if (stateB.getKey().equals(stateA)) break; // we only process a triangular subset.
				int index = fsm.wmethod.vertexToInt(stateA, stateB.getKey());
				WChar[index] = null;
				if (stateAaccept == stateB.getKey().isAccept())
					WNext[index] = W_INDIST;// where two states have the same acceptance conditions,
					// they cannot be distinguished at this stage.
				else
					WNext[index] = W_NOPREV;// an empty sequence distinguishes between the two,
				// the fact that seq is empty is due to WChar[index] being null.
			}
		}
	}


	/**
	 * Constructs equivalence classes based on pairwise separability of states.
	 *
	 * @param fsm                the automaton to generate equivalence classes for
	 * @param sinkState          the sink state (to permit the DFA and Mealy codebase to be shared).
	 * @param equivalenceClasses map from states to equivalence classes
	 */
	public static void constructMealyEquivalenceClasses(LearnerGraph fsm, CmpVertex sinkState, Map<CmpVertex,Integer> equivalenceClasses) {
		assert fsm.config.getLabelKind() == Configuration.LABELKIND.LABEL_INPUT_OUTPUT;

		Map<Map<String,String>,Integer> sortedRows = new HashMap<Map<String,String>,Integer>();

		int equivalenceClassNumber=0;
		if (sinkState != null) {
			sortedRows.put(new TreeMap<String,String>(),equivalenceClassNumber);equivalenceClasses.put(sinkState, equivalenceClassNumber);
			equivalenceClassNumber++;
		}

		for(Entry<CmpVertex,MapWithSearch<Label, Label, CmpVertex>> entry:fsm.transitionMatrix.entrySet())
		{
			if (!entry.getKey().isAccept() && !entry.getValue().isEmpty())
				throw new IllegalArgumentException("Unable to handle Mealy-like automata with transitions from a non-accept state "+entry.getKey());

			Map<String,String> map = new TreeMap<String,String>();
			for(Entry<Label,CmpVertex> transition:entry.getValue().entrySet())
			{
				LabelInputOutput io = (LabelInputOutput) transition.getKey();
				map.put(io.input,io.output);
			}
			if (!sortedRows.containsKey(map))
			{
				sortedRows.put(map,equivalenceClassNumber);equivalenceClasses.put(entry.getKey(), equivalenceClassNumber);
				equivalenceClassNumber++;
			}
			else
				equivalenceClasses.put(entry.getKey(), sortedRows.get(map));
		}

	}

	/** Computes a characterising set, assuming that there are no unreachable states (with unreachable states, 
	 * it will take a bit longer to perform the computation). 
	 * Additionally, it attempts to reduce the size of W.
	 * <br>
	 * This one only works if every pair of states is distinguishable because it starts by assigning empty seq to all
	 * pairs and then extending them - there is no provision to mark pairs as indistinguishable since empty sequence
	 * separates an accept from a reject-state.
	 * 
	 * @param fsm the machine to process
	 * @return characterising set
	 */
	public static Collection<List<Label>> computeWSet_reducedw(LearnerGraph fsm) throws EquivalentStatesException
	{
		Map<CmpVertex,Integer> equivalenceClasses = new HashMap<CmpVertex,Integer>(fsm.getStateNumber()), newEquivClasses = new HashMap<CmpVertex,Integer>(fsm.getStateNumber());
		Map<Map<Label,Integer>,Integer> sortedRows = new HashMap<Map<Label,Integer>,Integer>();
		Map<CmpVertex,Map<CmpVertex,List<Label>>> Wdata = new HashMap<CmpVertex,Map<CmpVertex,List<Label>>>(fsm.getStateNumber());
		Map<CmpVertex,Map<CmpVertex,Set<Label>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<Label>>>(fsm.getStateNumber());
		CmpVertex sink = generateSinkState(fsm);
		boolean mealy = fsm.config.getLabelKind() == Configuration.LABELKIND.LABEL_INPUT_OUTPUT;
		if (mealy) {
			constructMealyEquivalenceClasses(fsm, sink, equivalenceClasses);
			constructMealy_NextChar(fsm,equivalenceClasses,Wdata);
		}
		else {
			constructDFAEquivalenceClasses(fsm, sink, equivalenceClasses);
			constructDFA_NextChar(fsm,equivalenceClasses,Wdata);
		}

		int equivalenceClassNumber = 0,oldEquivalenceClassNumber;
		int statesEquivalentToSink;
		do
		{
			oldEquivalenceClassNumber = equivalenceClassNumber;statesEquivalentToSink = 0;
			Map<CmpVertex,TransitionRowEqClass> newMap = new HashMap<CmpVertex,TransitionRowEqClass>(fsm.getStateNumber());
			equivalenceClassNumber = 0;sortedRows.clear();
			//noinspection RedundantOperationOnEmptyContainer
			newEquivClasses.clear();
			int sinkEqClass = equivalenceClasses.get(sink);
			for(Map.Entry<CmpVertex,Integer> stateA_entry:equivalenceClasses.entrySet())
			{
				CmpVertex stateA = stateA_entry.getKey();
				TransitionRowEqClass map = new TransitionRowEqClass(stateA_entry.getValue());
				Map<Label,CmpVertex> labelNSmap = fsm.transitionMatrix.get(stateA);
				if (labelNSmap != null)
				{
					for(Entry<Label,CmpVertex> labelstate:labelNSmap.entrySet())
					{
						int targetEqClass = equivalenceClasses.get(labelstate.getValue());
						if (mealy || targetEqClass != sinkEqClass) // filter out all transitions to sink
							map.put(labelstate.getKey(), targetEqClass);
					}
				}
				
				newMap.put(stateA, map);
				if ((map.isEmpty() && mealy) || map.looksLikeSink(sinkEqClass)) statesEquivalentToSink++;
				if (!sortedRows.containsKey(map))
				{
					equivalenceClassNumber++;
					sortedRows.put(map,equivalenceClassNumber);newEquivClasses.put(stateA, equivalenceClassNumber);
				}
				else
					newEquivClasses.put(stateA, sortedRows.get(map));
			}

			distinguishingLabels.clear();// clear a map from pairs of states to sets of labels which distinguish between them
			
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				for (Entry<CmpVertex, Integer> stateB : equivalenceClasses.entrySet()) {
					if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.

					if (stateA.getValue().equals(stateB.getValue()) &&
							!newEquivClasses.get(stateA.getKey()).equals(newEquivClasses.get(stateB.getKey()))) {// the two states used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.

						// the two states used to be equivalent but not any more, find inputs which 
						// distinguish between them and update the histogram to count the number
						// of inputs which can be used distinguish between states at this stage.
						distinguishingLabels.computeIfAbsent(stateA.getKey(), k->new HashMap<CmpVertex, Set<Label>>(fsm.getStateNumber()));
						Map<CmpVertex, Set<Label>> stateToDist = distinguishingLabels.get(stateA.getKey());
						stateToDist.put(stateB.getKey(), newMap.get(stateA.getKey()).computeDistinguishingLabel(newMap.get(stateB.getKey())));
					}
				}
			}

			// First, we compute the histogram of label usage
			Label topLabel = computeTopLabel(distinguishingLabels);
			while(topLabel != null)
			{// distinguishingLabels contains all labels we may use; the choice of an optimal subset is NP, hence we simply pick
			 // those which look best until we collect enough to distinguish all states (the loop is guaranteed to terminate
			 // because we have enough data in distinguishingLabels for this, by the virtue of us getting as far as this
		     // in the current procedure).
				
				// Now topLabel is the most often used one, we use it to separate all relevant states
				for(Entry<CmpVertex,Map<CmpVertex,Set<Label>>> stateA:distinguishingLabels.entrySet())
				{
					for(Entry<CmpVertex,Set<Label>> stateB:stateA.getValue().entrySet())
					{// the two states used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.
	
						if (distinguishingLabels.get(stateA.getKey()).get(stateB.getKey()).contains(topLabel))
						{
							List<Label> Wsequence;
							if (Wdata.get(stateA.getKey()).containsKey(stateB.getKey()))
								Wsequence = Wdata.get(stateA.getKey()).get(stateB.getKey());// depending on the ordering in the matrix, either (A,B) or (B,A) should be defined.
							else
								Wsequence = Wdata.get(stateB.getKey()).get(stateA.getKey());
							assert Wsequence != null : "In states ("+stateA.getKey()+","+stateB.getKey()+") Wsequence is null";
							assert Wsequence.isEmpty() : "In states ("+stateA.getKey()+","+stateB.getKey()+") Wsequence is non-empty and contains "+Wsequence;

							//assert topLabel != null;
														
							CmpVertex toA = stateA.getKey() == sink? sink:fsm.transitionMatrix.get(stateA.getKey()).get(topLabel);if (toA == null) toA=sink;
							CmpVertex toB = stateB.getKey() == sink? sink:fsm.transitionMatrix.get(stateB.getKey()).get(topLabel);if (toB == null) toB=sink;
							
							Wsequence.add(topLabel);
							List<Label> Wprevious;
							if (Wdata.get(toA).containsKey(toB))
								Wprevious = Wdata.get(toA).get(toB);// depending on the ordering in the matrix, either (A,B) or (B,A) should be defined.
							else
								Wprevious = Wdata.get(toB).get(toA);
							assert Wprevious != null : "In states ("+stateA.getKey()+","+stateB.getKey()+") previous pair ("+toA+","+toB+") has a null sequence";
							assert !Wprevious.isEmpty() || toA.isAccept() != toB.isAccept(): "In states ("+stateA.getKey()+","+stateB.getKey()+") previous pair ("+toA+","+toB+") was not separated";
							
							Wsequence.addAll(Wprevious);
						
							// now that we added the top label, we need  to remove it from the appropriate sets of labels.
							distinguishingLabels.get(stateA.getKey()).get(stateB.getKey()).clear();
						}
					}			
				}
				
				topLabel = computeTopLabel(distinguishingLabels);
			} // while(topLabel != null)
			
			equivalenceClasses = newEquivClasses;newEquivClasses = new HashMap<CmpVertex,Integer>(fsm.getStateNumber());
		}
		while(equivalenceClassNumber > oldEquivalenceClassNumber);

		Collection<List<Label>> result = new HashSet<List<Label>>();
		assert statesEquivalentToSink > 0: "missing equivalence class associated with sink state";
		if ((statesEquivalentToSink <= 2 && oldEquivalenceClassNumber == fsm.transitionMatrix.size()+2-statesEquivalentToSink )
			|| fsm.config.getEquivalentStatesAllowedForW())
		{
			boolean sinkAsRealState =  !fsm.config.isPrefixClosed() && statesEquivalentToSink == 1;
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
				if (sinkAsRealState || stateA.getKey() != sink)
				{
					for (Entry<CmpVertex, Integer> stateB : equivalenceClasses.entrySet()) {
						if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
						if (sinkAsRealState || stateB.getKey() != sink) {
							List<Label> seq = Wdata.get(stateA.getKey()).get(stateB.getKey());
							result.add(seq);
						}
					}
				}			
		}
		else
			// report equivalent states
			throw EquivalentStatesException.construct(equivalenceClasses,fsm,sink);

		return result;
	}

	/** This one is used to indicate that a two machines are not accepting the same language - 
	 * I need to check that it is the incompatibility exception thrown by the <i>checkM</i> 
	 * method and not any other <i>IllegalArgumentException</i>.
	 */
	public static class DifferentFSMException extends IllegalArgumentException 
	{
		/**
		 *  Serialisation ID.
		 */
		private static final long serialVersionUID = 6126662147586264877L;

		public DifferentFSMException(String arg)
		{
			super(arg);
		}
	}
	
	public enum VERTEX_COMPARISON_KIND { NONE, NAMES, DEEP }
	
	private static class CollectionOfPairs
	{
		public final LearnerGraph first,second;
		
		public CollectionOfPairs(LearnerGraph f,LearnerGraph s)
		{
			first=f;second=s;
		}
		
		public final Map<CmpVertex,Set<CmpVertex>> pairs = new TreeMap<CmpVertex,Set<CmpVertex>>();
		
		/** Adds the supplied pairs to the collection and returns true if the pair was not yet in the collection
		 * and false otherwise.
		 * @param pairAB pair to add
		 * @return false if the pair is already in the collection.
		 */
		public boolean addAndCheck(StatePair pairAB)
		{
			Set<CmpVertex> row = pairs.computeIfAbsent(pairAB.firstElem, k -> new TreeSet<CmpVertex>());
			boolean result = row.contains(pairAB.secondElem);// if we've just created a row, the outcome will be false, same if the value does not exist.
			if (!result)
				row.add(pairAB.secondElem);

			return !result;
		}
		
		/** Given a state in the first graph (expected), this method takes set of states related to it and for each of them
		 * takes states associated with them on the right-hand side of <em>StatePair</em> 
		 * @param state to handle
		 * @return states related to the provided state
		 */
		public Map<PAIRCOMPATIBILITY,Set<CmpVertex>> statesAssociatedToThoseRelatedTo(CmpVertex state)
		{
			Map<PAIRCOMPATIBILITY,Set<CmpVertex>> result = new TreeMap<PAIRCOMPATIBILITY,Set<CmpVertex>>();
			Map<CmpVertex,PAIRCOMPATIBILITY> map = first.pairCompatibility.compatibility.get(state);
			if (map != null)
				for(Entry<CmpVertex,PAIRCOMPATIBILITY> entry:map.entrySet())
				{
					Set<CmpVertex> row = result.computeIfAbsent(entry.getValue(), k -> new TreeSet<CmpVertex>());
					Set<CmpVertex> rightHand = pairs.get(entry.getKey());
					String errorDescrPart1 = "state "+ state+" is mapped with "+entry.getValue().name()+" to "+entry.getKey()+" of the graph ";
					if (rightHand == null)
						throw new DifferentFSMException(errorDescrPart1 + "which does not have a corresponding state in the expected graph, only "+ pairs +" are known");
					row.addAll(rightHand);
				}
			
			return result;
		}
		
		/** Given a state in the second graph (actual), returns a map relating PAIRCOMPATIBILITY to
		 * its associated states. 
		 */
		public Map<PAIRCOMPATIBILITY,Set<CmpVertex>> statesAssociatedTo(CmpVertex state)
		{
			Map<PAIRCOMPATIBILITY,Set<CmpVertex>> result = new TreeMap<PAIRCOMPATIBILITY,Set<CmpVertex>>();
			Map<CmpVertex,PAIRCOMPATIBILITY> map = second.pairCompatibility.compatibility.get(state);
			if (map != null)
				for(Entry<CmpVertex,PAIRCOMPATIBILITY> entry:map.entrySet())
				{
					Set<CmpVertex> row = result.computeIfAbsent(entry.getValue(), k -> new TreeSet<CmpVertex>());
					row.add(entry.getKey());
				}
			return result;
		}
		
		/** Iterates through states in the collection and checks the associations of the vertices
		 * are preserved by the <em>pairs</em> relation.
		 */
		public void checkPairsAssociatedCorrectly(String expectedGraphName, String actualGraphName)
		{
			for(Entry<CmpVertex,Set<CmpVertex>> entry:pairs.entrySet())
			{
				Map<PAIRCOMPATIBILITY,Set<CmpVertex>> expectedMap = statesAssociatedToThoseRelatedTo(entry.getKey());
				for(CmpVertex secondVertex:entry.getValue())
				{
					Map<PAIRCOMPATIBILITY,Set<CmpVertex>> actualMap = statesAssociatedTo(secondVertex);
					if (!expectedMap.equals(actualMap))
						throw new DifferentFSMException("state pair "+entry.getKey()+" and "+secondVertex+" have incompatible associations : "+expectedMap+
								" for graph \"" + expectedGraphName+ "\" v.s. "+actualMap+ " for \""+actualGraphName+"\"");
				}
			}
		}
	}
	
	/** Checks the equivalence between the two states, stateG of graphA and stateB of graphB.
	 * Unreachable states  are ignored.
	 * Compatibility labelling other than INCOMPATIBLE is only checked for deterministic graphs.
	 * <p>
	 * Note that it's not possible to convert pairs to bi-directional transitions and match
	 * the resulting transition diagrams (see testPair7). All "A" transitions of the diagram have
	 * the same language and same for the "B"s, which is why
	 * if there are bi-directional transitions from/to all of "A" and all of "B", then the languages
	 * are the same regardless of the number of those transitions or the states actually connected. 
	 * When these bi-directional transitions are interpreted as associations between states,
	 * the outcome of merging will depend on the specific associations which are present, hence
	 * conversion of associations to transitions for the purpose of checking for language equivalence
	 * cannot be done.
	 * 
	 * @param compareVertices if DEEP, compares attributes of every pair of states reached; NAMES means only names are compared.
	 * @param checkAssociations if True, checks that pairwise associations between vertices (such as INCOMPATIBLE or THEN) match. 
	 * @return DifferentFSMException if machines are different and null otherwise.
	 */
	public static <TARGET_A_TYPE,TARGET_B_TYPE,
	CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
	CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>> 
	DifferentFSMException checkM(
			AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> expectedArg,CmpVertex stateExpectedArg,
			AbstractLearnerGraph<TARGET_B_TYPE, CACHE_B_TYPE> actualArg,CmpVertex stateActualArg,
			VERTEX_COMPARISON_KIND compareVertices, boolean checkAssociations) {
		assert stateExpectedArg != null && stateActualArg != null;
		LearnerGraph expected = null, actual = null;
		CmpVertex stateActual, stateExpected;

		if (expectedArg.config.getLabelKind() == Configuration.LABELKIND.LABEL_INPUT_OUTPUT && expectedArg.config.getLabelKind() != actualArg.config.getLabelKind())
			throw new IllegalArgumentException("Cannot compare a Mealy automaton with an LTS");

		boolean mealy = expectedArg.config.getLabelKind() == Configuration.LABELKIND.LABEL_INPUT_OUTPUT;

		if (expectedArg instanceof LearnerGraph && actualArg instanceof LearnerGraph) {// deterministic case
			expected = (LearnerGraph) expectedArg;
			actual = (LearnerGraph) actualArg;
			stateActual = stateActualArg;
			stateExpected = stateExpectedArg;
		} else {// non-deterministic case
			try {// This one potentially makes copies of states with different names.
				expected = expectedArg.pathroutines.buildDeterministicGraph(stateExpectedArg);
				actual = actualArg.pathroutines.buildDeterministicGraph(stateActualArg);
			} catch (IncompatibleStatesException e) {
				Helper.throwUnchecked("failed to build a deterministic version of a supplied graph", e);
			}
			assert actual != null;assert expected != null;
			stateActual = actual.getInit();
			stateExpected = expected.getInit();
		}

		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
		CollectionOfPairs statesAddedToBoundary = new CollectionOfPairs(expected, actual);
		currentExplorationBoundary.add(new StatePair(stateExpected, stateActual));
		statesAddedToBoundary.addAndCheck(new StatePair(stateExpected, stateActual));
		switch (compareVertices) {
			case DEEP:
				if (!DeterministicDirectedSparseGraph.deepEquals(stateExpected, stateActual))
					return new DifferentFSMException("vertices " + stateExpected + " and " + stateActual + " have different values of attributes");
				if (stateActual.getOrigState() == null) {
					if (stateExpected.getOrigState() != null)
						return new DifferentFSMException("vertices " + stateExpected + " and " + stateActual + " have different names");
				} else if (!stateActual.getOrigState().equals(stateExpected.getOrigState()))
					return new DifferentFSMException("vertices " + stateExpected + " and " + stateActual + " have different names");
				break;
			case NAMES:
				if (!stateActual.getOrigState().equals(stateExpected.getOrigState()))
					return new DifferentFSMException("vertices " + stateExpected + " and " + stateActual + " have different names");
				break;
			case NONE:// nothing needs doing
				break;
		}

		CmpVertex sink = generateSinkState(expected);
		boolean prefixClosed = expected.config.isPrefixClosed() && actual.config.isPrefixClosed();
		MapWithSearch<Label,Label, CmpVertex> sinkRow = expected.createNewRow();
		while (!currentExplorationBoundary.isEmpty()) {
			StatePair statePair = currentExplorationBoundary.remove();
			assert statePair.firstElem == sink || expected.transitionMatrix.containsKey(statePair.firstElem) : "state " + statePair.firstElem + " is not known to the expected graph";
			assert statePair.secondElem == sink || actual.transitionMatrix.containsKey(statePair.secondElem) : "state " + statePair.secondElem + " is not known to the actual graph";
			if (statePair.firstElem.isAccept() != statePair.secondElem.isAccept())
				return new DifferentFSMException("states " + statePair.firstElem + " and " + statePair.secondElem + " have a different acceptance labelling between the machines");

			MapWithSearch<Label, Label, CmpVertex> expectedTargets = statePair.firstElem == sink ? sinkRow : expected.transitionMatrix.get(statePair.firstElem);
			MapWithSearch<Label, Label, CmpVertex> actualTargets = statePair.secondElem == sink ? sinkRow : actual.transitionMatrix.get(statePair.secondElem);
			//if (prefixClosed && expectedTargets.size() != actualTargets.size())// each of them is equal to the keyset size from determinism
			//	return new DifferentFSMException("different number of transitions from states "+statePair);
			Set<Label> outgoing = new TreeSet<>();
			outgoing.addAll(expectedTargets.keySet());
			outgoing.addAll(actualTargets.keySet());
			for (Label label : outgoing) {
				CmpVertex nextExpectedState;
				Label expectedLabel = expectedTargets.findKey(label);
				Label actualLabel = actualTargets.findKey(label);

				if (mealy) {
					if ( expectedLabel == null && actualLabel != null )
						return new DifferentFSMException("For input \""+((LabelInputOutput) actualLabel).input+"\" from a state \"" + statePair+" actual produces "+((LabelInputOutput) actualLabel).output+" and expected one cannot take a transition");
					if ( expectedLabel != null && actualLabel == null )
						return new DifferentFSMException("For input \""+((LabelInputOutput) expectedLabel).input+"\" from a state \"" + statePair+" expected produces "+((LabelInputOutput) expectedLabel).output+" and actual one cannot take a transition");
					assert (expectedLabel != null && actualLabel != null);
					String expectedOutput = ((LabelInputOutput) expectedLabel).output, actualOutput = ((LabelInputOutput) actualLabel).output;
					if (!expectedOutput.equals(actualOutput))
						return new DifferentFSMException("different output on input \"" + ((LabelInputOutput) expectedLabel).input + "\" from a state \"" + statePair);
				}

				if (!expectedTargets.containsKey(label)) {
					if (prefixClosed)
						return new DifferentFSMException("no transition with expected label \"" + label + "\" from a state \"" + statePair.secondElem + "\" corresponding to \"" + statePair.firstElem + "\"");

					nextExpectedState = sink;
				} else
					nextExpectedState = expectedTargets.get(label);

				assert nextExpectedState != null : "null target for " + label + " from state " + statePair.firstElem + " in row " + expectedTargets;

				CmpVertex nextActualState;
				if (!actualTargets.containsKey(label)) {
					if (prefixClosed)
						return new DifferentFSMException("no transition with actual label \"" + label + "\" from a state \"" + statePair.firstElem + "\" corresponding to \"" + statePair.secondElem + "\"");

					nextActualState = sink;
				} else
					nextActualState = actualTargets.get(label);
				assert nextActualState != null : "null target for " + label + " from state " + statePair.secondElem + " in row " + actualTargets;

				StatePair nextPair = new StatePair(nextExpectedState, nextActualState);
				//System.out.println("outgoing: "+statePair.getR()+","+statePair.getQ()+"-"+label+"->"+nextPair.getR()+","+nextPair.getQ());// elements of the pairs are in reverse order
				if (statesAddedToBoundary.addAndCheck(nextPair)) {
					switch (compareVertices) {
						case DEEP:
							//System.out.println("looking at "+expectedState+" ("+expectedState.getColour()+") and "+tState+" ("+tState.getColour()+") ");
							if (!DeterministicDirectedSparseGraph.deepEquals(nextExpectedState, nextActualState))
								return new DifferentFSMException("vertices " + nextExpectedState + " and " + nextActualState + " have different values of attributes");
							if (nextActualState.getOrigState() == null) {
								if (nextExpectedState.getOrigState() != null)
									return new DifferentFSMException("vertices " + nextExpectedState + " and " + nextActualState + " have different names");
							} else if (!nextActualState.getOrigState().equals(nextExpectedState.getOrigState()))
								return new DifferentFSMException("vertices " + nextExpectedState + " and " + nextActualState + " have different names");
							break;
						case NAMES:
							if (!nextActualState.getOrigState().equals(nextExpectedState.getOrigState()))
								return new DifferentFSMException("vertices " + nextExpectedState + " and " + nextActualState + " have different names");
							break;
						case NONE:// nothing needs doing
							break;
					}

					currentExplorationBoundary.offer(nextPair);
				}
			}
		}

		// now iterate through the maps of incompatible states and check them.
		if (checkAssociations)
			statesAddedToBoundary.checkPairsAssociatedCorrectly(expected.getName(),actual.getName());
		return null;
	}

	public static
	DifferentFSMException checkReduction(LearnerGraph graphMoreGeneral,CmpVertex stateExpectedArg,
			LearnerGraph graphLessGeneral,CmpVertex stateActualArg)
	{
		assert stateExpectedArg != null && stateActualArg != null;

		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
		CollectionOfPairs statesAddedToBoundary = new CollectionOfPairs(graphMoreGeneral,graphLessGeneral);
		currentExplorationBoundary.add(new StatePair(stateExpectedArg, stateActualArg));statesAddedToBoundary.addAndCheck(new StatePair(stateExpectedArg, stateActualArg));
	
		CmpVertex sink = generateSinkState(graphMoreGeneral);
		Map<Label, CmpVertex> sinkRow = graphMoreGeneral.createNewRow();
		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair statePair = currentExplorationBoundary.remove();
			assert statePair.firstElem == sink || graphMoreGeneral.transitionMatrix.containsKey(statePair.firstElem) : "state "+statePair.firstElem+" is not known to the expected graph";
			assert statePair.secondElem == sink || graphLessGeneral.transitionMatrix.containsKey(statePair.secondElem) : "state "+statePair.secondElem+" is not known to the actual graph";
			if (statePair.firstElem.isAccept() != statePair.secondElem.isAccept())
				return new DifferentFSMException("states "+statePair.firstElem+" and " + statePair.secondElem+" have a different acceptance labelling between the machines");
	
			Map<Label,CmpVertex> moreGeneralTargets = statePair.firstElem == sink?sinkRow:graphMoreGeneral.transitionMatrix.get(statePair.firstElem);
			Map<Label,CmpVertex> lessGeneralTargets = statePair.secondElem == sink?sinkRow:graphLessGeneral.transitionMatrix.get(statePair.secondElem);
			//if (prefixClosed && expectedTargets.size() != actualTargets.size())// each of them is equal to the keyset size from determinism
			//	return new DifferentFSMException("different number of transitions from states "+statePair);
			Set<Label> outgoingAll = new TreeSet<Label>();outgoingAll.addAll(moreGeneralTargets.keySet());outgoingAll.addAll(lessGeneralTargets.keySet());
			for(Label label:outgoingAll)
			{
				CmpVertex nextExpectedState;
				if (!moreGeneralTargets.containsKey(label))
					return new DifferentFSMException("no transition with expected label \""+label+"\" from a state \""+statePair.secondElem+"\" corresponding to \""+statePair.firstElem+"\"");
				nextExpectedState = moreGeneralTargets.get(label);
				
				if (lessGeneralTargets.containsKey(label))
				{// only explore where both can take a transition
					CmpVertex nextActualState = lessGeneralTargets.get(label);
	
					StatePair nextPair = new StatePair(nextExpectedState,nextActualState);
					//System.out.println("outgoing: "+statePair.getR()+","+statePair.getQ()+"-"+label+"->"+nextPair.getR()+","+nextPair.getQ());// elements of the pairs are in reverse order
					if (statesAddedToBoundary.addAndCheck(nextPair))
						currentExplorationBoundary.offer(nextPair);
				}
			}
		}
	
		return null;
	}

	/** Checks the equivalence between the two states, stateG of graphA and stateB of graphB.
	 * Unreachable states are ignored. 
	 */
	public static <TARGET_A_TYPE,TARGET_B_TYPE,
		CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
		CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>> 
		DifferentFSMException checkM(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> expected, AbstractLearnerGraph<TARGET_B_TYPE, CACHE_B_TYPE> graph)
	{
		return checkM(expected,expected.getInit(), graph, graph.getInit(),VERTEX_COMPARISON_KIND.NONE, true);
	}
	
	/** Verifies that vertices contain the same attributes in the two graphs
	 * in addition to checking for isomorphism of the graphs.
	 * Used for consistency checking.
	 * <p>
	 * Important: Unreachable states  are ignored.
	 * Compatibility labelling other than INCOMPATIBLE is only checked for deterministic graphs.
	 */
		public static <TARGET_A_TYPE,TARGET_B_TYPE,
		CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
		CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>> 
		DifferentFSMException checkM_and_colours(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> A, AbstractLearnerGraph<TARGET_B_TYPE, CACHE_B_TYPE> B, VERTEX_COMPARISON_KIND howToCompare)
				
	{
		return WMethod.checkM(A, A.getInit(),B,B.getInit(),howToCompare, true);
	}

	/** Given a W set, checks if it is a valid W set for the current state machine and throws if not.
	 *
	 * @param wset the set to check validity of.
	 * @param prefixClosed whether we are talking of prefix-closed languages
	 * @param equivalentVertices the set of equivalent vertices which should be ignored. Can be null if not used.
	 */
	public String checkW_is_corrent_boolean(Collection<List<Label>> wset, boolean prefixClosed, Set<StatePair> equivalentVertices)
	{
		for(DeterministicDirectedSparseGraph.CmpVertex stateA:coregraph.transitionMatrix.keySet())
		{
			for(DeterministicDirectedSparseGraph.CmpVertex stateB:coregraph.transitionMatrix.keySet())
				if (stateA != stateB && (equivalentVertices == null ||
						(!equivalentVertices.contains(new StatePair(stateA, stateB)) &&
								!equivalentVertices.contains(new StatePair(stateB, stateA)))))
				{
					boolean foundString = false;
					Iterator<List<Label>> pathIt = wset.iterator();
					while(pathIt.hasNext() && !foundString)
					{
						List<Label> path = pathIt.next();
						int aResult = coregraph.paths.tracePath(path, stateA,prefixClosed),
								bResult = coregraph.paths.tracePath(path, stateB,prefixClosed);

						if ( (aResult == AbstractOracle.USER_ACCEPTED && bResult >= 0) ||
								(bResult == AbstractOracle.USER_ACCEPTED && aResult >= 0))
							foundString = true;
					}

					if (!foundString)
						return "W set "+wset+" does not distinguish between "+stateA+" and "+stateB;
				}
		}

		return null;
	}

	public interface FsmPermutator {
		/** Returns a collection representing an order in which elements of an FSM should be placed in a string. */
		ArrayList<Pair<CmpVertex,Label>> getPermutation(Collection<Pair<CmpVertex,Label>> from);
	}

	/** This method permutes states of a supplied machine using the permutation function provided.
	 * 
	 * @param perm the permutator to use.
	 * @param converter used to intern labels of the graph, sadly has to be a parameter 
	 * because such converters are associated with higher-level entities such as 
	 * learners or learnerevaluationconvigurations rather than graphs.
	 * @return the state machine permuted from the current one using the supplied permutator.
	 */
	public LearnerGraph Permute(FsmPermutator perm, ConvertALabel converter)
	{
		ArrayList<Pair<CmpVertex,Label>> transitionList = new ArrayList<Pair<CmpVertex,Label>>();
		for(Entry<CmpVertex,MapWithSearch<Label,Label,CmpVertex>> row:coregraph.transitionMatrix.entrySet())
			for(Entry<Label,CmpVertex> nextState:row.getValue().entrySet())
				transitionList.add(new Pair<CmpVertex,Label>(row.getKey(),nextState.getKey()));
		
		ArrayList<Pair<CmpVertex,Label>> permutation = perm.getPermutation(transitionList);
		assert transitionList.size() == permutation.size();
		StringBuilder newFsm = new StringBuilder();
		for(Pair<CmpVertex,Label> p:permutation)
		{
			CmpVertex from = p.firstElem;Label label = p.secondElem;
			newFsm.append("\n").append(from).append("-").append(label).append("->").append(coregraph.transitionMatrix.get(from).get(label));
		}

		LearnerGraph permFsm = buildLearnerGraph(newFsm.toString(), "testDeterminism_perm",coregraph.config, converter);
		permFsm.setInit(permFsm.findVertex(coregraph.getInit()));
		return permFsm;
	}

	/** Returns true if IDs of all states in a graph are numeric rather than text. */
	public boolean checkGraphNumeric()
	{
		for(CmpVertex vert:coregraph.transitionMatrix.keySet())
			if (vert.getKind() == DeterministicDirectedSparseGraph.VertexID.VertKind.NONE)
				return false;
		return true;
	}
	
	public int vertexToInt(CmpVertex vertexA, CmpVertex vertexB)
	{
		int x=coregraph.learnerCache.getStateToNumber().get(vertexA), y = coregraph.learnerCache.getStateToNumber().get(vertexB);
		if (x <= y)
			return x+y*(y+1)/2;
		
		return y+x*(x+1)/2;
	}
}

