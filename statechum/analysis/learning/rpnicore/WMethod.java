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

import static org.junit.Assert.fail;
import static statechum.analysis.learning.rpnicore.FsmParser.buildGraph;

import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import junit.framework.Assert;

import statechum.DeterministicDirectedSparseGraph;
import statechum.Helper;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTA_FSMStructure;
import statechum.model.testset.PrefixFreeCollection;
import statechum.model.testset.SlowPrefixFreeCollection;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

public class WMethod {
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

	private Collection<List<String>> fullTestSet;
	private Collection<List<String>> transitionCover, characterisationSet;
	
	public Collection<List<String>> getFullTestSet(int extraStates){
		if (fullTestSet == null)
			fullTestSet = computeNewTestSet(extraStates);
		return fullTestSet;
	}
	
	public Collection<List<String>> getCharacterisationSet() {
		return characterisationSet;
	}

	public Collection<List<String>> getTransitionCover() {
		return transitionCover;
	}

	/** Given a database of sequences and a test sequence, converts a given sequence into
	 * a fundamental test sequence and appends a the result to the <em>sequences</em> set 
	 * if the fundamental test sequence is not a prefix of an existing sequence in that set.  
	 */
	public void appendSequence(PrefixFreeCollection sequences, List<String> path)
	{
		List<String> seq = coregraph.paths.truncateSequence(path);
		sequences.addSequence(seq);
	}
	
	public void appendAllSequences(PrefixFreeCollection sequences, List<List<String>> paths)
	{
		for(List<String> path:paths)
			appendSequence(sequences, path);
	}
	
	public Collection<List<String>> computeOldTestSet(int numberOfExtraStates)
	{
		Set<String> alphabet =  coregraph.learnerCache.getAlphabet();
		List<List<String>> partialSet = coregraph.pathroutines.computeStateCover();
		characterisationSet = computeWSet_reducedmemory(coregraph);if (characterisationSet.isEmpty()) characterisationSet.add(Arrays.asList(new String[]{}));
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

	public Collection<List<String>> computeNewTestSet(int numberOfExtraStates)
	{
		Set<String> alphabet =  coregraph.learnerCache.getAlphabet();
		List<List<String>> stateCover = coregraph.pathroutines.computeStateCover();
		characterisationSet = computeWSet_reducedmemory(coregraph);if (characterisationSet.isEmpty()) characterisationSet.add(Arrays.asList(new String[]{}));
		transitionCover = crossWithSet(stateCover,alphabet);transitionCover.addAll(stateCover);

		PTASequenceEngine engine = new PTA_FSMStructure(coregraph);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(stateCover);
		
		partialPTA.cross(characterisationSet);
		for(int i=0;i<=numberOfExtraStates;i++)
		{
			partialPTA = partialPTA.crossWithSet(alphabet);
			partialPTA.cross(characterisationSet);
		}
		
		return engine.getData();
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
	
	public static List<List<String>> cross(Collection<List<String>> a, Collection<List<String>> b){
		List<List<String>> returnVect = new LinkedList<List<String>>();
		for(List<String> elemA:a){
			for(List<String> elemB:b) {
				List<String> cross = new LinkedList<String>();
				cross.addAll(elemA);
				cross.addAll(elemB);
				returnVect.add( cross );
			}
		}
		return returnVect;
	}

	/** Important: this one destroys the first operand. */
	public static List<List<String>> crossWithSet_One(List<List<String>> a, Collection<String> b)
	{
		if (b.size() == 1)
		{
			String element = b.iterator().next();
			for(List<String> elemA:a)
				elemA.add(element);
			return a;
		}

		return crossWithSet(a, b);
	}
	
	public static List<List<String>> crossWithSet(Collection<List<String>> a, Collection<String> b){
		List<List<String>> returnVect = new LinkedList<List<String>>();
		for(List<String> elemA:a){
			for(String elemB:b) {
				List<String> cross = new LinkedList<String>();
				cross.addAll(elemA);
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
	public static List<List<String>> makeSingleton(Collection<String> alphabet){
		List<List<String>> functionList = new LinkedList<List<String>>();
		for(String stim:alphabet){
			List<String> path = new LinkedList<String>();
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
		
		private final Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> statesToReduce;
		
		public Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> getEquivalentStates()
		{
			Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> equivalentStates = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			for(AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> eqClass:getStatesToComputeReduction())
				if (eqClass.getStates().size() > 1)
					equivalentStates.add(eqClass);
			
			return equivalentStates;
		}
		
		/** Returns a collection which can be passed to <em>mergeCollectionOfVertices</em> in order to compute a reduced FSM. */
		public Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> getStatesToComputeReduction()
		{
			return statesToReduce;
		}
		
		public static Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> IdentifyEquivalentStates(final Map<CmpVertex,Integer> equivalenceClasses,LearnerGraph graph)
		{
			Map<Integer,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mapOfEquivalentStates = new TreeMap<Integer,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> equivalentStates = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
			int number =0;
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				AMEquivalenceClass<CmpVertex,LearnerGraphCachedData> equiv = mapOfEquivalentStates.get(stateA.getValue());
				if (equiv == null)
				{
					equiv = new AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>(number++,graph);mapOfEquivalentStates.put(stateA.getValue(), equiv);
				}
				try {
					equiv.addFrom(stateA.getKey(),graph.transitionMatrix.get(stateA.getKey()).entrySet());
				} catch (IncompatibleStatesException e) {
					Helper.throwUnchecked("equivalent states cannot be incompatible", e);
				}
			}
			
			boolean equivFound = false;
			for(Entry<Integer,AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> entry:mapOfEquivalentStates.entrySet())
			{
				equivalentStates.add(entry.getValue());
				if (entry.getValue().getStates().size() > 1) equivFound=true;
			}
			assert equivFound: "equivalent states were not found";
			return equivalentStates; 
		}
		
		public static EquivalentStatesException construct(final Map<CmpVertex,Integer> equivalenceClasses,LearnerGraph graph)
		{
			return new EquivalentStatesException(IdentifyEquivalentStates(equivalenceClasses,graph));
		}
		
		private EquivalentStatesException(Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> argStatesToReduce)
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
	
	/** Computes a set of inputs which distinguish a given pair of states, 
	 * when supplied with a map between states and their equivalence classes.
	 * 
	 */
	private static Set<String> computeDistinguishingLabel(CmpVertex stateA, CmpVertex stateB, Map<CmpVertex,Map<String,Integer>> newMap)
	{
		Map<String,Integer> mapA = newMap.get(stateA), mapB = newMap.get(stateB);
		Set<String> distInputs = new HashSet<String>();
		
		Iterator<Entry<String,Integer>> mapAiter = mapA.entrySet().iterator();
		while(mapAiter.hasNext())
		{
			Entry<String,Integer> enA = mapAiter.next();
			if (!mapB.entrySet().contains(enA))
				distInputs.add(enA.getKey());
		}
		
		Iterator<Entry<String,Integer>> mapBiter = mapB.entrySet().iterator();
		while(mapBiter.hasNext())
		{
			Entry<String,Integer> enB = mapBiter.next();
			if (!mapA.entrySet().contains(enB))
				distInputs.add(enB.getKey());
		}
		
		assert !distInputs.isEmpty();
		return distInputs;
	}
	
	/** Computes a characterising set, assuming that there are no unreachable states (with unreachable states, 
	 * it will take a bit longer to perform the computation. 
	 * 
	 * @param fsm the machine to process
	 * @param alphabet
	 * @return characterising set
	 */
	public static List<List<String>> computeWSetOrig(LearnerGraph fsm) throws EquivalentStatesException
	{
		
		Map<CmpVertex,Integer> equivalenceClasses = new LinkedHashMap<CmpVertex,Integer>(), newEquivClasses = new LinkedHashMap<CmpVertex,Integer>();
		Map<Map<String,Integer>,Integer> sortedRows = new HashMap<Map<String,Integer>,Integer>();
		Map<CmpVertex,Map<CmpVertex,List<String>>> Wdata = new HashMap<CmpVertex,Map<CmpVertex,List<String>>>();
		
		for(CmpVertex state:fsm.transitionMatrix.keySet()) 
			equivalenceClasses.put(state, 0);
		for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
		{
			Map<CmpVertex,List<String>> row = new HashMap<CmpVertex,List<String>>();
			Wdata.put(stateA.getKey(), row);

			Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
			while(stateB_It.hasNext())
			{
				Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
				row.put(stateB.getKey(), new LinkedList<String>());
			}
		}
		
		int equivalenceClassNumber = 0,oldEquivalenceClassNumber=0;
		do
		{
			oldEquivalenceClassNumber = equivalenceClassNumber;
			Map<CmpVertex,Map<String,Integer>> newMap = new HashMap<CmpVertex,Map<String,Integer>>();
			equivalenceClassNumber = 0;sortedRows.clear();newEquivClasses.clear();
			for(Entry<CmpVertex,Map<String,CmpVertex>> stateEntry:fsm.transitionMatrix.entrySet())
			{
				Map<String,Integer> map = new HashMap<String,Integer>();
				Map<String,CmpVertex> labelNSmap = stateEntry.getValue();
				if (labelNSmap != null)
					for(Entry<String,CmpVertex> labelstate:labelNSmap.entrySet())
						map.put(labelstate.getKey(), equivalenceClasses.get(labelstate.getValue()));
				
				newMap.put(stateEntry.getKey(), map);
				if (!sortedRows.containsKey(map))
				{
					equivalenceClassNumber++;
					sortedRows.put(map,equivalenceClassNumber);newEquivClasses.put(stateEntry.getKey(), equivalenceClassNumber);
				}
				else
					newEquivClasses.put(stateEntry.getKey(), sortedRows.get(map));
			}

			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.

					if (stateA.getValue().equals(stateB.getValue()) &&
							!newEquivClasses.get(stateA.getKey()).equals(newEquivClasses.get(stateB.getKey())))
					{// the two states used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.
						
						List<String> Wsequence = null;
						if (Wdata.get(stateA.getKey()).containsKey(stateB.getKey()))
							Wsequence = Wdata.get(stateA.getKey()).get(stateB.getKey());// depending on the ordering in the matrix, either (A,B) or (B,A) should be defined.
						else
							Wsequence = Wdata.get(stateB.getKey()).get(stateA.getKey());
						assert Wsequence != null : "In states ("+stateA.getKey()+","+stateB.getKey()+") Wsequence is null";
						assert Wsequence.isEmpty() : "In states ("+stateA.getKey()+","+stateB.getKey()+") Wsequence is non-empty and contains "+Wsequence;
						
						// the two states used to be equivalent but not any more, find the different element
						String label = computeDistinguishingLabel(stateA.getKey(), stateB.getKey(), newMap).iterator().next();
						CmpVertex toA = fsm.transitionMatrix.get(stateA.getKey()).get(label);
						CmpVertex toB = fsm.transitionMatrix.get(stateB.getKey()).get(label);
						Wsequence.add(label);
						if (toA != null && toB != null) // these can be null at the first iteration, where states are distinguished based on their response to inputs rather then on the states they lead to.
						{
							List<String> Wprevious = null;
							if (Wdata.get(toA).containsKey(toB))
								Wprevious = Wdata.get(toA).get(toB);// depending on the ordering in the matrix, either (A,B) or (B,A) should be defined.
							else
								Wprevious = Wdata.get(toB).get(toA);
							assert Wprevious != null : "In states ("+stateA.getKey()+","+stateB.getKey()+") previous pair ("+toA+","+toB+") has a null sequence";
							assert !Wprevious.isEmpty() : "In states ("+stateA.getKey()+","+stateB.getKey()+") previous pair ("+toA+","+toB+") was not separated";
							
							Wsequence.addAll(Wprevious);
						}
					}
				}			
			}			

			equivalenceClasses = newEquivClasses;newEquivClasses = new LinkedHashMap<CmpVertex,Integer>();
		}
		while(equivalenceClassNumber > oldEquivalenceClassNumber);

		List<List<String>> result = new LinkedList<List<String>>();
		if (oldEquivalenceClassNumber == fsm.transitionMatrix.size() )
		{
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
					List<String> seq = Wdata.get(stateA.getKey()).get(stateB.getKey());
					if (!seq.isEmpty()) 
						result.add(seq);
				}
			}			
		}
		else
			// report equivalent states
			throw EquivalentStatesException.construct(equivalenceClasses,fsm);
		
		return result;
	}

	/** Given a map from pairs of states to sets of labels which distinguish between these pairs, 
	 * this method returns a label which will distinguish the largest number of pairs.
	 * 
	 * @param distinguishingLabels a map from pairs of states to sets of distinguishing labels.
	 * @return
	 */ 
	public static String computeTopLabel(Map<CmpVertex,Map<CmpVertex,Set<String>>> distinguishingLabels)
	{
		Map<String,AtomicInteger> distLabelUsage = new HashMap<String,AtomicInteger>();// reset the histogram ...
		String topLabel = null;int topLabelCounter = -1;// ... and the top element
		for(Entry<CmpVertex,Map<CmpVertex,Set<String>>> stateAdist:distinguishingLabels.entrySet())
		{
			for(Entry<CmpVertex,Set<String>> stateB:stateAdist.getValue().entrySet())
			{// the two states used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.
				for(String label:stateB.getValue())
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

	/** Computes a characterising set, assuming that there are no unreachable states (with unreachable states, 
	 * it will take a bit longer to perform the computation). 
	 * Additionally, it attempts to reduce the size of W and conserve computer memory. W is not reduced as much
	 * as per computeWSet_reducedw though because the distribution of labels is not recomputed each time we 
	 * separate a pair, but is performed once per step.
	 * 
	 * @param fsm the machine to process
	 * @param alphabet
	 * @return characterising set
	 */
	public static Collection<List<String>> computeWSet_reducedmemory(LearnerGraph fsm) throws EquivalentStatesException
	{
		Map<CmpVertex,Integer> equivalenceClasses = new LinkedHashMap<CmpVertex,Integer>(), newEquivClasses = new LinkedHashMap<CmpVertex,Integer>();
		Map<Map<String,Integer>,Integer> sortedRows = new HashMap<Map<String,Integer>,Integer>();
		int WNext[] = new int[fsm.transitionMatrix.size()*(fsm.transitionMatrix.size()+1)/2];
		String WChar[] = new String[fsm.transitionMatrix.size()*(fsm.transitionMatrix.size()+1)/2];for(int i=0;i < WNext.length;++i) { WChar[i]=null;WNext[i]=-1; }
		final Map<String,AtomicInteger> distinguishingLabels = new HashMap<String,AtomicInteger>();

		for(CmpVertex state:fsm.transitionMatrix.keySet()) 
			equivalenceClasses.put(state, 0);
		for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
		{
			Map<CmpVertex,List<String>> row = new HashMap<CmpVertex,List<String>>();

			Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
			while(stateB_It.hasNext())
			{
				Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
				row.put(stateB.getKey(), new LinkedList<String>());
			}
		}

		int equivalenceClassNumber = 0,oldEquivalenceClassNumber=0;
		do
		{
			oldEquivalenceClassNumber = equivalenceClassNumber;
			Map<CmpVertex,Map<String,Integer>> newMap = new HashMap<CmpVertex,Map<String,Integer>>();
			equivalenceClassNumber = 0;sortedRows.clear();newEquivClasses.clear();
			for(Entry<CmpVertex,Map<String,CmpVertex>> stateEntry:fsm.transitionMatrix.entrySet())
			{
				Map<String,Integer> map = new HashMap<String,Integer>();
				Map<String,CmpVertex> labelNSmap = stateEntry.getValue();
				if (labelNSmap != null)
					for(Entry<String,CmpVertex> labelstate:labelNSmap.entrySet())
						map.put(labelstate.getKey(), equivalenceClasses.get(labelstate.getValue()));
				
				newMap.put(stateEntry.getKey(), map);
				if (!sortedRows.containsKey(map))
				{
					equivalenceClassNumber++;
					sortedRows.put(map,equivalenceClassNumber);newEquivClasses.put(stateEntry.getKey(), equivalenceClassNumber);
				}
				else
					newEquivClasses.put(stateEntry.getKey(), sortedRows.get(map));
			}


			distinguishingLabels.clear();// clear a map from pairs of states to sets of labels which distinguish between them
			
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.

					if (stateA.getValue().equals(stateB.getValue()) &&
							!newEquivClasses.get(stateA.getKey()).equals(newEquivClasses.get(stateB.getKey())))
					{// the two states used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.
						
						// the two states used to be equivalent but not any more, find inputs which 
						// distinguish between them and update the histogram to count the number
						// of inputs which can be used distuigish between states at this stage.
						for(String distLabel:computeDistinguishingLabel(stateA.getKey(), stateB.getKey(), newMap))
						{
							AtomicInteger aint = distinguishingLabels.get(distLabel);
							if (aint == null) { aint = new AtomicInteger(1);distinguishingLabels.put(distLabel, aint); }
							else aint.addAndGet(1);
						}
					}
				}
			}

			// distinguishingLabels contains all labels we may use; the choice of an optimal subset is NP, hence we simply pick
			// those which look best until we distinguish all states.
			ArrayList<String> labelList = new ArrayList<String>(distinguishingLabels.size());labelList.addAll(0, distinguishingLabels.keySet());
			Collections.sort(labelList, new Comparator<String>(){

				public int compare(String o1, String o2) {
					int diffInNumberOfdistStates = -distinguishingLabels.get(o1).get() + distinguishingLabels.get(o2).get();
					if (diffInNumberOfdistStates != 0) return diffInNumberOfdistStates;
					return o1.compareTo(o2);// otherwise, just compare strings.
				}
				
			});
			
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
				
					if (stateA.getValue().equals(stateB.getValue()) &&
							!newEquivClasses.get(stateA.getKey()).equals(newEquivClasses.get(stateB.getKey())))
					{// the two states used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.
						Set<String> distLabels = computeDistinguishingLabel(stateA.getKey(), stateB.getKey(), newMap);
						String topLabel = null;
						Iterator<String> topLabelIter = labelList.iterator();
						while(topLabel == null)
						{
							String lbl = topLabelIter.next();if (distLabels.contains(lbl)) topLabel = lbl;
						}
	
						CmpVertex toA = fsm.transitionMatrix.get(stateA.getKey()).get(topLabel);
						CmpVertex toB = fsm.transitionMatrix.get(stateB.getKey()).get(topLabel);
						int index = fsm.wmethod.vertexToInt(stateA.getKey(),stateB.getKey());
						assert WChar[index] == null : "In states ("+stateA.getKey()+","+stateB.getKey()+") Wsequence is non-empty and contains "+WChar[index];
						WChar[index] = topLabel;
						if (toA != null && toB != null) // these can be null at the first iteration, where states are distinguished based on their response to inputs rather then on the states they lead to.
						{
							int previous = fsm.wmethod.vertexToInt(toA,toB);
							assert WChar[previous] != null : "In states ("+stateA.getKey()+","+stateB.getKey()+") previous pair ("+toA+","+toB+") has a null sequence";
							WNext[index] = previous;
						}
					}						
				}			
			}
			
			
			equivalenceClasses = newEquivClasses;newEquivClasses = new LinkedHashMap<CmpVertex,Integer>();
		}
		while(equivalenceClassNumber > oldEquivalenceClassNumber);

		Collection<List<String>> result = new HashSet<List<String>>();
		if (oldEquivalenceClassNumber == fsm.transitionMatrix.size() )
		{
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
					LinkedList<String> seq = new LinkedList<String>();
					int index = fsm.wmethod.vertexToInt(stateA.getKey(),stateB.getKey());
					while(index >= 0)
					{
						seq.add(WChar[index]);index=WNext[index];
					}
					if (!seq.isEmpty()) 
						result.add(seq);
				}
			}			
		}
		else
			// report equivalent states
			throw EquivalentStatesException.construct(equivalenceClasses,fsm);

		return result;
	}

	/** Computes a characterising set, assuming that there are no unreachable states (with unreachable states, 
	 * it will take a bit longer to perform the computation). 
	 * Additionally, it attempts to reduce the size of W.
	 * 
	 * @param fsm the machine to process
	 * @param alphabet
	 * @return characterising set
	 */
	public static Collection<List<String>> computeWSet_reducedw(LearnerGraph fsm) throws EquivalentStatesException
	{
		
		Map<CmpVertex,Integer> equivalenceClasses = new LinkedHashMap<CmpVertex,Integer>(), newEquivClasses = new LinkedHashMap<CmpVertex,Integer>();
		Map<Map<String,Integer>,Integer> sortedRows = new HashMap<Map<String,Integer>,Integer>();
		Map<CmpVertex,Map<CmpVertex,List<String>>> Wdata = new HashMap<CmpVertex,Map<CmpVertex,List<String>>>();
		Map<CmpVertex,Map<CmpVertex,Set<String>>> distinguishingLabels = new HashMap<CmpVertex,Map<CmpVertex,Set<String>>>();

		for(CmpVertex state:fsm.transitionMatrix.keySet()) 
			equivalenceClasses.put(state, 0);
		for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
		{
			Map<CmpVertex,List<String>> row = new HashMap<CmpVertex,List<String>>();
			Wdata.put(stateA.getKey(), row);

			Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
			while(stateB_It.hasNext())
			{
				Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
				row.put(stateB.getKey(), new LinkedList<String>());
			}
		}

		int equivalenceClassNumber = 0,oldEquivalenceClassNumber=0;
		do
		{
			oldEquivalenceClassNumber = equivalenceClassNumber;
			Map<CmpVertex,Map<String,Integer>> newMap = new HashMap<CmpVertex,Map<String,Integer>>();
			equivalenceClassNumber = 0;sortedRows.clear();newEquivClasses.clear();
			for(Entry<CmpVertex,Map<String,CmpVertex>> stateEntry:fsm.transitionMatrix.entrySet())
			{
				Map<String,Integer> map = new HashMap<String,Integer>();
				Map<String,CmpVertex> labelNSmap = stateEntry.getValue();
				if (labelNSmap != null)
					for(Entry<String,CmpVertex> labelstate:labelNSmap.entrySet())
						map.put(labelstate.getKey(), equivalenceClasses.get(labelstate.getValue()));
				
				newMap.put(stateEntry.getKey(), map);
				if (!sortedRows.containsKey(map))
				{
					equivalenceClassNumber++;
					sortedRows.put(map,equivalenceClassNumber);newEquivClasses.put(stateEntry.getKey(), equivalenceClassNumber);
				}
				else
					newEquivClasses.put(stateEntry.getKey(), sortedRows.get(map));
			}


			distinguishingLabels.clear();// clear a map from pairs of states to sets of labels which distinguish between them
			
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.

					if (stateA.getValue().equals(stateB.getValue()) &&
							!newEquivClasses.get(stateA.getKey()).equals(newEquivClasses.get(stateB.getKey())))
					{// the two states used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.
						
						// the two states used to be equivalent but not any more, find inputs which 
						// distinguish between them and update the histogram to count the number
						// of inputs which can be used distuigish between states at this stage.
						Map<CmpVertex,Set<String>> stateToDist = distinguishingLabels.get(stateA.getKey());
						if (stateToDist == null) stateToDist = new HashMap<CmpVertex,Set<String>>();
						stateToDist.put(stateB.getKey(), computeDistinguishingLabel(stateA.getKey(), stateB.getKey(), newMap));
						distinguishingLabels.put(stateA.getKey(), stateToDist);
					}
				}
			}

			// First, we compute the histogram of label usage
			String topLabel = computeTopLabel(distinguishingLabels);
			while(topLabel != null)
			{// distinguishingLabels contains all labels we may use; the choice of an optimal subset is NP, hence we simply pick
			 // those which look best until we collect enough to distinguish all states (the loop is guaranteed to terminate
			 // because we have enough data in distinguishingLabels for this, by the virtue of us getting as far as this
		     // in the current procedure).
				
				// Now topLabel is the most often used one, we use it to separate all relevant states
				for(Entry<CmpVertex,Map<CmpVertex,Set<String>>> stateA:distinguishingLabels.entrySet())
				{
					for(Entry<CmpVertex,Set<String>> stateB:stateA.getValue().entrySet())
					{// the two states used to be in the same equivalence class, now they are in different ones, hence we populate the matrix.
	
						if (distinguishingLabels.get(stateA.getKey()).get(stateB.getKey()).contains(topLabel))
						{
							List<String> Wsequence = null;
							if (Wdata.get(stateA.getKey()).containsKey(stateB.getKey()))
								Wsequence = Wdata.get(stateA.getKey()).get(stateB.getKey());// depending on the ordering in the matrix, either (A,B) or (B,A) should be defined.
							else
								Wsequence = Wdata.get(stateB.getKey()).get(stateA.getKey());
							assert Wsequence != null : "In states ("+stateA.getKey()+","+stateB.getKey()+") Wsequence is null";
							assert Wsequence.isEmpty() : "In states ("+stateA.getKey()+","+stateB.getKey()+") Wsequence is non-empty and contains "+Wsequence;

							//assert topLabel != null;
														
							CmpVertex toA = fsm.transitionMatrix.get(stateA.getKey()).get(topLabel);
							CmpVertex toB = fsm.transitionMatrix.get(stateB.getKey()).get(topLabel);
							Wsequence.add(topLabel);
							if (toA != null && toB != null) // these can be null at the first iteration, where states are distinguished based on their response to inputs rather then on the states they lead to.
							{
								List<String> Wprevious = null;
								if (Wdata.get(toA).containsKey(toB))
									Wprevious = Wdata.get(toA).get(toB);// depending on the ordering in the matrix, either (A,B) or (B,A) should be defined.
								else
									Wprevious = Wdata.get(toB).get(toA);
								assert Wprevious != null : "In states ("+stateA.getKey()+","+stateB.getKey()+") previous pair ("+toA+","+toB+") has a null sequence";
								assert !Wprevious.isEmpty() : "In states ("+stateA.getKey()+","+stateB.getKey()+") previous pair ("+toA+","+toB+") was not separated";
								
								Wsequence.addAll(Wprevious);
							}
							
							// now that we added the top label, we need  to remove it from the appropriate sets of labels.
							distinguishingLabels.get(stateA.getKey()).get(stateB.getKey()).clear();
						}
					}			
				}
				
				topLabel = computeTopLabel(distinguishingLabels);
			} // while(topLabel != null)
			
			equivalenceClasses = newEquivClasses;newEquivClasses = new LinkedHashMap<CmpVertex,Integer>();
		}
		while(equivalenceClassNumber > oldEquivalenceClassNumber);

		Collection<List<String>> result = new HashSet<List<String>>();
		if (oldEquivalenceClassNumber == fsm.transitionMatrix.size() )
		{
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
					List<String> seq = Wdata.get(stateA.getKey()).get(stateB.getKey());
					if (!seq.isEmpty()) 
						result.add(seq);
				}
			}			
		}
		else
			// report equivalent states
			throw EquivalentStatesException.construct(equivalenceClasses,fsm);

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
		 * @param pairAB
		 * @return false if the pair is already in the collection.
		 */
		public boolean addAndCheck(StatePair pairAB)
		{
			Set<CmpVertex> row = pairs.get(pairAB.firstElem);
			if (row == null)
			{
				row = new TreeSet<CmpVertex>();pairs.put(pairAB.firstElem,row);
			}
			boolean result = row.contains(pairAB.secondElem);// if we've just created a row, the outcome will be false, same if the value does not exist.
			if (!result)
				row.add(pairAB.secondElem);

			return !result;
		}
		
		/** Given a state in the first graph (expected), this method takes set of states related to it and for each of them
		 * takes states associated with them on the right-hand side of <em>StatePair</em> 
		 * @param state
		 * @return
		 */
		public Map<PAIRCOMPATIBILITY,Set<CmpVertex>> statesAssociatedToThoseRelatedTo(CmpVertex state)
		{
			Map<PAIRCOMPATIBILITY,Set<CmpVertex>> result = new TreeMap<PAIRCOMPATIBILITY,Set<CmpVertex>>();
			Map<CmpVertex,PAIRCOMPATIBILITY> map = first.pairCompatibility.compatibility.get(state);
			if (map != null)
				for(Entry<CmpVertex,PAIRCOMPATIBILITY> entry:map.entrySet())
				{
					Set<CmpVertex> row = result.get(entry.getValue());
					if (row == null)
					{
						row = new TreeSet<CmpVertex>();result.put(entry.getValue(), row);
					}
					Set<CmpVertex> rightHand = pairs.get(entry.getKey());
					String errorDescrPart1 = "state "+ state+" is mapped with "+entry.getValue().name()+" to "+entry.getKey()+" of the graph ";
					if (rightHand == null)
						throw new DifferentFSMException(errorDescrPart1 + "which does not have a corresponding state in the expected graph, only "+pairs.toString()+" are known");
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
					Set<CmpVertex> row = result.get(entry.getValue());
					if (row == null)
					{
						row = new TreeSet<CmpVertex>();result.put(entry.getValue(), row);
					}
					row.add(entry.getKey());
				}
			return result;
		}
		
		/** Iterates through states in the collection and checks the associations of the vertices
		 * are preserved by the <em>pairs</em> relation.
		 */
		public void checkPairsAssociatedCorrectly()
		{
			for(Entry<CmpVertex,Set<CmpVertex>> entry:pairs.entrySet())
			{
				Map<PAIRCOMPATIBILITY,Set<CmpVertex>> firstMap = statesAssociatedToThoseRelatedTo(entry.getKey());
				for(CmpVertex secondVertex:entry.getValue())
				{
					Map<PAIRCOMPATIBILITY,Set<CmpVertex>> secondMap = statesAssociatedTo(secondVertex);
					if (!firstMap.equals(secondMap))
						throw new DifferentFSMException("state pair "+entry.getKey()+" and "+secondVertex+" have incompatible associations");
				}
			}
		}
	}
	
	/** Checks the equivalence between the two states, stateG of graphA and stateB of graphB.
	 * Unreachable states  are ignored.
	 * Compatibility labelling other than INCOMPATIBLE is only checked for deterministic graphs.
	 * <p>
	 * Note that its not possible to convert pairs to bi-directional transitions and match
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
	 * @return DifferentFSMException if machines are different and null otherwise.
	 */
	@SuppressWarnings("null")
	public static <TARGET_A_TYPE,TARGET_B_TYPE,
		CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
		CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>> 
		DifferentFSMException checkM(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> expectedArg,CmpVertex stateExpectedArg,
				AbstractLearnerGraph<TARGET_B_TYPE, CACHE_B_TYPE> actualArg,CmpVertex stateActualArg, VERTEX_COMPARISON_KIND compareVertices)
	{
		assert stateExpectedArg != null && stateActualArg != null;
		LearnerGraph expected = null, actual = null;
		CmpVertex stateActual = null, stateExpected = null;

		if (expectedArg instanceof LearnerGraph && actualArg instanceof LearnerGraph)
		{// deterministic case
			expected = (LearnerGraph)expectedArg;actual = (LearnerGraph)actualArg;stateActual = stateActualArg;stateExpected = stateExpectedArg;
		}
		else
		{// non-deterministic case
			try {// This one potentially makes copies of states with different names.
				expected = expectedArg.pathroutines.buildDeterministicGraph(stateExpectedArg);
				actual = actualArg.pathroutines.buildDeterministicGraph(stateActualArg);
			} catch (IncompatibleStatesException e) {
				Helper.throwUnchecked("failed to build a deterministic version of a supplied graph", e);
			}
			stateActual = actual.init;stateExpected = expected.init;
		}
		
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue
		CollectionOfPairs statesAddedToBoundary = new CollectionOfPairs(expected,actual);
		currentExplorationBoundary.add(new StatePair(stateExpected,stateActual));statesAddedToBoundary.addAndCheck(new StatePair(stateExpected,stateActual));
		switch(compareVertices)
		{
		case DEEP:
			if (!DeterministicDirectedSparseGraph.deepEquals(stateExpected,stateActual))
				return new DifferentFSMException("vertices "+stateExpected+" and "+stateActual+" have different values of attributes");
			if (stateActual.getOrigState() == null)
			{
				if (stateExpected.getOrigState() != null)
				return new DifferentFSMException("vertices "+stateExpected+" and "+stateActual+" have different names");
			}
			else
				if (!stateActual.getOrigState().equals(stateExpected.getOrigState()))
					return new DifferentFSMException("vertices "+stateExpected+" and "+stateActual+" have different names");
			break;
		case NAMES:	
			if (!stateActual.getOrigState().equals(stateExpected.getOrigState()))
				return new DifferentFSMException("vertices "+stateExpected+" and "+stateActual+" have different names");
			break;
		case NONE:// nothing needs doing 
			break;
		}

		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair statePair = currentExplorationBoundary.remove();
			assert expected.transitionMatrix.containsKey(statePair.firstElem) : "state "+statePair.firstElem+" is not known to the expected graph";
			assert actual.transitionMatrix.containsKey(statePair.secondElem) : "state "+statePair.secondElem+" is not known to the actual graph";
			if (statePair.firstElem.isAccept() != statePair.secondElem.isAccept())
				return new DifferentFSMException("states "+statePair.firstElem+" and " + statePair.secondElem+" have a different acceptance labelling between the machines");

			Map<String,CmpVertex> expectedTargets = expected.transitionMatrix.get(statePair.firstElem);
			Map<String,CmpVertex> actualTargets = actual.transitionMatrix.get(statePair.secondElem);
			if (expectedTargets.size() != actualTargets.size())// each of them is equal to the keyset size from determinism
				return new DifferentFSMException("different number of transitions from states "+statePair);
				
			for(Entry<String,CmpVertex> labelToActualState:actualTargets.entrySet())
			{
				String label = labelToActualState.getKey();
				if (!expectedTargets.containsKey(label))
					return new DifferentFSMException("no transition with expected label \""+label+"\" from a state \""+statePair.secondElem+"\" corresponding to \""+statePair.firstElem+"\"");
				CmpVertex nextExpectedState = expectedTargets.get(label);
				CmpVertex nextActualState = labelToActualState.getValue();// the original one
				
				StatePair nextPair = new StatePair(nextExpectedState,nextActualState);
				//System.out.println("outgoing: "+statePair.getR()+","+statePair.getQ()+"-"+label+"->"+nextPair.getR()+","+nextPair.getQ());// elements of the pairs are in reverse order
				if (statesAddedToBoundary.addAndCheck(nextPair))
				{
					switch(compareVertices)
					{
					case DEEP:
						//System.out.println("looking at "+expectedState+" ("+expectedState.getColour()+") and "+tState+" ("+tState.getColour()+") ");
						if (!DeterministicDirectedSparseGraph.deepEquals(nextExpectedState,nextActualState))
							return new DifferentFSMException("vertices "+nextExpectedState+" and "+nextActualState+" have different values of attributes");
						if (nextActualState.getOrigState() == null)
						{
							if (nextExpectedState.getOrigState() != null)
								return new DifferentFSMException("vertices "+nextExpectedState+" and "+nextActualState+" have different names");
						}
						else
							if (!nextActualState.getOrigState().equals(nextExpectedState.getOrigState()))
								return new DifferentFSMException("vertices "+nextExpectedState+" and "+nextActualState+" have different names");
						break;
					case NAMES:	
						if (!nextActualState.getOrigState().equals(nextExpectedState.getOrigState()))
							return new DifferentFSMException("vertices "+nextExpectedState+" and "+nextActualState+" have different names");
						break;
					case NONE:// nothing needs doing 
						break;
					}

					currentExplorationBoundary.offer(nextPair);
				}
			}
		}

		// now iterate through the maps of incompatible states and make updates.
		statesAddedToBoundary.checkPairsAssociatedCorrectly();
		return null;
	}

	/** Checks the equivalence between the two states, stateG of graphA and stateB of graphB.
	 * Unreachable states are ignored. 
	 */
	public static <TARGET_A_TYPE,TARGET_B_TYPE,
		CACHE_A_TYPE extends CachedData<TARGET_A_TYPE, CACHE_A_TYPE>,
		CACHE_B_TYPE extends CachedData<TARGET_B_TYPE, CACHE_B_TYPE>> 
		DifferentFSMException checkM(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> expected,
				AbstractLearnerGraph<TARGET_B_TYPE, CACHE_B_TYPE> graph)
	{
		return checkM(expected,expected.init, graph, graph.init,VERTEX_COMPARISON_KIND.NONE);
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
		DifferentFSMException checkM_and_colours(AbstractLearnerGraph<TARGET_A_TYPE, CACHE_A_TYPE> A,
				AbstractLearnerGraph<TARGET_B_TYPE, CACHE_B_TYPE> B, VERTEX_COMPARISON_KIND howToCompare)
				
	{
		DifferentFSMException ex = WMethod.checkM(A, A.init,B,B.init,howToCompare);if (ex != null) return ex;
		return null;
	}
		
				
	/** Checks if the two graphs have the same set of states. */
	public static boolean sameStateSet(LearnerGraph expected, LearnerGraph graph)
	{
		Set<CmpVertex> A=expected.transitionMatrix.keySet(), B=graph.transitionMatrix.keySet();
		Set<CmpVertex> AmB = new TreeSet<CmpVertex>(),BmA=new TreeSet<CmpVertex>();
		AmB.addAll(A);AmB.removeAll(B);BmA.addAll(B);BmA.removeAll(A);
		if (!A.equals(B))
		{
			System.out.println("different sets of states,\nA-B="+AmB+"\nB-A="+BmA);
		}
		return A.equals(B);
	}

	/** Given an FSM and a W set, checks if it is a valid W set and throws if not.
	 * 
	 * @param fsm the machine
	 * @param wset the set to check validity of.
	 */
	public void checkW_is_corrent(Collection<List<String>> wset)
	{
		String result = checkW_is_corrent_boolean(wset);
		if (result != null)
			fail(result);
	}
	
	public String checkW_is_corrent_boolean(Collection<List<String>> wset)
	{
		for(CmpVertex stateA:coregraph.transitionMatrix.keySet())
		{
			for(CmpVertex stateB:coregraph.transitionMatrix.keySet())
				if (stateA != stateB)
				{
					boolean foundString = false;
					Iterator<List<String>> pathIt = wset.iterator();
					while(pathIt.hasNext() && !foundString)
					{
						List<String> path = pathIt.next();
						int aResult = coregraph.paths.tracePath(path, stateA),
							bResult = coregraph.paths.tracePath(path, stateB);
						
						if ( (aResult == AbstractOracle.USER_ACCEPTED && bResult >= 0) ||
								(bResult == AbstractOracle.USER_ACCEPTED && aResult >= 0))
							foundString = true;
					}
					
					if (!foundString)
						return "W set does not distinguish between "+stateA+" and "+stateB;
				}
		}
		
		return null;
	}
	
	public interface FsmPermutator {
		/** Returns a collection representing an order in which elements of an FSM should be placed in a string. */
		ArrayList<Pair<CmpVertex,String>> getPermutation(Collection<Pair<CmpVertex,String>> from);
	}

	/** This method permutes states of a supplied machine using the permutation function provided.
	 * 
	 * @param perm the permutator to use.
	 * @return the state machine permuted from the current one using the supplied permutator.
	 */
	public LearnerGraph Permute(FsmPermutator perm)
	{
		ArrayList<Pair<CmpVertex,String>> transitionList = new ArrayList<Pair<CmpVertex,String>>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> row:coregraph.transitionMatrix.entrySet())
			for(Entry<String,CmpVertex> nextState:row.getValue().entrySet())
				transitionList.add(new Pair<CmpVertex,String>(row.getKey(),nextState.getKey()));
		
		ArrayList<Pair<CmpVertex,String>> permutation = perm.getPermutation(transitionList);
		Assert.assertEquals(transitionList.size(), permutation.size());
		StringBuffer newFsm = new StringBuffer();
		for(Pair<CmpVertex,String> p:permutation)
		{
			CmpVertex from = p.firstElem;String label = p.secondElem;
			newFsm.append("\n"+from+"-"+label+"->"+coregraph.transitionMatrix.get(from).get(label));
		}
		LearnerGraph permFsm = new LearnerGraph(buildGraph(newFsm.toString(), "testDeterminism_perm"),coregraph.config);
		permFsm.init = permFsm.findVertex(coregraph.init.getID());
		return permFsm;
	}

	/** Returns true if IDs of all states in a graph are numeric rather than text. */
	public boolean checkGraphNumeric()
	{
		for(CmpVertex vert:coregraph.transitionMatrix.keySet())
			if (vert.getID().getKind() == DeterministicDirectedSparseGraph.VertexID.VertKind.NONE)
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

