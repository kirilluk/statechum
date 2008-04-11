/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.analysis.learning.rpnicore;

import static org.junit.Assert.fail;
import static statechum.analysis.learning.TestFSMAlgo.buildGraph;

import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import junit.framework.Assert;

import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
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
	
	public Set<String> computeAlphabet()
	{
		Set<String> result = new HashSet<String>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
			result.addAll(entry.getValue().keySet());
		return result;
	}
	
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
		Set<String> alphabet =  computeAlphabet();
		List<List<String>> partialSet = computeStateCover();
		characterisationSet = computeWSet(coregraph);if (characterisationSet.isEmpty()) characterisationSet.add(Arrays.asList(new String[]{}));
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
		Set<String> alphabet =  computeAlphabet();
		List<List<String>> stateCover = computeStateCover();
		characterisationSet = computeWSet(coregraph);if (characterisationSet.isEmpty()) characterisationSet.add(Arrays.asList(new String[]{}));
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

	/** Checks if the supplied FSM has unreachable states.
	 * 
	 * @return true if there are any unreachable states.
	 */
	public boolean checkUnreachableStates()
	{
		return computeStateCover().size() != coregraph.transitionMatrix.size();
	}
	
	/** Checks if the supplied FSM has equivalent states. */
	public static boolean checkEquivalentStates(LearnerGraph fsm)
	{
		try
		{
			computeWSet(fsm);
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
	
	/** Computes a state cover (a collection of sequences to reach every state in this machine). */
	public List<List<String>> computeStateCover()
	{
		Queue<CmpVertex> fringe = new LinkedList<CmpVertex>();
		Set<CmpVertex> statesInFringe = new HashSet<CmpVertex>();// in order not to iterate through the list all the time.
		Map<CmpVertex,LinkedList<String>> stateToPath = new HashMap<CmpVertex,LinkedList<String>>();stateToPath.put(coregraph.init, new LinkedList<String>());
		fringe.add(coregraph.init);statesInFringe.add(coregraph.init);
		while(!fringe.isEmpty())
		{
			CmpVertex currentState = fringe.remove();
			LinkedList<String> currentPath = stateToPath.get(currentState);
			Map<String,CmpVertex> targets = coregraph.transitionMatrix.get(currentState);
			if(targets != null && !targets.isEmpty())
				for(Entry<String,CmpVertex> labelstate:targets.entrySet())
				{
					CmpVertex target = labelstate.getValue();
					if (!statesInFringe.contains(target))
					{
						LinkedList<String> newPath = (LinkedList<String>)currentPath.clone();newPath.add(labelstate.getKey());
						stateToPath.put(target,newPath);
						fringe.offer(target);
						statesInFringe.add(target);
					}
				}
		}
		List<List<String>> outcome = new LinkedList<List<String>>();outcome.addAll(stateToPath.values());
		return outcome;
	}
	
	public static class EquivalentStatesException extends Error 
	{
		/**
		 *  For serialization.
		 */
		private static final long serialVersionUID = 6988899034488999997L;
		
		private final CmpVertex stateA, stateB;
		private int NumberOfEquivalentStates;
		
		public CmpVertex getA()
		{
			return stateA;
		}
		
		public CmpVertex getB()
		{
			return stateB;
		}
		
		public EquivalentStatesException(CmpVertex a, CmpVertex b, int Number)
		{
			stateA = a;stateB = b;NumberOfEquivalentStates = Number;
		}
		
		@Override
		public String toString()
		{
			return "There are "+NumberOfEquivalentStates+" equivalent states, such as "+stateA+ " and "+stateB;
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
		{// report equivalent states
			LinkedHashSet<CmpVertex> equivalentStates = new LinkedHashSet<CmpVertex>();
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
					if (stateA.getValue().equals(stateB.getValue()) && !stateA.getKey().equals(stateB.getKey()))
					{
						equivalentStates.add(stateA.getKey());equivalentStates.add(stateB.getKey());
					}
				}
			}
			assert equivalentStates.size() > 0: "equivalent states were not found";
			Iterator<CmpVertex> equivIt = equivalentStates.iterator(); 
			throw new EquivalentStatesException(equivIt.next(), equivIt.next(), equivalentStates.size());
		}
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
					if (counter == null) counter = new AtomicInteger(0);else counter.addAndGet(1);
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
	 * Additionally, it attempts to reduce the size of W.
	 * 
	 * @param fsm the machine to process
	 * @param alphabet
	 * @return characterising set
	 */
	public static Collection<List<String>> computeWSet(LearnerGraph fsm) throws EquivalentStatesException
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
		{// report equivalent states
			LinkedHashSet<CmpVertex> equivalentStates = new LinkedHashSet<CmpVertex>();
			for(Entry<CmpVertex,Integer> stateA:equivalenceClasses.entrySet())
			{
				Iterator<Entry<CmpVertex,Integer>> stateB_It = equivalenceClasses.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Integer> stateB = stateB_It.next();if (stateB.getKey().equals(stateA.getKey())) break; // we only process a triangular subset.
					if (stateA.getValue().equals(stateB.getValue()) && !stateA.getKey().equals(stateB.getKey()))
					{
						equivalentStates.add(stateA.getKey());equivalentStates.add(stateB.getKey());
					}
				}
			}
			assert equivalentStates.size() > 0: "equivalent states were not found";
			Iterator<CmpVertex> equivIt = equivalentStates.iterator(); 
			throw new EquivalentStatesException(equivIt.next(), equivIt.next(), equivalentStates.size());
		}
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
	
	/** Checks the equivalence between the two states, stateG of graphA and stateB of graphB.
	 * Unreachable states are ignored. 
	 */
	public static void checkM(LearnerGraph graph, LearnerGraph expected, CmpVertex stateGraph, CmpVertex stateExpected)
	{
		Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue

		Set<StatePair> statesAddedToBoundary = new HashSet<StatePair>();
		currentExplorationBoundary.add(new StatePair(stateGraph,stateExpected));statesAddedToBoundary.add(new StatePair(stateGraph,stateExpected));
		
		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair statePair = currentExplorationBoundary.remove();
			assert graph.transitionMatrix.containsKey(statePair.firstElem) : "state "+statePair.firstElem+" is not known to the first graph";
			assert expected.transitionMatrix.containsKey(statePair.secondElem) : "state "+statePair.secondElem+" is not known to the second graph";
			if (statePair.firstElem.isAccept() != statePair.secondElem.isAccept())
				throw new DifferentFSMException("states "+statePair.firstElem+" and " + statePair.secondElem+" have a different acceptance labelling between the machines");
						
			Map<String,CmpVertex> targets = graph.transitionMatrix.get(statePair.firstElem), expectedTargets = expected.transitionMatrix.get(statePair.secondElem);
			if (expectedTargets.size() != targets.size())// each of them is equal to the keyset size from determinism
				throw new DifferentFSMException("different number of transitions from state "+statePair);
				
			for(Entry<String,CmpVertex> labelstate:targets.entrySet())
			{
				String label = labelstate.getKey();
				if (!expectedTargets.containsKey(label))
					throw new DifferentFSMException("no transition with expected label "+label+" from a state corresponding to "+statePair.secondElem);
				CmpVertex tState = labelstate.getValue();// the original one
				CmpVertex expectedState = expectedTargets.get(label);
				
				StatePair nextPair = new StatePair(tState,expectedState);
				if (!statesAddedToBoundary.contains(nextPair))
				{
					currentExplorationBoundary.offer(nextPair);
					statesAddedToBoundary.add(nextPair);
				}
			}
		}
		
	}

	/** Checks the equivalence between the two states, stateG of graphA and stateB of graphB.
	 * Unreachable states are ignored. 
	 */
	public static void checkM(LearnerGraph graph, LearnerGraph expected)
	{
		checkM(graph, expected, graph.init,expected.init);
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
						
						if ( (aResult == RPNIBlueFringeLearner.USER_ACCEPTED && bResult >= 0) ||
								(bResult == RPNIBlueFringeLearner.USER_ACCEPTED && aResult >= 0))
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
}

