package statechum.analysis.learning;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Observable;
import java.util.Set;

import statechum.analysis.learning.TestFSMAlgo.FSMStructure;

public class AngluinLearner extends Observable {
	public static final String rejectState = "REJECT";
	
	FSMStructure fsm = new FSMStructure();
	Set<String> coreStates = new HashSet<String>();// core states, from which we do transition cover
	
	private int stateNumber = 1;
	
	protected String inventNameForNextState()
	{
		return "S"+stateNumber++;
	}
	
	public AngluinLearner()
	{
		fsm.init = "INIT";coreStates.add(fsm.init);
	}
	
	/** Given a sequence of inputs, adds all its prefixes to the graph.
	 * 
	 * @param seq
	 * @param rejectNumber
	 */
	public void addSequence(List<String> seq, int rejectNumber)
	{
		assert rejectNumber < seq.size();
		if (seq.isEmpty()) return;
		String state = fsm.init;
		int i=0;
		Iterator<String> inputIt = seq.iterator();
		while(inputIt.hasNext() && i < rejectNumber)
		{
			String input = inputIt.next();
			Map<String,String> nextStateMap = fsm.trans.get(state);
			String nextState = nextStateMap.get(input);
			if (nextState == rejectState)
				throw new IllegalArgumentException("a positive sequence entered a reject state");
			
			if (nextState == null)
			{// the transition from the given sequence leads to a new state.
				nextState = inventNameForNextState();
				nextStateMap.put(input,nextState);
				
				// if the current sequence is positive and the input belongs to a 
				// proper prefix of seq, we need to mark our new state as the core one.
				if (i < seq.size()-1)
				{// this presupposes that the sequence is positive.
					coreStates.add(nextState);
				}
			}
			state = nextState;
			++i;
		}
		
		if (i == rejectNumber)
		{
			String input = seq.get(i);
			Map<String,String> nextStateMap = fsm.trans.get(state);
			String nextState = nextStateMap.get(input);

			if (nextState != null && nextState != rejectState)
				throw new IllegalArgumentException("a negative sequence is entering a previously-accepted state");
			if (nextState == null)
			{// the transition from the given sequence leads to the reject state.
				nextStateMap.put(input,rejectState);
			}				
		}
	}
	
	
}
