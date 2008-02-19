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

	Set<String> coreStates = new HashSet<String>();// core states, from which
													// we do transition cover

	private int stateNumber = 1;

	protected String inventNameForNextState() {
		return "S" + stateNumber++;
	}

	public AngluinLearner() {
		fsm.init = "INIT";
		coreStates.add(fsm.init);
	}

	/**
	 * Given a sequence of inputs, adds all its prefixes to the graph.
	 * 
	 * @param seq
	 * @param rejectNumber
	 */
	public void addSequence(List<String> seq, int rejectNumber) {
		assert rejectNumber < seq.size();
		if (seq.isEmpty())
			return;
		String state = fsm.init;
		int i = 0;
		Iterator<String> inputIt = seq.iterator();
		while (inputIt.hasNext() && i < rejectNumber) {
			String input = inputIt.next();
			Map<String, String> nextStateMap = fsm.trans.get(state);
			String nextState = nextStateMap.get(input);
			if (nextState == rejectState)
				throw new IllegalArgumentException(
						"a positive sequence entered a reject state");

			if (nextState == null) {// the transition from the given sequence
									// leads to a new state.
				nextState = inventNameForNextState();
				nextStateMap.put(input, nextState);

				// if the current sequence is positive and the input belongs to
				// a
				// proper prefix of seq, we need to mark our new state as the
				// core one.
				if (i < seq.size() - 1) {// this presupposes that the
											// sequence is positive.
					coreStates.add(nextState);
				}
			}
			state = nextState;
			++i;
		}

		if (i == rejectNumber) {
			String input = seq.get(i);
			Map<String, String> nextStateMap = fsm.trans.get(state);
			String nextState = nextStateMap.get(input);

			if (nextState != null && nextState != rejectState)
				throw new IllegalArgumentException(
						"a negative sequence is entering a previously-accepted state");
			if (nextState == null) {// the transition from the given sequence
									// leads to the reject state.
				nextStateMap.put(input, rejectState);
			}
		}
	}

}
