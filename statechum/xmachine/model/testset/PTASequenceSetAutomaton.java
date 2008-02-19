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

package statechum.xmachine.model.testset;

import statechum.xmachine.model.testset.PTATestSequenceEngine.FSMAbstraction;

/**
 * @author kirill
 * 
 */
public class PTASequenceSetAutomaton implements FSMAbstraction {
	public static final String theOnlyState = "TheOnlyState";

	public PTASequenceSetAutomaton() {
	}

	public Object getInitState() {
		return theOnlyState;
	}

	public Object getNextState(Object currentState, String input) {
		return theOnlyState;
	}

	public boolean isAccept(Object currentState) {
		return true;
	}

	public boolean shouldBeReturned(Object elem) {
		return true;
	}
}
