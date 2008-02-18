/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of statechum.

statechum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Foobar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.xmachine.model.testset;

import java.util.Map;

import statechum.analysis.learning.TestFSMAlgo.FSMStructure;

public class PTA_FSMStructure extends PTATestSequenceEngine {
	public PTA_FSMStructure(FSMStructure machine) {
		fsm = machine;
		init(new FSM());
	}

	private FSMStructure fsm;

	class FSM implements FSMAbstraction {
		public Object getInitState() {
			return fsm.init;
		}

		public Object getNextState(Object currentState, String input) {
			String result = null;
			Map<String, String> row = fsm.trans.get(currentState);
			if (row != null)
				result = row.get(input);
			return result;
		}

		public boolean isAccept(Object currentState) {
			return fsm.accept.get(currentState);
		}

		public boolean shouldBeReturned(Object elem) {
			return true;
		}
	}
}
