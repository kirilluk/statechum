package statechum.xmachine.model.testset;

import java.util.Map;

import statechum.analysis.learning.TestFSMAlgo.FSMStructure;

public class PTA_FSMStructure extends PTATestSequenceEngine {
	public PTA_FSMStructure(FSMStructure machine) {
		fsm = machine;
		init(new FSM());
	}

	private FSMStructure fsm; 

	class FSM implements FSMAbstraction
	{
		public Object getInitState() {
			return fsm.init;
		}
	
		public Object getNextState(Object currentState, String input) 
		{
			String result = null;
			Map<String,String> row = fsm.trans.get(currentState);
			if (row != null)
				result = row.get(input);
			return result;
		}
	
		public boolean isAccept(Object currentState) 
		{
			return fsm.accept.get(currentState);
		}

		public boolean shouldBeReturned(Object elem) {
			return true;
		}
	}
}
