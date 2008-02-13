/**
 * 
 */
package statechum.xmachine.model.testset;

import statechum.xmachine.model.testset.PTATestSequenceEngine.FSMAbstraction;

/**
 * @author kirill
 *
 */
public class PTASequenceSetAutomaton implements FSMAbstraction {
	public static final String theOnlyState = "TheOnlyState";
	
	public PTASequenceSetAutomaton() 
	{ 
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
