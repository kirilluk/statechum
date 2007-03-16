package statechum.xmachine.model;



/**
 * This is meant to store the memory that corresponds to a state in an X-Machine. Currently this is 
 * only a preliminary implementation, where the state is simply stored as a string. The string could 
 * be a FOL condition on the state variables of a class for example. 
 * 
 * @author Neil Walkinshaw
 *
 */


public class MemState {
	
	private String state;

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

}
