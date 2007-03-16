package statechum.xmachine.model;



/**
 * This represents a simple transition function that can be attributed to a transition
 * between a pair of states. It can be specified as taking some input and producing an output. 
 * 
 * @author Neil Walkinshaw
 *
 */

public class TransitionFunction {
	
	private Object input, output;
	private String label;
	
	public Object getInput() {
		return input;
	}
	public void setInput(Object input) {
		this.input = input;
	}
	public String getLabel() {
		return label;
	}
	public void setLabel(String label) {
		this.label = label;
	}
	public Object getOutput() {
		return output;
	}
	public void setOutput(Object output) {
		this.output = output;
	}

}
