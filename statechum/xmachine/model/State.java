package statechum.xmachine.model;

public class State {
	
	private String label;
	private MemState memory;
	boolean start, term;
	
	public State(String label){
		this.label=label;
	}

	public MemState getMemory() {
		return memory;
	}

	public void setMemory(MemState memory) {
		this.memory = memory;
	}

	public String getLabel() {
		return label;
	}
	
	public boolean equals(State s){
		if(s.getLabel().equals(this.getLabel())){
			return true;
		}
		else
			return false;
	}

	public boolean isStart() {
		return start;
	}

	public void setStart(boolean start) {
		this.start = start;
	}

	public boolean isTerm() {
		return term;
	}

	public void setTerm(boolean term) {
		this.term = term;
	}
	

}
