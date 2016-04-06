package statechum.analysis.learning.experiments.mutation;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;


public class Transition  
{
	private CmpVertex from, to;
	private Label label;
	
	public Transition(CmpVertex argFrom, CmpVertex argTo, Label argLabel)
	{
		this.from = argFrom; this.to = argTo; this.label=argLabel;
	}

	public int compareTo(Object compare) 
	{
		return toString().compareTo(compare.toString());
	}
	
	@Override
	public int hashCode(){
		final int prime = 31;
		int result = 1;
		result = prime * result + from.hashCode();
		result = prime * result + to.hashCode();
		result = prime * result + label.hashCode();
		return result;
	}
	
	@Override
	public boolean equals(Object o){
		Transition t = (Transition)o;
		return from.equals(t.from) && to.equals(t.to) && label.equals(t.label);
	}
	
	@Override
	public String toString(){
		return from + "-" + label+"->"+to;
	}

	public CmpVertex getFrom() {
		return from;
	}

	public void setFrom(CmpVertex argFrom) {
		this.from = argFrom;
	}

	public CmpVertex getTo() {
		return to;
	}

	public void setTo(CmpVertex argTo) {
		this.to = argTo;
	}
	
	public Label getLabel(){
		return this.label;
	}
	
}