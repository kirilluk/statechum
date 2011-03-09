package statechum.analysis.learning.experiments;


public class Transition implements Comparable{
	public String from, to, label;
	
	public Transition(String from, String to, String label){
		this.from = from; this.to = to; this.label=label;
	}

	public int compareTo(Object arg0) {
		if(arg0 instanceof Transition){
			Transition compare = (Transition)arg0;
			if (compare.from.equals(this.from)
					&& compare.to.equals(this.to)
					&& compare.label.equals(this.label))
				return 0;
		}
		return -1;
	}
	
	public int hashCode(){
		return 0;
	}
	
	public boolean equals(Object o){
		if(o instanceof Transition){
			Transition compare = (Transition)o;
			if (compare.from.equals(this.from)
					&& compare.to.equals(this.to)
					&& compare.label.equals(this.label))
				return true;
		}
		return false;
	}
	
	public String toString(){
		return from + "-" + label+"->"+to;
	}

	public String getFrom() {
		return from;
	}

	public void setFrom(String from) {
		this.from = from;
	}

	public String getTo() {
		return to;
	}

	public void setTo(String to) {
		this.to = to;
	}
	
	
}