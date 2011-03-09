package statechum.analysis.learning.experiments;


public class Transition 
{
	public String from, to, label;
	
	public Transition(String argFrom, String argTo, String argLabel)
	{
		this.from = argFrom; this.to = argTo; this.label=argLabel;
	}

	public int compareTo(Transition compare) 
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
		if(o instanceof Transition){
			Transition compare = (Transition)o;
			if (compare.from.equals(this.from)
					&& compare.to.equals(this.to)
					&& compare.label.equals(this.label))
				return true;
		}
		return false;
	}
	
	@Override
	public String toString(){
		return from + "-" + label+"->"+to;
	}

	public String getFrom() {
		return from;
	}

	public void setFrom(String argFrom) {
		this.from = argFrom;
	}

	public String getTo() {
		return to;
	}

	public void setTo(String argTo) {
		this.to = argTo;
	}
	
	
}