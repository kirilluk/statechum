package statechum.analysis.learning;

import edu.uci.ics.jung.graph.*;

public class StatePair implements Comparable {
	
	private Vertex q, r;
	
	public StatePair(Vertex q, Vertex r){
		this.q = q;
		this.r = r;
	}
	
	private int intLabel(Vertex v){
		String vLabel = v.getUserDatum("label").toString();
		return Integer.valueOf(vLabel);
	}
	
	public int compareTo(Object b){
		StatePair pB = (StatePair)b;
		int aQLabel = intLabel(q);
		int aRLabel = intLabel(r);
		int bQLabel = intLabel(pB.getQ());
		int bRLabel = intLabel(pB.getR());
		
		if(aQLabel<bQLabel)
			return -1;
		else if (aQLabel>bQLabel)
			return 1;			
		else if(aRLabel<bRLabel)
			return -1;
		else if(aRLabel>bRLabel)
			return 1;
		
		return 0;
	}
	
	public Vertex getQ(){
		return q;
	}
	
	public Vertex getR(){
		return r;
	}
	
	public String toString(){
		return q.getUserDatum("label")+", "+r.getUserDatum("label");
	}
	
	public int hashCode(){
		return q.getUserDatum("label").hashCode()+r.getUserDatum("label").hashCode();
	}
	
	public boolean equals(Object o){
		if(o == null)
			return false;
		if(o instanceof StatePair){
			StatePair other = (StatePair)o;
			Object otherQ = (Object)other.getQ().getUserDatum("label");
			Object otherR = (Object)other.getR().getUserDatum("label");
			Object thisQ = (Object)q.getUserDatum("label");
			Object thisR = (Object)r.getUserDatum("label");
			if(thisQ.equals(otherQ)&&thisR.equals(otherR))
				return true;
		}
		return false;
	}
}
