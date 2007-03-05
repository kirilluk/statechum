package statechum.analysis.learning;

import edu.uci.ics.jung.graph.*;

public class StatePair {
	
	private Vertex q, r;
	
	public StatePair(Vertex q, Vertex r){
		this.q = q;
		this.r = r;
	}
	
	private int intLabel(Vertex v){
		String vLabel = v.getUserDatum("label").toString();
		return Integer.valueOf(vLabel);
	}
	
	public boolean greaterThan(StatePair p){
		int qInt = intLabel(q);
		int rInt = intLabel(r);
		int pQLabel = intLabel(p.getQ());
		int pRLabel = intLabel(p.getR());
		if(qInt>pQLabel)
			return true;
		else if(pQLabel == qInt && rInt>pRLabel)
			return true;
		else{
			//System.out.println(this + " <= "+ p);
			return false;
		}
		
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
