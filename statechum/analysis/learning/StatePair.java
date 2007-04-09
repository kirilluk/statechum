package statechum.analysis.learning;

import statechum.JUConstants;
import edu.uci.ics.jung.graph.*;

public class StatePair implements Comparable {
	
	private Vertex q, r;
	
	public StatePair(Vertex q, Vertex r){
		this.q = q;
		this.r = r;
	}
	
	private String strLabel(Vertex v){
		String vLabel = v.getUserDatum(JUConstants.LABEL).toString();
		return vLabel;
	}
	
	public int compareTo(Object b){
		StatePair pB = (StatePair)b;
		int qLabels = strLabel(q).compareTo(strLabel(pB.getQ()));
		int rLabels = strLabel(r).compareTo(strLabel(pB.getR()));
		
		if(qLabels != 0)
			return qLabels; 
		return rLabels;
	}
	
	public Vertex getQ(){
		return q;
	}
	
	public Vertex getR(){
		return r;
	}
	
	public String toString(){
		return "[ "+q.getUserDatum(JUConstants.LABEL)+", "+r.getUserDatum(JUConstants.LABEL)+" ]";
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode(){
		final int PRIME = 31;
		return q.getUserDatum(JUConstants.LABEL).hashCode()+PRIME*r.getUserDatum(JUConstants.LABEL).hashCode();
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o){
		if(o == null)
			return false;
		if(o instanceof StatePair){
			StatePair other = (StatePair)o;
			Object otherQ = (Object)other.getQ().getUserDatum(JUConstants.LABEL);
			Object otherR = (Object)other.getR().getUserDatum(JUConstants.LABEL);
			Object thisQ = (Object)q.getUserDatum(JUConstants.LABEL);
			Object thisR = (Object)r.getUserDatum(JUConstants.LABEL);
			if(thisQ.equals(otherQ)&&thisR.equals(otherR))
				return true;
		}
		return false;
	}
}
