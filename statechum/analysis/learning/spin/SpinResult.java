package statechum.analysis.learning.spin;

import java.util.List;
import java.util.Set;

import statechum.analysis.learning.rpnicore.LearnerGraph;

public class SpinResult{
	
	boolean pass;
	List<List<String>>counters;
	
	public SpinResult(){
		pass = false;
		counters = null;
	}
	
	public SpinResult(boolean pass, List<List<String>>counters){
		this.pass=pass;
		this.counters=counters;
	}
	
	public boolean isPass() {
		return pass;
	}
	public List<List<String>> getCounters() {
		return counters;
	}

	public void setPass(boolean pass) {
		this.pass = pass;
	}

	public void setCounters(List<List<String>> counters) {
		this.counters = counters;
	}
	
	public void ensureCountersConsistent(LearnerGraph current){
		SpinUtil.removeInvalidPrefixCounters(counters, current);
	}
	
	
	
}