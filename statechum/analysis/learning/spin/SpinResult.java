package statechum.analysis.learning.spin;

import java.util.List;

import statechum.analysis.learning.rpnicore.LearnerGraph;

public class SpinResult{
	
	boolean pass;
	List<List<String>>counters;
	
	public SpinResult(){
		pass = false;
		counters = null;
	}
	
	public SpinResult(boolean passArg, List<List<String>>countersArg){
		this.pass=passArg;
		this.counters=countersArg;
	}
	
	public boolean isPass() {
		return pass;
	}
	public List<List<String>> getCounters() {
		return counters;
	}

	public void setPass(boolean passArg) {
		this.pass = passArg;
	}

	public void setCounters(List<List<String>> countersArg) {
		this.counters = countersArg;
	}
	
	public void ensureCountersConsistent(LearnerGraph current){
		SpinUtil.removeInvalidPrefixCounters(counters, current);
	}
	
	
	
}