package statechum.analysis.learning.spin;

import java.util.List;

import statechum.Label;
import statechum.analysis.learning.rpnicore.LearnerGraph;

public class SpinResult{
	
	boolean pass;
	List<List<Label>>counters;
	
	public SpinResult(){
		pass = false;
		counters = null;
	}
	
	public SpinResult(boolean passArg, List<List<Label>>countersArg){
		this.pass=passArg;
		this.counters=countersArg;
	}
	
	public boolean isPass() {
		return pass;
	}
	public List<List<Label>> getCounters() {
		return counters;
	}

	public void setPass(boolean passArg) {
		this.pass = passArg;
	}

	public void setCounters(List<List<Label>> countersArg) {
		this.counters = countersArg;
	}
	
	public void ensureCountersConsistent(LearnerGraph current){
		SpinUtil.removeInvalidPrefixCounters(counters, current);
	}
	
	
	
}