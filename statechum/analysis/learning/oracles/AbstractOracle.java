package statechum.analysis.learning.oracles;

import java.util.List;

public interface AbstractOracle {
	
	/** Retrieves a stored answer. */
	public int getAnswer(List<String> question);

}
