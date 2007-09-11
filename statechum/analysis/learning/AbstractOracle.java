package statechum.analysis.learning;

import java.util.List;

public interface AbstractOracle {
	
	/** Retrieves a stored answer. */
	public int getAnswer(List<String> question);

}
