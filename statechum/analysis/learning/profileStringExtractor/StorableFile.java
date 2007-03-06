package statechum.analysis.learning.profileStringExtractor;

import java.io.Serializable;
import java.util.*;

public class StorableFile implements Serializable {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -7986277390493548791L;
	HashMap namesToMethods;
	
	public StorableFile(HashMap names){
		this.namesToMethods = names;
	}


	public HashMap getNamesToMethods() {
		return namesToMethods;
	}

}
