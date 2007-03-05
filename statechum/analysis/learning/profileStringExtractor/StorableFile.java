package statechum.analysis.learning.profileStringExtractor;

import java.io.Serializable;
import java.util.*;

public class StorableFile implements Serializable {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -7986277390493548791L;
	HashMap filesToHandlers, namesToMethods;
	
	public StorableFile(HashMap names, HashMap files){
		this.namesToMethods = names;
		this.filesToHandlers = files;
	}

	public HashMap getFilesToHandlers() {
		return filesToHandlers;
	}


	public HashMap getNamesToMethods() {
		return namesToMethods;
	}

}
