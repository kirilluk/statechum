package statechum.analysis.learning.profileStringExtractor;

import java.io.Serializable;
import java.util.*;

import javax.swing.tree.TreePath;

public class StorableFile implements Serializable {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -7986277390493548791L;
	
	private Map<String, List<TreePath>> namesToMethods;
	
	public StorableFile(Map<String, List<TreePath>> names){
		this.namesToMethods = names;
	}


	public Map<String, List<TreePath>> getNamesToMethods() {
		return namesToMethods;
	}

}
