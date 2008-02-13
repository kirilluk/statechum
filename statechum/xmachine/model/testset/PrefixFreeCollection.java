package statechum.xmachine.model.testset;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/** Makes it possible to construct a collection of sequences of 
 * strings where no string is a prefix of another one.
 * 
 * @author Kirill
 */
public abstract class PrefixFreeCollection {
	
	public abstract Collection<List<String>> getData();
	
	/** Adds a sequence to this collection. */
	public abstract void addSequence(List<String> sequence);
	
	/** Returns true if what is a prefix of str.
	 * 
	 * @param str string from a database
	 * @param what what to check against <em>str</em>
	 * @return true if <em>what</em> is a prefix of <em>str</em>.
	 */
	public static boolean isPrefix(List<String> str, List<String> what)
	{
		if (what.size() > str.size()) return false;
		Iterator<String> strIt=str.iterator(), whatIt=what.iterator();
		while(whatIt.hasNext() && strIt.hasNext())
			if (!strIt.next().equals(whatIt.next()))
				return false;
		
		return true;
	}	

	
}
