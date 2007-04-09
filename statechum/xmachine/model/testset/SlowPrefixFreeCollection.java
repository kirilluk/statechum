package statechum.xmachine.model.testset;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;

public class SlowPrefixFreeCollection extends PrefixFreeCollection {
	private Collection<List<String>> data = new LinkedHashSet<List<String>>();
	
	@Override
	public void addSequence(List<String> seq) {
		List<List<String>> seqToRemove = new LinkedList<List<String>>();
		for(List<String> s:data)
		{
			if (isPrefix(s, seq))
			{
				assert(seqToRemove.isEmpty());// no sequences are prefixes of the current one - an internal consistency invariant of this procedure.
				return; // the sequence to add is already a prefix of another one
			}
			if (isPrefix(seq, s))
				seqToRemove.add(s);
		}	
		data.add(seq);data.removeAll(seqToRemove);
	}

	@Override
	public Collection<List<String>> getData() {
		return data;
	}

}
