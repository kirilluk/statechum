/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

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
