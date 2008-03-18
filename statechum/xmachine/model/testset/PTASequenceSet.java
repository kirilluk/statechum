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
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import statechum.xmachine.model.testset.PTASequenceEngine.FilterPredicate;

public class PTASequenceSet extends PrefixFreeCollection implements Set<List<String>>
{
	protected PTASequenceEngine engine = new PTASequenceEngine();
	protected PTASequenceEngine.SequenceSet initSet;

	protected boolean empty = true;
	
	public PTASequenceSet()
	{
		engine.init(new PTASequenceSetAutomaton());		
		initSet = engine.new SequenceSet();initSet.setIdentity();
	}
	
	/** Creates this set with a specific automaton implementing PTA. Important when
	 * such an automaton can be an odd one, such is the one with changing state or
	 * changing accept-condition.
	 * 
	 * @param automaton
	 */
	public PTASequenceSet(PTASequenceSetAutomaton automaton)
	{
		engine.init(automaton);		
		initSet = engine.new SequenceSet();initSet.setIdentity();
	}
	
	public boolean add(List<String> o) {
		initSet.crossWithSequence(o);empty = false;
		return true;
	}
	
	public boolean addAll(Collection<? extends List<String>> c) {
		if (empty && c.size() > 0)
			empty = false;
		initSet.cross((Collection<List<String>>)c);
		return true;
	}

	public void clear() {
		throw new UnsupportedOperationException();
	}

	public boolean contains(Object o) {
		return engine.containsSequence( (List<String>)o);
	}
	
	public boolean containsAsLeaf(Object o)
	{
		return engine.containsAsLeaf( (List<String>)o);
	}

	public boolean containsAll(Collection<?> c) {
		for(Object elem:c)
			if (!contains(elem)) return false;
		return true;
	}

	public boolean isEmpty() {
		return empty;
	}

	public Iterator<List<String>> iterator() {
		if (isEmpty())
			return new Iterator<List<String>>() {

				public boolean hasNext() {
					return false;
				}

				public List<String> next() {
					throw new UnsupportedOperationException();
				}

				public void remove() {
					throw new UnsupportedOperationException();
				}
			};
		
		return getData().iterator();
	}

	public boolean remove(@SuppressWarnings("unused") Object o) {
		throw new UnsupportedOperationException();
	}

	public boolean removeAll(@SuppressWarnings("unused") Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/** Fairly fast. */
	public int size() {
		if (empty) 
			return 0;
		return engine.numberOfLeafNodes();
	}
	
	public int treeSize() {
		if (empty) 
			return 0;
		return engine.treeSize();
	}

	public Object[] toArray() {
		throw new UnsupportedOperationException();
	}

	public <T> T[] toArray(@SuppressWarnings("unused") T[] a) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void addSequence(List<String> sequence) {
		add(sequence);
	}

	@Override
	public Collection<List<String>> getData() {
		if (isEmpty()) return Collections.emptySet();
		return engine.getData();
	}
	
	public Collection<List<String>> getData(FilterPredicate pred) {
		if (isEmpty()) return Collections.emptySet();
		return engine.getData(pred);
	}

	public PTASequenceEngine filter(final FilterPredicate predicate)
	{
		return engine.filter(predicate);
	}
}
