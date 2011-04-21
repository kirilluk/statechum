/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */

package statechum.model.testset;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import statechum.Label;

import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class PTASequenceSet extends PrefixFreeCollection implements Set<List<Label>>
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
	
	@Override
	public boolean add(List<Label> o) {
		initSet.crossWithSequence(o);empty = false;
		return true;
	}
	
	@Override
	public boolean addAll(Collection<? extends List<Label>> c) {
		if (empty && c.size() > 0)
			empty = false;
		initSet.cross((Collection<List<Label>>)c);
		return true;
	}

	@Override
	public void clear() {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean contains(Object o) {
		return engine.containsSequence( (List<Label>)o);
	}
	
	public boolean containsAsLeaf(Object o)
	{
		return engine.containsAsLeaf( (List<Label>)o);
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		for(Object elem:c)
			if (!contains(elem)) return false;
		return true;
	}

	@Override
	public boolean isEmpty() {
		return empty;
	}

	@Override
	public Iterator<List<Label>> iterator() {
		if (isEmpty())
			return new Iterator<List<Label>>() {

				@Override
				public boolean hasNext() {
					return false;
				}

				@Override
				public List<Label> next() {
					throw new UnsupportedOperationException();
				}

				@Override
				public void remove() {
					throw new UnsupportedOperationException();
				}
			};
		
		return getData().iterator();
	}

	@Override
	public boolean remove(@SuppressWarnings("unused") Object o) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean removeAll(@SuppressWarnings("unused") Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean retainAll(@SuppressWarnings("unused") Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/** Fairly fast. */
	@Override
	public int size() {
		if (empty) 
			return 0;
		return engine.numberOfLeafNodes();
	}
	
	@Override
	public Object[] toArray() {
		throw new UnsupportedOperationException();
	}

	@Override
	public <T> T[] toArray(@SuppressWarnings("unused") T[] a) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void addSequence(List<Label> sequence) {
		add(sequence);
	}

	@Override
	public Collection<List<Label>> getData() {
		if (isEmpty()) return Collections.emptySet();
		return engine.getData();
	}
	
	public Collection<List<Label>> getData(FilterPredicate pred) {
		if (isEmpty()) return Collections.emptySet();
		return engine.getData(pred);
	}

	/** Given a predicate, filters out a subset of the elements in this collection. 
	 * Where all elements need to be returned, it is more efficient to use <em>getEngine()</em> instead.
	 * 
	 * @param predicate filtering predicate
	 * @return a new collection
	 */
	public PTASequenceEngine filter(final FilterPredicate predicate)
	{
		return engine.filter(predicate);
	}

	/** Returns the engine underlying this set - vitally important if used in 
	 * computations where a compact representation of a collection of sequences is needed. 
	 */
	public PTASequenceEngine getEngine()
	{
		return engine;
	}

	public boolean extendsLeaf(List<Label> path) {
		return engine.extendsLeaf(path);
	}
}
