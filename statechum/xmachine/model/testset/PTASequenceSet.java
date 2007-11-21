package statechum.xmachine.model.testset;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class PTASequenceSet extends PrefixFreeCollection implements Set<List<String>>
{
	public static final String theOnlyState = "TheOnlyState";
	protected PTATestSequenceEngine engine = new PTATestSequenceEngine();
	protected PTATestSequenceEngine.sequenceSet initSet;

	protected boolean empty = true;
	
	public PTASequenceSet()
	{
		engine.init(new PTATestSequenceEngine.FSMAbstraction() {
			public Object getInitState() {
				return theOnlyState;
			}

			public Object getNextState(Object currentState, String input) {
				return theOnlyState;
			}

			public boolean isAccept(Object currentState) {
				return true;
			}

			public boolean shouldBeReturned(Object elem) {
				return true;
			}
			
		});
		
		initSet = engine.new sequenceSet();initSet.setIdentity();
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
		throw new UnsupportedOperationException();
	}

	public boolean containsAll(Collection<?> c) {
		throw new UnsupportedOperationException();
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
		else
			return getData().iterator();
	}

	public boolean remove(Object o) {
		throw new UnsupportedOperationException();
	}

	public boolean removeAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	public boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/** Extremely slow. */
	public int size() {
		if (empty) 
			return 0;
		return getData().size();
	}
	
	public int treeSize() {
		if (empty) 
			return 0;
		return engine.treeSize();
	}

	public Object[] toArray() {
		throw new UnsupportedOperationException();
	}

	public <T> T[] toArray(T[] a) {
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

}
