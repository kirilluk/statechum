package statechum.xmachine.model.testset;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

/** Expects sequences of (mostly) increasing length to be added. */ 
public class HashBucketPrefixFreeCollection extends PrefixFreeCollection {
	protected static class Bucket {
		Map<List<String>,Integer> data = new LinkedHashMap<List<String>,Integer>();
		Set<Integer> bucketHashes = new HashSet<Integer>();
		
		/** Given a sequence of strings, this one goes through the bucket and eliminates those sequences
		 * in the bucket which are prefixes of the list supplied. Returns true if the list supplied
		 * is a prefix of an existing sequnce in the bucket, false otherwise.
		 * 
		 * @param seq
		 * @return
		 */
		protected boolean removeExistingPrefixesSlowly(List<String> seq) {
			List<List<String>> seqToRemove = new LinkedList<List<String>>();
			for(Entry<List<String>,Integer> s:data.entrySet())
			{
				if (isPrefix(s.getKey(), seq))
				{
					assert(seqToRemove.isEmpty());// no sequences are prefixes of the current one - an internal consistency invariant of this procedure.
					return true; // the sequence to add is already a prefix of another one
				}
				if (isPrefix(seq, s.getKey()))
					seqToRemove.add(s.getKey());
			}	
			
			for(List<String> whatToRemove:seqToRemove)
				data.remove(whatToRemove);
			
			bucketHashes.clear();
			for(Entry<List<String>,Integer> s:data.entrySet())
				bucketHashes.add(s.getValue()); // since multiple sequences may have the same hashes, we have to rebuild the whole table
			return false;
		}
		
		protected void addSequence(List<String> seq)
		{
			for(Entry<List<String>,Integer> s:data.entrySet())
			{
				if (isPrefix(s.getKey(), seq))
					return ; // the sequence to add is already a prefix of another one
			}	
			int hash = seq.hashCode();
			data.put(seq,hash);
			bucketHashes.add(hash);
		}
		
		/** Intelligently removes all prefixes of a given sequence from the bucket and returns true if this
		 * sequence is a prefix of some sequence in the bucket, false otherwise. 
		 * 
		 * @param prefixHashes the sequence of hash codes of all prefixes of the supplied list.
		 * @param seq
		 * @return
		 */ 
		public boolean removeExistingPrefixes(List<Integer> prefixHashes,List<String> seq)
		{
			Iterator<Integer> prefixHashIt = prefixHashes.iterator();
			boolean hashMatched = false;
			while(prefixHashIt.hasNext() && !hashMatched)
				if (bucketHashes.contains(prefixHashIt.next()))
					hashMatched = true;
			
			if (!hashMatched)
				return false;
			else
				return removeExistingPrefixesSlowly(seq);
		}
		
		public Collection<List<String>> getData()
		{
			return data.keySet();
		}
	}
	
	private Bucket data = new Bucket();// right now, only one bucket
	
	@Override
	public void addSequence(List<String> seq) {
		List<Integer> hashesOfPrefixes = null;
		hashesOfPrefixes = new ArrayList<Integer>(seq.size()+1);
		for(int i=0;i<=seq.size();++i)
			hashesOfPrefixes.add(new Integer(seq.subList(0, i).hashCode()));
				
		if (!data.removeExistingPrefixes(hashesOfPrefixes, seq))
			data.addSequence(seq);
	}

	@Override
	public Collection<List<String>> getData() {
		return data.getData();
	}
}
