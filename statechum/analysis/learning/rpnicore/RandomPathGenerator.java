/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
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

package statechum.analysis.learning.rpnicore;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraDistance;

import java.util.*;
import java.util.Map.Entry;

import statechum.DeterministicDirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSet;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class RandomPathGenerator {
	
	protected LearnerGraph g;
	final int pathLength;
	final Random randomNumberGenerator;
	
	/** An array representation of the transition matrix of the graph, needed for fast computation of random walks. */
	private Map<CmpVertex,ArrayList<Entry<String,CmpVertex>>> transitions = new TreeMap<CmpVertex,ArrayList<Entry<String,CmpVertex>>>();
	/** For each state, stores inputs not accepted from it, needed for fast computation of random walks. */
	private Map<CmpVertex,ArrayList<String>> inputsRejected = new TreeMap<CmpVertex,ArrayList<String>>();
	
	/** The random number generator passed in is used to generate walks; one can pass a mock in order to 
	 * produce walks devised by a tester. Note that the object will be modified in the course of walks thanks
	 * to java's Random being non-serialisable.
	 *  
	 * @param baseGraph the graph to operate on
	 * @param random the random number generator.
	 * @param extra the length of paths will be diameter plus this value.
	 */ 
	public RandomPathGenerator(LearnerGraph graph, Random random, int extra) {
		g = graph;randomNumberGenerator = random;
		pathLength = diameter(g)+extra;
		
		transitions.clear();inputsRejected.clear();
		/** The alphabet of the graph. */
		Set<String> alphabet = g.pathroutines.computeAlphabet();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:graph.transitionMatrix.entrySet())
		{
			ArrayList<Entry<String,CmpVertex>> row = new ArrayList<Entry<String,CmpVertex>>();row.addAll(entry.getValue().entrySet());
			transitions.put(entry.getKey(), row);
			
			Set<String> negatives = new LinkedHashSet<String>();
			negatives.addAll(alphabet);negatives.removeAll(entry.getValue().keySet());
			ArrayList<String> rejects = new ArrayList<String>();rejects.addAll(negatives);
			inputsRejected.put(entry.getKey(), rejects);
		}
		initAllSequences();
	}
	
	public static int diameter(LearnerGraph graph)
	{// TODO: to rewrite using a flowgraph or not, given that this is only used once per experiment? 
		DirectedSparseGraph g = graph.pathroutines.getGraph();
		DijkstraDistance dd = new DijkstraDistance(g);
		Collection<Double> distances = dd.getDistanceMap(DeterministicDirectedSparseGraph.findInitial(g)).values();
		Double result =-1.;
		for(Double distance:distances) if (result<distance) result=distance;
		return result.intValue();
	}

	/** All new states will be given this %% and accept condition. Used to change names for states, 
	 * in order to ensure that tail states have special names 
	 * (i.e. %% of a full set of walk they belong to), 
	 * which can subsequently be used to filter the PTA.
	 */
	class StateName
	{
		final int percent;
		final boolean accept;
		
		public StateName(int per,boolean ac)
		{
			percent = per;accept=ac;
		}
	}
	
	StateName tag;
	
	class PercentLabelledPTA extends PTASequenceSetAutomaton
	{
		// The automaton to back a PTA tree containing all walks generated.
		@Override
		public Object getTheOnlyState() {
			return tag;
		}
		
		@Override
		public boolean shouldBeReturned(Object elem) {
			if (elem == null) // the reject-node of a PTA engine
				return false;
			return ((StateName)elem).accept;
		}
	}
	
	protected PTASequenceSet allSequences = null,
		extraSequences = null;
	
	/** Generates a random walk through the graph. Since it attempts to add elements to a graph
	 * in the course of checking whether they belong to it, it should be used to add
	 * elements longest-first (this is also important to prevent short paths blocking walks
	 * if an automaton has a narrow path leading to some parts of it). 
	 * A path is only generated if it is not contained in the <em>allSequences</em> collection.
	 *  
	 * @param walkLength the length of a walk. 
	 * @param prefixLen the length of the path to check for existence in a PTA. This is 
	 * useful for generation of negative paths such that for each such path,
	 * where a positive prefix is unique in a PTA. 
	 * @param positive whether to generate a positive walk.
	 * @param allSequences contains all sequences generated.
	 * @return the path computed. Throws IllegalArgument exception if arguments are wrong. 
	 * Returns null if the requested number of paths cannot be computed (because graph does not 
	 * have enough transitions).
	 */
	List<String> generateRandomWalk(int walkLength, int prefixLen, boolean positive)
	{
		if (walkLength < 1) 
			throw new IllegalArgumentException("cannot generate paths with length less than one");
		if (prefixLen < 1 || prefixLen > walkLength)
			throw new IllegalArgumentException("invalid prefix length");
		
		int generationAttempt = 0;
		List<String> path = new ArrayList<String>(walkLength);
		do
		{
			path.clear();
			CmpVertex current = g.init;

			int positiveLength = positive?walkLength:walkLength-1;
			if (positiveLength>0)
			{// if we are asked to generate negative paths of length 1, we cannot start with anything positive.
				for(int i=0;i<positiveLength;i++)
				{
					ArrayList<Entry<String,CmpVertex>> row = transitions.get(current);
					if(row.isEmpty())
						break;// cannot make a transition
					Entry<String,CmpVertex> inputState = row.get(randomNumberGenerator.nextInt(row.size()));
					path.add(inputState.getKey());current = inputState.getValue();
				}
			}
			
			if (path.size() == positiveLength && !positive)
			{// successfully generated a positive path of the requested length, append a negative transition.
				// In the situation where we'd like to generate both negatives and 
				// one element shorter positives, we'd have to copy our positive 
				// and then append a negative to the copy. It takes as long to take 
				// all negatives and make copy of all but one elements, given that 
				// they are ArrayLists. 
				ArrayList<String> rejects = inputsRejected.get(current);
				if (!rejects.isEmpty())
					path.add(rejects.get(randomNumberGenerator.nextInt(rejects.size())));
			}
			
			generationAttempt++;
				
			if (generationAttempt > g.config.getRandomPathAttemptThreshold())
				return null;
		}
		while(path.size() < walkLength || allSequences.contains(path.subList(0, prefixLen)));
		return path;
	}
	
	class PercentFilter implements FilterPredicate
	{
		private final int filter; 
		public PercentFilter(int percent)
		{
			filter=percent;
		}
		
		public boolean shouldBeReturned(Object name) {
			return ((StateName)name).percent <= filter;
		}
		
	}
	
	class PercentIntervalFilter implements FilterPredicate
	{
		private final int filter; 
		public PercentIntervalFilter(int percent)
		{
			filter=percent;
		}
		
		public boolean shouldBeReturned(Object name) {
			return ((StateName)name).percent == filter;
		}
		
	}
	
	/** Returns a PTA consisting of chunks number 0 .. upToChunk (inclusive).
	 * 
	 * @param upToChunk how many chunks to combine before returning the result.
	 * @return the result. 
	 */
	public PTASequenceEngine getAllSequencesPercentageInterval(int chunk)
	{
		if (chunk < 0 || chunk >= getChunkNumber())
			throw new IllegalArgumentException("chunk number "+chunk+" is out of range 0.."+getChunkNumber());
		
		return allSequences.filter(new PercentIntervalFilter(chunk));
	}
	
	/** Returns a PTA consisting of chunk.
	 * 
	 * @param chunk which chunk to return.
	 * @return the result. 
	 */
	public PTASequenceEngine getExtraSequencesPercentageInterval(int chunk)
	{
		if (chunk < 0 || chunk >= getChunkNumber())
			throw new IllegalArgumentException("chunk number "+chunk+" is out of range 0.."+getChunkNumber());
		
		return extraSequences.filter(new PercentIntervalFilter(chunk));
	}
	
	/** Returns a PTA consisting of chunks number 0 .. upToChunk (inclusive).
	 * 
	 * @param upToChunk how many chunks to combine before returning the result.
	 * @return the result. 
	 */
	public PTASequenceEngine getAllSequences(int upToChunk)
	{
		if (upToChunk < 0 || upToChunk >= getChunkNumber())
			throw new IllegalArgumentException("chunk number "+upToChunk+" is out of range 0.."+getChunkNumber());
		
		return allSequences.filter(new PercentFilter(upToChunk));
	}
	
	/** Returns a PTA consisting of chunks number 0 .. upToChunk (inclusive).
	 * 
	 * @param upToChunk how many chunks to combine before returning the result.
	 * @return the result. 
	 */
	public PTASequenceEngine getExtraSequences(int upToChunk)
	{
		if (upToChunk < 0 || upToChunk >= getChunkNumber())
			throw new IllegalArgumentException("chunk number "+upToChunk+" is out of range 0.."+getChunkNumber());
		
		return extraSequences.filter(new PercentFilter(upToChunk));
	}
	
	
	
	/** Generates positive and negative paths where negatives are just 
	 * positives with an extra element added at the end. 
	 * Data added is split into a number of parts, with a specific 
	 * number of sequences per chunk.
	 * 
	 * @param numberPerChunk number of sequences per chunk.
	 * @param chunks the number of chunks to generate.
	 * @param length the maximal length of paths, minimal is 1.
	 */
	public void generatePosNeg(int numberPerChunk, int chunks)
	{
		if (pathLength < 2)
			throw new IllegalArgumentException("Cannot generate paths with length of less than 2");
		if (numberPerChunk % 2 != 0)
			throw new IllegalArgumentException("Number of sequences per chunk must be even");
		chunksGenerated = 0;
		int seqNumber = chunks*numberPerChunk/2;
		int distribution [] = new int[seqNumber];
		RandomLengthGenerator rnd = new RandomLengthGenerator(){

			public int getLength() {
				return randomNumberGenerator.nextInt(pathLength-1)+2;// the shortest length is 2
			}

			public int getPrefixLength(int len) {
				return len-1;
			}
			
		};
		for(int i=0;i < seqNumber;++i)
			distribution[i]= rnd.getLength();
		Arrays.sort(distribution);
		//for(int i=0;i<distribution.length;++i)
		//	System.out.println("distribution[i] = "+distribution[i]);
		initAllSequences();
		StateName [] positives = new StateName[chunks], negatives = new StateName[chunks];
		for(int i=0;i< chunks;++i) { positives[i]=new StateName(i,true);negatives[i]=new StateName(i,false); }

		for(int i=seqNumber-1;i>=0;--i)
		{
			tag = negatives[i % chunks];
			//System.out.println("generating for chunk "+tag+" with length "+distribution[i]);
			List<String> path = generateRandomWalkWithFudge(distribution[i],rnd,false);
			allSequences.add(path);
			tag = positives[i % chunks];
			extraSequences.add(path.subList(0, rnd.getPrefixLength(path.size())));// all positives go there
		}
		chunksGenerated = chunks;
	}
	
	public int getChunkNumber()
	{
		return chunksGenerated;
	}
	
	protected int chunksGenerated = 0;
	
	interface RandomLengthGenerator 
	{
		public int getLength();
		public int getPrefixLength(int len);
	}

	/** Initialises the collection of data. Used to reset the whole thing before
	 * generating walks.
	 */
	protected void initAllSequences()
	{
		tag = new StateName(0,false);
		allSequences = new PTASequenceSet(new PercentLabelledPTA());extraSequences = new PTASequenceSet(new PercentLabelledPTA());
	}
	
	/** Generates random positive and negative paths. 
	 * Data added is split into a number of parts, with a specific number of sequences per chunk.
	 * 
	 * @param numberPerChunk number of sequences per chunk.
	 * @param chunks the number of chunks to generate.
	 */
	public void generateRandomPosNeg(int numberPerChunk, int chunks)
	{
		if (pathLength < 2)
			throw new IllegalArgumentException("Cannot generate paths with length of less than 2");
		if (numberPerChunk % 2 != 0)
			throw new IllegalArgumentException("Number of sequences per chunk must be even");
		chunksGenerated = 0;

		int seqNumber = chunks*numberPerChunk/2;
		int distribution [] = new int[seqNumber];
		RandomLengthGenerator rnd = new RandomLengthGenerator(){

			public int getLength() {
				return randomNumberGenerator.nextInt(pathLength)+1;
			}

			public int getPrefixLength(int len) {
				return len;
			}
			
		};
		
		for(int i=0;i < seqNumber;++i)
			distribution[i]=rnd.getLength();
		Arrays.sort(distribution);
		initAllSequences();

		StateName [] positives = new StateName[chunks], negatives = new StateName[chunks];
		for(int i=0;i< chunks;++i) { positives[i]=new StateName(i,true);negatives[i]=new StateName(i,false); }

		for(int i=seqNumber-1;i>=0;--i)
		{
			tag = negatives[i % chunks];
			allSequences.add(generateRandomWalkWithFudge(distribution[i],rnd,false));
			tag = positives[i % chunks];
			allSequences.add(generateRandomWalkWithFudge(distribution[i],rnd,true));
		}
		chunksGenerated = chunks;
	}
	
	/** Counts the number of times sequence length was revised during
	 * sequence generation. 0 means that the length was never revised. 
	 */
	private List<String> fudgeDetails = new LinkedList<String>();
	
	public List<String> getFudgeDetails()
	{
		return fudgeDetails;
	}
	
	/** Generates a walk, but if none can be produced for a given seq length, 
	 * attempts to randomly choose a different length.
	 */
	List<String> generateRandomWalkWithFudge(int origWalkLength, RandomLengthGenerator rnd,boolean positive)
	{
		List<String> path = generateRandomWalk(origWalkLength, rnd.getPrefixLength(origWalkLength), positive);
		if (path != null)
			return path;

		for(int i=1;i<g.config.getRandomPathAttemptFudgeThreshold();++i)
		{
			int revisedWalkLength = rnd.getLength();
			path = generateRandomWalk(revisedWalkLength, rnd.getPrefixLength(revisedWalkLength), positive);
			if (path != null)
			{
				boolean notPrefix = verifyNoPrefixOf(path, allSequences, rnd) &&
					verifyNoPrefixOf(path, extraSequences, rnd);

				fudgeDetails.add(origWalkLength+","+rnd.getPrefixLength(origWalkLength)+" "+(positive?"positive":"negative")+"->"+revisedWalkLength+","+rnd.getPrefixLength(revisedWalkLength)+
						" "+(notPrefix?"done":"ATTEMPT FAILED"));
				if (notPrefix)
					return path;
			}
		}
		
		throw new IllegalArgumentException("failed to generate a "+(positive?"positive":"negative")+
				" path of length "+origWalkLength+" (prefix length "+rnd.getPrefixLength(origWalkLength)+") after even after trying to fudge it "+
				g.config.getRandomPathAttemptFudgeThreshold()+" times");
	}
	
	/** Checks that the path supplied does not have a prefix currently in the collection supplied.
	 * 
	 * @param path path to check
	 * @param engine the collection
	 * @param rnd used to determine how long prefix to look for
	 * @return true if there is no prefix leading to a leaf in the collection.
	 */
	private static boolean verifyNoPrefixOf(List<String> path,PTASequenceSet engine,RandomLengthGenerator rnd)
	{
		boolean notPrefix = true;
		int pathPrefixLen=rnd.getPrefixLength(path.size())-1;
		for(;
			pathPrefixLen>0 && !engine.contains(path.subList(0, pathPrefixLen));
			--pathPrefixLen);
		if (pathPrefixLen > 0)
		{// there is a sequence path.subList(0, pathPrefixLen) in our PTA, check that
		 // the end of it is not a tail node.
			notPrefix = !engine.containsAsLeaf(path.subList(0, pathPrefixLen));
		}
		
		return notPrefix;
	}
}
