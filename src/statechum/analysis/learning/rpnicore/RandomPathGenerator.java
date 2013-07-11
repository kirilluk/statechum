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
import statechum.Label;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSet;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class RandomPathGenerator {
	
	final protected LearnerGraph g;
	final CmpVertex initialState;
	int pathLength;
	
	final Random randomNumberGenerator;
	
	/** An array representation of the transition matrix of the graph, needed for fast computation of random walks. */
	private Map<CmpVertex,ArrayList<Entry<Label,CmpVertex>>> transitions = new TreeMap<CmpVertex,ArrayList<Entry<Label,CmpVertex>>>();
	/** For each state, stores inputs not accepted from it, needed for fast computation of random walks. */
	private Map<CmpVertex,ArrayList<Label>> inputsRejected = new TreeMap<CmpVertex,ArrayList<Label>>();
	
	/** The random number generator passed in is used to generate walks; one can pass a mock in order to 
	 * produce walks devised by a tester. Note that the object will be modified in the course of walks thanks
	 * to java's Random being non-serialisable.
	 *  
	 * @param baseGraph the graph to operate on
	 * @param random the random number generator.
	 * @param extra the length of paths will be diameter plus this value.
	 * @param initial the state to start the traversal with. null means use the initial state of <em>graph</em>.
	 * @param alphabet in some cases we would want to generate walks using a different alphabet than the one visible in a graph. 
	 * For instance, if we take a single state of a graph, only a portion of the whole 
	 * graph will be accessible and this will limit visible alphabet while the alphabet for the whole has to be used.
	 * Alphabet obtained from the supplied graph if <em>null</em>.
	 */ 
	public RandomPathGenerator(LearnerGraph graph, Random random, int extra, CmpVertex initial) 
	{
		this(graph,random,extra,initial,null);
	}
	
	/** The random number generator passed in is used to generate walks; one can pass a mock in order to 
	 * produce walks devised by a tester. Note that the object will be modified in the course of walks thanks
	 * to java's Random being non-serialisable.
	 *  
	 * @param baseGraph the graph to operate on
	 * @param random the random number generator.
	 * @param extra the length of paths will be diameter plus this value.
	 * @param initial the state to start the traversal with. null means use the initial state of <em>graph</em>.
	 * @param alphabet in some cases we would want to generate walks using a different alphabet than the one visible in a graph. 
	 * For instance, if we take a single state of a graph, only a portion of the whole 
	 * graph will be accessible and this will limit visible alphabet while the alphabet for the whole has to be used.
	 * Alphabet obtained from the supplied graph if <em>null</em>.
	 */ 
	public RandomPathGenerator(LearnerGraph graph, Random random, int extra, CmpVertex initial,Set<Label> alphabetArg)
	{
		g = graph;randomNumberGenerator = random;
		if (initial != null) 
		{ 
			g.verifyVertexInGraph(initial);initialState=initial; 
		}
		else initialState=g.getInit();
		pathLength = diameter(g)+extra;
		
		transitions.clear();inputsRejected.clear();
		/** The alphabet of the graph. */
		Set<Label> alphabet = null;
		if (alphabetArg == null)
			alphabet = g.pathroutines.computeAlphabet();
		else
		{// check that the supplied actual alphabet includes the one in the graph 
			Set<Label> origAlphabet = g.pathroutines.computeAlphabet();
			origAlphabet.removeAll(alphabetArg);if(!origAlphabet.isEmpty()) throw new IllegalArgumentException("the supplied alphabet does not include the one of the graph, "+origAlphabet+" elements are new to the graph");
			alphabet=alphabetArg;
		}
 		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
		{
			ArrayList<Entry<Label,CmpVertex>> row = new ArrayList<Entry<Label,CmpVertex>>();row.addAll(entry.getValue().entrySet());
			transitions.put(entry.getKey(), row);
			
			Set<Label> negatives = new LinkedHashSet<Label>();
			negatives.addAll(alphabet);negatives.removeAll(entry.getValue().keySet());
			ArrayList<Label> rejects = new ArrayList<Label>();rejects.addAll(negatives);
			inputsRejected.put(entry.getKey(), rejects);
		}
		initAllSequences();
	}
	
	/** Returns the maximal length of sequences that will be generated by default (that is, without a user-defined {@link RandomLengthGenerator}. */
	public int getPathLength()
	{
		return pathLength;
	}
	
	public void setPathLength(int value)
	{
		if (value <= 0) throw new IllegalArgumentException("path length has to be positive");
		pathLength = value;
	}
	
	public static int diameter(LearnerGraph graph)
	{// Decided not to rewrite using a flowgraph or not, given that this is only used once per experiment. 
		DirectedSparseGraph g = graph.pathroutines.getGraph();
		DijkstraDistance dd = new DijkstraDistance(g);
		@SuppressWarnings("unchecked")
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
	public class StateName
	{
		public final int percent;
		public final boolean accept;
		
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
	
	protected PTASequenceSet allSequences = null,extraSequences = null;
	
	/** Generates a random walk through the graph. Since it attempts to add elements to a graph
	 * in the course of checking whether they belong to it, it should be used to add
	 * elements longest-first (this is also important to prevent short paths blocking walks
	 * if an automaton has a narrow path leading to some parts of it). 
	 * A path is only generated if it is not contained in the <em>allSequences</em> collection.
	 *  
	 * @param walkLength the length of a walk. 
	 * @param prefixLen the length of the path to check for existence in a PTA. This is 
	 * useful for generation of negative paths such that for each such path,
	 * a positive prefix of it is unique in a PTA. 
	 * @param positive whether to generate a positive walk.
	 * The member collection <em>allSequences</em> contains all sequences generated so far.
	 * @return the path computed. Throws IllegalArgument exception if arguments are wrong. 
	 * Returns null if the requested number of paths cannot be computed (because graph does not 
	 * have enough transitions).
	 */
	List<Label> generateRandomWalk(int walkLength, int prefixLen, boolean positive)
	{
		if (walkLength < 1) 
			throw new IllegalArgumentException("cannot generate paths with length less than one");
		if (prefixLen < 0 || prefixLen > walkLength)
			throw new IllegalArgumentException("invalid prefix length");
		
		int generationAttempt = 0;
		List<Label> path = new ArrayList<Label>(walkLength);
		do
		{
			path.clear();
			CmpVertex current = initialState;

			int positiveLength = positive?walkLength:walkLength-1;
			if (positiveLength>0)
			{// if we are asked to generate negative paths of length 1, we cannot start with anything positive.
				for(int i=0;i<positiveLength;i++)
				{
					ArrayList<Entry<Label,CmpVertex>> row = transitions.get(current);
					if(row.isEmpty())
						break;// cannot make a transition
					Entry<Label,CmpVertex> inputState = row.get(randomNumberGenerator.nextInt(row.size()));
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
				ArrayList<Label> rejects = inputsRejected.get(current);
				if (!rejects.isEmpty())
					path.add(rejects.get(randomNumberGenerator.nextInt(rejects.size())));
			}
			
			generationAttempt++;
				
			if (generationAttempt > g.config.getRandomPathAttemptThreshold())
				return null;
		}
		while(prefixLen > 0 && (path.size() < walkLength || allSequences.contains(path.subList(0, prefixLen))));
		return path;
	}
	
	class PercentFilter implements FilterPredicate
	{
		private final int filter; 
		public PercentFilter(int percent)
		{
			filter=percent;
		}
		
		@Override
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
		
		@Override
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
	 * If the requested length of sequences is 1, no positives are generated.
	 *  
	 * Data added is split into a number of parts, with a specific 
	 * number of sequences per chunk.
	 * 
	 * @param numberPerChunk number of sequences per chunk.
	 * @param chunks the number of chunks to generate.
	 * @param length the maximal length of paths, minimal is 1.
	 */
	public void generatePosNeg(int numberPerChunk, int chunks)
	{
		if (pathLength < 1)
			throw new IllegalArgumentException("Cannot generate paths with length less than 1");
		if (numberPerChunk % 2 != 0)
			throw new IllegalArgumentException("Number of sequences per chunk must be even");
		chunksGenerated = 0;
		int seqNumber = chunks*numberPerChunk/2;
		int distribution [] = new int[seqNumber];
		RandomLengthGenerator rnd = new RandomLengthGenerator(){

			@Override
			public int getLength() {
				if (pathLength > 1)
					return randomNumberGenerator.nextInt(pathLength-1)+2;// the shortest length is 2
				return 1;
			}

			@Override
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
			List<Label> path = generateRandomWalkWithFudge(distribution[i],rnd,false);
			if (path == null)
				throw new IllegalArgumentException("failed to generate a negative"+
						" path of length "+distribution[i]+" (prefix length "+rnd.getPrefixLength(distribution[i])+") after even after trying to fudge it "+
						g.config.getRandomPathAttemptFudgeThreshold()+" times");
			allSequences.add(path);
			tag = positives[i % chunks];
			int prefixLength = rnd.getPrefixLength(path.size());
			if (prefixLength > 0)
				extraSequences.add(path.subList(0, rnd.getPrefixLength(path.size())));// all positives go there
		}
		chunksGenerated = chunks;
	}
	
	public int getChunkNumber()
	{
		return chunksGenerated;
	}
	
	protected int chunksGenerated = 0;
	
	public interface RandomLengthGenerator 
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
		generateRandomPosNeg(numberPerChunk,chunks,true);
	}
	
	/** Generates random positive and negative paths. 
	 * Data added is split into a number of parts, with a specific number of sequences per chunk.
	 * 
	 * @param numberPerChunk number of sequences per chunk.
	 * @param chunks the number of chunks to generate.
	 * @param exceptionOnFailure whether to throw an exception if paths cannot be generated.  
	 */
	public void generateRandomPosNeg(int numberPerChunk, int chunks, boolean exceptionOnFailure)
	{
		generateRandomPosNeg(numberPerChunk,chunks,exceptionOnFailure,null);
	}

	/** Generates random positive and negative paths. 
	 * Data added is split into a number of parts, with a specific number of sequences per chunk.
	 * 
	 * @param numberPerChunk number of sequences per chunk.
	 * @param chunks the number of chunks to generate.
	 * @param exceptionOnFailure whether to throw an exception if paths cannot be generated.  
	 */
	public void generateRandomPosNeg(int numberPerChunk, int chunks, boolean exceptionOnFailure, RandomLengthGenerator rnd)
	{
		generateRandomPosNeg(numberPerChunk, chunks, exceptionOnFailure, rnd, true, true, null);
	}
	
	/** Generates random positive and negative paths. 
	 * Data added is split into a number of parts, with a specific number of sequences per chunk.
	 * 
	 * @param numberPerChunk number of sequences per chunk.
	 * @param chunks the number of chunks to generate.
	 * @param exceptionOnFailure whether to throw an exception if paths cannot be generated.  
	 * @param attemptPositive whether to attempt to generate positive sequences. If false, positives are not generated (but the number of negatives if requested is still numberPerChunk/2).
	 * @param attemptNegative whether to attempt to generate positive sequences. If false, negatives are not generated (but the number of positives if requested is still numberPerChunk/2).
	 * @param initialSet if non-null, the collection to initialise our sequences with. Useful for the purpose of augmenting an existing set with new sequences that are supposed to be different from the existing ones.
	 */
	public void generateRandomPosNeg(int numberPerChunk, int chunks, boolean exceptionOnFailure, RandomLengthGenerator argRnd, boolean attemptPositive, boolean attemptNegative, Collection<List<Label>> initialSet)
	{
		if (pathLength < 1)
			throw new IllegalArgumentException("Cannot generate paths with length of less than 1");
		if (numberPerChunk % 2 != 0)
			throw new IllegalArgumentException("Number of sequences per chunk must be even");
		chunksGenerated = 0;

		int seqNumber = chunks*numberPerChunk/2;
		int distribution [] = new int[seqNumber];
		
		RandomLengthGenerator rnd = argRnd;
		if (rnd == null) rnd = new RandomLengthGenerator(){

			@Override
			public int getLength() {
				return randomNumberGenerator.nextInt(pathLength)+1;
			}

			@Override
			public int getPrefixLength(int len) {
				return len;
			}
			
		};

		for(int i=0;i < seqNumber;++i)
			distribution[i]=rnd.getLength();
		Arrays.sort(distribution);
		initAllSequences();
		if (initialSet != null) for (List<Label> seq:initialSet) allSequences.add(seq);

		StateName [] positives = new StateName[chunks], negatives = new StateName[chunks];
		for(int i=0;i< chunks;++i) { positives[i]=new StateName(i,true);negatives[i]=new StateName(i,false); }

		for(int i=seqNumber-1;i>=0;--i)
		{
			tag = negatives[i % chunks];
			List<Label> path = null;
			
			if (attemptNegative)
			{
				path = generateRandomWalkWithFudge(distribution[i],rnd,false);
				if (path != null)
					allSequences.add(path);
				else
					if (exceptionOnFailure)
						throw new IllegalArgumentException("failed to generate a negative"+
							" path of length "+distribution[i]+" (prefix length "+rnd.getPrefixLength(distribution[i])+") after even after trying to fudge it "+
							g.config.getRandomPathAttemptFudgeThreshold()+" times");
			}
			tag = positives[i % chunks];
			
			if (attemptPositive)
			{
				path=generateRandomWalkWithFudge(distribution[i],rnd,true);
				if (path != null)
					allSequences.add(path);
				else
					if (exceptionOnFailure)
						throw new IllegalArgumentException("failed to generate a positive"+
							" path of length "+distribution[i]+" (prefix length "+rnd.getPrefixLength(distribution[i])+") after even after trying to fudge it "+
							g.config.getRandomPathAttemptFudgeThreshold()+" times");
			}
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
	
	/** Generates a walk, but if none can be produced for a given sequence length, 
	 * attempts to randomly choose a different length.
	 */
	List<Label> generateRandomWalkWithFudge(int origWalkLength, RandomLengthGenerator rnd,boolean positive)
	{
		List<Label> path = generateRandomWalk(origWalkLength, rnd.getPrefixLength(origWalkLength), positive);
		if (path != null)
		{
			assert !allSequences.containsAsLeaf(path);assert !extraSequences.containsAsLeaf(path);
			return path;
		}
		for(int i=1;i<g.config.getRandomPathAttemptFudgeThreshold();++i)
		{
			int revisedWalkLength = rnd.getLength();
			path = generateRandomWalk(revisedWalkLength, rnd.getPrefixLength(revisedWalkLength), positive);
			if (path != null)
			{
				boolean notPrefix = !allSequences.extendsLeaf(path) && !extraSequences.extendsLeaf(path);
				fudgeDetails.add(origWalkLength+","+rnd.getPrefixLength(origWalkLength)+" "+(positive?"positive":"negative")+"->"+revisedWalkLength+","+rnd.getPrefixLength(revisedWalkLength)+
						" "+(notPrefix?"done":"ATTEMPT FAILED"));
				if (notPrefix)
					return path;
			}
		}
		return null;
	}
	
}
