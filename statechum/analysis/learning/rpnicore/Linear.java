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

package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.Visualiser.VIZ_PROPERTIES;
import cern.colt.bitvector.BitVector;
import cern.colt.function.IntComparator;
import cern.colt.list.DoubleArrayList;
import cern.colt.list.IntArrayList;

public class Linear {
	final LearnerGraph coregraph;
	
	/** Associates this object to LinearGraph it is using for data to operate on. 
	 * Important: the constructor should not access any data in LinearGraph 
	 * because it is usually invoked during the construction phase of LinearGraph 
	 * when no data is yet available.
	 */
	Linear(LearnerGraph g)
	{
		coregraph =g;
	}

	public interface HandleRow 
	{
		/** Initialises this job. 
		 * @throws IllegalAccessException 
		 * @throws InstantiationException */
		public void init(int threadNo) throws InstantiationException, IllegalAccessException;
		
		/** Called for each row of our transition matrix. */
		public void handleEntry(Entry<CmpVertex,Map<String,CmpVertex>> entry, int threadNo);
	}
	
	protected class Job implements Callable<Integer>
	{
		private final int[]workLoad;
		private final int threadNo;
		private final HandleRow handler;
		
		public Job(final int[]wLoad,int thNo,final HandleRow h)
		{
			workLoad = wLoad;threadNo = thNo;handler=h;
		}
		
		public Integer call() throws Exception {
			if (workLoad[threadNo] >= workLoad[threadNo+1])
				;//System.out.println("thread "+threadNo+" nothing to do");
			else
			{
				handler.init(threadNo);
				int currentRow = 0;
				Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> stateB_It = coregraph.transitionMatrix.entrySet().iterator();
				while(stateB_It.hasNext() && currentRow < workLoad[threadNo])
				{
					Entry<CmpVertex,Map<String,CmpVertex>> entry = stateB_It.next();
					if (entry.getKey().isAccept()) ++currentRow;// only increment the row number if we are at the accept-state
				}
				//System.out.println("thread "+threadNo+" started from row "+currentRow);
				while(stateB_It.hasNext() && currentRow < workLoad[threadNo+1])
				{
					Entry<CmpVertex,Map<String,CmpVertex>> stateB = stateB_It.next();
					if (stateB.getKey().isAccept()) ++currentRow;// only increment the row number if we are at the accept-state. We still run our worker on all the reject-rows, 
						// but we expect it to be fast on those rows, so no problems.
					handler.handleEntry(stateB, threadNo);
				}
				//System.out.println("thread "+threadNo+" finished at row "+currentRow);
			}
			return 0;
	}}
	
	/** Runs the supplied handler on all the rows in our matrix, using the specified number of threads.
	 * 
	 * @param handlerList A list of handlers. Each instance of this class is associated with a collection of rows and the appropriate method is called for each row.
	 * The reason we are not using a single instance is to make it possible for different handlers to have instance variables,
	 * i.e. variables shared between different handlers (and hence different threads).
	 * 
	 * @param ThreadNumber the number of threads to create. If this is one, no 
	 * <em>ExecutorService</em> is created and the handler is called directly.
	  */
	protected void performRowTasks(List<? extends HandleRow> handlerList, int ThreadNumber)
	{
		final int[]workLoad = partitionWorkLoad(ThreadNumber);
		
		/** The runner of computational threads. */
		ExecutorService executorService = null;
		try
		{
			if (ThreadNumber > 1)
			{// Run multi-threaded
				executorService = Executors.newFixedThreadPool(ThreadNumber);

				/** Stores tasks to complete. */
				CompletionService<Integer> runner = new ExecutorCompletionService<Integer>(executorService);
				
				for(int count=0;count < ThreadNumber;++count) 
					runner.submit(new Job(workLoad,count,handlerList.get(count)));
			
				for(int count=0;count < ThreadNumber;++count)
					runner.take().get();// this will throw an exception if any of the tasks failed.
			}
			else
				// Run single-threaded.
				new Job(workLoad,0,handlerList.get(0)).call();
		}
		catch(Exception ex)
		{
			IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);throw e;
		}
		finally
		{
			if (executorService != null) executorService.shutdown();
		}
	}

	/** Processing of data for a triangular matrix using multiple CPUs has to be done by
	 * identifying subsets of rows and columns to handle.
	 * 
	 * @param ThreadNumber number of threads to parallelise for.
	 * @return the row to start from for each thread
	 */
	public int [] partitionWorkLoad(int ThreadNumber)
	{
		// The idea is that if a set of rows for some processor contains d rows
		// and starts at row a, then the job has the time complexity
		// of [(a+d)*(a+d+1) - a*(a+1)]/2. 
		// We'd like to allocate a n(n+1)/(2*ThreadNumber) to each CPU.
		// Solving quadratic equation, the result is
		// d = ( -2*a-1 + sqrt((2*a+1)*(2*a+1)+4*n*(n+1)/ThreadNumber) ) /2
		// We can hence iteratively compute different values of a.
		if (ThreadNumber <= 0) throw new IllegalArgumentException("invalid processor number");
		int result []= new int[ThreadNumber+1];
		result[0]=0;
		int acceptStateNumber = coregraph.learnerCache.getAcceptStateNumber();
		
		for(int count=1;count < ThreadNumber;++count)
		{
			int a= result[count-1];
			result[count]=a+(int)Math.round(( -2*a-1 + Math.sqrt((double)(2*a+1)*(2*a+1)+
					4*(double)acceptStateNumber*(acceptStateNumber+1)/ThreadNumber) ) /2);
			assert result[count] >= 0 && result[count] <= acceptStateNumber : "obtained row "+result[count]+" while the range is 0.."+acceptStateNumber;
		}
		result[ThreadNumber]=coregraph.getStateNumber();
		return result;
	}
	
	/** Makes it possible to use pluggable objects to determine default values of a diagonal and
	 * the right-hand side (aka b in Ax=b).
	 * 
	 */
	abstract static class DetermineDiagonalAndRightHandSide
	{
		/** The number of shared transitions which lead to states with the same <em>isHightlight()</em> value
		 * (this can be used to interpret an arbitrary subset of states as pseudo-reject ones; 
		 * in practice useful when I make <em>isHightlight() = !isAccept()</em>).
		 */ 
		int sharedSameHightlight = 0;
		
		/** Number of shared transitions which lead to accept states. */
		int sharedOutgoing = 0;
		
		/** The total number of outgoing transitions. */
		int totalOutgoing = 0;
		
		/** Counts the number of outgoing transitions from a pair of states with the same label.
		 * Hence if A has a,b,c outgoing and B has b,c,e,f, the outcome will be 2 because 
		 * there are two transitions, b,c in common.
		 */
		protected void compute(Map<String,CmpVertex> A, Map<String,CmpVertex> B)
		{
			sharedSameHightlight = 0;sharedOutgoing = 0;
			int totalShared = 0;
			
			for(Entry<String,CmpVertex> entry:A.entrySet())
			{
				CmpVertex to=B.get(entry.getKey());
				if (to != null)
				{
					++totalShared;
					if (entry.getValue().isAccept() && to.isAccept()) 
					{
						++sharedOutgoing;
						if (entry.getValue().isHighlight() == to.isHighlight()) ++sharedSameHightlight;
					}
				}
			}
			
			totalOutgoing = A.size()+B.size()-totalShared;
		}
		
		/** Returns the diagonal value (before it is reduced by
		 *  <em>coregraph.config.getAttenuationK()</em> for every single-transition loop).
		 *  If value returned is zero, it is assumed to be one. 
		 */
		abstract public int getDiagonal();
		
		/** Returns a value for the right-hand side. */
		abstract public int getRightHandSide();
	}
	
	/** Interprets highlighted states as reject ones in the course of computation of a diagonal;
	 * where two states are incompatible for a particular matched transition,
	 * the score they get is zero for it. 
	 */
	public static class DDRH_highlight extends DetermineDiagonalAndRightHandSide
	{

		@Override
		public int getDiagonal() {
			return sharedSameHightlight;
		}

		@Override
		public int getRightHandSide() {
			return sharedSameHightlight; 
		}
		
	}
	
	/** Interprets highlighted states as reject ones in the course of computation of a diagonal.
	 * where two states are incompatible for a particular matched transition,
	 * the score they get is -1 for it. 
	 */
	public static class DDRH_highlight_Neg extends DetermineDiagonalAndRightHandSide
	{

		@Override
		public int getDiagonal() {
			return sharedOutgoing;
		}

		@Override
		public int getRightHandSide() {
			return sharedSameHightlight-(sharedOutgoing-sharedSameHightlight); 
		}
		
	}
	
	public static class DDRH_default extends DetermineDiagonalAndRightHandSide
	{

		@Override
		public int getDiagonal() {
			return totalOutgoing;
		}

		@Override
		public int getRightHandSide() {
			return sharedOutgoing;
		}
		
	}
	
	/** Highlights all negative states and make all the normal vertices accept-vertices. */
	public void moveRejectToHightlight()
	{
		for(CmpVertex v:coregraph.transitionMatrix.keySet())
		{
			v.setHighlight(!v.isAccept());v.setAccept(true);
		}
	}
	
	/** Used to designate incompatible pairs of states. */
	public static final int PAIR_INCOMPATIBLE =-1;
	
	/** A temporary designation for compatible pairs of states, before they are numbered. */
	public static final int PAIR_OK=-2;

	Map<CmpVertex,BitVector> inputsAccepted = null,inputsRejected = null;

	/** A number of pairs of states will not be compatible, hence we do not need to include 
	 * them in a matrix for computation of compatibility scores. This method updates the set of  
	 * incompatible pairs of states.
	 *  
	 * @param incompatiblePairs pairs currently considered incompatible
	 * @param ThreadNumber number of CPUs to use.
	 * @return
	 */
	protected void findDirectlyIncompatiblePairs(int ThreadNumber)
	{
		inputsAccepted = new TreeMap<CmpVertex,BitVector>();inputsRejected = new TreeMap<CmpVertex,BitVector>();
		int num =0;
		Map<String,Integer> inputToInt = new TreeMap<String,Integer>();for(String str:coregraph.learnerCache.getAlphabet()) inputToInt.put(str, num++);
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
			if (entry.getKey().isAccept())
			{
				BitVector 
					acceptVector = new BitVector(coregraph.learnerCache.getAlphabet().size()),
					rejectVector = new BitVector(coregraph.learnerCache.getAlphabet().size());
				for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
					if (!transition.getValue().isAccept())
						rejectVector.set(inputToInt.get(transition.getKey()));
					else
						acceptVector.set(inputToInt.get(transition.getKey()));
				
				inputsAccepted.put(entry.getKey(), acceptVector);
				inputsRejected.put(entry.getKey(), rejectVector);
			}
	}
	
	/** Checks the supplied bit-vectors for an intersection.
	 * 
	 * @param A
	 * @param B
	 * @return true if there is any bit in common between these two bit vectors.
	 */
	public static boolean intersects(BitVector A, BitVector B)
	{
		long [] bufA=A.elements(),bufB=B.elements();
		assert bufA.length == bufB.length;
		for(int i=0;i<bufA.length;++i) 
			if ( (bufA[i] & bufB[i]) != 0)
				return true;
		
		return false;
	}

	/** A number of pairs of states will not be compatible, hence we do not need to include 
	 * them in a matrix for computation of compatibility scores. This method updates the set of  
	 * incompatible pairs of states.
	 *  
	 * @param incompatiblePairs pairs currently considered incompatible
	 * @param ThreadNumber number of CPUs to use.
	 * @return
	 */
	int findIncompatiblePairs(final int [] incompatiblePairs, int ThreadNumber)
	{
		final int pairsNumber = coregraph.learnerCache.getAcceptStateNumber()*(coregraph.learnerCache.getAcceptStateNumber()+1)/2;

		if (incompatiblePairs.length != pairsNumber) throw new IllegalArgumentException("invalid array length");
		findDirectlyIncompatiblePairs(ThreadNumber);

		final Queue<StatePair> currentExplorationBoundary = new LinkedList<StatePair>();// FIFO queue containing pairs to be explored
		
		List<HandleRow> handlerList = new LinkedList<HandleRow>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
		handlerList.add(new HandleRow()
		{
			public void init(@SuppressWarnings("unused") int threadNo) {}

			Set<Integer> sourceData = new TreeSet<Integer>();

			/** Used to detect non-consecutive state pair numbers - in this case an internal error should be reported. */
			int prevStatePairNumber =-1;

			public void handleEntry(Entry<CmpVertex, Map<String, CmpVertex>> entryA, @SuppressWarnings("unused") int threadNo) 
			{
				if (entryA.getKey().isAccept())
				{
					Collection<Entry<String,List<CmpVertex>>> rowA_collection = coregraph.learnerCache.getSortaInverse().get(entryA.getKey()).entrySet();
					BitVector inputsAcceptedFromA = inputsAccepted.get(entryA.getKey()), inputsRejectedFromA = inputsRejected.get(entryA.getKey());
					
					// Now iterate through states
					Iterator<Entry<CmpVertex,Map<String,List<CmpVertex>>>> stateB_It = coregraph.learnerCache.getSortaInverse().entrySet().iterator();
					while(stateB_It.hasNext())
					{
						Entry<CmpVertex,Map<String,List<CmpVertex>>> stateB = stateB_It.next();// stateB is an accept one by construction of getSortaInverse()
						int currentStatePair = coregraph.wmethod.vertexToIntNR(stateB.getKey(),entryA.getKey());
						assert prevStatePairNumber < 0 || currentStatePair == prevStatePairNumber+1;prevStatePairNumber=currentStatePair;
						
						// Note that we are iterating state pairs consecutively in an increasing order and 
						// different threads handle non-intersecting ranges of them, hence most of the time,
						// there should be no "cache thrashing".
						
						BitVector B_accepted=inputsAccepted.get(stateB.getKey()),B_rejected=inputsRejected.get(stateB.getKey());
						if (intersects(inputsAcceptedFromA,B_rejected) || intersects(inputsRejectedFromA,B_accepted))
						{// an incompatible pair, which was not already marked as such, hence propagate incompatibility
							sourceData.clear();incompatiblePairs[currentStatePair]=PAIR_INCOMPATIBLE;
							Map<String,List<CmpVertex>> rowB = stateB.getValue();
							
							for(Entry<String,List<CmpVertex>> outLabel:rowA_collection)
							{
								List<CmpVertex> to = rowB.get(outLabel.getKey());
								if (to != null)
								{// matched pair of transitions, now we need to build a cross-product 
								 // of the states leading to the current pair of states, that is,
								 // to (entryA.getKey(),stateB)
		
									for(CmpVertex srcA:outLabel.getValue())
										for(CmpVertex srcB:to)
										{
											// It is possible that for the same inpus (srcA,srcB)=(A,B) and (B,A)
											// in this case, we have to avoid including (B,A) in the list, but 
											// it is not known in advance if any such case occurs, so we have to store
											// the pairs we encountered and eliminate them. 
											int sourcePair = coregraph.wmethod.vertexToIntNR(srcB,srcA);// Note that it does not matter if we use the correct one or the wrong one (vertexToInt) call here because all it is used for is to identify pairs, both do this uniquely. The queue of pairs to process gets actual state pairs because it need to map from them to the source states. For this reasaon, we are immune from the wrong call at this point. 
											if (!sourceData.contains(sourcePair))
											{
												sourceData.add(sourcePair);
												synchronized (currentExplorationBoundary) 
												{
													currentExplorationBoundary.add(new StatePair(srcB,srcA));
												}
											}
										}
								}
							}
						}// if intersects
						else 
							if (incompatiblePairs[currentStatePair] != PAIR_INCOMPATIBLE) // it is not possible for this loop to set this -
								// we are going through the vertices sequentially, but it could have been set by whoever called us.
							incompatiblePairs[currentStatePair]=PAIR_OK;// potentially compatible pair

						if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
					}// B-loop
				}// if (entryA.getKey().isAccept())
			}
		});
		performRowTasks(handlerList, ThreadNumber);
		inputsAccepted=null;inputsRejected=null;
		
		// At this point, we've marked all clearly incompatible pairs of states and need to propagate 
		// this information further, to states which have transitions leading to the currently considered set of states.
		while(!currentExplorationBoundary.isEmpty())
		{
			StatePair pair = currentExplorationBoundary.remove();
			
			int currentStatePair = coregraph.wmethod.vertexToIntNR(pair.firstElem,pair.secondElem);
			incompatiblePairs[currentStatePair]=PAIR_INCOMPATIBLE;

			Map<String,List<CmpVertex>> rowB = coregraph.learnerCache.getSortaInverse().get(pair.secondElem);
			
			for(Entry<String,List<CmpVertex>> outLabel:coregraph.learnerCache.getSortaInverse().get(pair.firstElem).entrySet())
			{
				List<CmpVertex> to = rowB.get(outLabel.getKey());
				if (to != null)
				{// matched pair of transitions, now we need to build a cross-product 
				 // of the states leading to the current pair of states, that is,
				 // to (entryA.getKey(),stateB)
					for(CmpVertex srcA:outLabel.getValue())
						for(CmpVertex srcB:to)
						{
							// It is possible that for the same inputs (srcA,srcB)=(A,B) and (B,A)
							// in this case, we have to avoid including (B,A) in the list, but 
							// it is not known in advance if any such case occurs, so we have to store
							// the pairs we encountered and eliminate them. Happily, this is handled by 
							// incompatiblePairs[sourcePair]=PAIR_INCOMPATIBLE
							int sourcePair = coregraph.wmethod.vertexToIntNR(srcA, srcB);
							if (incompatiblePairs[sourcePair] == PAIR_OK)
							{
								currentExplorationBoundary.offer(new StatePair(srcA,srcB)); // append to the list
								incompatiblePairs[sourcePair]=PAIR_INCOMPATIBLE;
							}
						}
				}
			}
			
		}
		
		return numberNonNegativeElements(incompatiblePairs);
	}

	protected void printOK(int []data)
	{
		int cnt=0;
		for(int i=0;i<data.length;++i)
			if (data[i] == PAIR_OK) ++cnt;
		System.out.println("OK pairs: "+cnt);
	}
	
	/** Sequentially number elements in the array which are not negative. */
	public static int numberNonNegativeElements(int data[])
	{
		int num=0;
		for(int i=0;i<data.length;++i)
			if (data[i] == PAIR_OK) data[i]=num++;
		
		return num;
	}
	
	/** This routine is used for testing buildMatrix_internal. */
	public LSolver buildMatrix(final int ThreadNumber)
	{
		final int [] incompatiblePairs = new int[coregraph.learnerCache.getAcceptStateNumber()*(coregraph.learnerCache.getAcceptStateNumber()+1)/2];for(int i=0;i<incompatiblePairs.length;++i) incompatiblePairs[i]=PAIR_OK;
		final int pairsNumber = findIncompatiblePairs(incompatiblePairs,ThreadNumber);
		return buildMatrix_internal(incompatiblePairs, pairsNumber, ThreadNumber,null);
	}
	
	/** Computes the compatibility between pairs of states.
	 * 
	 * @param ThreadNumber how many CPUs to use
	 * @return a map from numbers returned by <em>wmethod.vertexToIntNR(A,B)</em>
	 * to compatibility score of states <em>A</em> and <em>B</em>.
	 */ 
	public double [] computeStateCompatibility(int ThreadNumber,final Class<? extends DetermineDiagonalAndRightHandSide> ddrh)
	{
		final int [] incompatiblePairs = new int[coregraph.learnerCache.getAcceptStateNumber()*(coregraph.learnerCache.getAcceptStateNumber()+1)/2];for(int i=0;i<incompatiblePairs.length;++i) incompatiblePairs[i]=PAIR_OK;
		final int pairsNumber = findIncompatiblePairs(incompatiblePairs,ThreadNumber);
		LSolver solver = buildMatrix_internal(incompatiblePairs, pairsNumber, ThreadNumber,ddrh);
		solver.solve();
		solver.freeAllButResult();// deallocate memory before creating a large array.
		double statePairScores[] = new double[incompatiblePairs.length];
		// now fill in the scores in the array.
		for(int i=0;i<incompatiblePairs.length;++i)
			if (incompatiblePairs[i] >=0) statePairScores[i]=solver.j_x[incompatiblePairs[i]];
			else statePairScores[i]=incompatiblePairs[i];// PAIR_INCOMPATIBLE
			
		return statePairScores;
	}
	
	public LSolver buildMatrix_internal(final int [] incompatiblePairs, final int pairsNumber, final int ThreadNumber, 
			final Class<? extends DetermineDiagonalAndRightHandSide> ddrh)
	{ 
		if (Boolean.valueOf(Visualiser.getProperty(VIZ_PROPERTIES.LINEARWARNINGS, "false")))
			System.out.println("Initial number of pairs: "+(coregraph.getStateNumber()*(coregraph.getStateNumber()+1)/2)+", after reduction: "+pairsNumber);

		final int expectedMatrixSize = coregraph.learnerCache.getExpectedIncomingPerPairOfStates()*pairsNumber;
		/** This one is supposed to contain indices into Ai where each column starts. Every thread
		 * processes a continuous sequence of state pairs (ensured by exploring a triangular subset of 
		 * all the pairs and matching it to calls to vertexToIntNR()). For this reason, it is easy to 
		 * compute a range of state pairs handled by every thread and subsequently renumber the array.
		 */ 
		final int Ap[]=new int[pairsNumber+1];
		final int Ap_threadStart[]=new int[ThreadNumber+1];for(int i=0;i<Ap_threadStart.length;++i) Ap_threadStart[i]=-1;
		
		// one array per thread.
		final IntArrayList Ai_array[]=new IntArrayList[ThreadNumber];
		final DoubleArrayList Ax_array[]=new DoubleArrayList[ThreadNumber];
		final double b[] =new double[pairsNumber];
		final int currentPosition[]=new int[ThreadNumber];// stores the last used index in the Ai and Ax arrays.
		final double k = coregraph.config.getAttenuationK();

		// We need next to no locking since state pairs considered are disjoint and work arrays are split between threads.
		List<HandleRow> handlerList = new LinkedList<HandleRow>();
		for(int threadCnt=0;threadCnt<ThreadNumber;++threadCnt)
		handlerList.add(new HandleRow()
		{
			IntArrayList tmpAi = null;
			//long sourcePairEncountered[] = null;
			/** sourcePairEncountered is a set of state pairs which has to be frequently reset. For this reason,
			 * we assign uniqueValueForsourcePairEncountered values to elements of the state pairs.
			 *  uniqueValueForsourcePairEncountered gets incremented whenever we need to reset the set. 
			 *  The maximal number is number of state pairs x number of inputs.
			 */
			long uniqueValueForSourcePairEncountered = 0;
			
			/** Used to detect non-consecutive state pair numbers - in this case an internal error should be reported. */
			int prevStatePairNumber =-1;
			
			final int debugThread = -1;
			DetermineDiagonalAndRightHandSide ddrhInstance = null;
			
			public void init(int threadNo) throws InstantiationException, IllegalAccessException
			{
				tmpAi = new IntArrayList(coregraph.learnerCache.getExpectedIncomingPerPairOfStates()*pairsNumber);
				if (ddrh == null) ddrhInstance = new DDRH_default();else ddrhInstance = ddrh.newInstance();
				Ai_array[threadNo]=new IntArrayList(expectedMatrixSize/ThreadNumber+coregraph.learnerCache.getExpectedIncomingPerPairOfStates());
				Ax_array[threadNo]=new DoubleArrayList(expectedMatrixSize/ThreadNumber+coregraph.learnerCache.getExpectedIncomingPerPairOfStates());
				currentPosition[threadNo]=0;
				
			}
			
			Set<Integer> sourceData = new TreeSet<Integer>();
			
			public void handleEntry(Entry<CmpVertex, Map<String, CmpVertex>> entryA, int threadNo) 
			{
				IntArrayList Ai = Ai_array[threadNo];
				DoubleArrayList Ax = Ax_array[threadNo];
				if (entryA.getKey().isAccept())
				{
					Collection<Entry<String,List<CmpVertex>>> rowA_collection = coregraph.learnerCache.getSortaInverse().get(entryA.getKey()).entrySet();
					
					// Now iterate through states
					Iterator<Entry<CmpVertex,Map<String,List<CmpVertex>>>> stateB_It = coregraph.learnerCache.getSortaInverse().entrySet().iterator();
					while(stateB_It.hasNext())
					{
						Entry<CmpVertex,Map<String,List<CmpVertex>>> stateB = stateB_It.next();
						Map<String,List<CmpVertex>> rowB = stateB.getValue();
						
						// At this point, we consider a pair of states (entryA.getKey(),stateB),
						// by iterating through inputs associated with incoming transitions and
						// attempting to check if there is a match.
						
						int currentStatePair = incompatiblePairs[coregraph.wmethod.vertexToIntNR(stateB.getKey(),entryA.getKey())];// the order 
								// of arguments is important:
								// we have to iterate such that each thread has a continuous sequence of state pair number
								// (and these numbers are never shared).
						
						if (currentStatePair >= 0)
						{
							assert prevStatePairNumber < 0 || currentStatePair == prevStatePairNumber+1;prevStatePairNumber=currentStatePair;
	
							if (Ap_threadStart[threadNo] < 0) Ap_threadStart[threadNo]=currentStatePair;
							if (debugThread == threadNo) System.out.println("states: ("+entryA.getKey()+","+stateB+"), id "+currentStatePair);
							int colEntriesNumber=0;
							tmpAi.setQuick(colEntriesNumber++, currentStatePair);// we definitely need a diagonal element, hence add it.
							
							for(Entry<String,List<CmpVertex>> outLabel:rowA_collection)
							{
								List<CmpVertex> to = rowB.get(outLabel.getKey());
								if (to != null)
								{// matched pair of transitions, now we need to build a cross-product 
								 // of the states leading to the current pair of states, that is,
								 // to (entryA.getKey(),stateB)
									uniqueValueForSourcePairEncountered++;sourceData.clear();
									
									int maxSize = colEntriesNumber+outLabel.getValue().size()*to.size();
									if (tmpAi.elements().length < maxSize)
									{
										if (Boolean.valueOf(Visualiser.getProperty(VIZ_PROPERTIES.LINEARWARNINGS, "false")))
											System.out.println("buildMatrix: warning - resizing arrays tmpAi[thread] from "+tmpAi.elements().length+" to "+maxSize);
										tmpAi.ensureCapacity(maxSize);
									}
									if (debugThread == threadNo) System.out.println("matched "+outLabel.getKey());
									for(CmpVertex srcA:outLabel.getValue())
										for(CmpVertex srcB:to)
										{
											// It is possible that for the same inputs (srcA,srcB)=(A,B) and (B,A)
											// in this case, we have to avoid including (B,A) in the list, but 
											// it is not known in advance if any such case occurs, so we have to store
											// the pairs we encountered and eliminate them.
											int sourcePair = incompatiblePairs[coregraph.wmethod.vertexToIntNR(srcB, srcA)];
											
											// If sourcePair <0, it means that we are attempting to add an entry for a row we've already discarded, hence ignore this entry
											if (sourcePair >= 0 && !sourceData.contains(sourcePair))
													//sourcePairEncountered[sourcePair] != uniqueValueForSourcePairEncountered)
											{
												//sourcePairEncountered[sourcePair] = uniqueValueForSourcePairEncountered;
												sourceData.add(sourcePair);
												if (debugThread == threadNo) System.out.println(outLabel.getKey()+" : "+srcB+","+srcA);
												tmpAi.setQuick(colEntriesNumber++,sourcePair);
											}
										}
								}
							}
							ddrhInstance.compute(entryA.getValue(),coregraph.transitionMatrix.get(stateB.getKey()));
							b[currentStatePair]=ddrhInstance.getRightHandSide();
							if (debugThread == threadNo) System.out.println("shared outgoing: "+ddrhInstance.getRightHandSide());
							
							// At this point, we populated Ai and b with elements for the current row (number currentStatePair),
							// so it is time to sort these entries. There is no need to populate Ax right now:
							// all we care about is the state pairs from which there have been transitions leading to the 
							// current state (and a diagonal element). 
							cern.colt.Sorting.quickSort(tmpAi.elements(),0, colEntriesNumber,new IntComparator() {
								public int compare(int o1, int o2) { return o1-o2; }});
							if (debugThread == threadNo)  { for(int i=0;i< colEntriesNumber;++i) System.out.print(tmpAi.getQuick(i)+" ");System.out.println(); }
							// Now we have to copy the result to the target array.
							int pos = currentPosition[threadNo]-1;// the position where to start writing into Ai and Ax, minus 1 since it will be incremented when we find the our new value is above prev (below).
							Ap[currentStatePair]=pos+1;// Ap maps each state pair to the corresponding position in Ai and Ax. We record here the first index (in Ai) of the current column 
							
							int prev = -1;
							boolean diagonalSet = false;
		
							// Check if we have the capacity in the arrays for this column - in an ugly way,
							// but resizing for each element as per Ax.add seems ridiculous.
							int expectedMaxSize = pos+colEntriesNumber+1;
							if (Ax.elements().length < expectedMaxSize)
							{
								if (Boolean.valueOf(Visualiser.getProperty(VIZ_PROPERTIES.LINEARWARNINGS, "false")))
									System.out.println("buildMatrix: warning - resizing arrays Ax[thread] and Ai[thread] from "+Ax.elements().length+" to "+expectedMaxSize);
								Ax.ensureCapacity(expectedMaxSize);
								Ai.ensureCapacity(expectedMaxSize);
							}
		
							for(int i=0;i<colEntriesNumber;++i)
							{
								int currentValue = tmpAi.getQuick(i);
								if(currentValue!=prev)
								{
									prev=currentValue;++pos;
									
									if (!diagonalSet && currentValue == currentStatePair)
									{// this is the time to handle a diagonal. When this condition becomes true,
									 // currentValue == currentStatePair for the first time.
										int totalOutgoing = ddrhInstance.getDiagonal();//entryA.getValue().size()+coregraph.transitionMatrix.get(stateB.getKey()).size()-sharedOutgoing;
										if (totalOutgoing == 0)
											totalOutgoing = 1; // if neither element of a pair of states has an outgoing transition, force the identity to ensure that the solution will be zero.
										if (debugThread == threadNo) System.out.println("setting diagonal to "+totalOutgoing);
										Ax.setQuick(pos,totalOutgoing);
										Ai.setQuick(pos,prev);
										diagonalSet = true;
										// Now that we've added a diagonal, skip to the next element (we always add an entry for a diagonal, hence we have to "eat" it here.
									}
									else		
									{// new value but not a diagonal.							
										Ax.setQuick(pos,-k);
										Ai.setQuick(pos,prev);
									}
								}
								else Ax.setQuick(pos,Ax.getQuick(pos)-k);
							}
							++pos;
							if (debugThread == threadNo) { for(int i=currentPosition[threadNo];i<pos;++i) System.out.println(i+ ": "+Ai.getQuick(i)+ " , "+Ax.getQuick(i)); }
							
							currentPosition[threadNo]=pos;
						}// if (incompatiblePairs[currentStatePair] != PAIR_INCOMPATIBLE)
						if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
					}// stateB_It.hasNext()
					
				} // entryA.getKey().isAccept())
			}
		});
		performRowTasks(handlerList, ThreadNumber);
		// At this point, we are finished building the matrices, it's time to populate the main arrays.
		
		// First, we compute the number of non-zero elements.
		int size = 0;for(int thread=0;thread<ThreadNumber;++thread) size+=currentPosition[thread];
		// Second, allocate arrays.
		int Ai[]=new int[size];double Ax[]=new double[size];

		// In a number of cases we'll have more threads than rows to handle, hence threads will have nothing to do
		// and Ap_threadStart will be -1 for those threads. In order to ensure that Ap_threadStart contains
		// continuous values, we set those -1 elements to Ap_threadStart of the subsequent thread (or pairsNumber if there is none).
		int currentLastStatePair = pairsNumber;
		for(int th=ThreadNumber;th>=0;--th) if (Ap_threadStart[th] < 0) Ap_threadStart[th]=currentLastStatePair;else currentLastStatePair=Ap_threadStart[th];
		
		int prevLastPos = 0;
		// Finally, populate them.
		for(int thread=0;thread<ThreadNumber;++thread)
		{
/* I'll try to draw a picture relating different arrays, 

StatePair values  : 0  1  2 | 3  4  5 | 6  7  8
				Ap:	0  8 12 | 0 11 16 | 0  4 12
                            |         |
  threadNo		  :     0        1         2 		  
  currentPosition :    20       18        19
  
  The above picture with three threads shows the following relation between columns
  and the number of elements in them:
	0	8
	1	12-8
	2	20-12
	
	3	11
	4	16-11
	5	18-16
	
	....
	
  When we copy the outcome into Ai and Ax, we have to introduce sequential numbers, 
  so column 3 will start where column 2 finished in Ai populated by thread 0, 
  i.e. at position 20 (last element filled in by thread 0), 
  column 4 will start at 11+20 and so on until data filled in by thread 2,
  hence column 6 will start at position 20+18.
  
  With the above illustration in mind, the code below updates Ap and fills in Ax and Ai.
*/

			if (Ap_threadStart[thread] >=0)
			{// this thread has actually done some work, hence merge its results into our arrays.
				
				int lastPair = Ap_threadStart[thread+1];// the position where the next thread has started (or would've started if it was not the last thread).
				for(int i=Ap_threadStart[thread];i<lastPair;++i)
					Ap[i]+=prevLastPos;
				
				// If a thread had no work to do, currentPosition[thread] would stay at zero so we shall not corrupt anything,
				// but Ai_array[thread] may be null, so we'd best check for "no work".
				if (currentPosition[thread]>0)
				{
					System.arraycopy(Ai_array[thread].elements(), 0, Ai, prevLastPos, currentPosition[thread]);
					System.arraycopy(Ax_array[thread].elements(), 0, Ax, prevLastPos, currentPosition[thread]);
					prevLastPos+=currentPosition[thread];
				}
			}
		}
		Ap[pairsNumber]=prevLastPos;
		return new LSolver(Ap,Ai,Ax,b,new double[pairsNumber]);
	}

	/** This is a kind of an inverse of vertexToIntNR, takes a number and a score returns a corresponding <em>PairScore</em>.
	 *  
	 */ 
	public PairScore getPairScore(int pair, int score, int compat)
	{
		int row=(int)(-1+Math.sqrt(pair*8+1))/2;// rounding by truncation
		int column = pair-row*(row+1)/2;
		assert row >=0 && column >= 0;
		assert row < coregraph.learnerCache.getAcceptStateNumber() && column < coregraph.learnerCache.getAcceptStateNumber();
		assert column <= row;
		return new PairScore(coregraph.learnerCache.getNumberToStateNoReject()[row],coregraph.learnerCache.getNumberToStateNoReject()[column],score,compat);
	}
	
	/** Returns a stack of states with scores over a given threshold. 
	 * 
	 * @param threshold the threshold to use, prior to scaling.
	 * @param scale We are using floating-point numbers here but compatibility scores are integers, hence we scale them before truncating into integers.
	 * @param ThreadNumber the number of CPUs to use
	 * @return
	 */
	public Stack<PairScore> chooseStatePairs(double threshold, double scale, int ThreadNumber, 
			final Class<? extends DetermineDiagonalAndRightHandSide> ddrh)
	{
		final int [] incompatiblePairs = new int[coregraph.learnerCache.getAcceptStateNumber()*(coregraph.learnerCache.getAcceptStateNumber()+1)/2];for(int i=0;i<incompatiblePairs.length;++i) incompatiblePairs[i]=PAIR_OK;
		final int pairsNumber = findIncompatiblePairs(incompatiblePairs,ThreadNumber);
		LSolver solver = buildMatrix_internal(incompatiblePairs, pairsNumber, ThreadNumber,ddrh);
		solver.solve();
		solver.freeAllButResult();// deallocate memory before creating a large array.
		coregraph.pairsAndScores.clear();
		// now fill in the scores in the array.
		for(int i=0;i<incompatiblePairs.length;++i)
		{
			int index = incompatiblePairs[i];
			if (index >= 0) 
			{
				double value = solver.j_x[incompatiblePairs[i]];
				if (value > threshold) coregraph.pairsAndScores.add(getPairScore(i, (int)(scale*value), 0));
			}
			else // PAIR_INCOMPATIBLE
				if (threshold < PAIR_INCOMPATIBLE) coregraph.pairsAndScores.add(getPairScore(i, (int)(scale*PAIR_INCOMPATIBLE), 0));
		}
		return coregraph.pairscores.getSortedPairsAndScoresStackFromUnsorted();
	}
	
	/** Acts as an oracle, comparing two graphs are returning compatibility score. 
	 *
	 * @param forceAccept if true, assumes that all states are accept-states
	 */
	public double getSimilarity(LearnerGraph gr, boolean forceAccept, int ThreadNumber)
	{
		Configuration copyConfig = (Configuration)coregraph.config.clone();copyConfig.setLearnerCloneGraph(true);
		LearnerGraph copy = coregraph.copy(copyConfig);
		CmpVertex grInit = Transform.addToGraph(copy, gr);
		if (forceAccept) for(CmpVertex vert:copy.transitionMatrix.keySet()) vert.setAccept(true);
		copy.learnerCache.invalidate();
		assert copy.learnerCache.getStateToNumberNoReject().containsKey(copy.init);
		assert copy.learnerCache.getStateToNumberNoReject().containsKey(grInit);
		return copy.linear.computeStateCompatibility(ThreadNumber,DDRH_default.class)[copy.wmethod.vertexToIntNR(copy.init, grInit)]; 
	}

	
	/** Acts as an oracle, comparing two graphs are returning compatibility score, however
	 * reject states are not ignored. Accept/reject decisions are copied into highlight
	 * and then objects of the supplied class are used to compute numbers. 
	 */
	public double getSimilarityWithNegatives(LearnerGraph gr, int ThreadNumber, 
			final Class<? extends DetermineDiagonalAndRightHandSide> ddrh)
	{
		Configuration copyConfig = (Configuration)coregraph.config.clone();copyConfig.setLearnerCloneGraph(true);
		LearnerGraph copy = coregraph.copy(copyConfig);
		CmpVertex grInit = Transform.addToGraph(copy, gr);
		copy.linear.moveRejectToHightlight();
		copy.learnerCache.invalidate();
		double result = copy.linear.computeStateCompatibility(ThreadNumber,ddrh)[copy.wmethod.vertexToIntNR(copy.init, grInit)];
		
		return result;
	}
}
