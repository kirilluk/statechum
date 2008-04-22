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
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import cern.colt.function.IntComparator;
import cern.colt.list.DoubleArrayList;
import cern.colt.list.IntArrayList;

public class Linear {
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	Linear(LearnerGraph g)
	{
		coregraph =g;
	}

	/*
	public void addToBuffer(AddToMatrix resultReceiver, CmpVertex A, CmpVertex B)
	{
		Map<Integer,Double> targetCnt = new TreeMap<Integer,Double>();
		Map<String,CmpVertex> rowB = coregraph.transitionMatrix.get(B);
		int totalOutgoing = coregraph.transitionMatrix.get(A).size()+rowB.size();
		int valueFirst = 1+coregraph.wmethod.vertexToInt(A,B);
		int matchedNumber = 0;targetCnt.clear();
		for(Entry<String,CmpVertex> outLabel:coregraph.transitionMatrix.get(A).entrySet())
		{
			CmpVertex to = rowB.get(outLabel.getKey());
			if (to != null)
			{// matched pair of transitions
				++matchedNumber;
				int valueSecond = 1+coregraph.wmethod.vertexToInt(outLabel.getValue(),to);
				Double current = targetCnt.get(valueSecond);
				if (current == null)
					targetCnt.put(valueSecond,-valueK);
				else
					targetCnt.put(valueSecond,current-valueK);
			}
		}
		if (totalOutgoing == 0)
			targetCnt.put(valueFirst, new Double(1));
		else
		{
			Double current = targetCnt.get(valueFirst);
			double increment = (double)totalOutgoing-matchedNumber;
			if (current == null)
				targetCnt.put(valueFirst, increment);
			else
				targetCnt.put(valueFirst,increment+current);
		}
		for(Entry<Integer,Double> entry:targetCnt.entrySet())
			resultReceiver.addMapping(valueFirst,entry.getKey(),entry.getValue());
	}
	*/
	
	public interface HandleRow 
	{
		/** Initialises this job. */
		public void init(int threadNo);
		
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
				System.out.println("thread "+threadNo+" nothing to do");
			else
			{
				handler.init(threadNo);
				int currentRow = 0;
				Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> stateB_It = coregraph.transitionMatrix.entrySet().iterator();
				while(stateB_It.hasNext() && currentRow < workLoad[threadNo])
				{
					stateB_It.next();++currentRow;
				}
				System.out.println("thread "+threadNo+" started from row "+currentRow);
				while(stateB_It.hasNext() && currentRow < workLoad[threadNo+1])
				{
					Entry<CmpVertex,Map<String,CmpVertex>> stateB = stateB_It.next();++currentRow;
					handler.handleEntry(stateB, threadNo);
				}
				System.out.println("thread "+threadNo+" finished at row "+currentRow);
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

	/** Updates the cached data with a transition matrix where all transitions point in 
	 * an opposite direction to the current one. The matrix produced is used to scan 
	 * the state comparison matrix columnwise.
	 */
	public void prepareForLinear()
	{
		coregraph.learnerCache.stateToNumber = coregraph.wmethod.buildStateToIntegerMap();
		
		Map<CmpVertex,Map<String,List<CmpVertex>>> sortaInverse = new TreeMap<CmpVertex,Map<String,List<CmpVertex>>>();
		
		// First, we fill the map with empty entries.
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
			sortaInverse.put(entry.getKey(),new TreeMap<String,List<CmpVertex>>());
		
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
		{
			for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
			{
				Map<String,List<CmpVertex>> row = sortaInverse.get(transition.getValue());
				List<CmpVertex> sourceStates = row.get(transition.getKey());
				if (sourceStates == null)
				{
					sourceStates=new LinkedList<CmpVertex>();row.put(transition.getKey(), sourceStates);
				}
				sourceStates.add(entry.getKey());
			}
		}
		
		coregraph.learnerCache.sortaInverse = sortaInverse;
		
		// Now we need to estimate expectedIncomingPerPairOfStates
		int indegreeSum=0, incomingCnt = 0, maxInDegree = -1;
		for(Entry<CmpVertex,Map<String,List<CmpVertex>>> entry:sortaInverse.entrySet())
			for(Entry<String,List<CmpVertex>> transition:entry.getValue().entrySet())
			{
				++incomingCnt;
				int size = transition.getValue().size()*entry.getValue().size();indegreeSum+=size;
				if (size > maxInDegree) maxInDegree=size;
			}
		
		// The logic is simple: if maxInDegree is much higher than the 
		// average, double the average indegree, otherwise leave it unchanged.  
		coregraph.learnerCache.expectedIncomingPerPairOfStates = 2;
		if (incomingCnt > 0) coregraph.learnerCache.expectedIncomingPerPairOfStates=1+indegreeSum/incomingCnt;// 1 is to account for a diagonal
/*		if (maxInDegree/4 > coregraph.learnerCache.expectedIncomingPerPairOfStates)
			coregraph.learnerCache.expectedIncomingPerPairOfStates<<=1;// double it
	*/	
		System.out.println("Indegree sum: "+indegreeSum+" state pairs: "+incomingCnt+" average indegree: "+(incomingCnt>0 ?indegreeSum/incomingCnt:0)+" max indegree: "+maxInDegree);
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
		for(int count=1;count < ThreadNumber;++count)
		{
			int a= result[count-1];
			result[count]=a+(int)Math.round(( -2*a-1 + Math.sqrt((double)(2*a+1)*(2*a+1)+
					4*(double)coregraph.getStateNumber()*(coregraph.getStateNumber()+1)/ThreadNumber) ) /2);
			assert result[count] >= 0 && result[count] <= coregraph.getStateNumber() : "obtained row "+result[count]+" while the range is 0.."+coregraph.getStateNumber();
		}
		result[ThreadNumber]=coregraph.getStateNumber();
		return result;
	}
	
	/** Counts the number of outgoing transitions from a pair of states with the same label.
	 * Hence if A has a,b,c outgoing and B has b,c,e,f, the outcome will be 2 because 
	 * there are two transitions, b,c in common.
	 */
	protected int countMatchingOutgoing(Map<String,CmpVertex> A, Map<String,CmpVertex> B)
	{
		int counter = 0;
		for(Entry<String,CmpVertex> entry:A.entrySet())
		if (B.containsKey(entry.getKey())) ++counter;
		return counter;
	}
	
	public ExternalSolver buildMatrix(final int ThreadNumber)
	{
		if (coregraph.learnerCache.expectedIncomingPerPairOfStates <= 0 || 
				coregraph.learnerCache.sortaInverse == null || coregraph.learnerCache.stateToNumber == null)
			prepareForLinear();
		
		final int pairsNumber = coregraph.getStateNumber()*(coregraph.getStateNumber()+1)/2;
		final int expectedMatrixSize = coregraph.learnerCache.expectedIncomingPerPairOfStates*pairsNumber;
		/** This one is supposed to contain indices into Ai where each column starts. Every thread
		 * processes a continuous sequence of state pairs (ensured by exploring a triangular subset of 
		 * all the pairs and matching it to calls to vertexToInt()). For this reason, it is easy to 
		 * compute a range of state pairs handled by every thread and subsequently renumber the array.
		 */ 
		final int Ap[]=new int[pairsNumber+1];
		final int Ap_threadStart[]=new int[ThreadNumber+1];for(int i=0;i<Ap_threadStart.length;++i) Ap_threadStart[i]=-1;
		//final int alphabetSize = coregraph.wmethod.computeAlphabet().size();
		
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
			
			final int debugThread = -1;

			public void init(int threadNo)
			{
				tmpAi = new IntArrayList(coregraph.learnerCache.expectedIncomingPerPairOfStates*pairsNumber);
				//sourcePairEncountered = new long[pairsNumber];
				Ai_array[threadNo]=new IntArrayList(expectedMatrixSize/ThreadNumber+coregraph.learnerCache.expectedIncomingPerPairOfStates);
				Ax_array[threadNo]=new DoubleArrayList(expectedMatrixSize/ThreadNumber+coregraph.learnerCache.expectedIncomingPerPairOfStates);
				currentPosition[threadNo]=0;
				
			}
			
			Set<Integer> sourceData = new TreeSet<Integer>();
			
			public void handleEntry(Entry<CmpVertex, Map<String, CmpVertex>> entryA, int threadNo) 
			{
				IntArrayList Ai = Ai_array[threadNo];
				DoubleArrayList Ax = Ax_array[threadNo];
				Collection<Entry<String,List<CmpVertex>>> rowA_collection = coregraph.learnerCache.sortaInverse.get(entryA.getKey()).entrySet();
				
				// Now iterate through states
				Iterator<Entry<CmpVertex,Map<String,List<CmpVertex>>>> stateB_It = coregraph.learnerCache.sortaInverse.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Map<String,List<CmpVertex>>> stateB = stateB_It.next();
					Map<String,List<CmpVertex>> rowB = stateB.getValue();
					
					// At this point, we consider a pair of states (entryA.getKey(),stateB),
					// by iterating through inputs associated with incoming transitions and
					// attempting to check if there is a match.
					
					int currentStatePair = coregraph.wmethod.vertexToInt(stateB.getKey(),entryA.getKey());// the order 
							// of arguments is important:
							// we have to iterate such that each thread has a continuous sequence of state pair number
							// (and these numbers are never shared).
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
								System.out.println("buildMatrix: warning - resizing arrays tmpAi[thread] from "+tmpAi.elements().length+" to "+maxSize);
								tmpAi.ensureCapacity(maxSize);
							}
							if (debugThread == threadNo) System.out.println("matched "+outLabel.getKey());
							for(CmpVertex srcA:outLabel.getValue())
								for(CmpVertex srcB:to)
								{
									// It is possible that for the same inpus (srcA,srcB)=(A,B) and (B,A)
									// in this case, we have to avoid including (B,A) in the list, but 
									// it is not known in advance if any such case occurs, so we have to store
									// the pairs we encountered and eliminate them. This is accomplished using 
									// the array sourcePairEncountered which maps (srcA,srcB) to the last currentStatePair+1
									// they have been encountered at. This way, we do not need to reset the array
									// between computations; not even at the initial stage, thanks to "+1".
									int sourcePair = coregraph.wmethod.vertexToInt(srcA, srcB);
									if (!sourceData.contains(sourcePair))
											//sourcePairEncountered[sourcePair] != uniqueValueForSourcePairEncountered)
									{
										//sourcePairEncountered[sourcePair] = uniqueValueForSourcePairEncountered;
										sourceData.add(sourcePair);
										if (debugThread == threadNo) System.out.println(outLabel.getKey()+" : "+srcA+","+srcB);
										tmpAi.setQuick(colEntriesNumber++,sourcePair);
									}
								}
						}
					}
					
					int sharedOutgoing = countMatchingOutgoing(entryA.getValue(),coregraph.transitionMatrix.get(stateB.getKey()));
					if (debugThread == threadNo) System.out.println("shared outgoing: "+sharedOutgoing);
					b[currentStatePair]=sharedOutgoing;
					
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
								int totalOutgoing = entryA.getValue().size()+coregraph.transitionMatrix.get(stateB.getKey()).size()-sharedOutgoing;
								if (totalOutgoing == 0) totalOutgoing = 1;// if neither element of a pair of states has an outgoing transition, force the identity to ensure that the solution will be zero. 
								if (debugThread == threadNo) System.out.println("setting diagonal to "+totalOutgoing+" (shared outgoing is "+sharedOutgoing+" )");
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
					
					if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
				}
				
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
		return new ExternalSolver(Ap,Ai,Ax,b,new double[pairsNumber]);
	}
}
