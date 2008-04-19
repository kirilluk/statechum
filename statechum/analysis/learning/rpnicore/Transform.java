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

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.experiments.AbstractExperiment;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

public class Transform {
	final LearnerGraph coregraph;
	
	/** Associates this object to ComputeStateScores it is using for data to operate on. 
	 * Important: the constructor should not access any data in computeStateScores 
	 * because it is usually invoked during the construction phase of ComputeStateScores 
	 * when no data is yet available.
	 */
	Transform(LearnerGraph g)
	{
		coregraph =g;
	}
	
	public static int HammingDistance(List<Boolean> A, List<Boolean> B)
	{
		if (A.size() != B.size())
			throw new IllegalArgumentException("sequences of different length passed");
		int distance = 0;
		Iterator<Boolean> A_Iter = A.iterator(), B_Iter = B.iterator();
		while(A_Iter.hasNext())
		{
			if (!A_Iter.next().equals(B_Iter.next())) ++distance;
		}
		return distance;
	}

	public static void addAssignement(StringBuffer buffer, Integer A, Integer B, double value)
	{
		buffer.append("mat(");
		buffer.append(A);
		buffer.append(",");
		buffer.append(B);
		buffer.append(")=");buffer.append(value);buffer.append(";");
	}
	
	public final double valueK=0.9;
	
	public interface AddToMatrix
	{
		public void addMapping(int A, int B, double value);
	}
	
	/** Given a row of a transition matrix this one computes the number of pairs of outgoing transitions
	 * in common between a state corresponding to that row and all other states in this machine.
	 * 
	 * @param rowB the row to consider
	 * @return the number of common pairs - this is useful to compute the number of nonzero elements in the matrix.
	 */
	public int estimateSize(Entry<CmpVertex,Map<String,CmpVertex>> entryA)
	{
		int result = 0;
		Map<String,CmpVertex> rowA = entryA.getValue();
		Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> stateB_It = coregraph.transitionMatrix.entrySet().iterator();
		while(stateB_It.hasNext())
		{
			Entry<CmpVertex,Map<String,CmpVertex>> stateB = stateB_It.next();++result;
			for(Entry<String,CmpVertex> outLabel:stateB.getValue().entrySet())
			{
				if (rowA.containsKey(outLabel.getKey()))
					++result;
			}
			if (stateB.getKey() == entryA.getKey()) break; // we only process a triangular subset.
		}
		return result;
	}
	
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
	
	public interface HandleRow 
	{
		/** Called for each row of our transition matrix. */
		public void handleEntry(Entry<CmpVertex,Map<String,CmpVertex>> entry, int threadNo);
		public void finished(int threadNo);
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
				handler.finished(threadNo);
				System.out.println("thread "+threadNo+" finished at row "+currentRow);
			}
			return 0;
	}}
	
	/** Runs this handler on all the rows in our matrix, using all the available CPUs. */
	protected void performRowTasks(final HandleRow handler, int ThreadNumber)
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
					runner.submit(new Job(workLoad,count,handler));
			
				for(int count=0;count < ThreadNumber;++count)
					runner.take().get();
			}
			else
				// Run single-threaded.
				new Job(workLoad,0,handler).call();
		}
		catch(Exception ex)
		{
			IllegalArgumentException e = new IllegalArgumentException("failed to compute, the is problem: "+ex);e.initCause(ex);throw e;
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
	protected Map<CmpVertex,Map<CmpVertex,String>> buildSortaInverse()
	{
		Map<CmpVertex,Map<CmpVertex,String>> sortaInverse = new TreeMap<CmpVertex,Map<CmpVertex,String>>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
		{
			for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
			{
				Map<CmpVertex,String> row = sortaInverse.get(transition.getValue());
				if (row == null)
				{
					row = new TreeMap<CmpVertex,String>();sortaInverse.put(entry.getKey(),row);
				}
				row.put(entry.getKey(), transition.getKey());
			}
		}
		return sortaInverse;
	}
	
	public void buildJniMatrix(int ThreadNumber)
	{
		coregraph.buildCachedData();
		//int ThreadNumber = AbstractExperiment.getCpuNumber();
		final int[]arraySize= new int[ThreadNumber];
		performRowTasks(new HandleRow()
		{
			int arrayChunkSize =0; 

			public void handleEntry(Entry<CmpVertex, Map<String, CmpVertex>> entry, int threadNo) {
				arrayChunkSize+=estimateSize(entry);
			}

			public void finished(int threadNo) {
				arraySize[threadNo] = arrayChunkSize;
			}
		}, ThreadNumber);
			
		int s=0;
		for(int i=0;i< ThreadNumber;++i)
		{
			System.out.println(i+" : "+arraySize[i]);s+=arraySize[i];
		}
		System.out.println(s);	

		final double buffer[] = new double[s];
		class MatrixPopulator implements AddToMatrix
		{
			private int pos=0;
			public synchronized void addMapping(int A, int B, double value) {
				buffer[pos++]=0;
			}
			
			int getSize()
			{
				return pos;
			}
		};
		MatrixPopulator populator = new MatrixPopulator();
		buildMatrix(populator,ThreadNumber);
		System.out.println(populator.getSize());
	}
	
	public void toOctaveMatrix(final Writer wr)
	{		
		AddToMatrix resultAdder = new AddToMatrix()
		{
			public void addMapping(int A, int B, double value) {
				try
				{
				wr.append("mat(");
				wr.append(""+A);
				wr.append(",");
				wr.append(""+B);
				wr.append(")=");wr.append(""+value);wr.append(";");
				}
				catch(IOException ex)
				{
					IllegalArgumentException e = new IllegalArgumentException("failed to write file");e.initCause(ex);
					throw e;
				}
			}
		};
		buildMatrix(resultAdder, AbstractExperiment.getCpuNumber());
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
			assert result[count] > 0 && result[count] <= coregraph.getStateNumber();
		}
		result[ThreadNumber]=coregraph.getStateNumber();
		return result;
	}
	
	public void buildMatrix(final AddToMatrix resultAdder, int ThreadNumber)
	{		
		performRowTasks(new HandleRow()
		{
			public void handleEntry(Entry<CmpVertex, Map<String, CmpVertex>> entryA, int threadNo) {
				Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> stateB_It = coregraph.transitionMatrix.entrySet().iterator();
				while(stateB_It.hasNext())
				{
					Entry<CmpVertex,Map<String,CmpVertex>> stateB = stateB_It.next();
					for(Entry<CmpVertex,Map<String,CmpVertex>> entryB:coregraph.transitionMatrix.entrySet())
						addToBuffer(resultAdder,entryA.getKey(),entryB.getKey());
					
					if (stateB.getKey().equals(entryA.getKey())) break; // we only process a triangular subset.
				}
			}

			public void finished(int threadNo) {
			}
		}, ThreadNumber);
		
	}
	
	
	
	/** The standard beginning of our graphML files. */
	public static final String graphML_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns/graphml\"  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\nxsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns/graphml\">\n<graph edgedefault=\"directed\">\n";
	/** The standard ending of our graphML files. */
	public static final String graphML_end = "</graph></graphml>\n"; 
	/** a marker for an initial state in a graphML file. */
	public static final String Initial = "Initial";
	
	/** Returns the ID of the node, prepending Initial as appropriate for the initial state. */
	protected String transformNodeName(CmpVertex node)
	{
		
		return (node == coregraph.init? Initial+" ":"")+node.getID().toString(); 
	}
	
	protected void writeNode(Writer writer, CmpVertex node) throws IOException
	{
		if (node.getID().toString().contains(Initial))
			throw new IllegalArgumentException("Invalid node name "+node.getID().toString());
		writer.write("<node id=\""+transformNodeName(node)+
				"\" VERTEX=\""+transformNodeName(node)+"\"");
		if (!node.isAccept()) writer.write(" "+JUConstants.ACCEPTED+"=\""+node.isAccept()+"\"");
		if (node.isHighlight()) writer.write(" "+JUConstants.HIGHLIGHT+"=\""+node.isHighlight()+"\"");
		if (node.getColour() != null) writer.write(" "+JUConstants.COLOUR+"=\""+node.getColour()+"\"");
		writer.write("/>\n");
	}
	
	/** Writes a graph into a graphML file. All vertices are written. */
	public void writeGraphML(String name) throws IOException
	{
		FileWriter writer = new FileWriter(name);writeGraphML(writer);
	}
	
	/** Writes a graph into a graphML file. All vertices are written. */
	public void writeGraphML(Writer writer) throws IOException
	{
		writer.write(graphML_header);
		
		for(Entry<CmpVertex,Map<String,CmpVertex>> vert:coregraph.transitionMatrix.entrySet())
				writeNode(writer, vert.getKey());
		// Sample initial state entry: <node id="1" VERTEX="Initial State 0" />
		// For non-initial states, there should be no vertex called "Initial".
		// Sample edge entry: <edge source="21" target="19" directed="true" EDGE="a1" />
		for(Entry<CmpVertex,Map<String,CmpVertex>> vert:coregraph.transitionMatrix.entrySet())
			for(Entry<String,CmpVertex> transition:vert.getValue().entrySet())
				writer.write("<edge source=\""+transformNodeName(vert.getKey())+
						"\" target=\""+transformNodeName(transition.getValue())+
						"\" directed=\"true\" EDGE=\""+transition.getKey()+"\"/>\n");
		writer.write(graphML_end);writer.close();
	}
	
	/** Returns a state, randomly chosen according to the supplied random number generator. */
	public static CmpVertex pickRandomState(LearnerGraph g, Random rnd)
	{
		int nr = rnd.nextInt(g.transitionMatrix.size());System.out.print(" st="+nr+" ");
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:g.transitionMatrix.entrySet())
			if (nr-- == 0)
				return entry.getKey();
		
		throw new IllegalArgumentException("something wrong with the graph - expected state was not found");
	}
	
	/** 
	 * Relabels graph, keeping NrToKeep original labels. All new ones are generated with
	 * prefix PrefixNew.
	 * 
	 * @param g graph to transform.
	 * @param NrToKeep number of labels to keep.
	 * @param PrefixNew prefix of new labels.
	 * @throws IllegalArgumentException if PrefixNew is a prefix of an existing vertex. The graph supplied is destroyed in this case.
	 */
	public static void relabel(LearnerGraph g, int NrToKeep, String PrefixNew)
	{
		Map<String,String> fromTo = new TreeMap<String,String>();
		int newLabelCnt = 0;
		TreeMap<CmpVertex,Map<String,CmpVertex>> newMatrix = new TreeMap<CmpVertex,Map<String,CmpVertex>>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:g.transitionMatrix.entrySet())
		{
			Map<String,CmpVertex> newRow = new TreeMap<String,CmpVertex>();
			for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
			{
				if (NrToKeep > 0 && !fromTo.containsKey(transition.getKey()))
				{
					NrToKeep--;fromTo.put(transition.getKey(),transition.getKey());// keep the label and reduce the counter.
				}
				else
					if (!fromTo.containsKey(transition.getKey()))
					{
						if(transition.getKey().startsWith(PrefixNew))
							throw new IllegalArgumentException("there is already a transition with prefix "+PrefixNew+" in the supplied graph");
						fromTo.put(transition.getKey(), PrefixNew+newLabelCnt++);
					}
				newRow.put(fromTo.get(transition.getKey()), transition.getValue());
			}
			newMatrix.put(entry.getKey(), newRow);
		}
		g.transitionMatrix = newMatrix;
	}
	
	/** Adds all states and transitions from graph <em>what</em> to graph <em>g</em>.
	 * Very useful for renumbering nodes on graphs loaded from GraphML and such, because
	 * numerical node IDs are needed for both matrix and W set generation,
	 * <pre>
	 * LearnerGraph grTmp = new LearnerGraph(g.config);
	 * CmpVertex newInit = addToGraph(gr,g);StatePair whatToMerge = new StatePair(g.init,newInit);
	 * LinkedList<Collection<CmpVertex>> collectionOfVerticesToMerge = new LinkedList<Collection<CmpVertex>>();
	 * grTmp.pairscores.computePairCompatibilityScore_general(whatToMerge,collectionOfVerticesToMerge);
	 * LearnerGraph result = MergeStates.mergeAndDeterminize_general(grTmp, whatToMerge,collectionOfVerticesToMerge);
	 * WMethod.computeWSet(result);
	 * </pre>
	 * @param g target into which to merge what
	 * @param what graph to merge into g.
	 * @return vertex in g corresponding to the initial vertex in what 
	 */ 
	public static CmpVertex addToGraph(LearnerGraph g, LearnerGraph what)
	{
		Map<CmpVertex,CmpVertex> whatToG = new HashMap<CmpVertex,CmpVertex>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
		{
			CmpVertex newVert = LearnerGraph.generateNewCmpVertex(g.nextID(entry.getKey().isAccept()), g.config);
			newVert.setAccept(entry.getKey().isAccept());
			newVert.setHighlight(entry.getKey().isHighlight());
			newVert.setColour(entry.getKey().getColour());
			whatToG.put(entry.getKey(),newVert);
		}
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
		{
			Map<String,CmpVertex> row = new TreeMap<String,CmpVertex>();g.transitionMatrix.put(whatToG.get(entry.getKey()),row);
			for(Entry<String,CmpVertex> transition:entry.getValue().entrySet())
				row.put(transition.getKey(), whatToG.get(transition.getValue()));
		}					
		return whatToG.get(what.init);
	}

	public static void addToGraph_tmp(LearnerGraph g, List<String> initPath, LearnerGraph what)
	{
		PTASequenceEngine engine = new PTASequenceEngine();
		engine.init(new PTASequenceSetAutomaton());		
		SequenceSet initSet = engine.new SequenceSet();initSet.setIdentity();initSet.crossWithSequence(initPath);
		
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:what.transitionMatrix.entrySet())
		{// for all states in our new machine, compute paths to them.
			SequenceSet result = engine.new SequenceSet();
			what.paths.computePathsSBetween(what.init, entry.getKey(), initSet, result);
			result.crossWithSet(entry.getValue().keySet());
		}
		
		// Now engine contains a sort of a transition cover of "what", except that only 
		// transitions present in "what" will be covered. All sequences in this set have
		// initPath prepended to them, so that I can merge the result into g directly.
		g.paths.augmentPTA(engine);
	}
	
	/** Given a state and a W set, computes a map from those sequences to booleans representing
	 * whether those sequences to true/false depending whether a specific can be followed from
	 * the given state. 
	 * @param state the state to compute applicability of paths from
	 * @param wSet the set of sequences to manipulate
	 * @return a list of booleans representing applicability of sequences.
	 */
	public static List<Boolean> wToBooleans(LearnerGraph g, CmpVertex state, Collection<List<String>> wSet)
	{
		List<Boolean> result = new LinkedList<Boolean>();
		for(List<String> seq:wSet)
			result.add(g.paths.tracePath(seq,state) == RPNIBlueFringeLearner.USER_ACCEPTED);
		return result;
	}
	
	Set<CmpVertex> fragileStates = new HashSet<CmpVertex>();
	
	/** Computes Hamming distances between elements of a W set and outputs 
	 * the description of the results, potentially including a lot of statistical information. 
	 * 
	 * @param produceStatistics whether to output lots of details.
	 * @return Hamming distances between vectors corresponding to responses of states 
	 * of our graph to sequences in the W set.
	 */
	public String ComputeHamming(boolean produceStatistics)
	{
		List<List<String>> wSet = new LinkedList<List<String>>();wSet.addAll(WMethod.computeWSet_reducedmemory(coregraph));
		Map<CmpVertex,List<Boolean>> bitVector = new TreeMap<CmpVertex,List<Boolean>>();
		for(Entry<CmpVertex,Map<String,CmpVertex>> state:coregraph.transitionMatrix.entrySet())
			bitVector.put(state.getKey(),wToBooleans(coregraph,state.getKey(), wSet));
		int min=Integer.MAX_VALUE,max=0;double average = 0;
		Map<Integer,AtomicInteger> statistics = new HashMap<Integer,AtomicInteger>();
		Object stateToBitVector[] = bitVector.entrySet().toArray();
		for(int i=0;i< stateToBitVector.length;++i)
			for(int j=i+1;j<stateToBitVector.length;++j)
			{
				Entry<CmpVertex,List<Boolean>> vecI = (Entry<CmpVertex,List<Boolean>>) stateToBitVector[i],vecJ = (Entry<CmpVertex,List<Boolean>>)stateToBitVector[j];
				int h = HammingDistance(vecI.getValue(), vecJ.getValue());
				average+=h;
				if (min > h) min = h;
				if (max < h) max = h;
				AtomicInteger atomicH = statistics.get(h);if (atomicH == null) { atomicH = new AtomicInteger(1);statistics.put(h, atomicH); } else atomicH.addAndGet(1);
				if (h == 1) fragileStates.add(vecI.getKey());
			}
		String result =" ONE:"+statistics.get(1)+" ";
		for(Entry<Integer,AtomicInteger> pair:statistics.entrySet()) result+=" "+pair.getKey()+":"+pair.getValue();
		result+="\n";
		for(CmpVertex fragile:fragileStates) result+=" "+fragile;result+="\n";
		int counter =  bitVector.size()*(bitVector.size()-1)/2;
		String basicInfo = "Hamming distances min: "+min+" max: "+max;
		result+="\n"+basicInfo+" average: "+(average/counter);
		return produceStatistics?result:basicInfo;
	}

	/** Takes a graph and outputs vectors corresponding to responses of states 
	 * of our graph to sequences in the W set.
	 * 
	 * @param g the graph which states to examine
	 * @param wSet the W set to compute the response of g to.
	 */
	public static String getVectors(LearnerGraph g, Collection<List<String>> wSet)
	{
		String result = "";
		for(Entry<CmpVertex,Map<String,CmpVertex>> state:g.transitionMatrix.entrySet())
			result+="\n"+wToBooleans(g,state.getKey(), wSet);
		result+="\n";
		return result;
	}
	
	/** A graph may be completely random or built with a specific distribution of labels in mind.
	 * The likelyhood that a label will be used on a transition from some state is referred to
	 * as a <em>fill factor</em>. You can think of a graph as a glass which is filled with 
	 * transitions, but the idea is to distribute them such that each state has the same number 
	 * of outgoing and incoming transitions. This is the reason why populating a graph is compared to
	 * filling a glass with water - transitions are added in "levels".<p>
	 * A W set may consist of singleton sequences (a very common case), in this case we may compute
	 * a restriction of g to a subset of alphabet, using only inputs used in W. This function 
	 * computes a fill factor of the resulting graph - even if the original graph was built well,
	 * the fill factor of the considered subgraph may happen to be rather different.
	 *  
	 * @param g the graph to restrict
	 * @param wSet a collection of singleton sequences to restrict g to.
	 * @return the fill factor of the restriction.
	 */
	public static double getEffectiveFillRate(LearnerGraph g, Collection<List<String>> wSet)
	{
		int positives=0;
		for(Entry<CmpVertex,Map<String,CmpVertex>> state:g.transitionMatrix.entrySet())
			for(Boolean b:wToBooleans(g,state.getKey(), wSet))
				if (b.booleanValue()) ++positives;
		return ((double)positives)/(g.getStateNumber()*wSet.size());
	}
	
	// grep SUCCESS *|awk -F, 'BEGIN {a=0;count=0;} { a+=$3;++count;} END{print a/count;}'
	
	/** Adds all possible transitions to a graph and computes the likelyhood that the original
	 * W will not work in a modified graph. The implementation only adds transitions 
	 * with labels from the W set (all the other labels will not cause a W set to fail
	 * to distinguish states) and only adds loopback transitions in each state (target
	 * state does not affect applicability of W consisting of singleton sequences which
	 * is the only case considered below and an exception will be thrown if W fails to 
	 * consist of singletons).
	 * 
	 *  @return description of the results.
	 */ 
	public String checkWChanged()
	{
		String result = "";
		Collection<List<String>> wSet = WMethod.computeWSet_reducedmemory(coregraph);
		Set<String> Walphabet = new HashSet<String>();
		for(List<String> wSeq:wSet)
		{
			if (wSeq.size() != 1)
				throw new IllegalArgumentException("non-singleton W");
			Walphabet.add(wSeq.iterator().next());
		}
		Collection<String> alphabet = coregraph.wmethod.computeAlphabet();
		double fillFactor = getEffectiveFillRate(coregraph, wSet);//transitionsFromEveryState/alphabet.size();
		result+=getVectors(coregraph, wSet);
		double average = (1-fillFactor)*wSet.size()*coregraph.getStateNumber();
		int changeNumber = 0, total =0;
		Map<String,AtomicInteger> labelUsage = new HashMap<String,AtomicInteger>();for(String l:alphabet) labelUsage.put(l, new AtomicInteger());
		for(Entry<CmpVertex,Map<String,CmpVertex>> entry:coregraph.transitionMatrix.entrySet())
		{
			Collection<String> newLabels = new HashSet<String>();newLabels.addAll(Walphabet);newLabels.removeAll(entry.getValue().keySet());
			int changesForThisState = 0;
			
			for(String lbl:entry.getValue().keySet()) labelUsage.get(lbl).addAndGet(1);
			
			for(String label:newLabels)
			{
				LearnerGraph newGraph = coregraph.copy(coregraph.config);
				CmpVertex currState = newGraph.findVertex(entry.getKey().getID());
				newGraph.transitionMatrix.get(currState).put(label, currState);
				String description = newGraph.wmethod.checkW_is_corrent_boolean(wSet);
				boolean changed = (description != null);
/*
				for(Entry<CmpVertex,Map<String,CmpVertex>> state:graph.transitionMatrix.entrySet())
				{
					LearnerGraph aGraph = graph.copy(graph.config);
					CmpVertex aState = aGraph.findVertex(entry.getKey().getName());
					aGraph.transitionMatrix.get(aState).put(label, aGraph.findVertex(state.getKey().getName()));
					if (changed != (aGraph.wmethod.checkW_is_corrent_boolean(wSet) != null))
						throw new IllegalArgumentException("inconsistent W set results");
				}
*/
				if (changed)
					++changesForThisState;
				++total;
			}
			changeNumber+=changesForThisState;
			result+="changes for "+entry.getKey().getID().toString()+" "+changesForThisState+" (max "+newLabels.size()+"), max for add/remove is "+Walphabet.size()+"\n";
		}
		double stateNumber = coregraph.getStateNumber();
		double wsize = wSet.size();
		double expectedNrOfChanges = wsize*2*fillFactor*(1-fillFactor)*Math.pow(fillFactor*fillFactor+(1-fillFactor)*(1-fillFactor), wsize-1)*
			stateNumber*(stateNumber-1)/2;
		result+="Distribution of labels: ";for(Entry<String,AtomicInteger> en:labelUsage.entrySet()) result+=" "+en.getValue();result+="\n";
		result+="Distribution of elements of W: ";for(String wElem:Walphabet) result+=" "+labelUsage.get(wElem);result+="\n";
		return Math.abs(expectedNrOfChanges-changeNumber)/changeNumber+"\n"+result+"W size: "+wSet.size()+" W changes: "+changeNumber+ " out of "+total+" (expected "+average+"), \nfill factor is "+fillFactor+"\n "+
			"Expected number of changes is: "+expectedNrOfChanges
		;
	}
	
}
