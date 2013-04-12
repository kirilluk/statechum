/* Copyright (c) 2012 Neil Walkinshaw and Kirill Bogdanov
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

package statechum.analysis.learning.experiments;

// Options to run this program,
// JVM options: -ea -DVIZ_CONFIG=kirill_office -Dthreadnum=2 -Djava.library.path=linear/.libs;smt/.libs;"C:/Program Files/R/R-2.15.1/library/rJava/jri/x64"  -Xmx1500m
// Program arguments: C:\experiment\workspace\ModelInferenceUAS\seed1_d.txt
// C:\experiment\workspace\ModelInferenceUAS\seed2_d.txt
// C:\experiment\workspace\ModelInferenceUAS\seed3_d.txt
// C:\experiment\workspace\ModelInferenceUAS\seed4_d.txt

// Install R,
// run 
//  install.packages(c("JavaGD","rJava","aplpack"))
// from R's console,

// Replace jri.jar and javaGD.jar in statechum's lib directory with those from R tool,
// C:\Program Files\R\R-2.15.1\library\rJava\jri\jri.jar
// C:\Program Files\R\R-2.15.1\library\JavaGD\java\javaGD.jar
//
// Add the following to the path,
// C:\Program Files\Java\jre6\bin\server;C:\Program Files\R\R-2.15.1\bin\x64

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Stack;
import java.util.Map.Entry;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairOfPaths;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.rpnicore.PathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.Transform.InternStringLabel;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Helper;
import statechum.Label;
import statechum.Pair;
import statechum.ProgressIndicator;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.experiments.PaperUAS.TracesForSeed.Automaton;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.linear.GD;
import statechum.analysis.learning.linear.GD.ChangesCounter;
import statechum.analysis.learning.observers.DummyLearner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.apps.QSMTool;
import statechum.apps.QSMTool.TraceAdder;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

public class PaperUAS 
{
	/** All traces, maps a seed to a collection of traces for the specific seed. */
	protected Map<String,TracesForSeed> collectionOfTraces = new TreeMap<String,TracesForSeed>();
	
	/** The maximal frame number encountered. */
	protected int maxFrameNumber = -1;
	
	/** When processing graphs with numerous data points, the task is to cluster those points so as not to have too many of them
	 * on the graphs. The easiest way is to divide a frame number by the maximal number so as to get %%.
	 */
	protected int divisor=1;
	
	/** Data recorded for each seed. */
	public static class TracesForSeed
	{
	    Map<String,Map<Integer,Set<List<Label>>>> collectionOfPositiveTraces = new TreeMap<String,Map<Integer,Set<List<Label>>>>();
	    Map<String,Map<Integer,Set<List<Label>>>> collectionOfNegativeTraces = new TreeMap<String,Map<Integer,Set<List<Label>>>>();

	    public Map<String,Map<Integer,PTASequenceEngine>> tracesForUAVandFrame = null;
	    
	    /** Whether a state is deemed accept or reject is used to determine whether a sequence being added to PTASequenceSet will be extended or will terminate. 
	     * Here we accumulate all sequences so {@link #isAccept(Object) isAccept} should always return true. In contrast, the {@link #shouldBeReturned(Object) shouldBeReturned} method is only used 
	     * on states associated with tail nodes. It should return true or false depending on whether a sequence is supposed to be considered accept or reject
	     * by the {@link PathRoutines#augmentPTA(PTASequenceEngine)} method.  
	     *
	     */
	    public static class Automaton extends PTASequenceSetAutomaton
		{
	    	private boolean acceptValue = true;
	    	
	    	public void setAccept(boolean value)
	    	{
	    		acceptValue = value;
	    	}
	    	
			@Override
			public Object getTheOnlyState() {
				return acceptValue?Boolean.TRUE:Boolean.FALSE;
			}
			
			@Override
			public boolean shouldBeReturned(Object elem) {
				return elem != null && ((Boolean)elem).booleanValue();
			}
		}
	    
	    /** Used to construct long traces from the supplied traces, for an experiment to check whether we can efficiently learn
	     * from single long traces for a UAV. 
	     */
	    Map<String,List<Label>> lastPointOnTrace = null;
	    
	    /** Last encountered timeframe for a specific UAV. */
	    Map<String,Integer> maxFrameNumber=new TreeMap<String,Integer>();
	}
	
	
    /** Updates the collection of traces for a specific UAV with a new trace. */
    protected static void addTraceToUAV(String UAV, int frame, List<Label> trace,Map<String,Map<Integer,Set<List<Label>>>> collectionOfTraces)
    {
       	Map<Integer,Set<List<Label>>> UAVdetails = collectionOfTraces.get(UAV);
    	if (UAVdetails == null)
    	{
    		UAVdetails = new TreeMap<Integer,Set<List<Label>>>();collectionOfTraces.put(UAV,UAVdetails);
    	}
   	
    	Set<List<Label>> traces = UAVdetails.get(frame);
    	if (traces == null)
    	{
    		traces = new HashSet<List<Label>>();UAVdetails.put(frame, traces);
    	}
    	
    	traces.add(trace);
    }
    
    public static final String UAVAll = "All", UAVAllSeeds="AllS";
        
    private static final int lexSimulatedTimestamp=1;
    private static final int lexUAV=2;
    private static final int lexSeed=3;
    private static final int lexTrace=4;
    
    Lexer lexer = new Lexer("(\\w+)\\s*,\\s*(\\w+)\\s*,\\s*(\\w+)\\s*,\\s*(.*)");

    /** Loads traces from the file and returns the maximal frame number.
     *  
     * @param inputData
     * @param config
     */
    public int getMaxFrame(final Reader []inputData)
    {
    	final AtomicInteger maxFrame=new AtomicInteger(-1);
        scanData(inputData, new HandleOneTrace() {
			
			@Override
			public void process(final int frame, @SuppressWarnings("unused") final String UAV, @SuppressWarnings("unused") final String seed) {
				if (maxFrame.get() < frame)
					maxFrame.getAndSet(frame);
			}
        });
        return maxFrame.get();
    }            
    
    public interface HandleOneTrace
    {
    	void process(int frame, String UAV, String seed);
    }
    
    public void loadData(final Reader inputData)
    {
    	loadData(new Reader[]{inputData});
    }
    
    /** Loads traces from the file into the pair of positive/negative maps,
     * parameterised by UAV and timeframe.
     *  
     * @param inputData data to load
     */
    public void loadData(final Reader []inputData)
    {
        final Map<String,TracesForSeed> data = new TreeMap<String,TracesForSeed>();
        final Set<String> UAVs = new TreeSet<String>();UAVs.add(UAVAll);UAVs.add(UAVAllSeeds);
        final Set<Integer> frameNumbers = new TreeSet<Integer>();
        
        scanData(inputData, new HandleOneTrace() {
			
			@Override
			public void process(final int frame, final String UAV, final String seed) {
	           	final int frameNumber = frame/divisor;
            	if (!data.containsKey(UAVAllSeeds))
            		data.put(UAVAllSeeds,new TracesForSeed());
             	TracesForSeed dataForSeedTmp = data.get(seed);
             	if (dataForSeedTmp == null)
             	{
             		dataForSeedTmp = new TracesForSeed();data.put(seed,dataForSeedTmp);
             	}
             	final TracesForSeed dataForSeed = dataForSeedTmp;

             	if (!dataForSeed.maxFrameNumber.containsKey(UAV))
             		dataForSeed.maxFrameNumber.put(UAV,-1);
            	
            	// This is a consistency check which is unnecessary for analysis but could be useful to catch problems with logging.
            	if (frameNumber < dataForSeed.maxFrameNumber.get(UAV))
            		throw new IllegalArgumentException("current frame number "+frameNumber+", previous one "+dataForSeed.maxFrameNumber.get(UAV));
             	if (UAV.equals(UAVAll) || UAV.equals(UAVAllSeeds))
             		throw new IllegalArgumentException("UAV name cannot be \""+UAVAll+"\"");
            	if (seed.equals(UAVAllSeeds))
             		throw new IllegalArgumentException("seed name cannot be \""+UAVAll+"\"");
              	dataForSeed.maxFrameNumber.put(UAV, frameNumber);
             	UAVs.add(UAV);frameNumbers.add(frameNumber);
             	if (frameNumber > maxFrameNumber) maxFrameNumber = frameNumber;
             	
            	QSMTool.parseSequenceOfTraces(lexer.group(lexTrace),learnerInitConfiguration.config, new TraceAdder() {

    				@Override
    				public void addTrace(List<Label> traceArg, boolean positive) 
    				{
    					final List<Label> trace = internTrace(traceArg);
    					if (positive)
    					{
	    					addTraceToUAV(UAVAllSeeds,frameNumber,trace,data.get(UAVAllSeeds).collectionOfPositiveTraces);
	    					addTraceToUAV(UAVAll,frameNumber,trace,dataForSeed.collectionOfPositiveTraces);
	    					addTraceToUAV(UAV,frameNumber,trace,dataForSeed.collectionOfPositiveTraces);
    					}
    					else
    					{
	    					addTraceToUAV(UAVAllSeeds,frameNumber,trace,data.get(UAVAllSeeds).collectionOfNegativeTraces);
    						addTraceToUAV(UAVAll,frameNumber,trace,dataForSeed.collectionOfNegativeTraces);
    						addTraceToUAV(UAV,frameNumber,trace,dataForSeed.collectionOfNegativeTraces);
    					}
    				}
            		
            	});
			}
		});
		
		
      	constructSequencesForAllUAVandFrame(data, UAVs, frameNumbers);
   }

   protected void AddLastPositiveTrace(String seed, String UAV, Map<String,TracesForSeed> data)
   {
   		TracesForSeed dataForSeed = data.get(seed);
   		Integer lastFrameNumberInteger = dataForSeed.maxFrameNumber.get(UAV);
   		if (lastFrameNumberInteger == null && dataForSeed.collectionOfNegativeTraces.containsKey(UAV))
   			throw new IllegalArgumentException("no max frame number for UAV "+UAV+" when seed is "+seed);
   		if (lastFrameNumberInteger != null)
   		{
			int lastFrameNumber = lastFrameNumberInteger.intValue();
			List<Label> currentLastTrace = dataForSeed.lastPointOnTrace.get(UAV);
			final List<Label> lastPositiveTrace = new ArrayList<Label>(currentLastTrace.size());lastPositiveTrace.addAll(currentLastTrace);// make a copy because the positive trace constantly gets added to
			addTraceToUAV(UAVAllSeeds,lastFrameNumber,lastPositiveTrace,data.get(UAVAllSeeds).collectionOfPositiveTraces);
			addTraceToUAV(UAVAll,lastFrameNumber,lastPositiveTrace,dataForSeed.collectionOfPositiveTraces);
			addTraceToUAV(UAV,lastFrameNumber,lastPositiveTrace,dataForSeed.collectionOfPositiveTraces);
   		}
   }

   /** Number to be assigned to the next generated label. */
   int labelNumber=0;
   
   InternStringLabel labelConverter = new InternStringLabel();
   
   /** Converts all elements in a trace to those in the alphabet. Where a label is seen that is not the same as an already known one, it is added to the alphabet. If there is 
    * an identical one, the known one is used.
    * 
    * @param trace trace to intern
    */
   public List<Label> internTrace(List<Label> trace)
   {
	   if (trace.isEmpty())
		   return trace;
	   
	   List<Label> outcome = new ArrayList<Label>();
	   for(Label l:trace)
		   outcome.add(labelConverter.convertLabelToLabel(l));
	   
	   return outcome;
   }
   
    /** Loads traces from the file into the pair of positive/negative maps,
     * parameterised by UAV and timeframe. The process is to assume a specific starting point and concatenate the following 
     * negative traces with it, until we meet a positive trace. After concatenation of that positive trace, we have a revised starting point.
     * Every time a new timeframe is encountered, record the old starting point trace and start appending new traces to it. 
     *  
     * @param inputData data to load
     * @param config configuration to use
     */
    public void loadDataByConcatenation(final Reader []inputData)
    {
        final Map<String,TracesForSeed> data = new TreeMap<String,TracesForSeed>();
        final Set<String> UAVs = new TreeSet<String>();UAVs.add(UAVAll);UAVs.add(UAVAllSeeds);
        final Set<Integer> frameNumbers = new TreeSet<Integer>();

        if (!data.containsKey(UAVAllSeeds))
    	{
    		TracesForSeed dataForSeedTmp = new TracesForSeed();data.put(UAVAllSeeds,dataForSeedTmp);dataForSeedTmp.lastPointOnTrace=new TreeMap<String,List<Label>>();
    	}
         
        scanData(inputData, new HandleOneTrace() {
			
			@Override
			public void process(final int frame, final String UAV, final String seed) {
	           	final int frameNumber = frame/divisor;
            	
            	TracesForSeed dataForSeedTmp = data.get(seed);
             	if (dataForSeedTmp == null)
             	{
             		dataForSeedTmp = new TracesForSeed();data.put(seed,dataForSeedTmp);dataForSeedTmp.lastPointOnTrace=new TreeMap<String,List<Label>>();
             	}
             	final TracesForSeed dataForSeed = dataForSeedTmp;

             	if (UAV.equals(UAVAll) || UAV.equals(UAVAllSeeds))
             		throw new IllegalArgumentException("UAV name cannot be \""+UAVAll+"\"");
            	if (seed.equals(UAVAllSeeds))
             		throw new IllegalArgumentException("seed name cannot be \""+UAVAll+"\"");

            	if (!dataForSeed.maxFrameNumber.containsKey(UAV))
             	{// add an entry for this UAV to both maxFrameNumber and lastPointOnTrace
             		dataForSeed.maxFrameNumber.put(UAV,-1);dataForSeed.lastPointOnTrace.put(UAV, new ArrayList<Label>());
             		
             	}
            	
            	// This is a consistency check which is unnecessary for analysis but could be useful to catch problems with logging.
            	if (frameNumber < 0)
            		throw new IllegalArgumentException("current frame number "+frameNumber+" is invalid");
                if (frameNumber < 0 || frameNumber < dataForSeed.maxFrameNumber.get(UAV))
            		throw new IllegalArgumentException("current frame number "+frameNumber+", previous one "+dataForSeed.maxFrameNumber.get(UAV));

            	final List<Label> lastPositiveTrace = dataForSeed.lastPointOnTrace.get(UAV);
            	if (frameNumber > dataForSeed.maxFrameNumber.get(UAV) && dataForSeed.maxFrameNumber.get(UAV) >=0)
            	// new frame started, dump the last positive sequence
            		AddLastPositiveTrace(seed, UAV, data);
            	
              	dataForSeed.maxFrameNumber.put(UAV, frameNumber);
             	UAVs.add(UAV);frameNumbers.add(frameNumber);
        
             	if (frameNumber > maxFrameNumber) maxFrameNumber = frameNumber;
             	
            	QSMTool.parseSequenceOfTraces(lexer.group(lexTrace),learnerInitConfiguration.config, new TraceAdder() {

    				@Override
    				public void addTrace(List<Label> traceArg, boolean positive) 
    				{
    					final List<Label> trace = internTrace(traceArg);
    					if (positive)
    					{// The last element should be the same as the starting element in the positive trace, if there is one.
    						int traceLen = trace.size();
    						if (traceLen < 2)
    							throw new IllegalArgumentException("traces are expected to loop in the initial state, hence they should contain at least two elements");
    						Label lastElement = trace.get(traceLen-1);
    						if (!lastElement.equals(trace.get(0)))
    							throw new IllegalArgumentException("the last element of each positive trace is not the same as the starting element of a the same trace for trace "+trace);
    						if (!lastPositiveTrace.isEmpty() && !lastPositiveTrace.get(0).equals(lastElement))
    							throw new IllegalArgumentException("the first element of the last positive trace ("+lastPositiveTrace.get(0)+") is not the same as the starting element of the current trace "+trace);
    						
    						lastPositiveTrace.addAll(trace.subList(0, traceLen-1));// we do not append it to a collection of traces here because it will be done when we hit a new frame
    					}
    					else
    					{
    						List<Label> negativeTrace = new ArrayList<Label>(lastPositiveTrace.size()+trace.size());negativeTrace.addAll(lastPositiveTrace);negativeTrace.addAll(trace);
	    					addTraceToUAV(UAVAllSeeds,frameNumber,negativeTrace,data.get(UAVAllSeeds).collectionOfNegativeTraces);
    						addTraceToUAV(UAVAll,frameNumber,negativeTrace,dataForSeed.collectionOfNegativeTraces);
    						addTraceToUAV(UAV,frameNumber,negativeTrace,dataForSeed.collectionOfNegativeTraces);
    					}
    				}
            		
            	});
			}
		});
		
        // This one adds the last positive trace from the last frame. It would not be added otherwise because the above loop is looking for a new frame which will not appear 
        // once they have all been dealt with.
        for(String seed:data.keySet())
        	if (!seed.equals(UAVAllSeeds))
	        for(String UAV:UAVs)
	        	if (!UAV.equals(UAVAll) && !UAV.equals(UAVAllSeeds))
	        		AddLastPositiveTrace(seed, UAV, data);

      	constructSequencesForAllUAVandFrame(data, UAVs, frameNumbers);
   }
    

    protected void constructSequencesForAllUAVandFrame(Map<String,TracesForSeed> data,Set<String> UAVs, Set<Integer> frameNumbers)
    {
    	collectionOfTraces.clear();
    	lexer=null;
    	for(Entry<String,TracesForSeed> entry:data.entrySet())
    	{
    		TracesForSeed traceDetails = entry.getValue(), newData = new TracesForSeed();
    		collectionOfTraces.put(entry.getKey(), newData);
    		newData.tracesForUAVandFrame=new TreeMap<String,Map<Integer,PTASequenceEngine>>();
    		
    		turnTracesIntoPTAs(newData.tracesForUAVandFrame,traceDetails.collectionOfPositiveTraces,true,frameNumbers,UAVs);
    		newData.collectionOfPositiveTraces=null;
    		
    		turnTracesIntoPTAs(newData.tracesForUAVandFrame,traceDetails.collectionOfNegativeTraces,false,frameNumbers,UAVs);
   		
    		newData.collectionOfNegativeTraces=null;
    		
    		//System.out.println(newData.tracesForUAVandFrame.size())
    		
    		if (!entry.getKey().equals(UAVAllSeeds))
    			for(String UAV:UAVs)
    				if (!UAV.equals(UAVAll) && !UAV.equals(UAVAllSeeds) && traceDetails.lastPointOnTrace != null && traceDetails.lastPointOnTrace.containsKey(UAV)) 
    					System.out.println("Seed: "+entry.getKey()+", UAV: "+UAV+" length: "+traceDetails.lastPointOnTrace.get(UAV).size());
    		
    	}
    	
    	int maxFrame = -1;
    	for(int f:frameNumbers) if (maxFrame < f) maxFrame=f;
    	int estimatedMaxNumberOfStates = 1000;
    	if (maxFrame >= 0)		
    	{        	
	    	if (collectionOfTraces.containsKey(UAVAllSeeds) && collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame != null)
	    		estimatedMaxNumberOfStates = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrame).getSize()*2;
    	}
    	//learnerInitConfiguration.config.setMaxStateNumber(estimatedMaxNumberOfStates);// we're using Array-based collections now, no point setting this high.
    	System.out.println("Highest frame number : "+maxFrame+", estimated max number of states : "+estimatedMaxNumberOfStates);
    }
    
    /** Loads traces from the file, calling the user-supplied observer for every line.
     *  
     * @param inputData data to load
     * @param loader called for each line
     */
    public void scanData(Reader []inputData,HandleOneTrace loader)
    {
        BufferedReader in = null;
       
        try 
        {
        	for(Reader rd:inputData)
        	{
	            in = new BufferedReader(rd);
	            String fileString;
	            
	            
	            while ((fileString = in.readLine()) != null) {
	            	lexer.startParsing(fileString);
	            	int match = lexer.getMatchType();
	            	if (match != 1)
	            		throw new IllegalArgumentException("invalid match");
	            	final String timeSimulated=lexer.group(lexSimulatedTimestamp),
	            		//timeReal=lexer.group(lexRealTimestamp),
	            		UAV=lexer.group(lexUAV),seed=lexer.group(lexSeed);
	
	            	final int frameNumber = Integer.parseInt(timeSimulated);
	            	loader.process(frameNumber, UAV, seed);
	             }
            
        	}
        } catch (IOException e) {
            statechum.Helper.throwUnchecked("failed to read learner initial data", e);
        } finally {
           if (in != null) { try { in.close();in=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
        }

    }
    
    /** UAV(frame) should be an a sum of data across all frames before the specific frame.
     * Such a calculation is done after we load all data because frames from different UAVs might
     * not be synchronized and we need to accumulate data across all of them for UAVAll.
     *   
     * @param whatToUpdate collection of traces to add to
     * @param traces traces to add
     * @param isAccept whether traces to be added are accept or reject-traces
     * @param frameNumbers all possible frames, to ensure that all UAV frame numbers range over the same set.
     * @param UAVs names of all UAVs, used to ensure that all UAV frame numbers range over the same set.
     */
    protected void turnTracesIntoPTAs(Map<String,Map<Integer,PTASequenceEngine>> whatToUpdate,
    		Map<String,Map<Integer,Set<List<Label>>>> traces, boolean isAccept, Set<Integer> frameNumbers,Set<String> UAVs)
	{
        for(String uav:UAVs)
        {
        	
        	Map<Integer,PTASequenceEngine> outcomeUAV = whatToUpdate.get(uav);
        	if (outcomeUAV == null)
        	{
        		outcomeUAV = new TreeMap<Integer,PTASequenceEngine>();whatToUpdate.put(uav, outcomeUAV);
        	}
        	
        	for(Integer frame:frameNumbers)
        	{
        		PTASequenceEngine traceDetailsUAV = outcomeUAV.get(frame);
        		if (traceDetailsUAV == null)
        		{
        			traceDetailsUAV = new PTASequenceEngine(true);
        			traceDetailsUAV.init(new Automaton());
        			outcomeUAV.put(frame, traceDetailsUAV);
        		}
        		
        		if (traces.containsKey(uav))
        		{
        			for(int earlierFrame:frameNumbers)
        			{
        				if (earlierFrame>frame)
        					break;

        				Set<List<Label>> traceData = traces.get(uav).get(earlierFrame);
	        			
	        			if (traceData != null)
	        			{
		        			SequenceSet initSeq = traceDetailsUAV.new SequenceSet();initSeq.setIdentity();((Automaton)traceDetailsUAV.getFSM()).setAccept(isAccept);
		        			initSeq.cross(traceData);
	        			}
       				}
        		}
        	}
        }
        
	}
    
    public LearnerEvaluationConfiguration learnerInitConfiguration = new LearnerEvaluationConfiguration(null);
   
    public static final int pairchoiceMIN=-2, pairchoiceMAX=-1, pairchoiceORIG=-3;

    /** Counts the number with the same score at the top of the stack. */
    public static int countChoices(Stack<PairScore> stack)
    {
    	if (stack.isEmpty())
    		return 0;
    	int outcome = 1;
    	PairScore top = stack.peek();
    	
    	int i=stack.size()-2;
    	while(i>=0)
    	{
    		PairScore curr = stack.get(i);--i;
    		if (curr.getScore() != top.getScore() || curr.getR() != top.getR() )
    			break;
    		++outcome;
    	}
    	return outcome;
    }
    
    
    /** Returns the pair corresponding to the smallest or largest machine for one of the pairs with the same score at the top of the stack. */ 
    public static PairScore selectPairMinMax(LearnerGraph graph, Stack<PairScore> stack, int pairChoice)
    {
    	PairScore top = stack.peek();
    	int value = MergeStates.mergeAndDeterminize(graph, top).getStateNumber();
    	int i=stack.size()-2;
    	while(i>=0)
    	{
    		PairScore pair = stack.get(i);--i;
    		if (pair.getScore() != top.getScore()) break;
    		int stateNumber = MergeStates.mergeAndDeterminize(graph, pair).getStateNumber();
    		switch(pairChoice)
    		{
    		case pairchoiceMIN:
    			if (stateNumber < value)
    			{
    				top=pair;value=stateNumber;
    			}
    			break;
    		case pairchoiceMAX:
    			if (stateNumber > value)
    			{
    				top=pair;value=stateNumber;
    			}
    			break;
    		default:
    			throw new IllegalArgumentException("invalid argument "+pairChoice);
    		}
    	}
    	return top;
    }
    
    /** Picks a pair at the top of the stack at random. */
    public static PairScore selectPairAtRandom(Stack<PairScore> stack, Random rnd)
    {
    	PairScore top = stack.get(stack.size()-1-rnd.nextInt(countChoices(stack)));
    	assert top.getScore() == stack.peek().getScore();
    	return top;
    }
    
    /** Given a graph, leaves only the states that are connected to the root state with paths of length at most the specified number.
     * Very useful to visualise parts of complex graphs where Jung will take forever to run it spring-based algorithm and all states will be pushed towards the edges of the 
     * window.
     * @param graph graph to trim
     * @param pathsToLeave the maximal length of paths to root state
     * @return trimmed graph. The original is not modified.
     */
    public static LearnerGraph trimGraphTo(LearnerGraph graph, int pathsToLeave)
    {
		Set<CmpVertex> whatToRemove = new TreeSet<CmpVertex>();
		for(Entry<CmpVertex,LinkedList<Label>> entry:graph.pathroutines.computeShortPathsToAllStates().entrySet())
		{
			if (entry.getValue().size() > pathsToLeave)
				whatToRemove.add(entry.getKey());
		}
		
		LearnerGraph trimmedOne = new LearnerGraph(graph.config);
		AbstractLearnerGraph.copyGraphs(graph, trimmedOne);
		// Since we'd like to modify a transition matrix, we iterate through states of the original machine and modify the result.
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
			if (whatToRemove.contains(entry.getKey())) trimmedOne.transitionMatrix.remove(entry.getKey());// a copied state should be identical to the original one, so doing remove is appropriate 
			else
			{
				Map<Label,CmpVertex> row = trimmedOne.transitionMatrix.get(entry.getKey());
				for(Entry<Label,CmpVertex> targetRow:entry.getValue().entrySet())
					for(CmpVertex target:graph.getTargets(targetRow.getValue()))
						if (whatToRemove.contains(target)) trimmedOne.removeTransition(row, targetRow.getKey(), target);
			}
		return trimmedOne;
    }
    
   	public class RPNIBlueFringe
    {
    	private Learner learner;
    	
    	/** pairchoiceMIN means choose best, pairchoiceMAX means choose worst, everything else is a seed for random generator. */
    	final int pairChoice;
    	final Random rnd;
    	double logOfChoiceNumber=0;
    	final Configuration config;
    	
    	VertexID phantomVertex = null;
    	
		public RPNIBlueFringe(final Configuration conf,int choice) 
		{
			pairChoice=choice;
			final LearnerEvaluationConfiguration bfLearnerInitConfiguration = new LearnerEvaluationConfiguration(conf);bfLearnerInitConfiguration.ifthenSequences = PaperUAS.this.learnerInitConfiguration.ifthenSequences;
			config = bfLearnerInitConfiguration.config;// our config is a copy of the one supplied as an argument.
			config.setAskQuestions(false); 
			if (choice == pairchoiceMAX || choice == pairchoiceMIN) rnd = null;else rnd = new Random(choice);
			
			learner = new DummyLearner(new RPNIUniversalLearner(null, bfLearnerInitConfiguration)) 
			{
				@Override
				public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
				{// Use the old and limited version to compute the merge because the general one is too slow on large graphs and we do not need either to merge arbitrary states or to handle "incompatibles".
					return MergeStates.mergeAndDeterminize(original, pair);
				}


				@Override 
				public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
				{
					System.out.println("Current number of states: "+graph.getStateNumber());
					Stack<PairScore> outcome = decoratedLearner.ChooseStatePairs(graph);
					if (!outcome.isEmpty())
					{
						PairScore selected = null;

						selected = outcome.pop();/*
						while(!outcome.isEmpty())
						{
							PairScore next = outcome.pop();
							if (next.getScore() == selected.getScore() && next.compareInTermsOfDepth(selected) > 0)
								selected = next;
						}
						*/
						/*
						logOfChoiceNumber+=Math.log(countChoices(outcome));
						if (pairChoice == pairchoiceMAX || pairChoice == pairchoiceMIN)
							selected = selectPairMinMax(graph, outcome, pairChoice);
						else
							if (pairChoice == pairchoiceORIG)
								selected = outcome.peek();
						else
							selected = selectPairAtRandom(outcome, rnd);
						PairScore pair2=null;
						if (!outcome.isEmpty())
						{
							pair2 = outcome.pop();
							if (pair2.getScore() == selected.getScore() && pair2.getR() != selected.getR())
							{
								Set<CmpVertex> whatToRemove = new TreeSet<CmpVertex>();
								for(Entry<CmpVertex,LinkedList<Label>> entry:graph.pathroutines.computeShortPathsToAllStates().entrySet())
								{
									if (entry.getValue().size() > 4)
										whatToRemove.add(entry.getKey());
								}
								
								LearnerGraph trimmedOne = new LearnerGraph(graph.config);
								AbstractLearnerGraph.copyGraphs(graph, trimmedOne);
								// Since we'd like to modify a transition matrix, we iterate through states of the original machine and modify the result.
								for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
									if (whatToRemove.contains(entry.getKey())) trimmedOne.transitionMatrix.remove(entry.getKey());// a copied state should be identical to the original one, so doing remove is appropriate 
									else
									{
										Map<Label,CmpVertex> row = trimmedOne.transitionMatrix.get(entry.getKey());
										for(Entry<Label,CmpVertex> targetRow:entry.getValue().entrySet())
											for(CmpVertex target:graph.getTargets(targetRow.getValue()))
												if (whatToRemove.contains(target)) trimmedOne.removeTransition(row, targetRow.getKey(), target);
									}
								Visualiser.updateFrame(trimmedOne, null);System.out.println(selected+" "+pair2);Visualiser.waitForKey();
							}
								
						}*/
						outcome.clear();
						outcome.push(selected);
					}
					return outcome;
				}
		
				@SuppressWarnings("unused")
				@Override 
				public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) 
				{
					throw new IllegalArgumentException("should not be called");
				}
				
				@Override 
				public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
				{
					LearnerGraph graph = decoratedLearner.init(engine,plusSize,minusSize);
					Set<Label> alphabet = graph.pathroutines.computeAlphabet();
					// Create a state to ensure that the entire alphabet is visible when if-then automata are loaded.
					phantomVertex = graph.nextID(true);
					CmpVertex dummyState = AbstractLearnerGraph.generateNewCmpVertex(phantomVertex, bfLearnerInitConfiguration.config);
					Map<Label,CmpVertex> row = graph.createNewRow();
					for(Label lbl:alphabet) graph.addTransition(row, lbl, dummyState);
					graph.transitionMatrix.put(dummyState, row);
				
					for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
						if (!entry.getKey().equals(phantomVertex) && entry.getKey().getDepth() < 0)
							throw new IllegalArgumentException("no depth assigned to vertex "+entry.getKey());
					
					return graph;
				}
			};
	
		}

		public LearnerGraph learn(final PTASequenceEngine engineArg, boolean useNegatives)
		{
			PTASequenceEngine engine = null;
			if (!useNegatives)
			{
				PTASequenceEngine positives = new PTASequenceEngine();positives.init(new Automaton());
    			SequenceSet initSeq = positives.new SequenceSet();initSeq.setIdentity();
    			initSeq.cross(engineArg.getData());
    			engine = positives;
			}
			else
				engine = engineArg;

			LearnerGraph outcome = learner.learnMachine(engine,0,0);
			if (phantomVertex != null) 
				outcome.transitionMatrix.remove(outcome.findVertex(phantomVertex));
			return outcome;
		}
		
    } // RPNIVariabilityExperiment
	
   	public void learnIfThenFromTraces()
   	{
 	   LearnerGraph pta = new LearnerGraph(learnerInitConfiguration.config);
 	   pta.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber));
 	   Map<Label,Map<Label,Double>> outcome = learnIfThen(pta,0.0,0.8);
 	   
 	   for(Entry<Label,Map<Label,Double>> entry:outcome.entrySet())
 		   for(Entry<Label,Double> lblToDouble:entry.getValue().entrySet())
 		   {
 			   System.out.println(entry.getKey()+" "+lblToDouble.getKey()+" , "+lblToDouble.getValue());
 		   }
   	}
   	
   	
   /** Uses a technique describe in David Lo's papers to infer pairwise constraints between events.
    * 
    * @param pta PTA to process.
    * @param thresholdEvent threshold for an event to be considered significant, 0..1.
    * @param thresholdPair threshold for a pair of events to be considered significant, 0..1.
    * @return Map from events to the other component of a pair of events. 
    * Positive values are confidence values that the two events always lead to an accept-state, negative ones are for those leading to reject-states. 
    */
   public static Map<Label,Map<Label,Double>> learnIfThen(LearnerGraph pta,double thresholdEvent,double thresholdPair)
   {
	   long total = 0;
	   
	   Map<Label,Map<Label,Double>> outcome = new TreeMap<Label,Map<Label,Double>>();
	   
	   Map<Label,AtomicInteger> supportForEvent = new TreeMap<Label,AtomicInteger>();
	   Map<Label,Map<Label,AtomicInteger>> supportForPair = new TreeMap<Label,Map<Label,AtomicInteger>>();// positive number means the number of positive pairs, negative is for negatives. Where there is a clash, a zero is used to mean that the pair is inconclusive

	   for(Label lbl:pta.pathroutines.computeAlphabet())
	   {
		   supportForEvent.put(lbl, new AtomicInteger(0));supportForPair.put(lbl, new TreeMap<Label,AtomicInteger>());
	   }
	   
	   for(Entry<CmpVertex,Map<Label,CmpVertex>> state:pta.transitionMatrix.entrySet())
		   for(Entry<Label,CmpVertex> next:state.getValue().entrySet())
		   {
			   Label lbl = next.getKey();
			   supportForEvent.get(lbl).incrementAndGet();++total;
			   
			   if (next.getValue().isAccept())
			   {
				   Map<Label,AtomicInteger> supportStartingFromlbl = supportForPair.get(lbl);
				   for(Entry<Label,CmpVertex> subsequent:pta.transitionMatrix.get(next.getValue()).entrySet())
				   {
					   Label nextLbl = subsequent.getKey();boolean nextAccept = subsequent.getValue().isAccept();
					   AtomicInteger counter = supportStartingFromlbl.get(nextLbl);
					   if (counter == null)
						   supportStartingFromlbl.put(nextLbl, new AtomicInteger(nextAccept?1:-1));
					   else
						   if (counter.get() > 0)
						   {// previously recorded value is positive, that means we've seen positive examples
							   if (nextAccept)
								   counter.incrementAndGet();
							   else
								   counter.set(0);// inconclusive results
						   }
						   else
						   if (counter.get() < 0)
						   {// previously recorded value is negative, that means we've seen negative examples
							   if (nextAccept)
								   counter.set(0);// inconclusive results
							   else
								   counter.decrementAndGet();
						   }
						   
					   }
			   }
		  }
	   
	   if (total > 0)
		   for(Entry<Label,AtomicInteger> lblDetails:supportForEvent.entrySet())
			   if (lblDetails.getValue().get()/(double)total > thresholdEvent)
			   {// enough support for the label
				   for(Entry<Label,AtomicInteger> entry:supportForPair.get(lblDetails.getKey()).entrySet())
				   {
					   double pairSupport = entry.getValue().get()/(double)lblDetails.getValue().get();
					   if (Math.abs(pairSupport)>thresholdPair)
					   {
						   Map<Label,Double> outcomeEntry = outcome.get(lblDetails.getKey());
						   if (outcomeEntry == null)
						   {
							   outcomeEntry = new TreeMap<Label,Double>();outcome.put(lblDetails.getKey(),outcomeEntry);
						   }
						   outcomeEntry.put(entry.getKey(), pairSupport);
					   }
				   }
			   }
	   
	   return outcome;
   }
   	
   
   public void runExperimentWithSingleAutomaton(String name) throws IOException
   {
       final Configuration learnerConfig = learnerInitConfiguration.config.copy();learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
       learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);
        long tmStarted = new Date().getTime();
        LearnerGraph fullGraph =  new RPNIBlueFringe(learnerConfig,pairchoiceORIG).learn(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber),true);
        
        final LearnerGraph graphReference = new LearnerGraph(learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/uas_reference_automaton.xml",graphReference,labelConverter);
   		final Collection<List<Label>> wMethod = graphReference.wmethod.getFullTestSet(1);
   		int wPos=0;
   		for(List<Label> seq:wMethod) if (graphReference.paths.tracePathPrefixClosed(seq) == AbstractOracle.USER_ACCEPTED) wPos++;
   		System.out.println("before rnd: "+wMethod.size()+" sequences, "+wPos+" positives");
   		RandomPathGenerator pathGen = new RandomPathGenerator(graphReference,new Random(0),5,graphReference.getInit());
   		pathGen.generatePosNeg(2*(wMethod.size()-wPos), 1);
   		wMethod.addAll(pathGen.getExtraSequences(0).getData());
   		System.out.println("after rnd: "+wMethod.size()+" sequences");
   		
   		
   		DrawGraphs gr = new DrawGraphs();
		final RBoxPlot<Integer>
				uas_S=new RBoxPlot<Integer>("Time","BCR",new File("time_S_"+name+"_bcr.pdf"));
		Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).keySet();
		ProgressIndicator progress = new ProgressIndicator("UAS", allFrames.size());
		
		Random rnd = new Random(0);
		final int []SelectionChoices=new int [10];
		
		SelectionChoices[0]=pairchoiceORIG;
		for(int i=1;i<SelectionChoices.length;++i) 
		{ 
			SelectionChoices[i]=rnd.nextInt();assert SelectionChoices[i] != pairchoiceMIN && SelectionChoices[i] != pairchoiceMAX && SelectionChoices[i] != pairchoiceORIG; 
		}
		
  		for(final Integer frame:allFrames)
  		{
  			List<LearnerGraph> graphs = new LinkedList<LearnerGraph>();
  			for(int choice:SelectionChoices)
	   		{
  				RPNIBlueFringe learner = new RPNIBlueFringe(learnerConfig,choice);
		        final LearnerGraph actualAutomaton = learner.learn(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame),true);
		        long tmFinished = new Date().getTime();
		        System.out.println("Learning complete, "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
		        ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphReference, actualAutomaton);
		        System.out.println("BCR for frame : "+frame+" = "+matrix.BCR()+", precision: "+matrix.getPrecision()+", recall: "+matrix.getRecall()+", specificity: "+matrix.getSpecificity()+", matrix: "+matrix+", log of choices: "+learner.logOfChoiceNumber);
		        //Visualiser.updateFrame(actualAutomaton, graphReference);
		        /*
		        boolean foundSame = false;
		        for(LearnerGraph g:graphs)
		        	if (WMethod.checkM(g, actualAutomaton) != null)
		        	{
		        		foundSame = true;break;
		        	}
		        if (!foundSame) */graphs.add(actualAutomaton);
		        
				GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
				ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> counter = new ChangesCounter<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>(graphReference, actualAutomaton, null);
				gd.computeGD(graphReference, actualAutomaton, ExperimentRunner.getCpuNumber(),counter,learnerConfig);
				
				int referenceEdges = graphReference.pathroutines.countEdges(), actualEdges = actualAutomaton.pathroutines.countEdges();
				System.out.println(counter.getRemoved()+","+counter.getAdded()+", difference is "+(((double)referenceEdges-counter.getRemoved())/referenceEdges+((double)actualEdges-counter.getAdded())/actualEdges)/2);
				
				uas_S.add(frame,matrix.BCR());
				//uas_S.drawInteractive(gr);
	 		}
  			
  			int count=0;
  			for(LearnerGraph g:graphs)
  				g.storage.writeGraphML("resources/"+name+"_"+frame+"_"+(count++)+".xml");
  			System.out.println("=== "+graphs.size()+" ===");
  			progress.next();
  		}
  		uas_S.drawPdf(gr);
		DrawGraphs.end();// the process will not terminate without it because R has its own internal thread
  }
   	
    public void runExperiment() throws IOException
    {
        final Configuration learnerConfig = learnerInitConfiguration.config.copy();learnerConfig.setGeneralisationThreshold(0);
       /*new LearnerGraph(learnerInitConfiguration.config);
        AbstractPersistence.loadGraph("shorttraceautomaton.xml",graphSolution);*/
		/*
        long tmStarted = new Date().getTime();
        final LearnerGraph graphReference =
        		//new LearnerGraph(learnerInitConfiguration.config);AbstractPersistence.loadGraph("shorttraceautomaton.xml",graphReference);
        	new RPNIBlueFringe(learnerConfig,useOptimizedMerge,null,null).learn(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber),true);
        long tmFinished = new Date().getTime();
        System.out.println("Learning reference complete, "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
        graphReference.storage.writeGraphML("traceautomaton.xml");
        */
        final LearnerGraph graphReference = new LearnerGraph(learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/uas_reference_automaton.xml",graphReference,labelConverter);
		final Collection<List<Label>> wMethod = graphReference.wmethod.getFullTestSet(1);
        
		// Here I need to moderate the effort because choosing traces for all seeds is good but I need
		// that many times more traces, so I have to create a graph in terms of effort v.s. quailty (or even better, scale
		// the existing one).
		DrawGraphs gr = new DrawGraphs();

		final RBoxPlot<Pair<Integer,String>> 
			uas_outcome = new RBoxPlot<Pair<Integer,String>>("Time","BCR",new File("time_bcr.pdf"));
		final RBoxPlot<Integer>
					uas_A=new RBoxPlot<Integer>("Time","BCR",new File("time_A_bcr.pdf")),
							uas_S=new RBoxPlot<Integer>("Time","BCR",new File("time_S_bcr.pdf")),
									uas_U=new RBoxPlot<Integer>("Time","BCR",new File("time_U_bcr.pdf"))
			;
		final RBoxPlot<Integer>
			uas_threshold=new RBoxPlot<Integer>("Threshold","BCR",new File("threshold_bcr.pdf"));

		Random rnd=new Random(0);
		final int []SelectionChoices=new int [10];
		
		SelectionChoices[0]=pairchoiceORIG;
		for(int i=1;i<SelectionChoices.length;++i) 
		{ 
			SelectionChoices[i]=rnd.nextInt();assert SelectionChoices[i] != pairchoiceMIN && SelectionChoices[i] != pairchoiceMAX && SelectionChoices[i] != pairchoiceORIG; 
		}
		
		Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).keySet();

		/** The runner of computational threads. */
		int threadNumber = ExperimentRunner.getCpuNumber();
		ExecutorService executorService = Executors.newFixedThreadPool(threadNumber);
		ProgressIndicator progress = null;
		if (threadNumber <= 1)
			progress = new ProgressIndicator("UAS", allFrames.size());
		
		try
		{
			List<Future<?>> outcomes = new LinkedList<Future<?>>();
			for(final Integer frame:allFrames)
			{
				{// For all frames and all seeds
					Runnable interactiveRunner = new Runnable() {
	
						@Override
						public void run() {
							for(int choice:SelectionChoices)
							{
								ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphReference,
										new RPNIBlueFringe(learnerConfig,choice).learn(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame),true));
								uas_outcome.add(new Pair<Integer,String>(frame,"S"),matrix.BCR());
								uas_S.add(frame,matrix.BCR());
							}
						}
						
					};
					if (threadNumber > 1) outcomes.add(executorService.submit(interactiveRunner));else interactiveRunner.run();
				}
				
				for(final String seed:collectionOfTraces.keySet())
					if (!seed.equals(UAVAllSeeds))
					{// Just for all frames of the a single seed
						Runnable interactiveRunner = new Runnable() {

							@Override
							public void run() {
								TracesForSeed tracesForThisSeed = collectionOfTraces.get(seed);
								
								for(int choice:SelectionChoices)
								{
									ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphReference,
											new RPNIBlueFringe(learnerConfig,choice).learn(tracesForThisSeed.tracesForUAVandFrame.get(UAVAll).get(frame),true));
									uas_outcome.add(new Pair<Integer,String>(frame,"A"),matrix.BCR());
									uas_A.add(frame,matrix.BCR());
								}
							}
							
						};
						if (threadNumber > 1) outcomes.add(executorService.submit(interactiveRunner));else interactiveRunner.run();
					}
				for(final String UAV:collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.keySet())
					if (!UAV.equals(UAVAllSeeds) && !UAV.equals(UAVAll))
						for(final String seed:collectionOfTraces.keySet())
							if (!seed.equals(UAVAllSeeds))
							{
								Runnable interactiveRunner = new Runnable() {

									@Override
									public void run() {
										for(int choice:SelectionChoices)
										{
											ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphReference,
													new RPNIBlueFringe(learnerConfig,choice).learn(collectionOfTraces.get(seed).tracesForUAVandFrame.get(UAV).get(frame),true));
											uas_outcome.add(new Pair<Integer,String>(frame,"U"),matrix.BCR());
											uas_U.add(frame,matrix.BCR());
										}
									}
									
								};
								if (threadNumber > 1) outcomes.add(executorService.submit(interactiveRunner));else interactiveRunner.run();
							}
				
				if (threadNumber <= 1)
				{/*
					uas_outcome.drawInteractive(gr);
					uas_A.drawInteractive(gr);
					uas_S.drawInteractive(gr);
					uas_U.drawInteractive(gr);
					*/
					progress.next();
				}
			}
			
			for(int i=0;i<5;++i)
			{
				final int arg=i;
				Runnable interactiveRunner = new Runnable() {

					@Override
					public void run() {
						for(int choice:SelectionChoices)
						{
							Configuration tmpConf = learnerConfig.copy();tmpConf.setGeneralisationThreshold(arg);
							ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphReference,new RPNIBlueFringe(tmpConf,choice).learn(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber),false));
							uas_threshold.add(arg, matrix.BCR());
						}
					}
					
				};
				if (threadNumber > 1) 
					outcomes.add(executorService.submit(interactiveRunner));
				else 
				{ 
					interactiveRunner.run();
					//uas_threshold.drawInteractive(gr);
				}
			}

			if (threadNumber > 1)
			{
				progress = new ProgressIndicator("running concurrent experiment",outcomes.size());
				for(Future<?> task:outcomes) { task.get();progress.next(); }// wait for termination of all tasks
			}
		}
		catch(Exception ex)
		{
			Helper.throwUnchecked("failed to run experiment", ex);
		}
		finally
		{
			if (executorService != null) executorService.shutdown();
		}
		
		uas_outcome.drawPdf(gr);uas_A.drawPdf(gr);uas_S.drawPdf(gr);uas_U.drawPdf(gr);
		uas_threshold.drawPdf(gr);
		DrawGraphs.end();// the process will not terminate without it because R has its own internal thread
    }
    
    
	public class RPNIBlueFringeTestVariability
    {
    	private Learner learner;
    	final List<PairOfPaths> listOfPairsToWrite;
    	final Iterator<PairOfPaths> listOfPairsToCheckAgainstIterator;
    	final boolean useOptimizedMerge;
    	LearnerGraph initPta = null;
    	final Configuration config;
    	
    	VertexID phantomVertex = null;
    	
    	public Learner getLearner()
    	{
    		return learner;
    	}
    	
		public RPNIBlueFringeTestVariability(final Configuration conf, boolean optimisedMerge, final List<PairOfPaths> lw, final List<PairOfPaths> lc) 
		{
			listOfPairsToWrite = lw;useOptimizedMerge = optimisedMerge;
			if (lc != null) listOfPairsToCheckAgainstIterator = lc.iterator();else listOfPairsToCheckAgainstIterator = null;
			final LearnerEvaluationConfiguration bfLearnerInitConfiguration = new LearnerEvaluationConfiguration(conf);bfLearnerInitConfiguration.ifthenSequences = PaperUAS.this.learnerInitConfiguration.ifthenSequences;
			bfLearnerInitConfiguration.setLabelConverter(PaperUAS.this.learnerInitConfiguration.getLabelConverter());
			config = bfLearnerInitConfiguration.config;// our config is a copy of the one supplied as an argument.
			config.setAskQuestions(false); 
			learner = new DummyLearner(new RPNIUniversalLearner(null, bfLearnerInitConfiguration)) 
			{
				
				@Override
				public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
				{
					LearnerGraph outcome = null;
					int extraPhantomVertices = 0;
					if (useOptimizedMerge)
						// Use the old and limited version to compute the merge because the general one is too slow on large graphs and we do not need either to merge arbitrary states or to handle "incompatibles".
						outcome = MergeStates.mergeAndDeterminize(original, pair);
					else
					{
						Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedVertices = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
						long score = original.pairscores.computePairCompatibilityScore_general(pair,mergedVertices);
						outcome = MergeStates.mergeCollectionOfVertices(original,pair.getR(),mergedVertices);
						
						if (score != original.getStateNumber()-outcome.getStateNumber())
						{// This is either a bug somewhere in the merger or (most likely) that the phantomVertex has been removed by the generalised learner. 
						 // The computation below is expensive on large graphs but only needs to be done once.
							LinkedHashSet<CmpVertex> removedStates = new LinkedHashSet<CmpVertex>();removedStates.addAll(original.transitionMatrix.keySet());
							removedStates.removeAll(outcome.transitionMatrix.keySet());removedStates.remove(pair.getQ());removedStates.remove(pair.getR());
							Assert.assertEquals(1,removedStates.size());// if it were a phantom vertex, there would only be one of them.
							CmpVertex tentativePhantom = removedStates.iterator().next();
							Set<Label> alphabetUsedOnPhantom = new TreeSet<Label>();alphabetUsedOnPhantom.addAll(original.pathroutines.computeAlphabet());
							for(Entry<Label,CmpVertex> transition:original.transitionMatrix.get(tentativePhantom).entrySet())
							{
								Assert.assertSame(tentativePhantom,transition.getValue());alphabetUsedOnPhantom.remove(transition.getKey());
							}
							Assert.assertEquals(0, alphabetUsedOnPhantom.size());
							extraPhantomVertices = 1;// now certain it was indeed a phantom vertex added when the PTA was initially built.
						}
						
						Assert.assertEquals(score+extraPhantomVertices,original.getStateNumber()-outcome.getStateNumber());
					}
					ScoreMode origScore = original.config.getLearnerScoreMode();original.config.setLearnerScoreMode(ScoreMode.COMPATIBILITY);
					long compatibilityScore = original.pairscores.computePairCompatibilityScore(pair);
					original.config.setLearnerScoreMode(origScore);
					
					Assert.assertEquals(compatibilityScore+1+extraPhantomVertices,original.getStateNumber()-outcome.getStateNumber());
					return outcome;
				}

				@Override 
				public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
				{
					/*
					try {
						graph.storage.writeGraphML("D:/experiment/data/"+graph.config.getTransitionMatrixImplType().toString()+"-"+(i++)+"-THU.xml");
					} catch (IOException e) {
						e.printStackTrace();
					}
					*/
					Stack<PairScore> outcome = graph.pairscores.chooseStatePairs(new PairScoreComputation.RedNodeDecisionProcedure(){

						@Override
						public CmpVertex selectRedNode(LearnerGraph coregraph,
								@SuppressWarnings("unused") Collection<CmpVertex> reds,
								Collection<CmpVertex> tentativeRedNodes) 
						{
							CmpVertex redVertex = null;
							if (listOfPairsToWrite != null)
							{
								redVertex = tentativeRedNodes.iterator().next();
								listOfPairsToWrite.add(new PairOfPaths(coregraph, new PairScore(null, redVertex, 0, 0)));
							}
							
							if(listOfPairsToCheckAgainstIterator != null)
							{
								PairOfPaths pair = listOfPairsToCheckAgainstIterator.next();
								Assert.assertNull(pair.getQ());
								redVertex = coregraph.getVertex(pair.getR());
							}
							return redVertex;
						}});
					if (!outcome.isEmpty())
					{
						if (listOfPairsToWrite != null)
						{
							//System.out.println("Optimized: "+useOptimizedMerge+", matrix: "+graph.config.getTransitionMatrixImplType()+", pair : "+outcome.peek());
							listOfPairsToWrite.add(new PairOfPaths(graph, outcome.peek()));
						}
						
						if(listOfPairsToCheckAgainstIterator != null)
						{
							PairOfPaths pair = listOfPairsToCheckAgainstIterator.next();
							//System.out.println("chosen "+outcome.peek()+", expected "+new PairScore(graph.getVertex(pair.getQ()),graph.getVertex(pair.getR()),0,0));
							pair.rebuildStack(graph, outcome);
						}
					}
					
					return outcome;
				}
			
				@Override 
				public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) 
				{
					if (initPta != null)
					{
						LearnerGraph graph = decoratedLearner.init(plus,minus);
						LearnerGraph.copyGraphs(initPta, graph);
						return initPta;
					}
					throw new IllegalArgumentException("should not be called");
				}
				
				@Override 
				public LearnerGraph init(PTASequenceEngine engine, int plusSize, int minusSize) 
				{
					LearnerGraph graph = decoratedLearner.init(engine,plusSize,minusSize);

					if (initPta != null)
					{
						LearnerGraph.copyGraphs(initPta, graph);
					}
					else
					{
						Set<Label> alphabet = graph.pathroutines.computeAlphabet();
						// Create a state to ensure that the entire alphabet is visible when if-then automata are loaded.
						phantomVertex = graph.nextID(true);
						CmpVertex dummyState = AbstractLearnerGraph.generateNewCmpVertex(phantomVertex, bfLearnerInitConfiguration.config);
						Map<Label,CmpVertex> row = graph.createNewRow();
						for(Label lbl:alphabet) graph.addTransition(row, lbl, dummyState);
						graph.transitionMatrix.put(dummyState, row);
					}
					return graph;
				}
			};
	
		}
		
		/** After this method is called, the learner used above no longer looks at the PTA is it given but assumes that the PTA supplied to this call should be returned as a result of initialisation.
		 * This is used to get around the problem of  
		 * @param initPTAArg
		 */
		public void setInitPta(LearnerGraph initPTAArg)
		{
			initPta = initPTAArg;
		}
		
		/** Learns starting with the supplied PTA. */
		public LearnerGraph learn(LearnerGraph initPTAArg)
		{
			setInitPta(initPTAArg);
			LearnerGraph outcome = learner.learnMachine(new LinkedList<List<Label>>(), new LinkedList<List<Label>>());
			if (phantomVertex != null) 
				outcome.transitionMatrix.remove(outcome.findVertex(phantomVertex));
			return outcome;
		}
		
		public LearnerGraph learn(PTASequenceEngine engineArg, boolean useNegatives)
		{
			PTASequenceEngine engine = null;
			if (!useNegatives)
			{
				PTASequenceEngine positives = new PTASequenceEngine();positives.init(new Automaton());
    			SequenceSet initSeq = positives.new SequenceSet();initSeq.setIdentity();
    			initSeq.cross(engineArg.getData());
    			engine = positives;
			}
			else
				engine = engineArg;
			
			LearnerGraph outcome = learner.learnMachine(engine,0,0);
			if (phantomVertex != null) 
				outcome.transitionMatrix.remove(outcome.findVertex(phantomVertex));
			return outcome;
		}
		
    }
    
    void checkTraces(Configuration config) throws IOException
    {
    	LearnerGraph correctAnswer = new LearnerGraph(config);
    	AbstractPersistence.loadGraph("shorttraceautomaton.xml", correctAnswer,labelConverter);
		Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).collectionOfPositiveTraces.get(UAVAllSeeds).keySet();
		for(final Integer frame:allFrames)
		{
			for(final String UAV:collectionOfTraces.get(UAVAllSeeds).collectionOfPositiveTraces.keySet())
				if (!UAV.equals(UAVAllSeeds))
					for(final String seed:collectionOfTraces.keySet())
						if (!seed.equals(UAVAllSeeds))
						{
							for(List<Label> positiveTrace:collectionOfTraces.get(seed).collectionOfPositiveTraces.get(UAV).get(frame))
							{
								int rejectPosition = correctAnswer.paths.tracePathPrefixClosed(positiveTrace);
								if (rejectPosition != AbstractOracle.USER_ACCEPTED)
									throw new IllegalArgumentException("accepted trace rejected at "+rejectPosition+", trace "+positiveTrace);
							}
							for(List<Label> negativeTrace:collectionOfTraces.get(seed).collectionOfNegativeTraces.get(UAV).get(frame))
							{
								int rejectPosition = correctAnswer.paths.tracePathPrefixClosed(negativeTrace);
								if (rejectPosition == AbstractOracle.USER_ACCEPTED)
									throw new IllegalArgumentException("rejected trace accepted, trace "+negativeTrace);
							}
						}
			for(List<Label> positiveTrace:collectionOfTraces.get(UAVAllSeeds).collectionOfPositiveTraces.get(UAVAllSeeds).get(frame))
			{
				int rejectPosition = correctAnswer.paths.tracePathPrefixClosed(positiveTrace);
				if (rejectPosition != AbstractOracle.USER_ACCEPTED)
					throw new IllegalArgumentException("accepted trace rejected at "+rejectPosition+", trace "+positiveTrace);
			}
			for(List<Label> negativeTrace:collectionOfTraces.get(UAVAllSeeds).collectionOfNegativeTraces.get(UAVAllSeeds).get(frame))
			{
				int rejectPosition = correctAnswer.paths.tracePathPrefixClosed(negativeTrace);
				if (rejectPosition == AbstractOracle.USER_ACCEPTED)
					throw new IllegalArgumentException("rejected trace accepted, trace "+negativeTrace);
			}
		}
    }
    
    public void process(String fileString) 
    {
        if (fileString.length() == 0) {
            return;// ignore empty lines.
        }
		if (QSMTool.isCmdWithArgs(fileString, QSMTool.cmdLTL) || QSMTool.isCmdWithArgs(fileString, QSMTool.cmdIFTHENAUTOMATON)) {
		            if (learnerInitConfiguration.ifthenSequences == null) {
		                learnerInitConfiguration.ifthenSequences = new TreeSet<String>();
		            }
		            learnerInitConfiguration.ifthenSequences.add(fileString);
		} else if (QSMTool.isCmdWithArgs(fileString, QSMTool.cmdConfig)) {
            StringTokenizer tokenizer = new StringTokenizer(fileString.substring(QSMTool.cmdConfig.length() + 1)," ,");
            if (tokenizer.hasMoreTokens())
            {
            	String key = tokenizer.nextToken();
            	if (!tokenizer.hasMoreTokens())
            		throw new IllegalArgumentException("missing value for "+key);
            	String value = tokenizer.nextToken();
            	learnerInitConfiguration.config.assignValue(key, value, true);
            }
       } else if (fileString.startsWith(QSMTool.cmdComment)) {// do nothing
       } else {
            throw new IllegalArgumentException("invalid command " + fileString);
       }
    }
    
   public void loadReducedConfigurationFile(String configFile)
    {
        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(configFile));
            String fileString;
            while ((fileString = in.readLine()) != null) {
                    process(fileString);
            }
        } catch (IOException e) {
            statechum.Helper.throwUnchecked("failed to read learner initial data", e);
        } finally {
            if (in != null) { try { in.close();in=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
        }    	
    }
    
    /**
	 * @param args trace file to load.
     * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
		PaperUAS paper = new PaperUAS();
    	Configuration config = Configuration.getDefaultConfiguration().copy();config.setDebugMode(false);
    	paper.learnerInitConfiguration.config = config;config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
    	paper.loadReducedConfigurationFile(args[0]);
    	
		int offset=1;
    	{
        	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
	    	int maxFrame = paper.getMaxFrame(inputFiles);
	    	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
    	}
    	
    	{
        	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]);
        	//paper.loadData(inputFiles);paper.runExperimentWithSingleAutomaton("tmp");
	    	//paper.loadData(inputFiles);paper.runExperiment();
	    	paper.loadDataByConcatenation(inputFiles);
	    	//Visualiser.waitForKey();
	    	paper.runExperimentWithSingleAutomaton("large");
	    	//paper.checkTraces(config);
	    	//paper.loadData(inputFiles);
	    	//paper.compareTwoLearners();
	    	//paper.evaluateVariability();
	    	//paper.learnIfThenFromTraces();
    	}
	}

}
