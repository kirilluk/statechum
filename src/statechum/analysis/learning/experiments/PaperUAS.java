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
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
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
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.PathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.Helper;
import statechum.Label;
import statechum.Pair;
import statechum.ProgressIndicator;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.InitialConfigurationAndData;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatCanClassifyPairs;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.SampleData;
import statechum.analysis.learning.experiments.PairSelection.WekaDataCollector;
import statechum.analysis.learning.experiments.PaperUAS.TracesForSeed.Automaton;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.apps.QSMTool;
import statechum.apps.QSMTool.TraceAdder;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.model.testset.PTASequenceEngine.SequenceSet;
import weka.classifiers.Classifier;
import weka.core.Instances;

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
    				public void addTrace(List<Label> trace, boolean positive) 
    				{
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
            		
            	}, learnerInitConfiguration.getLabelConverter());
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
    				public void addTrace(List<Label> trace, boolean positive) 
    				{
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
            		
            	},learnerInitConfiguration.getLabelConverter());
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
            statechum.Helper.throwUnchecked("failed to read learner initial data from ", e);
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
        			traceDetailsUAV = new PTASequenceEngine(learnerInitConfiguration.config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY);
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
   	
   public void runExperimentWithSingleAutomaton(String name, String arffName, LearnerGraph referenceGraph) throws IOException
   {
        long tmStarted = new Date().getTime();
        
   		final Collection<List<Label>> evaluationTestSet = referenceGraph.wmethod.getFullTestSet(1);
   		int wPos=0;
   		for(List<Label> seq:evaluationTestSet) if (referenceGraph.paths.tracePathPrefixClosed(seq) == AbstractOracle.USER_ACCEPTED) wPos++;
   		/*
   		System.out.println("before rnd: "+evaluationTestSet.size()+" sequences, "+wPos+" positives");
   		RandomPathGenerator pathGen = new RandomPathGenerator(graphReference,new Random(0),5,graphReference.getInit());
   		pathGen.generatePosNeg(2*(wMethod.size()-wPos), 1);
   		wMethod.addAll(pathGen.getExtraSequences(0).getData());
   		System.out.println("after rnd: "+wMethod.size()+" sequences");
*/
   		DrawGraphs gr = new DrawGraphs();
		final RBoxPlot<Integer>
				uas_S=new RBoxPlot<Integer>("Time","F-measure",new File("time_S_"+name+"_f.pdf"));
		Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).keySet();
		Classifier classifiers[] = loadClassifierFromArff(arffName);
		ProgressIndicator progress = new ProgressIndicator("UAS", allFrames.size()*classifiers.length);
		Set<Label> alphabetForIfThen = referenceGraph.pathroutines.computeAlphabet();
  		for(final Integer frame:allFrames)
  			for(int i=0;i<classifiers.length;++i)
	  		{
	  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));
	  			LearnerThatCanClassifyPairs learner = new LearnerThatUsesWekaResults(learnerInitConfiguration,referenceGraph,classifiers[i],initPTA);
	  			learner.setAlphabetUsedForIfThen(alphabetForIfThen);
	 	        final LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		        long tmFinished = new Date().getTime();
		        System.out.println("Learning complete, "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
		        double difference = PairQualityLearner.estimationOfDifference(referenceGraph, actualAutomaton, evaluationTestSet, learnerInitConfiguration.config, 1);
		        System.out.println("F-measure for frame : "+frame+" = "+ difference);
				
				uas_S.add(frame,difference);
				uas_S.drawInteractive(gr);
	
				actualAutomaton.storage.writeGraphML("resources/"+name+"_"+frame+"_"+i+".xml");
	  			progress.next();
	  		}
  		uas_S.drawPdf(gr);
		DrawGraphs.end();// the process will not terminate without it because R has its own internal thread
  }
   	
   
   /** Used to training a few different classifiers from a full PTA by comparing metrics on pairs considered by QSM and checking them against the reference solution. */ 
   protected Classifier []loadClassifierFromArff(String arffWithTrainingData)
   {
		weka.classifiers.trees.REPTree tree =new weka.classifiers.trees.REPTree();tree.setMaxDepth(4); 		
		tree.setNoPruning(true);// since we only use the tree as a classifier (as a conservative extension of what is currently done) and do not actually look at it, elimination of pruning is not a problem. 
   		// As part of learning, we also prune some of the nodes where the ratio of correctly-classified pairs to those incorrectly classified is comparable.
 		// The significant advantage of not pruning is that the result is no longer sensitive to the order of elements in the tree and hence does not depend on the order in which elements have been obtained by concurrent threads.
		weka.classifiers.trees.J48 tree48 =new weka.classifiers.trees.J48(); 		
		tree48.setUnpruned(true);// since we only use the tree as a classifier (as a conservative extension of what is currently done) and do not actually look at it, elimination of pruning is not a problem. 
   		// As part of learning, we also prune some of the nodes where the ratio of correctly-classified pairs to those incorrectly classified is comparable.
  		// The significant advantage of not pruning is that the result is no longer sensitive to the order of elements in the tree and hence does not depend on the order in which elements have been obtained by concurrent threads.
		weka.classifiers.lazy.IBk ibk = new weka.classifiers.lazy.IBk(1);
		Classifier []outcome = new Classifier[]{tree,tree48,ibk};
		for(Classifier c:outcome) trainClassifierFromArff(c,arffWithTrainingData);
		return outcome;
   }
   
   /** Used to load the classifier from a full PTA by comparing metrics on pairs considered by QSM and checking them against the reference solution. */ 
   protected void trainClassifierFromArff(Classifier classifier,String arffWithTrainingData)
   {
		Reader arffReader = null;
		try
		{
			arffReader = new FileReader(arffWithTrainingData);
			Instances trainingData = new Instances(arffReader);
			if (!"class".equals(trainingData.attribute(trainingData.numAttributes()-1).name()))
				throw new IllegalArgumentException("last element is not a class");
			trainingData.setClassIndex(trainingData.numAttributes()-1);
			
			classifier.buildClassifier(trainingData);
		}
		catch(Exception ex)
		{// we cannot proceed if this happens because every classifier should be able to both learn and deliver. Throw the exception.
			Helper.throwUnchecked("failed to train classifier "+classifier.getClass(), ex);
		}
		finally
		{
			if (arffReader != null)
				try { arffReader.close(); } catch (IOException e) {
					// ignore this, we have opened the file for reading hence not much else we can do in terms of cleanup other than doing a close.
				}
		}
   }
   
   public void LearnReferenceAutomaton() throws Exception
   {
	   long tmStarted = new Date().getTime();
       LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber));
       final LearnerGraph graphReference = new LearnerThatUsesWekaResults(learnerInitConfiguration,null,null,initPTA).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
       long tmFinished = new Date().getTime();
       System.out.println("Learning reference complete, "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
       graphReference.storage.writeGraphML("traceautomaton.xml");
   }
   
    public void runExperiment(String arffName, final LearnerGraph referenceGraph, final Collection<Label> labelsToMergeA, final Collection<Label> labelsToMergeB) throws IOException
    {
   		final Collection<List<Label>> evaluationTestSet = referenceGraph.wmethod.getFullTestSet(1);
       
		// Here I need to moderate the effort because choosing traces for all seeds is good but I need
		// that many times more traces, so I have to create a graph in terms of effort v.s. quailty (or even better, scale
		// the existing one).
		DrawGraphs gr = new DrawGraphs();

		final RBoxPlot<Pair<Integer,String>> 
			uas_outcome = new RBoxPlot<Pair<Integer,String>>("Time","f-measure",new File("time_f.pdf"));
		final RBoxPlot<Integer>
					uas_A=new RBoxPlot<Integer>("Time","f-measure",new File("time_A_f.pdf")),
							uas_S=new RBoxPlot<Integer>("Time","f-measure",new File("time_S_f.pdf")),
									uas_U=new RBoxPlot<Integer>("Time","f-measure",new File("time_U_f.pdf"))
			;
		final RBoxPlot<Integer>
			uas_threshold=new RBoxPlot<Integer>("Threshold","f-measure",new File("threshold_f.pdf"));
		final Set<Label> alphabetForIfThen = referenceGraph.pathroutines.computeAlphabet();
		final Classifier classifiers[] = loadClassifierFromArff(arffName);
		Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).keySet();
		ProgressIndicator progress = new ProgressIndicator("UAS", allFrames.size());
		/** The runner of computational threads. */
		int threadNumber = ExperimentRunner.getCpuNumber();
		ExecutorService executorService = Executors.newFixedThreadPool(threadNumber);
		
		try
		{
			List<Future<?>> outcomes = new LinkedList<Future<?>>();
			for(final Integer frame:allFrames)
			{
				{// For all frames and all seeds
					Runnable interactiveRunner = new Runnable() {
	
						@Override
						public void run() {
							for(Classifier c:classifiers)
							{
					  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));
					  			LearnerThatUsesWekaResults learner = new LearnerThatUsesWekaResults(learnerInitConfiguration,referenceGraph,c,initPTA);
					  			learner.setLabelsLeadingToStatesToBeMerged(labelsToMergeA);learner.setLabelsLeadingFromStatesToBeMerged(labelsToMergeB);learner.setAlphabetUsedForIfThen(alphabetForIfThen);
					 	        final LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
						        double difference = PairQualityLearner.estimationOfDifference(referenceGraph, actualAutomaton, evaluationTestSet, learnerInitConfiguration.config, 1);

						        uas_outcome.add(new Pair<Integer,String>(frame,"S"),difference);
								uas_S.add(frame,difference);
							}
						}
						
					};
					outcomes.add(executorService.submit(interactiveRunner));
				}
				
				for(final String seed:collectionOfTraces.keySet())
					if (!seed.equals(UAVAllSeeds))
					{// Just for all frames of the a single seed
						Runnable interactiveRunner = new Runnable() {

							@Override
							public void run() {
								TracesForSeed tracesForThisSeed = collectionOfTraces.get(seed);
								
								for(Classifier c:classifiers)
								{
						  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(tracesForThisSeed.tracesForUAVandFrame.get(UAVAll).get(frame));
						  			LearnerThatUsesWekaResults learner = new LearnerThatUsesWekaResults(learnerInitConfiguration,referenceGraph,c,initPTA);
						  			learner.setLabelsLeadingToStatesToBeMerged(labelsToMergeA);learner.setLabelsLeadingFromStatesToBeMerged(labelsToMergeB);learner.setAlphabetUsedForIfThen(alphabetForIfThen);
						 	        final LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
							        double difference = PairQualityLearner.estimationOfDifference(referenceGraph, actualAutomaton, evaluationTestSet, learnerInitConfiguration.config, 1);
									uas_outcome.add(new Pair<Integer,String>(frame,"A"),difference);
									uas_A.add(frame,difference);
								}
							}
							
						};
						outcomes.add(executorService.submit(interactiveRunner));
					}
				for(final String UAV:collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.keySet())
					if (!UAV.equals(UAVAllSeeds) && !UAV.equals(UAVAll))
						for(final String seed:collectionOfTraces.keySet())
							if (!seed.equals(UAVAllSeeds))
							{
								Runnable interactiveRunner = new Runnable() {

									@Override
									public void run() {
										for(Classifier c:classifiers)
										{
								  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(seed).tracesForUAVandFrame.get(UAV).get(frame));
								  			LearnerThatUsesWekaResults learner = new LearnerThatUsesWekaResults(learnerInitConfiguration,referenceGraph,c,initPTA);
								  			learner.setLabelsLeadingToStatesToBeMerged(labelsToMergeA);learner.setLabelsLeadingFromStatesToBeMerged(labelsToMergeB);learner.setAlphabetUsedForIfThen(alphabetForIfThen);
								 	        final LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
									        double difference = PairQualityLearner.estimationOfDifference(referenceGraph, actualAutomaton, evaluationTestSet, learnerInitConfiguration.config, 1);
											uas_outcome.add(new Pair<Integer,String>(frame,"U"),difference);
											uas_U.add(frame,difference);
										}
									}
									
								};
								outcomes.add(executorService.submit(interactiveRunner));
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
						for(Classifier c:classifiers)
						{
				  			LearnerGraph initPTAWithNegatives = new LearnerGraph(learnerInitConfiguration.config);initPTAWithNegatives.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber));
				  			Configuration tmpConf = learnerInitConfiguration.config.copy();tmpConf.setGeneralisationThreshold(arg);
				  			LearnerGraph initPTA = new LearnerGraph(tmpConf);
				  			AbstractPathRoutines.removeRejectStates(initPTAWithNegatives,initPTA);
				  			LearnerThatUsesWekaResults learner = new LearnerThatUsesWekaResults(learnerInitConfiguration,referenceGraph,c,initPTA);
				  			learner.setLabelsLeadingToStatesToBeMerged(labelsToMergeA);learner.setLabelsLeadingFromStatesToBeMerged(labelsToMergeB);learner.setAlphabetUsedForIfThen(alphabetForIfThen);
				 	        final LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
					        double difference = PairQualityLearner.estimationOfDifference(referenceGraph, actualAutomaton, evaluationTestSet, learnerInitConfiguration.config, 1);
							uas_threshold.add(arg, difference);
						}
					}
					
				};
					outcomes.add(executorService.submit(interactiveRunner));
			}

			progress = new ProgressIndicator("running concurrent experiment",outcomes.size());
			for(Future<?> task:outcomes) { task.get();progress.next(); }// wait for termination of all tasks
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
    
    
    void checkTraces(Configuration config) throws IOException
    {
    	LearnerGraph correctAnswer = new LearnerGraph(config);
    	AbstractPersistence.loadGraph("shorttraceautomaton.xml", correctAnswer,learnerInitConfiguration.getLabelConverter());
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
   /*
   public static void checkValidityOfInitialPTA()
   {
		LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsList = PairQualityLearner.LearnerThatUsesWekaResults.buildVerticesToMerge(initialConfigAndData.initial.graph,Arrays.asList(new Label[]{lblToRoot}));
		if (initialConfigAndData.initial.graph.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge) < 0)
			throw new IllegalArgumentException("inconsistent initial PTA: vertices that lead to unique state in the reference graph cannot be merged in the PTA");
   }*/
   
   /**
	 * @param args trace file to load.
    * @throws IOException 
	 */
	public static void mainA(String[] args) throws Exception 
	{
		PaperUAS paper = new PaperUAS();
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setDebugMode(false);config.setGdLowToHighRatio(0.75);config.setGdKeyPairThreshold(0.5);
		paper.learnerInitConfiguration.config = config;config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
       	paper.learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
          	
    	paper.loadReducedConfigurationFile(args[0]);
   	
		int offset=1;
	   	{
	       	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
		    	int maxFrame = paper.getMaxFrame(inputFiles);
		    	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
	   	}
   	
       	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]);
       	//paper.loadData(inputFiles);paper.runExperimentWithSingleAutomaton("tmp");
	    paper.loadData(inputFiles);
	    // paper.runExperiment();
	   //paper.loadDataByConcatenation(inputFiles);
	    	//Visualiser.waitForKey();

	   LearnerGraph initialPTA = new LearnerGraph(config);
	   initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
	   System.out.println(initialPTA.getAcceptStateNumber()+", total: "+initialPTA.getStateNumber());
	   LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		System.out.println("constructing vertices to merge");
		List<StatePair> pairsList = PairQualityLearner.LearnerThatUsesWekaResults.buildVerticesToMerge(initialPTA,Collections.<Label>emptyList(),
				Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", config,paper.learnerInitConfiguration.getLabelConverter())}));
		System.out.println("trying to do merge");
		if (initialPTA.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge) < 0)
			throw new IllegalArgumentException("inconsistent initial PTA: vertices that lead to unique state in the reference graph cannot be merged in the PTA");
		System.out.println("done attempt to merge, everything ok");
	}
	
	public LearnerGraph writeArff(LearnerGraph referenceGraph, String whereToWrite)
	{
	    LearnerGraph initialPTA = new LearnerGraph(learnerInitConfiguration.config);
		initialPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber));

		WekaDataCollector dataCollector = PairQualityLearner.createDataCollector();
		// Run the learner that will find out how to select the correct pairs.
		LearnerThatCanClassifyPairs learnerOfPairs = new PairQualityLearner.LearnerThatUpdatesWekaResults(learnerInitConfiguration,referenceGraph,dataCollector,initialPTA,null);
		LearnerGraph actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		
		// final weka.classifiers.trees.J48 classifier = new weka.classifiers.trees.J48();
		FileWriter wekaInstances= null;
		try
		{
			wekaInstances = new FileWriter(whereToWrite);
			wekaInstances.write(dataCollector.trainingData.toString());
			wekaInstances.close();
		}
		catch(Exception ex)
		{
			Helper.throwUnchecked("failed to create a file with training data for "+whereToWrite, ex);
		}
		finally
		{
			if (wekaInstances != null)
				try {
					wekaInstances.close();
				} catch (IOException e) {
					// ignore this, we are not proceeding anyway due to an earlier exception so whether the file was actually written does not matter
				}
		}
		
		// the learning of the pairs experiment should always produce the correct outcome since decisions to merge pairs are guaranteed to be correct.
		DifferentFSMException diff = WMethod.checkM(referenceGraph, actualAutomaton);if (diff != null) throw diff;
		
		return actualAutomaton;
	}

	/**
	 * @param args trace file to load.
     * @throws IOException 
	 */
	public static void mainSingleHugeAutomaton(String[] args) throws Exception 
	{
		PaperUAS paper = new PaperUAS();
    	paper.learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
        final Configuration learnerConfig = paper.learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
        learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
        learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
        paper.loadReducedConfigurationFile(args[0]);
        
		final int offset=1;
    	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
    	int maxFrame = paper.getMaxFrame(inputFiles);
    	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
    	for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]);// refill the input (it was drained by the computation of maxFrame).
    	paper.loadDataByConcatenation(inputFiles);
       	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraph, paper.learnerInitConfiguration.getLabelConverter());
       	String arffName = "resources/largePTA/pairsEncounteredHuge.arff";
       	//paper.writeArff(referenceGraph,arffName);// this part can be skipped if arff has already been generated.
    	paper.runExperimentWithSingleAutomaton("huge",arffName,referenceGraph);
	}
		
	/**
	 * @param args trace file to load.
     * @throws IOException 
	 */
	public static void mainSmallAutomata(String[] args) throws Exception 
	{
		PaperUAS paper = new PaperUAS();
		paper.learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
        final Configuration learnerConfig = paper.learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
        learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
        learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
        paper.loadReducedConfigurationFile(args[0]);
        
		final int offset=1;
    	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
    	int maxFrame = paper.getMaxFrame(inputFiles);
    	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
    	paper.loadData(inputFiles);
    	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraph, paper.learnerInitConfiguration.getLabelConverter());
    	
    	String arffName = "resources/largePTA/pairsEncounteredPartiallyMerged.arff";

	    LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
		initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
		System.out.println(initialPTA.getAcceptStateNumber()+", total: "+initialPTA.getStateNumber());
 	   LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
 		System.out.println("constructing vertices to merge");
 		List<StatePair> pairsList = PairQualityLearner.LearnerThatUsesWekaResults.buildVerticesToMerge(initialPTA,Collections.<Label>emptyList(),
 				Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", paper.learnerInitConfiguration.config,paper.learnerInitConfiguration.getLabelConverter())}));
 		System.out.println("trying to do merge");
 		if (initialPTA.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge) < 0)
 			throw new IllegalArgumentException("inconsistent initial PTA: vertices that lead to unique state in the reference graph cannot be merged in the PTA");
 		System.out.println("done attempt to merge, everything ok");

    	paper.writeArff(referenceGraph,arffName);
    	paper.runExperiment(arffName,referenceGraph,Collections.<Label>emptyList(),Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", paper.learnerInitConfiguration.config,paper.learnerInitConfiguration.getLabelConverter())}));
	}

	/**
	 * @param args trace file to load.
     * @throws IOException 
	 */
	public static void main(String[] args) throws Exception 
	{
		try
		{
			mainSingleHugeAutomaton(args);
		}
		finally
		{
			DrawGraphs.end();
		}
	}

	/**
	 * @param args trace file to load.
     * @throws IOException 
	 */
	public static void generateHugeArff(@SuppressWarnings("unused") String[] args) throws Exception 
	{
		PaperUAS paper = new PaperUAS();
   		final InitialConfigurationAndData initialConfigAndData = PairQualityLearner.loadInitialAndPopulateInitialConfiguration(PairQualityLearner.largePTAFileName, STATETREE.STATETREE_ARRAY, new Transform.InternStringLabel());
		paper.learnerInitConfiguration = initialConfigAndData.learnerInitConfiguration;
        /*final Configuration learnerConfig = paper.learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
        learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
        learnerConfig.setAskQuestions(false);
        paper.loadReducedConfigurationFile(args[0]);*/
		LearnerGraph original = initialConfigAndData.initial.graph;
		LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		StatePair pair = new StatePair(original.getInit(),original.getInit());
		original.pairscores.computePairCompatibilityScore_general(pair, null, verticesToMerge);
		LearnerGraph reduced = MergeStates.mergeCollectionOfVertices(original,pair.getR(),verticesToMerge);
		Assert.assertTrue(original.getStateNumber() > reduced.getStateNumber());
		LinkedHashSet<CmpVertex> removedStates = new LinkedHashSet<CmpVertex>();removedStates.addAll(original.transitionMatrix.keySet());
		removedStates.removeAll(reduced.transitionMatrix.keySet());removedStates.remove(pair.getQ());removedStates.remove(pair.getR());
		Assert.assertEquals(1,removedStates.size());// if it were a phantom vertex, there would only be one of them.
		CmpVertex tentativePhantom = removedStates.iterator().next();
		Set<Label> alphabetUsedOnPhantom = new TreeSet<Label>();alphabetUsedOnPhantom.addAll(original.pathroutines.computeAlphabet());
		for(Entry<Label,CmpVertex> transition:original.transitionMatrix.get(tentativePhantom).entrySet())
		{
			Assert.assertSame(tentativePhantom,transition.getValue());alphabetUsedOnPhantom.remove(transition.getKey());
		}
		Assert.assertEquals(0, alphabetUsedOnPhantom.size());
		System.out.println("phantom number was "+tentativePhantom);
		initialConfigAndData.initial.graph.transitionMatrix.remove(tentativePhantom);
    	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraph, paper.learnerInitConfiguration.getLabelConverter());
    	WekaDataCollector dataCollector = PairQualityLearner.createDataCollector();
    	LearnerThatCanClassifyPairs learnerOfPairs = new PairQualityLearner.LearnerThatUpdatesWekaResults(initialConfigAndData.learnerInitConfiguration,referenceGraph,dataCollector,initialConfigAndData.initial.graph,null);
		learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		
		FileWriter wekaInstances=new FileWriter("resources/largePTA/pairsEncountered3.arff");
		wekaInstances.write(dataCollector.trainingData.toString());
		wekaInstances.close();
	}
	
	/**
	 * @param args trace file to load.
     * @throws IOException 
	 */
	public static void mainC(String[] args) throws Exception 
	{
		DrawGraphs gr = new DrawGraphs();
		PaperUAS paper = new PaperUAS();
    	Configuration config = Configuration.getDefaultConfiguration().copy();config.setDebugMode(false);
    	paper.learnerInitConfiguration.config = config;config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
    	paper.learnerInitConfiguration.setLabelConverter( new Transform.InternStringLabel() );
    	config.setGeneralisationThreshold(0);config.setGdFailOnDuplicateNames(false);
    	config.setGdLowToHighRatio(0.75);config.setGdKeyPairThreshold(0.5);config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
            
            LearnerGraph referenceGraph = new LearnerGraph(config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraph, paper.learnerInitConfiguration.getLabelConverter());
       		final Collection<List<Label>> evaluationTestSet = referenceGraph.wmethod.getFullTestSet(1);
            /*
    		final InitialConfigurationAndData initialConfigAndData = PairQualityLearner.loadInitialAndPopulateInitialConfiguration(PairQualityLearner.largePTAFileName, STATETREE.STATETREE_ARRAY, paper.labelConverter);
    		Map<Label,CmpVertex> map=PairQualityLearner.uniqueIntoState(referenceGraph);
    		Label lblToRoot = null;
    		for(Entry<Label,CmpVertex> entry:map.entrySet())
    			if (entry.getValue().equals(referenceGraph.getInit()))
				{
					lblToRoot = entry.getKey();break;
				}
    		assert lblToRoot != null;
    		//PairQualityLearner.addIfThenForMandatoryMerge(initialConfigAndData.learnerInitConfiguration,Arrays.asList(new Label[]{lblToRoot}));
    		*/
       		paper.loadReducedConfigurationFile(args[0]);
    		int offset=1;
    	   	{
    	       	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
    		    	int maxFrame = paper.getMaxFrame(inputFiles);
    		    	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
    	   	}
       	
           	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]);
    	    paper.loadData(inputFiles);
    	    LearnerGraph initialPTA = new LearnerGraph(config);
    		initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
          
    		final RBoxPlot<String> gr_PairQuality = new RBoxPlot<String>("Correct v.s. wrong","%%",new File("percentage_correctwrong.pdf"));
    		final RBoxPlot<String> gr_ErrorsAndDeadends = new RBoxPlot<String>("Errors and deadends","Red states",new File("errors_deadends.pdf"));
    		SquareBagPlot gr_NewToOrig = new SquareBagPlot("orig score","score with learnt selection",new File("new_to_orig.pdf"),0,1,true);
    		LearnerThatUsesWekaResults learnerOfPairs = null;
    		LearnerGraph actualAutomaton = null;
			weka.classifiers.trees.REPTree tree =new weka.classifiers.trees.REPTree();tree.setMaxDepth(4); 		
			tree.setNoPruning(true);// since we only use the tree as a classifier (as a conservative extension of what is currently done) and do not actually look at it, elimination of pruning is not a problem. 
		   		// As part of learning, we also prune some of the nodes where the ratio of correctly-classified pairs to those incorrectly classified is comparable.
	    		// The significant advantage of not pruning is that the result is no longer sensitive to the order of elements in the tree and hence does not depend on the order in which elements have been obtained by concurrent threads.

				
			final weka.classifiers.Classifier classifier = tree;//new weka.classifiers.lazy.IBk(1);
    				//new weka.classifiers.trees.J48(); 
    		/*
    		WekaDataCollector dataCollector = PairQualityLearner.createDataCollector();
			
			// Run the learner that will find out how to select the correct pairs.
			LearnerThatCanClassifyPairs learnerOfPairs = new LearnerThatUpdatesWekaResults(frame,initialConfigAndData.learnerInitConfiguration,referenceGraph,dataCollector,initialConfigAndData.initial.graph,gr_ErrorsAndDeadends);
			LearnerGraph actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
			
			
			// final weka.classifiers.trees.J48 classifier = new weka.classifiers.trees.J48();
			
			FileWriter wekaInstances=new FileWriter("resources/largePTA/pairsEncountered.arff");
			wekaInstances.write(dataCollector.trainingData.toString());
			wekaInstances.close();
			
			classifier.buildClassifier(dataCollector.trainingData);
			System.out.println("Entries in the classifier: "+dataCollector.trainingData.numInstances());
			System.out.println(classifier);
			dataCollector=null;// throw all the training data away.
*/
    		Instances trainingData = new Instances(new FileReader("resources/largePTA/pairsEncountered.arff"));
    		if (!"class".equals(trainingData.attribute(trainingData.numAttributes()-1).name()))
    			throw new IllegalArgumentException("last element is not a class");
    		trainingData.setClassIndex(trainingData.numAttributes()-1);
    		
    		classifier.buildClassifier(trainingData);
			System.out.println("Entries in the classifier: "+trainingData.numInstances());
			System.out.println(classifier);
			
			learnerOfPairs = new LearnerThatUsesWekaResults(paper.learnerInitConfiguration,referenceGraph,classifier,initialPTA);
			learnerOfPairs.setLabelsLeadingToStatesToBeMerged(Collections.<Label>emptyList());
			learnerOfPairs.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", config,paper.learnerInitConfiguration.getLabelConverter())}));
			actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
			actualAutomaton.storage.writeGraphML("resources/LargePTA/learntWithWekaAndForcedMergers.xml");

			if (gr_PairQuality != null) gr_PairQuality.drawPdf(gr);if (gr_NewToOrig != null) gr_NewToOrig.drawPdf(gr);
			if (gr_ErrorsAndDeadends != null) gr_ErrorsAndDeadends.drawPdf(gr);
			
			System.out.println("learning complete");
			VertID rejectVertexID = null;
			for(CmpVertex v:actualAutomaton.transitionMatrix.keySet())
				if (!v.isAccept())
				{
					assert rejectVertexID == null : "multiple reject vertices in learnt automaton, such as "+rejectVertexID+" and "+v;
					rejectVertexID = v;break;
				}
			
			actualAutomaton.pathroutines.completeGraphPossiblyUsingExistingVertex(rejectVertexID);// we need to complete the graph, otherwise we are not matching it with the original one that has been completed.
			SampleData dataSampleWeka = new SampleData(null,null);
			dataSampleWeka.difference = PairQualityLearner.estimationOfDifference(referenceGraph, actualAutomaton, evaluationTestSet, config, ExperimentRunner.getCpuNumber());
			System.out.println("difference to actual is "+dataSampleWeka.difference);
			//LearnerGraph outcomeOfReferenceLearner = new ReferenceLearner(frame,initialConfigAndData.learnerInitConfiguration,referenceGraph,initialConfigAndData.initial.graph).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
			//outcomeOfReferenceLearner.storage.writeGraphML("resources/LargePTA/referenceOutcome.xml");
			LearnerGraph outcomeOfReferenceLearner = new LearnerGraph(initialPTA.config);AbstractPersistence.loadGraph("resources/LargePTA/referenceOutcome.xml", outcomeOfReferenceLearner, paper.learnerInitConfiguration.getLabelConverter());
			dataSampleWeka.differenceForReferenceLearner = PairQualityLearner.estimationOfDifference(referenceGraph,outcomeOfReferenceLearner, evaluationTestSet, config, ExperimentRunner.getCpuNumber());
		
			System.out.println("Difference between learnt and reference, using Weka: "+dataSampleWeka.difference+" and without is "+dataSampleWeka.differenceForReferenceLearner);
          /*
             LearnerGraph fullGraph =  paper.new RPNIBlueFringe(learnerConfig,pairchoiceORIG).learn(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber),true);
             fullGraph.storage.writeGraphML("resources/largePTA/correctOutcome.xml");
        	*/
        	
	    	//paper.runExperimentWithSingleAutomaton("large");
	    	//paper.checkTraces(config);
	    	//paper.loadData(inputFiles);
	    	//paper.compareTwoLearners();
	    	//paper.evaluateVariability();
	    	//paper.learnIfThenFromTraces();
    	//}
			
		DrawGraphs.end();
	}

}
