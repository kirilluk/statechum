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
import java.io.FileOutputStream;
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
import java.util.Iterator;
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

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.RPNIBlueFringeVariability;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.PathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.Label;
import statechum.Pair;
import statechum.ProgressIndicator;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReference;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceDiff;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceLanguage;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.InitialConfigurationAndData;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatCanClassifyPairs;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;
import statechum.analysis.learning.experiments.PairSelection.WekaDataCollector;
import statechum.analysis.learning.experiments.PaperUAS.TracesForSeed.Automaton;
import statechum.analysis.learning.linear.GD;
import statechum.analysis.learning.observers.RecordProgressDecorator;
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
			final List<Label> lastPositiveTrace = new ArrayList<Label>(currentLastTrace.size());lastPositiveTrace.addAll(currentLastTrace);// make a copy because the positive trace constantly gets added to. 
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
    						
    						if (lastPositiveTrace.isEmpty())
    							lastPositiveTrace.addAll(trace);
    						else
    							lastPositiveTrace.addAll(trace.subList(1, traceLen));// we do not append it to a collection of traces here because it will be done when we hit a new frame
    					}
    					else
    					{
    						List<Label> negativeTrace = new ArrayList<Label>(lastPositiveTrace.size()-1+trace.size());
    						Iterator<Label> iterLbl = lastPositiveTrace.iterator();
    						for(int i=0;i<lastPositiveTrace.size()-1;++i) negativeTrace.add(iterLbl.next());
    						negativeTrace.addAll(trace);
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
    	}
    	
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
   	
   	
   /** Uses a technique described in David Lo's papers to infer pairwise constraints between events.
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
   	
   public static Collection<List<Label>> computeEvaluationSet(LearnerGraph referenceGraph, int seqLength, int numberOfSeq)
   {
	   for(CmpVertex vert:referenceGraph.transitionMatrix.keySet()) 
		   if (!vert.isAccept())
			   throw new IllegalArgumentException("test set generation should not be attempted on an automaton with reject-states");
	   assert numberOfSeq > 0 && seqLength > 0;
		RandomPathGenerator pathGen = new RandomPathGenerator(referenceGraph,new Random(0),seqLength,referenceGraph.getInit());
		pathGen.generateRandomPosNeg(numberOfSeq, 1, false, null, true,true,null,null);
		return  pathGen.getAllSequences(0).getData(PTASequenceEngine.truePred);
	   /*
		Collection<List<Label>> evaluationTestSet = referenceGraph.wmethod.getFullTestSet(1);
		
		RandomPathGenerator pathGen = new RandomPathGenerator(referenceGraph,new Random(0),5,referenceGraph.getInit());
		int wPos=0;
		for(List<Label> seq:evaluationTestSet)
			if (referenceGraph.paths.tracePathPrefixClosed(seq) == AbstractOracle.USER_ACCEPTED) wPos++;
		pathGen.generateRandomPosNeg(2*(evaluationTestSet.size()-2*wPos), 1, false, null, true,false,evaluationTestSet,null);
		evaluationTestSet = pathGen.getAllSequences(0).getData(PTASequenceEngine.truePred);// we replacing the test set with new sequences rather than adding to it because existing sequences could be prefixes of the new ones.
		wPos = 0;
		for(List<Label> seq:evaluationTestSet) if (referenceGraph.paths.tracePathPrefixClosed(seq) == AbstractOracle.USER_ACCEPTED) wPos++;
		return evaluationTestSet;
	    */
   }
   
   public void runExperimentWithSingleAutomaton(int ifDepth, String name, String arffName, LearnerGraph referenceGraph) throws Exception
   {
	   final Collection<List<Label>> evaluationTestSet = computeEvaluationSet(referenceGraph,-1,-1);
   		DrawGraphs gr = new DrawGraphs();
		final RBoxPlot<String>
			uas_F=new RBoxPlot<String>("Time","F-measure",new File("time_"+name+"_f.pdf")),
			uas_Diff=new RBoxPlot<String>("Time","Diff-measure",new File("time_"+name+"_Diff.pdf"));
		SquareBagPlot gr_diff_to_f = new SquareBagPlot("f-measure","diff-based measure",new File("diff-to-f.pdf"),0,1,true);

		Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).keySet();
		Classifier classifiers[] = loadClassifierFromArff(arffName);
		ProgressIndicator progress = new ProgressIndicator("UAS", allFrames.size()*classifiers.length);
		LearnerEvaluationConfiguration initConfiguration = new LearnerEvaluationConfiguration(learnerInitConfiguration.config);
		initConfiguration.setLabelConverter(learnerInitConfiguration.getLabelConverter());// we do not copy if-then automata here because we do not wish to augment from if-then on every iteration because our properties are pairwise and this permits augmentation to be carried out first thing and not any more.
		initConfiguration.config.setUseConstraints(false);// do not use if-then during learning (refer to the explanation above)
		
		LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(learnerInitConfiguration.ifthenSequences, null, referenceGraph, learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
		System.out.println(new Date().toString()+" learning commencing.");
		final Integer frame=6;
  		//for(final Integer frame:allFrames)
  		{
  			LearnerGraph initPTA = new LearnerGraph(initConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));
  			Transform.augmentFromIfThenAutomaton(initPTA, null, ifthenAutomata, initConfiguration.config.getHowManyStatesToAddFromIFTHEN());// we only need  to augment our PTA once (refer to the explanation above).
  			System.out.println("total states : "+initPTA.getStateNumber()+", "+initPTA.getAcceptStateNumber()+" accept-states");

  			final Set<Label> alphabetForIfThen = referenceGraph.pathroutines.computeAlphabet();
  			Label uniqueLabel = AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", initConfiguration.config,initConfiguration.getLabelConverter());

  			/*
  			for(int i=0;i<classifiers.length;++i)
	  		{
	  			{
	  				initPTA.storage.writeGraphML("resources/"+name+"-init_"+frame+"_"+i+".xml");
		  			LearnerThatCanClassifyPairs learner = new LearnerThatUsesWekaResults(ifDepth,initConfiguration,referenceGraph,classifiers[i],initPTA);
		  			//learner.setAlphabetUsedForIfThen(alphabetForIfThen);
		  			learner.setLabelsLeadingToStatesToBeMerged(Collections.<Label>emptyList());learner.setLabelsLeadingFromStatesToBeMerged(Collections.<Label>emptyList());
		  			//learner.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", initConfiguration.config,initConfiguration.getLabelConverter())}));
		 	        final LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		 	        //actualAutomaton.storage.writeGraphML("resources/"+name+"_"+frame+"_"+i+".xml");
			        
		 	        double differenceF = PairQualityLearner.estimationOfDifferenceFmeasure(referenceGraph, actualAutomaton, evaluationTestSet);
		 	        double differenceD = PairQualityLearner.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, initConfiguration.config, ExperimentRunner.getCpuNumber());
			        System.out.println(new Date().toString()+" _L: For frame : "+frame+" (classifier "+i+"), long traces f-measure = "+ differenceF+" diffmeasure = "+differenceD);
					uas_F.add(frame+"_L",differenceF,"green");uas_Diff.add(frame+"_L",differenceD,"green");gr_diff_to_f.add(differenceF,differenceD);
	  			}
	  				  			
	  			{
		  			LearnerThatCanClassifyPairs learner = new LearnerThatUsesWekaResults(ifDepth,initConfiguration,referenceGraph,classifiers[i],initPTA);
		  			learner.setAlphabetUsedForIfThen(alphabetForIfThen);
		  			learner.setLabelsLeadingToStatesToBeMerged(Collections.<Label>emptyList());
		  			learner.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueLabel}));
		 	        LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
			        //actualAutomaton.storage.writeGraphML("resources/"+name+"-mm_"+frame+"_"+i+".xml");

			        // Now merge everything that we need to merge
			        LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(actualAutomaton,learner.getLabelsLeadingToStatesToBeMerged(),learner.getLabelsLeadingFromStatesToBeMerged());
					if (!pairsList.isEmpty())
					{
						int score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
						if (score < 0) throw new RuntimeException("last merge in the learning process was not possible");
						actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge);
					}

			        double differenceF = PairQualityLearner.estimationOfDifferenceFmeasure(referenceGraph, actualAutomaton, evaluationTestSet);
			        double differenceD = PairQualityLearner.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, initConfiguration.config, ExperimentRunner.getCpuNumber());
			        System.out.println(new Date().toString()+" _M: For frame : "+frame+" (classifier "+i+"), long traces f-measure = "+ differenceF+" diffmeasure = "+differenceD);
					uas_F.add(frame+"_M",differenceF,"blue");uas_Diff.add(frame+"_M",differenceD,"blue");gr_diff_to_f.add(differenceF,differenceD);
	  			}
	  			{
	  				LearnerGraph ptaAfterMergingBasedOnUniques = PairQualityLearner.mergeStatesForUnique(initPTA,uniqueLabel);
		  			LearnerThatCanClassifyPairs learner = new LearnerThatUsesWekaResults(ifDepth,initConfiguration,referenceGraph,classifiers[i],ptaAfterMergingBasedOnUniques);
		  			learner.setAlphabetUsedForIfThen(alphabetForIfThen);
		  			learner.setLabelsLeadingToStatesToBeMerged(Collections.<Label>emptyList());
		  			learner.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueLabel}));
		 	        LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
			        //actualAutomaton.storage.writeGraphML("resources/"+name+"-mm_"+frame+"_"+i+".xml");

			        // Now merge everything that we need to merge
			        LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
					List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(actualAutomaton,learner.getLabelsLeadingToStatesToBeMerged(),learner.getLabelsLeadingFromStatesToBeMerged());
					if (!pairsList.isEmpty())
					{
						int score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
						if (score < 0) throw new RuntimeException("last merge in the learning process was not possible");
						actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge);
					}

			        double differenceF = PairQualityLearner.estimationOfDifferenceFmeasure(referenceGraph, actualAutomaton, evaluationTestSet);
			        double differenceD = PairQualityLearner.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, initConfiguration.config, ExperimentRunner.getCpuNumber());
			        System.out.println(new Date().toString()+" _UM: For frame : "+frame+" (classifier "+i+"), long traces f-measure = "+ differenceF+" diffmeasure = "+differenceD);
					uas_F.add(frame+"_UM",differenceF,"blue");uas_Diff.add(frame+"_UM",differenceD,"blue");gr_diff_to_f.add(differenceF,differenceD);
	  			}
	  			progress.next();
	  		}
 				 */

  			initPTA.storage.writeGraphML("hugegraph.xml");
			LearnerGraph ptaSmall = PairQualityLearner.mergeStatesForUnique(initPTA,uniqueLabel);
  			//Visualiser.updateFrame(initPTA.transform.trimGraph(4, initPTA.getInit()), ptaSmall.transform.trimGraph(4, ptaSmall.getInit()));
  			//Visualiser.waitForKey();
  			
  			{
  	  			final RBoxPlot<Long> gr_PairQuality = new RBoxPlot<Long>("Correct v.s. wrong","%%",new File("percentage_score_huge_ref.pdf"));
  				final Map<Long,TrueFalseCounter> pairQualityCounter = new TreeMap<Long,TrueFalseCounter>();

  				PairQualityLearner.ReferenceLearner referenceLearner = new PairQualityLearner.ReferenceLearner(initConfiguration, referenceGraph, initPTA);
  				referenceLearner.setPairQualityCounter(pairQualityCounter);
		        LearnerGraph referenceOutcome = referenceLearner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		        //referenceOutcome.storage.writeGraphML("resources/"+name+"-ref_"+frame+".xml");
		        
		        DifferenceToReference differenceF = DifferenceToReferenceLanguage.estimationOfDifference(referenceGraph, referenceOutcome, evaluationTestSet);
		        DifferenceToReference differenceD = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, referenceOutcome, initConfiguration.config, ExperimentRunner.getCpuNumber());
		        System.out.println(new Date().toString()+" _R: For frame : "+frame+", long traces f-measure = "+ differenceF.getValue()+" diffmeasure = "+differenceD.getValue());
				uas_F.add(frame+"_R",differenceF.getValue(),"red");uas_Diff.add(frame+"_R",differenceD.getValue(),"red");gr_diff_to_f.add(differenceF.getValue(),differenceD.getValue());

				//PairQualityLearner.updateGraph(gr_PairQuality,pairQualityCounter);
				//gr_PairQuality.drawInteractive(gr);gr_PairQuality.drawPdf(gr);
			}

  			{
  	  			final RBoxPlot<Long> gr_PairQuality = new RBoxPlot<Long>("Correct v.s. wrong","%%",new File("percentage_score_huge_refM.pdf"));
  				final Map<Long,TrueFalseCounter> pairQualityCounter = new TreeMap<Long,TrueFalseCounter>();

  				LearnerGraph ptaAfterMergingBasedOnUniques = PairQualityLearner.mergeStatesForUnique(initPTA,uniqueLabel);
  				PairQualityLearner.ReferenceLearner referenceLearner = new PairQualityLearner.ReferenceLearner(initConfiguration, referenceGraph, ptaAfterMergingBasedOnUniques);
  				referenceLearner.setPairQualityCounter(pairQualityCounter);
		        LearnerGraph referenceOutcome = referenceLearner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		        //referenceOutcome.storage.writeGraphML("resources/"+name+"-ref_"+frame+".xml");
		        
		        DifferenceToReference differenceF = DifferenceToReferenceLanguage.estimationOfDifference(referenceGraph, referenceOutcome, evaluationTestSet);
		        DifferenceToReference differenceD = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, referenceOutcome, initConfiguration.config, ExperimentRunner.getCpuNumber());
		        System.out.println(new Date().toString()+" _R: For frame : "+frame+", long traces f-measure = "+ differenceF+" diffmeasure = "+differenceD);
				uas_F.add(frame+"_RM",differenceF.getValue(),"red");uas_Diff.add(frame+"_RM",differenceD.getValue(),"red");gr_diff_to_f.add(differenceF.getValue(),differenceD.getValue());

				//PairQualityLearner.updateGraph(gr_PairQuality,pairQualityCounter);
				//gr_PairQuality.drawInteractive(gr);gr_PairQuality.drawPdf(gr);
			}

			
			uas_F.drawInteractive(gr);uas_Diff.drawInteractive(gr);gr_diff_to_f.drawInteractive(gr);
  		}
  		uas_F.drawPdf(gr);uas_Diff.drawPdf(gr);gr_diff_to_f.drawPdf(gr);
		DrawGraphs.end();// the process will not terminate without it because R has its own internal thread
  }
   	
   /*
   protected static void noveltyInCaseStudyExperiment(String [] args) throws IOException
   {
		PaperUAS paper = new PaperUAS();
    	paper.learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
        final Configuration learnerConfig = paper.learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
        learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
        learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
        paper.loadReducedConfigurationFile(args[0]);
>>>>>>> .merge-right.r793
        
       	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
       	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);
       	
       	Visualiser.updateFrame(referenceGraph, null);
       	Visualiser.waitForKey();
   }
   */
   /** Used to training a few different classifiers from a full PTA by comparing metrics on pairs considered by QSM and checking them against the reference solution. */ 
   protected Classifier []loadClassifierFromArff(String arffWithTrainingData)
   {
		weka.classifiers.trees.REPTree tree =new weka.classifiers.trees.REPTree();tree.setMaxDepth(3); 		
		tree.setNoPruning(true);// since we only use the tree as a classifier (as a conservative extension of what is currently done) and do not actually look at it, elimination of pruning is not a problem. 
   		// As part of learning, we also prune some of the nodes where the ratio of correctly-classified pairs to those incorrectly classified is comparable.
 		// The significant advantage of not pruning is that the result is no longer sensitive to the order of elements in the tree and hence does not depend on the order in which elements have been obtained by concurrent threads.
		weka.classifiers.trees.J48 tree48 =new weka.classifiers.trees.J48(); 		
		tree48.setUnpruned(true);// since we only use the tree as a classifier (as a conservative extension of what is currently done) and do not actually look at it, elimination of pruning is not a problem. 
   		// As part of learning, we also prune some of the nodes where the ratio of correctly-classified pairs to those incorrectly classified is comparable.
  		// The significant advantage of not pruning is that the result is no longer sensitive to the order of elements in the tree and hence does not depend on the order in which elements have been obtained by concurrent threads.
		weka.classifiers.lazy.IBk ibk = new weka.classifiers.lazy.IBk(1);
		weka.classifiers.lazy.IB1 ib1 = new weka.classifiers.lazy.IB1();
		weka.classifiers.functions.MultilayerPerceptron perceptron = new weka.classifiers.functions.MultilayerPerceptron();
		Classifier []outcome = new Classifier[]{ib1};//tree};//,tree48,ibk};//,perceptron};
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
       final LearnerGraph graphReference = new PairQualityLearner.ReferenceLearner(learnerInitConfiguration,null,initPTA).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
       long tmFinished = new Date().getTime();
       System.out.println("Learning reference complete, "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
       graphReference.storage.writeGraphML("traceautomaton.xml");
   }
   
   
   /**
    * Given a PTA, this one learns it using the supplied classifier to select pairs. If null, uses QSM learner instead.
    *  
    * @param ifDepth the length of if-chains built from REL metrics.
    * @param initPTA initial PTA
    * @param referenceGraph reference graph to compare to the one learnt.
    * @param c classifier to use
    * @param labelsToMergeTo specific transitions may identify the states they lead to, we could use this to ensure that mergers are consistent with those expectations
    * @param labelsToMergeFrom specific transitions may identify the states they lead from, we could use this to ensure that mergers are consistent with those expectations
    * @return difference between the learnt graph and the reference one.
    */
   public DifferenceToReference learnAndEstimateDifference(int ifDepth,LearnerGraph initPTA, LearnerGraph referenceGraph,Classifier c, final Collection<Label> labelsToMergeTo, final Collection<Label> labelsToMergeFrom)
   {
		LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(learnerInitConfiguration.ifthenSequences, null, referenceGraph, learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
			try {
			Transform.augmentFromIfThenAutomaton(initPTA, null, ifthenAutomata,learnerInitConfiguration.config.getHowManyStatesToAddFromIFTHEN());
		} catch (AugmentFromIfThenAutomatonException e) {
			Helper.throwUnchecked("failed to augment using if-then", e);
		}// we only need  to augment our PTA once (refer to the explanation above).
			LearnerThatCanClassifyPairs learner =  c != null? new PairQualityLearner.LearnerThatUsesWekaResults(ifDepth,learnerInitConfiguration,referenceGraph,c,initPTA):
					new PairQualityLearner.ReferenceLearner(learnerInitConfiguration,referenceGraph,initPTA);
			learner.setLabelsLeadingToStatesToBeMerged(labelsToMergeTo);learner.setLabelsLeadingFromStatesToBeMerged(labelsToMergeFrom);learner.setAlphabetUsedForIfThen(referenceGraph.pathroutines.computeAlphabet());
        LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
        
        // Now merge everything that we need to merge
        LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
		List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(actualAutomaton,learner.getLabelsLeadingToStatesToBeMerged(),learner.getLabelsLeadingFromStatesToBeMerged());
		if (!pairsList.isEmpty())
		{
			int score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
			if (score < 0) throw new RuntimeException("last merge in the learning process was not possible");
			actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge);
		}
       	LearnerGraph learntGraph = new LearnerGraph(learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(actualAutomaton,learntGraph);
       System.out.println("state number: "+referenceGraph.getStateNumber()+" for reference and "+learntGraph.getStateNumber()+" for the actual one");
		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		DirectedSparseGraph gr = gd.showGD(
					learntGraph,referenceGraph,
					ExperimentRunner.getCpuNumber());
			Visualiser.updateFrame(gr,null);
			Visualiser.waitForKey();
       return DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, learntGraph, learnerInitConfiguration.config,1);
   }
   
    public void runExperimentWithSmallAutomata(final int ifDepth, final String arffName, final LearnerGraph referenceGraph) throws IOException
    {
 	   //final Collection<List<Label>> evaluationTestSet = computeEvaluationSet(referenceGraph);
       
		// Here I need to moderate the effort because choosing traces for all seeds is good but I need
		// that many times more traces, so I have to create a graph in terms of effort v.s. quailty (or even better, scale
		// the existing one).
		final DrawGraphs gr = new DrawGraphs();

		final RBoxPlot<Pair<Integer,String>> 
			uas_outcome = new RBoxPlot<Pair<Integer,String>>("Time","f-measure",new File("time_f.pdf"));
		final RBoxPlot<String>
					uas_A=new RBoxPlot<String>("Time","f-measure",new File("time_A_f.pdf")),
							uas_S=new RBoxPlot<String>("Time","f-measure",new File("time_S_f.pdf")),
									uas_U=new RBoxPlot<String>("Time","f-measure",new File("time_U_f.pdf"))
			;
		final RBoxPlot<Integer>
			uas_threshold=new RBoxPlot<Integer>("Threshold","f-measure",new File("threshold_f.pdf"));
		final Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).keySet();
		
		/** The runner of computational threads. */
		int threadNumber = ExperimentRunner.getCpuNumber();
		ExecutorService executorService = Executors.newFixedThreadPool(threadNumber);
		
		try
		{
			List<Future<?>> outcomes = new LinkedList<Future<?>>();
			System.out.println(allFrames);
			for(final Integer frame:allFrames)
			{
				Runnable interactiveRunner = new Runnable() {

					@Override
					public void run() 
					{/*
						final Classifier classifiers[] = loadClassifierFromArff(arffName);
						for(Classifier c:classifiers)
						{
							{
					  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));
					  			DifferenceToReference difference = learnAndEstimateDifference(ifDepth, initPTA,referenceGraph,c,Collections.<Label>emptyList(),Collections.<Label>emptyList());
	
						        uas_outcome.add(new Pair<Integer,String>(frame,"S"),difference.getValue());
								uas_S.add(frame+"C",difference.getValue());
							}
		
							{
								final Collection<Label> labelsToMergeTo=Collections.emptyList(), labelsToMergeFrom=Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter())});
					  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));
					  			DifferenceToReference difference = learnAndEstimateDifference(ifDepth, initPTA,referenceGraph,c,labelsToMergeTo,labelsToMergeFrom);
	
						        uas_outcome.add(new Pair<Integer,String>(frame,"S"),difference.getValue());
								uas_S.add(frame+"CM",difference.getValue());								
							}

							{
								final Collection<Label> labelsToMergeTo=Collections.emptyList(), labelsToMergeFrom=Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter())});
					  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));

								List<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new ArrayList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>(1000000);
								List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(initPTA,labelsToMergeTo,labelsToMergeFrom);
								int mergeScore = initPTA.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
								assert mergeScore >= 0:"initial PTA is inconsistent with the expectation that transitions lead to an initial state";
					  			initPTA = MergeStates.mergeCollectionOfVertices(initPTA, null, verticesToMerge);verticesToMerge = null;
					  			initPTA.pathroutines.updateDepthLabelling();
					  			DifferenceToReference difference = learnAndEstimateDifference(ifDepth, initPTA,referenceGraph,null,Collections.<Label>emptyList(),Collections.<Label>emptyList());
						        uas_outcome.add(new Pair<Integer,String>(frame,"T"),difference.getValue());
								uas_S.add(frame+"T",difference.getValue());								
							}
						}
					 */
			  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));
			  			DifferenceToReference difference = learnAndEstimateDifference(ifDepth, initPTA,referenceGraph,null,Collections.<Label>emptyList(),Collections.<Label>emptyList());
			  			uas_S.add(frame+"R",difference.getValue());
			  			synchronized(gr)
			  			{
			  				uas_S.drawInteractive(gr);
			  			}
					}
				};
				outcomes.add(executorService.submit(interactiveRunner));
			}
				
				/*
			for(final String seed:collectionOfTraces.keySet())
				if (!seed.equals(UAVAllSeeds))
				{// Just for all frames of the a single seed
					interactiveRunner = new Runnable() {

						@Override
						public void run() {
							final Classifier classifiers[] = loadClassifierFromArff(arffName);
							for(final Integer frame:allFrames)
							{
								TracesForSeed tracesForThisSeed = collectionOfTraces.get(seed);
								
								for(Classifier c:classifiers)
								{
						  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(tracesForThisSeed.tracesForUAVandFrame.get(UAVAll).get(frame));
						  			double difference = learnAndEstimateDifference(initPTA,referenceGraph,c,labelsToMergeTo,labelsToMergeFrom);

						  			uas_outcome.add(new Pair<Integer,String>(frame,"A"),difference);
									uas_A.add(""+frame,difference);
								}

								LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(tracesForThisSeed.tracesForUAVandFrame.get(UAVAll).get(frame));
					  			double difference = learnAndEstimateDifference(initPTA,referenceGraph,null,labelsToMergeTo,labelsToMergeFrom);
					  			uas_A.add(""+frame+"R",difference);
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
							interactiveRunner = new Runnable() {

								@Override
								public void run() {
									final Classifier classifiers[] = loadClassifierFromArff(arffName);
									for(final Integer frame:allFrames)
									{
										for(Classifier c:classifiers)
										{
								  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(seed).tracesForUAVandFrame.get(UAV).get(frame));
								  			double difference = learnAndEstimateDifference(initPTA,referenceGraph,c,labelsToMergeTo,labelsToMergeFrom);
											uas_outcome.add(new Pair<Integer,String>(frame,"U"),difference);
											uas_U.add(""+frame,difference);
										}
							  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(seed).tracesForUAVandFrame.get(UAV).get(frame));
							  			double difference = learnAndEstimateDifference(initPTA,referenceGraph,null,labelsToMergeTo,labelsToMergeFrom);
							  			uas_U.add(""+frame+"R",difference);
									}
								}
							};
							outcomes.add(executorService.submit(interactiveRunner));
						}
			/*
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
					        double difference = PairQualityLearner.estimationOfDifferenceFmeasure(referenceGraph, actualAutomaton, evaluationTestSet);
							uas_threshold.add(arg, difference);
						}
					}
					
				};
					outcomes.add(executorService.submit(interactiveRunner));
			}
*/
			ProgressIndicator progress = new ProgressIndicator("running concurrent experiment",outcomes.size());
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
   
   public static LearnerGraph mergePTA(LearnerGraph initialPTA,Label labelToMerge)
   {
	   LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
	   List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(initialPTA,Collections.<Label>emptyList(),
				Arrays.asList(new Label[]{labelToMerge}));
		if (initialPTA.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge) < 0)
			throw new IllegalArgumentException("inconsistent initial PTA: vertices that are associated with the unique state cannot be merged in the PTA");
		return MergeStates.mergeCollectionOfVertices(initialPTA, null, verticesToMerge);
   }
   
   public static LearnerGraph makeMerge(PaperUAS paper, String faileNameToWriteResultTo, String transitionNameToMerge) throws IOException
   {
	   LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
	   //String fileNameToLoad = faileNameToWriteResultTo+"-before_merging.xml";
	   //AbstractPersistence.loadGraph(fileNameToLoad, initialPTA, paper.learnerInitConfiguration.getLabelConverter());
	   initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
	   initialPTA.storage.writeGraphML(faileNameToWriteResultTo+"-before_merging.xml");
	   
		LearnerGraph outcome = mergePTA(initialPTA,AbstractLearnerGraph.generateNewLabel(transitionNameToMerge,initialPTA.config,paper.learnerInitConfiguration.getLabelConverter()));
		outcome.storage.writeGraphML(faileNameToWriteResultTo+"-after_merging.xml");
		return outcome;
   }
   
   /** Records the initial PTA. The Pta from the paper is recorded as the initial automaton and the automaton passed as an argument is recorded as the expected outcome of learning of the evaluation configuration. 
    */
   public void recordInitialConfiguration(LearnerGraph smallPTA) throws IOException
   {
       PTASequenceEngine engine = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber);
       
       FileOutputStream log = new java.io.FileOutputStream("resources/largePTA/VeryLargePTA.zip");
       RPNIBlueFringeVariability ourLearner = new RPNIBlueFringeVariability(learnerInitConfiguration,true,null,null);
       LearnerGraph automatonAfterInit = ourLearner.getLearner().init(engine,0,0);
       final Configuration shallowCopy = automatonAfterInit.config.copy();shallowCopy.setLearnerCloneGraph(false);
       LearnerGraph copyOfAutomaton = new LearnerGraph(shallowCopy);LearnerGraph.copyGraphs(automatonAfterInit, copyOfAutomaton);
       ourLearner.setInitPta(copyOfAutomaton);
       RecordProgressDecorator recorder = new RecordProgressDecorator(ourLearner.getLearner(),log,1,learnerInitConfiguration.config,true);
       learnerInitConfiguration.graph = smallPTA;learnerInitConfiguration.testSet = new LinkedList<List<statechum.Label>>();
		recorder.writeLearnerEvaluationData(learnerInitConfiguration);
		recorder.init(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		recorder.close();
		log.close();// double-close in fact (should already been closed by recorder) but it does not really matter.
   }

 	public LearnerGraph writeArff(final int ifDepth, LearnerGraph referenceGraph, String whereToWrite) throws Exception
	{
		
		LearnerEvaluationConfiguration initConfiguration = new LearnerEvaluationConfiguration(learnerInitConfiguration.config);
		initConfiguration.setLabelConverter(learnerInitConfiguration.getLabelConverter());// we do not copy if-then automata here because we do not wish to augment from if-then on every iteration because our properties are pairwise and this permits augmentation to be carried out first thing and not any more.
	    LearnerGraph initialPTA = new LearnerGraph(initConfiguration.config);
		initialPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber));
		/*
		try {
			initialPTA.storage.writeGraphML("resources/automaton_to_learn_arff_from.xml");
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		*/
		LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(learnerInitConfiguration.ifthenSequences, null, referenceGraph, initConfiguration.config, initConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
		initConfiguration.config.setUseConstraints(false);// do not use if-then during learning (refer to the explanation above)
		
		System.out.println(new Date().toString()+" Graph loaded: "+initialPTA.getStateNumber()+" states, adding at most "+ initConfiguration.config.getHowManyStatesToAddFromIFTHEN()+" if-then states");
		Transform.augmentFromIfThenAutomaton(initialPTA, null, ifthenAutomata, initConfiguration.config.getHowManyStatesToAddFromIFTHEN());// we only need  to augment our PTA once (refer to the explanation above).
		System.out.println(new Date().toString()+" if-then states added, now "+initialPTA.getStateNumber()+" states");
		WekaDataCollector dataCollector = PairQualityLearner.createDataCollector(ifDepth);
		// Run the learner that will find out how to select the correct pairs.
		LearnerThatCanClassifyPairs learnerOfPairs = new PairQualityLearner.LearnerThatUpdatesWekaResults(initConfiguration,referenceGraph,dataCollector,initialPTA);
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
       	LearnerGraph learntWithoutNegatives = new LearnerGraph(learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(actualAutomaton,learntWithoutNegatives);
		DifferentFSMException diff = WMethod.checkM(referenceGraph, learntWithoutNegatives);if (diff != null) throw diff;
		System.out.println(new Date().toString()+" arff written");
		return actualAutomaton;
	}
 	
 	
 	public static int computeLeafCount(LearnerGraph graph)
 	{
 		int count = 0;
 		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
 			if (entry.getValue().isEmpty()) ++count;
 		return count;
 	}
 	
 	public static int computeTransitionCount(LearnerGraph graph)
 	{
 		int count = 0;
 		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:graph.transitionMatrix.entrySet())
 			count+=entry.getValue().size();
 		return count;
 	}
 	
 	public static void computePTASize(String description,LearnerGraph pta, LearnerGraph referenceGraph)
 	{
 		final int stateCoverSize = referenceGraph.getAcceptStateNumber()*referenceGraph.pathroutines.computeAlphabet().size();
 		System.out.println(description+computeLeafCount(pta)/(double)stateCoverSize+" leaves, "+computeTransitionCount(pta)/(double)stateCoverSize+" transitions");
 		
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
       	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
       	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);

       	String arffName = "resources/largePTA/pairsEncounteredHuge.arff";
       	final int ifDepth = 1;
       	//paper.writeArff(ifDepth, referenceGraph,arffName);// this part can be skipped if arff has already been generated.
    	paper.runExperimentWithSingleAutomaton(ifDepth,"huge",arffName,referenceGraph);
		/*
    	Label uniqueLabel = AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", paper.learnerInitConfiguration.config,paper.learnerInitConfiguration.getLabelConverter());
	    LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
		initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
 		LearnerGraph smallPta = PairQualityLearner.mergeStatesForUnique(initialPTA,uniqueLabel);
       	PaperUAS.computePTASize("small pta: ",smallPta,referenceGraph);PaperUAS.computePTASize("huge pta: ",initialPTA,referenceGraph);
       	*/
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
        learnerConfig.setLearnerScoreMode(Configuration.ScoreMode.GENERAL);
        paper.loadReducedConfigurationFile(args[0]);
        
		final int offset=1;
    	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
    	int maxFrame = paper.getMaxFrame(inputFiles);
    	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
    	for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]);// refill the input (it was drained by the computation of maxFrame).
    	paper.loadData(inputFiles);
       	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
       	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);
    	
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");

       	
    	String arffName = "resources/largePTA/pairsEncounteredPartiallyMerged.arff";
    	/*

	    LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
		initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
		System.out.println(initialPTA.getAcceptStateNumber()+", total: "+initialPTA.getStateNumber());
		LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(paper.learnerInitConfiguration.ifthenSequences, null, referenceGraph, paper.learnerInitConfiguration.config, paper.learnerInitConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
		Transform.augmentFromIfThenAutomaton(initialPTA, null, ifthenAutomata,paper.learnerInitConfiguration.config.getHowManyStatesToAddFromIFTHEN());// we only need  to augment our PTA once (refer to the explanation above).
		System.out.println("After if-then, "+initialPTA.getAcceptStateNumber()+", total: "+initialPTA.getStateNumber());
		System.out.println(initialPTA.getAcceptStateNumber()+", total: "+initialPTA.getStateNumber());
 	   LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
 		System.out.println("constructing vertices to merge");
 		List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(initialPTA,Collections.<Label>emptyList(),
 				Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", paper.learnerInitConfiguration.config,paper.learnerInitConfiguration.getLabelConverter())}));
 		if (initialPTA.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge) < 0)
 			throw new IllegalArgumentException("inconsistent initial PTA: vertices that lead to unique state in the reference graph cannot be merged in the PTA");
 		System.out.println("done attempt to merge, everything ok");
 		PairQualityLearner.ReferenceLearner learner = new PairQualityLearner.ReferenceLearner(null,paper.learnerInitConfiguration,referenceGraph,initialPTA);
        final LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
        DifferentFSMException different = WMethod.checkM(referenceGraph, actualAutomaton);
 		if (different != null)
 			throw different;
		 */
        final int ifDepth = 0;
    	paper.writeArff(ifDepth, referenceGraph,arffName);
    	paper.runExperimentWithSmallAutomata(ifDepth, arffName,referenceGraph);
    			//Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", paper.learnerInitConfiguration.config,paper.learnerInitConfiguration.getLabelConverter())}));
	}

	/**
	 * @param args trace file to load.
     * @throws IOException 
	 */
	public static void main(String[] args) throws Exception 
	{
		try
		{
			//checkSmallPTA();
			//checkDataConsistency();
			//mainCheckMerging(args);
	        //mainSingleHugeAutomaton(args);
			//noveltyInCaseStudyExperiment(args);
			mainSmallAutomata(args);
	       /*
			PaperUAS paper = new PaperUAS();
			paper.learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
	        final Configuration learnerConfig = paper.learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
	        learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
	        learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
	       	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
	       	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);
	       	Visualiser.updateFrame(referenceGraph, null);Visualiser.waitForKey();
	       	*/
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
	public static void generateHugeArff(final int ifDepth, @SuppressWarnings("unused") String[] args) throws Exception 
	{
		final Configuration learnerConfig = Configuration.getDefaultConfiguration().copy();learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
        learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
        learnerConfig.setAskQuestions(false);
        
   		final InitialConfigurationAndData initialConfigAndData = PairQualityLearner.loadInitialAndPopulateInitialConfiguration(PairQualityLearner.largePTAFileName, learnerConfig, new Transform.InternStringLabel());

		LearnerGraph referenceGraph = new LearnerGraph(initialConfigAndData.initial.graph.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraph, initialConfigAndData.learnerInitConfiguration.getLabelConverter());
    	WekaDataCollector dataCollector = PairQualityLearner.createDataCollector(ifDepth);
    	LearnerThatCanClassifyPairs learnerOfPairs = new PairQualityLearner.LearnerThatUpdatesWekaResults(initialConfigAndData.learnerInitConfiguration,referenceGraph,dataCollector,initialConfigAndData.initial.graph);
		learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		
		FileWriter wekaInstances=new FileWriter("resources/largePTA/pairsEncountered3.arff");
		wekaInstances.write(dataCollector.trainingData.toString());
		wekaInstances.close();
	}
	
}
