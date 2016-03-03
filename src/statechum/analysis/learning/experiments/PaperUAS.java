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
import java.io.FileNotFoundException;
import java.io.FileReader;
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
import java.util.Map.Entry;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.PathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.Label;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ReferenceLearner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ReferenceLearner.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceDiff;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceLanguageBCR;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResult;
import statechum.analysis.learning.experiments.PaperUAS.TracesForSeed.Automaton;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
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
	
	public Map<String,TracesForSeed> getCollectionOfTraces()
	{
		return collectionOfTraces;
	}
	/** The maximal frame number encountered. */
	protected int maxFrameNumber = -1;
	
	public int getMaxFrameNumber()
	{
		return maxFrameNumber;
	}
	
	/** When processing graphs with numerous data points, the task is to cluster those points so as not to have too many of them
	 * on the graphs. The easiest way is to divide a frame number by the maximal number so as to get %%.
	 */
	protected int divisor=1;
	
	public int getDivisor()
	{
		return divisor;
	}
	
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
	
	
    /** Updates the (provided as an argument) collection of traces for a specific UAV with a new trace. */
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
		
		
      	constructSequencesForAllUAVandFrame(data, frameNumbers);
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
    					{// constructs a negative sequence by appending to the current positive trace. The positive trace itself is preserved because we'll be appending any positive sequence to come to it.  
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
            	
          		// Ensure that only one of the two is present.
          		if (seed == UAVAllSeeds)
          		{
          			assert !dataForSeed.collectionOfPositiveTraces.containsKey(UAVAll);
          			assert !dataForSeed.collectionOfNegativeTraces.containsKey(UAVAll);

          			assert dataForSeed.collectionOfPositiveTraces.containsKey(UAVAllSeeds);
          			assert dataForSeed.collectionOfNegativeTraces.containsKey(UAVAllSeeds);
          		}
          		else
          		{
          			assert !dataForSeed.collectionOfPositiveTraces.containsKey(UAVAllSeeds);
          			assert !dataForSeed.collectionOfNegativeTraces.containsKey(UAVAllSeeds);
          		}
       
			}
		});
		
        // This one adds the last positive trace from the last frame. It would not be added otherwise because the above loop is looking for a new frame which will not appear 
        // once they have all been dealt with.
        for(String seed:data.keySet())
        	if (!seed.equals(UAVAllSeeds))
	        for(String UAV:UAVs)
	        	if (!UAV.equals(UAVAll) && !UAV.equals(UAVAllSeeds))
	        		AddLastPositiveTrace(seed, UAV, data);

      	constructSequencesForAllUAVandFrame(data, frameNumbers);
   }
    

    protected void constructSequencesForAllUAVandFrame(Map<String,TracesForSeed> data,Set<Integer> frameNumbers)
    {
  		Configuration config = Configuration.getDefaultConfiguration().copy();
   		
   		config.setGeneralisationThreshold(0);config.setGdFailOnDuplicateNames(false);
      config.setGdLowToHighRatio(0.75);config.setGdKeyPairThreshold(0.5);
      config.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);
      //config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
      config.setAskQuestions(false);config.setDebugMode(false);
      config.setLearnerScoreMode(Configuration.ScoreMode.ONLYOVERRIDE);

      collectionOfTraces.clear();
    	lexer=null;
    	for(Entry<String,TracesForSeed> entry:data.entrySet())
    	{
    		TracesForSeed traceDetails = entry.getValue(), newData = new TracesForSeed();
/*
    		for(Entry<String,Map<Integer,Set<List<Label>>>> uavToFrameDetails:traceDetails.collectionOfPositiveTraces.entrySet())
	    		for(Entry<Integer,Set<List<Label>>> frameToData:uavToFrameDetails.getValue().entrySet())
		    		for(List<Label> trace:frameToData.getValue())
		    		{
		    			boolean foundValue = false;
		      		LearnerGraph pta = new LearnerGraph(config);
			    		pta.paths.augmentPTA(trace,true,false,null);
			 				for (Label l:pta.pathroutines.computeAlphabet())
			 					if (l.toString().equals("Data_Deprecates_Waypoint"))
			 					{
			 						foundValue = true;
			 						System.out.println("BEFORE BUILDING TREES: Data_Deprecates_Waypoint is in seed "+entry.getKey()+" and UAV "+uavToFrameDetails.getKey());break;
			 					}
			 				
			 				if (foundValue)
			 					break;
		    		}
*/
    		if (entry.getKey() == UAVAllSeeds)
    		{
    			assert !traceDetails.collectionOfPositiveTraces.containsKey(UAVAll);
    			assert !traceDetails.collectionOfNegativeTraces.containsKey(UAVAll);

    			assert traceDetails.collectionOfPositiveTraces.containsKey(UAVAllSeeds);
    			assert traceDetails.collectionOfNegativeTraces.containsKey(UAVAllSeeds);
    		}
    		else
    		{
    			assert !traceDetails.collectionOfPositiveTraces.containsKey(UAVAllSeeds);
    			assert !traceDetails.collectionOfNegativeTraces.containsKey(UAVAllSeeds);
    		}

    		collectionOfTraces.put(entry.getKey(), newData);
    		newData.tracesForUAVandFrame=new TreeMap<String,Map<Integer,PTASequenceEngine>>();
    		System.out.println("*** processing seed "+entry.getKey()+", positive entries ***");
    		turnTracesIntoPTAs(newData.tracesForUAVandFrame,traceDetails.collectionOfPositiveTraces,true,frameNumbers);
    		newData.collectionOfPositiveTraces=null;// garbage collect traces, they are now part of PTA

    		System.out.println("*** processing seed "+entry.getKey()+", negative entries ***");
    		turnTracesIntoPTAs(newData.tracesForUAVandFrame,traceDetails.collectionOfNegativeTraces,false,frameNumbers);
    		newData.collectionOfNegativeTraces=null;// garbage collect traces, they are now part of PTA
    		
    		// Ensure that only one of the two is present.
    		if (newData.tracesForUAVandFrame.containsKey(UAVAllSeeds))
    		{
    			assert entry.getKey() == UAVAllSeeds;
    			assert !newData.tracesForUAVandFrame.containsKey(UAVAll);
    		}
    		else
    		{
    			assert entry.getKey() != UAVAllSeeds;
    			assert newData.tracesForUAVandFrame.containsKey(UAVAll);
    			assert !newData.tracesForUAVandFrame.containsKey(UAVAllSeeds);
    		}
    		

    		String uavToUse = (entry.getKey() == UAVAllSeeds)?UAVAllSeeds:UAVAll;
    		for(Entry<Integer,PTASequenceEngine> frameToData:newData.tracesForUAVandFrame.get(uavToUse).entrySet())
    		{
    			boolean foundValue = false;
      		LearnerGraph pta = new LearnerGraph(config);
	    		pta.paths.augmentPTA(frameToData.getValue());
	 				for (Label l:pta.pathroutines.computeAlphabet())
	 					if (l.toString().equals("Data_Deprecates_Waypoint"))
	 					{
	 						foundValue = true;
	 						System.out.println("AFTER AUGMENT: Data_Deprecates_Waypoint is in seed "+entry.getKey());break;
	 					}
	 				
	 				if (foundValue)
	 					break;
    		}

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
    
    /** UAV(frame) should be a sum of data across all frames before the specific frame.
     * Such a calculation is done after we load all data because frames from different UAVs might
     * not be synchronized and we need to accumulate data across all of them for UAVAll.
     *   
     * @param whatToUpdate collection of traces to add to
     * @param traces traces to add
     * @param isAccept whether traces to be added are accept or reject-traces
     * @param frameNumbers all possible frames, to ensure that all UAV frame numbers range over the same set. Should be an ordered set, such as a TreeSet.
     * @param UAVs names of all UAVs, used to ensure that all UAV frame numbers range over the same set.
     */
    protected void turnTracesIntoPTAs(Map<String,Map<Integer,PTASequenceEngine>> whatToUpdate,
    		Map<String,Map<Integer,Set<List<Label>>>> traces, boolean isAccept, Set<Integer> frameNumbers)
	{
        for(String uav:traces.keySet())
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
        			if (GlobalConfiguration.getConfiguration().isAssertEnabled())
        			{
        				Set<Integer> usedFrames = new TreeSet<Integer>();usedFrames.addAll(traces.get(uav).keySet());usedFrames.removeAll(frameNumbers);
        				assert(usedFrames.isEmpty());// ensure that there are no frames that are not going to be iterated through.
        			}
        			for(int earlierFrame:frameNumbers)
        			{// here we accumulate traces for all frames up to and including the current frame in the traceDetailsUAV trace collection.
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
        		
        		/*
        		Configuration config = Configuration.getDefaultConfiguration().copy();
         		
         		config.setGeneralisationThreshold(0);config.setGdFailOnDuplicateNames(false);
            config.setGdLowToHighRatio(0.75);config.setGdKeyPairThreshold(0.5);
            config.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);
            //config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
            config.setAskQuestions(false);config.setDebugMode(false);
            config.setLearnerScoreMode(Configuration.ScoreMode.ONLYOVERRIDE);
                 
        		LearnerGraph pta = new LearnerGraph(config);
        		pta.paths.augmentPTA(traceDetailsUAV);
        		
     				for (Label l:pta.pathroutines.computeAlphabet())
     					if (l.toString().equals("Data_Deprecates_Waypoint"))
     						System.out.println("AFTER AUGMENTING: Data_Deprecates_Waypoint is in uav " + uav +" and frame "+frame);
     						*/
        	}
      }
      
	}
    
    public LearnerEvaluationConfiguration learnerInitConfiguration = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
   

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
   
   public void LearnReferenceAutomaton(LearningAlgorithms.ReferenceLearner.ScoringToApply scoringToUse) throws Exception
   {
	   long tmStarted = new Date().getTime();
       LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber));
       final LearnerGraph graphReference = new LearningAlgorithms.ReferenceLearner(learnerInitConfiguration,initPTA,scoringToUse).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
       long tmFinished = new Date().getTime();
       System.out.println("Learning reference complete, "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
       graphReference.storage.writeGraphML("traceautomaton.xml");
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
   
   public static LearnerGraph mergePTA(LearnerGraph initialPTA,Label labelToMerge, boolean buildAuxInfo)
   {
	   LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
	   List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(initialPTA,Collections.<Label>emptyList(),
				Arrays.asList(new Label[]{labelToMerge}));
		if (initialPTA.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge,buildAuxInfo) < 0)
			throw new IllegalArgumentException("inconsistent initial PTA: vertices that are associated with the unique state cannot be merged in the PTA");
		return MergeStates.mergeCollectionOfVertices(initialPTA, null, verticesToMerge, buildAuxInfo);
   }
   
   public static LearnerGraph makeMerge(PaperUAS paper, String faileNameToWriteResultTo, String transitionNameToMerge, boolean buildAuxInfo) throws IOException
   {
	   LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
	   //String fileNameToLoad = faileNameToWriteResultTo+"-before_merging.xml";
	   //AbstractPersistence.loadGraph(fileNameToLoad, initialPTA, paper.learnerInitConfiguration.getLabelConverter());
	   initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
	   initialPTA.storage.writeGraphML(faileNameToWriteResultTo+"-before_merging.xml");
	   
		LearnerGraph outcome = mergePTA(initialPTA,AbstractLearnerGraph.generateNewLabel(transitionNameToMerge,initialPTA.config,paper.learnerInitConfiguration.getLabelConverter()),buildAuxInfo);
		outcome.storage.writeGraphML(faileNameToWriteResultTo+"-after_merging.xml");
		return outcome;
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
	
	public static class UASExperiment implements Callable<ThreadResult>
	{
		protected final LearnerEvaluationConfiguration learnerInitConfiguration;
		protected final LearnerGraph referenceGraph;
		protected final String experimentTitle, inputGraphFileName;
		protected final boolean useIfThen;

		public UASExperiment(LearnerEvaluationConfiguration eval, LearnerGraph referenceGraph, String experimentName, String inputGraphFileName, boolean useIfThen)
		{
			learnerInitConfiguration = eval;this.referenceGraph = referenceGraph;this.experimentTitle = experimentName;this.useIfThen = useIfThen;
			this.inputGraphFileName = inputGraphFileName;
		}

		protected LearnerGraph buildPTA() throws AugmentFromIfThenAutomatonException, IOException
		{
 		    LearnerGraph pta = new LearnerGraph(learnerInitConfiguration.config);AbstractPersistence.loadGraph(PaperUAS.fileName(inputGraphFileName), pta, learnerInitConfiguration.getLabelConverter());
 		    
 			//pta.storage.writeGraphML("resources/"+experimentName+"-initial.xml");
 		    if (useIfThen)
 		    {
 		    	Collection<String> filteredLTLsequences = new TreeSet<String>(learnerInitConfiguration.ifthenSequences);
 		    	for(String str:learnerInitConfiguration.ifthenSequences)
 		    	{
 		    		boolean elemFound = false;
 		    		for(Label elem:pta.pathroutines.computeAlphabet())
 		    			if (str.contains(elem.toString()))
 		    			{// a match here could be spurious because str contains other text, however for the purpose of UAS experiment, label names are sufficiently distinctive so that it is not a problem.
 		    				elemFound = true;break;
 		    			}
 		    		
 		    		if (!elemFound)
 		    			filteredLTLsequences.remove(str);
 		    	}
	 			LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(filteredLTLsequences, pta.pathroutines.computeAlphabet(), learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
	 			Transform.augmentFromIfThenAutomaton(pta, null, ifthenAutomata, learnerInitConfiguration.config.getHowManyStatesToAddFromIFTHEN());// we only need  to augment our PTA once.
	 			//pta.storage.writeGraphML("resources/"+experimentName+"-afterifthen.xml");
 		    }	
			return pta;
		}
		
		
		protected String constructFileName(String experimentSuffix)
		{
			return inputGraphFileName+"_"+experimentSuffix+".xml";
		}
		
		/** The outcome of learning might have been stored in a file from the previous run. For this reason, it makes sense to try to load it. 
		 * @throws IOException if the outcome of learning exists but cannot be loaded
		 */
		protected LearnerGraph loadOutcomeOfLearning(String experimentSuffix)
		{
			LearnerGraph outcome = null;
			String graphFileName = constructFileName(experimentSuffix);
			
	    	if (new File(graphFileName).canRead())
	    	{
		    	outcome = new LearnerGraph(learnerInitConfiguration.config);
	    		try
					{
	    			AbstractPersistence.loadGraph(graphFileName, outcome, learnerInitConfiguration.getLabelConverter());
					} catch (IOException e)
					{
						System.out.println("ERROR LOADING OUTCOME OF LEARNING \""+experimentSuffix+"\", exception text: "+e.getMessage());return null;
					}
	    		catch (IllegalArgumentException e)
					{
						System.out.println("ERROR LOADING OUTCOME OF LEARNING \""+experimentSuffix+"\", exception text: "+e.getMessage());return null;
					}
	    	}
	    	
	    	return outcome;
		}
		
		protected void saveOutcomeOfLearning(String experimentSuffix, LearnerGraph outcome) throws IOException
		{
			outcome.storage.writeGraphML(constructFileName(experimentSuffix));	
		}
		
		@Override
		public ThreadResult call() throws Exception 
		{
			ThreadResult outcome = new ThreadResult();
			for(ReferenceLearner.ScoringToApply scoringMethod:new ReferenceLearner.ScoringToApply[]{ReferenceLearner.ScoringToApply.SCORING_EDSM,ReferenceLearner.ScoringToApply.SCORING_SICCO})
			{
	 			PairQualityLearner.SampleData sample = new PairQualityLearner.SampleData();sample.experimentName = experimentTitle;
				
				{
					String experimentName = "usual_"+scoringMethod.toString();
					LearnerGraph actualAutomaton = loadOutcomeOfLearning(experimentName);
					if(actualAutomaton == null)
					{
						LearnerGraph pta = buildPTA();
			 			ReferenceLearner learner = new ReferenceLearner(learnerInitConfiguration, pta,scoringMethod);
			 			
			 			actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
			 			saveOutcomeOfLearning(experimentName,actualAutomaton);
					}		
		 			DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		 			DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		 			actualAutomaton.setName(experimentName);
		 			//Visualiser.updateFrame(actualAutomaton,referenceGraph);
		 			sample.referenceLearner = new ScoresForGraph(); 
		 			sample.referenceLearner.differenceStructural = diffMeasure;sample.referenceLearner.differenceBCR = bcrMeasure;
		 			sample.referenceLearner.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
				}
				
	 			Label uniqueLabel = AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter());
	 			
	 			if (scoringMethod != ScoringToApply.SCORING_SICCO)
				{// pre-merge and then learn. SICCO scoring expects learning from a PTA and hence is not easy to apply.
	 				String experimentName = "premerge_"+scoringMethod.toString();
					LearnerGraph actualAutomaton = loadOutcomeOfLearning(experimentName);
					if(actualAutomaton == null)
					{
						LearnerGraph smallPta = mergePTA(buildPTA(),uniqueLabel,false);
						ReferenceLearner learner = new ReferenceLearner(learnerInitConfiguration, smallPta,scoringMethod);
	
		 				actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
			 			saveOutcomeOfLearning(experimentName,actualAutomaton);
					}		
		 			
		 			DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		 			DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		 			actualAutomaton.setName(experimentName);
		 			//Visualiser.updateFrame(actualAutomaton,referenceGraph);
		 			sample.actualLearner = new ScoresForGraph();
		 			sample.actualLearner.differenceStructural = diffMeasure;sample.actualLearner.differenceBCR = bcrMeasure;
		 			sample.actualLearner.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
				}
	
				{// conventional learning, but check each merger against the unique-label merge
					String experimentName = "checkunique_"+scoringMethod.toString();
					LearnerGraph actualAutomaton = loadOutcomeOfLearning(experimentName);
					if(actualAutomaton == null)
					{
						LearnerGraph pta = buildPTA();
			 			ReferenceLearner learner = new ReferenceLearner(learnerInitConfiguration, pta,scoringMethod);
			 			learner.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueLabel}));
		
			 			actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
			 			saveOutcomeOfLearning(experimentName,actualAutomaton);
					}		
		 			
		 			DifferenceToReferenceDiff diffMeasure = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, learnerInitConfiguration.config, 1);
		 			DifferenceToReferenceLanguageBCR bcrMeasure = DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, actualAutomaton,learnerInitConfiguration.testSet);
		 			actualAutomaton.setName(experimentName);
		 			//Visualiser.updateFrame(actualAutomaton,referenceGraph);
		 			sample.actualConstrainedLearner = new ScoresForGraph(); 
		 			sample.actualConstrainedLearner.differenceStructural = diffMeasure;sample.actualConstrainedLearner.differenceBCR = bcrMeasure;
		 			sample.actualConstrainedLearner.nrOfstates = new PairQualityLearner.DifferenceOfTheNumberOfStates(actualAutomaton.getStateNumber() - referenceGraph.getStateNumber());
				}
				
				outcome.samples.add(sample);
			}
 			return outcome;
		}		
	}
	
	/** Given a collection of frames and a value, identifies the closest frame from the set to the value.
	 * 
	 * @param frames frames to look in
	 * @param value what to search.
	 * @return value from the set that most closely matches the value. Throws an exception if the set of frames is empty.
	 */
	public static int constructFractionOfFrameNumberThatIsAvailable(Set<Integer> frames,long value)
	{
		// since we have a relatively small set of frames and do not need to do the search often, linear search is sufficient.
		int prevValue = -1;
		for(Integer v:frames)
		{
			if (v < 0)
				throw new IllegalArgumentException("All frames should be non-negative");
			
			if (v < value)
				prevValue = v;
			else
			if (v == value)
				return v;
			else
			{// transition: we have seen value > prevValue, now v > value
					return prevValue;
			}
		}
		
		if (prevValue < 0)
			throw new IllegalArgumentException("The set of frames to look in should be non-empty");
		
		return prevValue;
	}
	
	public static void printLastFrame(String name,Set<Integer> frames)
	{
		int v = constructFractionOfFrameNumberThatIsAvailable(frames,Integer.MAX_VALUE);
		System.out.printf("last frame of %s is %d\n",name,v);
	}
	
	public static void augmentPTAWithTracesForFrameRange(LearnerGraph pta,Map<Integer,PTASequenceEngine> frameToTraces,long value)
	{
		int v=constructFractionOfFrameNumberThatIsAvailable(frameToTraces.keySet(),value);
		pta.paths.augmentPTA(frameToTraces.get(v));
	}

 	public static String fileName(String arg)
 	{
 		return arg+".xml";
 	}
 	
 	/** Loads traces using the supplied args. Will concatenate them if the <i>concatenateTraces</i> argument is true.
 	 * 
 	 * @param args first is a path to most files, followed by a configuration file parameters.txt and then all the seed files. These are all expected to be in the UASpaper directory.
 	 * @param concatenateTraces whether to concatenate traces (as they will be when observed from an actual experiment).
 	 * @return constructed instance of the paper.
 	 * @throws FileNotFoundException 
 	 */
 	public static PaperUAS loadTraces(String args[], boolean concatenateTraces) throws FileNotFoundException
 	{
		ConvertALabel labelConverter = new Transform.InternStringLabel();
		PaperUAS paper = new PaperUAS();
 		
 		paper.learnerInitConfiguration.setLabelConverter(labelConverter);
         final Configuration learnerConfig = paper.learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
         learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);
         learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);
         //learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
         learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
         learnerConfig.setLearnerScoreMode(Configuration.ScoreMode.ONLYOVERRIDE);
         
         String path = args[0];
         
         paper.loadReducedConfigurationFile(path+File.separator+args[1]);
         
 		final int offset=2;
     	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(path+File.separator+args[i]); 
     	int maxFrame = paper.getMaxFrame(inputFiles);
     	paper.divisor = (maxFrame+1)/20;// the +1 ensures that the last class of frames includes the last point.
     	for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(path+File.separator+args[i]);// refill the input (it was drained by the computation of maxFrame).
     	
     	if (concatenateTraces)
     		paper.loadDataByConcatenation(inputFiles);
     	else
     		paper.loadData(inputFiles);
 		
     	return paper;
 	}
 	
	public static String sprintf(String format,Object ...args)
	{
		java.io.ByteArrayOutputStream tmpOutput = new java.io.ByteArrayOutputStream();
		new java.io.PrintStream(tmpOutput).printf(format, args);
		return tmpOutput.toString();
	}
	
	public final static String nameForExperimentRun = "feb_2016_3_hashmap";
	
	
	// A very simple experiment that learns from absolutely all traces and that's it. Expects the initial traces to be recorded in an appropriate configuration file rather than loaded
	// from the source data, which makes learning start much faster.
 	public static void mainA(String args[]) throws Exception
 	{
		String outDir = "tmp"+File.separator+nameForExperimentRun;
		if (!new java.io.File(outDir).isDirectory())
		{
			if (!new java.io.File(outDir).mkdir())
			{
				System.out.println("failed to create a work directory");return;
			}
		}
		String outPathPrefix = outDir + File.separator;
		ConvertALabel labelConverter = new Transform.InternStringLabel();
		PaperUAS paper = new PaperUAS();
 		
    	System.out.println(new Date()+" started");
 		paper.learnerInitConfiguration.setLabelConverter(labelConverter);
         final Configuration learnerConfig = paper.learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
         learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);
         learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);//learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
         learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);learnerConfig.setUseConstraints(false);
         learnerConfig.setLearnerScoreMode(Configuration.ScoreMode.ONLYOVERRIDE);
     	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
     	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);
     	paper.learnerInitConfiguration.testSet = PaperUAS.computeEvaluationSet(referenceGraph,referenceGraph.getAcceptStateNumber()*3,referenceGraph.getAcceptStateNumber()*referenceGraph.pathroutines.computeAlphabet().size());
     	paper.learnerInitConfiguration.config.setUseConstraints(false);// do not use if-then during learning (enough to augment once)
         String path = args[0];
        paper.loadReducedConfigurationFile(path+File.separator+args[1]);
    	/*
 		final int offset=2;
     	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(path+File.separator+args[i]); 
     	int maxFrame = paper.getMaxFrame(inputFiles);
     	paper.divisor = (maxFrame+1)/20;// the +1 ensures that the last class of frames includes the last point.
     	for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(path+File.separator+args[i]);// refill the input (it was drained by the computation of maxFrame).
     	paper.loadDataByConcatenation(inputFiles);
		LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
		Map<Integer,PTASequenceEngine> framesToTraces = paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds); 
		initialPTA.paths.augmentPTA(framesToTraces.get(paper.maxFrameNumber));
    	System.out.println(new Date()+" constructed graph");
		initialPTA.storage.writeGraphML(sprintf("all.xml",outPathPrefix));
		System.out.println("all alphabet: "+initialPTA.pathroutines.computeAlphabet());
		PairQualityLearner.loadInitialAndPopulateInitialConfiguration(PairQualityLearner.veryLargePTAFileName, mainConfiguration, labelConverter).initial.graph.storage.writeGraphML(sprintf("huge.xml",outPathPrefix));;
		System.out.println("huge alphabet: "+initialPTA.pathroutines.computeAlphabet());
		*/
		UASExperiment experiment = new UASExperiment(paper.learnerInitConfiguration,referenceGraph,"all",outPathPrefix+"uas-All",true);
    	experiment.call();
 	}	

 	// A very simple experiment that learns from absolutely all traces; it also records the initial traces in a file.
 	// Arguments: first is a path to most files, followed by a configuration file parameters.txt and then all the seed files.
 	// Example: 
 	// C:\experiment\research\xmachine\ModelInferenceUAS\traces parameters.txt seed1_d.txt seed2_d.txt seed3_d.txt seed4_d.txt seed5_d.txt seed6_d.txt seed7_d.txt seed8_d.txt seed9_d.txt seed10_d.txt seed11_d.txt seed12_d.txt seed13_d.txt  seed14_d.txt seed15_d.txt seed16_d.txt seed17_d.txt seed18_d.txt seed19_d.txt
 	public static void mainB(String args[]) throws Exception
 	{
		String outDir = "tmp"+File.separator+nameForExperimentRun;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		if (!new java.io.File(outDir).isDirectory())
		{
			if (!new java.io.File(outDir).mkdir())
			{
				System.out.println("failed to create a work directory");return ;
			}
		}
		String outPathPrefix = outDir + File.separator;
     	PaperUAS paper = loadTraces(args,false);
    	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
    	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);
    	paper.learnerInitConfiguration.testSet = PaperUAS.computeEvaluationSet(referenceGraph,referenceGraph.getAcceptStateNumber()*3,referenceGraph.getAcceptStateNumber()*referenceGraph.pathroutines.computeAlphabet().size());

    	paper.learnerInitConfiguration.config.setUseConstraints(false);// do not use if-then during learning (enough to augment once)
		Map<Integer,PTASequenceEngine> framesToTraces = null; 
		// compute the maximal depth for filtering.
		int depth = -1;
		{// load the data
			framesToTraces = paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds); 
			LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
			initialPTA.paths.augmentPTA(framesToTraces.get(paper.maxFrameNumber));
			for(CmpVertex vert:initialPTA.transitionMatrix.keySet())		
				depth = Math.max(depth, vert.getDepth());
			System.out.println("maximal depth: "+depth);
		}
		
		{// process all the traces from all UAVs and seeds in one go
			String graphName = outPathPrefix+"uas-All";
			if (!new File(PaperUAS.fileName(graphName)).canRead())
			{
				LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
				initialPTA.paths.augmentPTA(framesToTraces.get(paper.maxFrameNumber));
				initialPTA.storage.writeGraphML(PaperUAS.fileName(graphName));
			}
			UASExperiment experiment = new UASExperiment(paper.learnerInitConfiguration,referenceGraph,"All",graphName,true);
			experiment.call();
		}
	}	
 	
 	/** Returns a string padded to the specified width with the supplied character.
 	 * 
 	 * @param whatToPad
 	 * @param ch character to pad with
 	 * @param length the length to pad to
 	 * @return
 	 */
 	public static String padString(String whatToPad, char ch, int length)
 	{
 		StringBuffer buf = new StringBuffer();
 		for(int i=0;i<length-whatToPad.length();++i)
 			buf.append(ch);
 		buf.append(whatToPad);
 		return buf.toString();
 	}
 	
 	// Arguments: first is a path to most files, followed by a configuration file parameters.txt and then all the seed files.
 	// Example: 
 	// C:\experiment\research\xmachine\ModelInferenceUAS\traces parameters.txt seed1_d.txt seed2_d.txt seed3_d.txt seed4_d.txt seed5_d.txt seed6_d.txt seed7_d.txt seed8_d.txt seed9_d.txt seed10_d.txt seed11_d.txt seed12_d.txt seed13_d.txt  seed14_d.txt seed15_d.txt seed16_d.txt seed17_d.txt seed18_d.txt seed19_d.txt
 	public static void main(String args[]) throws Exception
 	{
		String outDir = "tmp"+File.separator+nameForExperimentRun;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		if (!new java.io.File(outDir).isDirectory())
		{
			if (!new java.io.File(outDir).mkdir())
			{
				System.out.println("failed to create a work directory");return ;
			}
		}
		String outPathPrefix = outDir + File.separator;
		/*
		InitialConfigurationAndData initialConfigurationData = PairQualityLearner.loadInitialAndPopulateInitialConfiguration(PairQualityLearner.veryLargePTAFileName, mainConfiguration, labelConverter);
		LearnerGraph smallGraph = new LearnerGraph(initialConfigurationData.learnerInitConfiguration.graph,mainConfiguration);
		Set<Label> alphabetToUse = smallGraph.pathroutines.computeAlphabet();
		LearnerGraph hugeGraph = new LearnerGraph(initialConfigurationData.initial.graph,mainConfiguration);
		
		
		System.out.println("Huge: "+hugeGraph.getStateNumber()+" states, "+(hugeGraph.getStateNumber()-hugeGraph.getAcceptStateNumber())+" reject states");
		System.out.println("Small: "+smallGraph.getStateNumber()+" states, "+(smallGraph.getStateNumber()-smallGraph.getAcceptStateNumber())+" reject states");
		System.out.printf("positive leaf nodes: %d, transitions: %d\n",computeLeafCount(hugeGraph)-(hugeGraph.getStateNumber()-hugeGraph.getAcceptStateNumber()),computeTransitionCount(hugeGraph));
*/
     	PaperUAS paper = loadTraces(args,true);
    	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
    	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);
    	paper.learnerInitConfiguration.testSet = PaperUAS.computeEvaluationSet(referenceGraph,referenceGraph.getAcceptStateNumber()*3,referenceGraph.getAcceptStateNumber()*referenceGraph.pathroutines.computeAlphabet().size());

    	paper.learnerInitConfiguration.config.setUseConstraints(false);// do not use if-then during learning (enough to augment once)
		RunSubExperiment<ThreadResult> experimentRunner = new RunSubExperiment<PairQualityLearner.ThreadResult>(ExperimentRunner.getCpuNumber(),"data",new String[]{PhaseEnum.RUN_STANDALONE.toString()});
   	
    	// Experiments:
    	// all UAV, all data (to show that even having all data does not help)
    	// for each seed, all UAVs (all frames), 
    	// for each seed, all UAVs for a subset of frames  (to show that with a good start, we can learn from a subset of frames).
    	// for each seed, each UAV (all frames), 
    	// for each seed, each UAV for a subset of frames.
    	
    	// Variation on the experiment: 
    	// EDSM/Sicco scoring
    	// ifthen/ no ifthen (also no point)
    	// positive only or pos-neg (no point)
    	// EDSM/check constraints but merge EDSM-way/premerge on the transition of interest.
    	
		final DrawGraphs gr = new DrawGraphs();
		final RBoxPlot<String> BCR_vs_experiment = new RBoxPlot<String>("experiment","BCR",new File(outPathPrefix+"BCR_vs_experiment.pdf"));
		final RBoxPlot<String> diff_vs_experiment = new RBoxPlot<String>("experiment","Structural difference",new File(outPathPrefix+"diff_vs_experiment.pdf"));

    	processSubExperimentResult<PairQualityLearner.ThreadResult> resultHandler = new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

			public void recordResultsFor(RunSubExperiment<ThreadResult> experimentrunner, String experimentName,ReferenceLearner.ScoringToApply scoring,ScoresForGraph difference) throws IOException
			{
				String scoringAsString = null;
				switch(scoring)
				{
				case SCORING_EDSM:
					scoringAsString = "E";break;
				case SCORING_SICCO:
					scoringAsString = "D";break;
				default:
					throw new IllegalArgumentException("Unexpected scoring");
				}
				
				System.out.println(experimentName + "_" + scoringAsString+" has BCR  score of "+difference.differenceBCR.getValue() +" and diffscore " + difference.differenceStructural.getValue()+
						", learning outcome has "+difference.nrOfstates.getValue());
				experimentrunner.Record(BCR_vs_experiment,experimentName + "_" + scoringAsString ,difference.differenceBCR.getValue(),null,null);
				experimentrunner.Record(diff_vs_experiment,experimentName + "_" + scoringAsString ,difference.differenceStructural.getValue(),null,null);
			}
			@Override
			public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
			{
				PairQualityLearner.SampleData edsmScore = result.samples.get(0);
				recordResultsFor(experimentrunner, edsmScore.experimentName+"_R",ReferenceLearner.ScoringToApply.SCORING_EDSM,edsmScore.referenceLearner);
				recordResultsFor(experimentrunner, edsmScore.experimentName+"_C",ReferenceLearner.ScoringToApply.SCORING_EDSM,edsmScore.actualConstrainedLearner);
				recordResultsFor(experimentrunner, edsmScore.experimentName+"_A",ReferenceLearner.ScoringToApply.SCORING_EDSM,edsmScore.actualLearner);
				PairQualityLearner.SampleData siccoScore = result.samples.get(1);
				recordResultsFor(experimentrunner, siccoScore.experimentName+"_R",ReferenceLearner.ScoringToApply.SCORING_SICCO,siccoScore.referenceLearner);				
				recordResultsFor(experimentrunner, siccoScore.experimentName+"_C",ReferenceLearner.ScoringToApply.SCORING_SICCO,siccoScore.actualConstrainedLearner);				
				if (siccoScore.actualLearner != null) recordResultsFor(experimentrunner, siccoScore.experimentName+"_R",ReferenceLearner.ScoringToApply.SCORING_SICCO,siccoScore.actualLearner);
				BCR_vs_experiment.drawInteractive(gr);diff_vs_experiment.drawInteractive(gr);
			}
			
			@Override
			public String getSubExperimentName()
			{
				return "UAV experiments";
			}
			
			@SuppressWarnings("rawtypes")
			@Override
			public DrawGraphs.RGraph[] getGraphs() {
				return new DrawGraphs.RGraph[]{BCR_vs_experiment,diff_vs_experiment};
			}
		};

		
		int []rangeOfValues = new int[]{8,4,2};
		// all UAV, all data 
		Map<Integer,PTASequenceEngine> framesToTraces = null; 
		// compute the maximal depth for filtering.
		int depth = -1;
		{// load the data
			framesToTraces = paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds); 
			LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
			initialPTA.paths.augmentPTA(framesToTraces.get(paper.maxFrameNumber));
			for(CmpVertex vert:initialPTA.transitionMatrix.keySet())		
				depth = Math.max(depth, vert.getDepth());
			System.out.println("maximal depth: "+depth);
		}
		List<UASExperiment> listOfExperiments = new ArrayList<UASExperiment>();
		
		// Try ktails
		{
			for(int i=1;i<4;++i)
			{
				String graphName = outPathPrefix+"uas-All-ktails"+i;
				if (!new File(PaperUAS.fileName(graphName)).canRead())
				{
					Configuration config = paper.learnerInitConfiguration.config.copy();config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
					System.out.println(new Date()+" trying ktails "+i);
					/*
					final PTASequenceEngine samples = framesToTraces.get(paper.maxFrameNumber);
					PTASequenceEngine.FilterPredicate posPredicate = samples.getFSM_filterPredicate();
					PTASequenceEngine.FilterPredicate negPredicate = new FilterPredicate() {
						FilterPredicate origFilter = samples.getFSM_filterPredicate();
						public @Override boolean shouldBeReturned(Object name) {
							return !origFilter.shouldBeReturned(name);
						}
					};
					LearnerGraph kTailsOutcome = LearningAlgorithms.incrementalKtails(samples.getData(posPredicate),samples.getData(negPredicate),i,config);
					*/
					LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
					initialPTA.paths.augmentPTA(framesToTraces.get(paper.maxFrameNumber));
					LearnerGraph kTailsOutcome = LearningAlgorithms.ptaConcurrentKtails(initialPTA, i);
					kTailsOutcome.storage.writeGraphML(PaperUAS.fileName(graphName));
					System.out.println(new Date()+" finished ktails "+i);
				}
			}
		}
		
		{// process all the traces from all UAVs and seeds in one go
			String graphName = outPathPrefix+"uas-All";
			if (!new File(PaperUAS.fileName(graphName)).canRead())
			{
				LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
				initialPTA.paths.augmentPTA(framesToTraces.get(paper.maxFrameNumber));
				initialPTA.storage.writeGraphML(PaperUAS.fileName(graphName));
			}
			UASExperiment experiment = new UASExperiment(paper.learnerInitConfiguration,referenceGraph,"All",graphName,true);
			//printLastFrame("All",framesToTraces.keySet());
			listOfExperiments.add(experiment);
		}    	
		
		// for each seed, all UAVs
		for(String seed:paper.collectionOfTraces.keySet())
     		if (seed != UAVAllSeeds)
     		{
     			String seedPadded = padString(seed, '_', 2);
     			String graphName = sprintf("%suas-%s-AU",outPathPrefix,seedPadded);
     			if (!new File(PaperUAS.fileName(graphName)).canRead())
     			{
     				LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
     				framesToTraces = paper.collectionOfTraces.get(seed).tracesForUAVandFrame.get(UAVAll);
     				augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,paper.maxFrameNumber);
     				for (Label l:initialPTA.pathroutines.computeAlphabet())
     					if (l.toString().equals("Data_Deprecates_Waypoint"))
     						System.out.println("Data_Deprecates_Waypoint is in seed "+seed);
     				initialPTA.storage.writeGraphML(PaperUAS.fileName(graphName));
     			}
     			UASExperiment experiment = new UASExperiment(paper.learnerInitConfiguration,referenceGraph,"AU",graphName,true);
     			//printLastFrame("AU_"+seed,framesToTraces.keySet());
     			listOfExperiments.add(experiment);

     			for(int fraction:rangeOfValues)
     			{
     				graphName = sprintf("%suas-%s-AU-%02d",outPathPrefix,seedPadded,fraction);
         			if (!new File(PaperUAS.fileName(graphName)).canRead())
         			{
         				LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
	         			augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,paper.maxFrameNumber);initialPTA.transform.trimGraph(depth/fraction, initialPTA.getInit());
	         			//augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,Math.round(paper.maxFrameNumber/fraction));
	         			initialPTA.storage.writeGraphML(PaperUAS.fileName(graphName));
         			}
         			experiment = new UASExperiment(paper.learnerInitConfiguration,referenceGraph,"AU"+fraction,graphName,true);
         			listOfExperiments.add(experiment);
     			}
     		}

		// for each seed, individual UAVs
    	for(String seed:paper.collectionOfTraces.keySet())
     		if (seed != UAVAllSeeds)
	     	{
     		  TracesForSeed tracesSeed = paper.collectionOfTraces.get(seed);
     			Set<String> UAVsInSeed = tracesSeed.tracesForUAVandFrame.keySet(); 
     			String seedPadded = padString(seed, '_', 2);
     			for(String uav:UAVsInSeed)
     				if (uav != UAVAll && uav != UAVAllSeeds)
	     			{
     					String uavPadded = padString(uav, '_', 2);
     					String graphName = sprintf("%suas-%s-%s-U",outPathPrefix,seedPadded,uavPadded);
     					if (!new File(PaperUAS.fileName(graphName)).canRead())
         			{
	         			LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
	         			framesToTraces = tracesSeed.tracesForUAVandFrame.get(uav);
	         			augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,paper.maxFrameNumber);
	       				for (Label l:initialPTA.pathroutines.computeAlphabet())
	       					if (l.toString().equals("Data_Deprecates_Waypoint"))
	       						System.out.println("Data_Deprecates_Waypoint is in uav " + uav +" and seed "+seed);
	         			initialPTA.storage.writeGraphML(PaperUAS.fileName(graphName));
         			}
         			UASExperiment experiment = new UASExperiment(paper.learnerInitConfiguration,referenceGraph,"U",graphName,true);
         			//printLastFrame("U_"+seed+"_"+uav,framesToTraces.keySet());
         			listOfExperiments.add(experiment);

         			for(int fraction:rangeOfValues)
         			{
         				graphName = sprintf("%suas-%s-%s-U-%02d",outPathPrefix,seedPadded,uavPadded,fraction);
         				if (!new File(PaperUAS.fileName(graphName)).canRead())
             		{
             			LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
             			augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,paper.maxFrameNumber);initialPTA.transform.trimGraph(depth/fraction, initialPTA.getInit());
             			//augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,Math.round(paper.maxFrameNumber/fraction));
             			initialPTA.storage.writeGraphML(PaperUAS.fileName(graphName));
	             	}
	             	experiment = new UASExperiment(paper.learnerInitConfiguration,referenceGraph,"U"+fraction,graphName,true);
	             	listOfExperiments.add(experiment);
	         		}
	     	         			
	     			}
 	     	}

    	System.out.println("completed constructing the source graphs");
    	paper = null;// throw the original traces away
    	System.gc();
    	for(UASExperiment e:listOfExperiments)
    		experimentRunner.submitTask(e);
    	experimentRunner.collectOutcomeOfExperiments(resultHandler);
		
    	/*
		//initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
		System.out.printf("positive leaf nodes: %d, transitions: %d\n",computeLeafCount(initialPTA)-(initialPTA.getStateNumber()-initialPTA.getAcceptStateNumber()),computeTransitionCount(initialPTA));
		System.out.println(new Date().toString()+" Graph loaded: "+initialPTA.getStateNumber()+" states, adding at most "+ paper.learnerInitConfiguration.config.getHowManyStatesToAddFromIFTHEN()+" if-then states");
		System.out.println(new Date().toString()+" if-then states added, now "+initialPTA.getStateNumber()+" states");
		*/
		if (BCR_vs_experiment != null) BCR_vs_experiment.drawPdf(gr);
		if (diff_vs_experiment != null) diff_vs_experiment.drawPdf(gr);
		
		DrawGraphs.end();// the process will not terminate without it because R has its own internal thread
		experimentRunner.successfulTermination();
	}
}
