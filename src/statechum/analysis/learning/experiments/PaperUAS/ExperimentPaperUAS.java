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

package statechum.analysis.learning.experiments.PaperUAS;

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
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.PathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.Label;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.PaperUAS.ExperimentPaperUAS.TracesForSeed.Automaton;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.EvaluationOfLearners.EvaluationOfLearnersParameters.LearningType;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.apps.QSMTool;
import statechum.apps.QSMTool.TraceAdder;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

public class ExperimentPaperUAS 
{
	/** All traces, maps a seed to a collection of traces for the specific seed. */
	public Map<String,TracesForSeed> collectionOfTraces = new TreeMap<String,TracesForSeed>();
	
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

    		if (entry.getKey() == UAVAllSeeds)
    		{
    			assert !traceDetails.collectionOfPositiveTraces.containsKey(UAVAll);
    			assert !traceDetails.collectionOfNegativeTraces.containsKey(UAVAll);

    			assert traceDetails.collectionOfPositiveTraces.isEmpty() || traceDetails.collectionOfPositiveTraces.containsKey(UAVAllSeeds);
    			assert traceDetails.collectionOfNegativeTraces.isEmpty() || traceDetails.collectionOfNegativeTraces.containsKey(UAVAllSeeds);
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
    		if (!newData.tracesForUAVandFrame.isEmpty())
    		{
    			assert entry.getKey() != UAVAllSeeds;
    			assert newData.tracesForUAVandFrame.isEmpty() || newData.tracesForUAVandFrame.containsKey(UAVAll);
    			assert !newData.tracesForUAVandFrame.containsKey(UAVAllSeeds);
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
    
    public LearnerEvaluationConfiguration learnerInitConfiguration = UASExperiment.constructLearnerInitConfiguration();

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
   	   
   public void LearnReferenceAutomaton(LearningAlgorithms.ScoringToApply scoringToUse,Configuration.ScoreMode scoringForEDSM) throws Exception
   {
	   long tmStarted = new Date().getTime();
       LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber));
       final LearnerGraph graphReference = LearningAlgorithms.constructLearner(learnerInitConfiguration,initPTA,scoringToUse,scoringForEDSM).learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
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
   
   
   public static LearnerGraph makeMerge(ExperimentPaperUAS paper, String faileNameToWriteResultTo, String transitionNameToMerge, boolean buildAuxInfo) throws IOException
   {
	   LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
	   //String fileNameToLoad = faileNameToWriteResultTo+"-before_merging.xml";
	   //AbstractPersistence.loadGraph(fileNameToLoad, initialPTA, paper.learnerInitConfiguration.getLabelConverter());
	   initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
	   initialPTA.storage.writeGraphML(faileNameToWriteResultTo+"-before_merging.xml");
	   
		LearnerGraph outcome = UASExperiment.mergePTA(initialPTA,AbstractLearnerGraph.generateNewLabel(transitionNameToMerge,initialPTA.config,paper.learnerInitConfiguration.getLabelConverter()),buildAuxInfo);
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
 		final int transitionCoverSize = referenceGraph.getAcceptStateNumber()*referenceGraph.pathroutines.computeAlphabet().size();
 		System.out.println(description+computeLeafCount(pta)/(double)transitionCoverSize+" leaves, "+computeTransitionCount(pta)/(double)transitionCoverSize+" transitions");
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
 	public static ExperimentPaperUAS loadTraces(String args[], boolean concatenateTraces) throws FileNotFoundException
 	{
		ExperimentPaperUAS paper = new ExperimentPaperUAS();
        
        String path = args[0];
        paper.loadReducedConfigurationFile(path+File.separator+args[1]);
        // the call above enable the use of constraints that should be disabled when we run an experiment rather than standalone QSMTool. Disable them here.
        paper.learnerInitConfiguration.config.setUseConstraints(false);
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
	
	public final static String directoryNamePrefix = "uaspaper_May_2016";
	public static final String directoryExperimentResult = "experimentresult"+File.separator;

 	public static class UASCaseStudy extends UASExperiment<PaperUASParameters,ExperimentResult<PaperUASParameters>>
 	{
 		private final String inputGraphFileName;
 		
 		protected LearnerGraph buildPTAWithAllNegatives() throws AugmentFromIfThenAutomatonException, IOException
 		{
 		    LearnerGraph pta = new LearnerGraph(learnerInitConfiguration.config);AbstractPersistence.loadGraph(ExperimentPaperUAS.fileName(inputGraphFileName), pta, learnerInitConfiguration.getLabelConverter());
 		    
 			//pta.storage.writeGraphML("resources/"+experimentName+"-initial.xml");
 		    if (par.useIfThen)
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
 		
 		public UASCaseStudy(PaperUASParameters parameters, LearnerGraph reference, String graphFileName,LearnerEvaluationConfiguration eval) 
 		{
			super(parameters,new LearnerEvaluationConfiguration(eval),directoryNamePrefix);
			referenceGraph = reference;
			inputGraphFileName = graphFileName;
		}

		@Override
 		public ExperimentResult<PaperUASParameters> call() throws Exception 
 		{
			ExperimentResult<PaperUASParameters> outcome = new ExperimentResult<PaperUASParameters>(par);
 			PairQualityLearner.SampleData sample = new PairQualityLearner.SampleData();
 			UASExperiment.BuildPTAInterface ptaWithNegatives = new BuildPTAInterface() {
 				@Override
				public String kindOfPTA()
 				{
 					return "posneg";
 				}
				@Override
				public LearnerGraph buildPTA() throws AugmentFromIfThenAutomatonException, IOException {
					return buildPTAWithAllNegatives();
				}
			};
 			
 			UASExperiment.BuildPTAInterface ptaWithoutNegatives = new BuildPTAInterface() {
 				@Override
				public String kindOfPTA()
 				{
 					return "pos";
 				}
				
				@Override
				public LearnerGraph buildPTA() throws AugmentFromIfThenAutomatonException, IOException {
					return LearningSupportRoutines.removeAllNegatives(buildPTAWithAllNegatives());
				}
			};

 			Label uniqueLabel = AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter());
			switch(par.learningType)
			{
			case CONVENTIONAL:
				sample.actualLearner = runExperimentUsingConventional(par.onlyUsePositives?ptaWithoutNegatives:ptaWithNegatives,par,par.scoringMethod,par.scoringForEDSM);
				break;
			case PREMERGE:
				sample.actualLearner = runExperimentUsingPremerge(par.onlyUsePositives?ptaWithoutNegatives:ptaWithNegatives,par,par.scoringMethod,par.scoringForEDSM,uniqueLabel);
				break;
			case CONSTRAINTS:
				sample.actualLearner = runExperimentUsingConstraints(par.onlyUsePositives?ptaWithoutNegatives:ptaWithNegatives,par,par.scoringMethod,par.scoringForEDSM,uniqueLabel);
				break;
			default:
				throw new IllegalArgumentException("invalid learning type "+par.learningType);
			}
			
			outcome.samples.add(sample);
 			return outcome;
 		}		
 	}
 	 	
 	// Arguments: first is a path to most files, followed by a configuration file parameters.txt and then all the seed files.
 	// Example: 
 	// C:\experiment\research\xmachine\ModelInferenceUAS\traces parameters.txt seed1_d.txt seed2_d.txt seed3_d.txt seed4_d.txt seed5_d.txt seed6_d.txt seed7_d.txt seed8_d.txt seed9_d.txt seed10_d.txt seed11_d.txt seed12_d.txt seed13_d.txt  seed14_d.txt seed15_d.txt seed16_d.txt seed17_d.txt seed18_d.txt seed19_d.txt
 	public static void main(String args[]) throws Exception
 	{
 		System.out.println("Started "+new Date());
		String outDir = "tmp"+File.separator+directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		UASExperiment.mkDir(outDir);
		String outPathPrefix = outDir + File.separator;

		String argsForLoading [] = {"/home/kirill/experiment/research/xmachine/ModelInferenceUAS/traces", 
				"parameters.txt", 
				"seed1_d.txt", "seed2_d.txt", "seed3_d.txt", "seed4_d.txt", "seed5_d.txt", "seed6_d.txt", "seed7_d.txt", "seed8_d.txt", "seed9_d.txt", "seed10_d.txt", 
				"seed11_d.txt", "seed12_d.txt", "seed13_d.txt", "seed14_d.txt", "seed15_d.txt", "seed16_d.txt", "seed17_d.txt", "seed18_d.txt", "seed19_d.txt"};
     	ExperimentPaperUAS paper = loadTraces(argsForLoading,true);
    	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
    	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);
    	paper.learnerInitConfiguration.testSet = LearningAlgorithms.buildEvaluationSet(referenceGraph);

 		RunSubExperiment<PaperUASParameters,ExperimentResult<PaperUASParameters>> experimentRunner = new RunSubExperiment<PaperUASParameters,ExperimentResult<PaperUASParameters>>(ExperimentRunner.getCpuNumber(),outPathPrefix + directoryExperimentResult,args);
		SGE_ExperimentRunner.configureCPUFreqNormalisation();

    	// Experiments:
    	// all UAV, all data (to show that even having all data does not help)
    	// for each seed, all UAVs (all frames), 
    	// for each seed, all UAVs for a subset of frames (to show that with a good start, we can learn from a subset of frames).
    	// for each seed, each UAV (all frames), 
    	// for each seed, each UAV for a subset of frames.
    	
    	// Variation on the experiment: 
    	// EDSM/Sicco scoring
    	// ifthen/ no ifthen (also no point)
    	// positive only or pos-neg (no point, except for ktails where we learn from positives only)
    	// EDSM/check constraints but merge EDSM-way/premerge on the transition of interest.
    	
		final RBoxPlot<String> BCR_vs_experiment = new RBoxPlot<String>("experiment","BCR",new File(outPathPrefix+"BCR_vs_experiment.pdf"));
		final RBoxPlot<String> diff_vs_experiment = new RBoxPlot<String>("experiment","Structural difference",new File(outPathPrefix+"diff_vs_experiment.pdf"));

		final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(outPathPrefix+"results.csv"));

    	processSubExperimentResult<PaperUASParameters,ExperimentResult<PaperUASParameters>> resultHandler = new processSubExperimentResult<PaperUASParameters,ExperimentResult<PaperUASParameters>>() {
			@Override
			public void processSubResult(ExperimentResult<PaperUASParameters> result, RunSubExperiment<PaperUASParameters,ExperimentResult<PaperUASParameters>> experimentrunner) throws IOException 
			{
				ScoresForGraph difference = result.samples.get(0).actualLearner;
				StringBuffer csvLine = new StringBuffer();
				csvLine.append(difference.differenceBCR.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.differenceStructural.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.nrOfstates.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(difference.executionTime/1000000000.));// execution time is in nanoseconds, we only need seconds.
				experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
				String experimentName = result.parameters.ptaName+"_"+result.parameters.learningType+result.parameters.scoringMethod.name;
						//EvaluationOfLearnersParameters.ptaMergersToString(result.parameters.ptaMergers)+"-"+result.parameters.matrixType.name;

				experimentrunner.RecordR(BCR_vs_experiment,experimentName,difference.differenceBCR.getValue(),null,null);
				experimentrunner.RecordR(diff_vs_experiment,experimentName,difference.differenceStructural.getValue(),null,null);
				
				//BCR_vs_experiment.drawInteractive(gr);diff_vs_experiment.drawInteractive(gr);
			}
						
			@Override
			public SGEExperimentResult[] getGraphs() {
				return new SGEExperimentResult[]{BCR_vs_experiment,diff_vs_experiment, resultCSV};
			}
		};

		
		int []rangeOfValues = new int[]{8,4,2};
		Configuration.ScoreMode scoringForEDSM = Configuration.ScoreMode.GENERAL_NOFULLMERGE;
		
		boolean mergePTA = false;
		Configuration.STATETREE matrix = Configuration.STATETREE.STATETREE_ARRAY;
		LearningType[] learningTypes = new LearningType[]{LearningType.CONVENTIONAL, LearningType.PREMERGE, LearningType.CONSTRAINTS};
		
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
		List<UASCaseStudy> listOfExperiments = new ArrayList<UASCaseStudy>();
				
		{// process all the traces from all UAVs and seeds in one go
			String graphName = outPathPrefix+"uas-All";
			if (!new File(ExperimentPaperUAS.fileName(graphName)).canRead())
			{
				LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
				initialPTA.paths.augmentPTA(framesToTraces.get(paper.maxFrameNumber));
				initialPTA.storage.writeGraphML(ExperimentPaperUAS.fileName(graphName));
			}
			
 			for(LearningAlgorithms.ScoringToApply scoringMethod:UASExperiment.listOfScoringMethodsToApplyThatDependOnEDSMScoring())
 				for(LearningType type:learningTypes)
	 			{
					PaperUASParameters parameters = new PaperUASParameters(scoringForEDSM, scoringMethod, type, mergePTA, matrix);
					parameters.setParameters(false, true, "", "All");
					UASCaseStudy experiment = new UASCaseStudy(parameters,referenceGraph,graphName,paper.learnerInitConfiguration);
					//printLastFrame("All",framesToTraces.keySet());
					listOfExperiments.add(experiment);
	 			}
		}    	
		
		// for each seed, all UAVs
		for(String seed:paper.collectionOfTraces.keySet())
     		if (seed != UAVAllSeeds)
     		{
     			String seedPadded = LearningSupportRoutines.padString(seed, '_', 2);
     			String graphName = sprintf("%suas-%s-AU",outPathPrefix,seedPadded);
     			if (!new File(ExperimentPaperUAS.fileName(graphName)).canRead())
     			{
     				LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
     				framesToTraces = paper.collectionOfTraces.get(seed).tracesForUAVandFrame.get(UAVAll);
     				augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,paper.maxFrameNumber);
     				for (Label l:initialPTA.pathroutines.computeAlphabet())
     					if (l.toString().equals("Data_Deprecates_Waypoint"))
     						System.out.println("Data_Deprecates_Waypoint is in seed "+seed);
     				initialPTA.storage.writeGraphML(ExperimentPaperUAS.fileName(graphName));
     			}
     			for(LearningAlgorithms.ScoringToApply scoringMethod:UASExperiment.listOfScoringMethodsToApplyThatDependOnEDSMScoring())
     				for(LearningType type:learningTypes)
    	 			{
    					PaperUASParameters parameters = new PaperUASParameters(scoringForEDSM, scoringMethod, type, mergePTA, matrix);
    					parameters.setParameters(false, true, seed, "AU");
    					UASCaseStudy experiment = new UASCaseStudy(parameters,referenceGraph,graphName,paper.learnerInitConfiguration);
    	     			//printLastFrame("AU_"+seed,framesToTraces.keySet());
    					listOfExperiments.add(experiment);
    	 			}

     			for(int fraction:rangeOfValues)
     			{
     				graphName = sprintf("%suas-%s-AU-%02d",outPathPrefix,seedPadded,fraction);
         			if (!new File(ExperimentPaperUAS.fileName(graphName)).canRead())
         			{
         				LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
	         			augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,paper.maxFrameNumber);initialPTA.transform.trimGraph(depth/fraction, initialPTA.getInit());
	         			//augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,Math.round(paper.maxFrameNumber/fraction));
	         			initialPTA.storage.writeGraphML(ExperimentPaperUAS.fileName(graphName));
         			}
         			for(LearningAlgorithms.ScoringToApply scoringMethod:UASExperiment.listOfScoringMethodsToApplyThatDependOnEDSMScoring())
         				for(LearningType type:learningTypes)
        	 			{
           					PaperUASParameters parameters = new PaperUASParameters(scoringForEDSM, scoringMethod, type, mergePTA, matrix);
        					parameters.setParameters(false, true, seed, "AU"+fraction);
        					UASCaseStudy experiment = new UASCaseStudy(parameters,referenceGraph,graphName,paper.learnerInitConfiguration);
        	     			//printLastFrame("AU_"+seed,framesToTraces.keySet());
        					listOfExperiments.add(experiment);
        	 			}
     			}
     		}

		// for each seed, individual UAVs
    	for(String seed:paper.collectionOfTraces.keySet())
     		if (seed != UAVAllSeeds)
	     	{
     		  TracesForSeed tracesSeed = paper.collectionOfTraces.get(seed);
     			Set<String> UAVsInSeed = tracesSeed.tracesForUAVandFrame.keySet(); 
     			String seedPadded = LearningSupportRoutines.padString(seed, '_', 2);
     			for(String uav:UAVsInSeed)
     				if (uav != UAVAll && uav != UAVAllSeeds)
	     			{
     					String uavPadded = LearningSupportRoutines.padString(uav, '_', 2);
     					String graphName = sprintf("%suas-%s-%s-U",outPathPrefix,seedPadded,uavPadded);
     					if (!new File(ExperimentPaperUAS.fileName(graphName)).canRead())
	         			{
		         			LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
		         			framesToTraces = tracesSeed.tracesForUAVandFrame.get(uav);
		         			augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,paper.maxFrameNumber);
		       				for (Label l:initialPTA.pathroutines.computeAlphabet())
		       					if (l.toString().equals("Data_Deprecates_Waypoint"))
		       						System.out.println("Data_Deprecates_Waypoint is in uav " + uav +" and seed "+seed);
		         			initialPTA.storage.writeGraphML(ExperimentPaperUAS.fileName(graphName));
	         			}
            			for(LearningAlgorithms.ScoringToApply scoringMethod:UASExperiment.listOfScoringMethodsToApplyThatDependOnEDSMScoring())
             				for(LearningType type:learningTypes)
            	 			{
               					PaperUASParameters parameters = new PaperUASParameters(scoringForEDSM, scoringMethod, type, mergePTA, matrix);
            					parameters.setParameters(false, true, seed+"-"+uav, "U");
            					UASCaseStudy experiment = new UASCaseStudy(parameters,referenceGraph,graphName,paper.learnerInitConfiguration);
        	         			//printLastFrame("U_"+seed+"_"+uav,framesToTraces.keySet());
            					listOfExperiments.add(experiment);
            	 			}

	         			for(int fraction:rangeOfValues)
	         			{
	         				graphName = sprintf("%suas-%s-%s-U-%02d",outPathPrefix,seedPadded,uavPadded,fraction);
	         				if (!new File(ExperimentPaperUAS.fileName(graphName)).canRead())
		             		{
		             			LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
		             			augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,paper.maxFrameNumber);initialPTA.transform.trimGraph(depth/fraction, initialPTA.getInit());
		             			//augmentPTAWithTracesForFrameRange(initialPTA,framesToTraces,Math.round(paper.maxFrameNumber/fraction));
		             			initialPTA.storage.writeGraphML(ExperimentPaperUAS.fileName(graphName));
			             	}

	            			for(LearningAlgorithms.ScoringToApply scoringMethod:UASExperiment.listOfScoringMethodsToApplyThatDependOnEDSMScoring())
		         				for(LearningType type:learningTypes)
	            	 			{
	               					PaperUASParameters parameters = new PaperUASParameters(scoringForEDSM, scoringMethod, type, mergePTA, matrix);
	            					parameters.setParameters(false, true, seed+"-"+uav, "U"+fraction);
	            					UASCaseStudy experiment = new UASCaseStudy(parameters,referenceGraph,graphName,paper.learnerInitConfiguration);
	        	         			//printLastFrame("U_"+seed+"_"+uav,framesToTraces.keySet());
	            					listOfExperiments.add(experiment);
	            	 			}
		         		}
	     			}
 	     	}

    	System.out.println("completed constructing the source graphs "+new Date());
    	paper = null;// throw the original traces away
    	System.gc();
    	try
    	{
	    	for(UASCaseStudy e:listOfExperiments)
	    		experimentRunner.submitTask(e);
    		experimentRunner.collectOutcomeOfExperiments(resultHandler);
		}
		finally
		{
			experimentRunner.successfulTermination();
			DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		}
		
    	/*
		//initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
		System.out.printf("positive leaf nodes: %d, transitions: %d\n",computeLeafCount(initialPTA)-(initialPTA.getStateNumber()-initialPTA.getAcceptStateNumber()),computeTransitionCount(initialPTA));
		System.out.println(new Date().toString()+" Graph loaded: "+initialPTA.getStateNumber()+" states, adding at most "+ paper.learnerInitConfiguration.config.getHowManyStatesToAddFromIFTHEN()+" if-then states");
		System.out.println(new Date().toString()+" if-then states added, now "+initialPTA.getStateNumber()+" states");
		*/
    	/*
    	if (resultCSV != null) resultCSV.reportResults(gr);
		if (BCR_vs_experiment != null) BCR_vs_experiment.reportResults(gr);
		if (diff_vs_experiment != null) diff_vs_experiment.reportResults(gr);
		*/
	}
}
