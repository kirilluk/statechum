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
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Timer;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;

import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.ErlangOracleVisualiser;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;

import statechum.Configuration;
import statechum.Helper;
import statechum.Label;
import statechum.Pair;
import statechum.ProgressIndicator;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.observers.DummyLearner;
import statechum.analysis.learning.observers.LearnerDecorator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.apps.QSMTool;
import statechum.apps.QSMTool.TraceAdder;

public class PaperUAS 
{
	/** All traces. */
	Map<String,TracesForSeed> collectionOfTraces = new TreeMap<String,TracesForSeed>();
	
	/** The maximal frame number encountered. */
	int maxFrameNumber = -1;
	
	/** When processing graphs with numerous data points, the task is to cluster those points so as not to have too many of them
	 * on the graphs. The easiest way is to divide a frame number by the maximal number so as to get %%.
	 */
	int divisor=1;
	
	/** Data recorded for each seed. */
	public static class TracesForSeed
	{
	    Map<String,Map<Integer,Set<List<Label>>>> collectionOfPositiveTraces = new TreeMap<String,Map<Integer,Set<List<Label>>>>();
	    Map<String,Map<Integer,Set<List<Label>>>> collectionOfNegativeTraces = new TreeMap<String,Map<Integer,Set<List<Label>>>>();

	    
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
    //private static final int lexRealTimestamp=2;
    private static final int lexUAV=2;
    private static final int lexSeed=3;
    private static final int lexTrace=4;
    
    private final Lexer lexer = new Lexer("(\\w+)\\s*,\\s*(\\w+)\\s*,\\s*(\\w+)\\s*,\\s*(.*)");

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
			public void process(final int frame, final String UAV, final String seed) {
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
    
    public void loadData(final Reader inputData,final Configuration config)
    {
    	loadData(new Reader[]{inputData},config);
    }
    
    /** Loads traces from the file into the pair of positive/negative maps,
     * parameterised by UAV and timeframe.
     *  
     * @param inputData data to load
     * @param config configuration to use
     */
    public void loadData(final Reader []inputData,final Configuration config)
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
             	
            	QSMTool.parseSequenceOfTraces(lexer.group(lexTrace),config, new TraceAdder() {

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
            		
            	});
			}
		});
		
		
       	collectionOfTraces.clear();
    	for(Entry<String,TracesForSeed> entry:data.entrySet())
    	{
    		TracesForSeed traceDetails = entry.getValue(), newData = new TracesForSeed();
    		collectionOfTraces.put(entry.getKey(), newData);
    		newData.collectionOfPositiveTraces=accumulateTraces(traceDetails.collectionOfPositiveTraces,frameNumbers,UAVs);
    		newData.collectionOfNegativeTraces=accumulateTraces(traceDetails.collectionOfNegativeTraces,frameNumbers,UAVs);
    	}
      	

   }

   protected static void AddLastPositiveTrace(String seed, String UAV, Map<String,TracesForSeed> data)
   {
   		TracesForSeed dataForSeed = data.get(seed);
   		Integer lastFrameNumberInteger = dataForSeed.maxFrameNumber.get(UAV);
   		if (lastFrameNumberInteger == null && dataForSeed.collectionOfNegativeTraces.containsKey(UAV))
   			throw new IllegalArgumentException("no max frame number for UAV "+UAV+" when seed is "+seed);
   		if (lastFrameNumberInteger != null)
   		{
			int lastFrameNumber = lastFrameNumberInteger.intValue();
			final List<Label> lastPositiveTrace = new LinkedList<Label>();lastPositiveTrace.addAll(dataForSeed.lastPointOnTrace.get(UAV));// make a copy because the positive trace constantly gets added to
			addTraceToUAV(UAVAllSeeds,lastFrameNumber,lastPositiveTrace,data.get(UAVAllSeeds).collectionOfPositiveTraces);
			addTraceToUAV(UAVAll,lastFrameNumber,lastPositiveTrace,dataForSeed.collectionOfPositiveTraces);
			addTraceToUAV(UAV,lastFrameNumber,lastPositiveTrace,dataForSeed.collectionOfPositiveTraces);
   		}
   }
    
    /** Loads traces from the file into the pair of positive/negative maps,
     * parameterised by UAV and timeframe. The process is to assume a specific starting point and concatenate the following 
     * negative traces with it, until we meet a positive trace. After concatenation of that positive trace, we have a revised starting point.
     * Every time a new timeframe is encountered, record the old starting point trace and start appending new traces to it. 
     *  
     * @param inputData data to load
     * @param config configuration to use
     */
    public void loadDataByConcatenation(final Reader []inputData,final Configuration config)
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
             		dataForSeed.maxFrameNumber.put(UAV,-1);dataForSeed.lastPointOnTrace.put(UAV, new LinkedList<Label>());
             		
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
             	
            	QSMTool.parseSequenceOfTraces(lexer.group(lexTrace),config, new TraceAdder() {

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
    						List<Label> negativeTrace = new LinkedList<Label>();negativeTrace.addAll(lastPositiveTrace);negativeTrace.addAll(trace);
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

        // This is the same process as done for normal traces, it is proper to keep it unchanged because PTA construction will silently "swallow" positive prefixes
        // and these are the only new ones here.
       	collectionOfTraces.clear();
    	for(Entry<String,TracesForSeed> entry:data.entrySet())
    	{
    		TracesForSeed traceDetails = entry.getValue(), newData = new TracesForSeed();
    		collectionOfTraces.put(entry.getKey(), newData);
    		newData.collectionOfPositiveTraces=accumulateTraces(traceDetails.collectionOfPositiveTraces,frameNumbers,UAVs);
    		newData.collectionOfNegativeTraces=accumulateTraces(traceDetails.collectionOfNegativeTraces,frameNumbers,UAVs);
    	
    		if (!entry.getKey().equals(UAVAllSeeds))
    			for(String UAV:UAVs)
    				if (!UAV.equals(UAVAll) && !UAV.equals(UAVAllSeeds) && traceDetails.lastPointOnTrace.containsKey(UAV)) 
    					System.out.println("Seed: "+entry.getKey()+", UAV: "+UAV+" length: "+traceDetails.lastPointOnTrace.get(UAV).size());
    					
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
       
        try {
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
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {// ignored.
                }
            }
        }

    }
    
    /** UAV(frame) should be an a sum of data across all frames before the specific frame.
     * Such a calculation is done after we load all data because frames from different UAVs might
     * not be synchronized and we need to accumulate data across all of them for UAVAll.
     *   
     * @param traces map to operate on.
     * @param frameNumbers all possible frames, to ensure that all UAV frame numbers range over the same set.
     * @param UAVs names of all UAVs, used to ensure that all UAV frame numbers range over the same set.
     * @return the accumulated results
     */
    protected Map<String,Map<Integer,Set<List<Label>>>> accumulateTraces(
    		Map<String,Map<Integer,Set<List<Label>>>> traces, Set<Integer> frameNumbers,Set<String> UAVs)
	{
    	Map<String,Map<Integer,Set<List<Label>>>> outcome = new TreeMap<String,Map<Integer,Set<List<Label>>>>();
    	
        for(String uav:UAVs)
        {
        	
        	Map<Integer,Set<List<Label>>> outcomeUAV = new TreeMap<Integer,Set<List<Label>>>();
        	outcome.put(uav, outcomeUAV);
        	Set<List<Label>> accumulatedTraces = new HashSet<List<Label>>();
        	for(Integer frame:frameNumbers)
        	{
           		Set<List<Label>> traceDetailsUAV = outcomeUAV.get(frame);
        		if (traceDetailsUAV == null)
        		{
        			traceDetailsUAV = new HashSet<List<Label>>();outcomeUAV.put(frame, traceDetailsUAV);
        		}
        		
        		Set<List<Label>> newTrace =
        			traces.containsKey(uav)? traces.get(uav).get(frame) : null;
        		if (newTrace != null)
        			accumulatedTraces.addAll(newTrace);
        		traceDetailsUAV.addAll(accumulatedTraces);
        	}
        }
        
        return outcome;
	}
    
    final LearnerEvaluationConfiguration learnerInitConfiguration = new LearnerEvaluationConfiguration(null);
    
    public void runExperiment(final Configuration config) throws IOException
    {
        learnerInitConfiguration.config = config;
        final int threshold = 0;
        long tmStarted = new Date().getTime();
        final LearnerGraph graphReference = new RPNIBlueFringe(threshold,config,"Learning reference graph").learn(UAVAllSeeds, UAVAllSeeds,maxFrameNumber,true);
        long tmFinished = new Date().getTime();
        System.out.println("Learning reference complete, "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
        graphReference.storage.writeGraphML("longtraceautomaton.xml");
		final Collection<List<Label>> wMethod = graphReference.wmethod.getFullTestSet(1);
		tmFinished = new Date().getTime();
        System.out.println("Test generation complete, "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
		
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

		Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).collectionOfPositiveTraces.get(UAVAllSeeds).keySet();

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
							ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphReference,
									new RPNIBlueFringe(threshold,config,null).learn(UAVAllSeeds, UAVAllSeeds, frame,true));
							uas_outcome.add(new Pair<Integer,String>(frame,"S"),matrix.BCR());
							uas_S.add(frame,matrix.BCR());
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
								ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphReference,
										new RPNIBlueFringe(threshold,config,null).learn(UAVAll, seed, frame,true));
								uas_outcome.add(new Pair<Integer,String>(frame,"A"),matrix.BCR());
								uas_A.add(frame,matrix.BCR());
							}
							
						};
						if (threadNumber > 1) outcomes.add(executorService.submit(interactiveRunner));else interactiveRunner.run();
					}
				for(final String UAV:collectionOfTraces.get(UAVAllSeeds).collectionOfPositiveTraces.keySet())
					if (!UAV.equals(UAVAllSeeds) && !UAV.equals(UAVAll))
						for(final String seed:collectionOfTraces.keySet())
							if (!seed.equals(UAVAllSeeds))
							{
								Runnable interactiveRunner = new Runnable() {

									@Override
									public void run() {
										ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphReference,
												new RPNIBlueFringe(threshold,config,null).learn(UAV, seed, frame,true));
										uas_outcome.add(new Pair<Integer,String>(frame,"U"),matrix.BCR());
										uas_U.add(frame,matrix.BCR());
									}
									
								};
								if (threadNumber > 1) outcomes.add(executorService.submit(interactiveRunner));else interactiveRunner.run();
							}
				
				if (threadNumber <= 1)
				{
					uas_outcome.drawInteractive(gr);
					uas_A.drawInteractive(gr);
					uas_S.drawInteractive(gr);
					uas_U.drawInteractive(gr);
					progress.next();
				}
			}
			
			for(int i=0;i<5;++i)
			{
				final int arg=i;
				Runnable interactiveRunner = new Runnable() {

					@Override
					public void run() {
						ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphReference,new RPNIBlueFringe(arg,config,null).learn(UAVAllSeeds, UAVAllSeeds,maxFrameNumber,false));
						uas_threshold.add(arg, matrix.BCR());
					}
					
				};
				if (threadNumber > 1) 
					outcomes.add(executorService.submit(interactiveRunner));
				else 
				{ 
					interactiveRunner.run();
					uas_threshold.drawInteractive(gr);
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
        //Visualiser.updateFrame(graphReference, otherGraph);Visualiser.waitForKey();
    }
    
    public class RPNIBlueFringe
    {
    	private final int confidenceThreshold;
    	private Learner learner;
    	
		public RPNIBlueFringe(int threshold, Configuration conf,final String showProgress) 
		{
			confidenceThreshold = threshold;
			Configuration config = conf.copy();
			config.setGeneralisationThreshold(confidenceThreshold);config.setAskQuestions(false); 
			LearnerEvaluationConfiguration evalConf = new LearnerEvaluationConfiguration(config);
			learner = new RPNIUniversalLearner(null, evalConf);
			if (showProgress != null)
				learner = new DummyLearner(learner) {
					ProgressIndicator indicator;
					int stateNumber = 0;
					@Override 
					public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) {
						int newStateNumber = graph.getStateNumber();
						for(int i=stateNumber;i>=newStateNumber;--i) indicator.next();stateNumber = newStateNumber;
						return decoratedLearner.ChooseStatePairs(graph);
					}
				
					@Override 
					public LearnerGraph init(Collection<List<Label>> plus,	Collection<List<Label>> minus) 
					{
						LearnerGraph graph = decoratedLearner.init(plus, minus);
						stateNumber = graph.getStateNumber();
						System.out.println("initial number of states: "+stateNumber);
						indicator = new ProgressIndicator(showProgress, stateNumber);
						return graph;
					}
				};
	
		}

		public LearnerGraph learn(String UAV, String seed,int frameNumber, boolean useNegatives)
		{
			return  learner.learnMachine(collectionOfTraces.get(seed).collectionOfPositiveTraces.get(UAV).get(frameNumber),
					useNegatives?collectionOfTraces.get(seed).collectionOfNegativeTraces.get(UAV).get(frameNumber)
							: new HashSet<List<Label>>()
			);

		}
		
    }
    
    void checkTraces(Configuration config) throws IOException
    {
    	LearnerGraph correctAnswer = new LearnerGraph(config);
    	AbstractPersistence.loadGraph("shorttraceautomaton.xml", correctAnswer);
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
    
    /**
	 * @param args trace file to load.
     * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
		PaperUAS paper = new PaperUAS();
    	Configuration config = Configuration.getDefaultConfiguration().copy();config.setDebugMode(false);
    	{
	    	Reader []inputFiles = new Reader[args.length];for(int i=0;i<args.length;++i) inputFiles[i]=new FileReader(args[i]); 
	    	int maxFrame = paper.getMaxFrame(inputFiles);
	    	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
    	}
    	
    	{
	    	Reader []inputFiles = new Reader[args.length];for(int i=0;i<args.length;++i) inputFiles[i]=new FileReader(args[i]); 
	    	paper.loadDataByConcatenation(inputFiles, config);
	    	paper.checkTraces(config);
	    	//paper.loadData(inputFiles, config);
	    	//paper.runExperiment(config);
    	}
	}

}
