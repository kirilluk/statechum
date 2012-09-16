package statechum.apps;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;

import statechum.Configuration;
import statechum.Label;
import statechum.Pair;
import statechum.ProgressIndicator;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.apps.QSMTool.TraceAdder;

public class PaperUAS 
{
	/** All traces. */
	Map<String,TracesForSeed> collectionOfTraces = new TreeMap<String,TracesForSeed>();
	
	/** The maximal frame number encountered. */
	int maxFrameNumber = -1;
	
	/** Data recorded for each seed. */
	public static class TracesForSeed
	{
	    Map<String,Map<Integer,Set<List<Label>>>> collectionOfPositiveTraces = new TreeMap<String,Map<Integer,Set<List<Label>>>>();
	    Map<String,Map<Integer,Set<List<Label>>>> collectionOfNegativeTraces = new TreeMap<String,Map<Integer,Set<List<Label>>>>();

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
    private static final int lexRealTimestamp=2;
    private static final int lexUAV=3;
    private static final int lexSeed=4;
    private static final int lexTrace=5;
    
    /** Loads traces from the file into the pair of positive/negative maps,
     * parameterised by UAV and timeframe.
     *  
     * @param inputData
     * @param config
     */
    public void loadData(Reader inputData,Configuration config)
    {
        BufferedReader in = null;
        Lexer lexer = new Lexer("(\\w+)\\s*,\\s*(\\w+)\\s*,\\s*(\\w+)\\s*,\\s*(\\w+)\\s*,\\s*(.*)");
        final Map<String,TracesForSeed> data = new TreeMap<String,TracesForSeed>();
       
        try {
            in = new BufferedReader(inputData);
            String fileString;
            
            Set<String> UAVs = new TreeSet<String>();UAVs.add(UAVAll);UAVs.add(UAVAllSeeds);
            Set<Integer> frameNumbers = new TreeSet<Integer>();
            
            while ((fileString = in.readLine()) != null) {
            	lexer.startParsing(fileString);
            	int match = lexer.getMatchType();
            	if (match != 1)
            		throw new IllegalArgumentException("invalid match");
            	final String timeSimulated=lexer.group(lexSimulatedTimestamp),
            		timeReal=lexer.group(lexRealTimestamp),
            		UAV=lexer.group(lexUAV),seed=lexer.group(lexSeed);
            	final int frameNumber = Integer.parseInt(timeSimulated);
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
            	if (frameNumber < dataForSeed.maxFrameNumber.get(UAV) || frameNumber > 1+dataForSeed.maxFrameNumber.get(UAV))
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

            	collectionOfTraces.clear();
            	for(Entry<String,TracesForSeed> entry:data.entrySet())
            	{
            		TracesForSeed traceDetails = entry.getValue(), newData = new TracesForSeed();
            		collectionOfTraces.put(entry.getKey(), newData);
            		newData.collectionOfPositiveTraces=accumulateTraces(traceDetails.collectionOfPositiveTraces,frameNumbers,UAVs);
            		newData.collectionOfNegativeTraces=accumulateTraces(traceDetails.collectionOfNegativeTraces,frameNumbers,UAVs);
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
    
    public void runExperiment(Configuration config)
    {
        learnerInitConfiguration.config = config;
        
        LearnerGraph graphReference = new RPNIBlueFringe(0,config).learn(UAVAllSeeds, UAVAllSeeds,maxFrameNumber,true);
		Collection<List<Label>> wMethod = graphReference.wmethod.getFullTestSet(1);
		
		LearnerGraph otherGraph = graphReference;
		ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphReference,otherGraph);
		System.out.println(matrix.BCR()+" "+matrix.fMeasure());
		
		//Visualiser.updateFrame(new RPNIBlueFringe(2,config).learn(UAVAll, 0,false), null);Visualiser.waitForKey();
		DrawGraphs gr = new DrawGraphs();

		RBoxPlot<Pair<String,Integer>> 
			uas_outcome = new RBoxPlot<Pair<String,Integer>>("Time","BCR",new File("time_bcr.pdf"));
		Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).collectionOfPositiveTraces.get(UAVAllSeeds).keySet();
		ProgressIndicator progress = new ProgressIndicator("UAS", allFrames.size());
		for(Integer frame:allFrames)
		{
			matrix = DiffExperiments.classify(wMethod, graphReference,
					new RPNIBlueFringe(frame,config).learn(UAVAllSeeds, UAVAllSeeds, frame,true));
			uas_outcome.add(new Pair<String,Integer>("AS",frame),matrix.BCR());

			for(String seed:collectionOfTraces.keySet())
				if (!seed.equals(UAVAllSeeds))
				{
					matrix = DiffExperiments.classify(wMethod, graphReference,
							new RPNIBlueFringe(frame,config).learn(UAVAll, seed, frame,true));
					uas_outcome.add(new Pair<String,Integer>("A",frame),matrix.BCR());
				}
			for(String UAV:collectionOfTraces.get(UAVAllSeeds).collectionOfPositiveTraces.keySet())
				if (!UAV.equals(UAVAllSeeds) && !UAV.equals(UAVAll))
					for(String seed:collectionOfTraces.keySet())
						if (!seed.equals(UAVAllSeeds))
						{
							matrix = DiffExperiments.classify(wMethod, graphReference,
									new RPNIBlueFringe(frame,config).learn(UAV, seed, frame,true));
							uas_outcome.add(new Pair<String,Integer>("U",frame),matrix.BCR());
						}
			
			uas_outcome.drawInteractive(gr);
			progress.next();
		}
		uas_outcome.drawPdf(gr);
		/*
		for(int i=0;i<10;++i)
		{
			matrix = DiffExperiments.classify(wMethod, graphReference,new RPNIBlueFringe(i,config).learn(UAVAllSeeds, UAVAllSeeds,maxFrameNumber,false));
			System.out.println(matrix.BCR()+" "+matrix.fMeasure());
		}
		*/
        //Visualiser.updateFrame(graphReference, null);Visualiser.waitForKey();
    }
    
    public class RPNIBlueFringe
    {
    	private final int confidenceThreshold;
    	private RPNIUniversalLearner learner;
    	
		public RPNIBlueFringe(int threshold, Configuration conf) 
		{
			confidenceThreshold = threshold;
			Configuration config = conf.copy();
			config.setGeneralisationThreshold(confidenceThreshold);config.setAskQuestions(false); 
			LearnerEvaluationConfiguration evalConf = new LearnerEvaluationConfiguration(config);
			learner = new RPNIUniversalLearner(null, evalConf);
		}

		public LearnerGraph learn(String UAV, String seed,int frameNumber, boolean useNegatives)
		{
			return  learner.learnMachine(collectionOfTraces.get(seed).collectionOfPositiveTraces.get(UAV).get(frameNumber),
					useNegatives?collectionOfTraces.get(seed).collectionOfNegativeTraces.get(UAV).get(frameNumber)
							: new HashSet<List<Label>>()
			);

		}
		
    }
    
    /**
	 * @param args trace file to load.
     * @throws FileNotFoundException 
	 */
	public static void main(String[] args) throws FileNotFoundException {
		PaperUAS paper = new PaperUAS();
    	Configuration config = Configuration.getDefaultConfiguration().copy();
		paper.loadData(new FileReader(args[0]), config);
		paper.runExperiment(config);
	}

}
