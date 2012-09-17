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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;

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
    
    public void runExperiment(Configuration config)
    {
        learnerInitConfiguration.config = config;
        final int threshold = 0;
        LearnerGraph graphReference = new RPNIBlueFringe(threshold,config).learn(UAVAllSeeds, UAVAllSeeds,maxFrameNumber,true);
		Collection<List<Label>> wMethod = graphReference.wmethod.getFullTestSet(1);
		
		LearnerGraph otherGraph = new RPNIBlueFringe(threshold,config).learn(UAVAllSeeds, UAVAllSeeds,4,true);
		ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphReference,otherGraph);
		System.out.println(matrix.BCR()+" "+matrix.fMeasure());
		
        /*
        for(int i=1;i<5;++i)
        {
        LearnerGraph graphA = new RPNIBlueFringe(threshold,config).learn(UAVAllSeeds, UAVAllSeeds,i,true);
 		Collection<List<Label>> wMethod = graphA.wmethod.getFullTestSet(1);
 		
 		LearnerGraph graphB = new RPNIBlueFringe(threshold,config).learn(UAVAllSeeds, UAVAllSeeds,i-1,true);
 		ConfusionMatrix matrix = DiffExperiments.classify(wMethod, graphA,graphB);
 		System.out.println(matrix.BCR()+" "+matrix.fMeasure());
        }*/
		//Visualiser.updateFrame(new RPNIBlueFringe(2,config).learn(UAVAll, 0,false), null);Visualiser.waitForKey();
		
		
		DrawGraphs gr = new DrawGraphs();

		RBoxPlot<Pair<Integer,String>> 
			uas_outcome = new RBoxPlot<Pair<Integer,String>>("Time","BCR",new File("time_bcr.pdf"));
		RBoxPlot<Integer>
					uas_A=new RBoxPlot<Integer>("Time","BCR",new File("time_A_bcr.pdf")),
							uas_S=new RBoxPlot<Integer>("Time","BCR",new File("time_S_bcr.pdf")),
									uas_U=new RBoxPlot<Integer>("Time","BCR",new File("time_U_bcr.pdf"))
			;
		Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).collectionOfPositiveTraces.get(UAVAllSeeds).keySet();
		ProgressIndicator progress = new ProgressIndicator("UAS", allFrames.size());
		for(Integer frame:allFrames)
		{
			matrix = DiffExperiments.classify(wMethod, graphReference,
					new RPNIBlueFringe(threshold,config).learn(UAVAllSeeds, UAVAllSeeds, frame,true));
			uas_outcome.add(new Pair<Integer,String>(frame,"S"),matrix.BCR());
			uas_S.add(frame,matrix.BCR());
			for(String seed:collectionOfTraces.keySet())
				if (!seed.equals(UAVAllSeeds))
				{
					matrix = DiffExperiments.classify(wMethod, graphReference,
							new RPNIBlueFringe(threshold,config).learn(UAVAll, seed, frame,true));
					uas_outcome.add(new Pair<Integer,String>(frame,"A"),matrix.BCR());
					uas_A.add(frame,matrix.BCR());
				}
			for(String UAV:collectionOfTraces.get(UAVAllSeeds).collectionOfPositiveTraces.keySet())
				if (!UAV.equals(UAVAllSeeds) && !UAV.equals(UAVAll))
					for(String seed:collectionOfTraces.keySet())
						if (!seed.equals(UAVAllSeeds))
						{
							matrix = DiffExperiments.classify(wMethod, graphReference,
									new RPNIBlueFringe(threshold,config).learn(UAV, seed, frame,true));
							uas_outcome.add(new Pair<Integer,String>(frame,"U"),matrix.BCR());
							uas_U.add(frame,matrix.BCR());

						}
			
			uas_outcome.drawInteractive(gr);
			uas_A.drawInteractive(gr);
			uas_S.drawInteractive(gr);
			uas_U.drawInteractive(gr);
			progress.next();
		}
		uas_outcome.drawPdf(gr);uas_A.drawPdf(gr);uas_S.drawPdf(gr);uas_U.drawPdf(gr);
		RBoxPlot<Integer>
			uas_threshold=new RBoxPlot<Integer>("Threshold","BCR",new File("threshold_bcr.pdf"));
		for(int i=0;i<5;++i)
		{
			matrix = DiffExperiments.classify(wMethod, graphReference,new RPNIBlueFringe(i,config).learn(UAVAllSeeds, UAVAllSeeds,maxFrameNumber,false));
			uas_threshold.add(i, matrix.BCR());uas_threshold.drawInteractive(gr);
		}
		uas_threshold.drawPdf(gr);
        //Visualiser.updateFrame(graphReference, otherGraph);Visualiser.waitForKey();
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
    	{
	    	Reader []inputFiles = new Reader[args.length];for(int i=0;i<args.length;++i) inputFiles[i]=new FileReader(args[i]); 
	    	int maxFrame = paper.getMaxFrame(inputFiles);
	    	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
    	}
    	
    	{
	    	Reader []inputFiles = new Reader[args.length];for(int i=0;i<args.length;++i) inputFiles[i]=new FileReader(args[i]); 
	    	paper.loadData(inputFiles, config);
	    	paper.runExperiment(config);
    	}
	}

}
