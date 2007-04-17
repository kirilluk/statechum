/*
 * INCOMPLETE
 */

package statechum.analysis.learning;


import java.awt.Point;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import javax.swing.SwingUtilities;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.io.GraphMLFile;
import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.*;
import static statechum.xmachine.model.testset.WMethod.getGraphData;
import static statechum.xmachine.model.testset.WMethod.tracePath;

public class AccuracyAndQuestionsExperiment {

	private final ExecutorService executorService;
	
	public AccuracyAndQuestionsExperiment(String outputD)
	{
		int ThreadNumber = 1; // the default for single-cpu systems.
		String cpuNum = System.getProperty("threadnum");
		if (cpuNum != null)
		{
			int parsedNumber = -1;
			try  { parsedNumber = Integer.parseInt(cpuNum); }
			catch(NumberFormatException ex) { parsedNumber = -1; }
			if (parsedNumber > 0 && parsedNumber < 31)
				ThreadNumber = parsedNumber;
		}

		results = new LinkedList<Future<String>>();executorService = Executors.newFixedThreadPool(ThreadNumber);
		runner = new ExecutorCompletionService<String>(executorService);
		outputDir = outputD;
	}
	
	public abstract static class LearnerEvaluatorGenerator 
	{
		abstract LearnerEvaluator getLearnerEvaluator(String inputFile, String ouputDir, int percent);
	}
	
	public abstract static class LearnerEvaluator implements Callable<String>
	{
		protected Collection<List<String>> sPlus=null, sMinus=null;
		protected DirectedSparseGraph graph=null;
		protected String inputFileName = null, outputDir = null;
		protected Collection<List<String>> tests = null;
		protected int percent;
		
		public LearnerEvaluator(String inputFile, String outputD, int per) 
		{
			inputFileName = inputFile;outputDir = outputD;percent = per;			
		}

		protected void buildSets()
		{
	    	GraphMLFile graphmlFile = new GraphMLFile();
	    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
	    	graph = new DirectedSparseGraph();
	    	graph.getEdgeConstraints().clear();
	    	graph = (DirectedSparseGraph)graphmlFile.load(inputFileName);

	    	RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(percent+100));// the seed for Random should be the same for each file
			WMethod tester = new WMethod(graph,0);
			Collection<List<String>> fullTestSet = (Collection<List<String>>)tester.getFullTestSet();
			tests = randomHalf(fullTestSet,new Random(0));
			Collection<List<String>> fullSet = rpg.getAllPaths();
			//l.setCertaintyThreshold(2);
			Collection<List<String>> sampleSet = randomHalf(fullSet,new Random(1));
			Vector<List<String>> samples = new Vector<List<String>>();
			samples.addAll(sampleSet);
			tests.removeAll(sampleSet);
			Set<List<String>> currentSamples = new LinkedHashSet<List<String>>();
			
			currentSamples = addPercentageFromSamples(currentSamples, samples, percent);
			sPlus = getPositiveStrings(graph,currentSamples);
			sMinus = currentSamples;
			sMinus.removeAll(sPlus);
			sMinus = trimToNegatives(graph, sMinus);
		}

		public enum FileType { 
			DATA {String getFileName(String prefix, String suffix) { return prefix+"_data"+suffix+".xml"; } }, 
			RESULT {String getFileName(String prefix, String suffix) { return prefix+"_result"+suffix+".txt"; } };
			
			abstract String getFileName(String prefix, String suffix);
		};
		
		protected String getFileName(FileType fileNameType)
		{
			return fileNameType.getFileName(outputDir+System.getProperty("file.separator")+(new File(inputFileName).getName()),"-"+percent); 
		}
	}
	
	/** This one is not static because it refers to the frame to display results. */
	public static class RPNIEvaluator extends LearnerEvaluator
	{
		public RPNIEvaluator(String inputFile, String outputDir, int per)
		{
			super(inputFile, outputDir, per);			
		}

		/** This one may be overridden by subclass to customise the learner. */
		protected void changeParametersOnLearner(RPNIBlueFringeLearner l)
		{
			
		}
		
		public String call()
		{
			System.out.println(inputFileName+" "+percent + "% started");
			buildSets();
			
			final FSMStructure fsm = WMethod.getGraphData(graph);
			RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null)
			{
				protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
				{
					return tracePath(fsm, question);
				}
			};
			changeParametersOnLearner(l);
			DirectedSparseGraph learningOutcome = null;
			String result = "" + percent+"%,";
			String stats = "";
			String stdOutput = null;
			try
			{
				learningOutcome = l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);
				result = result+l.getQuestionCounter()+", "+computeAccuracy(learningOutcome, graph,tests);							
				//updateFrame(g,learningOutcome);
				l.setQuestionCounter(0);
				if (learningOutcome != null)
					stats = learningOutcome.containsUserDatumKey("STATS")? "\n"+learningOutcome.getUserDatum("STATS").toString():"";
				System.out.println(inputFileName+" "+percent +"% terminated");
				
				// now record the result
				Writer outputWriter = new BufferedWriter(new FileWriter(getFileName(FileType.RESULT)));
				outputWriter.write(result+"\nSTATS: "+stats);outputWriter.close();
				
				stdOutput = inputFileName+" "+result+"\nSTATS: "+stats;
			}
			catch(Throwable th)
			{
				StringWriter writer = new StringWriter();
				th.printStackTrace();
				th.printStackTrace(new PrintWriter(writer));
				stdOutput = result+"FAILED\nSTACK: "+writer.toString();
			}
			return stdOutput;
		}
	}
	
	/** Stores results of execution of evaluators. */
	final List<Future<String>> results;
	
	/** Stores tasks to complete. */
	final CompletionService<String> runner;
			
	/** Displays twos graphs passed as arguments in the Jung window.
	 * @param g the graph to display 
	 * @param lowerGraph the graph to display below it
	 */
	public static void updateFrame(final DirectedSparseGraph g,final DirectedSparseGraph lowerGraph)
	{
		final Visualiser v=new Visualiser();
		v.update(null, g);
		if (lowerGraph != null)
		{
			try {// I'm assuming here that Swing has only one queue of threads to run on the AWT thread, hence the
				// thread scheduled by invokeLater will be run to completion before the next one (below) runs and hence
				// I rely on the results of execution of the above thread below in order to position the window.
				SwingUtilities.invokeAndWait(new Runnable() 
				{
					public void run()
					{
						Visualiser viz=new Visualiser();viz.update(null, lowerGraph);
						Point newLoc = viz.getLocation();newLoc.move(0, v.getHeight());v.setLocation(newLoc);
					}
				});
			} catch (InterruptedException e) {
				// cannot do much about this
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// cannot do much about this
				e.printStackTrace();
			}
		}
	}
	
	private static double computeAccuracy(DirectedSparseGraph learned, DirectedSparseGraph correct, Collection<List<String>> tests){
		int failed = 0;
		for (List<String> list : tests) {
			Vertex hypVertex = RPNIBlueFringeLearner.getVertex(learned, list);
			Vertex correctVertex = RPNIBlueFringeLearner.getVertex(correct, list);
			if((hypVertex == null)&(correctVertex != null)){
				if(correctVertex.getUserDatum(JUConstants.ACCEPTED).equals("true")){
					//updateFrame(learned, correct);
					failed ++;
				}
			}
			else if(hypVertex !=null & correctVertex!=null){
				if(!(hypVertex.getUserDatum(JUConstants.ACCEPTED).toString().equals(correctVertex.getUserDatum(JUConstants.ACCEPTED).toString()))){
					//updateFrame(learned, correct);
					failed ++;
				}
			}
			else if(hypVertex!=null & correctVertex == null){
				if(hypVertex.getUserDatum(JUConstants.ACCEPTED).equals("true")){
					//updateFrame(learned, correct);
					failed++;
				}
			}
				
		}
		double accuracy = 1-((double)failed/(double)tests.size());
		return accuracy;
	}
	
	public static Set<List<String>> addPercentageFromSamples(Set<List<String>> current, Vector<List<String>> samples, double percent){
		double size = samples.size();
		double number = (size/100)*percent;
		//samples.removeAll(current);
		List<String>[] sampleArray = (List<String>[])samples.toArray(new List[samples.size()]);
		for(int i=0;i<(int)number;i++){
			current.add(sampleArray[i]);
		}
		return current;
	}
	
	public static Set<List<String>> trimToNegatives(DirectedSparseGraph g, Collection<List<String>> sMinus ){
		Set<List<String>> returnSet = new HashSet<List<String>>();
		Iterator<List<String>> sMinusIt = sMinus.iterator();
		while(sMinusIt.hasNext()){
			List<String> currentString = sMinusIt.next();
			final FSMStructure expected = getGraphData(g);
			int reject = tracePath(expected, currentString);
			returnSet.add(currentString.subList(0, reject+1));
		}
		return returnSet;
	}

	public static Collection<List<String>> getPositiveStrings(DirectedSparseGraph graph, Collection<List<String>> samples){
		Iterator<List<String>> sampleIt = samples.iterator();
		HashSet<List<String>> positiveStrings = new HashSet<List<String>>();
		while(sampleIt.hasNext()){
			List<String> v = sampleIt.next();
			if(RPNIBlueFringeLearner.getVertex(graph, v) != null)
				positiveStrings.add(v);
			else
				System.out.println(v);
		}
		return positiveStrings;
	}
	public static Collection<List<String>> randomHalf(Collection<List<String>> v, Random halfRandomNumberGenerator){
		Object[]samples = v.toArray();
		List<List<String>> returnSet = new LinkedList<List<String>>();
		Set<Integer> done = new HashSet<Integer>();
		for(int i=0;i<v.size()/2;i++){
			int randomIndex = 0;
			boolean newInteger = false;
			while(!newInteger){
				randomIndex = halfRandomNumberGenerator.nextInt(v.size());
				Integer current = new Integer(randomIndex);
				if(!done.contains(current)){
					done.add(current);
					newInteger = true;
				}
			}
			returnSet.add((List<String>)samples[randomIndex]);
		}
		return returnSet;
	}
	
	public static final LearnerEvaluatorGenerator [] learnerGenerators = {
		new LearnerEvaluatorGenerator() {
			@Override
			LearnerEvaluator getLearnerEvaluator(String inputFile, String outputDir, int percent) {
				return new RPNIEvaluator(inputFile,outputDir, percent);
			}
		}
		// at this point, one may add the above learners with different arguments or completely different learners such as the Angluin's one
	};
	
	public static final int stageNumber = 10;

	/** The set of possible numbers (non-negative) is divided into sets for each learner, and then into a number
	 * of percent divisions.
	 * 
	 * @param inputFile the input file
	 * @param Number the parameter of the array task.
	 */
	public void processDataSet(String inputFile, int Number)
	{
		final int LearnerNumber = learnerGenerators.length;
		if (Number < 0 || Number >= stageNumber*LearnerNumber)
			throw new IllegalArgumentException("Array task number "+Number+" for file "+inputFile+" is out of range, it should be between 0 and "+LearnerNumber*stageNumber);
		int percentStage = Number % stageNumber;
		results.add(runner.submit(learnerGenerators[Number / stageNumber].getLearnerEvaluator(inputFile, outputDir, 100*(1+percentStage)/stageNumber)));
	}
	
	protected final String outputDir;
	
	/**
	 * For dual-core operation, VM args should be -ea -Xmx1600m -Xms300m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2
	 * Quad-core would use -Dthreadnum=4 instead.
	 * 
	 * There are multiple modes of operation, process graphs, generate data for processing 
	 * and dump sets to files, load data from files and process them, using a user-chosen learner. 
	 * 
	 * @param args command-line arguments
	 */
	public static void main(String[] args)
	{
        if (100 % stageNumber != 0)
        	throw new IllegalArgumentException("wrong compiled-in stageNumber="+stageNumber+": it should be a divisor of 100");

        String outputDir = "output";
        AccuracyAndQuestionsExperiment experiment = new AccuracyAndQuestionsExperiment(outputDir);
		if (args.length < 2)
		{
			File graphDir = new File(args[0]);
					//"C:\\experiment\\graphs-150\\Neil-Data2\\50-6"); 
					//"D:\\experiment\\Neil-Data2\\50-6");
					//System.getProperty("user.dir")+System.getProperty("file.separator")+"resources"+
					//System.getProperty("file.separator")+"TestGraphs"+System.getProperty("file.separator") +"50-6");
	        String[] graphFileList = graphDir.list();
	        String wholePath = graphDir.getAbsolutePath()+System.getProperty("file.separator");
	        for(int i=0;i<graphFileList.length;i++)
	        {
	        	if(!graphFileList[i].startsWith("N"))
	        		continue;
        		for(int learner=0;learner < learnerGenerators.length;++learner)
        			for(int percentStage=0;percentStage < stageNumber;++percentStage)
	        			experiment.processDataSet(wholePath+graphFileList[i], learner*stageNumber+percentStage);
	        }
		}
		else
		{// args.length >=2
            experiment.processDataSet(args[0], Integer.parseInt(args[1]));			
 		}

		Writer outputWriter = null;
		try {
			outputWriter = new StringWriter(); //new FileWriter(outputDir+System.getProperty("file.separator")+"LOG.txt");
			// now obtain the results
	        for(Future<String> computationOutcome:experiment.results)
				outputWriter.write("RESULT: "+computationOutcome.get()+"\n");
	        outputWriter.close();
	        System.out.println(outputWriter.toString());
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		finally
		{
			experiment.shutDown();
		}
	}

	public void shutDown() {
		executorService.shutdown();
	}
	
}
