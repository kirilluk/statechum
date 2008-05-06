/*
 * INCOMPLETE
 */

package statechum.analysis.learning.experiments;


import java.awt.Point;
import java.beans.XMLEncoder;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.SwingUtilities;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.io.GraphMLFile;
import statechum.JUConstants;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.computeStateScores;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.computeStateScores.IDMode;
import statechum.xmachine.model.testset.*;
import statechum.xmachine.model.testset.PTATestSequenceEngine.sequenceSet;
import static statechum.xmachine.model.testset.WMethod.getGraphData;
import static statechum.xmachine.model.testset.WMethod.tracePath;

public class IncrementalAccuracyAndQuestionsExperiment {

	private final ExecutorService executorService;
	
	public IncrementalAccuracyAndQuestionsExperiment(String outputD)
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
		abstract LearnerEvaluator getLearnerEvaluator(String inputFile, String ouputDir, int instanceID);
	}
	
	public abstract static class LearnerEvaluator implements Callable<String>
	{
		protected Collection<List<String>> sPlus=null, sMinus=null;
		protected DirectedSparseGraph graph=null;
		protected String inputFileName = null, outputDir = null;
		protected Collection<List<String>> tests = null;
		protected final int instanceID;
		protected int percent;
		protected String inputFileShortName = null;
		
		public LearnerEvaluator(String inputFile, String outputD, int inID) 
		{
			inputFileName = inputFile;outputDir = outputD;instanceID = inID;
			inputFileShortName = new File(inputFileName).getName();
		}

		protected void loadGraph()
		{
			synchronized (computeStateScores.syncObj) 
			{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
		    	GraphMLFile graphmlFile = new GraphMLFile();
		    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
		    	graph = new DirectedSparseGraph();
		    	graph.getEdgeConstraints().clear();
		    	graph = (DirectedSparseGraph)graphmlFile.load(inputFileName);
			}
		}

		enum OUTCOME { SUCCESS, FAILURE };
		
		protected String FS = ",";
		
		/** Write the provided string into the result file. 
		 * 
		 * @param result what to write
		 * @return null on success and an error message on failure
		 */
		protected String writeResult(OUTCOME outcome, String result)
		{
			Writer outputWriter = null;
			String stdOutput = null;
			try
			{
				outputWriter = new BufferedWriter(new FileWriter(getFileName(FileType.RESULT)));
				outputWriter.write(inputFileName+FS+outcome+(result == null? "":FS+result)+"\n");
			}
			catch(IOException e)
			{
				StringWriter writer = new StringWriter();
				e.printStackTrace();
				e.printStackTrace(new PrintWriter(writer));
				stdOutput = "\nFAILED TO WRITE A REPORT :"+writer.toString();
			}
			finally
			{
				try { if (outputWriter != null) outputWriter.close(); } catch (IOException e) { e.printStackTrace(); }
			}
			return stdOutput;
		}
		
		protected RandomPathGenerator buildSetsHalfNegative(int size)
		{
			
	    	RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),size,5);// the seed for Random should be the same for each file
	    	WMethod tester = new WMethod(graph,1);
			tests = (Collection<List<String>>)tester.getFullTestSet();
			tests.addAll(tester.getTransitionCover());
			tests.removeAll(rpg.getAllPaths());
			tests.removeAll(rpg.getNegativePaths());
			
	    	return rpg;
			
		}

		public static Collection<List<String>> plus = null;
		
		public enum FileType { 
			DATA {String getFileName(String prefix, String suffix) { return prefix+"_data"+suffix+".xml"; } }, 
			LEARNT {String getFileName(String prefix, String suffix) { return prefix+"_learnt"+".xml"; } }, 
			MINUS_AND_TEST {String getFileName(String prefix, String suffix) { return prefix+"_mt"+".xml"; } }, 
			RESULT {String getFileName(String prefix, String suffix) { return prefix+"_result"+".txt"; } };
			
			abstract String getFileName(String prefix, String suffix);
		};
		
		protected String getFileName(FileType fileNameType)
		{
			return fileNameType.getFileName(outputDir+System.getProperty("file.separator")+instanceID+"_"+(new File(inputFileName).getName()),"-"); 
		}
	}
	
	/** This one is not static because it refers to the frame to display results. */
	public static abstract class RPNIEvaluator extends LearnerEvaluator
	{
		public RPNIEvaluator(String inputFile, String outputDir, int instanceID)
		{
			super(inputFile, outputDir, instanceID);			
		}

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParametersOnComputeStateScores(computeStateScores c);

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParametersOnLearner(RPNIBlueFringeLearner l);
		
		public String call()
		{
			//System.out.println(inputFileName+" (instance "+instanceID+"), learner "+this+", "+percent + "% started at "+Calendar.getInstance().getTime());
			OUTCOME currentOutcome = OUTCOME.FAILURE;
			String stdOutput = writeResult(currentOutcome,null);// record the failure result in case something fails later and we fail to update the file, such as if we are killed or run out of memory
			if (stdOutput != null) return stdOutput;
			loadGraph();
			int size = 4*graph.numVertices();
			RandomPathGenerator rpg = buildSetsHalfNegative(size);
			sMinus = new HashSet<List<String>>();
			sPlus = new HashSet<List<String>>();
			Collection<List<String>> allPositive = rpg.getAllPaths();
			final FSMStructure fsm = WMethod.getGraphData(graph);
			WMethod wm = new WMethod(fsm,1);
			final AtomicInteger atomicInt = new AtomicInteger(0);
			RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null)
			{
				protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
				{
					atomicInt.addAndGet(1);
					return tracePath(fsm, question);
				}
			};
			//l.setCertaintyThreshold(3);
			l.setMinCertaintyThreshold(-1); //question threshold
			int number = size/10;
			for(int per=10;per<101;per=per+10){
				this.percent = per;atomicInt.set(0);
				sPlus = addNumberFromSamples(sPlus, allPositive, number);
				if(per!=30 && per!=100)
					continue;
				sMinus = rpg.makeCollectionNegative(sPlus, 1);
				//posNegExperiment(fsm, l, wm);
				questionsExperiment(fsm, l, wm,atomicInt);
			}
			
			return inputFileName+"success";
		}
		
		private void questionsExperiment(FSMStructure fsm, RPNIBlueFringeLearnerTestComponentOpt l, WMethod wm,AtomicInteger questionNumber){
			PosNegPrecisionRecall prNeg = computePR(fsm, l, wm, new HashSet<List<String>>(), sMinus);
			System.out.println(inputFileName+", "+percent+", "+prNeg.precision+", "+prNeg.recall+", "+l.getStats()+", "+questionNumber);
		}
		
		private PosNegPrecisionRecall computePR(FSMStructure fsm, RPNIBlueFringeLearnerTestComponentOpt l, 
				WMethod wm, Collection<List<String>> splus, Collection<List<String>> sminus){
			
			FSMStructure learned = learn(l,splus, sminus);
			PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(learned);
			Collection<List<String>> testSet = wm.getFullTestSet();

			PTATestSequenceEngine engineTestSet = new PTA_FSMStructure(fsm);
			sequenceSet partialPTA = engineTestSet.new sequenceSet();partialPTA.setIdentity();
			partialPTA = partialPTA.cross(testSet);

			PTATestSequenceEngine engineSetMinus = new PTA_FSMStructure(fsm);
			sequenceSet origMinus = engineSetMinus.new sequenceSet();origMinus.setIdentity();
			origMinus.cross(sminus);
			
			try {
				computeStateScores.writeGraphML(learned, getFileName(FileType.LEARNT));
				XMLEncoder outData = new XMLEncoder(new FileOutputStream(getFileName(FileType.MINUS_AND_TEST)));
				outData.writeObject(sminus);
				outData.writeObject(testSet);
				outData.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			precRec.crossWith(engineSetMinus);
			return precRec.crossWith(engineTestSet);
		}
		
		private FSMStructure learn(RPNIBlueFringeLearnerTestComponentOpt l, Collection<List<String>> sPlus, Collection<List<String>> sMinus){
			DirectedSparseGraph learningOutcome = null;
			PTASequenceSet plusPTA = new PTASequenceSet();plusPTA.addAll(sPlus);PTASequenceSet minusPTA = new PTASequenceSet();minusPTA.addAll(sMinus);
			changeParametersOnComputeStateScores(l.getScoreComputer());
			l.init(plusPTA, minusPTA);
			changeParametersOnLearner(l);
			learningOutcome = l.learnMachine();
			/*if(percent == 30)
				System.out.print(l.getQuestionCounter()+",");
			else
				System.out.println(l.getQuestionCounter());
			*/
			l.setQuestionCounter(0);
			return WMethod.getGraphData(learningOutcome);
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
				if(!hypVertex.getUserDatum(JUConstants.ACCEPTED).equals("false")){
					//updateFrame(learned, correct);
					failed++;
				}
			}
				
		}
		double accuracy = 1-((double)failed/(double)tests.size());
		return accuracy;
	}
	
	public static Set<List<String>> addPercentageFromSamples(Set<List<String>> current, Collection<List<String>> samples, double percent){
		double size = samples.size();
		double number = size*percent/100;
		List<String>[] sampleArray = (List<String>[])samples.toArray(new List[samples.size()]);
		for(int i=0;i<(int)number;i++){
			current.add(sampleArray[i]);
		}
		return current;
	}
	
	public static Collection<List<String>> addNumberFromSamples(Collection<List<String>> current, Collection<List<String>> samples, double number){
		double size = samples.size();
		List<String>[] sampleArray = (List<String>[])samples.toArray(new List[samples.size()]);
		int currSize = current.size();
		for(int i=currSize;i<(int)currSize+number&&i<size;i++){
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
		}
		return positiveStrings;
	}
	
	public static Collection<List<String>> getNegativeStrings(DirectedSparseGraph graph, Collection<List<String>> samples){
		Iterator<List<String>> sampleIt = samples.iterator();
		HashSet<List<String>> negativeStrings = new HashSet<List<String>>();
		while(sampleIt.hasNext()){
			List<String> v = sampleIt.next();
			if(RPNIBlueFringeLearner.getVertex(graph, v) == null)
				negativeStrings.add(v);
		}
		return negativeStrings;
	}
	
	private static Collection half(Collection original){
		Object[] array = original.toArray();
		Collection newList = new HashSet();
		for(int i=0;i<array.length/2;i++)
			newList.add(array[i]);
		return newList;
	}
	
	public static final LearnerEvaluatorGenerator [] learnerGenerators = {
		new LearnerEvaluatorGenerator() {
			@Override
			LearnerEvaluator getLearnerEvaluator(String inputFile, String outputDir, int instanceID) {
				return new RPNIEvaluator(inputFile,outputDir, instanceID)
				{
					@Override
					protected void changeParametersOnLearner(RPNIBlueFringeLearner l)
					{
					}
					
					@Override
					protected void changeParametersOnComputeStateScores(computeStateScores c) 
					{
						c.setMode(IDMode.POSITIVE_NEGATIVE);						
					}

					@Override
					public String toString()
					{
						return "RPNI, POSITIVE_NEGATIVE";
					}
				};
			}
		}
	};
	
	protected ArrayList<String> fileName = new ArrayList<String>(100);

	/** Given a name containing a file with file names, this one adds names of those which can be read, to the
	 * list of them.
	 * 
	 * @param inputFiles the name of a file containing a list of files to load.
	 */
	private void loadFileNames(Reader fileNameListReader)
	{
		try {
			BufferedReader reader = new BufferedReader(fileNameListReader);//new FileReader(inputFiles));
			String line = reader.readLine();
			while(line != null)
			{
				line = line.trim();
				if (line.length() > 0 && !line.startsWith("#")
						&& new File(line).canRead())
					fileName.add(line);
				
				line = reader.readLine();
			}
		} catch (IOException e) {
			throw new IllegalArgumentException("failed to read the list of files");
		}		
		if (fileName.isEmpty())
			throw new IllegalArgumentException("no usable files found");
	}
	
	/** The set of possible numbers (non-negative) is divided into sets for each learner, and then into a number
	 * of percent divisions.
	 * 
	 * @param inputFile the input file
	 * @param Number the parameter of the array task. negative means "return the highest positive number which can be passed" 
	 * @return what Java process should return
	 */
	public int processDataSet(Reader fileNameListReader, int Number)
	{
		if (fileName.isEmpty()) loadFileNames(fileNameListReader);
		final int LearnerNumber = learnerGenerators.length;
		final int NumberMax = fileName.size()*LearnerNumber;
		if (Number < 0 || Number >= NumberMax)
			throw new IllegalArgumentException("Array task number "+Number+" is out of range, it should be between 0 and "+NumberMax);
		else
		{// the number is valid.
			int learnerStep = fileName.size();
			int learnerType = Number / learnerStep;
			int fileNumber = (Number % learnerStep);
			int percentStage = (Number % learnerStep);
			results.add(runner.submit(learnerGenerators[learnerType].getLearnerEvaluator(fileName.get(fileNumber), outputDir, Number)));
			return 0;
		}
	}

	public int computeMaxNumber(Reader fileNameListReader)
	{
		int NumberMax = 0;
		try
		{
			loadFileNames(fileNameListReader);
			final int LearnerNumber = learnerGenerators.length;
			NumberMax = fileName.size()*LearnerNumber;
		}
		catch(Exception e)
		{// ignore the exception - NumberMax will remain at 0
		}
		return NumberMax;
	}
	
	protected final String outputDir;
	
	/**
	 * For dual-core operation, VM args should be -ea -Xmx1600m -Xms300m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2
	 * Quad-core would use -Dthreadnum=4 instead.
	 * 
	 * There are multiple modes of operation, process graphs, generate data for processing 
	 * and dump sets to files, load data from files and process them, using a user-chosen learner. 
	 * 
	 * @param args command-line arguments, a directory name to process all files inside it or
	 * <FILENAMES_FILE> <OUTPUT_DIR> <NUMBER> where FILENAMES_FILE contains files to process, OUTPUT_DIR is where to store output and NUMBER is the task number
	 */
	public static void main(String[] args)
	{
        
        IncrementalAccuracyAndQuestionsExperiment experiment = null;
        
		if (args.length < 2)
		{
			File graphDir = new File(args[0]);
					//"C:\\experiment\\graphs-150\\Neil-Data2\\50-6"); 
					//"D:\\experiment\\Neil-Data2\\50-6");
					//System.getProperty("user.dir")+System.getProperty("file.separator")+"resources"+
					//System.getProperNty("file.separator")+"TestGraphs"+System.getProperty("file.separator") +"50-6");
	        String[] graphFileList = graphDir.list();String listOfFileNames = "";int fileNumber = 0;
	        String wholePath = graphDir.getAbsolutePath()+System.getProperty("file.separator");
	        for(int i=0;i<graphFileList.length;i++)
	        	if(graphFileList[i].endsWith("xml"))
	        	{
	        		listOfFileNames+=wholePath+graphFileList[i]+"\n";fileNumber++;
	        	}
	        StringReader fileNameListReader = new StringReader(listOfFileNames);
	        File outputDir = new File("output_"+graphDir.getName());
	        if (outputDir.canRead() || outputDir.mkdirs())
	        {
		        experiment = new IncrementalAccuracyAndQuestionsExperiment(outputDir.getAbsolutePath());
		        assert fileNumber*learnerGenerators.length == experiment.computeMaxNumber(fileNameListReader);
	       		for(int number=0;number < fileNumber*learnerGenerators.length;++number)
		        			experiment.processDataSet(fileNameListReader, number);
	        }
		}
		else
		{// args.length >=2
	        experiment = new IncrementalAccuracyAndQuestionsExperiment(args[1]);
            try {
            	int num = Integer.parseInt(args[2]);
            	if (num >= 0)
            	{
            		for(int i=2;i< args.length;++i)
            			experiment.processDataSet(new FileReader(args[0]), Integer.parseInt(args[i]));
            	}
            	else
            		try
            		{
            			System.out.println(experiment.computeMaxNumber(new FileReader(args[0])));
            		}
            		catch(Exception ex)
            		{
            			System.out.println(0);
            		}
            	
			} catch (Exception e) {
				System.out.println("FAILED");
				e.printStackTrace();
			}			
 		}

		// Everywhere here is it enough to simply say "FAILED" because on failure, the output file will either
		// not exist at all or will simply contain "FAILED" in it.
		
		// now obtain the results
		if (experiment != null && experiment.results != null)
	        for(Future<String> computationOutcome:experiment.results)
				try {
						//        System.out.println("RESULT: "+computationOutcome.get()+"\n");
				} catch (Exception e) {
					System.out.println("FAILED");
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
