/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 
/*
 * INCOMPLETE
 */

package statechum.analysis.learning.experiments;


import java.awt.Point;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
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

import javax.swing.SwingUtilities;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.io.GraphMLFile;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.analysis.learning.Configuration;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.Configuration.IDMode;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.xmachine.model.testset.*;
import statechum.xmachine.model.testset.PTATestSequenceEngine.sequenceSet;

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
		
		public LearnerEvaluator(String inputFile, String outputD, int inID) 
		{
			inputFileName = inputFile;outputDir = outputD;instanceID = inID;			
		}

		protected void loadGraph()
		{
			synchronized (LearnerGraph.syncObj) 
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
	    	final int numberOfExtraStates = 1;
	    	LearnerGraph tester = new LearnerGraph(graph,Configuration.getDefaultConfiguration());
			tests = (Collection<List<String>>)tester.wmethod.getFullTestSet(numberOfExtraStates);
			tests.addAll(tester.wmethod.getTransitionCover());
			tests.removeAll(rpg.getAllPaths());
			tests.removeAll(rpg.getNegativePaths());
			
	    	return rpg;
			
		}

		public static Collection<List<String>> plus = null;
		
		public enum FileType { 
			DATA {String getFileName(String prefix, String suffix) { return prefix+"_data"+suffix+".xml"; } }, 
			RESULT {String getFileName(String prefix, String suffix) { return prefix+"_result"+suffix+".txt"; } };
			
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
		/** Configuration for the learner and related.
		 * Important: clone is there to prevent subsequent changes to 
		 * configuration for a given evaluator from changing the 
		 * global (default) configuration. 
		 */
		protected Configuration config = (Configuration)Configuration.getDefaultConfiguration().clone();
		
		public RPNIEvaluator(String inputFile, String outputDir, int instanceID)
		{
			super(inputFile, outputDir, instanceID);			
		}

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParameters(Configuration c);

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
			final LearnerGraph fsm = new LearnerGraph(graph,config);
			
			RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null,config)
			{
				protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
				{
					return fsm.paths.tracePath(question);
				}
			};
			l.setCertaintyThreshold(3);
			l.setMinCertaintyThreshold(0); //question threshold
			int number = size/10;
			for(int percent=10;percent<101;percent=percent+10){
				this.percent = percent;
				sPlus = addNumberFromSamples(sPlus, allPositive, number);
				if(/*percent!=30/*&*/percent!=100)
					continue;
				sMinus = rpg.makeCollectionNegative(sPlus, 1);
				questionsExperiment(fsm, l);
			}
			
			return inputFileName+"success";
		}

		private void questionsExperiment(LearnerGraph fsm, RPNIBlueFringeLearnerTestComponentOpt l){
			PosNegPrecisionRecall prNeg = computePR(fsm, l, new HashSet<List<String>>(), sMinus);
			System.out.println(prNeg.precision+", "+prNeg.recall);
		}
		
		private void posNegExperiment(LearnerGraph fsm, RPNIBlueFringeLearnerTestComponentOpt l){
			PosNegPrecisionRecall prNeg = computePR(fsm, l, new HashSet<List<String>>(), sMinus);
			PosNegPrecisionRecall pr= computePR(fsm, l, sPlus, new HashSet<List<String>>());
			System.out.println(pr.getPosprecision()+", "+pr.getPosrecall()/*+", "+pr.getNegprecision()+", "+pr.getNegrecall()+", "+prNeg.getPosprecision()+", "+prNeg.getPosrecall()+", "+prNeg.getNegprecision()+", "+prNeg.getNegrecall()*/);
		}
		
		private PosNegPrecisionRecall computePR(LearnerGraph fsm, RPNIBlueFringeLearnerTestComponentOpt l, Collection<List<String>> splus, Collection<List<String>> sminus){
			
			LearnerGraph learned = learn(l,splus, sminus);
			PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(learned);
			PTATestSequenceEngine engine = new PTA_FSMStructure(fsm);
			sequenceSet partialPTA = engine.new sequenceSet();partialPTA.setIdentity();
			partialPTA = partialPTA.cross(fsm.wmethod.getFullTestSet(1));
			return precRec.crossWith(engine);
		}

		private LearnerGraph learn(RPNIBlueFringeLearnerTestComponentOpt l, Collection<List<String>> sPlus, Collection<List<String>> sMinus){
			DirectedSparseGraph learningOutcome = null;
			changeParameters(config);
			PTASequenceSet plusPTA = new PTASequenceSet();plusPTA.addAll(sPlus);PTASequenceSet minusPTA = new PTASequenceSet();minusPTA.addAll(sMinus);
			l.init(plusPTA, minusPTA);
			learningOutcome = l.learnMachine();
			/*if(percent == 30)
				System.out.print(l.getQuestionCounter()+",");
			else
				System.out.println(l.getQuestionCounter());
			*/
			l.setQuestionCounter(0);
			return new LearnerGraph(learningOutcome,config);
		}
		
	
	}
		
	private static void printPR(Vector<PosNegPrecisionRecall> results){
		/*PosNegPrecisionRecall thirty = results.get(0);
		PosNegPrecisionRecall hundred = results.get(1);
		System.out.println(thirty.getRecall()+","+thirty.getPrecision()+","+hundred.getRecall()+","+hundred.getPrecision());
		//System.out.println(hundred.getPosprecision()+","+hundred.getPosrecall()+","+hundred.getNegprecision()+","+hundred.getNegrecall());
		*/System.out.print("p:"+",");
		for (PrecisionRecall recall : results) {
			System.out.print(recall.getPrecision()+",");
		}
		System.out.println();
		System.out.print("r:"+",");
		for (PrecisionRecall recall : results) {
			System.out.print(recall.getRecall()+",");
		}
		System.out.println();
	}
	
	/** Stores results of execution of evaluators. */
	final List<Future<String>> results;
	
	/** Stores tasks to complete. */
	final CompletionService<String> runner;
			
	private static double computeAccuracy(DirectedSparseGraph learned, DirectedSparseGraph correct, Collection<List<String>> tests){
		int failed = 0;
		for (List<String> list : tests) {
			Vertex hypVertex = RPNIBlueFringeLearner.getVertex(learned, list);
			Vertex correctVertex = RPNIBlueFringeLearner.getVertex(correct, list);
			if((hypVertex == null)&(correctVertex != null)){
				if(DeterministicDirectedSparseGraph.isAccept(correctVertex)){
					//updateFrame(learned, correct);
					failed ++;
				}
			}
			else if(hypVertex !=null & correctVertex!=null){
				if(DeterministicDirectedSparseGraph.isAccept(hypVertex) != DeterministicDirectedSparseGraph.isAccept(correctVertex)){
					//updateFrame(learned, correct);
					failed ++;
				}
			}
			else if(hypVertex!=null & correctVertex == null){
				if(DeterministicDirectedSparseGraph.isAccept(hypVertex)){
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
					protected void changeParameters(Configuration c) 
					{
						c.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);						
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
