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
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.io.GraphMLFile;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.analysis.learning.Configuration;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.Configuration.IDMode;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.xmachine.model.testset.*;

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
		abstract LearnerEvaluator getLearnerEvaluator(String inputFile, String ouputDir, int percent, int instanceID);
	}
	
	public abstract static class LearnerEvaluator implements Callable<String>
	{
		protected Collection<List<String>> sPlus=null, sMinus=null;
		protected DirectedSparseGraph graph=null;
		protected String inputFileName = null, outputDir = null;
		protected ArrayList<List<String>> tests = null;
		protected int percent;
		protected final int instanceID;
		
		/** Configuration for the learner and related.
		 * Important: clone is there to prevent subsequent changes to 
		 * configuration for a given evaluator from changing the 
		 * global (default) configuration. 
		 */
		protected Configuration config = (Configuration)Configuration.getDefaultConfiguration().clone();

		public LearnerEvaluator(String inputFile, String outputD, int per, int inID) 
		{
			inputFileName = inputFile;outputDir = outputD;percent = per;instanceID = inID;			
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
				outputWriter.write(inputFileName+FS+percent+FS+outcome+(result == null? "":FS+result)+"\n");
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
		
		protected void buildSetsHalfNegative()
		{
			loadGraph();
			int size = (graph.numVertices()*graph.numVertices())/2;
	    	RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),size/2,5);// the seed for Random should be the same for each file
	    	LearnerGraph tester = new LearnerGraph(graph,config);
			tests = new ArrayList<List<String>>();tests.addAll(tester.wmethod.getFullTestSet(0));
			tests.removeAll(rpg.getAllPaths());
			Set<List<String>> currentSamples = new LinkedHashSet<List<String>>();
			int number = size*percent/100;
			currentSamples = addNumberFromSamples(currentSamples, rpg.getAllPaths(), number/2);
			currentSamples = addNumberFromSamples(currentSamples, rpg.getNegativePaths(), number/2);
			sPlus = getPositiveStrings(graph,currentSamples);
			
			sMinus = getNegativeStrings(graph,currentSamples);
			/*FSMStructure fsm = WMethod.getGraphData(graph);
			for(int i=0;i<size;i++){
				sMinus.add(pickNegativeTest(fsm));
			}*/
			System.out.println("total at this percentage: "+currentSamples.size()+", plus : "+sPlus.hashCode()+"-"+sPlus.size()+" minus: "+sMinus.hashCode()+"-"+sMinus.size());
		}
		
		public List<String> pickNegativeTest(LearnerGraph fsm){
			Random generator = new Random();
			boolean accepted = true;
			List<String> negativeString = null;
			while(accepted){
				int randomIndex = generator.nextInt(tests.size()-1);
				List<String> testSequence = tests.get(randomIndex);

				int pos = fsm.paths.tracePath(testSequence);
				if (pos >= 0)
				{// Note: the part from pos = until the line with subList (both inclusive) have been tested as a part of testing of truncateSequence
					negativeString = testSequence.subList(0, pos+1);// up to a rejected position plus one
					if(!sMinus.contains(negativeString))
						accepted=false;
				}
			}
			tests.remove(negativeString);
			return negativeString;
		}
		
		protected void buildSets()
		{
			loadGraph();
			int size = graph.getEdges().size()*4;
	    	RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),size,5);// the seed for Random should be the same for each file
	    	LearnerGraph tester = new LearnerGraph(graph,config);
	    	tests = new ArrayList<List<String>>();tests.addAll(tester.wmethod.getFullTestSet(2));
			//tests = randomHalf(fullTestSet,new Random(0));
			//Collection<List<String>> fullSampleSet = WMethod.crossWithSet(rpg.getAllPaths(),WMethod.computeAlphabet(graph));
			
			// this one ensures that walks are of length diameter+5 if they exist and some will not exist
			//l.setCertaintyThreshold(2);
			//Collection<List<String>> sampleSet = randomHalf(fullSampleSet,new Random(1));
			//Vector<List<String>> samples = new Vector<List<String>>();
			//samples.addAll(sampleSet);
			tests.removeAll(rpg.getAllPaths());
			Set<List<String>> currentSamples = new LinkedHashSet<List<String>>();
			
			currentSamples = addPercentageFromSamples(currentSamples, rpg.getAllPaths(), percent);
			sPlus = getPositiveStrings(graph,currentSamples);
			sMinus = new HashSet<List<String>>();
			
			//System.out.println("total at this percentage: "+currentSamples.size()+", plus : "+sPlus.hashCode()+"-"+sPlus.size()+" minus: "+sMinus.hashCode()+"-"+sMinus.size());
		}

		public static Collection<List<String>> plus = null;
		
		public enum FileType { 
			DATA {String getFileName(String prefix, String suffix) { return prefix+"_data"+suffix+".xml"; } }, 
			RESULT {String getFileName(String prefix, String suffix) { return prefix+"_result"+suffix+".txt"; } };
			
			abstract String getFileName(String prefix, String suffix);
		};
		
		protected String getFileName(FileType fileNameType)
		{
			return fileNameType.getFileName(outputDir+System.getProperty("file.separator")+instanceID+"_"+(new File(inputFileName).getName()),"-"+percent); 
		}
	}
	
	/** This one is not static because it refers to the frame to display results. */
	public static abstract class RPNIEvaluator extends LearnerEvaluator
	{
		public RPNIEvaluator(String inputFile, String outputDir, int per, int instanceID)
		{
			super(inputFile, outputDir, per,instanceID);			
		}

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParameters(Configuration c);

		public String call()
		{
			//System.out.println(inputFileName+" (instance "+instanceID+"), learner "+this+", "+percent + "% started at "+Calendar.getInstance().getTime());
			OUTCOME currentOutcome = OUTCOME.FAILURE;
			String stdOutput = writeResult(currentOutcome,null);// record the failure result in case something fails later and we fail to update the file, such as if we are killed or run out of memory
			if (stdOutput != null) return stdOutput;
			
			buildSetsHalfNegative();
			
			final LearnerGraph fsm = new LearnerGraph(graph,config);
			RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null,config)
			{
				protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
				{
					return fsm.paths.tracePath(question);
				}
			};
			//l.setCertaintyThreshold(10);
			l.setMinCertaintyThreshold(500000); //question threshold
			DirectedSparseGraph learningOutcome = null;
			String result = "";
			String stats = "Instance: "+instanceID+", learner: "+this+", sPlus: "+sPlus.size()+" sMinus: "+sMinus.size()+" tests: "+tests.size()+ "\n";
			try
			{
				changeParameters(config);
				PTASequenceSet plusPTA = new PTASequenceSet();plusPTA.addAll(sPlus);PTASequenceSet minusPTA = new PTASequenceSet();minusPTA.addAll(sMinus);
				stats = stats + "Actual sequences, sPlus: "+plusPTA.size()+" sMinus: "+minusPTA.size()+ " ";
				l.init(plusPTA, minusPTA);

				learningOutcome = l.learnMachine();
				result = result+l.getQuestionCounter()+FS+computeAccuracy(learningOutcome, graph,tests);
				if(this.percent == 10)
					System.out.println();
				System.out.print(computeAccuracy(learningOutcome, graph,tests)+",");
				//System.out.println(instanceID+","+result);
				//updateFrame(g,learningOutcome);
				l.setQuestionCounter(0);
				if (learningOutcome != null)
					stats = stats+(learningOutcome.containsUserDatumKey(JUConstants.STATS)? "\n"+learningOutcome.getUserDatum(JUConstants.STATS).toString():"");
				//System.out.println(inputFileName+" (instance "+instanceID+"), learner "+this+", "+ percent+"% terminated at "+Calendar.getInstance().getTime());
				currentOutcome = OUTCOME.SUCCESS;
			}
			catch(Throwable th)
			{
				StringWriter writer = new StringWriter();
				th.printStackTrace();
				th.printStackTrace(new PrintWriter(writer));
				stats = stats+"\nFAILED\nSTACK: "+writer.toString();
			}
			
			// now record the result
			stdOutput = writeResult(currentOutcome, result + "\n"+ stats);
			if (stdOutput != null) return stdOutput;
			return inputFileName+FS+percent+FS+currentOutcome;
		}
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
	
	public static Set<List<String>> addNumberFromSamples(Set<List<String>> current, Collection<List<String>> samples, double number){
		double size = samples.size();
		List<String>[] sampleArray = (List<String>[])samples.toArray(new List[samples.size()]);
		for(int i=0;i<(int)number;i++){
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
			LearnerEvaluator getLearnerEvaluator(String inputFile, String outputDir, int percent, int instanceID) {
				return new RPNIEvaluator(inputFile,outputDir, percent, instanceID)
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
		}/*,
		new LearnerEvaluatorGenerator() {
			@Override
			LearnerEvaluator getLearnerEvaluator(String inputFile, String outputDir, int percent, int instanceID) {
				return new RPNIEvaluator(inputFile,outputDir, percent, instanceID)
				{
					@Override
					protected void changeParametersOnLearner(RPNIBlueFringeLearner l)
					{
					}

					@Override
					protected void changeParametersOnComputeStateScores(ComputeStateScores c) 
					{
						c.setMode(IDMode.POSITIVE_ONLY);						
					}

					@Override
					public String toString()
					{
						return "RPNI, POSITIVE_ONLY";
					}
				};
			}
		},
		new LearnerEvaluatorGenerator() {
			@Override
			LearnerEvaluator getLearnerEvaluator(String inputFile, String outputDir, int percent, int instanceID) {
				return new RPNIEvaluator(inputFile,outputDir, percent, instanceID)
				{
					protected void changeParametersOnLearner(RPNIBlueFringeLearner l)
					{
					}

					@Override
					protected void changeParametersOnComputeStateScores(ComputeStateScores c) 
					{
						c.bumpPositive();
						c.setMode(IDMode.POSITIVE_ONLY);
					}

					@Override
					public String toString()
					{
						return "RPNI, POSITIVE_ONLY with a bump of 1";
					}
				};
			}
		},
		new LearnerEvaluatorGenerator() {
			@Override
			LearnerEvaluator getLearnerEvaluator(String inputFile, String outputDir, int percent, int instanceID) {
				return new RPNIEvaluator(inputFile,outputDir, percent, instanceID)
				{
					protected void changeParametersOnLearner(RPNIBlueFringeLearner l)
					{
					}

					@Override
					protected void changeParametersOnComputeStateScores(ComputeStateScores c) 
					{
						c.bumpPositive();c.useCompatibilityScore();
						c.setMode(IDMode.POSITIVE_ONLY);
					}

					@Override
					public String toString()
					{
						return "RPNI, POSITIVE_ONLY with a bump of 1 and compatibility scores";
					}
				};
			}
		}*/
		// at this point, one may add the above learners with different arguments or completely different learners such as the Angluin's one
	};
	
	public static final int stageNumber = 10;
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
	
	/** The set of possible numbers (non-negative) is divided into sets for 
	 * each learner, and then into a number of percent divisions.
	 * 
	 * @param inputFile the input file
	 * @param Number the parameter of the array task.
	 */
	public void processDataSet(Reader fileNameListReader, int Number)
	{
		if (fileName.isEmpty()) loadFileNames(fileNameListReader);
		final int LearnerNumber = learnerGenerators.length;
		final int NumberMax = fileName.size()*stageNumber*LearnerNumber;
		if (Number < 0 || Number >= NumberMax)
			throw new IllegalArgumentException("Array task number "+Number+" is out of range, it should be between 0 and "+NumberMax);
		// the number is valid.
		int learnerStep = fileName.size()*stageNumber;
		int learnerType = Number / learnerStep;
		int fileNumber = (Number % learnerStep) / stageNumber;
		int percentStage = (Number % learnerStep) % stageNumber;
		results.add(runner.submit(learnerGenerators[learnerType].getLearnerEvaluator(fileName.get(fileNumber), outputDir, 100*(1+percentStage)/stageNumber, Number)));
	}

	public int computeMaxNumber(Reader fileNameListReader)
	{
		int NumberMax = 0;
		try
		{
			loadFileNames(fileNameListReader);
			final int LearnerNumber = learnerGenerators.length;
			NumberMax = fileName.size()*stageNumber*LearnerNumber;
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
        if (100 % stageNumber != 0)
        	throw new IllegalArgumentException("wrong compiled-in stageNumber="+stageNumber+": it should be a divisor of 100");
        AccuracyAndQuestionsExperiment experiment = null;
        
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
		        experiment = new AccuracyAndQuestionsExperiment(outputDir.getAbsolutePath());
		        assert fileNumber*learnerGenerators.length*stageNumber == experiment.computeMaxNumber(fileNameListReader);
	       		for(int number=0;number < fileNumber*learnerGenerators.length*stageNumber;++number)
		        			experiment.processDataSet(fileNameListReader, number);
	        }
		}
		else
		{// args.length >=2
	        experiment = new AccuracyAndQuestionsExperiment(args[1]);
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
