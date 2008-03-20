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
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import statechum.Configuration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.io.GraphMLFile;

abstract public class AbstractExperiment 
{
	/** The runner of computational threads. */
	private ExecutorService executorService;

	/** Stores results of execution of evaluators. */
	private List<Future<String>> results;
	
	/** Stores tasks to complete. */
	private CompletionService<String> runner;
	
	/** Where to place the results. */
	private String outputDir;

	public String getOutputDir()
	{
		return outputDir;
	}
	
	public AbstractExperiment()
	{
	}
	
	private void initExecutors()
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
	}
	
	public abstract static class LearnerEvaluatorGenerator 
	{
		abstract LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, AbstractExperiment exp);
	}
	
	public abstract static class LearnerEvaluator implements Callable<String>
	{
		/** Configuration for the learner and related.
		 * Important: clone is there to prevent subsequent changes to 
		 * configuration for a given evaluator from changing the 
		 * global (default) configuration. 
		 */
		protected Configuration config = (Configuration)Configuration.getDefaultConfiguration().clone();

		protected LearnerGraph graph=null;
		protected final String inputFileName;
		protected final int instanceID;
		protected final int percent;
		protected final AbstractExperiment experiment;
		
		public LearnerEvaluator(String inputFile, int per, int inID, AbstractExperiment exp) 
		{
			inputFileName = inputFile;instanceID = inID;percent=per;experiment=exp;		
		}

		protected void loadGraph()
		{
			synchronized (LearnerGraph.syncObj) 
			{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
		    	GraphMLFile graphmlFile = new GraphMLFile();
		    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
		    	DirectedSparseGraph g = new DirectedSparseGraph();
		    	g.getEdgeConstraints().clear();
		    	g = (DirectedSparseGraph)graphmlFile.load(inputFileName);
		    	graph = new LearnerGraph(g,config);
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
		
		public static Collection<List<String>> plus = null;
		
		public enum FileType { 
			DATA {String getFileName(String prefix, String suffix) { return prefix+"_data"+suffix+".xml"; } }, 
			RESULT {String getFileName(String prefix, String suffix) { return prefix+"_result"+suffix+".txt"; } };
			
			abstract String getFileName(String prefix, String suffix);
		};
		
		protected String getFileName(FileType fileNameType)
		{
			return fileNameType.getFileName(experiment.getOutputDir()+System.getProperty("file.separator")+instanceID+"_"+(new File(inputFileName).getName()),"-"); 
		}
	}			

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
	public void processDataSet(Reader fileNameListReader, int Number)
	{
		if (fileName.isEmpty()) loadFileNames(fileNameListReader);
		List<LearnerEvaluatorGenerator> evaluatorGenerators = getLearnerGenerators();
		final int LearnerNumber = evaluatorGenerators.size();
		final int NumberMax = fileName.size()*LearnerNumber;
		if (Number < 0 || Number >= NumberMax)
			throw new IllegalArgumentException("Array task number "+Number+" is out of range, it should be between 0 and "+NumberMax);
		// the number is valid.
		int learnerStep = fileName.size();
		int learnerType = Number / learnerStep;
		int fileNumber = (Number % learnerStep);
		int percentStage = (Number % learnerStep);
		results.add(runner.submit(evaluatorGenerators.get(learnerType).getLearnerEvaluator(fileName.get(fileNumber), 100*(1+percentStage)/getStageNumber(),Number, this)));
	}

	
	/** Many experiments involve supplying the learner with a specific %% of 
	 * data and looking at how it performs. This number reflects the number of
	 * %% to split 100% into. If set to 5, this means we'll supply the learner
	 * with 20%,40%,60%,80%,100% of the input data set.
	 */
	abstract public int getStageNumber();
	
	/** Returns a collection of generators for learners. */
	abstract public List<LearnerEvaluatorGenerator> getLearnerGenerators();
	
	/** Given a reader returning a list of graphs to process, this 
	 * method computes the effective number of files to process.
	 * 
	 * @param fileNameListReader
	 * @return
	 */
	public int computeMaxNumber(Reader fileNameListReader)
	{
		int NumberMax = 0;
		try
		{
			loadFileNames(fileNameListReader);
			NumberMax = fileName.size()*getLearnerGenerators().size();
		}
		catch(Exception e)
		{// ignore the exception - NumberMax will remain at 0
		}
		return NumberMax;
	}
		
	/**
	 * For dual-core operation, VM args should be -ea -Xmx1600m -Xms300m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2
	 * Quad-core would use -Dthreadnum=4 instead.
	 * 
	 * There are multiple modes of operation, process graphs, generate data for processing 
	 * and dump sets to files, load data from files and process them, using a user-chosen learner. 
	 * 
	 * @param args command-line arguments, a directory name to process all files inside it or
	 * <FILENAMES_FILE> <OUTPUT_DIR> <NUMBER1> <NUMBER2> <NUMBER3> ... 
	 * where FILENAMES_FILE contains files to process, OUTPUT_DIR is where to store output and NUMBER1 ... are task numbers
	 */
	public void main(String[] args)
	{
        if (100 % getStageNumber() != 0)
        	throw new IllegalArgumentException("wrong stageNumber="+getStageNumber()+": it should be a divisor of 100");

        initExecutors();
        
		if (args.length < 2)
		{
			File graphDir = new File(args[0]);
					//"C:\\experiment\\graphs-150\\Neil-Data2\\50-6"); 
					//"D:\\experiment\\Neil-Data2\\50-6");
					//System.getProperty("user.dir")+System.getProperty("file.separator")+"resources"+
					//System.getProperty("file.separator")+"TestGraphs"+System.getProperty("file.separator") +"50-6");
	        String[] graphFileList = graphDir.list();String listOfFileNames = "";int fileNumber = 0;
	        String wholePath = graphDir.getAbsolutePath()+System.getProperty("file.separator");
	        for(int i=0;i<graphFileList.length;i++)
	        	if(graphFileList[i].endsWith("xml"))
	        	{
	        		listOfFileNames+=wholePath+graphFileList[i]+"\n";fileNumber++;
	        	}
	        StringReader fileNameListReader = new StringReader(listOfFileNames);
	        File outputDirAsFile = new File("output_"+graphDir.getName());
	        if (outputDirAsFile.canRead() || outputDirAsFile.mkdirs())
	        {
	        	outputDir = outputDirAsFile.getAbsolutePath();
		        assert fileNumber*getLearnerGenerators().size() == computeMaxNumber(fileNameListReader);
	       		for(int number=0;number < fileNumber*getLearnerGenerators().size();++number)
		        			processDataSet(fileNameListReader, number);
	        }
		}
		else
		{// args.length >=2
	        outputDir = args[1];
            try {
            	int num = Integer.parseInt(args[2]);
            	if (num >= 0)
            	{
            		for(int i=2;i< args.length;++i)
            			processDataSet(new FileReader(args[0]), Integer.parseInt(args[i]));
            	}
            	else
            		try
            		{
            			System.out.println(computeMaxNumber(new FileReader(args[0])));
            		}
            		catch(Exception ex)
            		{
            			System.out.println(0);// if we cannot compute the number of files to process, return zero.
            		}
            	
			} catch (Exception e) {
				System.out.println("FAILED");
				e.printStackTrace();
			}			
 		}

		// Everywhere here is it enough to simply say "FAILED" because on failure, the output file will either
		// not exist at all or will simply contain "FAILED" in it.
		
		// now obtain the results of processing.
		if (results != null)
	        for(Future<String> computationOutcome:results)
				try {
					System.out.println("RESULT: "+computationOutcome.get()+"\n");
				} catch (Exception e) {
					System.out.println("FAILED");
					e.printStackTrace();
				}
				finally
				{
					executorService.shutdown();
				}
				
	}
	
	
	
}