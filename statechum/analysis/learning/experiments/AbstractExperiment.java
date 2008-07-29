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
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import statechum.Configuration;
import statechum.Pair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import edu.uci.ics.jung.io.GraphMLFile;

abstract public class AbstractExperiment 
{
	/** Field separator in CSV. */
	protected static final String FS = ",";
	
	public enum FileType { 
		LEARNT {String getFileName(String prefix, String suffix) { return prefix+"_learnt"+suffix+".xml"; } }, 
		MINUS_AND_TEST {String getFileName(String prefix, String suffix) { return prefix+"_mt"+suffix+".xml"; } }, 
		//CSV {String getFileName(String prefix, String suffix) { return "experiment_"+prefix+".csv"; } }, 
		RESULT {String getFileName(String prefix, String suffix) { return prefix+"_result"+suffix+".txt"; } };

		abstract String getFileName(String prefix, String suffix);
	};

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
	
	public static int getCpuNumber()
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
		return ThreadNumber;
	}
	
	private void initExecutors()
	{
		int ThreadNumber = getCpuNumber();
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

		String result = "";
		String stats = "";//"Instance: "+instanceID+ "\n";

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParameters(Configuration c);

		/** This one may be overridden by subclass to run the actual experiment.
		 * The result of the experiment is expected to be appended to result. The first line will be post-processed into csv.
		 */
		protected abstract void runTheExperiment();

		/** Returns the name of this evaluator. */
		protected abstract String getLearnerName();
		
		public String call()
		{
			OUTCOME currentOutcome = OUTCOME.FAILURE;
			String stdOutput = writeResult(currentOutcome,null);// record the failure result in case something fails later and we fail to update the file, such as if we are killed or run out of memory
			if (stdOutput != null) return stdOutput;

			try
			{
				loadGraph();
				changeParameters(config);
				runTheExperiment();
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
			String percentValue = "";
			if (experiment.useStages())
				percentValue = percent+FS;
			String fileName = new File(inputFileName).getName();
			stdOutput = writeResult(currentOutcome, fileName + FS + getLearnerName() + FS + percentValue + result + "\n"+ stats);
			if (stdOutput != null) return stdOutput;
			return fileName+FS+getLearnerName()+FS+percentValue+currentOutcome;
		}
		
		protected void loadGraph()
		{
			synchronized (LearnerGraph.syncObj) 
			{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
		    	GraphMLFile graphmlFile = new GraphMLFile();
		    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
		    	Configuration cnf = (Configuration)config.clone();cnf.setLearnerCloneGraph(true);cnf.setLearnerUseStrings(true);
		    	graph = new LearnerGraph(graphmlFile.load(inputFileName),cnf);
			}
		}

		enum OUTCOME { SUCCESS, FAILURE };
		
		/** Write the provided string into the result file. 
		 * 
		 * @param resultString what to write
		 * @return null on success and an error message on failure
		 */
		protected String writeResult(OUTCOME outcome, String resultString)
		{
			Writer outputWriter = null;
			String stdOutput = null;
			try
			{
				assert !getFileName(FileType.RESULT).contains(FS);
				outputWriter = new BufferedWriter(new FileWriter(getFileName(FileType.RESULT)));
				String percentValue = "";
				if (experiment.useStages())
					percentValue = FS+percent;

				outputWriter.write(inputFileName+percentValue+FS+outcome+"\n"+(resultString == null? "":resultString)+"\n");
			}
			catch(IOException e)
			{
				StringWriter writer = new StringWriter();
				e.printStackTrace(new PrintWriter(writer));
				stdOutput = "\nFAILED TO WRITE A REPORT :"+writer.toString();
			}
			finally
			{
				try { if (outputWriter != null) outputWriter.close(); } 
				catch (IOException e) {
					// ignore this
				}
			}
			return stdOutput;
		}
		
		public static Collection<List<String>> plus = null;
				
		protected String getFileName(FileType fileNameType)
		{
			String percentValue = "";
			if (experiment.useStages())
				percentValue = "-"+percent;
			return fileNameType.getFileName(experiment.getOutputDir()+System.getProperty("file.separator")+instanceID+"_"+(new File(inputFileName).getName()),percentValue); 
		}
	}			
	
	protected ArrayList<String> fileName = new ArrayList<String>(100);

	/** Given a name containing a file with file names, this one adds names of those which can be read, to the
	 * list of them.
	 * 
	 * @param inputFiles the name of a file containing a list of files to load.
	 * @throws IOException 
	 */
	private void loadFileNames(Reader fileNameListReader) throws IOException
	{
		BufferedReader reader = new BufferedReader(fileNameListReader);//new FileReader(inputFiles));
		String line = reader.readLine();
		while(line != null)
		{
			line = line.trim();
			if (line.length() > 0 && !line.startsWith("#"))
			{
				if (new File(line).canRead())
					fileName.add(line);
				else
					throw new IOException("cannot load file "+line);
			}
			line = reader.readLine();
		}
		reader.close();
		
		if (fileName.isEmpty())
			throw new IllegalArgumentException("no usable files found");
	}
	
	/** The set of possible numbers (non-negative) is divided into sets for each learner, and then into a number
	 * of percent divisions.
	 * 
	 * @param inputFile the input file
	 * @param Number the parameter of the array task. negative means "return the highest positive number which can be passed" 
	 * @return what Java process should return
	 * @throws IOException 
	 */
	public void processDataSet(Reader fileNameListReader, int Number) throws IOException
	{
		results.add(runner.submit(getLearnerEvaluator(fileNameListReader,Number)));
	}

	private LearnerEvaluator getLearnerEvaluator(Reader fileNameListReader, int Number) throws IOException
	{
		if (fileName.isEmpty()) loadFileNames(fileNameListReader);
		List<LearnerEvaluatorGenerator> evaluatorGenerators = getLearnerGenerators();
		final int LearnerNumber = evaluatorGenerators.size();
		final int NumberMax = fileName.size()*getStageNumber()*LearnerNumber;
		if (Number < 0 || Number >= NumberMax)
			throw new IllegalArgumentException("Array task number "+Number+" is out of range, it should be between 0 and "+NumberMax);
		// the number is valid.

		int learnerStep = fileName.size()*getStageNumber();
		int learnerType = Number / learnerStep;
		int fileNumber = (Number % learnerStep) / getStageNumber();
		int percentStage = (Number % learnerStep) % getStageNumber();
		int actualPercent = useStages()? getStages()[percentStage]:100;
		return 
			evaluatorGenerators.get(learnerType).getLearnerEvaluator(fileName.get(fileNumber), actualPercent,Number, this);
	}
	
	public boolean useStages()
	{
		return getStages() != null;
	}
	
	public int getStageNumber()
	{
		if (getStages() != null && getStages().length > 0) 
			return getStages().length;
		return 1;
	}
	
	public static final String resultName = "result.csv";
	
	/** Performs post-processing of results, by loading all files with results, picking the second line from those 
	 * start with a line ending on SUCCESS and appends it to a spreadsheet.
	 * <p/>
	 * Throws an exception if any files could not be processed, but nevertheless builds csv files.
	 * @throws IOException 
	 */
	public void postProcess(Reader fileNameListReader) throws IOException
	{
		if (fileName.isEmpty()) loadFileNames(fileNameListReader);
		List<LearnerEvaluatorGenerator> evaluatorGenerators = getLearnerGenerators();
		final int LearnerNumber = evaluatorGenerators.size();
		final int NumberMax = fileName.size()*getStageNumber()*LearnerNumber;
		int failures = 0;
		File resultFile = new File(getOutputDir(),resultName);resultFile.delete();
		Map<String,List<String>> resultMap = new TreeMap<String,List<String>>();
		
		for(int Number=0;Number < NumberMax;++Number)
		{
			LearnerEvaluator evaluator = getLearnerEvaluator(fileNameListReader,Number);
			String line = null;
			try
			{
				BufferedReader reader = new BufferedReader(new FileReader(evaluator.getFileName(FileType.RESULT)));
				line = reader.readLine();
				if (line != null && line.contains(LearnerEvaluator.OUTCOME.SUCCESS.toString()))
				{// get the next line
					line = reader.readLine();if (line != null && line.length() == 0) line = null;
				}
				else line = null;
				reader.close();
			}
			catch(IOException e)
			{
				line = null;
			}
			
			if (line != null)
			{
				List<String> data = resultMap.get(evaluator.getLearnerName());
				if (data == null) { data = new LinkedList<String>();resultMap.put(evaluator.getLearnerName(),data); }
				data.add(line);
			}
			else
				failures++;
		}

		Writer csvWriter=new FileWriter(resultFile);
		for(Entry<String,List<String>> entry:resultMap.entrySet())
			for(String str:entry.getValue())
			csvWriter.write(str+"\n");
		csvWriter.close();

		if (failures > 0)
			throw new IOException(failures+" files could not be processed");
	}
	
	/** Many experiments involve supplying the learner with a specific %% of 
	 * data and looking at how it performs. This could reflect the number of
	 * %% to split 100% such as with 20%,40%,60%,80%,100%.
	 */
	abstract public int [] getStages();
	
	/** Returns a collection of generators for learners. */
	abstract public List<LearnerEvaluatorGenerator> getLearnerGenerators();
	
	/** Given a reader returning a list of graphs to process, this 
	 * method computes the effective number of files to process.
	 * 
	 * @param fileNameListReader
	 * @return
	 * @throws IOException 
	 */
	public int computeMaxNumber(Reader fileNameListReader) throws IOException
	{
		int NumberMax = 0;
		loadFileNames(fileNameListReader);
		NumberMax = fileName.size()*getLearnerGenerators().size()*getStageNumber();
		return NumberMax;
	}
	
	public final int argCMD_COUNT = -1, argCMD_POSTPROCESS = -2;
	
	/** All output directories start with this string. */
	public static final String outputDirNamePrefix = "output_";

	/** Depending on the experiment, we might wish to customise an output directory name. */
	private String outputDirName="";

	public void setOutputDir(String partOfName)
	{
		outputDirName=partOfName;
	}
	
	/** Goes through all files in a specified directory and builds a list of them. */
	Pair<String,Integer> getFileListFromDirName(String dirName)
	{
		File graphDir = new File(dirName);
		if (!graphDir.isDirectory()) throw new IllegalArgumentException("invalid directory");
        String[] graphFileList = graphDir.list();StringBuffer listOfFileNames = new StringBuffer();int fileNumber = 0;
        String wholePath = graphDir.getAbsolutePath()+System.getProperty("file.separator");
        for(int i=0;i<graphFileList.length;i++)
        	if(graphFileList[i].endsWith("xml"))
        	{
        		listOfFileNames.append(wholePath);listOfFileNames.append(graphFileList[i]);listOfFileNames.append('\n');fileNumber++;
        	}
        
        return new Pair<String,Integer>(listOfFileNames.toString(),fileNumber);
	}
	
	/**
	 * For dual-core operation, VM args should be -ea -Xmx1600m -Xms300m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2
	 * Quad-core would use -Dthreadnum=4 instead.
	 * <p/>
	 * There are multiple modes of operation, process graphs, generate data for processing 
	 * and dump sets to files, load data from files and process them, using a user-chosen learner. 
	 * 
	 * @param args command-line arguments, a directory name to process all files inside it or
	 * <FILENAMES_FILE> <OUTPUT_DIR> <NUMBER1> <NUMBER2> <NUMBER3> ... 
	 * where FILENAMES_FILE contains files to process, OUTPUT_DIR is where to store output and NUMBER1 ... are task numbers
	 * If <NUMBER1> is -1, returns the number of files to process.
	 * If <NUMBER1> is -2, post-processes the results.
	 * @throws NumberFormatException 
	 * @throws IOException 
	 */
	public int runExperiment(String[] args) throws NumberFormatException, IOException
	{
		int result = 0;
        StringReader fileNameListReader = null;
        initExecutors();
        File graphDir = new File(args[0]);
        
		if (args.length < 2)
		{
			if (!graphDir.isDirectory()) throw new IllegalArgumentException("invalid directory");
			Pair<String,Integer> stringInt = getFileListFromDirName(graphDir.getAbsolutePath());
	        fileNameListReader = new StringReader(stringInt.firstElem);int fileNumber = stringInt.secondElem;
	        File outputDirAsFile = new File(graphDir.getParent(),outputDirNamePrefix+outputDirName+graphDir.getName());
	        if (outputDirAsFile.canRead() || outputDirAsFile.mkdirs())
	        {
	        	outputDir = outputDirAsFile.getAbsolutePath();
		        assert fileNumber*getLearnerGenerators().size()*getStageNumber() == computeMaxNumber(fileNameListReader);
	       		for(int number=0;number < fileNumber*getLearnerGenerators().size()*getStageNumber();++number)
		        			processDataSet(fileNameListReader, number);
	        }
		}
		else
		{// args.length >=2
	        outputDir = args[1];
	        if (args.length < 3) throw new IllegalArgumentException("expected at least three args in grid mode");
           	int num = Integer.parseInt(args[2]);
       		Reader reader = null;
			try
    		{
				if (!graphDir.isDirectory()) 
					reader = new FileReader(graphDir.getAbsolutePath());
				else
					reader = new StringReader(getFileListFromDirName(graphDir.getAbsolutePath()).firstElem);
				
            	if (num >= 0)
            	{
            		for(int i=2;i< args.length;++i)
            			processDataSet(reader, Integer.parseInt(args[i]));
            	}
            	else
        		if (num == argCMD_COUNT)
        		{
    				// the number is placed on the standard output.
        			result = computeMaxNumber(reader);
        			System.out.println(result);
        		}
            	else
            		if (num == argCMD_POSTPROCESS)
            			postProcess(reader);
            		else throw new IllegalArgumentException("invalid command "+num);
    		}
    		catch(IOException ex)
    		{
    			System.out.println(0);// if we cannot compute the number of files to process, return zero, but rethrow the exception.
    			throw ex;
    		}
    		finally
    		{
    			if (reader != null) reader.close();
    		}
 		}

		// Everywhere here is it enough to simply say "FAILED" because on failure, the output file will either
		// not exist at all or will simply contain "FAILED" in it.
		
		// now obtain the results of processing.
		if (results != null)
		{
	        for(Future<String> computationOutcome:results)
				try {
					System.out.println("RESULT: "+computationOutcome.get());
				} catch (Exception e) {// here we ignore the exceptions, since a failure in a learner will manifest itself as a failure recorded in a file. In a grid environment, this (reading a file) is the only way we can learn about a failure, hence let post-processor handle this case. 
					System.out.println("FAILED");
					//e.printStackTrace();
				}
				finally
				{
					executorService.shutdown();
				}
       		if (fileNameListReader != null) postProcess(fileNameListReader);
		}
		return result;
	}
	
	/** Takes the result and pulls R-bagplot compatible tables out of it.
	 * @param numbers whether column headers are numeric (and hence will be sorted as numbers). 
	 * @throws IOException 
	 */
	public void postProcessIntoR(int colSort,boolean numbers,int colNumber, File result) throws IOException
	{
		BufferedReader tableReader = null;
		Writer resultWriter = null;
		try
		{
			tableReader = new BufferedReader(new FileReader(new File(getOutputDir(),resultName)));
			resultWriter = new BufferedWriter(new FileWriter(result));
			postProcessIntoR(colSort,numbers,colNumber, tableReader,resultWriter);
		}
		finally
		{
			if (tableReader != null) tableReader.close();
			if (resultWriter != null) resultWriter.close();
		}		
	}
	

	/** Takes the result and pulls R-bagplot compatible tables out of it.
	 * Sorts rows according to the value in column colSort and for each value in that
	 * column, places values from column colNumber in a column.
	 * 
	 * Example:
	 * 
	 * A,10,4
	 * A,15,6
	 * B,10,8
	 * B,15,7
	 * A,10,3
	 * A,15,5
	 * 
	 * postprocessIntoR(1,2) would produce
	 * 
	 * 10,15
	 * 4 ,16
	 * 8 ,7
	 * 3 ,5
	 * 
	 * postprocessIntoR(0,2) would produce
	 * 
	 * A,B
	 * 4,8
	 * 6,7
	 * 3,
	 * 5,
	 * 
	 * @throws IOException 
	 */
	public static void postProcessIntoR(int colSort,boolean numbers, int colNumber, BufferedReader tableReader,Writer resultWriter) throws IOException
	{
		if (colSort < 0 || colNumber < 0)
			throw new IllegalArgumentException("invalid column number");
		
		Map<Object,List<String>> resultMap = new TreeMap<Object,List<String>>();
		int maxCol = Math.max(colSort,colNumber);
		
		String line = tableReader.readLine();
		while(line != null)
		{
			String splitResult[]=line.split(FS, maxCol+2);
			if (splitResult.length <= maxCol)
				throw new IllegalArgumentException("invalid result file: cannot access column "+maxCol+" (failed to parse \""+line+"\")");
			Object colHeader = numbers?new Integer(splitResult[colSort]):splitResult[colSort];
			List<String> col = resultMap.get(colHeader);
			if (col == null) { col = new LinkedList<String>();resultMap.put(colHeader, col); }
			col.add(splitResult[colNumber]);
			line = tableReader.readLine();
		}
		if (resultMap.isEmpty())
			throw new IllegalArgumentException("no data to dump");
		
		StringBuffer outHeading = new StringBuffer();
		List<Iterator<String>> iterators = new LinkedList<Iterator<String>>();
		boolean isFirst = true;
		for(Entry<Object,List<String>> entry:resultMap.entrySet()) 
		{
			if (!isFirst) outHeading.append(FS);isFirst=false;outHeading.append(entry.getKey());
			iterators.add(entry.getValue().iterator());
		}
		outHeading.append("\n");
		resultWriter.write(outHeading.toString());
		boolean iteratorsEmpty = true;
		
		do
		{
			StringBuffer outBuffer = new StringBuffer();
			isFirst = true;iteratorsEmpty = true;
			for(Iterator<String> iter:iterators)
			{
				if (!isFirst) outBuffer.append(FS);isFirst=false;
				if (iter.hasNext())
					outBuffer.append(iter.next());
				if (iter.hasNext())
					iteratorsEmpty=false;
			}
			outBuffer.append("\n");
			resultWriter.write(outBuffer.toString());
		}
		while(!iteratorsEmpty);
		
	}
}
