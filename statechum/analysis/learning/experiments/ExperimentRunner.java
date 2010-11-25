/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
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

package statechum.analysis.learning.experiments;

import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.*;
import java.lang.management.ManagementFactory;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
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
import statechum.GlobalConfiguration;
import statechum.Pair;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;

public class ExperimentRunner 
{
	/** Field separator in CSV. */
	protected static final String FS = ",";
	
	public enum FileType { 
		LEARNT {@Override String getFileName(String prefix, String suffix) { return prefix+"_learnt"+suffix+".xml"; } }, 
		TESTDATA {@Override String getFileName(String prefix, String suffix) { return prefix+"_testdata"+suffix+".xml"; } }, 
		MINUS_AND_TEST {@Override String getFileName(String prefix, String suffix) { return prefix+"_mt"+suffix+".xml"; } }, 
		LOG {@Override String getFileName(String prefix, String suffix) { return prefix+"_log"+suffix+".xml"; } },
		//CSV {String getFileName(String prefix, String suffix) { return "experiment_"+prefix+".csv"; } }, 
		RESULT {@Override String getFileName(String prefix, String suffix) { return prefix+"_result"+suffix+".txt"; } };

		abstract String getFileName(String prefix, String suffix);
	}

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
	
	/** All output directories start with this string. */
	public static final String outputDirNamePrefix = "output_";

	/** Depending on the experiment, we might wish to customise an output directory name. */
	private String outputDirNameSuffix="";

	public void setOutputDirSuffix(String partOfName)
	{
		outputDirNameSuffix=partOfName;
	}
	
	public String getOutputDirSuffix()
	{
		return outputDirNameSuffix;
	}

	public ExperimentRunner()
	{// Empty default constructor
	}

	/** Whether to force <em>setLearnerOverwriteOutput(true)</em> on all the learners. */
	private boolean robust = false;
	
	public boolean isRobust()
	{
		return robust;
	}
	
	public void setRobust(boolean newValue)
	{
		robust = newValue;
	}
	
	/** %% to run a learner through. <em>null</em> means only one stage. */
	private int [] learnerStages = null;
	
	public void setLearnerStages(int [] data)
	{
		if (data == null || data.length == 0)
			learnerStages = null;
		else
		{			
			learnerStages = new int[data.length];System.arraycopy(data, 0, learnerStages, 0, data.length);
		}
	}
	
	public int [] getLearnerStages()
	{
		return learnerStages;
	}
	
	/** Many experiments involve supplying the learner with a specific %% of 
	 * data and looking at how it performs. This could reflect the number of
	 * %% to split 100% such as with 20%,40%,60%,80%,100%.
	 */
	public int [] getStages()
	{
		return learnerStages;
	}
	
	/** Contains information necessary to produce a generator for a learner evaluator. These generators 
	 * are then used to produce learners, for different input files and different percentages. The purpose
	 * for this design is making it possible to only define a few parameters and then run 
	 * each learner on a different thread.
	 */
	public static class GeneratorConfiguration 
	{
		/** Configuration to use with this learner evaluator. */
		private Configuration generatorConfig;
		
		/** Class for the learner evaluator. */
		private Class<? extends LearnerEvaluator> generatorEvaluator;
		
		/** Learner name. */
		private String name;

		
		
		public enum GENERATORCONFIG{ ELEM_CONFIG, ATTR_NAME, ATTR_CLASS }
		
		public GeneratorConfiguration() {
			generatorConfig = Configuration.getDefaultConfiguration().copy();
			generatorEvaluator=null;name=null;
		}
		
		public GeneratorConfiguration(Configuration config, Class<? extends LearnerEvaluator> evalClass, String learnerName)
		{
			generatorConfig = config.copy();generatorEvaluator = evalClass;name = learnerName;
		}
		
		public LearnerEvaluator createNewInstance(String inputFile, int percent, int instanceID, ExperimentRunner exp)
		{
			LearnerEvaluator result = null;
			Constructor<? extends LearnerEvaluator> c = null;
			try {
				c = generatorEvaluator.getConstructor(String.class,int.class,int.class,ExperimentRunner.class,Configuration.class,String.class);
			} catch (Exception e) {
				for(Object o:generatorEvaluator.getConstructors())
					System.out.println(o);
				statechum.Helper.throwUnchecked("failed to create an instance of learner evaluator", e);
			}
			try {
				Configuration cnf = generatorConfig.copy();
				if (exp.isRobust()) cnf.setLearnerOverwriteOutput(false);
				result = c.newInstance(inputFile, percent,instanceID,exp,cnf,name);
			} catch (InvocationTargetException e)
			{
				statechum.Helper.throwUnchecked("failed to create an instance", e.getCause());
			} catch (Exception e) {
				statechum.Helper.throwUnchecked("failed to create an instance", e);
			}
			return result;
		}

		/**
		 * @return the generatorConfig
		 */
		public Configuration getGeneratorConfig() {
			return generatorConfig;
		}

		/**
		 * @param argGeneratorConfig the generatorConfig to set
		 */
		public void setGeneratorConfig(Configuration argGeneratorConfig) {
			this.generatorConfig = argGeneratorConfig;
		}

		/**
		 * @return the generatorEvaluator
		 */
		public Class<? extends LearnerEvaluator> getGeneratorEvaluator() {
			return generatorEvaluator;
		}

		/**
		 * @param argGeneratorEvaluator the generatorEvaluator to set
		 */
		public void setGeneratorEvaluator(
				Class<? extends LearnerEvaluator> argGeneratorEvaluator) {
			this.generatorEvaluator = argGeneratorEvaluator;
		}

		/**
		 * @return the name
		 */
		public String getName() {
			return name;
		}

		/**
		 * @param argName the name to set
		 */
		public void setName(String argName) {
			this.name = argName;
		}
	}
	
	private List<GeneratorConfiguration> generators = new LinkedList<GeneratorConfiguration>();
	
	/** Given a learner generator configuration, adds it to the internal list. */
	public void addLearnerEvaluator(GeneratorConfiguration cnf)
	{
		generators.add(cnf);
	}
	
	/** Given a list learner generator configurations, sets the internal list to it. */
	public void setLearnerEvaluator(List<GeneratorConfiguration> cnfList)
	{
		generators = cnfList;
	}
	
	/** Returns the list of evaluator generator configurations. */
	public List<GeneratorConfiguration> getLearnerEvaluator()
	{
		return generators;
	}
	
	/** Returns a collection of generators for learners. */
	public List<LearnerEvaluatorGenerator> getLearnerGenerators()
	{
		List<LearnerEvaluatorGenerator> result = new LinkedList<LearnerEvaluatorGenerator>();
		for(GeneratorConfiguration gen:generators)
		{
			final GeneratorConfiguration g = gen;
			result.add(new LearnerEvaluatorGenerator() {

				@Override
				LearnerEvaluator getLearnerEvaluator(String inputFile,
						int percent, int instanceID, ExperimentRunner exp) {
					return g.createNewInstance(inputFile, percent, instanceID, exp);
				}});
		}
		return result;
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
		abstract LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, ExperimentRunner exp);
	}
	
	public abstract static class LearnerEvaluator implements Callable<String>
	{
		/** Configuration for the learner and related.
		 */
		final protected Configuration config;

		protected LearnerGraph graph=null;
		protected final String inputFileName;
		protected final int instanceID;
		protected final int percent;
		protected final ExperimentRunner experiment;
		protected final String learnerName;
		
		/** Constructs the experiment runner. 
		 * 
		 * @param inputFile input file to process
		 * @param per percentage
		 * @param instance a single number which can be used to identify this file/percentage/learner_kind combo.
		 * @param exp the enclosing instance of the experiment - a poor man's way to subclassing nested classes.
		 * @param cnf configuration to base this learner experiment on
		 * @param name the name to give to this learner.
		 */
		public LearnerEvaluator(String inputFile, int per, int inID, ExperimentRunner exp, Configuration cnf, String name) 
		{
			inputFileName = inputFile;instanceID = inID;percent=per;experiment=exp;
			config = cnf.copy();learnerName = name;
			if (name == null || name.length() == 0 || name.contains(FS))
				throw new IllegalArgumentException("invalid learner name - it cannot contain \""+FS+"\"");
		}

		String result = "";
		String stats = "";//"Instance: "+instanceID+ "\n";

		/** This one may be overridden by subclass to run the actual experiment.
		 * The result of the experiment is expected to be appended to result. The first line will be post-processed into csv.
		 */
		protected abstract void runTheExperiment();

		/** Returns the name of this evaluator. */
		public String getLearnerName()
		{
			return learnerName; 
		}
		
		@Override 
		public String call()
		{
			// first, check if we need to do anything (i.e. data file exists and shows a successful completion. */
			boolean needToProcess = config.getLearnerOverwriteOutput();
			if (!needToProcess)
			{
				BufferedReader reader = null;
				try
				{
					reader = new BufferedReader(new FileReader(getFileName(FileType.RESULT)));
					String line = reader.readLine();
					if (line != null && line.contains(LearnerEvaluator.OUTCOME.RUNNING.name()))
						needToProcess = true;// previously incomplete run
				}
				catch(Exception ex)
				{
					needToProcess = true;// cannot read the output file, hence rewrite it.
					// This may happen if JVM crashed before even starting to process a graph,
					// which will take place when there are multiple runners and one of them
					// crashes before others have a chance to run.
				}
				finally
				{
					if (reader != null)
						try {
							reader.close();
						} catch (IOException e) {
							// ignore the exception
						}
				}
			}
			OUTCOME currentOutcome = OUTCOME.RUNNING;
			String percentValue = "";
			if (experiment.useStages())
				percentValue = percent+FS;
			String fileName = new File(inputFileName).getName();

			if (needToProcess)
			{// If processing is needed, do this.
				String stdOutput = writeResult(currentOutcome,null);// record the failure result in case something fails later and we fail to update the file, such as if we are killed or run out of memory
				if (stdOutput != null) return stdOutput;
	
				try
				{
					loadGraph();
					runTheExperiment();
					currentOutcome = OUTCOME.SUCCESS;
				}
				catch(Throwable th)
				{
					currentOutcome = OUTCOME.FAILURE;
					StringWriter writer = new StringWriter();
					th.printStackTrace();
					th.printStackTrace(new PrintWriter(writer));
					stats = stats+"\nFAILED\nSTACK: "+writer.toString();
				}
				
				// now record the result.
				// Important: the second element on the second line is the learner name.
				stdOutput = writeResult(currentOutcome, fileName + FS + getLearnerName() + FS + percentValue + result + "\n"+ stats);
				if (stdOutput != null) return stdOutput;
			}// if (needToProcess)
			else currentOutcome = OUTCOME.SUCCESS;// since processing is not needed.
			return fileName+FS+getLearnerName()+FS+percentValue+currentOutcome;
		}

		protected void loadGraph()
		{
			Configuration cnf = config.copy();cnf.setLearnerCloneGraph(true);cnf.setLearnerUseStrings(true);
			try
			{
				graph = new LearnerGraph(cnf);
				AbstractPersistence.loadGraph(new FileReader(inputFileName),graph);
			}
			catch(Exception ex)
			{
				IllegalArgumentException e = new IllegalArgumentException("could not load "+inputFileName);
				e.initCause(ex);throw e;
			}
		}

		/** The first one means successful completion, the second - currently running
		 * (or crashed JVM) and the last one means a failure. The difference between
		 * the last two is that we'd like to restart the learning process when a 
		 * crash is detected, but there is no point doing so upon a failure.
		 * 
		 * @author kirill
		 */
		enum OUTCOME { SUCCESS, RUNNING, FAILURE }

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
				try { if (outputWriter != null) outputWriter.close(); 
				} 
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
			return fileNameType.getFileName(experiment.getOutputDir()+File.separator+instanceID+"_"+(new File(inputFileName).getName()),percentValue); 
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
		try
		{
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
		}
		finally
		{
			reader.close();
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
	 * @throws IOException 
	 */
	public void processDataSet(Reader fileNameListReader, int Number) throws IOException
	{
		results.add(runner.submit(getLearnerEvaluator(fileNameListReader,Number)));
	}

	/** Checks if the supplied instance number is valid.
	 * 
	 * @param fileNameListReader collection of names to check against
	 * @param Number the instance number to check
	 * @throws IOException if it is not possible to load file names.
	 * @throws IllegalArgumentException if the instance number is not valid. 
	 */
	private void verifyInstanceNumber(Reader fileNameListReader, int Number) throws IOException
	{
		if (fileName.isEmpty()) loadFileNames(fileNameListReader);
		List<LearnerEvaluatorGenerator> evaluatorGenerators = getLearnerGenerators();
		final int LearnerNumber = evaluatorGenerators.size();
		final int NumberMax = fileName.size()*getStageNumber()*LearnerNumber;
		if (Number < 0 || Number >= NumberMax)
			throw new IllegalArgumentException("Array task number "+Number+" is out of range, it should be between 0 and "+NumberMax);
		// the number is valid.
	}
	
	private LearnerEvaluator getLearnerEvaluator(Reader fileNameListReader, int Number) throws IOException
	{
		if (fileName.isEmpty()) loadFileNames(fileNameListReader);
		List<LearnerEvaluatorGenerator> evaluatorGenerators = getLearnerGenerators();
		verifyInstanceNumber(fileNameListReader, Number);

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
	 * Returns the number of files which could not be processed.
	 * @throws IOException 
	 */
	public int postProcess_noException(Reader fileNameListReader) throws IOException
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
				if (line != null && line.contains(LearnerEvaluator.OUTCOME.SUCCESS.name()))
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

		return failures;
	}
	
	/** Performs post-processing of results, by loading all files with results, picking the second line from those 
	 * start with a line ending on SUCCESS and appends it to a spreadsheet.
	 * <p/>
	 * Throws an exception if any files could not be processed, but nevertheless builds csv files.
	 * @throws IOException 
	 */
	public void postProcess(Reader fileNameListReader) throws IOException
	{
		int failures = postProcess_noException(fileNameListReader);
		if (failures > 0)
			throw new LearnerFailed(failures+" files could not be processed");
	}
	
	public static class LearnerFailed extends IOException
	{

		/**
		 * ID for serialisation.
		 */
		private static final long serialVersionUID = -586971522348894343L;
		
		public LearnerFailed(String descr)
		{
			super(descr);
		}
		
	}
	
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
	
	public final static int argCMD_COUNT = -1, argCMD_POSTPROCESS = -2;
		
	/** Goes through all files in a specified directory and builds a list of them. */
	Pair<String,Integer> getFileListFromDirName(String dirName)
	{
		File graphDir = new File(dirName);
		if (!graphDir.isDirectory()) throw new IllegalArgumentException("invalid directory");
        String[] graphFileList = graphDir.list();StringBuffer listOfFileNames = new StringBuffer();int fileNumber = 0;
        String wholePath = graphDir.getAbsolutePath()+File.separator;
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
	 * If <NUMBER1> is -1, returns (and displays on the screen) the number of files to process.
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
		{// only a single directory to process, enter the desktop mode.
			if (!graphDir.isDirectory()) throw new IllegalArgumentException("invalid directory");
			Pair<String,Integer> stringInt = getFileListFromDirName(graphDir.getAbsolutePath());
	        fileNameListReader = new StringReader(stringInt.firstElem);int fileNumber = stringInt.secondElem;
	        File outputDirAsFile = new File(graphDir.getParent(),outputDirNamePrefix+outputDirNameSuffix+graphDir.getName());
	        if (outputDirAsFile.canRead() || outputDirAsFile.mkdirs())
	        {
	        	outputDir = outputDirAsFile.getAbsolutePath();
		        assert fileNumber*getLearnerGenerators().size()*getStageNumber() == computeMaxNumber(fileNameListReader);
	       		for(int number=0;number < fileNumber*getLearnerGenerators().size()*getStageNumber();++number)
		        			processDataSet(fileNameListReader, number);
	        }
		}
		else
		{// args.length >=2, enter the Grid mode.
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
            		// This verifies that if some args cannot be parsed, we 
            		// immediately fail. If this is not done, JVM may crash
            		// in one of the files; we'd like all non-learner errors 
            		// to be reported immediately rather.
            		for(int i=2;i< args.length;++i)
            			verifyInstanceNumber(reader, Integer.parseInt(args[i]));
            		
            		for(int i=2;i< args.length;++i)
            			processDataSet(reader, Integer.parseInt(args[i]));
            	}
            	else
        		if (num == argCMD_COUNT)
        		{
    				// the number is placed on the standard output.
        			try
        			{
        				result = computeMaxNumber(reader);
        			}
        			finally
        			{// if we cannot compute the number of files to process, return zero, 
        			 // but propagate the exception.
        				System.out.println(result);
        			}
        		}
            	else
            		if (num == argCMD_POSTPROCESS)
            			postProcess(reader);
            		else throw new IllegalArgumentException("invalid command "+num);
    		}
    		finally
    		{
    			if (reader != null) reader.close();
    		}
 		}

		// Everywhere here is it enough to simply say "FAILED" because on failure, the output file will either
		// not exist at all or will simply contain "FAILED" or "RUNNING" in it.
		
		// now obtain the results of processing.
		if (results != null)
		{
	        for(Future<String> computationOutcome:results)
				try {
					computationOutcome.get();
					//System.out.println("RESULT: "+computationOutcome.get());
				} catch (Exception e) {// here we ignore the exceptions, since a failure in a learner will manifest itself as a failure recorded in a file. In a grid environment, this (reading a file) is the only way we can learn about a failure, hence let post-processor handle this case. 
					//System.out.println("FAILED");
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
	
	/** Expects the first argument to be a file name pointing to the 
	 * serialised instance of this object, deserialises it and runs an experiment
	 * using the subsequent arguments. This way, arguments to 
	 * <em>runExperiment</em> are shifted from those suppiled by one.
	 * @param args arguments to use.
	 */
	public static void main(String args[])
	{
		try 
		{
			
			XMLDecoder decoder = new XMLDecoder(new FileInputStream(args[0]));
			final ExperimentRunner exp = (ExperimentRunner) decoder.readObject();decoder.close();
			String []expArgs = new String[args.length-1];System.arraycopy(args, 1, expArgs, 0, args.length-1);
			if (exp.isForked())
			{
				Thread heartbeatThread = new Thread(new Runnable() {
					@Override 
					public void run() 
					{
						// after processing has started and did not generate an exception, we need
						// to keep heartbeat going to ensure that this process will terminate
						// when the parent process has terminated.
						
						final int timeOutTicks = 5;// how many heartbeats to wait before timing out
						
						long prevTime = new Date().getTime();// each instance remembers the time it was created on, so I cannot re-use an existing instance.
						long currentTime = prevTime;
						try {
							Thread.sleep(exp.timeBetweenHearbeats*3);// wait for the main thread to start processing

							prevTime = new Date().getTime();// account for the time spent waiting for threads to start.
							currentTime=prevTime;
						} catch (InterruptedException e1) {
							// this means that we've been asked to terminate, in this case we
							// pretend that the heartbeat time has elapsed and terminate.

							currentTime = prevTime + (timeOutTicks+1)*exp.timeBetweenHearbeats;// force the following loop to exit immediately 
						}
						
						try
						{
							while (currentTime - timeOutTicks*exp.timeBetweenHearbeats < prevTime)
							{
								int avail = System.in.available();
								if (avail > 0) 
								{
									prevTime = currentTime;// reset the timer.
									while(avail-->0) System.in.read();// drop the chars 
								} 
								try {
									Thread.sleep(exp.timeBetweenHearbeats);// wait for a bit.
								} catch (InterruptedException e1) {
									// this means that we've been asked to terminate, in this case we
									// pretend that the heartbeat time has elapsed and terminate.
									break;
								}
								currentTime = new Date().getTime();
							}
						} catch(IOException e)
						{// failed to read I/O, assume that parent process terminated and abort.
						}
						System.exit(-1);// did not receive a heartbeat or were asked to terminate.
					}
				},"heartbeat");
				heartbeatThread.setDaemon(true);
				heartbeatThread.start();
			} // if forked

			exp.runExperiment(expArgs);
		} catch (Exception e) {
			// no point handling this because the caller will do post-process and will know what happened.
			e.printStackTrace();
		}
		
	}

	/** Indicates if the learner is running in a separate JVM or not - separate one
	 * need heartbeat, integrated does not. 
	 */
	private boolean forked = false;
	
	public boolean isForked()
	{
		return forked;
	}
	
	public void setForked(boolean newValue)
	{
		forked = newValue;
	}
	
	/** How often to check i/o streams and send heartbeat data. */
	protected int timeBetweenHearbeats = Integer.parseInt(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TIMEBETWEENHEARTBEATS));
	
	public int getTimeBetweenHearbeats()
	{
		return timeBetweenHearbeats;
	}
	
	public void setTimeBetweenHearbeats(int newValue)
	{
		timeBetweenHearbeats = newValue;
	}
	
	public interface HandleProcessIO
	{
		/** Called each heartbeat, could be used to prod a process. 
		 * @throws IOException if something fails. 
		 */
		public void OnHeartBeat() throws IOException;
		
		/** Called each time a line is read from the standard output of the monitored process. */
		public void StdOut(StringBuffer b);
		/** Called each time a line is read from the standard error stream of the monitored process.. */
		public void StdErr(StringBuffer b);
	}
	
	/** Displays output/error streams of the supplied process
	 * and supplies the process with heartbeat data.
	 * 
	 * @param p process of interest.
	 * @param probeInterval how often to check a process for input and/or prod it.
	 * @param handler object to handle input/output data.
	 * @throws IOException 
	 */
	public static void dumpStreams(Process p, int probeInterval, HandleProcessIO handler)
	{
		java.io.InputStream err = p.getErrorStream(),out = p.getInputStream();
		StringBuffer errBuffer = new StringBuffer(), outBuffer = new StringBuffer();
		byte []dataBuffer = new byte[100];
		long prevTime = new Date().getTime()-2*probeInterval;
		boolean processRunning = true;
		
		try
		{
			do
			{
				long currentTime = new Date().getTime();
				if (currentTime - probeInterval > prevTime)
				{
					prevTime = currentTime;
				
					int avail = err.available();
					while (avail>0) 
					{// some data available
						int availErr = Math.min(dataBuffer.length,avail);
						err.read(dataBuffer, 0, availErr);
						for(int i=0;i<availErr;++i) 
						{
							byte ch = dataBuffer[i];
							errBuffer.append((char)ch);
							if (ch == '\n') 
							{
								handler.StdErr(errBuffer);errBuffer.setLength(0);// received a full line, display and flush
							}
						}
						avail = err.available();
					}
					
					avail = out.available();
					while (avail>0) 
					{ // some data available
						int availOut = Math.min(dataBuffer.length,avail);
						out.read(dataBuffer, 0, availOut);
						for(int i=0;i<availOut;++i) 
						{
							byte ch = dataBuffer[i];
							outBuffer.append((char)ch);
							if (ch == '\n') 
							{
								handler.StdOut(outBuffer);outBuffer.setLength(0);// received a full line, display and flush
							}
						}
						avail = out.available();
					}
					handler.OnHeartBeat();
				}
				
				try {
					Thread.sleep(probeInterval);// wait for a bit.
				} catch (InterruptedException e1) {
					// this means that we've been asked to terminate, in this case we
					// pretend that the process has terminated and exit.
					processRunning = false;
				}
				
				if (processRunning)
					try
					{
						p.exitValue();
						processRunning = false;// process terminated if exception was not thrown.
					}
					catch(IllegalThreadStateException e)
					{// process not terminated yet, hence continue
						
					}
			} while(processRunning);
		} catch(IOException e)
		{// assume that child process terminated.
			
		}
		
		try
		{
			int avail = err.available();
			while (avail>0) 
			{// some data available
				int availErr = Math.min(dataBuffer.length,avail);
				err.read(dataBuffer, 0, availErr);
				for(int i=0;i<availErr;++i) 
				{
					byte ch = dataBuffer[i];
					errBuffer.append((char)ch);
				}
				avail = err.available();
			}
		}
		catch(IOException e) { 
			// failed to copy data from the stream. Dump to stderr but ignore otherwise.
			e.printStackTrace();
		}
		
		try
		{
			int avail = out.available();
			while (avail>0) 
			{ // some data available
				int availOut = Math.min(dataBuffer.length,avail);
				out.read(dataBuffer, 0, availOut);
				for(int i=0;i<availOut;++i) 
				{
					byte ch = dataBuffer[i];
					outBuffer.append((char)ch);
				}
				avail = out.available();
			}
		}
		catch(IOException e) { 
			// failed to copy data from the stream. Dump to stderr but ignore otherwise.
			e.printStackTrace();
		}
		if (outBuffer.length() > 0) handler.StdOut(outBuffer);
		if (errBuffer.length() > 0) handler.StdOut(errBuffer);
	}
	
	/** Removes the directory and all its files. If the directory contains 
	 * other directories, aborts, but not before deleting some files. 
	 */
	public static void zapDir(File directory)
	{
		if (directory.isDirectory())
		{
			for(File f:directory.listFiles())
			{
				if (f.isDirectory()) throw new IllegalArgumentException("directory to erase should not contain directories");
				if (!f.delete())
				{
					try {
						Thread.sleep(1000);// wait for a second, hoping the lock will be released.
					} catch (InterruptedException e) {
						// ignore
					}
					if (!f.delete()) throw new IllegalArgumentException("cannot delete file "+f);// if we cannot delete a file after waiting for a second, no point to keep waiting 
				}
			}
			directory.delete();
		}
	}

	/** Whether to clear the output directory when we start running - defaults to true.
	 * Occasionally set to false for testing.
	 */
	protected boolean zapOutputDir = true;
	
	/** This method is a Grid-mode-only runner which can be provided with
	 * a configuration to run experiments on and a subclass of 
	 * <em>LearnerEvaluatorGenerator</em> which actually runs them. It also
	 * expects a collection of file names to run on and a path to those files.
	 * The runner is robust - it will restart running if JVM crashes and will
	 * also report failures.
	 * @throws IOException 
	 * @throws InterruptedException 
	 */
	public void robustRunExperiment(String graphDirName, String outputDirName) throws IOException
	{
  		Reader reader = null;File serialisedExperiment = null;
        outputDir = outputDirName;

		try
		{
			File graphDir = new File(graphDirName);
			if (!graphDir.isDirectory()) 
				reader = new FileReader(graphDir.getAbsolutePath());
			else
				reader = new StringReader(getFileListFromDirName(graphDir.getAbsolutePath()).firstElem);
			File outputDirectory = new File(outputDirName);
			
			if (zapOutputDir)
			{
				zapDir(outputDirectory.getAbsoluteFile()); // clear the output directory
				if (!outputDirectory.mkdirs())
					throw new IllegalArgumentException("failed to create directory "+outputDirectory);
			}
			
			// First, we compute the number of files to process. This also verifies that we can obtain a list of files without throwing an exception.
			int graphNumber = computeMaxNumber(reader);
			assert graphNumber > 0;// on zero, exception is thrown by computeMaxNumber.
			
			// following http://forums.sun.com/thread.jspa?threadID=563892&messageID=2788740
			List<String> jvmArgs = ManagementFactory.getRuntimeMXBean().getInputArguments();
			boolean debuggerArgsFound = false;
			for(String arg:jvmArgs)
				if (arg.startsWith("-agentlib:jdwp"))
				{
					debuggerArgsFound = true;break;
				}
			setForked(!debuggerArgsFound);setRobust(!debuggerArgsFound);
			
			// Now serialise this experiment and get the new JVM to load it back.
			serialisedExperiment = File.createTempFile("experiment", ".xml", new File(statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)));
			XMLEncoder encoder = new XMLEncoder(new FileOutputStream(serialisedExperiment));
			encoder.writeObject(this);encoder.close();
			int learnerCounter = 1;
			int failures = -1, attempts =isForked()?0:restarts-1;// if we start under debugger, only run learner once.
			while(failures != 0 && attempts++<restarts)
			{
				for(int currentGraph = 0;currentGraph < graphNumber;currentGraph+=graphsPerRunner)
				{
					int maxGraph = Math.min(currentGraph+graphsPerRunner,graphNumber);
					List<String> commandLine = new LinkedList<String>();
					if (isForked())
					{// arguments for the JVM to run
						commandLine.add(System.getProperty("java.home")+File.separator+"bin/java");
						commandLine.addAll(jvmArgs);commandLine.add("-Djava.awt.headless=true");commandLine.add("-cp");commandLine.add(ManagementFactory.getRuntimeMXBean().getClassPath());
						commandLine.add(this.getClass().getCanonicalName());
					}
					commandLine.add(serialisedExperiment.getAbsolutePath());commandLine.add(graphDir.getAbsolutePath());commandLine.add(outputDirectory.getAbsolutePath());
					for(int i=currentGraph;i<maxGraph;++i)
		    			commandLine.add(Integer.toString(i));
					String []strArgs = new String[commandLine.size()];int pos=0;for(String str:commandLine) strArgs[pos++]=str;
		    		
					if (isForked())
					{// run the JVM and wait for it to terminate
						final Process jvm = Runtime.getRuntime().exec(strArgs);// run every few graphs in a separate JVM
						System.out.println("started learner process "+learnerCounter++);
			    		dumpStreams(jvm,timeBetweenHearbeats,new HandleProcessIO() {
			    			final java.io.OutputStream in = jvm.getOutputStream();

			    			@Override 
			    			public void OnHeartBeat() throws IOException {
								in.write('\n');in.flush();// send heartbeat. If a process has terminated, this will fail, but we would have already absorbed all its output.
			    			}

			    			@Override 
			    			public void StdErr(StringBuffer b) {
			    				System.err.print(b.toString());
			    			}

			    			@Override 
			    			public void StdOut(StringBuffer b) {
			    				System.out.print(b.toString());
			    			}});
			    		try {
							jvm.waitFor();
						} catch (InterruptedException e) {
							statechum.Helper.throwUnchecked("wait for child jvm aborted", e);
						}
					}
					else
					{// running under debugger, hence run directly (the heartbeat flag would've been serialised)
						ExperimentRunner.main(strArgs);
					}
				}
				
				// Now check if processing was successful.
				failures = postProcess_noException(reader);
			}
			if (failures > 0)
				throw new LearnerFailed(failures+" files could not be processed");
			
 		}
		finally
		{
			if (reader != null) reader.close();
			if (serialisedExperiment != null) serialisedExperiment.delete();
		}

	}
	
	/** The number of graphs to learn per runner process. */
	protected int graphsPerRunner = 40;
	
	/** The number of times to restart learning before giving up. */
	protected int restarts = 3;
	
	/** Takes the result and pulls R-bagplot compatible tables out of it.
	 * @param numbers whether column headers are numeric (and hence will be sorted as numbers). 
	 * @throws IOException 
	 */
	public void postProcessIntoR(String learnerFilter,int colSort,boolean numbers,int colNumber, File result) throws IOException
	{
		BufferedReader tableReader = null;
		Writer resultWriter = null;
		try
		{
			tableReader = new BufferedReader(new FileReader(new File(getOutputDir(),resultName)));
			resultWriter = new BufferedWriter(new FileWriter(result));
			postProcessIntoR(learnerFilter,colSort,numbers,colNumber, tableReader,resultWriter);
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
	 * <p>
	 * Example:
	 * <code>
	 * A,10,4
	 * A,15,6
	 * B,10,8
	 * B,15,7
	 * A,10,3
	 * A,15,5
	 * </code>
	 * <em>postprocessIntoR(1,2)</em> would produce
	 * <code>
	 * 10,15
	 * 4 ,16
	 * 8 ,7
	 * 3 ,5
	 * </code>
	 * <em>postprocessIntoR(0,2)</em> would produce
	 * <code>
	 * A,B
	 * 4,8
	 * 6,7
	 * 3,
	 * 5,
	 * </code>
	 * @param numbers whether to expect elements of the table to be numbers and throw an exception if any is not.
	 * @param learnerFilter name for a learner to filter on. When not <em>null</em>, 
	 * only uses files written by the learner with the supplied name.
	 * @throws IOException if something goes wrong. 
	 */
	public static void postProcessIntoR(String learnerFilter, int colSort,boolean numbers, int colNumber, BufferedReader tableReader,Writer resultWriter) throws IOException
	{
		if (colSort < 0 || colNumber < 0)
			throw new IllegalArgumentException("invalid column number");
		
		Map<Object,List<String>> resultMap = new java.util.LinkedHashMap<Object,List<String>>();
		int maxCol = Math.max(colSort,colNumber);
		
		String line = tableReader.readLine();
		while(line != null)
		{
			String splitResult[]=line.split(FS, maxCol+2);
			if (learnerFilter == null || splitResult[1].equals(learnerFilter))
			{// [1] refers to the position of the learner name in the output file.
			 // See the line with 
			 // writeResult(currentOutcome
			 // for details.
				if (splitResult.length <= maxCol)
					throw new IllegalArgumentException("invalid result file: cannot access column "+maxCol+" (failed to parse \""+line+"\")");
				Object colHeader = splitResult[colSort];
				if (numbers)
				{
					try {
						colHeader = new Integer(splitResult[colSort]);
					} catch(NumberFormatException e)
					{// guess it is not an integer, try a double, will throw if it is not a double either.
						colHeader = new Double(splitResult[colSort]);
					}
				}
				List<String> col = resultMap.get(colHeader);
				if (col == null) { col = new LinkedList<String>();resultMap.put(colHeader, col); }
				col.add(splitResult[colNumber]);
			}
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
