/* Copyright (c) 2014 The University of Sheffield.
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.ProgressIndicator;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.RExperimentResult;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;

/**
 * @author kirill
 *
 */
public class SGE_ExperimentRunner 
{
	
	/**
	 * Default constructor. 
	 */
	public SGE_ExperimentRunner() {
	}

	public static final String separator = "|", separatorRegEx="\\|";
	
	
	public interface processSubExperimentResult<EXPERIMENT_PARAMETERS extends ThreadResultID,RESULT extends ExperimentResult<EXPERIMENT_PARAMETERS>>
	{
		/** Called to plot results of the experiment, using the result <i>r</i>. The <i>experimentrunner</i> is what is to be used to perform plotting, via 
		 * calls to {@link statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment#RecordR(RExperimentResult, Comparable, Double, String, String)}.
		 * The outcome of these calls are stored in a file and subsequently assembled and plotted.
		 * 
		 * @param result the outcome of running an experiment.
		 * @param runSubExperiment
		 */
        void processSubResult(RESULT result, RunSubExperiment<EXPERIMENT_PARAMETERS, RESULT> runSubExperiment)  throws IOException;
		
		/** Returns all graphs that will be plotted. This is needed because we would rather not store axis names in text files. */
        SGEExperimentResult[] getGraphs();
	}
	
	
	public enum PhaseEnum
	{
		RUN_TASK, // takes task ID as an argument and runs tasklets of the specific task in sequence. Intended to run on Iceberg.
		COLLECT_RESULTS, // takes results from tasks and builds a spreadsheet or graphs with results. Throws an exception if any task did not complete.
		COLLECT_AVAILABLE, // similar to COLLECT_RESULTS but only collates results from experiments that have completed. Missing cells are replaced with blanks and rows with no data are omitted completely.
		COUNT_TASKS, // performs workload partitioning and constructs a map from tasks to tasklets. The parameter is the number of tasks to split the work into.
		RUN_STANDALONE,  // runs all tasks in one go. This does not create progress files and therefore only useful when everything is likely to complete fast. 
		// A much better choice is to run "COUNT_TASKS 1" followed by "RUN_PARALLEL 1" which constructs progress files. 
		RUN_PARALLEL, // this is similar to RUN_TASK but will run all tasklets corresponding to the same virtual task in parallel. This permits multiple PCs to easily run different segments of work across their CPUs.
		 // It is important to point out that learnt graphs are recorded and evaluation can be repeated, except that run time will be reset to 0 because it is not recorded in graphs.
		PROGRESS_INDICATOR, // used to report the %% of completed tasklets.
		REPORT_TASKPARAMETERS // used to report which task IDs are associated with which tasklets. This is necessary to be able to debug specific tasklets.
	}
	
	public static class RunSubExperiment<EXPERIMENT_PARAMETERS extends ThreadResultID,RESULT extends ExperimentResult<EXPERIMENT_PARAMETERS>> 
	{
		private final PhaseEnum phase;
		/** We need both taskCounterFromPreviousSubExperiment and taskCounter in order to run multiple series of experiments,
		 * where a number of submitTask calls are followed with the same number of processResults.
		 */
		private int taskCounter=0, taskCounterFromPreviousSubExperiment;
		/** Virtual task to run, each virtual corresponds to a set of actual tasks that have not finished. */
		private int virtTask = 0, tasksToSplitInto =0;
		private ExecutorService executorService;
		private final Map<Integer,EXPERIMENT_PARAMETERS> taskIDToParameters = new TreeMap<>();
		
		protected CompletionService<RESULT> runner = null; 
		
		protected Set<Integer> taskletWasRun = new TreeSet<>();
		private final DrawGraphs gr = new DrawGraphs();

		private final String tmpDir;
		
		/** Makes it possible to obtain the current phase in order to run phase-specific tasks such as result analysis for the COLLECT_RESULTS phase. */
		public PhaseEnum getPhase()
		{
			return phase;
		}
		
		public RunSubExperiment(int cpuNumber,String dir, String []args)
		{
			tmpDir = dir+File.separator;
			if (args.length == 0)
				// no args means standalone
				phase = PhaseEnum.RUN_STANDALONE;
			else
			{
				phase = PhaseEnum.valueOf(args[0]);
				switch(phase)
				{
				case RUN_TASK:
				case RUN_PARALLEL:
					if (args.length != 2)
						throw new IllegalArgumentException("task number should be provided");
					int taskValue = 0;
					try
					{
						taskValue = Integer.parseInt(args[1]);
					}
					catch(NumberFormatException e)
					{
						Helper.throwUnchecked("invalid number", e);
					}
					if (taskValue <= 0)
						throw new IllegalArgumentException("task number should be positive");
					virtTask = taskValue;
					virtTaskToRealTask = loadVirtTaskToReal(tmpDir);
					break;
				case RUN_STANDALONE:
				case PROGRESS_INDICATOR:
					if (args.length != 1)
						throw new IllegalArgumentException("no arguments is permitted for phase "+phase);
					break;
				case COUNT_TASKS:
					if (args.length != 2)
						throw new IllegalArgumentException("the number of tasks per virtual task has to be provided");
					try
					{
						tasksToSplitInto = Integer.parseInt(args[1]);
					}
					catch(NumberFormatException e)
					{
						Helper.throwUnchecked("invalid number", e);
					}
					if (tasksToSplitInto <= 0)
						throw new IllegalArgumentException("the number of real tasks to run should be positive");
					break;
				case REPORT_TASKPARAMETERS:
					if (args.length != 1)
						throw new IllegalArgumentException("no arguments is permitted for phase "+phase);
					virtTaskToRealTask = loadVirtTaskToReal(tmpDir);
					break;
				case COLLECT_RESULTS:
				case COLLECT_AVAILABLE:
					if (args.length == 2)
						plotName = args[1];
					else
						if (args.length != 1)
							throw new IllegalArgumentException("at most one argument is permitted for phase "+phase);
					break;
				}
			}
			executorService = Executors.newFixedThreadPool(phase == PhaseEnum.RUN_TASK?1:cpuNumber);// only create one thread if running on Iceberg on in uni-processor mode.
			runner = new ExecutorCompletionService<>(executorService);
		}

		protected void shutdown()
		{
			if (executorService != null) { executorService.shutdownNow();executorService = null; }
			// if I call DrawGraphs.end() here, tests will fail because I'm repeatedly start/stop R
		}

		/** Has  to be called at the very end of computation. */
		public int successfulTermination()
		{
			int outcome = 0;
			switch(phase)
			{
			case COUNT_TASKS:
				outcome = constructVirtToReal();				
				System.out.println(outcome);
				break;
			case PROGRESS_INDICATOR:
				outcome = taskCounter > 0?(100*(taskCounter-availableTasks.size())/taskCounter):0;
				System.out.println("Progress: "+outcome+"%");
				break;
			case REPORT_TASKPARAMETERS:
				for(Entry<Integer,Set<Integer>> entry:virtTaskToRealTask.entrySet())
				{
					for(int task:entry.getValue())
						System.out.println("{"+entry.getKey()+","+task+"} - "+taskIDToParameters.get(task).getSubExperimentName()+" [ "+taskIDToParameters.get(task).getRowID()+" , "+taskIDToParameters.get(task).getColumnID()+" ]");
				}
				break;
			default:
				break;
			}
			shutdown();
			return outcome;
		}

		/** Submits a task for processing. The way runner is to be used is by submitting a number of tasks then collecting
		 * outcome then submitting more, collecting more outcome etc. Variable <b>taskCounter</b> is the number of the currently
		 * submitted task. It is important to point out that
		 */
		public void submitTask(UASExperiment<EXPERIMENT_PARAMETERS,RESULT> task)
		{
			// At the start we check that names do not contain invalid characters.
			if (task.par.getSubExperimentName().contains(SGE_ExperimentRunner.separator))
				throw new IllegalArgumentException("experiment name for "+task.par.getSubExperimentName()+" should not contain \""+SGE_ExperimentRunner.separator+"\"");
			if (task.par.getRowID().contains(SGE_ExperimentRunner.separator))
				throw new IllegalArgumentException("row ID for "+task.par.getSubExperimentName()+" should not contain \""+SGE_ExperimentRunner.separator+"\"");
			if (task.par.getColumnID().contains(SGE_ExperimentRunner.separator))
				throw new IllegalArgumentException("column ID for "+task.par.getSubExperimentName()+" should not contain \""+SGE_ExperimentRunner.separator+"\"");
			for(String t:task.par.getColumnText())
				if (t.contains(SGE_ExperimentRunner.separator))
					throw new IllegalArgumentException("column data text for experiment "+task.par.getSubExperimentName()+" should not contain \""+SGE_ExperimentRunner.separator+"\"");
			for(String t:task.par.headerValuesForEachCell())
				if (t.contains(SGE_ExperimentRunner.separator))
					throw new IllegalArgumentException("cell text for experiment "+task.par.getSubExperimentName()+" should not contain \""+SGE_ExperimentRunner.separator+"\"");

			taskIDToParameters.put(taskCounter, task.par);
			switch(phase)
			{
			case RUN_TASK:// when running in Grid mode, each task runs as a separate process given that we intend to run them on separate nodes.
			{
				Set<Integer> tasksForVirtualTask = virtTaskToRealTask.get(virtTask);
				if (tasksForVirtualTask != null && tasksForVirtualTask.contains(taskCounter))
				{
					if (!checkExperimentComplete(taskCounter)) // only run a task if we do not have a result, without it it will overwrite a result and execution time and other transient data not stored in the outcome such as true/false counters will be lost.
					{
							taskletWasRun.add(taskCounter);// mark the task as started.
							BufferedWriter writer = null;
							try
							{
								writer = new BufferedWriter(new FileWriter(constructTaskStartedFileName(taskIDToParameters.get(taskCounter))));// indicates tasks that have started
							}
							catch(IOException ex)
							{// ignore an error if a 'taskstarted' file cannot be created.							
							}
							finally
							{
								if (writer != null)
									try {
										writer.close();
									} catch (IOException e) {
										// cannot do much about it here, ignore it.
									}
								writer = null;
							}
							runner.submit(task);
					}
				}
				break;
			}	
			case RUN_PARALLEL:
			{// Here we are running tasks in parallel rather than on a grid therefore no point creating tasks-started files.
				Set<Integer> tasksForVirtualTask = virtTaskToRealTask.get(virtTask);
				if (tasksForVirtualTask != null && tasksForVirtualTask.contains(taskCounter))
				{
					if (!checkExperimentComplete(taskCounter)) // only run a task if we do not have a result, without it it will overwrite a result and execution time and other transient data not stored in the outcome such as true/false counters will be lost.
					{
						taskletWasRun.add(taskCounter);// mark the task as added to the queue.
						runner.submit(task);
					}
				}
				break;
			}
			case RUN_STANDALONE:
				runner.submit(task);
				break;
			case COUNT_TASKS:
			case PROGRESS_INDICATOR:
			case REPORT_TASKPARAMETERS:
				break;
			case COLLECT_RESULTS:
			case COLLECT_AVAILABLE:
				break;
			}
			++taskCounter;
		}

		/** Returns the number of the current task. The returned value is only updated on a call to {@link RunSubExperiment#submitTask(UASExperiment)}. */
		public int getTaskID()
		{
			return taskCounter;
		}
		
		public static String sanitiseFileName(String name)
		{
			return name.replaceAll("[:\\\\/ ]", "_");
		}
		
		public static String constructFileName(String dirToUse, String prefix, ThreadResultID par)
		{
			String pathName = 
			 dirToUse+sanitiseFileName(par.getSubExperimentName())+"-"+
					sanitiseFileName(par.getRowID());
			statechum.analysis.learning.experiments.UASExperiment.mkDir(pathName);
			return pathName+File.separator+sanitiseFileName((prefix == null?"":prefix+"-")+par.getColumnID());
		}

		public static String constructFileName(String dirToUse, ThreadResultID par)
		{
			return constructFileName(dirToUse,null,par);
		}
		
		protected String constructFileName(int rCounter)
		{
			if (!taskIDToParameters.containsKey(rCounter))
				throw new IllegalArgumentException("task ID "+rCounter+" does not have associated parameters recorded anywhere, has it been added via submitTask?");
			return constructFileName(tmpDir,taskIDToParameters.get(rCounter));
		}
		
		protected String constructTaskStartedFileName(EXPERIMENT_PARAMETERS pars)
		{
			String pathName = 
					 tmpDir+sanitiseFileName(pars.getSubExperimentName())+"-"+
							sanitiseFileName(pars.getRowID());
					statechum.analysis.learning.experiments.UASExperiment.mkDir(pathName);
			return pathName+File.separator+sanitiseFileName(pars.getColumnID())+".sgetaskstarted-"+getHostName();
		}
		
		/** Plots the supplied graphs. If the task number is divisible by 10, plots them on the screen, if negative - dumps a pdf.  
		 * It is also responsible for a progress indicator in an interactive mode.
		 * 
		 * @param graphs graphs to plot
		 * @param counter determines whether to plot them on the screen or into a file.
		 */
		protected void plotAllGraphs(Collection<SGEExperimentResult> graphs, int counter)
		{
//			if (counter > 0 && counter % 10 == 0)
//				for(@SuppressWarnings("rawtypes") RGraph g:graphs)
//					g.drawInteractive(gr);
				
			if (counter < 0)
				for(SGEExperimentResult g:graphs)
					g.reportResults(gr);
		}
		
		public static final String CHECKSUMFIELD = "CHECKSUM", CPUSPEEDFIELD = "CPUSPEED";
		
		/** Updates the CRC with data in the provided text, ignoring end of line characters. */
		public static void updateCRC(java.util.zip.CRC32 crc, String text)
		{
			for(int idx=0;idx < text.length();idx++)
			{
				char ch = text.charAt(idx);
				if ( ch != '\n' && ch != '\r' && ch != '\t' )
					crc.update(ch);
			}
		}
		
		/** Returns true if this is run all in the same jvm, permitting one to collect statistics during collection of results and output it via println. 
		 * This is not possible where run on a grid because collection of results is pulling a subset of data from the disk.
		 */
		public boolean isInteractive()
		{
			return phase == PhaseEnum.RUN_STANDALONE;
		}
		
		/** If a task returns null as an outcome, throw an exception. nulls are returned on fatal errors such as where an exception was thrown by a task. 
		 * This is mostly used for testing, because an exception in a single task is no excuse for stopping other tasks that are run in parallel.
		 */
		private boolean throwOnTaskReturningNull = false;
		
		public void setThrowOnTaskReturningNull(boolean value)
		{
			throwOnTaskReturningNull = value;
		}
		
		public boolean getThrowOnTaskReturningNull()
		{
			return throwOnTaskReturningNull;
		}
		
		Map<String,SGEExperimentResult> nameToGraph = null;
		StringWriter outputWriter = null;
		/** Plot to pick. If not null, only plots with that name will be handled. If null, all plots will be constructed.
		 * Used to avoid plotting graphs that take a long time (which applies to R graphs with a large number of data points).
		 */
		String plotName;
		
		/** For a given task ID, loads the result and feeds them into R or spreadsheet. 
		 * 
		 * @param rCounter task ID
		 * plotName instance variable is plot to pick. If not null, only plots with that name will be handled. If null, all plots will be constructed. Used to avoid plotting graphs that take a long time (which applies to R graphs with a large number of data points).
		 */
		private void loadExperimentResult(int rCounter)
		{
			BufferedReader reader = null;
			try
			{
				reader = new BufferedReader(new FileReader(constructFileName(rCounter)));
				String line = reader.readLine();
				boolean foundCRC = false;
			    java.util.zip.CRC32 crc = new java.util.zip.CRC32();
				while(line != null)
				{
					String [] data = line.split(separatorRegEx,-2);
					if (data.length < 1)
						throw new IllegalArgumentException("Experiment in "+constructFileName(rCounter)+" did not log any result");
					String name = data[0];
					if (name.equals(CHECKSUMFIELD))
					{// checksum field
						if (data.length != 2)
							throw new IllegalArgumentException("Experiment in "+constructFileName(rCounter)+" has a truncated/too long CRC field");
						long extractedValue = Long.parseLong(data[1],16);
						if (crc.getValue() != extractedValue)
							throw new IllegalArgumentException("Experiment in "+constructFileName(rCounter)+" has an invalid CRC");
						foundCRC = true;
					}
					else
					if (name.equals(CPUSPEEDFIELD))
					{// cpu field, ignored because its purpose is to ensure we can split tasks into those run on different CPU types on Iceberg, therefore permitting adjustment of the time correction value for each cpu type.
						updateCRC(crc,line);
					}
					else
					{// normal field
						if (!nameToGraph.containsKey(name))
							throw new IllegalArgumentException("Experiment in "+constructFileName(rCounter)+" refers to an unknown graph "+name);
						updateCRC(crc,line);
						if (plotName == null || plotName.equals(name))
						{
							SGEExperimentResult thisPlot = nameToGraph.get(name);
							thisPlot.parseTextLoadedFromExperimentResult(data, constructFileName(rCounter), taskIDToParameters.get(rCounter),false);
						}
					}
					line = reader.readLine();
				}
				if (!foundCRC)
					throw new IllegalArgumentException("Experiment in "+constructFileName(rCounter)+" does not have a CRC");
			}
			catch(IOException ex)
			{
				Helper.throwUnchecked("failed to load results of experiment number ["+rCounter+"]", ex);
			}
			finally
			{
				if (reader != null)
					try {
						reader.close();
					} catch (IOException e) {
						// ignore close failure
					}
			}
		}
		
		private boolean checkExperimentComplete(int rCounter)
		{
			boolean outcome = true;
			BufferedReader reader = null;
			try
			{
				reader = new BufferedReader(new FileReader(constructFileName(rCounter)));
				String line = reader.readLine();
				boolean foundCRC = false;
			    java.util.zip.CRC32 crc = new java.util.zip.CRC32();
				while(line != null)
				{
					String [] data = line.split(separatorRegEx,-2);
					if (data.length < 1)
					{
						outcome = false;
						break;
					}
					String name = data[0];
					if (name.equals(CHECKSUMFIELD))
					{// checksum field
						if (data.length != 2)
						{
							outcome = false;
							break;
						}
						long extractedValue = Long.parseLong(data[1],16);
						if (crc.getValue() != extractedValue)
						{
							outcome = false;
							break;
						}
						foundCRC = true;
					}
					else
					if (name.equals(CPUSPEEDFIELD))
					{// cpu field, ignored because its purpose is to ensure we can split tasks into those run on different CPU types on Iceberg, therefore permitting adjustment of the time correction value for each cpu type.
						updateCRC(crc,line);
					}
					else
					{// normal field
						if (!nameToGraph.containsKey(name))
						{
							outcome = false;
							break;
						}
						updateCRC(crc,line);
					
						SGEExperimentResult thisPlot = nameToGraph.get(name);
						
						thisPlot.parseTextLoadedFromExperimentResult(data, constructFileName(rCounter), taskIDToParameters.get(rCounter), true);
					}
					line = reader.readLine();
				}
				if (!foundCRC)
					outcome = false;
			}
			catch(Exception ex)
			{// if we are here, it means that loading failed, regardless of exception that was thrown.
				outcome = false;
			}
			finally
			{
				if (reader != null)
					try {
						reader.close();
					} catch (IOException e) {
						// ignore close failure
					}
			}
			
			return outcome;
		}
		private Map<Integer,Set<Integer>> virtTaskToRealTask = null;
		
		public static Map<Integer,Set<Integer>> loadVirtTaskToReal(String tmpDir)
		{
			BufferedReader reader = null;Map<Integer,Set<Integer>> virtTaskToRealTask = new TreeMap<>();
			try
			{
				reader = new BufferedReader(new FileReader(tmpDir+GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.SGE_MAP_FILENAMEPREFIX)+"-virtToReal.map"));
				StringBuilder text = new StringBuilder();
				String line = reader.readLine();
				while(line != null)
				{
					text.append(line);line = reader.readLine();
				}
				OtpErlangObject obj = ErlangLabel.parseText(text.toString());
				if (!(obj instanceof OtpErlangList))
					throw new IllegalArgumentException("loading virtTaskToRealTask: expected a sequence of sequences, got "+obj);
				for(OtpErlangObject o:((OtpErlangList)obj))
				{
					if (!(o instanceof OtpErlangTuple))
						throw new IllegalArgumentException("loading virtTaskToRealTask: expected a sequence of sequences, got "+o);
					OtpErlangTuple tuple = (OtpErlangTuple)o;
					if (!(tuple.elementAt(0) instanceof OtpErlangInt))
						throw new IllegalArgumentException("loading virtTaskToRealTask: expected a sequence of sequences, got "+tuple.elementAt(0));
					int virtTaskID = ((OtpErlangInt)tuple.elementAt(0)).intValue();
					OtpErlangObject listOfTasksObj = tuple.elementAt(1);
					if (!(listOfTasksObj instanceof OtpErlangList))
						throw new IllegalArgumentException("loading virtTaskToRealTask: expected a sequence, got "+listOfTasksObj);
					OtpErlangList listOfTasks = (OtpErlangList)listOfTasksObj;
					Set<Integer> outcome = new TreeSet<>();
					for(OtpErlangObject taskNumberObj:listOfTasks)
					{
						if (!(taskNumberObj instanceof OtpErlangInt))
							throw new IllegalArgumentException("loading virtTaskToRealTask: expected an int among tasks, got "+taskNumberObj);
						outcome.add(((OtpErlangInt)taskNumberObj).intValue());
					}
					virtTaskToRealTask.put(virtTaskID,outcome);
				}
			}
			catch(IOException | OtpErlangRangeException ex)
			{
				Helper.throwUnchecked("failed to load virtToReal.map", ex);
			}
			finally
			{
				if (reader != null)
					try {
						reader.close();
					} catch (IOException e) {
						// ignore close failure
					}
			}
			
			return virtTaskToRealTask;
		}
		
		private List<Integer> availableTasks= null;
		
		private void updateAvailableTasks(int from, int to)
		{
			if (availableTasks == null)
				availableTasks= new ArrayList<>();
			for(int task=from;task < to;++task)
			{// ensures we only consider tasks that need running.
				if (!checkExperimentComplete(task))
					availableTasks.add(task);
			}
		}
		
		private int constructVirtToReal()
		{
			int currentVirtualTask=0, currentTask=0;
			int tasksPerVirtualTask = availableTasks.size()/tasksToSplitInto;if (tasksPerVirtualTask == 0) tasksPerVirtualTask=1;

			BufferedWriter outWriter = null;
			try
			{
				UASExperiment.mkDir(tmpDir);
				outWriter = new BufferedWriter(new FileWriter(tmpDir+GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.SGE_MAP_FILENAMEPREFIX)+"-virtToReal.map"));outWriter.write("[\n");
				boolean firstTuple = true;
				for(int vTaskCnt=0;vTaskCnt<tasksToSplitInto && currentTask < availableTasks.size();++vTaskCnt)
				{
					if (firstTuple) firstTuple = false;else outWriter.write("\n,\n");
					outWriter.write('{');outWriter.write(Integer.toString(++currentVirtualTask));outWriter.write(",\n\t[");
					boolean first=true;
					int howManyForThisVTask = Math.max(tasksPerVirtualTask, // max takes into account the case where availableTasks.size() is smaller than tasksToSplitInto
							(int)( (1+vTaskCnt)*(long)availableTasks.size()/tasksToSplitInto-currentTask)// this takes into account the rounding error from computing tasksPerVirtualTask
					);
					for(int i=0;(i<howManyForThisVTask || vTaskCnt == tasksToSplitInto-1) && currentTask < availableTasks.size();++i)
					{
						if (first) first = false;else outWriter.append(',');
						outWriter.append(Integer.toString(availableTasks.get(currentTask++)));
					}
					outWriter.write("]\n}\n");
				}
				
				outWriter.write("]\n");
			}
			catch(IOException ex)
			{
				Helper.throwUnchecked("failed to record virtToReal.map", ex);
			}
			finally
			{
				if (outWriter != null)
					try {
						outWriter.close();
					} catch (IOException e) {
						// ignore this one
					}
				outWriter = null;
			}
			
			return currentVirtualTask;
		}

		/** The expected structure of the code is to submit a number of tasks then call
		 * {@link RunSubExperiment#collectOutcomeOfExperiments(processSubExperimentResult)}
		 * in order to process results such as plot them or (most likely) store them in relevant files such as CSV.
		 * This can be done a number of times and we'd better remember what was done before. For this reason, every batch
		 * of {@link RunSubExperiment#submitTask(UASExperiment)} calls adds a number of tasks and every call to
		 * {@link RunSubExperiment#collectOutcomeOfExperiments(processSubExperimentResult)} processes the results of those
		 * tasks. Variable <b>taskCounterFromPreviousSubExperiment</b> stores the starting task number and <b>taskCounter</b>
		 * is the total number of tasks submitted so far.
		 *
		 * @param handlerForExperimentResults handler for results
		 */
		public void collectOutcomeOfExperiments(processSubExperimentResult<EXPERIMENT_PARAMETERS,RESULT> handlerForExperimentResults)
		{
			nameToGraph = new TreeMap<>();for(SGEExperimentResult g:handlerForExperimentResults.getGraphs()) nameToGraph.put(g.getFileName(),g);
			if (nameToGraph.size() != handlerForExperimentResults.getGraphs().length)
				throw new IllegalArgumentException("duplicate file names in some graphs");
			if (plotName != null && !nameToGraph.containsKey(plotName))
				throw new IllegalArgumentException("invalid plot \""+plotName+"\" requested for phase "+phase+", only valid ones are "+nameToGraph.keySet());

			try
			{
				switch(phase)
				{
				case RUN_STANDALONE:
					ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+(taskCounter-taskCounterFromPreviousSubExperiment)+" tasks", taskCounter-taskCounterFromPreviousSubExperiment);
					for(int count=taskCounterFromPreviousSubExperiment;count < taskCounter;++count)
					{
						RESULT result = runner.take().get();// this will throw an exception if any of the tasks failed.
						if (result == null)
							throw new IllegalArgumentException("experiment "+count+" did not complete or returned null");
						handlerForExperimentResults.processSubResult(result,this);plotAllGraphs(nameToGraph.values(),count);
						progress.next();
					}
					plotAllGraphs(nameToGraph.values(),-1);
					break;
				case COUNT_TASKS:
				case PROGRESS_INDICATOR:
				case REPORT_TASKPARAMETERS:
					updateAvailableTasks(taskCounterFromPreviousSubExperiment,taskCounter);
					break;
					
				case RUN_TASK: 
				case RUN_PARALLEL:
				{
					Set<Integer> tasksForVirtualTask = virtTaskToRealTask.get(virtTask);
					for(int rCounter=taskCounterFromPreviousSubExperiment;rCounter < taskCounter;++rCounter)
						if (tasksForVirtualTask != null && tasksForVirtualTask.contains(rCounter) && taskletWasRun.contains(rCounter)) // only run a task if we do not have a result,
							// without it it will overwrite a result and execution time and other transient data not stored in the outcome such as true/false counters will be lost.
						{// it is worth noting that the only use of rCounter above is to ensure we do the same number of 'get()' as we scheduled the tasks. Tasks complete in any order making it
							// impossible to expect them to complete in a specific order. This is why the name of the file is constructed based on parameters rather than rCounter.
							outputWriter = new StringWriter();
							RESULT result = null;
							try
							{
								result = runner.take().get();
								BufferedWriter writer = null;
								if (result != null)
								{
									try
									{
										// The call to processSubResult below will record results into the file by
										// adding values to outputWriter via calls to RecordCSV and RecordR with this
										// experiment runner as a parameter.
										handlerForExperimentResults.processSubResult(result,this);// we use StringWriter here in order to avoid creating a file if constructing output fails.
										// At this point outputWriter has all the research data we need and what is left is to record cpu information and CRC value.
										writer = new BufferedWriter(new FileWriter(constructFileName(tmpDir, result.parameters)));
										java.util.zip.CRC32 crc = new java.util.zip.CRC32();
										outputWriter.append(CPUSPEEDFIELD);outputWriter.append(separator);outputWriter.append(getCpuFreq());outputWriter.append('\n');
										String text = outputWriter.toString();updateCRC(crc, text);
										writer.append(text);
										writer.append(CHECKSUMFIELD);writer.append(separator);writer.append(Long.toHexString(crc.getValue()));writer.append('\n');
									}
									finally
									{
										if (writer != null)
										{
											writer.close();writer = null;
										}
									}
									new File(constructTaskStartedFileName(result.parameters)).delete();// remove the file once the task finished. This means that timed out tasks are those with this file still left.
									// start/stop files are not realistic on parallel executions because those are not run on a grid.
								}
							}
							catch(Exception ex)
							{
								ex.printStackTrace();// report failure but continue with other tasks.
							}
							if (throwOnTaskReturningNull && result == null)
								throw new RuntimeException("null returned by task, most likely due to exception");
						}
					break;
				}
				case COLLECT_RESULTS:
				case COLLECT_AVAILABLE:
					progress = new ProgressIndicator(new Date()+" collecting results", taskCounter-taskCounterFromPreviousSubExperiment);
					for(int rCounter=taskCounterFromPreviousSubExperiment;rCounter < taskCounter;++rCounter)
					{
						if (phase == PhaseEnum.COLLECT_RESULTS || checkExperimentComplete(rCounter)) // tasks are loaded if either we request all of them to be loaded or they are complete for COLLECT_AVAILABLE
							loadExperimentResult(rCounter);
						
						progress.next();
					}
					// if we got here without bailing out due to an exception, handling of the output has been successful, plot graphs.
					if (plotName == null)
						plotAllGraphs(nameToGraph.values(),-1);
					else
						nameToGraph.get(plotName).reportResults(gr);
					break;
				default:
					break;
				}
			}
			catch(Exception e)
			{
				shutdown();Helper.throwUnchecked("collecting results failed: "+e.getMessage(), e);
			}

			taskCounterFromPreviousSubExperiment = taskCounter;			
		}
		
		/**
		 * Stores argument values into a file. Does not use serialisation in order to make those arguments readable for a human.
		 * 
		 * @param graph graph to which the x-y value relates to
		 * @param x position of the dot along the X axis
		 * @param y position of the dot along the Y axis
		 * @param colour colour to use, <i>null</i> is a valid value.
		 * @param label label on the axis, <i>null</i> if not used.
		 */
		@SuppressWarnings({ "rawtypes", "unchecked" })
		public void RecordR(RExperimentResult graph, Comparable x, Double y, String colour, String label) throws IOException
		{
			if (graph.getFileName().split(separatorRegEx).length > 1)
				throw new IllegalArgumentException("invalid file name "+graph.getFileName()+" in graph");
			if (!nameToGraph.containsKey(graph.getFileName()))
				throw new IllegalArgumentException("unknown graph "+graph.getFileName());
			
			switch(phase)
			{
			case RUN_TASK:
			case RUN_PARALLEL:
				graph.writeTaskOutput(outputWriter,x,y,colour,label);
				break;
			case COUNT_TASKS:
			case PROGRESS_INDICATOR:
			case REPORT_TASKPARAMETERS:
				break;
			case COLLECT_RESULTS:
			case COLLECT_AVAILABLE:
				throw new IllegalArgumentException("this should not be called during phase "+phase);
			case RUN_STANDALONE:			
				graph.add(x,y,colour,label);			
				break;
			}
		}
		
		public void RecordCSV(CSVExperimentResult experimentResult, ThreadResultID id, String text) throws IOException
		{
			if (experimentResult.getFileName().split(separatorRegEx).length > 1)
				throw new IllegalArgumentException("invalid file name "+experimentResult.getFileName()+" in spreadsheet");
			if (!nameToGraph.containsKey(experimentResult.getFileName()))
				throw new IllegalArgumentException("unknown graph "+experimentResult.getFileName());
			if (text.contains("\n"))
				throw new IllegalArgumentException("lines of text should not contain newlines, append multiple lines instead");
			switch(phase)
			{
			case RUN_TASK:
			case RUN_PARALLEL:
				experimentResult.writeTaskOutput(outputWriter,id,text);
				break;
			case COUNT_TASKS:
			case PROGRESS_INDICATOR:
			case REPORT_TASKPARAMETERS:
				break;
			case COLLECT_RESULTS:
			case COLLECT_AVAILABLE:
				throw new IllegalArgumentException("this should not be called during phase "+phase);
			case RUN_STANDALONE:			
				experimentResult.add(id,text);			
				break;
			}		
		}
		
	} // class RunSubExperiment

	/** Given a reader for a file with corrections and the text of the CPU ID, loads the file and returns the global time correction. 
	 * This is not using .properties file format to ensure we can handle unusual characters.
	 *  
	 * @throws IOException throw if something went wrong. 
	 */  
	public static String getCorrection(BufferedReader reader,String cpuStringArg) throws IOException
	{
		String cpuString = LearningSupportRoutines.removeSpaces(cpuStringArg);
		String line = null;
		while((line=reader.readLine()) != null)
		{
			String[] elems =line.split(separatorRegEx);
			if (elems.length != 2)
				throw new IllegalArgumentException("invalid file format, line \""+line+"\"should have two components");
			if (cpuString.equals(LearningSupportRoutines.removeSpaces(elems[0])))
				return LearningSupportRoutines.removeSpaces(elems[1]);
		}
		System.err.println("WARNING: no correction for CPU string "+cpuStringArg);
		return null;
	}
	static final String cpuName = "model name";
	
	public static String getCpuFreqValue(BufferedReader cpuinfoReader) throws IOException
	{
		String line = null;
		while((line=cpuinfoReader.readLine()) != null)
		{
			if (line.startsWith(cpuName))
				return LearningSupportRoutines.removeColonsAndSpaces(line.substring(cpuName.length()));
		}
		return null;
	}
	
	public static String getCpuFreqValue()
	{
		String result = null;
		BufferedReader rd = null;
		try
		{
			rd = new BufferedReader(new FileReader("/proc/cpuinfo"));// BufferedReader will accept null and will close its parent reader when closed.
			result = getCpuFreqValue(rd);
		}
		catch(IOException ex)
		{// ignore this - null will be returned.			
		}
		finally
		{
			if (rd != null)
			{
				try {
					rd.close();
				} catch (IOException e) {
					// ignore this
				}rd = null;
			}
		}
		return result;
	}
	
	public static final String executionTimeProperties = "executionTimeScale.map";
	
	public static String getCpuFreq()
	{
		String outcome = getCpuFreqValue();
		if (outcome == null)
			outcome = getHostName();
		if (outcome == null)
			throw new IllegalArgumentException("failure to obtain a value of argument to look up CPU speed normalisation value");
		return outcome;
	}
	
	public static String getHostName()
	{
		String outcome = null;
		try
		{
			outcome = java.net.InetAddress.getLocalHost().getHostName();
		}
		catch(Exception ex)
		{// ignore this, arg will remain null.
		}
		
		if (outcome == null)
		{// This part is based on http://stackoverflow.com/questions/7348711/recommended-way-to-get-hostname-in-java thread, answer by Malt
	        String OS = System.getProperty("os.name").toLowerCase();

	        if (OS.contains("win"))
	        	outcome = System.getenv("COMPUTERNAME");
	        else
	            if (OS.contains("nix") || OS.contains("nux"))
	            	outcome = System.getenv("HOSTNAME");
		}		
		return outcome;
	}
	
	/** Extracts the CPU speed correction value and assigns the SGE_EXECUTIONTIME_SCALE value. This is determined in two different ways,
	 * <ul>
	 * <li>By reading values from /proc/cpuinfo and looking up the correction in the <pre>executionTimeScale.map</pre> or <pre>iceberg/executionTimeScale.map</pre> file.
	 * This will work well on Linux boxes including Iceberg or any other grid engine.
	 * </li>
	 * <li>If the above fails, it obtain host name and looks up the correction in the <pre>executionTimeScale.map</pre> or <pre>iceberg/executionTimeScale.map</pre> file. Intended for Windows PCs or Macs.</li>
	 * </ul> 
	 */
	public static void configureCPUFreqNormalisation()
	{
		String argToLookUp = getCpuFreq();
		
		BufferedReader propertiesFile = null;
				
		try
		{
			List<String> candidatePaths = new LinkedList<>();
			for(String p:new String[]{executionTimeProperties, "iceberg"+File.separator+executionTimeProperties}) {
				candidatePaths.add(p);candidatePaths.add(".."+File.separator+p);
			}
			for(String p:candidatePaths)
			if (new File(p).canRead())
			{
				propertiesFile = new BufferedReader(new FileReader(p));break;
			}

			if (propertiesFile == null)
				throw new IllegalArgumentException("Unable to read CPU freq normalisation properties");

			String outcome = getCorrection(propertiesFile,argToLookUp);
			if (outcome != null)
				GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.SGE_EXECUTIONTIME_SCALING, outcome);
		}
		catch(IOException ex)
		{
			Helper.throwUnchecked("Failed to load scaling value", ex);
		}
		finally
		{
			if (propertiesFile != null)
			{
				try {
					propertiesFile.close();
				} catch (IOException e) {
					// ignored - we can do little if close fails.
				}
				propertiesFile = null;
			}
		}
	}
	
}
