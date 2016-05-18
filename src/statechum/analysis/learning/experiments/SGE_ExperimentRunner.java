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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
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

import statechum.Helper;
import statechum.ProgressIndicator;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.RExperimentResult;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
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
	
	
	public static final String passCollectTasks="collectTasks", passRunTask="passRunTask", passCollateResults="passCollate", passStandalone = "passStandalone";
	
	public interface processSubExperimentResult<RESULT> 
	{
		/** Called to plot results of the experiment, using the result <i>r</i>. The <i>experimentrunner</i> is what is to be used to perform plotting, via 
		 * calls to {@link runSubExperiment#Record(String, Object, Double, String)}. The outcome of these calls are stored in a file and subsequently assembled and plotted.
		 * 
		 * @param result the outcome of running an experiment.
		 * @param runSubExperiment
		 */
		public void processSubResult(RESULT result,RunSubExperiment<RESULT> runSubExperiment)  throws IOException;
		
		/** Returns all graphs that will be plotted. This is needed because we would rather not store axis names in text files. */
		public SGEExperimentResult[] getGraphs();
		
		/** Returns the name of the current experiment. */
		public String getSubExperimentName();
	}
	
	
	public enum PhaseEnum
	{
		RUN_TASK, COLLECT_RESULTS, COUNT_TASKS, RUN_STANDALONE, 
		RUN_PARALLEL // this is similar to RUN_TASK but will run all tasks corresponding to the same virtual task in parallel. This permits multiple PCs to easily run different segments of work across their CPUs.
	}
	
	public static class RunSubExperiment<RESULT> 
	{
		private PhaseEnum phase;
		/** We need both taskCounterFromPreviousSubExperiment and taskCounter in order to run multiple series of experiments, where a number of submitTask calls are followed with the same number of processResults. */  
		private int taskCounter=0, taskCounterFromPreviousSubExperiment;
		/** Virtual task to run, each virtual corresponds to a set of actual tasks that have not finished. */
		private int virtTask = 0, tasksToSplitInto =0;
		private String experimentName;
		private ExecutorService executorService;
		
		protected CompletionService<RESULT> runner = null; 
		
		protected Map<Integer,RESULT> outcomeOfExperiment = new TreeMap<Integer,RESULT>();
		private DrawGraphs gr = new DrawGraphs();

		private final String tmpDir;
		
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
						taskValue = Integer.valueOf(args[1]);
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
					if (args.length != 1)
						throw new IllegalArgumentException("no arguments is permitted for phase "+phase);
					break;
				case COUNT_TASKS:
					if (args.length != 2)
						throw new IllegalArgumentException("the number of tasks per virtual task has to be provided");
					try
					{
						tasksToSplitInto = Integer.valueOf(args[1]);
					}
					catch(NumberFormatException e)
					{
						Helper.throwUnchecked("invalid number", e);
					}
					if (tasksToSplitInto <= 0)
						throw new IllegalArgumentException("the number of real tasks to run should be positive");
					break;
				case COLLECT_RESULTS:
					if (args.length != 1)
						throw new IllegalArgumentException("no arguments is permitted for phase "+phase);
					break;
				}
			}
			executorService = Executors.newFixedThreadPool(cpuNumber);runner = new ExecutorCompletionService<RESULT>(executorService);
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
			if (phase == PhaseEnum.COUNT_TASKS)
			{
				outcome = constructVirtToReal();				
				System.out.println(outcome);
			}
			shutdown();
			return outcome;
		}

		@SuppressWarnings({ "unchecked", "rawtypes" })
		public void submitTask(Callable<? extends RESULT> task)
		{
			switch(phase)
			{
			case RUN_TASK:// when running in Grid mode, each task runs as a separate process given that we intend to run them on separate nodes.
			{
				Set<Integer> tasksForVirtualTask = virtTaskToRealTask.get(virtTask);
				if (tasksForVirtualTask != null && tasksForVirtualTask.contains(taskCounter))
					try
					{
						outcomeOfExperiment.put(taskCounter,task.call());// this one asks the handler to record the results of the experiment in a form that can subsequently be passed to R.
					}
					catch(Exception ex)
					{
						Helper.throwUnchecked("running task failed: "+ex.getMessage(), ex);
					}
					finally
					{
						shutdown();
					}
				break;
			}	
			case RUN_PARALLEL:
			{
				Set<Integer> tasksForVirtualTask = virtTaskToRealTask.get(virtTask);
				if (tasksForVirtualTask != null && tasksForVirtualTask.contains(taskCounter))
					runner.submit((Callable)task);
				break;
			}
			case RUN_STANDALONE:
				runner.submit((Callable)task);
				break;
			case COUNT_TASKS:
				break;
			case COLLECT_RESULTS:
				break;

			}
			++taskCounter;
		}

		/** Returns the number of the current task. The returned value is only updated on a call to {@link RunSubExperiment#submitTask(Callable)}. */
		public int getTaskID()
		{
			return taskCounter;
		}

		protected String constructFileName(int rCounter)
		{
			return tmpDir+experimentName.replaceAll("[:\\// ]", "_")+"-"+rCounter;				
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
		
		/** Returns true if this is run all in the same jvm, permitting one to collect statistics during collection of results and output it via println. 
		 * This is not possible where run on a grid because collection of results is pulling a subset of data from the disk.
		 * @return
		 */
		public boolean isInteractive()
		{
			return phase == PhaseEnum.RUN_STANDALONE;
		}
		
		Map<String,SGEExperimentResult> nameToGraph = null;
		StringWriter outputWriter = null;

		private void loadExperimentResult(int rCounter)
		{
			BufferedReader reader = null;
			try
			{
				reader = new BufferedReader(new FileReader(constructFileName(rCounter)));
				String line = reader.readLine();
				while(line != null)
				{
					String [] data = line.split(separatorRegEx,-2);
					if (data.length < 1)
						throw new IllegalArgumentException("Experiment in "+constructFileName(rCounter)+" did not log any result");
					String name = data[0];
					if (!nameToGraph.containsKey(name))
						throw new IllegalArgumentException("Experiment in "+constructFileName(rCounter)+" refers to an unknown graph "+name);
					
					SGEExperimentResult thisPlot = nameToGraph.get(name);
					
					thisPlot.parseTextLoadedFromExperimentResult(data, constructFileName(rCounter), false);
					line = reader.readLine();
				}
	
				// if we got here, handling of the output has been successful, plot graphs.
				plotAllGraphs(nameToGraph.values(),-1);
			}
			catch(IOException ex)
			{
				Helper.throwUnchecked("failed to load results of experiment "+rCounter, ex);
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
		
		private Map<Integer,Set<Integer>> virtTaskToRealTask = null;
		
		public static Map<Integer,Set<Integer>> loadVirtTaskToReal(String tmpDir)
		{
			BufferedReader reader = null;Map<Integer,Set<Integer>> virtTaskToRealTask = new TreeMap<Integer,Set<Integer>>();
			try
			{
				reader = new BufferedReader(new FileReader(tmpDir+"virtToReal.map"));
				StringBuffer text = new StringBuffer();
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
					Set<Integer> outcome = new TreeSet<Integer>();
					for(OtpErlangObject taskNumberObj:listOfTasks)
					{
						if (!(taskNumberObj instanceof OtpErlangInt))
							throw new IllegalArgumentException("loading virtTaskToRealTask: expected an int among tasks, got "+taskNumberObj);
						outcome.add(((OtpErlangInt)taskNumberObj).intValue());
					}
					virtTaskToRealTask.put(virtTaskID,outcome);
				}
			}
			catch(IOException ex)
			{
				Helper.throwUnchecked("failed to load virtToReal.map", ex);
			}
			catch(OtpErlangRangeException ex)
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
				availableTasks=new ArrayList<Integer>();
			for(int task=from;task < to;++task)
			{
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
				outWriter = new BufferedWriter(new FileWriter(tmpDir+"virtToReal.map"));outWriter.write("[\n");
				boolean firstTuple = true;
				for(int vTaskCnt=0;vTaskCnt<tasksToSplitInto && currentTask < availableTasks.size();++vTaskCnt)
				{
					if (firstTuple) firstTuple = false;else outWriter.write("\n,\n");
					outWriter.write('{');outWriter.write(Integer.toString(++currentVirtualTask));outWriter.write(",\n\t[");
					boolean first=true;
					for(int i=0;(i<tasksPerVirtualTask || vTaskCnt == tasksToSplitInto-1) && currentTask < availableTasks.size();++i)
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
		
		private boolean checkExperimentComplete(int rCounter)
		{
			boolean outcome = true;
			BufferedReader reader = null;
			try
			{
				reader = new BufferedReader(new FileReader(constructFileName(rCounter)));
				String line = reader.readLine();
				while(line != null)
				{
					String [] data = line.split(separatorRegEx,-2);
					if (data.length < 1)
					{
						outcome = false;
						break;
					}
					String name = data[0];
					if (!nameToGraph.containsKey(name))
					{
						outcome = false;
						break;
					}
					
					SGEExperimentResult thisPlot = nameToGraph.get(name);
					
					thisPlot.parseTextLoadedFromExperimentResult(data, constructFileName(rCounter), true);
					line = reader.readLine();
				}
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
		
		public void collectOutcomeOfExperiments(processSubExperimentResult<RESULT> handlerForExperimentResults)
		{
			nameToGraph = new TreeMap<String,SGEExperimentResult>();for(SGEExperimentResult g:handlerForExperimentResults.getGraphs()) nameToGraph.put(g.getFileName(),g);
			if (nameToGraph.size() != handlerForExperimentResults.getGraphs().length)
				throw new IllegalArgumentException("duplicate file names in some graphs");

			experimentName = handlerForExperimentResults.getSubExperimentName();
			try
			{
				switch(phase)
				{
				case RUN_STANDALONE:
					ProgressIndicator progress = new ProgressIndicator(new Date()+" evaluating "+(taskCounter-taskCounterFromPreviousSubExperiment)+" tasks for "+experimentName, taskCounter-taskCounterFromPreviousSubExperiment);
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
					updateAvailableTasks(taskCounterFromPreviousSubExperiment,taskCounter);
					break;
					
				case RUN_TASK: 
					// We run only one task of the many that might be submitted, here we need to do a single "collect" of the many that might be submitted 
					// (which actually means 'write execution results constructed by processSubResult handler to disk'). 
					// The condition below chooses a call to processSubResult that corresponds the executed sequence of commands (there could be many 
					// sequences of 'submit' with each sequence followed by 'collect'). In order to choose 'collect' for the right 'submit' sequence, 
					// we use the one matching the requested number of a taskToRun.  
					// Since taskCounter is incremented after running each task, the value of taskCounter corresponds to a total number of executed tasks 
					// in the earlier sequence of tasks plus one.
					for(Entry<Integer,RESULT> resultOfRunningTasks:outcomeOfExperiment.entrySet())
					{
						if (resultOfRunningTasks.getKey() < taskCounterFromPreviousSubExperiment || resultOfRunningTasks.getKey() >= taskCounter)
							throw new IllegalArgumentException("task "+resultOfRunningTasks.getKey()+" was run when it was not expected, expected range ["+taskCounterFromPreviousSubExperiment+".."+(taskCounter-1)+"]");

						outputWriter = new StringWriter();
						handlerForExperimentResults.processSubResult(resultOfRunningTasks.getValue(),this);// we use StringWriter here in order to avoid creating a file if constructing output fails.
						BufferedWriter writer = null;
						try
						{
							writer = new BufferedWriter(new FileWriter(constructFileName(resultOfRunningTasks.getKey())));
							writer.append(outputWriter.toString());
						}
						finally
						{
							if (writer != null)
							{
								writer.close();writer = null;
							}
						}
					}
					outcomeOfExperiment.clear();
					break;
				case RUN_PARALLEL:
				{
					assert outcomeOfExperiment.isEmpty();						
					Set<Integer> tasksForVirtualTask = virtTaskToRealTask.get(virtTask);
					for(int rCounter=taskCounterFromPreviousSubExperiment;rCounter < taskCounter;++rCounter)
						if (tasksForVirtualTask != null && tasksForVirtualTask.contains(rCounter))
						{
							outputWriter = new StringWriter();
							RESULT result = runner.take().get();
							handlerForExperimentResults.processSubResult(result,this);// we use StringWriter here in order to avoid creating a file if constructing output fails.
							BufferedWriter writer = null;
							try
							{
								writer = new BufferedWriter(new FileWriter(constructFileName(rCounter)));
								writer.append(outputWriter.toString());
							}
							finally
							{
								if (writer != null)
								{
									writer.close();writer = null;
								}
							}
						}
					break;
				}
				case COLLECT_RESULTS:
					for(int rCounter=taskCounterFromPreviousSubExperiment;rCounter < taskCounter;++rCounter)
						loadExperimentResult(rCounter);
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
		 * @throws IOException 
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
				break;
			case COLLECT_RESULTS:
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
				break;
			case COLLECT_RESULTS:
				throw new IllegalArgumentException("this should not be called during phase "+phase);
			case RUN_STANDALONE:			
				experimentResult.add(id,text);			
				break;
			}		
		}
	}

}
