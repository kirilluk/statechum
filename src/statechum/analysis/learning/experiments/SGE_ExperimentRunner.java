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
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import statechum.Helper;
import statechum.ProgressIndicator;
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
		RUN_TASK, COLLECT_RESULTS, COUNT_TASKS, RUN_STANDALONE
	}
	
	public static class RunSubExperiment<RESULT> 
	{
		private PhaseEnum phase;
		// We need both taskCounterFromPreviousSubExperiment and taskCounter in order to run multiple series of experiments, where a number of submitTask calls are followed with the same number of processResults.  
		private int taskCounter=0, taskCounterFromPreviousSubExperiment, taskToRun=-1;
		private String experimentName;
		private ExecutorService executorService;
		
		protected CompletionService<RESULT> runner = null; 
		
		protected RESULT outcomeOfExperiment = null;
		private DrawGraphs gr = new DrawGraphs();

		private final String tmpDir;
		
		public RunSubExperiment(int cpuNumber,String dir, String []args)
		{
			tmpDir = dir+"/";
			if (args.length == 0)
				// no args means standalone
				phase = PhaseEnum.RUN_STANDALONE;
			else
			{
				phase = PhaseEnum.valueOf(args[0]);
				switch(phase)
				{
				case RUN_TASK:
					if (args.length != 2)
						throw new IllegalArgumentException("task number should be provided");
					int taskValue = Integer.valueOf(args[1]);
					if (taskValue <= 0)
						throw new IllegalArgumentException("task number should be positive");
					taskToRun = taskValue-1;
					break;
				case RUN_STANDALONE:
					if (args.length != 1)
						throw new IllegalArgumentException("no arguments is permitted for phase "+phase);
					break;
				case COUNT_TASKS:
					if (args.length != 1)
						throw new IllegalArgumentException("no arguments is permitted for phase "+phase);
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
		}
		
		public int successfulTermination()
		{
			int outcome = 0;
			if (phase == PhaseEnum.COUNT_TASKS)
			{
				System.out.println(taskCounter);outcome = taskCounter;
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
				if (taskToRun == taskCounter)
					try
					{
						outcomeOfExperiment = task.call();// this one asks the handler to record the results of the experiment in a form that can subsequently be passed to R.
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
				
			case RUN_STANDALONE:// only submit the task of interest
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
		BufferedWriter outputWriter = null;

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
							throw new IllegalArgumentException("experiment "+taskToRun+" did not complete or returned null");
						handlerForExperimentResults.processSubResult(result,this);plotAllGraphs(nameToGraph.values(),count);
						progress.next();
					}
					plotAllGraphs(nameToGraph.values(),-1);
					break;
				case COUNT_TASKS:
					break;
					
				case RUN_TASK: // we run only one task of the many that might be submitted, here we need to do one collect of the many that might be submitted, the condition below chooses the one matching taskToRun.
					if (taskToRun >= taskCounterFromPreviousSubExperiment && taskToRun < taskCounter) // we increment taskCounter after running each task, hence this one corresponds to a task plus one.
					{
						if (outcomeOfExperiment == null)
							throw new IllegalArgumentException("experiment "+taskToRun+" did not complete or returned null");
						outputWriter = new BufferedWriter(new FileWriter(constructFileName(taskToRun)));
						try
						{
							handlerForExperimentResults.processSubResult(outcomeOfExperiment,this);
						}
						finally
						{
							outputWriter.close();outputWriter = null;
						}
					}
					break;
				case COLLECT_RESULTS:
					for(int rCounter=taskCounterFromPreviousSubExperiment;rCounter < taskCounter;++rCounter)
					{
						BufferedReader reader = new BufferedReader(new FileReader(constructFileName(rCounter)));
						try
						{
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
								
								thisPlot.parseTextLoadedFromExperimentResult(data, constructFileName(rCounter));
								line = reader.readLine();
							}
							
							// if we got here, handling of the output has been successful, plot graphs.
							plotAllGraphs(nameToGraph.values(),-1);
						}
						finally
						{
							reader.close();
						}
					}
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
