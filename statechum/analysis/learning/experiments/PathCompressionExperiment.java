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
import java.io.FileNotFoundException;
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
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.ComputeStateScores;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.ComputeStateScores.IDMode;
import statechum.xmachine.model.testset.*;
import static statechum.analysis.learning.TestFSMAlgo.buildSet;
import static statechum.xmachine.model.testset.WMethod.getGraphData;
import static statechum.xmachine.model.testset.WMethod.tracePath;

public class PathCompressionExperiment {

	private final ExecutorService executorService;

	public PathCompressionExperiment(String outputD) {
		int ThreadNumber = 1; // the default for single-cpu systems.
		String cpuNum = System.getProperty("threadnum");
		if (cpuNum != null) {
			int parsedNumber = -1;
			try {
				parsedNumber = Integer.parseInt(cpuNum);
			} catch (NumberFormatException ex) {
				parsedNumber = -1;
			}
			if (parsedNumber > 0 && parsedNumber < 31)
				ThreadNumber = parsedNumber;
		}

		results = new LinkedList<Future<String>>();
		executorService = Executors.newFixedThreadPool(ThreadNumber);
		runner = new ExecutorCompletionService<String>(executorService);
		outputDir = outputD;
	}

	public abstract static class LearnerEvaluatorGenerator {
		abstract LearnerEvaluator getLearnerEvaluator(String inputFile,
				String ouputDir, int percent, int instanceID);
	}

	public abstract static class LearnerEvaluator implements Callable<String> {
		protected Collection<List<String>> sPlus = null;

		protected DirectedSparseGraph graph = null;

		protected String inputFileName = null, outputDir = null;

		protected int percent;

		protected final int instanceID;

		public LearnerEvaluator(String inputFile, String outputD, int per,
				int inID) {
			inputFileName = inputFile;
			outputDir = outputD;
			percent = per;
			instanceID = inID;
		}

		protected void loadGraph() {
			synchronized (ComputeStateScores.syncObj) {// ensure that the calls
														// to Jung's
														// vertex-creation
														// routines do not occur
														// on different threads.
				GraphMLFile graphmlFile = new GraphMLFile();
				graphmlFile
						.setGraphMLFileHandler(new ExperimentGraphMLHandler());
				graph = new DirectedSparseGraph();
				graph.getEdgeConstraints().clear();
				graph = (DirectedSparseGraph) graphmlFile.load(inputFileName);
			}
		}

		enum OUTCOME {
			SUCCESS, FAILURE
		};

		protected String FS = ",";

		/**
		 * Write the provided string into the result file.
		 * 
		 * @param result
		 *            what to write
		 * @return null on success and an error message on failure
		 */
		protected String writeResult(OUTCOME outcome, String result) {
			Writer outputWriter = null;
			String stdOutput = null;
			try {
				outputWriter = new BufferedWriter(new FileWriter(
						getFileName(FileType.RESULT)));
				outputWriter.write(inputFileName + FS + percent + FS + outcome
						+ (result == null ? "" : FS + result) + "\n");
			} catch (IOException e) {
				StringWriter writer = new StringWriter();
				e.printStackTrace();
				e.printStackTrace(new PrintWriter(writer));
				stdOutput = "\nFAILED TO WRITE A REPORT :" + writer.toString();
			} finally {
				try {
					if (outputWriter != null)
						outputWriter.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			return stdOutput;
		}

		protected void buildSets() {
			loadGraph();
			int size = graph.getEdges().size() * 4;
			RandomPathGenerator rpg = new RandomPathGenerator(graph,
					new Random(100), size, 3);// the seed for Random should be
												// the same for each file
			Set<List<String>> currentSamples = new LinkedHashSet<List<String>>();
			currentSamples = addPercentageFromSamples(currentSamples, rpg
					.getAllPaths(), percent);
			sPlus = getPositiveStrings(graph, currentSamples);
		}

		public static Collection<List<String>> plus = null;

		public enum FileType {
			DATA {
				String getFileName(String prefix, String suffix) {
					return prefix + "_data" + suffix + ".xml";
				}
			},
			RESULT {
				String getFileName(String prefix, String suffix) {
					return prefix + "_result" + suffix + ".txt";
				}
			};

			abstract String getFileName(String prefix, String suffix);
		};

		protected String getFileName(FileType fileNameType) {
			return fileNameType.getFileName(outputDir
					+ System.getProperty("file.separator") + instanceID + "_"
					+ (new File(inputFileName).getName()), "-" + percent);
		}
	}

	/** This one is not static because it refers to the frame to display results. */
	public static abstract class RPNIEvaluator extends LearnerEvaluator {
		public RPNIEvaluator(String inputFile, String outputDir, int per,
				int instanceID) {
			super(inputFile, outputDir, per, instanceID);

		}

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParametersOnComputeStateScores(
				ComputeStateScores c);

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParametersOnLearner(
				RPNIBlueFringeLearner l);

		protected static int stringCollectionSize(
				Collection<List<String>> strings) {
			int size = 0;
			for (List<String> list : strings) {
				size = size + list.size();
			}
			return size;
		}

		public String call() {
			// System.out.println(inputFileName+" (instance "+instanceID+"),
			// learner "+this+", "+percent + "% started at
			// "+Calendar.getInstance().getTime());
			OUTCOME currentOutcome = OUTCOME.FAILURE;
			String stdOutput = writeResult(currentOutcome, null);// record
																	// the
																	// failure
																	// result in
																	// case
																	// something
																	// fails
																	// later and
																	// we fail
																	// to update
																	// the file,
																	// such as
																	// if we are
																	// killed or
																	// run out
																	// of memory
			if (stdOutput != null)
				return stdOutput;

			buildSets();
			int uncompressed = stringCollectionSize(sPlus);

			String stats = instanceID + "," + sPlus.size() + "," + uncompressed;
			try {
				PTASequenceSet plusPTA = new PTASequenceSet();
				plusPTA.addAll(sPlus);
				int compressed = plusPTA.treeSize();
				float compression = 100 - ((new Float(compressed) / new Float(
						uncompressed)) * 100);
				// stats = stats + ","+compressed+","+compression;
				stats = compression + ",";
				currentOutcome = OUTCOME.SUCCESS;
				if (this.percent == 10)
					System.out.println();
				System.out.print(stats);
			} catch (Throwable th) {
				StringWriter writer = new StringWriter();
				th.printStackTrace();
				th.printStackTrace(new PrintWriter(writer));
				stats = stats + "\nFAILED\nSTACK: " + writer.toString();
			}

			// now record the result
			stdOutput = writeResult(currentOutcome, stats);
			if (stdOutput != null)
				return stdOutput;
			return inputFileName + FS + percent + FS + currentOutcome;
		}
	}

	/** Stores results of execution of evaluators. */
	final List<Future<String>> results;

	/** Stores tasks to complete. */
	final CompletionService<String> runner;

	public static Set<List<String>> addPercentageFromSamples(
			Set<List<String>> current, Collection<List<String>> samples,
			double percent) {
		double size = samples.size();
		double number = size * percent / 100;
		List<String>[] sampleArray = (List<String>[]) samples
				.toArray(new List[samples.size()]);
		for (int i = 0; i < (int) number; i++) {
			current.add(sampleArray[i]);
		}
		return current;
	}

	public static Set<List<String>> trimToNegatives(DirectedSparseGraph g,
			Collection<List<String>> sMinus) {
		Set<List<String>> returnSet = new HashSet<List<String>>();
		Iterator<List<String>> sMinusIt = sMinus.iterator();
		while (sMinusIt.hasNext()) {
			List<String> currentString = sMinusIt.next();
			final FSMStructure expected = getGraphData(g);
			int reject = tracePath(expected, currentString);
			returnSet.add(currentString.subList(0, reject + 1));
		}
		return returnSet;
	}

	public static Collection<List<String>> getPositiveStrings(
			DirectedSparseGraph graph, Collection<List<String>> samples) {
		Iterator<List<String>> sampleIt = samples.iterator();
		HashSet<List<String>> positiveStrings = new HashSet<List<String>>();
		while (sampleIt.hasNext()) {
			List<String> v = sampleIt.next();
			if (RPNIBlueFringeLearner.getVertex(graph, v) != null)
				positiveStrings.add(v);
		}
		return positiveStrings;
	}

	public static Collection<List<String>> randomHalf(
			Collection<List<String>> v, Random halfRandomNumberGenerator) {
		Object[] samples = v.toArray();
		List<List<String>> returnSet = new LinkedList<List<String>>();
		Set<Integer> done = new HashSet<Integer>();
		for (int i = 0; i < v.size() / 2; i++) {
			int randomIndex = 0;
			boolean newInteger = false;
			while (!newInteger) {
				randomIndex = halfRandomNumberGenerator.nextInt(v.size());
				Integer current = new Integer(randomIndex);
				if (!done.contains(current)) {
					done.add(current);
					newInteger = true;
				}
			}
			returnSet.add((List<String>) samples[randomIndex]);
		}
		return returnSet;
	}

	public static final LearnerEvaluatorGenerator[] learnerGenerators = { new LearnerEvaluatorGenerator() {
		@Override
		LearnerEvaluator getLearnerEvaluator(String inputFile,
				String outputDir, int percent, int instanceID) {
			return new RPNIEvaluator(inputFile, outputDir, percent, instanceID) {
				@Override
				protected void changeParametersOnLearner(RPNIBlueFringeLearner l) {
				}

				@Override
				protected void changeParametersOnComputeStateScores(
						ComputeStateScores c) {
					c.setMode(IDMode.POSITIVE_NEGATIVE);
				}

				@Override
				public String toString() {
					return "RPNI, POSITIVE_NEGATIVE";
				}
			};
		}
	}
	// at this point, one may add the above learners with different arguments or
	// completely different learners such as the Angluin's one
	};

	public static final int stageNumber = 10;

	protected ArrayList<String> fileName = new ArrayList<String>(100);

	/**
	 * Given a name containing a file with file names, this one adds names of
	 * those which can be read, to the list of them.
	 * 
	 * @param inputFiles
	 *            the name of a file containing a list of files to load.
	 */
	private void loadFileNames(Reader fileNameListReader) {
		try {
			BufferedReader reader = new BufferedReader(fileNameListReader);// new
																			// FileReader(inputFiles));
			String line = reader.readLine();
			while (line != null) {
				line = line.trim();
				if (line.length() > 0 && !line.startsWith("#")
						&& new File(line).canRead())
					fileName.add(line);

				line = reader.readLine();
			}
		} catch (IOException e) {
			throw new IllegalArgumentException(
					"failed to read the list of files");
		}
		if (fileName.isEmpty())
			throw new IllegalArgumentException("no usable files found");
	}

	/**
	 * The set of possible numbers (non-negative) is divided into sets for each
	 * learner, and then into a number of percent divisions.
	 * 
	 * @param inputFile
	 *            the input file
	 * @param Number
	 *            the parameter of the array task. negative means "return the
	 *            highest positive number which can be passed"
	 * @return what Java process should return
	 */
	public int processDataSet(Reader fileNameListReader, int Number) {
		if (fileName.isEmpty())
			loadFileNames(fileNameListReader);
		final int LearnerNumber = learnerGenerators.length;
		final int NumberMax = fileName.size() * stageNumber * LearnerNumber;
		if (Number < 0 || Number >= NumberMax)
			throw new IllegalArgumentException("Array task number " + Number
					+ " is out of range, it should be between 0 and "
					+ NumberMax);
		else {// the number is valid.
			int learnerStep = fileName.size() * stageNumber;
			int learnerType = Number / learnerStep;
			int fileNumber = (Number % learnerStep) / stageNumber;
			int percentStage = (Number % learnerStep) % stageNumber;
			results.add(runner.submit(learnerGenerators[learnerType]
					.getLearnerEvaluator(fileName.get(fileNumber), outputDir,
							100 * (1 + percentStage) / stageNumber, Number)));
			return 0;
		}
	}

	public int computeMaxNumber(Reader fileNameListReader) {
		int NumberMax = 0;
		try {
			loadFileNames(fileNameListReader);
			final int LearnerNumber = learnerGenerators.length;
			NumberMax = fileName.size() * stageNumber * LearnerNumber;
		} catch (Exception e) {// ignore the exception - NumberMax will remain
								// at 0
		}
		return NumberMax;
	}

	protected final String outputDir;

	/**
	 * For dual-core operation, VM args should be -ea -Xmx1600m -Xms300m
	 * -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 Quad-core would use
	 * -Dthreadnum=4 instead.
	 * 
	 * There are multiple modes of operation, process graphs, generate data for
	 * processing and dump sets to files, load data from files and process them,
	 * using a user-chosen learner.
	 * 
	 * @param args
	 *            command-line arguments, a directory name to process all files
	 *            inside it or <FILENAMES_FILE> <OUTPUT_DIR> <NUMBER> where
	 *            FILENAMES_FILE contains files to process, OUTPUT_DIR is where
	 *            to store output and NUMBER is the task number
	 */
	public static void main(String[] args) {
		if (100 % stageNumber != 0)
			throw new IllegalArgumentException("wrong compiled-in stageNumber="
					+ stageNumber + ": it should be a divisor of 100");
		PathCompressionExperiment experiment = null;

		if (args.length < 2) {
			File graphDir = new File(args[0]);
			// "C:\\experiment\\graphs-150\\Neil-Data2\\50-6");
			// "D:\\experiment\\Neil-Data2\\50-6");
			// System.getProperty("user.dir")+System.getProperty("file.separator")+"resources"+
			// System.getProperty("file.separator")+"TestGraphs"+System.getProperty("file.separator")
			// +"50-6");
			String[] graphFileList = graphDir.list();
			String listOfFileNames = "";
			int fileNumber = 0;
			String wholePath = graphDir.getAbsolutePath()
					+ System.getProperty("file.separator");
			for (int i = 0; i < graphFileList.length; i++)
				if (graphFileList[i].endsWith(".xml")) {
					listOfFileNames += wholePath + graphFileList[i] + "\n";
					fileNumber++;
				}
			StringReader fileNameListReader = new StringReader(listOfFileNames);
			File outputDir = new File("output_" + graphDir.getName());
			if (outputDir.canRead() || outputDir.mkdirs()) {
				experiment = new PathCompressionExperiment(outputDir
						.getAbsolutePath());
				assert fileNumber * learnerGenerators.length * stageNumber == experiment
						.computeMaxNumber(fileNameListReader);
				for (int number = 0; number < fileNumber
						* learnerGenerators.length * stageNumber; ++number)
					experiment.processDataSet(fileNameListReader, number);
			}
		} else {// args.length >=2
			experiment = new PathCompressionExperiment(args[1]);
			try {
				int num = Integer.parseInt(args[2]);
				if (num >= 0) {
					for (int i = 2; i < args.length; ++i)
						experiment.processDataSet(new FileReader(args[0]),
								Integer.parseInt(args[i]));
				} else
					try {
						System.out.println(experiment
								.computeMaxNumber(new FileReader(args[0])));
					} catch (Exception ex) {
						System.out.println(0);
					}

			} catch (Exception e) {
				System.out.println("FAILED");
				e.printStackTrace();
			}
		}

		// Everywhere here is it enough to simply say "FAILED" because on
		// failure, the output file will either
		// not exist at all or will simply contain "FAILED" in it.

		// now obtain the results
		if (experiment != null && experiment.results != null)
			for (Future<String> computationOutcome : experiment.results)
				try {
					// System.out.println("RESULT:
					// "+computationOutcome.get()+"\n");
				} catch (Exception e) {
					System.out.println("FAILED");
					e.printStackTrace();
				} finally {
					experiment.shutDown();
				}

	}

	public void shutDown() {
		executorService.shutdown();
	}

}
