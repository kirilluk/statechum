/*
 * INCOMPLETE
 */

package statechum.analysis.learning;


import java.awt.Point;
import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.swing.SwingUtilities;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.io.GraphMLFile;
import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.*;
import statechum.xmachine.model.testset.PTATestSequenceEngine.sequenceSet;
import static statechum.xmachine.model.testset.WMethod.getGraphData;
import static statechum.xmachine.model.testset.WMethod.tracePath;

public class AccuracyAndQuestionsExperiment {

	public AccuracyAndQuestionsExperiment(LearnerEvaluatorFactory eval)
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

		results = new LinkedList<Future<String>>();
		runner = new ExecutorCompletionService<String>(Executors.newFixedThreadPool(ThreadNumber));
		evaluatorFactory = eval;
	}
	
	public abstract static class LearnerEvaluatorFactory 
	{
		public abstract LearnerEvaluator createLearnerEvaluator(Collection<List<String>> plus, Collection<List<String>> minus, DirectedSparseGraph g, String name, int per, Collection<List<String>> tests);
	}
	
	public abstract static class LearnerEvaluator implements Callable<String>
	{
		protected Collection<List<String>> sPlus=null, sMinus=null;
		protected DirectedSparseGraph graph=null;
		protected String fileName = null;
		protected Collection<List<String>> tests = null;
		protected int percent;
		
		private LearnerEvaluator() {}

		/**
		 * 
		 * @param plus positive set.
		 * @param minus negative set.
		 * @param machine machine to check the result against.
		 * @param name the name of the graph.
		 * @param per the percentage of the total set of inputs this evaluator is being run on.
		 * @param the set of data used for checking the accuracy of learning.
		 */
		public LearnerEvaluator(Collection<List<String>> plus, Collection<List<String>> minus, DirectedSparseGraph g, String name, int per, Collection<List<String>> in_tests)
		{
			graph = g;sPlus = plus;sMinus = minus;fileName = name;percent = per;tests=in_tests;
		}

		public enum FileType { 
			DATA {String getFileName(String prefix, String suffix) { return prefix+"_data"+suffix+".xml"; } }, 
			RESULT {String getFileName(String prefix, String suffix) { return prefix+"_result"+suffix+".txt"; } };
			
			abstract String getFileName(String prefix, String suffix);
		};
		
		protected String getFileName(FileType fileNameType)
		{
			return fileNameType.getFileName(fileName,"-"+percent); 
		}
	}
	
	
	/** This one is not static because it refers to the frame to display results. */
	public static class RPNIEvaluator extends LearnerEvaluator
	{
		public RPNIEvaluator(Collection<List<String>> plus, Collection<List<String>> minus, DirectedSparseGraph g, String name, int per, Collection<List<String>> in_tests)
		{
			super(plus,minus,g,name,per,in_tests);			
		}
		
		public String call()
		{
			final FSMStructure fsm = WMethod.getGraphData(graph);
			RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null)
			{
				protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
				{
					return tracePath(fsm, question);
				}
			};
			System.out.println(""+percent + "% started");
			DirectedSparseGraph learningOutcome = null;
			String result = "" + percent+"%,";
			String stats = "";
			String stdOutput = null;
			try
			{
				learningOutcome = l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);
				result = result+l.getQuestionCounter()+", "+computeAccuracy(learningOutcome, graph,tests);							
				//updateFrame(g,learningOutcome);
				l.setQuestionCounter(0);
				if (learningOutcome != null)
					stats = learningOutcome.containsUserDatumKey("STATS")? "\n"+learningOutcome.getUserDatum("STATS").toString():"";
				System.out.println(percent +"% terminated");
				
				// now record the result
				Writer outputWriter = new BufferedWriter(new FileWriter(getFileName(FileType.RESULT)));
				outputWriter.write(result+"\nSTATS: "+stats);outputWriter.close();
				
				stdOutput = fileName+" "+result+"\nSTATS: "+stats;
			}
			catch(Throwable th)
			{
				StringWriter writer = new StringWriter();
				th.printStackTrace();
				th.printStackTrace(new PrintWriter(writer));
				stdOutput = result+"FAILED\nSTACK: "+writer.toString();
			}
			return stdOutput;
		}
	}
	
	public class SaveData extends LearnerEvaluator
	{
		public String call() throws Exception {
			XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(new FileOutputStream(getFileName(FileType.DATA))));
			encoder.writeObject(sPlus);
			encoder.writeObject(sMinus);
			encoder.writeObject(fileName);encoder.writeObject(new Integer(percent));
			encoder.writeObject(tests);
			ByteArrayOutputStream outStream = new ByteArrayOutputStream();
			new GraphMLFile().save(graph, new PrintStream(outStream));
			encoder.writeObject(outStream.toString());// Jung uses PrintStream to dump bytes of the graph but read is in using Reader which interprets unicode in the input data - extremely strange, so I ignore all that and use strings
			encoder.close();	
			return "Wrote "+getFileName(FileType.DATA);
		}
		
	}
	
	public class LoadData extends LearnerEvaluator
	{
		/**
		 * ID for serialisation, same as SaveData
		 */
		private static final long serialVersionUID = -6070805972757350807L;

		public String call() throws Exception {
			XMLDecoder decoder = new XMLDecoder(new BufferedInputStream(new FileInputStream(getFileName(FileType.DATA))));
			sPlus = (Collection<List<String>>)decoder.readObject();
			sMinus = (Collection<List<String>>) decoder.readObject();
			fileName = (String) decoder.readObject();percent = ((Integer)decoder.readObject()).intValue();
			tests = (Collection<List<String>>) decoder.readObject();
			String data = (String) decoder.readObject();
			graph = (DirectedSparseGraph) new GraphMLFile().load(new StringReader(data));
			decoder.close();	
			return "Loaded "+getFileName(FileType.DATA);
		}
		
	}
	
	public class LoadDataAndProcessIt extends LoadData
	{
		final protected LearnerEvaluatorFactory evalFactory;
		
		public LoadDataAndProcessIt(String name, int percent, LearnerEvaluatorFactory evalFac)
		{
			evalFactory = evalFac;
		}
		
		public String call() throws Exception 
		{
			String result = super.call();
			LearnerEvaluator processor =
				evalFactory.createLearnerEvaluator(sPlus, sMinus, graph, fileName, percent, tests);
			return result+"\n"+processor.call();
		}		
	}
	
	/** Stores results of execution of evaluators. */
	final List<Future<String>> results;
	
	/** Stores tasks to complete. */
	final CompletionService<String> runner;
	
	/** The class to perform experiments. */
	final LearnerEvaluatorFactory evaluatorFactory;
	
	/** Since we need predictable generation of sets, construction of them cannot be parallelised because
	 * then the sequence of calls to the random number generator will be unpredictable.
	 *  
	 * @param outputFileName the output file name
	 * @param g graph to process
	 */ 
	public void evaluate(String outputFileName,final DirectedSparseGraph g){
		//updateFrame(g,g);
		RandomPathGenerator rpg = new RandomPathGenerator(g);
		WMethod tester = new WMethod(g,0);
		Collection<List<String>> fullTestSet = (Collection<List<String>>)tester.getFullTestSet();
		System.out.println("test set size: "+fullTestSet.size());
		final Collection<List<String>> tests = randomHalf(fullTestSet);
		Collection<List<String>> fullSet = rpg.getAllPaths();
		//l.setCertaintyThreshold(2);
		Collection<List<String>> sampleSet = randomHalf(fullSet);
		Vector<List<String>> samples = new Vector<List<String>>();
		samples.addAll(sampleSet);
		tests.removeAll(sampleSet);
		Set<List<String>> currentSamples = new LinkedHashSet<List<String>>();
		
		for(int i=10;i<=100;i=i+10){
			currentSamples = addPercentageFromSamples(currentSamples, samples, i);
			final Collection<List<String>> sPlus = getPositiveStrings(g,currentSamples);
			Collection<List<String>> sMinus = currentSamples;
			sMinus.removeAll(sPlus);
			sMinus = trimToNegatives(g, sMinus);
			final Collection<List<String>> minus = new LinkedList<List<String>>();minus.addAll(sMinus);
			results.add(runner.submit(evaluatorFactory.createLearnerEvaluator(sPlus, sMinus, g, outputFileName, i, tests)));
		}

	}
	
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
				if(hypVertex.getUserDatum(JUConstants.ACCEPTED).equals("true")){
					//updateFrame(learned, correct);
					failed++;
				}
			}
				
		}
		double accuracy = 1-((double)failed/(double)tests.size());
		return accuracy;
	}
	
	public static Set<List<String>> addPercentageFromSamples(Set<List<String>> current, Vector<List<String>> samples, double percent){
		double size = samples.size();
		double number = (size/100)*percent;
		//samples.removeAll(current);
		List<String>[] sampleArray = (List<String>[])samples.toArray(new List[samples.size()]);
		for(int i=0;i<(int)number;i++){
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
			else
				System.out.println(v);
		}
		return positiveStrings;
	}

	public static final Random halfRandomNumberGenerator = new Random(1); 

	public static Collection<List<String>> randomHalf(Collection<List<String>> v){
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
	
	private void processFile(String fileName)
	{
    	GraphMLFile graphmlFile = new GraphMLFile();
    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
    	DirectedSparseGraph dg = new DirectedSparseGraph();
    	dg.getEdgeConstraints().clear();
    	dg = (DirectedSparseGraph)graphmlFile.load(fileName);
    	//Iterator<Vertex> vIt = dg.getVertices().iterator();
    	System.out.println(fileName);
    	evaluate("w_"+new File(fileName).getName(),dg);		
	}
	

	/**
	 * For dual-core operation, VM args should be -ea -Xmx1600m -Xms300m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2
	 * Quad-core would use -Dthreadnum=4 instead.
	 * 
	 * There are multiple modes of operation, process graphs, generate data for processing 
	 * and dump sets to files, load data from files and process them, using a user-chosen learner. 
	 * 
	 * @param args command-line arguments
	 */
	public static void main(String[] args)
	{
		AccuracyAndQuestionsExperiment experiment = null;
		if (args.length < 2)
		{
			experiment = new AccuracyAndQuestionsExperiment(new LearnerEvaluatorFactory() {

				@Override
				public LearnerEvaluator createLearnerEvaluator(Collection<List<String>> plus, Collection<List<String>> minus, DirectedSparseGraph g, String name, int per, Collection<List<String>> tests) {
					return new RPNIEvaluator(plus, minus, g,name,per,tests);
				}
				
			});
			File graphDir = new File(args[0]);
					//"C:\\experiment\\graphs-150\\Neil-Data2\\50-6"); 
					//"D:\\experiment\\Neil-Data2\\50-6");
					//System.getProperty("user.dir")+System.getProperty("file.separator")+"resources"+
					//System.getProperty("file.separator")+"TestGraphs"+System.getProperty("file.separator") +"50-6");
	        String[] graphFileList = graphDir.list();
	         
	        String wholePath = graphDir.getAbsolutePath()+System.getProperty("file.separator");
	        for(int i=0;i<graphFileList.length;i++){
	        	if(!graphFileList[i].startsWith("N"))
	        		continue;
	            experiment.processFile(wholePath+graphFileList[i]);
	        }
		}
		else
		{// args.length >=2
            experiment.processFile(args[0]);			
		}

		
		// now display the results
        for(Future<String> computationOutcome:experiment.results)
		{
			try {
				System.out.println("RESULT: "+computationOutcome.get());
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ExecutionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
}
