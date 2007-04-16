/*
 * INCOMPLETE
 */

package statechum.analysis.learning;


import java.awt.Point;
import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
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

	Visualiser v = new Visualiser();
	
	public void evaluate(final DirectedSparseGraph g){
		//updateFrame(g,g);
		RandomPathGenerator rpg = new RandomPathGenerator(g);
		WMethod tester = new WMethod(g,0);
		Collection<List<String>> fullTestSet = (Collection<List<String>>)tester.getFullTestSet();
		System.out.println("test set size: "+fullTestSet.size());
		final Collection<List<String>> tests = randomHalf(fullTestSet);
		Collection<List<String>> fullSet = rpg.getAllPaths();
		final FSMStructure expected = getGraphData(g);
		//l.setCertaintyThreshold(2);
		Collection<List<String>> sampleSet = randomHalf(fullSet);
		Vector<List<String>> samples = new Vector<List<String>>();
		samples.addAll(sampleSet);
		tests.removeAll(sampleSet);
		Set<List<String>> currentSamples = new LinkedHashSet<List<String>>();
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
		final List<Future<String>> results = new LinkedList<Future<String>>();
		CompletionService<String> runner = new ExecutorCompletionService<String>(
				Executors.newFixedThreadPool(ThreadNumber)
				//Executors.newSingleThreadExecutor() // this one for single-threaded operation
				//Executors.newCachedThreadPool()
				);
		
		for(int i=10;i<=100;i=i+10){
			currentSamples = addPercentageFromSamples(currentSamples, samples, i);
			final Collection<List<String>> sPlus = getPositiveStrings(g,currentSamples);
			Collection<List<String>> sMinus = currentSamples;
			sMinus.removeAll(sPlus);
			sMinus = trimToNegatives(g, sMinus);
			final int percentNo = i;
			final Collection<List<String>> minus = new LinkedList<List<String>>();minus.addAll(sMinus);
			results.add(runner.submit(
				new Callable<String>() {
					public String call()
					{
						RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(v)
						{
							protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
							{
								return tracePath(expected, question);
							}
						};
						System.out.println(percentNo + "% started");
						DirectedSparseGraph learningOutcome = null;
						String result = "" + percentNo+"%,";
						String stats = "";
						try
						{
							learningOutcome = l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, minus);
							result = result+l.getQuestionCounter()+", "+computeAccuracy(learningOutcome, g,tests);							
						}
						catch(Throwable th)
						{
							StringWriter writer = new StringWriter();
							th.printStackTrace();
							th.printStackTrace(new PrintWriter(writer));
							result = result+"FAILED";
							stats = "STACK: "+writer.toString();
						}
						//updateFrame(g,learningOutcome);
						l.setQuestionCounter(0);
						if (learningOutcome != null)
							stats = learningOutcome.containsUserDatumKey("STATS")? "\n"+learningOutcome.getUserDatum("STATS").toString():"";
						System.out.println(percentNo +"% terminated");
						return result+"\nSTATS: "+stats;
					}
				}));
		}

		for(Future<String> computationOutcome:results)
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
	
	/** Displays twos graphs passed as arguments in the Jung window.
	 * @param g the graph to display 
	 * @param lowerGraph the graph to display below it
	 */
	public void updateFrame(final DirectedSparseGraph g,final DirectedSparseGraph lowerGraph)
	{
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
	
	private double computeAccuracy(DirectedSparseGraph learned, DirectedSparseGraph correct, Collection<List<String>> tests){
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
    	evaluate(dg);		
	}
	

	/**
	 * For dual-core operation, VM args should be -ea -Xmx1024m -Xms300m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2
	 * Quad-core would use -Dthreadnum=4 instead.
	 * 
	 * @param args command-line arguments
	 */
	public static void main(String[] args){
		final AccuracyAndQuestionsExperiment experiment = new AccuracyAndQuestionsExperiment();
		
		if (args.length < 2)
		{
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
	}
	
}
