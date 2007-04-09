/*
 * INCOMPLETE
 */

package statechum.analysis.learning;


import java.awt.Point;
import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

import javax.swing.SwingUtilities;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.io.GraphMLFile;
import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.*;
import static statechum.xmachine.model.testset.WMethod.getGraphData;
import static statechum.xmachine.model.testset.WMethod.tracePath;

public class AccuracyAndQuestionsExperiment {

	Visualiser v = new Visualiser();
	
	public void evaluate(DirectedSparseGraph g){
		//updateFrame(g,g);
		RandomPathGenerator rpg = new RandomPathGenerator(g);
		WMethod tester = new WMethod(g,0);
		Set<List<String>> fullTestSet = (Set<List<String>>)tester.getFullTestSet();
		fullTestSet.addAll(tester.getTransitionCover());
		Set<List<String>> tests = randomHalf(fullTestSet);
		Set<List<String>> fullSet = rpg.getAllPaths();
		final FSMStructure expected = getGraphData(g);
		RPNIBlueFringeLearnerTestComponent l = new RPNIBlueFringeLearnerTestComponent(v)
		{
			protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
			{
				return tracePath(expected, question);
			}
		};
		//l.setCertaintyThreshold(2);
		Set<List<String>> sampleSet = randomHalf(fullSet);
		Vector<List<String>> samples = new Vector<List<String>>();
		samples.addAll(sampleSet);
		tests.removeAll(sampleSet);
		Set<List<String>> currentSamples = new HashSet<List<String>>();
		for(int i=10;i<=100;i=i+10){
			System.out.print(i + "%");
			currentSamples = addPercentageFromSamples(currentSamples, samples, i);
			Set<List<String>> sPlus = getPositiveStrings(g,currentSamples);
			Set<List<String>> sMinus = currentSamples;
			sMinus.removeAll(sPlus);
			sMinus = trimToNegatives(g, sMinus);
			try{
				DirectedSparseGraph learningOutcome = l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);
				//updateFrame(g,learningOutcome);
				System.out.print(","+l.getQuestionCounter());
				System.out.println(", "+computeAccuracy(learningOutcome, g,tests));
			}
			catch(InterruptedException e){return;};
			l.setQuestionCounter(0);
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
	
	private double computeAccuracy(DirectedSparseGraph learned, DirectedSparseGraph correct, Set<List<String>> tests){
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
	
	public static Set<List<String>> trimToNegatives(DirectedSparseGraph g, Set<List<String>> sMinus ){
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

	public static Set<List<String>> getPositiveStrings(DirectedSparseGraph graph, Set<List<String>> samples){
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
	
	public static Set<List<String>> randomHalf(Set<List<String>> v){
		Object[]samples = v.toArray();
		HashSet<List<String>> returnSet = new HashSet<List<String>>();
		Random generator = new Random();
		Set<Integer> done = new HashSet<Integer>();
		for(int i=0;i<v.size()/2;i++){
			int randomIndex = 0;
			boolean newInteger = false;
			while(!newInteger){
				randomIndex = generator.nextInt(v.size());
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
	
	public static void main(String[] args){
		AccuracyAndQuestionsExperiment experiment = new AccuracyAndQuestionsExperiment();
		File graphDir = new File(System.getProperty("user.dir")+System.getProperty("file.separator")+"resources"+
				System.getProperty("file.separator")+"TestGraphs"+System.getProperty("file.separator") +"50-2");
        String[] graphFileList = graphDir.list();
        for(int i=0;i<graphFileList.length;i++){
        	if(!graphFileList[i].startsWith("N"))
        		continue;
        	String wholePath = graphDir.getAbsolutePath()+System.getProperty("file.separator");
        	GraphMLFile graphmlFile = new GraphMLFile();
        	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
        	DirectedSparseGraph dg = new DirectedSparseGraph();
        	dg.getEdgeConstraints().clear();
        	dg = (DirectedSparseGraph)graphmlFile.load(wholePath+graphFileList[i]);
        	Iterator<Vertex> vIt = dg.getVertices().iterator();
        	System.out.println(graphFileList[i]);
        	experiment.evaluate(dg);
        		
        }
	}
	
}
