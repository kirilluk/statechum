 package statechum.analysis.learning;

import static statechum.analysis.learning.TestFSMAlgo.buildSet;
import static statechum.xmachine.model.testset.WMethod.getGraphData;
import static statechum.xmachine.model.testset.WMethod.tracePath;

import java.io.File;
import java.util.*;

import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.WMethod;

import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.io.GraphMLFile;
import edu.uci.ics.jung.utils.UserData;



public class RPNITester {

	public static void main(String[] args){
		System.out.println("started.");
		File graphDir = new File(args[0]);//new File(System.getProperty("user.dir")+System.getProperty("file.separator")+"resources"+
				//System.getProperty("file.separator")+"TestGraphs"+System.getProperty("file.separator") +args[0]);
    	String wholePath = graphDir.getAbsolutePath()+System.getProperty("file.separator");
    	GraphMLFile graphmlFile = new GraphMLFile();
    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
    	DirectedSparseGraph dg = new DirectedSparseGraph();
    	dg.getEdgeConstraints().clear();
    	dg = (DirectedSparseGraph)graphmlFile.load(wholePath+args[1]);
		
		RandomPathGenerator rpg = new RandomPathGenerator(dg);
		Set<List<String>> fullSet = rpg.getAllPaths();
		final FSMStructure expected = getGraphData(dg);
		
		RPNIBlueFringeLearnerTestComponent l = new RPNIBlueFringeLearnerTestComponentOpt(null) // CHOOSE non-Opt for original version
		{
			protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
			{
				return tracePath(expected, question);
			}
		};
		
		Set<List<String>> sampleSet = AccuracyAndQuestionsExperiment.randomHalf(fullSet);
		Vector<List<String>> samples = new Vector<List<String>>();
		samples.addAll(sampleSet);
		Set<List<String>> tests = fullSet;
		tests.removeAll(samples);
		Set<List<String>> currentSamples = new HashSet<List<String>>();
		currentSamples = AccuracyAndQuestionsExperiment.addPercentageFromSamples(currentSamples, samples, 10);
		Set<List<String>> sPlus = AccuracyAndQuestionsExperiment.getPositiveStrings(dg,currentSamples);
		Set<List<String>> sMinus = currentSamples;
		sMinus.removeAll(sPlus);
		sMinus = AccuracyAndQuestionsExperiment.trimToNegatives(dg, sMinus);
		l.setQuestionCounter(0);
		
		for(int i=0;i< 1;++i)
		{
			System.out.println("iteration "+i+" building PTA");
			DirectedSparseGraph g= l.createAugmentedPTA(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);
			RPNIBlueFringeLearner.numberVertices(g);
			Vertex init = RPNIBlueFringeLearner.findVertex("property", "init",g);
			init.setUserDatum("colour", "red", UserData.SHARED);
			System.out.println("computing pairs");
			StatePair pair=(StatePair)l.chooseStatePairs(g, sPlus, sMinus).peek();
			System.out.println("merging");
			DirectedSparseGraph temp = l.mergeAndDeterminize(g, pair);
			System.out.println("generating questions");
			l.generateQuestions(g, temp, pair);
		}
		/*
		try{
			DirectedSparseGraph learningOutcome = l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);
			//updateFrame(g,learningOutcome);
			//System.out.println(", "+computeAccuracy(learningOutcome, dg,tests));
		}
		catch(InterruptedException e){return;};
		*/
	}
}
