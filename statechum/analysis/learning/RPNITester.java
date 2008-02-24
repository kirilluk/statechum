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

 package statechum.analysis.learning;

import static statechum.xmachine.model.testset.WMethod.getGraphData;
import static statechum.xmachine.model.testset.WMethod.tracePath;

import java.beans.XMLDecoder;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.*;

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.experiments.AccuracyAndQuestionsExperiment;
import statechum.analysis.learning.experiments.ExperimentGraphMLHandler;
import statechum.analysis.learning.experiments.RandomPathGenerator;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.io.GraphMLFile;

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
    	int size = dg.getEdges().size()*4;
		RandomPathGenerator rpg = new RandomPathGenerator(dg, new Random(1),size, 5);
		Collection<List<String>> fullSet = rpg.getAllPaths();
		final FSMStructure expected = getGraphData(dg);
		
		RPNIBlueFringeLearnerTestComponentOpt l = new RPNIBlueFringeLearnerTestComponentOpt(null) // CHOOSE non-Opt for original version
		{
			protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
			{
				return tracePath(expected, question);
			}
		};
		
		Collection<List<String>> sampleSet = AccuracyAndQuestionsExperiment.randomHalf(fullSet, new Random(1));
		Vector<List<String>> samples = new Vector<List<String>>();
		samples.addAll(sampleSet);
		Collection<List<String>> tests = fullSet;
		tests.removeAll(samples);
		Set<List<String>> currentSamples = new HashSet<List<String>>();
		currentSamples = AccuracyAndQuestionsExperiment.addPercentageFromSamples(currentSamples, samples, 10);
		Collection<List<String>> sPlus = AccuracyAndQuestionsExperiment.getPositiveStrings(dg,currentSamples);
		Collection<List<String>> sMinus = currentSamples;
		sMinus.removeAll(sPlus);
		sMinus = AccuracyAndQuestionsExperiment.trimToNegatives(dg, sMinus);
		l.setQuestionCounter(0);
/*
		for(int i=0;i< 20;++i)
		{
			dg.copy();
			ComputeStateScores.copy(dg);
		}
*/		
		/*
		DirectedSparseGraph g= l.createAugmentedPTA(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);
		RPNIBlueFringeLearner.numberVertices(g);
		Vertex init = RPNIBlueFringeLearner.findVertex(JUConstants.PROPERTY, "init",g);
		init.setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
		System.out.println("computing pairs");
		for(int i=0;i< 5;++i)
		{
			System.out.println("computing pairs, iteration "+i);
			l.chooseStatePairs(g, sPlus, sMinus);
			l.scoreComputer.chooseStatePairs();
		}
		*/
/*
 		StatePair pair=(StatePair)l.chooseStatePairs(g, sPlus, sMinus).peek();
		DirectedSparseGraph temp = l.mergeAndDeterminize(g, pair);
		for(int i=0;i< 5;++i)
		{
			System.out.println("generating questions, iteration "+i);
			l.generateQuestions(g, temp, pair);
			l.scoreComputer.computeQS(pair, temp);
		}
*/		
	/*	
		for(int i=0;i< 5;++i)
		{
			System.out.println("iteration "+i+" building PTA");
			//DirectedSparseGraph g= 
			l.createAugmentedPTA(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);
			l.scoreComputer.augmentPTA(l.scoreComputer.augmentPTA(RPNIBlueFringeLearner.initialise(), sPlus, true), sMinus, false);
			
			RPNIBlueFringeLearner.numberVertices(g);
			Vertex init = RPNIBlueFringeLearner.findVertex(JUConstants.PROPERTY, "init",g);
			init.setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
			System.out.println("computing pairs");
			StatePair pair=(StatePair)l.chooseStatePairs(g, sPlus, sMinus).peek();
			System.out.println("merging");
			DirectedSparseGraph temp = l.mergeAndDeterminize(g, pair);
			System.out.println("generating questions");
			l.generateQuestions(g, temp, pair);
		}
			*/

		try {
			XMLDecoder decoder = new XMLDecoder(new BufferedInputStream(new FileInputStream("strings_4k.xml")));
			sPlus = (Collection<List<String>>)decoder.readObject();
			sMinus = (Collection<List<String>>)decoder.readObject();
			decoder.close();
			
			for(int i=0;i< 2;++i)
			{
				//DirectedSparseGraph g=RPNIBlueFringeLearner.initialise();
				//g.getEdgeConstraints().clear();
				l.init(sPlus, sMinus);// KIRR: node labelling is done by createAugmentedPTA
				ComputeStateScores aa = l.getScoreComputer();
				//l.findVertex(JUConstants.PROPERTY, "init",model).setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
	
				System.out.println("computing pairs");
				//StatePair pair = (StatePair)l.chooseStatePairs(model, sPlus, sMinus).pop();
				Stack stk = aa.chooseStatePairs();
				StatePair pair = (StatePair) stk.peek();
				while(!stk.isEmpty())
				{
					StatePair p = (StatePair)stk.pop();
					assert p.getQ().getUserDatum(JUConstants.COLOUR) == JUConstants.BLUE;
					assert p.getR().getUserDatum(JUConstants.COLOUR) == JUConstants.RED;					
				}
				System.out.println("merging");
				//DirectedSparseGraph temp = l.mergeAndDeterminize(model, pair);
				//System.out.println("Pair : "+pair+" compatibility: "+aa.computePairCompatibilityScore(pair));
				ComputeStateScores temp = ComputeStateScores.mergeAndDeterminize(aa, pair);
				
				System.out.println("generating questions");
				//l.generateQuestions(model, temp, pair);
				aa.computeQS(pair, temp);
			}			

		} catch (FileNotFoundException e1) {
			e1.printStackTrace();return;
		}
		/*
		 
		try
		{
			DirectedSparseGraph learningOutcome = l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);
		}
		catch (InterruptedException e2) {
			e2.printStackTrace();
		}
	*/
	}
}
