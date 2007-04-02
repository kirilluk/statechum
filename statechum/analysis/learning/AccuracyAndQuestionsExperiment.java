/*
 * INCOMPLETE
 */

package statechum.analysis.learning;


import java.io.File;
import java.util.*;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.io.GraphMLFile;
import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.*;


public class AccuracyAndQuestionsExperiment {

	
	
	public void evaluate(DirectedSparseGraph g){
		Visualiser viz = new Visualiser();
		WMethod wm = new WMethod(g,0);
		Set<List<String>> fullTestSet = wm.getFullTestSetStrings();
		String fsmString = getFSMString(g);
		DirectedSparseGraph testMachine = TestFSMAlgo.buildGraph(fsmString, "test machine");
		final FSMStructure expected = TestFSMAlgo.getGraphData(testMachine);
		for(int i=10;i<=100;i=i+10){
			Set<List<String>> samples = randomHalf(fullTestSet);
			Set<List<String>> tests = fullTestSet;
			tests.removeAll(samples);
			Set<List<String>> sPlus = getPositiveStrings(testMachine,samples);
			Set<List<String>> sMinus = samples;
			sMinus.removeAll(sPlus);
			sMinus = trimToNegatives(testMachine, sMinus);
			RPNIBlueFringeLearnerTestComponent l = new RPNIBlueFringeLearnerTestComponent(viz)
			{
				protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions)
				{
					return TestFSMAlgo.tracePath(expected.init, expected.trans, expected.accept, question);
				}
			};
			l.addObserver(viz);
			try{
			DirectedSparseGraph learningOutcome = l.learnMachine(RPNIBlueFringeLearner.initialise(), sPlus, sMinus);

			//TestFSMAlgo.checkM(
				//	learningOutcome,
					//fsmString);
			}
			catch(InterruptedException e){return;};
		}
	}
	
	private Set<List<String>> trimToNegatives(DirectedSparseGraph g, Set<List<String>> sMinus ){
		Set<List<String>> returnSet = new HashSet<List<String>>();
		Iterator<List<String>> sMinusIt = sMinus.iterator();
		while(sMinusIt.hasNext()){
			List<String> currentString = sMinusIt.next();
			final FSMStructure expected = TestFSMAlgo.getGraphData(g);
			int reject = TestFSMAlgo.tracePath(expected.init, expected.trans, expected.accept, currentString);
			returnSet.add(currentString.subList(0, reject+1));
		}
		return returnSet;
	}

	private static Set<List<String>> getPositiveStrings(DirectedSparseGraph graph, Set<List<String>> samples){
		Iterator<List<String>> sampleIt = samples.iterator();
		HashSet<List<String>> positiveStrings = new HashSet<List<String>>();
		while(sampleIt.hasNext()){
			List<String> v = sampleIt.next();
			if(RPNIBlueFringeLearner.getVertex(graph, v) != null)
				positiveStrings.add(v);
		}
		return positiveStrings;
	}
	
	private String getFSMString(DirectedSparseGraph g){
		String fsmString = "";
		for(DirectedSparseEdge e:(Collection<DirectedSparseEdge>)g.getEdges()){
			String sourceLabel = e.getSource().getUserDatum("VERTEX").toString();
			String targetLabel = e.getDest().getUserDatum("VERTEX").toString();
			if(targetLabel.startsWith("Initial"))
				targetLabel = targetLabel.substring(13);
			String edgeLabel = e.getUserDatum("EDGE").toString();
			if(sourceLabel.startsWith("Initial"))
				fsmString = ("\\n"+sourceLabel.substring(13).trim()+"-"+edgeLabel+"->"+targetLabel.trim()).concat(fsmString);
			else
				fsmString = fsmString.concat("\\n"+sourceLabel.trim()+"-"+edgeLabel+"->"+targetLabel.trim());
		}
		return fsmString;
	}
	
	private Set<List<String>> randomHalf(Set<List<String>> v){
		Object[]samples = v.toArray();
		HashSet<List<String>> returnSet = new HashSet<List<String>>();
		Random generator = new Random();
		for(int i=0;i<v.size()/2;i++){
			returnSet.add((List<String>)samples[(generator.nextInt(v.size()))]);
		}
		return returnSet;
	}
	
	public static void main(String[] args){
		AccuracyAndQuestionsExperiment experiment = new AccuracyAndQuestionsExperiment();
		File graphDir = new File(System.getProperty("user.dir")+System.getProperty("file.separator")+"resources"+
				System.getProperty("file.separator")+"5-2"+System.getProperty("file.separator")+"Format");
        String[] graphFileList = graphDir.list();
        for(int i=0;i<graphFileList.length;i++){
        	String wholePath = graphDir.getAbsolutePath()+System.getProperty("file.separator");
        	GraphMLFile graphmlFile = new GraphMLFile();
        	DirectedSparseGraph g = (DirectedSparseGraph)graphmlFile.load(wholePath+graphFileList[i]);
        	Iterator<Vertex> vIt = g.getVertices().iterator();
        	while(vIt.hasNext()){
        		Vertex v = vIt.next();
        		if(v.getUserDatum("VERTEX").toString().startsWith("Initial")){
        			v.addUserDatum("startOrTerminal", "start", UserData.SHARED);
        			break;
        		}
        	}
        	Iterator<Edge> edgeIt = g.getEdges().iterator();
        	while(edgeIt.hasNext()){
        		Edge e = edgeIt.next();
        		HashSet set = new HashSet();
        		set.add(e.getUserDatum("EDGE").toString());
        		e.setUserDatum(JUConstants.LABEL, set, UserData.SHARED);
        	}
        	experiment.evaluate(g);
        		
        }
	}
	
}
