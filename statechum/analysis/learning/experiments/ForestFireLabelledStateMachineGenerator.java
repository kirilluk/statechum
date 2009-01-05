package statechum.analysis.learning.experiments;

import java.util.Formatter;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import statechum.JUConstants;

import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

public class ForestFireLabelledStateMachineGenerator extends
		ForestFireStateMachineGenerator {
	
	Set<String> alphabet;

	public ForestFireLabelledStateMachineGenerator(double forwards, double backwards, int alphabetSize){
		super(forwards, backwards);
		this.alphabet=getAlphabet(alphabetSize);
	}
	
	public ForestFireLabelledStateMachineGenerator(double forwards, double backwards, Set<String> alphabet){
		super(forwards, backwards);
		this.alphabet=alphabet;
	}
	
	@Override
	protected void addEdge(DirectedSparseVertex v, DirectedSparseVertex random) {
		Set<String> vertexAlphabet = new HashSet<String>();
		for (Object e : random.getOutEdges()) {
			DirectedSparseEdge edge = (DirectedSparseEdge)e;
			Set<String>labels = (Set<String>)edge.getUserDatum(JUConstants.LABEL);
			if(labels!=null)
				vertexAlphabet.addAll(labels);
		}
		Set<String> possibles = alphabet;
		alphabet.removeAll(vertexAlphabet);
		String label = pickRandom(possibles);
		Set<String> labelSet = new HashSet<String>();
		labelSet.add(label);
		DirectedSparseEdge e = new DirectedSparseEdge(v,random);
		e.addUserDatum(JUConstants.LABEL, labelSet, UserData.SHARED);
		machine.addEdge(e);
	}

	private String pickRandom(Set<String> possibles) {
		Random index = new Random(0);
		return (String)possibles.toArray()[index.nextInt(possibles.size()-1)];
	}

	private static Set<String> getAlphabet(int number){
		Set<String> alphabet = new HashSet<String>();
		for (int i=0;i<number;i++){
			String next = new Formatter().format("%05d", i).toString();
			alphabet.add(next);
		}
		return alphabet;
	}
	
}
