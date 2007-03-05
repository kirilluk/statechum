package statechum.xmachine.model;

import java.util.*;
import edu.uci.ics.jung.graph.*;

public class XMachineModel {
	
	private HashSet transitions;
	private String name;
	private Graph jungGraph;
	
	public XMachineModel(String name){
		transitions = new HashSet();
		this.name = name;
	}
	
	public void addTransition(Transition t){
		Transition existingTransition = getTransition(t.getFromState(), t.getToState());
		if(existingTransition != null){
			t.addFunctions(existingTransition.getFunctions());
			transitions.remove(existingTransition);
		}
		transitions.add(t);
	}
	
	public HashSet getStates(){
		HashSet states = new HashSet();
		Iterator transitionIt = transitions.iterator();
		while(transitionIt.hasNext()){
			Transition t = (Transition)transitionIt.next();
			states.add(t.getFromState());
			states.add(t.getToState());
		}
		return states;
	}
	
	public HashSet getTransitions(){
		return transitions;
	}
	
	public String getName(){
		return name;
	}
	
	public void attachGraph(Graph g){
		this.jungGraph = g;
	}
	
	public Graph getAttachedGraph(){
		return this.jungGraph;
	}
	
	public Transition getTransition(State from, State to){
		Iterator transIt = transitions.iterator();
		while(transIt.hasNext()){
			Transition t = (Transition)transIt.next();
			if(t.getFromState().equals(from) && t.getToState().equals(to))
				return t;
		}
		return null;
	}

}
