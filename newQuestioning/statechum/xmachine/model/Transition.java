package statechum.xmachine.model;

/**
 * This represents a transition between two states in the state transition diagram.
 * It can be assigned several transition functions. The label is generated automatically,
 * and consists of the concatenated transition function names.
 * @author Neil Walkinshaw
 */

import java.util.*;

public class Transition {
	
	private String label;
	private State from, to;
	private HashMap functions;
	
	public Transition(String from, String to){
		this.from = new State(from.trim());
		this.to = new State(to.trim());
		this.label = new String();
		this.functions = new HashMap();
	}
	
	public Transition(){
		this.label = new String();
		this.functions = new HashMap();
	}
	
	public void addFunctions(HashMap functions){
		Iterator functionIt = functions.values().iterator();
		while(functionIt.hasNext()){
			TransitionFunction f = (TransitionFunction)functionIt.next();
			addFunction(f);
		}
	}
	
	public void addFunction(TransitionFunction f){
		if(f instanceof XMachineTransitionFunction){
			XMachineTransitionFunction xf = (XMachineTransitionFunction)f;
			if(functions.keySet().contains(xf.getLabel())){
				XMachineTransitionFunction existing = (XMachineTransitionFunction)functions.get(f.getLabel());
				xf.addBasicBlocks(existing.getBasicBlocks());
				functions.put(xf.getLabel(), xf);
			}
			else{
				functions.put(f.getLabel(), f);
				label = label.concat(f.getLabel()+", \n");
			}
		}
		else{
			if(functions.keySet().contains(f.getLabel()))
				return;
			functions.put(f.getLabel(), f);
			label = label.concat(f.getLabel()+", \n");
		}
	}

	
	public State getFromState(){
		return from;
	}
	
	public State getToState(){
		return to;
	}
	
	public void setFromState(State s){
		this.from = s;
	}
	
	public void setToState(State s){
		this.to = s;
	}
	
	public String getName(){
		return label;
	}
	
	public HashMap getFunctions(){
		return functions;
	}

}
