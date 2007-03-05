package statechum.xmachine.model;


import java.util.*;

/**
 * An X-Machine transition function is an extension of conventional transition functions. 
 * X-Machines allow the developer to specify a function not only in terms of input and output, 
 * but also in terms of a transformation from one memory state to another. Notably, an X-Machine 
 * function can itself be represented as an X-Machine, resulting in a hierarchy.
 * 
 * @author Neil Walkinshaw
 *
 */


public class XMachineTransitionFunction extends TransitionFunction {
	
	private MemState fromMemory, toMemory;
	private HashSet basicBlocks;
	private XMachineModel functionMachine;
	private String pathCondition;
	
	public XMachineModel getFunctionMachine() {
		return functionMachine;
	}

	public void setFunctionMachine(XMachineModel functionMachine) {
		this.functionMachine = functionMachine;
	}

	public XMachineTransitionFunction(String label){
		this.setLabel(label);
		basicBlocks = new HashSet();
	}

	public MemState getFromMemory() {
		return fromMemory;
	}

	public void setFromMemory(MemState fromMemory) {
		this.fromMemory = fromMemory;
	}

	public MemState getToMemory() {
		return toMemory;
	}

	public void setToMemory(MemState toMemory) {
		this.toMemory = toMemory;
	}

	public HashSet getBasicBlocks() {
		return basicBlocks;
	}

	public void addBasicBlocks(Collection blocks){
		Iterator blockIt = blocks.iterator();
		while(blockIt.hasNext()){
			Block b = (Block)blockIt.next();
			if(b==null)
				continue;
			if(!contains(b)){
				basicBlocks.add(b);
			}
		}
	}
	
	private boolean contains(Block b){
		Iterator blockIt = basicBlocks.iterator();
		while(blockIt.hasNext()){
			Block iteratorBlock = (Block)blockIt.next();
			if(b.equals(iteratorBlock))
					return true;
		}
		return false;
	}
	
	public void printBlocks(){
		System.out.println(this.getLabel());
		Iterator blockIt = basicBlocks.iterator();
		while(blockIt.hasNext()){
			Block b = (Block)blockIt.next();
			System.out.println(b.toString());
		}
		System.out.println("--------------");
	}

	public String getPathCondition() {
		return pathCondition;
	}

	public void setPathCondition(String pathCondition) {
		this.pathCondition = pathCondition;
	}

}
