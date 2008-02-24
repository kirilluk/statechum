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
