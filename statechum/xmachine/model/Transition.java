/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of statechum.

statechum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Foobar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
*/ 

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

	public Transition(String from, String to) {
		this.from = new State(from.trim());
		this.to = new State(to.trim());
		this.label = new String();
		this.functions = new HashMap();
	}

	public Transition() {
		this.label = new String();
		this.functions = new HashMap();
	}

	public void addFunctions(HashMap functions) {
		Iterator functionIt = functions.values().iterator();
		while (functionIt.hasNext()) {
			TransitionFunction f = (TransitionFunction) functionIt.next();
			addFunction(f);
		}
	}

	public void addFunction(TransitionFunction f) {
		if (f instanceof XMachineTransitionFunction) {
			XMachineTransitionFunction xf = (XMachineTransitionFunction) f;
			if (functions.keySet().contains(xf.getLabel())) {
				XMachineTransitionFunction existing = (XMachineTransitionFunction) functions
						.get(f.getLabel());
				xf.addBasicBlocks(existing.getBasicBlocks());
				functions.put(xf.getLabel(), xf);
			} else {
				functions.put(f.getLabel(), f);
				label = label.concat(f.getLabel() + ", \n");
			}
		} else {
			if (functions.keySet().contains(f.getLabel()))
				return;
			functions.put(f.getLabel(), f);
			label = label.concat(f.getLabel() + ", \n");
		}
	}

	public State getFromState() {
		return from;
	}

	public State getToState() {
		return to;
	}

	public void setFromState(State s) {
		this.from = s;
	}

	public void setToState(State s) {
		this.to = s;
	}

	public String getName() {
		return label;
	}

	public HashMap getFunctions() {
		return functions;
	}

}
