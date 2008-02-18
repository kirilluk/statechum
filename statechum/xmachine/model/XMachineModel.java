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

import java.util.*;
import edu.uci.ics.jung.graph.*;

public class XMachineModel {

	private HashSet transitions;

	private String name;

	private Graph jungGraph;

	public XMachineModel(String name) {
		transitions = new HashSet();
		this.name = name;
	}

	public void addTransition(Transition t) {
		Transition existingTransition = getTransition(t.getFromState(), t
				.getToState());
		if (existingTransition != null) {
			t.addFunctions(existingTransition.getFunctions());
			transitions.remove(existingTransition);
		}
		transitions.add(t);
	}

	public HashSet getStates() {
		HashSet states = new HashSet();
		Iterator transitionIt = transitions.iterator();
		while (transitionIt.hasNext()) {
			Transition t = (Transition) transitionIt.next();
			states.add(t.getFromState());
			states.add(t.getToState());
		}
		return states;
	}

	public HashSet getTransitions() {
		return transitions;
	}

	public String getName() {
		return name;
	}

	public void attachGraph(Graph g) {
		this.jungGraph = g;
	}

	public Graph getAttachedGraph() {
		return this.jungGraph;
	}

	public Transition getTransition(State from, State to) {
		Iterator transIt = transitions.iterator();
		while (transIt.hasNext()) {
			Transition t = (Transition) transIt.next();
			if (t.getFromState().equals(from) && t.getToState().equals(to))
				return t;
		}
		return null;
	}

}
