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

package statechum.analysis.learning.oracles;

import java.io.File;
import java.util.*;

import javax.swing.*;

import soot.*;
import soot.options.*;
import soot.util.*;
import soot.jimple.*;
import soot.jimple.spark.*;
import soot.jimple.toolkits.callgraph.*;

import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.RPNIBlueFringeLearner;

public class SootCallGraphOracle implements AbstractOracle {

	private SootCallGraphManager scm;

	private String from = new String();

	public SootCallGraphOracle() {
		scm = new SootCallGraphManager();
	}

	public int getAnswer(List<String> question) {
		HashMap<MethodOrMethodContext, String> methodToString = new HashMap<MethodOrMethodContext, String>();
		Stack<MethodOrMethodContext> methodStack = new Stack<MethodOrMethodContext>();
		int length = question.size();
		if (question.get(0).equals("ret"))
			return 0;
		MethodOrMethodContext fromMethod = getSootMethod(question.get(0));
		methodToString.put(fromMethod, question.get(0));
		methodStack.push(fromMethod);
		CallGraph cg = Scene.v().getCallGraph();

		for (int i = 1; i < length; i++) {
			String next = question.get(i);
			if (next.equals("ret")) {
				if (!methodStack.isEmpty()) {
					methodStack.pop();
					continue;
				} else
					return i;
			}
			MethodOrMethodContext toMethod = getSootMethod(next);
			methodToString.put(toMethod, next);
			if (!methodStack.isEmpty()) {
				boolean found = false;
				Iterator<Edge> outEdges = cg.edgesOutOf(methodStack.peek());
				while (outEdges.hasNext()) {
					Edge e = outEdges.next();
					if (e.getTgt().equals(toMethod)) {
						found = true;
						break;
					}

				}
				if (!found) {
					System.out.println("not found: "
							+ methodStack.peek().method().getSignature() + "->"
							+ toMethod.method().getSignature());
					from = methodToString.get(methodStack.peek());
					return i;
				}
			}
			methodStack.push(toMethod);
		}
		return RPNIBlueFringeLearner.USER_ACCEPTED;
	}

	private List<Type> getTypes(String params) {
		List<Type> typeList = new ArrayList<Type>();
		for (int i = 0; i < params.length(); i++) {
			char character = params.charAt(i);
			if (character == 'Z')
				typeList.add(BooleanType.v());
			else if (character == 'B')
				typeList.add(ByteType.v());
			else if (character == 'C')
				typeList.add(CharType.v());
			else if (character == 'S')
				typeList.add(ShortType.v());
			else if (character == 'I')
				typeList.add(IntType.v());
			else if (character == 'J')
				typeList.add(LongType.v());
			else if (character == 'F')
				typeList.add(FloatType.v());
			else if (character == 'D')
				typeList.add(DoubleType.v());
			else if (character == 'L') {
				int skip = params.indexOf(';', i + 1);
				String refString = params.substring(i + 1, skip);
				typeList.add(Scene.v().getRefType(refString));
				i = skip;
			}
		}
		return typeList;

	}

	private MethodOrMethodContext getSootMethod(String signature) {
		int parenthesisIndex = signature.indexOf('(');
		String params = new String();
		try {
			params = signature.substring(parenthesisIndex + 1, signature
					.indexOf(')'));
		} catch (Exception e) {
			System.out.println(signature);
			System.exit(0);
		}
		String classString = signature.substring(0, parenthesisIndex);
		classString = classString.substring(0, classString.lastIndexOf('.'));
		SootClass sc = Scene.v().getSootClass(classString);
		params = params.replace('/', '.');
		String methodName = signature.substring(classString.length() + 1,
				parenthesisIndex);
		List<Type> paramTypes = getTypes(params);
		if (methodName.contains("-init-"))
			return sc.getMethod("<init>", paramTypes);
		return sc.getMethod(methodName, paramTypes);
	}

	public String getFrom() {
		return from;
	}

}
