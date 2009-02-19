/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 *
 * This file is part of StateChum.
 * 
 * statechum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */
package statechum.analysis.learning.rpnicore;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import cern.colt.Arrays;

import statechum.GlobalConfiguration;
import statechum.StatechumXML;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.model.testset.PTAExploration;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.SequenceSet;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.Smt;

/** Transition labels may have complex behaviours associated with them and
 * SMT can be used to check
 * 
 * @author kirill
 *
 */
public class LabelRepresentation {
	/** Constraints declaration of memory and input variables, together with constraints
	 * on input variables to ensure that design-for-test conditions are met are stored as a precondition of MEM0
	 * A postcondition of MEM0 describes the initial memory value.
	 */
	public static final String INITMEM="MEM0";
	
	/** Eliminates spaces at the beginning and end of the supplied 
	 * string. If not empty, the outcome is appended to the buffer provided. 
	 * This is needed because a in number 
	 * of cases we cannot simply include an empty string into Yices expression - 
	 * we have to know that the sting is empty and put "true".
	 * For this reason, empty values are designated with nulls and this method makes it possible to maintain them
	 * until a non-empty string is appended. Usage:
	 * <pre>
	 * String text = null;
	 * ...
	 * text = appendStringToBuffer(string,text);
	 * </pre>
	 * 
	 * @param str what to append
	 * @param where to append
	 * @param result, which could be null 
	 */
	public static String appendToString(String str,String buffer)
	{
		String result = buffer;
		if (str == null) return result; // nothing to do
		String trimmed = str.trim();if (trimmed.length() == 0) return result;// nothing to do
		
		if (buffer == null) result = trimmed;else { result= result+ENDL+trimmed; }
		return result;
	}
	
	public class Label
	{
		protected String name, pre = null, post = null;
		
		public Label(String labelName)
		{
			name = appendToString(labelName, null);
			if (name == null)
				throw new IllegalArgumentException("invalid label name");

			if (labelMap.containsKey(name))
				throw new IllegalArgumentException("duplicate label "+name);
		}
		
		public Label(String labelName, String preCondition, String postCondition)
		{
			this(labelName);pre=appendToString(preCondition,pre);post=appendToString(postCondition,post);
		}
		
		/** Returns the name of this label. */
		public String getName()
		{
			return name;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((name == null) ? 0 : name.hashCode());
			result = prime * result + ((post == null) ? 0 : post.hashCode());
			result = prime * result + ((pre == null) ? 0 : pre.hashCode());
			return result;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof Label))
				return false;
			Label other = (Label) obj;
			if (name == null) {
				if (other.name != null)
					return false;
			} else if (!name.equals(other.name))
				return false;
			if (post == null) {
				if (other.post != null)
					return false;
			} else if (!post.equals(other.post))
				return false;
			if (pre == null) {
				if (other.pre != null)
					return false;
			} else if (!pre.equals(other.pre))
				return false;
			return true;
		}
	}

	static public final String varNewSuffix = "_N",varOldSuffix="_M"; 
	
	/** Given a string with variables, renumbers them according to the number passed. This is used
	 * to generate memory and input variables corresponding to different states/labels on a path.
	 * 
	 * @param constraint what to renumber
	 * @param num how far on a path we are.
	 * @param previous the way to refer to previous values of variables (so that a postcondition can use them).
	 * @return a constraint with renumbered variables. 
	 */
	protected static String toCurrentMem(String constraint, int num, int previous)
	{
		String result = constraint;
		if (num>=0)
			result = result.replaceAll(varNewSuffix,"_"+num);
		else
			result = result.replaceAll(varNewSuffix,"_@"+(-num));

		if (previous>=0)
			result = result.replaceAll(varOldSuffix,"_"+previous);
		else
			result = result.replaceAll(varOldSuffix,"_@"+(-previous));
		
		return result;
	}

	/** Given a constraint, makes all variables in it match the current state number and appends the result
	 * to the provided string buffer.
	 * 
	 * @param what text to convert
	 * @param stateNumber state number
	 * @param oldStateNumber previous state number
	 * @return true if something was appended.
	 */
	protected static boolean addStringRepresentation(String what, int stateNumber, int oldStateNumber, StringBuffer result)
	{
		boolean outcome = false;
		if (what != null)
		{
			result.append(toCurrentMem(what,stateNumber,oldStateNumber));result.append(ENDL);
			outcome = true;
		}
		return outcome;
	}

	/** Maps names of labels to their representation - a learner does not need to know the details, albeit
	 * perhaps we should've incorporated an abstract label into our transition structure. 
	 */
	protected Map<String,Label> labelMap = new TreeMap<String,Label>();
	
	/** This one stores the text from which label descriptions have been loaded. */
	private List<String> originalText = new LinkedList<String>();
	
	public Element storeToXML(Document doc)
	{
		Element labelText = doc.createElement(StatechumXML.ELEM_LABELDETAILS.name());
		StringWriter labelDetails = new StringWriter();ProgressDecorator.writeInputSequence(labelDetails, originalText);
		labelText.setTextContent(labelDetails.toString());
		return labelText;
	}
	
	public void loadXML(Element elem)
	{
		parseLabels(ProgressDecorator.readInputSequence(new StringReader( elem.getTextContent()),-1));		
	}

	/** Given a string of text, parses it as a label. */
	public void parseLabel(String text)
	{
		if (text == null || text.length() == 0) return;// ignore empty input
		
		StringTokenizer tokenizer = new StringTokenizer(text);
		if (!tokenizer.hasMoreTokens()) return;// ignore empty input
		String labelName = tokenizer.nextToken();
		if (!tokenizer.hasMoreTokens()) throw new IllegalArgumentException("expected details for label "+labelName);
		XM_DATA kind = null;String prepost = tokenizer.nextToken();
		try
		{
			kind=XM_DATA.valueOf(prepost);
		}
		catch(IllegalArgumentException ex)
		{
			throw new IllegalArgumentException("expected "+Arrays.toString(XM_DATA.values())+" but got: "+prepost);
		}
		if (!tokenizer.hasMoreTokens()) throw new IllegalArgumentException("expected specification for label "+labelName);
		StringBuffer labelSpec = new StringBuffer(tokenizer.nextToken());
		while(tokenizer.hasMoreTokens())
		{
			labelSpec.append(' ');labelSpec.append(tokenizer.nextToken());
		}
		
		Label lbl = getLabel(labelName);
		switch(kind)
		{
		case PRE:
			lbl.pre = appendToString(labelSpec.toString(), lbl.pre);break;
		case POST:
			lbl.post = appendToString(labelSpec.toString(), lbl.post);break;
		}
		originalText.add(text);
	}

	/** Loads from a collection of strings. */
	public void parseLabels(Collection<String> data)
	{
		for(String str:data) parseLabel(str);
	}
	
	public enum XM_DATA { PRE,POST }
	
	/** Extracts a label; if it does not exist, creates a new one.
	 *  
	 * @param name label name to use
	 * @param extracted or newly-create label. 
	 */
	protected Label getLabel(String name)
	{
		Label lbl = labelMap.get(name);
		if (lbl == null)
		{
			lbl = new Label(name);labelMap.put(lbl.getName(),lbl);
		}
		return lbl;
	}
	
	public LabelRepresentation()
	{
	}
	
	/** Whenever we generate paths, new variables have to be introduced. By incrementing this variable,
	 * new ones can be generated.
	 */
	protected int currentNumber = 0;
	
	public static final String commentForNewSeq = ";; sequence", commentForLabel = ";; label ", commentForTransition = ";; transition ",assertString="(assert ";
	public static final char ENDL = '\n';
	
	/** Given a path in a graph returns an expression which can be used to check whether that path can be followed. 
	 * Note that if a path cannot be followed, this means that either the precondition is not satisfied or that
	 * a postcondition cannot be satisfied.
	 * <p>
	 * The supplied path cannot contain newlines.
	 */
	public synchronized AbstractState getConjunctionForPath(List<String> path)
	{
		StringBuffer axiom = new StringBuffer(), varDeclaration = new StringBuffer();
		axiom.append(commentForNewSeq);axiom.append(path);axiom.append(ENDL);

		// Initial memory value.
		Label init = labelMap.get(INITMEM);if (init == null) throw new IllegalArgumentException("missing initial memory value");
		
		// We always have to add variable declarations (even for empty paths) because
		// they may be used in operations involving the abstract state we are returning here.
		for(int i=0;i<=path.size();++i) 
			if (init.pre != null)
			{
				varDeclaration.append(toCurrentMem(init.pre, i+currentNumber, i+currentNumber));varDeclaration.append(ENDL);
			}

		boolean pathNotEmpty = false;

		// Now walk through the path generating constraints.
		axiom.append("(and");axiom.append(ENDL);

		pathNotEmpty |= addStringRepresentation(init.post,currentNumber,currentNumber,axiom);

		int i=0;
		Label lastLabel = null;
		for(String lbl:path) 
		{
			Label currentLabel=labelMap.get(lbl);if (currentLabel == null) throw new IllegalArgumentException("unknown label "+lbl);
			lastLabel = currentLabel;
			axiom.append(commentForLabel+currentLabel.getName());axiom.append(ENDL);
			int previousNumber = i+currentNumber;++i;
			pathNotEmpty |= addStringRepresentation(currentLabel.pre,previousNumber,previousNumber,axiom);
			pathNotEmpty |= addStringRepresentation(currentLabel.post,i+currentNumber,previousNumber,axiom);
		}
		axiom.append(")");
		int lastValueNumber=currentNumber+path.size();
		currentNumber+=path.size()+1;
		
		axiom.append(ENDL);
		return new AbstractState(lastValueNumber, varDeclaration.toString(), pathNotEmpty?axiom.toString():commentForNewSeq+path+ENDL+"true"+ENDL,lastLabel);
	}
	
	public static String getAssertionFromAbstractState(AbstractState state)
	{
		return state.variableDeclarations+assertString+state.abstractState+")"+ENDL;
	}
	
	protected Map<VertexID,AbstractState> idToState = new TreeMap<VertexID,AbstractState>();

	public static class AbstractState
	{
		/** Axiom so far. */
		public final String abstractState;
		
		/** Variable declarations to support the axiom. */
		public final String variableDeclarations;
		
		/** The number corresponding to this abstract state. */
		public final int stateNumber;

		/** The transition leading to this state, making it possible to compute a postcondition
		 * for an arbitrarily-chosen abstract state number. 
		 * This would be null for an initial state. 
		 */
		public final Label lastLabel;
		
		public AbstractState(int number, String varDecl, String axiom,Label arglastLabel)
		{
			stateNumber=number;variableDeclarations=varDecl;abstractState=axiom;lastLabel=arglastLabel;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((abstractState == null) ? 0 : abstractState.hashCode());
			result = prime * result
					+ ((lastLabel == null) ? 0 : lastLabel.hashCode());
			result = prime * result + stateNumber;
			result = prime
					* result
					+ ((variableDeclarations == null) ? 0
							: variableDeclarations.hashCode());
			return result;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof AbstractState))
				return false;
			AbstractState other = (AbstractState) obj;
			if (abstractState == null) {
				if (other.abstractState != null)
					return false;
			} else if (!abstractState.equals(other.abstractState))
				return false;
			if (lastLabel == null) {
				if (other.lastLabel != null)
					return false;
			} else if (!lastLabel.equals(other.lastLabel))
				return false;
			if (stateNumber != other.stateNumber)
				return false;
			if (variableDeclarations == null) {
				if (other.variableDeclarations != null)
					return false;
			} else if (!variableDeclarations.equals(other.variableDeclarations))
				return false;
			return true;
		}
	}

	/** Given PTA computes a path which can be used as an axiom in later computations. 
	 * In addition to the construction of axioms, the method populates the map associating
	 * vertex identifiers to abstract states which would be reached in the supplied graph
	 * when those states are reached. 
	 */
	public String constructPathAxioms(LearnerGraph gr)
	{
		final Label init = labelMap.get(INITMEM);if (init == null) throw new IllegalArgumentException("missing initial memory value");
		PTASequenceEngine engine = new PTASequenceEngine();
		engine.init(gr.new NonExistingPaths() {
			@Override
			public boolean shouldBeReturned(Object currentState) {
				assert !nonExistingVertices.contains(currentState) : "by construction of a tree, all paths should exist";
				return ((CmpVertex)currentState).isAccept();
			}});
		SequenceSet pathsToAllStates=engine.new SequenceSet();pathsToAllStates.setIdentity();
		gr.pathroutines.computePathsSBetween_All(gr.init, engine, pathsToAllStates);		
		
		final StringBuffer variableDeclarations = new StringBuffer(), lemmas = new StringBuffer();
		PTAExploration<Integer> exploration = new PTAExploration<Integer>(engine) 
		{
			@Override
			public Integer newUserObject() 
			{
				return 0;
			}

			@Override
			public void nodeEntered(PTAExplorationNode currentNode, LinkedList<PTAExplorationNode> pathToInit)
			{
				handleNode(currentNode, pathToInit);
			}
			
			@Override
			public void leafEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				handleNode(currentNode, pathToInit);
			}
			
			public void handleNode(PTAExplorationNode currentNode, LinkedList<PTAExplorationNode> pathToInit) 
			{
				// Now we generate path axioms using state IDs 
				
				if (!pathToInit.isEmpty())
				{// a state which is a continuation of an existing path.
					PTAExplorationNode prevNode = pathToInit.getFirst();
					assert prevNode.shouldBeReturned() : "there should be no transitions after a reject state";
					
					Label currentLabel=labelMap.get(prevNode.getInput());if (currentLabel == null) throw new IllegalArgumentException("unknown label "+prevNode.getInput());
					Iterator<PTAExplorationNode> pathToInitIter = pathToInit.iterator();LinkedList<String> labelsFromInit = new LinkedList<String>();
					pathToInitIter.next();// trash the last element
					for(int i=0;i<pathToInit.size()-1;++i)
						labelsFromInit.addFirst(pathToInitIter.next().getInput());
					AbstractState prevState = getConjunctionForPath(labelsFromInit);

					final int currentStateNumber = currentNumber;
					variableDeclarations.append(prevState.variableDeclarations);
					variableDeclarations.append(toCurrentMem(init.pre, currentStateNumber, currentStateNumber));variableDeclarations.append(ENDL);
					
					String textPre = "", textPost = "";
					if (currentLabel.pre != null)
						textPre = " "+toCurrentMem(currentLabel.pre,prevState.stateNumber,prevState.stateNumber);
					if (currentLabel.post != null)
						textPost = " "+toCurrentMem(currentLabel.post,currentStateNumber, prevState.stateNumber);
					++currentNumber;
					
					if (currentNode.shouldBeReturned())
					{// a path to the current state exists
						if (currentLabel.pre != null || currentLabel.post != null)
						{
							// now we need to add a condition that p => pre next and post next
							lemmas.append(commentForTransition+prevNode.getState()+"("+(prevState.stateNumber)+")-"+currentLabel.getName()+"->"+
									currentNode.getState()+"("+currentStateNumber+")");lemmas.append(ENDL);
							
							lemmas.append(assertString);lemmas.append("(implies");lemmas.append(ENDL);lemmas.append(prevState.abstractState);
							lemmas.append("\t(and");lemmas.append(textPre);lemmas.append(textPost);lemmas.append(")))");lemmas.append(ENDL);
						}
						else
						{
							if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.SMTWARNINGS)))
							{// if a precondition is empty, we cannot really generate a useful lemma, hence ignore this case but give a warning.
								System.out.println("label "+currentLabel+" does not have a pre- or post-condition");
							}
						}
					}
					else
					{// the transition to the last node cannot be taken.
						if (currentLabel.pre != null)
						{
							// now we need to add a condition that p => ¬pre next
							lemmas.append(commentForTransition+prevNode.getState()+"("+(prevState.stateNumber)+")-"+currentLabel.getName()+"-#"+
									currentNode.getState()+"("+currentStateNumber+")");lemmas.append(ENDL);
									
							lemmas.append(assertString);lemmas.append("(implies");lemmas.append(ENDL);lemmas.append(prevState.abstractState);
							lemmas.append("\t(not");lemmas.append(textPre);lemmas.append(")))");lemmas.append(ENDL);
						} 
						else
							if (Boolean.valueOf(GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.SMTWARNINGS)))
							{// if a precondition is empty, we cannot really generate a useful lemma, hence ignore this case but give a warning.
								System.out.println("label "+currentLabel+" leading to a reject-state does not have a precondition hence it is not possible to generate a lemma");
							}
					}
				}
			}

			@Override
			public void nodeLeft(
						@SuppressWarnings("unused") PTAExplorationNode currentNode,
						@SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit) 
			{
				// this is needed to implement an interface, but we only care if a leaf is entered.
			}
			
		};
		exploration.walkThroughAllPaths();
		StringBuffer outcome = new StringBuffer(variableDeclarations);outcome.append(lemmas);
		outcome.append(";; END OF PATH AXIOMS");outcome.append(ENDL);
		return outcome.toString();
	}

	/** Given PTA constructs a map associating every vertex to the corresponding
	 * abstract state.
	 */
	public void mapVerticesToAbstractStates(LearnerGraph gr)
	{
		final Label init = labelMap.get(INITMEM);if (init == null) throw new IllegalArgumentException("missing initial memory value");
		PTASequenceEngine engine = new PTASequenceEngine();
		engine.init(gr.new NonExistingPaths() {
			@Override
			public boolean shouldBeReturned(Object currentState) {
				assert !nonExistingVertices.contains(currentState) : "by construction of a tree, all paths should exist";
				return ((CmpVertex)currentState).isAccept();
			}});
		SequenceSet pathsToAllStates=engine.new SequenceSet();pathsToAllStates.setIdentity();
		gr.pathroutines.computePathsSBetween_All(gr.init, engine, pathsToAllStates);		
		
		PTAExploration<Integer> exploration = new PTAExploration<Integer>(engine) 
		{
			
			@Override
			public Integer newUserObject() 
			{
				return 0;
			}

			@Override
			public void nodeEntered(PTAExplorationNode currentNode, LinkedList<PTAExplorationNode> pathToInit)
			{
				handleNode(currentNode, pathToInit);
			}
			
			@Override
			public void leafEntered(PTAExplorationNode currentNode,	LinkedList<PTAExplorationNode> pathToInit) 
			{
				handleNode(currentNode, pathToInit);
			}
			
			public void handleNode(PTAExplorationNode currentNode, LinkedList<PTAExplorationNode> pathToInit) 
			{
				LinkedList<String> fullPath = new LinkedList<String>();
				for(PTAExplorationNode node:pathToInit)	fullPath.addFirst(node.getInput());

				updateMap(((CmpVertex)currentNode.getState()).getID(),fullPath);
			}

			@Override
			public void nodeLeft(
						@SuppressWarnings("unused") PTAExplorationNode currentNode,
						@SuppressWarnings("unused")	LinkedList<PTAExplorationNode> pathToInit) 
			{
				// this is needed to implement an interface, but we only care if a leaf is entered.
			}
			
		};
		exploration.walkThroughAllPaths();
	}

	/** Given an id and a path, updates the corresponding map. */
	final void updateMap(VertexID id, List<String> path)
	{
		assert !idToState.containsKey(id);
		AbstractState currentAbstractState = getConjunctionForPath(path);
		idToState.put(id,currentAbstractState);
	}
	
	/** Given a path from an initial state, this method updates the map from identifiers to 
	 * the corresponding abstract states. If <em>solver</em> is not null, this method also checks that 
	 * all abstract states encountered are satisfiable.
	 * 
	 * @param pathFromInit path to follow.
	 * @param isAccept whether the path should end at accept-state or reject-state.
	 */
	public void AugmentAbstractStates(Smt solver,List<String> pathFromInit, LearnerGraph graph, boolean isAccept)
	{
		CmpVertex vertex = graph.init;
		List<String> currentPath = new LinkedList<String>();
		if (!idToState.containsKey(vertex.getID())) 
		{	
			updateMap(vertex.getID(), currentPath);
			if (solver != null)
			{
				if ( (isAccept || pathFromInit.size() > currentPath.size()) && !checkSatisfiability(solver, vertex.getID()))
					throw new IllegalArgumentException("state "+vertex+" has an unsatisfiable abstract state");
			}
		}

		for(String input:pathFromInit)
		{
			vertex = graph.transitionMatrix.get(vertex).get(input);
			currentPath.add(input);
			if (!idToState.containsKey(vertex.getID()))
			{
				updateMap(vertex.getID(), currentPath);
				if (solver != null) 
					if ( (isAccept || pathFromInit.size() > currentPath.size()) && !checkSatisfiability(solver, vertex.getID()))
						throw new IllegalArgumentException("state "+vertex+" has an unsatisfiable abstract state");
			}
		}
	}
	
	/** Checks if abstract states corresponding to the supplied vertices are compatible,
	 * assuming that abstract states are themselves satisfiable. 
	 */
	public boolean abstractStatesCompatible(Smt solver,VertexID a, VertexID b)
	{
		AbstractState A=idToState.get(a),B=idToState.get(b);
		if (A.lastLabel == null && B.lastLabel == null)
			return true;// both states are initial states.
		
		if (B.lastLabel == null)
		{// If both states are not null, we make B's last label not null by swapping it with A
			AbstractState C = A;A = B;B = C;
		}
		
		solver.pushContext();

		String text=A.variableDeclarations+B.variableDeclarations+
			assertString+A.abstractState+')'+ENDL+
			assertString+B.abstractState+')'+ENDL
			+";; now check that the two states can be equal"+ENDL+
			assertString+"(and"+ENDL+
			toCurrentMem(B.lastLabel.post,A.stateNumber,B.stateNumber-1)+ENDL+
			"))";

		solver.loadData(text);
		boolean outcome = solver.check();
		solver.popContext();
		return outcome;
	}
	
	/** Checks that all states correspond to satisfiable abstract states.
	 * @throws IllegalArgumentException if any state cannot be reached. 
	 */
	public void checkAllStatesExist(Smt solver) throws IllegalArgumentException
	{
		for(Entry<VertexID,AbstractState> entry:idToState.entrySet())
			if (!checkSatisfiability(solver, entry.getKey()))
					throw new IllegalArgumentException("state "+entry.getKey()+" has an unsatisfiable abstract state");
	}

	/** Checks if a path condition corresponding to an abstract state is satisfiable.
	 * @return false if a path leading to the supplied state is not satisfiable. 
	 */
	protected boolean checkSatisfiability(Smt solver, VertexID state)
	{
		solver.pushContext();
		solver.loadData(LabelRepresentation.getAssertionFromAbstractState(idToState.get(state)));
		boolean outcome = solver.check();
		solver.popContext();return outcome;		
	}
	
	/** Extracts an ID of a supplied vertex. */
	public static VertexID getID(CmpVertex vertex)
	{
		if (vertex.getOrigState() != null) return vertex.getOrigState();
		return vertex.getID();
	}
	
	/** Similar to CheckWithEndUser, but works via SMT. 
	 */
	public int CheckWithEndUser(Smt solver,List<String> question)
	{
		int pos = -1;
		List<String> partialPath = new LinkedList<String>();
		for(String label:question)
		{
			++pos;
			partialPath.add(label);
			solver.pushContext();
			solver.loadData(LabelRepresentation.getAssertionFromAbstractState(getConjunctionForPath(partialPath)));
			boolean outcome = solver.check();
			solver.popContext();if (!outcome) return pos;
		}
		return AbstractOracle.USER_ACCEPTED;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() 
	{
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((labelMap == null) ? 0 : labelMap.hashCode());
		result = prime * result 
				+ ((idToState == null)? 0 : idToState.hashCode());
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) 
	{
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof LabelRepresentation))
			return false;
		LabelRepresentation other = (LabelRepresentation) obj;
		if (!labelMap.equals(other.labelMap))
			return false;
		
		if (idToState == null)
		{
			if (other.idToState != null)
				return false;
		}
		else
			if (!idToState.equals(other.idToState))
				return false;

		return true;
	}
}

