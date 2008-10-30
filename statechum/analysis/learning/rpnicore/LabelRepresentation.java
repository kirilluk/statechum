/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeMap;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import cern.colt.Arrays;

import statechum.GlobalConfiguration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.observers.ProgressDecorator.ELEM_KINDS;
import statechum.model.testset.PTAExploration;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

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
	 */
	protected static void addStringRepresentation(String what, int stateNumber, int oldStateNumber, StringBuffer result)
	{
		if (what != null)
		{
			result.append(assertString);result.append(ENDL);
			result.append(toCurrentMem(what,stateNumber,oldStateNumber));result.append(ENDL);
			result.append(')');result.append(ENDL);
		}
	}

	/** Maps names of labels to their representation - a learner does not need to know the details, albeit
	 * perhaps we should've incorporated an abstract label into our transition structure. 
	 */
	protected Map<String,Label> labelMap = new TreeMap<String,Label>();
	
	/** This one stores the text from which label descriptions have been loaded. */
	private List<String> originalText = new LinkedList<String>();
	
	public Element storeToXML(Document doc)
	{
		Element labelText = doc.createElement(ELEM_KINDS.ELEM_LABELDETAILS.name());
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
	
	public static final String commentForNewSeq = ";; new sequence", commentForLabel = ";; label ", assertString="(assert ";
	public static final char ENDL = '\n';
	
	/** Given a path in a graph returns an expression which can be used to check whether that path can be followed. 
	 * Note that if a path cannot be followed, this means that either the precondition is not satisfied or that
	 * a postcondition cannot be satisfied.
	 * <p>
	 * The supplied path cannot contain newlines.
	 */
	public String getConjunctionForPath(List<String> path)
	{
		StringBuffer result = new StringBuffer();
		result.append(commentForNewSeq);result.append(path);result.append(ENDL);

		// Initial memory value.
		Label init = labelMap.get(INITMEM);if (init == null) throw new IllegalArgumentException("missing initial memory value");

		for(int i=0;i<=path.size();++i) 
			if (init.pre != null)
			{
				result.append(toCurrentMem(init.pre, i+currentNumber, i+currentNumber));result.append(ENDL);
			}
		
		addStringRepresentation(init.post,currentNumber,currentNumber,result);
		// Now walk through the path generating constraints.
		for(String lbl:path) 
		{
			Label currentLabel=labelMap.get(lbl);if (currentLabel == null) throw new IllegalArgumentException("unknown label "+lbl);
			result.append(commentForLabel+currentLabel.getName());result.append(ENDL);
			int previousNumber = currentNumber;++currentNumber;
			addStringRepresentation(currentLabel.pre,previousNumber,previousNumber,result);
			addStringRepresentation(currentLabel.post,currentNumber,previousNumber,result);
		}
		return result.toString();
	}
	
	
	/** Given PTA computes a path which can be used as a lemma in later computations. */
	public String getLemma(LearnerGraph gr)
	{
		final Label init = labelMap.get(INITMEM);if (init == null) throw new IllegalArgumentException("missing initial memory value");
		PTASequenceEngine engine = new PTASequenceEngine();
		engine.init(gr.new NonExistingPaths() {
			@Override
			public boolean shouldBeReturned(Object currentState) {
				assert currentState != junkVertex : "by construction of a tree, all paths should exist";
				return ((CmpVertex)currentState).isAccept();
			}});
		SequenceSet pathsToAllStates=engine.new SequenceSet();pathsToAllStates.setIdentity();
		gr.paths.computePathsSBetween_All(gr.init, engine, pathsToAllStates);

		class PathConditionForState
		{
			/** Lemma so far. */
			public String pathCondition = null;
			/** The number of the previous memory state. */
			public final int stateNumber;
			
			public PathConditionForState(int number)
			{
				stateNumber=number;
			}
		}
		
		final StringBuffer variableDeclarations = new StringBuffer(), lemmas = new StringBuffer();
		PTAExploration<PathConditionForState> exploration = new PTAExploration<PathConditionForState>(engine) {
			
			/** The number reflecting the current state being processed. */
			int currNumber = -1;
			
			@Override
			public PathConditionForState newUserObject() {
				PathConditionForState result = new PathConditionForState(currNumber);
				variableDeclarations.append(toCurrentMem(init.pre, currNumber,currNumber));variableDeclarations.append(ENDL);
				--currNumber; 
				return result;
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
			// we need to add a lemma for a current element of a path 
			// to the collection of lemmas on paths to this element.
			
				if (pathToInit.isEmpty())
				{// generate a condition for an initial state.
					currentNode.userObject.pathCondition =  (init.post == null?"":toCurrentMem(init.post,currentNode.userObject.stateNumber,currentNode.userObject.stateNumber));
				}
				else
				{// a state which is a continuation of an existing path.
					PTAExplorationNode prevNode = pathToInit.getFirst();
					assert prevNode.shouldBeReturned() : "there should be no transitions after a reject state";
					
					Label currentLabel=labelMap.get(prevNode.getInput());if (currentLabel == null) throw new IllegalArgumentException("unknown label "+prevNode.getInput());
					StringBuffer pathToPreLemma = new StringBuffer("(implies ");pathToPreLemma.append(prevNode.userObject.pathCondition);

					String textPre = "", textPost = "";
					if (currentLabel.pre != null)
						textPre = " "+toCurrentMem(currentLabel.pre,currentNode.userObject.stateNumber, currentNode.userObject.stateNumber);
					if (currentLabel.post != null)
						textPost = " "+toCurrentMem(currentLabel.post,currentNode.userObject.stateNumber, prevNode.userObject.stateNumber);

					if (currentNode.shouldBeReturned())
					{// a path to the current state exists
						if (currentLabel.pre != null || currentLabel.post != null)
						{
							currentNode.userObject.pathCondition = prevNode.userObject.pathCondition+textPre+textPost;

							// now we need to add a condition that p => pre next and post next
							lemmas.append(commentForLabel+"from @"+(-prevNode.userObject.stateNumber)+"-"+currentLabel.getName()+"->@"+(-currentNode.userObject.stateNumber));lemmas.append(ENDL);
							lemmas.append(assertString);lemmas.append("(implies (and ");lemmas.append(prevNode.userObject.pathCondition);
							lemmas.append(")");lemmas.append(ENDL);lemmas.append(" (and ");lemmas.append(textPre);lemmas.append(textPost);lemmas.append("))");lemmas.append(ENDL);
						}
						else
						{
							currentNode.userObject.pathCondition = prevNode.userObject.pathCondition; // unchanged path condition.
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
							lemmas.append(commentForLabel+"from @"+(-prevNode.userObject.stateNumber)+"-"+currentLabel.getName()+"-#@"+(-currentNode.userObject.stateNumber));lemmas.append(ENDL);
							lemmas.append(assertString);lemmas.append("(implies (and ");lemmas.append(prevNode.userObject.pathCondition);
							lemmas.append(")");lemmas.append(ENDL);lemmas.append(" (not ");lemmas.append(textPre);lemmas.append("))");lemmas.append(ENDL);
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
		outcome.append(";; END OF LEMMAS");outcome.append(ENDL);
		return outcome.toString();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((labelMap == null) ? 0 : labelMap.hashCode());
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
		if (!(obj instanceof LabelRepresentation))
			return false;
		LabelRepresentation other = (LabelRepresentation) obj;
		if (!labelMap.equals(other.labelMap))
			return false;
		return true;
	}
}

