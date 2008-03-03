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

package statechum.analysis.learning;

import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.algorithms.shortestpath.*;

public class RPNIBlueFringeLearner extends Observable {
	protected Graph currentGraph = DeterministicDirectedSparseGraph.initialise();
	protected HashSet doneEdges;
	protected Collection<List<String>> sPlus, sMinus;
	protected int questionCounter = 0;
	
	protected final Configuration config;
	
	/** Retrieves the configuration used by this learner. */
	public Configuration getConfig()
	{
		return config;
	}
	
	/** Given a score, we need to determine whether to ask questions. This depends on a number
	 * of configuration parameters.
	 * 
	 * @param score score we are looking at
	 * @return whether any questions should be asked or we could just proceed with merging.
	 */ 
	protected boolean shouldAskQuestions(int score)
	{
		if (!config.getAskQuestions())
			return false;
		
		if (config.getCertaintyThreshold() >= 0 && score > config.getCertaintyThreshold())
			return false;
		
		if (score < config.getMinCertaintyThreshold())
			return false;
		
		return true;
	}
	
	public static Collection<String> getAlphabetForEdges(Collection<Edge> edges){
		HashSet<String> alphabet = new HashSet<String>();
		Iterator<Edge> edgeIt = edges.iterator();
		while(edgeIt.hasNext()){
			Edge e = (edgeIt.next());
			alphabet.addAll((Collection)e.getUserDatum(JUConstants.LABEL));
		}
		return alphabet;
	}
	
	public Collection<List<String>> getSMinus() {
		return sMinus;
	}

	public Collection<List<String>> getSPlus() {
		return sPlus;
	}
	

	protected Graph removeNegatives(Graph g){
		Iterator vertexIt = g.getVertices().iterator();
		HashSet remove = new HashSet();
		while(vertexIt.hasNext()){
			Vertex v = (Vertex)vertexIt.next();
			if(!DeterministicDirectedSparseGraph.isAccept(v)&&!DeterministicDirectedSparseGraph.isInitial(v))
				remove.add(v);
		}
		Iterator<Vertex> removeIt = remove.iterator();
		while(removeIt.hasNext()){
			g.removeVertex(removeIt.next());
		}
		return g;
	}
	

	/** Updates listeners only if this object has been modified and debug mode is on, by calling
	 * <pre>
	 * setChanged()
	 * </pre>
	 * @param g the graph to display in the associated view
	 */
	public void updateGraph(Graph g)
	{
		if (config.getDebugMode())
			notifyObservers(g);
	}
	
	protected Frame parentFrame;

	public RPNIBlueFringeLearner(Frame parent, Configuration c){
		config = c;
		this.parentFrame = parent;
	}
	
	/** Takes all red-labelled nodes; non-red nodes which can be reached by a single transition from any 
	 * red one is labelled blue.
	 * 
	 * @param model
	 * @return the set of blue nodes.
	 */ 
	protected Set<Vertex> computeBlue(DirectedSparseGraph model){
		Set<Vertex> blues = new HashSet<Vertex>();
		for(Vertex v: DeterministicDirectedSparseGraph.findVertices(JUConstants.COLOUR, JUConstants.RED, model))
		{
			Iterator<DirectedSparseEdge>neighbourIt = v.getOutEdges().iterator();
			while(neighbourIt.hasNext()){
				DirectedSparseEdge next = neighbourIt.next();
				Vertex neighbour = next.getDest();
				JUConstants neighbourColour = (JUConstants)neighbour.getUserDatum(JUConstants.COLOUR);
				if(neighbourColour!=null){
					if(neighbourColour == JUConstants.RED)
						continue;
				}
				neighbour.setUserDatum(JUConstants.COLOUR, JUConstants.BLUE, UserData.SHARED);
				blues.add(neighbour);
				
			}
		}
		//updateGraph(model);
		return blues;
	}

	public static class OrigStatePair implements Comparable<OrigStatePair> {
		
		private Vertex q, r;
		
		public OrigStatePair(Vertex blue, Vertex red){
			this.q = blue;
			this.r = red;
		}
		
		private static String strLabel(Vertex v){
			String vLabel = v.getUserDatum(JUConstants.LABEL).toString();
			return vLabel;
		}
		
		public int compareTo(OrigStatePair pB){
			int qLabels = strLabel(q).compareTo(strLabel(pB.getQ()));
			int rLabels = strLabel(r).compareTo(strLabel(pB.getR()));
			
			if(qLabels != 0)
				return qLabels; 
			return rLabels;
		}
		
		public Vertex getQ(){
			return q;
		}
		
		public Vertex getR(){
			return r;
		}
		
		public String toString(){
			return "[ "+((q == null)?"NULL":q.getUserDatum(JUConstants.LABEL))+", "+((r == null)?"NULL":r.getUserDatum(JUConstants.LABEL))+" ]";
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode(){
			final int PRIME = 31;
			return q.getUserDatum(JUConstants.LABEL).hashCode()+PRIME*r.getUserDatum(JUConstants.LABEL).hashCode();
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object o){
			if(o == null)
				return false;
			if(o instanceof OrigStatePair){
				OrigStatePair other = (OrigStatePair)o;
				Object otherQ = (Object)other.getQ().getUserDatum(JUConstants.LABEL);
				Object otherR = (Object)other.getR().getUserDatum(JUConstants.LABEL);
				Object thisQ = (Object)q.getUserDatum(JUConstants.LABEL);
				Object thisR = (Object)r.getUserDatum(JUConstants.LABEL);
				if(thisQ.equals(otherQ)&&thisR.equals(otherR))
					return true;
			}
			return false;
		}
	}	
	
	/** Makes a copy of the graph given to it and merges states in the pair supplied. */
	protected DirectedSparseGraph mergeAndDeterminize(Graph model, OrigStatePair pair){
		Graph original = (Graph)model.copy();
		Vertex q = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, pair.getQ().getUserDatum(JUConstants.LABEL),original);
		Vertex qDash = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, pair.getR().getUserDatum(JUConstants.LABEL),original);
		pair = new OrigStatePair(q,qDash);
		DirectedSparseGraph temp = merge((DirectedSparseGraph)original, pair);
		OrigStatePair mergable = findMergablePair(temp);
		while(mergable!=null){
			temp=merge(temp, mergable);
			mergable = findMergablePair(temp);
		}
		return temp;
	}
	
	protected DirectedSparseGraph createAugmentedPTA(DirectedSparseGraph model, Collection<List<String>> sPlus, Collection<List<String>> sMinus){
		model = augmentPTA(model, sMinus, false);
		model = augmentPTA(model, sPlus, true);
		DeterministicDirectedSparseGraph.numberVertices(model);
		return model;
	}
	
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model, Collection<List<String>> sPlus, Collection<List<String>> sMinus) {
		this.sPlus = sPlus;
		this.sMinus = sMinus;
		model = createAugmentedPTA(model, sPlus, sMinus);
		
		Vertex init = DeterministicDirectedSparseGraph.findInitial(model);
		init.setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
		setChanged();
		Stack possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		while(!possibleMerges.isEmpty()){
			OrigStatePair pair = (OrigStatePair)possibleMerges.pop();

			DirectedSparseGraph temp = mergeAndDeterminize((Graph)model.copy(), pair);
			if(compatible(temp, sPlus, sMinus)){// KIRR: the should always return true
				pair.getQ().setUserDatum(JUConstants.HIGHLIGHT, pair, UserData.SHARED);
				pair.getR().setUserDatum(JUConstants.HIGHLIGHT, pair, UserData.SHARED);
				setChanged();
				List<List<String>> questions;
				if(config.getAskQuestions())
					questions = generateQuestions(model, pair);
				else
					questions = new ArrayList<List<String>>();
				questions = trimSet(questions);
				Iterator<List<String>> questionIt = questions.iterator();
				while(questionIt.hasNext()){
					List<String> question = questionIt.next();
					boolean accepted = DeterministicDirectedSparseGraph.isAccept(pair.getQ());// Q is the blue vertex
					updateGraph(model);
					int response = checkWithEndUser(model,question,new Object[0]);// zero means "yes", everything else is "no"
					questionCounter++;
					pair.getQ().removeUserDatum(JUConstants.HIGHLIGHT);
					pair.getR().removeUserDatum(JUConstants.HIGHLIGHT);
					if(response == USER_ACCEPTED){
						sPlus.add(question);
						System.out.println(setByAuto+question+ " <yes>");
						if(accepted == false)// KIRR: how can this be true? If it were so, there would be no questions to ask
							return learnMachine(DeterministicDirectedSparseGraph.initialise(), sPlus, sMinus);
					}
					else{
						sMinus.add(question.subList(0, response));
						System.out.println(setByAuto+question+ " <no>");
						if(accepted == true){// KIRR: this cannot be false either
							return learnMachine(DeterministicDirectedSparseGraph.initialise(), sPlus, sMinus);
						}
					}
				}
				model = temp;
			}
			possibleMerges = chooseStatePairs(model, sPlus, sMinus);
			
			updateGraph(model);
		}
		System.out.println("finished");
		return model;
	}
	
	protected boolean compatible(DirectedSparseGraph model, Collection<List<String>> sPlus, Collection<List<String>> sMinus){
		boolean returnValue = true;
		for(List<String> string:sMinus)
		{
			Vertex v = getVertex(model,string);
			if(v != null){
				if(DeterministicDirectedSparseGraph.isAccept(v))
					returnValue = false;
			}
		}
		for(List<String> string:sPlus)
		{
			Vertex v = getVertex(model,string);
			if(v == null)
				returnValue = false;
			else{
				if(!DeterministicDirectedSparseGraph.isAccept(v))
					returnValue = false;
			}
		}
		return returnValue;
	}
	
	/** Removes question sequences which are prefixes of other questions.
	 * 
	 * @param questions
	 * @return a set of questions which are not prefixes of other questions.
	 */
	protected List<List<String>> trimSet(List<List<String>> questions){
		Set<String> done = new HashSet<String>();
		List<List<String>> trimmedSet = new ArrayList<List<String>>();
		Iterator<List<String>> questionIt = questions.iterator();
		while(questionIt.hasNext()){
			List<String> question = questionIt.next();
			Iterator<String> stringIt = question.iterator();
			String questionString=new String();
			while(stringIt.hasNext())
				questionString = questionString.concat("]["+stringIt.next());
			if(!done.contains(questionString.trim())){
				done.add(questionString.trim());
				trimmedSet.add(question);
			}
		}
		return trimmedSet;
	}

	final String questionPrefix="<html><font color=green>";
	
	protected List<String> getShortenedQuestion(List<String> question){
		List<String> questionList = new LinkedList<String>();
		assert(question.size()>=1);
		int counter=1;
		Iterator<String> questionIter = question.iterator();
		String lastQuestion = questionIter.next();
		
		while(questionIter.hasNext())
		{
				String current = questionIter.next();
				if(current.equals(lastQuestion))
					counter++;
				else{
					questionList.add(lastQuestion.concat( (counter>1)? "(*"+counter+")":""));// in the string case, I could add "\n" at the end
					counter = 1;lastQuestion = current;					
				}
		}
		questionList.add(lastQuestion.concat( (counter>1)? "(*"+counter+")":""));
		return questionList;
	}

	protected List<String> beautifyQuestionList(List<String> question)
	{
		List<String> questionList = new LinkedList<String>();
		int i=0;
		for(String q:question)
				questionList.add(questionPrefix+"("+i++ +") "+q);
		
		return questionList;
	}
	
	/** The dialog to be displayed to a user with questions to select. */
	protected JDialog dialog = null;
	
	/** the option pane. */
	protected JOptionPane jop = null;	
	
	/** Cancels a dialog, if present. With no dialog, learner thread will terminate within a reasonable amount of time.
	 */
	public void terminateLearner()
	{
		assert(SwingUtilities.isEventDispatchThread());
		if (dialog != null && jop != null && dialog.isVisible())
			jop.setValue(new Integer(
                    JOptionPane.CLOSED_OPTION));// from http://java.sun.com/docs/books/tutorial/uiswing/components/examples/CustomDialog.java		}
	}
	
	/** Stores recorded answers. */
	protected AbstractOracle ans = null;
	
	/** Makes it possible to answer questions automatically.
	 *  
	 * @param a the class holding stored answers.
	 */
	public void setAnswers(AbstractOracle a)
	{
		ans = a;
	}
	
	public static final int USER_CANCELLED = -2;
	public static final int USER_ACCEPTED = -3;
	public static final int USER_WAITINGFORSELECTION = -1;
	
	public final static String QUESTION_AUTO = "<auto> "; 
	protected String setByAuto = "";
	
	protected int processAnswer(List<String> question){
		return ans.getAnswer(question);
	}
	
	protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions){
		if (ans != null)
		{
			int AutoAnswer = processAnswer(question);
			if (AutoAnswer != USER_CANCELLED)
			{
				setByAuto = QUESTION_AUTO;
				return AutoAnswer;
			}
			else
				setByAuto = "";
		}
		updateGraph(model);
		final List<String> questionList = beautifyQuestionList(question);
		final AtomicInteger answer = new AtomicInteger(USER_WAITINGFORSELECTION);
		
		try {
			SwingUtilities.invokeAndWait(new Runnable() {
				public void run() {
					final Object[] options = new Object[1+moreOptions.length];
					final JList nonrejectElements = new JList(new String[] { "<html><font color=gray>a","<html><font color=gray>b"});
					final JList rejectElements = new JList(questionList.toArray());
					options[0]="Accept";System.arraycopy(moreOptions, 0, options, 1, moreOptions.length);
					final JLabel label = new JLabel("<html><font color=red>Click on the first non-accepting element below", JLabel.CENTER);
					jop = new JOptionPane(new Object[] {label,nonrejectElements,rejectElements},
			                JOptionPane.QUESTION_MESSAGE,JOptionPane.YES_NO_CANCEL_OPTION,null,options, options[0]);
					dialog = new JDialog(parentFrame,"Valid input string?",false);
					dialog.setContentPane(jop);
					
					// the following chunk is partly from http://java.sun.com/docs/books/tutorial/uiswing/components/dialog.html
					dialog.setDefaultCloseOperation(
						    WindowConstants.DO_NOTHING_ON_CLOSE);
					dialog.addWindowListener(new WindowAdapter() {
					    public void windowClosing(@SuppressWarnings("unused") WindowEvent we) {
					    	jop.setValue(new Integer(
                                    JOptionPane.CLOSED_OPTION));// from http://java.sun.com/docs/books/tutorial/uiswing/components/examples/CustomDialog.java
					    }
					});
					jop.addPropertyChangeListener(new PropertyChangeListener() {
				        public void propertyChange(PropertyChangeEvent e) {
				            String prop = e.getPropertyName();
				            
							Object value = e.getNewValue();

							if (dialog.isVisible() && e.getSource() == jop
					            		 && (prop.equals(JOptionPane.VALUE_PROPERTY))) 
							{
								int i = 0;for(;i < options.length && options[i] != value;++i);
									if (i == options.length)
										i = USER_CANCELLED;// nothing was chosen
									else
										i = USER_ACCEPTED-i; // to ensure that zero translates into USER_ACCEPTED and other choices into lower numbers 
									
								// one of the choices was made, determine which one and close the window
								answer.getAndSet( i );
								synchronized(answer)
								{
									answer.notifyAll();
								}

								dialog.setVisible(false);dialog.dispose();
				            }
				        }
				    });
					rejectElements.addListSelectionListener(new ListSelectionListener() {

						public void valueChanged(ListSelectionEvent e) {
							if (dialog.isVisible() && e.getSource() == rejectElements &&
									!e.getValueIsAdjusting() && !rejectElements.isSelectionEmpty())
							{
								answer.getAndSet( rejectElements.getLeadSelectionIndex() );
								synchronized(answer)
								{
									answer.notifyAll();
								}
								
								dialog.setVisible(false);dialog.dispose();
							}
						}
						
					});				
					dialog.pack();
					//rejectElements.setListData(questionList.toArray());
					dialog.setVisible(true);
				}
			});
			synchronized (answer) {
				while(answer.get() == USER_WAITINGFORSELECTION)
						answer.wait();// wait for a user to make a response
			}
		} catch (InvocationTargetException e) {
			e.printStackTrace();
			// if we cannot make a call, return a negative number - nothing do not know what else to do about it.
		}
		catch (InterruptedException e) {
			
			// if we are interrupted, return a negative number - nothing do not know what else to do about it.
		}
		if (answer.get() == USER_WAITINGFORSELECTION) // this one if an exception was thrown
			answer.getAndSet(USER_CANCELLED);
		return answer.get();
	}
	
	/*
	 * needs to be refactored into smaller methods
	 */
	protected List<List<String>> generateQuestions(DirectedSparseGraph model, OrigStatePair pair){
		Vertex q = pair.getQ();
		Vertex r = pair.getR();
		if(q==null || r ==null)
			return new ArrayList<List<String>>();
		boolean accepted = DeterministicDirectedSparseGraph.isAccept(q);
		if(accepted == false)
			return new ArrayList<List<String>>();
		List<String> sp = null;
		Set<List<String>> w =null;
		if(!hasAcceptedNeighbours(q)){
			sp = getShortPrefix(model, q);
			w = getShortSuffixes(model, r);
		}
		else{
			sp = getShortPrefix(model, r);// shortest sequence to the red state 
			w = getSuffixes(model,q, accepted);
		}
		Iterator<List<String>> wIt;
		ArrayList<List<String>> questions = new ArrayList<List<String>>();
		Set<String>loopLabels = new HashSet<String>();
		boolean loopToR = r.getSuccessors().contains(r);
		boolean redAndBlueNeighbours = r.getNeighbors().contains(q);
		if(loopToR||redAndBlueNeighbours){ //there either exists a loop to r or will do if r and b merge
			if(loopToR){
				Edge e = findEdge(r, r);
				HashSet labels = (HashSet<String>)e.getUserDatum(JUConstants.LABEL);
				loopLabels.addAll(labels);
			}
			if(redAndBlueNeighbours){
				Edge e = findEdge(r,q);
				HashSet labels = (HashSet<String>)e.getUserDatum(JUConstants.LABEL);
				loopLabels.addAll(labels);
			}
		}
		wIt = w.iterator();
		while(wIt.hasNext()){
			List<String> suffix = wIt.next();
			Iterator<String> labelIt = loopLabels.iterator();
			if(!loopLabels.isEmpty()){
				while(labelIt.hasNext()){
					List<String> newQuestion = new ArrayList<String>();
					newQuestion.addAll(sp);
					newQuestion.add(labelIt.next());
					newQuestion.addAll(suffix);
					Vertex v = getVertex(model, newQuestion);
					if(v==null)
						questions.add(newQuestion);
				}
			}
			List<String> newQuestion = new ArrayList<String>();
			newQuestion.addAll(sp);
			newQuestion.addAll(suffix);
			Vertex v = getVertex(model, newQuestion);
			if(v==null)
				questions.add(newQuestion);
		}
		return questions;
	}
	
	/*
	 * The conventional approach to computing suffixes presumes that the source vertex is the root
	 * of a tree. This method does not make that presumption, but simply returns the direct successors
	 * of the source vertex that are accepted.
	 */
	protected static HashSet<List<String>> getShortSuffixes(DirectedSparseGraph g, Vertex v){
		HashSet<List<String>> returnStrings = new HashSet<List<String>>();
		Iterator<Edge> outEdgeIt = v.getOutEdges().iterator();
		while(outEdgeIt.hasNext()){
			Edge e = outEdgeIt.next();
			if(DeterministicDirectedSparseGraph.isAccept(e.getOpposite(v))){
				ArrayList l = new ArrayList();
				l.add(e);
				returnStrings.addAll(getPaths(l));
			}
		}
		return returnStrings;
	}
	
	public static boolean hasAcceptedNeighbours(Vertex v){
		Iterator<DirectedSparseEdge> neighbourIt = v.getOutEdges().iterator();
		while (neighbourIt.hasNext()){
			DirectedSparseEdge e = neighbourIt.next();
			Vertex to = e.getDest();
			if(DeterministicDirectedSparseGraph.isAccept(to))
				return true;
		}
		return false;
	}
	
	/** Returns shortest paths from the given vertex to states at the end of the 
	 * PTA (those with no outgoing transitions) which 
	 * satisfy the requested accept/reject labelling.
	 * @param graph
	 * @param r the vertex to consider.
	 * @param accepted labelling requested.
	 * @return the list of paths.
	 */
	protected Set<List<String>> getSuffixes(DirectedSparseGraph graph, Vertex r, boolean accepted){
		Set<List<String>> setOfPaths = new HashSet<List<String>>();
		Iterator<Vertex> vertexIt = graph.getVertices().iterator();
		Set<Vertex> endVertices = new HashSet<Vertex>();
		DijkstraShortestPath p = new DijkstraShortestPath(graph);
		while(vertexIt.hasNext()){
			Vertex v = vertexIt.next();
			if(v.getSuccessors().isEmpty()&& DeterministicDirectedSparseGraph.isAccept(v) == accepted)
				endVertices.add(v);
		}
		for(Vertex v:endVertices)
		{
			List l = p.getPath(r, v);
			if(!l.isEmpty())
				setOfPaths.addAll(getPaths(l));
		}
		return setOfPaths;
	}
	
	/** Given a sequence of edges, builds a set of sequences of labels which will traverse
	 * the given sequence. This builds l[0]*l[1] .. where l[i] is a set of labels associated
	 * with l[i] and * is pairwise set concatenation operation.
	 * KIRR: this is the set multiplication from test set generation algorithm - surely the code must be possible to share
	 * 
	 * @param l path
	 * @return
	 */
	protected static Set<List<String>> getPaths(List<Edge> l){
		TreeMap<Integer,Set<List<String>>> returnSet = new TreeMap<Integer,Set<List<String>>>();// KIRR: this should not be done in this way since access to this map is not random - you only need the last element in which case simply storing the last set of lists is best
		for(int i=0;i<l.size();i++){// for each element of the source list
			Edge e = l.get(i);
			Set<String> labels = (Set<String>)e.getUserDatum(JUConstants.LABEL);
			Set<List<String>> strings = new HashSet<List<String>>();
			for(String s:labels)
			{
				if(i==0){
					List<String> string = new ArrayList<String>();
					string.add(s);
					strings.add(string);			
				}
				else{
					Set<List<String>> oldStrings = returnSet.get(i-1);
					Iterator<List<String>> stringIt = oldStrings.iterator();
					while(stringIt.hasNext()){
						List<String> newString = new ArrayList<String>();
						newString.addAll(stringIt.next());
						newString.add(s);
						strings.add(newString);
					}	
				}
			}
			returnSet.put(i, strings);
		}
		if(!returnSet.isEmpty())
			return returnSet.get(returnSet.lastKey());
		else
			return new HashSet<List<String>>();
	}
	
	/** Returns a sequence of names labelling a shortest path from the initial node to node q. */
	protected static List<String> getShortPrefix(DirectedSparseGraph model, Vertex q){
		Vertex init = DeterministicDirectedSparseGraph.findInitial(model);
		UnweightedShortestPath p = new UnweightedShortestPath(model);
		Iterator<Edge> pathIt =  ShortestPathUtils.getPath(p, init, q).iterator();
		List<String> list = new ArrayList<String>();
		while(pathIt.hasNext()){
			Edge e = pathIt.next();
			Set s = (HashSet)e.getUserDatum(JUConstants.LABEL);
			Object[] strings = s.toArray();
			list.add(strings[0].toString());
		}
			
		return list;
	}

	
	protected static DirectedSparseGraph merge(DirectedSparseGraph model, OrigStatePair pair){
		Vertex q = pair.getQ();
		Vertex qDash = pair.getR();
		Iterator<DirectedSparseEdge> inEdges = q.getInEdges().iterator();
		Set<DirectedSparseEdge> removeEdges = new HashSet<DirectedSparseEdge>();
		while(inEdges.hasNext()){
			DirectedSparseEdge e = inEdges.next();
			DirectedSparseEdge eDash = new DirectedSparseEdge(e.getSource(), qDash);
			eDash.addUserDatum(JUConstants.LABEL, e.getUserDatum(JUConstants.LABEL), UserData.CLONE);
			if(!e.getSource().getSuccessors().contains(qDash)) 
				model.addEdge(eDash);
			else{
				Edge existing = findEdge(e.getSource(), qDash);
				Set<String> labels = (Set<String>)existing.getUserDatum(JUConstants.LABEL);// KIRR: if you use UserData.SHARED, you do not need to copy the result back using put
				labels.addAll((Set)e.getUserDatum(JUConstants.LABEL));
				existing.setUserDatum(JUConstants.LABEL, labels, UserData.CLONE);
			}
			removeEdges.add(e);
		}
		Iterator outEdges = q.getOutEdges().iterator();
		while(outEdges.hasNext()){
			DirectedSparseEdge e = (DirectedSparseEdge)outEdges.next();
			DirectedSparseEdge eDash = new DirectedSparseEdge(qDash, e.getDest());
			eDash.addUserDatum(JUConstants.LABEL, e.getUserDatum(JUConstants.LABEL), UserData.CLONE);
			if(!qDash.getSuccessors().contains(e.getDest())) 
				model.addEdge(eDash);
			else{
				Edge existing = findEdge(qDash, e.getDest());
				Set<String> labels = (Set<String>)existing.getUserDatum(JUConstants.LABEL);
				labels.addAll((Set)e.getUserDatum(JUConstants.LABEL));
				existing.setUserDatum(JUConstants.LABEL, labels, UserData.CLONE);
			}
			removeEdges.add(e);
		}
		model.removeEdges(removeEdges);
		model.removeVertex(q);
		return model;
	}
	
	
	public static List<Vertex> getBFSList(Graph g){
		List<Vertex> queue = new LinkedList<Vertex>();
		Vertex init = DeterministicDirectedSparseGraph.findInitial(g);
		queue.add(0,init);
		int i=0;
		int j= queue.size();
		Set<Vertex> done = new HashSet<Vertex>();
		while(i<j){
			DirectedSparseVertex v = (DirectedSparseVertex)queue.get(i);
			done.add(v);
			Iterator succIt = v.getSuccessors().iterator();
			while(succIt.hasNext()){
				Vertex succ = (Vertex)succIt.next();
				if(!done.contains(succ))
					queue.add(succ);
			}
			j = queue.size();
			i++;
		}
		return queue;
	}

	/** After merging, a graph may exhibit non-determinism, in which case it is made deterministic
	 * by merging nodes. For instance, for A->B and A->C being a non-deterministic choice at node A, 
	 * nodes B and C are to
	 * be merged. This function identifies such a (B,C).
	 * 
	 * @param model
	 * @return a pair of states to be merged or null if the graph is deterministic.
	 */
	protected static OrigStatePair findMergablePair(DirectedSparseGraph model){
		List<Vertex> queue = getBFSList(model);
		Iterator<Vertex> queueIt = queue.iterator();
		while(queueIt.hasNext()){
			Vertex v = queueIt.next();
			Set<DirectedSparseEdge> edges = v.getOutEdges();
			Iterator<DirectedSparseEdge> edgeIt = edges.iterator();
			Map<String,DirectedSparseEdge> doneLabels = new HashMap<String,DirectedSparseEdge>();
			while(edgeIt.hasNext()){
				DirectedSparseEdge e = edgeIt.next();
				Set<String> labels = (Set<String>)e.getUserDatum(JUConstants.LABEL);
				Iterator<String> labelIt = labels.iterator();
				while(labelIt.hasNext()){
					String label = labelIt.next();
					if(doneLabels.get(label)==null)
						doneLabels.put(label, e);
					else {
						DirectedSparseEdge eDash = doneLabels.get(label);
						OrigStatePair p = null;
						if(DeterministicDirectedSparseGraph.isInitial(eDash.getDest()))
							p = new OrigStatePair(e.getDest(), eDash.getDest());
						else
							p = new OrigStatePair(eDash.getDest(),e.getDest());
						if(!DeterministicDirectedSparseGraph.different(p)) // KIRR: strange - the two should never be different if the original pair to choose was selected properly
							return p;
					}
				}
			}
		}
		return null;
	}

	protected Stack<OrigStatePair> chooseStatePairs(DirectedSparseGraph g, Collection<List<String>> sPlus, Collection<List<String>> sMinus){
		Stack<Vertex> blueStack = new Stack<Vertex>();
		blueStack.addAll(computeBlue(g));
		TreeMap<Integer,Vector<OrigStatePair> > scoreToPair = new TreeMap<Integer,Vector<OrigStatePair> >();// maps scores to pairs which have those scores
		while(!blueStack.isEmpty()){
			TreeMap<Integer,Vector<OrigStatePair> > singleSet = new TreeMap<Integer,Vector<OrigStatePair> >();
			Vertex blueVertex = blueStack.pop();
			Stack<Vertex> redStack = new Stack<Vertex>();
			redStack.addAll(DeterministicDirectedSparseGraph.findVertices(JUConstants.COLOUR, JUConstants.RED, g));
			while(!redStack.isEmpty()){
				Vertex redVertex = redStack.pop();
				OrigStatePair pair = new OrigStatePair(blueVertex, redVertex);
				doneEdges = new HashSet();
				Integer score = new Integer(computeScore(g,pair));
				DirectedSparseGraph temp = mergeAndDeterminize((Graph)g.copy(), pair);
				if(compatible(temp, sPlus, sMinus)){
					// singleSet maps scores to pairs which have those scores
					if(score<config.getKlimit())
						continue;
					if(singleSet.get(score) == null){
						// nothing yet with this score
						Vector<OrigStatePair> s = new Vector<OrigStatePair>();
						s.add(pair);
						singleSet.put(score, s);
					}
					else{
						// already some pairs with this score
						Vector<OrigStatePair> s = singleSet.get(score);
						s.add(pair);
						singleSet.put(score, s);
					}	
				}
			}
			if(singleSet.isEmpty()){// no new pairs found, marking the current blue vertex as red.
				blueVertex.setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);
				blueStack.clear();
				scoreToPair.clear();
				blueStack.addAll(computeBlue(g));
			}
			else{
				Iterator keyIt = singleSet.keySet().iterator();
				while(keyIt.hasNext()){
					Integer key = (Integer)keyIt.next();
					Vector<OrigStatePair> s = scoreToPair.get(key);
					if(s!=null){
						s.addAll(singleSet.get(key));
						Collections.sort(s);
					}
					else
					{
						Vector<OrigStatePair> value = singleSet.get(key);
						Collections.sort(value);
						scoreToPair.put(key, value);
					}
				}
			}
		}
		return createOrderedStack(scoreToPair);
	}
	
	protected Stack<OrigStatePair> createOrderedStack(TreeMap<Integer,Vector<OrigStatePair> > sets){
		Stack<OrigStatePair> values = new Stack<OrigStatePair>();
		if(config.getPairsMergedPerHypothesis()>0){
			Stack<Integer> keys = new Stack<Integer>();
			keys.addAll(sets.keySet());
			for(int i = 0;i< config.getPairsMergedPerHypothesis(); i++){
				if(keys.isEmpty())
					continue;
				Vector<OrigStatePair> pairs = sets.get(keys.pop());
				values.addAll(pairs);
			}
		}
		else for(Vector<OrigStatePair> v:sets.values()){
			values.addAll(v);
		}
		return values;
	}
		
	/** For every pair of transitions with the same label from a supplied pair of states
	 *  this function recursively invokes itself and adds 1 to the result. 
	 *  KIRR: this should do the W set construction to determine whether two states are equivalent, 
	 *  but do it in the clever way, considering incompleteness.
	 *  
	 * @param original the graph to operate on
	 * @param blueRed the pair of states
	 * @return
	 */
	protected int computeScore(DirectedSparseGraph original, OrigStatePair blueRed){
		int returnValue = 0;
		if(DeterministicDirectedSparseGraph.different(blueRed))
				return -1;

		Iterator<DirectedSparseEdge> edgesOut = blueRed.getQ().getOutEdges().iterator();
		while(edgesOut.hasNext()){
			DirectedSparseEdge e = edgesOut.next();
			if(this.doneEdges.contains(e))
				continue;
			else
				doneEdges.add(e);
			HashSet<String> labels = (HashSet<String>)e.getUserDatum(JUConstants.LABEL);
			Iterator<String> labelIt = labels.iterator();
			while(labelIt.hasNext()){
				List string = new ArrayList();
				string.add(labelIt.next());
				Vertex qi = e.getDest();
				Vertex qj = getVertex(original,blueRed.getR(), string);
				OrigStatePair newPair = new OrigStatePair(qi, qj);
				if(qj!=null){
					int equivalent = computeScore(original, newPair);
					if(equivalent<0){
						return -1;
					}
					else{
						returnValue++;
						returnValue= returnValue + equivalent;
					}
				}
			}
		}

		return returnValue;
	}
	
	/** Adds a given set of sequences to a PTA, with a specific accept-reject labelling.
	 * 
	 * @param pta
	 * @param strings sequences to be added
	 * @param accepted whether sequences are accept or reject ones.
	 * @return the result of adding.
	 */ 
	DirectedSparseGraph augmentPTA(DirectedSparseGraph pta, Collection<List<String>> strings, boolean accepted){
		Iterator<List<String>> stringsIt = strings.iterator();
		while(stringsIt.hasNext()){
			List<String> string = stringsIt.next();
			if (string.isEmpty() && !accepted)
				throw new IllegalArgumentException("since the initial state is an accept one, a negative string should not be empty");
			
			for(int i = 1;i<=string.size();i++){
				List<String> current = string.subList(0, i);
				Vertex existing = getVertex(pta,current);

				Vertex newVertex = new DirectedSparseVertex();
				if (i == string.size())// KIRR: every prefix of a reject sequence is an accept sequence.
					newVertex.setUserDatum(JUConstants.ACCEPTED, accepted, UserData.SHARED);
				else
					newVertex.setUserDatum(JUConstants.ACCEPTED, true, UserData.SHARED);
				
				if(existing == null){
					pta.addVertex(newVertex);
					Vertex previous;
					previous = getVertex(pta, string.subList(0, i-1));// for i==1, getVertex will return the initial vertex
					DirectedSparseEdge e = new DirectedSparseEdge(previous, newVertex);
					Set<String> labels = new HashSet<String>();
					labels.add(string.get(i-1));
					e.addUserDatum(JUConstants.LABEL, labels, UserData.CLONE);
					pta.addEdge(e);
				}
				else
					if (DeterministicDirectedSparseGraph.different(new OrigStatePair(existing,newVertex)))
					{
						existing.addUserDatum(JUConstants.HIGHLIGHT, "whatever", UserData.SHARED);
						updateGraph(pta);
						StringBuffer errorPath = new StringBuffer();
						errorPath.append( string.get(0) );
						for(int j = 1;j<i;j++)
							errorPath.append(" ").append( string.get(j) );
						throw new IllegalArgumentException("incompatible "+(accepted?"accept":"reject")+" labelling: "+errorPath);
					}
			}
		}
		return pta;
	}
	
	protected static Vertex getVertex (DirectedSparseGraph g,Vertex v, List<String> string){
		Vertex current = v;
		if (current == null)
			return null;
		
		for(int i = 0;i<string.size();i++){
			String label = string.get(i);
			DirectedSparseEdge edge = getEdgeWithLabel(current.getOutEdges(), label);
			if(edge == null)
				return null;
			current = edge.getDest();
		}
		return current;
	}
	
	public static Vertex getVertex (DirectedSparseGraph g, List<String> string){
		return getVertex(g, DeterministicDirectedSparseGraph.findInitial(g), string);
	}

	public static DirectedSparseEdge getEdgeWithLabel(Set edges, String label){
		Iterator edgeIt = edges.iterator();
		while(edgeIt.hasNext()){
			DirectedSparseEdge e = (DirectedSparseEdge)edgeIt.next();
			Set<String> labels = (Set<String>)e.getUserDatum(JUConstants.LABEL);
			if(labels.contains(label))
				return e;
		}
		return null;
	}

	public static Edge findEdge(Vertex from, Vertex to){
		Iterator<DirectedSparseEdge> edgesOut = from.getOutEdges().iterator();
		while(edgesOut.hasNext()){
			DirectedSparseEdge current = edgesOut.next();
			if(current.getDest().equals(to))
				return current;
		}
		return null;
	}

	public int getQuestionCounter() {
		return questionCounter;
	}

	public void setQuestionCounter(int questionCounter) {
		this.questionCounter = questionCounter;
	}
}
