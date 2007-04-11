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

import statechum.JUConstants;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.algorithms.shortestpath.*;

public class RPNIBlueFringeLearner extends Observable {
	protected Graph currentGraph = RPNIBlueFringeLearner.initialise();
	protected HashSet doneEdges;
	protected Collection<List<String>> sPlus, sMinus;
	protected int generalisationThreshold, pairsMergedPerHypothesis, certaintyThreshold=100000;
	protected int questionCounter = 0;
	
	
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
			if(v.getUserDatum(JUConstants.ACCEPTED)==null)
				continue;
			if(v.getUserDatum(JUConstants.ACCEPTED).toString().equals("false")&&!v.equals(findVertex("property", "init",g)))
				remove.add(v);
		}
		Iterator<Vertex> removeIt = remove.iterator();
		while(removeIt.hasNext()){
			g.removeVertex(removeIt.next());
		}
		return g;
	}
	
	/** Determines whether the interface pops up during test execution - the interface helps debugging but slows down execution termendously.
	 */
	protected boolean debugMode = false;

	/** Updates listeners only if this object has been modified and debug mode is on, by calling
	 * <pre>
	 * setChanged()
	 * </pre>
	 * @param g the graph to display in the associated view
	 */
	public void updateGraph(Graph g)
	{
		if (debugMode)
			notifyObservers(g);
	}
	
	private Frame parentFrame;

	public RPNIBlueFringeLearner(Frame parentFrame){
		this.pairsMergedPerHypothesis = 0;
		this.generalisationThreshold = 0;
		this.parentFrame = parentFrame;
	}
	
	/** Takes all red-labelled nodes; non-red nodes which can be reached by a single transition from any 
	 * red one is labelled blue.
	 * 
	 * @param model
	 * @return the set of blue nodes.
	 */ 
	protected Set<Vertex> computeBlue(DirectedSparseGraph model){
		Set<Vertex> blues = new HashSet<Vertex>();
		for(Vertex v: findVertices("colour", "red", model))
		{
			Iterator<DirectedSparseEdge>neighbourIt = v.getOutEdges().iterator();
			while(neighbourIt.hasNext()){
				DirectedSparseEdge next = neighbourIt.next();
				Vertex neighbour = next.getDest();
				String neighbourColour = (String)neighbour.getUserDatum("colour");
				if(neighbourColour!=null){
					if(neighbourColour.equals("red"))
						continue;
				}
				neighbour.setUserDatum("colour", "blue", UserData.SHARED);
				blues.add(neighbour);
				
			}
		}
		//updateGraph(model);
		return blues;
	}
	
	/** Makes a copy of the graph given to it and merges states in the pair supplied. */
	protected DirectedSparseGraph mergeAndDeterminize(Graph model, StatePair pair){
		Graph original = (Graph)model.copy();
		Vertex q = findVertex(JUConstants.LABEL, pair.getQ().getUserDatum(JUConstants.LABEL),original);
		Vertex qDash = findVertex(JUConstants.LABEL, pair.getR().getUserDatum(JUConstants.LABEL),original);
		pair = new StatePair(q,qDash);
		DirectedSparseGraph temp = merge((DirectedSparseGraph)original, pair);
		StatePair mergable = findMergablePair(temp);
		while(mergable!=null){
			temp=merge(temp, mergable);
			mergable = findMergablePair(temp);
		}
		return temp;
	}
	
	protected DirectedSparseGraph createAugmentedPTA(DirectedSparseGraph model, Collection<List<String>> sPlus, Collection<List<String>> sMinus){
		model = augmentPTA(model, sMinus, false);
		model = augmentPTA(model, sPlus, true);
		numberVertices(model);
		return model;
	}
	
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model, Collection<List<String>> sPlus, Collection<List<String>> sMinus)throws InterruptedException{
		this.sPlus = sPlus;
		this.sMinus = sMinus;
		model = createAugmentedPTA(model, sPlus, sMinus);
		
		Vertex init = findVertex("property", "init",model);
		init.setUserDatum("colour", "red", UserData.SHARED);
		setChanged();
		Stack possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		while(!possibleMerges.isEmpty()){
			StatePair pair = (StatePair)possibleMerges.pop();
			DirectedSparseGraph temp = mergeAndDeterminize((Graph)model.copy(), pair);
			if(compatible(temp, sPlus, sMinus)){// KIRR: the should always return true
				pair.getQ().setUserDatum("pair", pair, UserData.SHARED);
				pair.getR().setUserDatum("pair", pair, UserData.SHARED);
				setChanged();
				List<List<String>> questions = generateQuestions(model, pair);
				questions = trimSet(questions);
				Iterator<List<String>> questionIt = questions.iterator();
				while(questionIt.hasNext()){
					List<String> question = questionIt.next();
					String accepted = pair.getQ().getUserDatum(JUConstants.ACCEPTED).toString();// Q is the blue vertex
					updateGraph(model);
					int response = checkWithEndUser(model,question,new Object[0]);// zero means "yes", everything else is "no"
					questionCounter++;
					pair.getQ().removeUserDatum("pair");
					pair.getR().removeUserDatum("pair");
					if(response == USER_ACCEPTED){
						sPlus.add(question);
						System.out.println(setByAuto+question+ " <yes>");
						if(accepted.equals("false"))// KIRR: how can this be true? If it were so, there would be no questions to ask
							return learnMachine(initialise(), sPlus, sMinus);
					}
					else{
						sMinus.add(question.subList(0, response));
						System.out.println(setByAuto+question+ " <no>");
						if(accepted.equals("true")){// KIRR: this cannot be false either
							return learnMachine(initialise(), sPlus, sMinus);
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
				Object accepted = v.getUserDatum(JUConstants.ACCEPTED);
				if(accepted.toString().equals("true"))
					returnValue = false;
			}
		}
		for(List<String> string:sPlus)
		{
			Vertex v = getVertex(model,string);
			if(v == null)
				returnValue = false;
			else{
				Object accepted = v.getUserDatum(JUConstants.ACCEPTED);
				if(accepted.toString().equals("false"))
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
	JOptionPane jop = null;	
	
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
	protected StoredAnswers ans = null;
	
	/** Makes it possible to answer questions automatically.
	 *  
	 * @param a the class holding stored answers.
	 */
	public void setAnswers(StoredAnswers a)
	{
		ans = a;
	}
	
	public static final int USER_CANCELLED = -2;
	public static final int USER_ACCEPTED = -3;
	public static final int USER_WAITINGFORSELECTION = -1;
	
	public final static String QUESTION_AUTO = "<auto> "; 
	protected String setByAuto = "";
	
	protected int checkWithEndUser(DirectedSparseGraph model,List<String> question, final Object [] moreOptions){
		if (ans != null)
		{
			int AutoAnswer = ans.getAnswer(question);
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
						    JDialog.DO_NOTHING_ON_CLOSE);
					dialog.addWindowListener(new WindowAdapter() {
					    public void windowClosing(WindowEvent we) {
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
	protected List<List<String>> generateQuestions(DirectedSparseGraph model, StatePair pair){
		Vertex q = pair.getQ();
		Vertex r = pair.getR();
		if(q==null || r ==null)
			return new ArrayList<List<String>>();
		String accepted = q.getUserDatum(JUConstants.ACCEPTED).toString();
		if(accepted.equals("false"))
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
			if(e.getOpposite(v).getUserDatum(JUConstants.ACCEPTED).toString().equals("true")){
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
			if(to.getUserDatum(JUConstants.ACCEPTED).toString().equals("true"))
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
	protected Set<List<String>> getSuffixes(DirectedSparseGraph graph, Vertex r, String accepted){
		Set<List<String>> setOfPaths = new HashSet<List<String>>();
		Iterator<Vertex> vertexIt = graph.getVertices().iterator();
		Set<Vertex> endVertices = new HashSet<Vertex>();
		DijkstraShortestPath p = new DijkstraShortestPath(graph);
		while(vertexIt.hasNext()){
			Vertex v = vertexIt.next();
			if(v.getSuccessors().isEmpty()&& v.getUserDatum(JUConstants.ACCEPTED).toString().equals(accepted))
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
		Vertex init = findVertex("property", "init",model);
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

	
	protected static DirectedSparseGraph merge(DirectedSparseGraph model, StatePair pair){
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
	
	
	protected static List<Vertex> getBFSList(Graph g){
		List<Vertex> queue = new LinkedList<Vertex>();
		Vertex init = findVertex("property", "init",g);
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
	protected static StatePair findMergablePair(DirectedSparseGraph model){
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
						StatePair p = null;
						if(eDash.getDest().getUserDatum("property")!=null)
							p = new StatePair(e.getDest(), eDash.getDest());
						else
							p = new StatePair(eDash.getDest(),e.getDest());
						if(!different(p)) // KIRR: strange - the two should never be different if the original pair to choose was selected properly
							return p;
					}
				}
			}
		}
		return null;
	}

	protected Stack chooseStatePairs(DirectedSparseGraph g, Collection<List<String>> sPlus, Collection<List<String>> sMinus){
		Stack<Vertex> blueStack = new Stack<Vertex>();
		blueStack.addAll(computeBlue(g));
		TreeMap<Integer,Vector<StatePair> > scoreToPair = new TreeMap<Integer,Vector<StatePair> >();// maps scores to pairs which have those scores
		while(!blueStack.isEmpty()){
			TreeMap<Integer,Vector<StatePair> > singleSet = new TreeMap<Integer,Vector<StatePair> >();
			Vertex blueVertex = blueStack.pop();
			Stack<Vertex> redStack = new Stack<Vertex>();
			redStack.addAll(findVertices("colour", "red", g));
			while(!redStack.isEmpty()){
				Vertex redVertex = redStack.pop();
				StatePair pair = new StatePair(blueVertex, redVertex);
				doneEdges = new HashSet();
				Integer score = new Integer(computeScore(g,pair));
				if(score.intValue()<this.generalisationThreshold)
					continue;
				DirectedSparseGraph temp = mergeAndDeterminize((Graph)g.copy(), pair);
				if(compatible(temp, sPlus, sMinus)){
					// singleSet maps scores to pairs which have those scores
					if(singleSet.get(score) == null){
						// nothing yet with this score
						Vector<StatePair> s = new Vector<StatePair>();
						s.add(pair);
						singleSet.put(score, s);
					}
					else{
						// already some pairs with this score
						Vector<StatePair> s = singleSet.get(score);
						s.add(pair);
						singleSet.put(score, s);
					}	
				}
			}
			if(singleSet.isEmpty()){// no new pairs found, marking the current blue vertex as red.
				blueVertex.setUserDatum("colour", "red", UserData.SHARED);
				blueStack.clear();
				scoreToPair.clear();
				blueStack.addAll(computeBlue(g));
			}
			else{
				Iterator keyIt = singleSet.keySet().iterator();
				while(keyIt.hasNext()){
					Integer key = (Integer)keyIt.next();
					Vector<StatePair> s = scoreToPair.get(key);
					if(s!=null){
						s.addAll(singleSet.get(key));
						Collections.sort(s);
					}
					else
					{
						Vector<StatePair> value = singleSet.get(key);
						Collections.sort(value);
						scoreToPair.put(key, value);
					}
				}
			}
		}
		return createOrderedStack(scoreToPair);
	}
	
	protected Stack createOrderedStack(TreeMap<Integer,Vector<StatePair> > sets){
		Stack<StatePair> values = new Stack<StatePair>();
		if(this.pairsMergedPerHypothesis>0){
			Stack<Integer> keys = new Stack<Integer>();
			keys.addAll(sets.keySet());
			for(int i = 0;i< this.pairsMergedPerHypothesis; i++){
				if(keys.isEmpty())
					continue;
				Vector<StatePair> pairs = sets.get(keys.pop());
				values.addAll(pairs);
			}
		}
		else for(Vector<StatePair> v:sets.values()){
			values.addAll(v);
		}
		return values;
	}
	
	/** Checks for shallow compatibility between states, in other words if the two are both accept or both reject states
	 * 
	 * @param pair a pair states to check for compatibility. 
	 * @return whether the two are different.
	 */
	protected  static boolean different(StatePair pair){
		Object qAcceptedO = pair.getQ().getUserDatum(JUConstants.ACCEPTED);
		Object rAcceptedO = pair.getR().getUserDatum(JUConstants.ACCEPTED);
		if(rAcceptedO==null || qAcceptedO == null)
			return false;
		Boolean qAccepted = new Boolean(qAcceptedO.toString());
		Boolean rAccepted = new Boolean(rAcceptedO.toString());
		if(rAccepted.booleanValue()!=qAccepted.booleanValue())
			return true;
		return false;
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
	protected int computeScore(DirectedSparseGraph original, StatePair blueRed){
		int returnValue = 0;
		if(different(blueRed))
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
				StatePair newPair = new StatePair(qi, qj);
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
	
	/** Creates a graph with a single accept-vertex. */
	protected static DirectedSparseGraph initialise(){
		DirectedSparseGraph pta = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum("property", "init", UserData.SHARED);
		init.addUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
		pta.setUserDatum(JUConstants.TITLE, "Hypothesis machine", UserData.SHARED);
		pta.addVertex(init);
		numberVertices(pta);
		return pta;
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
			for(int i = 1;i<=string.size();i++){
				List<String> current = string.subList(0, i);
				Vertex existing = getVertex(pta,current);

				Vertex newVertex = new DirectedSparseVertex();
				if (i == string.size())// KIRR: every prefix of a reject sequence is an accept sequence.
					newVertex.setUserDatum(JUConstants.ACCEPTED, accepted, UserData.SHARED);
				else
					newVertex.setUserDatum(JUConstants.ACCEPTED, "true", UserData.SHARED);
				
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
					if (different(new StatePair(existing,newVertex)))
					{
						existing.addUserDatum("pair", "whatever", UserData.SHARED);
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
	
	/**
	 * Labels vertices according to breadth-first search
	 * KIRR: this should label vertices according to their Jung ID instead, because those IDs 
	 * are shown in the debugger and it is pain to dig through to find labels in user-added data.
	 * 
	 * @param pta the graph to operate on.
	 */
	protected static void numberVertices(DirectedSparseGraph pta){
		Iterator<Vertex> vertexIt = getBFSList(pta).iterator();
		while(vertexIt.hasNext()){
			Vertex v = vertexIt.next();
			v.removeUserDatum(JUConstants.LABEL);// since we'd like this method to run multiple times, once immediately after initialisation and subsequently when sPlus and sMinus are added.
			v.addUserDatum(JUConstants.LABEL, v.toString(), UserData.SHARED);
		}
	}
	
	protected static Vertex getVertex (DirectedSparseGraph g,Vertex v, List<String> string){
		Vertex current = v;
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
		Vertex init = findVertex("property", "init",g);
		Vertex current = init;
		for(int i = 0;i<string.size();i++){
			String label = string.get(i);
			DirectedSparseEdge edge = getEdgeWithLabel(current.getOutEdges(), label);
			if(edge == null)
				return null;
			current = edge.getDest();
		}
		return current;
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
	
	public static Vertex findVertex(String property, Object value, Graph g){
		Iterator<Vertex> vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext()){
			Vertex v = vertexIt.next();
			if(v.getUserDatum(property) == null)
				continue;
			if(v.getUserDatum(property).equals(value))
				return v;
		}
		return null;
	}
	
	public static Set<Vertex> findVertices(String property, Object value, Graph g){
		Set<Vertex> vertices = new HashSet<Vertex>();
		Iterator<Vertex> vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext()){
			Vertex v = vertexIt.next();
			if(v.getUserDatum(property) == null)
				continue;
			if(v.getUserDatum(property).equals(value))
				vertices.add(v);
		}
		return vertices;
	}
	
	protected static Edge findEdge(Vertex from, Vertex to){
		Iterator<DirectedSparseEdge> edgesOut = from.getOutEdges().iterator();
		while(edgesOut.hasNext()){
			DirectedSparseEdge current = edgesOut.next();
			if(current.getDest().equals(to))
				return current;
		}
		return null;
	}

	public void setGeneralisationThreshold(int generalisationThreshold) {
		this.generalisationThreshold = generalisationThreshold;
	}

	public void setPairsMergedPerHypothesis(int pairsMergedPerHypothesis) {
		this.pairsMergedPerHypothesis = pairsMergedPerHypothesis;
	}

	public int getQuestionCounter() {
		return questionCounter;
	}

	public void setQuestionCounter(int questionCounter) {
		this.questionCounter = questionCounter;
	}

	public void setCertaintyThreshold(int certaintyThreshold) {
		this.certaintyThreshold = certaintyThreshold;
	}

	public void setDebugMode(boolean debugMode) {
		this.debugMode = debugMode;
	}

}
