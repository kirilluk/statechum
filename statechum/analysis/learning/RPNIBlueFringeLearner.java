package statechum.analysis.learning;

import java.util.*;

import javax.swing.*;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.algorithms.shortestpath.*;

public class RPNIBlueFringeLearner extends Observable implements Learner {
	protected Graph currentGraph;
	protected HashSet doneEdges;
	
	public Graph getGraph(){
		//return removeNegatives(currentGraph);
		return currentGraph;
	}
	
	protected Graph removeNegatives(Graph g){
		Iterator vertexIt = g.getVertices().iterator();
		HashSet remove = new HashSet();
		while(vertexIt.hasNext()){
			Vertex v = (Vertex)vertexIt.next();
			if(v.getUserDatum("accepted")==null)
				continue;
			if(v.getUserDatum("accepted").toString().equals("false")&&!v.equals(findVertex("property", "init",g)))
				remove.add(v);
		}
		Iterator<Vertex> removeIt = remove.iterator();
		while(removeIt.hasNext()){
			g.removeVertex(removeIt.next());
		}
		return g;
	}
	
	public synchronized void updateGraph(Graph g){
		setChanged();
		currentGraph = (Graph)g.copy();
		notifyObservers();
	}
	
	public RPNIBlueFringeLearner(){
		super();
	}
	
	/** Takes all red-labelled nodes; non-red nodes which can be reached by a single transition from any 
	 * red one is labelled blue.
	 * 
	 * @param model
	 * @return the set of blue nodes.
	 */ 
	protected Set<Vertex> computeBlue(DirectedSparseGraph model){
		Set<Vertex> blues = new HashSet<Vertex>();
		Set reds = findVertices("colour", "red", model);
		Iterator<Vertex> redIt = reds.iterator();
		while(redIt.hasNext()){
			Vertex v = redIt.next();
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
	
	protected DirectedSparseGraph mergeAndDeterminize(Graph original, StatePair pair){
		Vertex q = findVertex("label", pair.getQ().getUserDatum("label"),original);
		Vertex qDash = findVertex("label", pair.getR().getUserDatum("label"),original);
		pair = new StatePair(q,qDash);
		DirectedSparseGraph temp = merge((DirectedSparseGraph)original, pair);
		StatePair mergable = findMergablePair(temp);
		while(mergable!=null){
			temp=merge(temp, mergable);
			mergable = findMergablePair(temp);
		}
		return temp;
	}
	
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model, Set<List<String>> sPlus, Set<List<String>> sMinus){
		model = augmentPTA(model, sMinus, false);
		model = augmentPTA(model, sPlus, true);
		numberVertices(model);
		Vertex init = findVertex("property", "init",model);
		init.setUserDatum("colour", "red", UserData.SHARED);
		updateGraph(model);
		Stack possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		while(!possibleMerges.isEmpty()){
			StatePair pair = (StatePair)possibleMerges.pop();
			DirectedSparseGraph temp = mergeAndDeterminize((Graph)model.copy(), pair);
			if(compatible(temp, sPlus, sMinus)){// KIRR: the should always return true
				pair.getQ().setUserDatum("pair", pair, UserData.SHARED);
				pair.getR().setUserDatum("pair", pair, UserData.SHARED);
				updateGraph(model);
				Set<List<String>> questions = generateQuestions(model, pair);
				questions = trimSet(questions);
				Iterator<List<String>> questionIt = questions.iterator();
				while(questionIt.hasNext()){
					List<String> question = questionIt.next();
					String accepted = pair.getQ().getUserDatum("accepted").toString();// Q is the blue vertex
					updateGraph(model);
					boolean response = checkWithEndUser(question);
					pair.getQ().removeUserDatum("pair");
					pair.getR().removeUserDatum("pair");
					if(response){
						sPlus.add(question);
						System.out.println(question+ " <yes>");
						if(accepted.equals("false"))// KIRR: how can this be true? If it were so, there would be no questions to ask
							return learnMachine(initialise(), sPlus, sMinus);
					}
					else{
						sMinus.add(question);
						System.out.println(question+ " <no>");
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
	
	protected boolean compatible(DirectedSparseGraph model, Set<List<String>> sPlus, Set<List<String>> sMinus){
		boolean returnValue = true;
		Iterator<List<String>> negativeIt = sMinus.iterator();
		while(negativeIt.hasNext()){
			List<String> string = negativeIt.next();
			Vertex v = getVertex(model,string);
			if(v != null){
				Object accepted = v.getUserDatum("accepted");
				if(accepted.toString().equals("true"))
					returnValue = false;
			}
		}
		Iterator<List<String>> positiveIt = sPlus.iterator();
		while(positiveIt.hasNext()){
			List<String> string = positiveIt.next();
			Vertex v = getVertex(model,string);
			if(v == null)
				returnValue = false;
			else{
				Object accepted = v.getUserDatum("accepted");
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
	protected Set<List<String>> trimSet(Set<List<String>> questions){
		Set<String> done = new HashSet<String>();
		Set<List<String>> trimmedSet = new HashSet<List<String>>();
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
	
	private boolean checkWithEndUser(List question){
		String questionString = new String();
		Iterator questionIt = question.iterator();
		while(questionIt.hasNext()){
			questionString = questionString.concat(", "+(String)questionIt.next());
		}
		JFrame jf = new JFrame();
		int answer;
		answer = JOptionPane.showConfirmDialog(jf,
                questionString, "Valid input?",JOptionPane.YES_NO_OPTION);
		if(answer==0)
			return true;
		else
			return false;
	}
	
	protected Set<List<String>> generateQuestions(DirectedSparseGraph model, StatePair pair){
		Vertex q = pair.getQ();
		Vertex r = pair.getR();
		if(q==null || r ==null)
			return new HashSet<List<String>>();
		String accepted = q.getUserDatum("accepted").toString();
		if(accepted.equals("false"))
			return new HashSet<List<String>>();
		List<String> sp = getShortPrefix(model, r);// shortest sequence to the red state 
		Set<List<String>> w = getSuffixes(model,q, accepted);
		Iterator<List<String>> wIt;
		Set<List<String>> questions = new HashSet<List<String>>();
		/*if(r.getSuccessors().contains(q)){
			wIt = w.iterator();
			while(wIt.hasNext()){
				ArrayList suffix = new ArrayList();
				List newQuestion = new ArrayList();
				List path = wIt.next();
				Iterator pathIt = path.iterator();
				while(pathIt.hasNext()){
					String label = (String)pathIt.next();
					suffix.add(label);
				}
				HashSet labels = (HashSet)findEdge(r,q).getUserDatum("label");
				Object[] labelArray = labels.toArray();
				newQuestion.addAll(sp);
				newQuestion.add(labelArray[0]);
				newQuestion.addAll(suffix);
				Vertex v = getVertex(model, newQuestion);
				if(v==null)
					questions.add(newQuestion);
			}
		}*/
		wIt = w.iterator();
		while(wIt.hasNext()){
			List<String> suffix = new ArrayList<String>();
			List<String> newQuestion = new ArrayList<String>();
			List<String> path = wIt.next();
			Iterator<String> pathIt = path.iterator();
			while(pathIt.hasNext()){
				String label = pathIt.next();
				suffix.add(label);
			}
			newQuestion.addAll(sp);
			newQuestion.addAll(suffix);
			Vertex v = getVertex(model, newQuestion);
			if(v==null)
				questions.add(newQuestion);
		}
		
		return questions;
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
			if(v.getSuccessors().isEmpty()&& v.getUserDatum("accepted").toString().equals(accepted))
				endVertices.add(v);
		}
		Iterator<Vertex> endVertexIt = endVertices.iterator();
		while(endVertexIt.hasNext()){
			Vertex v = endVertexIt.next();
			List l = p.getPath(r, v);
			if(!l.isEmpty())
				setOfPaths.addAll(getPaths(l));
		}
		if(r.getSuccessors().contains(r)){
			ArrayList l = new ArrayList();
			l.add(findEdge(r,r));
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
			Set<String> labels = (Set<String>)e.getUserDatum("label");
			Iterator<String> labelIt = labels.iterator();
			Set<List<String>> strings = new HashSet<List<String>>();
			while(labelIt.hasNext()){
				String s = labelIt.next();
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
		return returnSet.get(returnSet.lastKey());
	}
	
	/** Returns a sequence of names labelling a shortest path from the initial node to node q. */
	protected static List<String> getShortPrefix(DirectedSparseGraph model, Vertex q){
		Vertex init = findVertex("property", "init",model);
		UnweightedShortestPath p = new UnweightedShortestPath(model);
		Iterator<Edge> pathIt =  ShortestPathUtils.getPath(p, init, q).iterator();
		List<String> list = new ArrayList<String>();
		while(pathIt.hasNext()){
			Edge e = pathIt.next();
			Set s = (HashSet)e.getUserDatum("label");
			Object[] strings = s.toArray();
			list.add(strings[0].toString());
		}
			
		return list;
	}

	
	protected DirectedSparseGraph merge(DirectedSparseGraph model, StatePair pair){
		Vertex q = pair.getQ();
		Vertex qDash = pair.getR();
		Iterator<DirectedSparseEdge> inEdges = q.getInEdges().iterator();
		Set<DirectedSparseEdge> removeEdges = new HashSet<DirectedSparseEdge>();
		while(inEdges.hasNext()){
			DirectedSparseEdge e = inEdges.next();
			DirectedSparseEdge eDash = new DirectedSparseEdge(e.getSource(), qDash);
			eDash.addUserDatum("label", e.getUserDatum("label"), UserData.CLONE);
			if(!e.getSource().getSuccessors().contains(qDash)) 
				model.addEdge(eDash);
			else{
				Edge existing = findEdge(e.getSource(), qDash);
				Set<String> labels = (Set<String>)existing.getUserDatum("label");// KIRR: if you use UserData.SHARED, you do not need to copy the result back using put
				labels.addAll((Set)e.getUserDatum("label"));
				existing.setUserDatum("label", labels, UserData.CLONE);
			}
			removeEdges.add(e);
		}
		Iterator outEdges = q.getOutEdges().iterator();
		while(outEdges.hasNext()){
			DirectedSparseEdge e = (DirectedSparseEdge)outEdges.next();
			DirectedSparseEdge eDash = new DirectedSparseEdge(qDash, e.getDest());
			eDash.addUserDatum("label", e.getUserDatum("label"), UserData.CLONE);
			if(!qDash.getSuccessors().contains(e.getDest())) 
				model.addEdge(eDash);
			else{
				Edge existing = findEdge(qDash, e.getDest());
				Set<String> labels = (Set<String>)existing.getUserDatum("label");
				labels.addAll((Set)e.getUserDatum("label"));
				existing.setUserDatum("label", labels, UserData.CLONE);
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
				Set<String> labels = (Set<String>)e.getUserDatum("label");
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

	private Stack chooseStatePairs(DirectedSparseGraph g, Set<List<String>> sPlus, Set<List<String>> sMinus){
		int threshold = 1;
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
				if(score.intValue()<threshold)
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
	
	protected static Stack createOrderedStack(TreeMap<Integer,Vector<StatePair> > sets){
		Iterator<Vector<StatePair> > valueIt = sets.values().iterator();
		Stack<StatePair> allValues = new Stack<StatePair>();
		while(valueIt.hasNext()){
			Vector<StatePair> s = valueIt.next();
			allValues.addAll(s);
		}
		return allValues;
	}
	
	/** Checks for shallow compatibility between states, in other words if the two are both accept or both reject states
	 * 
	 * @param pair a pair states to check for compatibility. 
	 * @return whether the two are different.
	 */
	protected  static boolean different(StatePair pair){
		Object qAcceptedO = pair.getQ().getUserDatum("accepted");
		Object rAcceptedO = pair.getR().getUserDatum("accepted");
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
			HashSet<String> labels = (HashSet<String>)e.getUserDatum("label");
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
		init.addUserDatum("accepted", "true", UserData.SHARED);
		pta.addVertex(init);
		return pta;
	}
	
	
	/** Adds a given set of sequences to a PTA, with a specific accept-reject labelling.
	 * 
	 * @param pta
	 * @param strings sequences to be added
	 * @param accepted whether sequences are accept or reject ones.
	 * @return the result of adding.
	 */ 
	protected DirectedSparseGraph augmentPTA(DirectedSparseGraph pta, Set<List<String>> strings, boolean accepted){
		Iterator<List<String>> stringsIt = strings.iterator();
		while(stringsIt.hasNext()){
			List<String> string = stringsIt.next();
			for(int i = 1;i<=string.size();i++){
				List<String> current = string.subList(0, i);
				Vertex existing = getVertex(pta,current);

				Vertex newVertex = new DirectedSparseVertex();
				//nw: if the prefix of a negative is not explicitly accepted, it is presumed not to be accepted
				newVertex.setUserDatum("accepted", accepted, UserData.SHARED);
				if(existing == null){
					pta.addVertex(newVertex);
					Vertex previous;
					previous = getVertex(pta, string.subList(0, i-1));// for i==1, getVertex will return the initial vertex
					DirectedSparseEdge e = new DirectedSparseEdge(previous, newVertex);
					Set<String> labels = new HashSet<String>();
					labels.add(string.get(i-1));
					e.addUserDatum("label", labels, UserData.CLONE);
					pta.addEdge(e);
				}
				else
					if (i == string.size() && different(new StatePair(existing,newVertex)))
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
			v.addUserDatum("label", v.toString(), UserData.SHARED);
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
	
	protected static Vertex getVertex (DirectedSparseGraph g, List<String> string){
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
	
	protected static DirectedSparseEdge getEdgeWithLabel(Set edges, String label){
		Iterator edgeIt = edges.iterator();
		while(edgeIt.hasNext()){
			DirectedSparseEdge e = (DirectedSparseEdge)edgeIt.next();
			Set<String> labels = (Set<String>)e.getUserDatum("label");
			if(labels.contains(label))
				return e;
		}
		return null;
	}
	
	protected static Vertex findVertex(String property, Object value, Graph g){
		Iterator vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext()){
			DirectedSparseVertex v = (DirectedSparseVertex)vertexIt.next();
			if(v.getUserDatum(property) == null)
				continue;
			if(v.getUserDatum(property).equals(value))
				return v;
		}
		return null;
	}
	
	protected static Set<Vertex> findVertices(String property, Object value, Graph g){
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

}
