package statechum.analysis.learning;

import java.util.*;

import javax.swing.*;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.algorithms.shortestpath.*;

public class RPNIBlueFringeLearner implements Learner {
	private Graph currentGraph;
	private HashSet observers, doneEdges;
	
	public Graph getGraph(){
		return removeNegatives(currentGraph);
		//return currentGraph;
	}
	
	private Graph removeNegatives(Graph g){
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
	
	public void updateGraph(Graph g){
		currentGraph = (Graph)g.copy();
		notifyObservers();
	}
	
	public RPNIBlueFringeLearner(){
		observers = new HashSet();
	}
	
	public void addObserver(Observer o){
		observers.add(o);
	}
	
	public void removeObserver(Observer o){
		observers.remove(o);
	}
	
	private void notifyObservers() {
	    Iterator i = observers.iterator();
	    while( i.hasNext() ) {
	          Observer o = ( Observer ) i.next();
	          o.update( this );
	    }
	}
	
	private HashSet computeBlue(DirectedSparseGraph model){
		HashSet blues = new HashSet();
		HashSet reds = findVertices("colour", "red", model);
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
				Object accepted = neighbour.getUserDatum("accepted");
				neighbour.setUserDatum("colour", "blue", UserData.SHARED);
				blues.add(neighbour);
				
			}
		}
		updateGraph(model);
		return blues;
	}
	
	private DirectedSparseGraph mergeAndDeterminize(Graph original, StatePair pair){
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
	
	public DirectedSparseGraph learnMachine(DirectedSparseGraph model, HashSet sPlus, HashSet sMinus){
		model = augmentPTA(model, sPlus, true);
		model = augmentPTA(model, sMinus, false);
		numberVertices(model);
		Vertex init = findVertex("property", "init",model);
		init.setUserDatum("colour", "red", UserData.SHARED);
		Stack possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		while(!possibleMerges.isEmpty()){
			StatePair pair = (StatePair)possibleMerges.pop();
			DirectedSparseGraph temp = mergeAndDeterminize((Graph)model.copy(), pair);
			if(compatible(temp, sPlus, sMinus)){
				HashSet questions = generateQuestions(model, pair);
				questions = trimSet(questions);
				Iterator<List> questionIt = questions.iterator();
				while(questionIt.hasNext()){
					List question = questionIt.next();
					String accepted = pair.getQ().getUserDatum("accepted").toString();
					if(checkWithEndUser(question)){
						sPlus.add(question);
						System.out.println(question+ " <yes>");
						if(accepted.equals("false"))
							return learnMachine(initialise(), sPlus, sMinus);
					}
					else{
						sMinus.add(question);
						System.out.println(question+ " <no>");
						if(accepted.equals("true")){
							return learnMachine(initialise(), sPlus, sMinus);
						}
					}
				}
				model = temp;
				//updateGraph(model);
			}
			possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		}
		System.out.println("finished");
		return model;
	}
	
	private boolean compatible(DirectedSparseGraph model, HashSet sPlus, HashSet sMinus){
		boolean returnValue = true;
		Iterator negativeIt = sMinus.iterator();
		while(negativeIt.hasNext()){
			List string = (List)negativeIt.next();
			Vertex v = getVertex(model,string);
			if(v != null){
				Object accepted = v.getUserDatum("accepted");
				if(accepted.toString().equals("true"))
					returnValue = false;
			}
		}
		Iterator positiveIt = sPlus.iterator();
		while(positiveIt.hasNext()){
			List string = (List)positiveIt.next();
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
	
	
	private HashSet trimSet(HashSet questions){
		HashSet done = new HashSet();
		HashSet trimmedSet = new HashSet();
		Iterator questionIt = questions.iterator();
		while(questionIt.hasNext()){
			List question = (List)questionIt.next();
			Iterator stringIt = question.iterator();
			String questionString=new String();
			while(stringIt.hasNext())
				questionString = questionString.concat((String)stringIt.next());
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
	
	private HashSet generateQuestions(DirectedSparseGraph model, StatePair pair){
		Vertex q = pair.getQ();
		Vertex r = pair.getR();
		if(q==null || r ==null)
			return new HashSet();
		String accepted = q.getUserDatum("accepted").toString();
		if(accepted.equals("false"))
			return new HashSet();
		List sp = getShortPrefix(model, r);
		HashSet w = getSuffixes(model,q, accepted);
		Iterator<List> wIt;
		HashSet questions = new HashSet();
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
			ArrayList suffix = new ArrayList();
			List newQuestion = new ArrayList();
			List path = wIt.next();
			Iterator pathIt = path.iterator();
			while(pathIt.hasNext()){
				String label = (String)pathIt.next();
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
	
	private HashSet<List> getSuffixes(DirectedSparseGraph graph, Vertex r, String accepted){
		HashSet setOfPaths = new HashSet();
				Iterator vertexIt = graph.getVertices().iterator();
		HashSet endVertices = new HashSet();
		DijkstraShortestPath p = new DijkstraShortestPath(graph);
		while(vertexIt.hasNext()){
			Vertex v = (Vertex)vertexIt.next();
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
	
	private Collection getPaths(List<Edge> l){
		TreeMap returnSet = new TreeMap();
		for(int i=0;i<l.size();i++){
			Edge e = l.get(i);
			HashSet labels = (HashSet)e.getUserDatum("label");
			Iterator<String> labelIt = labels.iterator();
			HashSet strings = new HashSet();
			while(labelIt.hasNext()){
				String s = labelIt.next();
				if(i==0){
					ArrayList string = new ArrayList();
					string.add(s);
					strings.add(string);
			
				}
				else{
					HashSet oldStrings = (HashSet)returnSet.get(i-1);
					Iterator<ArrayList> stringIt = oldStrings.iterator();
					while(stringIt.hasNext()){
						ArrayList newString = new ArrayList();
						newString.addAll(stringIt.next());
						newString.add(s);
						strings.add(newString);
					}	
				}
			}
			returnSet.put(i, strings);
		}
		return (HashSet)returnSet.get(returnSet.lastKey());
	}
	
	private List getShortPrefix(DirectedSparseGraph model, Vertex q){
		Vertex init = findVertex("property", "init",model);
		UnweightedShortestPath p = new UnweightedShortestPath(model);
		Iterator pathIt =  ShortestPathUtils.getPath(p, init, q).iterator();
		ArrayList list = new ArrayList();
		while(pathIt.hasNext()){
			Edge e = (Edge)pathIt.next();
			HashSet s = (HashSet)e.getUserDatum("label");
			Object[] strings = s.toArray();
			list.add(strings[0].toString());
		}
		
			
		return list;
	}

	
	private DirectedSparseGraph merge(DirectedSparseGraph model, StatePair pair){
		Vertex q = pair.getQ();
		Vertex qDash = pair.getR();
		Iterator inEdges = q.getInEdges().iterator();
		HashSet removeEdges = new HashSet();
		while(inEdges.hasNext()){
			DirectedSparseEdge e = (DirectedSparseEdge)inEdges.next();
			DirectedSparseEdge eDash = new DirectedSparseEdge(e.getSource(), qDash);
			eDash.addUserDatum("label", e.getUserDatum("label"), UserData.CLONE);
			if(!e.getSource().getSuccessors().contains(qDash)) 
				model.addEdge(eDash);
			else{
				Edge existing = findEdge(e.getSource(), qDash);
				HashSet<String> labels = (HashSet<String>)existing.getUserDatum("label");
				labels.addAll((HashSet)e.getUserDatum("label"));
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
				HashSet<String> labels = (HashSet<String>)existing.getUserDatum("label");
				labels.addAll((HashSet)e.getUserDatum("label"));
				existing.setUserDatum("label", labels, UserData.CLONE);
			}
			removeEdges.add(e);
		}
		model.removeEdges(removeEdges);
		model.removeVertex(q);
		return model;
	}
	
	
	private ArrayList getBFSQueue(Graph g){
		ArrayList queue = new ArrayList();
		Vertex init = findVertex("property", "init",g);
		queue.add(0,init);
		int i=0;
		int j= queue.size();
		HashSet done = new HashSet();
		while(i<j){
			DirectedSparseVertex v = (DirectedSparseVertex)queue.get(i);
			done.add(v);
			Iterator succIt = v.getSuccessors().iterator();
			while(succIt.hasNext()){
				Vertex succ = (Vertex)succIt.next();
				if(!done.contains(succ))
					queue.add(succ);
			}
			queue.trimToSize();
			j = queue.size();
			i++;
		}
		return queue;
	}
	
	private StatePair findMergablePair(DirectedSparseGraph model){
		ArrayList queue = getBFSQueue(model);
		Iterator<Vertex> queueIt = queue.iterator();
		while(queueIt.hasNext()){
			Vertex v = queueIt.next();
			Set edges = v.getOutEdges();
			Iterator<DirectedSparseEdge> edgeIt = edges.iterator();
			HashMap doneLabels = new HashMap();
			while(edgeIt.hasNext()){
				DirectedSparseEdge e = edgeIt.next();
				HashSet<String> labels = (HashSet<String>)e.getUserDatum("label");
				Iterator<String> labelIt = labels.iterator();
				while(labelIt.hasNext()){
					String label = labelIt.next();
					if(doneLabels.get(label)==null)
						doneLabels.put(label, e);
					else {
						DirectedSparseEdge eDash = (DirectedSparseEdge)doneLabels.get(label);
						StatePair p = null;
						if(eDash.getDest().getUserDatum("property")!=null)
							p = new StatePair(e.getDest(), eDash.getDest());
						else
							p = new StatePair(eDash.getDest(),e.getDest());
						if(!different(p))
							return p;
					}
				}
			}
		}
		return null;
	}
	
	private Stack chooseStatePairs(DirectedSparseGraph g, HashSet sPlus, HashSet sMinus){
		int threshold = 1;
		Stack<Vertex> blueStack = new Stack();
		blueStack.addAll(computeBlue(g));
		TreeMap scoreToPair = new TreeMap();
		while(!blueStack.isEmpty()){
			TreeMap singleSet = new TreeMap();
			Vertex blueVertex = blueStack.pop();
			Stack<Vertex> redStack = new Stack();
			redStack.addAll(findVertices("colour", "red", g));
			while(!redStack.isEmpty()){
				Vertex redVertex = redStack.pop();
				StatePair pair = new StatePair(blueVertex, redVertex);
				doneEdges = new HashSet();
				Integer score = new Integer(equivalent(g,pair));
				if(score.intValue()<threshold)
					continue;
				DirectedSparseGraph temp = mergeAndDeterminize((Graph)g.copy(), pair);
				if(compatible(temp, sPlus, sMinus)){
					if(singleSet.get(score) == null){
						Stack<StatePair> s = new Stack<StatePair>();
						s.add(pair);
						singleSet.put(score, s);
					}
					else{
						Stack<StatePair> s = (Stack<StatePair>)singleSet.get(score);
						s.add(pair);
						singleSet.put(score, s);
					}	
				}
			}
			if(singleSet.isEmpty()){
				blueVertex.setUserDatum("colour", "red", UserData.SHARED);
				blueStack.clear();
				scoreToPair.clear();
				blueStack.addAll(computeBlue(g));
			}
			else{
				Iterator keyIt = singleSet.keySet().iterator();
				while(keyIt.hasNext()){
					Object key = keyIt.next();
					Stack<StatePair> s = (Stack<StatePair>)scoreToPair.get(key);
					if(s!=null){
						s.addAll((Stack<StatePair>)singleSet.get(key));
						scoreToPair.put(key, sort(s));
					}
					else
						scoreToPair.put(key, sort((Stack<StatePair>)singleSet.get(key)));
				}
			}
		}
		return createOrderedStack(scoreToPair);
	}
	
	private Vector<StatePair> sort(Stack pairs){
		Stack returnStack = new Stack();
		HashSet set = new HashSet();
		set.addAll(pairs);
		Object[] list = set.toArray();
		boolean swapped = true;
		while(swapped){
			swapped = false;
			for(int i=0;i<list.length;i++){
				StatePair p = (StatePair)list[i];
				if(i<=(list.length-2)){
					StatePair q = (StatePair)list[i+1];
					if(p.greaterThan(q)){
						list[i]=q;
						list[i+1]=p;
						swapped = true;
					}
				}
				else
					swapped = false;
				if(swapped){
					swapped = false;
					i=-1;
				}
			}
		}
		for(int i=list.length-1;i>=0;i--){
			returnStack.push(list[i]);
		}
		return returnStack;
	}
	
	private Stack createOrderedStack(TreeMap sets){
		Iterator<Stack> valueIt = sets.values().iterator();
		Stack allValues = new Stack();
		while(valueIt.hasNext()){
			Stack s = valueIt.next();
			allValues.addAll(s);
		}
		return allValues;
	}
	
	private boolean different(StatePair pair){
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
	
	
	private int equivalent(DirectedSparseGraph original, StatePair blueRed){
		int returnValue = 0;
		if(different(blueRed))
				return -1;
		else{
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
						int equivalent = equivalent(original, newPair);
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
		}
		return returnValue;
	}
	
	public DirectedSparseGraph initialise(){
		DirectedSparseGraph pta = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum("property", "init", UserData.SHARED);
		init.addUserDatum("accepted", "true", UserData.SHARED);
		pta.addVertex(init);
		return pta;
	}
	
	
	private DirectedSparseGraph augmentPTA(DirectedSparseGraph pta, HashSet strings, boolean accepted){
		Iterator negativeIt = strings.iterator();
		while(negativeIt.hasNext()){
			List string = (List)negativeIt.next();
			for(int i = 0;i<=string.size();i++){
				List current = string.subList(0, i);
				if(getVertex(pta,current)==null){
					Vertex v = new DirectedSparseVertex();
					v.setUserDatum("accepted", accepted, UserData.SHARED);
					pta.addVertex(v);
					Vertex previous;
					if(i==1)
						previous = findVertex("property", "init", pta);
					else
						previous = getVertex(pta, string.subList(0, i-1));
					DirectedSparseEdge e = new DirectedSparseEdge(previous, v);
					HashSet labels = new HashSet();
					labels.add(string.get(i-1));
					e.addUserDatum("label", labels, UserData.CLONE);
					pta.addEdge(e);
				}
			}
		}
		return pta;
	}
	
	/*
	 * Labels vertices according to breadth-first search
	 */
	private void numberVertices(DirectedSparseGraph pta){
		ArrayList queue = new ArrayList();
		Vertex init = findVertex("property", "init",pta);
		queue.add(init);
		int i=0;
		int j= queue.size();
		while(i<j){
			Vertex v = (Vertex)queue.get(i);
			v.addUserDatum("label", i, UserData.SHARED);
			queue.addAll(v.getSuccessors());
			j = queue.size();
			i++;
		}
		
	}
	
	private Vertex getVertex (DirectedSparseGraph g,Vertex v, List<String> string){
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
	
	private Vertex getVertex (DirectedSparseGraph g, List<String> string){
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
	
	private DirectedSparseEdge getEdgeWithLabel(Set edges, String label){
		Iterator edgeIt = edges.iterator();
		while(edgeIt.hasNext()){
			DirectedSparseEdge e = (DirectedSparseEdge)edgeIt.next();
			HashSet<String> labels = (HashSet<String>)e.getUserDatum("label");
			if(labels.contains(label))
				return e;
		}
		return null;
	}
	
	private DirectedSparseVertex findVertex(String property, Object value, Graph g){
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
	
	private HashSet findVertices(String property, Object value, Graph g){
		HashSet vertices = new HashSet();
		Iterator vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext()){
			Vertex v = (Vertex)vertexIt.next();
			if(v.getUserDatum(property) == null)
				continue;
			if(v.getUserDatum(property).equals(value))
				vertices.add(v);
		}
		return vertices;
	}
	
	private Edge findEdge(Vertex from, Vertex to){
		Iterator<DirectedSparseEdge> edgesOut = from.getOutEdges().iterator();
		while(edgesOut.hasNext()){
			DirectedSparseEdge current = edgesOut.next();
			if(current.getDest().equals(to))
				return current;
		}
		return null;
	}

}
