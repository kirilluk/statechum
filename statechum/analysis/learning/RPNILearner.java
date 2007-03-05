package statechum.analysis.learning;

import java.util.*;

import javax.swing.*;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.algorithms.shortestpath.*;

public class RPNILearner implements Learner {
	
	private HashSet<List> sPlus, sMinus;
	private Graph currentGraph;
	private HashSet observers;
	private HashSet<StatePair>consolidated;
	
	public Graph getGraph(){
		return currentGraph;
	}
	
	public void updateGraph(Graph g){
		currentGraph = g;
		notifyObservers();
	}
	
	public RPNILearner(HashSet<List> sPlus, HashSet<List> sMinus){
		observers = new HashSet();
		consolidated = new HashSet<StatePair>();
		this.sPlus = sPlus;
		this.sMinus = sMinus;
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
	
	public DirectedSparseGraph learnMachine(){
		DirectedSparseGraph model = initialise();
		updateGraph(model);
		ArrayList<StatePair> possibleMerges = chooseStatePairs(model);
		while(!possibleMerges.isEmpty()){
			StatePair pair = possibleMerges.get(0);
			consolidated.add(pair);
			possibleMerges.remove(0);
			DirectedSparseGraph copy = (DirectedSparseGraph)model.copy();
			DirectedSparseGraph temp = merge(copy, pair);
			StatePair mergable = findMergablePair(temp);
			while(mergable!=null){
				copy = (DirectedSparseGraph)temp.copy();
				temp=merge(copy, mergable);
				mergable = findMergablePair(temp);
			}
			if(compatibleWithNegative(temp)){
				boolean ok = true;
				HashSet questions = generateQuestions(model, pair);
				questions = trimSet(questions);
				Iterator<List> questionIt = questions.iterator();
				while(questionIt.hasNext()){
					List question = questionIt.next();
					if(checkWithEndUser(question)){
						sPlus.add(question);
						System.out.println(question+ " <yes>");
					}
					else{
						sMinus.add(question);
						System.out.println(question+ " <no>");
						ok = false;
						break;
					}
				}
				if(ok){
					model = temp;
					updateGraph(model);
				}
			}
		}
		System.out.println("finished");
		return model;
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
		int answer = JOptionPane.showConfirmDialog(
                jf,
                questionString, "Valid input?",JOptionPane.YES_NO_OPTION);
		if(answer==0)
			return true;
		else
			return false;
	}
	
	private HashSet generateQuestions(DirectedSparseGraph model,  StatePair pair){
		Object qlabel = pair.getQ().getUserDatum("label");
		Object rlabel = pair.getR().getUserDatum("label");
		Vertex q = findVertex("label", qlabel,model);
		Vertex r = findVertex("label", rlabel,model);
		if(q==null || r ==null)
			return new HashSet();
		List sp = getShortPrefix(model, r);
		HashSet w = getSuffixes(model,q);
		Iterator<List> wIt = w.iterator();
		HashSet questions = new HashSet();
		while(wIt.hasNext()){
			ArrayList suffix = new ArrayList();
			List newQuestion = new ArrayList();
			List path = wIt.next();
			Iterator pathIt = path.iterator();
			while(pathIt.hasNext()){
				Edge e = (Edge)pathIt.next();
				suffix.add(e.getUserDatum("label").toString().trim());
			}
			newQuestion.addAll(sp);
			newQuestion.addAll(suffix);
			Vertex v = getVertex(model, newQuestion);
			if(v==null)
				questions.add(newQuestion);
		}
		return questions;
	}
	
	private HashSet getSuffixes(DirectedSparseGraph graph, Vertex r){
		HashSet setOfPaths = new HashSet();
				Iterator vertexIt = graph.getVertices().iterator();
		HashSet endVertices = new HashSet();
		DijkstraShortestPath p = new DijkstraShortestPath(graph);
		while(vertexIt.hasNext()){
			Vertex v = (Vertex)vertexIt.next();
			if(v.getSuccessors().isEmpty())
				endVertices.add(v);
		}
		Iterator<Vertex> endVertexIt = endVertices.iterator();
		while(endVertexIt.hasNext()){
			Vertex v = endVertexIt.next();
			List l = p.getPath(r, v);
			if(!l.isEmpty())
				setOfPaths.add(l);
		}
		return setOfPaths;
	}
	
	private List getShortPrefix(DirectedSparseGraph model, Vertex q){
		Vertex init = findVertex("property", "init",model);
		UnweightedShortestPath p = new UnweightedShortestPath(model);
		Iterator pathIt =  ShortestPathUtils.getPath(p, init, q).iterator();
		ArrayList list = new ArrayList();
		while(pathIt.hasNext()){
			Edge e = (Edge)pathIt.next();
			list.add(e.getUserDatum("label").toString().trim());
		}
		return list;
	}
	
	private boolean compatibleWithNegative(DirectedSparseGraph model){
		if(sMinus == null)
			return true;
		Iterator negativeIt = sMinus.iterator();
		boolean compatible = true;
		while(negativeIt.hasNext()){
			List string = (List)negativeIt.next();
			if(getVertex(model,string)!=null){
				compatible = false;
			}
		}
		return compatible;
	}
	
	private DirectedSparseGraph merge(DirectedSparseGraph model, StatePair pair){
		Vertex q = findVertex("label", pair.getQ().getUserDatum("label"),model);
		Vertex qDash = findVertex("label", pair.getR().getUserDatum("label"),model);
		if(!model.getVertices().contains(q)||!model.getVertices().contains(qDash))
			return model;
		Iterator inEdges = q.getInEdges().iterator();
		HashSet removeEdges = new HashSet();
		while(inEdges.hasNext()){
			DirectedSparseEdge e = (DirectedSparseEdge)inEdges.next();
			DirectedSparseEdge eDash = new DirectedSparseEdge(e.getSource(), qDash);
			eDash.addUserDatum("label", e.getUserDatum("label"), UserData.SHARED);
			if(!e.getSource().getSuccessors().contains(qDash))
				model.addEdge(eDash);
			removeEdges.add(e);
		}
		Iterator outEdges = q.getOutEdges().iterator();
		while(outEdges.hasNext()){
			DirectedSparseEdge e = (DirectedSparseEdge)outEdges.next();
			DirectedSparseEdge eDash = new DirectedSparseEdge(qDash, e.getDest());
			eDash.addUserDatum("label", e.getUserDatum("label"), UserData.SHARED);
			if(!qDash.getSuccessors().contains(e.getDest()))
				model.addEdge(eDash);
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
				if(doneLabels.get(e.getUserDatum("label"))==null)
					doneLabels.put(e.getUserDatum("label"), e);
				else {
					DirectedSparseEdge eDash = (DirectedSparseEdge)doneLabels.get(e.getUserDatum("label"));
					if(eDash.getDest().getUserDatum("property")!=null)
						continue;
					StatePair p = new StatePair(eDash.getDest(),e.getDest());
					return p;
				}
			}
		}
		return null;
	}
	
	private ArrayList<StatePair> chooseStatePairs(DirectedSparseGraph g){
		TreeMap stateList = new TreeMap();
		Iterator<Vertex> vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext()){
			Vertex current = vertexIt.next();
			Integer number = Integer.valueOf(current.getUserDatum("label").toString());
			stateList.put(number, current);
		}
		Iterator<Integer> keyIt = stateList.keySet().iterator();
		ArrayList<StatePair> pairs = new ArrayList<StatePair>();
		while(keyIt.hasNext()){
			Object key = keyIt.next();
			if(key.equals(stateList.firstKey()))
				continue;
			Vertex v = (Vertex)stateList.get(key);
			SortedMap subList = stateList.subMap(stateList.firstKey(), key);
			Iterator<Integer>subKeyIt = subList.keySet().iterator();
			while(subKeyIt.hasNext()){
				Object subKey = subKeyIt.next();
				Vertex w = (Vertex)subList.get(subKey);
				if(w.equals(v))
					continue;
				StatePair pair = new StatePair(v,w);
				if(!consolidated.contains(pair))
					pairs.add(pair);
			}
		}
		return pairs;
	}
	
	
	private DirectedSparseGraph initialise(){
		DirectedSparseGraph pta = new DirectedSparseGraph();
		DirectedSparseVertex init = new DirectedSparseVertex();
		init.addUserDatum("property", "init", UserData.SHARED);
		pta.addVertex(init);
		Iterator positiveIt = sPlus.iterator();
		while(positiveIt.hasNext()){
			List string = (List)positiveIt.next();
			for(int i = 1;i<=string.size();i++){
				List current = string.subList(0, i);
				if(getVertex(pta,current)==null){
					Vertex v = new DirectedSparseVertex();
					pta.addVertex(v);
					Vertex previous;
					if(i==1)
						previous = findVertex("property", "init", pta);
					else
						previous = getVertex(pta, string.subList(0, i-1));
					DirectedSparseEdge e = new DirectedSparseEdge(previous, v);
					e.addUserDatum("label", string.get(i-1), UserData.SHARED);
					pta.addEdge(e);
				}
				
			}
		}
		numberVertices(pta);
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
	
	private Vertex getVertex (DirectedSparseGraph g, List string){
		Vertex init = findVertex("property", "init",g);
		Vertex current = init;
		for(int i = 0;i<string.size();i++){
			Object label = string.get(i);
			DirectedSparseEdge edge = getEdgeWithLabel(current.getOutEdges(), label);
			if(edge == null)
				return null;
			current = edge.getDest();
		}
		return current;
	}
	
	private DirectedSparseEdge getEdgeWithLabel(Set edges, Object label){
		Iterator edgeIt = edges.iterator();
		while(edgeIt.hasNext()){
			DirectedSparseEdge e = (DirectedSparseEdge)edgeIt.next();
			if(e.getUserDatum("label").equals(label))
				return e;
		}
		return null;
	}
	
	private Vertex findVertex(String property, Object value, Graph g){
		Iterator vertexIt = g.getVertices().iterator();
		while(vertexIt.hasNext()){
			Vertex v = (Vertex)vertexIt.next();
			if(v.getUserDatum(property) == null)
				continue;
			if(v.getUserDatum(property).equals(value))
				return v;
		}
		return null;
	}

}
