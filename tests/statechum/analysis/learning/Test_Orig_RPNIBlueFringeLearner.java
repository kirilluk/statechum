/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
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
 * 
 * 
 * This is the original version of the learner, kept in order to perform regression testing.
 */ 
package statechum.analysis.learning;

import java.awt.Frame;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSet;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.algorithms.shortestpath.ShortestPathUtils;
import edu.uci.ics.jung.algorithms.shortestpath.UnweightedShortestPath;
import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;
import static statechum.DeterministicDirectedSparseGraph.findEdge;

/** An old implementation of the learner, kept only for regression testing at a
 * unit level, hence methods supporting logging for integration testing purpose
 * are not implemented. 
 */
public class Test_Orig_RPNIBlueFringeLearner extends RPNILearner {
	
	public Test_Orig_RPNIBlueFringeLearner(Frame parent, Configuration c, ConvertALabel conv) {
		super(parent, c, conv);
	}

	protected Graph currentGraph = DeterministicDirectedSparseGraph.initialise();
	protected HashSet<DirectedSparseEdge> doneEdges;
	protected Collection<List<Label>> sPlus, sMinus;

	@SuppressWarnings("unchecked")
	public static Collection<Label> getAlphabetForEdges(Collection<Edge> edges){
		Set<Label> alphabet = new TreeSet<Label>();
		Iterator<Edge> edgeIt = edges.iterator();
		while(edgeIt.hasNext()){
			Edge e = (edgeIt.next());
			alphabet.addAll((Collection<Label>)e.getUserDatum(JUConstants.LABEL));
		}
		return alphabet;
	}

	protected Graph removeNegatives(Graph g){
		@SuppressWarnings("unchecked")
		Iterator<Vertex> vertexIt = g.getVertices().iterator();
		HashSet<Vertex> remove = new HashSet<Vertex>();
		while(vertexIt.hasNext()){
			Vertex v = vertexIt.next();
			if(!DeterministicDirectedSparseGraph.isAccept(v)&&!DeterministicDirectedSparseGraph.isInitial(v))
				remove.add(v);
		}
		Iterator<Vertex> removeIt = remove.iterator();
		while(removeIt.hasNext()){
			g.removeVertex(removeIt.next());
		}
		return g;
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
			@SuppressWarnings("unchecked")
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
		
		@Override
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
		
		@Override
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
				Object otherQ = other.getQ().getUserDatum(JUConstants.LABEL);
				Object otherR = other.getR().getUserDatum(JUConstants.LABEL);
				Object thisQ = q.getUserDatum(JUConstants.LABEL);
				Object thisR = r.getUserDatum(JUConstants.LABEL);
				if(thisQ.equals(otherQ)&&thisR.equals(otherR))
					return true;
			}
			return false;
		}
	}	
	
	/** Makes a copy of the graph given to it and merges states in the pair supplied. */
	public static DirectedSparseGraph mergeAndDeterminize(Graph model, OrigStatePair pair){
		Graph original = (Graph)model.copy();
		Vertex q = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, pair.getQ().getUserDatum(JUConstants.LABEL),original);
		Vertex qDash = DeterministicDirectedSparseGraph.findVertex(JUConstants.LABEL, pair.getR().getUserDatum(JUConstants.LABEL),original);
		
		DirectedSparseGraph temp = merge((DirectedSparseGraph)original, new OrigStatePair(q,qDash));
		OrigStatePair mergable = findMergablePair(temp);
		while(mergable!=null){
			temp=merge(temp, mergable);
			mergable = findMergablePair(temp);
		}
		return temp;
	}
	
	protected static DirectedSparseGraph createAugmentedPTA(Collection<List<Label>> plus, Collection<List<Label>> minus){
		DirectedSparseGraph model = DeterministicDirectedSparseGraph.initialise();
		model = augmentPTA(model, minus, false);
		model = augmentPTA(model, plus, true);
		DeterministicDirectedSparseGraph.numberVertices(model);
		return model;
	}
	
	@Override
	public LearnerGraph init(Collection<List<Label>> plus, Collection<List<Label>> minus)
	{
		sPlus = plus;
		sMinus = minus;		
		return new LearnerGraph(createAugmentedPTA(sPlus, sMinus),Configuration.getDefaultConfiguration());
	}

	@Override
	public LearnerGraph init(@SuppressWarnings("unused") PTASequenceEngine en, 
			@SuppressWarnings("unused") int plus, 
			@SuppressWarnings("unused") int minus) 
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public LearnerGraph learnMachine(@SuppressWarnings("unused") PTASequenceEngine en, 
			@SuppressWarnings("unused") int plus, 
			@SuppressWarnings("unused") int minus) 
	{
		throw new UnsupportedOperationException();
	}

	/** This one is a very old implementation of the learner, kept for some regression testing 
	 * but not participating in the logging-and-comparing type of testing.
	 */ 
	@Override 
	public LearnerGraph learnMachine(@SuppressWarnings("unused") Collection<List<Label>> plus, @SuppressWarnings("unused") Collection<List<Label>> minus) {
		DirectedSparseGraph model = init(sPlus, sMinus).pathroutines.getGraph();
		
		Vertex init = DeterministicDirectedSparseGraph.findInitial(model);
		init.setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.SHARED);

		Stack<OrigStatePair> possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		while(!possibleMerges.isEmpty()){
			OrigStatePair pair = possibleMerges.pop();

			DirectedSparseGraph temp = mergeAndDeterminize((Graph)model.copy(), pair);
			if(compatible(temp, sPlus, sMinus)){// KIRR: this should always return true
				pair.getQ().setUserDatum(JUConstants.HIGHLIGHT, pair, UserData.SHARED);
				pair.getR().setUserDatum(JUConstants.HIGHLIGHT, pair, UserData.SHARED);
				setChanged();
				Collection<List<Label>> questions;
				if(config.getAskQuestions())
					questions = generateQuestions(model, pair);
				else
					questions = new LinkedList<List<Label>>();
				questions = trimSet(questions);
				Iterator<List<Label>> questionIt = questions.iterator();
				while(questionIt.hasNext()){
					List<Label> question = questionIt.next();
					boolean accepted = DeterministicDirectedSparseGraph.isAccept(pair.getQ());// Q is the blue vertex
					Pair<Integer,String> response = CheckWithEndUser(new LearnerGraph(model,Configuration.getDefaultConfiguration()),question,
							AbstractOracle.USER_CANCELLED,null,null,new Object[0]);// zero means "yes", everything else is "no"
					pair.getQ().removeUserDatum(JUConstants.HIGHLIGHT);
					pair.getR().removeUserDatum(JUConstants.HIGHLIGHT);
					if(response.firstElem == AbstractOracle.USER_ACCEPTED){
						sPlus.add(question);
						System.out.println(question+ " <yes>");
						if(accepted == false)// KIRR: how can this be true? If it were so, there would be no questions to ask
							return learnMachine(sPlus,sMinus);
					}
					else{
						sMinus.add(question.subList(0, response.firstElem));
						System.out.println(question+ " <no>");
						if(accepted == true){// KIRR: this cannot be false either
							return learnMachine(sPlus,sMinus);
						}
					}
				}
				model = temp;
			}
			possibleMerges = chooseStatePairs(model, sPlus, sMinus);
		}
		System.out.println("finished");
		return new LearnerGraph(model,Configuration.getDefaultConfiguration());
	}
	
	protected boolean compatible(DirectedSparseGraph model, Collection<List<Label>> plus, Collection<List<Label>> minus){
		boolean returnValue = true;
		for(List<Label> string:minus)
		{
			Vertex v = getVertex(model,string);
			if(v != null){
				if(DeterministicDirectedSparseGraph.isAccept(v))
					returnValue = false;
			}
		}
		for(List<Label> string:plus)
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
	protected Collection<List<Label>> trimSet(Collection<List<Label>> questions){
		PTASequenceSet collection = new PTASequenceSet();collection.addAll(questions);
		return collection.getData();
	}

	/*
	 * needs to be refactored into smaller methods
	 */
	@SuppressWarnings("unchecked")
	protected List<List<Label>> generateQuestions(DirectedSparseGraph model, OrigStatePair pair){
		Vertex q = pair.getQ();
		Vertex r = pair.getR();
		if(q==null || r ==null)
			return new LinkedList<List<Label>>();
		boolean accepted = DeterministicDirectedSparseGraph.isAccept(q);
		if(accepted == false)
			return new LinkedList<List<Label>>();
		List<Label> sp = null;
		Set<List<Label>> w =null;
		if(!hasAcceptedNeighbours(q)){
			sp = getShortPrefix(model, q);
			w = getShortSuffixes(model, r);
		}
		else{
			sp = getShortPrefix(model, r);// shortest sequence to the red state 
			w = getSuffixes(model,q, accepted);
		}
		Iterator<List<Label>> wIt;
		List<List<Label>> questions = new LinkedList<List<Label>>();
		Set<Label>loopLabels = new TreeSet<Label>();
		boolean loopToR = r.getSuccessors().contains(r);
		boolean redAndBlueNeighbours = r.getNeighbors().contains(q);
		if(loopToR||redAndBlueNeighbours){ //there either exists a loop to r or will do if r and b merge
			if(loopToR){
				Edge e = findEdge(r, r);
				Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
				loopLabels.addAll(labels);
			}
			if(redAndBlueNeighbours){
				Edge e = findEdge(r,q);
				Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
				loopLabels.addAll(labels);
			}
		}
		wIt = w.iterator();
		while(wIt.hasNext()){
			List<Label> suffix = wIt.next();
			Iterator<Label> labelIt = loopLabels.iterator();
			if(!loopLabels.isEmpty()){
				while(labelIt.hasNext()){
					List<Label> newQuestion = new LinkedList<Label>();
					newQuestion.addAll(sp);
					newQuestion.add(labelIt.next());
					newQuestion.addAll(suffix);
					Vertex v = getVertex(model, newQuestion);
					if(v==null)
						questions.add(newQuestion);
				}
			}
			List<Label> newQuestion = new LinkedList<Label>();
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
	protected static Set<List<Label>> getShortSuffixes(@SuppressWarnings("unused") DirectedSparseGraph g, Vertex v){
		Set<List<Label>> returnStrings = new LinkedHashSet<List<Label>>();
		@SuppressWarnings("unchecked")
		Iterator<Edge> outEdgeIt = v.getOutEdges().iterator();
		while(outEdgeIt.hasNext()){
			Edge e = outEdgeIt.next();
			if(DeterministicDirectedSparseGraph.isAccept(e.getOpposite(v))){
				ArrayList<Edge> l = new ArrayList<Edge>();
				l.add(e);
				returnStrings.addAll(getPaths(l));
			}
		}
		return returnStrings;
	}
	
	public static boolean hasAcceptedNeighbours(Vertex v){
		@SuppressWarnings("unchecked")
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
	@SuppressWarnings("unchecked")
	protected Set<List<Label>> getSuffixes(DirectedSparseGraph graph, Vertex r, boolean accepted){
		Set<List<Label>> setOfPaths = new HashSet<List<Label>>();
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
			List<Edge> l = p.getPath(r, v);
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
	protected static Set<List<Label>> getPaths(List<Edge> l){
		TreeMap<Integer,Set<List<Label>>> returnSet = new TreeMap<Integer,Set<List<Label>>>();// KIRR: this should not be done in this way since access to this map is not random - you only need the last element in which case simply storing the last set of lists is best
		for(int i=0;i<l.size();i++){// for each element of the source list
			Edge e = l.get(i);
			@SuppressWarnings("unchecked")
			Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
			Set<List<Label>> strings = new HashSet<List<Label>>();
			for(Label s:labels)
			{
				if(i==0){
					List<Label> string = new LinkedList<Label>();
					string.add(s);
					strings.add(string);			
				}
				else{
					Set<List<Label>> oldStrings = returnSet.get(i-1);
					Iterator<List<Label>> stringIt = oldStrings.iterator();
					while(stringIt.hasNext()){
						List<Label> newString = new LinkedList<Label>();
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
		
		return new LinkedHashSet<List<Label>>();
	}
	
	/** Returns a sequence of names labelling a shortest path from the initial node to node q. */
	@SuppressWarnings("unchecked")
	protected static List<Label> getShortPrefix(DirectedSparseGraph model, Vertex q){
		Vertex init = DeterministicDirectedSparseGraph.findInitial(model);
		UnweightedShortestPath p = new UnweightedShortestPath(model);
		Iterator<Edge> pathIt =  ShortestPathUtils.getPath(p, init, q).iterator();
		List<Label> list = new LinkedList<Label>();
		while(pathIt.hasNext()){
			Edge e = pathIt.next();
			Set<Label> s = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
			Label[] strings = s.toArray(new Label[0]);
			list.add(strings[0]);
		}
			
		return list;
	}

	
	@SuppressWarnings("unchecked")
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
				Set<Label> labels = (Set<Label>)existing.getUserDatum(JUConstants.LABEL);// KIRR: if you use UserData.SHARED, you do not need to copy the result back using put
				labels.addAll((Set<Label>)e.getUserDatum(JUConstants.LABEL));
				existing.setUserDatum(JUConstants.LABEL, labels, UserData.CLONE);
			}
			removeEdges.add(e);
		}
		Iterator<DirectedSparseEdge> outEdges = q.getOutEdges().iterator();
		while(outEdges.hasNext()){
			DirectedSparseEdge e = outEdges.next();
			DirectedSparseEdge eDash = new DirectedSparseEdge(qDash, e.getDest());
			eDash.addUserDatum(JUConstants.LABEL, e.getUserDatum(JUConstants.LABEL), UserData.CLONE);
			if(!qDash.getSuccessors().contains(e.getDest())) 
				model.addEdge(eDash);
			else{
				Edge existing = findEdge(qDash, e.getDest());
				Set<Label> labels = (Set<Label>)existing.getUserDatum(JUConstants.LABEL);
				labels.addAll((Set<Label>)e.getUserDatum(JUConstants.LABEL));
				existing.setUserDatum(JUConstants.LABEL, labels, UserData.CLONE);
			}
			removeEdges.add(e);
		}
		model.removeEdges(removeEdges);
		model.removeVertex(q);
		return model;
	}
	
	/** After merging, a graph may exhibit non-determinism, in which case it is made deterministic
	 * by merging nodes. For instance, for A->B and A->C being a non-deterministic choice at node A, 
	 * nodes B and C are to be merged. This function identifies such a (B,C).
	 * 
	 * @param model
	 * @return a pair of states to be merged or null if the graph is deterministic.
	 */
	@SuppressWarnings("unchecked")
	protected static OrigStatePair findMergablePair(DirectedSparseGraph model){
		List<Vertex> queue = DeterministicDirectedSparseGraph.getBFSList(model);
		Iterator<Vertex> queueIt = queue.iterator();
		while(queueIt.hasNext()){
			Vertex v = queueIt.next();
			Set<DirectedSparseEdge> edges = v.getOutEdges();
			Iterator<DirectedSparseEdge> edgeIt = edges.iterator();
			Map<Label,DirectedSparseEdge> doneLabels = new HashMap<Label,DirectedSparseEdge>();
			while(edgeIt.hasNext()){
				DirectedSparseEdge e = edgeIt.next();
				Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
				Iterator<Label> labelIt = labels.iterator();
				while(labelIt.hasNext()){
					Label label = labelIt.next();
					if(doneLabels.get(label)==null)
						doneLabels.put(label, e);
					else {
						DirectedSparseEdge eDash = doneLabels.get(label);
						OrigStatePair p = null;
						if(DeterministicDirectedSparseGraph.isInitial(eDash.getDest()))
							p = new OrigStatePair(e.getDest(), eDash.getDest());
						else
							p = new OrigStatePair(eDash.getDest(),e.getDest());
						if(!different(p)) // KIRR: strange - the two should never be different if the original pair to choose was selected properly
							return p;
					}
				}
			}
		}
		return null;
	}

	protected Stack<OrigStatePair> chooseStatePairs(DirectedSparseGraph g, Collection<List<Label>> sPlusArg, Collection<List<Label>> sMinusArg){
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
				doneEdges = new HashSet<DirectedSparseEdge>();
				Integer score = new Integer(computeScore(g,pair));
				DirectedSparseGraph temp = mergeAndDeterminize((Graph)g.copy(), pair);
				if(compatible(temp, sPlusArg, sMinusArg)){
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
				Iterator<Integer> keyIt = singleSet.keySet().iterator();
				while(keyIt.hasNext()){
					Integer key = keyIt.next();
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
	@SuppressWarnings("unchecked")
	protected int computeScore(DirectedSparseGraph original, OrigStatePair blueRed){
		int returnValue = 0;
		if(different(blueRed))
				return -1;

		Iterator<DirectedSparseEdge> edgesOut = blueRed.getQ().getOutEdges().iterator();
		while(edgesOut.hasNext()){
			DirectedSparseEdge e = edgesOut.next();
			if(this.doneEdges.contains(e))
				continue;
			
			doneEdges.add(e);
			Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
			Iterator<Label> labelIt = labels.iterator();
			while(labelIt.hasNext()){
				List<Label> string = new LinkedList<Label>();
				string.add(labelIt.next());
				Vertex qi = e.getDest();
				Vertex qj = getVertex(original,blueRed.getR(), string);
				OrigStatePair newPair = new OrigStatePair(qi, qj);
				if(qj!=null){
					int equivalent = computeScore(original, newPair);
					if(equivalent<0){
						return -1;
					}
					
					returnValue++;
					returnValue= returnValue + equivalent;
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
	static DirectedSparseGraph augmentPTA(DirectedSparseGraph pta, Collection<List<Label>> strings, boolean accepted)
	{
		Iterator<List<Label>> stringsIt = strings.iterator();
		while(stringsIt.hasNext()){
			List<Label> string = stringsIt.next();
			if (string.isEmpty() && !accepted)
				throw new IllegalArgumentException("since the initial state is an accept one, a negative string should not be empty");
			
			for(int i = 1;i<=string.size();i++){
				List<Label> current = string.subList(0, i);
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
					Set<Label> labels = new TreeSet<Label>();
					labels.add(string.get(i-1));
					e.addUserDatum(JUConstants.LABEL, labels, UserData.CLONE);
					pta.addEdge(e);
				}
				else
					if (different(new OrigStatePair(existing,newVertex)))
					{
						existing.addUserDatum(JUConstants.HIGHLIGHT, "whatever", UserData.SHARED);
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
	
	protected static Vertex getVertex (@SuppressWarnings("unused") DirectedSparseGraph g,Vertex v, List<Label> string){
		Vertex current = v;
		if (current == null)
			return null;
		
		for(int i = 0;i<string.size();i++){
			Label label = string.get(i);
			@SuppressWarnings("unchecked")
			DirectedSparseEdge edge = getEdgeWithLabel(current.getOutEdges(), label);
			if(edge == null)
				return null;
			current = edge.getDest();
		}
		return current;
	}
	
	public static Vertex getVertex (DirectedSparseGraph g, List<Label> string){
		return getVertex(g, DeterministicDirectedSparseGraph.findInitial(g), string);
	}

	public static DirectedSparseEdge getEdgeWithLabel(Set<DirectedSparseEdge> edges, Label label){
		Iterator<DirectedSparseEdge> edgeIt = edges.iterator();
		while(edgeIt.hasNext()){
			DirectedSparseEdge e = edgeIt.next();
			@SuppressWarnings("unchecked")
			Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
			if(labels.contains(label))
				return e;
		}
		return null;
	}

	/** Checks for shallow compatibility between states, in other words if the two are both accept or both reject states
	 * 
	 * @param pair a pair states to check for compatibility. 
	 * @return whether the two are different.
	 */
	public static boolean different(OrigStatePair pair){
		boolean qAcceptedO = DeterministicDirectedSparseGraph.isAccept(pair.getQ());
		boolean rAcceptedO = DeterministicDirectedSparseGraph.isAccept(pair.getR());
	
		return qAcceptedO != rAcceptedO;
	}

	@Override
	public String getResult() {
		return null;
	}

	@Override
	public Stack<PairScore> ChooseStatePairs(@SuppressWarnings("unused") LearnerGraph graph) 
	{
		throw new UnsupportedOperationException("uses an internal method");
	}

	@Override
	public List<List<Label>> ComputeQuestions(@SuppressWarnings("unused") PairScore pair,
			@SuppressWarnings("unused")	LearnerGraph original, 
			@SuppressWarnings("unused")	LearnerGraph temp) 
	{
		throw new UnsupportedOperationException("uses an internal method");
	}

	@Override
	public List<List<Label>> RecomputeQuestions(@SuppressWarnings("unused") PairScore pair,
			@SuppressWarnings("unused")	LearnerGraph original, 
			@SuppressWarnings("unused")	LearnerGraph temp) 
	{
		throw new UnsupportedOperationException("uses an internal method");
	}

	@Override
	public LearnerGraph MergeAndDeterminize(@SuppressWarnings("unused")	LearnerGraph original,
			@SuppressWarnings("unused")	StatePair pair) 
	{
		throw new UnsupportedOperationException("uses an internal method");
	}

	@Override
	public LearnerGraph learnMachine() 
	{
		throw new UnsupportedOperationException("uses an internal method");
	}

	@Override
	public void AugmentPTA(@SuppressWarnings("unused") LearnerGraph pta, 
			@SuppressWarnings("unused")	RestartLearningEnum ptaKind,
			@SuppressWarnings("unused")	List<Label> sequence, 
			@SuppressWarnings("unused")	boolean accepted, 
			@SuppressWarnings("unused")	JUConstants newColour) 
	{
		throw new UnsupportedOperationException("uses an internal method");
	}

	@Override
	public boolean AddConstraints(@SuppressWarnings("unused") LearnerGraph graph, @SuppressWarnings("unused") LearnerGraph outcome, @SuppressWarnings("unused") StringBuffer counterExampleHolder) 
	{
		throw new UnsupportedOperationException("does not use constraints");
	}
}
