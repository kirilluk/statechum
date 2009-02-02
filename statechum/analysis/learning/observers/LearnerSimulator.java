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
 */ 

package statechum.analysis.learning.observers;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import statechum.StatechumXML;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import statechum.JUConstants;
import statechum.Pair;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.model.testset.PTASequenceEngine;
import statechum.analysis.learning.AbstractOracle;

/** An instance of this class behaves like a learner including calls to its decorators, 
 * but instead of running an experiment all data is retrieved from the supplied
 * XML file. This is useful in order to transform existing traces from a learner 
 * or make analysis of what it was doing, without actually re-running an experiment
 * which may be time-consuming. 
 *  
 * @author kirill
 */
public class LearnerSimulator extends ProgressDecorator
{
	/** Element of our XML file to consider next (if not a zip file). */
	protected int childOfTopElement =0;
	
	/** Elements of XML document we're going to play back (if not a zip file). */
	protected NodeList childElements = null;

	/** Zip input stream. */
	protected ZipInputStream inputZip = null;
	
	/** Graph compressor. */
	protected GraphSeries series = null;

	/** This method is aimed for loading an XML file with a fixed structure.
	 * Every time <em>expectNextElement</em> is called a caller expects that
	 * the next element in XML will have a specific tag and this method throws
	 * {@link IllegalArgumentException} if this is not the case.
	 * <p>Short description:
	 * extracts next element from the collection of children of the top-level one.
	 * Text nodes with whitespace are ignored.
	 * 
	 * @param name expected name, exception otherwise
	 * @return element
	 */ 
	public Element expectNextElement(String name)
	{
		org.w3c.dom.Node result = null;
		result = getNextElement();
		if (result == null)
			throw new IllegalArgumentException("failed to find element called "+name);

		if (!name.equals(result.getNodeName()))
			throw new IllegalArgumentException("encountered "+result.getNodeName()+" instead of "+name);
		return (Element)result;
	}

	protected InputStream inputStreamForXMLparser = new InputStream(){

		/* (non-Javadoc)
		 * @see java.io.InputStream#available()
		 */
		@Override
		public int available() throws IOException {
			return inputZip.available();
		}

		/* (non-Javadoc)
		 * @see java.io.InputStream#close()
		 */
		@Override
		public void close() {
			// does nothing to prevent XML parser from closing this stream.
		}

		/* (non-Javadoc)
		 * @see java.io.InputStream#mark(int)
		 */
		@Override
		public synchronized void mark(int readlimit) {
			inputZip.mark(readlimit);
		}

		/* (non-Javadoc)
		 * @see java.io.InputStream#markSupported()
		 */
		@Override
		public boolean markSupported() {
			return inputZip.markSupported();
		}

		/* (non-Javadoc)
		 * @see java.io.InputStream#read(byte[], int, int)
		 */
		@Override
		public int read(byte[] b, int off, int len)	throws IOException {
			return inputZip.read(b, off, len);
		}

		/* (non-Javadoc)
		 * @see java.io.InputStream#read(byte[])
		 */
		@Override
		public int read(byte[] b) throws IOException {
			return inputZip.read(b);
		}

		/* (non-Javadoc)
		 * @see java.io.InputStream#reset()
		 */
		@Override
		public synchronized void reset() throws IOException {
			inputZip.reset();
		}

		/* (non-Javadoc)
		 * @see java.io.InputStream#skip(long)
		 */
		@Override
		public long skip(long n) throws IOException {
			return inputZip.skip(n);
		}

		@Override
		public int read() throws IOException {
			return inputZip.read();
		}
		
	};
	
	/** There are cases when I'd like to step back to the previous element. */
	protected Element nextElement = null;
	
	/** This makes it possible to handle look-ahead by one token, by doing a <em>getNextElement()</em>
	 * and subsequently putting it back.
	 * 
	 * @param elem element to put back. This is expected to be an element obtained by 
	 * calling <em>getNextElement()</em>. 
	 */
	public void setNextElement(Element elem)
	{
		nextElement = elem;
	}
	
	/** Loads the next element from XML file. Returns <em>null</em> if there are 
	 * no more elements.
	 * Text nodes are ignored.
	 * <p>
	 * If there is a next element set, returns that element. 
	 */
	public Element getNextElement()
	{
		org.w3c.dom.Node result = null;
		if (nextElement != null)
		{
			result=nextElement;nextElement=null;
		}
		else
		if (readZip)
		{
			ZipEntry entry = null;
			try {
				entry = inputZip.getNextEntry();
			} catch (IOException e) {
				statechum.Helper.throwUnchecked("failed to load ZIP file entry", e);
			}

			if (entry != null) 
			{
				// Thanks to XML parser closing the input stream, I cannot directly pass
				// the Zip stream to it. Hence I have to subclass it in order to "disable" the 
				// close() command.
				Document d = getDocumentOfXML(new InputStreamReader(inputStreamForXMLparser));
				result = d.getFirstChild();
			}
		}
		else
		{
			do
			{
				result = childElements.item(childOfTopElement++);
			}
			while(childOfTopElement < childElements.getLength() &&
					result.getNodeType() == org.w3c.dom.Node.TEXT_NODE);
			
			if (result.getNodeType() == org.w3c.dom.Node.TEXT_NODE)
				result = null;
		}
		return (Element)result;
	}
	
	/** Whether the incoming stream is a zip file. */
	protected boolean readZip = true;
	
	/** Loads an XML from the supplied reader and returns the <em>Document</em> corresponding
	 * to it.
	 * 
	 * @param reader the reader from which to load XML.
	 * @return XML document.
	 */
	public static Document getDocumentOfXML(Reader reader)
	{
		Document result = null;
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		try
		{
			factory.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			result = factory.newDocumentBuilder().parse(new org.xml.sax.InputSource(reader));
		}
		catch(Exception e)
		{
			statechum.Helper.throwUnchecked("failed to construct/load DOM document",e);
		}
		return result;
	}
	
	public LearnerSimulator(InputStream inStream, boolean useZip) 
	{
		super(null);decoratedLearner=this;readZip=useZip;
		if (readZip)
		{
			inputZip = new ZipInputStream(new java.io.BufferedInputStream(inStream));
		}
		else
		{
			doc = getDocumentOfXML(new InputStreamReader(inStream));childElements = doc.getDocumentElement().getChildNodes();
		}
	}
	
	protected Learner topLevelListener = this;
	
	/** Sets the highest listener to receive notification calls; 
	 * it is expected that listeners will propagate calls down the chain
	 * so that this learner eventually gets to execute its own methods.
	 * 
	 * @param top new top of the stack of listeners.
	 */
	@Override
	public void setTopLevelListener(Learner top)
	{
		topLevelListener = top;
	}

	/** The element corresponding to the current method call. */
	protected Element currentElement = null;

	protected static final Map<String,StatechumXML> stringToEnumMap;
	
	static {
		stringToEnumMap = new TreeMap<String,StatechumXML>();
		for(StatechumXML kind: StatechumXML.values())
			stringToEnumMap.put(kind.name(), kind);
	}
	
	@Override
	public LearnerGraph learnMachine()
	{
		currentElement = expectNextElement(StatechumXML.ELEM_INIT.name());
		LearnerGraph graph = null, temp = null, result = null;
		while(currentElement != null)
		{
			final String elemName = currentElement.getNodeName();
			if (result != null) // we already know the final graph but there are more elements to come
				throw new IllegalArgumentException("unexpected element "+elemName+" after the learner result is known");
			StatechumXML kind = stringToEnumMap.get(elemName);
			
		 	if (elemName.equals(StatechumXML.graphmlNodeNameNS.toString()) ||
					elemName.equals(StatechumXML.gdGD.toString()))
			{
				String graphKind = currentElement.getAttribute(StatechumXML.ATTR_GRAPHKIND.name());
				if (graphKind.equals(StatechumXML.ATTR_LEARNINGOUTCOME.name()))
				{
					result = series.readGraph(currentElement);
					kind = StatechumXML.ATTR_GRAPHKIND;// means that this case was handled successfully.
				}
				else
					if (graphKind.equals(StatechumXML.ATTR_WITHCONSTRAINTS.name()))
					{
						kind = StatechumXML.ATTR_WITHCONSTRAINTS;// means that this case was handled successfully.
					}
					else
						throw new IllegalArgumentException("unexpected kind of graph: "+graphKind);

			}
		 	else
			if (kind != null)
				switch(kind)
				{
				case ATTR_WITHCONSTRAINTS:
					topLevelListener.AddConstraints(graph);break;
				case ELEM_ANSWER:
					List<String> question = readInputSequence(new java.io.StringReader(currentElement.getAttribute(StatechumXML.ATTR_QUESTION.name())),-1);
					Object outcome = topLevelListener.CheckWithEndUser(graph, question, AbstractOracle.USER_CANCELLED, AbstractOracle.USER_CANCELLED, null);
					assert outcome == expectedReturnValue;// yes, this should be b
					break;
				case ELEM_PAIRS:
					topLevelListener.ChooseStatePairs(graph);
					break;
				case ELEM_QUESTIONS:
					checkSingles(currentElement, childrenQuestions);
					topLevelListener.ComputeQuestions(readPair(graph, getElement(StatechumXML.ELEM_PAIR.name())),graph,temp);
					break;
				case ELEM_MERGEANDDETERMINIZE:
					if (StatechumXML.getChildWithTag(currentElement,StatechumXML.ELEM_PAIR.name()).getLength() != 1)
						throw new IllegalArgumentException("missing or duplicate pair, found "+StatechumXML.getChildWithTag(currentElement,StatechumXML.ELEM_PAIR.name()).getLength()+" pairs");
					
					temp = topLevelListener.MergeAndDeterminize(graph, readPair(graph, getElement(StatechumXML.ELEM_PAIR.name())));
					break;
				case ELEM_RESTART:
					if (!currentElement.hasAttribute(StatechumXML.ATTR_KIND.name())) throw new IllegalArgumentException("absent KIND attribute on RESTART");
					String restartKind = currentElement.getAttribute(StatechumXML.ATTR_KIND.name());
					RestartLearningEnum mode = Enum.valueOf(RestartLearningEnum.class, restartKind);
					topLevelListener.Restart(mode);
					if (mode == RestartLearningEnum.restartNONE)
						graph = temp;
					// if we are restarting, graph is unchanged.
					break;
				case ELEM_INIT:
					InitialData initial = readInitialData(currentElement);
					graph = topLevelListener.init(initial.plus,initial.minus);
					break;
				case ELEM_AUGMENTPTA:
					AugmentPTAData augmentData = readAugmentPTA(currentElement);
					topLevelListener.AugmentPTA(null, augmentData.kind, augmentData.sequence, augmentData.accept, augmentData.colour);
					break;
				default: kind = null; // force an exception
				}
				
		 		if (kind == null) throw new IllegalArgumentException("Unknown element in XML file "+elemName);
			currentElement = getNextElement();
		}
		
		return result;
	}

	/** Ideally, We'd like to detect whether decorators change our return values - 
	 * we cannot accommodate changes because we are only playing back rather 
	 * than doing learning. I think even user-abort should not be responded because
	 * our trace may include that one too, at least in principle. 
	 * Some changes can be easily detected,
	 * others, such as changes to mutable objects like graphs are perhaps not worth it.
	 */ 
	protected Object expectedReturnValue = null;
	
	/** Simulated check. Arguments are unused since they are loaded from XML by <em>learnMachine</em>.
	 *  
	 * @param g estimated graph, not loaded.
	 * @param question question loaded from XML
	 * @param responseForNoRestart ignored.
	 * @param lengthInHardFacts ignored.
	 * @param options set to null by the simulator.
	 * @return value loaded from XML
	 */
	public Pair<Integer,String> CheckWithEndUser(LearnerGraph g, List<String> question, int responseForNoRestart, @SuppressWarnings("unused") int lengthInHardFacts, Object[] options) 
	{
		Integer failedPosition = Integer.valueOf(currentElement.getAttribute(StatechumXML.ATTR_FAILEDPOS.name()));
		String ltlValue = null;
		if (currentElement.hasAttribute(StatechumXML.ATTR_LTL.name())) ltlValue = currentElement.getAttribute(StatechumXML.ATTR_LTL.name());
		Pair<Integer,String> returnValue = new Pair<Integer,String>(failedPosition,ltlValue);expectedReturnValue=returnValue;
		return returnValue;
	}

	/** Called by the simulator.
	 * 
	 * @param graph estimated graph
	 * @return loaded values from XML.
	 */
	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
	{
		org.w3c.dom.NodeList Pairs = currentElement.getChildNodes();
		Stack<PairScore> result = new Stack<PairScore>();
		for(int i=0;i<Pairs.getLength();++i)
		{
			org.w3c.dom.Node pair = Pairs.item(i);
			if (pair.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE)
			{
				Element elem = (Element) pair;
				if (!elem.getNodeName().equals(StatechumXML.ELEM_PAIR.name()))
						throw new IllegalArgumentException("unexpected node "+pair.getNodeName()+" among pairs");
				
				result.add(readPair(graph, elem));
			}
		}
		return result;
	}

	/** Called by the simulator.
	 * 
	 * @param pair loaded from XML.
	 * @param original estimated value.
	 * @param temp estimated value.
	 * @return loaded from XML.
	 */
	public List<List<String>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp)
	{
		return readSequenceList(getElement(StatechumXML.ELEM_SEQ.name()),StatechumXML.ATTR_QUESTIONS.name());
	}

	/** Extracts the child of the current element with the provided name. 
	 * 
	 * @param name the name of the element to retrieve
	 * @return loaded element
	 */
	public Element getElement(String name)
	{
		return (Element)StatechumXML.getChildWithTag(currentElement,name).item(0);
	}

	final static Set<String> childrenQuestions;
	
	static
	{
		childrenQuestions = new TreeSet<String>();childrenQuestions.addAll(Arrays.asList(new String[]{StatechumXML.ELEM_PAIR.name(),StatechumXML.ELEM_SEQ.name()}));
	}
	
	/** Loads the current learner input parameters and initialises the internal data in the simulator.
	 * If possible, this also loads the configuration and uses it for all methods requiring a configuration. 
	 */
	public LearnerEvaluationConfiguration readLearnerConstructionData()
	{
		Element evaluationData = expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name());
		LearnerEvaluationConfiguration cnf = readLearnerEvaluationConfiguration(evaluationData);
		config = cnf.config;
		series = new GraphSeries(config);
		return cnf;
	}

	/** Returns the graph stored in XML. Arguments are unused since they are passed from <em>learnMachine</em> which loads them from XML.
	 * 
	 * @param original graph to be processed, the simulator attempts to supply a relevant value, however it is not certain to be correct.
	 * @param pair the pair to be merged. Loaded from XML file.
	 * @return graph loaded from XML file.
	 */
	public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
	{
		Element graphNode = getElement(StatechumXML.gdGD.toString());
		if (graphNode == null) graphNode = getElement(StatechumXML.graphmlNodeNameNS.toString());
		if (graphNode == null) throw new IllegalArgumentException("failed to find a node with a graph");
		return series.readGraph(graphNode);
	}

	/** Does nothing in the simulator. 
	 * 
	 * @param mode value loaded from XML.
	 */
	public void Restart(RestartLearningEnum mode) 
	{// Does nothing in the simulator. 
	}

	/** We deliberately avoid storing this so as to be able to change 
	 * the format of diagnostics without having to regenerate test data. 
	 */
	public String getResult() 
	{
		return null;
	}

	/** Both arguments and the return value are stored by the simulator.
	 * Arguments are unused since they are passed from <em>learnMachine</em> which loads them from XML.
	 * 
	 * @param plus value loaded from XML
	 * @param minus value loaded from XML
	 */
	public LearnerGraph init(Collection<List<String>> plus, Collection<List<String>> minus) 
	{
		InitialData initial = readInitialData(currentElement);// wastefully load the element once again - does not matter because this is done very infrequently
		return initial.graph;
	}

	/** Since it is a simulator, only the return value is loaded from XML and whatever is 
	 * passed in is estimated.
	 */
	public LearnerGraph AddConstraints(@SuppressWarnings("unused") LearnerGraph graph) 
	{
		return series.readGraph(currentElement);
	}
	
	/** Since the learner does not know that the answer should be, we cannot 
	 * easily reconstruct the PTAEngine which is expected to be parameterised
	 * by the automaton. For this reason, we only store the corresponding collections and the 
	 * expected sizes in the xml. If called, this method will throw unsupported exception.
	 */
	public LearnerGraph init(@SuppressWarnings("unused") PTASequenceEngine engine, 
			@SuppressWarnings("unused")	int plusSize, 
			@SuppressWarnings("unused")	int minusSize) 
	{
		throw new UnsupportedOperationException("only init with collections is supported");
	}

	/** Does nothing in the simulator.
	 * 
	 * @param pta is always null in the simulator.
	 * @param ptaKind loaded from XML.
	 * @param sequence loaded from XML.
	 * @param accepted loaded from XML.
	 * @param newColour loaded from XML.
	 */
	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind, 
			List<String> sequence, boolean accepted, JUConstants newColour) 
	{// Does nothing in the simulator. 		
	}

	/** Since this is a simulator, values of the collections passed are ignored.
	 */
	@Override
	public LearnerGraph learnMachine(@SuppressWarnings("unused") Collection<List<String>> plus, 
			@SuppressWarnings("unused")	Collection<List<String>> minus)
	{
		return learnMachine();
	}

	/** Since this is a simulator, values of the collections passed are ignored.
	 */
	@Override
	public LearnerGraph learnMachine(@SuppressWarnings("unused") PTASequenceEngine engine, 
			@SuppressWarnings("unused") int plusSize, 
			@SuppressWarnings("unused") int minusSize)
	{
		return learnMachine();
	}

}
