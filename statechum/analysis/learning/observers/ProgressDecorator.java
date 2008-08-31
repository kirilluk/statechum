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

/* Design problems:
 * 
 * XMLEncoder is a good idea in theory - it records a sequence of Java commands 
 * which would be necessary to set the properties of Java beans to load the data 
 * (which has yet to be stored). This mechanism even supports non-default 
 * constructors and factory methods. The trouble is that it is very verbose - 
 * 500Meg of a trace of Statechum is too much for CVS version control over 256k 
 * uplink, likely even for SF to store. GZIPpping kind of defeats the purpose, 
 * because I'd like CVS to do diff and be able to look at it on the screen
 * when tests fail some day (with having to check out the version when they
 * still worked, or install a different JVM or whatever). 
 * For this reason, I have to find ways for compate serialisation/deserialisation 
 * of all such data. Simply using a comma-separated format seems best 
 * for the purpose of storing tests - I do feel like XML being too verbose, 
 * but sequences are not hierarchical hence no need to worry about schema 
 * in need of subclassing and such. For this reason, graphs are stored as GraphML
 * and the rest in some XML interspersed with comma-separated sequences.
 * The intension for the graphs to be stored in the graph-difference format.
 * The best thing about XMLEncoder is that it is not easy to integrate it into
 * convential XML processing by getting it to write to a node so that I could
 * then include this node in a document - the output of XMLEncoder will have to
 * be turned into a string and parsed by DOM parser. Loading will also involve
 * strings - seems like a crazy thing to do. 
 */

package statechum.analysis.learning.observers;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.computeStateScores.PairScore;
import statechum.analysis.learning.Transform322;
import statechum.analysis.learning.computeStateScores;

public abstract class ProgressDecorator extends LearnerDecorator
{
	public ProgressDecorator(Learner learner) {
		super(learner);setTopLevelListener(this);
	}

	private final static Pattern patternBadChars;
	
	/** A mini-parser is used in conjunction with XML to reduce the size of test trace
	 * files - using single chars helps reduce test trace files greatly, making sure
	 * both my slow uplink and SF are happy.
	 */
	public static final char seqStart='{',seqEnd='}',seqSep=',',seqNewLine='\n';
	
	protected Document doc = null;

	static
	{
		patternBadChars = Pattern.compile("["+"\\"+seqStart+"\\"+seqSep+"\\"+seqEnd+seqNewLine+"]");
	}
	
	public static enum ELEM_KINDS { ELEM_ANSWER, ELEM_QUESTIONS, ATTR_QUESTIONS, ELEM_PAIRS, ELEM_STATECHUM_TESTTRACE, 
		ATTR_QUESTION, ATTR_TESTSET, ATTR_FAILEDPOS, ATTR_LTL, ELEM_PAIR, ELEM_SEQ, ATTR_SEQ, ATTR_Q, ATTR_R, ATTR_SCORE, ATTR_OTHERSCORE, ELEM_RESTART, ATTR_KIND, 
		ELEM_EVALUATIONDATA,ATTR_GRAPHKIND, ELEM_INIT, ELEM_MERGEANDDETERMINIZE, ATTR_LEARNINGOUTCOME, 
		ATTR_POSITIVE_SIZE, ATTR_POSITIVE_SEQUENCES, ATTR_NEGATIVE_SIZE, ATTR_NEGATIVE_SEQUENCES,
		ELEM_LTL,ELEM_AUGMENTPTA, ATTR_ACCEPT, ATTR_COLOUR, ELEM_PROGRESSINDICATOR, ATTR_GRAPHNUMBER
	}
	
	/** Writes the supplied element into XML.
	 * 
	 * @param element to write
	 * @return the constructed XML element.
	 */
	public Element writePair(PairScore element)
	{
		Element pairElement = doc.createElement(ELEM_KINDS.ELEM_PAIR.name());
		pairElement.setAttribute(ELEM_KINDS.ATTR_Q.name(), element.getQ().getUserDatum(JUConstants.LABEL).toString());
		pairElement.setAttribute(ELEM_KINDS.ATTR_R.name(), element.getR().getUserDatum(JUConstants.LABEL).toString());
		pairElement.setAttribute(ELEM_KINDS.ATTR_SCORE.name(), Integer.toString(element.getScore()));
		pairElement.setAttribute(ELEM_KINDS.ATTR_OTHERSCORE.name(), Integer.toString(element.getAnotherScore()));
		return pairElement;
	}
	
	/** Loads a pair from the supplied XML element.
	 * 
	 * @param graph the graph which elements to load 
	 * @param elem element to load from
	 * @return loaded state pair.
	 */
	public PairScore readPair(DirectedSparseGraph graph, Element elem)
	{
		if (!elem.getNodeName().equals(ELEM_KINDS.ELEM_PAIR.name()))
			throw new IllegalArgumentException("expected to load a pair but got "+elem.getNodeName());
		if (!elem.hasAttribute(ELEM_KINDS.ATTR_Q.name()) || !elem.hasAttribute(ELEM_KINDS.ATTR_R.name()) ||
				!elem.hasAttribute(ELEM_KINDS.ATTR_SCORE.name()) || !elem.hasAttribute(ELEM_KINDS.ATTR_OTHERSCORE.name()))
				throw new IllegalArgumentException("missing attribute in a pair");
		String q = elem.getAttribute(ELEM_KINDS.ATTR_Q.name()), r = elem.getAttribute(ELEM_KINDS.ATTR_R.name()),
			score=elem.getAttribute(ELEM_KINDS.ATTR_SCORE.name()), otherscore = elem.getAttribute(ELEM_KINDS.ATTR_OTHERSCORE.name());
		int scoreInt = -1, otherScoreInt = -1;
		try { scoreInt = Integer.valueOf(score); } catch(NumberFormatException ex) { statechum.Helper.throwUnchecked("failed to read a score in a pair", ex); }
		try { otherScoreInt = Integer.valueOf(otherscore); } catch(NumberFormatException ex) { statechum.Helper.throwUnchecked("failed to read a anotherscore in a pair", ex); }
		PairScore result = null;
		synchronized(computeStateScores.syncObj)
		{
			result = new PairScore(
				new DeterministicDirectedSparseGraph.DeterministicVertex(q),
				new DeterministicDirectedSparseGraph.DeterministicVertex(r),
				//RPNIBlueFringeLearner.findVertex(JUConstants.LABEL,q,graph),
				//RPNIBlueFringeLearner.findVertex(JUConstants.LABEL,r,graph),
				scoreInt,otherScoreInt);
		}
		return result;
	}
	
	/** Checks that the supplied element contains single children with the provided names 
	 * and throws {@link IllegalArgumentException} otherwise. 
	 * 
	 * @param elem element to check
	 * @param elemNames names to check for. Ignored if <em>null</em>.
	 */
	public static void checkSingles(Element elem,final Set<String> elemNames)
	{
		NodeList children = elem.getChildNodes();
		Set<String> namesEncountered = new HashSet<String>();
		for(int i=0;i<children.getLength();++i)
			if (children.item(i).getNodeType() == Node.ELEMENT_NODE)
			{
				if (namesEncountered.contains(children.item(i).getNodeName()))
					throw new IllegalArgumentException("duplicate element "+children.item(i).getNodeName());

				namesEncountered.add(children.item(i).getNodeName());
			}
		
		if (elemNames != null)
		{
			namesEncountered.removeAll(elemNames);if (!namesEncountered.isEmpty()) throw new IllegalArgumentException("found unexpected elements "+namesEncountered);
		}
	}

	public static class LearnerEvaluationConfiguration 
	{
		public Graph graph = null;
		public Collection<List<String>> testSet = null;
		public Configuration config = Configuration.getDefaultConfiguration().copy();// making a clone is important because the configuration may later be modified and we do not wish to mess up the default one.
		public Collection<String> ltlSequences = null;
		
		/** The number of graphs to be included in this log file. This one does not participate in equality of hashcode computations.*/
		public transient int graphNumber = -1; 

		public LearnerEvaluationConfiguration() {
			// rely on defaults above.
		}
		
		public LearnerEvaluationConfiguration(DirectedSparseGraph gr, Collection<List<String>> tests, Configuration cnf, Collection<String> ltl)
		{
			graph = gr;testSet = tests;config = cnf;ltlSequences = ltl;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((config == null) ? 0 : config.hashCode());
			result = prime * result + ((graph == null) ? 0 : graph.hashCode());
			result = prime * result
					+ ((ltlSequences == null) ? 0 : ltlSequences.hashCode());
			result = prime * result
					+ ((testSet == null) ? 0 : testSet.hashCode());
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
			if (!(obj instanceof LearnerEvaluationConfiguration))
				return false;
			final LearnerEvaluationConfiguration other = (LearnerEvaluationConfiguration) obj;
			assert config != null && other.config != null;
			if (!config.equals(other.config))
				return false;
			
			assert graph != null && other.graph != null;
			if (graph == null) {
				if (other.graph != null)
					return false;
			} else if (!graph.equals(other.graph))
				return false;
			if (ltlSequences == null) {
				if (other.ltlSequences != null)
					return false;
			} else if (!ltlSequences.equals(other.ltlSequences))
				return false;
			if (testSet == null) {
				if (other.testSet != null)
					return false;
			} else if (!testSet.equals(other.testSet))
				return false;
			return true;
		}
		
	}

	/** Data need to construct an experiment and evaluate the results. This is not 
	 * a part of <em>AbstractExperiment</em> because this is only for testing and
	 * hence one would only want to record
	 * data from <b>some</b> experiments, not all of them.
	 * <p>
	 * If possible, this also loads the configuration and uses it for all methods requiring a configuration.
	 * Unexpected elements are ignored.
	 */
	public static LearnerEvaluationConfiguration readLearnerEvaluationConfiguration(Element evaluationDataElement)
	{
		if (!evaluationDataElement.getNodeName().equals(ELEM_KINDS.ELEM_EVALUATIONDATA.name()))
			throw new IllegalArgumentException("expecting to load learner evaluation data but found "+evaluationDataElement.getNodeName());
		NodeList nodesGraph = evaluationDataElement.getElementsByTagName(Transform322.graphmlNodeName),
		nodesSequences = evaluationDataElement.getElementsByTagName(ELEM_KINDS.ELEM_SEQ.name()),
		nodesLtl = evaluationDataElement.getElementsByTagName(ELEM_KINDS.ELEM_LTL.name()),
		nodesConfigurations = evaluationDataElement.getElementsByTagName(Configuration.configXMLTag),
		graphNumberNodes = evaluationDataElement.getElementsByTagName(ELEM_KINDS.ELEM_PROGRESSINDICATOR.name());
		if (nodesGraph.getLength() < 1) throw new IllegalArgumentException("missing graph");
		if (nodesGraph.getLength() > 1) throw new IllegalArgumentException("duplicate graph");
		if (nodesSequences.getLength() < 1) throw new IllegalArgumentException("missing test set");
		if (nodesSequences.getLength() > 1) throw new IllegalArgumentException("duplicate test set");
		if (nodesLtl.getLength() > 1) throw new IllegalArgumentException("duplicate ltl sets");
		if (nodesConfigurations.getLength() > 1) throw new IllegalArgumentException("duplicate configuration");
		int graphNumber =-1;
		if (graphNumberNodes.getLength() > 1)
			try
			{
					graphNumber = Integer.parseInt(((Element)graphNumberNodes.item(0)).getAttribute(ELEM_KINDS.ATTR_GRAPHNUMBER.name()));
			}
			catch(Exception e)
			{// ignore - graphNumber is unchanged.
			}

		LearnerEvaluationConfiguration result = new LearnerEvaluationConfiguration();
		if (nodesConfigurations.getLength() > 0)
			result.config.readXML(nodesConfigurations.item(0));
		result.graph = Transform322.loadGraph((Element)nodesGraph.item(0));
		result.testSet = readSequenceList((Element)nodesSequences.item(0),ELEM_KINDS.ATTR_TESTSET.name());
		if (nodesLtl.getLength() > 0)
			result.ltlSequences = readInputSequence(new StringReader( nodesLtl.item(0).getTextContent() ),-1);
		result.graphNumber=graphNumber;
		return result;
	}

	/** Writes the supplied learner evaluation configuration.
	 * 
	 * @param cnf configuration to write
	 * @return the recorded XML element
	 */
	public Element writeLearnerEvaluationConfiguration(LearnerEvaluationConfiguration cnf)
	{
		Element evaluationData = doc.createElement(ELEM_KINDS.ELEM_EVALUATIONDATA.name());
		evaluationData.appendChild(new FSMStructure(cnf.graph,null).transform322.createGraphMLNode(doc));
		Element sequenceListElement = writeSequenceList(ELEM_KINDS.ATTR_TESTSET.name(), cnf.testSet);
		evaluationData.appendChild(Transform322.endl(doc));
		evaluationData.appendChild(sequenceListElement);evaluationData.appendChild(Transform322.endl(doc));
		evaluationData.appendChild(cnf.config.writeXML(doc));evaluationData.appendChild(Transform322.endl(doc));
		if (cnf.ltlSequences != null)
		{
			Element ltl = doc.createElement(ELEM_KINDS.ELEM_LTL.name());
			StringWriter ltlsequences = new StringWriter();writeInputSequence(ltlsequences, cnf.ltlSequences);
			ltl.setTextContent(ltlsequences.toString());
			evaluationData.appendChild(ltl);evaluationData.appendChild(Transform322.endl(doc));
		}
		if (cnf.graphNumber >= 0)
		{
			Element progressIndicatorElement = doc.createElement(ELEM_KINDS.ELEM_PROGRESSINDICATOR.name());
			progressIndicatorElement.setAttribute(ELEM_KINDS.ATTR_GRAPHNUMBER.name(), Integer.toString(cnf.graphNumber));
			evaluationData.appendChild(progressIndicatorElement);
		}
		return evaluationData;
	}
	
	/** Given a collection of sequences, it writes them out in a form of XML element.
	 * 
	 * @param name the tag of the new element
	 * @param data what to write
	 * @return the written element.
	 */ 
	protected Element writeSequenceList(final String name, Collection<List<String>> data)
	{
		Element sequenceListElement = doc.createElement(ELEM_KINDS.ELEM_SEQ.name());
		sequenceListElement.setAttribute(ELEM_KINDS.ATTR_SEQ.name(), name.toString());
		StringWriter strWriter = new StringWriter();strWriter.append('\n');
		for(List<String> seq:data)
		{
			writeInputSequence(strWriter, seq);strWriter.append('\n');
		}
		org.w3c.dom.Text dataInNode = doc.createTextNode(strWriter.toString());// if the string is empty at this point, the text node will not get added, so I have to check that there is any at the loading stage.
		sequenceListElement.appendChild(dataInNode);
		return sequenceListElement;
	}
	
	/** Given an element, loads the data contained in it back into a collection.
	 * (this is an inverse of <em>addSequenceList</em>.
	 * 
	 * @param elem the element to load from
	 * @param expectedName the name which should have been given to this collection
	 * @return the collection of sequences of strings loaded from that element.
	 */
	protected static List<List<String>> readSequenceList(Element elem, String expectedName)
	{
		List<List<String>> result = new LinkedList<List<String>>();
		if (!elem.getNodeName().equals(ELEM_KINDS.ELEM_SEQ.name()))
			throw new IllegalArgumentException("expecting to load a list of sequences "+elem.getNodeName());
		if (!elem.getAttribute(ELEM_KINDS.ATTR_SEQ.name()).equals(expectedName))
			throw new IllegalArgumentException("expecting to load a list with name "+expectedName+
					" but found a list named "+elem.getAttribute(ELEM_KINDS.ATTR_SEQ.name()));
		if (elem.getFirstChild() != null)
		{
			Reader reader = new StringReader(elem.getFirstChild().getTextContent());
			try
			{
				int ch = reader.read();while(ch == seqNewLine) ch=reader.read();
				if (ch != -1 && ch != seqStart) throw new IllegalArgumentException("invalid char "+ch+" instead of a sequence");
				
				while(ch == seqStart)
				{
					result.add(readInputSequence(reader,ch));
					ch = reader.read();while(ch == seqNewLine) ch=reader.read();
				}
				if (ch != -1 && ch != seqStart) throw new IllegalArgumentException("invalid char "+ch+" instead of a sequence");
			}
			catch(IOException e)
			{
				statechum.Helper.throwUnchecked("failed to write to writer ",e);
			}
		}
		return result;
	}
	
	/** Dumps a sequence of inputs to the writer.
	 * 
	 * @param wr where to write sequences
	 * @param str sequence of inputs to write out. The collection can be empty but no input can be of length zero.
	 * @throws IOException
	 */	
	public static void writeInputSequence(Writer wr,Collection<String> str)
	{
		try
		{
			wr.append(seqStart);
			boolean firstElem = true;
			for(String st:str)
			{
				if (!firstElem)	wr.append(seqSep);else firstElem = false;
				if (st.length() == 0)
					throw new IllegalArgumentException("empty input in sequence");
				if (patternBadChars.matcher(st).find())
					throw new IllegalArgumentException("invalid characters in sequence "+str+" : it matches "+patternBadChars.toString());
				wr.append(st);
			}
			wr.append(seqEnd);
		}
		catch(IOException e)
		{
			statechum.Helper.throwUnchecked("failed to write to writer ",e);
		}
	}
	
	/** Loads a sequence of inputs from a reader. Since I do not wish to use <em>mark</em>
	 * or some form of put-back but would like a way to see ahead, I decided simply to pass
	 * the first character as a parameter, -1 if there is none.
	 * 
	 * @param rd stream to read
	 * @param firstChar the first character, -1 if the first char is to be read from a stream
	 * @return collection of inputs read from a stream
	 */
	public static List<String> readInputSequence(Reader rd, int firstChar)
	{
		List<String> result = new LinkedList<String>();
		try
		{
			int ch = firstChar == -1?rd.read():firstChar;while(ch == seqNewLine) ch=rd.read();if (ch != seqStart) throw new IllegalArgumentException("invalid char "+ch+" instead of a sequence");
			boolean after_open_bracket = true;
			do
			{
				StringBuffer input = new StringBuffer();
				ch = rd.read();
				while(ch != -1 && ch != seqEnd && ch != seqSep)
				{
					input.append((char)ch);
					ch = rd.read();
				}
				if (ch == -1) throw new IllegalArgumentException("premature end of stream");
				if (!after_open_bracket && input.length() == 0)
					throw new IllegalArgumentException("empty input in a sequence of inputs");
				after_open_bracket = false;
				if (input.length() > 0)
					result.add(input.toString());// if length is zero and we did not throw an exception, this means that the stream is empty.
			}		
			while(ch != seqEnd);
		}
		catch(IOException e)
		{
			statechum.Helper.throwUnchecked("failed to read from reader ",e);
		}
		return result;
	}
	
	/** This method stores all the details of initialisation of a learner. 
	 * 
	 * @param initialData data to store
	 * @return the constructed XML element
	 */
	protected Element writeInitialData(InitialData initialData)
	{
		Element elemInit = doc.createElement(ELEM_KINDS.ELEM_INIT.name());
		Element positive = writeSequenceList(ELEM_KINDS.ATTR_POSITIVE_SEQUENCES.name(), initialData.plus);positive.setAttribute(ELEM_KINDS.ATTR_POSITIVE_SIZE.name(), Integer.toString(initialData.plusSize));
		Element negative = writeSequenceList(ELEM_KINDS.ATTR_NEGATIVE_SEQUENCES.name(), initialData.minus);negative.setAttribute(ELEM_KINDS.ATTR_NEGATIVE_SIZE.name(), Integer.toString(initialData.minusSize));
		elemInit.appendChild(new FSMStructure(initialData.graph,null).transform322.createGraphMLNode(doc));elemInit.appendChild(Transform322.endl(doc));
		elemInit.appendChild(positive);elemInit.appendChild(Transform322.endl(doc));
		elemInit.appendChild(negative);elemInit.appendChild(Transform322.endl(doc));
		return elemInit;
	}
	
	/** Configuration used. The default is null to catch cases when I'm using the simulator without loading configuration from XML. */
	protected Configuration config = null;
	
	/** Loads the initial data from the supplied XML element.
	 * 
	 * @param elem where to load from
	 * @return initial data
	 */
	public InitialData readInitialData(Element elem)
	{
		if (!elem.getNodeName().equals(ELEM_KINDS.ELEM_INIT.name()))
			throw new IllegalArgumentException("expecting to load learner initial data "+elem.getNodeName());
		NodeList children = elem.getChildNodes();
		InitialData result = new InitialData();
		for(int i=0;i<children.getLength();++i)
			if (children.item(i).getNodeType() == Node.ELEMENT_NODE)
			{
				Element e = (Element)children.item(i);
				if (e.getNodeName().equals(Transform322.graphmlNodeName))
				{
					if (result.graph != null)
						throw new IllegalArgumentException("duplicate graph element");
					result.graph = Transform322.loadGraph(e);
				}
				else
					if (e.getNodeName().equals(ELEM_KINDS.ELEM_SEQ.name()))
					{
						String sequenceName = e.getAttribute(ELEM_KINDS.ATTR_SEQ.name());
						if (sequenceName.equals(ELEM_KINDS.ATTR_POSITIVE_SEQUENCES.name()))
						{
							if (result.plus != null)
								throw new IllegalArgumentException("duplicate positive element");
							result.plus = readSequenceList(e, ELEM_KINDS.ATTR_POSITIVE_SEQUENCES.name());
							if (!e.hasAttribute(ELEM_KINDS.ATTR_POSITIVE_SIZE.name())) throw new IllegalArgumentException("missing positive size");
							String size = e.getAttribute(ELEM_KINDS.ATTR_POSITIVE_SIZE.name());
							try{ result.plusSize = Integer.valueOf(size); } catch(NumberFormatException ex) { statechum.Helper.throwUnchecked("positive value is not an integer "+size, ex);}
						}
						else
							if (sequenceName.equals(ELEM_KINDS.ATTR_NEGATIVE_SEQUENCES.name()))
							{
								if (result.minus != null)
									throw new IllegalArgumentException("duplicate negative element");
								result.minus = readSequenceList(e, ELEM_KINDS.ATTR_NEGATIVE_SEQUENCES.name());
								if (!e.hasAttribute(ELEM_KINDS.ATTR_NEGATIVE_SIZE.name())) throw new IllegalArgumentException("missing negative size");
								String size = e.getAttribute(ELEM_KINDS.ATTR_NEGATIVE_SIZE.name());
								try{ result.minusSize = Integer.valueOf(size); } catch(NumberFormatException ex) { statechum.Helper.throwUnchecked("negative value is not an integer "+size, ex);}
							}
							else throw new IllegalArgumentException("unexpected kind of sequences: "+sequenceName);
					}
					else throw new IllegalArgumentException("unexpected element "+e.getNodeName());
			}
		 
		if (result.graph == null) throw new IllegalArgumentException("missing graph");
		if (result.plus == null) throw new IllegalArgumentException("missing positive sequences");
		if (result.minus == null) throw new IllegalArgumentException("missing negative sequences");
			
		return result;
	}
	
	public static class InitialData
	{
		public Collection<List<String>> plus = null, minus = null;
		public int plusSize=-1, minusSize =-1;
		public DirectedSparseGraph graph=null;
		
		public InitialData() {
			// rely on defaults above.
		}
		
		/** Constructs an instance containing the data from which an initial PTA can be built.
		 * @param plus positive sequences
		 * @param plusSize the number of positive sequences, perhaps greater than plus if plus was prefix-reduced.
		 * @param minus negative sequences
		 * @param minusSize the number of negative sequences, should be equal to the number of sequences in minus - negatives cannot be prefix-reduced.
		 * @param pta the initial PTA
		 */
		public InitialData(Collection<List<String>> argPlus, int argPlusSize, 
				Collection<List<String>> argMinus, int argMinusSize, DirectedSparseGraph pta)
		{
			plus = argPlus;minus = argMinus;plusSize = argPlusSize;minusSize = argMinusSize;graph = pta;
			
			if (plus.size() > plusSize) throw new IllegalArgumentException("inconsistent positive size, "+plus.size()+" elements in collection but expected "+plusSize);
			if (minus.size() != minusSize) throw new IllegalArgumentException("inconsistent negative size, "+minus.size()+" elements in collection but expected "+minusSize);
		}
	}

	protected static class AugmentPTAData
	{
		public RestartLearningEnum kind = RestartLearningEnum.restartNONE;
		
		/** Sequence to add to PTA. Could be empty. */
		public List<String> sequence = null;
		public boolean accept = false;
		public JUConstants colour = null;
		
		public AugmentPTAData() {
			// rely on defaults above.
		}
		/**
		* Constructs a class representing arguments to AugmentPTA
		* @param kind which kind of PTA to modify
		* @param sequence what to add
		* @param accept whether it is an accept or a reject sequence
		* @param colour the colour to give to the new nodes to be constructed.
		*/
		public AugmentPTAData(RestartLearningEnum argKind,List<String> argSequence, boolean argAccept, JUConstants argColour)
		{
			kind = argKind;sequence = argSequence;accept = argAccept;colour = argColour;
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (accept ? 1231 : 1237);
			result = prime * result
					+ ((colour == null) ? 0 : colour.hashCode());
			result = prime * result + ((kind == null) ? 0 : kind.hashCode());
			result = prime * result
					+ ((sequence == null) ? 0 : sequence.hashCode());
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
			if (!(obj instanceof AugmentPTAData))
				return false;
			final AugmentPTAData other = (AugmentPTAData) obj;
			if (accept != other.accept)
				return false;
			if (colour == null) {
				if (other.colour != null)
					return false;
			} else if (!colour.equals(other.colour))
				return false;
			if (!kind.equals(other.kind))
				return false;
			if (!sequence.equals(other.sequence))
				return false;
			return true;
		}
	}
	
	/** Writes a request to AugmentPTA into an XML node and returns the constructed node.
	 *
	 * @param data what to write
	 * @return constructed XML node.
	 */
	protected Element writeAugmentPTA(AugmentPTAData data)
	{
		Element result = doc.createElement(ELEM_KINDS.ELEM_AUGMENTPTA.name());
		result.setAttribute(ELEM_KINDS.ATTR_KIND.name(), data.kind.name());
		result.setAttribute(ELEM_KINDS.ATTR_ACCEPT.name(), Boolean.toString(data.accept));
		StringWriter writer = new StringWriter();
		writeInputSequence(writer, data.sequence);result.setTextContent(writer.toString());
		return result;
	}
	
	/** Reads the arguments to AugmentPTA from XML element.
	 * <p>
	 * At the moment, storage of instances of leaf nodes in trees is not implemented
	 * (and leaf nodes are used in filtering), hence 
	 * I have to rely on storage of the whole set of sequences. 
	 * 
	 * @param element data to load from
	 * @return constructed arguments.
	 */
	protected AugmentPTAData readAugmentPTA(Element element)
	{
		if (!element.getNodeName().equals(ELEM_KINDS.ELEM_AUGMENTPTA.name()))
			throw new IllegalArgumentException("cannot load augmentPTA data from "+element.getNodeName());
		AugmentPTAData result = new AugmentPTAData();
		if (!element.hasAttribute(ELEM_KINDS.ATTR_ACCEPT.name())) throw new IllegalArgumentException("missing accept");
		if (!element.hasAttribute(ELEM_KINDS.ATTR_KIND.name())) throw new IllegalArgumentException("missing kind");

		String accept = element.getAttribute(ELEM_KINDS.ATTR_ACCEPT.name()),
				kind = element.getAttribute(ELEM_KINDS.ATTR_KIND.name()),
				sequence = element.getTextContent();
		if (sequence.length() == 0) throw new IllegalArgumentException("missing sequence");
		StringReader reader = new StringReader(sequence);result.sequence = readInputSequence(reader, -1);
		result.accept = Boolean.valueOf(accept);
		result.kind=Enum.valueOf(RestartLearningEnum.class, kind);
		return result;
	}
}
