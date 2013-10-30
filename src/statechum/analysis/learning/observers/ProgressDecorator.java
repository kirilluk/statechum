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

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.StatechumXML;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.smt.SmtLabelRepresentation;
import statechum.StatechumXML.SequenceIO;

public abstract class ProgressDecorator extends LearnerDecorator
{
	public ProgressDecorator(Learner learner) {
		super(learner);setTopLevelListener(this);
	}
	
	public ProgressDecorator(ConvertALabel conv) {
		super(conv);
	}
	
	protected Document doc = null;

	protected SequenceIO<statechum.Label> labelio = null;//new LEGACY_StringLabelSequenceWriter(doc, result.config);
	protected SequenceIO<String> stringio = null;//new LEGACY_StringSequenceWriter(doc);

	/** Writes the supplied element into XML.
	 * 
	 * @param element to write
	 * @return the constructed XML element.
	 */
	public static Element writePair(PairScore element, Document doc)
	{
		Element pairElement = doc.createElement(StatechumXML.ELEM_PAIR.name());
		pairElement.setAttribute(StatechumXML.ATTR_Q.name(), element.getQ().getStringId());
		pairElement.setAttribute(StatechumXML.ATTR_R.name(), element.getR().getStringId());
		if (element.getScore() != JUConstants.intUNKNOWN) pairElement.setAttribute(StatechumXML.ATTR_SCORE.name(), Long.toString(element.getScore()));
		if (element.getAnotherScore() != JUConstants.intUNKNOWN) pairElement.setAttribute(StatechumXML.ATTR_OTHERSCORE.name(), Long.toString(element.getAnotherScore()));
		return pairElement;
	}
	
	/** Loads a pair from the supplied XML element.
	 * 
	 * @param graph the graph which elements to load 
	 * Ideally, I need to match string IDs loaded to those of the graph but this is not done because
	 * <ul> 
	 * <li>graphseries used to mangle names of vertices, now this should not happen often (and if I use
	 * relabelling when loading graphs, this would never happen).</li>
	 * <li>this method is expected to be general - purpose hence we do not expect a matching graph to be present.</li>
	 * <li>matching was only needed when loading a compatibility table from a graph - graph loader now does 
	 * the matching after reading pairs.</li>
	 * </ul>
	 * 
	 * @param elem element to load from
	 * @return loaded state pair.
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> 
		PairScore readPair(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph, Element elem)
	{
		if (!elem.getNodeName().equals(StatechumXML.ELEM_PAIR.name()))
			throw new IllegalArgumentException("expected to load a pair but got "+elem.getNodeName());
		if (!elem.hasAttribute(StatechumXML.ATTR_Q.name()) || !elem.hasAttribute(StatechumXML.ATTR_R.name()) )
				throw new IllegalArgumentException("missing attribute in a pair");
		String q = elem.getAttribute(StatechumXML.ATTR_Q.name()), r = elem.getAttribute(StatechumXML.ATTR_R.name()),
			score=elem.getAttribute(StatechumXML.ATTR_SCORE.name()), otherscore = elem.getAttribute(StatechumXML.ATTR_OTHERSCORE.name());
		int scoreInt = JUConstants.intUNKNOWN, otherScoreInt = JUConstants.intUNKNOWN;
		if (score != null && score.length() > 0)
			try { scoreInt = Integer.valueOf(score); } catch(NumberFormatException ex) { statechum.Helper.throwUnchecked("failed to read a score in a pair", ex); }
		if (otherscore != null && otherscore.length() > 0)
			try { otherScoreInt = Integer.valueOf(otherscore); } catch(NumberFormatException ex) { statechum.Helper.throwUnchecked("failed to read a anotherscore in a pair", ex); }
		return new PairScore(AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(q), graph.config),
				AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(r), graph.config),
				scoreInt,otherScoreInt);		
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
		public LearnerGraph graph = null;
		public Collection<List<Label>> testSet = null;
		public Configuration config = null; 
		public Collection<String> ifthenSequences = null;
		public SmtLabelRepresentation labelDetails = null;
		
		/** Used to intern labels in an experiment with large graphs. Although not set a final, many things will break if different graphs used together are relying
		 * on different intern engines because integer IDs of labels could be passed from a graph to a different one where they could mean a completely different label. */
		private ConvertALabel labelConverter = null; 
		
		public ConvertALabel getLabelConverter()
		{
			return labelConverter;
		}
		
		public void setLabelConverter(ConvertALabel conv)
		{
			labelConverter = conv;
		}
		
		/** The number of graphs to be included in this log file. This one does not participate in equality of hashcode computations.*/
		public transient int graphNumber = -1; 

		public LearnerEvaluationConfiguration(Configuration defaultCnf) {
			// Mostly rely on defaults above.
			if (defaultCnf == null)
				config = Configuration.getDefaultConfiguration().copy();// making a clone is important because the configuration may later be modified and we do not wish to mess up the default one.
			else
				config = defaultCnf.copy();
		}
		
		public LearnerEvaluationConfiguration(LearnerGraph gr, Collection<List<Label>> tests, Configuration cnf, 
				Collection<String> ltl, SmtLabelRepresentation lblDetails)
		{
			graph = gr;testSet = tests;config = cnf;ifthenSequences = ltl;labelDetails=lblDetails;
			
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
					+ ((ifthenSequences == null) ? 0 : ifthenSequences.hashCode());
			result = prime * result
				+ ((testSet == null) ? 0 : testSet.hashCode());
			result = prime * result
				+ ((labelDetails == null) ? 0 : labelDetails.hashCode());
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
			if (ifthenSequences == null) {
				if (other.ifthenSequences != null)
					return false;
			} else if (!ifthenSequences.equals(other.ifthenSequences))
				return false;

			if (testSet == null) {
				if (other.testSet != null)
					return false;
			} else if (!testSet.equals(other.testSet))
				return false;

			if (labelDetails == null) {
				if (other.labelDetails != null)
					return false;
			} else if (!labelDetails.equals(other.labelDetails))
				return false;
			return true;
		}
		
	}

	/** <p>
	 * Data need to construct an experiment and evaluate the results. This is not 
	 * a part of <em>AbstractExperiment</em> because this is only for testing and
	 * hence one would only want to record
	 * data from <b>some</b> experiments, not all of them.
	 * </p><p>
	 * If possible, this also loads the configuration and uses it for all methods requiring a configuration.
	 * Unexpected elements are ignored.
	 * </p><p>
	 * {@link LearnerEvaluationConfiguration#labelConverter} is not assigned since it is a function that is supposed to be set globally for an entire experiment.
	 * </p>
	 */
	public LearnerEvaluationConfiguration readLearnerEvaluationConfiguration(Element evaluationDataElement,Configuration defaultConfig)
	{
		if (!evaluationDataElement.getNodeName().equals(StatechumXML.ELEM_EVALUATIONDATA.name()))
			throw new IllegalArgumentException("expecting to load learner evaluation data but found "+evaluationDataElement.getNodeName());
		NodeList nodesGraph = StatechumXML.getChildWithTag(evaluationDataElement,StatechumXML.graphmlNodeNameNS.toString()),
		nodesSequences = StatechumXML.getChildWithTag(evaluationDataElement,StatechumXML.ELEM_SEQ.name()),
		nodesLtl = StatechumXML.getChildWithTag(evaluationDataElement,StatechumXML.ELEM_CONSTRAINTS.name()),
		nodesConfigurations = StatechumXML.getChildWithTag(evaluationDataElement,Configuration.configXMLTag),
		graphNumberNodes = StatechumXML.getChildWithTag(evaluationDataElement,StatechumXML.ELEM_PROGRESSINDICATOR.name()),
		nodesLabelDetails = StatechumXML.getChildWithTag(evaluationDataElement,StatechumXML.ELEM_LABELDETAILS.name());
		if (nodesGraph.getLength() < 1) throw new IllegalArgumentException("missing graph");
		if (nodesGraph.getLength() > 1) throw new IllegalArgumentException("duplicate graph");
		if (nodesSequences.getLength() < 1) throw new IllegalArgumentException("missing test set");
		if (nodesSequences.getLength() > 1) throw new IllegalArgumentException("duplicate test set");
		if (nodesLtl.getLength() > 1) throw new IllegalArgumentException("duplicate ltl sets");
		if (nodesConfigurations.getLength() > 1) throw new IllegalArgumentException("duplicate configuration");
		if (nodesLabelDetails.getLength() > 1) throw new IllegalArgumentException("duplicate label details");
		int graphNumber =-1;
		if (graphNumberNodes.getLength() > 1)
			try
			{
				graphNumber = Integer.parseInt(((Element)graphNumberNodes.item(0)).getAttribute(StatechumXML.ATTR_GRAPHNUMBER.name()));
			}
			catch(Exception e)
			{// ignore - graphNumber is unchanged.
			}

		LearnerEvaluationConfiguration result = new LearnerEvaluationConfiguration(defaultConfig);
		if (nodesConfigurations.getLength() > 0)
			result.config.readXML(nodesConfigurations.item(0));

		initIO(evaluationDataElement.getOwnerDocument(),result.config);
		
		result.graph = new LearnerGraph(result.config);AbstractPersistence.loadGraph((Element)nodesGraph.item(0), result.graph, decoratedLearner.getLabelConverter());
		
		result.testSet = labelio.readSequenceList((Element)nodesSequences.item(0),StatechumXML.ATTR_TESTSET.name());
		if (nodesLtl.getLength() > 0)
			result.ifthenSequences = stringio.readInputSequence(nodesLtl.item(0).getTextContent());
		if (nodesLabelDetails.getLength() > 0)
		{
			result.labelDetails = new SmtLabelRepresentation(result.config,getLabelConverter());
			result.labelDetails.loadXML( (Element)nodesLabelDetails.item(0),stringio );
		}
		result.graphNumber=graphNumber;
		return result;
	}

	/** Performs initialization if necessary of the io routines aimed at loading/saving sequences of labels. */
	protected void initIO(Document document, Configuration configuration)
	{
		if (configuration.getLegacyXML())
		{
			if (labelio == null)
				labelio = new StatechumXML.LEGACY_StringLabelSequenceWriter(document,configuration,getLabelConverter());
			if (stringio == null)
				stringio = new StatechumXML.LEGACY_StringSequenceWriter(document);
		}
		else
		{// current
			if (labelio == null)
				labelio = new StatechumXML.LabelSequenceWriter(document,configuration, getLabelConverter());
			if (stringio == null)
				stringio = new StatechumXML.StringSequenceWriter(document);
		}
	}

	/** Writes the supplied learner evaluation configuration.
	 * 
	 * @param cnf configuration to write
	 * @return the recorded XML element
	 */
	public Element writeLearnerEvaluationConfiguration(LearnerEvaluationConfiguration cnf)
	{
		Element evaluationData = doc.createElement(StatechumXML.ELEM_EVALUATIONDATA.name());
		evaluationData.appendChild(cnf.graph.storage.createGraphMLNode(doc));
		
		Element sequenceListElement = labelio.writeSequenceList(StatechumXML.ATTR_TESTSET.name(), cnf.testSet);
		evaluationData.appendChild(AbstractPersistence.endl(doc));
		evaluationData.appendChild(sequenceListElement);evaluationData.appendChild(AbstractPersistence.endl(doc));
		evaluationData.appendChild(cnf.config.writeXML(doc));evaluationData.appendChild(AbstractPersistence.endl(doc));
		if (cnf.ifthenSequences != null)
		{
			Element ltl = doc.createElement(StatechumXML.ELEM_CONSTRAINTS.name());
			StringBuffer ltlsequences = new StringBuffer();stringio.writeInputSequence(ltlsequences, cnf.ifthenSequences);
			ltl.setTextContent(ltlsequences.toString());
			evaluationData.appendChild(ltl);evaluationData.appendChild(AbstractPersistence.endl(doc));
		}
		if (cnf.labelDetails != null)
		{
			evaluationData.appendChild(cnf.labelDetails.storeToXML(doc,stringio));evaluationData.appendChild(AbstractPersistence.endl(doc));
		}
		if (cnf.graphNumber >= 0)
		{
			Element progressIndicatorElement = doc.createElement(StatechumXML.ELEM_PROGRESSINDICATOR.name());
			progressIndicatorElement.setAttribute(StatechumXML.ATTR_GRAPHNUMBER.name(), Integer.toString(cnf.graphNumber));
			evaluationData.appendChild(progressIndicatorElement);
		}
		return evaluationData;
	}
	
	/** This method stores all the details of initialisation of a learner. 
	 * 
	 * @param initialData data to store
	 * @return the constructed XML element
	 */
	protected Element writeInitialData(InitialData initialData)
	{
		Element elemInit = doc.createElement(StatechumXML.ELEM_INIT.name());
		Element positive = labelio.writeSequenceList(StatechumXML.ATTR_POSITIVE_SEQUENCES.name(), initialData.plus);positive.setAttribute(StatechumXML.ATTR_POSITIVE_SIZE.name(), Integer.toString(initialData.plusSize));
		Element negative = labelio.writeSequenceList(StatechumXML.ATTR_NEGATIVE_SEQUENCES.name(), initialData.minus);negative.setAttribute(StatechumXML.ATTR_NEGATIVE_SIZE.name(), Integer.toString(initialData.minusSize));
		elemInit.appendChild(initialData.graph.storage.createGraphMLNode(doc));elemInit.appendChild(AbstractPersistence.endl(doc));
		elemInit.appendChild(positive);elemInit.appendChild(AbstractPersistence.endl(doc));
		elemInit.appendChild(negative);elemInit.appendChild(AbstractPersistence.endl(doc));
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
		if (!elem.getNodeName().equals(StatechumXML.ELEM_INIT.name()))
			throw new IllegalArgumentException("expecting to load learner initial data "+elem.getNodeName());
		NodeList children = elem.getChildNodes();
		InitialData result = new InitialData();
		for(int i=0;i<children.getLength();++i)
			if (children.item(i).getNodeType() == Node.ELEMENT_NODE)
			{
				Element e = (Element)children.item(i);
				if (e.getNodeName().equals(StatechumXML.graphmlNodeNameNS.toString()))
				{
					if (result.graph != null)
						throw new IllegalArgumentException("duplicate graph element");
					result.graph = new LearnerGraph(config);AbstractPersistence.loadGraph(e, result.graph, decoratedLearner.getLabelConverter());
				}
				else
					if (e.getNodeName().equals(StatechumXML.ELEM_SEQ.name()))
					{
						String sequenceName = e.getAttribute(StatechumXML.ATTR_SEQ.name());
						if (sequenceName.equals(StatechumXML.ATTR_POSITIVE_SEQUENCES.name()))
						{
							if (result.plus != null)
								throw new IllegalArgumentException("duplicate positive element");
							result.plus = labelio.readSequenceList(e, StatechumXML.ATTR_POSITIVE_SEQUENCES.name());
							if (!e.hasAttribute(StatechumXML.ATTR_POSITIVE_SIZE.name())) throw new IllegalArgumentException("missing positive size");
							String size = e.getAttribute(StatechumXML.ATTR_POSITIVE_SIZE.name());
							try{ result.plusSize = Integer.valueOf(size); } catch(NumberFormatException ex) { statechum.Helper.throwUnchecked("positive value is not an integer "+size, ex);}
						}
						else
							if (sequenceName.equals(StatechumXML.ATTR_NEGATIVE_SEQUENCES.name()))
							{
								if (result.minus != null)
									throw new IllegalArgumentException("duplicate negative element");
								result.minus = labelio.readSequenceList(e, StatechumXML.ATTR_NEGATIVE_SEQUENCES.name());
								if (!e.hasAttribute(StatechumXML.ATTR_NEGATIVE_SIZE.name())) throw new IllegalArgumentException("missing negative size");
								String size = e.getAttribute(StatechumXML.ATTR_NEGATIVE_SIZE.name());
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
		/** Sets of positive and negative sequences to initialise the PTA. */
		public Collection<List<Label>> plus = null, minus = null;
		public int plusSize=-1, minusSize =-1;
		
		/** The initial PTA as a PTA, useful where storing sequences of labels will take a lot of space (sparse PTA). */
		public LearnerGraph graph=null;
		public SmtLabelRepresentation labelDetails = null;

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
		public InitialData(Collection<List<Label>> argPlus, int argPlusSize, 
				Collection<List<Label>> argMinus, int argMinusSize, LearnerGraph pta)
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
		public List<Label> sequence = null;
		public boolean accept = false;
		public JUConstants colour = null;
		
		public AugmentPTAData() {
			// rely on defaults above.
		}
		/**
		* Constructs a class representing arguments to AugmentPTA
		* @param kind which kind of PTA to modify
		* @param argSequence what to add
		* @param accept whether it is an accept or a reject sequence
		* @param colour the colour to give to the new nodes to be constructed.
		*/
		public AugmentPTAData(RestartLearningEnum argKind,List<Label> argSequence, boolean argAccept, JUConstants argColour)
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
		Element result = doc.createElement(StatechumXML.ELEM_AUGMENTPTA.name());
		result.setAttribute(StatechumXML.ATTR_KIND.name(), data.kind.name());
		if (data.colour != null) result.setAttribute(StatechumXML.ATTR_COLOUR.name(), data.colour.name());
		result.setAttribute(StatechumXML.ATTR_ACCEPT.name(), Boolean.toString(data.accept));
		StringBuffer writer = new StringBuffer();
		labelio.writeInputSequence(writer, data.sequence);result.setTextContent(writer.toString());
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
		if (!element.getNodeName().equals(StatechumXML.ELEM_AUGMENTPTA.name()))
			throw new IllegalArgumentException("cannot load augmentPTA data from "+element.getNodeName());
		AugmentPTAData result = new AugmentPTAData();
		if (!element.hasAttribute(StatechumXML.ATTR_ACCEPT.name())) throw new IllegalArgumentException("missing accept");
		if (!element.hasAttribute(StatechumXML.ATTR_KIND.name())) throw new IllegalArgumentException("missing kind");

		String accept = element.getAttribute(StatechumXML.ATTR_ACCEPT.name()),
				colour = element.getAttribute(StatechumXML.ATTR_COLOUR.name()),
				kind = element.getAttribute(StatechumXML.ATTR_KIND.name()),
				sequence = element.getTextContent();
		if (sequence.length() == 0) throw new IllegalArgumentException("missing sequence");
		result.sequence = labelio.readInputSequence(sequence);
		result.accept = Boolean.valueOf(accept);
		if (colour.length() > 0)
			result.colour=Enum.valueOf(JUConstants.class, colour);
		result.kind=Enum.valueOf(RestartLearningEnum.class, kind);
		return result;
	}
}
