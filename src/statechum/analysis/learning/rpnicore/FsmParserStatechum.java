package statechum.analysis.learning.rpnicore;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.JUConstants;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.Label;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parent class for different parsers that make it possible to load automata.
 */
public class FsmParserStatechum
{
	protected final String text;
	protected Matcher lexer;
	protected String lastMatch = null;

	public static final int LABEL=0;
	public static final int LARROW=1;
	public static final int RARROW=2;
	public static final int LARROWREJ=3;
	public static final int RARROWREJ=4;
	public static final int DASH =5;
	public static final int EQUIV = 6;
	public static final int NEWL =7;

	/** Creates an instance of the parser.
	 *
	 * @param whatToParse text to parse
	 * @param slashForEndl whether to use forward slash to separate expressions. Useful for LTS but cannot be used for Mealy automata.
	 */
	public FsmParserStatechum(String whatToParse, boolean slashForEndl)
	{
		text = "\n"+whatToParse;
		String patternSlashForEndl = "([^\n#\\055<>/=]+)|( *<\\055+ *)|( *\\055+> *)|( *#\\055+ *)|( *\\055+# *)|( *\\055+ *)|( *=+ *)|( *[\n/] *)";
		String patternNoSlashForEndl = "([^\n#\\055<>|=]+)|( *<\\055+ *)|( *\\055+> *)|( *#\\055+ *)|( *\\055+# *)|( *\\055+ *)|( *=+ *)|( *[\n|] *)";
		lexer = Pattern.compile(slashForEndl? patternSlashForEndl:patternNoSlashForEndl).matcher(text);
	}

	protected boolean isFinished()
	{
		return lexer.regionStart() == lexer.regionEnd();
	}

	protected void throwException(String errMsg)
	{
		throw new IllegalArgumentException(errMsg+" starting from "+text.substring(lexer.regionStart()));
	}

	protected int getNextMatch()
	{
		if (!lexer.lookingAt())
			throwException("failed to lex");

		int i=1;
		//noinspection StatementWithEmptyBody
		for(;i<lexer.groupCount()+1 && lexer.group(i) == null;++i)
		{}
		if (i == lexer.groupCount()+1)
			throwException("failed to lex (group number is out of boundary)");

		lastMatch = lexer.group(i).trim();
		lexer.region(lexer.end(i),lexer.regionEnd());
		return i-1;// to bring it to 0..max from 1..max+1
	}

	protected String getMatch()
	{
		return lastMatch;
	}

	public void parse(TransitionReceiver receiver,Configuration config,final ConvertALabel conv)
	{
		String currentState = null;
		do {
			int left = getNextMatch();// this is expected to be a label of the left state;
			// we start with a newline hence the first thing that happens is a fallthrough
			// to a label of the first state.
			if (left == NEWL)
			{
				while(left == NEWL && !isFinished())
					left = getNextMatch();
				if (left == NEWL && isFinished())
					break;// finished parsing
				if (left != FsmParserStatechum.LABEL)
					throwException("state name expected");// there should be a state name after a newline
				currentState = getMatch();
				left=getNextMatch();
			}

			if (left != FsmParserStatechum.LARROW && left != FsmParserStatechum.LARROWREJ && left != FsmParserStatechum.DASH && left != FsmParserStatechum.EQUIV)
				throwException("a left arrow or a dash expected here");

			if (getNextMatch() != FsmParserStatechum.LABEL)
				throwException("label expected");
			String label = getMatch();
			JUConstants.PAIRCOMPATIBILITY pairRelation = null;
			int right = getNextMatch();
			if (left == FsmParserStatechum.LARROW || left == FsmParserStatechum.LARROWREJ)
			{
				if (right != FsmParserStatechum.DASH)
					throwException("a dash was expected here");
			}
			else
			if (left == FsmParserStatechum.DASH)
			{
				if (right != FsmParserStatechum.RARROW && right != FsmParserStatechum.RARROWREJ)
					throwException("a right-arrow was expected here");
			}
			else
			{
				assert left == FsmParserStatechum.EQUIV;
				if (right != FsmParserStatechum.EQUIV)
					throwException("equiv on the left-hand side should be accompanied by on on the right");
				pairRelation = JUConstants.PAIRCOMPATIBILITY.valueOf(label);
			}

			if (getNextMatch() != FsmParserStatechum.LABEL)
				throwException("state name expected");
			String anotherState = getMatch();

			if (left == FsmParserStatechum.LARROW)
				receiver.accept(anotherState, currentState, AbstractLearnerGraph.generateNewLabel(label,config,conv));
			else
			if (left == FsmParserStatechum.LARROWREJ)
				receiver.reject(anotherState, currentState, AbstractLearnerGraph.generateNewLabel(label,config,conv));
			else
			if (left == FsmParserStatechum.DASH)
			{
				if (right == FsmParserStatechum.RARROW)
					receiver.accept(currentState, anotherState, AbstractLearnerGraph.generateNewLabel(label,config,conv));
				else
					receiver.reject(currentState, anotherState, AbstractLearnerGraph.generateNewLabel(label,config,conv));
			}
			else // left == FsmParser.EQUIV
			{
				//noinspection ConstantConditions
				assert left == FsmParserStatechum.EQUIV;
				receiver.pairCompatibility(currentState,pairRelation,anotherState);
			}
			currentState = anotherState;
		} while(!isFinished());

	}

	/** Given a textual representation of an fsm, builds a corresponding deterministic graph
	 *
	 * @param fsm the textual representation of an FSM
	 * @param name graph name, to be displayed as the caption of the Jung window.
	 * @param conv label converter, ignored if null.
	 * @return LearnerGraph graph for it
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public static LearnerGraph buildLearnerGraph(String fsm, String name, Configuration config, final ConvertALabel conv)
	{
		LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
		buildGraph(fsm,name,config,graph,conv);
		return graph;
	}

	/** Given a textual representation of an fsm, builds a corresponding non-deterministic learner graph
	 *
	 * @param fsm the textual representation of an FSM
	 * @param name graph name, to be displayed as the caption of the Jung window.
	 * @param conv label converter, ignored if null.
	 * @return LearnerGraphND graph for it
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public static LearnerGraphND buildLearnerGraphND(String fsm, String name, Configuration config, final ConvertALabel conv)
	{
		LearnerGraphND graph = new LearnerGraphND(config);graph.initEmpty();
		buildGraph(fsm,name,config,graph,conv);
		return graph;
	}

	/** Given a textual representation of an fsm, builds a corresponding graph
	 *
	 * @param fsm the textual representation of an FSM
	 * @param name graph name.
	 * @param conv label converter, ignored if null.
	 * @throws IllegalArgumentException if fsm cannot be parsed.
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> void buildGraph(String fsm, String name, final Configuration config,
																									  final AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> target, final ConvertALabel conv)
	{
		assert config.getTransitionMatrixImplType() != STATETREE.STATETREE_ARRAY || conv != null : "converter has to be set for an ARRAY transition matrix";
		//assert conv == null || config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY : "non-null converter may only accompany an ARRAY transition matrix in tests";
		target.setName(name);
		new FsmParserStatechum(fsm,config.getLabelKind() != Configuration.LABELKIND.LABEL_INPUT_OUTPUT).parse(new TransitionReceiver()
		{
			public void put(String from, String to, Label label, boolean accept) {
				CmpVertex fromVertex = target.transitionMatrix.findKey(VertexID.parseID(from)), toVertex = target.transitionMatrix.findKey(VertexID.parseID(to));

				if (fromVertex == null)
				{
					fromVertex = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(from), config);
					if (target.transitionMatrix.isEmpty())
						target.setInit(fromVertex);
					target.transitionMatrix.put(fromVertex, target.createNewRow());
					fromVertex.setAccept(true);
				}
				else
				if (!fromVertex.isAccept())
					throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+from);

				if (from.equals(to))
				{
					if (!accept) throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+to);
					toVertex = fromVertex;
				}
				else
				if (toVertex == null)
				{
					toVertex = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID(to),config);
					toVertex.setAccept(accept);
					target.transitionMatrix.put(toVertex, target.createNewRow());
				}
				else
				if (toVertex.isAccept() != accept)
					throw new IllegalArgumentException("conflicting acceptance assignment on vertex "+to);

				Label convertedLabel = label;if (conv != null) convertedLabel = conv.convertLabelToLabel(label);
				target.addTransition(target.transitionMatrix.get(fromVertex),convertedLabel,toVertex);
			}

			@Override
			public void accept(String from, String to, Label label) {
				put(from,to,label,true);
			}

			@Override
			public void reject(String from, String to, Label label) {
				put(from,to,label,false);
			}

			@Override
			public void pairCompatibility(String stateA, PAIRCOMPATIBILITY pairRelation, String stateB) {
				CmpVertex fromVertex = target.transitionMatrix.findKey(VertexID.parseID(stateA)), toVertex = target.transitionMatrix.findKey(VertexID.parseID(stateB));
				if (fromVertex == null)
					throw new IllegalArgumentException("unknown vertex "+stateA);
				if (toVertex == null)
					throw new IllegalArgumentException("unknown vertex "+stateB);
				target.addToCompatibility(fromVertex, toVertex, pairRelation);
			}
		},config,conv);
	}
}
