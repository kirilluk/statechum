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

package statechum.analysis.learning;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.GlobalConfiguration;
import statechum.JUConstants;
import statechum.Label;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.PathRoutines;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.spin.SpinResult;
import statechum.analysis.learning.spin.SpinUtil;
import statechum.apps.QSMTool;
import statechum.apps.QSMTool.TraceAdder;
import statechum.model.testset.PTASequenceEngine;

import java.awt.Frame;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.JOptionPane;

public class RPNIUniversalLearner extends RPNILearner 
{

	/** If-then automata in a text form. */
	private Collection<String> ifthenAutomataAsText;
	
	/** If-then automata can be defined with a larger alphabet than is present on the graph being augmented, in which case it is not possible to apply the considered if-then automata. 
	 * In addition, if-then automata can use negations on transitions which are interpreted as a complement but the universe is not usually made clear. This variable contains such a universal set.
	 * Where null, the alphabet of the graph being augmented is used. 
	 */
	protected Set<Label> alphabetUsedForIfThen = null; 

	public void setAlphabetUsedForIfThen(Set<Label> alph)
	{
		alphabetUsedForIfThen = alph;
	}
	
	public RPNIUniversalLearner(Frame parent, LearnerEvaluationConfiguration evalCnf) 
	{
		super(parent, evalCnf.config, evalCnf.getLabelConverter());
		ifthenAutomataAsText = evalCnf.ifthenSequences;
		if(ifthenAutomataAsText == null)
			ifthenAutomataAsText = new HashSet<String>();
		setTentativeAutomaton(new LearnerGraph(evalCnf.config));
	}

	public RPNIUniversalLearner(Frame parent, Configuration conf, ConvertALabel cnv) 
	{
		super(parent,conf,cnv);
		setTentativeAutomaton(new LearnerGraph(conf));
	}	
	private LearnerGraph tentativeAutomaton = null;

	@Override
	public LearnerGraph init(Collection<List<Label>> plus, Collection<List<Label>> minus)
	{// Given that we may have a graph with a single reject-state, we'd like to start by adding
	 // reject-sequences first.
		getTentativeAutomaton().initPTA();		
		getTentativeAutomaton().paths.augmentPTA(minus, false,false);
		getTentativeAutomaton().paths.augmentPTA(plus, true,false);
		return getTentativeAutomaton();
	}

	@Override
	public LearnerGraph init(PTASequenceEngine en, 
			@SuppressWarnings("unused") int plusSize, @SuppressWarnings("unused") int minusSize)
	{
		getTentativeAutomaton().initPTA();
		getTentativeAutomaton().paths.augmentPTA(en);

		return getTentativeAutomaton();
	}
	
	@Override
	public LearnerGraph init(LearnerGraph gr) 
	{
		//getTentativeAutomaton()
		final Configuration shallowCopy = gr.config.copy();shallowCopy.setLearnerCloneGraph(false);
		tentativeAutomaton = new LearnerGraph(shallowCopy);
		LearnerGraph.copyGraphs(gr,tentativeAutomaton);
		return tentativeAutomaton;
	}
	
	/** Returns statistics reflecting the learning. 
	 */
	@Override 
	public String getResult()
	{
		return null;
	}

	/** Identifies a collection of states to merge, sorted in the order of scores. */
	@Override 
	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph)
	{
		Stack<PairScore> pairs = graph.pairscores.chooseStatePairs(null);
		return pairs;
	}
	
	/** Given a graph, merges a pair of states from it and returns the result. */
	@Override 
	public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair)
	{
		LearnerGraph result = MergeStates.mergeAndDeterminize_general(original, pair);
		return result;
	}

	/** A graph representing constraints to be folded into PTA before learning commences and 
	 * upon every restart.
	 */
	protected LearnerGraph [] ifthenAutomata = null;
	
	@Override 
	public boolean AddConstraints(LearnerGraph pta, LearnerGraph outcome, StringBuffer counterExampleHolder)
	{
		assert ifthenAutomata != null;
		
		System.out.println("adding constraints");
		boolean result = true;
		LearnerGraph.copyGraphs(pta, outcome);
		try {
			Transform.augmentFromIfThenAutomaton(outcome, null, ifthenAutomata, config.getHowManyStatesToAddFromIFTHEN());
		} catch (AugmentFromIfThenAutomatonException e) {
			// merge failed because the constraints disallowed it, hence return a failure
			result = false;e.getFailureLocation(counterExampleHolder);
		}
		return result;
	}
	
	/** Given a pair of graphs, computes the set of questions to validate the merge which 
	 * resulted in the second graph
	 * 
	 * @param original the original graph
	 * @param tempNew the merged graph
	 * @param pair the pair of states merged in the original graph
	 */
	@Override 
	public List<List<Label>> ComputeQuestions(PairScore pair, @SuppressWarnings("unused") LearnerGraph original, LearnerGraph tempNew)
	{
		if (ifthenAutomata == null && config.isUseConstraints()) 
			ifthenAutomata = Transform.buildIfThenAutomata(ifthenAutomataAsText, alphabetUsedForIfThen, config, getLabelConverter()).toArray(new LearnerGraph[0]);
		return ComputeQuestions.computeQS(pair, getTentativeAutomaton(),tempNew, ifthenAutomata);
	}

	/** Given a pair of graphs, rebuilds a set of questions to validate the merge which 
	 * resulted in the second graph. This one retains a Pta of questions previously asked
	 * and hence can be used to dynamically update the collection of questions using if-then automata.
	 * 
	 * @param original the original graph
	 * @param temp the merged graph
	 * @param pair the pair of states merged in the original graph
	 */
	@Override 
	public List<List<Label>> RecomputeQuestions(PairScore pair,@SuppressWarnings("unused") LearnerGraph original, LearnerGraph temp)
	{
		if (ifthenAutomata == null && config.isUseConstraints()) 
			ifthenAutomata = Transform.buildIfThenAutomata(ifthenAutomataAsText, alphabetUsedForIfThen, config, getLabelConverter()).toArray(new LearnerGraph[0]);
		return ComputeQuestions.RecomputeQS(pair, getTentativeAutomaton(),temp, ifthenAutomata);
	}
	
	@Override 
	public void AugmentPTA(LearnerGraph pta, @SuppressWarnings("unused") RestartLearningEnum ptaKind,
			List<Label> sequence, boolean accepted, JUConstants newColour) {
		pta.paths.augmentPTA(sequence, accepted, false, newColour);
	}

	public void AugumentPTA_and_QuestionPTA(LearnerGraph pta, RestartLearningEnum ptaKind,
			List<Label> sequence, boolean accepted, JUConstants newColour)
	{
		topLevelListener.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
		if (!config.getUseSpin())
		{	// This check has to be performed because if constraints are used, 
			// LTL will be converted to a maximal automata which is not appropriate if the traditional way (with Spin) is used.
			@SuppressWarnings("unused")
			Boolean augmentResult = getTentativeAutomaton().transform.AugmentNonExistingMatrixWith(sequence, accepted);// rule out a question.
			// Note that since we've attempted to augment our new tentative automaton right after 
			// merging and reached no contradiction, we can add new paths one-by one here 
			// and expect no contradiction.
			
			// The above AugmentNonExistingMatrixWith may possibly return null where we are adding paths returned
			// by Erlang and these paths do not necessarily correspond to any specific question. This is why 
			// the following assertion is commented out.			
			//assert augmentResult == null || augmentResult.booleanValue() : "trace "+sequence+"/"+accepted+" did not honour the sequence returned by mapPathToConfirmedElements";
		}
	}

	protected String learntGraphName = null;
	
	public void setGraphNameSuffix(String suffix)
	{
		learntGraphName = suffix;
	}
	
	protected String getGraphName()
	{
		if (learntGraphName == null) {
		 learntGraphName = "beinglearnt";
		}
		return GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP)+"/"+learntGraphName;
	}
	
	public LearnerGraph ptaHardFacts = null;
	
	@Override 
	public LearnerGraph learnMachine()
	{
		final Configuration shallowCopy = getTentativeAutomaton().config.copy();shallowCopy.setLearnerCloneGraph(false);
		ptaHardFacts = new LearnerGraph(shallowCopy);// this is now cloned to eliminate counter-examples added to ptaSoftFacts by Spin
		SpinUtil spin = null;
		LearnerGraph.copyGraphs(getTentativeAutomaton(), ptaHardFacts);
		setAlphabetUsedForIfThen(ptaHardFacts.pathroutines.computeAlphabet());
		LearnerGraph ptaSoftFacts = getTentativeAutomaton();
		setChanged();getTentativeAutomaton().setName(getGraphName()+"_init");
		final List<List<Label>> extraTracesPlus = new LinkedList<List<Label>>(), extraTracesMinus = new LinkedList<List<Label>>();

		if (config.isUseConstraints()) 
		{
			LearnerGraph updatedTentativeAutomaton = new LearnerGraph(shallowCopy);
			StringBuffer counterExampleHolder = new StringBuffer();
			if (ifthenAutomata == null) 
				ifthenAutomata = Transform.buildIfThenAutomata(ifthenAutomataAsText, alphabetUsedForIfThen, config, topLevelListener.getLabelConverter()).toArray(new LearnerGraph[0]);

			if (!topLevelListener.AddConstraints(getTentativeAutomaton(),updatedTentativeAutomaton,counterExampleHolder))
				throw new IllegalArgumentException(getHardFactsContradictionErrorMessage(ifthenAutomataAsText, counterExampleHolder.toString()));
			setTentativeAutomaton(updatedTentativeAutomaton);
		}
		if (getTentativeAutomaton().config.getUseLTL() && getTentativeAutomaton().config.getUseSpin() && !ifthenAutomataAsText.isEmpty()){
			spin = new SpinUtil(config,getLabelConverter());
			SpinResult sr = spin.check(ptaHardFacts, ifthenAutomataAsText);
			if(!sr.isPass())
				throw new IllegalArgumentException(getHardFactsContradictionErrorMessage(ifthenAutomataAsText, sr.getCounters()));
		}

		Stack<PairScore> possibleMerges = topLevelListener.ChooseStatePairs(getTentativeAutomaton());
		int iterations = 0, currentNonAmber = ptaHardFacts.getStateNumber()-ptaHardFacts.getAmberStateNumber();
		JUConstants colourToAugmentWith = getTentativeAutomaton().config.getUseAmber()? JUConstants.AMBER:null;
		updateGraph(getTentativeAutomaton(),ptaHardFacts);
		while (!possibleMerges.isEmpty()) 
		{
			iterations++;
			PairScore pair = possibleMerges.pop();
			final LearnerGraph temp = topLevelListener.MergeAndDeterminize(getTentativeAutomaton(), pair);
			Collection<List<Label>> questions = new LinkedList<List<Label>>();
			long score = pair.getScore();
			RestartLearningEnum restartLearning = RestartLearningEnum.restartNONE;// whether we need to rebuild a PTA and restart learning.

			if (getTentativeAutomaton().config.getUseLTL() && getTentativeAutomaton().config.getUseSpin() && !ifthenAutomataAsText.isEmpty()){

				Collection<List<Label>> counterExamples = spin.check(temp, getTentativeAutomaton(), ifthenAutomataAsText).getCounters();
				Iterator<List<Label>> counterExampleIt = counterExamples.iterator();
				while(counterExampleIt.hasNext())
				{
					List<Label> counterExample = counterExampleIt.next();
					topLevelListener.AugmentPTA(ptaSoftFacts, RestartLearningEnum.restartSOFT, counterExample, false,colourToAugmentWith);
					System.out.println("<temp> "+counterExample);
					
				}
				if(counterExamples.size()>0)
					restartLearning = RestartLearningEnum.restartSOFT;
			}

			if (config.isUseConstraints()) 
			{
				LearnerGraph updatedTentativeAutomaton = new LearnerGraph(shallowCopy);
				StringBuffer counterExampleHolder = new StringBuffer();
				if (!topLevelListener.AddConstraints(temp,updatedTentativeAutomaton,counterExampleHolder))
				{
					getTentativeAutomaton().addToCompatibility(pair.firstElem, pair.secondElem, PAIRCOMPATIBILITY.INCOMPATIBLE);
					restartLearning = RestartLearningEnum.restartRECOMPUTEPAIRS;
					System.out.println("<info> pair "+pair+" contradicts constraints, hence recorded as incompatible");
				}
				// since we still need the outcome of merging to ask questions, 
				// we delay actually performing augmentation until the time we are 
				// finished with questions.
			}

			Iterator<List<Label>> questionIt = null;

			if (restartLearning == RestartLearningEnum.restartNONE)
			{
				Map<CmpVertex,JUConstants.PAIRCOMPATIBILITY> compatibilityMap = ptaHardFacts.pairCompatibility.compatibility.get(pair.getQ());
				if (pair.getScore() > config.getScoreForAutomergeUponRestart() && compatibilityMap != null && compatibilityMap.get(pair.getR()) == JUConstants.PAIRCOMPATIBILITY.MERGED)
					System.out.println("<automerge of previously merged pair> "+pair);
				else
				{
					// ask questions if needed
					if (shouldAskQuestions(score)) 
					{
						temp.setName(getGraphName()+"_"+iterations);
						//LearnerGraph updatedGraphActual = ComputeQuestions.constructGraphWithQuestions(pair, tentativeAutomaton, temp);
						//updatedGraphActual.setName(getGraphName(config)+"questions "+iterations);setChanged();updateGraph(updatedGraphActual,ptaHardFacts);
						
						questions = topLevelListener.ComputeQuestions(pair, getTentativeAutomaton(), temp);// all answers are considered "hard", hence we have to ask questions based on hard facts in order to avoid prefixes which are not valid in hard facts
						questionIt = questions.iterator();
						if (questionIt.hasNext())
						{
							pair.firstElem.setHighlight(true);
							pair.secondElem.setHighlight(true);
							updateGraph(getTentativeAutomaton(),ptaHardFacts);
							pair.firstElem.setHighlight(false);
							pair.secondElem.setHighlight(false);
						}
					}
				}
			}

			while (restartLearning == RestartLearningEnum.restartNONE && questionIt != null && questionIt.hasNext()) 
			{
				List<Label> question = questionIt.next();
				
				boolean accepted = pair.getQ().isAccept();
				Pair<Integer,String> answer = null;
				if (getTentativeAutomaton().config.getUseLTL() && getTentativeAutomaton().config.getUseSpin() && !ifthenAutomataAsText.isEmpty())
					answer = new Pair<Integer,String>(spin.check(question, ifthenAutomataAsText),null);
				
				CmpVertex tempVertex = temp.getVertex(question);
				boolean answerFromSpin = false;
				if(answer != null && answer.firstElem >= 0) 
					answerFromSpin = true;
				else
				{
					if (GlobalConfiguration.getConfiguration().isAssertEnabled())
						if (ptaHardFacts.paths.tracePathPrefixClosed(question) == AbstractOracle.USER_ACCEPTED) {
							throw new IllegalArgumentException("question "+ question+ " has already been answered");
						}
					List<Boolean> acceptedElements = PathRoutines.mapPathToConfirmedElements(ptaHardFacts,question,ifthenAutomata);

					answer = topLevelListener.CheckWithEndUser(getTentativeAutomaton(), question, 
							tempVertex.isAccept()?AbstractOracle.USER_ACCEPTED:question.size() - 1,
									acceptedElements, pair,
							new Object[] { "LTL","IFTHEN","IGNORE QUESTION","MARK AS INCOMPATIBLE","Add trace"});
				}
				if (answer.firstElem == AbstractOracle.USER_CANCELLED) 
				{
					System.out.println("CANCELLED");
					return null;
				}
				else
				if (answer.firstElem == AbstractOracle.USER_IGNORED)
				{// do nothing
					restartLearning = RestartLearningEnum.restartNONE;
				}
				else
				if (answer.firstElem == AbstractOracle.USER_INCOMPATIBLE)
				{
					/* When autoanswers says that a particular pair is incompatible, <em>StoredAnswers</em> the answer refers to the current
					 * question, but we have to return a pair which is incompatible. Although strange, it is not really surprising - the whole 
					 * AutoAnswers framework aims to automate debugging hence we are expected to go through the same sequence of questions
					 * over and over again - it is enough to validate that we do not deviate and hence all that is necessary is to check that 
					 * the current pair to be merged is the same as the one recorded as incompatible when an answer was recorded.
					 */

					getTentativeAutomaton().addToCompatibility(pair.firstElem, pair.secondElem, PAIRCOMPATIBILITY.INCOMPATIBLE);
					restartLearning = RestartLearningEnum.restartRECOMPUTEPAIRS;
				}
				else
				if (answer.firstElem == AbstractOracle.USER_NEWTRACE)
				{
					String traceDescr = answer.secondElem;
					boolean obtainedViaAuto = answer.secondElem != null;
					if (traceDescr == null) traceDescr = JOptionPane.showInputDialog("New trace :");
					if(traceDescr != null && traceDescr.length() != 0)
					{
						final JUConstants colour = colourToAugmentWith;
						final AtomicBoolean whetherToRestart = new AtomicBoolean(false);
						QSMTool.parseSequenceOfTraces(traceDescr, config, new TraceAdder() {

							@Override
							public void addTrace(List<Label> trace,	boolean positive) {
								if (positive) extraTracesPlus.add(trace);else extraTracesMinus.add(trace);
								CmpVertex tailVertex = temp.getVertex(trace);
								if (tailVertex != null && tailVertex.isAccept() != positive)
									whetherToRestart.set(true);
							}
							
						}, getLabelConverter());
					
						if (!obtainedViaAuto) System.out.println(RPNILearner.QUESTION_USER+" "+question.toString()+" "+RPNILearner.QUESTION_NEWTRACE+" "+traceDescr);
						// At this point, we attempt to augment the current automaton with the supplied traces,
						// which may be successful or not (if we did some erroneous mergers earlier), in which case we restart.

			            for(List<Label> positive:extraTracesPlus)
			            	AugumentPTA_and_QuestionPTA(ptaHardFacts,RestartLearningEnum.restartHARD,positive, true,colour);
						for(List<Label> negative:extraTracesMinus) 
							AugumentPTA_and_QuestionPTA(ptaHardFacts,RestartLearningEnum.restartHARD,negative, false,colour);
						
						if (whetherToRestart.get())
							restartLearning = RestartLearningEnum.restartHARD;// we've seen at least one trace which contradicts the new tentative PTA.
						else
							restartLearning = RestartLearningEnum.restartRECOMPUTEQUESTIONS;// the set of questions will be rebuilt because we possibly modified the "nonexistent" PTA containing questions.
					}
				}
				else
				if (answer.firstElem == AbstractOracle.USER_ACCEPTED) 
				{
					if(!answerFromSpin) // only add to hard facts when obtained directly from a user or from autofile
						AugumentPTA_and_QuestionPTA(ptaHardFacts,RestartLearningEnum.restartHARD,question, true,colourToAugmentWith);
					
					if (getTentativeAutomaton().config.getUseLTL() && getTentativeAutomaton().config.getUseSpin()) topLevelListener.AugmentPTA(ptaSoftFacts,RestartLearningEnum.restartSOFT,question, true,colourToAugmentWith);

					if (!tempVertex.isAccept()) 
					{// contradiction with the result of merging
						if(!answerFromSpin)
							restartLearning = RestartLearningEnum.restartHARD;
						else
							restartLearning = RestartLearningEnum.restartSOFT;
						
					}
				} 
				else 
				if (answer.firstElem >= 0) 
				{// The sequence has been rejected by a user
					assert answer.firstElem < question.size();
					LinkedList<Label> subAnswer = new LinkedList<Label>();
					subAnswer.addAll(question.subList(0, answer.firstElem + 1));
					if(!answerFromSpin) // only add to hard facts when obtained directly from a user or from autofile
						AugumentPTA_and_QuestionPTA(ptaHardFacts, RestartLearningEnum.restartHARD,subAnswer, false,colourToAugmentWith);

					if (getTentativeAutomaton().config.getUseLTL() && getTentativeAutomaton().config.getUseSpin()) topLevelListener.AugmentPTA(ptaSoftFacts,RestartLearningEnum.restartSOFT,subAnswer, false,colourToAugmentWith);
					// important: since vertex IDs is
					// only unique for each instance of ComputeStateScores, only
					// one instance should ever receive calls to augmentPTA

					if ((answer.firstElem < question.size() - 1) || tempVertex.isAccept()) 
					{// contradiction with the result of merging
						assert accepted == true;
						if(!answerFromSpin)
							restartLearning = RestartLearningEnum.restartHARD;
						else
							restartLearning = RestartLearningEnum.restartSOFT;
						
					}
				} 
				else 
					if(answer.firstElem == AbstractOracle.USER_LTL || answer.firstElem == AbstractOracle.USER_IFTHEN)
					{
						String answerType = null;
						if (answer.firstElem == AbstractOracle.USER_LTL)
							answerType = QSMTool.cmdLTL;
						else
							if (answer.firstElem == AbstractOracle.USER_IFTHEN)
								answerType = QSMTool.cmdIFTHENAUTOMATON;
							else
								throw new IllegalArgumentException("unexpected user choice kind "+answer.firstElem);

						restartLearning = RestartLearningEnum.restartRECOMPUTEQUESTIONS;
						String addedConstraint = answer.secondElem;
						boolean obtainedLTLViaAuto = addedConstraint != null;
						if (addedConstraint == null) addedConstraint = JOptionPane.showInputDialog("New "+answerType+" formula:");
						if(addedConstraint != null && addedConstraint.length() != 0)
						{
							if (!obtainedLTLViaAuto) System.out.println(QUESTION_USER+" "+question.toString()+ " <"+answerType+"> "+addedConstraint);
							Set<String> tmpLtl = new HashSet<String>();tmpLtl.addAll(ifthenAutomataAsText);tmpLtl.add(answerType+" "+addedConstraint);
							if(!config.isUseConstraints())
							{
								Collection<List<Label>> counters = spin.check(ptaHardFacts, tmpLtl).getCounters();
								if (counters.size()>0)
								{
									String errorMessage = getHardFactsContradictionErrorMessage(tmpLtl, counters);
									if (obtainedLTLViaAuto) // cannot recover from autosetting, otherwise warn a user
										throw new IllegalArgumentException(errorMessage);
									
									// if not obtained via auto, complain
									System.out.println(errorMessage);
								}
							}
							else
							{
								LearnerGraph tmpIfthenAutomata[] = Transform.buildIfThenAutomata(tmpLtl, alphabetUsedForIfThen, config, getLabelConverter()).toArray(new LearnerGraph[0]);
								LearnerGraph updatedTentativeAutomaton = new LearnerGraph(shallowCopy);

								LearnerGraph.copyGraphs(getTentativeAutomaton(), updatedTentativeAutomaton);
								try {
									Transform.augmentFromIfThenAutomaton(updatedTentativeAutomaton, null, tmpIfthenAutomata, config.getHowManyStatesToAddFromIFTHEN());
								} catch (AugmentFromIfThenAutomatonException e) {
								// merge failed because the constraints disallowed it, hence return a failure
									StringBuffer counterExampleHolder = new StringBuffer();
									e.getFailureLocation(counterExampleHolder);
									String errorMessage = getHardFactsContradictionErrorMessage(ifthenAutomataAsText, counterExampleHolder.toString());
									if (obtainedLTLViaAuto) // cannot recover from autosetting, otherwise warn a user
										throw new IllegalArgumentException(errorMessage);
									// if not obtained via auto, complain
						        	System.out.println(errorMessage);
								} 
							}

							// the current set of constraints does not contradict hard facts, update them and restart learning.
							ifthenAutomataAsText.add(answerType+" "+addedConstraint);
							
							// make sure constraints are rebuilt if in use
							if (config.isUseConstraints())
								ifthenAutomata = Transform.buildIfThenAutomata(ifthenAutomataAsText, alphabetUsedForIfThen, config, getLabelConverter()).toArray(new LearnerGraph[0]);
							
							restartLearning = RestartLearningEnum.restartHARD;
						}
						// no formula was entered, do not set the <em>questionAnswered</em> to answered, hence 
					    // when we get to the top of the loop, we'll re-pop the previous question.
					}
					else
						throw new IllegalArgumentException("unexpected user choice "+answer);
				
				if ( (config.isUseConstraints() && restartLearning == RestartLearningEnum.restartNONE) || restartLearning == RestartLearningEnum.restartRECOMPUTEQUESTIONS)
				{
					questions = topLevelListener.RecomputeQuestions(pair, getTentativeAutomaton(), temp);// all answers are considered "hard", hence we have to ask questions based on hard facts in order to avoid prefixes which are not valid in hard facts
					questionIt = questions.iterator();restartLearning = RestartLearningEnum.restartNONE;
				}
			} // loop of questions

			if (restartLearning == RestartLearningEnum.restartHARD || restartLearning == RestartLearningEnum.restartSOFT) 
			{// restart learning
				
				if (restartLearning == RestartLearningEnum.restartHARD)
				{
					if (config.isSpeculativeQuestionAsking())
						if (speculativeGraphUpdate(possibleMerges, ptaHardFacts))
							return null;// this is the case when a user cancels the learning process when presented by "speculative" questions.
					LearnerGraph.copyGraphs(ptaHardFacts,ptaSoftFacts);// this is cloned to eliminate counter-examples added to ptaSoftFacts by Spin
				}
				setTentativeAutomaton(ptaSoftFacts);// no need to clone - this is the job of mergeAndDeterminize anyway
				getTentativeAutomaton().clearColoursButAmber();// this one will clear all colours if amber mode is not set.

				setChanged();
			} 
			else 
			if (restartLearning == RestartLearningEnum.restartNONE)
			{
				// At this point, tentativeAutomaton may have been modified because
				// it may point to
				// the original PTA which will be modified as a result of new
				// sequences being added to it.
				// temp is different too, hence there is no way for me to
				// compute compatibility score here.
				// This is hence computed inside the obtainPair method.

				// keep going with the existing model
				setTentativeAutomaton(temp);
				
				// if we record the fact that the pair was merged so that we do not ask questions upon restart, do it here.
				if (config.getScoreForAutomergeUponRestart() != Integer.MAX_VALUE)
				{
					ptaHardFacts.pairCompatibility.addToCompatibility(pair.getQ(), pair.getR(), PAIRCOMPATIBILITY.MERGED);
				}
			}
			// if restartLearning == RestartLearningEnum.restartRECOMPUTEPAIRS, we do nothing, i.e. attempt to get state pairs again.

			if (restartLearning != RestartLearningEnum.restartRECOMPUTEPAIRS) 
			{
				
				if (config.isUseConstraints())
				{
					// Augmentation from IF-THEN does not use incompatibility constraints in a tentative automaton hence no point in re-augmenting (especially given that I do not have an automaton from before augmentation preserved).
					LearnerGraph updatedTentativeAutomaton = new LearnerGraph(shallowCopy);
					StringBuffer counterExampleHolder = new StringBuffer();
					if (!topLevelListener.AddConstraints(getTentativeAutomaton(),updatedTentativeAutomaton,counterExampleHolder))
						throw new IllegalArgumentException(getHardFactsContradictionErrorMessage(ifthenAutomataAsText, counterExampleHolder.toString()));
					setTentativeAutomaton(updatedTentativeAutomaton);
				}
				// Where Erlang learner adds additional traces, these have to be recorded in extraTracesPlus/extraTracesMinus 
				// and applied later. If these are applied immediately, AugmentPTA resets cache and the PTA of questions is lost.
				// The map could be null if no merger completed yet
				try
				{
		            Map<VertID, Collection<VertID>> mergedToHard = getTentativeAutomaton().getCache().getMergedToHardFacts();
		            for(List<Label> positive:extraTracesPlus)
		            {
		            	topLevelListener.AugmentPTA(getTentativeAutomaton(),RestartLearningEnum.restartHARD,positive, true,colourToAugmentWith);
						CmpVertex hardFactsVertex = ptaHardFacts.getVertex(positive), tentativeVertex=getTentativeAutomaton().getVertex(positive);
						List<VertID> idList = new LinkedList<VertID>();idList.add(hardFactsVertex);
						if (mergedToHard != null) mergedToHard.put(tentativeVertex, idList);
		            }
					for(List<Label> negative:extraTracesMinus) 
					{
						topLevelListener.AugmentPTA(getTentativeAutomaton(),RestartLearningEnum.restartHARD,negative, false,colourToAugmentWith);
						CmpVertex hardFactsVertex = ptaHardFacts.getVertex(negative), tentativeVertex=getTentativeAutomaton().getVertex(negative);
						List<VertID> idList = new LinkedList<VertID>();idList.add(hardFactsVertex);
						if (mergedToHard != null) mergedToHard.put(tentativeVertex, idList);
					}
				}
				catch(IllegalArgumentException ex)
				{// one of the traces contradicts the decisions we made earlier, hence restart the learning process
					assert ex.getMessage().contains("incompatible");
					restartLearning = RestartLearningEnum.restartHARD;
				}
				finally
				{
					extraTracesPlus.clear();extraTracesMinus.clear();
				}
			}

			topLevelListener.Restart(restartLearning);
			
			//System.out.println("<info> restart: "+restartLearning);

			possibleMerges = topLevelListener.ChooseStatePairs(getTentativeAutomaton());
		}
		
		assert !config.getUseAmber() || currentNonAmber == ptaHardFacts.getStateNumber()-ptaHardFacts.getAmberStateNumber();
		updateGraph(getTentativeAutomaton(),ptaHardFacts);
		return getTentativeAutomaton();
	}

	protected String getHardFactsContradictionErrorMessage(Collection<String> tmpLtl, Collection<List<Label>> counters)
	{
		StringBuffer errString = new StringBuffer();
		Iterator<List<Label>> counterIt = counters.iterator();
		while(counterIt.hasNext()){
			errString.append(counterIt.next());errString.append('\n');
		}
		for(String elem:tmpLtl) errString.append(elem+"\n");
		return getHardFactsContradictionErrorMessage(tmpLtl,errString.toString());
	}
	
	protected String getHardFactsContradictionErrorMessage(Collection<String> tmpLtl, String counterExample)
	{
		String errString = "LTL formula or IFTHEN automata "+tmpLtl+" contradict hard facts\n"+counterExample;
		return errString;
	}
	
	/** We might be doing a restart, but it never hurts to go through the existing 
	 * collection of vertices to merge and see if we can update the graph.
	 * FIXME: there is no support for LTL/IFTHEN/IGNORE in this method.
	 * @return true if question answering has been cancelled by a user.
	 */
	boolean speculativeGraphUpdate(Stack<PairScore> possibleMerges, LearnerGraph hardFacts)
	{
		JUConstants colourToAugmentWith = getTentativeAutomaton().config.getUseAmber()? JUConstants.AMBER:null;

		while(!possibleMerges.isEmpty())
		{
			PairScore pair = possibleMerges.pop();
			long score = pair.getScore();

			if(shouldAskQuestions(score))
			{
				LearnerGraph tempNew = null;
				try
				{
					tempNew = topLevelListener.MergeAndDeterminize(hardFacts, pair);
				}
				catch(IllegalArgumentException ex)
				{// ignore - tempNew is null anyway					
				}
				
				if (tempNew != null) // merge successful - it would fail if our updates to newPTA have modified tentativeAutomaton (the two are often the same graph)
				{					
					for(List<Label> question:topLevelListener.ComputeQuestions(pair, hardFacts, tempNew))
					{
						List<Boolean> acceptedElements = null;
						if (getTentativeAutomaton().config.isUseConstraints())
							acceptedElements = PathRoutines.mapPathToConfirmedElements(hardFacts,question,ifthenAutomata);
						Pair<Integer,String> answer = topLevelListener.CheckWithEndUser(getTentativeAutomaton(),question, tempNew.getVertex(question).isAccept()?AbstractOracle.USER_ACCEPTED:question.size() - 1,acceptedElements,pair,new Object [] {"Test"});
						if (answer.firstElem == AbstractOracle.USER_CANCELLED)
						{
							System.out.println("CANCELLED");
							return true;
						}
						
						if(answer.firstElem == AbstractOracle.USER_ACCEPTED)
						{
							topLevelListener.AugmentPTA(hardFacts,RestartLearningEnum.restartHARD,question, true,colourToAugmentWith);
						}
						else 
							if(answer.firstElem >= 0)
							{// The sequence has been rejected by a user
								assert answer.firstElem < question.size();
								LinkedList<Label> subAnswer = new LinkedList<Label>();subAnswer.addAll(question.subList(0, answer.firstElem+1));
								topLevelListener.AugmentPTA(hardFacts,RestartLearningEnum.restartHARD,subAnswer, false,colourToAugmentWith);
							}
					}
				}
			}
		}
		
		return false;
	}

	public void setTentativeAutomaton(LearnerGraph automaton) {
		this.tentativeAutomaton = automaton;
	}

	public LearnerGraph getTentativeAutomaton() {
		return tentativeAutomaton;
	}
}
