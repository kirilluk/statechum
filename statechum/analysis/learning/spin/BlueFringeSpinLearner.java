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

package statechum.analysis.learning.spin;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

public class BlueFringeSpinLearner extends RPNIBlueFringeLearnerTestComponentOpt {

	private Set<String> ltl;

	public BlueFringeSpinLearner(Frame parent, Set<String> ltlFormulae, Configuration conf) {
		super(parent, conf);
		ltl = ltlFormulae;
	}

	enum RestartLearningEnum { restartNONE, restartHARD, restartSOFT };
	public static final String learntGraphName="tmp/GRAPH_BEING_LEARNT";
	
	public DirectedSparseGraph learnMachine() {
		LearnerGraph.testMode = true;
		setAutoOracle();
		Map<Integer, AtomicInteger> whichScoresWereUsedForMerging = new HashMap<Integer, AtomicInteger>(), restartScoreDistribution = new HashMap<Integer, AtomicInteger>();
		Map<PairScore, Integer> scoresToIterations = new HashMap<PairScore, Integer>();
		Map<PairScore, Integer> restartsToIterations = new HashMap<PairScore, Integer>();
		final Configuration shallowCopy = scoreComputer.config.copy();shallowCopy.setLearnerCloneGraph(false);
		LearnerGraph ptaHardFacts = scoreComputer.copy(shallowCopy);// this is cloned to eliminate counter-examples added to ptaSoftFacts by Spin
		LearnerGraph ptaSoftFacts = scoreComputer;

		if (!SpinUtil.check(ptaHardFacts.paths.getGraph(), ltl))
			throw new IllegalArgumentException(getHardFactsContradictionErrorMessage(ltl));
		
		String pairsMerged = "";
		StringWriter report = new StringWriter();
		counterAccepted = 0;
		counterRejected = 0;
		counterRestarted = 0;
		counterEmptyQuestions = 0;
		report.write("\n[ PTA: " + scoreComputer.paths.getStatistics(false) + " ] ");
		setChanged();scoreComputer.setName(learntGraphName+"_init");
		Stack<PairScore> possibleMerges = scoreComputer.pairscores.chooseStatePairs();
		int plusSize = origPlusSize, minusSize = origMinusSize, iterations = 0;
		while (!possibleMerges.isEmpty()) {

			iterations++;
			// populateScores(possibleMerges,possibleMergeScoreDistribution);
			PairScore pair = possibleMerges.pop();
			
			LearnerGraph temp = MergeStates.mergeAndDeterminize_general(scoreComputer, pair);
			setChanged();
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();
			RestartLearningEnum restartLearning = RestartLearningEnum.restartNONE;// whether we need to rebuild a PTA
											// and restart learning.

			//Visualiser.updateFrame(scoreComputer.paths.getGraph(learntGraphName+"_"+iterations)
			//updateGraph(temp.paths.getGraph(learntGraphName+"_"+counterRestarted+"_"+iterations));
			if (!SpinUtil.check(temp.paths.getGraph(), ltl)) {
				List<String> counterexample = new LinkedList<String>();
				counterexample.addAll(SpinUtil.getCurrentCounterExample());
				System.out.println("<temp> "+counterexample.subList(0, counterexample.size()-1));
				ptaSoftFacts.paths.augmentPTA(counterexample.subList(0, counterexample.size()-1), false,null);
				++minusSize;
				restartLearning = RestartLearningEnum.restartSOFT;
			}
			if (shouldAskQuestions(score) && restartLearning == RestartLearningEnum.restartNONE) 
			{
				temp.setName(learntGraphName+"_"+counterRestarted+"_"+iterations);
				updateGraph(temp);
				questions = ComputeQuestions.computeQS(pair, ptaHardFacts, temp);
				if (questions.isEmpty())
					++counterEmptyQuestions;
			}
			Iterator<List<String>> questionIt = questions.iterator();
			boolean questionAnswered = true;
			List<String> question = null;
			while (questionIt.hasNext() && restartLearning == RestartLearningEnum.restartNONE) {
				if (questionAnswered) question = questionIt.next();// only pick next question if we've got an answer to the previous one
				questionAnswered = false;
				
				boolean accepted = pair.getQ().isAccept();howAnswerWasObtained="";
				Pair<Integer,String> answer = new Pair<Integer,String>(checkWithSPIN(question),null);
				if(answer.firstElem >= 0) 
					howAnswerWasObtained=QUESTION_SPIN;// label reject with <spin>
				else
					answer = checkWithEndUser(scoreComputer.paths.getGraph(), question, new Object[] { "LTL"});
				
				this.questionCounter++;
				if (answer.firstElem == AbstractOracle.USER_CANCELLED) {
					System.out.println("CANCELLED");
					return null;
				}

				CmpVertex tempVertex = temp.getVertex(question);

				if (answer.firstElem == AbstractOracle.USER_ACCEPTED) {
					++counterAccepted;
					if(howAnswerWasObtained == QUESTION_USER || howAnswerWasObtained == QUESTION_AUTO) // only add to hard facts when obtained directly from a user or from autofile
						ptaHardFacts.paths.augmentPTA(question, true,null);
					ptaSoftFacts.paths.augmentPTA(question, true,null);
					++plusSize;
					if (ans != null) System.out.println(howAnswerWasObtained+" "+question.toString()+ " <yes>");
					questionAnswered = true;
					if (!tempVertex.isAccept()) {
						pairsMerged = pairsMerged
								+ "ABOUT TO RESTART due to acceptance of a reject vertex for a pair "
								+ pair + " ========\n";
						if(howAnswerWasObtained == QUESTION_USER || howAnswerWasObtained == QUESTION_AUTO)
							restartLearning = RestartLearningEnum.restartHARD;
						else
							restartLearning = RestartLearningEnum.restartSOFT;
						break;
					}
				} else if (answer.firstElem >= 0) {// The sequence has been rejected by a user
					assert answer.firstElem < question.size();
					++counterRejected;
					questionAnswered = true;
					LinkedList<String> subAnswer = new LinkedList<String>();
					subAnswer.addAll(question.subList(0, answer.firstElem + 1));
					if(howAnswerWasObtained == QUESTION_USER || howAnswerWasObtained == QUESTION_AUTO) // only add to hard facts when obtained directly from a user or from autofile
						ptaHardFacts.paths.augmentPTA(subAnswer, false,null);
					ptaSoftFacts.paths.augmentPTA(subAnswer, false,null);
					++minusSize;// important: since vertex IDs is
					// only unique for each instance of ComputeStateScores, only
					// one instance should ever receive calls to augmentPTA

					if (ans != null) System.out.println(howAnswerWasObtained+" "+question.toString()+ " <no> at position "+answer.firstElem+", element "+question.get(answer.firstElem));
					if ((answer.firstElem < question.size() - 1) || tempVertex.isAccept()) {
						assert accepted == true;
						pairsMerged = pairsMerged
								+ "ABOUT TO RESTART because accept vertex was rejected for a pair "
								+ pair + " ========\n";
						if(howAnswerWasObtained == QUESTION_USER || howAnswerWasObtained == QUESTION_AUTO)
							restartLearning = RestartLearningEnum.restartHARD;
						else
							restartLearning = RestartLearningEnum.restartSOFT;
						break;
					}
				} else if(answer.firstElem == AbstractOracle.USER_LTL){
					String newLtl = answer.secondElem;
					if (newLtl == null) newLtl = JOptionPane.showInputDialog("New LTL formula:");
					if(newLtl != null && newLtl.length() != 0){
						if (ans != null) System.out.println(howAnswerWasObtained+" "+question.toString()+ " <ltl> "+newLtl);
						Set<String> tmpLtl = new HashSet<String>();tmpLtl.addAll(ltl);tmpLtl.add(newLtl);
						if (!SpinUtil.check(ptaHardFacts.paths.getGraph(), tmpLtl))
						{
							if (howAnswerWasObtained == QUESTION_AUTO) // cannot recover from autosetting, otherwise warn a user
								throw new IllegalArgumentException(getHardFactsContradictionErrorMessage(tmpLtl));
							
							System.out.println(getHardFactsContradictionErrorMessage(tmpLtl));
						}
						else // LTL does not contradict hard facts
						{
							ltl.add(newLtl);
							restartLearning = RestartLearningEnum.restartHARD;
							break;
						}
					}
					// no formula was entered, do not set the question to answered, hence re-pop the previous question.
				}
				else
					throw new IllegalArgumentException("unexpected user choice "+answer);
			}

			if (restartLearning != RestartLearningEnum.restartNONE) {// restart learning
				System.out.println("RESTART - "+restartLearning);
				if (restartLearning == RestartLearningEnum.restartHARD)
					ptaSoftFacts = ptaHardFacts.copy(shallowCopy);// this is cloned to eliminate counter-examples added to ptaSoftFacts by Spin
				scoreComputer = ptaSoftFacts;// no need to clone - this is the job of mergeAndDeterminize anyway
				scoreComputer.clearColours();
				setChanged();
				++counterRestarted;
				pairsMerged = pairsMerged + "========== RESTART " + counterRestarted + " ==========\n";
				AtomicInteger count = restartScoreDistribution.get(pair.getScore());
				if (count == null) {
					count = new AtomicInteger();
					restartScoreDistribution.put(pair.getScore(), count);
				}
				count.incrementAndGet();
				restartsToIterations.put(pair, iterations);
				iterations = 0;
			} else {
				// At this point, scoreComputer may have been modified because
				// it may point to
				// the original PTA which will be modified as a result of new
				// sequences being added to it.
				// temp is different too, hence there is no way for me to
				// compute compatibility score here.
				// This is hence computed inside the obtainPair method.
				pairsMerged = pairsMerged + pair + " questions: " + questions.size() + "\n";

				// keep going with the existing model
				scoreComputer = temp;
				// now update the statistics
				AtomicInteger count = whichScoresWereUsedForMerging.get(pair.getScore());
				if (count == null) {
					count = new AtomicInteger();
					whichScoresWereUsedForMerging.put(pair.getScore(), count);
				}
				count.incrementAndGet();
				scoresToIterations.put(pair, iterations);
			}
			possibleMerges = scoreComputer.pairscores.chooseStatePairs();
		}
		report.write("\n[ Questions: " + counterAccepted + " accepted "
				+ counterRejected + " rejected resulting in "
				+ counterRestarted + " restarts; " + counterEmptyQuestions
				+ " empty sets of questions ]\n[ Learned automaton: "
				+ scoreComputer.paths.getStatistics(true) + " ] ");
		report.write("\n[ final sets of questions, plus: " + plusSize
				+ " minus: " + minusSize + " ] ");
		report.write("\n[ Pair scores to iteration numbers:"
				+ pairScoresAndIterations(scoresToIterations,
						"MERGED-ITERATIONS"));
		report.write("\n[ Restart scores to iteration numbers:"
				+ pairScoresAndIterations(restartsToIterations,
						"RESTART-ITERATIONS"));
		report.write("\n[ Pairs merged (score-number of times):"
				+ HistogramToSeries(whichScoresWereUsedForMerging, "MERGED"));
		report.write("\n[ Pairs restarted (score-number of times):"
				+ HistogramToSeries(restartScoreDistribution, "RESTARTED"));
		report.write("\n Pair merge details: \n" + pairsMerged);
		DirectedSparseGraph result = scoreComputer.paths.getGraph(learntGraphName+"_result");
		result.addUserDatum(JUConstants.STATS, report.toString(),
				UserData.SHARED);
		updateGraph(scoreComputer);
		return result;
	}

	protected String getHardFactsContradictionErrorMessage(Set<String> tmpLtl)
	{
		String errString = "LTL formula contradicts hard facts, counter-example is "+SpinUtil.getCurrentCounterExample()+"\n";
		for(String elem:tmpLtl) errString+=elem+"\n";
		return errString;
	}
	
	protected int checkWithSPIN (List<String> question){
		int ret = -1;
		if(!SpinUtil.check(question, ltl)){
			List<String> counterExample = SpinUtil.getCurrentCounterExample();
			//System.out.println(counterExample.subList(0, counterExample.size()));
			return counterExample.size()-1;
		}
		
		return ret;
	}
	
	protected Pair<Integer,String> checkWithEndUser(@SuppressWarnings("unused") DirectedSparseGraph model,
			List<String> question, final Object[] moreOptions) 
	{
		Pair<Integer,String> autoAnswer = handleAutoAnswer(question);if (autoAnswer != null) return autoAnswer;

		final List<String> questionList = beautifyQuestionList(question);
		final AtomicInteger answer = new AtomicInteger(AbstractOracle.USER_WAITINGFORSELECTION);

		try {
			SwingUtilities.invokeAndWait(new Runnable() {
				public void run() {
					final Object[] options = new Object[1 + moreOptions.length];
					
					final JList rejectElements = new JList(questionList
							.toArray());
					options[0] = "Accept";
					System.arraycopy(moreOptions, 0, options, 1,
							moreOptions.length);
					final JLabel label = new JLabel(
							"<html><font color=red>Click on the first non-accepting element below",
							SwingConstants.CENTER);
					jop = new JOptionPane(new Object[] { label,
							null, rejectElements },
							JOptionPane.QUESTION_MESSAGE,
							JOptionPane.YES_NO_CANCEL_OPTION, null, options,
							options[0]);
					dialog = new JDialog(parentFrame, "Valid input string?",
							false);
					dialog.setContentPane(jop);

					// the following chunk is partly from
					// http://java.sun.com/docs/books/tutorial/uiswing/components/dialog.html
					dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
					dialog.addWindowListener(new WindowAdapter() {
						public void windowClosing(@SuppressWarnings("unused") WindowEvent we) {
							jop.setValue(new Integer(JOptionPane.CLOSED_OPTION));// from http://java.sun.com/docs/books/tutorial/uiswing/components/examples/CustomDialog.java
						}
					});
					jop.addPropertyChangeListener(new PropertyChangeListener() {
						public void propertyChange(PropertyChangeEvent e) {
							String prop = e.getPropertyName();

							Object value = e.getNewValue();

							if (dialog.isVisible()
									&& e.getSource() == jop
									&& (prop.equals(JOptionPane.VALUE_PROPERTY))) {
								int i = 0;
								for (; i < options.length
										&& options[i] != value; ++i)
									;
								if (i == options.length)
									i = AbstractOracle.USER_CANCELLED;// nothing was chosen
								else
									i = AbstractOracle.USER_ACCEPTED - i;// to ensure that zero translates into USER_ACCEPTED and other choices into lower numbers

								// one of the choices was made, determine which
								// one and close the window
								answer.getAndSet(i);
								synchronized (answer) {
									answer.notifyAll();
								}

								dialog.setVisible(false);
								dialog.dispose();
							}
						}
					});
					rejectElements
							.addListSelectionListener(new ListSelectionListener() {

								public void valueChanged(ListSelectionEvent e) {
									if (dialog.isVisible()
											&& e.getSource() == rejectElements
											&& !e.getValueIsAdjusting()
											&& !rejectElements
													.isSelectionEmpty()) {
										answer.getAndSet(rejectElements
												.getLeadSelectionIndex());
										synchronized (answer) {
											answer.notifyAll();
										}

										dialog.setVisible(false);
										dialog.dispose();
									}
								}

							});
					dialog.pack();
					// rejectElements.setListData(questionList.toArray());
					dialog.setVisible(true);
				}
			});
			synchronized (answer) {
				while (answer.get() == AbstractOracle.USER_WAITINGFORSELECTION)
					answer.wait();// wait for a user to make a response
			}
		} catch (InvocationTargetException e) {
			e.printStackTrace();
			// if we cannot make a call, return a negative number - nothing do
			// not know what else to do about it.
		} catch (InterruptedException e) {

			// if we are interrupted, return a negative number - nothing do not
			// know what else to do about it.
		}
		if (answer.get() == AbstractOracle.USER_WAITINGFORSELECTION) // this one if an
														// exception was thrown
			answer.getAndSet(AbstractOracle.USER_CANCELLED);
		return new Pair<Integer,String>(answer.get(),null);
	}

}
