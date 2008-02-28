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

import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.xmachine.model.testset.*;
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
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

public class BlueFringeSpinLearner extends
		RPNIBlueFringeLearnerTestComponentOpt {

	private Set<String> ltl;

	public BlueFringeSpinLearner(Frame parentFrame, Set<String> ltlFormulae) {
		super(parentFrame);
		ltl = ltlFormulae;
	}

	public DirectedSparseGraph learnMachine() {
		Map<Integer, AtomicInteger> whichScoresWereUsedForMerging = new HashMap<Integer, AtomicInteger>(), restartScoreDistribution = new HashMap<Integer, AtomicInteger>();
		Map<PairScore, Integer> scoresToIterations = new HashMap<PairScore, Integer>();
		Map<PairScore, Integer> restartsToIterations = new HashMap<PairScore, Integer>();
		LearnerGraph newPTA = scoreComputer;// no need to clone - this
													// is the job of
													// mergeAndDeterminize
													// anyway
		String pairsMerged = "";
		StringWriter report = new StringWriter();
		counterAccepted = 0;
		counterRejected = 0;
		counterRestarted = 0;
		counterEmptyQuestions = 0;
		report.write("\n[ PTA: " + scoreComputer.paths.getStatistics(false) + " ] ");
		setChanged();

		Stack<PairScore> possibleMerges = scoreComputer.pairscores.chooseStatePairs();
		int plusSize = sPlus.size(), minusSize = sMinus.size(), iterations = 0;
		while (!possibleMerges.isEmpty()) {

			iterations++;
			// populateScores(possibleMerges,possibleMergeScoreDistribution);
			PairScore pair = possibleMerges.pop();
			LearnerGraph temp = MergeStates.mergeAndDeterminize(
					scoreComputer, pair);
			setChanged();
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();
			if(score<klimit)
				continue;
			boolean restartLearning = false;// whether we need to rebuild a PTA
											// and restart learning.

			// System.out.println(Thread.currentThread()+ " "+pair + "
			// "+questions);
			
			if (!SpinUtil.check(temp.paths.getGraph(), ltl)) {
				List<String> counterexample = new LinkedList<String>();
				counterexample.addAll(SpinUtil.getCurrentCounterExample());
				newPTA.paths.augmentPTA(counterexample.subList(0, counterexample.size()-1), false);
				System.out.println(counterexample.subList(0, counterexample.size()-1));
				++minusSize;
				restartLearning = true;
			}
			if ((score < this.certaintyThreshold && score > minCertaintyThreshold)
					&& !restartLearning && askQuestions) {
				questions = ComputeQuestions.computeQS(pair, scoreComputer, temp);
				if (questions.isEmpty())
					++counterEmptyQuestions;
			}
			Iterator<List<String>> questionIt = questions.iterator();
			while (questionIt.hasNext() && !restartLearning) {

				List<String> question = questionIt.next();
				boolean accepted = pair.getQ().isAccept();
				int answer = checkWithSPIN(question);
				if(answer<0)
					answer = checkWithEndUser(scoreComputer.paths.getGraph(),
						question, new Object[] { "LTL"});
				this.questionCounter++;
				if (answer == USER_CANCELLED) {
					System.out.println("CANCELLED");
					return null;
				}

				CmpVertex tempVertex = temp.getVertex(question);
				if (tempVertex == null)
					System.out.println();

				if (answer == USER_ACCEPTED) {
					++counterAccepted;
					// sPlus.add(question);
					newPTA.paths.augmentPTA(question, true);
					++plusSize;
					// System.out.println(setByAuto+question.toString()+ "
					// <yes>");

					if (!tempVertex.isAccept()) {
						pairsMerged = pairsMerged
								+ "ABOUT TO RESTART due to acceptance of a reject vertex for a pair "
								+ pair + " ========\n";
						restartLearning = true;
						break;
					}
				} else if (answer >= 0) {// The sequence has been rejected by
											// a user
					assert answer < question.size();
					++counterRejected;
					LinkedList<String> subAnswer = new LinkedList<String>();
					subAnswer.addAll(question.subList(0, answer + 1));
					// sMinus.add(subAnswer);
					newPTA.paths.augmentPTA(subAnswer, false);
					++minusSize;// important: since vertex IDs is
					// only unique for each instance of ComputeStateScores, only
					// once
					// instance should ever receive calls to augmentPTA

					// System.out.println(setByAuto+question.toString()+ " <no>
					// at position "+answer+", element "+question.get(answer));
					if ((answer < question.size() - 1) || tempVertex.isAccept()) {
						assert accepted == true;
						pairsMerged = pairsMerged
								+ "ABOUT TO RESTART because accept vertex was rejected for a pair "
								+ pair + " ========\n";
						restartLearning = true;
						break;
					}
				} else if(answer==-4){
					String newLtl = JOptionPane.showInputDialog("New LTL formula:");
					if(newLtl.length() != 0){
						ltl.add(newLtl);
						restartLearning = true;
						break;
					}
				}
				else{
					System.out.println(answer);
					throw new IllegalArgumentException("unexpected user choice");
				}
			}

			if (restartLearning) {// restart learning
				// ComputeStateScores expected = createAugmentedPTA(sPlus,
				// sMinus);// KIRR: node labelling is done by createAugmentedPTA
				scoreComputer = newPTA;// no need to clone - this is the job of
										// mergeAndDeterminize anyway
				scoreComputer.clearColours();
				setChanged();
				++counterRestarted;
				pairsMerged = pairsMerged + "========== RESTART "
						+ counterRestarted + " ==========\n";
				AtomicInteger count = restartScoreDistribution.get(pair
						.getScore());
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
				pairsMerged = pairsMerged + pair + " questions: "
						+ questions.size() + "\n";

				// keep going with the existing model
				scoreComputer = temp;
				// now update the statistics
				AtomicInteger count = whichScoresWereUsedForMerging.get(pair
						.getScore());
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
		DirectedSparseGraph result = scoreComputer.paths.getGraph();
		result.addUserDatum(JUConstants.STATS, report.toString(),
				UserData.SHARED);
		updateGraph(result);
		return result;
	}
	
	protected int checkWithSPIN (List<String> question){
		int ret = -1;
		if(!SpinUtil.check(question, ltl)){
			List<String> counterExample = SpinUtil.getCurrentCounterExample();
			System.out.println(counterExample.subList(0, counterExample.size()));
			return counterExample.size()-1;
		}
		else
			return ret;
	}
	
	protected int checkWithEndUser(DirectedSparseGraph model,
			List<String> question, final Object[] moreOptions) {
		if (ans != null) {
			int AutoAnswer = processAnswer(question);
			if (AutoAnswer != USER_CANCELLED) {
				setByAuto = QUESTION_AUTO;
				return AutoAnswer;
			} else
				setByAuto = "";
		}
		updateGraph(model);
		final List<String> questionList = beautifyQuestionList(question);
		final AtomicInteger answer = new AtomicInteger(USER_WAITINGFORSELECTION);

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
							JLabel.CENTER);
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
					dialog
							.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
					dialog.addWindowListener(new WindowAdapter() {
						public void windowClosing(WindowEvent we) {
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
									i = USER_CANCELLED;// nothing was chosen
								else
									i = USER_ACCEPTED - i;// to ensure that zero translates into USER_ACCEPTED and other choices into lower numbers

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
				while (answer.get() == USER_WAITINGFORSELECTION)
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
		if (answer.get() == USER_WAITINGFORSELECTION) // this one if an
														// exception was thrown
			answer.getAndSet(USER_CANCELLED);
		return answer.get();
	}

}
