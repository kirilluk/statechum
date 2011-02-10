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

import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import statechum.JUConstants;
import statechum.Configuration;
import statechum.Pair;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.PathRoutines;
import statechum.model.testset.PTASequenceEngine;
import statechum.DeterministicDirectedSparseGraph.VertexID;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.CodeCoverage.CodeCoverageMap;

public abstract class RPNILearner extends Observable implements Learner {

    protected final Configuration config;
    /** The frame in relation to which to pop dialog boxes. */
    protected Frame parentFrame;

    public RPNILearner(Frame parent, Configuration c) {
        config = c;
        parentFrame = parent;
    }

    /** Retrieves the configuration used by this learner. */
    public Configuration getConfig() {
        return config;
    }

    /** Initialises this learner. */
    abstract
    @Override
    public LearnerGraph init(Collection<List<String>> plus, Collection<List<String>> minus);

    /** Initialises this learner. */
    abstract
    @Override
    public LearnerGraph init(PTASequenceEngine en, int plus, int minus);

    public
    @Override
    LearnerGraph learnMachine(PTASequenceEngine en, int plusSize, int minusSize) {
        init(en, plusSize, minusSize);
        return learnMachine();
    }

    public
    @Override
    LearnerGraph learnMachine(Collection<List<String>> plus, Collection<List<String>> minus) {
        topLevelListener.init(plus, minus);
        return learnMachine();
    }
    protected Learner topLevelListener = this;

    /** Sets the highest listener to receive notification calls;
     * it is expected that listeners will propagate calls down the chain
     * so that this learner eventually gets to execute its own methods.
     *
     * @param top new top of the stack of listeners.
     */
    public
    @Override
    void setTopLevelListener(Learner top) {
        topLevelListener = top;
    }

    /** Given a score, we need to determine whether to ask questions. This depends on a number
     * of configuration parameters.
     *
     * @param score score we are looking at
     * @return whether any questions should be asked or we could just proceed with merging.
     */
    protected boolean shouldAskQuestions(int score) {
        if (!config.getAskQuestions()) {
            return false;
        }

        if (config.getCertaintyThreshold() >= 0 && score > config.getCertaintyThreshold()) {
            return false;
        }

        if (score < config.getMinCertaintyThreshold()) {
            return false;
        }

        return true;
    }

    /** Updates listeners only if this object has been modified and debug mode is on, by calling
     * <pre>
     * setChanged()
     * </pre>
     * @param g the graph to display in the associated view
     * @param hardFacts the graph from which the current one was built
     */
    public void updateGraph(LearnerGraph g, LearnerGraph hardFacts) {
        setChanged();
        if (config.getDebugMode()) {
            DirectedSparseGraph gr = g.pathroutines.getGraph();
            PathRoutines.convertPairAssociationsToTransitions(gr, g, g.config);
            Map<VertexID, Collection<VertexID>> mergedToHard = g.getCache().getMergedToHardFacts();
            if (hardFacts != null && mergedToHard != null) {
                Map<CmpVertex, LinkedList<String>> vertToPath = hardFacts.pathroutines.computeShortPathsToAllStates();
                for (Object vert : gr.getVertices()) {
                    if (vert instanceof DeterministicVertex) {
                        DeterministicVertex v = (DeterministicVertex) vert;
                        LinkedList<CodeCoverageMap> coverage = new LinkedList<CodeCoverageMap>();
                        Collection<LinkedList<String>> allPrefixTraces = new LinkedList<LinkedList<String>>();
                        for (VertexID hard : mergedToHard.get(v.getID())) {
                            LinkedList<String> path = vertToPath.get(hardFacts.findVertex(hard));
                            allPrefixTraces.add(path);
                            // Walk the hardFacts from here to determine all possible paths from this state, then determine their code coverage
                            CmpVertex hardv = hardFacts.findVertex(hard);
                            Collection<LinkedList<String>> paths = getPaths(path, hardv, hardFacts);
                            for (LinkedList<String> p : paths) {
                                //System.out.println("Generating coverage for " + path + "-" + p);
                                Collection<LinkedList<String>> onlyPath = new LinkedList<LinkedList<String>>();
                                onlyPath.add(p);
                                String[] prefixSuffix = ErlangOracleVisualiser.getPrefixSuffixPair(allPrefixTraces, onlyPath);
                                CodeCoverageMap map = ErlangOracleVisualiser.getCoverageMap(prefixSuffix[0], prefixSuffix[1]);
                                //System.out.println("Generated coverage " + prefixSuffix[0] + "-" + prefixSuffix[1] + ": " + map.toString());
                                coverage.add(map);
                            }
                        }
                        v.addUserDatum(JUConstants.PATH, allPrefixTraces, UserData.SHARED);

                        v.addUserDatum(JUConstants.COVERAGE, coverage, UserData.SHARED);

                    }
                }

            }
            notifyObservers(gr);
        }
    }

    protected Collection<LinkedList<String>> getPaths(LinkedList<String> prefix, CmpVertex v, LearnerGraph hardFacts) {
        Collection<LinkedList<String>> result = new LinkedList<LinkedList<String>>();

        Map<String, CmpVertex> edges = hardFacts.getTransitionMatrix().get(v);
        if (edges.isEmpty()) {
            result.add(prefix);
        } else {
            for (Map.Entry<String, CmpVertex> e : edges.entrySet()) {
                LinkedList<String> newPath = (LinkedList<String>) prefix.clone();
                newPath.add(e.getKey());
                result.addAll(getPaths(newPath, e.getValue(), hardFacts));
            }
        }

        return result;
    }
    final String questionPrefix = "";

    protected List<String> getShortenedQuestion(List<String> question) {
        List<String> questionList = new LinkedList<String>();
        assert (question.size() >= 1);
        int counter = 1;
        Iterator<String> questionIter = question.iterator();
        String lastQuestion = questionIter.next();

        while (questionIter.hasNext()) {
            String current = questionIter.next();
            if (current.equals(lastQuestion)) {
                counter++;
            } else {
                questionList.add(lastQuestion.concat((counter > 1) ? "(*" + counter + ")" : ""));// in the string case, I could add "\n" at the end
                counter = 1;
                lastQuestion = current;
            }
        }
        questionList.add(lastQuestion.concat((counter > 1) ? "(*" + counter + ")" : ""));
        return questionList;
    }

    protected List<String> beautifyQuestionList(List<String> question) {
        List<String> questionList = new LinkedList<String>();
        int i = 0;
        for (String q : question) {
            questionList.add(questionPrefix + "(" + i++ + ") " + q);
        }

        return questionList;
    }
    /** The dialog to be displayed to a user with questions to select.
     * This field should not really be public, but since different packages refer to it,
     * I chose to make it public for the time being.
     */
    public JDialog dialog = null;
    /** the option pane. */
    protected JOptionPane jop = null;

    /** Cancels a dialog, if present. With no dialog, learner thread will terminate within a reasonable amount of time.
     */
    public void terminateUserDialogueFrame() {
        assert (SwingUtilities.isEventDispatchThread());
        if (dialog != null && jop != null && dialog.isVisible()) {
            jop.setValue(new Integer(
                    JOptionPane.CLOSED_OPTION));// from http://java.sun.com/docs/books/tutorial/uiswing/components/examples/CustomDialog.java		}
        }		// The setting of the option above corresponds to hitting "close" or "ESC" on the dialogue,
        // which is interpreted by the learner as a request to terminate learning, hence
        // the learner stops and the corresponding thread terminates. For this reason,
        // it is appropriate to issue a .join() on the learner thread.
    }
    public final static String QUESTION_AUTO = "<auto>";
    public final static String QUESTION_SPIN = "<spin>";
    public final static String QUESTION_USER = "<USER>";

    @Override
    public void Restart(@SuppressWarnings("unused") RestartLearningEnum mode) {// this method is used to let observers know what is going on, the actual restarts are handled by the main learner routine.
    }

    /** Where we expect a specific value in order to proceed without a restart, the answer is
     * highlighted in the user-displayed dialog box.
     *
     * @param str string to augment
     * @return resulting value
     */
    static String addAnnotationExpected(String str) {
        if (str.length() < 1) {
            return str;
        }

        return "<html>" + str + " <font color=blue>&nbsp;&larr;";
    }

    /** Displays a tentative graph and asks user a supplied question.
     * Options are to be shown as choices in addition to yes/element_not_accepted.
     */
    @Override
    public Pair<Integer, String> CheckWithEndUser(@SuppressWarnings("unused") LearnerGraph model, final List<String> question, final int expectedForNoRestart,
            final List<Boolean> consistentFacts, final Object[] moreOptions) {


        final List<String> questionList = beautifyQuestionList(question);
        final AtomicInteger answer = new AtomicInteger(AbstractOracle.USER_WAITINGFORSELECTION);
        Integer outcome = PathRoutines.identifyTheOnlyChoice(consistentFacts);
        if (outcome != null) {
            answer.getAndSet(outcome);
        } else {
            try {
                SwingUtilities.invokeAndWait(new Runnable() {

                    public
                    @Override
                    void run() {
                        final Object[] options = new Object[1 + moreOptions.length];

                        // A click on an element means a reject for that element and accept to all
                        // earlier elements, hence we have to disable all those which should not
                        // be rejected. The fact that we do only consider prefix-closed questions
                        // implies that a whole prefix will be accept-only.
                        Iterator<String> inputIter = questionList.iterator();
                        Iterator<Boolean> factsIter = consistentFacts == null ? null : consistentFacts.iterator();
                        List<String> listElements = new ArrayList<String>(questionList.size());
                        int inputCounter = 0;
                        while (inputIter.hasNext()) {
                            String currentElement = inputIter.next();
                            Boolean fact = factsIter == null ? null : factsIter.next();
                            String elementHtml = null;
                            if (fact != null && fact.booleanValue()) {
                                elementHtml = "<html><font color=gray>" + currentElement;
                            } else {
                                elementHtml = "<html><font color=green>" + currentElement;
                            }

                            if (inputCounter == expectedForNoRestart) {
                                elementHtml = addAnnotationExpected(elementHtml);
                            }
                            ++inputCounter;
                            listElements.add(elementHtml);
                        }
                        final JList javaList = new JList(listElements.toArray());
                        String optionZero = null;
                        Boolean lastFact = consistentFacts.get(consistentFacts.size() - 1);
                        if (lastFact != null && !lastFact.booleanValue()) // last element has to be a reject
                        {
                            optionZero = "<html><font color=grey>cannot accept";
                        } else {
                            optionZero = "<html><font color=green>Accept";
                        }

                        if (expectedForNoRestart == AbstractOracle.USER_ACCEPTED) {
                            optionZero = addAnnotationExpected(optionZero);
                            assert lastFact == null || lastFact.booleanValue();
                        }
                        options[0] = optionZero;
                        System.arraycopy(moreOptions, 0, options, 1, moreOptions.length);
                        final JLabel label = new JLabel("<html><font color=red>Click on the first non-accepting element below", SwingConstants.CENTER);
                        jop = new JOptionPane(new Object[]{label, javaList},
                                JOptionPane.QUESTION_MESSAGE, JOptionPane.YES_NO_CANCEL_OPTION, null, options, options[0]);
                        dialog = new JDialog(parentFrame, "Valid input string?", false);
                        dialog.setContentPane(jop);

                        // the following chunk is partly from http://java.sun.com/docs/books/tutorial/uiswing/components/dialog.html
                        dialog.setDefaultCloseOperation(
                                WindowConstants.DO_NOTHING_ON_CLOSE);
                        dialog.addWindowListener(new WindowAdapter() {

                            @Override
                            public void windowClosing(@SuppressWarnings("unused") WindowEvent we) {
                                jop.setValue(new Integer(
                                        JOptionPane.CLOSED_OPTION));// from http://java.sun.com/docs/books/tutorial/uiswing/components/examples/CustomDialog.java
                            }
                        });
                        jop.addPropertyChangeListener(new PropertyChangeListener() {

                            public
                            @Override
                            void propertyChange(PropertyChangeEvent e) {
                                String prop = e.getPropertyName();

                                Object value = e.getNewValue();

                                if (dialog.isVisible() && e.getSource() == jop
                                        && (prop.equals(JOptionPane.VALUE_PROPERTY))) {
                                    boolean clickValid = true;
                                    int i = 0;
                                    for (; i < options.length && options[i] != value; ++i);
                                    if (i == options.length) {
                                        i = AbstractOracle.USER_CANCELLED;// nothing was chosen
                                    } else {
                                        if (i == 0) {
                                            Boolean fact = consistentFacts.get(consistentFacts.size() - 1);
                                            clickValid = (fact == null || fact.booleanValue());
                                        }
                                        i = AbstractOracle.USER_ACCEPTED - i; // to ensure that zero translates into USER_ACCEPTED and other choices into lower numbers
                                    }

                                    if (clickValid) {
                                        // one of the valid choices was made, record which one and close the window
                                        answer.getAndSet(i);
                                        synchronized (answer) {
                                            answer.notifyAll();
                                        }

                                        dialog.setVisible(false);
                                        dialog.dispose();
                                    }
                                }
                            }
                        });
                        javaList.addListSelectionListener(new ListSelectionListener() {

                            public
                            @Override
                            void valueChanged(ListSelectionEvent e) {
                                if (dialog.isVisible() && e.getSource() == javaList
                                        && !e.getValueIsAdjusting() && !javaList.isSelectionEmpty()) {
                                    int position = javaList.getLeadSelectionIndex();
                                    Boolean fact = consistentFacts.get(position);
                                    if (fact == null || !fact.booleanValue()) {
                                        answer.getAndSet(position);
                                        synchronized (answer) {
                                            answer.notifyAll();
                                        }

                                        dialog.setVisible(false);
                                        dialog.dispose();
                                    }
                                }
                            }
                        });
                        dialog.pack();
                        //rejectElements.setListData(questionList.toArray());
                        dialog.setVisible(true);
                    }
                });
                synchronized (answer) {
                    while (answer.get() == AbstractOracle.USER_WAITINGFORSELECTION) {
                        answer.wait();// wait for a user to make a response
                    }
                }
            } catch (InvocationTargetException e) {
                //e.printStackTrace();
                // if we cannot make a call, return a negative number - nothing do not know what else to do about it.
            } catch (InterruptedException e) {
                // if we are interrupted, return a negative number - nothing do not know what else to do about it.
            }
        }
        if (answer.get() == AbstractOracle.USER_WAITINGFORSELECTION) // this one if an exception was thrown
        {
            answer.getAndSet(AbstractOracle.USER_CANCELLED);
        }
        return new Pair<Integer, String>(answer.get(), null);
    }
}
