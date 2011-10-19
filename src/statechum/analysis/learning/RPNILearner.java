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

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Pair;
import statechum.StringLabel;
import statechum.analysis.learning.Visualiser.LayoutOptions;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.PathRoutines;
import statechum.model.testset.PTASequenceEngine;


import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;
import statechum.Trace;
import statechum.analysis.CodeCoverage.CodeCoverageMap;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.OTPBehaviour;

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
    public LearnerGraph init(Collection<List<Label>> plus, Collection<List<Label>> minus);

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
    LearnerGraph learnMachine(Collection<List<Label>> plus, Collection<List<Label>> minus) {
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

    /** Pretty-prints a supplied question. Useful for questions with Erlang labels which 
     * contain functions they are associated with. 
     */ 
    public static String questionToString(List<? extends Label> question)
    {
		StringBuffer questionString = new StringBuffer();questionString.append('[');
		boolean first = true;
		for(Label lbl:question)
		{
			if (!first) questionString.append(',');else first = false;
			if (lbl instanceof StringLabel)
				questionString.append(lbl.toErlangTerm());
			else
				if (lbl instanceof ErlangLabel)
					questionString.append(OTPBehaviour.convertModToErl(lbl).toErlangTerm());
				else
					throw new IllegalArgumentException("unknown type of label "+lbl.getClass());
		}
		questionString.append(']');
		return questionString.toString();
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
            Map<VertexID, Collection<VertexID>> mergedToHard = g.getCache().getMergedToHardFacts();
            if (hardFacts != null && mergedToHard != null) {
                Map<CmpVertex, LinkedList<Label>> vertToPath = hardFacts.pathroutines.computeShortPathsToAllStates();
                Map<CmpVertex,CachedData.ErlangCoverageData> vertexToCoverage = new TreeMap<CmpVertex,CachedData.ErlangCoverageData>();
                for(CmpVertex v:g.transitionMatrix.keySet())
                {
                	CachedData.ErlangCoverageData erlCoverage = new CachedData.ErlangCoverageData();
                	erlCoverage.coverage = new LinkedList<CodeCoverageMap>();
                    Collection<Trace> allPrefixTraces = new LinkedList<Trace>();
                    Collection<VertexID> verticesInHardFacts=mergedToHard.get(v.getID());
                    if (verticesInHardFacts != null)
                    {
                    	// this one will be null if 
                        for (VertexID hard : verticesInHardFacts) {
                            Trace path = new Trace(vertToPath.get(hardFacts.findVertex(hard)));
                            allPrefixTraces.add(path);

                            // Walk the hardFacts from here to determine all possible paths from this state, then determine their code coverage
                            CmpVertex hardv = hardFacts.findVertex(hard);
                            Collection<Trace> paths = getPaths(path, hardv, hardFacts);
                            for (Trace p : paths) {
                                //System.out.println("Generating coverage for " + path + "-" + p);
                                Collection<Trace> onlyPath = new LinkedList<Trace>();
                                onlyPath.add(p);
                                Pair<Trace, Trace> prefixSuffix = ErlangOracleVisualiser.getPrefixSuffixPair(allPrefixTraces, onlyPath);
                                CodeCoverageMap map = ErlangOracleVisualiser.getCoverageMap(prefixSuffix.firstElem, prefixSuffix.secondElem);
                                //System.out.println("Generated coverage " + prefixSuffix[0] + "-" + prefixSuffix[1] + ": " + map.toString());
                                erlCoverage.coverage.add(map);
                            }
                        }
                    }
                    
                    vertexToCoverage.put(v, erlCoverage);
                }
                g.getCache().setErlangCoverage(vertexToCoverage);
            }
        	notifyObservers(g);
        }
    }
    
    /* This one is actually a specialised version of computeShortPathsToAllStates()
     */
    protected Collection<Trace> getPaths(Trace prefix, CmpVertex v, LearnerGraph hardFacts) {
        Collection<Trace> result = new LinkedList<Trace>();

        Map<Label, CmpVertex> edges = hardFacts.getTransitionMatrix().get(v);
        if (edges.isEmpty()) {
            result.add(prefix);
        } else {
            for (Map.Entry<Label, CmpVertex> e : edges.entrySet()) {
                Trace newPath = (Trace) prefix.clone();
                newPath.add(e.getKey());
                result.addAll(getPaths(newPath, e.getValue(), hardFacts));
            }
        }

        return result;
    }
    
    final String questionPrefix = "";

    protected List<String> beautifyQuestionList(List<Label> question) {
        List<String> questionList = new LinkedList<String>();
        int i = 0;
        for (Label q : question) {
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
    public final static String QUESTION_IGNORE = "<ignore>";
    public final static String QUESTION_INCOMPATIBLE = "<incompatible>";
    public final static String QUESTION_NEWTRACE = "<newtrace>";
    
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
    public Pair<Integer, String> CheckWithEndUser(@SuppressWarnings("unused") LearnerGraph model, 
    		final List<Label> question, 
    		final int expectedForNoRestart,
            final List<Boolean> consistentFacts, 
            @SuppressWarnings("unused") final PairScore pairBeingMerged, 
            final Object[] moreOptions) 
    {
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
