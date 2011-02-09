/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.learning;

import statechum.analysis.CodeCoverage.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.utils.UserData;
import java.io.BufferedReader;
import java.io.FileReader;
import statechum.JUConstants;

import statechum.analysis.learning.observers.AutoAnswers;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.SmtLearnerDecorator;
import statechum.analysis.learning.util.*;
import statechum.apps.ErlangQSMOracle;
import javax.swing.*;
import java.awt.event.*;
import java.util.*;
import java.io.IOException;
import statechum.analysis.CodeCoverage.CodeCoverageMapletNotFoundException;

/**
 *
 * @author ramsay
 */
public class ErlangOracleVisualiser extends PickNegativesVisualiser {

    /**
     * ID for serialization
     */
    private static final long serialVersionUID = -6159624335802103334L;
    public static final int CoverageMode = 1;
    public static final int CoverageCompareMode = 2;
    public static int mode = 1;

    @Override
    public void construct(Graph g) {
        super.construct(g);
        JMenuItem item = new JMenuItem("Coverage");
        item.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
                ErlangOracleVisualiser.mode = ErlangOracleVisualiser.CoverageMode;
            }
        });
        popupMenu.add(item);
        item = new JMenuItem("Coverage Comparison");
        item.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
                ErlangOracleVisualiser.mode = ErlangOracleVisualiser.CoverageCompareMode;
                ErlangOracleVisualiser.lastmap = null;
            }
        });
        popupMenu.add(item);
    }

    @Override
    public void startLearner(final ThreadStartedInterface whomToNotify) {
        learnerThread = new Thread(new Runnable() {

            @Override
            public void run() {
                if (conf.ifthenSequences != null) {
                    innerLearner = new ErlangOracleLearner(ErlangOracleVisualiser.this, conf);
                } else if (split != null) {
                    innerLearner = new Test_Orig_RPNIBlueFringeLearnerTestComponent(ErlangOracleVisualiser.this, conf.config);
                } else {
                    innerLearner = new ErlangOracleLearner(ErlangOracleVisualiser.this, conf);// at this point ifthenSequences will always be null.
                }
                innerLearner.addObserver(ErlangOracleVisualiser.this);
                Learner mainDecorator = new AutoAnswers(innerLearner);
                if (conf.labelDetails != null) {
                    mainDecorator = new SmtLearnerDecorator(mainDecorator, conf.labelDetails);
                }
                if (whomToNotify != null) {
                    whomToNotify.threadStarted();
                }
                LearnerGraph graph = mainDecorator.learnMachine(sPlus, sMinus);
                if (graph != null) {
                    DirectedSparseGraph learnt = graph.pathroutines.getGraph();
                    if (conf.config.isGenerateTextOutput()) {
                        OutputUtil.generateTextOutput(learnt, "textOutput.txt");
                    }
                    if (conf.config.isGenerateDotOutput()) {
                        OutputUtil.generateDotOutput(learnt, "dotOutput.dot");
                    }
                }
            }
        }, "ErlangOracle learner thread");
        learnerThread.start();

    }
    public static Object[] previousPicked = null;
    public static Object[] previousSelected = null;
    public static CodeCoverageMap lastmap = null;
    public static String lastTrace = null;

    @Override
    public void mouseReleased(@SuppressWarnings("unused") MouseEvent e) {
        if (mode == CoverageMode) {
            coverageSelection();
            if (lastmap != null) {
                CodeCoverageStringFrame frameS = new CodeCoverageStringFrame(traceColorise(lastmap, new CodeCoverageMap()), lastTrace);
                lastmap = null;
            }
        } else if (mode == CoverageCompareMode) {
            if ((lastmap == null) || (previousPicked == null)) {
                coverageSelection();
            } else {
                CodeCoverageMap map1 = lastmap;
                String trace1 = lastTrace;
                coverageSelection();
                // Display

                CodeCoverageMapCombination intersection = map1.intersection(lastmap);
                System.out.println("Intersection: " + intersection);
                CodeCoverageMap disjunction = lastmap.disjunction(map1);
                System.out.println("Disjunction: " + disjunction);
                String colorful = traceColorise(map1, lastmap);
                CodeCoverageStringFrame frameS = new CodeCoverageStringFrame(colorful, trace1 + " vs " + lastTrace);
                lastmap = null;
            }
        }
    }

    protected void coverageSelection() {
        Object[] vs = viewer.getPickedState().getPickedVertices().toArray();
        if ((ErlangOracleVisualiser.previousPicked == null) && (vs.length > 0)) {
            ErlangOracleVisualiser.previousPicked = vs;
        } else if (vs.length == 0) {
            if (ErlangOracleVisualiser.previousSelected != null) {
                for (Object v : ErlangOracleVisualiser.previousSelected) {
                    ((Vertex) v).setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.CLONE);
                }
            }
            ErlangOracleVisualiser.previousPicked = null;
        } else {
            if (ErlangOracleVisualiser.previousSelected != null) {
                for (Object v : ErlangOracleVisualiser.previousSelected) {
                    ((Vertex) v).setUserDatum(JUConstants.COLOUR, JUConstants.RED, UserData.CLONE);
                }
            }
            viewer.getPickedState().clearPickedVertices();
            for (Object v : ErlangOracleVisualiser.previousPicked) {
                ((Vertex) v).setUserDatum(JUConstants.COLOUR, JUConstants.BLUE, UserData.CLONE);
            }
            for (Object v : vs) {
                ((Vertex) v).setUserDatum(JUConstants.COLOUR, JUConstants.AMBER, UserData.CLONE);
            }
            Vertex start = (Vertex) ErlangOracleVisualiser.previousPicked[0];
            Vertex end = (Vertex) vs[0];
            String[] tracePair = getPrefixSuffixPair(start, end);
            String startTrace = tracePair[0];
            String endTrace = tracePair[1];
            if ((startTrace == null) || (endTrace == null)) {
                System.out.println("No traces found for these endpoints");
            } else {
                lastTrace = startTrace.toString() + "-" + endTrace.toString();
                lastmap = getCoverageMap(startTrace, endTrace);
                System.out.println("Trace suffix coverage map: " + lastmap.toString());
            }
            Object[] merge = new Object[ErlangOracleVisualiser.previousPicked.length + vs.length];
            System.arraycopy(ErlangOracleVisualiser.previousPicked, 0, merge, 0, ErlangOracleVisualiser.previousPicked.length);
            System.arraycopy(vs, 0, merge, ErlangOracleVisualiser.previousPicked.length, vs.length);
            ErlangOracleVisualiser.previousSelected = merge;
            ErlangOracleVisualiser.previousPicked = null;
        }
    }

    public static String[] getPrefixSuffixPair(Vertex start, Vertex end) {
        // We need to select a start and end trace such that the start trace is a prefix of the end trace...
        String startTrace = null;
        String endTrace = null;
        boolean found = false;
        for (Collection<String> st : ((LinkedList<LinkedList<String>>) start.getUserDatum(JUConstants.PATH))) {
            for (Collection<String> et : ((LinkedList<LinkedList<String>>) end.getUserDatum(JUConstants.PATH))) {
                if (isPrefix(st, et)) {
                    startTrace = toErlangList(st);
                    endTrace = toErlangList(et);
                    found = true;
                    break;
                }
            }
            if (found) {
                break;
            }
        }
        String[] result = new String[2];
        result[0] = startTrace;
        result[1] = endTrace;
        return result;
    }

    protected static boolean isPrefix(Collection<String> st, Collection<String> et) {
        Iterator<String> sit = st.iterator();
        Iterator<String> eit = et.iterator();
        while (sit.hasNext()) {
            if (eit.hasNext()) {
                if (!sit.next().equals(eit.next())) {
                    return false;
                }
            } else {
                return false;
            }
        }
        return true;
    }

    protected static String toErlangList(Collection<String> list) {
        String result = "[";
        for (String s : list) {
            if (!result.equals("[")) {
                result += ",";
            }
            result += s;
        }
        return result + "]";
    }

    public static CodeCoverageMap getCoverageMap(String prefixArg, String suffixArg) {
        // Trying to pass lists with spaces through bash goes horribly wrong so we need to conver [a, b] into [a,b]
        String prefix = prefixArg.replaceAll(", ", ",");
        String suffix = suffixArg.replaceAll(", ", ",");

        // Lookup coverage map in the coverage collection...
        CodeCoverageMap result = ErlangQSMOracle.coverageMaps.get(prefix + "-" + suffix);
        if (result != null) {
            return result;
        } else {
            // Calculate coverage data...
            CodeCoverageMap prefixMap = ErlangQSMOracle.coverageMaps.get("[]-" + prefix);
            CodeCoverageMap suffixMap = ErlangQSMOracle.coverageMaps.get("[]-" + suffix);
            if (prefixMap == null) {
                throw new RuntimeException("ZOMG!!! NO COVERAGE DATA FOR []-" + prefix + "!!");
            }
            if (suffixMap == null) {
                throw new RuntimeException("ZOMG!!! NO COVERAGE DATA FOR []-" + prefix + "!!");
            }
            return suffixMap.subtract(prefixMap);
        }
    }

    protected String traceColorise(CodeCoverageMap map1, CodeCoverageMap map2) {
        String result = "<html><body><pre>";

        String sourceFile = ErlangQSMOracle.ErlangFolder + "/" + ErlangQSMOracle.erlangModule + ".erl";
        try {
            BufferedReader input = new BufferedReader(new FileReader(sourceFile));
            int linenum = 0;
            String line = null;
            while ((line = input.readLine()) != null) {
                linenum++;
                int m1, m2;
                try {
                    m1 = map1.findLine(linenum);
                } catch (CodeCoverageMapletNotFoundException e) {
                    m1 = 0;
                }
                try {
                    m2 = map2.findLine(linenum);
                } catch (CodeCoverageMapletNotFoundException e) {
                    m2 = 0;
                }
                result += "<font color=";
                if ((m1 > 0) && (m2 == 0)) {
                    result += "\"red\"";
                } else if ((m1 == 0) && (m2 > 0)) {
                    result += "\"blue\"";
                } else if ((m1 > 0) && (m2 > 0)) {
                    result += "\"#ff00ff\"";
                } else {
                    result += "\"#aaaaaa\"";
                }
                result += ">" + linenum + ":&nbsp;" + line + "</font>\n";
            }
            input.close();
        } catch (IOException e) {
            System.out.println("Couldn't open " + sourceFile);
        }
        return result + "</pre></body></html>";
    }
}
