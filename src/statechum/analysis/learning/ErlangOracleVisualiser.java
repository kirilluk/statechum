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
import statechum.Pair;
import statechum.Trace;
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
    public static final int AllSuffixesCoverageMode = 3;
    public static final int AllSuffixesCompareMode = 4;
    public static final int NoCoverage = 5;
    public static int mode = 5;

    @Override
    public void construct(Graph g,LayoutOptions options) {
        super.construct(g,options);
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
        item = new JMenuItem("All suffixes coverage");
        item.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
                ErlangOracleVisualiser.mode = ErlangOracleVisualiser.AllSuffixesCoverageMode;
                ErlangOracleVisualiser.lastmap = null;
            }
        });
        popupMenu.add(item);
        item = new JMenuItem("All suffixes coverage comparison");
        item.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
                ErlangOracleVisualiser.mode = ErlangOracleVisualiser.AllSuffixesCompareMode;
                ErlangOracleVisualiser.lastmap = null;
            }
        });
        popupMenu.add(item);
        item = new JMenuItem("No coverage display");
        item.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
                ErlangOracleVisualiser.mode = ErlangOracleVisualiser.NoCoverage;
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
        if (e.getButton() != MouseEvent.BUTTON1) {
            return;
        }
        if (mode == NoCoverage) {
            return;
        }
        if (mode == AllSuffixesCoverageMode) {
            Object[] vs = viewer.getPickedState().getPickedVertices().toArray();
            if (vs.length > 0) {
                LinkedList<CodeCoverageMap> allMaps = (LinkedList<CodeCoverageMap>) ((Vertex) vs[0]).getUserDatum(JUConstants.COVERAGE);
                if (allMaps == null) {
                    System.out.println("No coverage data");
                } else {
                    CodeCoverageMap sum = new CodeCoverageMap();
                    for (CodeCoverageMap m : allMaps) {
                        // This is nice and readable....
                        sum = sum.sum(m);
                    }
                    new CodeCoverageStringFrame(traceColorise(sum, new CodeCoverageMap(), false), ((Vertex) vs[0]).getUserDatum(JUConstants.LABEL).toString());
                }
            }
        } else if (mode == AllSuffixesCompareMode) {
            Object[] vs = viewer.getPickedState().getPickedVertices().toArray();
            if (vs.length > 0) {
                if (previousPicked == null) {
                    previousPicked = vs;
                } else {
                    LinkedList<CodeCoverageMap> previousMaps = (LinkedList<CodeCoverageMap>) ((Vertex) previousPicked[0]).getUserDatum(JUConstants.COVERAGE);
                    CodeCoverageMap previousSum = new CodeCoverageMap();
                    for (CodeCoverageMap m : previousMaps) {
                        // This is nice and readable....
                        previousSum = previousSum.sum(m);
                    }
                    LinkedList<CodeCoverageMap> thisMaps = (LinkedList<CodeCoverageMap>) ((Vertex) vs[0]).getUserDatum(JUConstants.COVERAGE);
                    CodeCoverageMap thisSum = new CodeCoverageMap();
                    for (CodeCoverageMap m : thisMaps) {
                        // This is nice and readable....
                        thisSum = thisSum.sum(m);
                    }
                    String previousLabel = (String) ((Vertex) previousPicked[0]).getUserDatum(JUConstants.LABEL).toString();
                    String thisLabel = (String) ((Vertex) vs[0]).getUserDatum(JUConstants.LABEL).toString();
                    //System.out.println(previousSum.toString() + " vs " + thisSum.toString());
                    new CodeCoverageStringFrame(traceColorise(previousSum, thisSum, false), previousLabel + " vs " + thisLabel);
                    previousPicked = null;
                }
            } else {
                previousPicked = null;
            }
        } else if (mode == CoverageMode) {
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
            Pair<Trace, Trace> tracePair = getPrefixSuffixPair(start, end);
            Trace startTrace = tracePair.firstElem;
            Trace endTrace = tracePair.secondElem;
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

    public static Pair<Trace, Trace> getPrefixSuffixPair(Vertex start, Vertex end) {
        return getPrefixSuffixPair(((Collection<Trace>) start.getUserDatum(JUConstants.PATH)), ((Collection<Trace>) end.getUserDatum(JUConstants.PATH)));
    }

    public static Pair<Trace, Trace> getPrefixSuffixPair(Collection<Trace> start, Vertex end) {
        return getPrefixSuffixPair(start, ((Collection<Trace>) end.getUserDatum(JUConstants.PATH)));
    }

    public static Pair<Trace, Trace> getPrefixSuffixPair(Vertex start, Collection<Trace> end) {
        return getPrefixSuffixPair((Collection<Trace>) start.getUserDatum(JUConstants.PATH), end);
    }

    public static Pair<Trace, Trace> getPrefixSuffixPair(Collection<Trace> start, Collection<Trace> end) {
        // We need to select a start and end trace such that the start trace is a prefix of the end trace...
        Trace startTrace = null;
        Trace endTrace = null;
        boolean found = false;
        for (Trace st : start) {
            for (Trace et : end) {
                if (st.isPrefix(et)) {
                    startTrace = st;
                    endTrace = et;
                    found = true;
                    break;
                }
            }
            if (found) {
                break;
            }
        }
        Pair<Trace, Trace> result = new Pair<Trace, Trace>(startTrace, endTrace);
        return result;
    }

    public static String toErlangList(Collection<String> list) {
        String result = "[";
        for (String s : list) {
            if (!result.equals("[")) {
                result += ",";
            }
            result += s;
        }
        return result + "]";
    }

    public static CodeCoverageMap getCoverageMap(Trace prefixArg, Trace suffixArg) {
        while (ErlangQSMOracle.coverageMapLock) {
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                ;
            }
        }
        ErlangQSMOracle.coverageMapLock = true;

        // Trying to pass lists with spaces through bash goes horribly wrong so we need to conver [a, b] into [a,b]
        Trace prefix = prefixArg.replaceAll(", ", ",");
        Trace suffix = suffixArg.replaceAll(", ", ",");

        // Lookup coverage map in the coverage collection...
        CodeCoverageMap result = ErlangQSMOracle.coverageMaps.get(new Pair<Trace, Trace>(prefix, suffix));
        if (result == null) {
            // Calculate coverage data...
            CodeCoverageMap prefixMap = ErlangQSMOracle.coverageMaps.get(new Pair<Trace, Trace>(new Trace(), prefix));
            CodeCoverageMap suffixMap = ErlangQSMOracle.coverageMaps.get(new Pair<Trace, Trace>(new Trace(), suffix));
            if (prefixMap == null) {
                // Try with wildcard matching...
                for (Pair<Trace, Trace> s : ErlangQSMOracle.coverageMaps.keySet()) {
                    //System.out.print("Seeking \"" + prefix + "\" in \"" + s.secondElem + "\" == " + (prefix.equals(s.secondElem)));
                    if (Trace.matchWithWildcard(prefix, s.secondElem)) {
                        prefixMap = ErlangQSMOracle.coverageMaps.get(s);
                        //System.out.println(" HIT! " + prefixMap);
                        break;
                    }
                    //System.out.println("");
                }
                if (prefixMap == null) {
                    prefixMap = new CodeCoverageMap();
//                    throw new RuntimeException("ZOMG!!! NO COVERAGE DATA FOR " + prefix + "!!");
                }
            }
            if (suffixMap == null) {
                // Try with wildcard matching...
                for (Pair<Trace, Trace> s : ErlangQSMOracle.coverageMaps.keySet()) {
                    //System.out.println("Seeking \"" + suffix + "\" in \"" + s.secondElem + "\"");
                    if (Trace.matchWithWildcard(suffix, s.secondElem)) {
                        //System.out.println("HIT! " + s.secondElem + " ==>> " + ErlangQSMOracle.coverageMaps.get(s));
                        suffixMap = ErlangQSMOracle.coverageMaps.get(s);
                    }
                }
                if (suffixMap == null) {
                    suffixMap = new CodeCoverageMap();
//                    throw new RuntimeException("ZOMG!!! NO COVERAGE DATA FOR " + suffix + "!!");
                }
            }
            result = suffixMap.subtract(prefixMap);
        }
        ErlangQSMOracle.coverageMapLock = false;
        return result;
    }

    protected String traceColorise(CodeCoverageMap map1, CodeCoverageMap map2) {
        return traceColorise(map1, map2, false);
    }

    protected String traceColorise(CodeCoverageMap map1, CodeCoverageMap map2, boolean hide) {
        String result = "<html><body>";
        String sourceFile = "";
        try {
            for (String m : ErlangQSMOracle.erlangModules) {
                result += "<h2>" + m + "</h2>";
                result += "<pre>";
                sourceFile = ErlangQSMOracle.ErlangFolder + "/" + m + ".erl";
                BufferedReader input = new BufferedReader(new FileReader(sourceFile));
                int linenum = 0;
                String line = null;
                String prevLine = "";
                boolean spacing = false;
                while ((line = input.readLine()) != null) {
                    linenum++;
                    int m1, m2;
                    try {
                        m1 = map1.findLine(m + "." + linenum);
                    } catch (CodeCoverageMapletNotFoundException e) {
                        m1 = 0;
                    }
                    try {
                        m2 = map2.findLine(m + "." + linenum);
                    } catch (CodeCoverageMapletNotFoundException e) {
                        m2 = 0;
                    }
                    line = line.replace("<", "&lt;");
                    line = line.replace(">", "&gt;");
                    if ((m1 == 0) && (m2 == 0)) {
                        if (hide) {
                            if (!spacing) {
                                result += "<font color=\"#aaaaaa\">" + linenum + ":&nbsp;" + line + "</font>\n";
                                result += "<font color=\"#aaaaaa\">...</font>\n";
                                spacing = true;
                            }
                        } else {
                            result += "<font color=\"#aaaaaa\">" + linenum + ":&nbsp;" + line + "</font>\n";
                        }
                    } else {
                        if (spacing) {
                            result += "<font color=\"#aaaaaa\">...</font>\n";
                            result += "<font color=\"#aaaaaa\">" + (linenum - 1) + ":&nbsp;" + prevLine + "</font>\n";

                        }
                        spacing = false;
                        result += "<font color=";
                        if ((m1 > 0) && (m2 == 0)) {
                            result += "\"red\"";
                        } else if ((m1 == 0) && (m2 > 0)) {
                            result += "\"blue\"";
                        } else {
                            result += "\"#ff00ff\"";
                        }
                        result += ">" + linenum + ":&nbsp;" + line + "</font>\n";
                    }
                    prevLine = line;
                }
                input.close();
                result += "</pre>";
            }
        } catch (IOException e) {
            System.out.println("Couldn't open " + sourceFile);
        }
        return result + "</body></html>";
    }
}
