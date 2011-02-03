/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.learning;

import analysis.Erlang.ErlangCoverageFileFrame;
import analysis.Erlang.ErlangCoverageMap;
import analysis.Erlang.ErlangCoverageMapCombination;
import analysis.Erlang.ErlangCoverageMaplet;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.utils.UserData;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.InputStreamReader;
import statechum.JUConstants;

import statechum.analysis.learning.observers.AutoAnswers;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.SmtLearnerDecorator;
import statechum.analysis.learning.util.*;
import statechum.apps.ErlangQSMOracle;
import javax.swing.*;
import java.awt.event.*;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 *
 * @author ramsay
 */
public class ErlangOracleVisualiser extends PickNegativesVisualiser {

    public static final int CoverageMode = 1;
    public static final int CoverageCompareMode = 2;
    public static int mode = 1;

    @Override
    public void construct(Graph g) {
        super.construct(g);
        JMenuItem item = new JMenuItem("Coverage");
        item.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                ErlangOracleVisualiser.mode = ErlangOracleVisualiser.CoverageMode;
            }
        });
        popupMenu.add(item);
        item = new JMenuItem("Coverage Comparison");
        item.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
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
    public static ErlangCoverageMap lastmap = null;
    public static File lastCoverFile = null;

    @Override
    public void mouseReleased(@SuppressWarnings("unused") MouseEvent e) {
        if (mode == CoverageMode) {
            coverageSelection();
            try {
                if(lastCoverFile != null) {
                    ErlangCoverageFileFrame frame1 = new ErlangCoverageFileFrame("file://" + lastCoverFile.getCanonicalFile(), "Coverage");
                    lastCoverFile = null;
                }
            } catch (IOException f) {
                ;
            }
        } else if (mode == CoverageCompareMode) {
            if ((lastmap == null) || (previousPicked == null)) {
                coverageSelection();
            } else {
                ErlangCoverageMap map1 = lastmap;
                File file1 = lastCoverFile;
                coverageSelection();
                // Display

                ErlangCoverageMapCombination intersection = map1.intersection(lastmap);
                System.out.println("Intersection: " + intersection);
                try {
                    ErlangCoverageFileFrame frame1 = new ErlangCoverageFileFrame("file://" + file1.getCanonicalFile(), "First");
                    ErlangCoverageFileFrame frame2 = new ErlangCoverageFileFrame("file://" + lastCoverFile.getCanonicalFile(), "Second");
                } catch (IOException f) {
                    ;
                }
                lastmap = null;
                lastCoverFile = null;
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
            lastmap = execErlangSuffixCoverageMapFinder(start.getUserDatum(JUConstants.PATH).toString(), end.getUserDatum(JUConstants.PATH).toString());
            System.out.println("Trace suffix coverage map: " + lastmap.toString());
            Object[] merge = new Object[ErlangOracleVisualiser.previousPicked.length + vs.length];
            System.arraycopy(ErlangOracleVisualiser.previousPicked, 0, merge, 0, ErlangOracleVisualiser.previousPicked.length);
            System.arraycopy(vs, 0, merge, ErlangOracleVisualiser.previousPicked.length, vs.length);
            ErlangOracleVisualiser.previousSelected = merge;
            ErlangOracleVisualiser.previousPicked = null;
        }
    }

    protected ErlangCoverageMap execErlangSuffixCoverageMapFinder(String prefix, String suffix) {
        ErlangCoverageMap result = new ErlangCoverageMap();
        // Trying to pass lists with spaces through bash goes horribly wrong so we need to conver [a, b] into [a,b]
        prefix = prefix.replaceAll(", ", ",");
        suffix = suffix.replaceAll(", ", ",");
        try {
            String mapfile = "map" + Integer.toString(prefix.hashCode()).substring(0, 4) + Integer.toString(suffix.hashCode()).substring(0, 4) + ".map";
            String erlCmd = "./erlcovermap.sh " + ErlangQSMOracle.erlangModule + " " + ErlangQSMOracle.erlangFunction + " " + prefix + " " + suffix + " " + mapfile;
            System.out.println("Using coverage results file: " + mapfile);
            //System.out.println("Running " + erlCmd + " in folder " + ErlangQSMOracle.ErlangFolder);
            Process p = Runtime.getRuntime().exec(erlCmd, null, new File(ErlangQSMOracle.ErlangFolder));
            BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
            //System.out.println("Process output:");
            String line;
            while ((line = input.readLine()) != null) {
                System.out.println(line);
            }
            input.close();

            p.waitFor();

            //System.out.println("Traces file:");
            File f = new File(ErlangQSMOracle.ErlangFolder + "/" + mapfile);
            try {
            input = new BufferedReader(new FileReader(f));

            while ((line = input.readLine()) != null) {
                //System.out.println(line);
                String[] elems = line.split("\\{");
                for (String e : elems) {
                    if (!e.contains("[")) {
                        e = e.replaceAll("\\}.*", "");
                        String[] vals = e.split(",");
                        result.map.add(new ErlangCoverageMaplet(Integer.parseInt(vals[0]), Integer.parseInt(vals[1])));
                    }
                }
            }
            input.close();
            f.delete();
            lastCoverFile = new File(ErlangQSMOracle.ErlangFolder + "/" + mapfile + ".html");
            } catch (FileNotFoundException e) {
                System.out.println("Hmmmmmm, impossible trace ...(" + prefix + "," + suffix + ")");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return result;
    }
}
