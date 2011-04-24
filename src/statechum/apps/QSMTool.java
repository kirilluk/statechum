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
package statechum.apps;

/**
 * Takes a text file, structured as follows:
 * 
 * first line: either "active" or "passive" followed by \n
 * following lines:
 * strings that belong to the target machine:
 * + function1, function2...
 * + function1, function3...
 * and optionally strings that do NOT belong to the target machine:
 * -function1, function4
 * 
 * @author nw
 */
import java.io.*;
import java.util.*;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Label;
import statechum.analysis.learning.PickNegativesVisualiser;
import statechum.analysis.learning.RPNIUniversalLearner;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.Learner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LTL_to_ba;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.smt.SmtLabelRepresentation;
import statechum.analysis.learning.rpnicore.PathRoutines;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class QSMTool {

    protected int k = -1;
    /** Learner configuration to be set. */
    protected Set<List<Label>> sPlus = new HashSet<List<Label>>();
    protected Set<List<Label>> sMinus = new HashSet<List<Label>>();
    protected final LearnerEvaluationConfiguration learnerInitConfiguration = new LearnerEvaluationConfiguration();
    protected Collection<String> dataDescription = null;
    protected boolean active = true;
    protected boolean showLTL = false;

    public static void main(String[] args) {
        QSMTool tool = new QSMTool();
        tool.loadConfig(args[0]);
        if (tool.showLTL) {
            Learner l = new RPNIUniversalLearner(null, tool.learnerInitConfiguration);
            LTL_to_ba ba = new LTL_to_ba(tool.learnerInitConfiguration.config);
            if (ba.ltlToBA(tool.learnerInitConfiguration.ifthenSequences, l.init(tool.sPlus, tool.sMinus), true,
                    GlobalConfiguration.getConfiguration().getProperty(GlobalConfiguration.G_PROPERTIES.LTL2BA))) {
                try {
                    LearnerGraph graph = Transform.ltlToIfThenAutomaton(ba.getLTLgraph().pathroutines.buildDeterministicGraph());
                    DirectedSparseGraph gr = graph.pathroutines.getGraph();
                    PathRoutines.convertPairAssociationsToTransitions(gr, graph, tool.learnerInitConfiguration.config);
                    Visualiser.updateFrame(gr, null);
                } catch (IncompatibleStatesException e) {
                    e.printStackTrace();
                }
            } else {
                throw new IllegalArgumentException("no LTL constraint defined");
            }
        } else {
            tool.runExperiment();
        }
    }

    public void loadConfig(String inputFileName) {
        try {
            loadConfig(new FileReader(inputFileName));
        } catch (FileNotFoundException e) {
            statechum.Helper.throwUnchecked("could not open a file with initial data", e);
        }
    }

    public void loadConfig(Reader inputData) {
        String AutoName = System.getProperty(statechum.GlobalConfiguration.G_PROPERTIES.VIZ_AUTOFILENAME.name());
        if (AutoName != null) {
            learnerInitConfiguration.config.setAutoAnswerFileName(AutoName);
        }

        BufferedReader in = null;
        try {
            in = new BufferedReader(inputData);
            String fileString;
            while ((fileString = in.readLine()) != null) {
                if (!((fileString.equals("- ")) || (fileString.equals("+ ")))) {
                    process(fileString);
                }
            }
            if (learnerInitConfiguration.labelDetails != null) {
                learnerInitConfiguration.labelDetails.parseCollection(dataDescription);
            }
        } catch (IOException e) {
            statechum.Helper.throwUnchecked("failed to read learner initial data", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {// ignored.
                }
            }
        }
    }

    public void runExperiment() {
        setSimpleConfiguration(learnerInitConfiguration.config, active, k);
        if (learnerInitConfiguration.ifthenSequences != null && !learnerInitConfiguration.ifthenSequences.isEmpty()) {
            learnerInitConfiguration.config.setUseLTL(true);
        }
        if (learnerInitConfiguration.labelDetails != null) {
            sPlus.addAll(learnerInitConfiguration.labelDetails.getSPlus());
            sMinus.addAll(learnerInitConfiguration.labelDetails.getSMinus());
        }
        PickNegativesVisualiser pnv = new PickNegativesVisualiser();
        pnv.construct(sPlus, sMinus, learnerInitConfiguration);

        pnv.startLearner(null);
        // new PickNegativesVisualiser(new
        // SootCallGraphOracle()).construct(sPlus, sMinus,null, active);
        //config.setMinCertaintyThreshold(1);
        //config.setQuestionPathUnionLimit(1);
        
    }

    public static void setSimpleConfiguration(Configuration config, final boolean active, final int k) {
        if (!active) {
            config.setKlimit(k);
            config.setAskQuestions(false);
            if (k >= 0) {
                config.setLearnerScoreMode(Configuration.ScoreMode.KTAILS);
            }
        } else {
            config.setKlimit(-1);
        }
        config.setDebugMode(true);
    }

    public static boolean isCmdWithArgs(String arg, String cmd) {
        if (arg.equals(cmd)) {
            throw new IllegalArgumentException("Argument required for command " + cmd);
        }
        return arg.startsWith(cmd);
    }

    public void process(String lineOfText) {
        String fileString = lineOfText.trim();
        if (fileString.length() == 0) {
            return;// ignore empty lines.
        }
        if (isCmdWithArgs(fileString, cmdPositive)) {
            sPlus.add(AbstractLearnerGraph.parseTrace(fileString.substring(cmdPositive.length() + 1),learnerInitConfiguration.config));
        } else if (isCmdWithArgs(fileString, cmdNegative)) {
            sMinus.add(AbstractLearnerGraph.parseTrace(fileString.substring(cmdNegative.length() + 1),learnerInitConfiguration.config));
        } else if (isCmdWithArgs(fileString, cmdLTL) || isCmdWithArgs(fileString, cmdIFTHENAUTOMATON)) {
            if (learnerInitConfiguration.ifthenSequences == null) {
                learnerInitConfiguration.ifthenSequences = new TreeSet<String>();
            }
            learnerInitConfiguration.ifthenSequences.add(fileString);
        } else if (isCmdWithArgs(fileString, cmdK)) {
            String value = fileString.substring(cmdK.length() + 1).trim();
            k = Integer.valueOf(value);
        } else if (fileString.startsWith(cmdTextOutput)) {
            learnerInitConfiguration.config.setGenerateTextOutput(true);
        } else if (fileString.startsWith(cmdDotOutput)) {
            learnerInitConfiguration.config.setGenerateDotOutput(true);
        } else if (fileString.startsWith(cmdPassive)) {
            active = false;
        } else if (isCmdWithArgs(fileString, cmdConfig)) {
            List<String> values = tokeniseInput(fileString.substring(cmdConfig.length() + 1));
            if (values.size() != 2) {
                throw new IllegalArgumentException("invalid configuration option " + fileString);
            }

            learnerInitConfiguration.config.assignValue(values.get(0), values.get(1), true);
        } else if (fileString.startsWith(cmdComment)) {// do nothing
        } else if (fileString.startsWith(cmdShowLTL)) {
            showLTL = true;
        } else if (fileString.startsWith(cmdOperation) || fileString.startsWith(cmdDataTrace) || fileString.startsWith(cmdLowLevelFunction)) {
            if (learnerInitConfiguration.labelDetails == null) {
                learnerInitConfiguration.labelDetails = new SmtLabelRepresentation(learnerInitConfiguration.config);
                dataDescription = new LinkedList<String>();
            }
            dataDescription.add(fileString.trim());
        } else {
            throw new IllegalArgumentException("invalid command " + fileString);
        }
    }
    public static final String cmdLTL = "ltl",
            cmdIFTHENAUTOMATON = "ifthenFSM",
            cmdK = "k",
            cmdPositive = "+",
            cmdNegative = "-",
            cmdDataTrace = "xT",
            cmdConfig = "config",
            cmdTextOutput = "textoutput",
            cmdDotOutput = "dotoutput",
            cmdComment = "#",
            cmdPassive = "passive",
            cmdOperation = "xm",
            cmdVarInput = "varInput",
            cmdLowLevelFunction = "func",
            cmdShowLTL = "showltl";

    /** Splits a string into a series of tokens at spaces.
     * Construction of labels can be done via <em>buildList</em>.
     */
    public static List<String> tokeniseInput(String str) {
        StringTokenizer tokenizer = new StringTokenizer(str," ,");
        List<String> sequence = new LinkedList<String>();
        while (tokenizer.hasMoreTokens()) {
            sequence.add(tokenizer.nextToken());
        }
        assert !sequence.isEmpty();
        return sequence;
    }
}
