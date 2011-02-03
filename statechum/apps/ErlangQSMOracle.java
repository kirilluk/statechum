/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.apps;

import java.io.*;
import java.util.*;

import javax.swing.*;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.analysis.learning.*;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LTL_to_ba;
import statechum.analysis.learning.rpnicore.LabelRepresentation;
import statechum.analysis.learning.rpnicore.PathRoutines;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;

/**
 *
 * @author ramsay
 */
/** Extends QSMTool to use an Erlang oracle to answer the questions...
 *
 * @author ramsay
 */
public class ErlangQSMOracle extends QSMTool {

    public static String erlangModule;
    public static String erlangFunction;
    public static String erlangAlphabet;
    public static String tracesFile;
    public static String ErlangFolder = "ErlangOracle";

    public static void main(String[] args) {
        // Generate some basic traces to get QSM started
        erlangModule = args[1];
        erlangFunction = args[2];
        tracesFile = args[0];
        erlangAlphabet = args[3];
        createInitTraces();
        ErlangQSMOracle tool = new ErlangQSMOracle();
        tool.loadConfig(ErlangFolder + "/" + tracesFile);
        tool.runExperiment();
    }

    @Override
    public void runExperiment() {
        setSimpleConfiguration(learnerInitConfiguration.config, active, k);
        if (learnerInitConfiguration.ifthenSequences != null && !learnerInitConfiguration.ifthenSequences.isEmpty()) {
            learnerInitConfiguration.config.setUseLTL(true);
        }
        if (learnerInitConfiguration.labelDetails != null) {
            sPlus.addAll(learnerInitConfiguration.labelDetails.getSPlus());
            sMinus.addAll(learnerInitConfiguration.labelDetails.getSMinus());
        }
        // This is the one line thats actually changed...
        ErlangOracleVisualiser pnv = new ErlangOracleVisualiser();
        pnv.construct(sPlus, sMinus, learnerInitConfiguration);

        pnv.startLearner(null);
        // new PickNegativesVisualiser(new
        // SootCallGraphOracle()).construct(sPlus, sMinus,null, active);
        //config.setMinCertaintyThreshold(1);
        //config.setQuestionPathUnionLimit(1);
    }

    public static void createInitTraces() {
        try {
            String erlCmd = "./erlinittraces.sh " + erlangModule + " " + erlangFunction + " " + erlangAlphabet + " " + tracesFile;
            System.out.println("Running " + erlCmd + " in folder " + ErlangFolder);
            Process p = Runtime.getRuntime().exec(erlCmd, null, new File(ErlangFolder));
            System.out.println("Creating init traces...");
            BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
            //System.out.println("Process output:");
            String line;
            while ((line = input.readLine()) != null) {
                System.out.println(line);
            }
            input.close();

            p.waitFor();

            System.out.println("Traces file:");
            input = new BufferedReader(new FileReader(ErlangQSMOracle.ErlangFolder + "/" + tracesFile));
            
            while ((line = input.readLine()) != null) {
                System.out.println(line);
            }
            input.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
