/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.learning;

import java.util.*;
import java.io.*;

import statechum.apps.ErlangQSMOracle;
import statechum.apps.QSMTool;

import java.awt.Frame;

import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.Pair;
import statechum.analysis.learning.rpnicore.LTL_to_ba;
import statechum.analysis.learning.rpnicore.LearnerGraph;

/**
 *
 * @author ramsay
 */
public class ErlangOracleLearner extends RPNIUniversalLearner {

    public ErlangOracleLearner(Frame parent, LearnerEvaluationConfiguration evalCnf) {
        super(parent, evalCnf);
    }

    @Override
    public LearnerGraph learnMachine() {
        try {
            erlangProcess = Runtime.getRuntime().exec(new String[]{"erl"}, null, new File(ErlangQSMOracle.ErlangFolder));
            int response = erlangProcess.getInputStream().read();
            while (response != '>' && response != -1) {
                System.out.print((char) response);
                response = erlangProcess.getInputStream().read();
            }

        } catch (IOException e) {
            killErlang();
            return null;
        }

        LearnerGraph result = super.learnMachine();
        killErlang();
        return result;
    }
    Process erlangProcess = null;

    protected void killErlang() {
        if (erlangProcess != null) {
            try {
                erlangProcess.getOutputStream().write("halt().\n".getBytes());
                erlangProcess.getOutputStream().flush();
            } catch (IOException e1) {
                statechum.Helper.throwUnchecked("failed to communicate with Erlang process", e1);
            }
            ExperimentRunner.dumpStreams(erlangProcess, LTL_to_ba.timeBetweenHearbeats, new HandleProcessIO() {

                @Override
                public void OnHeartBeat() {// no prodding is done for a short-running converter.
                }

                @Override
                public void StdErr(@SuppressWarnings("unused") StringBuffer b) {
                    //System.err.print(b.toString());
                }

                @Override
                public void StdOut(@SuppressWarnings("unused") StringBuffer b) {
                    //System.err.print(b.toString());
                }
            });
            try {
                erlangProcess.waitFor();
            } catch (InterruptedException e) {
                statechum.Helper.throwUnchecked("wait for Erlang to terminate aborted", e);
            }
            erlangProcess = null;
        }
    }

    @Override
    public Pair<Integer, String> CheckWithEndUser(@SuppressWarnings("unused") LearnerGraph model, final List<String> question, final int expectedForNoRestart,
            final List<Boolean> consistentFacts, final Object[] moreOptions) {

        Iterator<String> it = question.iterator();
        //System.out.println("Question for " + erlangModule + ":" + erlangWrapperModule + " is:");
        String erlList = "[";
        while (it.hasNext()) {
            if (!erlList.equals("[")) {
                erlList += ",";
            }
            erlList += it.next();
        }
        erlList += "]";
        int failure = AbstractOracle.USER_CANCELLED;
        try {
            // Lets see if QSM is being silly and we already know the answer...
            failure = firstFailure(ErlangQSMOracle.ErlangFolder + "/" + ErlangQSMOracle.tracesFile, question);
            if (failure == AbstractOracle.USER_TRACENOTFOUND) {
                // We didn't find the answer in the existing traces file so lets extend it

                String erlArgs = "tracer2:first_failure(" + ErlangQSMOracle.erlangWrapperModule + "," + ErlangQSMOracle.erlangModule + "," + erlList + ",\"" + ErlangQSMOracle.tracesFile + "\"," + ErlangOracleVisualiser.toErlangList(ErlangQSMOracle.erlangModules) + ")";
                System.out.println("Evaluating " + erlArgs + " in folder " + ErlangQSMOracle.ErlangFolder);
                erlangProcess.getOutputStream().write(erlArgs.getBytes());
                erlangProcess.getOutputStream().write('.');
                erlangProcess.getOutputStream().write('\n');
                erlangProcess.getOutputStream().flush();

                // now wait for a response.
                int response = erlangProcess.getInputStream().read();
                while (response != '\n' && response != -1) {
                    System.out.print((char) response);
                    response = erlangProcess.getInputStream().read();
                }
                while (response != '>' && response != -1) {
                    System.out.print((char) response);
                    response = erlangProcess.getInputStream().read();
                }

                if (response == -1) {
                    throw new IllegalArgumentException("end of input reached when reading Erlang output");
                }

                // This is returning before the file is written...
                // FIXME this is a stupid fix...
                try {
                    Thread.sleep(500);
                } catch (Exception e) {
                    ;
                }
                //ErlangQSMOracle.loadCoverageMaps(ErlangQSMOracle.ErlangFolder + "/" + outFile + ".covermap");
                ErlangQSMOracle.loadCoverageMaps();
                //(new File(ErlangQSMOracle.ErlangFolder + "/" + outFile + ".covermap")).delete();

                failure = firstFailure(ErlangQSMOracle.ErlangFolder + "/" + ErlangQSMOracle.tracesFile, question);
                // We really should have found the answer now...
                if (failure == AbstractOracle.USER_TRACENOTFOUND) {
                    throw new RuntimeException("Errrr, answer not found even though we asked Erlang (" + question + ")...");
                }
            }
        } catch (IOException err) {
            statechum.Helper.throwUnchecked("failed to run Erlang", err);
        }
        return new Pair<Integer, String>(failure, null);
    }

    /** Evaluates the supplied command in Erlang environment.
     * @param  
     */
    public static void runErlang(String ErlangCommand) throws IOException, InterruptedException {
        Process erlangProcess = Runtime.getRuntime().exec(new String[]{"erl", "-eval", ErlangCommand + ",halt()."}, null, new File(ErlangQSMOracle.ErlangFolder));
        ExperimentRunner.dumpStreams(erlangProcess, LTL_to_ba.timeBetweenHearbeats, new HandleProcessIO() {

            @Override
            public void OnHeartBeat() {// no prodding is done for a short-running converter.
            }

            @Override
            public void StdErr(@SuppressWarnings("unused") StringBuffer b) {
                System.err.print(b.toString());
            }

            @Override
            public void StdOut(@SuppressWarnings("unused") StringBuffer b) {
                System.out.print(b.toString());
            }
        });
        erlangProcess.waitFor();
    }

    /** Returns -1 if the string is shown as accepted, returns -2 if it is not found, and returns the point at which it is rejected otherwise */
    protected int firstFailure(String file, List<String> erlTrace) throws IOException {
        BufferedReader input = new BufferedReader(new FileReader(file));

        String line;
        int count = AbstractOracle.USER_TRACENOTFOUND;
        while ((line = input.readLine()) != null && count == AbstractOracle.USER_TRACENOTFOUND) {
            String traceString = line.substring(1).trim();
            List<String> traceFromFile;
            if (traceString.equals("")) {
                traceFromFile = new LinkedList<String>();
            } else {
                traceFromFile = QSMTool.tokeniseInput(traceString);
            }
            if (line.substring(0, 1).equals("-")) {
                if (traceFromFile.size() <= erlTrace.size()
                        && ErlangOracleVisualiser.isPrefix(traceFromFile, erlTrace)) {
                    count = traceFromFile.size() - 1;
                    break;
                }
            } else {
                assert line.substring(0, 1).equals("+");

                if (traceFromFile.size() >= erlTrace.size() && ErlangOracleVisualiser.isPrefix(erlTrace, traceFromFile)) {
                    // This is an accept line for our string.
                    count = AbstractOracle.USER_ACCEPTED;
                    break;
                }
            }
        }
        input.close();
        return count;
    }
}
