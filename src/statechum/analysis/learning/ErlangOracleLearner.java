/* Copyright (c) 2011 The University of Sheffield.
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
 *
*/
package statechum.analysis.learning;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import java.util.*;
import java.io.*;

import statechum.apps.ErlangQSMOracle;
import statechum.apps.QSMTool;

import java.awt.Frame;
import statechum.Helper;

import statechum.analysis.Erlang.ErlangRunner;
import statechum.analysis.learning.Visualiser.LayoutOptions;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.Label;
import statechum.Pair;
import statechum.PrefixTraceTree;
import statechum.Trace;
import statechum.analysis.learning.rpnicore.LearnerGraph;

/**
 *
 * @author ramsay
 */
public class ErlangOracleLearner extends RPNIUniversalLearner {

    // This is going to be a named node so it can only exist once 
    public static Process erlangProcess = null;

    public ErlangOracleLearner(Frame parent, LearnerEvaluationConfiguration evalCnf) {
        super(parent, evalCnf);
    }

    @Override
    public LearnerGraph learnMachine() {

        erlangProcess = startErlang();
        if (erlangProcess == null) {
            return null;
        }
        LearnerGraph result = super.learnMachine();
        // Retain erlangProcess for use on re-learns....
        //killErlang();
        return result;
    }

    public static Process startErlang() {
        if (erlangProcess != null) {
            return erlangProcess;
        }
        try {
            erlangProcess = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin() + "erl", "-sname", "tracernode"}, null, new File(ErlangQSMOracle.ErlangFolder));
            int response = erlangProcess.getInputStream().read();
            while (response != '>' && response != -1) {
                System.out.print((char) response);
                response = erlangProcess.getInputStream().read();
            }
            // Init the trace server....
            String erlArgs = "tracer3:trace_server().\n";
            System.out.println("Evaluating " + erlArgs + " in folder " + ErlangQSMOracle.ErlangFolder);
            erlangProcess.getOutputStream().write(erlArgs.getBytes());
            erlangProcess.getOutputStream().flush();

        } catch (IOException e) {
            if (erlangProcess != null) {
                killErlang();
            }
            Helper.throwUnchecked("There was an error starting Erlang...", e);
        }
        return erlangProcess;
    }

    public static void killErlang() {
        if (erlangProcess != null) {
            try {
                erlangProcess.getOutputStream().write("halt().\n".getBytes());
                erlangProcess.getOutputStream().flush();
            } catch (IOException e1) {
                statechum.Helper.throwUnchecked("failed to communicate with Erlang process", e1);
            }
            ExperimentRunner.dumpStreams(erlangProcess, ErlangRunner.timeBetweenChecks, new HandleProcessIO() {

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
    public Pair<Integer, String> CheckWithEndUser(@SuppressWarnings("unused") LearnerGraph model,
            final List<String> question,
            @SuppressWarnings("unused") final int expectedForNoRestart,
            @SuppressWarnings("unused") final List<Boolean> consistentFacts,
            @SuppressWarnings("unused") final PairScore pairBeingMerged,
            @SuppressWarnings("unused") final Object[] moreOptions) {

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
        String prefixString = null;
        Trace qtrace = Trace.fromListOfStrings(question);
        int failure = AbstractOracle.USER_CANCELLED;
        try {
            Trace prefix = ErlangQSMOracle.ErlangTraces.findPrefix(qtrace);
            // Lets see if QSM is being silly and we already know the answer...

            if ((prefix != null) && (prefix.size() > 0)) {
                if (prefix.negative) {
                    failure = prefix.size() - 1;
                } else {
                    if (prefix.equals(qtrace)) {
                        throw new RuntimeException("Er, why are you asking? " + qtrace.toString() + " (" + prefix.toString() + ")");
                        //failure = AbstractOracle.USER_ACCEPTED;
                    }
                    /*
                    } else {
                    // Positive prefix - try alternative output
                    Pair<Integer, String> alt = altOutput(prefix, qtrace);
                    if (alt != null) {
                    failure = alt.firstElem;
                    prefixString = alt.secondElem;
                    }

                    }
                     * 
                     */
                }
            }


            //failure = firstFailure(ErlangQSMOracle.ErlangFolder + "/" + ErlangQSMOracle.tracesFile, new Trace(question));
            if (failure == AbstractOracle.USER_CANCELLED) {
                // We didn't find the answer in the existing traces file so lets extend it
                // OR we did find a negative answer but it might be based on a wildcard for the output, so lets try again anyway!
                String erlArgs = "tracer2:first_failure(" + ErlangQSMOracle.erlangWrapperModule + "," + ErlangQSMOracle.erlangModule + "," + erlList + ",\"" + ErlangQSMOracle.tracesFile + "\"," + ErlangOracleVisualiser.toErlangList(ErlangQSMOracle.erlangModules) + ")";
                System.out.println("Evaluating " + erlArgs + " in folder " + ErlangQSMOracle.ErlangFolder);
                erlangProcess.getOutputStream().write(erlArgs.getBytes());
                erlangProcess.getOutputStream().write('.');
                erlangProcess.getOutputStream().write('\n');
                erlangProcess.getOutputStream().flush();

                // now wait for a response.
                int response = erlangProcess.getInputStream().read();
                boolean finished = false;
                while (response != -1 && !finished) {
                    //System.out.print((char) response);
                    response = erlangProcess.getInputStream().read();
                    if (response == '>') {
                        // If we get a promt lets see if it just sits there for a while...
                        try {
                            Thread.sleep(100);
                        } catch (InterruptedException e) {
                            ;
                        }
                        // We often get a space afterwards?
                        if (erlangProcess.getInputStream().available() >= 1) {
                            response = erlangProcess.getInputStream().read();
                        }
                        //System.out.println("Got prompt and '" + ((char) response) + "'");
                        if ((response == ' ') && (erlangProcess.getInputStream().available() <= 0)) {
                            finished = true;
                        }
                    }
                }

                if (response == -1) {
                    throw new IllegalArgumentException("end of input reached when reading Erlang output");
                }

                //ErlangQSMOracle.loadCoverageMaps(ErlangQSMOracle.ErlangFolder + "/" + outFile + ".covermap");
                ErlangQSMOracle.loadCoverageMaps();
                //(new File(ErlangQSMOracle.ErlangFolder + "/" + outFile + ".covermap")).delete();

                ErlangQSMOracle.ErlangTraces = new PrefixTraceTree(ErlangQSMOracle.ErlangFolder + "/" + ErlangQSMOracle.tracesFile);
                //System.out.println("Traces Tree:\n" + ErlangQSMOracle.ErlangTraces.toString());
                //System.out.flush();
                prefix = ErlangQSMOracle.ErlangTraces.findPrefix(qtrace);
                if (prefix != null) {
                    if (prefix.negative) {
                        failure = prefix.size() - 1;
                    } else {
                        if (prefix.equals(qtrace)) {
                            failure = AbstractOracle.USER_ACCEPTED;
                        } else {
                            // Positive prefix but not actual data....
                            // Output alternative?
                            Pair<Integer, String> alt = altOutput(prefix, qtrace);
                            if (alt != null) {
                                failure = alt.firstElem;
                                prefixString = alt.secondElem;
                            } else {
                                throw new RuntimeException("Er, what?\n>question>>" + qtrace.toString() + "\n>prefix >>" + prefix.toString() + "\n");
                            }
                        }
                    }
                }
                // We really should have found the answer now...
                if (failure == AbstractOracle.USER_TRACENOTFOUND) {
                    throw new RuntimeException("Errrr, answer not found even though we asked Erlang (" + question + ")...");
                } else {
                    //System.out.println("Erlang says " + prefix.toString() + " (" + prefix.size() + " - " + prefix.negative + ") vs " + question + " ==> " + failure);
                }
            }
        } catch (IOException err) {
            statechum.Helper.throwUnchecked("failed to run Erlang", err);
        }
        System.out.println("<Erlang> " + question + " " + failure + " " + prefixString);
        if (failure != AbstractOracle.USER_NEWTRACE) {
            return new Pair<Integer, String>(failure, null);
        } else {
            return new Pair<Integer, String>(failure, prefixString);
        }
    }

    public static OtpErlangTuple askErlang(String module, String wrapper, OtpErlangList question) {
        if (erlangProcess == null) {
            startErlang();
        }
        OtpErlangTuple result = null;
        try {
            OtpNode myNode = new OtpNode("statechum");
            //OtpConnection connect = myNode.connect(new OtpPeer("tracernode"));
            OtpMbox myMbox = myNode.createMbox("statechumbox");

            OtpErlangTuple myMsg = new OtpErlangTuple(
                    new OtpErlangObject[]{
                        new OtpErlangAtom("first_failure"),
                        new OtpErlangTuple(new OtpErlangObject[]{
                            new OtpErlangAtom(module),
                            new OtpErlangAtom(wrapper),
                            question
                        }),
                        myMbox.self()
                    });

            System.out.println("trace_server@tracernode ! " + myMsg.toString());
            myMbox.send("trace_server", "tracernode", myMsg);
            try {
                OtpErlangObject msg = myMbox.receive(1000);
                if (msg == null) {
                    Helper.throwUnchecked("Timed out waiting for tracer process", new RuntimeException(""));
                } else if (msg instanceof OtpErlangTuple) {
                    return (OtpErlangTuple) msg;
                } else {
                    Helper.throwUnchecked("Wrong type of message from the tracer process", new RuntimeException(msg.toString()));
                }
            } catch (OtpErlangException e) {
                Helper.throwUnchecked("Erlang problem", e);
            }


            myNode.close();
        } catch (IOException e) {
            Helper.throwUnchecked("Error communicating with Erlang...", e);
        }
        return result;
    }

    protected static Pair<Integer, String> altOutput(Trace prefix, Trace qtrace) {
        Pair<Integer, String> result = null;
        if (prefix.size() < qtrace.size()) {
            //System.out.println("Prefix found: " + prefix.toString());
            Label item = qtrace.get(prefix.size());
            // Wildcard the output
            item = item.replaceAll(",[^,}]*}$", ",'*'}");
            Trace newPrefix = prefix.clone();
            newPrefix.add(item);
            //System.out.println("Trying " + newPrefix.toString());
            Trace alt = ErlangQSMOracle.ErlangTraces.findPrefix(newPrefix);
            if ((alt != null) && (alt.size() > prefix.size())) {
                qtrace.negative = true;
                result = new Pair<Integer, String>(AbstractOracle.USER_NEWTRACE, alt.toTraceString() + "/" + qtrace.toTraceString());
                //System.out.println("Got: " + alt.toTraceString());
                //System.exit(1);
            } else {
                //System.out.println("Nope...");
            }
        }
        return result;

    }

    /** Evaluates the supplied command in Erlang environment.
     * @param  
     */
    public static void runErlang(String ErlangCommand) throws IOException {
        Process erlangProcess = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin() + "erl", "-eval", ErlangCommand + ",halt()."}, null, new File(ErlangQSMOracle.ErlangFolder));
        ExperimentRunner.dumpStreams(erlangProcess, ErlangRunner.timeBetweenChecks, new HandleProcessIO() {

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
        try {
            erlangProcess.waitFor();
        } catch (InterruptedException e) {
            // assume we were expected to terminate
            ;
        }
    }

    /** Returns -1 if the string is shown as accepted, returns -2 if it is not found, and returns the point at which it is rejected otherwise */
    protected int firstFailure(String file, Trace erlTrace) throws IOException {
        //System.out.println("Seeking first failure for " + erlTrace);
        BufferedReader input = new BufferedReader(new FileReader(file));

        String line;
        int count = AbstractOracle.USER_TRACENOTFOUND;
        // We may find a short negative trace with a wildcard but we should look for longer positive traces
        // that may use a different instantiation
        int negativecount = AbstractOracle.USER_TRACENOTFOUND;
        while ((line = input.readLine()) != null && count == AbstractOracle.USER_TRACENOTFOUND) {
            String traceString = line.substring(1).trim();
            Trace traceFromFile;
            if (traceString.equals("")) {
                traceFromFile = new Trace();
            } else {
                traceFromFile = Trace.fromListOfStrings(QSMTool.tokeniseInput(traceString));
            }
            if (line.substring(0, 1).equals("-")) {
                if (traceFromFile.size() <= erlTrace.size()
                        && traceFromFile.isPrefix(erlTrace)) {
                    // We have to be careful not to pick a negative trace if there is a longer positive trace with a different instantiation of wildcards...
                    // We also want to find the LONGEST negative trace...
                    if (traceFromFile.size() - 1 > negativecount) {
                        //System.out.println("                        - " + traceFromFile);
                        negativecount = traceFromFile.size() - 1;
                    }
                    //break;
                }
            } else {
                assert line.substring(0, 1).equals("+");

                if (traceFromFile.size() >= erlTrace.size() && traceFromFile.isPrefix(erlTrace)) {
                    // System.out.println("                        + " + traceFromFile);

                    // This is an accept line for our string.
                    count = AbstractOracle.USER_ACCEPTED;
                    break;
                }
            }
        }
        input.close();
        if (count != AbstractOracle.USER_TRACENOTFOUND) {
            // If we have a positive trace lets use that
            return count;
        } else {
            return negativecount;
        }
    }

    /** Determines the default options with which a graph should be displayed. */
    @Override
    protected LayoutOptions layoutOptions() {
        LayoutOptions options = new LayoutOptions();
        options.showNegatives = false;
        return options;
    }
}
