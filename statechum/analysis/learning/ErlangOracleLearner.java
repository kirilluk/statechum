/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.learning;

import java.util.*;
import java.io.*;
import statechum.apps.ErlangQSMOracle;
import java.awt.Frame;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.Pair;
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
    public Pair<Integer, String> CheckWithEndUser(@SuppressWarnings("unused") LearnerGraph model, final List<String> question, final int expectedForNoRestart,
            final List<Boolean> consistentFacts, final Object[] moreOptions) {

        Iterator<String> it = question.iterator();
        //System.out.println("Question for " + erlangModule + ":" + erlangFunction + " is:");
        String erlList = "[";
        while (it.hasNext()) {
            if (!erlList.equals("[")) {
                erlList += ",";
            }
            erlList += it.next();
        }
        erlList += "]";
        int val = execErlang(erlList);
        //System.out.println("Responding " + val);
        // Second elem seems to be unused...
        return new Pair<Integer, String>(val, null);
    }

    protected int execErlang(String erlString) {
        //String outFile = "question" + erlString.hashCode() + ".out";

        //File f = new File(ErlangQSMOracle.ErlangFolder + "/" + ErlangQSMOracle.tracesFile);

        int failure = -2;
        try {
            // Lets see if QSM is being silly and we already know the answer...
            failure = firstFailure(ErlangQSMOracle.ErlangFolder + "/" + ErlangQSMOracle.tracesFile, erlString);
            if (failure == -2) {
                // We didn't find the answer in the existing traces file so lets extend it
                BufferedReader input;
                //System.out.println("Evaluating " + outFile);
                //String erlCmd = "erl -eval 'tracer:trace(" + erlangModule + ", " + erlangFunction + ", " + erlString + ", \"" + outFile + "\"),halt().'";
                String erlCmd = "./erlscript.sh " + ErlangQSMOracle.erlangModule + " " + ErlangQSMOracle.erlangFunction + " " + erlString + " " + ErlangQSMOracle.tracesFile + " " + ErlangOracleVisualiser.toErlangList(ErlangQSMOracle.erlangModules);
                System.out.println("Running " + erlCmd + " in folder " + ErlangQSMOracle.ErlangFolder);
                Process p = Runtime.getRuntime().exec(erlCmd, null, new File(ErlangQSMOracle.ErlangFolder));
                input = new BufferedReader(new InputStreamReader(p.getInputStream()));
                //System.out.println("Process output:");
                String line;
                while ((line = input.readLine()) != null) {
                    //System.out.println(line);
                }
                input.close();
                int exit = p.waitFor();
                //System.out.println("Exit value: " + exit);

                //f.delete();
                //ErlangQSMOracle.loadCoverageMaps(ErlangQSMOracle.ErlangFolder + "/" + outFile + ".covermap");
                ErlangQSMOracle.loadCoverageMaps();
                //(new File(ErlangQSMOracle.ErlangFolder + "/" + outFile + ".covermap")).delete();

                failure = firstFailure(ErlangQSMOracle.ErlangFolder + "/" + ErlangQSMOracle.tracesFile, erlString);
                // We really should have found the answer now...
            }
        } catch (Exception err) {
            err.printStackTrace();
        }
        if (failure == -1) {
            return AbstractOracle.USER_ACCEPTED;
        } else if (failure == -2) {
            throw new RuntimeException("Errrr, answer not found even though we asked Erlang (" + erlString + ")...");
        } else {
            return failure;
        }

    }

    /** Returns -1 if the string is shown as accepted, returns -2 if it is not found, and returns the point at which it is rejected otherwise */
    protected int firstFailure(String file, String erlString) throws IOException {
        BufferedReader input = new BufferedReader(new FileReader(file));
        //System.out.println("Output file:");
        // Convert erlang string into traces string
        // i.e. "[a,b,c]" becomes "a b c"
        String searchString = erlString.replaceAll(",", " ").substring(1, erlString.length() - 1);
        String line;
        int count = -2;
        while ((line = input.readLine()) != null) {
            String traceString = line.substring(1).trim();
            if (line.substring(0, 1).equals("-")) {
                if (searchString.startsWith(traceString)) {
                    // This line represents a rejection of a prefix of our question...
                    count = -1;
                    StringTokenizer st = new StringTokenizer(traceString);
                    while (st.hasMoreTokens()) {
                        count++;
                        String t = st.nextToken();
                    }
                    break;
                }
            } else {
                if (traceString.equals(searchString)) {
                    // This is an accept line for our string.
                    count = -1;
                    break;
                }
            }
        }
        //System.out.println(line);
        input.close();
        return count;
    }
}
