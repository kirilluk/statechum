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
        String outFile = "question" + erlString.hashCode() + ".out";

        File f = new File(ErlangQSMOracle.ErlangFolder + "/" + outFile);
        boolean accept = true;
        int count = AbstractOracle.USER_ACCEPTED;

        try {
            String line;
            BufferedReader input;
            while (f.exists()) {
                System.out.println("Waiting for a separate instance of the question " + outFile);
                Thread.sleep(1000);
            }
            //System.out.println("Evaluating " + outFile);
            //String erlCmd = "erl -eval 'tracer:trace(" + erlangModule + ", " + erlangFunction + ", " + erlString + ", \"" + outFile + "\"),halt().'";
            String erlCmd = "./erlscript.sh " + ErlangQSMOracle.erlangModule + " " + ErlangQSMOracle.erlangFunction + " " + erlString + " " + outFile;
            //System.out.println("Running " + erlCmd + " in folder " + ErlangFolder);
            Process p = Runtime.getRuntime().exec(erlCmd, null, new File(ErlangQSMOracle.ErlangFolder));
            input = new BufferedReader(new InputStreamReader(p.getInputStream()));
            //System.out.println("Process output:");
            int exit = p.waitFor();
            while ((line = input.readLine()) != null) {
                //System.out.println(line);
            }
            input.close();
            //System.out.println("Exit value: " + exit);
            input = new BufferedReader(new FileReader(ErlangQSMOracle.ErlangFolder + "/" + outFile));
            //System.out.println("Output file:");
            while ((line = input.readLine()) != null) {
                if (line.substring(0, 1).equals("-")) {
                    accept = false;
                    count = -1;
                    StringTokenizer st = new StringTokenizer(line.substring(1));
                    while (st.hasMoreTokens()) {
                        count++;
                        String t = st.nextToken();
                    }
                }
                //System.out.println(line);
            }
            input.close();
            f.delete();
        } catch (Exception err) {
            err.printStackTrace();
        }
        if (accept) {
            return AbstractOracle.USER_ACCEPTED;
        } else {
            return count;
        }

    }
}
