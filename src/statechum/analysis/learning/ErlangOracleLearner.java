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
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import java.util.*;
import java.io.*;

import statechum.apps.ErlangQSMOracle;
import java.awt.Frame;

import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.Erlang.ErlangRunner;
import statechum.analysis.learning.ErlangOracleLearner.TraceOutcome.TRACEOUTCOME;
import statechum.analysis.learning.Visualiser.LayoutOptions;
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
	protected final ErlangModule module;
	
    public ErlangOracleLearner(Frame parent, LearnerEvaluationConfiguration evalCnf, ErlangModule mod) {
        super(parent, evalCnf);module = mod;
        
        ErlangRunner.getRunner().configurationToErlang(evalCnf.config);
    	ErlangRunner.getRunner().call(new OtpErlangObject[]{
    			new OtpErlangAtom("addPath"),
        		new OtpErlangString(mod.sourceFolder.getAbsolutePath()) }, "addPath");

    }

    public void finished()
    {
    	ErlangRunner.getRunner().call(new OtpErlangObject[]{
    			new OtpErlangAtom("delPath"),
        		new OtpErlangString(module.sourceFolder.getAbsolutePath()) }, "delPath");
    }
    
    @Override
    public LearnerGraph learnMachine() {

         LearnerGraph result = super.learnMachine();
        return result;
    }


    @Override
    public Pair<Integer, String> CheckWithEndUser(@SuppressWarnings("unused") LearnerGraph model,
            final List<Label> question,
            @SuppressWarnings("unused") final int expectedForNoRestart,
            @SuppressWarnings("unused") final List<Boolean> consistentFacts,
            @SuppressWarnings("unused") final PairScore pairBeingMerged,
            @SuppressWarnings("unused") final Object[] moreOptions) {

        String prefixString = null;
        Trace qtrace = null;//KIRR: commented out Trace.fromListOfStrings(question);
        int failure = AbstractOracle.USER_CANCELLED;
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
            }
        }


        //failure = firstFailure(ErlangQSMOracle.ErlangFolder + "/" + ErlangQSMOracle.tracesFile, new Trace(question));
        if (failure == AbstractOracle.USER_CANCELLED) {
            // We didn't find the answer in the existing traces file so lets extend it
            // OR we did find a negative answer but it might be based on a wildcard for the output, so lets try again anyway!
        	/*
            String erlArgs = "tracer2:first_failure(" + ErlangQSMOracle.erlangWrapperModule + "," + ErlangQSMOracle.erlangModule + "," + erlList + ",\"" + ErlangQSMOracle.tracesFile + "\"," + ErlangOracleVisualiser.toErlangList(ErlangQSMOracle.erlangModules) + ")";
            System.out.println("Evaluating " + erlArgs + " in folder " + ErlangQSMOracle.ErlangFolder);
        	 */

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
        System.out.println("<Erlang> " + question + " " + failure + " " + prefixString);
        if (failure != AbstractOracle.USER_NEWTRACE) {
            return new Pair<Integer, String>(failure, null);
        } else {
            return new Pair<Integer, String>(failure, prefixString);
        }
    }
    
    public static class TraceOutcome
    {
    	public static enum TRACEOUTCOME { TRACE_OK,TRACE_FAIL, TRACE_DIFFERENTOUTPUT };
    	public final Label []answerDetails;
    	public final TRACEOUTCOME outcome;
    	
    	public TraceOutcome(Label []trace, TRACEOUTCOME out)
    	{
    		answerDetails = trace;outcome = out;
    	}
    }
    
    public static ErlangLabel stripOutput(ErlangLabel label)
    {
    	return new ErlangLabel(label.function,label.callName,label.input,null);
    }
    
    public TraceOutcome askErlang(List<Label> question) 
    {
    	ErlangLabel []questionDetails = new ErlangLabel[question.size()];
    	int i=0;for(Label lbl:question) 
    	{
    		if (!(lbl instanceof ErlangLabel))
    			throw new IllegalArgumentException("question element "+lbl+" is not of Erlang type");
    		questionDetails[i++]=(ErlangLabel)lbl;
    	}
    	
    	OtpErlangTuple result = ErlangRunner.getRunner().call(new OtpErlangObject[]{
                        new OtpErlangAtom("runTrace"),
                        new OtpErlangAtom(module.getName()),
                        new OtpErlangAtom(module.behaviour.getWrapperName()),
                        new OtpErlangList(questionDetails),
                		new OtpErlangList() // other modules
                        },"running trace");
    	
    	OtpErlangAtom outcome = (OtpErlangAtom)result.elementAt(1);
    	
    	TRACEOUTCOME outcomeEnum = null;
    	if (outcome.atomValue().equals("ok"))
    		outcomeEnum = TRACEOUTCOME.TRACE_OK;
    	else
    		if (outcome.atomValue().equals("failed_but"))
    			outcomeEnum = TRACEOUTCOME.TRACE_DIFFERENTOUTPUT;
    		else
    			if (outcome.atomValue().equals("failed"))
    				outcomeEnum = TRACEOUTCOME.TRACE_FAIL;
    			else
    				throw new IllegalArgumentException("unknown response "+outcome);
    	
    	OtpErlangList trace = (OtpErlangList)result.elementAt(2);
    	Label []answerDetails = new ErlangLabel[trace.arity()];
    	for(i=0;i<trace.arity();++i)
    	{
    		OtpErlangTuple elemAti = (OtpErlangTuple)trace.elementAt(i);
    		if (elemAti.arity() < 2 || elemAti.arity() > 3)
    			throw new IllegalArgumentException("received tuple "+elemAti+" of invalid arity");
    		if (elemAti.arity() == 3)
    			answerDetails[i] = new ErlangLabel(questionDetails[i].function,questionDetails[i].callName,
    				questionDetails[i].input, elemAti.elementAt(2));
    		else
    			answerDetails[i] = questionDetails[i];
    	}
    
    	return new TraceOutcome(answerDetails, outcomeEnum);
    }

    protected static Pair<Integer, String> altOutput(Trace prefix, Trace qtrace) {
        Pair<Integer, String> result = null;
        if (prefix.size() < qtrace.size()) {
            //System.out.println("Prefix found: " + prefix.toString());
            Label item = qtrace.get(prefix.size());
            // Wildcard the output
            // KIRR: commented out  //item = item.replaceAll(",[^,}]*}$", ",'*'}");
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
                traceFromFile = null;//Trace.fromListOfStrings(QSMTool.tokeniseInput(traceString));
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
