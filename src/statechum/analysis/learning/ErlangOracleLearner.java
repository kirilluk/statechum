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
import java.awt.Frame;

import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.Erlang.ErlangRunner;
import statechum.analysis.learning.ErlangOracleLearner.TraceOutcome.TRACEOUTCOME;
import statechum.analysis.learning.Visualiser.LayoutOptions;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.Label;
import statechum.Pair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.model.testset.PTASequenceEngine;

/**
 *
 * @author ramsay
 */
public class ErlangOracleLearner extends RPNIUniversalLearner {
	protected final ErlangModule module;
	
    public ErlangOracleLearner(Frame parent, LearnerEvaluationConfiguration evalCnf, ErlangModule mod) {
        super(parent, evalCnf);module = mod;
        
        // this one configures the runner.
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
        finished(); 
        return result;
    }

    /** We often need to add all possible failed outputs for each input rejected by Erlang, 
     * this collection holds all those outputs which correspond to a specific input.
     */
    protected Map<Label,Set<Label>> inputToPossibleOutputs = new TreeMap<Label,Set<Label>>();
    
    protected void updateInputToPossibleOutputs(Label label)
    {
		Label inputPortionOfLabel = stripOutput((ErlangLabel)label);
		Set<Label> rejects = inputToPossibleOutputs.get(inputPortionOfLabel);
		if (rejects == null)
		{
			rejects = new TreeSet<Label>();inputToPossibleOutputs.put(inputPortionOfLabel, rejects);
		}
		rejects.add(label);
    }

    @Override
    public Pair<Integer, String> CheckWithEndUser(@SuppressWarnings("unused") LearnerGraph model,
            final List<Label> question,
            @SuppressWarnings("unused") final int expectedForNoRestart,
            @SuppressWarnings("unused") final List<Boolean> consistentFacts,
            @SuppressWarnings("unused") final PairScore pairBeingMerged,
            @SuppressWarnings("unused") final Object[] moreOptions) 
    {
    	TraceOutcome outcome = askErlang(question);
    	StringBuffer response = null;
    	switch(outcome.outcome)
    	{
    	case TRACE_DIFFERENTOUTPUT:
    		// we generate a string because it will most likely need to be stored in logs etc,
    		// hence no point being efficient and returning the actual data.
    		response = new StringBuffer();
    		response.append("- [");response.append(RPNILearner.questionToString(question));response.append("] ");
    		response.append("+ [");response.append(RPNILearner.questionToString(Arrays.asList(outcome.answerDetails)));response.append("] ");
    		// since we implicitly extend the alphabet here, add a new trace to our collection.
    		updateInputToPossibleOutputs(outcome.answerDetails[outcome.answerDetails.length-1]);
    		break;
    	case TRACE_OK:
    		// trace was accepted, response remains null indicating success.    		
    		break;
    	case TRACE_FAIL:
    		// generate a collection of traces corresponding to failures
    		response = new StringBuffer();
    		int prefixLen = outcome.answerDetails.length;
    		List<Label> prefix = question.subList(0, prefixLen);
    		Label failedLabel = question.get(prefixLen);
    		response.append("- [");
    		boolean first = true;
    		for(Label lbl:inputToPossibleOutputs.get(stripOutput((ErlangLabel)failedLabel)))
    		{
    			if (!first) response.append(',');else first=false;
    			List<Label> failedSequence = new LinkedList<Label>(prefix);failedSequence.add(lbl);
    			response.append(RPNILearner.questionToString(failedSequence));
    		}
    		response.append(" ]");
    	}
    	
    	if (response != null)
    	{
    		return new Pair<Integer, String>(AbstractOracle.USER_NEWTRACE, response.toString());
    	}
    	return new Pair<Integer, String>(AbstractOracle.USER_ACCEPTED,null);
    }
    

	/** Erlang states are represented by traces used to enter those states.
	 * The reason for this is that we cannot re-enter states in an Erlang engine
	 * without re-running traces, hence the only realistic way is to associate
	 * states with paths.
	 */
	private static class ErlangState implements Comparable<ErlangState>
	{
		public final ErlangState previousState;
		public final Label inputToThisState;
		public final boolean accept;
		
		/** Inputs which have been rejected from this state. */
		public final Set<Label> rejects = new TreeSet<Label>();
		
		private final int ident;
		
	   	/** ErlangState id. */
		private static int ErlangStateId=0;
		
		public static synchronized ErlangState newErlangState(ErlangState prev, Label label, boolean a)
		{
			return new ErlangState(ErlangStateId++,prev,label,a);
		}
		
		private ErlangState(int id,ErlangState prev, Label label, boolean a)
		{
			previousState=prev;ident=id;inputToThisState=label;accept=a;
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() 
		{
			final int prime = 31;
			int result = 1;
			result = prime * result + ident;
			return result;
		}
		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof ErlangState))
				return false;
			ErlangState other = (ErlangState) obj;
			if (ident != other.ident)
				return false;
			return true;
		}
		@Override
		public int compareTo(ErlangState o) {
			return ident - o.ident;
		}
	}
	ErlangState initialState = null;
	
	/** The purpose of this class is to explore Erlang state machine without having 
     * to think about sequences which may lead somewhere or not lead anywhere at all.
     */
    protected class ErlangMachine implements PTASequenceEngine.FSMAbstraction
    {
		@Override
		public Object getNextState(Object currentState, Label input) {
			assert input instanceof ErlangLabel;
			ErlangState state = (ErlangState)currentState;
			
			// First of all, we check if we already have a wildcard-transition recorded from this state,
			Label inputPortionOfLabel = stripOutput((ErlangLabel)input);
			if (state.rejects.contains(inputPortionOfLabel))
				return null;// failed
			
			// now normal input
			if (state.rejects.contains(input))
				return null;
			
			// no luck, attempting the transition, first compute a path from the initial state to this state
			LinkedList<Label> newTrace = new LinkedList<Label>();
			newTrace.addFirst(input);
			ErlangState nextState = state;int traceLength = 1;
			while(nextState != initialState)
			{
				++traceLength;
				newTrace.addFirst(nextState.inputToThisState);
				nextState = nextState.previousState;
			}
			
			TraceOutcome outcome = askErlang(newTrace);
			
			// at this point, nextState == null
			switch(outcome.outcome)
			{
			case TRACE_OK:
				// we store the actual label
				assert outcome.answerDetails.length == traceLength;
				nextState = ErlangState.newErlangState(state, input, true);
				//System.out.println("OK : "+RPNILearner.questionToString(newTrace));
				break;
			case TRACE_DIFFERENTOUTPUT:
				assert outcome.answerDetails.length == traceLength;
				ErlangLabel nextLabel = outcome.answerDetails[traceLength-1];
				state.rejects.add(input);// record the reject.
				
				//nextState = ErlangState.newErlangState(state, nextLabel, true);
				if (!module.behaviour.getAlphabet().contains(nextLabel))
				{
					module.behaviour.getAlphabet().add(nextLabel);// extend the alphabet
					// (if the input is already in the alphabet, fine, it would be attempted soon anyway).
					//System.out.println("A  :"+RPNILearner.questionToString(newTrace)+" extended alphabet with "+OTPBehaviour.convertModToErl(nextLabel).toErlangTerm());
				}
				nextState = null;
				break;
			case TRACE_FAIL:
				// put a wildcard - this will come handy when we extend alphabet.
				state.rejects.add(inputPortionOfLabel);
				nextState = null;
				break;
			default:
				assert false;
			}
			return nextState;
		}

		@Override
		public Object getInitState() {
			if (initialState == null)
			{
				TraceOutcome outcome = askErlang(new LinkedList<Label>());
				assert outcome.outcome == TRACEOUTCOME.TRACE_OK;
				initialState = ErlangState.newErlangState(null, null, true);
			}
			return initialState;
		}

		@Override
		public boolean isAccept(Object currentState) {
			return ((ErlangState)currentState).accept;
		}

		/** This method should not be called.
		 * 
		 * @see statechum.model.testset.PTASequenceEngine.FSMAbstraction#setAccept(java.lang.Object, boolean)
		 */
		@Override
		public void setAccept(@SuppressWarnings("unused") Object currentState, @SuppressWarnings("unused") boolean value) 
		{
			throw new UnsupportedOperationException("this method should not be called");
		}

		@Override
		public boolean shouldBeReturned(Object elem) 
		{
			if (elem == null) return false;
			return ((ErlangState)elem).accept;
		}
    	
    }
    
    /** Starts the learning process by generating a number of traces. */
    public void GenerateInitialTraces()
    {
		PTASequenceEngine engine = new PTASequenceEngine();
		engine.init(new ErlangMachine());
/*    	 System.out.println(askErlang(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel(
    			 "{"+ErlangLabel.missingFunction+",init,AnyWibble,'aa'}", config)
    			 })).outcome);*/
		PTASequenceEngine.SequenceSet seq = engine.new SequenceSet();
		seq.setIdentity();
		PTASequenceEngine.SequenceSet seqNext = null;
    	 
		for(int waveNo=0;waveNo<5;++waveNo)
		{
			// have to make a copy to avoid concurrentModification exception when updating our alphabet.
			LinkedHashSet<Label> currentAlphabet = new LinkedHashSet<Label>(module.behaviour.getAlphabet());
			seqNext = seq.crossWithSet(currentAlphabet);
			if (currentAlphabet.size() < module.behaviour.getAlphabet().size())
			{
				LinkedHashSet<Label> newAlphabet = new LinkedHashSet<Label>(module.behaviour.getAlphabet());
				seqNext = seq.crossWithSet(newAlphabet);
				assert newAlphabet.size() == module.behaviour.getAlphabet().size() : "alphabet was extended for the second time";
			}
			seq = seqNext;
	    }
		
		// Create map associating input to all possible outputs
		for(Label label:module.behaviour.getAlphabet())
			updateInputToPossibleOutputs(label);
		
		topLevelListener.init(engine, 0, 0);
    }
    
    /** Records the result of running a trace past Erlang. */
    public static class TraceOutcome
    {
    	public static enum TRACEOUTCOME { TRACE_OK,TRACE_FAIL, TRACE_DIFFERENTOUTPUT };
    	public final ErlangLabel []answerDetails;
    	public final TRACEOUTCOME outcome;
    	
    	public TraceOutcome(ErlangLabel []trace, TRACEOUTCOME out)
    	{
    		answerDetails = trace;outcome = out;
    	}
    }
    
    public static ErlangLabel stripOutput(ErlangLabel label)
    {
    	return new ErlangLabel(label.function,label.callName,label.input,null);
    }
    
    /** Determines the outcome of running a trace past Erlang. */
    public TraceOutcome askErlang(List<? extends Label> question) 
    {
    	ErlangLabel []questionDetails = new ErlangLabel[question.size()];
    	int i=0;for(Label lbl:question) 
    	{
    		if (!(lbl instanceof ErlangLabel))
    			throw new IllegalArgumentException("question element "+lbl+" is not of Erlang type");
    		questionDetails[i++]=(ErlangLabel)lbl;
    	}
    	return askErlang(questionDetails);
    }
    
    /** Determines the outcome of running a trace past Erlang. */
    public TraceOutcome askErlang(ErlangLabel []questionDetails) 
    {
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
    				throw new IllegalArgumentException("unknown Erlang response "+outcome);
    	
    	OtpErlangList trace = (OtpErlangList)result.elementAt(2);
    	ErlangLabel []answerDetails = new ErlangLabel[trace.arity()];
    	for(int i=0;i<trace.arity();++i)
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

    /** Determines the default options with which a graph should be displayed. */
    @Override
    protected LayoutOptions layoutOptions() {
        LayoutOptions options = new LayoutOptions();
        options.showNegatives = false;
        return options;
    }
}
