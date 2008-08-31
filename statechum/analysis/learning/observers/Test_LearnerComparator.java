/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/
package statechum.analysis.learning.observers;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import statechum.JUConstants;
import statechum.Pair;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.observers.ProgressDecorator.AugmentPTAData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import statechum.model.testset.PTASequenceEngine;

/**
 * @author kirill
 *
 */
public class Test_LearnerComparator extends LearnerDecorator {

	protected Learner whatToCompareWith = null;
	
	protected LearnerGraph learningOutcome = null;
	
	protected abstract class runComparator 
	{
		/** Runs the learner and returns the result of learning. */
		abstract LearnerGraph runLearner(Learner learner);
		
		public LearnerGraph runCmp()
		{
			secondThread = new Thread(new Runnable() {

				public void run() {
					try
					{
						learningOutcome=runLearner(whatToCompareWith);
						checkCall(KIND_OF_METHOD.M_FINISHED);
					}
					catch(IllegalArgumentException ex)
					{
						if (failureCode == null) failureCode = ex;
						synchronized(Test_LearnerComparator.this)
						{
							Test_LearnerComparator.this.notify();// the only place the first thread can be stuck is the wait() statement in checkCall,
							// now it will proceed and throw on the exception, unrolling the stack.
						}
					}
					
				}
				
			},"other learner");
			secondThread.setDaemon(true);// ensures termination of the second thread when I kill the main thread.
			secondThread.start();
			LearnerGraph result = null;
			try
			{
				result = runLearner(decoratedLearner);
				checkCall(KIND_OF_METHOD.M_FINISHED);
			}
			catch(IllegalArgumentException ex)
			{
				if (failureCode == null) failureCode = ex;
				
				synchronized(Test_LearnerComparator.this)
				{
					Test_LearnerComparator.this.notify();// the only place the second thread can be stuck is the wait() statement in checkCall,
						// now it will proceed and throw on the exception, unrolling the stack.
				}
			}
			
			try {
				secondThread.join();
			} catch (InterruptedException e) {
				if (failureCode == null) 
				{ 
					IllegalArgumentException ex = new IllegalArgumentException("interrupted : "+e);ex.initCause(e);failureCode = ex;
				}
			}
			
			if (learningOutcome != null && result != null)
				checkGraphEquality(learningOutcome, result);

			if (failureCode != null) throw failureCode;
			
			learningOutcome=null;// reset stored data
			return result;
			
		}
	}
	
	@Override
	public LearnerGraph learnMachine(final Collection<List<String>> plus, 
			final Collection<List<String>> minus)
	{
		return new runComparator() {
			@Override
			LearnerGraph runLearner(Learner learner) 
			{
				return learner.learnMachine(plus, minus);
			}
		}.runCmp();
	}

	@Override
	public LearnerGraph learnMachine(final PTASequenceEngine engine, 
			final int plusSize,	final int minusSize)
	{
		return new runComparator() {
			@Override
			LearnerGraph runLearner(Learner learner) 
			{
				return learner.learnMachine(engine, plusSize, minusSize);
			}
		}.runCmp();

	}

	/** Whether to check colouring of states - not for use with GD because
	 * since GD does not preserve colours of existing states. 
	 */
	protected final boolean checkColours; 
	
	protected void checkGraphEquality(LearnerGraph what, LearnerGraph with)
	{
		DifferentFSMException ex = null;
		if (checkColours)
			ex = MergeStates.checkM_and_colours(what,with);
		else
			ex = WMethod.checkM(what, with);
		
		//Assert.assertTrue(WMethod.sameStateSet(what,with));
		if (ex != null && failureCode == null) failureCode = ex;
	}
	
	/** Constructs this comparator.
	 * 
	 * @param what what to compare
	 * @param with with what
	 * @param checkCol whether to bother comparing colours of states - do not use if GD is used
	 * since it does not preserve colours of existing states.
	 */
	public Test_LearnerComparator(Learner what, Learner with, boolean checkCol)
	{
		super(what);checkColours = checkCol;
		whatToCompareWith = with;
		what.setTopLevelListener(this);with.setTopLevelListener(this);
	}
	
	protected enum KIND_OF_METHOD { M_AUGMENT, M_CHECKWITHUSER,M_CHOOSEPAIRS,M_QUESTIONS,M_MERGEANDDETERMINIZE,M_RESTART,M_INIT,M_FINISHED, M_METHODEXIT}
	
	/** Next expected call. */
	protected KIND_OF_METHOD expected = null;
		
	/** Assigned if both threads are to terminate with a failure. */
	protected IllegalArgumentException failureCode = null;
	
	/** Sets the expected method call kind.
	 * 
	 * @param method expected method call.
	 * @return true if the assignment happened, because <em>expected</em> was null,
	 * i.e. this is the first thread.
	 */
	synchronized void checkCall(KIND_OF_METHOD method)
	{
		try 
		{
			if (expected == null)
			{
				expected = method;
				wait();// we wait for the second method to get here
			}
			else
			{// the second method
				if (method != expected && failureCode == null)
					failureCode = new IllegalArgumentException("inconsistent method calls: "+expected.toString()+" and "+method.toString());
				expected = null; // reset the expected value
				notify();// let the first method know we've been there
			}
		} catch (InterruptedException e) {
			if (failureCode == null) 
			{ 
				IllegalArgumentException ex = new IllegalArgumentException("interrupted : "+e);ex.initCause(e);failureCode = ex;
			}
		}

		if (failureCode != null) throw failureCode;
	}
	
	
	protected AugmentPTAData augmentData = null;

	/** Does nothing in the simulator.
	 * 
	 * @param pta is always null in the simulator.
	 * @param ptaKind loaded from XML.
	 * @param sequence loaded from XML.
	 * @param accepted loaded from XML.
	 * @param newColour loaded from XML.
	 */
	public synchronized void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind,
			List<String> sequence, boolean accepted, JUConstants newColour) {

		AugmentPTAData data = new AugmentPTAData(ptaKind,sequence,accepted,newColour);
		
		// now call the expected method
		if (Thread.currentThread() == secondThread)
		{
			whatToCompareWith.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
			augmentData = data;
		}
		else
			decoratedLearner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);

		checkCall(KIND_OF_METHOD.M_AUGMENT);

		if (Thread.currentThread() != secondThread)
		{
			if (!data.equals(augmentData))  // second thread, checking.
				failureCode = new IllegalArgumentException("different augment PTA values");
			augmentData=null;// reset stored data
		}

		checkCall(KIND_OF_METHOD.M_METHODEXIT);// aims to stop one of the threads running fast 
		// from the first checkCall and overwriting the stored value before the other 
		// thread had a chance to use it in a comparison.

	}

	protected Thread secondThread = null;
	
	protected List<String> question = null;
	protected Pair<Integer,String> cPair = null;
	
	/** Simulated check.
	 * @param g estimated graph, not loaded from XML.
	 * @param question question loaded from XML or computed by a learner.
	 * @param options set to null by the simulator.
	 * @return value loaded from XML or computed by the learner.
	 */
	public synchronized Pair<Integer, String> CheckWithEndUser(LearnerGraph graph, List<String> argQuestion, Object[] options) 
	{
		Pair<Integer, String> result = null;
		// First, we call the expected method
		if (Thread.currentThread() == secondThread)
		{
			result = whatToCompareWith.CheckWithEndUser(graph, argQuestion, options);
			question = argQuestion;cPair = result;
		}
		else
			result = decoratedLearner.CheckWithEndUser(graph, argQuestion, options);

		checkCall(KIND_OF_METHOD.M_CHECKWITHUSER);

		if (Thread.currentThread() != secondThread)
		{// checking.
			if (!question.equals(argQuestion))
				failureCode = new IllegalArgumentException("different CheckWithEndUser questions");
			if (!cPair.equals(result))
				failureCode = new IllegalArgumentException("different CheckWithEndUser results "+cPair.firstElem+" v.s. "+result.firstElem+" and "+cPair.secondElem+" v.s. "+result.secondElem);
			cPair =null;question=null;// reset stored data
		}

		checkCall(KIND_OF_METHOD.M_METHODEXIT);// aims to stop one of the threads running fast 
		// from the first checkCall and overwriting the stored value before the other 
		// thread had a chance to use it in a comparison.

		return result;
	}

	protected List<PairScore> pairs = null;
	
	/** Called by the simulator.
	 * 
	 * @param graph estimated graph
	 * @return loaded values from XML.
	 */
	public synchronized Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
	{
		List<PairScore> pairsAndScores = new LinkedList<PairScore>();
		Stack<PairScore> result = null;
		if (Thread.currentThread() == secondThread)
		{
			result = whatToCompareWith.ChooseStatePairs(graph);
			pairsAndScores.addAll(result); // make sure that we do not depend on 
			// subsequent modification of the stack of pairs (one thread may 
			// outrun another one and make changes to it before another one 
			// had a chance to compare the two stacks).
			pairs = pairsAndScores;
		}
		else
		{
			result = decoratedLearner.ChooseStatePairs(graph);
			pairsAndScores.addAll(result);
		}
		
		checkCall(KIND_OF_METHOD.M_CHOOSEPAIRS);
		
		if (Thread.currentThread() != secondThread)
		{// checking. 
			
		// Since accept/reject labelling is not stored in the XML file, we have to compare pairs discounting accept/reject
			if (pairs.size() != pairsAndScores.size())
				new IllegalArgumentException("different sizes of ChooseStatePairs collections of pairs, \n"+pairs+" v.s. \n"+pairsAndScores);
			Iterator<PairScore> ps1 = pairs.iterator(), ps2=pairsAndScores.iterator();
			while(ps1.hasNext())
			{
				PairScore p1 = ps1.next(),p2=ps2.next();
				if (!p1.getQ().getID().equals(p2.getQ().getID()) || !p1.getR().getID().equals(p2.getR().getID()) ||
						p1.getScore() != p2.getScore() || p1.getAnotherScore() != p2.getAnotherScore())
					new IllegalArgumentException("different ChooseStatePairs pairs, "+p1+" v.s. "+p2);
			}
			
			pairs =null;// reset stored data
		}

		checkCall(KIND_OF_METHOD.M_METHODEXIT);// aims to stop one of the threads running fast 
		// from the first checkCall and overwriting the stored value before the other 
		// thread had a chance to use it in a comparison.

		return result;
	}

	protected PairScore qPair = null;
	protected Collection<List<String>> questions = null;
	
	/** Called by the simulator.
	 * 
	 * @param pair loaded from XML.
	 * @param original estimated value.
	 * @param temp estimated value.
	 * @return loaded from XML.
	 */
	public synchronized List<List<String>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) 
	{
		List<List<String>> result = null;
		// First, we call the expected method
		if (Thread.currentThread() == secondThread)
		{
			result = whatToCompareWith.ComputeQuestions(pair, original, temp);
			qPair = pair;questions = result;
		}
		else
			result = decoratedLearner.ComputeQuestions(pair, original, temp);

		checkCall(KIND_OF_METHOD.M_QUESTIONS);
		
		if (Thread.currentThread() != secondThread)
		{// checking, ignoring scores and accept-conditions.
			if (!qPair.getQ().getID().equals(pair.getQ().getID()) || !qPair.getR().getID().equals(pair.getR().getID()))
					failureCode = new IllegalArgumentException("different ComputeQuestions pair "+qPair+" v.s. "+pair);
			if (!questions.equals(result))
				failureCode = new IllegalArgumentException("different ComputeQuestions questions: \n"+questions+"\n v.s.\n"+result);
			qPair =null;questions=null;// reset stored data
		}

		checkCall(KIND_OF_METHOD.M_METHODEXIT);// aims to stop one of the threads running fast 
		// from the first checkCall and overwriting the stored value before the other 
		// thread had a chance to use it in a comparison.

		return result;
	}

	protected StatePair mPair = null;
	protected LearnerGraph mGraph = null;
	
	/** Returns the graph stored in XML.
	 * 
	 * @param original graph to be processed, the simulator attempts to supply a relevant value, however it is not certain to be correct.
	 * @param pair the pair to be merged. Loaded from XML file (without scores).
	 * @return graph loaded from XML file.
	 */
	public synchronized LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
	{
		LearnerGraph result = null, copyOfResult = null;
		// First, we call the expected method
		if (Thread.currentThread() == secondThread)
		{
			result = whatToCompareWith.MergeAndDeterminize(original, pair);
			copyOfResult = result.copy(result.config);// since a tread which produced result may exit and modify the graph, we have to take a copy of it.
			mPair = pair;mGraph = copyOfResult;
		}
		else
		{
			result = decoratedLearner.MergeAndDeterminize(original, pair);
			copyOfResult = result.copy(result.config);
		}
		checkCall(KIND_OF_METHOD.M_MERGEANDDETERMINIZE);

		if (Thread.currentThread() != secondThread)
		{// checking, considering that acceptance conditions are not stored in XML.
			if (!mPair.getQ().getID().equals(pair.getQ().getID()) || !mPair.getR().getID().equals(pair.getR().getID()))
				failureCode = new IllegalArgumentException("different MergeAndDeterminize pair "+mPair+" v.s. "+pair);
			checkGraphEquality(mGraph, copyOfResult);

			mPair =null;mGraph=null;// reset stored data
		}

		checkCall(KIND_OF_METHOD.M_METHODEXIT);// aims to stop one of the threads running fast 
		// from the first checkCall and overwriting the stored value before the other 
		// thread had a chance to use it in a comparison.
		
		return result;
	}

	protected RestartLearningEnum rMode = null;
	
	/** Does nothing in the simulator. 
	 * 
	 * @param mode value loaded from XML.
	 */
	public synchronized void Restart(RestartLearningEnum mode) {
		// First, we call the expected method
		if (Thread.currentThread() == secondThread)
		{
			whatToCompareWith.Restart(mode);
			rMode = mode;
		}
		else
			decoratedLearner.Restart(mode);

		checkCall(KIND_OF_METHOD.M_RESTART);

		if (Thread.currentThread() != secondThread)
		{// checking.
			if (!rMode.equals(mode))
				failureCode = new IllegalArgumentException("different Restart mode");
			rMode=null;// reset stored data
		}

		checkCall(KIND_OF_METHOD.M_METHODEXIT);// aims to stop one of the threads running fast 
		// from the first checkCall and overwriting the stored value before the other 
		// thread had a chance to use it in a comparison.
	}

	/** Not used by the simulator. */
	public synchronized String getResult() {
		return null;
	}

	protected LearnerGraph iGraph = null;

	/** Both arguments and the return value are stored by the simulator.
	 *  
	 * @param plus value loaded from XML
	 * @param minus value loaded from XML
	 */
	public synchronized LearnerGraph init(Collection<List<String>> plus,	Collection<List<String>> minus) {
		LearnerGraph result = null, copyOfResult = null;
		// First, we call the expected method
		if (Thread.currentThread() == secondThread)
		{
			result = whatToCompareWith.init(plus, minus);
			copyOfResult = result.copy(result.config);
			iGraph = copyOfResult;
		}
		else
		{
			result = decoratedLearner.init(plus, minus);
			copyOfResult = result.copy(result.config);
		}
		checkCall(KIND_OF_METHOD.M_INIT);

		if (Thread.currentThread() != secondThread)
		{// second thread, checking.
			checkGraphEquality(iGraph, copyOfResult);

			iGraph=null;// reset stored data
		}

		checkCall(KIND_OF_METHOD.M_METHODEXIT);// aims to stop one of the threads running fast 
		// from the first checkCall and overwriting the stored value before the other 
		// thread had a chance to use it in a comparison.

		return result;
	}

	/** Not supported. */
	public LearnerGraph init(@SuppressWarnings("unused") PTASequenceEngine engine, 
			@SuppressWarnings("unused")	int plusSize,
			@SuppressWarnings("unused")	int minusSize) 
	{
		throw new UnsupportedOperationException("only init with collections is supported");
	}

}
