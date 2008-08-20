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
import java.util.List;
import java.util.Stack;

import statechum.JUConstants;
import statechum.Pair;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.observers.ProgressDecorator.AugmentPTAData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
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
	
	@Override
	public LearnerGraph learnMachine(final Collection<List<String>> plus, 
			final Collection<List<String>> minus)
	{
		secondThread = new Thread(new Runnable() {

			public void run() {
				try
				{
					learningOutcome=whatToCompareWith.learnMachine(plus, minus);
				}
				catch(IllegalArgumentException ex)
				{
					if (failureCode == null) failureCode = ex;
					System.out.println(Thread.currentThread());ex.printStackTrace();
					synchronized(this) { notifyAll(); }
				}
			}
			
		},"other learner");
		secondThread.start();
		LearnerGraph result = null;
		try
		{
			result = decoratedLearner.learnMachine(plus, minus);
		}
		catch(IllegalArgumentException ex)
		{
			if (failureCode == null) failureCode = ex;
			System.out.println(Thread.currentThread());ex.printStackTrace();
		}
		
		try {
			synchronized(this) { notifyAll(); }
			secondThread.join();
		} catch (InterruptedException e) {
			if (failureCode == null) 
			{ 
				IllegalArgumentException ex = new IllegalArgumentException("interrupted : "+e);ex.initCause(e);failureCode = ex;
			}
		}
		if (failureCode != null) throw failureCode;
		
		WMethod.checkM(learningOutcome, result);
		learningOutcome=null;// reset stored data
		return result;
	}

	@Override
	public LearnerGraph learnMachine(final PTASequenceEngine engine, 
			final int plusSize,	final int minusSize)
	{
		secondThread = new Thread(new Runnable() {

			public void run() {
				try
				{
					learningOutcome=whatToCompareWith.learnMachine(engine, plusSize, minusSize);
				}
				catch(IllegalArgumentException ex)
				{
					if (failureCode == null) failureCode = ex;
					System.out.println(Thread.currentThread());ex.printStackTrace();
					synchronized(this) { notifyAll(); }
				}
			}
			
		},"other learner");
		secondThread.start();
		LearnerGraph result = null;
		try
		{
			result = decoratedLearner.learnMachine(engine, plusSize, minusSize);
		}
		catch(IllegalArgumentException ex)
		{
			if (failureCode == null) failureCode = ex;
			System.out.println(Thread.currentThread());ex.printStackTrace();
		}
		
		try {
			synchronized(this) { notifyAll(); }
			secondThread.join();
		} catch (InterruptedException e) {
			if (failureCode == null) 
			{ 
				IllegalArgumentException ex = new IllegalArgumentException("interrupted : "+e);ex.initCause(e);failureCode = ex;
			}
		}
		if (failureCode != null) throw failureCode;
		
		WMethod.checkM(learningOutcome, result);
		learningOutcome=null;// reset stored data
		return result;
	}

	
	public Test_LearnerComparator(Learner what, Learner with)
	{
		super(what);
		whatToCompareWith = with;
		what.setTopLevelListener(this);with.setTopLevelListener(this);
	}
	
	protected enum KIND_OF_METHOD { M_AUGMENT, M_CHECKWITHUSER,M_CHOOSEPAIRS,M_QUESTIONS,M_MERGEANDDETERMINIZE,M_RESTART,M_INIT};
	
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
	private synchronized boolean checkCall(KIND_OF_METHOD method)
	{
		if (failureCode != null) throw failureCode;
		if (expected != null && method != expected)
		{
			failureCode = new IllegalArgumentException("inconsistent method calls: "+expected.toString()+" and "+method.toString());
			notify();
			throw failureCode;
		}
		
		boolean result = false;// second thread
		if (expected == null)
		{
			expected = method;result = true;// first thread
		}
		return result;
	}
	
	/** Synchronises with another thread.
	 * 
	 * @param first if this is the first thread to grab <em>expected</em>.
	 */
	private synchronized void waitForOtherThread(boolean first)
	{
		try {
			// First thread will wait for the second one to finish its task and notify the first one.
			if (first)
			{
				wait();
				expected = null;// this will complete before the second thread enters another method.
				notify();
			}
			else
			{
				notify();
				wait();// used to stop the other thread from outrunning the first one by a large margin.
			}
		} catch (InterruptedException e) 
		{
			if (failureCode != null) 
			{ 
				IllegalArgumentException ex = new IllegalArgumentException("interrupted : "+e);ex.initCause(e);failureCode = ex;
			}
		}
		
		if (failureCode != null)
		{
			notify();
			throw failureCode;
		}
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
		
		boolean first = checkCall(KIND_OF_METHOD.M_AUGMENT);
		AugmentPTAData data = new AugmentPTAData(ptaKind,sequence,accepted,newColour);
		if (first) 
			augmentData = data;// we are the first thread here
		else
		{
			if (!data.equals(augmentData))  // second thread, checking.
				failureCode = new IllegalArgumentException("different augment PTA values");
			augmentData=null;// reset stored data
		}
		waitForOtherThread(first);// make sure that both threads synchronize at this point.

		// now call the expected method
		if (Thread.currentThread() == secondThread)
			whatToCompareWith.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
		else
			decoratedLearner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
	}

	protected Thread secondThread = null;
	
	protected List<String> question = null;
	protected Pair<Integer,String> cPair = null;
	
	/** Simulated check.
	 * @param g estimated graph, not loaded.
	 * @param question question loaded from XML
	 * @param options set to null by the simulator.
	 * @return value loaded from XML
	 */
	public synchronized Pair<Integer, String> CheckWithEndUser(LearnerGraph graph, List<String> argQuestion, Object[] options) 
	{
		Pair<Integer, String> result = null;
		// First, we call the expected method
		if (Thread.currentThread() == secondThread)
			result = whatToCompareWith.CheckWithEndUser(graph, argQuestion, options);
		else
			result = decoratedLearner.CheckWithEndUser(graph, argQuestion, options);

		boolean first = checkCall(KIND_OF_METHOD.M_CHECKWITHUSER);
		if (first)
		{// we are the first thread here
			question = argQuestion;cPair = result;
		}
		else
		{// second thread, checking.
			if (!question.equals(argQuestion))
				failureCode = new IllegalArgumentException("different CheckWithEndUser questions");
			if (!cPair.equals(result))
				failureCode = new IllegalArgumentException("different CheckWithEndUser results "+cPair.firstElem+" v.s. "+result.firstElem+" and "+cPair.secondElem+" v.s. "+result.secondElem);
			cPair =null;question=null;// reset stored data
		}
		waitForOtherThread(first);// make sure that both threads synchronize at this point.

		return result;
	}

	protected Stack<PairScore> pairs = null;
	
	/** Called by the simulator.
	 * 
	 * @param graph estimated graph
	 * @return loaded values from XML.
	 */
	public synchronized Stack<PairScore> ChooseStatePairs(LearnerGraph graph) 
	{
		Stack<PairScore> result = null;
		if (Thread.currentThread() == secondThread)
			result = whatToCompareWith.ChooseStatePairs(graph);
		else
			result = decoratedLearner.ChooseStatePairs(graph);

		boolean first = checkCall(KIND_OF_METHOD.M_CHOOSEPAIRS);
		if (first)
		{// we are the first thread here
			pairs = result;
		}
		else
		{// second thread, checking. 
			
		// Since accept/reject labelling is not stored in the XML file, we have to compare pairs discounting accept/reject
			if (pairs.size() != result.size())
				new IllegalArgumentException("different sizes of ChooseStatePairs collections of pairs, \n"+pairs+" v.s. \n"+result);
			Iterator<PairScore> ps1 = pairs.iterator(), ps2=result.iterator();
			while(ps1.hasNext())
			{
				PairScore p1 = ps1.next(),p2=ps2.next();
				if (!p1.getQ().getID().equals(p2.getQ().getID()) || !p1.getR().getID().equals(p2.getR().getID()) ||
						p1.getScore() != p2.getScore() || p1.getAnotherScore() != p2.getAnotherScore())
					new IllegalArgumentException("different ChooseStatePairs pairs, "+p1+" v.s. "+p2);
			}
			
			pairs =null;// reset stored data
		}
		waitForOtherThread(first);// make sure that both threads synchronize at this point.

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
	public synchronized Collection<List<String>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) 
	{
		Collection<List<String>> result = null;
		// First, we call the expected method
		if (Thread.currentThread() == secondThread)
			result = whatToCompareWith.ComputeQuestions(pair, original, temp);
		else
			result = decoratedLearner.ComputeQuestions(pair, original, temp);

		boolean first = checkCall(KIND_OF_METHOD.M_QUESTIONS);
		if (first)
		{// we are the first thread here
			qPair = pair;questions = result;
		}
		else
		{// second thread, checking, ignoring scores and accept-conditions.
			if (!qPair.getQ().getID().equals(pair.getQ().getID()) || !qPair.getR().getID().equals(pair.getR().getID()))
					failureCode = new IllegalArgumentException("different ComputeQuestions pair "+qPair+" v.s. "+pair);
			if (!questions.equals(result))
				failureCode = new IllegalArgumentException("different ComputeQuestions questions");
			qPair =null;questions=null;// reset stored data
		}
		waitForOtherThread(first);// make sure that both threads synchronize at this point.

		return result;
	}

	protected LearnerGraph mGraph = null;
	
	protected StatePair mPair = null;
	
	/** Returns the graph stored in XML.
	 * 
	 * @param original graph to be processed, the simulator attempts to supply a relevant value, however it is not certain to be correct.
	 * @param pair the pair to be merged. Loaded from XML file (without scores).
	 * @return graph loaded from XML file.
	 */
	public synchronized LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
	{
		LearnerGraph result = null;
		// First, we call the expected method
		if (Thread.currentThread() == secondThread)
			result = whatToCompareWith.MergeAndDeterminize(original, pair);
		else
			result = decoratedLearner.MergeAndDeterminize(original, pair);

		boolean first = checkCall(KIND_OF_METHOD.M_MERGEANDDETERMINIZE);
		if (first)
		{// we are the first thread here
			mPair = pair;mGraph = result;
		}
		else
		{// second thread, checking, considering that acceptance conditions are not stored in XML.
			if (!mPair.getQ().getID().equals(pair.getQ().getID()) || !mPair.getR().getID().equals(pair.getR().getID()))
				failureCode = new IllegalArgumentException("different MergeAndDeterminize pair "+mPair+" v.s. "+pair);
			try
			{
				WMethod.checkM(mGraph, result);
			}
			catch(DifferentFSMException ex)
			{
				failureCode = ex;
			}
			mPair =null;mGraph=null;// reset stored data
		}
		waitForOtherThread(first);// make sure that both threads synchronize at this point.
		
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
			whatToCompareWith.Restart(mode);
		else
			decoratedLearner.Restart(mode);

		boolean first = checkCall(KIND_OF_METHOD.M_RESTART);
		if (first)
		{// we are the first thread here
			rMode = mode;
		}
		else
		{// second thread, checking.
			if (!rMode.equals(mode))
				failureCode = new IllegalArgumentException("different Restart mode");
			rMode=null;// reset stored data
		}
		waitForOtherThread(first);// make sure that both threads synchronize at this point.
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
		LearnerGraph result = null;
		// First, we call the expected method
		if (Thread.currentThread() == secondThread)
			result = whatToCompareWith.init(plus, minus);
		else
			result = decoratedLearner.init(plus, minus);

		boolean first = checkCall(KIND_OF_METHOD.M_INIT);
		if (first)
		{// we are the first thread here
			iGraph = result;
		}
		else
		{// second thread, checking.
			try
			{
				WMethod.checkM(iGraph, result);
			}
			catch(DifferentFSMException ex)
			{
				failureCode = ex;
			}
			iGraph=null;// reset stored data
		}
		waitForOtherThread(first);// make sure that both threads synchronize at this point.


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
