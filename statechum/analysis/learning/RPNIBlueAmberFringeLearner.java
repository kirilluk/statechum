package statechum.analysis.learning;

import java.awt.Frame;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.Configuration.QuestionGeneratorKind;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.observers.Learner.RestartLearningEnum;
import statechum.analysis.learning.rpnicore.ComputeQuestions;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSet;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class RPNIBlueAmberFringeLearner extends RPNIBlueFringeLearner {
	
	public RPNIBlueAmberFringeLearner(Frame parent, Configuration c) {
		super(parent,c);
		scoreComputer = new LearnerGraph(c);
	}
	
	protected LearnerGraph scoreComputer = null;

	
	/** The size of the initial plus/minus sets. */
	protected int origPlusSize, origMinusSize;
	
	@Override
	public void init(Collection<List<String>> plus, Collection<List<String>> minus)
	{
		scoreComputer.initPTA();
		scoreComputer.paths.augmentPTA(minus, false);
		scoreComputer.paths.augmentPTA(plus, true);
		origMinusSize = plus.size();origMinusSize = minus.size();
	}
	
	public void init(PTASequenceEngine en, int plus, int minus)
	{
		scoreComputer.initPTA();
		scoreComputer.paths.augmentPTA(en);

		origMinusSize = plus;origMinusSize = minus;
	}

	public String DifferenceBetweenPairOfSets(String prefix, Collection<List<String>> seqOrig,Collection<List<String>> seqNew)
	{
		Set<List<String>> newInQS = new HashSet<List<String>>();newInQS.addAll(seqNew);newInQS.removeAll(seqOrig); 
		Set<List<String>> newInOrig = new HashSet<List<String>>();newInOrig.addAll(seqOrig);newInOrig.removeAll(seqNew);
		return prefix+": new in QS:\n"+newInQS+"\n"+prefix+": new In Orig:\n"+newInOrig;
	}
	
	int plusSize = 0, minusSize = 0;
	
	/** Does 
	 * <pre>
	 * MergeStates.mergeAndDeterminize_general(scoreComputer, pair);
	 * </pre>
	 * but additionally checks for consistency.
	 */
	protected LearnerGraph getMergedGraph(StatePair pair, LearnerGraph newPTA)
	{
		int nonAmberA = 0;
		if (scoreComputer.config.isConsistencyCheckMode()) nonAmberA = newPTA.getStateNumber()-newPTA.getAmberStateNumber();
		LearnerGraph tempNew = MergeStates.mergeAndDeterminize_general(scoreComputer, pair);
		if (scoreComputer.config.isConsistencyCheckMode()) assert (newPTA.getStateNumber()-newPTA.getAmberStateNumber()) == nonAmberA;

		if (scoreComputer.config.isConsistencyCheckMode())
		{
			LearnerGraph tempOrig = MergeStates.mergeAndDeterminize(scoreComputer, pair);
			MergeStates.verifySameMergeResults(tempOrig, tempNew);
		}
		return tempNew;
	}
	
	/** Returns a collection of questions, but also checks them for consistency.
	 * 
	 * @param tempNew the graph after merge.
	 * @param pair pair of states to consider.
	 * @return questions to ask.
	 */
	protected List<List<String>> getQuestions(LearnerGraph tempNew, PairScore pair)
	{
		List<List<String>> questions = ComputeQuestions.computeQS(pair, scoreComputer,tempNew);
		if (scoreComputer.config.isConsistencyCheckMode()) 
		{// checking that all the old questions are included in the new ones
			assert scoreComputer.config.getQuestionGenerator() == QuestionGeneratorKind.CONVENTIONAL;
			assert scoreComputer.config.getQuestionPathUnionLimit() < 0;
			
			Collection<List<String>> questionsOrigA = ComputeQuestions.computeQS_orig(pair, scoreComputer,MergeStates.mergeAndDeterminize(scoreComputer, pair));
			CmpVertex Rnew = tempNew.getStateLearnt();
			assert Rnew == tempNew.getVertex(scoreComputer.wmethod.computeShortPathsToAllStates().get(pair.getR()));
			Collection<List<String>> questionsOrigB = ComputeQuestions.computeQS_orig(new StatePair(Rnew,Rnew), scoreComputer,tempNew);
			PTASequenceSet newQuestions =new PTASequenceSet();newQuestions.addAll(questions);
			assert newQuestions.containsAll(questionsOrigA);
			assert newQuestions.containsAll(questionsOrigB);
		}
		
		return questions;
	}

	public Learner getLearner()
	{
		return thisLearner;
	}
	
	protected final Learner thisLearner = new Learner()
	{

		public void AugmentPTA(LearnerGraph pta, 
				@SuppressWarnings("unused") RestartLearningEnum ptaKind,
				List<String> sequence, boolean accepted, JUConstants newColour) 
		{
			pta.paths.augmentPTA(sequence, accepted, newColour);
		}

		public Pair<Integer, String> CheckWithEndUser(LearnerGraph graph,
				List<String> question, Object[] options) {
			return RPNIBlueAmberFringeLearner.this.checkWithEndUser(graph, question, options);
		}

		public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) {
			return graph.pairscores.chooseStatePairs();
		}

		public List<List<String>> ComputeQuestions(PairScore pair,
				@SuppressWarnings("unused")	LearnerGraph original, LearnerGraph temp) 
		{
			return getQuestions(temp, pair);
		}

		public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
		{
			return getMergedGraph(pair,original);
		}

		public void Restart(@SuppressWarnings("unused")	RestartLearningEnum mode) 
		{
			// does nothing
		}

		public String getResult() {
			return null;
		}

		public LearnerGraph init(Collection<List<String>> plus,	Collection<List<String>> minus) 
		{
			RPNIBlueAmberFringeLearner.this.init(plus, minus);
			return RPNIBlueAmberFringeLearner.this.scoreComputer;
		}

		public LearnerGraph init(PTASequenceEngine engine, int plus, int minus) {
			RPNIBlueAmberFringeLearner.this.init(engine, plus, minus);
			return RPNIBlueAmberFringeLearner.this.scoreComputer;
		}

		public LearnerGraph learnMachine() {
			return new LearnerGraph(RPNIBlueAmberFringeLearner.this.learnMachine(),Configuration.getDefaultConfiguration());
		}

		public LearnerGraph learnMachine(PTASequenceEngine engine, int plus, int minus) 
		{
			topLearner.init(engine, plus, minus);
			return new LearnerGraph(RPNIBlueAmberFringeLearner.this.learnMachine(),scoreComputer.config);
		}

		public LearnerGraph learnMachine(Collection<List<String>> plus, Collection<List<String>> minus) 
		{
			topLearner.init(plus, minus);
			return new LearnerGraph(RPNIBlueAmberFringeLearner.this.learnMachine(),scoreComputer.config);
		}

		
		public void setTopLevelListener(Learner top) {
			topLearner = top;
		}
		
	};
	
	Learner topLearner = thisLearner;
	
	@Override
	public DirectedSparseGraph learnMachine() {
		setAutoOracle();
		LearnerGraph initialPTA = scoreComputer;// no need to clone - this is the job of mergeAndDeterminize anyway
		setChanged();
		initialPTA.setName("merge_debug"+0);
		updateGraph(initialPTA);
		
		Stack<PairScore> possibleMerges = topLearner.ChooseStatePairs(scoreComputer);
		plusSize = origPlusSize;minusSize = origMinusSize;
		int iterations = 0, currentNonAmber = initialPTA.getStateNumber()-initialPTA.getAmberStateNumber();
		counterRestarted = 0;
		while(!possibleMerges.isEmpty())
		{
			iterations++;
			//populateScores(possibleMerges,possibleMergeScoreDistribution);
			PairScore pair = possibleMerges.pop();
			LearnerGraph temp = topLearner.MergeAndDeterminize(initialPTA, pair);// FIXME: should be scoreComputer
			//System.out.println("considering "+pair+" non-amber: "+(newPTA.getStateNumber()-newPTA.getAmberStateNumber()));
			//Visualiser.updateFrame(scoreComputer.paths.getGraph(), temp.paths.getGraph());Visualiser.waitForKey();
			setChanged();temp.setName("merge_debug"+iterations);
			updateGraph(temp);
			Collection<List<String>> questions = new LinkedList<List<String>>();
			int score = pair.getScore();

			if(shouldAskQuestions(score))
				questions = topLearner.ComputeQuestions(pair, scoreComputer, temp);

			boolean restartLearning = false;// whether we need to rebuild a PTA and restart learning.
			
			//System.out.println(Thread.currentThread()+ " "+pair + " "+questions);
			Iterator<List<String>> questionIt = questions.iterator();
			while(questionIt.hasNext()){
				List<String> question = questionIt.next();
				boolean accepted = pair.getQ().isAccept();
				Pair<Integer,String> answer = topLearner.CheckWithEndUser(scoreComputer,question, new Object [] {"Test"});
				this.questionCounter++;
				if (answer.firstElem == USER_CANCELLED)
				{
					System.out.println("CANCELLED");
					return null;
				}
				
				CmpVertex tempVertex = temp.getVertex(question);
				
				if(answer.firstElem == USER_ACCEPTED)
				{
					//sPlus.add(question);
					topLearner.AugmentPTA(initialPTA, RestartLearningEnum.restartHARD, question, true,JUConstants.AMBER);
					//initialPTA.paths.augmentPTA(question, true,JUConstants.AMBER);
					++plusSize;
					if (ans != null) System.out.println(howAnswerWasObtained+question.toString()+ " <yes>");
					if(!tempVertex.isAccept())
					{
						restartLearning = true;break;
					}
				}
				else 
					if(answer.firstElem >= 0)
					{// The sequence has been rejected by a user
						assert answer.firstElem < question.size();
						LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer.firstElem+1));
						//sMinus.add(subAnswer);
						topLearner.AugmentPTA(initialPTA, RestartLearningEnum.restartHARD, subAnswer, false,JUConstants.AMBER);
						//initialPTA.paths.augmentPTA(subAnswer, false,JUConstants.AMBER);
						++minusSize;
						// important: since vertex IDs are 
						// only unique for each instance of ComputeStateScores, only once 
						// instance should ever receive calls to augmentPTA
						if (ans != null) System.out.println(howAnswerWasObtained+question.toString()+ " <no> at position "+answer.firstElem+", element "+question.get(answer.firstElem));
						if( (answer.firstElem < question.size()-1) || tempVertex.isAccept())
						{
							assert accepted == true;
							restartLearning = true;break;
						}
					}
					else 
						throw new IllegalArgumentException("unexpected user choice");
				
			}
			
			if (restartLearning)
			{// restart learning
				//ComputeStateScores expected = createAugmentedPTA(sPlus, sMinus);// KIRR: node labelling is done by createAugmentedPTA
				//System.out.println("restart at pair "+pair+", currently "+scoreComputer.getStateNumber()+" states, "+(scoreComputer.getStateNumber()-scoreComputer.getAmberStateNumber())+" non-amber");
				if (config.isSpeculativeQuestionAsking())
					if (speculativeGraphUpdate(possibleMerges, initialPTA))
						return null;
				scoreComputer = initialPTA;// no need to clone - this is the job of mergeAndDeterminize anyway
				scoreComputer.clearColoursButAmber();
				//System.out.println("finished with speculative update, currently "+scoreComputer.getStateNumber()+" states, "+(scoreComputer.getStateNumber()-scoreComputer.getAmberStateNumber())+" non-amber");
				iterations = 0;counterRestarted++;
				topLearner.Restart(RestartLearningEnum.restartHARD);
			}
			else
			{
				// At this point, scoreComputer may have been modified because it may point to 
				// the original PTA which will be modified as a result of new sequences being added to it.
				// temp is different too, hence there is no way for me to compute compatibility score here.
				// This is hence computed inside the obtainPair method.
				
				// keep going with the existing model
				scoreComputer = temp;
				topLearner.Restart(RestartLearningEnum.restartNONE);
			}
			
			possibleMerges = topLearner.ChooseStatePairs(scoreComputer);
		}
		assert currentNonAmber == initialPTA.getStateNumber()-initialPTA.getAmberStateNumber();
		DirectedSparseGraph result = scoreComputer.paths.getGraph();
		if(config.getDebugMode())
			updateGraph(scoreComputer);
		return result;
	}
	
	/** We might be doing a restart, but it never hurts to go through the existing 
	 * collection of vertices to merge and see if we can update the graph.
	 *  
	 * @return true if question answering has been cancelled by a user.
	 */
	boolean speculativeGraphUpdate(Stack<PairScore> possibleMerges, LearnerGraph originalPTA)
	{
		while(!possibleMerges.isEmpty())
		{
			PairScore pair = possibleMerges.pop();
			int score = pair.getScore();

			if(shouldAskQuestions(score))
			{
				LearnerGraph tempNew = null;
				try
				{
					tempNew = topLearner.MergeAndDeterminize(originalPTA, pair);
				}
				catch(IllegalArgumentException ex)
				{// ignore - tempNew is null anyway					
				}
				
				if (tempNew != null) // merge successful - it would fail if our updates to newPTA have modified scoreComputer (the two are often the same graph)
				{					
					for(List<String> question:getQuestions(tempNew, pair))
					{
						Pair<Integer,String> answer = topLearner.CheckWithEndUser(scoreComputer,question, new Object [] {"Test"});
						this.questionCounter++;
						if (answer.firstElem == USER_CANCELLED)
						{
							System.out.println("CANCELLED");
							return true;
						}
						
						if(answer.firstElem == USER_ACCEPTED)
						{
							topLearner.AugmentPTA(originalPTA, RestartLearningEnum.restartHARD, question, true,JUConstants.AMBER);
							//originalPTA.paths.augmentPTA(question, true,JUConstants.AMBER);
							++plusSize;
						}
						else 
							if(answer.firstElem >= 0)
							{// The sequence has been rejected by a user
								assert answer.firstElem < question.size();
								LinkedList<String> subAnswer = new LinkedList<String>();subAnswer.addAll(question.subList(0, answer.firstElem+1));
								topLearner.AugmentPTA(originalPTA, RestartLearningEnum.restartHARD, subAnswer, false,JUConstants.AMBER);
								//originalPTA.paths.augmentPTA(subAnswer, false,JUConstants.AMBER);
								++minusSize;
							}
					}
				}
			}
		}
		
		return false;
	}
}
