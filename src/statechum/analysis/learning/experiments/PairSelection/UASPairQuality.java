/* Copyright (c) 2016 The University of Sheffield.
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
 */

package statechum.analysis.learning.experiments.PairSelection;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.Label;
import statechum.Pair;
import statechum.ProgressIndicator;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovHelper;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovParameters;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.LearnerThatCanClassifyPairs;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.StateMergingStatistics;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ReferenceLearner;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DataCollectorParameters;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReference;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceDiff;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.DifferenceToReferenceFMeasure;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.InitialConfigurationAndData;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.LearnerThatUsesWekaResults.TrueFalseCounter;
import statechum.analysis.learning.experiments.PaperUAS.ExperimentPaperUAS;
import statechum.analysis.learning.linear.GD;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.EquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphCachedData;
import statechum.analysis.learning.rpnicore.MergeStates;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;
import weka.classifiers.Classifier;
import weka.core.Instances;

/** This was an experiment to see how different choices affect the outcome of learning of the UAS automaton. */
public class UASPairQuality extends ExperimentPaperUAS
{
    public static final int pairchoiceMIN=-2, pairchoiceMAX=-1, pairchoiceORIG=-3;

    /** Counts the number with the same score at the top of the stack. */
    public static int countChoices(Stack<PairScore> stack)
    {
    	if (stack.isEmpty())
    		return 0;
    	int outcome = 1;
    	PairScore top = stack.peek();
    	
    	int i=stack.size()-2;
    	while(i>=0)
    	{
    		PairScore curr = stack.get(i);--i;
    		if (curr.getScore() != top.getScore() || curr.getR() != top.getR() )
    			break;
    		++outcome;
    	}
    	return outcome;
    }
    
    
    /** Returns the pair corresponding to the smallest or largest machine for one of the pairs with the same score at the top of the stack. */ 
    public static PairScore selectPairMinMax(LearnerGraph graph, Stack<PairScore> stack, int pairChoice)
    {
    	PairScore top = stack.peek();
    	int value = MergeStates.mergeAndDeterminize(graph, top).getStateNumber();
    	int i=stack.size()-2;
    	while(i>=0)
    	{
    		PairScore pair = stack.get(i);--i;
    		if (pair.getScore() != top.getScore()) break;
    		int stateNumber = MergeStates.mergeAndDeterminize(graph, pair).getStateNumber();
    		switch(pairChoice)
    		{
    		case pairchoiceMIN:
    			if (stateNumber < value)
    			{
    				top=pair;value=stateNumber;
    			}
    			break;
    		case pairchoiceMAX:
    			if (stateNumber > value)
    			{
    				top=pair;value=stateNumber;
    			}
    			break;
    		default:
    			throw new IllegalArgumentException("invalid argument "+pairChoice);
    		}
    	}
    	return top;
    }
    
    /** Picks a pair at the top of the stack at random. */
    public static PairScore selectPairAtRandom(Stack<PairScore> stack, Random rnd)
    {
    	PairScore top = stack.get(stack.size()-1-rnd.nextInt(countChoices(stack)));
    	assert top.getScore() == stack.peek().getScore();
    	return top;
    }

    
    public void runExperimentWithSingleAutomaton(String name, String arffName, LearnerGraph referenceGraph) throws Exception
    {
 	   final Collection<List<Label>> evaluationTestSet = LearningAlgorithms.computeEvaluationSet(referenceGraph,-1,-1);
    		DrawGraphs gr = new DrawGraphs();
 		final RBoxPlot<String>
 			uas_F=new RBoxPlot<String>("Time","F-measure",new File("time_"+name+"_f.pdf")),
 			uas_Diff=new RBoxPlot<String>("Time","Diff-measure",new File("time_"+name+"_Diff.pdf"));
 		SquareBagPlot gr_diff_to_f = new SquareBagPlot("f-measure","diff-based measure",new File("diff-to-f.pdf"),0,1,true);

 		Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).keySet();
 		Classifier classifiers[] = loadClassifierFromArff(arffName);
 		ProgressIndicator progress = new ProgressIndicator("UAS", allFrames.size()*classifiers.length);
 		LearnerEvaluationConfiguration initConfiguration = new LearnerEvaluationConfiguration(learnerInitConfiguration.config);
 		initConfiguration.setLabelConverter(learnerInitConfiguration.getLabelConverter());// we do not copy if-then automata here because we do not wish to augment from if-then on every iteration because our properties are pairwise and this permits augmentation to be carried out first thing and not any more.
 		initConfiguration.config.setUseConstraints(false);// do not use if-then during learning (refer to the explanation above)
 		
 		LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(learnerInitConfiguration.ifthenSequences, referenceGraph.pathroutines.computeAlphabet(), learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
 		System.out.println(new Date().toString()+" learning commencing.");
 		final Integer frame=6;
   		//for(final Integer frame:allFrames)
   		{
   			LearnerGraph initPTA = new LearnerGraph(initConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));
   			Transform.augmentFromIfThenAutomaton(initPTA, null, ifthenAutomata, initConfiguration.config.getHowManyStatesToAddFromIFTHEN());// we only need  to augment our PTA once (refer to the explanation above).
   			System.out.println("total states : "+initPTA.getStateNumber()+", "+initPTA.getAcceptStateNumber()+" accept-states");

   			final Set<Label> alphabetForIfThen = referenceGraph.pathroutines.computeAlphabet();
   			Label uniqueLabel = AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", initConfiguration.config,initConfiguration.getLabelConverter());

   			/*
   			for(int i=0;i<classifiers.length;++i)
 	  		{
 	  			{
 	  				initPTA.storage.writeGraphML("resources/"+name+"-init_"+frame+"_"+i+".xml");
 		  			LearnerThatCanClassifyPairs learner = new LearnerThatUsesWekaResults(ifDepth,initConfiguration,referenceGraph,classifiers[i],initPTA);
 		  			//learner.setAlphabetUsedForIfThen(alphabetForIfThen);
 		  			learner.setLabelsLeadingToStatesToBeMerged(Collections.<Label>emptyList());learner.setLabelsLeadingFromStatesToBeMerged(Collections.<Label>emptyList());
 		  			//learner.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", initConfiguration.config,initConfiguration.getLabelConverter())}));
 		 	        final LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 		 	        //actualAutomaton.storage.writeGraphML("resources/"+name+"_"+frame+"_"+i+".xml");
 			        
 		 	        double differenceF = PairQualityLearner.estimationOfDifferenceFmeasure(referenceGraph, actualAutomaton, evaluationTestSet);
 		 	        double differenceD = PairQualityLearner.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, initConfiguration.config, ExperimentRunner.getCpuNumber());
 			        System.out.println(new Date().toString()+" _L: For frame : "+frame+" (classifier "+i+"), long traces f-measure = "+ differenceF+" diffmeasure = "+differenceD);
 					uas_F.add(frame+"_L",differenceF,"green");uas_Diff.add(frame+"_L",differenceD,"green");gr_diff_to_f.add(differenceF,differenceD);
 	  			}
 	  				  			
 	  			{
 		  			LearnerThatCanClassifyPairs learner = new LearnerThatUsesWekaResults(ifDepth,initConfiguration,referenceGraph,classifiers[i],initPTA);
 		  			learner.setAlphabetUsedForIfThen(alphabetForIfThen);
 		  			learner.setLabelsLeadingToStatesToBeMerged(Collections.<Label>emptyList());
 		  			learner.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueLabel}));
 		 	        LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			        //actualAutomaton.storage.writeGraphML("resources/"+name+"-mm_"+frame+"_"+i+".xml");

 			        // Now merge everything that we need to merge
 			        LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
 					List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(actualAutomaton,learner.getLabelsLeadingToStatesToBeMerged(),learner.getLabelsLeadingFromStatesToBeMerged());
 					if (!pairsList.isEmpty())
 					{
 						int score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
 						if (score < 0) throw new RuntimeException("last merge in the learning process was not possible");
 						actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge);
 					}

 			        double differenceF = PairQualityLearner.estimationOfDifferenceFmeasure(referenceGraph, actualAutomaton, evaluationTestSet);
 			        double differenceD = PairQualityLearner.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, initConfiguration.config, ExperimentRunner.getCpuNumber());
 			        System.out.println(new Date().toString()+" _M: For frame : "+frame+" (classifier "+i+"), long traces f-measure = "+ differenceF+" diffmeasure = "+differenceD);
 					uas_F.add(frame+"_M",differenceF,"blue");uas_Diff.add(frame+"_M",differenceD,"blue");gr_diff_to_f.add(differenceF,differenceD);
 	  			}
 	  			{
 	  				LearnerGraph ptaAfterMergingBasedOnUniques = PairQualityLearner.mergeStatesForUnique(initPTA,uniqueLabel);
 		  			LearnerThatCanClassifyPairs learner = new LearnerThatUsesWekaResults(ifDepth,initConfiguration,referenceGraph,classifiers[i],ptaAfterMergingBasedOnUniques);
 		  			learner.setAlphabetUsedForIfThen(alphabetForIfThen);
 		  			learner.setLabelsLeadingToStatesToBeMerged(Collections.<Label>emptyList());
 		  			learner.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueLabel}));
 		 	        LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 			        //actualAutomaton.storage.writeGraphML("resources/"+name+"-mm_"+frame+"_"+i+".xml");

 			        // Now merge everything that we need to merge
 			        LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
 					List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(actualAutomaton,learner.getLabelsLeadingToStatesToBeMerged(),learner.getLabelsLeadingFromStatesToBeMerged());
 					if (!pairsList.isEmpty())
 					{
 						int score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
 						if (score < 0) throw new RuntimeException("last merge in the learning process was not possible");
 						actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge);
 					}

 			        double differenceF = PairQualityLearner.estimationOfDifferenceFmeasure(referenceGraph, actualAutomaton, evaluationTestSet);
 			        double differenceD = PairQualityLearner.estimationOfDifferenceDiffMeasure(referenceGraph, actualAutomaton, initConfiguration.config, ExperimentRunner.getCpuNumber());
 			        System.out.println(new Date().toString()+" _UM: For frame : "+frame+" (classifier "+i+"), long traces f-measure = "+ differenceF+" diffmeasure = "+differenceD);
 					uas_F.add(frame+"_UM",differenceF,"blue");uas_Diff.add(frame+"_UM",differenceD,"blue");gr_diff_to_f.add(differenceF,differenceD);
 	  			}
 	  			progress.next();
 	  		}
  				 */

   			initPTA.storage.writeGraphML("hugegraph.xml");
 			LearnerGraph ptaSmall = LearningSupportRoutines.mergeStatesForUnique(initPTA,uniqueLabel);
   			//Visualiser.updateFrame(initPTA.transform.trimGraph(4, initPTA.getInit()), ptaSmall.transform.trimGraph(4, ptaSmall.getInit()));
   			//Visualiser.waitForKey();
 			StateMergingStatistics redReducer = LearningAlgorithms.ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown.constructReducerIfUsingSiccoScoring(referenceGraph,LearningAlgorithms.ReferenceLearner.OverrideScoringToApply.SCORING_SICCO);
  			{
   	  			final RBoxPlot<Long> gr_PairQuality = new RBoxPlot<Long>("Correct v.s. wrong","%%",new File("percentage_score_huge_ref.pdf"));
   				final Map<Long,TrueFalseCounter> pairQualityCounter = new TreeMap<Long,TrueFalseCounter>();

   				LearningAlgorithms.LearnerThatCanClassifyPairs referenceLearner = new LearningAlgorithms.LearnerThatCanClassifyPairs(initConfiguration, referenceGraph, initPTA,LearningAlgorithms.ReferenceLearner.OverrideScoringToApply.SCORING_SICCO,redReducer);
   				referenceLearner.setPairQualityCounter(pairQualityCounter,referenceGraph,null);
 		        LearnerGraph referenceOutcome = referenceLearner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 		        //referenceOutcome.storage.writeGraphML("resources/"+name+"-ref_"+frame+".xml");
 		        
 		        DifferenceToReference differenceF = DifferenceToReferenceFMeasure.estimationOfDifference(referenceGraph, referenceOutcome, evaluationTestSet);
 		        DifferenceToReference differenceD = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, referenceOutcome, initConfiguration.config, ExperimentRunner.getCpuNumber());
 		        System.out.println(new Date().toString()+" _R: For frame : "+frame+", long traces f-measure = "+ differenceF.getValue()+" diffmeasure = "+differenceD.getValue());
 				uas_F.add(frame+"_R",differenceF.getValue(),"red",null);uas_Diff.add(frame+"_R",differenceD.getValue(),"red",null);gr_diff_to_f.add(differenceF.getValue(),differenceD.getValue());

 				//PairQualityLearner.updateGraph(gr_PairQuality,pairQualityCounter);
 				//gr_PairQuality.drawInteractive(gr);gr_PairQuality.drawPdf(gr);
 			}

   			{
   	  			final RBoxPlot<Long> gr_PairQuality = new RBoxPlot<Long>("Correct v.s. wrong","%%",new File("percentage_score_huge_refM.pdf"));
   				final Map<Long,TrueFalseCounter> pairQualityCounter = new TreeMap<Long,TrueFalseCounter>();

   				LearnerGraph ptaAfterMergingBasedOnUniques = LearningSupportRoutines.mergeStatesForUnique(initPTA,uniqueLabel);
   				LearningAlgorithms.LearnerThatCanClassifyPairs referenceLearner = new LearningAlgorithms.LearnerThatCanClassifyPairs(initConfiguration, referenceGraph, ptaAfterMergingBasedOnUniques,LearningAlgorithms.ReferenceLearner.OverrideScoringToApply.SCORING_SICCO,redReducer);
   				referenceLearner.setPairQualityCounter(pairQualityCounter,referenceGraph,null);
 		        LearnerGraph referenceOutcome = referenceLearner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 		        //referenceOutcome.storage.writeGraphML("resources/"+name+"-ref_"+frame+".xml");
 		        
 		        DifferenceToReference differenceF = DifferenceToReferenceFMeasure.estimationOfDifference(referenceGraph, referenceOutcome, evaluationTestSet);
 		        DifferenceToReference differenceD = DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, referenceOutcome, initConfiguration.config, ExperimentRunner.getCpuNumber());
 		        System.out.println(new Date().toString()+" _R: For frame : "+frame+", long traces f-measure = "+ differenceF+" diffmeasure = "+differenceD);
 				uas_F.add(frame+"_RM",differenceF.getValue(),"red",null);uas_Diff.add(frame+"_RM",differenceD.getValue(),"red",null);gr_diff_to_f.add(differenceF.getValue(),differenceD.getValue());

 				//PairQualityLearner.updateGraph(gr_PairQuality,pairQualityCounter);
 				//gr_PairQuality.drawInteractive(gr);gr_PairQuality.drawPdf(gr);
 			}

 			
 			uas_F.drawInteractive(gr);uas_Diff.drawInteractive(gr);gr_diff_to_f.drawInteractive(gr);
   		}
   		uas_F.reportResults(gr);uas_Diff.reportResults(gr);gr_diff_to_f.reportResults(gr);
 		DrawGraphs.end();// the process will not terminate without it because R has its own internal thread
   }
    	
    /*
    protected static void noveltyInCaseStudyExperiment(String [] args) throws IOException
    {
 		PaperUAS paper = new PaperUAS();
     	paper.learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
         final Configuration learnerConfig = paper.learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
         learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
         learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
         paper.loadReducedConfigurationFile(args[0]);
 >>>>>>> .merge-right.r793
         
        	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
        	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);
        	
        	Visualiser.updateFrame(referenceGraph, null);
        	Visualiser.waitForKey();
    }
    */
    /** Used to training a few different classifiers from a full PTA by comparing metrics on pairs considered by QSM and checking them against the reference solution. */ 
    protected Classifier []loadClassifierFromArff(String arffWithTrainingData)
    {
 		weka.classifiers.trees.REPTree tree =new weka.classifiers.trees.REPTree();tree.setMaxDepth(3); 		
 		tree.setNoPruning(true);// since we only use the tree as a classifier (as a conservative extension of what is currently done) and do not actually look at it, elimination of pruning is not a problem. 
    		// As part of learning, we also prune some of the nodes where the ratio of correctly-classified pairs to those incorrectly classified is comparable.
  		// The significant advantage of not pruning is that the result is no longer sensitive to the order of elements in the tree and hence does not depend on the order in which elements have been obtained by concurrent threads.
 		weka.classifiers.trees.J48 tree48 =new weka.classifiers.trees.J48(); 		
 		tree48.setUnpruned(true);// since we only use the tree as a classifier (as a conservative extension of what is currently done) and do not actually look at it, elimination of pruning is not a problem. 
    		// As part of learning, we also prune some of the nodes where the ratio of correctly-classified pairs to those incorrectly classified is comparable.
   		// The significant advantage of not pruning is that the result is no longer sensitive to the order of elements in the tree and hence does not depend on the order in which elements have been obtained by concurrent threads.
 		weka.classifiers.lazy.IBk ibk = new weka.classifiers.lazy.IBk(1);
 		weka.classifiers.functions.MultilayerPerceptron perceptron = new weka.classifiers.functions.MultilayerPerceptron();
 		Classifier []outcome = new Classifier[]{tree48};//tree};//,tree48,ibk};//,perceptron};
 		for(Classifier c:outcome) trainClassifierFromArff(c,arffWithTrainingData);
 		return outcome;
    }
    
    /** Used to load the classifier from a full PTA by comparing metrics on pairs considered by QSM and checking them against the reference solution. */ 
    protected void trainClassifierFromArff(Classifier classifier,String arffWithTrainingData)
    {
 		Reader arffReader = null;
 		try
 		{
 			arffReader = new FileReader(arffWithTrainingData);
 			Instances trainingData = new Instances(arffReader);
 			if (!"class".equals(trainingData.attribute(trainingData.numAttributes()-1).name()))
 				throw new IllegalArgumentException("last element is not a class");
 			trainingData.setClassIndex(trainingData.numAttributes()-1);
 			
 			classifier.buildClassifier(trainingData);
 		}
 		catch(Exception ex)
 		{// we cannot proceed if this happens because every classifier should be able to both learn and deliver. Throw the exception.
 			Helper.throwUnchecked("failed to train classifier "+classifier.getClass(), ex);
 		}
 		finally
 		{
 			if (arffReader != null)
 				try { arffReader.close(); } catch (IOException e) {
 					// ignore this, we have opened the file for reading hence not much else we can do in terms of cleanup other than doing a close.
 				}
 		}
    }
    /**
     * Given a PTA, this one learns it using the supplied classifier to select pairs. If null, uses QSM learner instead.
     *  
     * @param ifDepth the length of if-chains built from REL metrics.
     * @param initPTA initial PTA
     * @param referenceGraph reference graph to compare to the one learnt.
     * @param c classifier to use
     * @param labelsToMergeTo specific transitions may identify the states they lead to, we could use this to ensure that mergers are consistent with those expectations
     * @param labelsToMergeFrom specific transitions may identify the states they lead from, we could use this to ensure that mergers are consistent with those expectations
     * @return difference between the learnt graph and the reference one.
     */
    public DifferenceToReference learnAndEstimateDifference(int ifDepth,LearnerGraph initPTA, LearnerGraph referenceGraph,Classifier c, final Collection<Label> labelsToMergeTo, final Collection<Label> labelsToMergeFrom)
    {
 		LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(learnerInitConfiguration.ifthenSequences, referenceGraph.pathroutines.computeAlphabet(), learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
 			try {
 			Transform.augmentFromIfThenAutomaton(initPTA, null, ifthenAutomata,learnerInitConfiguration.config.getHowManyStatesToAddFromIFTHEN());
 		} catch (AugmentFromIfThenAutomatonException e) {
 			Helper.throwUnchecked("failed to augment using if-then", e);
 		}// we only need  to augment our PTA once (refer to the explanation above).
 		UseWekaResultsParameters parameters = new UseWekaResultsParameters(new DataCollectorParameters(ifDepth, null,false,DataCollectorParameters.enabledAll())); 	
 		MarkovParameters markovParameters = new MarkovParameters(0, 3,true, 1, true,1,0,1);
     	WekaDataCollector dataCollector = PairQualityLearner.createDataCollector(new DataCollectorParameters(ifDepth,null,false,DataCollectorParameters.enabledAll()), new MarkovHelper(markovParameters), null);
		StateMergingStatistics redReducer = LearningAlgorithms.ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown.constructReducerIfUsingSiccoScoring(
				referenceGraph,LearningAlgorithms.ReferenceLearner.OverrideScoringToApply.SCORING_SICCO);
		ReferenceLearner learner =  c != null? new PairQualityLearner.LearnerThatUsesWekaResults(parameters,learnerInitConfiguration,referenceGraph,c,initPTA, dataCollector, false):
 					new ReferenceLearner(learnerInitConfiguration,initPTA,ReferenceLearner.OverrideScoringToApply.SCORING_SICCO,redReducer);
 			learner.setLabelsLeadingToStatesToBeMerged(labelsToMergeTo);learner.setLabelsLeadingFromStatesToBeMerged(labelsToMergeFrom);learner.setAlphabetUsedForIfThen(referenceGraph.pathroutines.computeAlphabet());
         LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
         
         // Now merge everything that we need to merge
         LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<EquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
 		List<StatePair> pairsList = LearningSupportRoutines.buildVerticesToMerge(actualAutomaton,learner.getLabelsLeadingToStatesToBeMerged(),learner.getLabelsLeadingFromStatesToBeMerged());
 		if (!pairsList.isEmpty())
 		{
 			int score = actualAutomaton.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge, true);
 			if (score < 0) throw new RuntimeException("last merge in the learning process was not possible");
 			actualAutomaton = MergeStates.mergeCollectionOfVertices(actualAutomaton, null, verticesToMerge, null, true);
 		}
        	LearnerGraph learntGraph = new LearnerGraph(learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(actualAutomaton,learntGraph);
        System.out.println("state number: "+referenceGraph.getStateNumber()+" for reference and "+learntGraph.getStateNumber()+" for the actual one");
 		GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
 		DirectedSparseGraph gr = gd.showGD(
 					learntGraph,referenceGraph,
 					ExperimentRunner.getCpuNumber());
 			//Visualiser.updateFrame(gr,null);Visualiser.waitForKey();
        return DifferenceToReferenceDiff.estimationOfDifferenceDiffMeasure(referenceGraph, learntGraph, learnerInitConfiguration.config,1);
    }
    
     public void runExperimentWithSmallAutomata(final int ifDepth, final String arffName, final LearnerGraph referenceGraph) throws IOException
     {
  	   //final Collection<List<Label>> evaluationTestSet = computeEvaluationSet(referenceGraph);
        
 		// Here I need to moderate the effort because choosing traces for all seeds is good but I need
 		// that many times more traces, so I have to create a graph in terms of effort v.s. quailty (or even better, scale
 		// the existing one).
 		final DrawGraphs gr = new DrawGraphs();

 		final RBoxPlot<Pair<Integer,String>> 
 			uas_outcome = new RBoxPlot<Pair<Integer,String>>("Time","f-measure",new File("time_f.pdf"));
 		final RBoxPlot<String>
 					uas_A=new RBoxPlot<String>("Time","f-measure",new File("time_A_f.pdf")),
 							uas_S=new RBoxPlot<String>("Time","f-measure",new File("time_S_f.pdf")),
 									uas_U=new RBoxPlot<String>("Time","f-measure",new File("time_U_f.pdf"))
 			;
 		final RBoxPlot<Integer>
 			uas_threshold=new RBoxPlot<Integer>("Threshold","f-measure",new File("threshold_f.pdf"));
 		final Set<Integer> allFrames = collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).keySet();
 		
 		/** The runner of computational threads. */
 		int threadNumber = ExperimentRunner.getCpuNumber();
 		ExecutorService executorService = Executors.newFixedThreadPool(threadNumber);
 		
 		try
 		{
 			List<Future<?>> outcomes = new LinkedList<Future<?>>();
 			System.out.println(allFrames);
 			for(final Integer frame:allFrames)
 			{
 				Runnable interactiveRunner = new Runnable() {

 					@Override
 					public void run() 
 					{/*
 						final Classifier classifiers[] = loadClassifierFromArff(arffName);
 						for(Classifier c:classifiers)
 						{
 							{
 					  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));
 					  			DifferenceToReference difference = learnAndEstimateDifference(ifDepth, initPTA,referenceGraph,c,Collections.<Label>emptyList(),Collections.<Label>emptyList());
 	
 						        uas_outcome.add(new Pair<Integer,String>(frame,"S"),difference.getValue());
 								uas_S.add(frame+"C",difference.getValue());
 							}
 		
 							{
 								final Collection<Label> labelsToMergeTo=Collections.emptyList(), labelsToMergeFrom=Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter())});
 					  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));
 					  			DifferenceToReference difference = learnAndEstimateDifference(ifDepth, initPTA,referenceGraph,c,labelsToMergeTo,labelsToMergeFrom);
 	
 						        uas_outcome.add(new Pair<Integer,String>(frame,"S"),difference.getValue());
 								uas_S.add(frame+"CM",difference.getValue());								
 							}

 							{
 								final Collection<Label> labelsToMergeTo=Collections.emptyList(), labelsToMergeFrom=Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", learnerInitConfiguration.config,learnerInitConfiguration.getLabelConverter())});
 					  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));

 								List<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new ArrayList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>(1000000);
 								List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(initPTA,labelsToMergeTo,labelsToMergeFrom);
 								int mergeScore = initPTA.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge);
 								assert mergeScore >= 0:"initial PTA is inconsistent with the expectation that transitions lead to an initial state";
 					  			initPTA = MergeStates.mergeCollectionOfVertices(initPTA, null, verticesToMerge);verticesToMerge = null;
 					  			initPTA.pathroutines.updateDepthLabelling();
 					  			DifferenceToReference difference = learnAndEstimateDifference(ifDepth, initPTA,referenceGraph,null,Collections.<Label>emptyList(),Collections.<Label>emptyList());
 						        uas_outcome.add(new Pair<Integer,String>(frame,"T"),difference.getValue());
 								uas_S.add(frame+"T",difference.getValue());								
 							}
 						}
 					 */
 			  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(frame));
 			  			DifferenceToReference difference = learnAndEstimateDifference(ifDepth, initPTA,referenceGraph,null,Collections.<Label>emptyList(),Collections.<Label>emptyList());
 			  			uas_S.add(frame+"R",difference.getValue());
 			  			synchronized(gr)
 			  			{
 			  				uas_S.drawInteractive(gr);
 			  			}
 					}
 				};
 				outcomes.add(executorService.submit(interactiveRunner));
 			}
 				
 				/*
 			for(final String seed:collectionOfTraces.keySet())
 				if (!seed.equals(UAVAllSeeds))
 				{// Just for all frames of the a single seed
 					interactiveRunner = new Runnable() {

 						@Override
 						public void run() {
 							final Classifier classifiers[] = loadClassifierFromArff(arffName);
 							for(final Integer frame:allFrames)
 							{
 								TracesForSeed tracesForThisSeed = collectionOfTraces.get(seed);
 								
 								for(Classifier c:classifiers)
 								{
 						  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(tracesForThisSeed.tracesForUAVandFrame.get(UAVAll).get(frame));
 						  			double difference = learnAndEstimateDifference(initPTA,referenceGraph,c,labelsToMergeTo,labelsToMergeFrom);

 						  			uas_outcome.add(new Pair<Integer,String>(frame,"A"),difference);
 									uas_A.add(""+frame,difference);
 								}

 								LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(tracesForThisSeed.tracesForUAVandFrame.get(UAVAll).get(frame));
 					  			double difference = learnAndEstimateDifference(initPTA,referenceGraph,null,labelsToMergeTo,labelsToMergeFrom);
 					  			uas_A.add(""+frame+"R",difference);
 							}
 						}
 						
 					};
 					outcomes.add(executorService.submit(interactiveRunner));
 				}
 			for(final String UAV:collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.keySet())
 				if (!UAV.equals(UAVAllSeeds) && !UAV.equals(UAVAll))
 					for(final String seed:collectionOfTraces.keySet())
 						if (!seed.equals(UAVAllSeeds))
 						{
 							interactiveRunner = new Runnable() {

 								@Override
 								public void run() {
 									final Classifier classifiers[] = loadClassifierFromArff(arffName);
 									for(final Integer frame:allFrames)
 									{
 										for(Classifier c:classifiers)
 										{
 								  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(seed).tracesForUAVandFrame.get(UAV).get(frame));
 								  			double difference = learnAndEstimateDifference(initPTA,referenceGraph,c,labelsToMergeTo,labelsToMergeFrom);
 											uas_outcome.add(new Pair<Integer,String>(frame,"U"),difference);
 											uas_U.add(""+frame,difference);
 										}
 							  			LearnerGraph initPTA = new LearnerGraph(learnerInitConfiguration.config);initPTA.paths.augmentPTA(collectionOfTraces.get(seed).tracesForUAVandFrame.get(UAV).get(frame));
 							  			double difference = learnAndEstimateDifference(initPTA,referenceGraph,null,labelsToMergeTo,labelsToMergeFrom);
 							  			uas_U.add(""+frame+"R",difference);
 									}
 								}
 							};
 							outcomes.add(executorService.submit(interactiveRunner));
 						}
 			/*
 			for(int i=0;i<5;++i)
 			{
 				final int arg=i;
 				Runnable interactiveRunner = new Runnable() {

 					@Override
 					public void run() {
 						for(Classifier c:classifiers)
 						{
 				  			LearnerGraph initPTAWithNegatives = new LearnerGraph(learnerInitConfiguration.config);initPTAWithNegatives.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber));
 				  			Configuration tmpConf = learnerInitConfiguration.config.copy();tmpConf.setGeneralisationThreshold(arg);
 				  			LearnerGraph initPTA = new LearnerGraph(tmpConf);
 				  			AbstractPathRoutines.removeRejectStates(initPTAWithNegatives,initPTA);
 				  			LearnerThatUsesWekaResults learner = new LearnerThatUsesWekaResults(learnerInitConfiguration,referenceGraph,c,initPTA);
 				  			learner.setLabelsLeadingToStatesToBeMerged(labelsToMergeA);learner.setLabelsLeadingFromStatesToBeMerged(labelsToMergeB);learner.setAlphabetUsedForIfThen(alphabetForIfThen);
 				 	        final LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 					        double difference = PairQualityLearner.estimationOfDifferenceFmeasure(referenceGraph, actualAutomaton, evaluationTestSet);
 							uas_threshold.add(arg, difference);
 						}
 					}
 					
 				};
 					outcomes.add(executorService.submit(interactiveRunner));
 			}
 */
 			ProgressIndicator progress = new ProgressIndicator("running concurrent experiment",outcomes.size());
 			for(Future<?> task:outcomes) { task.get();progress.next(); }// wait for termination of all tasks
 		}
 		catch(Exception ex)
 		{
 			Helper.throwUnchecked("failed to run experiment", ex);
 		}
 		finally
 		{
 			if (executorService != null) executorService.shutdown();
 		}
 		
 		uas_outcome.reportResults(gr);uas_A.reportResults(gr);uas_S.reportResults(gr);uas_U.reportResults(gr);
 		uas_threshold.reportResults(gr);
 		DrawGraphs.end();// the process will not terminate without it because R has its own internal thread
     }
     
 	/**
 	 * @param args trace file to load.
      * @throws IOException 
 	 */
 	public static void mainSingleHugeAutomaton(String[] args) throws Exception 
 	{
 		UASPairQuality paper = new UASPairQuality();
     	paper.learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
         final Configuration learnerConfig = paper.learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
         learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
         learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
         paper.loadReducedConfigurationFile(args[0]);
         
 		final int offset=1;
     	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
     	int maxFrame = paper.getMaxFrame(inputFiles);
     	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
     	for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]);// refill the input (it was drained by the computation of maxFrame).
     	paper.loadDataByConcatenation(inputFiles);
        	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
        	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);

        	String arffName = "resources/largePTA/pairsEncounteredHuge.arff";
        	final int ifDepth = 1;
        	//paper.writeArff(ifDepth, referenceGraph,arffName);// this part can be skipped if arff has already been generated.
     	paper.runExperimentWithSingleAutomaton("huge",arffName,referenceGraph);
 		/*
     	Label uniqueLabel = AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", paper.learnerInitConfiguration.config,paper.learnerInitConfiguration.getLabelConverter());
 	    LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
 		initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
  		LearnerGraph smallPta = PairQualityLearner.mergeStatesForUnique(initialPTA,uniqueLabel);
        	PaperUAS.computePTASize("small pta: ",smallPta,referenceGraph);PaperUAS.computePTASize("huge pta: ",initialPTA,referenceGraph);
        	*/
 	}
 		
 	/**
 	 * @param args trace file to load.
      * @throws IOException 
 	 */
 	public static void mainSmallAutomata(String[] args) throws Exception 
 	{
 		UASPairQuality paper = new UASPairQuality();
 		paper.learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
         final Configuration learnerConfig = paper.learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
         learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
         learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
         learnerConfig.setLearnerScoreMode(Configuration.ScoreMode.GENERAL);
         paper.loadReducedConfigurationFile(args[0]);
         
 		final int offset=1;
     	Reader []inputFiles = new Reader[args.length-offset];for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]); 
     	int maxFrame = paper.getMaxFrame(inputFiles);
     	paper.divisor = (maxFrame+1)/10;// the +1 ensures that the last class of frames includes the last point.
     	for(int i=offset;i<args.length;++i) inputFiles[i-offset]=new FileReader(args[i]);// refill the input (it was drained by the computation of maxFrame).
     	paper.loadData(inputFiles);
        	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
        	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);
     	
 		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");

        	
     	String arffName = "resources/largePTA/pairsEncounteredPartiallyMerged.arff";
     	/*

 	    LearnerGraph initialPTA = new LearnerGraph(paper.learnerInitConfiguration.config);
 		initialPTA.paths.augmentPTA(paper.collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(paper.maxFrameNumber));
 		System.out.println(initialPTA.getAcceptStateNumber()+", total: "+initialPTA.getStateNumber());
 		LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(paper.learnerInitConfiguration.ifthenSequences, null, referenceGraph, paper.learnerInitConfiguration.config, paper.learnerInitConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
 		Transform.augmentFromIfThenAutomaton(initialPTA, null, ifthenAutomata,paper.learnerInitConfiguration.config.getHowManyStatesToAddFromIFTHEN());// we only need  to augment our PTA once (refer to the explanation above).
 		System.out.println("After if-then, "+initialPTA.getAcceptStateNumber()+", total: "+initialPTA.getStateNumber());
 		System.out.println(initialPTA.getAcceptStateNumber()+", total: "+initialPTA.getStateNumber());
  	   LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> verticesToMerge = new LinkedList<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>>();
  		System.out.println("constructing vertices to merge");
  		List<StatePair> pairsList = LearnerThatCanClassifyPairs.buildVerticesToMerge(initialPTA,Collections.<Label>emptyList(),
  				Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", paper.learnerInitConfiguration.config,paper.learnerInitConfiguration.getLabelConverter())}));
  		if (initialPTA.pairscores.computePairCompatibilityScore_general(null, pairsList, verticesToMerge) < 0)
  			throw new IllegalArgumentException("inconsistent initial PTA: vertices that lead to unique state in the reference graph cannot be merged in the PTA");
  		System.out.println("done attempt to merge, everything ok");
  		PairQualityLearner.ReferenceLearner learner = new PairQualityLearner.ReferenceLearner(null,paper.learnerInitConfiguration,referenceGraph,initialPTA);
         final LearnerGraph actualAutomaton = learner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
         DifferentFSMException different = WMethod.checkM(referenceGraph, actualAutomaton);
  		if (different != null)
  			throw different;
 		 */
         final int ifDepth = 0;
     	paper.writeArff(ifDepth, referenceGraph,arffName);
     	paper.runExperimentWithSmallAutomata(ifDepth, arffName,referenceGraph);
     			//Arrays.asList(new Label[]{AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", paper.learnerInitConfiguration.config,paper.learnerInitConfiguration.getLabelConverter())}));
 	}

 	/**
 	 * @param args trace file to load.
      * @throws IOException 
 	 */
 	public static void main(String[] args) throws Exception 
 	{
 		try
 		{
 			//checkSmallPTA();
 			//checkDataConsistency();
 			//mainCheckMerging(args);
 	        //mainSingleHugeAutomaton(args);
 			//noveltyInCaseStudyExperiment(args);
 			mainSmallAutomata(args);
 	       /*
 			PaperUAS paper = new PaperUAS();
 			paper.learnerInitConfiguration.setLabelConverter(new Transform.InternStringLabel());
 	        final Configuration learnerConfig = paper.learnerInitConfiguration.config;learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
 	        learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
 	        learnerConfig.setAskQuestions(false);learnerConfig.setDebugMode(false);
 	       	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
 	       	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);
 	       	Visualiser.updateFrame(referenceGraph, null);Visualiser.waitForKey();
 	       	*/
 		}
 		finally
 		{
 			DrawGraphs.end();
 		}
 	}

 	/**
 	 * @param args trace file to load.
      * @throws IOException 
 	 */
 	public static void generateHugeArff(final int ifDepth, @SuppressWarnings("unused") String[] args) throws Exception 
 	{
 		final Configuration learnerConfig = Configuration.getDefaultConfiguration().copy();learnerConfig.setGeneralisationThreshold(0);learnerConfig.setGdFailOnDuplicateNames(false);
         learnerConfig.setGdLowToHighRatio(0.75);learnerConfig.setGdKeyPairThreshold(0.5);learnerConfig.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
         learnerConfig.setAskQuestions(false);
         
    	final InitialConfigurationAndData initialConfigAndData = PairQualityLearner.loadInitialAndPopulateInitialConfiguration(PairQualityLearner.largePTAFileName, learnerConfig, new Transform.InternStringLabel());

 		LearnerGraph referenceGraph = new LearnerGraph(initialConfigAndData.initial.graph.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraph, initialConfigAndData.learnerInitConfiguration.getLabelConverter());
		MarkovParameters markovParameters = new MarkovParameters(0, 3,true, 1, true,1,0,1);
     	WekaDataCollector dataCollector = PairQualityLearner.createDataCollector(new DataCollectorParameters(ifDepth,null,false,DataCollectorParameters.enabledAll()), new MarkovHelper(markovParameters), null);
     	LearnerThatCanClassifyPairs learnerOfPairs = new PairQualityLearner.LearnerThatUpdatesWekaResults(initialConfigAndData.learnerInitConfiguration,referenceGraph,dataCollector,initialConfigAndData.initial.graph,dataCollector.markovHelper, null);
 		learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
 		
 		FileWriter wekaInstances=new FileWriter("resources/largePTA/pairsEncountered3.arff");
 		wekaInstances.write(dataCollector.trainingData.toString());
 		wekaInstances.close();
 	}

 	public LearnerGraph writeArff(final int ifDepth, LearnerGraph referenceGraph, String whereToWrite) throws Exception
	{
		
		LearnerEvaluationConfiguration initConfiguration = new LearnerEvaluationConfiguration(learnerInitConfiguration.config);
		initConfiguration.setLabelConverter(learnerInitConfiguration.getLabelConverter());// we do not copy if-then automata here because we do not wish to augment from if-then on every iteration because our properties are pairwise and this permits augmentation to be carried out first thing and not any more.
	    LearnerGraph initialPTA = new LearnerGraph(initConfiguration.config);
		initialPTA.paths.augmentPTA(collectionOfTraces.get(UAVAllSeeds).tracesForUAVandFrame.get(UAVAllSeeds).get(maxFrameNumber));
		/*
		try {
			initialPTA.storage.writeGraphML("resources/automaton_to_learn_arff_from.xml");
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		*/
		LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(learnerInitConfiguration.ifthenSequences, referenceGraph.pathroutines.computeAlphabet(), initConfiguration.config, initConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
		initConfiguration.config.setUseConstraints(false);// do not use if-then during learning (refer to the explanation above)
		
		System.out.println(new Date().toString()+" Graph loaded: "+initialPTA.getStateNumber()+" states, adding at most "+ initConfiguration.config.getHowManyStatesToAddFromIFTHEN()+" if-then states");
		Transform.augmentFromIfThenAutomaton(initialPTA, null, ifthenAutomata, initConfiguration.config.getHowManyStatesToAddFromIFTHEN());// we only need  to augment our PTA once (refer to the explanation above).
		System.out.println(new Date().toString()+" if-then states added, now "+initialPTA.getStateNumber()+" states");
		MarkovParameters markovParameters = new MarkovParameters(0, 3,true, 1, true,1,0,1);
     	WekaDataCollector dataCollector = PairQualityLearner.createDataCollector(new DataCollectorParameters(ifDepth,null,false,DataCollectorParameters.enabledAll()), new MarkovHelper(markovParameters), null);
		// Run the learner that will find out how to select the correct pairs.
		LearnerThatCanClassifyPairs learnerOfPairs = new PairQualityLearner.LearnerThatUpdatesWekaResults(initConfiguration,referenceGraph,dataCollector,initialPTA,dataCollector.markovHelper, null);
		LearnerGraph actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		
		// final weka.classifiers.trees.J48 classifier = new weka.classifiers.trees.J48();
		FileWriter wekaInstances= null;
		try
		{
			wekaInstances = new FileWriter(whereToWrite);
			wekaInstances.write(dataCollector.trainingData.toString());
			wekaInstances.close();
		}
		catch(Exception ex)
		{
			Helper.throwUnchecked("failed to create a file with training data for "+whereToWrite, ex);
		}
		finally
		{
			if (wekaInstances != null)
				try {
					wekaInstances.close();
				} catch (IOException e) {
					// ignore this, we are not proceeding anyway due to an earlier exception so whether the file was actually written does not matter
				}
		}
		
		// the learning of the pairs experiment should always produce the correct outcome since decisions to merge pairs are guaranteed to be correct.
       	LearnerGraph learntWithoutNegatives = new LearnerGraph(learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(actualAutomaton,learntWithoutNegatives);
		DifferentFSMException diff = WMethod.checkM(referenceGraph, learntWithoutNegatives);if (diff != null) throw diff;
		System.out.println(new Date().toString()+" arff written");
		return actualAutomaton;
	}
 	
}
