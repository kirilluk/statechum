/* Copyright (c) 2011 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
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
package statechum.analysis.learning.experiments.mutation;

import java.io.File;
import java.util.Collection;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.Helper;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Pair;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.RBagPlot;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.GD;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.GD.ChangesCounter;
import statechum.analysis.learning.rpnicore.TestGD;
import statechum.analysis.learning.rpnicore.TestGD.ProgressIndicator;
import statechum.model.testset.PTASequenceEngine;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ForestFireLabelledStateMachineGenerator;
import statechum.analysis.learning.experiments.mutation.ExperimentResult.DOUBLE_V;
import statechum.analysis.learning.experiments.mutation.ExperimentResult.LONG_V;
import statechum.analysis.learning.experiments.mutation.GraphMutator.ChangesRecorderAsCollectionOfTransitions;

public class DiffExperiments {
	
	
	Configuration config = Configuration.getDefaultConfiguration().copy();
	boolean skipLanguage = false;
	final int experimentsPerMutationCategory, mutationStages = 5, graphComplexityMax = 6;

	public static void main(String[] args){
		try
		{
			DiffExperiments exp = new DiffExperiments(30);
			exp.runExperiment(20, false);
		}
		finally
		{
			DrawGraphs.end();
		}
	}
	
	public DiffExperiments(int experimentsPerCategoryArg){
		experimentsPerMutationCategory = experimentsPerCategoryArg;
		//config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);
		//config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		
	}

	public void runExperiment(int initStates, boolean skip){
		DrawGraphs gr = new DrawGraphs();
		this.skipLanguage = skip;
		Random r = new Random(0);

		RBoxPlot<Integer> 
			gr_Diff_States = new RBoxPlot<Integer>("States","Diff/Mutations",new File("diff_states.pdf")),
			gr_W_States = new RBoxPlot<Integer>("States","Language-based f measure, using W",new File("w_states.pdf")),
			gr_Pairs_States = new RBoxPlot<Integer>("States","Key pairs: mismatched/total",new File("pairs_states.pdf")),
			gr_TimeDiff_States = new RBoxPlot<Integer>("States","Time taken by PLTSDiff",new File("TimeDiff_states.pdf")),
			gr_TimeW_States = new RBoxPlot<Integer>("States","Time taken by W",new File("TimeW_states.pdf")),
			gr_TimeRand_States = new RBoxPlot<Integer>("States","Time taken by Rand",new File("TimeRand_states.pdf"));
		RBoxPlot<Pair<Integer,Integer>> 
			gr_MutationsToOriginal_StatesLevel = new RBoxPlot<Pair<Integer,Integer>>("Mutation level,States","Mutations/Original Edges",new File("mutations_states.pdf")),
			gr_DiffGD_StatesLevel =new RBoxPlot<Pair<Integer,Integer>>("Mutation level,States","patch size",new File("diff_statesmutations.pdf")),
			gr_DiffW_StatesLevel =new RBoxPlot<Pair<Integer,Integer>>("Mutation level,States","Language-based f measure, using W",new File("w_statesmutations.pdf")),
			gr_DiffRand_StatesLevel =new RBoxPlot<Pair<Integer,Integer>>("Mutation level,States","Language-based f measure, using Rand",new File("rand_statesmutations.pdf")),
			gr_MismatchedPairs =new RBoxPlot<Pair<Integer,Integer>>("Mutation level,States","Mismatched key pairs",new File("pairs_statesmutations.pdf")),
			gr_TimeDiff_StatesLevel = new RBoxPlot<Pair<Integer,Integer>>("Mutation level,States","Time taken by PLTSDiff",new File("TimeDiff_statesmutations.pdf")),
			gr_TimeW_StatesLevel = new RBoxPlot<Pair<Integer,Integer>>("Mutation level,States","Time taken by W",new File("TimeW_statesmutations.pdf")),
			gr_TimeRand_StatesLevel = new RBoxPlot<Pair<Integer,Integer>>("Mutation level,States","Time taken by Random",new File("TimeRand_statesmutations.pdf"));
		RBagPlot gr_Diff_MutationsToOriginal = new RBagPlot("Mutations/Original Edges","patch size",new File("diff_mutations.pdf")),
			gr_Diff_W = new RBagPlot("W f-measure","patch size",new File("diff_w.pdf")),
			gr_Rand_W = new RBagPlot("W f-measure","Rand f-measure",new File("rand_W.pdf")),
			gr_Diff_MismatchedPairs = new RBagPlot("Mismatched key pairs","Mutations/Original Edges",new File("diff_pairs.pdf"));
		
		// Useful values of the threshold
		double pairThreshold[] = new double[]{0.2,0.4,0.6,0.8,0.95},lowToHigh[] = new double[]{0.2,0.4,0.6,0.8,0.95};
		RBoxPlot<Double> gr_Diff_threshold = new RBoxPlot<Double>("Threshold values, Low to High ratio = 0.5","ratio of patch size to mutations",new File("diff_threshold.pdf")),
			gr_Diff_lowtohigh = new RBoxPlot<Double>("Low to High values, threshold ratio = 0.7","ratio of patch size to mutations",new File("diff_lowtohigh.pdf"));
		RBoxPlot<Pair<Double,Double>> gr_Diff_thresholdslowhigh = new RBoxPlot<Pair<Double,Double>>("Low to High ratio, Threshold","ratio of patch size to mutations",new File("diff_thresholdlowhigh.pdf"));
		RBoxPlot<Double> gr_Diff_k = new RBoxPlot<Double>("attenuation factor","ratio of patch size to mutations",new File("diff_attenuation.pdf"));
		
		for(int graphComplexity=0;graphComplexity < graphComplexityMax;graphComplexity++)
		{
			int states=initStates+graphComplexity*50;
			int alphabet = states/2;
			
			MachineGenerator mg = new MachineGenerator(states, 40, states/6);
			int mutationsPerStage = (states/2) / 2;
			//System.out.print("\n"+states+": ");
			TestGD.ProgressIndicator progress = new ProgressIndicator(""+states, mutationStages*experimentsPerMutationCategory);
			for(int mutationStage = 0;mutationStage<mutationStages;mutationStage++)
			{
				for(int experiment=0;experiment<experimentsPerMutationCategory;experiment++)
				{
					ExperimentResult outcome = new ExperimentResult();
					while(!outcome.experimentValid)
					{
						int mutations = mutationsPerStage * (mutationStage+1);
						LearnerGraphND origGraph = mg.nextMachine(alphabet, experiment);
						GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> mutator = new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(origGraph,r);
						mutator.mutate(mutations);
						LearnerGraphND origAfterRenaming = new LearnerGraphND(origGraph.config);
						Map<CmpVertex,CmpVertex> origToNew = copyStatesAndTransitions(origGraph,origAfterRenaming);
						LearnerGraphND mutated = (LearnerGraphND)mutator.getMutated();
						Set<Transition> appliedMutations = new HashSet<Transition>();
						for(Transition tr:mutator.getDiff())
						{
							CmpVertex renamedFrom = origToNew.get(tr.getFrom());if (renamedFrom == null) renamedFrom = tr.getFrom();
							CmpVertex renamedTo = origToNew.get(tr.getTo());if (renamedTo == null) renamedTo = tr.getTo();
							appliedMutations.add(new Transition(renamedFrom,renamedTo,tr.getLabel()));
						}
						
						final double perfectLowToHigh=0.7,perfectThreshold=0.5;
						
						// These experiments are only run for the maximal number of states
						if (graphComplexity == graphComplexityMax-1)
						{
							for(double k:new double[]{0,.2,.4,.6,.8,.95})
							{
								config.setAttenuationK(k);
								config.setGdKeyPairThreshold(0.5);
								config.setGdLowToHighRatio(perfectLowToHigh);
								config.setGdPropagateDet(false);// this is to ensure that if we removed a transition 0 from to a state and then added one from that state to a different one, det-propagation will not force the two very different states into a key-pair relation. 
								linearDiff(origAfterRenaming,mutated, appliedMutations,outcome);
								
								gr_Diff_k.add(k, outcome.getValue(DOUBLE_V.OBTAINED_TO_EXPECTED));
								
							}
							
							for(double threshold:new double[]{0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95})
							{
								config.setGdKeyPairThreshold(threshold);
								config.setGdLowToHighRatio(perfectLowToHigh);
								config.setGdPropagateDet(false);// this is to ensure that if we removed a transition 0 from to a state and then added one from that state to a different one, det-propagation will not force the two very different states into a key-pair relation. 
								linearDiff(origAfterRenaming,mutated, appliedMutations,outcome);
								
								gr_Diff_threshold.add(threshold, outcome.getValue(DOUBLE_V.OBTAINED_TO_EXPECTED));
							}
							
							
							
							for(double lowtohigh:new double[]{0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95})
							{
								config.setGdKeyPairThreshold(perfectThreshold);
								config.setGdLowToHighRatio(lowtohigh);
								config.setGdPropagateDet(false);// this is to ensure that if we removed a transition 0 from to a state and then added one from that state to a different one, det-propagation will not force the two very different states into a key-pair relation. 
								linearDiff(origAfterRenaming,mutated, appliedMutations,outcome);
								
								gr_Diff_lowtohigh.add(lowtohigh, outcome.getValue(DOUBLE_V.OBTAINED_TO_EXPECTED));
							}
							
							
							for(int i=0;i<pairThreshold.length;++i)
								for(double ratio:lowToHigh)
								{
									config.setGdKeyPairThreshold(pairThreshold[i]);
									config.setGdLowToHighRatio(ratio);
									config.setGdPropagateDet(false);// this is to ensure that if we removed a transition 0 from to a state and then added one from that state to a different one, det-propagation will not force the two very different states into a key-pair relation. 
									linearDiff(origAfterRenaming,mutated, appliedMutations,outcome);
									
									gr_Diff_thresholdslowhigh.add(new Pair<Double,Double>(ratio,pairThreshold[i]), outcome.getValue(DOUBLE_V.OBTAINED_TO_EXPECTED));
								}
						}
						config.setAttenuationK(0.5);
						config.setGdKeyPairThreshold(perfectThreshold);
						config.setGdLowToHighRatio(perfectLowToHigh);
						config.setGdPropagateDet(false);// this is to ensure that if we removed a transition 0 from to a state and then added one from that state to a different one, det-propagation will not force the two very different states into a key-pair relation. 
						linearDiff(origAfterRenaming,mutated, appliedMutations,outcome);
						
						if (!skip)
						{
							LearnerGraph fromDet = null, toDet = null;
							try {
								fromDet = mergeAndDeterminize(origAfterRenaming);
								toDet = mergeAndDeterminize(mutated);
							} catch (IncompatibleStatesException e) {
								Helper.throwUnchecked("failed to build a deterministic graph from a nondet one", e);
							}
							languageDiff(fromDet,toDet,states, graphComplexity,outcome);
						}
						outcome.experimentValid = true;
						progress.next();
						gr_Diff_States.add(states, outcome.getValue(DOUBLE_V.OBTAINED_TO_EXPECTED));
						gr_W_States.add(states,outcome.getValue(DOUBLE_V.ACCURACY_W));
						Pair<Integer,Integer> mutations_states = new Pair<Integer,Integer>(mutationStage+1,states);
						Pair<Integer,Integer> states_mutations = new Pair<Integer,Integer>(states,mutationStage+1);
						gr_DiffGD_StatesLevel.add(mutations_states, outcome.getValue(DOUBLE_V.OBTAINED_TO_EXPECTED));
						gr_DiffW_StatesLevel.add(mutations_states, outcome.getValue(DOUBLE_V.ACCURACY_W));
						gr_DiffRand_StatesLevel.add(mutations_states, outcome.getValue(DOUBLE_V.ACCURACY_W));
						gr_MutationsToOriginal_StatesLevel.add(mutations_states, outcome.getValue(DOUBLE_V.MUTATIONS_TO_TRANSITIONS));
						gr_DiffRand_StatesLevel.add(mutations_states, outcome.getValue(DOUBLE_V.ACCURACY_W));
						gr_MismatchedPairs.add(states_mutations,outcome.getValue(DOUBLE_V.MISMATCHED_KEYPAIRS));
						gr_Pairs_States.add(states,outcome.getValue(DOUBLE_V.MISMATCHED_KEYPAIRS));
						
						// Time taken
						long ns = 1000000L;
						gr_TimeDiff_StatesLevel.add(states_mutations, (double)(outcome.getValue(LONG_V.DURATION_GD)/ns));
						gr_TimeW_StatesLevel.add(states_mutations, (double)(outcome.getValue(LONG_V.DURATION_W)/ns));
						gr_TimeRand_StatesLevel.add(states_mutations, (double)(outcome.getValue(LONG_V.DURATION_RAND)/ns));
						gr_TimeDiff_States.add(states,(double)(outcome.getValue(LONG_V.DURATION_GD)/ns));
						gr_TimeW_States.add(states,(double)(outcome.getValue(LONG_V.DURATION_W)/ns));
						gr_TimeRand_States.add(states,(double)(outcome.getValue(LONG_V.DURATION_RAND)/ns));
						
						gr_Diff_MutationsToOriginal.add(outcome.getValue(DOUBLE_V.MUTATIONS_TO_TRANSITIONS),outcome.getValue(DOUBLE_V.OBTAINED_TO_EXPECTED));
						gr_Diff_W.add(outcome.getValue(DOUBLE_V.ACCURACY_W),outcome.getValue(DOUBLE_V.OBTAINED_TO_EXPECTED));
						gr_Rand_W.add(outcome.getValue(DOUBLE_V.ACCURACY_W),outcome.getValue(DOUBLE_V.ACCURACY_RAND));
						gr_Diff_MismatchedPairs.add(outcome.getValue(DOUBLE_V.MISMATCHED_KEYPAIRS),outcome.getValue(DOUBLE_V.MUTATIONS_TO_TRANSITIONS));
					}

				}
				// finished doing this mutation stage, update graphs
				gr_Diff_States.drawInteractive(gr);gr_MutationsToOriginal_StatesLevel.drawInteractive(gr);					
				//gr_Diff_thresholds.drawInteractive(gr);
				//gr_Diff_k.drawInteractive(gr);
				gr_DiffGD_StatesLevel.drawInteractive(gr);gr_DiffW_StatesLevel.drawInteractive(gr);gr_DiffRand_StatesLevel.drawInteractive(gr);
				gr_Diff_MutationsToOriginal.drawInteractive(gr);
				gr_W_States.drawInteractive(gr);
				
				gr_Diff_W.drawInteractive(gr);
				gr_Rand_W.drawInteractive(gr);
				
				//gr_TimeDiff_StatesLevel.drawInteractive(gr);gr_TimeW_StatesLevel.drawInteractive(gr);gr_TimeRand_StatesLevel.drawInteractive(gr);
				//gr_MismatchedPairs.drawInteractive(gr);
				gr_Pairs_States.drawInteractive(gr);gr_TimeDiff_States.drawInteractive(gr);gr_TimeRand_States.drawInteractive(gr);gr_TimeW_States.drawInteractive(gr);
				gr_Diff_MismatchedPairs.drawInteractive(gr);
				
				//ExperimentResult average = getAverage(accuracyStruct,graphComplexity,mutationStage);
				//arrayWithDiffResults.add(diffValues);arrayWithKeyPairsResults.add(keyPairsValues);
				//arrayWithResultNames.add(""+states+"-"+Math.round(100*average.mutationsToTransitions)+"%");
				//System.out.print("["+average+"]");
				
				
				//List<String> names=arrayWithResultNames.size()>1?arrayWithResultNames:null;
			}

		}
		gr_Diff_States.drawPdf(gr);gr_W_States.drawPdf(gr);gr_MutationsToOriginal_StatesLevel.drawPdf(gr);
		gr_Diff_threshold.drawPdf(gr);gr_Diff_lowtohigh.drawPdf(gr);gr_Diff_thresholdslowhigh.drawPdf(gr);gr_Diff_k.drawPdf(gr);
		gr_Diff_MutationsToOriginal.drawPdf(gr);
		gr_DiffGD_StatesLevel.drawPdf(gr);gr_DiffW_StatesLevel.drawPdf(gr);gr_DiffRand_StatesLevel.drawPdf(gr);
		gr_Diff_W.drawPdf(gr);gr_Rand_W.drawPdf(gr);
		gr_MismatchedPairs.drawPdf(gr);gr_Diff_MismatchedPairs.drawPdf(gr);
		
		gr_TimeDiff_StatesLevel.drawPdf(gr);gr_TimeW_StatesLevel.drawPdf(gr);gr_TimeRand_StatesLevel.drawPdf(gr);
		gr_Pairs_States.drawPdf(gr);gr_TimeDiff_States.drawPdf(gr);gr_TimeRand_States.drawPdf(gr);gr_TimeW_States.drawPdf(gr);
	}
	

	private LearnerGraph mergeAndDeterminize(LearnerGraphND from) throws IncompatibleStatesException {
		LearnerGraph eval = null;
		eval = from.pathroutines.buildDeterministicGraph();
		eval = eval.paths.reduce();
		return eval;
	}

	private boolean languageDiff(LearnerGraph from,	LearnerGraph to,int states,int graphComplexity,ExperimentResult outcome) 
	{
		//Set<String> origAlphabet = to.pathroutines.computeAlphabet();
		//assert origAlphabet.equals(from.pathroutines.computeAlphabet());
		{
			long startTime = System.nanoTime();
			Collection<List<String>> wMethod = from.wmethod.getFullTestSet(1);
			outcome.setValue(LONG_V.DURATION_W,System.nanoTime()-startTime);
			Pair<Double,Long> wSeq=compareLang(from, to, wMethod);
			outcome.setValue(LONG_V.DURATION_W, outcome.getValue(LONG_V.DURATION_W)+wSeq.secondElem);
			outcome.setValue(DOUBLE_V.ACCURACY_W, wSeq.firstElem);
		}
		
		{
			Collection<List<String>> sequences =new LinkedHashSet<List<String>>();
			RandomPathGenerator rpg = new RandomPathGenerator(from, new Random(0),4, from.getInit());// the seed for Random should be the same for each file
			long startTime = System.nanoTime();
			rpg.generatePosNeg((graphComplexity+1)*states , 1);
			outcome.setValue(LONG_V.DURATION_RAND,System.nanoTime()-startTime);
			sequences.addAll(rpg.getAllSequences(0).getData(PTASequenceEngine.truePred));
			sequences.addAll(rpg.getExtraSequences(0).getData(PTASequenceEngine.truePred));
			Pair<Double,Long> randSeq = compareLang(from, to, sequences);
			outcome.setValue(LONG_V.DURATION_RAND, outcome.getValue(LONG_V.DURATION_RAND)+randSeq.secondElem);
			outcome.setValue(DOUBLE_V.ACCURACY_RAND, randSeq.firstElem);
		}
		
		return true;
	}

	/** Given two graphs and a test set, computes how well the sequences are accepted or rejected, returning
	 * both the F measure and time consumed.
	 * 
	 * @param from the original graph
	 * @param to mutated graph
	 * @param sequences sequences to test with
	 * @return a pair with the accuracy as the first element and time taken for the computation as the second one.
	 */
	private Pair<Double,Long> compareLang(LearnerGraph from, LearnerGraph to,
			Collection<List<String>> sequences) 
	{
		
		final long startTime = System.nanoTime();
		ConfusionMatrix matrix = classify(sequences, from,to);
		final long duration = System.nanoTime() - startTime;
		double result = matrix.fMeasure();
		assert !Double.isNaN(result);
		return new Pair<Double,Long>(result,duration);
	}

	public static ConfusionMatrix classify(Collection<List<String>> sequences,LearnerGraph from, LearnerGraph to) 
	{
		int tp=0, tn = 0, fp=0, fn=0;
		boolean inTarget,inMutated;
		for (List<String> list : sequences) {
			CmpVertex fromState = from.paths.getVertex(list);
			if(fromState==null)
				inTarget = false;
			else
				inTarget = fromState.isAccept();
			CmpVertex toState = to.paths.getVertex(list);
			if(toState == null)
				inMutated= false;
			else
				inMutated = toState.isAccept();
			if(inTarget && inMutated)
				tp++;
			else if(inTarget && !inMutated)
				fn++;
			else if(!inTarget && inMutated)
				fp++;
			else if(!inTarget && !inMutated)
				tn++;
		}
		return new ConfusionMatrix(tp,tn,fp,fn);
	}

	@SuppressWarnings("unused")
	private void displayDiff(LearnerGraphND from, LearnerGraphND to)
	{
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = 
			new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		DirectedSparseGraph gr = gd.showGD(
				from,to,
				ExperimentRunner.getCpuNumber());gr.setUserDatum(JUConstants.TITLE, "diff_"+from.getName(),UserData.SHARED);
		Visualiser.updateFrameWithPos(from, 1);
		Visualiser.updateFrameWithPos(to, 2);
		Visualiser.updateFrameWithPos(gr, 0);
	}
	
	void linearDiff(LearnerGraphND from, LearnerGraphND to, Set<Transition> expectedMutations, ExperimentResult outcome)
	{
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>  rec3 = new ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>(from,to,null);
		ChangesRecorderAsCollectionOfTransitions cd = new ChangesRecorderAsCollectionOfTransitions(rec3,false);
		final long startTime = System.nanoTime();
		gd.computeGD(from, to, ExperimentRunner.getCpuNumber(), cd, config);
		final long endTime = System.nanoTime();
		
		//displayDiff(from,to);
		outcome.setValue(LONG_V.DURATION_GD,endTime - startTime);
		Set<Transition> detectedDiff = cd.getDiff();
		
		outcome.experimentValid=true;outcome.setValue(DOUBLE_V.MUTATIONS_TO_TRANSITIONS,((double)expectedMutations.size())/from.pathroutines.countEdges());
		Map<CmpVertex,CmpVertex> keyPairs = gd.getKeyPairs();int mismatchedKeyPairs=0;
		for(Entry<CmpVertex,CmpVertex> pair:keyPairs.entrySet())
			if (!pair.getKey().getID().toString().equals("o"+pair.getValue().getID().toString()) && !pair.getValue().getID().toString().startsWith("added"))
				mismatchedKeyPairs++;//System.err.println("mismatched key pair: "+pair);
		if (keyPairs.size() > 0)
			outcome.setValue(DOUBLE_V.MISMATCHED_KEYPAIRS,((double)mismatchedKeyPairs)/keyPairs.size());
		// The observed difference can be below the expected one if we remove a transition making a state isolated
		// and then add one with the same label to a new state - in this case the new state and the old one are matched
		// but the mutator did not realise that this would be the case.
		
		// If the quality level of pairs to pick is set too high, in cases where a part of a graph is almost disconnected due to mutations
		// it will not be matched to the corresponding part of the original graph because what would usually be a key pair may receive
		// a relatively low score (compared to other well-matched pairs based on which the 50% threshold can be obtained).
		
		// If the quality level is low, we mis-match key pairs also leading to large patches.
		/*
		if (detectedDiff.size() < expectedMutations.size())
		{
			Set<Transition> set = new HashSet<Transition>();
			
			set.clear();set.addAll(expectedMutations);set.removeAll(detectedDiff);
			System.out.println("expected-detected = "+set);System.out.println();
			set.clear();set.addAll(detectedDiff);set.removeAll(expectedMutations);
			System.out.println("detected-expected = "+set);System.out.println();
			
			System.out.println("expected = "+expectedMutations);System.out.println();
			System.out.println("detected = "+detectedDiff);System.out.println();
			displayDiff(from, to);
			Visualiser.waitForKey();
		}*/
/*
		if (detectedDiff.size() > expectedMutations.size() )
		{
			Set<Transition> set = new HashSet<Transition>();
			
			set.clear();set.addAll(expectedMutations);set.removeAll(detectedDiff);
			System.out.println("expected-detected = "+set);System.out.println();
			set.clear();set.addAll(detectedDiff);set.removeAll(expectedMutations);
			System.out.println("detected-expected = "+set);System.out.println();
			
			System.out.println("expected = "+expectedMutations);System.out.println();
			System.out.println("detected = "+detectedDiff);System.out.println();
			//displayDiff(from, to);
			//Visualiser.waitForKey();
		}
*/
/*
		double f = computeFMeasure(expectedMutations, detectedDiff);
		performanceStruct[col][row][x] = duration;
		scoreStruct[col][row][x] = f;
		int tp = from.pathroutines.countEdges()-rec3.getRemoved();
		int fn = rec3.getRemoved();
		int fp = rec3.getAdded();
		int tn = 0;
		ConfusionMatrix cn = new ConfusionMatrix(tp,tn,fp,fn);
		*/

		if (detectedDiff.size() < expectedMutations.size()) outcome.setValue(DOUBLE_V.OBTAINED_TO_EXPECTED,1);// mutations mangled the graph too much
		else outcome.setValue(DOUBLE_V.OBTAINED_TO_EXPECTED,((double)detectedDiff.size())/expectedMutations.size());
	}
	
	/** Renames vertices in the supplied graph - very useful if we are to compare visually how the outcome
	 * is supposed to look like. In practice this is not necessary because GD will rename clashing vertices.
	 * 
	 * @param machineFrom machine which vertices to rename
	 * @param graphTo where to store the result
	 * @return the map from the original to the converted CmpVertices
	 */
	private Map<CmpVertex,CmpVertex> copyStatesAndTransitions(LearnerGraphND machineFrom, LearnerGraphND graphTo) {
		Map<CmpVertex,CmpVertex> machineVertexToGraphVertex = new HashMap<CmpVertex,CmpVertex>();
		graphTo.getTransitionMatrix().clear();
		Set<CmpVertex> machineStates = machineFrom.getTransitionMatrix().keySet();
		for (CmpVertex cmpVertex : machineStates) { //copy all vertices
			CmpVertex newVertex = AbstractLearnerGraph.generateNewCmpVertex(VertexID.parseID("o"+cmpVertex.getID().toString()), graphTo.config);
			DeterministicDirectedSparseGraph.copyVertexData(cmpVertex, newVertex);
			machineVertexToGraphVertex.put(cmpVertex, newVertex);
		}
		graphTo.setInit(machineVertexToGraphVertex.get(machineFrom.getInit()));
		AbstractLearnerGraph.addAndRelabelGraphs(machineFrom, machineVertexToGraphVertex, graphTo);
		graphTo.setName("orig_"+machineFrom.getName());
		graphTo.invalidateCache();return machineVertexToGraphVertex;
	}


	protected static double computeFMeasure(Set<Transition> from, Set<Transition> to){
		int tp,tn,fp,fn;
		Set<Transition> set = new HashSet<Transition>();

		set.clear();
		set.addAll(from);
		set.retainAll(to);
		tp = set.size();
		tn = 0;
		set.clear();
		set.addAll(to);
		set.removeAll(from);
		fp = set.size();
		set.clear();
		set.addAll(from);
		set.removeAll(to);
		fn = set.size();
		
		ConfusionMatrix conf = new ConfusionMatrix(tp, tn, fp, fn);
		return conf.fMeasure();
	}

	@SuppressWarnings("unused")
	private ExperimentResult getAverage(List<ExperimentResult> toPrint) 
	{
		ExperimentResult average = new ExperimentResult();average.experimentValid = true;
		int count=0;
		for(ExperimentResult exp:toPrint)
		{
			if (exp.experimentValid)
			{
				average.add(exp);
				count++;
			}
		}
		return average.divide(count);
	}

	public static class MachineGenerator{
		
		private final List<Integer> sizeSequence = new LinkedList<Integer>(); 
		private final int actualTargetSize, error, phaseSize;
		private int artificialTargetSize;

		
		public MachineGenerator(int target, int phaseArg, int errorArg){
			this.phaseSize = phaseArg;
			this.actualTargetSize = target;
			this.artificialTargetSize = target;
			this.error = errorArg;
		}
		
		//0.31,0.385
		public LearnerGraphND nextMachine(int alphabet, int counter){
			LearnerGraph machine = null;
			boolean found = false;
			while(!found){
				for(int i = 0; i< phaseSize; i++){
					//ForestFireNDStateMachineGenerator gen = new ForestFireNDStateMachineGenerator(0.365,0.3,0.2,seed,alphabet);
					ForestFireLabelledStateMachineGenerator gen = new ForestFireLabelledStateMachineGenerator(0.365,0.3,0.2,0.2,alphabet,counter);
					
					machine = gen.buildMachine(artificialTargetSize);
					machine.setName("forestfire_"+counter);
					int machineSize = machine.getStateNumber();
					//System.out.println("generated states: "+machineSize);
					sizeSequence.add(machineSize);
					
					if (Math.abs(machineSize - actualTargetSize) != 0)
							throw new RuntimeException();
					if(Math.abs(machineSize - actualTargetSize)<=error){
						found = true;
						break;
					}
						
				}
				if(!found)
					adjustArtificialTargetSize();
			}
			
			LearnerGraphND outcome = new LearnerGraphND(machine.config);AbstractLearnerGraph.copyGraphs(machine,outcome);
			return outcome;
		}

		private void adjustArtificialTargetSize() {
			int difference = actualTargetSize - average(sizeSequence);
			artificialTargetSize = artificialTargetSize+difference;
			sizeSequence.clear();
		}

		private int average(List<Integer> sizeSequence2) {
			int total = 0;
			for (Integer integer : sizeSequence2) {
				total = total + integer;
			}
			return total / sizeSequence.size();
		}
	
	}
}
