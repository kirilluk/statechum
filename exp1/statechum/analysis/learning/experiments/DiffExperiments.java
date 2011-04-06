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
package statechum.analysis.learning.experiments;

import java.util.Collection;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.Helper;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
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
import statechum.model.testset.PTASequenceEngine;
import statechum.analysis.learning.experiments.GraphMutator.ChangesRecorderAsCollectionOfTransitions;;

public class DiffExperiments {
	
	
	Configuration config = Configuration.getDefaultConfiguration();
	boolean skipLanguage = false;
	final int experimentsPerMutationCategory, mutationStages = 5, graphComplexityMax = 6;
	long[][][] performanceLang,performanceStruct; 
	double[][][] accuracyRandLang, accuracyWLang, scoreStruct; 
	ExperimentResult [][][]accuracyStruct;
	
	public static class ExperimentResult
	{
		double mismatchedKeyPairs = 0;
		double obtainedToExpected = 0;
		double mutationsToTransitions = 0;
		public boolean experimentValid = false;

		@Override
		public String toString()
		{
			if (!experimentValid) return "invalid experiment";
			return "Ave: "+obtainedToExpected+", mut/trans: "+mutationsToTransitions+", keypairs: "+mismatchedKeyPairs;
		}
		
		public void add(ExperimentResult exp) 
		{
			if (experimentValid && exp.experimentValid)
			{
				mismatchedKeyPairs+=exp.mismatchedKeyPairs;
				obtainedToExpected+=exp.obtainedToExpected;
				mutationsToTransitions+=exp.mutationsToTransitions;
			}
		}
		
		public ExperimentResult divide(int count) 
		{
			assert count > 0;assert experimentValid;
			ExperimentResult result = new ExperimentResult();result.experimentValid = true;
			result.mismatchedKeyPairs = mismatchedKeyPairs/count;result.obtainedToExpected=obtainedToExpected/count;
			result.mutationsToTransitions=mutationsToTransitions/count;
			return result;
		}
	}
	
	public static void main(String[] args){
		DiffExperiments exp = new DiffExperiments(30);
		exp.runExperiment(20, true);
		DrawGraphs.end();
	}
	
	public DiffExperiments(int experimentsPerCategoryArg){
		experimentsPerMutationCategory = experimentsPerCategoryArg;
		performanceLang = new long[graphComplexityMax][mutationStages][experimentsPerMutationCategory]; 
		performanceStruct = new long[graphComplexityMax][mutationStages][experimentsPerMutationCategory]; 
		
		accuracyRandLang = new double[graphComplexityMax][mutationStages][experimentsPerMutationCategory]; 
		accuracyWLang = new double[graphComplexityMax][mutationStages][experimentsPerMutationCategory];
		accuracyStruct = new ExperimentResult[graphComplexityMax][mutationStages][experimentsPerMutationCategory];
		
		scoreStruct = new double[graphComplexityMax][mutationStages][experimentsPerMutationCategory]; 
		//config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);
		//config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
		
	}
	
	public void runExperiment(int initStates, boolean skip){
		this.skipLanguage = skip;
		Random r = new Random(0);

		List<List<Double>> arrayWithDiffResults = new LinkedList<List<Double>>(), arrayWithKeyPairsResults = new LinkedList<List<Double>>();
		List<String> arrayWithResultNames = new LinkedList<String>();
		for(int graphComplexity=0;graphComplexity < graphComplexityMax;graphComplexity++)
		{
			int states=initStates+graphComplexity*50;
			int alphabet = states/2;
			
			MachineGenerator mg = new MachineGenerator(states, 40, states/6);
			int mutationsPerStage = (states/2) / 2;
			System.out.print("\n"+states+": ");
			for(int mutationStage = 0;mutationStage<mutationStages;mutationStage++)
			{
				List<Double> diffValues = new LinkedList<Double>(),keyPairsValues = new LinkedList<Double>();
				for(int experiment=0;experiment<experimentsPerMutationCategory;experiment++)
				{
					int counter=0;
					if(experiment%2==0)
						System.out.print(".");
					ExperimentResult worked = null;
					while(worked == null || !worked.experimentValid)
					{
						counter++;
						int mutations = mutationsPerStage * (mutationStage+1);
						LearnerGraphND origGraph = mg.nextMachine(alphabet, experiment);
						GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> mutator = 
							new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(origGraph,r);
						mutator.mutate(mutations);
						
						//mutator.changeStateLabels(); //to avoid exception that from and to share same labels
						
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
						
						worked = linearDiff(origAfterRenaming,mutated, appliedMutations,graphComplexity,mutationStage,experiment);
						if(!worked.experimentValid)
							throw new RuntimeException();
						diffValues.add(worked.obtainedToExpected);keyPairsValues.add(worked.mismatchedKeyPairs);
						
						if (!skip)
						{
							LearnerGraph fromDet = null, toDet = null;
							try {
								fromDet = mergeAndDeterminize(origAfterRenaming);
								toDet = mergeAndDeterminize(mutated);
							} catch (IncompatibleStatesException e) {
								Helper.throwUnchecked("failed to build a deterministic graph from a nondet one", e);
							}
							//worked = languageDiff(fromDet,toDet,states, graphComplexity,mutationStage,experiment);
						}
						
					}
				}
				ExperimentResult average = getAverage(accuracyStruct,graphComplexity,mutationStage);
				arrayWithDiffResults.add(diffValues);arrayWithKeyPairsResults.add(keyPairsValues);
				arrayWithResultNames.add(""+states+"-"+Math.round(100*average.mutationsToTransitions)+"%");
				System.out.print("["+average+"]");
			}
			
			/*
			printList(scoreStruct[graphComplexity]);
			System.out.println("Time-struct:");
			printList(performanceStruct[graphComplexity]);
			if(!skip){
				System.out.println("Time-lang:");
				printList(performanceLang[graphComplexity]);
				System.out.println("------");
				printLangScores(this.accuracyRandLang[graphComplexity],this.accuracyWLang[graphComplexity],this.accuracyStruct[graphComplexity]);
				System.out.println("------");
			}
			*/
		}
		DrawGraphs gr = new DrawGraphs();
		gr.drawBoxPlot(arrayWithDiffResults, arrayWithResultNames,"diffResults.pdf");
		gr.drawBoxPlot(arrayWithKeyPairsResults, arrayWithResultNames,"keypairsResults.pdf");
		/*System.out.println("\nSTRUCT SCORES");
		printAccuracyMatrix(this.scoreStruct);
		System.out.println("ACCURACY STRUCT");
		printAccuracyMatrix(this.accuracyStruct);
		System.out.println("ACCURACY RANDOM");
		printAccuracyMatrix(this.accuracyRandLang);
		System.out.println("ACCURACY W");
		printAccuracyMatrix(this.accuracyWLang);
		System.out.println("RANDOM-W-STRUCTURE");
		printLangScores(this.accuracyRandLang,this.accuracyWLang,this.accuracyStruct);
		System.out.println("TIME STRUCT");
		printAccuracyMatrix(this.performanceStruct);
		System.out.println("TIME LANG");
		printAccuracyMatrix(this.performanceLang);*/
	}
	

	private LearnerGraph mergeAndDeterminize(LearnerGraphND from) throws IncompatibleStatesException {
		LearnerGraph eval = null;
		eval = from.pathroutines.buildDeterministicGraph();
		eval = eval.paths.reduce();
		return eval;
	}

	private boolean languageDiff(
			LearnerGraph from,
			LearnerGraph to,
			int states,
			int i, int j, int j2) {
		//Set<String> origAlphabet = to.pathroutines.computeAlphabet();
		//assert origAlphabet.equals(from.pathroutines.computeAlphabet());
		final long startTime = System.nanoTime();
		final long endTime;
		Collection<List<String>> wMethod = from.wmethod.getFullTestSet(1);
		endTime = System.nanoTime();
		long wDuration = endTime - startTime;
		Collection<List<String>> sequences =new TreeSet<List<String>>();
		RandomPathGenerator rpg = new RandomPathGenerator(from, new Random(0),4, from.getInit());// the seed for Random should be the same for each file
		rpg.generatePosNeg((i+1)*states , 1);

		sequences.addAll(rpg.getAllSequences(0).getData(PTASequenceEngine.truePred));
		sequences.addAll(rpg.getExtraSequences(0).getData(PTASequenceEngine.truePred));
		compareLang(from, to, sequences, false,i,j,j2,0);
		compareLang(from, to, wMethod, true,i,j,j2,wDuration);
		return true;
	}

	private void compareLang(LearnerGraph from, LearnerGraph to,
			Collection<List<String>> sequences, boolean useWset, int col, int row, int x, long time) 
	{
		
		final long startTime = System.nanoTime();
		final long endTime;
		ConfusionMatrix result;
		result = classify(sequences, from,to);
		endTime = System.nanoTime();
		final long duration = endTime - startTime;
		if(!useWset){
			accuracyRandLang[col][row][x] = result.fMeasure();
			assert !Double.isNaN(accuracyRandLang[col][row][x]);
		}
		else{
			accuracyWLang[col][row][x] = result.fMeasure();
			performanceLang[col][row][x] = duration+time;
			assert !Double.isNaN(accuracyWLang[col][row][x]);
		}
		
	}

	private ConfusionMatrix classify(Collection<List<String>> sequences,
			LearnerGraph from, LearnerGraph to) {
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
	
	ExperimentResult linearDiff(LearnerGraphND from, LearnerGraphND to, 
			Set<Transition> expectedMutations,
			int col, int row, int x)
	{
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>  rec3 = new ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>(from,to,null);
		ChangesRecorderAsCollectionOfTransitions cd = new ChangesRecorderAsCollectionOfTransitions(rec3,false);
		final long startTime = System.nanoTime();
		config.setGdKeyPairThreshold(0.5);
		config.setGdLowToHighRatio(0.75);
		config.setGdPropagateDet(false);// this is to ensure that if we removed a transition 0 from to a state and then added one from that state to a different one, det-propagation will not force the two very different states into a key-pair relation. 
		gd.computeGD(from, to, ExperimentRunner.getCpuNumber(), cd, config);
		final long endTime = System.nanoTime();
		
		//displayDiff(from,to);
		final long duration = endTime - startTime;
		Set<Transition> detectedDiff = cd.getDiff();
		
		ExperimentResult outcome = new ExperimentResult();
		outcome.experimentValid=true;outcome.mutationsToTransitions=((double)expectedMutations.size())/from.pathroutines.countEdges();
		accuracyStruct[col][row][x]=outcome;
		Map<CmpVertex,CmpVertex> keyPairs = gd.getKeyPairs();int mismatchedKeyPairs=0;
		for(Entry<CmpVertex,CmpVertex> pair:keyPairs.entrySet())
			if (!pair.getKey().getID().toString().equals("o"+pair.getValue().getID().toString()) && !pair.getValue().getID().toString().startsWith("added"))
				mismatchedKeyPairs++;//System.err.println("mismatched key pair: "+pair);
		if (keyPairs.size() > 0)
			outcome.mismatchedKeyPairs = ((double)mismatchedKeyPairs)/keyPairs.size();
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

		if (detectedDiff.size() < expectedMutations.size()) outcome.obtainedToExpected = 1;// mutations mangled the graph too much
		else outcome.obtainedToExpected = ((double)detectedDiff.size())/expectedMutations.size();
		return outcome;
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

	private ExperimentResult getAverage(ExperimentResult[][][] toPrint, int i, int j) {
		ExperimentResult average = new ExperimentResult();average.experimentValid = true;
		int count=0;
		for(int k =0;k<experimentsPerMutationCategory;k++)
		{
			ExperimentResult exp = toPrint[i][j][k];
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
