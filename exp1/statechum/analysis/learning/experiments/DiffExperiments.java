package statechum.analysis.learning.experiments;

import java.util.Collection;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.Helper;
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
	double[][][] accuracyRandLang, accuracyWLang, accuracyStruct, scoreStruct; 
	
	
	public static void main(String[] args){
		DiffExperiments exp = new DiffExperiments(30);
		exp.runExperiment(20, true);
		
	}
	
	public DiffExperiments(int experimentsPerCategoryArg){
		experimentsPerMutationCategory = experimentsPerCategoryArg;
		performanceLang = new long[graphComplexityMax][mutationStages][experimentsPerMutationCategory]; 
		performanceStruct = new long[graphComplexityMax][mutationStages][experimentsPerMutationCategory]; 
		
		accuracyRandLang = new double[graphComplexityMax][mutationStages][experimentsPerMutationCategory]; 
		accuracyWLang = new double[graphComplexityMax][mutationStages][experimentsPerMutationCategory];
		accuracyStruct = new double[graphComplexityMax][mutationStages][experimentsPerMutationCategory];
		
		scoreStruct = new double[graphComplexityMax][mutationStages][experimentsPerMutationCategory]; 
		//config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);
		//config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
	}
	
	public void runExperiment(int initStates, boolean skip){
		this.skipLanguage = skip;
		Random r = new Random(0);
		for(int graphComplexity=0;graphComplexity < graphComplexityMax;graphComplexity++){
			int states=initStates+graphComplexity*50;
			int alphabet = states/2;
			
			MachineGenerator mg = new MachineGenerator(states, 40, states/6);
			int mutationsPerStage = (states/2) / 2;
			System.out.print("\n"+states+": ");
			for(int mutationStage = 0;mutationStage<mutationStages;mutationStage++){
				for(int experiment=0;experiment<experimentsPerMutationCategory;experiment++){
					int counter=0;
					if(experiment%2==0)
						System.out.print(".");
					boolean worked = false;
					while(worked == false)
					{
						counter++;
						int mutations = mutationsPerStage * (mutationStage+1);
						LearnerGraphND origGraph = mg.nextMachine(alphabet, counter);
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
						if(!worked)
							throw new RuntimeException();
						LearnerGraph fromDet = null, toDet = null;
						try {
							fromDet = mergeAndDeterminize(origAfterRenaming);
							toDet = mergeAndDeterminize(mutated);
						} catch (IncompatibleStatesException e) {
							Helper.throwUnchecked("failed to build a deterministic graph from a nondet one", e);
						}
						if (!skip)
							worked = languageDiff(fromDet,toDet,states, graphComplexity,mutationStage,experiment);
					}
				}
				System.out.print("|");
				System.out.print("["+getAverage(accuracyStruct,graphComplexity,mutationStage)+"]");
			}
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
		}
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

	private void printList(double[][] ds) {
		System.out.println();
		for(int j = 0; j<experimentsPerMutationCategory;j++){
			for(int i=0;i<ds.length;i++){
				System.out.print(ds[i][j]+",");
			}
			System.out.println();
		}
		System.out.println();
		
	}
	
	private void printList(long[][] ls) {
		System.out.println();
		for(int j = 0; j<experimentsPerMutationCategory;j++){
			for(int i=0;i<ls.length;i++){
				System.out.print(ls[i][j]+",");
			}
			System.out.println();
		}
		System.out.println();
		
	}

	private void printLangScores(double[][][] accuracyRandLang2,
			double[][][] accuracyWLang2, double[][][] accuracyStruct2) {
		for(int i=0;i<mutationStages;i++){
			for(int j=0;j<mutationStages;j++){
				for(int k =0;k<experimentsPerMutationCategory;k++){
					System.out.println(accuracyRandLang2[i][j][k]+","+accuracyWLang2[i][j][k]+","+accuracyStruct2[i][j][k]);
				}
			}
		}
		
	}
	
	private void printLangScores(double[][] accuracyRandLang2, double[][] accuracyWLang2, double[][] accuracyStruct2) {
		for(int j=0;j<mutationStages;j++){
			for(int k =0;k<experimentsPerMutationCategory;k++){
				System.out.println(accuracyRandLang2[j][k]+","+accuracyWLang2[j][k]+","+accuracyStruct2[j][k]);
			}
		}
		
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
				ExperimentRunner.getCpuNumber());
		Visualiser.updateFrame(to,gr);
	}
	
	boolean linearDiff(LearnerGraphND from, LearnerGraphND to, 
			Set<Transition> expectedMutations,
			int col, int row, int x)
	{
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>  rec3 = new ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>(from,to,null);
		ChangesRecorderAsCollectionOfTransitions cd = new ChangesRecorderAsCollectionOfTransitions(rec3);
		final long startTime = System.nanoTime();
		gd.computeGD(from, to, ExperimentRunner.getCpuNumber(), cd, config);
		final long endTime = System.nanoTime();
		
		//displayDiff(from,to);
		final long duration = endTime - startTime;
		Set<Transition> detectedDiff = cd.getDiff();
		
		if (!detectedDiff.equals(expectedMutations))
		{
			Set<Transition> set = new HashSet<Transition>();
			
			set.clear();set.addAll(expectedMutations);set.removeAll(detectedDiff);
			System.out.println(set);System.out.println();
			set.clear();set.addAll(detectedDiff);set.removeAll(expectedMutations);
			System.out.println(set);System.out.println();
			
			System.out.println(expectedMutations);System.out.println();
			System.out.println(detectedDiff);System.out.println();
			displayDiff(from, to);
			Visualiser.waitForKey();
		}
		
		double f = computeFMeasure(expectedMutations, detectedDiff);
		performanceStruct[col][row][x] = duration;
		scoreStruct[col][row][x] = f;
		int tp = from.pathroutines.countEdges()-rec3.getRemoved();
		int fn = rec3.getRemoved();
		int fp = rec3.getAdded();
		int tn = 0;
		ConfusionMatrix cn = new ConfusionMatrix(tp,tn,fp,fn);
		accuracyStruct[col][row][x]=cn.fMeasure();		
		return true;
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
	
	private void printAccuracyMatrix(double[][][] toPrint){
		for(int i=0;i<mutationStages;i++){
			for(int j=0;j<mutationStages;j++){
				double average = getAverage(toPrint,i,j);
				System.out.print(average+",");
			}
			System.out.println();
		}
	}

	private double getAverage(double[][][] toPrint, int i, int j) {
		double total = 0;
		for(int k =0;k<30;k++){
			total = total + toPrint[i][j][k];
		}
		return total/experimentsPerMutationCategory;
	}
	
	private void printAccuracyMatrix(long[][][] toPrint){
		for(int i=0;i<mutationStages;i++){
			for(int j=0;j<mutationStages;j++){
				long average = getAverage(toPrint,i,j);
				System.out.print(average+",");
			}
			System.out.println();
		}
	}

	private long getAverage(long[][][] toPrint, int i, int j) {
		long total = 0;
		for(int k =0;k<30;k++){
			total = total + toPrint[i][j][k];
		}
		return total/30;
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
		public LearnerGraphND nextMachine(int alphabet, int seed){
			LearnerGraph machine = null;
			boolean found = false;
			while(!found){
				for(int i = 0; i< phaseSize; i++){
					//ForestFireNDStateMachineGenerator gen = new ForestFireNDStateMachineGenerator(0.365,0.3,0.2,seed,alphabet);
					ForestFireLabelledStateMachineGenerator gen = new ForestFireLabelledStateMachineGenerator(0.365,0.3,0.2,0.2,alphabet,seed);
					
					machine = gen.buildMachine(artificialTargetSize);
					machine.setName("forestfire_"+alphabet);
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
