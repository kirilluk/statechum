package statechum.analysis.learning.experiments;

import java.util.Collection;

import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.Vector;

import statechum.Configuration;
import statechum.Configuration.GDScoreComputationAlgorithmEnum;
import statechum.Configuration.GDScoreComputationEnum;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.PrecisionRecall.ConfusionMatrix;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.GD;
import statechum.analysis.learning.rpnicore.GD.ChangesDisplay;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.GD.ChangesCounter;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;


public class DiffExperiments {
	
	
	Configuration config = Configuration.getDefaultConfiguration();
	boolean skipLanguage = false;
	int exp;
	long[][][] performanceLang,performanceStruct; 
	double[][][] accuracyRandLang, accuracyWLang, accuracyStruct, scoreStruct; 
	
	
	public static void main(String[] args){
		DiffExperiments exp = new DiffExperiments(30);
		exp.runExperiment(20, true);
		
	}
	
	public DiffExperiments(int experimentsPerCategory){
		performanceLang = new long[100][5][experimentsPerCategory]; 
		performanceStruct = new long[100][5][experimentsPerCategory]; 
		
		accuracyRandLang = new double[100][5][experimentsPerCategory]; 
		accuracyWLang = new double[100][5][experimentsPerCategory];
		accuracyStruct = new double[100][5][experimentsPerCategory];
		
		scoreStruct = new double[100][5][experimentsPerCategory]; 
		exp = experimentsPerCategory;
		//config.setGdScoreComputation(GDScoreComputationEnum.GD_DIRECT);
		//config.setGdScoreComputationAlgorithm(GDScoreComputationAlgorithmEnum.SCORE_TESTSET);
	}
	
	public void runExperiment(int initStates, boolean skip){
		this.skipLanguage = skip;
		for(int states=initStates;states<8000;states=states+50){
			int alphabet = states/2;
			
			MachineGenerator mg = new MachineGenerator(states, 40, states/6);
			int mutationsPerStage = (states/2) / 2;
			System.out.print("\n"+states+": ");
			for(int i = 1;i<6;i++){
				for(int j=0;j<exp;j++){
					int counter=0;
					if(j%2==0)
						System.out.print(".");
					boolean worked = false;
					while(worked == false){
						counter++;
						int mutations = mutationsPerStage * i;
						LearnerGraphND from = mg.nextMachine(alphabet, counter);
						//System.out.println(from.getTransitionMatrix().size());
						LearnerGraphND mutating =  (LearnerGraphND)from.copy(config);
						renameStates(mutating);
						GraphMutator mutator = new GraphMutator(mutating,false);
						mutator.mutate(mutations);
						//Visualiser.updateFrame(from, mutating);
						worked = linearDiff(from,mutating, mutator,states/50,i-1,j);
						if(!worked)
							continue;
						LearnerGraph fromDet = mergeAndDeterminize(from);
						LearnerGraph toDet = mergeAndDeterminize(mutating);
						//Visualiser.updateFrame(fromDet, toDet);
						if (!skip)
							worked = languageDiff(fromDet,toDet,states, (int)states/50,i-1,j);
					}
				}
				System.out.print("|");
				System.out.print("["+getAverage(scoreStruct,states/50,i-1)+"]");
			}
			printList(scoreStruct[(int)states/50]);
			System.out.println("Time-struct:");
			printList(performanceStruct[(int)states/50]);
			if(!skip){
				System.out.println("Time-lang:");
				printList(performanceLang[(int)states/50]);
				System.out.println("------");
				printLangScores(this.accuracyRandLang[(int)states/50],this.accuracyWLang[(int)states/50],this.accuracyStruct[(int)states/50]);
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
	
	

	

	private LearnerGraph mergeAndDeterminize(LearnerGraphND mutating) {
		LearnerGraph eval = null;
		try {
			eval = mutating.pathroutines.buildDeterministicGraph();
			eval = eval.paths.reduce();
		} catch (IncompatibleStatesException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return eval;
	}

	private void printList(double[][] ds) {
		System.out.println();
		for(int j = 0; j<exp;j++){
			for(int i=0;i<ds.length;i++){
				System.out.print(ds[i][j]+",");
			}
			System.out.println();
		}
		System.out.println();
		
	}
	
	private void printList(long[][] ls) {
		System.out.println();
		for(int j = 0; j<exp;j++){
			for(int i=0;i<ls.length;i++){
				System.out.print(ls[i][j]+",");
			}
			System.out.println();
		}
		System.out.println();
		
	}

	private void renameStates(LearnerGraphND mutating) {
		Set<CmpVertex> keySet = mutating.getTransitionMatrix().keySet();
		for (CmpVertex cmpVertex : keySet) {
			VertexID id = cmpVertex.getID();
			String label = id.getStringId();
			VertexID newID = new VertexID(label+"mut");
			cmpVertex.setID(newID);
		}
		
	}

	private void printLangScores(double[][][] accuracyRandLang2,
			double[][][] accuracyWLang2, double[][][] accuracyStruct2) {
		for(int i=0;i<5;i++){
			for(int j=0;j<5;j++){
				for(int k =0;k<exp;k++){
					System.out.println(accuracyRandLang2[i][j][k]+","+accuracyWLang2[i][j][k]+","+accuracyStruct2[i][j][k]);
				}
			}
		}
		
	}
	
	private void printLangScores(double[][] accuracyRandLang2, double[][] accuracyWLang2, double[][] accuracyStruct2) {
		for(int j=0;j<5;j++){
			for(int k =0;k<exp;k++){
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
		Collection<List<String>> wMethod;
		try{
			wMethod = from.wmethod.getFullTestSet(1);
		}
		finally{
			endTime = System.nanoTime();
		}
		long wDuration = endTime - startTime;
		Collection<List<String>> sequences =new HashSet<List<String>>();
		RandomPathGenerator rpg = new RandomPathGenerator(from, new Random(0),4, from.getInit());// the seed for Random should be the same for each file
		try{
			rpg.generatePosNeg((i+1)*states , 1);
		}
		catch(Exception e){
			System.out.print("-");
			return false;
		}
		final PTASequenceEngine samples = rpg.getAllSequences(0);
		final PTASequenceEngine positive = rpg.getExtraSequences(0);

		PTASequenceEngine.FilterPredicate posPredicate =samples.getFSM_filterPredicate();
			
			
		PTASequenceEngine.FilterPredicate negPredicate = new FilterPredicate() {
			FilterPredicate origFilter = samples.getFSM_filterPredicate();
			public boolean shouldBeReturned(Object name) {
				return !origFilter.shouldBeReturned(name);
			}
		};

		sequences.addAll(samples.getData(negPredicate));
		sequences.addAll(positive.getData(posPredicate));
		compareLang(from, to, sequences, false,i,j,j2,0);
		compareLang(from, to, wMethod, true,i,j,j2,wDuration);
		return true;
	}

	private void compareLang(LearnerGraph from, LearnerGraph to,
			Collection<List<String>> sequences, boolean w, int col, int row, int x, long time) {
		
		final long startTime = System.nanoTime();
		final long endTime;
		ConfusionMatrix result;
		try{
		result = classify(sequences, from,to);
		}
		finally{
			endTime = System.nanoTime();
		}
		final long duration = endTime - startTime;
		if(!w){
			accuracyRandLang[col][row][x] = result.fMeasure();
			assert(!Double.isNaN(accuracyRandLang[col][row][x]));
		}
		else{
			accuracyWLang[col][row][x] = result.fMeasure();
			performanceLang[col][row][x] = duration+time;
			assert(!Double.isNaN(accuracyWLang[col][row][x]));
		}
		
	}

	private ConfusionMatrix classify(Collection<List<String>> sequences,
			LearnerGraph from, LearnerGraph to) {
		double tp=0; double tn = 0; double fp=0; double fn=0;
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

	boolean linearDiff(AbstractLearnerGraph from, AbstractLearnerGraph to, GraphMutator mutator, int col, int row, int x)
	{
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>  rec3 = new ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>(from,to,null);
		ChangesDisplay cd = new ChangesDisplay(rec3);
		final long startTime = System.nanoTime();
		final long endTime;
		try{
			gd.computeGD(from, to, 2, cd, config);
		}
		catch(Exception e){
			e.printStackTrace();
			return false;
		}
		finally{
			endTime = System.nanoTime();
		}
		final long duration = endTime - startTime;
		Set<Transition> detectedDiff = cd.getDiff();
		Set<Transition> mutations = mutator.getDiff();
		mutations = removePrefixes(mutations);
		double f = computeFMeasure(mutations, detectedDiff);
		performanceStruct[col][row][x] = duration;
		scoreStruct[col][row][x] = f;
		//System.out.println(f);
		//assert(performanceStruct[col][row][x]>0);
		//assert(scoreStruct[col][row][x]>0);
		double tp = from.pathroutines.countEdges()-rec3.getRemoved();
		double fn = rec3.getRemoved();
		double fp = rec3.getAdded();
		double tn = 0.0;
		ConfusionMatrix cn = new ConfusionMatrix(tp,tn,fp,fn);
		accuracyStruct[col][row][x]=cn.fMeasure();		
		//assert(accuracyStruct[col][row][x]>0);
		System.gc();
		return true;
	}
	
	private Set<Transition> removePrefixes(Set<Transition> mutations) {
		for (Transition transition : mutations) {
			String label = transition.getFrom();
			if(label.substring(label.length()-3, label.length()).equals("mut"))
				transition.setFrom(label.substring(0,label.length()-3));
			label = transition.getTo();
			if(label.substring(label.length()-3, label.length()).equals("mut"))
				transition.setTo(label.substring(0,label.length()-3));
		}
		return mutations;
	}

	protected static double computeFMeasure(Set<Transition> from, Set<Transition> to){
		double tp,tn,fp,fn;
		Set<Transition> set = new HashSet<Transition>();
		set.addAll(from);
		set.retainAll(to);
		tp = (double)set.size();
		tn = 0.0;
		set.clear();
		set.addAll(to);
		set.removeAll(from);
		fp = (double)set.size();
		set.clear();
		set.addAll(from);
		set.removeAll(to);
		fn = (double)set.size();
		
		ConfusionMatrix conf = new ConfusionMatrix(tp, tn, fp, fn);
		return conf.fMeasure();
	}
	
	private void printAccuracyMatrix(double[][][] toPrint){
		for(int i=0;i<5;i++){
			for(int j=0;j<5;j++){
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
		return total/exp;
	}
	
	private void printAccuracyMatrix(long[][][] toPrint){
		for(int i=0;i<5;i++){
			for(int j=0;j<5;j++){
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
	
public class MachineGenerator{
		
		private Vector<Integer> sizeSequence; 
		private int actualTargetSize, artificialTargetSize, error, phase;

		
		public MachineGenerator(int target, int phase, int error){
			this.phase = phase;
			this.actualTargetSize = target;
			this.artificialTargetSize = target;
			this.sizeSequence = new Vector<Integer>();
			this.error = error;
		}
		
		//0.31,0.385
		public LearnerGraphND nextMachine(int alphabet, int seed){
			LearnerGraphND machine = null;
			boolean found = false;
			while(!found){
				for(int i = 0; i< phase; i++){
					ForestFireNDStateMachineGenerator gen = new ForestFireNDStateMachineGenerator(0.4,0.385,0.2,seed,alphabet);
					machine = gen.buildMachine(artificialTargetSize);
					int machineSize = machine.getStateNumber();
					sizeSequence.add(machineSize);
					if(Math.abs(machineSize - actualTargetSize)<=error){
						found = true;
						break;
					}
				}
				if(!found)
					adjustArtificialTargetSize();
			}
			return machine;
		}

		private void adjustArtificialTargetSize() {
			int difference = actualTargetSize - average(sizeSequence);
			artificialTargetSize = artificialTargetSize+difference;
			sizeSequence = new Vector<Integer>();
		}

		private int average(Vector<Integer> sizeSequence2) {
			int total = 0;
			for (Integer integer : sizeSequence2) {
				total = total + integer;
			}
			return total / sizeSequence.size();
		}
		
	
	}


}
