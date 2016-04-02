package statechum.analysis.learning.experiments.PairSelection;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import statechum.GlobalConfiguration;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResult;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

public class SmallvsHugeExperiment extends UASExperiment
{
	protected final int states,fsmSample;
	protected boolean learnUsingReferenceLearner, onlyUsePositives;
	protected final int seed;
	protected final int traceQuantity;
	protected final int lengthMultiplier;
	protected final int attempt;
	
	/** Whether to compute the value from QSM as a reference learner. */
	public void setEvaluateAlsoUsingReferenceLearner(boolean value)
	{
		learnUsingReferenceLearner = value;
	}
	
	/** Whether to filter the collection of traces such that only positive traces are used. */
	public void setOnlyUsePositives(boolean value)
	{
		onlyUsePositives = value;
	}
	
	private void setAlwaysRunExperiment(boolean b) 
	{
		alwaysRunExperiment = b;
	}

	public SmallvsHugeExperiment(int argStates, int argFsmSample, int argAttempt, int argSeed, int nrOfTraces, int lengthmult, LearnerEvaluationConfiguration eval)
	{
		super(eval);
		states = argStates;fsmSample = argFsmSample;seed = argSeed;traceQuantity=nrOfTraces;lengthMultiplier = lengthmult;attempt= argAttempt;

		String outDir = "tmp"+File.separator+"smallvshuge_Mar_2016-"+argStates+"-"+nrOfTraces+"-"+lengthmult;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		if (!new java.io.File(outDir).isDirectory())
		{
			if (!new java.io.File(outDir).mkdir())
				throw new RuntimeException("failed to create a work directory");
		}
		inputGraphFileName = outDir + File.separator+"rnd";
	}
	
	@Override
	public ThreadResult call() throws Exception 
	{
		final int alphabet = states*2;
		ThreadResult outcome = new ThreadResult();
		Label uniqueFromInitial = null;
		final boolean pickUniqueFromInitial = true;
		MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);

		do
		{
			referenceGraph = mg.nextMachine(alphabet,seed, learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
			if (pickUniqueFromInitial)
			{
				Map<Label,CmpVertex> uniques = LearningSupportRoutines.uniqueFromState(referenceGraph);
				if(!uniques.isEmpty())
				{ 
					// some uniques are loops, hence eliminate them to match our case study
					for(Entry<Label,CmpVertex> entry:uniques.entrySet())
						if (referenceGraph.transitionMatrix.get(entry.getValue()).get(entry.getKey()) != entry.getValue())
						{
							referenceGraph.setInit(entry.getValue());uniqueFromInitial = entry.getKey();break;// found a unique of interest
						}
				}
			}
		}
		while(pickUniqueFromInitial && uniqueFromInitial == null);

		//referenceGraph = mg.nextMachine(alphabet,seed, learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).pathroutines.buildDeterministicGraph();
		final LearnerGraph pta = new LearnerGraph(learnerInitConfiguration.config);
		final RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt*23+seed),5,referenceGraph.getVertex(Arrays.asList(new Label[]{uniqueFromInitial})));
		//final RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,new Random(attempt*23+seed),5,referenceGraph.getInit());//referenceGraph.getVertex(Arrays.asList(new Label[]{uniqueFromInitial})));
		//generator.setWalksShouldLeadToInitialState();
		final int tracesToGenerate = LearningSupportRoutines.makeEven(states*traceQuantity);
		final Random rnd = new Random(seed*31+attempt*states);

		generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomLengthGenerator() {
								
				@Override
				public int getLength() {
					return  generator.getPathLength();//lengthMultiplier*states;
				}

				@Override
				public int getPrefixLength(int len) {
					return len;
				}
			},true,true,null,Arrays.asList(new Label[]{uniqueFromInitial}));

		//generator.generateRandomPosNeg(tracesToGenerate, 1, false);
		if (onlyUsePositives)
			pta.paths.augmentPTA(generator.getAllSequences(0).filter(new FilterPredicate() {
				@Override
				public boolean shouldBeReturned(Object name) {
					return ((statechum.analysis.learning.rpnicore.RandomPathGenerator.StateName)name).accept;
				}
			}));
		else
			pta.paths.augmentPTA(generator.getAllSequences(0));// the PTA will have very few reject-states because we are generating few sequences and hence there will be few negative sequences.
			// In order to approximate the behaviour of our case study, we need to compute which pairs are not allowed from a reference graph and use those as if-then automata to start the inference.
			// This is done below if onlyUsePositives is not set. 
		

		pta.clearColours();
		
		if (!onlyUsePositives)
		{// now we have an even positive/negative split, add negatives by encoding them as if-then automata.
			assert pta.getStateNumber() > pta.getAcceptStateNumber() : "graph with only accept states but onlyUsePositives is not set";
			Map<Label,Set<Label>> infeasiblePairs = LearningSupportRoutines.computeInfeasiblePairs(referenceGraph);
			Map<Label,Set<Label>> subsetOfPairs = new TreeMap<Label,Set<Label>>();
			for(Entry<Label,Set<Label>> entry:infeasiblePairs.entrySet())
			{
				Set<Label> value = new TreeSet<Label>();
				if (!entry.getValue().isEmpty()) 
				{// we add a single entry per label, to mimic what was done with UAS study, where labels could not be repeated.
					Label possibleLabels[]=entry.getValue().toArray(new Label[]{});
					if (possibleLabels.length == 1)
						value.add(possibleLabels[0]);
					else
					{
						for(int cnt=0;cnt < 1;++cnt)
							value.add(possibleLabels[rnd.nextInt(possibleLabels.length)]);
					}
				}
				subsetOfPairs.put(entry.getKey(),value);
			}
			LearnerEvaluationConfiguration learnerEval = UASExperiment.constructLearnerInitConfiguration();
			LearningSupportRoutines.addIfThenForPairwiseConstraints(learnerEval,subsetOfPairs);
			LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(learnerEval.ifthenSequences, referenceGraph.pathroutines.computeAlphabet(), learnerEval.config, learnerEval.getLabelConverter()).toArray(new LearnerGraph[0]);
			learnerEval.config.setUseConstraints(false);// do not use if-then during learning (refer to the explanation above)
			int statesToAdd = 1;// we are adding pairwise constraints hence only one has to be added.
			
			Transform.augmentFromIfThenAutomaton(pta, null, ifthenAutomata, statesToAdd);// we only need  to augment our PTA once (refer to the explanation above).
		}
		else 
			assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
		
		
		//Visualiser.updateFrame(referenceGraph, null);
		//Visualiser.waitForKey();
		for(Entry<CmpVertex,List<Label>> path: pta.pathroutines.computeShortPathsToAllStates().entrySet())
		{
			boolean accept = path.getKey().isAccept();
			CmpVertex vert = referenceGraph.getVertex(path.getValue());
			boolean shouldBe = vert==null?false:vert.isAccept();
			assert accept == shouldBe: "state "+vert+" is incorrectly annotated as "+accept+" in path "+path;
		}

		learnerInitConfiguration.testSet = LearningAlgorithms.buildEvaluationSet(referenceGraph);
		final int attemptFinal = attempt;
		UASExperiment.BuildPTAInterface ptaConstructor = new BuildPTAInterface() {
			@Override
			public String kindOfPTA()
			{
				return states+"-"+fsmSample+"-"+seed+"-"+attemptFinal;
			}
			@Override
			public LearnerGraph buildPTA() throws AugmentFromIfThenAutomatonException, IOException {
				return pta;
			}
		};

		for(ScoringToApply scoringMethod:listOfScoringMethodsToApply())
		{
 			PairQualityLearner.SampleData sample = new PairQualityLearner.SampleData();sample.experimentName = states+"-"+fsmSample+"-"+seed+"-"+attemptFinal;// attempt is a passed via an instance of BuildPTAInterface
 			//sample.referenceLearner = runExperimentUsingConventional(ptaConstructor,scoringMethod);
 			sample.referenceLearner = runExperimentUsingConventionalWithUniqueLabel(ptaConstructor,scoringMethod, uniqueFromInitial);
 			//sample.premergeLearner = runExperimentUsingPremerge(ptaConstructor,scoringMethod,uniqueFromInitial);
 			//sample.actualConstrainedLearner = runExperimentUsingConstraints(ptaConstructor,scoringMethod,uniqueFromInitial);
			
			outcome.samples.add(sample);

			/*
			{// Perform semi-pre-merge by building a PTA rather than a graph with loops and learn from there without using constraints
				LearnerGraph reducedPTA = LearningSupportRoutines.mergeStatesForUnique(pta,uniqueFromInitial);
				//Visualiser.updateFrame(reducedPTA.transform.trimGraph(4, reducedPTA.getInit()), pta);
				// in these experiments I cannot use SICCO merging because it will stop any mergers with an initial state.
				learnerOfPairs = new LearningAlgorithms.ReferenceLearner(learnerEval, reducedPTA,scoringToUse);
				
				System.out.println("PTApremerge size: "+reducedPTA.getStateNumber()+" states, "+reducedPTA.getAcceptStateNumber()+" accept-states and "+reducedPTA.pathroutines.countEdges()+" transitions");
				actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());

				DifferenceToReference similarityMeasure = getMeasure(actualAutomaton,referenceGraph,testSet);
				System.out.println(sample+" PTA permerge, similarity = "+similarityMeasure.getValue()+" ( "+similarityMeasure+" )");
			}
			 */
			/*
			{// Perform semi-pre-merge by building a PTA rather than a graph with loops and then use constraints
				LearnerGraph reducedPTA = LearningSupportRoutines.mergeStatesForUnique(pta,uniqueFromInitial);
				//Visualiser.updateFrame(reducedPTA.transform.trimGraph(4, reducedPTA.getInit()), pta);
				// in these experiments I cannot use SICCO merging because it will stop any mergers with an initial state.
				learnerOfPairs = new LearningAlgorithms.ReferenceLearner(learnerEval, reducedPTA,scoringToUse);
				
				System.out.println("PTApremerge size: "+reducedPTA.getStateNumber()+" states, "+reducedPTA.getAcceptStateNumber()+" accept-states and "+reducedPTA.pathroutines.countEdges()+" transitions");
				learnerOfPairs.setLabelsLeadingFromStatesToBeMerged(Arrays.asList(new Label[]{uniqueFromInitial}));
				actualAutomaton = learnerOfPairs.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());

				DifferenceToReference similarityMeasure = getMeasure(actualAutomaton,referenceGraph,testSet);
				System.out.println(sample+" PTA permerge, similarity = "+similarityMeasure.getValue()+" ( "+similarityMeasure+" )");
			}
			 */
		}
		
		return outcome;
	}
	
	public static void main(String []args) throws IOException
	{
		String outDir = "tmp"+File.separator+"smallvshuge";//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		if (!new java.io.File(outDir).isDirectory())
		{
			if (!new java.io.File(outDir).mkdir())
			{
				System.out.println("failed to create a work directory");return ;
			}
		}
		String outPathPrefix = outDir + File.separator;
		RunSubExperiment<ThreadResult> experimentRunner = new RunSubExperiment<PairQualityLearner.ThreadResult>(ExperimentRunner.getCpuNumber(),"data",new String[]{PhaseEnum.RUN_STANDALONE.toString()});


		LearnerEvaluationConfiguration eval = UASExperiment.constructLearnerInitConfiguration();eval.config.setOverride_usePTAMerging(true);
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int ThreadNumber = ExperimentRunner.getCpuNumber();
		
		ExecutorService executorService = Executors.newFixedThreadPool(ThreadNumber);
		final int samplesPerFSMSize = 4;
		final int minStateNumber = 20;
		final int attemptsPerFSM = 4;

		final DrawGraphs gr = new DrawGraphs();
		final RBoxPlot<String> BCR_vs_experiment = new RBoxPlot<String>("experiment","BCR",new File(outPathPrefix+"BCR_vs_experiment.pdf"));
		final RBoxPlot<String> diff_vs_experiment = new RBoxPlot<String>("experiment","Structural difference",new File(outPathPrefix+"diff_vs_experiment.pdf"));
		
		final StringBuffer csv = new StringBuffer();
		StringBuffer firstLine = new StringBuffer(),secondLine = new StringBuffer(),thirdLine = new StringBuffer(), fourthLine = new StringBuffer();
		StringBuffer lines[]=new StringBuffer[]{firstLine,secondLine,thirdLine,fourthLine};
		for(ScoringToApply scoringMethod:UASExperiment.listOfScoringMethodsToApply())
		{
			// first column is for the experiment name hence it is appropriate for appendToLines to start by adding a separator.
			LearningSupportRoutines.appendToLines(lines,new String[]{"posNeg","reference",scoringMethod.toString()},new String[]{"BCR","Diff","States"});
			//LearningSupportRoutines.appendToLines(lines,new String[]{"posNeg","constraints",scoringMethod.toString()},new String[]{"BCR","Diff","States"});
			//LearningSupportRoutines.appendToLines(lines,new String[]{"posNeg","premerge",scoringMethod.toString()},new String[]{"BCR","Diff","States"});
		}
		for(StringBuffer line:lines)
		{
			csv.append(line.toString());csv.append('\n');
		}
		
    	processSubExperimentResult<PairQualityLearner.ThreadResult> resultHandler = new processSubExperimentResult<PairQualityLearner.ThreadResult>() {

			public void recordResultsFor(StringBuffer csvLine, RunSubExperiment<ThreadResult> experimentrunner, String experimentName,ScoringToApply scoring,ScoresForGraph difference) throws IOException
			{
				String scoringAsString = null;
				switch(scoring)
				{
				case SCORING_EDSM:
					scoringAsString = "E";break;
				case SCORING_EDSM_1:
					scoringAsString = "E1";break;
				case SCORING_EDSM_2:
					scoringAsString = "E2";break;
				case SCORING_SICCO:
					scoringAsString = "S";break;
				case SCORING_SICCO_NIS:
					scoringAsString = "SI";break;
				case SCORING_SICCO_REDBLUE:
					scoringAsString = "SRB";break;
				case SCORING_SICCO_RED:
					scoringAsString = "SR";break;
				default:
					throw new IllegalArgumentException("Unexpected scoring");
				}
				LearningSupportRoutines.addSeparator(csvLine);csvLine.append(difference.differenceBCR.getValue());
				LearningSupportRoutines.addSeparator(csvLine);csvLine.append(difference.differenceStructural.getValue());
				LearningSupportRoutines.addSeparator(csvLine);csvLine.append(difference.nrOfstates.getValue());

				System.out.println(experimentName + "_" + scoringAsString+" has BCR  score of "+difference.differenceBCR.getValue() +" and diffscore " + difference.differenceStructural.getValue()+
						", learning outcome has "+difference.nrOfstates.getValue()+" more states than the original");
				experimentrunner.Record(BCR_vs_experiment,experimentName + "_" + scoringAsString ,difference.differenceBCR.getValue(),null,null);
				experimentrunner.Record(diff_vs_experiment,experimentName + "_" + scoringAsString ,difference.differenceStructural.getValue(),null,null);
			}
			
			@Override
			public void processSubResult(ThreadResult result, RunSubExperiment<ThreadResult> experimentrunner) throws IOException 
			{
				int i=0;
				csv.append(result.samples.get(0).experimentName);
				for(ScoringToApply scoringMethod:UASExperiment.listOfScoringMethodsToApply())
				{
					PairQualityLearner.SampleData score = result.samples.get(i++);
					// the order in which elements are added has to match that where the three lines are constructed. It is possible that I'll add an abstraction for this to avoid such a dependency, however this is not done for the time being.
					recordResultsFor(csv,experimentrunner, score.experimentName+"_R",scoringMethod,score.referenceLearner);
					//recordResultsFor(csv,experimentrunner, score.experimentName+"_P",scoringMethod,score.premergeLearner);
					//recordResultsFor(csv,experimentrunner, score.experimentName+"_C",scoringMethod,score.actualConstrainedLearner);
				}
				csv.append('\n');
				BCR_vs_experiment.drawInteractive(gr);diff_vs_experiment.drawInteractive(gr);
			}
			
			@Override
			public String getSubExperimentName()
			{
				return "Small v.s huge experiments";
			}
			
			@Override
			public DrawGraphs.RGraph[] getGraphs() {
				return new DrawGraphs.RGraph[]{BCR_vs_experiment,diff_vs_experiment};
			}
		};
		List<UASExperiment> listOfExperiments = new ArrayList<UASExperiment>();
		/*
		for(final double threshold:new double[]{1,1.2,1.5,3,10})
		for(final int ifDepth:new int []{0,1})
		for(final boolean onlyPositives:new boolean[]{true,false})
			for(final boolean zeroScoringAsRed:new boolean[]{true,false})
			for(final boolean selectingRed:new boolean[]{false})
			for(final boolean useUnique:new boolean[]{true,false})*/
			{/*
				String selection = "TRUNK"+"I"+ifDepth+"_"+"T"+threshold+"_"+
						(onlyPositives?"P_":"-")+(selectingRed?"R":"-")+(useUnique?"U":"-")+(zeroScoringAsRed?"Z":"-");
		*/
				for(int traceQuantity=15;traceQuantity<=90;traceQuantity+=25)
					for(int traceLengthMultiplier=1;traceLengthMultiplier<=1;traceLengthMultiplier+=3)
				{
					try
					{
						int numberOfTasks = 0;
						for(int states=minStateNumber;states < minStateNumber+1;states+=5)
							for(int sample=0;sample<samplesPerFSMSize;++sample)
								for(int attempt=0;attempt<attemptsPerFSM;++attempt)
								{
									LearnerEvaluationConfiguration ev = new LearnerEvaluationConfiguration(eval);
									ev.config = eval.config.copy();ev.config.setOverride_maximalNumberOfStates(states*LearningAlgorithms.maxStateNumberMultiplier);
									SmallvsHugeExperiment learnerRunner = new SmallvsHugeExperiment(states,sample,attempt,1+numberOfTasks,traceQuantity, traceLengthMultiplier, ev);
									learnerRunner.setAlwaysRunExperiment(true);
									listOfExperiments.add(learnerRunner);
								}
					}
					catch(Exception ex)
					{
						IllegalArgumentException e = new IllegalArgumentException("failed to compute, the problem is: "+ex);e.initCause(ex);
						if (executorService != null) { executorService.shutdown();executorService = null; }
						throw e;
					}
				}
			}
			
			
    	for(UASExperiment e:listOfExperiments)
    		experimentRunner.submitTask(e);
    	experimentRunner.collectOutcomeOfExperiments(resultHandler);
		
    	FileWriter writer = null;
    	try
    	{
    		writer = new FileWriter(outPathPrefix+"results.csv");
    		writer.write(csv.toString());
    	}
    	finally
    	{
    		if (writer != null)
    			writer.close();
    	}
		if (BCR_vs_experiment != null) BCR_vs_experiment.drawPdf(gr);
		if (diff_vs_experiment != null) diff_vs_experiment.drawPdf(gr);
		
		DrawGraphs.end();// the process will not terminate without it because R has its own internal thread
		experimentRunner.successfulTermination();

	}

}
