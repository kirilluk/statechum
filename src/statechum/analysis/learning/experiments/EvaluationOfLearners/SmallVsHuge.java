/* Copyright (c) 2016 The University of Sheffield.
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
package statechum.analysis.learning.experiments.EvaluationOfLearners;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.DrawGraphs.CSVExperimentResult;
import statechum.analysis.learning.DrawGraphs.RBagPlot;
import statechum.analysis.learning.DrawGraphs.RBoxPlot;
import statechum.analysis.learning.DrawGraphs.SGEExperimentResult;
import statechum.analysis.learning.DrawGraphs.SquareBagPlot;
import statechum.analysis.learning.experiments.ComputePathLengthDistribution;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.UASExperiment;
import statechum.analysis.learning.experiments.EvaluationOfLearners.EvaluationOfLearnersParameters.LearningType;
import statechum.analysis.learning.experiments.PairSelection.ExperimentResult;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.StateMergingStatistics;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms.ScoringToApply;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ScoresForGraph;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;
import statechum.analysis.learning.experiments.PaperUAS.ExperimentPaperUAS2;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.PhaseEnum;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.RunSubExperiment;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner.processSubExperimentResult;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.RandomPathGenerator.RandomLengthGenerator;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;
import statechum.analysis.learning.DrawGraphs.RBoxPlotP;
import statechum.collections.MapWithSearch;

public class SmallVsHuge extends UASExperiment<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>>
{
	public static final String directoryNamePrefix = "small_vs_huge";
	public static final String directoryExperimentResult = "experimentresult"+File.separator;
	
	public SmallVsHuge(SmallVsHugeParameters parameters, LearnerEvaluationConfiguration eval)
	{
		super(parameters,eval,directoryNamePrefix);
	}
		
	/** Counts the number of times a path has a repeated label in it. This is used to determine the number of single-state loops in the path. 
	 * 
	 * @param path path to evaluate
	 * @return number of repeats.
	 */
	public static int calculateRepeatedStates(List<Label> path, LearnerGraph reference)
	{
		int count = 0;
		CmpVertex curVertex = reference.getInit();
		for(Label l:path)
		{
			CmpVertex nextState = reference.transitionMatrix.get(curVertex).get(l);
			assert nextState != null;
			if (nextState == curVertex)
				++count;
			curVertex = nextState;
		}
		
		return count; 
	}
	public static final Configuration.ScoreMode conventionalScoringToUse[] = new Configuration.ScoreMode[]{Configuration.ScoreMode.GENERAL, Configuration.ScoreMode.GENERAL_PLUS_NOFULLMERGE};
	
	@Override
	public ExperimentResult<SmallVsHugeParameters> runexperiment() throws Exception 
	{
		final double density = par.states*par.perStateSquaredDensityMultipliedBy10/10;
		final int alphabet = par.states*par.alphabetMultiplier;
		ExperimentResult<SmallVsHugeParameters> outcome = new ExperimentResult<SmallVsHugeParameters>(par);
		ConstructRandomFSM fsmConstruction = new ConstructRandomFSM();
		fsmConstruction.generateFSM(new Random(par.seed*31+par.states), alphabet, par.states, density, par.seed, true, learnerInitConfiguration);// par.pickUniqueFromInitial
		referenceGraph = fsmConstruction.referenceGraph;
		final Random rndFSM = new Random(par.attempt*23+par.seed);
		assert fsmConstruction.uniqueFromInitial != null : "unique transition has to be available";
		final LearnerGraph pta = new LearnerGraph(learnerInitConfiguration.config);
		
		// In the generation of traces, we generate par.traceQuantity traces, each consisting of par.lengthmult traces concatenated together.
		final int tracesToGenerate = par.states*par.traceQuantity*par.lengthmult;

		RandomPathGenerator generator = new RandomPathGenerator(referenceGraph,rndFSM,5,referenceGraph.getVertex(Arrays.asList(new Label[]{fsmConstruction.uniqueFromInitial})));
		// the number of traces generated is twice the provided number because it is expected that both positive and negative traces are generated.
		generator.generateRandomPosNeg(2*tracesToGenerate, 1, false, new RandomLengthGenerator() {
				static final double halfOffset = 0.5;
				
				@Override
				public int getLength() {
					int p = rndFSM.nextInt(100);
					if (p < 42)
						return 1+(int)( (par.states*halfOffset-1)*rndFSM.nextDouble());
					else if (p < 85)
						return 1+(int)( (par.states*halfOffset-1)+par.states*halfOffset*rndFSM.nextDouble());
					else
						return 1+(int)( 2*par.states*halfOffset-1+rndFSM.nextDouble()*par.states*LearningAlgorithms.maxStateNumberMultiplier);
				}

				@Override
				public int getPrefixLength(int len) {
					return len;
				}
			},true,false,null,Arrays.asList(new Label[]{fsmConstruction.uniqueFromInitial}));// this is important: it ensures that traces always start with the supplied label to mimic data from the UAS traces.
		//assert pta.getStateNumber() == pta.getAcceptStateNumber();// we only expect positives since attemptNegatives argument to generateRandomPosNeg is false.

		
		LearnerGraphND inverse = new LearnerGraphND(referenceGraph.config);
		AbstractPathRoutines.buildInverse(referenceGraph,LearnerGraphND.ignoreNone,inverse);
		Map<CmpVertex,List<Label>> shortestPathsIntoInit = new TreeMap<CmpVertex,List<Label>>();
		for(Entry<CmpVertex,List<Label>> path:inverse.pathroutines.computeShortPathsToAllStates().entrySet())
			if (path.getValue().size() == 0)
				shortestPathsIntoInit.put(path.getKey(),Collections.<Label>emptyList());
			else
			{
				Label[] invertedPath = new Label[path.getValue().size()];
				int i=path.getValue().size()-1;
				for(Label elem:path.getValue())
					invertedPath[i--]=elem;
				shortestPathsIntoInit.put(path.getKey(),Arrays.asList(invertedPath));
			}

		ComputePathLengthDistribution pld = new ComputePathLengthDistribution(3,2,referenceGraph.getAcceptStateNumber());

		/** For each state, stores inputs not accepted from it, needed for fast computation of random walks. */
		final Map<CmpVertex,ArrayList<Label>> inputsRejected = new TreeMap<CmpVertex,ArrayList<Label>>();
		Set<Label> alphabetSet = referenceGraph.pathroutines.computeAlphabet();
		for(Entry<CmpVertex, MapWithSearch<Label, Label, CmpVertex>> entry:referenceGraph.transitionMatrix.entrySet())
		{
			Set<Label> negatives = new LinkedHashSet<Label>();
			negatives.addAll(alphabetSet);negatives.removeAll(entry.getValue().keySet());
			ArrayList<Label> rejects = new ArrayList<Label>();rejects.addAll(negatives);
			inputsRejected.put(entry.getKey(), rejects);
		}
		
		Random rnd = new Random(par.seed*31+par.states+par.attempt);
		
		{
			// Now stitch all the sequences together.
			Iterator<List<Label>> traceIter = generator.getAllSequences(0).getData().iterator();
			generator = null;
			for(int seq=0;seq<par.states*par.traceQuantity;++seq)
			{
				List<Label> traceToAdd = new ArrayList<Label>(par.lengthmult*par.states*3);
				for(int elem=0;elem<par.lengthmult;++elem)
					if (traceIter.hasNext())
					{
						List<Label> trace = traceIter.next();
						pld.updatePathLengthStatistics(trace.size());
						List<Label> addedPathToInit = shortestPathsIntoInit.get(referenceGraph.getVertex(trace));
						traceToAdd.addAll(trace);traceToAdd.addAll(addedPathToInit);
					}
				if (!traceToAdd.isEmpty())
				{
					CmpVertex current = referenceGraph.getInit();
					for(int i=0;i<traceToAdd.size();++i)
					{
						current = referenceGraph.transitionMatrix.get(current).get(traceToAdd.get(i));

						ArrayList<Label> rejects = inputsRejected.get(current);
						if (!rejects.isEmpty())
						{
							List<Label> negativeTrace = new ArrayList<Label>(i+2);negativeTrace.addAll(traceToAdd.subList(0, i+1));
							negativeTrace.add(rejects.get(rnd.nextInt(rejects.size())));
							pta.paths.augmentPTA(negativeTrace, false, false, null);
						}
					}
					traceToAdd.add(fsmConstruction.uniqueFromInitial);
					pta.paths.augmentPTA(traceToAdd, true, false, null);
				}
			}
		}
		
		pta.clearColours();
		//System.out.println(""+par.getRowID()+" pta nodes: "+pta.getStateNumber()+", positive: "+pta.getAcceptStateNumber()+", transitions per state: "+((double)referenceGraph.pathroutines.countEdges()/referenceGraph.transitionMatrix.size()));
		
		PairQualityLearner.SampleData sample = new PairQualityLearner.SampleData();
/*
		if (!par.onlyUsePositives)
		{// now we have an even positive/negative split, add negatives by encoding them as if-then automata.
			Map<Label,Set<Label>> infeasiblePairs = LearningSupportRoutines.computeInfeasiblePairs(referenceGraph);
			Map<Label,Set<Label>> subsetOfPairs = new TreeMap<Label,Set<Label>>();
			int totalNegatives = 0;
			for(Entry<Label,Set<Label>> entry:infeasiblePairs.entrySet())
			{
				Set<Label> value = new TreeSet<Label>();
				if (!entry.getValue().isEmpty()) 
				{// we add two entries per label, to mimic what was done with UAS study, where labels could not be repeated.
					Label possibleLabels[]=entry.getValue().toArray(new Label[]{});
					if (possibleLabels.length == 1)
						value.add(possibleLabels[0]);
					else
					{
						for(int cnt=0;cnt < density;++cnt) // in the uas case study, it is one negative at the end of each sequence plus one per element of alphabet.
							value.add(possibleLabels[rnd.nextInt(possibleLabels.length)]);
					}
					totalNegatives+=value.size();
				}
				subsetOfPairs.put(entry.getKey(),value);
			}
			sample.percentageOfLabelsProhibitedPerLabel = (double)totalNegatives/alphabet;
			System.out.println(""+par.getRowID()+" total negatives per label: "+((double)totalNegatives/alphabet));
			LearningSupportRoutines.addIfThenForPairwiseConstraints(learnerInitConfiguration,subsetOfPairs);
			LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(learnerInitConfiguration.ifthenSequences, referenceGraph.pathroutines.computeAlphabet(), learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
			learnerInitConfiguration.config.setUseConstraints(false);// do not use if-then during learning (refer to the explanation above)
			int statesToAdd = 1;// we are adding pairwise constraints hence only one has to be added.
			Transform.augmentFromIfThenAutomaton(pta, null, ifthenAutomata, statesToAdd);// we only need  to augment our PTA once (refer to the explanation above).
		}
		else 
			assert pta.getStateNumber() == pta.getAcceptStateNumber() : "graph with negatives but onlyUsePositives is set";
*/
		sample.distributionOfTraceLength = pld.lengthStatisticsAsString(':');

		
		for(Entry<CmpVertex,List<Label>> path: pta.pathroutines.computeShortPathsToAllStates().entrySet())
		{
			boolean accept = path.getKey().isAccept();
			CmpVertex vert = referenceGraph.getVertex(path.getValue());
			boolean shouldBe = vert==null?false:vert.isAccept();
			assert accept == shouldBe: "state "+vert+" is incorrectly annotated as "+accept+" in path "+path;
		}

		learnerInitConfiguration.testSet = LearningAlgorithms.buildEvaluationSet(referenceGraph);
		UASExperiment.BuildPTAInterface ptaConstructor = new BuildPTAInterface() {
			@Override
			public String kindOfPTA()
			{
				return par.getRowID();
			}
			@Override
			public LearnerGraph buildPTA() throws AugmentFromIfThenAutomatonException, IOException 
			{
				saveGraph(namePTA,pta);
				return pta;
			}
		};
		StateMergingStatistics redReducer = ComputeMergeStatisticsWhenTheCorrectSolutionIsKnown.constructReducerIfUsingSiccoScoring(referenceGraph,par.scoringMethod);
			
		switch(par.learningType)
		{
		case CONVENTIONAL:
			sample.actualLearner = runExperimentUsingConventional(ptaConstructor,redReducer,par,par.scoringMethod,par.scoringForEDSM);
			break;
		case CONVENTIONALUNIQUE:
			sample.actualLearner = runExperimentUsingConventionalWithUniqueLabel(ptaConstructor,redReducer,par,par.scoringMethod,par.scoringForEDSM, fsmConstruction.uniqueFromInitial);
			break;
		case CONSTRAINTS:
			sample.actualLearner = runExperimentUsingConstraints(ptaConstructor,redReducer,par,par.scoringMethod,par.scoringForEDSM,fsmConstruction.uniqueFromInitial);
			break;
		case PREMERGE:
			sample.actualLearner = runExperimentUsingPremerge(ptaConstructor,redReducer,par,false,par.scoringMethod,par.scoringForEDSM,fsmConstruction.uniqueFromInitial);
			break;
		case PREMERGEUNIQUE:
			sample.actualLearner = runExperimentUsingPremerge(ptaConstructor,redReducer,par,true,par.scoringMethod,par.scoringForEDSM,fsmConstruction.uniqueFromInitial);
			break;
		case PTAPREMERGE:
			sample.actualLearner = runExperimentUsingPTAPremerge(ptaConstructor,redReducer,par,par.scoringMethod,par.scoringForEDSM,fsmConstruction.uniqueFromInitial);
			break;
		}

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
		
		return outcome;
	}
	
	public static final String unknownValue = "UNKNOWN";

	public static void main(String []args)
	{
		String outDir = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_EXPERIMENTRESULTS)+File.separator+directoryNamePrefix;//new Date().toString().replace(':', '-').replace('/', '-').replace(' ', '_');
		mkDir(outDir);
		String outPathPrefix = outDir + File.separator;
		mkDir(outPathPrefix+directoryExperimentResult);
		final RunSubExperiment<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>> experimentRunner = new RunSubExperiment<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>>(ExperimentRunner.getCpuNumber(),outPathPrefix + directoryExperimentResult,args);
		SGE_ExperimentRunner.configureCPUFreqNormalisation();
		
		LearnerEvaluationConfiguration eval = UASExperiment.constructLearnerInitConfiguration();
		eval.config.setTimeOut(3600000L*8L);// timeout for tasks, in milliseconds, equivalent to 8hrs runtime for an old Xeon 5670 @ 2.93Ghz, modern E5/i7 are 3x faster.
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		
		final int samplesPerFSMSize = 20;
		final int attemptsPerFSM = 2;
		final int stateNumberList[] = new int[]{20};//5,10,20,40};
		
		final RBoxPlotP<String> BCR_vs_experiment = new RBoxPlotP<String>("experiment","BCR",new File(outPathPrefix+"BCR_vs_experiment.pdf"));
		final RBoxPlotP<String> diff_vs_experiment = new RBoxPlotP<String>("experiment","Structural difference",new File(outPathPrefix+"diff_vs_experiment.pdf"));

		final Map<String,RBoxPlotP<String>> plotsBCR=new TreeMap<String,RBoxPlotP<String>>(),plotsDiff=new TreeMap<String,RBoxPlotP<String>>();
		final Map<String,RBoxPlot<String>> plotsTraceLength=new TreeMap<String,RBoxPlot<String>>();
		final Map<String,RBoxPlotP<String>> plotsInvalid=new TreeMap<String,RBoxPlotP<String>>(),plotsMissed=new TreeMap<String,RBoxPlotP<String>>();
		final Map<String,SquareBagPlot> plotsPreVsUnique = new TreeMap<String,SquareBagPlot>();
		final Map<String,CSVExperimentResult> tableCSV = new TreeMap<String,CSVExperimentResult>();
		
		final String [] traceLenValues = new String[] {"<50%","50..100%",">=100%"};
		for(int q=0;q<stateNumberList.length;++q)
		{
			String stateNum = ExperimentPaperUAS2.sprintf("%02d", stateNumberList[q]);
			RBoxPlot<String> traceLenGraph = new RBoxPlot<String>("","Trace Length",new File(outPathPrefix+stateNum+"-tracelen.pdf"));traceLenGraph.setOrderingOfLabels(Arrays.asList(traceLenValues));
			plotsTraceLength.put(stateNum,traceLenGraph);
			for(String descr:new String[] {"E","P","C","U"})
			{
				String fullDescr = ExperimentPaperUAS2.sprintf("%02d-%s", stateNumberList[q],descr);
				plotsBCR.put(fullDescr,new RBoxPlotP<String>("","BCR",new File(outPathPrefix+fullDescr+"-BCR.pdf")));
				plotsDiff.put(fullDescr,new RBoxPlotP<String>("","DIFF",new File(outPathPrefix+fullDescr+"-DIFF.pdf")));
				plotsInvalid.put(fullDescr,new RBoxPlotP<String>("","INVALID",new File(outPathPrefix+fullDescr+"-INVALID.pdf")));
				plotsMissed.put(fullDescr,new RBoxPlotP<String>("","MISSED",new File(outPathPrefix+fullDescr+"-MISSED.pdf")));
				tableCSV.put(fullDescr,new CSVExperimentResult(new File(outPathPrefix+fullDescr+"-table.csv")));
			}
		}
		
		final Map<Integer,Map<Object,Double>> uToBCR = new TreeMap<Integer,Map<Object,Double>>(),pToBCR = new TreeMap<Integer,Map<Object,Double>>();
		
		final CSVExperimentResult resultCSV = new CSVExperimentResult(new File(outPathPrefix+"results.csv"))
				{
					@Override
					/** Adds text to the spreadsheet. */
					public void add(ThreadResultID id, String text)
					{
						super.add(id, text);
						
						String descr = null;
						String rowDetails[]=id.getRowID().split("-",-2);
						int stateNumber = Integer.parseInt(rowDetails[0]);
						int position=-1;
						for(int q=0;q<stateNumberList.length;++q)
							if (stateNumberList[q] == stateNumber)
							{
								position = q;break;
							}
						String []columnText = id.getColumnText();// something like: [preU, GENN, E0, GEN, ARRAY, 4, 16]
						String scoring = columnText[2];
						if (scoring.equals(ScoringToApply.SCORING_SICCO.toString()))
							scoring = "SV";
						String AB = LearningSupportRoutines.padString(columnText[5],'0',2)+":"+LearningSupportRoutines.padString(columnText[6],'0',2);//+"-"+id.getColumnText()[6];
						String label = scoring+":"+AB;
						
						String [] idArray=Arrays.copyOf(columnText, columnText.length);idArray[0]=null;
						String idArrayAsString = Arrays.toString(idArray);
						
						String [] data = text.split(",", -2);
						// data could be ["Success","BCR","Diff","M_Invalid","M_Missed","States","prohibited","traceLength","Time"]
						// actual value: L_OK,0.9613003095975232,0.9744275944097531,0.0,0.0,1.0,1.475,0.3:0.45:0.25,9
						double bcr = Double.parseDouble(data[1]), diff = Double.parseDouble(data[2]);//, nrOfStates = Double.parseDouble(data[2]);
						double invalidMergers = Double.parseDouble(data[3]),missedMergers = Double.parseDouble(data[4]);
						if (id.getColumnText()[0].equals(LearningType.CONVENTIONAL.toString()))
							descr = "E";
						else
							if (id.getColumnText()[0].equals(LearningType.PREMERGE.toString()))
							{
								descr = "P";
								Map<Object,Double> entry = pToBCR.get(stateNumberList[position]);
								if (entry == null)
								{
									entry = new LinkedHashMap<Object,Double>();pToBCR.put(stateNumberList[position], entry);
								}
								entry.put(idArrayAsString, bcr);
							}
							else
								if (id.getColumnText()[0].equals(LearningType.PREMERGEUNIQUE.toString()))
								{
									descr = "U";
									Map<Object,Double> entry = uToBCR.get(stateNumberList[position]);
									if (entry == null)
									{
										entry = new LinkedHashMap<Object,Double>();uToBCR.put(stateNumberList[position], entry);
									}
									entry.put(idArrayAsString, bcr);
								}
								else
									if (id.getColumnText()[0].equals(LearningType.CONSTRAINTS.toString()))
										descr = "C";
						
						String fullDescr = ExperimentPaperUAS2.sprintf("%02d-%s", stateNumberList[position],descr);
		
						plotsBCR.get(fullDescr).add(label, bcr);
						plotsDiff.get(fullDescr).add(label, diff);
						String [] traceLenStats = data[7].split(":", -2);
						RBoxPlot<String> traceLengthPlot = plotsTraceLength.get(ExperimentPaperUAS2.sprintf("%02d", stateNumberList[position]));
						for(int i=0;i<traceLenValues.length;++i)
							traceLengthPlot.add(traceLenValues[i], Double.parseDouble(traceLenStats[i]));
						plotsInvalid.get(fullDescr).add(label, invalidMergers);
						plotsMissed.get(fullDescr).add(label, missedMergers);
						tableCSV.get(fullDescr).add(id, text);
					}
				};
		resultCSV.setMissingValue(unknownValue);
				
    	processSubExperimentResult<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>> resultHandler = new processSubExperimentResult<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>>() {

			@Override
			public void processSubResult(ExperimentResult<SmallVsHugeParameters> result, RunSubExperiment<SmallVsHugeParameters,ExperimentResult<SmallVsHugeParameters>> experimentrunner) throws IOException 
			{
				ScoresForGraph difference = result.samples.get(0).actualLearner;
				double percentageOfProhibititedLabelsPerLabel = result.samples.get(0).percentageOfLabelsProhibitedPerLabel;
				String distributionOfGeneratedTraceLength = result.samples.get(0).distributionOfTraceLength;
				StringBuffer csvLine = new StringBuffer();
				csvLine.append(difference.whetherLearningSuccessfulOrAborted);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.differenceBCR.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.differenceStructural.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.invalidMergers);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.missedMergers);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(difference.nrOfstates.getValue());
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(percentageOfProhibititedLabelsPerLabel);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(distributionOfGeneratedTraceLength);
				CSVExperimentResult.addSeparator(csvLine);csvLine.append(Math.round(difference.executionTime/1000000000.));// execution time is in nanoseconds, we only need seconds.
				experimentrunner.RecordCSV(resultCSV, result.parameters, csvLine.toString());
				String experimentName = result.parameters.states+"-"+result.parameters.perStateSquaredDensityMultipliedBy10+"-"+result.parameters.traceQuantity+"-"+result.parameters.lengthmult+"_"+EvaluationOfLearnersParameters.ptaMergersToString(result.parameters.ptaMergers)+"-"+result.parameters.matrixType.name;
				experimentrunner.RecordR(BCR_vs_experiment,experimentName ,difference.differenceBCR.getValue(),null,null);
				experimentrunner.RecordR(diff_vs_experiment,experimentName,difference.differenceStructural.getValue(),null,null);
			}

			@Override
			public SGEExperimentResult[] getGraphs() {
				return new SGEExperimentResult[]{BCR_vs_experiment,diff_vs_experiment,resultCSV};
			}
		};
		ScoringModeScore scoringPairEDSM = new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_EDSM),
				scoringPairEDSM4 = new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_EDSM_4),
				scoringPairSICCO = new ScoringModeScore(Configuration.ScoreMode.GENERAL_NOFULLMERGE,ScoringToApply.SCORING_SICCO);
		
		List<SmallVsHuge> listOfExperiments = new ArrayList<SmallVsHuge>();
		try
		{
			for(int states:stateNumberList)
			for(int alphabetMult:new int[] {2})
			for(int density:new int[] {0,1,2,3})
			{
				int seedThatIdentifiesFSM=0;
				for(int sample=0;sample<samplesPerFSMSize;++sample,++seedThatIdentifiesFSM)
					for(int attempt=0;attempt<attemptsPerFSM;++attempt)
					{
						for(int traceQuantity:new int[]{states/4,states/2})
							for(int traceLengthMultiplier:new int[]{1,2,4,16})
								//if (traceQuantity*traceLengthMultiplier <= 64)
									for(Configuration.STATETREE matrix:new Configuration.STATETREE[]{Configuration.STATETREE.STATETREE_ARRAY})
										for(boolean pta:new boolean[]{false})
										{
											for(ScoringModeScore scoringPair:new ScoringModeScore[]{
													scoringPairEDSM,scoringPairEDSM4,scoringPairSICCO
											})
											if (states <= 20 || scoringPair == scoringPairEDSM || scoringPair == scoringPairSICCO)
											{
													for(LearningType type:new LearningType[]{LearningType.PREMERGEUNIQUE,LearningType.PREMERGE,LearningType.CONVENTIONAL,LearningType.CONSTRAINTS})
													if (states <= 20 || type != LearningType.CONSTRAINTS)
													{
														LearnerEvaluationConfiguration ev = new LearnerEvaluationConfiguration(eval);
														ev.config.setOverride_maximalNumberOfStates(states*LearningAlgorithms.maxStateNumberMultiplier);
														ev.config.setOverride_usePTAMerging(pta);ev.config.setTransitionMatrixImplType(matrix);
														
														SmallVsHugeParameters par = new SmallVsHugeParameters(scoringPair.scoringForEDSM,scoringPair.scoringMethod,type,pta,matrix);
														par.setParameters(states, alphabetMult,density, sample, attempt, seedThatIdentifiesFSM, traceQuantity, traceLengthMultiplier);
														par.setPickUniqueFromInitial(true);
														SmallVsHuge learnerRunner = new SmallVsHuge(par, ev);
														learnerRunner.setAlwaysRunExperiment(true);// ensure that experiments that have no results are re-run rather than just re-evaluated (and hence post no execution time).
														listOfExperiments.add(learnerRunner);
													}
											}
										}
						}
			}
		}
		catch(Exception ex)
		{
			Helper.throwUnchecked("failed to compute", ex);
		}
		DrawGraphs gr = new DrawGraphs();
		try
		{
	    	for(SmallVsHuge e:listOfExperiments)
	    		experimentRunner.submitTask(e);
	    	experimentRunner.collectOutcomeOfExperiments(resultHandler);
	    	
	    	if (experimentRunner.getPhase() == PhaseEnum.COLLECT_RESULTS)
	    	{// post-process the results if needed.
	    		for(int q=0;q<stateNumberList.length;++q)
	    		{
	    			plotsTraceLength.get(ExperimentPaperUAS2.sprintf("%02d",stateNumberList[q])).reportResults(gr);
	    			for(String descr:new String[] {"E","P","C","U"})
	    			{
	    				String fullDescr = ExperimentPaperUAS2.sprintf("%02d-%s", stateNumberList[q],descr);
	    				plotsBCR.get(fullDescr).reportResults(gr);plotsDiff.get(fullDescr).reportResults(gr);tableCSV.get(fullDescr).reportResults(gr);
	    				plotsInvalid.get(fullDescr).reportResults(gr);plotsMissed.get(fullDescr).reportResults(gr);
	    			}
	    		}
	    		for(Entry<Integer,Map<Object,Double>> entry:pToBCR.entrySet())
	    		{
	    			String descrComparison = ExperimentPaperUAS2.sprintf("%02d", entry.getKey());
	    			SquareBagPlot plot = plotsPreVsUnique.get(descrComparison);
	    			if (plot == null)
	    			{
	    				plot = new SquareBagPlot("Pre", "Pre-Unique" , new File(outPathPrefix+descrComparison+"-unique_vs_non_unique.pdf"),0,1,true);plotsPreVsUnique.put(descrComparison, plot);
	    			}

	    			Map<Object,Double> entryOther = uToBCR.get(entry.getKey());
	    			if (entryOther != null)
	    			{
	    				for(Entry<Object,Double> value:entry.getValue().entrySet())
	    				{
	    					Double v = entryOther.get(value.getKey());
	    					if (v != null)
	    					{
	    						plotsPreVsUnique.get(descrComparison).add(v.doubleValue(), value.getValue().doubleValue());
	    						System.out.println(v.doubleValue()+" "+value.getValue().doubleValue());
	    					}
	    				}
	    				
	    			}
	    		}
	    		for(Entry<Integer,Map<Object,Double>> entry:pToBCR.entrySet())
	    		{
	    			String descrComparison = ExperimentPaperUAS2.sprintf("%02d", entry.getKey());
	    			RBagPlot plot = plotsPreVsUnique.get(descrComparison);
	    			if (plot != null)
	    				plot.reportResults(gr);
	    		}
	    	}
		}
		finally
		{
			experimentRunner.successfulTermination();
			DrawGraphs.end();// this is necessary to ensure termination of the JVM runtime at the end of experiments.
		}
	}
}
