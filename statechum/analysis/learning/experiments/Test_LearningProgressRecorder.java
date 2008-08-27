/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
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

package statechum.analysis.learning.experiments;


import java.beans.XMLDecoder;
import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import edu.uci.ics.jung.io.GraphMLFile;
import statechum.ArrayOperations;
import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.Configuration.LEARNER;
import statechum.analysis.learning.RPNIBlueAmberFringeLearner;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.RPNIBlueFringeLearnerTestComponentOpt;
import statechum.analysis.learning.observers.Learner;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.observers.RecordProgressDecorator;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.model.testset.*;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

public abstract class Test_LearningProgressRecorder extends AbstractExperiment 
{	
	public static final String origDir = "/home/kirill/W_experiment/matched/output_25StatesCrowded_-1";

	/** This one is not static because it refers to the frame to display results. */
	public static abstract class RPNIEvaluator extends LearnerEvaluator
	{
		PTASequenceEngine sPlus = null, sMinus = null;
		public RPNIEvaluator(String inputFile, int per, int instance, AbstractExperiment exp)
		{
			super(inputFile, per, instance, exp);			
		}

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParameters(Configuration c);

		protected AtomicInteger questionNumber = new AtomicInteger(0);
		
		@Override
		protected String getFileName(FileType fileNameType)
		{
			String percentValue = "";
			if (experiment.useStages())
				percentValue = "-"+percent;
			return fileNameType.getFileName(experiment.getOutputDir()+File.separator+instanceID+"_"+(new File(inputFileName).getName())+"_"+config.getLearnerToUse(),percentValue); 
		}
		
		/** The number this file went under when run through Dec 2007 version of the tool. */
		int number =-1;
		
		/** This method is executed on an executor thread. */
		public void runTheExperiment()
		{
			int size = 4*graph.getStateNumber();
			RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),5);// the seed for Random should be the same for each file
			int percentPerChunk = 10;
			int nrPerChunk = size/(100/percentPerChunk);nrPerChunk+=nrPerChunk % 2;// make the number even
			rpg.generatePosNeg(2*nrPerChunk , 100/percentPerChunk);// 2* reflects the fact that nrPerChunk denotes the number of elements in both chunks (positive and negative) combined.
			/*
			RPNIBlueFringeLearner l = new RPNIBlueFringeLearnerTestComponentOpt(null,config)
			{
				@Override
				protected Pair<Integer,String> checkWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<String> question, 
						@SuppressWarnings("unused") final Object [] moreOptions)
				{
					questionNumber.addAndGet(1);
					return new Pair<Integer,String>(graph.paths.tracePath(question),null);
				}
			};
			sPlus = rpg.getExtraSequences(percent/10-1);sMinus = rpg.getAllSequences(percent/10-1);
			LearnerGraph learned = learn(l,sMinus);
			 */
			LearnerGraph learnt = null;
			Collection<List<String>> minusTrainingSet = null, testSet = null;
			try {
				synchronized (LearnerGraph.syncObj) 
				{// ensure that the calls to Jung's vertex-creation routines do not occur on different threads.
			    	GraphMLFile graphmlFile = new GraphMLFile();
			    	graphmlFile.setGraphMLFileHandler(new ExperimentGraphMLHandler());
			    	final String mostOfFileName = "_"+(new File(inputFileName).getName());
			    	assert new File(origDir).isDirectory();
			    	for(String name:new File(origDir).list(new FilenameFilter(){
						public boolean accept(@SuppressWarnings("unused") File dir, String fileName) {
							return fileName.contains(mostOfFileName);
						}}))
			    	{
			    		int firstUnderscore = name.indexOf('_');assert firstUnderscore > 0;
			    		int currentNumber = Integer.parseInt(name.substring(0, firstUnderscore));
			    		if (number < 0) number = currentNumber;else assert number == currentNumber;
			    	}
			    	assert number >=0;
			    	/*
			    	learned = new LearnerGraph(graphmlFile.load(
			    			FileType.LEARNT.getFileName(dataDir+File.separator+number+"_"+(new File(inputFileName).getName()),"")
			    			),config);
			    	learned.transform.invertStates();
			    	learned.transform.writeGraphML(getFileName(FileType.LEARNT));
			    	*/
				}
				//computeStateScores.writeGraphML(learned, getFileName(FileType.LEARNT));
				XMLDecoder inData = new XMLDecoder(new FileInputStream(FileType.MINUS_AND_TEST.getFileName(origDir+File.separator+number+"_"+(new File(inputFileName).getName()),"")));
				minusTrainingSet = (Collection<List<String>>)inData.readObject();
				testSet = (Collection<List<String>>)inData.readObject();
				inData.close();
			} catch (IOException e) {
				StringWriter wr = new StringWriter();e.printStackTrace(new PrintWriter(wr));
				IllegalArgumentException ex = new IllegalArgumentException(e.getMessage());ex.initCause(e);
				throw ex;
			}
			
			/* Loads the original set. */
			PTASequenceEngine sOrigMinus = new PTASequenceEngine() 
			{
				{
					init(graph.new FSMImplementation() {
						@Override
						public boolean shouldBeReturned(@SuppressWarnings("unused")	Object elem) 
						{
							return elem != null;
						}
						
					});
				}
			};
			SequenceSet minusInit = sOrigMinus.new SequenceSet();minusInit.setIdentity();minusInit.cross(minusTrainingSet);
			int totalLenOrig = 0, origNumber=0;
			for(List<String> seq:minusTrainingSet)
			{
				assert seq.size() > 1;
				assert graph.getVertex(seq) == null;// the seq is negative indeed
				assert graph.getVertex(seq.subList(0, seq.size()-1)) != null;// the -1 prefix is positive
				assert sOrigMinus.containsSequence(seq);// make sure PTA construction did not fumble.
				assert graph.paths.tracePath(seq) == seq.size()-1;// and another check that a -1 prefix is ok
				totalLenOrig+=seq.size();origNumber++;
			}
			assert sOrigMinus.getData().size() == 0;// all negatives

			/* Builds the new set. */
			PTASequenceEngine sNewMinus = rpg.getAllSequences(percent/10-1);
			int totalLenNew = 0, newNumber = 0;
			for(List<String> seq:sNewMinus.getData(PTASequenceEngine.truePred))
			{
				assert seq.size() > 1;
				assert graph.getVertex(seq) == null;
				assert graph.getVertex(seq.subList(0, seq.size()-1)) != null;
				assert graph.paths.tracePath(seq) == seq.size()-1;
				totalLenNew+=seq.size();newNumber++;
			}
			// 22,23
			String comparison = "Orig ave len: "+FS+((double)totalLenOrig)/origNumber+FS+
			// 24,25
					" new ave len: "+FS+((double)totalLenNew)/newNumber+FS+
			// 26,27
					" fudge status: "+FS+rpg.getFudgeDetails();
			
			//int numberOfMinusSequences = minusTrainingSet.size();
			assert sNewMinus.getData().size() == 0;// all negatives
			
			if (config.getLearnerToUse() == LEARNER.LEARNER_BLUEFRINGE_DEC2007)
				sMinus = sOrigMinus;
			else
				sMinus = sNewMinus;
			
			
			RPNIBlueFringeLearner l = null;
			
			if (config.getLearnerToUse() == LEARNER.LEARNER_BLUEAMBER)
			{
				l = new RPNIBlueAmberFringeLearner(null,config)
				{
					@Override
					protected Pair<Integer,String> checkWithEndUser(
							@SuppressWarnings("unused")	LearnerGraph model,
							List<String> question, 
							@SuppressWarnings("unused") final Object [] moreOptions)
					{
						questionNumber.addAndGet(1);
						return new Pair<Integer,String>(graph.paths.tracePath(question),null);
					}
				};
			}
			else
			{
				l = new RPNIBlueFringeLearnerTestComponentOpt(null,config)
				{
					@Override
					protected Pair<Integer,String> checkWithEndUser(
							@SuppressWarnings("unused")	LearnerGraph model,
							List<String> question, 
							@SuppressWarnings("unused") final Object [] moreOptions)
					{
						questionNumber.addAndGet(1);
						return new Pair<Integer,String>(graph.paths.tracePath(question),null);
					}
				};
			}
			RecordProgressDecorator recorder = null;
			try {
				recorder = new RecordProgressDecorator(l.getLearner(),new java.io.FileOutputStream(getFileName(FileType.LOG)),1,config,true);
			} catch (IOException e) {
				statechum.Helper.throwUnchecked("could not open log file for writing", e);
			}
			Collection<List<String>> graphTestSet = graph.wmethod.getFullTestSet(1);
			recorder.writeLearnerEvaluationData(new ProgressDecorator.LearnerEvaluationConfiguration(graph,graphTestSet,config,null));
			l.getLearner().setTopLevelListener(recorder);
			learnt = learn(recorder,sMinus);
			//learned = new LearnerGraph(config);
			
			PTASequenceEngine testSetEngine = new PTA_FSMStructure(graph);
			SequenceSet ptaTestSet = testSetEngine.new SequenceSet();ptaTestSet.setIdentity();
			ptaTestSet.cross(graphTestSet);

			PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(learnt);
			
			PosNegPrecisionRecall ptaPR = precRec.crossWith(sMinus);
			PosNegPrecisionRecall prNeg = precRec.crossWith(testSetEngine);
			
			// Columns 3 and 4
			result = result+prNeg.precision+FS+prNeg.recall;
			
			result = result + FS + questionNumber+ FS + // 5
				// Columns 6 and 7
				ptaPR.precision  + FS + ptaPR.recall + FS;
			
			// 19 and 20
			result = result + FS + graph.paths.getExtentOfCompleteness() + FS + learnt.paths.getExtentOfCompleteness() + FS +
				l.getRestarts(); // 21
		}

		private LearnerGraph learn(Learner l, PTASequenceEngine pta)
		{
			LearnerGraph learningOutcome = null;
			changeParameters(config);
			if (config.getLearnerToUse() == LEARNER.LEARNER_BLUEFRINGE_DEC2007)
			{
				throw new UnsupportedOperationException();
				//l.loadPTA(origDir+File.separatorChar+number+"_"+(new File(inputFileName).getName())+"_data");
			}
			else 
			{
				//int ptaSize = pta.numberOfLeafNodes();
				//l.init(pta, ptaSize,ptaSize);// our imaginary positives are prefixes of negatives.
				learningOutcome = l.learnMachine(new LinkedList<List<String>>(), ArrayOperations.sort(pta.getData(PTASequenceEngine.truePred)));
			}
			
			if (config.getLearnerToUse() == LEARNER.LEARNER_BLUEFRINGE_DEC2007) WMethod.checkM(LearnerGraph.loadGraph(origDir+File.separatorChar+number+"_"+(new File(inputFileName).getName())+"_learnt", config),
					learningOutcome);
			try {
				learningOutcome.transform.writeGraphML(getFileName(FileType.LEARNT));
			} catch (IOException e) {
				e.printStackTrace();
			}

			return learningOutcome;
		}
	}
	
	public int [] getStages()
	{
		return new int[]{100};
	}
		
	static class Experiment extends Test_LearningProgressRecorder
	{
		protected final Configuration conf;
		
		/** Constructs an experiment class
		 * 
		 * @param qg the questioning strategy to use.
		 * @param limit the limit on the number of paths to choose when looking for paths between a pair of states.
		 * @param useSpeculative whether to use speculative question asking.
		 */
		public Experiment(Configuration.QuestionGeneratorKind qg, int limit, boolean useSpeculative, 
				Configuration.LEARNER learnerKind)
		{
			super();conf=(Configuration)Configuration.getDefaultConfiguration().clone();
			conf.setQuestionGenerator(qg);conf.setQuestionPathUnionLimit(limit);conf.setSpeculativeQuestionAsking(useSpeculative);
			conf.setLearnerToUse(learnerKind);
		}

		/** Constructs an experiment class for checking whether the improved merger and
		 * question generator does the same thing as the old one.
		 * 
		 * @param qg the questioning strategy to use.
		 * @param limit the limit on the number of paths to choose when looking for paths between a pair of states.
		 */
		public Experiment()
		{
			super();conf=(Configuration)Configuration.getDefaultConfiguration().clone();
			conf.setQuestionGenerator(Configuration.QuestionGeneratorKind.CONVENTIONAL);
			conf.setSpeculativeQuestionAsking(true);
			conf.setQuestionPathUnionLimit(-1);conf.setConsistencyCheckMode(true);
		}

		public List<LearnerEvaluatorGenerator> getLearnerGenerators() {
			return Arrays.asList(new LearnerEvaluatorGenerator[] {
				new LearnerEvaluatorGenerator() {
					@Override
					LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, AbstractExperiment exp) {
						return new RPNIEvaluator(inputFile, percent, instanceID, exp)
						{
							@Override
							protected void changeParameters(Configuration c) 
							{
								c.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);						
								//c.setCertaintyThreshold(2);c.setGeneralisationThreshold(3);
								//c.setMinCertaintyThreshold(0); //question threshold
								//c.setKlimit(0);c.setLearnerScoreMode(Configuration.ScoreMode.KTAILS);
								c.setQuestionGenerator(conf.getQuestionGenerator());
								c.setQuestionPathUnionLimit(conf.getQuestionPathUnionLimit());
								c.setSpeculativeQuestionAsking(conf.isSpeculativeQuestionAsking());
								c.setLearnerToUse(conf.getLearnerToUse());
							}

							@Override
							protected String getLearnerName() {
								return "Questions: "+conf.getQuestionGenerator()+
								"; union limited to "+conf.getQuestionPathUnionLimit()+"; using learner: "+conf.getLearnerToUse();
							}
						};
					}
				}
			});
		}
	}
	
	public static void main(String []args)
	{
		//DeterministicDirectedSparseGraph.VertexID.comparisonKind = DeterministicDirectedSparseGraph.VertexID.ComparisonKind.COMPARISON_LEXICOGRAPHIC_ORIG;
		try {
			LearnerGraph.testMode=true;
			//Experiment consistencyExperiment = new Experiment();consistencyExperiment.setOutputDir("consistency_");consistencyExperiment.runExperiment(args);// Consistency check
			LearnerGraph.testMode=false;
		} catch (Exception e1) {
			e1.printStackTrace();
		}
			
		for(Configuration.QuestionGeneratorKind qk:new Configuration.QuestionGeneratorKind[]{
				//Configuration.QuestionGeneratorKind.ORIGINAL,
				Configuration.QuestionGeneratorKind.CONVENTIONAL,
				//Configuration.QuestionGeneratorKind.CONVENTIONAL_IMPROVED,
				//Configuration.QuestionGeneratorKind.SYMMETRIC
				})
			for(Configuration.LEARNER learner:new Configuration.LEARNER[]{Configuration.LEARNER.LEARNER_BLUEFRINGE,Configuration.LEARNER.LEARNER_BLUEAMBER})
				for(int limit:((qk == Configuration.QuestionGeneratorKind.ORIGINAL?new int[]{-1}:
					new int[]{-1})))//,1,3})))
				{
					AbstractExperiment experiment = new Experiment(qk,limit,false,learner);

					try
					{
						experiment.runExperiment(args);
					}
					catch(Exception ex)
					{
						ex.printStackTrace();
					}
/*
					try
					{// the above might've failed, but we still try to build csv files from result.csv
						String ending = experimentDescription+".csv";
						experiment.postProcessIntoR(2,true, 3, new File(experiment.getOutputDir(),"precision"+ending));
						experiment.postProcessIntoR(2,true, 4, new File(experiment.getOutputDir(),"recall"+ending));
						experiment.postProcessIntoR(2,true, 5, new File(experiment.getOutputDir(),"questionNumber"+ending));
						experiment.postProcessIntoR(2,true, 16, new File(experiment.getOutputDir(),"linearA"+ending));
						experiment.postProcessIntoR(2,true, 17, new File(experiment.getOutputDir(),"linearN"+ending));
						experiment.postProcessIntoR(2,true, 18, new File(experiment.getOutputDir(),"linearB"+ending));
						experiment.postProcessIntoR(2,true, 20, new File(experiment.getOutputDir(),"completeness"+ending));
						experiment.postProcessIntoR(2,true, 21, new File(experiment.getOutputDir(),"restarts"+ending));

						experiment.postProcessIntoR(2,true, 23, new File(experiment.getOutputDir(),"origLen"+ending));
						experiment.postProcessIntoR(2,true, 25, new File(experiment.getOutputDir(),"newLen"+ending));
						experiment.postProcessIntoR(2,true, 27, new File(experiment.getOutputDir(),"fudgeStats"+ending));
					}
					catch(Exception ex)
					{
						ex.printStackTrace();
					}
*/					
				}
			
	}
}
