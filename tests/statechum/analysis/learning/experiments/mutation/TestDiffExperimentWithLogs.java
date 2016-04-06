package statechum.analysis.learning.experiments.mutation;

import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration.STATETREE;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.experiments.mutation.ExperimentResult.LONG_V;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.ProgressIndicator;

public class TestDiffExperimentWithLogs {
	@Test
	public void checkLogs() throws FileNotFoundException
	{
		final String logs = "DiffExperimentLogs.xml";
		String path = System.getProperty(G_PROPERTIES.VIZ_DIR.name());
		if (path == null) path=GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.RESOURCES);else path=path+File.separator+"..";
		SystemTestDiffExperiments runner = new SystemTestDiffExperiments(30,5,6);
		// This one compares logs with an actual experiment.
		runner.TestExperiment(new ResultChecker(path+File.separator+logs));
		
		// Uncomment this to record logs.
		//runner.TestExperiment(new ResultRecorder(path+File.separator+logs));
	}
	
	/** Called for each result, could either record results or match them to the recording. */
	public static interface ResultProcessor
	{
		public void process(ExperimentResult result);
		public void finished();
	}
	
	public static class ResultRecorder implements ResultProcessor
	{
        XMLEncoder encoder = null;
		OutputStream outputStream = null;
		
        public ResultRecorder(String where) throws FileNotFoundException
        {
        	outputStream = new FileOutputStream(where);
        	encoder = new XMLEncoder(outputStream);
        }
        
		@Override
		public void process(ExperimentResult result) {
			result.record(encoder);
		}

		@Override
		public void finished() {
			encoder.close();if (outputStream != null) { try { outputStream.close();outputStream=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
		}
		
	}
	
	public static class ResultChecker implements ResultProcessor
	{
        XMLDecoder decoder = null;
		InputStream inputStream = null;
		
		double difference = 0.;
		
        public ResultChecker(String where) throws FileNotFoundException
        {
        	inputStream = new FileInputStream(where);
        	decoder = new XMLDecoder(inputStream);
        }
        
		@Override
		public void process(ExperimentResult result) {
			ExperimentResult recorded = new ExperimentResult();
			recorded.load(decoder);
			recorded.experimentValid = true;
			Assert.assertEquals(result,recorded);
			//double newDiff = result.maxDiff(recorded);
		}

		@Override
		public void finished() {
			decoder.close();if (inputStream != null) { try { inputStream.close();inputStream=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
		}
		
	}
	
	protected static class SystemTestDiffExperiments extends DiffExperiments
	{
		public SystemTestDiffExperiments(int experimentsPerCategoryArg, int mutationStagesArg, int graphComplexityMaxArg)
		{
			super(experimentsPerCategoryArg,mutationStagesArg,graphComplexityMaxArg);
		}
		
		/** This one does not have to precisely replicate the experiment, its aim
		 * is to ensure that a similar experiment can be run and the outcome of such
		 * an experiment are unchanged whenever it is run. 
		 */
		public void TestExperiment(ResultProcessor processor)
		{
			int initStates=20;
			try
			{
				config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
				for(int graphComplexity=0;graphComplexity < graphComplexityMax;graphComplexity++)
				{
					int states=initStates+graphComplexity*50;
					int alphabet = states/2;
					
					MachineGenerator mg = new MachineGenerator(states, 40, states/10);
					int mutationsPerStage = (states/2) / 2;
					ProgressIndicator progress = new ProgressIndicator(""+states, mutationStages*experimentsPerMutationCategory);
					for(int mutationStage = 0;mutationStage<mutationStages;mutationStage++)
					{
						for(int experiment=0;experiment<experimentsPerMutationCategory;experiment++)
						{
							ExperimentResult outcome = new ExperimentResult();
							final double perfectLowToHigh=0.7,perfectThreshold=0.5;
		
							config.setAttenuationK(0.5);
							config.setGdKeyPairThreshold(perfectThreshold);
							config.setGdLowToHighRatio(perfectLowToHigh);
							config.setGdPropagateDet(false);// this is to ensure that if we removed a transition 0 from to a state and then added one from that state to a different one, det-propagation will not force the two very different states into a key-pair relation. 
		
							int mutations = mutationsPerStage * (mutationStage+1);
							LearnerGraphND origGraph = mg.nextMachine(alphabet, experiment,config,converter);
							GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData> mutator = new GraphMutator<List<CmpVertex>,LearnerGraphNDCachedData>(origGraph,new Random(mutationStage*experimentsPerMutationCategory+experiment));
							mutator.mutate(mutations);
							LearnerGraphND origAfterRenaming = new LearnerGraphND(origGraph.config);
							Map<CmpVertex,CmpVertex> origToNew = copyStatesAndTransitions(origGraph,origAfterRenaming);
							LearnerGraphND mutated = (LearnerGraphND)mutator.getMutated();
							mutated.setName(origAfterRenaming.getName()+"_mutated");origAfterRenaming.setName(origAfterRenaming.getName()+"_orig");
							Set<Transition> appliedMutations = new HashSet<Transition>();
							for(Transition tr:mutator.getDiff())
							{
								CmpVertex renamedFrom = origToNew.get(tr.getFrom());if (renamedFrom == null) renamedFrom = tr.getFrom();
								CmpVertex renamedTo = origToNew.get(tr.getTo());if (renamedTo == null) renamedTo = tr.getTo();
								appliedMutations.add(new Transition(renamedFrom,renamedTo,tr.getLabel()));
							}
							
							linearDiff(origAfterRenaming,mutated, appliedMutations,outcome);
							
							LearnerGraph fromDet = null, toDet = null;
							try {
								fromDet = mergeAndDeterminize(origAfterRenaming);
								toDet = mergeAndDeterminize(mutated);
							} catch (IncompatibleStatesException e) {
								Helper.throwUnchecked("failed to build a deterministic graph from a nondet one", e);
							}
							languageDiff(fromDet,toDet,states, graphComplexity,outcome);
							
							// zero the times.
							outcome.setValue(LONG_V.DURATION_GD, 0);
							outcome.setValue(LONG_V.DURATION_RAND, 0);
							outcome.setValue(LONG_V.DURATION_W, 0);
							
							outcome.experimentValid = true;
							processor.process(outcome);
							
							progress.next();
						}
					}
				}
			}
			finally
			{
				processor.finished();// ensure files are closed.
			}
		}
	}// class IntegrationTestDiffExperiments

}
