package statechum.apps;

import java.util.Collection;
import java.util.List;
import java.util.Random;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.collections.MapWithSearch;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;

public class AutomataGenerator {

	public static void main(String[] args) throws IncompatibleStatesException {
		int states=6, alphabet=(states+1)/2;
		for(int randomGraphSeed=0;randomGraphSeed < 5;++randomGraphSeed) {
			Configuration config = Configuration.getDefaultConfiguration().copy();
			final LearnerEvaluationConfiguration evaluationConfiguration = new LearnerEvaluationConfiguration(null, null, config, null, null);
			MachineGenerator mg = new MachineGenerator(states, 400, (int) Math.round((double) states / 5));
			mg.setGenerateConnected(true);
			LearnerGraph referenceGraph = mg.nextMachine(alphabet, -1.0, randomGraphSeed, evaluationConfiguration.config, evaluationConfiguration.getLabelConverter()).pathroutines.buildDeterministicGraph();
			for (Entry<CmpVertex, MapWithSearch<Label, Label, CmpVertex>> entry : referenceGraph.transitionMatrix.entrySet())
				for (Entry<Label, CmpVertex> transition : entry.getValue().entrySet())
					System.out.println(entry.getKey() + " - " + transition.getKey() + " -> " + transition.getValue());
			referenceGraph.setName("Random_graph_"+randomGraphSeed);
			for (int randomPathSeed = 0; randomPathSeed < 5; ++randomPathSeed) {
				RandomPathGenerator generator = new RandomPathGenerator(referenceGraph, new Random(randomPathSeed), 0, null);
				generator.generateRandomPosNeg(states + (states % 2), 1);
				Collection<List<Label>> positive_sequences = generator.getAllSequences(0).getData(name -> ((RandomPathGenerator.StateName) name).accept);

				System.out.println("Positive sequences");
				for (List<Label> seq : positive_sequences)
					System.out.println(seq);
				Collection<List<Label>> negative_sequences = generator.getAllSequences(0).getData(name -> !((RandomPathGenerator.StateName) name).accept);//PTASequenceEngine.truePred);

				System.out.println("Negative sequences");
				for (List<Label> seq : negative_sequences)
					System.out.println(seq);
			}
			Visualiser.updateFrame(referenceGraph, null);
		}
		Visualiser.waitForKey();
	}

}
