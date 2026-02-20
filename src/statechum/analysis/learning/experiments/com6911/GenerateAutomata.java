package statechum.analysis.learning.experiments.com6911;

import statechum.Configuration;
import statechum.analysis.learning.experiments.mutation.DiffExperiments;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.util.OutputUtil;

import java.io.File;
import java.io.IOException;

import static statechum.analysis.learning.util.OutputUtil.dotGraph;

public class GenerateAutomata {
    public static final String graphmlExt = ".xml";

    public static void main(String[] args) throws AMEquivalenceClass.IncompatibleStatesException, IOException {
        if (args.length < 2)
            throw new IllegalArgumentException("Usage: <DIRECTORY> <MAX_STATES>");
        if (!new File(args[0]).isDirectory())
            throw new IllegalArgumentException("Path "+args[0]+" is not a directory");
        int maxStates = Integer.parseInt(args[1]);
        System.out.println("Max number of states: "+maxStates+" , output to "+args[0]);
        for(int states=5;states <= maxStates;states*=2)
            // counter is used to seed the random number generator in nextMachine
            for(int counter = 0;counter < 10;counter++)
            {
                int graphDensity = 2, alphabet = states / 2;
                Configuration config = Configuration.getDefaultConfiguration().copy();
                final ProgressDecorator.LearnerEvaluationConfiguration evaluationConfiguration = new ProgressDecorator.LearnerEvaluationConfiguration(null, null, config, null, null);
                DiffExperiments.MachineGenerator mg = new DiffExperiments.MachineGenerator(states, 400, (int) Math.round((double) states / 5));
                mg.setGenerateConnected(true);
                LearnerGraph referenceGraph = mg.nextMachine(alphabet, graphDensity, counter, evaluationConfiguration.config, evaluationConfiguration.getLabelConverter()).pathroutines.buildDeterministicGraph();
                String outputFileName = args[0] + File.separator + "automaton_" + states + "_" + graphDensity + "_" + alphabet + "_"+counter;
                referenceGraph.storage.writeGraphML(outputFileName+graphmlExt);
                OutputUtil.write(dotGraph(referenceGraph.pathroutines.getGraph()).toString(),new File(outputFileName+".dot"));
            }
    }
}
