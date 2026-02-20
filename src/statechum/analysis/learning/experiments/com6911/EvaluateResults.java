package statechum.analysis.learning.experiments.com6911;

import statechum.Configuration;
import statechum.Label;
import statechum.StringLabel;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;

import java.io.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static statechum.analysis.learning.experiments.com6911.GenerateAutomata.graphmlExt;
import static statechum.analysis.learning.experiments.com6911.GenerateRandomWalks.walksExt;

public class EvaluateResults {
    public static Collection<List<Label>> loadEvaluationData(File file) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(file));
        String line = null;
        Transform.ConvertALabel converter = new Transform.InternStringLabel();
        Collection<List<Label>> outcome = new ArrayList<>();
        while ((line = reader.readLine()) != null) {
            List<Label> trace = new ArrayList<>();
            for (String s : line.substring(2).split(","))
                trace.add(converter.convertLabelToLabel(new StringLabel(s)));
            outcome.add(trace);
        }

        return outcome;
    }

    public static void main(String[] args) throws IOException {
        if (args.length < 3)
            throw new IllegalArgumentException("Usage: <REFERENCE AUTOMATA DIRECTORY> <LEARNT AUTOMATA DIRECTORY> <EVALUATION DATA DIRECTORY> <output name>");
        if (!new File(args[0]).isDirectory())
            throw new IllegalArgumentException("Path "+args[0]+" is not a directory");
        if (!new File(args[1]).isDirectory())
            throw new IllegalArgumentException("Path "+args[1]+" is not a directory");
        if (!new File(args[2]).isDirectory())
            throw new IllegalArgumentException("Path "+args[2]+" is not a directory");
        FileWriter writer = new FileWriter(args[3]);
        writer.append("automaton,"+PairQualityLearner.DifferenceToReferenceLanguageBCR.toCSVHeading()+"\n");
        for (File file : new File(args[1]).listFiles((dir, name) -> name.endsWith(graphmlExt))) {
            String name = file.getName();
            if (!name.startsWith(RunLearner.learntPrefix) || name.indexOf(GenerateRandomWalks.counterSuffix) < 0)
                throw new IllegalArgumentException("Invalid name "+name);
            Configuration config = Configuration.getDefaultConfiguration().copy();
            LearnerGraph learntGraph = new LearnerGraph(config);
            AbstractPersistence.loadGraph(file, learntGraph,null);learntGraph.setName(name);

            File referenceAutomatonFileName = new File(args[0]+File.separator+name.substring(RunLearner.learntPrefix.length(),name.indexOf(GenerateRandomWalks.counterSuffix))+graphmlExt);
            if (!referenceAutomatonFileName.canRead())
                throw new IllegalArgumentException("Can't read reference automaton from "+referenceAutomatonFileName.getName());
            LearnerGraph referenceGraph = new LearnerGraph(config);
            AbstractPersistence.loadGraph(referenceAutomatonFileName, referenceGraph,null);referenceGraph.setName(referenceAutomatonFileName.getName());
            String automatonName = name.substring(0,name.length()-graphmlExt.length());
            File evaluationDataFile = new File(args[2]+File.separator+automatonName.substring(RunLearner.learntPrefix.length())+walksExt);
            if (!evaluationDataFile.canRead())
                throw new IllegalArgumentException("Can't read evaluation data from "+evaluationDataFile);
            Collection<List<Label>> evaluationData = loadEvaluationData(evaluationDataFile);
            PairQualityLearner.DifferenceToReferenceLanguageBCR differenceBCRlearnt= PairQualityLearner.DifferenceToReferenceLanguageBCR.estimationOfDifference(referenceGraph, learntGraph,evaluationData);

            writer.append(automatonName+","+differenceBCRlearnt.toCSV()+"\n");
        }
        writer.close();
    }
}
