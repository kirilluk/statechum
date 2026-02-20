package statechum.analysis.learning.experiments.com6911;

import statechum.Configuration;
import statechum.Label;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Random;

import static statechum.analysis.learning.experiments.com6911.GenerateAutomata.graphmlExt;
import static statechum.analysis.learning.experiments.com6911.GenerateRandomWalks.walksExt;

public class GenerateRandomWalks {
    protected static String sequenceToString(String prefix,List<Label> seq) {
        StringBuffer outcome = new StringBuffer(prefix);
        for(Label lbl:seq) {
            if (outcome.length() > prefix.length())
                outcome.append(',');
            outcome.append(lbl.toString());
        }
        outcome.append('\n');
        return outcome.toString();
    }

    public final static char counterSuffix='~';
    public final static String walksExt = ".txt";

    public static void main(String[] args) throws IOException {
        if (args.length < 6)
            throw new IllegalArgumentException("Usage: <SOURCE DIRECTORY> <TARGET DIRECTORY> <seq_mult> <length_mult> <pos_only> <number of walks per automaton>");
        if (!new File(args[0]).isDirectory())
            throw new IllegalArgumentException("Path "+args[0]+" is not a directory");
        if (!new File(args[1]).isDirectory())
            throw new IllegalArgumentException("Path "+args[1]+" is not a directory");

        int seq_multiplier = Integer.parseInt(args[2]);
        int len_multiplier = Integer.parseInt(args[3]);
        boolean pos_only = Boolean.parseBoolean(args[4]);
        int walks_per_automaton = Integer.parseInt(args[5]);

        Random rnd = new Random(0);

        for(File file:new File(args[0]).listFiles((dir, name) -> name.endsWith(graphmlExt)))
        {
            LearnerGraph referenceGraph = new LearnerGraph(Configuration.getDefaultConfiguration().copy());
            AbstractPersistence.loadGraph(file, referenceGraph,null);referenceGraph.setName(file.getName());
            for(int counter=0;counter < walks_per_automaton;++counter) {
                RandomPathGenerator generator = new RandomPathGenerator(referenceGraph, new Random(counter), referenceGraph.getStateNumber()/2, null);
                final int tracesToGenerate = LearningSupportRoutines.makeEven(referenceGraph.getStateNumber() * seq_multiplier);
                generator.generateRandomPosNeg(tracesToGenerate, 1, false, new RandomPathGenerator.RandomLengthGenerator() {

                    @Override
                    public int getLength() {
                        // return len_multiplier * referenceGraph.getStateNumber();// ok for evaluation, bad for learning.
                        return rnd.nextInt(len_multiplier * referenceGraph.getStateNumber())+1;// good for learning because it generates a range of negative traces of different length.
                    }

                    @Override
                    public int getPrefixLength(int len) {
                        return len;
                    }
                }, true, !pos_only, null, null);
                FileWriter writer = new FileWriter(args[1] + File.separator+file.getName().substring(0,file.getName().length()-graphmlExt.length()) + counterSuffix+counter+walksExt);

                Collection<List<Label>> positive_sequences = generator.getAllSequences(0).getData(name -> ((RandomPathGenerator.StateName) name).accept);
                for (List<Label> seq : positive_sequences)
                    writer.append(sequenceToString("+ ",seq));

                Collection<List<Label>> negative_sequences = generator.getAllSequences(0).getData(name -> !((RandomPathGenerator.StateName) name).accept);

                for (List<Label> seq : negative_sequences)
                    writer.append(sequenceToString("- ",seq));
                writer.close();
            }
        }
    }
}
