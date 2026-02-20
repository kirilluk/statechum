package statechum.analysis.learning.experiments.com6911;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;

import java.io.File;
import java.io.IOException;

import static statechum.analysis.learning.experiments.com6911.GenerateAutomata.graphmlExt;

public class PlotLearningResult {

    public static void main(String [] args) throws IOException {
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.CLOSE_TERMINATE, "true");
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.ESC_TERMINATE,"true");

        if (args.length < 3)
            throw new IllegalArgumentException("Usage: <REFERENCE DIRECTORY> <LEARNT DIRECTORY> <learnt automaton name>");
        if (!new File(args[0]).isDirectory())
            throw new IllegalArgumentException("Path " + args[0] + " is not a directory");
        if (!new File(args[1]).isDirectory())
            throw new IllegalArgumentException("Path " + args[1] + " is not a directory");
        String name = args[2];
        if (!name.startsWith(RunLearner.learntPrefix) || name.indexOf(GenerateRandomWalks.counterSuffix) < 0)
            throw new IllegalArgumentException("Invalid name "+name);
        File referenceAutomatonFileName = new File(args[0]+File.separator+name.substring(RunLearner.learntPrefix.length(),name.indexOf(GenerateRandomWalks.counterSuffix))+graphmlExt);
        if (!referenceAutomatonFileName.canRead())
            throw new IllegalArgumentException("Can't read reference automaton from "+referenceAutomatonFileName.getName());
        Configuration config = Configuration.getDefaultConfiguration().copy();
        LearnerGraph learntGraph = new LearnerGraph(config);
        AbstractPersistence.loadGraph(new File(args[1]+File.separator+name), learntGraph,null);learntGraph.setName(name);
        LearnerGraph referenceGraph = new LearnerGraph(config);
        AbstractPersistence.loadGraph(referenceAutomatonFileName, referenceGraph,null);referenceGraph.setName(referenceAutomatonFileName.getName());
        try
        {
            Visualiser.updateFrame(referenceGraph, learntGraph);
            Visualiser.waitForKey();
        }
        catch(Exception ex)
        {
            ex.printStackTrace();
        }
        finally {
            DrawGraphs.end();
        }
    }
}
