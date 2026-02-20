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

public class PlotAutomaton {

    public static void main(String [] args) throws IOException {
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.CLOSE_TERMINATE, "true");
        GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.ESC_TERMINATE,"true");
        if (args.length < 2)
            throw new IllegalArgumentException("Usage: <DIRECTORY> <automaton name>");
        if (!new File(args[0]).isDirectory())
            throw new IllegalArgumentException("Path " + args[0] + " is not a directory");
        Configuration config = Configuration.getDefaultConfiguration().copy();
        LearnerGraph graph = new LearnerGraph(config);
        AbstractPersistence.loadGraph(new File(args[0]+File.separator+args[1]), graph,null);graph.setName(args[1]);
        try
        {
            Visualiser.updateFrame(graph,null);
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
