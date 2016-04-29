package statechum.analysis.learning.experiments;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.TreeSet;

import statechum.Label;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.AugmentFromIfThenAutomatonException;

public class VisualisePaperUAS
{
	public static void main(String args[]) throws IOException, AugmentFromIfThenAutomatonException
	{
		String graphName = "tmp"+File.separator+PaperUAS.nameForExperimentRun+ File.separator+"uas-All";
		PaperUAS paper = new PaperUAS();
        String path = args[0];
        paper.loadReducedConfigurationFile(path+File.separator+args[1]);
		
	    LearnerGraph pta = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph(PaperUAS.fileName(graphName), pta, paper.learnerInitConfiguration.getLabelConverter());
    	LearnerGraph referenceGraphWithNeg = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPersistence.loadGraph("resources/largePTA/outcome_correct", referenceGraphWithNeg, paper.learnerInitConfiguration.getLabelConverter());
    	LearnerGraph referenceGraph = new LearnerGraph(paper.learnerInitConfiguration.config);AbstractPathRoutines.removeRejectStates(referenceGraphWithNeg,referenceGraph);
    	paper.learnerInitConfiguration.testSet = LearningAlgorithms.buildEvaluationSet(referenceGraph);
		    
    	Collection<String> filteredLTLsequences = new TreeSet<String>(paper.learnerInitConfiguration.ifthenSequences);
    	for(String str:paper.learnerInitConfiguration.ifthenSequences)
    	{
    		boolean elemFound = false;
    		for(Label elem:pta.pathroutines.computeAlphabet())
    			if (str.contains(elem.toString()))
    			{// a match here could be spurious because str contains other text, however for the purpose of UAS experiment, label names are sufficiently distinctive so that it is not a problem.
    				elemFound = true;break;
    			}
    		
    		if (!elemFound)
    			filteredLTLsequences.remove(str);
    	}
		LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(filteredLTLsequences, pta.pathroutines.computeAlphabet(), paper.learnerInitConfiguration.config, paper.learnerInitConfiguration.getLabelConverter()).toArray(new LearnerGraph[0]);
		Transform.augmentFromIfThenAutomaton(pta, null, ifthenAutomata, paper.learnerInitConfiguration.config.getHowManyStatesToAddFromIFTHEN());// we only need  to augment our PTA once.
		Label uniqueLabel = AbstractLearnerGraph.generateNewLabel("Waypoint_Selected", paper.learnerInitConfiguration.config,paper.learnerInitConfiguration.getLabelConverter());
		LearnerGraph smallPta = UASExperiment.mergePTA(pta,uniqueLabel,false);
		Visualiser.updateFrame(smallPta.transform.trimGraph(3, smallPta.getInit()), referenceGraph);
		Visualiser.waitForKey();
	}
}
