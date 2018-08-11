package statechum.analysis.learning.experiments.EvaluationOfLearners;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.Map.Entry;

import statechum.Helper;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;

public class ConstructRandomFSM 
{
	public LearnerGraph referenceGraph = null;
	public Label uniqueFromInitial = null;
	
	public void generateFSM(Random rnd, int alphabet, int states, double perStateMultiplier, int seed, boolean pickUniqueFromInitial, final LearnerEvaluationConfiguration learnerInitConfiguration)
	{
		
		MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
		
		do
		{
			try {
				referenceGraph = mg.nextMachine(alphabet,perStateMultiplier,seed, learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter()).pathroutines.buildDeterministicGraph();// reference graph has no reject-states, because we assume that undefined transitions lead to reject states.
			} catch (IncompatibleStatesException e) {
				Helper.throwUnchecked("failed to build FSM", e);
			}
			
			if (pickUniqueFromInitial)
			{
				Map<Label,CmpVertex> uniques = LearningSupportRoutines.uniqueFromState(referenceGraph);
				if(!uniques.isEmpty())
				{ 
					// some uniques are loops, hence eliminate them to match our case study
					for(Entry<Label,CmpVertex> entry:uniques.entrySet())
						if (referenceGraph.transitionMatrix.get(entry.getValue()).get(entry.getKey()) != entry.getValue())
						{
							referenceGraph.setInit(entry.getValue());uniqueFromInitial = entry.getKey();break;// found a unique of interest
						}
				}
				if (uniqueFromInitial == null)
				{// need to generate a unique transition that did not occur through randomness.
					Set<Label> existingAlphabet = referenceGraph.pathroutines.computeAlphabet();
					if (existingAlphabet.size() < alphabet)
					{// There is scope for generation of a new (unique) label. Given how an alphabet is constructed by ForestFireLabelledStateMachineGenerator, 
					 // it seems appropriate.  
						Label uniqueLabel = AbstractLearnerGraph.generateNewLabel("unique", learnerInitConfiguration.config, learnerInitConfiguration.getLabelConverter());
						assert(!existingAlphabet.contains(uniqueLabel));
						List<CmpVertex> possibleVertices = new ArrayList<CmpVertex>();
						for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:referenceGraph.transitionMatrix.entrySet())
							if (entry.getValue().size() < alphabet)
								possibleVertices.add(entry.getKey());
						assert(!possibleVertices.isEmpty());
						CmpVertex newInit = possibleVertices.get(rnd.nextInt(possibleVertices.size()));
						referenceGraph.setInit(newInit);uniqueFromInitial = uniqueLabel;
						int targetIdx = rnd.nextInt(referenceGraph.getStateNumber()-1);
						CmpVertex target = null;
						for(CmpVertex v:referenceGraph.transitionMatrix.keySet())
							if (v != newInit)
							{// target should not be the same as the source
								if (targetIdx-- <= 0)
								{
									target = v;
									break;
								}
							}
						// Adding a new unique transition from the initial state does not affect reachability of vertices or the connectivity.
						// In addition, given that all states were distinguishable the uniqueness of the label does not make any of them equivalent.  
						referenceGraph.addTransition(referenceGraph.transitionMatrix.get(newInit), uniqueLabel,target);
					}
				}
			}
		}
		while(pickUniqueFromInitial && uniqueFromInitial == null);
	}
}
