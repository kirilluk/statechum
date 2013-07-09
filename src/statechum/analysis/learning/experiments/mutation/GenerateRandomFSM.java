package statechum.analysis.learning.experiments.mutation;

import java.io.FileWriter;
import java.io.Writer;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;
import statechum.ProgressIndicator;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

public class GenerateRandomFSM extends DiffExperiments {

	public GenerateRandomFSM()
	{
		super(30);
	}
	
	public static void main(String args[])
	{
		Writer randomFSM = null;
		try
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();
			ConvertALabel converter = null;
			int initStates = 25, graphComplexityMax=6, experimentsPerCategory = 100;
			for(int graphComplexity=0;graphComplexity < graphComplexityMax;graphComplexity++)
			{
				int states=initStates+graphComplexity*20;
				int alphabet = states/2;
				MachineGenerator mg = new MachineGenerator(states, 40, states/10);
				ProgressIndicator progress = new ProgressIndicator(""+states, experimentsPerCategory);
				
				for(int experiment=0;experiment<experimentsPerCategory;experiment++)
				{
					LearnerGraphND generatedFSM = mg.nextMachine(alphabet, experiment,config, converter);
					if (generatedFSM.getInit() != generatedFSM.transitionMatrix.entrySet().iterator().next().getKey())
						throw new RuntimeException("first state is not the initial state");
					
					//Visualiser.updateFrame(origGraph, null);
					randomFSM = new FileWriter("resources/randomFSM/fsm_"+states+"_"+experiment+".x_");
					for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:generatedFSM.transitionMatrix.entrySet())
						for(Entry<Label,List<CmpVertex>> targets:entry.getValue().entrySet())
							for(CmpVertex targetState:generatedFSM.getTargets(targets.getValue()))
							{
								randomFSM.write(entry.getKey().getStringId());randomFSM.write(' ');
								randomFSM.write(targetState.getStringId());randomFSM.write(' ');
								randomFSM.write(targets.getKey().toString());randomFSM.write('\n');
							}
					randomFSM.close();randomFSM = null;
					progress.next();
				}
			}
		}
		catch(Exception ex)
		{
			ex.printStackTrace();
		}
		finally
		{
			if (randomFSM != null) {
				try { randomFSM.close();randomFSM = null; } catch (java.io.IOException toBeIgnored) { /* Ignore exception */ }
			}
		}
	}
}
