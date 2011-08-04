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

public class GenerateRandomFSM extends DiffExperiments {

	public GenerateRandomFSM()
	{
		super(30);
	}
	
	public static void main(String args[])
	{
		try
		{
		Configuration config = Configuration.getDefaultConfiguration().copy();
		int initStates = 25, graphComplexityMax=6, experimentsPerCategory = 100;
		
		for(int graphComplexity=0;graphComplexity < graphComplexityMax;graphComplexity++)
		{
			int states=initStates+graphComplexity*20;
			int alphabet = states/2;
			MachineGenerator mg = new MachineGenerator(states, 40, states/10);
			ProgressIndicator progress = new ProgressIndicator(""+states, experimentsPerCategory);
			
			for(int experiment=0;experiment<experimentsPerCategory;experiment++)
			{
				LearnerGraphND generatedFSM = mg.nextMachine(alphabet, experiment,config);
				if (generatedFSM.getInit() != generatedFSM.transitionMatrix.entrySet().iterator().next().getKey())
					throw new RuntimeException("first state is not the initial state");
				
				//Visualiser.updateFrame(origGraph, null);
				Writer randomFSM = new FileWriter("resources/randomFSM/fsm_"+states+"_"+experiment+".x_");
				for(Entry<CmpVertex,Map<Label,List<CmpVertex>>> entry:generatedFSM.transitionMatrix.entrySet())
					for(Entry<Label,List<CmpVertex>> targets:entry.getValue().entrySet())
						for(CmpVertex targetState:generatedFSM.getTargets(targets.getValue()))
						{
							randomFSM.write(entry.getKey().getID().getStringId());randomFSM.write(' ');
							randomFSM.write(targetState.getID().getStringId());randomFSM.write(' ');
							randomFSM.write(targets.getKey().toString());randomFSM.write('\n');
						}
				randomFSM.close();
				
				progress.next();
			}
		}
		}
		catch(Exception ex)
		{
			ex.printStackTrace();
		}
	}
}
