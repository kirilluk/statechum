/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 
/*
 * INCOMPLETE
 */

package statechum.analysis.learning.experiments;


import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.*;

import edu.uci.ics.jung.graph.impl.*;
import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.analysis.learning.RPNIBlueFringeLearnerOrig;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.model.testset.*;
import static statechum.model.testset.PTASequenceEngine.stringCollectionSize;
public class PathCompressionExperiment extends AbstractExperiment {

	
	/** This one is not static because it refers to the frame to display results. */
	public static abstract class RPNIEvaluator extends LearnerEvaluator
	{	
		PTASequenceEngine pta = null;
		
		public RPNIEvaluator(String inputFile, int per, int instance, AbstractExperiment exp)
		{
			super(inputFile, per,instance, exp);
			
		}

		/** This one may be overridden by subclass to customise the learner. */
		protected abstract void changeParameters(Configuration c);
				
		public String call()
		{
			//System.out.println(inputFileName+" (instance "+instanceID+"), learner "+this+", "+percent + "% started at "+Calendar.getInstance().getTime());
			OUTCOME currentOutcome = OUTCOME.FAILURE;
			String stdOutput = writeResult(currentOutcome,null);// record the failure result in case something fails later and we fail to update the file, such as if we are killed or run out of memory
			if (stdOutput != null) return stdOutput;
			
			buildSets();
			Pair<Integer,Integer> uncompressed_compressed = stringCollectionSize(pta);
			double uncompressed = uncompressed_compressed.firstElem;
			String stats = instanceID+","+pta.numberOfLeafNodes()+ ","+uncompressed;
			try
			{
				double compressed = uncompressed_compressed.secondElem;
				double compression = 100*(uncompressed - compressed)/compressed;
				//stats = stats + ","+compressed+","+compression;
				stats = compression+",";
				currentOutcome = OUTCOME.SUCCESS;
				if(this.percent == 10)
					System.out.println();
				System.out.print(stats);
			}
			catch(Throwable th)
			{
				StringWriter writer = new StringWriter();
				th.printStackTrace();
				th.printStackTrace(new PrintWriter(writer));
				stats = stats+"\nFAILED\nSTACK: "+writer.toString();
			}
			
			// now record the result
			stdOutput = writeResult(currentOutcome, stats);
			if (stdOutput != null) return stdOutput;
			return inputFileName+FS+percent+FS+currentOutcome;
		}

		protected void buildSets()
		{
			loadGraph();
			int size = graph.paths.getGraph().getEdges().size()*4;// FIXME: this one ignores parallel edges
	    	RandomPathGenerator rpg = new RandomPathGenerator(graph, new Random(100),3);// the seed for Random should be the same for each file
			rpg.generatePosNeg(size, experiment.getStageNumber());
			pta = rpg.getAllSequences(percent);
		}
	}
	
	public static Collection<List<String>> getPositiveStrings(DirectedSparseGraph graph, Collection<List<String>> samples){
		Iterator<List<String>> sampleIt = samples.iterator();
		HashSet<List<String>> positiveStrings = new HashSet<List<String>>();
		while(sampleIt.hasNext()){
			List<String> v = sampleIt.next();
			if(RPNIBlueFringeLearnerOrig.getVertex(graph, v) != null)
				positiveStrings.add(v);
		}
		return positiveStrings;
	}
	
	@Override
	public int getStageNumber()
	{
		return 10;
	}
	
	@Override
	public List<LearnerEvaluatorGenerator> getLearnerGenerators()
	{
		return Arrays.asList(new LearnerEvaluatorGenerator[] {
			new LearnerEvaluatorGenerator() {
				@Override
				LearnerEvaluator getLearnerEvaluator(String inputFile, int percent, int instanceID, AbstractExperiment exp) {
					return new RPNIEvaluator(inputFile,percent, instanceID, exp)
					{
						@Override
						protected void changeParameters(Configuration c) 
						{
							c.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);						
						}
	
						@Override
						public String toString()
						{
							return evaluatorName();
						}
					};
				}

				@Override
				String evaluatorName() {
					return "RPNI_POSITIVE_NEGATIVE";
				}
			}
			// at this point, one may add the above learners with different arguments or completely different learners such as the Angluin's one
		});
	}
	
}
