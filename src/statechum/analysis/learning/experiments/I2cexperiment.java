/* Copyright (c) 2013 The University of Sheffield.
 * 
 * This file is part of StateChum.
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */

package statechum.analysis.learning.experiments;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.TreeMap;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.utils.UserData;
import statechum.Configuration.STATETREE;
import statechum.Configuration.ScoreMode;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.JUConstants;
import statechum.Label;
import statechum.StringLabel;
import statechum.analysis.learning.DrawGraphs;
import statechum.analysis.learning.MarkovClassifier;
import statechum.analysis.learning.MarkovClassifier.ConsistencyChecker;
import statechum.analysis.learning.MarkovClassifierLG;
import statechum.analysis.learning.MarkovModel;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovParameters;
import statechum.analysis.learning.experiments.MarkovEDSM.MarkovExperiment.EDSM_MarkovLearner;
import statechum.analysis.learning.experiments.PairSelection.LearningAlgorithms;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;


public class I2cexperiment extends PairQualityLearner
{
	
	public static void visualiseGraph(LearnerGraph actualAutomaton)
	{
		Map<String,String> labelling = new TreeMap<String,String>();
		for(Entry<CmpVertex,Map<Label,CmpVertex>> entry:actualAutomaton.transitionMatrix.entrySet())
			labelling.put(entry.getKey().toString(),Visualiser.extralabelToReplaceExisting+entry.getKey().getStringId());
		DirectedSparseGraph gr = actualAutomaton.pathroutines.getGraph();
		
		gr.addUserDatum(JUConstants.VERTEX, labelling, UserData.SHARED);
		Visualiser.updateFrame(gr,null);	
		Visualiser.waitForKey();		
	}
		
	public static Collection<StatePair> constructPairsToMergeBasedOnSetsToMerge(Set<CmpVertex> validStates, Collection<Set<CmpVertex>> verticesToMergeBasedOnInitialPTA)
	{
		List<StatePair> pairsList = new LinkedList<StatePair>();
		for(Set<CmpVertex> groupOfStates:verticesToMergeBasedOnInitialPTA)
		{
			Set<CmpVertex> validStatesInGroup = new TreeSet<CmpVertex>();validStatesInGroup.addAll(groupOfStates);validStatesInGroup.retainAll(validStates);
			if (validStatesInGroup.size() > 1)
			{
				CmpVertex v0=validStatesInGroup.iterator().next();
				for(CmpVertex v:validStatesInGroup)
				{
					if (v != v0)
						pairsList.add(new StatePair(v0,v));
					v0=v;
				}
			}
		}
		return pairsList;
	}

	public static void main(String args[]) throws Exception
	{
		try
		{
			runExperiment(args);
		}
		catch(Exception ex)
		{
			ex.printStackTrace();
		}
		finally
		{
			DrawGraphs.end();
		}
	}
	
	/** Alphabet that is expected in a trace to load. */
	protected static String alphabet[]=new String[]{
			"=S","@","=R","=TA","[*9]","-Ta","~SnA","~SnB","~R","#S","#W","#Ta","#s","#RA","[+1]","^Ra","~r",
			"~SwA","~swA","^E","#L","~Xr","?"					
	};
	public static String alphabetElementToConsiderErr = "?";
	protected static String errElement = "Err";
	public static List<Label> loadTrace(String inputFileName,ConvertALabel converter, String valueForErrElement)
	{
		List<Label> returnValue = new LinkedList<Label>();
		
        BufferedReader in = null;
        try {
           in = new BufferedReader(new FileReader(inputFileName));
            String fileString;
            while ((fileString = in.readLine()) != null) 
            {
            	String lineOfLog= fileString.trim();
            	while(lineOfLog.length() > 0)
            	{
	            	String aFound = null;
	            	for(String st:alphabet)
		            	if (lineOfLog.startsWith(st))
		            	{
		            		if (aFound != null)
		            			throw new IllegalArgumentException("both "+aFound+" and "+st+" match trace line "+lineOfLog);
		            		aFound = st;
		            	}
	            	if (aFound == null)
	            		throw new IllegalArgumentException("failed to match the beginning of line "+lineOfLog);
	            	lineOfLog = lineOfLog.substring(aFound.length());
	            	if (aFound.equals(alphabetElementToConsiderErr))
	            		aFound =valueForErrElement;
	            	returnValue.add(converter.convertLabelToLabel(new StringLabel(aFound)));
            	}
            }
         } catch (IOException e) {
            statechum.Helper.throwUnchecked("failed to read learner initial data", e);
        } finally {
            if (in != null) { try { in.close();in=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
        }
        System.out.println("trace length: "+returnValue.size());
        return returnValue;
	}
	/*
	public static void runExperimentCompare(@SuppressWarnings("unused") String args[]) throws Exception
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setAskQuestions(false);config.setDebugMode(false);config.setGdLowToHighRatio(0.7);config.setRandomPathAttemptFudgeThreshold(1000);
		config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);config.setLearnerScoreMode(ScoreMode.ONLYOVERRIDE);
		ConvertALabel converter = new Transform.InternStringLabel();
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.LINEARWARNINGS, "false");
		final int chunkSize = 7;
	
		// Inference from a few traces
		final boolean onlyPositives=true;
		LearnerGraph graphRef = new LearnerGraph(config);AbstractPersistence.loadGraph("resources/i2c_study/outcome_i2c_chunk7.xml",graphRef,converter);
		LearnerGraph graph = new LearnerGraph(config);AbstractPersistence.loadGraph("outcome_i2c.xml",graph,converter);
		
		DifferentFSMException diff = WMethod.checkM(graphRef, graph);
		if (diff != null)
			throw diff;
		System.out.println("same graph");
	}
*/
	public static void runExperiment(@SuppressWarnings("unused") String args[]) throws Exception
	{
		LearnerEvaluationConfiguration eval = UASExperiment.constructLearnerInitConfiguration();
		eval.config.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);eval.config.setLearnerScoreMode(ScoreMode.ONLYOVERRIDE);
		eval.config.setOverride_maximalNumberOfStates(50*LearningAlgorithms.maxStateNumberMultiplier);
		eval.config.setOverride_usePTAMerging(false);

		final int chunkSize = 7;

		LearnerGraph initialPTA = new LearnerGraph(eval.config);
		initialPTA.paths.augmentPTA(loadTrace("resources/i2c_study/log10.txt",eval.getLabelConverter(),errElement), true, false,null);
		// The purpose of if-then below is to make it clear that an error transition will not be repeated - this was the only problem in the inferred model. 
		LearnerGraph [] ifthenAutomata = Transform.buildIfThenAutomata(Arrays.asList(new String[]{"ifthenFSM graph1 A-!"+errElement+"->A-"+errElement+"->B-"+errElement+"->B-!"+errElement+"->A / P-"+errElement+"-#Q / P == THEN == B"}), initialPTA.pathroutines.computeAlphabet(), eval.config, eval.getLabelConverter()).toArray(new LearnerGraph[0]);
		Transform.augmentFromIfThenAutomaton(initialPTA, null, ifthenAutomata, 1);// we only need  to augment our PTA once.
		final MarkovModel m= new MarkovModel(chunkSize,true,true,true,false);
		new MarkovClassifierLG(m, initialPTA,null).updateMarkov(false);// construct Markov chain if asked for.
		initialPTA.clearColours();
		final ConsistencyChecker checker = new MarkovClassifier.DifferentPredictionsInconsistencyNoBlacklistingIncludeMissingPrefixes();
		MarkovParameters markovParameters = new MarkovParameters(0, chunkSize,true,1, true,1,0,1);
		EDSM_MarkovLearner markovLearner = new EDSM_MarkovLearner(eval,initialPTA,0,markovParameters, null);markovLearner.setMarkov(m);markovLearner.setChecker(checker);

		System.out.println("started: "+new Date());
		LearnerGraph graph = markovLearner.learnMachine(new LinkedList<List<Label>>(),new LinkedList<List<Label>>());
		System.out.println("finished: "+new Date());
		graph.storage.writeGraphML("outcome_i2c.xml");
	}
}