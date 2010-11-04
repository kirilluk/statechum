/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
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
package statechum.analysis.learning.rpnicore;

import static statechum.analysis.learning.rpnicore.FsmParser.buildGraph;

import java.util.List;
import java.util.Random;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.PrecisionRecall.PosNegPrecisionRecall;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.GD.ChangesCounter;
import statechum.model.testset.PTA_computePrecisionRecall;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;

/**
 * @author kirill
 *
 */
public class StructuralDifferencePaper {
	static final Configuration config = Configuration.getDefaultConfiguration();
	/*
	public PosNegPrecisionRecall computePrecisionRecall(LearnerGraph graph, LearnerGraph learnt)
	{
		PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(learnt);
		//PTASequenceEngine engine = new PTA_FSMStructure(graph);
		//PosNegPrecisionRecall ptaPR = precRec.crossWith(sMinus);
		SequenceSet ptaTestSet = engine.new SequenceSet();ptaTestSet.setIdentity();
		ptaTestSet = ptaTestSet.crossWith(randomPositiveEngine);
		return precRec.crossWith(engine);
	}
	*/
	/**
	 * @param args
	 * @throws IncompatibleStatesException 
	 */
	public static void main(String[] args) throws IncompatibleStatesException {
		
		String markovString = "a-initialise->d-connect->f-login->h-storefile->m\nh-listfiles->l-retrievefile->l-changedirectory->p-listfiles->l-logout->o-disconnect->q\nh-changedirectory->n-listnames->i-delete->i-delete->j-changedirectory->n\ni-appendfile->k-logout->o\nPART2-setfiletype->e-rename->g\nPART3-makedir->PART3";
		LearnerGraph cvsGraph = new LearnerGraph(buildGraph(
				"q0 -initialise->q1 /" +
				"q1-connect->q2 / " +
				"q3-setfiletype->q4 / " +
				"q3 - makedir -> q8 /" +
				"q4-rename->q6 /" +
				"q8-makedir->q8 /" +
				"q10 - changedir -> q9 /" +
				"q14 - listfiles -> q13 / " +
				"q11-logout->q16 / " +
				"q13-changeddir->q14 /" +
				"q9-listnames->q10 /" +
				"q6-storefile->q5 /" +
				"q3-listfiles->q13 /"+
				"q7-appendfie->q5 /" +
				"q13-logout->q16 /" +
				"q10-delete->q10 /" +
				"q2-login->q3 /" +
				"q4-storefile->q7 /" +
				"q8-logout->q16 /" +
				"q6-logout->q16 /" +
				"q7-logout->q16 /" +
				"q3-storefile->q11 /" +
				"q5-setfiletype->q4 /" +
				"q16-disconnect->q17 /" +
				"q3-changeddir->q9 /" +
				"q10-appendfile->q11 /" +
				"q13-retrievefile->q13","cvs"),config),
				markovD = new LearnerGraphND(buildGraph(markovString.replace("PART2", "a").replace("PART3", "a"),"markovD"),config).pathroutines.buildDeterministicGraph(),
				edsm = new LearnerGraph(buildGraph("a-initialise->b-connect->c-login->d-storefile->e-changedirectory->d-listfiles->e-retrievefile->e-logout->i-disconnect->l\nd-delete->d-makedir->d-changedirectory->f-listnames->d-logout->g-disconnect->j\nd-setfiletype->h-storefile->k-appendfile->m-setfiletype->n-rename->o-storefile->m\nd-appendfile->o-logout->p-disconnect->q","edsm"),config)
				;
				//"A-initialise->B-connect->C-login->D-setfiletype->D-retrievefile->D-delete->D-storefile->D-makedir->D-listfiles->D-changedirectory->E-listnames->D\nE-listfiles->D-appendfile->F-setfiletype->H-rename->D-rename->F-logout->G-disconnect->I\nD-logout->G",	"correct"),config);
		LearnerGraphND 
			markov = new LearnerGraphND(buildGraph(markovString,"markov"),config);
		/*
		Visualiser.updateFrame(cvsGraph, null);
		Visualiser.updateFrame(edsm, null);
		Visualiser.updateFrame(markovD, null);
		Visualiser.updateFrame(markov, null);
		
		Visualiser.waitForKey();
		*/
		final RandomPathGenerator generator = new RandomPathGenerator(cvsGraph,new Random(0),5,null);
		final int posOrNegPerChunk = 45;
		generator.generateRandomPosNeg(posOrNegPerChunk*2,1);

		System.out.println(generator.getAllSequences(0).getData(new FilterPredicate()
		{
			public boolean shouldBeReturned(Object name) {
				return !generator.getAllSequences(0).getFSM().shouldBeReturned(name);
			}
		}));
		//System.out.println(generator.getExtraSequences(0).getData(PTASequenceEngine.truePred).size());
		
		/*
		List<List<String>> data = generator.getExtraSequences(0).getData();
		for(List<String> str:data)
			System.out.println(str);
		*/
		PTA_computePrecisionRecall precRec = null;
		
		{
			precRec = new PTA_computePrecisionRecall(markovD);
			precRec.crossWith(generator.getAllSequences(0));PosNegPrecisionRecall result = precRec.getPosNegPrecisionRecallNum(); 
			System.out.println("Markov: precision "+result.getPosprecision()+" recall: "+result.getPosrecall());
		}

		{
			precRec = new PTA_computePrecisionRecall(edsm);
			precRec.crossWith(generator.getAllSequences(0));PosNegPrecisionRecall result = precRec.getPosNegPrecisionRecallNum(); 
			System.out.println("EDSM: precision "+result.getPosprecision()+" recall: "+result.getPosrecall());
		}
		
		//Visualiser.updateFrame(edsm, null);
		//Visualiser.waitForKey();
		//linearDiff(cvsGraph, markov);
		//linearDiff(cvsGraph, new LearnerGraphND(edsm,config));
	}

	static void linearDiff(LearnerGraph cvsGraphD, LearnerGraphND otherGraph)
	{
		//GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData> gd = new GD<CmpVertex,CmpVertex,LearnerGraphCachedData,LearnerGraphCachedData>();
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		LearnerGraphND cvsGraph = new LearnerGraphND(cvsGraphD,config);
		ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>  rec3 = new ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>(cvsGraph,otherGraph,null);
		gd.computeGD(cvsGraph, otherGraph, 1, rec3, config);
		int intersection = cvsGraph.pathroutines.countEdges()-rec3.getRemoved();
		assert otherGraph.pathroutines.countEdges() == intersection+rec3.getAdded();
		System.out.println(otherGraph.getName()+": precision = "+((double)intersection/otherGraph.pathroutines.countEdges())+" recall = "+((double)intersection/cvsGraph.pathroutines.countEdges()));
		
	}
}
