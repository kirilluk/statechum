/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
 * 
 * Statechum is free software: you can redistribute it and/or modify
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

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraphND;

import java.util.Collection;
import java.util.List;
import java.util.Random;
import java.util.Set;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;
import statechum.analysis.learning.PrecisionRecall.PosNegPrecisionRecall;
import statechum.analysis.learning.rpnicore.GD;
import statechum.analysis.learning.rpnicore.GDLearnerGraph;
import statechum.analysis.learning.rpnicore.LSolver;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.analysis.learning.rpnicore.RandomPathGenerator;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.GD.ChangesCounter;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTA_FSMStructure;
import statechum.model.testset.PTA_computePrecisionRecall;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

/**
 * @author kirill
 *
 */
public class StructuralDifferencesPaper {
	static final Configuration config = Configuration.getDefaultConfiguration();

	public static void dumpEquations()
	{
		GDLearnerGraph ndGraph = new GDLearnerGraph(buildLearnerGraph("A-a->B-a->B-b->A / B-c->C / E-a->F-a->F-d->F-b->E-c->F",	"dumpEquations",config),LearnerGraphND.ignoreRejectStates, false);
		final int [] incompatiblePairs = new int[ndGraph.getPairNumber()];
		final int pairsNumber = incompatiblePairs.length;
		for(int i=0;i<incompatiblePairs.length;++i) incompatiblePairs[i]=i;// emulate the behaviour or non-public part for(int i=0;i<incompatiblePairs.length;++i) incompatiblePairs[i]=PAIR_OK;final int pairsNumber = ndGraph.findIncompatiblePairs(incompatiblePairs,1);

		LSolver solver = ndGraph.buildMatrix_internal(incompatiblePairs, pairsNumber, 1,null);

		System.out.println(ndGraph.dumpEquations(solver, incompatiblePairs, null).toString());	}
	
	/**
	 * @param args
	 * @throws IncompatibleStatesException 
	 */
	public static void main(String[] args) throws IncompatibleStatesException 
	{
		dumpEquations();
		
		String markovString = "a-initialise->d-connect->f-login->h-storefile->m\nh-listfiles->l-retrievefile->l-changedir->p-listfiles->l-logout->o-disconnect->q\nh-changedir->n-listnames->i-delete->i-delete->j-changedir->n\ni-appendfile->k-logout->o\nPART2-setfiletype->e-rename->g\nPART3-makedir->PART3";
		LearnerGraph cvsGraph = buildLearnerGraph(
				"q0 -initialise->q1 /" +
				"q1-connect->q2 / " +
				"q3-setfiletype->q4 / " +
				"q3 - makedir -> q8 /" +
				"q4-rename->q6 /" +
				"q8-makedir->q8 /" +
				"q10 - changedir -> q9 /" +
				"q14 - listfiles -> q13 / " +
				"q11-logout->q16 / " +
				"q13-changedir->q14 /" +
				"q9-listnames->q10 /" +
				"q6-storefile->q5 /" +
				"q3-listfiles->q13 /"+
				"q7-appendfile->q5 /" +
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
				"q3-changedir->q9 /" +
				"q10-appendfile->q11 /" +
				"q13-retrievefile->q13","sd_cvs",config),
				markovD = buildLearnerGraphND(markovString.replace("PART2", "a").replace("PART3", "a"),"sd_markovD",config).pathroutines.buildDeterministicGraph(),
				edsm = buildLearnerGraph("a-initialise->b-connect->c-login->d-storefile->e-changedir->d-listfiles->e-retrievefile->e-logout->i-disconnect->l\nd-delete->d-makedir->d-changedir->f-listnames->d-logout->g-disconnect->j\nd-setfiletype->h-storefile->k-appendfile->m-setfiletype->n-rename->o-storefile->m\nd-appendfile->o-logout->p-disconnect->q","sd_edsm",config)
				;
		LearnerGraphND 
			markov = buildLearnerGraphND(markovString,"sd_markov",config);
		markovD.setName("sd_markovD");

		Set<Label> origAlphabet = cvsGraph.pathroutines.computeAlphabet();
		assert origAlphabet.equals(markov.pathroutines.computeAlphabet());
		assert origAlphabet.equals(markovD.pathroutines.computeAlphabet());
		assert origAlphabet.equals(edsm.pathroutines.computeAlphabet());
		final RandomPathGenerator generator = new RandomPathGenerator(cvsGraph,new Random(0),5,null);
		final int posOrNegPerChunk = 50;
		generator.generateRandomPosNeg(posOrNegPerChunk*2,1);
		Collection<List<Label>> sequences = cvsGraph.wmethod.getFullTestSet(1);//generator.getAllSequences(0).getData(PTASequenceEngine.truePred);

		PTASequenceEngine walkEngine = new PTA_FSMStructure(cvsGraph,null);
		SequenceSet ptaWalk = walkEngine.new SequenceSet();ptaWalk.setIdentity();
		ptaWalk = ptaWalk.cross(sequences);
		
		
		
		PTA_computePrecisionRecall precRec = null;
		
		{
			precRec = new PTA_computePrecisionRecall(markovD);
			precRec.crossWith(walkEngine);PosNegPrecisionRecall result = precRec.getPosNegPrecisionRecallNum(); 
			final String name = "Markov";
			System.out.println(name+": +precision "+result.getPosprecision()+" +recall: "+result.getPosrecall());
			System.out.println(name+": -precision "+result.getNegprecision()+" -recall: "+result.getNegrecall());
			System.out.println(name+": =precision "+result.getPrecision()+" =recall: "+result.getRecall());
		}

		{
			precRec = new PTA_computePrecisionRecall(edsm);
			precRec.crossWith(walkEngine);PosNegPrecisionRecall result = precRec.getPosNegPrecisionRecallNum(); 
			final String name = "EDSM";
			System.out.println(name+": +precision "+result.getPosprecision()+" +recall: "+result.getPosrecall());
			System.out.println(name+": -precision "+result.getNegprecision()+" -recall: "+result.getNegrecall());
			System.out.println(name+": =precision "+result.getPrecision()+" =recall: "+result.getRecall());
		}
		
		linearDiff(cvsGraph, markov);
		linearDiff(cvsGraph, new LearnerGraphND(edsm,config));
	}

	static void linearDiff(LearnerGraph cvsGraphD, LearnerGraphND otherGraph)
	{
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		LearnerGraphND cvsGraph = new LearnerGraphND(cvsGraphD,config);
		ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>  rec3 = new ChangesCounter<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>(cvsGraph,otherGraph,null);
		gd.computeGD(cvsGraph, otherGraph, 1, rec3, config);
		int intersection = cvsGraph.pathroutines.countEdges()-rec3.getRemoved();
		assert otherGraph.pathroutines.countEdges() == intersection+rec3.getAdded();
		System.out.println("PLTSDIFF, "+otherGraph.getName()+": precision = "+((double)intersection/otherGraph.pathroutines.countEdges())+" recall = "+((double)intersection/cvsGraph.pathroutines.countEdges()));	
	}
}
