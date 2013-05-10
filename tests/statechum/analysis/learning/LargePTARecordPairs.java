/* Copyright (c) 2012 The University of Sheffield, UK.
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
package statechum.analysis.learning;

import java.io.IOException;
import java.util.Date;
import java.util.List;

import org.junit.Test;

import statechum.Configuration;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner;
import statechum.analysis.learning.experiments.PaperUAS;
import statechum.analysis.learning.observers.ProgressDecorator;
import statechum.analysis.learning.rpnicore.Transform;

/** Records the choices of pairs made during the learning of a large PTA. */
public class LargePTARecordPairs
{
	@Test
	public void recordPairs() throws IOException
	{
		final Configuration.STATETREE trTypeFinal = Configuration.STATETREE.STATETREE_LINKEDHASH;
		Transform.InternStringLabel converter = new Transform.InternStringLabel();
		PaperUAS paper = new PaperUAS();
		ProgressDecorator.InitialData initial = PairQualityLearner.loadInitialAndPopulateInitialConfiguration(paper, PairQualityLearner.largePTAFileName, converter);
		Configuration learnerConf = paper.learnerInitConfiguration.config.copy();learnerConf.setTransitionMatrixImplType(trTypeFinal);
		
		for(boolean merger:new Boolean[]{true,false})
		{
			List<PairOfPaths> listOfPairs = new java.util.ArrayList<PairOfPaths>(1000);
	        long tmStarted = new Date().getTime();
	        paper.new RPNIBlueFringeTestVariability(learnerConf,merger,listOfPairs,null).learn(initial.graph);
	        long tmFinished = new Date().getTime();
	        System.out.println("Learning ("+merger+"), "+merger+" completed in "+((tmFinished-tmStarted)/1000)+" sec");tmStarted = tmFinished;
	        java.io.FileOutputStream pairsStream = new java.io.FileOutputStream(PairQualityLearner.largePTALogsDir+TestLearnerFromLargePTA.mergerTypeToXml(merger));
	        PairOfPaths.writePairs(listOfPairs, learnerConf, pairsStream);
	        pairsStream.close();
		}
		
	}
}