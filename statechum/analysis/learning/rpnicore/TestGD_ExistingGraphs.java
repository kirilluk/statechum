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

import java.io.File;
import java.io.FilenameFilter;
import java.util.Collection;
import java.util.LinkedList;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.GD.ChangesRecorder;

/**
 * @author kirill
 *
 */
@RunWith(Parameterized.class)
public class TestGD_ExistingGraphs {
	protected java.util.Map<CmpVertex,CmpVertex> newToOrig = null;

	/** Number of threads to use. */
	protected final int threadNumber;

	Configuration config = null;

	@Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		final String testFilePath = "resources/TestGraphs/75-6/";
		File path = new File(testFilePath);assert path.isDirectory();
		File files [] = path.listFiles(new FilenameFilter()
		{
			public boolean accept(@SuppressWarnings("unused") File dir, String name) 
			{
				return name.startsWith("N_");
			}});
		
		for(int fileNum = 0;fileNum < files.length;++fileNum)
			for(int threadNo=1;threadNo<8;++threadNo)
				for(double ratio:new double[]{0.5,0.68,0.9,-1})
					result.add(new Object[]{new Integer(threadNo), ratio,
							files[fileNum].getAbsolutePath(), files[(fileNum+1)%files.length].getAbsolutePath()});
		
		return result;
	}

	final String fileNameA,fileNameB;
	
	/** Positive value is the ratio of low-to-high above which key pairs are considered ok;
	 * negative value means that we set <em>setGdMaxNumberOfStatesInCrossProduct</em>
	 * to a low value and check that the outcome works.
	 */
	double low_to_high_ratio = -1;
	
	/** Creates the test class with the number of threads to create as an argument. */
	public TestGD_ExistingGraphs(int th, double ratio, String fileA, String fileB)
	{
		threadNumber = th;fileNameA=fileA;fileNameB=fileB;low_to_high_ratio=ratio;
	}
	
	public static String parametersToString(Integer th, Double ratio, String fileA, String fileB)
	{
		return "threshold: "+th+" ratio: "+ratio+", "+fileA+" v.s. "+fileB;
	}
	
	@Before
	public final void beforeTest()
	{
		newToOrig = new java.util.TreeMap<CmpVertex,CmpVertex>();
		config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		config.setGdKeyPairThreshold(.75);
		if (low_to_high_ratio > 0)
			config.setGdLowToHighRatio(low_to_high_ratio);
		else
		{
			config.setGdLowToHighRatio(0.5);
			config.setGdMaxNumberOfStatesInCrossProduct(10);
		}
		config.setAttenuationK(0.95);
	}
	
	protected String testDetails()
	{
		return fileNameA+"-"+fileNameB+" ["+threadNumber+" threads] ";
	}
	
	public final void runPatch(String fileA, String fileB)
	{
		LearnerGraph loadedA = LearnerGraph.loadGraph(fileA, config), loadedB = LearnerGraph.loadGraph(fileB, config); 
		LearnerGraph grA = loadedA;//Transform.convertToNumerical(loadedA);Assert.assertEquals(testDetails(),loadedA.getStateNumber(),grA.getStateNumber());
		LearnerGraph grB = loadedB;//Transform.convertToNumerical(loadedB);Assert.assertEquals(testDetails(),loadedB.getStateNumber(),grB.getStateNumber());
		GD gd = new GD();
		LearnerGraph loadedExpected = LearnerGraph.loadGraph(fileA, config);
		LearnerGraph graph = loadedExpected;//Assert.assertEquals(testDetails(),loadedExpected.getStateNumber(),graph.getStateNumber());
		ChangesRecorder patcher = new ChangesRecorder(null);
		//Map<CmpVertex,CmpVertex> testValueOfNewToOrig = new TreeMap<CmpVertex,CmpVertex>();
		gd.computeGD(grA, grB, threadNumber, patcher);
/*		gd.init(grA, grB, threadNumber);
		gd.identifyKeyPairs();
		List<PairScore> allKeyPairs = new LinkedList<PairScore>();
		gd.makeSteps(patcher,allKeyPairs);*/
		ChangesRecorder.applyGD(graph, patcher.writeGD(TestGD.createDoc()));//gd.computeGDToXML(grA, grB, threadNumber, TestGD.createDoc()));
		WMethod.checkM(graph, grB);
		Assert.assertEquals(testDetails(),grB.getStateNumber(),graph.getStateNumber());
	}
	
	@Test
	public final void testGD_AB()
	{
		runPatch(fileNameA, fileNameB);
	}
	
	@Test
	public final void testGD_BA()
	{
		runPatch(fileNameB, fileNameA);
	}
	
	@Test
	public final void testGD_AA()
	{
		LearnerGraph grA = Transform.convertToNumerical(LearnerGraph.loadGraph(fileNameA, config));
		LearnerGraph grB = Transform.convertToNumerical(LearnerGraph.loadGraph(fileNameA, config));
		LearnerGraph graph = Transform.convertToNumerical(LearnerGraph.loadGraph(fileNameA, config));
		GD gd = new GD();
		ChangesRecorder.applyGD(graph, gd.computeGDToXML(grA, grB, threadNumber, TestGD.createDoc(),null));
		WMethod.checkM(graph, grB);Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
}
