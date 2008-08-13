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
			result.add(new Object[]{new Integer(threadNo), files[fileNum].getAbsolutePath(), files[(fileNum+1)%files.length].getAbsolutePath()});
		
		return result;
	}

	final String fileNameA,fileNameB;
	
	/** Creates the test class with the number of threads to create as an argument. */
	public TestGD_ExistingGraphs(int th, String fileA, String fileB)
	{
		threadNumber = th;fileNameA=fileA;fileNameB=fileB;
	}
	
	@Before
	public final void beforeTest()
	{
		newToOrig = new java.util.TreeMap<CmpVertex,CmpVertex>();
	}
	
	public final void runPatch(String fileA, String fileB)
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = Transform.convertToNumerical(LearnerGraph.loadGraph(fileA, config));
		LearnerGraph grB = Transform.convertToNumerical(LearnerGraph.loadGraph(fileB, config));
		GD gd = new GD();
		LearnerGraph graph = Transform.convertToNumerical(LearnerGraph.loadGraph(fileA, config));
		ChangesRecorder.applyGD(graph, gd.computeGDToXML(grA, grB, threadNumber, TestGD.createDoc()));
		WMethod.checkM(graph, grB);Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
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
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraph grA = Transform.convertToNumerical(LearnerGraph.loadGraph(fileNameA, config));
		LearnerGraph grB = Transform.convertToNumerical(LearnerGraph.loadGraph(fileNameA, config));
		LearnerGraph graph = Transform.convertToNumerical(LearnerGraph.loadGraph(fileNameA, config));
		GD gd = new GD();
		ChangesRecorder.applyGD(graph, gd.computeGDToXML(grA, grB, threadNumber, TestGD.createDoc()));
		WMethod.checkM(graph, grB);Assert.assertEquals(grB.getStateNumber(),graph.getStateNumber());
	}
	
}
