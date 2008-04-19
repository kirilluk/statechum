/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

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

package statechum.analysis.learning.rpnicore;

import static statechum.analysis.learning.TestFSMAlgo.buildGraph;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.Map.Entry;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import cern.colt.function.DoubleFunction;
import cern.colt.matrix.DoubleFactory1D;
import cern.colt.matrix.DoubleFactory2D;
import cern.colt.matrix.DoubleMatrix1D;
import cern.colt.matrix.DoubleMatrix2D;
import cern.colt.matrix.linalg.LUDecompositionQuick;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.experiments.TestAbstractExperiment;
import statechum.analysis.learning.experiments.TestAbstractExperiment.whatToRun;
import statechum.analysis.learning.rpnicore.Transform.AddToMatrix;

public class TestLinear {
	
	static class TestMatrixEntryAdder implements AddToMatrix
	{
		private final Set<String> result = new HashSet<String>();
		
		public synchronized void addMapping(int A, int B, double value) {
			StringBuffer buffer = new StringBuffer();
			Transform.addAssignement(buffer, A, B, value);result.add(buffer.toString());
		}
		
		public Set<String> getResult()
		{
			return result;
		}
	}
	
	@Test
	public final void testAddToBuffer0()
	{
		LearnerGraph gr=new LearnerGraph(Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);gr.buildCachedData();
		ad.buildMatrix(testAdder,1);
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
			/* AA */ "mat(1,1)=1.0;"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}

	@Test
	public final void testAddToBuffer1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->A-b->B",	"testAddToBuffer1"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);gr.buildCachedData();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("B"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(2,2)=2.0;"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	@Test
	public final void testAddToBuffer2()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nC-a->D",	"testAddToBuffer2"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);gr.buildCachedData();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("C"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(4,4)=1.0;","mat(4,8)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}

	@Test
	public final void testAddToBuffer3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C",	"testAddToBuffer3"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);gr.buildCachedData();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(7,7)=2.0;","mat(7,5)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	@Test
	public final void testAddToBuffer4()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C","testAddToBuffer4"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);gr.buildCachedData();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(7,7)=2.0;","mat(7,5)=-"+ad.valueK+";","mat(7,6)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}

	@Test
	public final void testAddToBuffer5()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C\nD-c->A","testAddToBuffer5"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);gr.buildCachedData();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(7,7)=3.0;","mat(7,5)=-"+ad.valueK+";","mat(7,6)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	@Test
	public final void testAddToBuffer6()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nD-a->C\nD-b->C\nD-c->A","testAddToBuffer6"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);gr.buildCachedData();
		ad.addToBuffer(testAdder, gr.findVertex("D"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(10,10)=3.0;","mat(10,1)=-"+ad.valueK+";","mat(10,6)=-"+2*ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	@Test
	public final void testAddToBuffer7()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);gr.buildCachedData();
		ad.addToBuffer(testAdder, gr.findVertex("A"), gr.findVertex("D"));
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
				"mat(7,7)=4.0;","mat(7,5)=-"+ad.valueK+";","mat(7,6)=-"+2*ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	
	@Test
	public final void testAddToBuffer8_1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->B-b->A","testAddToBuffer8"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);gr.buildCachedData();
		ad.buildMatrix(testAdder,1);
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
			/* AA */ "mat(1,1)=1.0;","mat(1,3)=-"+ad.valueK+";",
			/* BB */ "mat(3,3)="+(2.0-ad.valueK)+";","mat(3,1)=-"+ad.valueK+";",
			/* AB */ "mat(2,2)="+2.0+";","mat(2,3)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	@Test
	public final void testAddToBuffer8_2()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->B-b->A","testAddToBuffer8"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);gr.buildCachedData();
		ad.buildMatrix(testAdder,2);
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
			/* AA */ "mat(1,1)=1.0;","mat(1,3)=-"+ad.valueK+";",
			/* BB */ "mat(3,3)="+(2.0-ad.valueK)+";","mat(3,1)=-"+ad.valueK+";",
			/* AB */ "mat(2,2)="+2.0+";","mat(2,3)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	@Test
	public final void testAddToBuffer8_3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B-a->B-b->A","testAddToBuffer8"),Configuration.getDefaultConfiguration());
		TestMatrixEntryAdder testAdder = new TestMatrixEntryAdder();Transform ad = new Transform(gr);gr.buildCachedData();
		ad.buildMatrix(testAdder,8);
		Collection<String> expected = new HashSet<String>();expected.addAll(Arrays.asList(new String[] {
			/* AA */ "mat(1,1)=1.0;","mat(1,3)=-"+ad.valueK+";",
			/* BB */ "mat(3,3)="+(2.0-ad.valueK)+";","mat(3,1)=-"+ad.valueK+";",
			/* AB */ "mat(2,2)="+2.0+";","mat(2,3)=-"+ad.valueK+";"}));
		Assert.assertEquals(expected, testAdder.getResult());
	}
	
	LearnerGraph grLoadDistribution=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_1()
	{
		grLoadDistribution.transform.partitionWorkLoad(0);
	}

	/** Tests how well the workload is distributed. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWorkLoadDistribution0_2()
	{
		grLoadDistribution.transform.partitionWorkLoad(-1);
	}

	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution1()
	{
		Assert.assertArrayEquals(new int[]{0,4},grLoadDistribution.transform.partitionWorkLoad(1));
	}

	/** Tests how well the workload is distributed. */
	@Test
	public final void testWorkLoadDistribution2()
	{
		Assert.assertArrayEquals(new int[]{0,2,3,4,4},grLoadDistribution.transform.partitionWorkLoad(4));
	}
	
	/** Tests the workload distribution. */
	@Test
	public final void testWorkLoadDistribution3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->B\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testAddToBuffer7"),Configuration.getDefaultConfiguration());
		for(int i=0;i< 4;++i) Transform.addToGraph(gr, grLoadDistribution);
		Assert.assertArrayEquals(new int[]{0,10,14,17,20},gr.transform.partitionWorkLoad(4));
	}
	
	@Test
	public final void testEstimation1()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C\nD-c->A","testEstimation1"),Configuration.getDefaultConfiguration());
		// AA: 3
		Assert.assertEquals(1+3,gr.transform.estimateSize(gr.transitionMatrix.entrySet().iterator().next()));
	}

	@Test
	public final void testEstimation2()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A","testEstimation2"),Configuration.getDefaultConfiguration());
		// ACD
		//A3
		//C11
		Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> iter = gr.transitionMatrix.entrySet().iterator();iter.next();
		Assert.assertEquals(2+2,gr.transform.estimateSize(iter.next()));
	}
	
	@Test
	public final void testEstimation3()
	{
		LearnerGraph gr=new LearnerGraph(buildGraph("A-a->Q\nA-b->C\nA-d->C\nD-a->C\nD-b->C\nD-d->C-a->C\nD-c->A","testEstimation2"),Configuration.getDefaultConfiguration());
		// ACD
		//A3
		//C11
		//D314
		Iterator<Entry<CmpVertex,Map<String,CmpVertex>>> iter = gr.transitionMatrix.entrySet().iterator();iter.next();iter.next();
		Assert.assertEquals(3+8,gr.transform.estimateSize(iter.next()));
	}

	protected DoubleMatrix2D testMatrix = null;
	
	@Before
	public final void beforeTests()
	{
		final int size = 5;
		testMatrix = DoubleFactory2D.sparse.make(size,size);
		testMatrix.set(0, 0, 2);
		testMatrix.set(1, 0, 3);
		testMatrix.set(0, 1, 3);
		testMatrix.set(1, 2, 4);
		testMatrix.set(1, 4, 6);
		testMatrix.set(2, 1,-1);
		testMatrix.set(2, 2,-3);
		testMatrix.set(2, 3, 2);
		testMatrix.set(3, 2, 1);
		testMatrix.set(4, 1, 4);
		testMatrix.set(4, 2, 2);
		testMatrix.set(4, 4, 1);

		ExternalSolver.loadLibrary();
	}
	
	@Test
	public final void testExternalSolver1()
	{
		ExternalSolver s = new ExternalSolver(testMatrix);
		DoubleMatrix1D x = DoubleFactory1D.dense.make(testMatrix.rows());
		final double b [ ] = {8., 45., -3., 3., 19.} ;System.arraycopy(b, 0, s.j_b, 0, b.length);
		for(int i=0;i<b.length;++i) x.setQuick(i, b[i]);
		
		// Test 1
		s.solveExternally();
		for(int i=0;i<testMatrix.rows();++i)
			Assert.assertEquals(i+1, s.j_x[i],1e-8);

		// Test 2
		LUDecompositionQuick solver = new LUDecompositionQuick();
		solver.decompose(testMatrix);solver.setLU(testMatrix);
		solver.solve(x);
		for(int i=0;i<testMatrix.rows();++i)
			Assert.assertEquals(i+1, x.getQuick(i),1e-8);

		// Test 3
		for(int i=0;i<testMatrix.rows();++i) s.j_x[i]=0;
		s.solveUsingColt();
		for(int i=0;i<testMatrix.rows();++i)
			Assert.assertEquals(i+1, s.j_x[i],1e-8);
	}
	
	@Test
	public final void testExternalSolver_fail0A()
	{
		final int   Ap [ ] = {0} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
		final double b [ ] = {8., 45., -3., 3., 19.} ;
		final double x[] = new double[b.length];
		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				ExternalSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"zero-sized problem");		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				new ExternalSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"zero-sized problem");		
	}
	
	@Test
	public final void testExternalSolver_fail0B()
	{
		final int   Ap [ ] = {0,1} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
		final double b [ ] = {8., 45., -3., 3., 19.} ;
		final double x[] = new double[b.length];
		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				ExternalSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"zero-sized problem");		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				new ExternalSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"zero-sized problem");		
	}
	
	
	@Test
	public final void testExternalSolver_fail1()
	{
		final int   Ap [ ] = {0, 2, 5, 9, 10, 12, 9999} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
		final double b [ ] = {8., 45., -3., 3., 19.} ;
		final double x[] = new double[b.length];
		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				ExternalSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				new ExternalSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
	}
	
	@Test
	public final void testExternalSolver_fail2()
	{
		final int   Ap [ ] = {9999, 2, 5, 9, 10, 12} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
		final double b [ ] = {8., 45., -3., 3., 19.} ;
		final double x[] = new double[b.length];
		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				ExternalSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"Ap[0] should be 0");		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				new ExternalSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"Ap[0] should be 0");		
	}
	
	@Test
	public final void testExternalSolver_fail3()
	{
		final int   Ap [ ] = {0, 2, 5, 9, 10, 12} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4,9999} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
		final double b [ ] = {8., 45., -3., 3., 19.} ;
		final double x[] = new double[b.length];

		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				ExternalSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				new ExternalSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
	}
	
	@Test
	public final void testExternalSolver_fail4()
	{
		final int   Ap [ ] = {0, 2, 5, 9, 10, 12} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
		final double b [ ] = {8., 45., -3., 3., 19.,99999} ;
		final double x[] = new double[b.length];
		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				ExternalSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				new ExternalSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
	}
	
	@Test
	public final void testExternalSolver_fail5()
	{
		final int   Ap [ ] = {0, 2, 5, 9, 10, 12} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
		final double b [ ] = {8., 45., -3., 3., 19.} ;
		final double x[] = new double[b.length+6];
		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				ExternalSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				new ExternalSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
	}
	
	@Test
	public final void testExternalSolver_fail6()
	{
		final int size = testMatrix.rows();
		final DoubleMatrix2D matrix = DoubleFactory2D.sparse.make(size,size);
		final ExternalSolver solver = new ExternalSolver(matrix);
		final double b [ ] = {8., 45., -3., 3., 19.} ;System.arraycopy(b, 0, solver.j_b, 0, b.length);
		final DoubleMatrix1D x = DoubleFactory1D.dense.make(testMatrix.rows());
		
		for(int i=0;i<b.length;++i) x.setQuick(i, b[i]);
		
		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				solver.solveExternally();
			}
		}, IllegalArgumentException.class,"singular");		

		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				LUDecompositionQuick coltSolver = new LUDecompositionQuick();
				coltSolver.decompose(matrix);coltSolver.setLU(matrix);
				coltSolver.solve(x);
			}
		}, IllegalArgumentException.class,"singular");		

		TestAbstractExperiment.checkForCorrectException(new whatToRun() {
			public void run() throws NumberFormatException, IOException {
				for(int i=0;i<testMatrix.rows();++i) solver.j_x[i]=0;
				solver.solveUsingColt();
			}
		}, IllegalArgumentException.class,"singular");		
	}
		
	/** A very simple test for conversion of a matrix into UMFTOOL (matlab) format.
	 * Real testing is where I run the two solvers side by side.
	 * The example is verbatim from umftool manual.
	 */
	@Test
	public final void testConversionToUMFTOOL()
	{

		/*
2  3  0 0 0
3  0  4 0 6
0 -1 -3 2 0
0  0  1 0 0
0  4  2 0 1
		 */
		ExternalSolver s = new ExternalSolver(testMatrix);
		Assert.assertArrayEquals(new int[] {0, 2, 5, 9, 10, 12}, s.j_Ap);
		Assert.assertArrayEquals(new int[] {0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4}, s.j_Ai);
		for(int i=0;i<s.j_Ap.length;++i)
			Assert.assertEquals(new double[]{2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.}[i], s.j_Ax[i],1e-8);
		
		DoubleMatrix2D mat = s.toDoubleMatrix2D();
		Assert.assertEquals(testMatrix, mat);
	}
	
	@Test
	public final void testMediumSizeSparseMatrix()
	{
		final int size=1500;//(int)Math.sqrt(Integer.MAX_VALUE)-1;
		DoubleFunction randomGenerator = new DoubleFunction() {
			private final Random rnd = new Random(0);
			
			public double apply(double argument) {
				return rnd.nextDouble()*10;
			}
			
		};

		final DoubleMatrix2D matrix = DoubleFactory2D.sparse.identity(size);
		//matrix.assign(randomGenerator);
		Random rnd = new Random(1);
		for(int cnt=0;cnt < 14000;++cnt)
		{
			int x = rnd.nextInt(size), y = rnd.nextInt(size);
			matrix.setQuick(x, y, 0.5);
		}
		final DoubleMatrix1D vector = DoubleFactory1D.dense.make(size);
		vector.assign(randomGenerator);
		final ExternalSolver solver = new ExternalSolver(matrix);System.arraycopy(vector.toArray(), 0, solver.j_b, 0, size);
		long tmStarted = new Date().getTime();
		LUDecompositionQuick coltSolver = new LUDecompositionQuick();
		coltSolver.decompose(matrix);coltSolver.setLU(matrix);
		coltSolver.solve(vector);
		long tmFinished = new Date().getTime();
		System.out.println(" time taken: "+((double)tmFinished-tmStarted)/1000);
		
		tmStarted = new Date().getTime();
		solver.solveExternally();
		tmFinished = new Date().getTime();
		System.out.println(" time taken: "+((double)tmFinished-tmStarted)/1000);		
		for(int i=0;i<matrix.rows();++i)
			Assert.assertEquals(solver.j_x[i], vector.getQuick(i),1e-8);
	}

	/** Builds a sparse random matrix for the solver. I have to use this for large
	 * matrices because Colt cannot handle 100k variables.
	 * <p>
	 * @param size the number of variables.
	 * @param perCol the number of non-zero entries per column (diagonal is always set).
	 * @return UMFPACK representation of the matrix.
	 */
	protected ExternalSolver buildSolver(final int size,int perCol)
	{
		int Ap[]=new int[size+1], Ai[]=new int[size*perCol];
		double Ax[] = new double[size*perCol],b[]=new double[size],x[]=new double[size];
		Random rnd = new Random(1);
		
		
		int rowCoords[] = new int[perCol];
		HashSet<Integer> encounteredCoords = new HashSet<Integer>();
		for(int col=0;col<size;++col)
		{
			Ap[col]=perCol*col;
			
			rowCoords[0]=col;encounteredCoords.clear();encounteredCoords.add(col);
			for(int cnt=1;cnt<perCol;++cnt)
			{
				int pos = rnd.nextInt(size);
				do
				{// ensure that we do not hit an existing element.
					pos = rnd.nextInt(size);
				} while(encounteredCoords.contains(pos));
				
				encounteredCoords.add(pos);
				rowCoords[cnt]=pos; 
			}
			Arrays.sort(rowCoords);
			for(int cnt=0;cnt<perCol;++cnt)
			{
				int idx = perCol*col+cnt;
				Ai[idx]=rowCoords[cnt];
				Ax[idx]=rowCoords[cnt]==col?1:rnd.nextDouble();
			}
		}
		Ap[size]=perCol*size;
		
		for(int i=0;i<size;++i) 
		{
			b[i]=rnd.nextDouble();x[i]=0;
		}
		return new ExternalSolver(Ap,Ai,Ax,b,x);
	}
	
	/** Tests whether buildSolver works. */
	@Test
	public final void testLargeMatrix0()
	{
		DoubleMatrix2D s = buildSolver(500,1).toDoubleMatrix2D();
		Assert.assertEquals(DoubleFactory2D.sparse.identity(500), s);
	}

	@Test
	public final void testLargeMatrix1()
	{
		ExternalSolver solver = buildSolver(10000,4);
		long tmStarted = new Date().getTime();
		solver.solveExternally();
		long tmFinished = new Date().getTime();
		System.out.println(" time taken: "+((double)tmFinished-tmStarted)/1000);
	}

	@Test
	public final void testLargeMatrix2()
	{
		buildSolver(15000,3).solveExternally();
	}
}
