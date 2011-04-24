/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

package statechum.analysis.learning.linear;

import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Random;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.analysis.learning.rpnicore.LSolver;

import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;
import cern.colt.function.DoubleFunction;
import cern.colt.list.DoubleArrayList;
import cern.colt.list.IntArrayList;
import cern.colt.matrix.DoubleFactory1D;
import cern.colt.matrix.DoubleFactory2D;
import cern.colt.matrix.DoubleMatrix1D;
import cern.colt.matrix.DoubleMatrix2D;
import cern.colt.matrix.linalg.LUDecompositionQuick;

public class TestSolver {
	protected DoubleMatrix2D testMatrix = null;
	
	/** Checks that the supplied arguments satisfy the <em>Ax=b</em> equation. 
	 * 
	 * @param A
	 * @param b
	 * @param x
	 */
	public static final void verifyAxb(DoubleMatrix2D A, DoubleMatrix1D b, DoubleMatrix1D x)
	{
		DoubleMatrix1D result = A.zMult(x, b);for(int i=0;i<b.size();++i) Assert.assertEquals(b.getQuick(i),result.getQuick(i),Configuration.fpAccuracy);
	}
	
	/** Checks that the supplied arguments satisfy the <em>Ax=b</em> equation. 
	 */
	public static final void verifyAxb(DoubleMatrix2D A, DoubleMatrix1D b, double[] x)
	{
		verifyAxb(A,b,DoubleFactory1D.dense.make(x));
	}
	
	/** Checks that the supplied arguments satisfy the <em>Ax=b</em> equation. 
	 */
	public static final void verifyAxb(LSolver solver)
	{
		verifyAxb(solver.toDoubleMatrix2D(),solver.toDoubleMatrix1D(),DoubleFactory1D.dense.make(solver.j_x));
	}
	
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

		LSolver.loadLibrary();
	}
	
	@Test
	public final void testExternalSolver1()
	{
		LSolver s = new LSolver(testMatrix,DoubleFactory1D.dense.make(new double[]{8., 45., -3., 3., 19.}));
		DoubleMatrix1D x = DoubleFactory1D.dense.make(testMatrix.rows());
		for(int i=0;i<s.j_b.length;++i) x.setQuick(i, s.j_b[i]);
		
		// Test 1
		s.solveExternally();
		for(int i=0;i<testMatrix.rows();++i)
			Assert.assertEquals(i+1, s.j_x[i],Configuration.fpAccuracy);
		verifyAxb(s);
		
		// Test 2
		LUDecompositionQuick solver = new LUDecompositionQuick();
		solver.decompose(testMatrix);solver.setLU(testMatrix);
		solver.solve(x);
		for(int i=0;i<testMatrix.rows();++i)
			Assert.assertEquals(i+1, x.getQuick(i),Configuration.fpAccuracy);
		verifyAxb(s);

		// Test 3
		for(int i=0;i<testMatrix.rows();++i) s.j_x[i]=0;
		s.solveUsingColt();
		for(int i=0;i<testMatrix.rows();++i)
			Assert.assertEquals(i+1, s.j_x[i],Configuration.fpAccuracy);
		verifyAxb(s);
	}
	
	@Test
	public final void testExternalSolver_fail0A()
	{
		final int   Ap [ ] = {0} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
		final double b [ ] = {8., 45., -3., 3., 19.} ;
		final double x[] = new double[b.length];
		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				LSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"zero-sized problem");		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				new LSolver(Ap, Ai, Ax, b, x);
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
		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				LSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				new LSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
	}
	

	@Test
	public final void testExternalSolver_fail1()
	{
		final int   Ap [ ] = {0, 2, 5, 9, 10, 12, 9999} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
		final double b [ ] = {8., 45., -3., 3., 19.} ;
		final double x[] = new double[b.length];
		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				LSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"too few");		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				new LSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"too few");		
	}
	
	@Test
	public final void testExternalSolver_fail2()
	{
		final int   Ap [ ] = {9999, 2, 5, 9, 10, 12} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
		final double b [ ] = {8., 45., -3., 3., 19.} ;
		final double x[] = new double[b.length];
		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				LSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"Ap[0] should be 0");		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				new LSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"Ap[0] should be 0");		
	}
	
	@Test
	public final void testExternalSolver_fail3a()
	{
		final int   Ap [ ] = {0, 2, 5, 9, 10, 12} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4,9999} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
		final double b [ ] = {8., 45., -3., 3., 19.} ;
		final double x[] = new double[b.length];

		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				LSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				new LSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
	}
	
	@Test
	public final void testExternalSolver_fail3b()
	{
		final int   Ap [ ] = {0, 2, 5, 9, 10, 12} ;
		final int    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4} ;
		final double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.,9999} ;
		final double b [ ] = {8., 45., -3., 3., 19.} ;
		final double x[] = new double[b.length];

		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				LSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				new LSolver(Ap, Ai, Ax, b, x);
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
		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				LSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				new LSolver(Ap, Ai, Ax, b, x);
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
		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				LSolver.extsolve(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				new LSolver(Ap, Ai, Ax, b, x);
			}
		}, IllegalArgumentException.class,"inconsistent dimension");		
	}
	
	@Test
	public final void testExternalSolver_fail6()
	{
		final int size = testMatrix.rows();
		final DoubleMatrix2D matrix = DoubleFactory2D.sparse.make(size,size);
		final LSolver solver = new LSolver(matrix,DoubleFactory1D.dense.make(new double[]{8., 45., -3., 3., 19.}));
		final DoubleMatrix1D x = DoubleFactory1D.dense.make(testMatrix.rows());
		
		for(int i=0;i<solver.j_b.length;++i) x.setQuick(i, solver.j_b[i]);
		
		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				solver.solveExternally();
			}
		}, IllegalArgumentException.class,"singular");		

		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
				LUDecompositionQuick coltSolver = new LUDecompositionQuick();
				coltSolver.decompose(matrix);coltSolver.setLU(matrix);
				coltSolver.solve(x);
			}
		}, IllegalArgumentException.class,"singular");		

		checkForCorrectException(new whatToRun() {
			public @Override void run() throws NumberFormatException {
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
	public final void testConversionToUMFPACK()
	{

		/*
2  3  0 0 0
3  0  4 0 6
0 -1 -3 2 0
0  0  1 0 0
0  4  2 0 1
		 */
		LSolver s = new LSolver(testMatrix,DoubleFactory1D.dense.make(testMatrix.rows(),0));
		Assert.assertArrayEquals(new int[] {0, 2, 5, 9, 10, 12}, s.j_Ap);
		Assert.assertArrayEquals(new int[] {0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4}, s.j_Ai);
		for(int i=0;i<s.j_Ap.length;++i)
			Assert.assertEquals(new double[]{2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.}[i], s.j_Ax[i],Configuration.fpAccuracy);
		
		DoubleMatrix2D mat = s.toDoubleMatrix2D();
		Assert.assertEquals(testMatrix, mat);
	}
	
	@Test
	public final void testMediumSizeSparseMatrix_takes_35sec()
	{
		final int size=1500;//(int)Math.sqrt(Integer.MAX_VALUE)-1;
		DoubleFunction randomGenerator = new DoubleFunction() {
			private final Random rnd = new Random(0);
			
			@Override 
			public double apply(@SuppressWarnings("unused")	double argument) {
				return rnd.nextDouble()*10;
			}
			
		};

		final DoubleMatrix2D matrix = DoubleFactory2D.sparse.identity(size);
		Random rnd = new Random(1);
		for(int cnt=0;cnt < 14000;++cnt)
		{
			int x = rnd.nextInt(size), y = rnd.nextInt(size);
			matrix.setQuick(x, y, 0.5);
		}
		final DoubleMatrix1D b = DoubleFactory1D.dense.make(size);
		b.assign(randomGenerator);

		final LSolver solver = new LSolver(matrix,b);
		long tmStarted = new Date().getTime();
		LUDecompositionQuick coltSolver = new LUDecompositionQuick();
		coltSolver.decompose(matrix);coltSolver.setLU(matrix);
		
		DoubleMatrix1D vector = b.copy();
		coltSolver.solve(vector);
		long tmFinished = new Date().getTime();
		System.out.println(" time taken: "+((double)tmFinished-tmStarted)/1000);
		verifyAxb(matrix, b, vector);
		
		tmStarted = new Date().getTime();
		solver.solveExternally();
		tmFinished = new Date().getTime();
		System.out.println(" time taken: "+((double)tmFinished-tmStarted)/1000);		
		for(int i=0;i<matrix.rows();++i)
			Assert.assertEquals(solver.j_x[i], vector.getQuick(i),Configuration.fpAccuracy);
		verifyAxb(solver);
	}

	/** Builds a sparse random matrix for the solver. I have to use this for large
	 * matrices because Colt cannot handle 100k variables.
	 * <p>
	 * @param size the number of variables.
	 * @param perCol the number of non-zero entries per column (diagonal is always set).
	 * @return UMFPACK representation of the matrix.
	 */
	protected LSolver buildSolver(final int size,int perCol)
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
		return new LSolver(Ap,Ai,Ax,b,x);
	}
	
	/** Tests whether buildSolver works. */
	@Test
	public final void testLargeMatrix_build0()
	{
		DoubleMatrix2D s = buildSolver(500,1).toDoubleMatrix2D();
		Assert.assertEquals(DoubleFactory2D.sparse.identity(500), s);
	}

	/** Tests whether buildSolver works. */
	@Test
	public final void testLargeMatrix_build1()
	{
		for(int size=100;size<2100;size+=400)
			for(int elemsPerColExp=0;elemsPerColExp< 7;elemsPerColExp++)
			{
				int elemsPerColExpected = (int)Math.pow(2, elemsPerColExp);
				DoubleMatrix2D matrix = buildSolver(size,elemsPerColExpected).toDoubleMatrix2D();
				
				IntArrayList CoordX = new IntArrayList(),CoordY = new IntArrayList();DoubleArrayList values = new DoubleArrayList();
				matrix.getNonZeros(CoordY, CoordX, values);
				int nz=CoordX.size();
				int diagElements = 0, nondiagElements = 0;
				for(int i=0;i<nz;++i)
				{
					if (CoordX.getQuick(i) == CoordY.getQuick(i))
					{
						Assert.assertEquals(1, values.getQuick(i),Configuration.fpAccuracy);diagElements++;
					}
					else nondiagElements++;
				}
				Assert.assertEquals(size, diagElements);
				Assert.assertEquals(size*(elemsPerColExpected-1), nondiagElements);
			}
	}

	@Test
	public final void testLargeMatrix1()
	{
		LSolver solver = buildSolver(10000,4);
		long tmStarted = new Date().getTime();
		solver.solveExternally();
		long tmFinished = new Date().getTime();
		System.out.println("time taken: "+((double)tmFinished-tmStarted)/1000);
		verifyAxb(solver);
	}

	@Test
	public final void testLargeMatrix2()
	{
		LSolver solver = buildSolver(15000,3);solver.solveExternally();verifyAxb(solver);
	}
}
