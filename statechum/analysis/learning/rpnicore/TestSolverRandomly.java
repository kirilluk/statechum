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
package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Random;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import cern.colt.function.DoubleFunction;
import cern.colt.matrix.DoubleFactory1D;
import cern.colt.matrix.DoubleFactory2D;
import cern.colt.matrix.DoubleMatrix1D;
import cern.colt.matrix.DoubleMatrix2D;
import cern.colt.matrix.linalg.LUDecompositionQuick;

@RunWith(Parameterized.class)
public class TestSolverRandomly {
	protected final DoubleFunction randomGenerator;
	protected final int size;
	
	public TestSolverRandomly(final Random conf, int s) {
		size = s;
		randomGenerator = new DoubleFunction() {
			private final Random rnd = conf;
			
			public double apply(double argument) {
				return rnd.nextDouble()*10;
			}
			
		};
	}
	
	@Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		for(int i=5;i<20;++i)
			result.add(new Object[]{new Random(i),new Integer(i*20)});
		
		return result;
	}

	@Test
	public final void testExternalSolver_random()
	{
		final DoubleMatrix2D matrix = DoubleFactory2D.sparse.make(size,size);
		matrix.assign(randomGenerator);
		final DoubleMatrix1D b = DoubleFactory1D.dense.make(size);
		b.assign(randomGenerator);
		final LSolver solver = new LSolver(matrix,DoubleFactory1D.dense.make(matrix.rows(), 0));

		boolean singular = false;
		try
		{
			solver.solveExternally();
		}
		catch(IllegalArgumentException ex)
		{
			if (ex.getMessage().contains("singular"))
				singular = true;
			else
				throw ex;
		}
		
		if (!singular)
			TestSolver.verifyAxb(solver);
		
		LUDecompositionQuick coltSolver = new LUDecompositionQuick();
		coltSolver.decompose(matrix);coltSolver.setLU(matrix);
		DoubleMatrix1D vector = DoubleFactory1D.dense.make(matrix.rows(), 0);
		
		try
		{
			coltSolver.solve(vector);
		}
		catch(IllegalArgumentException ex)
		{
			if (ex.getMessage().contains("singular"))
				Assert.assertTrue(singular);
			else
				throw ex;
		}

		if (!singular)
		{
			for(int i=0;i<matrix.rows();++i)
				Assert.assertEquals(solver.j_x[i], vector.getQuick(i),TestSolver.comparisonAccuracy);
			TestSolver.verifyAxb(matrix,solver.toDoubleMatrix1D(),vector);
		}
		
		try
		{
			for(int i=0;i<matrix.rows();++i) solver.j_x[i]=0;
			solver.solveUsingColt();
		}
		catch(IllegalArgumentException ex)
		{
			if (ex.getMessage().contains("singular"))
				Assert.assertTrue(singular);
			else
				throw ex;
		}
		
		if (!singular)
		{
			for(int i=0;i<matrix.rows();++i)
				Assert.assertEquals(solver.j_x[i], vector.getQuick(i),TestSolver.comparisonAccuracy);
			TestSolver.verifyAxb(solver);
		}
	}
}
