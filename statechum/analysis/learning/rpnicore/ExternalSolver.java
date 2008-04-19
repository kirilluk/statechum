/**
 * 
 */
package statechum.analysis.learning.rpnicore;

import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.Visualiser.VIZ_PROPERTIES;
import cern.colt.list.DoubleArrayList;
import cern.colt.list.IntArrayList;
import cern.colt.matrix.DoubleFactory1D;
import cern.colt.matrix.DoubleFactory2D;
import cern.colt.matrix.DoubleMatrix1D;
import cern.colt.matrix.DoubleMatrix2D;
import cern.colt.matrix.linalg.LUDecompositionQuick;

public class ExternalSolver 
{
	public final int j_Ap[];
	public final int[] j_Ai;
	public final double []j_Ax;
	public final double j_b[]; 
	public final double j_x[];
	
	public ExternalSolver(int Ap[], int[] Ai, double []Ax, double b[], double x[])
	{
		j_Ap=Ap;j_Ai=Ai;j_Ax=Ax;j_b=b;j_x=x;
		if (Ap.length <= 2)
			throw new IllegalArgumentException("zero-sized problem");
		
		if (Ap.length != b.length+1 || b.length != x.length || Ax.length != Ai.length)
			throw new IllegalArgumentException("inconsistent dimension of a matrix");
		
		if (j_Ap[0] != 0)
			throw new IllegalArgumentException("Ap[0] should be 0");			
	}
	
	public enum LibraryLoadResult { SUCCESS, FAILURE, NOT_ATTEMPTED };
	private static LibraryLoadResult libraryLoaded = LibraryLoadResult.NOT_ATTEMPTED;
	
	/** Solves the system of equations, placing the solution in the x array. For details, refer to UMFPACK manual. */
	static native boolean extsolve(int Ap[], int[] Ai, double []Ax, double b[], double x[]);
	
	/** Allocates the memory for use by the solver. */
	private static native boolean extmalloc(int n,int c);
	
	/** Enables (>1) or disables (=0) iterative refinement. */
	static native boolean setIRStep(int ir);
	
	/** Solves the system of equations using the external solver. */
	public boolean solveExternally()
	{
		boolean result = false;
		if (libraryLoaded == LibraryLoadResult.NOT_ATTEMPTED)
			loadLibrary();// throws on error

		if (libraryLoaded == LibraryLoadResult.SUCCESS)
		{
			result = extsolve(j_Ap,j_Ai,j_Ax,j_b,j_x);
		}
		else
			if (libraryLoaded == LibraryLoadResult.FAILURE)
				throw new IllegalArgumentException("earlier attempt to load a library failed");
		
		return result;
	}

	private void reallocate()
	{
		if (j_Ap.length > currentMaxSize)
		{
			currentMaxSize = j_Ap.length;extmalloc(currentMaxSize, current_c);
		}
	}
	
	/** Solves the system of equations using the external solver, 
	 * if it is available, otherwise falls back on Colt.
	 * 
	 * Throws IllegalArgumentException of the matrix is singular.
	 */
	public void solve()
	{
		if (libraryLoaded == LibraryLoadResult.NOT_ATTEMPTED)
		try
		{
			loadLibrary();// throws on error
		}
		catch(IllegalArgumentException ex)
		{
			if (Boolean.getBoolean(Visualiser.getProperty(VIZ_PROPERTIES.ASSERT, "false")))
			{
				System.err.println("WARNING: failed to load external solver library");
				ex.getCause().printStackTrace(System.err);
			}
		}
		
		if (libraryLoaded == LibraryLoadResult.SUCCESS)
			solveExternally();
		
		solveUsingColt();
	}

	public void solveUsingColt()
	{
		LUDecompositionQuick solver = new LUDecompositionQuick();
		DoubleMatrix2D matrix = toDoubleMatrix2D();
		solver.decompose(matrix);solver.setLU(matrix);
		DoubleMatrix1D result = toDoubleMatrix1D();
		solver.solve(result);result.toArray(j_x);		
	}
	
	/** The following two variables are not currently in use.
	 * The amount of memory currently allocated on the solver. Negative will force allocation. This is necessary for initialisation. */
	private static int currentMaxSize = -1;
	private static int current_c = 1;
	
	public static void loadLibrary()
	{
		try
		{
			if (libraryLoaded == LibraryLoadResult.NOT_ATTEMPTED)
				System.loadLibrary("cygStatechumSolver-1");

			setIRStep(1);
			libraryLoaded = LibraryLoadResult.SUCCESS;
		}
		catch(Exception ex)
		{// failed to load
			IllegalArgumentException e = new IllegalArgumentException("failed to load solver library: "+ex.getMessage());e.initCause(ex);
			libraryLoaded = LibraryLoadResult.FAILURE;
			throw e;
		}
	}
	
	/** Convertes a Colt matrix into the form appopriate for UMFPACK solver. */
	public ExternalSolver(DoubleMatrix2D matrix)
	{
		if (matrix.columns() != matrix.rows())
			throw new IllegalArgumentException("matrix should be square");
		int size = matrix.columns();
		j_b = new double[size];j_x = new double[size];j_Ap=new int[size+1];
		IntArrayList CoordX = new IntArrayList(),CoordY = new IntArrayList();DoubleArrayList values = new DoubleArrayList();
		matrix.getNonZeros(CoordY, CoordX, values);
		int nz=CoordX.size();
		j_Ai = new int[nz];j_Ax=new double[nz];
		j_Ap[0]=0;
		
		int elemsPerCol[] = new int[size+1];// stores the number of non-zero entries per column.
		elemsPerCol[size]=0;
		for(int i=0;i<nz;++i)
			elemsPerCol[CoordX.getQuick(i)]++;// fills in elemsPerCol, assuming a random distribution of coordinates in the Coord lists.
		
		int currentIdxCol[]= new int[size+1];// currentIdxCol[k] stores the current index into j_Ai where the next index from column k will go.
		int currentColumnIdx = 0;
		for(int i=0;i<=size;++i)
		{
			j_Ap[i]=currentColumnIdx;
			currentIdxCol[i] = currentColumnIdx;
			currentColumnIdx+=elemsPerCol[i];			              
		}
		assert j_Ap[size] == nz;

		for(int i=0;i<nz;++i)
		{
			int idx = currentIdxCol[CoordX.getQuick(i)]++;
			j_Ai[idx]=CoordY.getQuick(i);j_Ax[idx]=values.getQuick(i);
		}
	}
	
	/** Converts this matrix to Colt format, for a fallback on Colt when external solver is not found. */
	public DoubleMatrix2D toDoubleMatrix2D()
	{
		DoubleMatrix2D result = DoubleFactory2D.sparse.make(j_Ap.length-1, j_Ap.length-1);
		for(int y=0;y<j_Ap.length-1;++y)
			for(int i=j_Ap[y];i<j_Ap[y+1];++i)
				result.setQuick(j_Ai[i],y, j_Ax[i]);
		
		return result;
	}

	/** Converts the "b" vector to Colt format, for a fallback on Colt when external solver is not found. */
	public DoubleMatrix1D toDoubleMatrix1D()
	{
		DoubleMatrix1D result = DoubleFactory1D.dense.make(j_b.length);
		result.assign(j_b);
		return result;
	}
}