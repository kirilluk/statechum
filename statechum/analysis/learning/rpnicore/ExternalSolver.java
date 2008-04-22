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

import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.Visualiser.VIZ_PROPERTIES;
import cern.colt.list.DoubleArrayList;
import cern.colt.list.IntArrayList;
import cern.colt.matrix.DoubleFactory1D;
import cern.colt.matrix.DoubleFactory2D;
import cern.colt.matrix.DoubleMatrix1D;
import cern.colt.matrix.DoubleMatrix2D;
import cern.colt.matrix.linalg.LUDecompositionQuick;

/** Interfaces to an external solver.
 * <p> 
* Building the external solver is described below. 
* The procedure is similar for Linux and Win32 (with cygwin). 
* <ul>
* <li>
* You need Atlas (such as version 3.8.1), {@link http://math-atlas.sourceforge.net}.
* After uncompressing it, create a build directory builddir in its top folder,
* enter it
* <pre>
* cd ATLAS/builddir
* <pre>
* and run (replacing the prefix path with the one you'd like Atlas to be installed)
* <pre>  
* ../configure --prefix=/usr/local/soft/atlas-3.8.1 -Fa alg -fPIC
* </pre>
* Note: on Win32 with cygwin, 
* <ul>
* <li>make sure you include <em>-b 32</em> on the command line of configure 
* to ensure that it does not attempt to build a 64-bit library and fail.
* </li> 
* <li>the configure script may fail to detect the
* number of cores and decide to abort. Forcing this number to the actual
* number of processors by modifying the appropriate file would help.
* <pre>
* ATLAS/CONFIG/src/config.c
* </pre>
* To correct the problem, you need to add one line after the probe "GetIntProbe",
* as follows:
* <pre>
* iret = GetIntProbe(verb, ln, "arch", "NCPU", 2048);
* if (iret == 0) { printf("\nKIRR ugly hack: forcing NCPU to 2");iret=2;}
* </pre>
* </li></ul>
* 
* After configure succeeds, you need to run "make" to build Atlas, 
* <pre>
* make check
* </pre>
* to verify that it works and "make install" to install it.<p>
* Note that when building starts, you need an unloaded PC for it to collect the 
* relevant times for self-optimising. For this reason,
* <pre>
* cpufreq-selector -c 0 -g performance
* cpufreq-selector -c 1 -g performance
* </pre>
* might help to disable power-saving (Atlas calls it "CPU throttling" and 
* attempts to detect it).
* </li>
* 
* <li>
* You need UMFPACK, {@link http://www.cise.ufl.edu/research/sparse/umfpack/}
* which includes AMD and UFconfig. These three need to be uncompressed
* starting from the same directory.
* Subsequently, configuration needs to be set. The file to edit is 
* <pre> 
* UFconfig/UFconfig.mk
* </pre>
* It has to have the following two lines included (which would usually replace
* the appropriate lines in the file rather than added at the end):
* <pre>
* UMFPACK_CONFIG = -DNO_TIMER -fPIC
* BLAS = -L/usr/local/soft/atlas-3.8.1/lib -lf77blas -latlas -lgfortran
* </pre>
* Note the reference to the Atlas installation directory. You may also wish 
* to replace -llf77blas with -lptf77blas, but it should not affect anything.
* <p>
* Win32/cygwin note: if using gcc version 3 rather than 4, use the following line
* <pre>
* BLAS = -L/usr/local/soft/atlas-3.8.1/lib -lf77blas -latlas -lg2c
* </pre>
* Subsequently, running make in the UMFPACK directory will build UMFPACK (and AMD).
* </li>
* 
* <li>
* If file 
* <pre>
* linear/configure
* </pre>
* is not present, you need to build it as follows:
* <pre>
* cd linear
* ./bootstrap
* </pre>
* Building the interface involves running
* <pre>
* cd linear
* ./configure --with-blasdir=/usr/local/soft/atlas-3.8.1/lib --with-umfpack=/usr/local/src/umfpack && make
* </pre>
* Note that the above includes both Atlas directory and the one into which 
* UMFPACK was extracted. Subsequently, running make should build the library.
* <p>
* On Win32 with cygwin's gcc version 3, this will not work because libtool will not link to static libraries
* (.a import libraries from .dlls are ok). Linking to all the object files from atlas, amd and umfpack will
* not work either because these .o files have not been built with libtool. The only option is hence to 
* build a .dll from atlas, amd and umfpack and link to that dll.
* Multi-threaded support does not work because libptf77blas links to cygwin1.dll which we cannot use - 
* this dll does not support being dynamically loaded (a well-documented problem) so the library we build 
* with it will not work with Java (jvm will lock up on LoadLibrary). 
* </li>
* 
* <li>
* The easiest way to build a library is to edit <em>fullrebuild.sh</em> by adding the relevant paths 
* to it and then issue 
* <pre>
* cd linear
* ./fullrebuild.sh
* </pre>
* On Linux, this runs bootstrap and make. On Win32, 
* this script takes all .o files from atlas, umfpack and amd, builds a library out of them, converts it to .dll
* (following {@link http://www.kevinsheppard.org/research/matlabatlas}) and subsequently links the interface module to that .dll.
* If any changes are made to the .c or .h file, the library can be rebuilt with 
* <pre>
* make -f Makefile_win32
* </pre>
* where the <em>Makefile_win32</em> is built by <em>fullrebuild.sh</em>.
* <p>
* Note that the way the script builds a dll is very ugly but since it seems that cygwin does not do what one would
* expect it to (build a dll out of static libraries built with -fPIC), 
* I think it is not appropriate to "fix" configure scripts to work around these problems.
* </li>
 
* <li>
* In order for the just built library to be picked by Java, 
* you need to pass the following as a JVM option: 
* <pre>
* -Djava.library.path=linear/.libs
* </pre> 
* </li></ul>
*/
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
		if (Ap.length <= 1)
			throw new IllegalArgumentException("zero-sized problem");
		
		int nz = j_Ap[j_Ap.length-1];
		if (nz > Ai.length || nz > Ax.length)
			throw new IllegalArgumentException("too few elements in Ax or Ai");

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
			if (Boolean.valueOf(Visualiser.getProperty(VIZ_PROPERTIES.EXTERNALSOLVER, "false")))
			{
				System.err.println("WARNING: failed to load the external solver library");
				ex.getCause().printStackTrace(System.err);
			}
		}
		
		if (libraryLoaded == LibraryLoadResult.SUCCESS)
			solveExternally();
		else
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
	
	/** Attempts to load the library from the specified path 
	 * implicitly (considering java.library.path variable)
	 */
	private static UnsatisfiedLinkError tryLoading(String name)
	{
		UnsatisfiedLinkError result = null;
		
		try
		{ System.loadLibrary(name); }
		catch(UnsatisfiedLinkError ex)
		{ result = ex; }
		
		return result;
	}
	
	public static void loadLibrary()
	{
		if (libraryLoaded == LibraryLoadResult.NOT_ATTEMPTED)
		{
			UnsatisfiedLinkError ex = null;
			ex = tryLoading("cygStatechumSolver-1");
			if (ex != null) ex = tryLoading("StatechumSolver");

			if (ex != null)
			{// failed to load our library.
				libraryLoaded = LibraryLoadResult.FAILURE;
				IllegalArgumentException exc = new IllegalArgumentException("failed to load the solver library");
				exc.initCause(ex);throw exc;
			}

			setIRStep(1);
			libraryLoaded = LibraryLoadResult.SUCCESS;
		}
	}
	
	/** Converts a Colt matrix into the form appropriate for UMFPACK solver. */
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
	
	/** Converts this matrix to Colt format, for a fall back on Colt when external solver is not found. */
	public DoubleMatrix2D toDoubleMatrix2D()
	{
		DoubleMatrix2D result = DoubleFactory2D.sparse.make(j_Ap.length-1, j_Ap.length-1);
		for(int y=0;y<j_Ap.length-1;++y)
			for(int i=j_Ap[y];i<j_Ap[y+1];++i)
				result.setQuick(j_Ai[i],y, j_Ax[i]);
		
		return result;
	}

	/** Converts the "b" vector to Colt format, for a fall back on Colt when external solver is not found. */
	public DoubleMatrix1D toDoubleMatrix1D()
	{
		DoubleMatrix1D result = DoubleFactory1D.dense.make(j_b.length);
		result.assign(j_b);
		return result;
	}
}