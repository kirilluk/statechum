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
package statechum.analysis.learning.rpnicore;

import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.learning.experiments.ExperimentRunner;
import cern.colt.list.DoubleArrayList;
import cern.colt.list.IntArrayList;
import cern.colt.matrix.DoubleFactory1D;
import cern.colt.matrix.DoubleFactory2D;
import cern.colt.matrix.DoubleMatrix1D;
import cern.colt.matrix.DoubleMatrix2D;
import cern.colt.matrix.linalg.LUDecompositionQuick;

/** Interfaces to an external solver.
* <h1>General building instructions. </h1>
* Building the external solver is described below. 
* The procedure is similar for Linux, MacOS X and Win32 (with cygwin). 
* <ul>
* <li>
* On both Linux and Win32 you will need Atlas (such as version 3.8.1), {@link http://math-atlas.sourceforge.net}.
* After uncompressing it, create a build directory builddir in its top folder,
* enter it
* <pre>
* cd ATLAS/builddir
* <pre>
* and run (replacing the prefix path with the one where you'd like Atlas to be installed)
* <pre>  
* ../configure --prefix=/usr/local/soft/atlas-3.10.1 -Fa alg -fPIC
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
* if (iret == 0) { printf("\nKIRR: ugly hack: forcing NCPU to 2");iret=2;}
* </pre>
* </li></ul>
* 
* After configure succeeds, you need to run "make" to build Atlas, then run 
* <pre>
* make check
* </pre>
* to verify that it works and "make install" to install it.<p>
* Note that when building starts, you need an unloaded PC for it to collect the 
* relevant times for self-optimising. For this reason,
* <pre>
* cpufreq-selector -g performance
* </pre>
* might help to disable power-saving (Atlas calls it "CPU throttling" and 
* attempts to detect it).  
* </li>
* 
* <li>You need UMFPACK, {@link http://www.cise.ufl.edu/research/sparse/umfpack/}. Packages to download are
* AMD, COLAMD, CHOLMOD and SuiteSparse_config. These need to be uncompressed
* starting from the same directory.
* Subsequently, configuration needs to be set. The file to edit is 
* <pre> 
* SuiteSparse_config/SuiteSparse_config.mk
* </pre>
* (or any other depending on whether you use a Mac or GPU.
* It has to have the following two lines included (which would usually replace
* the appropriate lines in the file rather than be added at the end):
* <pre>
* UMFPACK_CONFIG = -fPIC
* BLAS = -L/usr/local/soft/atlas-3.10.1/lib -lf77blas -latlas -lgfortran
* </pre>
* Note the reference to the Atlas installation directory. You may also wish 
* to replace -llf77blas with -lptf77blas, but it should not affect anything.
* <p>
* You might also want to 
* <pre>
* CC = gcc
* CFLAGS = -O3
* </pre>
* or even 
* <pre>
* CC = gcc -fPIC
* </pre>
* to ensure that AMD is built as a relocable library - without it Statechum's .so library may fail to build.
* <p>
* Win32/cygwin note: if using gcc version 3 rather than 4, use the following line
* <pre>
* BLAS = -L/usr/local/soft/atlas-3.8.1/lib -lf77blas -latlas -lg2c
* </pre>
* Subsequently, running make in the UMFPACK directory will build UMFPACK (and other libraries).
* Most likely, you will have to run 
* <pre>
* make install 
* </pre> 
* in all of AMD/CHOLMOD/COLAMD/SuiteSparse_config/UMFPACK in order to get all the files installed. Without them, it will not be possible to build libStatechumsolver
* The best way to do this is via
* <pre>
* for D in AMD CHOLMOD COLAMD SuiteSparse_config UMFPACK;do (cd $D;make install );done
* </pre>
* </li>
* 
* <li>If files
* <em>linear/configure</em> or <em>linear/Makefile.in</em>
* are not present, you need to build them as follows:
* <pre>
* cd linear
* ./bootstrap
* </pre>
* Building the interface involves running
* <pre>
* cd linear
* ./configure --with-blasdir=/usr/local/soft/atlas-3.10.1/lib --with-umfpack=/usr/local/src/umfpack && make
* </pre>
* Note that the above includes both Atlas directory and the one into which 
* UMFPACK was extracted. Subsequently, running make should build the library.
* <p>
* If gcc has gcj installed, Sun JDK may include a jni_md.h which will pick gcc's jni_md.h rather than Sun's one and give
* an error about a boolean. The solution is to do something like 
* <pre>
* CC='gcc -I/usr/local/soft/jdk1.6.0_16/include/linux/' sh ./configure --with-blasdir=/usr/local/soft/atlas-3.10.1 --with-umfpack=/usr/local/src/umfpack/
* </pre>
* <p>
* On Win32 with cygwin's gcc version 3, this will not work because libtool will not link to static libraries
* (.a import libraries from .dlls are ok). Linking to all the object files from atlas, amd and umfpack will
* not work because these .o files have not been built with libtool. The only option is hence to 
* build a .dll from atlas, amd and umfpack and link to that dll.
* Multi-threaded support does not work because libptf77blas links to cygwin1.dll which we cannot use - 
* this dll does not support being dynamically loaded (a well-documented problem) so the library we build 
* with it will not work with Java (jvm will lock up on LoadLibrary). 
* </li>
* 
* <li>The easiest way to build a library is to edit <em>fullrebuild.sh</em> by adding the relevant paths 
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
* 
* 
 * <h1>MacOS X - specific instructions</h1>
 * It may be advisable to build gcc first (gmp web page suggests it will not work when built with XCode so using a recent gcc may be advisable),
 * <ul>
 * <li>Using <em>Applications/Utilities/Java/Java Preferences</em>, set the default JVM to be 64-bit version 6.0.</li> 
 * <li>build gmp
 * <pre>
 * CC='gcc-4.2 -m64' ./configure --prefix=/usr/local/soft_a/gmp
 * make
 * make install
 * </pre></li>
 * <li>build mpfr
 * <pre>
 * CC='gcc-4.2 -m64' ./configure --with-gmp=/usr/local/soft_a/gmp --prefix=/usr/local/soft_a/mpfr
 * make
 * make install
 * </pre></li>
 * <li>
 * Build gcc,
 * <pre>
 * CC='gcc-4.2 -m64' ./configure --prefix=/usr/local/soft_a/gcc-4.4.1 --with-gmp=/usr/local/soft_a/gmp-4.3.1 --with-mpfr=/usr/local/soft_a/mpfr-2.4.1 --host=x86_64-apple-darwin9.8.0 --build=x86_64-apple-darwin9.8.0 --enable-languages="c c++"
*  </pre>
*  The -m64 ensures all of gcc is built for 64-bit. --host and --build make sure that gcc rebuilds itself for 64-bit.
*  </li>
*  <li>Now repeat the above but build gmp,mpfr and gcc with the just-built gcc, replacing soft_a with soft above.
*  The distinction between soft_a and soft is whether Apple's gcc (gcc-4.2) has been used to build a library or not.
*  </li>
*  <p>MacOS X or XCode come with blas libraries, hence one does not need to install Atlas (which can be built using instructions for building
*  Atlas on Linux). If Atlas is installed into somewhere like <em>/usr/local/soft/atlas-3.8.3</em>, in the course of building
*  UMFPACK gcc may choose to pick <em>/usr/lib/libatlas.dylib</em>
*  instead of the newly-installed Atlas library <em>/usr/local/soft/atlas-3.8.3/lib/libatlas.dylib</em>, leading to build errors 
*  for umfpack's examples because Atlas's symbols have different names to that of Apples's Atlas.
*  </li>
*  <li>After unpacking all three parts of UMFPACK, options to be included in <em>UFconfig/UFconfig.mk</em> are :
* <pre>
* UMFPACK_CONFIG = -DNO_TIMER -fPIC
* BLAS = -L/System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A -lblas
* </pre>
* </li>
* <li>
* For building Statechum library, the following is done:
* <pre>
* cd linear
* ./configure --with-blasdir=/System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A --with-umfpack=/Users/Shared/experiment/umfpack && make
* </pre>
* Where <em>/Users/Shared/experiment/umfpack</em> is where UMFPACK was extracted, it should have subdirectories UMFPACK, AMD and UFconfig.
* </li>
* <li>
* For testing, providing the <em>-Djava.awt.headless=true</em> option to JVM is necessary on MacOS to stop loading of Statechum's 
* properties from making a connection to a monitor
* and triggering a bouncing coffee icon in the dock as well as a display of application name in the menu bar.
* The problem is caused by static data in, for instance, <em>java.awt.Rectangle</em> making the said connection upon classloading of <em>java.awt.Rectangle</em>.
* </li></ul>
* 
* It is worth noting that the full path of the interface is encoded in the shared libraries hence
* moving this interface is only possible if libraries are rebuilt. For this reason, LSolver
* is stuck in the rpnicore rather than in learner/linear. 
* 
* Instructions for building OpenBLAS on Linux,
* Uncompress the .zip, then from the main directory run
* <pre>
* make PREFIX=/usr/local/soft/OpenBLAS-be853da DYNAMIC_ARCH=1 UTEST_CHECK=1 NO_LAPACK=1	NO_AFFINITY=1
* </pre>
* The first parameter is responsible for building in support for all CPU architectures, otherwise only the current one will 
* This will attempt to download CUnit and then fail.
* <pre>
* cd utest;make
* </pre>
* should build and install CUnit into utest. Subsequently doing the following command from the main directory (the one just above utest)
* <pre> 
* ln -s utest/CUnit-2.1-2/include/CUnit .
* </pre>
* would fix unit tests that ship with OpenBLAS and they will run when the above make command is run.
* After doing
* <pre> 
* make PREFIX=/usr/local/soft/OpenBLAS-be853da DYNAMIC_ARCH=1 UTEST_CHECK=1 NO_LAPACK=1 install
* </pre>
* OpenBLAS is available.
* SuiteSparse_config.mk should have  
* <pre>
* CC=x86_64-w64-mingw32-gcc
* AR=x86_64-w64-mingw32-ar
* RANLIB = x86_64-w64-mingw32-ranlib
* F77 = x86_64-w64-mingw32-gfortran
* BLAS = /usr/local/soft/OpenBLAS-be853da/lib/libopenblas.lib
* INSTALL_LIB=/usr/local/soft/umfpack-5.6.1-openblas/lib
* INSTALL_INCLUDE=/usr/local/soft/umfpack-5.6.1-openblas/include
* </pre>
* OpenBLAS on Windows7-x86_64
* First, build OpenBLAS with both options for Unix (above) and those mentioned in quickbuild.64bit.
* <p/>
* When building UMFPACK, most options are the same as for Unix but there are a few changes,
* <ul>
* <li>BLAS = /usr/local/soft/OpenBLAS-be853da/lib/libopenblas.lib</li>
* <li>It is worth removing 
* -fPIC from the definition of CF since on Windows all code is position-independent and this eliminates the warning.</li>
* <li>It is worth removing 
* -lrt from definition LIB because the library is missing.</li>
* </ul>
* Finally, Makefile in the UMFPACK/Demo directory has the "run" target, where it runs demos and records the results. Every time it runs a demo, the output
* has to be piped through d2u to correct line endings on Windows and through sed  's/e\([+-]\)0\([0-9]\+\)/e\1\2/g' to convert the differences in which exponents are output on Windows.
* Hence the extra content is like
* <pre>
* ... _demo | d2u |  sed  's/e\([+-]\)0\([0-9]\+\)/e\1\2/g' > my_ ... .out
* </pre>
* 
* <p>
* Using MacPorts on 10.6.xx with OpenBLAS from MacPorts, autoconf, automake, libtool and such, SuiteSparse_config.mk should have
* <pre>
* BLAS = -L/opt/local/lib -lopenblas -lgfortran
* LAPACK =
* CHOLMOD_CONFIG = -DNPARTITION
* UMFPACK_CONFIG =
* INSTALL_LIB = /usr/local/soft/umfpack/lib
* INSTALL_INCLUDE = /usr/local/soft/umfpack/include
* RANLIB = /opt/local/bin/gcc-ar-mp-4.8
* F77 = /opt/local/bin/gfortran-mp-4.8
* CC = /opt/local/bin/gcc-mp-4.8
* CF = $(CFLAGS) -O3 -fno-common -fexceptions -DNTIMER
* </pre>
* Subsequently, running ./bootstrap should rebuild the corresponding files (such as those in config). Configuration expects
* <pre>
* CFLAGS=-L/opt/local/lib F77=/opt/local/bin/gfortran-mp-4.8 CC=/opt/local/bin/gcc-mp-4.8 CXX=/opt/local/bin/g++-mp-4.8 ./configure --with-blasdir=/opt/local --with-umfpack=/usr/local/soft/umfpack
* </pre>
* Running make will build the solver that will contain a dependency on the gcc compiler used, in this instance, gcc in /opt/local/lib.
*/
public class LSolver 
{
	public int j_Ap[] = null;
	public int[] j_Ai = null;
	public double []j_Ax = null;
	public double j_b[] = null; 
	public final double j_x[];
	
	/** This one is a dummy constructor for the case where no solving is needed. */
	protected LSolver(int size)
	{
		j_x=new double[size];
	}
	
	/** Constructs an instance of this class and checks that arguments are valid. */
	public LSolver(int Ap[], int[] Ai, double []Ax, double b[], double x[])
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
	
	/** Sets all working arrays to null; the result is unchanged. */
	public void freeAllButResult()
	{
		j_Ap=null;j_Ai=null;j_Ax=null;j_b=null;
	}
	
	public enum LibraryLoadResult { SUCCESS, FAILURE, NOT_ATTEMPTED }
	private static LibraryLoadResult libraryLoaded = LibraryLoadResult.NOT_ATTEMPTED;
	
	/** If true, the underlying solver supports configuring the number of cpus to use. */
	private static boolean threadNumberSupported = false;
	
	/** Solves the system of equations, placing the solution in the x array. For details, refer to UMFPACK manual. */
	public static native boolean extsolve(int Ap[], int[] Ai, double []Ax, double b[], double x[]);

	/** Configures the external solver to use a specific number of threads. 
	 * Can be ignored by some solvers such as Atlas.
	 * 
	 * @param threads number of threads to use.
	 * @return true if the function is supported. Old builds of the corresponding native library do not have this function at all, hence we have to check whether
	 * calling this throws unsatisfiedlinkerror.
	 */
	public static native boolean setThreadNumber(int threads);
	
	/** Allocates the memory for use by the solver. This was introduced to 
	 * combat "out-of-memory" errors on 32-bit WinXP but was not useful for this purpose. 
	 */
	//private static native boolean extmalloc(int n,int c);
	
	/** Enables (>1) or disables (=0) iterative refinement. */
	static native boolean setIRStep(int ir);
	
	/** Solves the system of equations using the external solver throws if 
	 * external library cannot be found. Use <em>solve()</em> if you want
	 * a fallback on Colt when external library is not available. 
	 *
	 * @param theads the number of threads to use, only supported with some native libraries such as OpenBlas on Linux
	 */
	public boolean solveExternally(int threads)
	{
		boolean result = false;
		if (libraryLoaded == LibraryLoadResult.NOT_ATTEMPTED)
			loadLibrary();// throws on error

		if (libraryLoaded == LibraryLoadResult.SUCCESS)
		{
			if (threadNumberSupported)
			{
				setThreadNumber(threads);
			}
			result = extsolve(j_Ap,j_Ai,j_Ax,j_b,j_x);
		}
		else
			if (libraryLoaded == LibraryLoadResult.FAILURE)
				throw new IllegalArgumentException("earlier attempt to load a library failed");
		
		return result;
	}
/*
	private void reallocate()
	{
		if (j_Ap.length > currentMaxSize)
		{
			currentMaxSize = j_Ap.length;extmalloc(currentMaxSize, current_c);
		}
	}
*/
	/** Solves the system of equations using the external solver 
	 * if it is available, otherwise falls back on Colt.
	 * 
	 * @param threads the number of threads to use
	 * 
	 * Throws IllegalArgumentException of the matrix is singular.
	 */
	public void solve(int threads)
	{
		if (libraryLoaded == LibraryLoadResult.NOT_ATTEMPTED)
		try
		{
			loadLibrary();// throws on error
		}
		catch(IllegalArgumentException ex)
		{
			if (Boolean.valueOf(statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.LINEARWARNINGS)))
			{
				System.err.println("WARNING: failed to load the external solver library");
				ex.getCause().printStackTrace(System.err);
			}
		}
		
		if (libraryLoaded == LibraryLoadResult.SUCCESS)
		{
			solveExternally(threads);
		}
		else
			solveUsingColt();
	}

	/** Solves the system using Colt, to be used only as a fallback when external solver is
	 * not available. Justification: construction of matrices is optimized for the 
	 * external solver, hence we spend time building them in conformance to the specific
	 * format expected by the external solver and then we have to convert them 
	 * to Colt format, followed by running Colt itself. Hence all this is
	 * extremely slow.
	 */  
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
	//private static int currentMaxSize = -1;
	//private static int current_c = 1;
	
	/** Attempts to load the library from the specified path 
	 * implicitly (considering java.library.path variable)
	 * or explicitly, using the {@link G_PROPERTIES#PATH_NATIVELIB} property, if set.
	 */
	public static UnsatisfiedLinkError tryLoading(String name)
	{
		UnsatisfiedLinkError result = null;
		
		String nativePath = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_NATIVELIB);
		if (nativePath != null)
		{
			try
			{ System.load(nativePath+System.mapLibraryName(name)); }// load, not loadLibrary to use the absolute path
			catch(UnsatisfiedLinkError ex)
			{ result = ex; }
		}
		 		
		if (result == null)
		{
			try
			{ System.loadLibrary(name); }
			catch(UnsatisfiedLinkError ex)
			{ result = ex; }
		}
		
		return result;
	}
	
	public static void loadLibrary()
	{
		if (libraryLoaded == LibraryLoadResult.NOT_ATTEMPTED)
		{
			UnsatisfiedLinkError ex = null;
			ex = tryLoading("cygStatechumSolver-1");
			if (ex != null) ex = tryLoading("StatechumSolver");
			if (ex != null) ex = tryLoading("libStatechumSolver");

			if (ex != null)
			{// failed to load our library.
				libraryLoaded = LibraryLoadResult.FAILURE;
				IllegalArgumentException exc = new IllegalArgumentException("failed to load the solver library");
				exc.initCause(ex);throw exc;
			}

			setIRStep(1);
			libraryLoaded = LibraryLoadResult.SUCCESS;
		}
		
		if (libraryLoaded == LibraryLoadResult.SUCCESS)
		{
			try
			{
				setThreadNumber(ExperimentRunner.getCpuNumber());
				threadNumberSupported = true;// if we got here, the method is supported
			}
			catch(UnsatisfiedLinkError err)
			{// not supported				
			}
		}
	}
	
	/** Converts a Colt matrix into the form appropriate for UMFPACK solver. */
	public LSolver(DoubleMatrix2D matrix, DoubleMatrix1D b)
	{
		if (matrix.columns() != matrix.rows())
			throw new IllegalArgumentException("matrix should be square");
		if (matrix.rows() != b.size())
			throw new IllegalArgumentException("the dimension of b has to match that of the matrix");
		
		int size = matrix.columns();
		j_b = new double[size];b.toArray(j_b);
		j_x = new double[size];j_Ap=new int[size+1];
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

	/** Converts this matrix to Colt format, for a fall back on Colt when 
	 * external solver is not found as well as for testing. 
	 */
	public DoubleMatrix2D toDoubleMatrix2D()
	{
		DoubleMatrix2D result = DoubleFactory2D.sparse.make(j_Ap.length-1, j_Ap.length-1);
		for(int y=0;y<j_Ap.length-1;++y)
			for(int i=j_Ap[y];i<j_Ap[y+1];++i)
				result.setQuick(j_Ai[i],y, j_Ax[i]);

		return result;
	}

	/** Converts the "b" vector to Colt format, for a fall back on Colt when 
	 * external solver is not found as well as for testing. 
	 */
	public DoubleMatrix1D toDoubleMatrix1D()
	{
		DoubleMatrix1D result = DoubleFactory1D.dense.make(j_b);
		return result;
	}
}