/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
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

 * Building Win32 version of smt library: 
 * download "Yices for Windows" from http://yices.csl.sri.com/download.shtml
 * The file is called yices-1.0.16-i686-pc-mingw32.tar.gz and should contain a .dll
 * (a slightly different name for 64-bit Windows). The path to the DLL (such as C:\\cygwin\\usr\\local\\soft\\yices-1.0.36\\lib), with single backslashes, should be included in Windows PATH.
 * Edit fullrebuild.sh to point it to the directory in which yices was unpacked.
 * 
 * From the smt directory, run
 * <pre>
 * sh fullrebuild.sh
 * </pre> 
 * If build fails to find Yices (failed to find a reasonable YICES distribution), try to build a binary yourself. If the outcome does not run 
 * ("error while loading shared libraries: ?: cannot open shared object file: No such file or directory"), Yices is not in the PATH.
 * 
 * On Linux, the process is conceptually same as for win32/cygwin (and fullrebuild.sh has a lot less work to do).
 * In practice this will only work if you've got libtool-2.2 and the like (not the default on Debian Lenny in Oct 2009).
 * For this reason, you may instead attempt the following:
 * <ul><li>Modify configure to include 
 * <pre>
 *  -Wl,-rpath,/usr/local/lib
 * </pre>
 * at the end of <em>YICES_LIBS</em> (rpath is a linker flag which should be at the end of gcc command line) and ensure that <em>LIBS="$YICES_LIBS $LIBS"</em>
 * is <em>LIBS="$LIBS $YICES_LIBS"</em>.
 * </li>
 * <li>
 * <pre> 
 * CC='gcc -I/usr/local/soft/jdk1.6.0_16/include/linux/' sh ./configure --with-yices=/usr/local/soft/yices-1.0.24
 * </pre>
 * </li>
 * 
 * <p/>
 * Building on MacOS 10.6.xx with macports,
 * install yices,
 * <pre>
 * install_name_tool -change /usr/local/lib/libgmp.10.dylib /opt/local/lib/libgmp.10.dylib /usr/local/soft/yices-1.0.40/bin/yices
 * install_name_tool -change /usr/local/lib/libgmp.10.dylib /opt/local/lib/libgmp.10.dylib /usr/local/soft/yices-1.0.40/lib/libyices.dylib
 * install_name_tool -id /usr/local/soft/yices-1.0.40/lib/libyices.dylib /usr/local/soft/yices-1.0.40/lib/libyices.dylib
 * </pre>
 * and then running 
 * <pre>
 * CFLAGS=-L/opt/local/lib F77=/opt/local/bin/gfortran-mp-4.8 CC=/opt/local/bin/gcc-mp-4.8 CXX=/opt/local/bin/g++-mp-4.8 ./configure --with-yices=/usr/local/soft/yices-1.0.40
 * </pre>
 */

package statechum.analysis.learning;

import statechum.analysis.learning.rpnicore.LSolver;
import statechum.analysis.learning.rpnicore.LSolver.LibraryLoadResult;

public class Smt {
	/** Yices' context, this field is actually a C pointer, accessed and set by the C code. */  
	private volatile long context =0;
	
	/** Whether library was loaded. */
	private static LibraryLoadResult libraryLoaded = LibraryLoadResult.NOT_ATTEMPTED;

	public static void loadLibrary()
	{
		if (libraryLoaded == LibraryLoadResult.NOT_ATTEMPTED)
		{
			UnsatisfiedLinkError ex = null;
			ex = LSolver.tryLoading("cygSMT_Yices-1");
			if (ex != null) ex = LSolver.tryLoading("SMT_Yices");
			if (ex != null) ex = LSolver.tryLoading("libSMT_Yices");

			if (ex != null)
			{// failed to load our library.
				libraryLoaded = LibraryLoadResult.FAILURE;
				IllegalArgumentException exc = new IllegalArgumentException("failed to load the Yices library");
				exc.initCause(ex);throw exc;
			}

			libraryLoaded = LibraryLoadResult.SUCCESS;
		}
	}
	
	/** Releases the current context. */
	private native void releaseContext();
	
	@Override
	public void finalize()
	{
		releaseContext();
	}
	
	/** Configures Yices.
	 * 
	 * @param verbosity how much information to dump. -1 means no output at all.
	 * @param typecheck whether to do any typechecking on an input.
	 */
	public static native void configureYices(int verbosity, boolean typecheck);

	/** Used for testing of a C method. */
	public static native boolean testGetField(Object obj); 
	
	/** Checks whether the current context is satisfiable. */
	public native boolean check();
	
	/** Pushes the current context on the stack of contexts. */
	public void pushContext()
	{
		loadData("(push)");
	}
	
	/** Pops the last context from the stack of contexts and makes this context the current one. */
	public void popContext()
	{
		loadData("(pop)");
	}
	
	/** Passes the supplied text to yices' parser.
	 * 
	 * @param text text to parse.
	 * @throws IllegalArgumentException if parsing fails. 
	 */
	public native void loadData(String text);
	
	/** The current version of Yices dumps "unsat" into standard output whenever it finds an unsatisfiable constraint.
	 * In order to avoid this, we can disable standard output; this is best done before doing a bunch of computations.
	 * It is interesting to note that Java's standard output is unaffected.
	 */
	public native static void closeStdOut(); 
	
	/** After standard output has been disabled, no output will appear; this one re-enables it back. */
	public native static void reopenStdOut(); 
	
}
