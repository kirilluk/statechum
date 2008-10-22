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

/*
 * Building Win32 version of smt library: 
 * download "Yices for Windows" from http://yices.csl.sri.com/download.shtml
 * The file is called yices-1.0.16-i686-pc-mingw32.tar.gz and should contain a .dll.
 * Edit fullrebuild.sh to point it to the directory in which yices was unpacked.
 * 
 * From the smt directory, run
 * <pre>
 * sh fullrebuild.sh
 * </pre>
 * This should build the library and you are done.
 * 
 * On Linux, the process is same as for win32/cygwin (and fullrebuild.sh has a lot less work to do).
 */

package statechum.analysis.learning;

import statechum.analysis.learning.rpnicore.LSolver;
import statechum.analysis.learning.rpnicore.LSolver.LibraryLoadResult;

public class Smt {
	/** Yices' context, this field is actually a C pointer. */  
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
	protected void finalize()
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
