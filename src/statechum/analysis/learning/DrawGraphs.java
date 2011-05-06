/* Copyright (c) 2011 Neil Walkinshaw and Kirill Bogdanov
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

/*
 * Installation of RJava, JavaGD and aplpack on Debian x86_64:
 * apt-get install r-base
 * 
 * (note: an alternative way to install javagd is from http://blog.binfalse.de/2011/02/talking-r-through-java/
 *  which boils down to apt-get install r-cran-rjava 
 * Asking R to install packages itself will fail on a PAX-enabled system because binaries it generates does not have
 * mprotect disabled, to make this work you have to change every line in the two configure files of rJava which 
 * says 
 * ac_try='./conftest$ac_exeext' 
 * to read
 * ac_try='paxctl -Cm ./conftest$ac_exeext;./conftest$ac_exeext'
 * After that, configure will be successful. The outcome can be turned back into rJava.tar.gz and R will install it via
 * install.packages("/tmp/RtmpxtSS7S/downloaded_packages/rJava-modified.tar.gz")
 * As root:
 * 
 * R CMD javareconf
 * 
 * (it uses getsp.java also available in JavaGD distribution (getsp.java takes java.library.path and splits it into segments prefixed by -L ,
 * on Debian it is stored in /usr/share/R/share/java/getsp.class )
 * Note that R CMD config CC obtains C compiler for R, similar for other flags.
 * 
 * Then unpack JavaGD into /usr/local/src/JavaGD and then run
 * 
 * R CMD INSTALL JavaGD
 * 
 * from /usr/local/src, this builds and installs the package.
 * 
 * In a similar way, I can install rJava, but perhaps it is prudent to disable extra package loading via JRIBootstrap.java
 * 
 * During installation packages are placed in /usr/local/lib/R/site-library and I have to include /usr/local/lib/R/site-library/rJava/jri/ in java.library.path
 * In addition, R_HOME has to be set to /usr/lib/R (in Eclipse Run configuration).
 * 
 * In order to use bagplots, the following is needed,
 * R CMD INSTALL aplpack
 * 
 * Installation of the above on MacOSX and Win32:
 * 
 * install.packages(c("JavaGD","rJava","aplpack"))
 * On MacOS, tcltk is a special download which installs into /usr/local.
 */


// This file is based on rtest.java

package statechum.analysis.learning;
import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import javax.swing.SwingUtilities;

import org.rosuda.JRI.REXP;
import org.rosuda.JRI.RMainLoopCallbacks;
import org.rosuda.JRI.Rengine;

import statechum.Helper;

public class DrawGraphs {
	/** Determines whether our callbacks are dummies (without a main loop) or active (main loop running).
	 */
	static boolean mainLoopRunning = false;
	
	public static class ConsoleCallbacks implements RMainLoopCallbacks
	{

		@SuppressWarnings("unused")
		@Override
		public void rBusy(Rengine arg0, int arg1) {
			// dummy
		}

		@SuppressWarnings("unused")
		@Override
		public String rChooseFile(Rengine arg0, int arg1) {
			// dummy
			return null;
		}

		@SuppressWarnings("unused")
		@Override
		public void rFlushConsole(Rengine arg0) {
			// dummy
		}

		@SuppressWarnings("unused")
		@Override
		public void rLoadHistory(Rengine arg0, String arg1) {
			// dummy
		}

        private BufferedReader br=new BufferedReader(new InputStreamReader(System.in));

        /** Mostly from rtest.java */
        @SuppressWarnings("unused")
		@Override
		public String rReadConsole(Rengine arg0, String prompt, int arg2) {
        	if (!mainLoopRunning) return null;
        	
	       System.out.print(prompt);
	        try {
	            String s=br.readLine();
	            return (s==null||s.length()==0)?s:s+"\n";
	        } catch (Exception e) {
	            System.out.println("rReadConsole exception: "+e.getMessage());
	        }
	        return null;
		}

		@SuppressWarnings("unused")
		@Override
		public void rSaveHistory(Rengine arg0, String arg1) {
			// dummy
		}

		@SuppressWarnings("unused")
		@Override
		public void rShowMessage(Rengine arg0, String arg1) {
			if (mainLoopRunning) System.out.println(arg1);
		}

		@SuppressWarnings("unused")
		@Override
		public void rWriteConsole(Rengine arg0, String arg1, int arg2) {
			if (mainLoopRunning) System.out.println(arg1);
			else
			{
				consoleBuffer.append(arg1);consoleBuffer.append('\n');
			}
		}
		
		private StringBuffer consoleBuffer = new StringBuffer();
		
		public void clearBuffer()
		{
			consoleBuffer = new StringBuffer();
		}
		
		public String getBuffer()
		{
			return consoleBuffer.toString();
		}
	}
	
	/** R engine - can only be initalised once - second time packages do not load (setwd(x) cannot change directory)
	 * because F:/R/windows64/R-2.12.2/library is passed to setwd and even if the correct value is passed, package
	 * loading fails.
	 */
	protected static Rengine engine;
	protected static ConsoleCallbacks callbacks = new ConsoleCallbacks();
	
	public DrawGraphs()
	{
		if (!Rengine.versionCheck()) 
			throw new IllegalArgumentException("R version mismatch");
		if (engine == null)
		{
			engine = new Rengine(new String[]{"--vanilla"},false,callbacks);
			//engine.setDaemon(true);
			if (!engine.waitForR())
				throw new IllegalArgumentException("loading R failed");

			Runtime.getRuntime().addShutdownHook(new Thread() {
			    @Override
				public void run() { end();
			    }
			});
			eval("library(aplpack)","loading BagPlot");

		}
	}
	
	protected static REXP eval(String whatToEval, String errMsg)
	{
		callbacks.clearBuffer();
		REXP result = engine.eval(whatToEval);
		if (result == null)
			throw new IllegalArgumentException(errMsg+" : "+callbacks.getBuffer());
		return result;
	}
	
	
	protected static <ELEM> String vectorToR(List<ELEM> vector, boolean addQuotes)
	{
		if (vector.size() == 0) throw new IllegalArgumentException("cannot plot an empty graph");
		StringBuffer result = new StringBuffer();
		result.append("c(");
		boolean startVector = true;
		for(ELEM elem:vector)
		{
			if (!startVector) result.append(",");else startVector=false;
			if (addQuotes) 	result.append('\"');
			result.append(elem);
			if (addQuotes) 	result.append('\"');
		}
		result.append(")");
		return result.toString();
	}
	
	/**
	 * Formats a box plotting command to R.
	 * 
	 * @param data what to plot
	 * @param names names of boxes
	 * @param colour colour of boxes
	 * @param otherAttrs additional attributes to set, null if not used.
	 * @return The string to be sent to R for evaluation.
	 */
	protected static String boxPlotToString(List<List<Double>> data,List<String> names,String colour, String otherAttrs)
	{
		if (data.size() == 0) throw new IllegalArgumentException("cannot plot an empty graph");
		if (data.size() == 1 && names != null) throw new IllegalArgumentException("in a graph with one component, names are not used");
		if (data.size() > 1 && names != null && names.size() != data.size()) throw new IllegalArgumentException("mismatch between name and data length"); 
		StringBuffer result = new StringBuffer();
		result.append("boxplot(");
		boolean firstVectorOfData = true;
		for(List<Double> arg:data)
		{
			if (!firstVectorOfData) result.append(",");else firstVectorOfData=false;
			result.append(vectorToR(arg,false));
		}
		
		// names
		if (names != null)
		{
			result.append(",names=");
			result.append(vectorToR(names, true));
		}
		
		// colours
		{
			result.append(",col=c(");
			boolean startVector = true;
			for(int i=0;i<data.size();++i)
			{
				if (!startVector) result.append(",");else startVector=false;
				result.append('\"');
				result.append(colour);
				result.append('\"');
			}
			result.append(")");
		}
		if (otherAttrs != null) { result.append(',');result.append(otherAttrs); }
		result.append(")");
		return result.toString();
	}
	
	/**
	 * Formats a bag plotting command to R.
	 * 
	 * @param yData what to plot vertically
	 * @param xData what to plot horizontally
	 * @param otherAttrs additional attributes to set, null if not used.
	 * @return The string to be sent to R for evaluation.
	 */
	protected static String bagPlotToString(List<List<Double>> yData,List<Double> xData, String otherAttrs)
	{
		if (yData.size() == 0) throw new IllegalArgumentException("cannot plot an empty graph");
		if (yData.size() != xData.size()) throw new IllegalArgumentException("mismatch between x and y length"); 
		StringBuffer result = new StringBuffer();
		result.append("bagplot(");
		StringBuffer yAxisData = new StringBuffer();
		Iterator<Double> xArgIterator = xData.iterator();
		result.append("c(");yAxisData.append(",c(");
		boolean startVector = true;
		for(List<Double> yArg:yData)
		{
			Double xArg = xArgIterator.next();
			for(Double elem:yArg)
			{
				if (!startVector) { yAxisData.append(",");result.append(','); } else startVector=false;
				yAxisData.append(elem);result.append(xArg);
			}
		}
		result.append(')');yAxisData.append(')');
		result.append(yAxisData);
		
		if (otherAttrs != null) { result.append(',');result.append(otherAttrs); }
		result.append(")");
		return result.toString();
	}

	boolean javaGDLoaded = false;
	
	
	/** Since I cannot pass arguments to RViewer during it construction, static values have to be set and 
	 * then copied into the new object. For this reason, creation of graphs has to be serialised, this is done
	 * by running it all on the Swing thread (avoids a deadlock associated with resizing of the window
	 * and us doing something to it at the same time on different threads).
	 */
	public void drawInteractivePlot(final String dataToPlot,final String title)
	{
		// First, load javaGD
		if (!javaGDLoaded)
		{
			// based on http://www.algorithm-forge.com/techblog/2010/07/using-javagd-in-java-applications/
			engine.eval(".setenv <- if (exists(\"Sys.setenv\")) Sys.setenv else Sys.putenv");
			engine.eval(".setenv(\"JAVAGD_CLASS_NAME\"=\"statechum/analysis/learning/RViewer\")");
			eval("library(JavaGD)","loading JavaGD");
			javaGDLoaded = true;
		}
		
		try {
			SwingUtilities.invokeAndWait(new Runnable() {
				@Override
				public void run()
				{
					if (RViewer.getGraph(title) == null)
					{// create new graph
						RViewer.setNewGraphName(title);
						eval("JavaGD(\"aa\")","JavaGD() failed");
						REXP devNum =eval("dev.cur()","failed to do dev.cur"); 
						
						RViewer.getGraph(title).init(devNum.asInt()-1);
					}
					eval("dev.set("+(RViewer.getGraph(title).getDeviceNumber()+1)+")","failed to do dev.set for "+title);
					eval(dataToPlot,"failed to run plot "+dataToPlot);		
				}
			});
		} catch (Exception e) {
			Helper.throwUnchecked("could not draw graph "+title, e);
		}
	}
	

	/** Draws a plot given the data to plot and stores it in the given file.
	 * 
	 * @param drawingCommand drawing command to pass to R
	 * @param xDim horizontal size in inches, R default is 7.
	 * @param yDim vertical size in inches, R default is 7.
	 * @param fileName where to store result.
	 */
	public void drawPlot(String drawingCommand,double xDim,double yDim,File file)
	{
		if (xDim < 1) 
			throw new IllegalArgumentException("horizontal size ("+xDim+") too small");
		if (yDim < 1) throw new IllegalArgumentException("vertical size ("+yDim+") too small");
		
		if (file.exists() && file.delete() == false)
			throw new IllegalArgumentException("cannot delete file "+file.getAbsolutePath());
		// Slashes have to be the Unix-way - R simply terminates the DLL on WinXP otherwise.
		String fullName = file.getAbsolutePath().replace(File.separatorChar, '/');
		eval("pdf(\""+fullName+"\","+xDim+","+yDim+")","redirection to pdf("+file.getAbsolutePath()+") failed");
		eval(drawingCommand,"failed to run boxplot");
		eval("dev.off()","failed to write to "+file.getAbsolutePath());
	}

	/**
	 * Shuts down jri thread - since jri is not a daemon thread JVM will not exit if this is not called.
	 */
	public static void end()
	{
		if (engine != null) 
		{
			for(RViewer v:RViewer.nameToGraph.values())	v.gdClose();
			engine.end();
			engine=null;	
		}		
	}
	
	/**
	 * Represents a graph.
	 * 
	 * @param <ELEM> type of elements for the X axis, veritical is always a Double
	 */
	public static abstract class RGraph<ELEM>
	{
		Map<ELEM,List<Double>> collectionOfResults = new TreeMap<ELEM,List<Double>>();
		
		protected final String xAxis,yAxis;
		protected final File file;
		
		public RGraph(String x,String y,File name)
		{
			xAxis=x;yAxis=y;file=name;
		}
		
		public void add(ELEM el,Double value)
		{
			List<Double> list = collectionOfResults.get(el);
			if (list == null) { list=new LinkedList<Double>();collectionOfResults.put(el,list); }
			list.add(value);
		}
		
		/** Returns a command to draw a graph in R. */
		abstract String getDrawingCommand();
		
		public void drawInteractive(DrawGraphs gr)
		{
			gr.drawInteractivePlot(getDrawingCommand(), file.getName());
		}
		
		protected double xSize = -1;
		
		/** Sets the horizonal size of the plot, vertical size is always set to 4 inches. 
		 * If no assignemnt is made, the size is guessed from the number of points on the drawing. 
		 */ 
		public void setXSize(double newSize)
		{
			xSize = newSize;
		}
		
		double ySize = 4;

		public void drawPdf(DrawGraphs gr)
		{
			String drawingCommand = getDrawingCommand();
			assert collectionOfResults.size() > 0;
			double horizSize = xSize;
			if (horizSize <= 0) horizSize=computeHorizSize();
			gr.drawPlot(drawingCommand, horizSize,ySize,file);
		}
		
		/* Computes the horizontal size of the drawing. */
		abstract protected double computeHorizSize();
	}
	
	public static class RBoxPlot<ELEM> extends RGraph<ELEM>
	{
		public RBoxPlot(String x, String y, File name) {
			super(x, y, name);
		}
		
		@Override
		public String getDrawingCommand()
		{
			List<List<Double>> data = new LinkedList<List<Double>>();
			List<String> names = new LinkedList<String>();
			for(Entry<ELEM,List<Double>> entry:collectionOfResults.entrySet())
			{
				data.add(entry.getValue());names.add(entry.getKey().toString());
			}
			return boxPlotToString(data, names.size()==1?null:names,"green","xlab=\""+xAxis+"\",ylab=\""+yAxis+"\"");
		}

		@Override
		protected double computeHorizSize() {
			double horizSize=ySize*collectionOfResults.keySet().size()/5;if (horizSize < ySize) horizSize = ySize;
			return horizSize;
		}
	}
	
	public static class RBagPlot extends RGraph<Double>
	{
		public RBagPlot(String x, String y, File name) {
			super(x, y, name);
		}
		
		@Override
		public String getDrawingCommand()
		{
			List<List<Double>> data = new LinkedList<List<Double>>();
			List<Double> names = new LinkedList<Double>();
			for(Entry<Double,List<Double>> entry:collectionOfResults.entrySet())
			{
				data.add(entry.getValue());names.add(entry.getKey());
			}
			
			return bagPlotToString(data, names,"xlab=\""+xAxis+"\",ylab=\""+yAxis+"\"");
		}

		@Override
		protected double computeHorizSize() {
			return ySize;
		}
	}
}

