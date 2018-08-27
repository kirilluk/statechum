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
 * 
 * Where R is install from Macports, I need to installed both as follows:
 * <pre>
 * port install tk +x11
 * port install R +gfortran48 +x11 +tcltk
 * </pre>
 * Installing a pre-built binary from R web site does not work because it is linked to Quartz that appears to be missing, most likely because they have used Lion or something similar).
 * 
 * R_HOME needs to be set to /opt/local/Library/Frameworks/R.framework/Resources
 * and /opt/local/Library/Frameworks/R.framework/Versions/3.0/Resources/library/rJava/jri needs adding to java.library.path
 * 
 * Specific configuration variable affecting Win64 and R-3.4 (not the case for R-3.0)
 * R_LIBS_USER="C:/Users/Kirill/Documents/R/win-library/3.4"
 */


// This file is based on rtest.java

package statechum.analysis.learning;
import java.awt.geom.Rectangle2D;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.swing.SwingUtilities;

import org.rosuda.JRI.REXP;
import org.rosuda.JRI.RMainLoopCallbacks;
import org.rosuda.JRI.Rengine;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper;
import statechum.StatechumXML.StringSequenceWriter;
import statechum.analysis.learning.experiments.SGE_ExperimentRunner;
import statechum.analysis.learning.experiments.PairSelection.LearningSupportRoutines;
import statechum.analysis.learning.experiments.PairSelection.PairQualityLearner.ThreadResultID;

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
	
	/** R engine - can only be initalised once - second time packages do not load ("setwd(x) cannot change directory")
	 * because F:/R/windows64/R-2.12.2/library is passed to setwd and even if the correct value is passed, package
	 * loading fails.
	 */
	protected static Rengine engine;
	protected static ConsoleCallbacks callbacks = new ConsoleCallbacks();
	
	public DrawGraphs()
	{
		java.lang.reflect.Method loadJri = null;
			
		try
		{
			loadJri = Rengine.class.getMethod("loadJri", new Class[]{String.class});
		} catch (Exception e)
		{// ignore this, loadJri remains null.
		}
		
		try
		{
			if (loadJri != null && !((Boolean)loadJri.invoke(null, GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_JRILIB))).booleanValue())
				throw new IllegalArgumentException("JRI library could not be loaded");
		} catch (IllegalAccessException e)
		{
			throw new IllegalArgumentException("Calling a method to load JRI library failed",e);
		} catch (InvocationTargetException e)
		{
			throw new IllegalArgumentException("Calling a method to load JRI library failed",e);
		} // IllegalArgumentException thrown by the above should propagate

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
				public void run() { 
			    	end();
			    }
			});
			eval("library(aplpack)","loading BagPlot");

			// detect if compression is supported
			pdfCompression = eval("is.element(\"compress\",names(formals(pdf)))","failed to detect whether compression is supported").asBool().isTRUE();
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
	
	public static final String defaultColour = "green";
	
	/**
	 * Formats a box plotting command to R.
	 * 
	 * @param data what to plot
	 * @param names names of boxes
	 * @param colour colour of boxes
	 * @param otherAttrs additional attributes to set, null if not used.
	 * @return The string to be sent to R for evaluation.
	 */
	protected static String boxPlotToString(List<List<Double>> data,List<String> names,List<String> colour, String otherAttrs)
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
			result.append(",col=");
			if (colour != null) result.append(vectorToR(colour,true));
			else
			{// no colours provided, using the default one
				boolean startVector = true;
				result.append("c(");
				for(int i=0;i<data.size();++i)
				{
					if (!startVector) result.append(",");else startVector=false;
					result.append('\"');
					String col = defaultColour;
					result.append(col);
					result.append('\"');
				}
				result.append(")");
			}
		}
		if (otherAttrs != null && otherAttrs.length() > 0) { result.append(',');result.append(otherAttrs); }
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
	protected static String datasetToString(String plotType,List<List<Double>> yData,List<Double> xData, String otherAttrs)
	{
		if (yData.size() == 0) throw new IllegalArgumentException("cannot plot an empty graph");
		if (yData.size() != xData.size()) throw new IllegalArgumentException("mismatch between x and y length"); 
		StringBuffer result = new StringBuffer();
		result.append(plotType);result.append("(");
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

		if (otherAttrs != null && !otherAttrs.isEmpty()) { result.append(',');result.append(otherAttrs); }
		result.append(")");
		return result.toString();
	}
	
	private static boolean javaGDLoaded = false;
	
	/** Newer versions of R permit pdf compression, but older ones choke if I attempt to use it. 
	 */
	private static boolean pdfCompression = false;
	
	/** Since I cannot pass arguments to RViewer during it construction, static values have to be set and 
	 * then copied into the new object. For this reason, creation of graphs has to be serialised, this is done
	 * by running it all on the Swing thread (avoids a deadlock associated with resizing of the window
	 * and us doing something to it at the same time on different threads).
	 */
	public void drawInteractivePlot(final List<String> dataToPlot,final String title)
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
						REXP devNum = eval("dev.cur()","failed to do dev.cur");
						
						RViewer.getGraph(title).init(devNum.asInt()-1);
					}
					eval("dev.set("+(RViewer.getGraph(title).getDeviceNumber()+1)+")","failed to do dev.set for "+title);
					for(String cmd:dataToPlot)
						eval(cmd,"failed to run plot "+cmd);
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
	public void drawPlot(List<String> drawingCommand,double xDim,double yDim,File file)
	{
		if (xDim < 1) 
			throw new IllegalArgumentException("horizontal size ("+xDim+") too small");
		if (yDim < 1) throw new IllegalArgumentException("vertical size ("+yDim+") too small");
		
		if (file.exists() && file.delete() == false)
			throw new IllegalArgumentException("cannot delete file "+file.getAbsolutePath());
		// Slashes have to be the Unix-way - R simply terminates the DLL on WinXP otherwise.
		String fullName = file.getAbsolutePath().replace(File.separatorChar, '/');
		eval("pdf(\""+fullName+"\","+xDim+","+yDim
				+(pdfCompression?",compress=FALSE":"") // disable pdf compression if enabled
				+")","redirection to pdf("+file.getAbsolutePath()+") failed");
		for(String cmd:drawingCommand)
			eval(cmd,"failed to run "+cmd);
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

	public static class DataColumn
	{
		/** Data to be displayed. */
		final List<Double> results;
		/** Colour to use, if {@code null} default colour is used. */
		String colour;
		/** Label to be used, if {@code null} column identifier is used. */
		String label;
		
		public DataColumn()
		{
			results = new ArrayList<Double>(1000); 
		}
	}
	
	public interface SGEExperimentResult
	{
		/** When experiment completes, the results are written into a file as text. We need to load it into the experiment result file in order to collate across experiments for the final output.
		 * The last argument is set to true if the only purpose is to check that the outcome can be parsed rather than to record result. This is important to check whether an experiment has
		 * abnormally terminated. 
		 */
		void parseTextLoadedFromExperimentResult(String []text, String fileNameForErrorMessages, ThreadResultID parameters, boolean onlyCheckItParses);
		
		/** Called to provide real-time updates to the learning results. The default does nothing. */
		public void drawInteractive(DrawGraphs gr);
		
		/** Records results in a file. The argument is used if R is needed. */
		public abstract void reportResults(DrawGraphs gr);
				
		/** Reports the name of the file with the graph, used for identification of different graphs. */
		public String getFileName();
	}

	public static class CSVExperimentResult implements SGEExperimentResult
	{
		protected final Map<String,Map<String,String>> rowColumnText = new TreeMap<String,Map<String,String>>();
		final Map<String,String []> columnIDToHeader = new TreeMap<String,String []>();// will return column values in an increasing order
		int headerRows = -1;
		final Map<String,String []> columnIDToCellHeader = new TreeMap<String,String []>();
		protected final File file;
		
		protected String missingValue = "";
		
		public void setMissingValue(String val)
		{
			missingValue = val;
		}
		
		public CSVExperimentResult(File arg)
		{
			file = arg;
		}
		
	 	public static void addSeparator(StringBuffer buf)
	 	{
	 		buf.append(',');
	 	}
	 	
	 	public static void addNewLine(StringBuffer buf)
	 	{
	 		buf.append('\n');
	 	}

	 	public static String concatenateWithSeparator(String [] text)
	 	{
	 		StringBuffer outcome = new StringBuffer();
	 		boolean firstEntry = true;
	 		for(String str:text)
	 		{
	 			if (firstEntry)
	 				firstEntry = false;
	 			else
	 				outcome.append(',');
	 			outcome.append(LearningSupportRoutines.removeSpaces(str));
	 		}
	 		return outcome.toString();
	 	}
	 	
	 	/** Adds text to the spreadsheet. */
		public void add(ThreadResultID id, String text)
		{
			if (id.getRowID() == null || LearningSupportRoutines.removeSpaces(id.getRowID()).isEmpty())
				throw new IllegalArgumentException("cannot add a cell without row id to spreadsheet "+getFileName());
			if (id.getColumnID() == null || LearningSupportRoutines.removeSpaces(id.getColumnID()).isEmpty())
				throw new IllegalArgumentException("cannot add a cell without column id to spreadsheet "+getFileName());
			if (id.getColumnText() == null || id.getColumnText().length == 0)
				throw new IllegalArgumentException("spreadsheet "+getFileName()+" contains cell "+id.getRowID()+","+id.getColumnID()+" with an invalid column header");
			if (id.headerValuesForEachCell() == null || id.headerValuesForEachCell().length == 0)
				throw new IllegalArgumentException("spreadsheet "+getFileName()+" contains cell "+id.getRowID()+","+id.getColumnID()+" with an invalid header values for cell");

			Map<String,String> columnText = rowColumnText.get(id.getRowID());
			if (columnText == null)
			{
				columnText = new TreeMap<String,String>();rowColumnText.put(id.getRowID(), columnText);
			}
			if (columnText.containsKey(id.getColumnID()))
				throw new IllegalArgumentException("spreadsheet "+getFileName()+" already contains cell "+id.getRowID()+","+id.getColumnID());
			String [] elements = text.split(",");
			if (elements.length != id.headerValuesForEachCell().length)
				throw new IllegalArgumentException("the number of values ("+elements.length+") passed via \""+Arrays.asList(elements)+"\" does not match those ("+id.headerValuesForEachCell().length+") in id.headerValuesForEachCell()=\""+Arrays.asList(id.headerValuesForEachCell())+"\"");
			String reducedLine = concatenateWithSeparator(elements);
			if (reducedLine.isEmpty())
				throw new IllegalArgumentException("empty line added at "+id.getRowID()+","+id.getColumnID()+" to spreadsheet "+getFileName());
			columnText.put(id.getColumnID(), reducedLine); 

			if (!columnIDToHeader.containsKey(id.getColumnID()))
			{
				if (headerRows > 0 && headerRows != id.getColumnText().length)
					throw new IllegalArgumentException("spreadsheet "+getFileName()+" contains cell "+id.getRowID()+","+id.getColumnID()+" with an invalid number of rows in column header, expected "+headerRows+", got "+id.getColumnText().length);
				headerRows = id.getColumnText().length;
				columnIDToHeader.put(id.getColumnID(), id.getColumnText());
				columnIDToCellHeader.put(id.getColumnID(), id.headerValuesForEachCell());
			}
			else
			{
				List<String> oldColumnHeaders = Arrays.asList(columnIDToHeader.get(id.getColumnID())), currColumnHeaders = Arrays.asList(id.getColumnText());
				List<String> oldCellHeaders = Arrays.asList(columnIDToCellHeader.get(id.getColumnID())), currCellHeaders = Arrays.asList(id.headerValuesForEachCell());
				if (!oldColumnHeaders.equals(currColumnHeaders))
					throw new IllegalArgumentException("different values of column headers between previous ("+oldColumnHeaders+")and current ("+currColumnHeaders+") values of column ID "+id.getColumnID());
				if (!oldCellHeaders.equals(currCellHeaders))
					throw new IllegalArgumentException("different values of cell headers between previous ("+oldCellHeaders+")and current ("+currCellHeaders+") values of column ID "+id.getColumnID());
			}
		}

		/** Called to provide real-time updates to the learning results. The default does nothing. */
		@Override
		public void drawInteractive(@SuppressWarnings("unused") DrawGraphs gr)
		{
		}

		public void writeFile(Writer wr) throws IOException
		{
			if (rowColumnText.isEmpty())
				return;// an empty file is better than a file with a part of a header and no data. 
			
			// construct the header
			Set<String> columnHeaders = columnIDToHeader.keySet();
			
			for(int headerRow=0;headerRow<headerRows;++headerRow)
			{
				for(String hdr:columnHeaders)
					for(int cnt=0;cnt<columnIDToCellHeader.get(hdr).length;++cnt)
					{
						wr.append(',');wr.append(LearningSupportRoutines.removeSpaces(columnIDToHeader.get(hdr)[headerRow]));
					}
				wr.append('\n');
			}
			
			wr.append("experiment");
			for(String hdr:columnHeaders)
			{
				String []cellHeaders = columnIDToCellHeader.get(hdr);
				for(String cellHeader:cellHeaders)
				{
					wr.append(',');wr.append(LearningSupportRoutines.removeSpaces(cellHeader));
				}
			}
			wr.append('\n');
			
			
			// finished with the header, output data
			for(Entry<String,Map<String,String>> rowEntry:rowColumnText.entrySet())
			{
				wr.append(rowEntry.getKey());
				
				for(Entry<String, String[]> columnAndCellHeaders:columnIDToCellHeader.entrySet())
				{
					wr.append(',');
					String value = rowEntry.getValue().get(columnAndCellHeaders.getKey());
					if (value == null)
					{
						wr.append(missingValue);// empty value indicates a missing value
						for(int i=0;i<columnAndCellHeaders.getValue().length-1;++i)
						{
							wr.append(',');
							wr.append(missingValue);// empty value indicates a missing value
						}
					}
					else
						wr.append(value);
				}
				wr.append('\n');
			}
		}
		
		/** This is the only part of the CSV table class that is permitted to perform transformations of data depending on the host that has run the experiment, in order to 
		 * perform the cpu frequency correction. COLLECT_TASKS could be run on a completely different host and different experiments easily run on different hosts as well. 
		 *  
		 * @param outputWriter where to write data. 
		 * @param id parameters of the experiment.
		 * @param text what to write.
		 * @throws IOException if something goes wrong.
		 */
		public void writeTaskOutput(Writer outputWriter, ThreadResultID id, String text) throws IOException
		{
			outputWriter.write(getFileName());outputWriter.write(SGE_ExperimentRunner.separator);
			StringSequenceWriter writer = new StringSequenceWriter(null);
			StringBuffer w = new StringBuffer();
			w.append(id.getSubExperimentName());w.append(SGE_ExperimentRunner.separator);w.append(id.getRowID());w.append(SGE_ExperimentRunner.separator);w.append(id.getColumnID());w.append(SGE_ExperimentRunner.separator);
			writer.writeInputSequence(w, Arrays.asList(id.getColumnText()));w.append(SGE_ExperimentRunner.separator);
			writer.writeInputSequence(w, Arrays.asList(id.headerValuesForEachCell()));w.append(SGE_ExperimentRunner.separator);
			
			String [] elements = text.split(",");
			if (elements.length != id.headerValuesForEachCell().length)
				throw new IllegalArgumentException("the number of values ("+elements.length+") passed via \""+Arrays.asList(elements)+"\" does not match those ("+id.headerValuesForEachCell().length+") in id.headerValuesForEachCell()=\""+Arrays.asList(id.headerValuesForEachCell())+"\"");
			if (id.executionTimeInCell() >= 0)
			{
				elements[id.executionTimeInCell()]=Integer.toString((int)Math.round(Integer.parseInt(elements[id.executionTimeInCell()])/LearningSupportRoutines.getFreqCorrectionValue()));// a rather long-winded way to scale execution time.
			}
			w.append(concatenateWithSeparator(elements));
			outputWriter.write(w.toString());
			outputWriter.write("\n");
		}
		
		@Override
		public void reportResults(@SuppressWarnings("unused") DrawGraphs gr)
		{
			FileWriter wr = null;
			
			try {
				wr = new FileWriter(file);
				writeFile(wr);
			} catch (IOException e) {
				Helper.throwUnchecked("failed to write file "+file.getAbsolutePath(), e);
			}
			finally
			{
				if (wr != null)
					try {
						wr.close();
					} catch (IOException e) {
						// ignored
					}
			}
		}

		/** Reports the name of the file with the graph, used for identification of different graphs. */
		@Override
		public String getFileName()
		{
			return file.getName();
		}

		public String getAbsoluteFileName()
		{
			return file.getAbsolutePath();
		}


		/** When experiment completes, the results are written into a file as text. We need to load it into the experiment result file in order to collate across experiments for the final output. */
		@Override
		public void parseTextLoadedFromExperimentResult(final String[] line, String fileNameForErrorMessages, final ThreadResultID parameters, boolean onlyCheckItParses)
		{
			final StringSequenceWriter reader = new StringSequenceWriter(null);
			if (line.length != 7)
				throw new IllegalArgumentException("experiment "+fileNameForErrorMessages+" has recorded invalid number of values ("+line.length+")for CSV output, it should record 7");
			//List<String> colText = new ArrayList<String>();colText.add(line[3].split("\\-")[0]);colText.addAll(reader.readInputSequence(line[4]));
			//final String[] columnText =  colText.toArray(new String[]{});
			final String[] columnText = reader.readInputSequence(line[4]).toArray(new String[]{});
			final String[] headerValuesForCells = reader.readInputSequence(line[5]).toArray(new String[]{});
			
			if (!onlyCheckItParses)
				add(new ThreadResultID(){
	
					@Override
					public String getRowID() {
						return line[2];
					}
	
					@Override
					public String[] getColumnText() {
						return columnText;
					}
	
					@Override
					public String getColumnID() {
						return line[3];
					}
	
					@Override
					public String[] headerValuesForEachCell() {
						return headerValuesForCells;
					}

					@Override
					public String getSubExperimentName() {
						return line[1];
					}

					@Override
					public int executionTimeInCell() {
						return parameters.executionTimeInCell();
					}
				},line[6]);
		}
	}

	public static int charToHex(int b)
	{
		if (b >=0 && b<10)
			return '0'+b;
		else
			if (b>=10 && b < 0x10)
				return 'A'+b-10;
			else
				throw new IllegalArgumentException("invalid byte, should be 0..0x10");
	}
	
	public static String objectAsText(Object obj)
	{
		ByteArrayOutputStream buffer = new ByteArrayOutputStream();
		try
		{
			ObjectOutputStream oo = new ObjectOutputStream(buffer); 
	        oo.writeObject(obj);
		}
		catch(Exception ex)
		{
			Helper.throwUnchecked("failed to serialise object", ex);
		}
        StringBuffer out = new StringBuffer();
        for(byte b:buffer.toByteArray())
        {// Integer.toHexString is not useful here because it is aimed at pretty-printing, that is, byte 0 will be '0' rather that '00'.
        	out.append((char)(charToHex( (b & 0xf0) >> 4)));
        	out.append((char)(charToHex(b & 0xf)));
        }
        return out.toString();
	}
	
	public static int parseChar(char ch)
	{
		if (ch >= '0' && ch <= '9')
			return ch-'0';
		else
			if (ch >= 'A' && ch <= 'F')
				return ch-'A'+0x0a;
			else
				if (ch >= 'a' && ch <= 'f')
					return ch-'a'+0x0a;
				else
					throw new IllegalArgumentException("invalid char, should be between 0..9 or A..F");
	}
	
	public static Object parseObject(String str)
	{
		List<Byte> buffer = new ArrayList<Byte>();
		int curByte=0;
		if (str.length() % 2 != 0)
			throw new IllegalArgumentException("the length of hex representation should be even");
		for(int i=0;i<str.length();++i)
		{
			char ch = str.charAt(i);
			if (i %2 == 0)
			{// even byte, record it
				curByte = parseChar(ch);
			}
			else
			{
				buffer.add((byte)(parseChar(ch)+(curByte << 4)));curByte=0;
			}
		}
		Object outcome = null;
		try
		{
			byte []buf =new byte[buffer.size()];for(int i=0;i<buffer.size();++i) buf[i]=buffer.get(i); 
			ObjectInputStream objectInputStream = new ObjectInputStream(new ByteArrayInputStream(buf)); 
			outcome = objectInputStream.readObject();
		}
		catch(Exception ex)
		{
			Helper.throwUnchecked("failed to deserialise object", ex);
		}
		return outcome;
	}
	
	public static String obtainValueFromCell(String columnValue, int cellNumber)
	{
		String [] elements = columnValue.split(",");
		if (elements.length <= cellNumber || cellNumber < 0)
			throw new IllegalArgumentException("invalid cell number "+cellNumber+", should be 0.."+elements.length);
		return elements[cellNumber];
	}
	
	/** Constructs a graph from a spreadsheet, using the supplied columns as data for the graph.
	 * 
	 * @param plot R graph to update
	 * @param whereFrom spreadsheet to get data from
	 * @param columnX column for the horizontal values.
	 * @param columnY column for the vertical values.
	 * @param colour the colour to use. Calling this method multiple times permits construction of coloured graphs.
	 * @param label label to add with each value.
	 */
	public static void spreadsheetToStringGraph(RBoxPlot<String> plot, CSVExperimentResult whereFrom, String columnX, int cellWithinX, String columnY, int cellWithinY, String colour, String label)
	{
		for(Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			String X = rowEntry.getValue().get(columnX);
			String Y = rowEntry.getValue().get(columnY);
			if (X != null && Y != null)
				plot.add(obtainValueFromCell(X,cellWithinX), Double.parseDouble(obtainValueFromCell(Y,cellWithinY)), colour, label);
		}
	}
	
	public static String getValueFromMapGivenRegexp(Map<String,String> map, String regexp)
	{
		for(Entry<String,String> entry:map.entrySet())
			if (entry.getKey().matches(regexp))
				return entry.getValue();
		return null;
	}
	
	/** Constructs a graph from a spreadsheet, using the supplied columns as data for the graph.
	 * 
	 * @param plot R graph to update
	 * @param whereFrom spreadsheet to get data from
	 * @param columnX column for the horizontal values.
	 * @param columnY column for the vertical values.
	 * @param colour the colour to use. Calling this method multiple times permits construction of coloured graphs.
	 * @param label label to add with each value.
	 */
	public static void spreadsheetToDoubleGraph(RBoxPlot<Double> plot, CSVExperimentResult whereFrom, String columnX, int cellWithinX, String columnY, int cellWithinY, String colour, String label)
	{
		for(Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			String X = getValueFromMapGivenRegexp(rowEntry.getValue(),columnX);
			String Y = getValueFromMapGivenRegexp(rowEntry.getValue(),columnY);
			if (X != null && Y != null)
				plot.add(Double.parseDouble(obtainValueFromCell(X,cellWithinX)), Double.parseDouble(obtainValueFromCell(Y,cellWithinY)), colour, label);
		}
	}
	
	public static void spreadsheetAsDouble(AggregateValues agg,CSVExperimentResult whereFrom, String columnX, int cellWithinX, String columnY, int cellWithinY)
	{
		for(Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			String X = getValueFromMapGivenRegexp(rowEntry.getValue(),columnX);
			String Y = getValueFromMapGivenRegexp(rowEntry.getValue(),columnY);
			if (X != null && Y != null)
				agg.merge(Double.parseDouble(obtainValueFromCell(X,cellWithinX)), Double.parseDouble(obtainValueFromCell(Y,cellWithinY)));
		}
	}

	public static void spreadsheetAsDouble(RStatisticalAnalysis analysis,CSVExperimentResult whereFrom, String columnX, int cellWithinX, String columnY, int cellWithinY)
	{
		for(Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			String X = getValueFromMapGivenRegexp(rowEntry.getValue(),columnX);
			String Y = getValueFromMapGivenRegexp(rowEntry.getValue(),columnY);
			if (X != null && Y != null)
				analysis.add(Double.parseDouble(obtainValueFromCell(X,cellWithinX)), Double.parseDouble(obtainValueFromCell(Y,cellWithinY)));
		}
	}

	public static void spreadsheetAsString(AggregateStringValues agg,CSVExperimentResult whereFrom, String columnX, int cellWithinX, String columnY, int cellWithinY)
	{
		for(Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			String X = getValueFromMapGivenRegexp(rowEntry.getValue(),columnX);
			String Y = getValueFromMapGivenRegexp(rowEntry.getValue(),columnY);
			agg.merge(X == null?null:obtainValueFromCell(X,cellWithinX),Y == null?null:obtainValueFromCell(Y,cellWithinY));
		}
	}

	public static class TimeAndCorrection
	{
		public final double average,stdev;
		public final int count;
		
		public TimeAndCorrection(double av, double st, int cnt)
		{
			average = av;stdev = st;count = cnt;
		}
	}
	
	/** Matches cell values between graphs and computes the correction for the timing values. 
	 * All values other than time should match exactly, time values could differ. 
	 * All pairs where at least one experiment timed out are ignored. 
	 * Negative timeout means do not use timeouts. 
	 */
	public static TimeAndCorrection computeTimeAndCorrection(CSVExperimentResult reference, CSVExperimentResult other, ThreadResultID par, long timeout, long minValue, double scaleOtherValuesBy)
	{
		int timeCell = par.executionTimeInCell();
		if (timeCell < 0)
			throw new IllegalArgumentException("cannot match time where no time is present");
		int cellsCnt = par.headerValuesForEachCell().length;
		if (timeCell >= cellsCnt)
			throw new IllegalArgumentException("time cell value is too high, should be 0.."+(cellsCnt-1));
		double sum=0,sumOfSquares = 0;
		int count=0;
		
		for(Entry<String,Map<String,String>> rowEntry:reference.rowColumnText.entrySet())
		{
			Map<String,String> otherValue = other.rowColumnText.get(rowEntry.getKey());
			if (otherValue != null)
			{
				for(Entry<String,String> pair:rowEntry.getValue().entrySet())
				{
					String text = pair.getValue();
					String otherText = otherValue.get(pair.getKey());
					if (otherText != null)
					{
						double denominator = Double.parseDouble(obtainValueFromCell(text,timeCell));
						double value = Double.parseDouble(obtainValueFromCell(otherText,timeCell))*scaleOtherValuesBy;
						if (timeout < 0 || (denominator < timeout && value < timeout))
						{// only consider pairs where none of the values is a timeout
							for(int i=0;i<cellsCnt;++i)
								if (i != timeCell)
									if (!obtainValueFromCell(text,i).equals(obtainValueFromCell(otherText,i)))
										throw new IllegalArgumentException("Cell ["+rowEntry.getKey()+","+pair.getKey()+"] is different between spreadsheets, \""+obtainValueFromCell(text,i)+"\" != \""+obtainValueFromCell(otherText,i)+"\"");
							if (denominator > minValue && value > minValue) // for really small values, we are not likely to obtain accurate values.
							{
								double ratio = value/denominator;
								++count;sum+=ratio;sumOfSquares+=ratio*ratio;
							}
						}
					}
				}
			}
		}
		
		if (count > 0)
		{
			double average = sum/count, averageSquare = sumOfSquares/count;
			double stdev = Math.sqrt(averageSquare-average*average);
			
			return new TimeAndCorrection(average,stdev,count);
		}
		
		return new TimeAndCorrection(0,0,0);// no values, hence return the default.
	}

	/** Constructs a graph from a spreadsheet, using the supplied columns as data for the graph.
	 * 
	 * @param plot R graph to update
	 * @param whereFrom spreadsheet to get data from
	 * @param columnX column for the horizontal values.
	 * @param columnY column for the vertical values.
	 * @param colour the colour to use. Calling this method multiple times permits construction of coloured graphs.
	 * @param label label to add with each value.
	 */
	public static void spreadsheetToBagPlot(RBagPlot plot, CSVExperimentResult whereFrom, String columnX, int cellWithinX, String columnY, int cellWithinY, String colour, String label)
	{
		for(Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			String X = getValueFromMapGivenRegexp(rowEntry.getValue(),columnX);
			String Y = getValueFromMapGivenRegexp(rowEntry.getValue(),columnY);
			if (X != null && Y != null)
				plot.add(Double.parseDouble(obtainValueFromCell(X,cellWithinX)), Double.parseDouble(obtainValueFromCell(Y,cellWithinY)), colour, label);
		}
	}
	
	
	public interface MergeObjects
	{
		/** Called to merge the provided object into that represented by a provider of this interface. */
		public void merge(String key, Object obj);
	}
	
	public interface AggregateValues
	{
		/** Called to handle values in a spreadsheet. */
		public void merge(double A, double B);
	}
	
	public interface AggregateStringValues
	{
		/** Called to handle values in a spreadsheet. */
		public void merge(String A, String B);
	}
	
	/** Makes it possible to take serialised data from a column of a spreadsheet and merge it into an aggregate. 
	 * Imaging a map from pair score to true/false counters. By constructing it for each 
	 * learner and dumping into spreadsheet cells via {@link DrawGraphs#objectAsText} and 
	 * when all experiments are complete, load them back and combine into a single map, subsequently constructing
	 * a graph.
	 *  
	 * @param m called for each deserialised object
	 * @param whereFrom spreadsheet to process
	 * @param columnX column for the <em>X</em> value, unused if columnX is null.
	 * @param columnY values to deserialise, will throw IllegalArgumentException if this fails.
	 */
	public static void spreadsheetObjectsCollect(MergeObjects m, CSVExperimentResult whereFrom, String columnX, int cellWithinX, String columnY, int cellWithinY)
	{
		for(Entry<String,Map<String,String>> rowEntry:whereFrom.rowColumnText.entrySet())
		{
			String X = columnX == null?null:rowEntry.getValue().get(columnX);
			String Y = rowEntry.getValue().get(columnY);
			if (Y != null && !Y.isEmpty())
				m.merge(X == null?null:obtainValueFromCell(X,cellWithinX),parseObject(obtainValueFromCell(Y,cellWithinY)));
		}
	}
	
	/**
	 * Represents a graph.
	 * 
	 * @param <ELEM> type of elements for the X axis, vertical is always a Double
	 */
	public static abstract class RExperimentResult<ELEM extends Comparable<? super ELEM>> implements SGEExperimentResult
	{
		protected final File file;
		
		/** Number of entries in the graph. */
		protected int size = 0;
		
		/** Additional drawing command to append to a plot, such as abline() command. */
		protected List<String> extraCommands = new LinkedList<String>();
		
		public void addExtraCommand(String cmd)
		{
			extraCommands.add(cmd);
		}
		
		public RExperimentResult(File name)
		{
			file=name;
		}
		
		protected ELEM xMin = null, xMax = null;
		protected Double yMin = null, yMax = null;
		
		public void setXboundaries(ELEM min, ELEM max)
		{
			xMin = min;xMax = max;
		}
		
		public void setYboundaries(double min, double max)
		{
			yMin = min;yMax = max;
		}
		
		public int size()
		{
			return size;
		}
		
		/** Adds key-value pair, additionally permitting one to set both colour and a label for this 
		 * column of data values.
		 * @param el identifier for the column
		 * @param value value to be added to it
		 * @param colour colour with which box plot values are to be shown
		 * @param label label to show on the horizonal axis, empty string for no label.
		 */
		public abstract void add(ELEM el,Double value, String colour, String label);
		
		public abstract void add(ELEM el,Double value);
		
		/** Called to provide real-time updates to the learning results. The default does nothing. */
		@Override
		public void drawInteractive(@SuppressWarnings("unused") DrawGraphs gr)
		{
		}
		

		@Override
		public abstract void reportResults(DrawGraphs gr);
				
		/** Reports the name of the file with the graph, used for identification of different graphs. */
		@Override
		public String getFileName()
		{
			return file.getName();
		}

		public <T> void writeTaskOutput(Writer outputWriter,Comparable<T> x, Double y, String colour, String label) throws IOException
		{
			outputWriter.write(getFileName());outputWriter.write(SGE_ExperimentRunner.separator);
			outputWriter.write(x.getClass().getCanonicalName());outputWriter.write(SGE_ExperimentRunner.separator);outputWriter.write(x.toString());outputWriter.write(SGE_ExperimentRunner.separator);
			outputWriter.write(y.toString());outputWriter.write(SGE_ExperimentRunner.separator);
			if (colour != null)
				outputWriter.write(colour);
			outputWriter.write(SGE_ExperimentRunner.separator);
			if (label != null)
				outputWriter.write(label);
			outputWriter.write("\n");
		}

		/** When experiment completes, the results are written into a file as text. We need to load it into the experiment result file in order to collate across experiments for the final output. */
		@SuppressWarnings("unchecked")
		@Override
		public void parseTextLoadedFromExperimentResult(String[] line, String fileNameForErrorMessages, @SuppressWarnings("unused") ThreadResultID ignoredParameters, boolean onlyCheckItParses)
		{
			if (line.length != 6)
				throw new IllegalArgumentException("Experiment in "+fileNameForErrorMessages+" logged result with invalid number of values ("+line.length+") at "+line);
			String argType = line[1], argStringValue = line[2], color=null, label = null;
			if (!line[4].isEmpty())
				color = line[4];// yes, colour is a string here because it is passed to the R tool as-is and Java color will confuse it.
			if (!line[5].isEmpty())
				label = line[5];
			Double yValue = new Double(line[3]);
			
			Object argValue = null;
			if (argType.equals("java.lang.String"))
				argValue = argStringValue;
			else
				if (argType.equals("java.lang.Double"))
					argValue = new Double(argStringValue);
				else
					if (argType.equals("java.lang.Float"))
						argValue = new Float(argStringValue);
					else
						if (argType.equals("java.lang.Integer"))
							argValue = new Integer(argStringValue);
						else
							if (argType.equals("java.lang.Long"))
								argValue = new Long(argStringValue);
							else
								throw new IllegalArgumentException("cannot load a value of type "+argType);
			if (!onlyCheckItParses)
				add((ELEM) argValue,yValue,color,label);
		}
	}
	
	/** Data points in a scatterplot. */
	public final static class DataPoint
	{
		public final double x,y;
		
		public DataPoint(double a,double b)
		{
			x=a;y=b;
		}
	}

	/** Makes it possible to construct a scatterplot of x/y values in a few colours (that it, it is a superimposed collection of two plots). It derives from {@link RExperimentResult} 
	 * in order to benefit from the possibility of recording results to files. */
	public static class ScatterPlot extends RExperimentResult<Double>
	{
		protected final String xAxis,yAxis;
		
		public ScatterPlot(String x, String y, File name) 
		{
			super(name);xAxis = x;yAxis = y;
		}

		protected final Map<String,Collection<DataPoint> > values = new TreeMap<String,Collection<DataPoint> >();
 
		/** Adds key-value pair, additionally permitting one to set both colour and a label for this 
		 * column of data values.
		 * @param el identifier for the column
		 * @param value value to be added to it
		 * @param colour colour with which box plot values are to be shown
		 * @param label label to show on the horizonal axis, empty string for no label.
		 */
		@Override
		public synchronized void add(Double el,Double value, String colour, @SuppressWarnings("unused") String label)
		{
			if (yMin != null && yMin.doubleValue() > value.doubleValue()) return;
			if (yMax != null && yMax.doubleValue() < value.doubleValue()) return;
			
			if (xMin != null && xMin.compareTo(el) > 0) return;
			if (xMax != null && xMax.compareTo(el) < 0) return;
			
			add(el.doubleValue(),value.doubleValue(),colour);
		}
		
		public synchronized void add(double x, double y, String colour)
		{
			Collection<DataPoint> valuesForColour = values.get(colour);
			if (valuesForColour == null)
			{
				valuesForColour = new ArrayList<DataPoint>();values.put(colour,valuesForColour);
			}
			valuesForColour.add(new DataPoint(x,y));
		}
		
		@SuppressWarnings("unused")
		@Override
		public void add(Double el, Double value) 
		{
			throw new UnsupportedOperationException("a colour is required");
		}
		
		@Override
		public void drawInteractive(DrawGraphs gr)
		{
			List<String> drawingCommands = new LinkedList<String>();
			drawingCommands.addAll(getDrawingCommand());drawingCommands.addAll(extraCommands);
			gr.drawInteractivePlot(drawingCommands, file.getName());
		}

		public static double plotSize = 4;
		
		@Override
		public void reportResults(DrawGraphs gr)
		{
			if (values.size() > 0)
			{
				List<String> drawingCommands = new LinkedList<String>();
				drawingCommands.addAll(getDrawingCommand());drawingCommands.addAll(extraCommands);
				gr.drawPlot(drawingCommands, plotSize,plotSize,file);

			}
			else
				if (GlobalConfiguration.getConfiguration().isAssertEnabled())
					System.out.println("WARNING: ignoring empty plot that was supposed to be written into "+file);
		}

		/** Returns a command to draw a graph in R. */
		public List<String> getDrawingCommand() 
		{
			List<String> outcome = new ArrayList<String>();
			if (values.isEmpty()) throw new IllegalArgumentException("cannot plot an empty graph");
			
			// plot(as.vector(t[[1]]),as.vector(t[[2]]) , type = "p",col="blue",xlim=range(0,20), ylim=range(0, 20))
			boolean firstGraph = true;
			for(Entry<String,Collection<DataPoint> > entry:values.entrySet())
				if (!entry.getValue().isEmpty())
				{
					StringBuffer result = new StringBuffer();

					if (firstGraph)
						firstGraph = false;
					else
						outcome.add("par(new=TRUE)");

					result.append("plot(c(");
					boolean startVector = true;
					for(DataPoint d:entry.getValue())
					{
						if (!startVector) result.append(",");else startVector=false;
						result.append(d.x);
					}
					result.append("),c(");
					startVector = true;
					for(DataPoint d:entry.getValue())
					{
						if (!startVector) result.append(",");else startVector=false;
						result.append(d.y);
					}
					result.append("),type = \"p\",col=\"");result.append(entry.getKey());result.append("\",xlab=\"");result.append(xAxis);result.append("\",ylab=\"");result.append(yAxis);
					// thanks to http://stackoverflow.com/questions/1154242/getting-rid-of-axis-values-in-r-plot for the way to remove axes.
					result.append("\",axes=FALSE, frame.plot=TRUE)");
					outcome.add(result.toString());
				}
			
			return outcome;
		}
		
	}
	
	public static abstract class RGraph<ELEM extends Comparable<? super ELEM>> extends RExperimentResult<ELEM>
	{
		protected final String xAxis,yAxis;
		
		public RGraph(String x, String y, File name) 
		{
			super(name);xAxis = x;yAxis = y;
		}

		Map<ELEM,DataColumn> collectionOfResults = new TreeMap<ELEM,DataColumn>();
 
		/** Additional options for R. */
		protected String otherOptions = "";
		
		/** Sets additional options for R. */
		public void setOtherOptions(String str)
		{
			otherOptions = str;
		}
		
		protected Map<ELEM,String> relabellingOfLabels = new TreeMap<ELEM,String>();
		protected List<ELEM> orderingOfLabels = null;
		
		/** Permits labels to be altered - currently these labels are based on enums and intended for internal use. 
		 * For graphs these labels need to be modified. 
		 */
		public void setRelabelling(Map<ELEM,String> alteration)
		{
			for(Entry<ELEM,String> entry:alteration.entrySet())
				relabellingOfLabels.put(entry.getKey(), entry.getValue());
		}
		
		/** Gives an order to the appearance of columns in the output graph. */
		public void setOrderingOfLabels(Collection<ELEM> ordering)
		{
			if (orderingOfLabels != null)
				orderingOfLabels.clear();
			else
				orderingOfLabels = new ArrayList<ELEM>();
			
			orderingOfLabels.addAll(ordering);
		}
		
		/** Adds key-value pair, additionally permitting one to set both colour and a label for this 
		 * column of data values.
		 * @param el identifier for the column
		 * @param value value to be added to it
		 * @param colour colour with which box plot values are to be shown
		 * @param label label to show on the horizonal axis, empty string for no label.
		 */
		@Override
		public synchronized void add(ELEM el,Double value, String colour, String label)
		{
			if (yMin != null && yMin.doubleValue() > value.doubleValue()) return;
			if (yMax != null && yMax.doubleValue() < value.doubleValue()) return;
			
			if (xMin != null && xMin.compareTo(el) > 0) return;
			if (xMax != null && xMax.compareTo(el) < 0) return;
			
			DataColumn column = collectionOfResults.get(el);
			if (column == null) { column=new DataColumn();collectionOfResults.put(el,column); }
			column.results.add(value);
			if (colour != null) collectionOfResults.get(el).colour=colour;
			if (label != null) collectionOfResults.get(el).label=label;
			++size;
		}

		@Override
		public synchronized void add(ELEM el,Double value)
		{
			add(el,value,null,null);
		}
		
		protected double xSize = -1;
		
		/** Sets the horizontal size of the plot, vertical size is always set to 4 inches. 
		 * If no assignment is made, the size is guessed from the number of points on the drawing. 
		 */ 
		public void setXSize(double newSize)
		{
			xSize = newSize;
		}
		
		double ySize = 4;

		/** Computes the horizontal size of the drawing. */
		abstract protected double computeHorizSize();

		/** Returns a command to draw a graph in R. */
		protected abstract List<String> getDrawingCommand();
		
		@Override
		public void drawInteractive(DrawGraphs gr)
		{
			List<String> drawingCommands = new LinkedList<String>();
			drawingCommands.addAll(getDrawingCommand());drawingCommands.addAll(extraCommands);
			gr.drawInteractivePlot(drawingCommands, file.getName());
		}

		@Override
		public void reportResults(DrawGraphs gr)
		{
			if (collectionOfResults.size() > 0)
			{
				double horizSize = xSize;
				if (horizSize <= 0) horizSize=computeHorizSize();
				List<String> drawingCommands = new LinkedList<String>();
				drawingCommands.addAll(getDrawingCommand());drawingCommands.addAll(extraCommands);
				gr.drawPlot(drawingCommands, horizSize,ySize,file);

			}
			else
				if (GlobalConfiguration.getConfiguration().isAssertEnabled())
					System.out.println("WARNING: ignoring empty plot that was supposed to be written into "+file);
		}
		
		
	}
	
	public static abstract class RStatisticalAnalysis extends RExperimentResult<Double>
	{
		protected final String testName, extraArg;
		
		public RStatisticalAnalysis(String name, String extra, File fileName) 
		{
			super(fileName);testName = name;extraArg = extra;
		}

		final List<Double> valuesA = new ArrayList<Double>(1000);
		final List<Double> valuesB = new ArrayList<Double>(1000);

		@Override
		public synchronized void add(Double el,Double value)
		{
			if (yMin != null && yMin.doubleValue() > value.doubleValue()) return;
			if (yMax != null && yMax.doubleValue() < value.doubleValue()) return;
			
			if (xMin != null && xMin.compareTo(el) > 0) return;
			if (xMax != null && xMax.compareTo(el) < 0) return;

			valuesA.add(el);valuesB.add(value);
			++size;
		}
		
		@SuppressWarnings("unused")
		@Override
		public void add(Double el,Double value, String colour, String label)
		{
			add(el,value);
		}
		
		protected static String variableName = "m";
		
		@Override
		public void reportResults(@SuppressWarnings("unused") DrawGraphs gr)
		{
			if (valuesA.size() > 0)
			{
				StatisticalTestResult o=obtainResultFromR();
				FileWriter writer = null;
				try {
					writer = new FileWriter(file);
					writetofile(o,writer);
				} catch (IOException e) {
					Helper.throwUnchecked("Failed to write results into file "+file.getAbsolutePath(), e);
				}
				finally
				{
					if (writer != null)
						try {
							writer.close();
						} catch (IOException e) {
							// ignore this
						}
				}
			}
			else
				if (GlobalConfiguration.getConfiguration().isAssertEnabled())
					System.out.println("WARNING: ignoring empty plot that was supposed to be written into "+file);
		}
		
		/** Requests results of statistical analysis from R. */
		public StatisticalTestResult obtainResultFromR()
		{
			List<String> drawingCommands = new LinkedList<String>();
			drawingCommands.addAll(getDrawingCommand());drawingCommands.addAll(extraCommands);
			return StatisticalTestResult.performAnalysis(drawingCommands, variableName,getMethodName());
		}

		public List<String> getDrawingCommand()
		{
			if (valuesA.size() == 0 || valuesB.size()==0) throw new IllegalArgumentException("cannot plot an empty graph");
			if (valuesA.size() != valuesB.size()) throw new IllegalArgumentException(" 'x' and 'y' must have the same length");

			StringBuffer result = new StringBuffer();
			result.append(variableName+"="+testName+".test(");
			
			result.append(vectorToR(valuesA,false));
			result.append(",");
			result.append(vectorToR(valuesB,false));

			if (extraArg != null)
			{
				result.append(",");result.append(extraArg);
			}
			
			result.append(")");
			return Collections.singletonList(result.toString());
		}
		
		public abstract String getMethodName();
		
		/**
		 * Records the result of statistical analysis to a file.
		 */
		public abstract void writetofile(StatisticalTestResult result, Writer writer) throws IOException;
	
		public void writeSeparator(Writer writer) throws IOException { writer.append(','); }
		public void writeEndl(Writer writer) throws IOException { writer.append('\n'); }
		
		public void writeHeaderToFile(Writer writer) throws IOException
		{			
		    writer.append("Method");
		    writeSeparator(writer);
		    writer.append("Statistic");
		    writeSeparator(writer);
		    writer.append("P-value");
		}
		
		public void writeMainData(StatisticalTestResult o, Writer writer) throws IOException
		{
			writer.append(getMethodName());
			writeSeparator(writer);
		    writer.append(String.valueOf(o.statistic));
		    writeSeparator(writer);
		    writer.append(String.valueOf(o.pvalue));
		}
		
	}

	/** Builds a map from an array, where each element corresponds to a pair of strings.
	 * 
	 * @param data source data
	 * @return a string->string map
	 */
	public static Map<String,String> buildStringMapFromStringPairs(String [][] data)
	{
		Map<String,String> result = new HashMap<String,String>();
		for(String[] str:data)
		{
			if (str.length != 2)
				throw new IllegalArgumentException("more than two elements in sequence "+str);
			if (str[0] == null || str[1] == null)
				throw new IllegalArgumentException("invalid data in array");
			result.put(str[0],str[1]);
		}
		return result;
	}
	

	public static class RBoxPlot<ELEM extends Comparable<? super ELEM>> extends RGraph<ELEM>
	{
		public RBoxPlot(String x, String y, File name) {
			super(x, y, name);
		}
		
		@Override
		public List<String> getDrawingCommand()
		{
			List<List<Double>> data = new LinkedList<List<Double>>();
			List<String> names = new LinkedList<String>(), colours = new LinkedList<String>();
			
			if (orderingOfLabels != null)
			{
				Set<ELEM> o1 = new TreeSet<ELEM>(orderingOfLabels);o1.addAll(orderingOfLabels);
				if (!o1.equals(collectionOfResults.keySet()))
					throw new IllegalArgumentException("ordering of labels is "+orderingOfLabels+", actual labels are : "+collectionOfResults.keySet());
			}
			
			Iterator<ELEM> orderOfExplorationOfColumns = orderingOfLabels == null?collectionOfResults.keySet().iterator():orderingOfLabels.iterator();
			while(orderOfExplorationOfColumns.hasNext())
			{
				ELEM entryKey = orderOfExplorationOfColumns.next();
				DataColumn datacolumn = collectionOfResults.get(entryKey);
				
				data.add(datacolumn.results);
				String label = datacolumn.label;
				if (label == null)
					label = entryKey.toString();
				String relabelled = relabellingOfLabels.get(label);
				//System.out.println("orig: "+label+" relabelled: "+relabelled);
				if (relabelled != null)
					label = relabelled;
				names.add(label);
				String colour = datacolumn.colour;
				if (colour == null) colour = defaultColour;
				colours.add(colour);
			}
			return Collections.singletonList( (otherOptions.length()>0?otherOptions+",":"")+boxPlotToString(data, names.size()==1?null:names,colours,
					(!xAxis.isEmpty() || !yAxis.isEmpty())?	"xlab=\""+xAxis+"\",ylab=\""+yAxis+"\"":""
					));
		}

		@Override
		protected double computeHorizSize() {
			double horizSize=ySize*collectionOfResults.keySet().size()/5;if (horizSize < ySize) horizSize = ySize;
			return horizSize;
		}
	}
	
	/** Almost the same as RBoxPlot but horizontal labels are rotated vertically. */
	public static class RBoxPlotP<ELEM extends Comparable<? super ELEM>> extends RGraph<ELEM>
	{
		public RBoxPlotP(String x, String y, File name) {
			super(x, y, name);
		}
		
		@Override
		public List<String> getDrawingCommand()
		{
			List<List<Double>> data = new LinkedList<List<Double>>();
			List<String> names = new LinkedList<String>(), colours = new LinkedList<String>();
			if (orderingOfLabels != null)
			{
				Set<ELEM> o1 = new TreeSet<ELEM>(orderingOfLabels);o1.addAll(orderingOfLabels);
				if (!o1.equals(collectionOfResults.keySet()))
					throw new IllegalArgumentException("ordering of labels is "+orderingOfLabels+", actual labels are : "+collectionOfResults.keySet());
			}
				
			Iterator<ELEM> orderOfExplorationOfColumns = orderingOfLabels == null?collectionOfResults.keySet().iterator():orderingOfLabels.iterator();
			while(orderOfExplorationOfColumns.hasNext())
			{
				ELEM entryKey = orderOfExplorationOfColumns.next();
				DataColumn datacolumn = collectionOfResults.get(entryKey);
				data.add(datacolumn.results);
				String label = datacolumn.label;
				if (label == null)
					label = entryKey.toString();
				String relabelled = relabellingOfLabels.get(label);
				if (relabelled != null)
					label = relabelled;
				names.add(label);
				String colour = datacolumn.colour;
				if (colour == null) colour = defaultColour;
				colours.add(colour);
			}
			return Collections.singletonList(boxPlotToString(data, names.size()==1?null:names,colours,"xlab=\""+xAxis+"\",ylab=\""+yAxis+"\",las=2"+(otherOptions.length()>0?","+otherOptions:"")));
		}

		@Override
		protected double computeHorizSize() {
			double horizSize=ySize*collectionOfResults.keySet().size()/5;if (horizSize < ySize) horizSize = ySize;
			return horizSize;
		}
	}
	
	public static class Wilcoxon extends RStatisticalAnalysis
	{
		public Wilcoxon(File name) {
			super("wilcox","paired=TRUE", name);
		}
		
		@Override
		public String getMethodName()
		{
			return "Wilcoxon signed rank test";
		}
		
		@Override
		public void writetofile(StatisticalTestResult result, Writer writer) throws IOException 
		{
			writeHeaderToFile(writer);
			writeEndl(writer);
		    writeMainData(result, writer);
			writeEndl(writer);
		}
	}
	
	public static class Mann_Whitney_U_Test extends RStatisticalAnalysis
	{
		public Mann_Whitney_U_Test(File name) {
			super("wilcox",null, name);
		}
		
		@Override
		public String getMethodName()
		{
			return "Wilcoxon rank sum test";
		}

		@Override
		public void writetofile(StatisticalTestResult result, Writer writer) throws IOException 
		{
			writeHeaderToFile(writer);
			writeEndl(writer);
		    writeMainData(result, writer);
			writeEndl(writer);
		}
	}
	
	public static class Kruskal_Wallis extends RStatisticalAnalysis
	{
		public Kruskal_Wallis(File name) {
			super("kruskal",null, name);
		}
		
		@Override
		public String getMethodName()
		{
			return "Kruskal-Wallis rank sum test";
		}

		@Override
		public void writetofile(StatisticalTestResult result, Writer writer) throws IOException 
		{
			writeHeaderToFile(writer);
			writeSeparator(writer);
		    writer.append("parameter");
			writeEndl(writer);
		    writeMainData(result, writer);
			writeSeparator(writer);
		    writer.append(String.valueOf(result.parameter));
			writeEndl(writer);
		}
	}
	
	
	public static class RBagPlot extends Graph2D
	{
		public RBagPlot(String x, String y, File name) {
			super(x, y,"bagplot", name);
		}
		/** Returns a string reflecting the number of points R bagplot analysis will be limited to. */
		protected String formatApproxLimit()
		{
			return (limit == null?"":"approx.limit="+limit.intValue());
		}
		protected Integer limit = null;

		/** By default, R's bagplot limits the number of points for analysis to 300, this one makes it possible to change that value. */
		public void setLimit(int value)
		{
			limit = value;
		}
		
		@Override
		public String otherOptions()
		{
			return formatApproxLimit();
		}
	}

	public static class Graph2D extends RGraph<Double>
	{
		public Graph2D(String x, String y, String plotKind, File name) {
			super(x, y, name);plotType=plotKind;
		}
		List<List<Double>> data = null;
		List<Double> names = null;
		
		public void computeDataSet()
		{
			data = new LinkedList<List<Double>>();
			names = new LinkedList<Double>();
			for(Entry<Double,DataColumn> entry:collectionOfResults.entrySet())
			{
				data.add(entry.getValue().results);names.add(entry.getKey());
			}
		}
		
		final protected String plotType;
		
		public String otherOptions()
		{
			return "";
		}
		
		@Override
		public List<String> getDrawingCommand()
		{
			computeDataSet();
			return Collections.singletonList(datasetToString(plotType,data, names,"xlab=\""+xAxis+"\",ylab=\""+yAxis+"\""+
					otherOptions()));
		}
		
		public boolean graphOk()
		{
			Rectangle2D.Double graphSize = getSize();
			if (graphSize.width < Configuration.fpAccuracy)
				return false;
			if (graphSize.height < Configuration.fpAccuracy)
				return false;
		
			return true;
		}
		
		/** Computes the data for abline to draw a diagonal. */
		public String computeDiagonal()
		{
			Rectangle2D.Double graphSize = getSize();
			if (graphSize.width < Configuration.fpAccuracy)
				throw new IllegalArgumentException("width is too small");
			if (graphSize.height < Configuration.fpAccuracy)
				throw new IllegalArgumentException("height is too small");
			
			double k = graphSize.height/graphSize.width;
			double diff = graphSize.y-k*graphSize.x;
			
			return "abline("+diff+","+k+")";
		}
		
		public Rectangle2D.Double getSize()
		{
			Double xValueMin = null, xValueMax = null, yValueMin = null, yValueMax = null;
			// if there is nothing useful to draw, do not pass the command to Bagplot - it will crash (as of May 24, 2011).
			Iterator<Entry<Double,DataColumn>> resultIterator = collectionOfResults.entrySet().iterator();

			while(resultIterator.hasNext())
			{
				Entry<Double,DataColumn> entry = resultIterator.next();
				if (xValueMin == null) xValueMin = entry.getKey();
				if (xValueMax == null) xValueMax = entry.getKey();
				if (xValueMin.compareTo(entry.getKey()) > 0) xValueMin = entry.getKey();
				if (xValueMax.compareTo(entry.getKey()) < 0) xValueMax = entry.getKey();
				
				for(Double y:entry.getValue().results)
				{
					if (yValueMin == null) yValueMin = y;
					if (yValueMax == null) yValueMax = y;
					if (yValueMin.compareTo(y) > 0) yValueMin = y;
					if (yValueMax.compareTo(y) < 0) yValueMax = y;
				}
			}
			
			if (xValueMin == null || yValueMin == null || xValueMax == null || yValueMax == null)
				return new Rectangle2D.Double();
			
			return new Rectangle2D.Double(xValueMin,yValueMin,xValueMax-xValueMin,yValueMax-yValueMin);
		}
		
		@Override
		public void drawInteractive(DrawGraphs gr)
		{
			if (!checkSingleDot())
				super.drawInteractive(gr);
			else
				if (GlobalConfiguration.getConfiguration().isAssertEnabled())
					System.out.println("WARNING: not popping bagplot "+file+" consisting of a single dot due to R bug");
		}
		
		@Override
		protected double computeHorizSize() {
			return ySize;
		}

		public boolean checkSingleDot() {
			return getSize().width < Configuration.fpAccuracy && getSize().height < Configuration.fpAccuracy;
		}
	}
	
	/** Draws a square bag plot. */
	public static class SquareBagPlot extends RBagPlot
	{
		protected final boolean diag;
		protected final double minValue, maxValue;

		/**
		 * @param x name of the X axis
		 * @param y name of the Y axis
		 * @param name file where to store .pdf (should include the extension).
		 * @param from the minimal value of x or y.
		 * @param to the maximal value of x or y.
		 * @param diagonal whether to draw a diagonal line
		 */
		public SquareBagPlot(String x, String y, File name, double from, double to, boolean diagonal) {
			super(x, y, name);diag = diagonal;minValue = from;maxValue = to;
		}
		
		@Override
		public List<String> getDrawingCommand()
		{
			computeDataSet();
			List<String> result = new LinkedList<String>();
			result.add("bplot<-compute."+datasetToString(plotType,data, names,formatApproxLimit()));
			result.add("plot(bplot,xlim=c("+minValue+","+maxValue+"), ylim=c("+minValue+","+maxValue+"),xlab=\""+xAxis+"\",ylab=\""+yAxis+"\")");
			if (diag) result.add("abline(0,1)");
			return result;
		}
	}
	
	/**
	 * Records the outcome of statistical analysis, extracted from R.
	 */
	public static class StatisticalTestResult
	{
		/** Data to be displayed. */
		double statistic=0.;
		double pvalue=0.;
		String alternative; 
		double parameter=0.;

		
		public static double valueAsDouble(REXP val)
		{
			switch(val.getType())
			{
			case REXP.XT_INT:
			case REXP.XT_ARRAY_INT:
				return val.asInt();
			case REXP.XT_DOUBLE:
			case REXP.XT_ARRAY_DOUBLE:
				return val.asDouble();
			case REXP.XT_NULL:
				return 0;
			default:
				throw new IllegalArgumentException("value "+val+" is not an integer or a double");			
			}
		}
		
		/** Using a supplied list of commands, obtains a result. 
		 * 
		 * @param drawingCommand commands to run, the outcome of the last one is reported.
		 * @param varName the variable used to assign the outcome in the commands executed.
		 * @param expectedMethodName When computing a result, R reports the name of the method used. We can use it to check that the right method was passed in the commands to compute the result, just in case.
		 * @return the results of the analysis, computed by running the supplied list of commands.
		 */
		public static StatisticalTestResult performAnalysis(List<String> drawingCommands,String varName, String expectedMethodName)
		{
			if (drawingCommands.isEmpty())
				throw new IllegalArgumentException("no command to perform statistical analysis");
			StatisticalTestResult STR=new StatisticalTestResult();
			for(String cmd:drawingCommands)
				eval(cmd,"failed to run "+cmd);
			STR.statistic=valueAsDouble(engine.eval(varName+"$statistic"));
			STR.pvalue=valueAsDouble(engine.eval(varName+"$p.value"));
			STR.alternative=engine.eval(varName+"$alternative").asString();
			STR.parameter=valueAsDouble(engine.eval(varName+"$parameter"));
			String methodName = engine.eval(varName+"$method").asString();
			if (!methodName.startsWith(expectedMethodName))
				throw new IllegalArgumentException("expected to use method \""+expectedMethodName+"\" but got \""+methodName+"\"");
			return STR;
		}
	}
}

