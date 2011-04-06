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

// This file is based on rtest.java

package statechum.analysis.learning.experiments;
import java.util.List;

import org.rosuda.JRI.REXP;
import org.rosuda.JRI.RMainLoopCallbacks;
import org.rosuda.JRI.Rengine;

public class DrawGraphs {
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

		@SuppressWarnings("unused")
		@Override
		public String rReadConsole(Rengine arg0, String prompt, int arg2) {
			// dummy
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
			// dummy
		}

		@SuppressWarnings("unused")
		@Override
		public void rWriteConsole(Rengine arg0, String arg1, int arg2) {
			consoleBuffer.append(arg1);consoleBuffer.append('\n');
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
	protected ConsoleCallbacks callbacks = new ConsoleCallbacks();
	
	public DrawGraphs()
	{
		if (!Rengine.versionCheck()) 
			throw new IllegalArgumentException("R version mismatch");
		if (engine == null)
		{
			engine = new Rengine(new String[]{},false,callbacks);
			if (!engine.waitForR())
				throw new IllegalArgumentException("loading R failed");
		}
	}
	
	protected REXP eval(String whatToEval, String errMsg)
	{
		callbacks.clearBuffer();
		REXP result = engine.eval(whatToEval);
		if (result == null)
			throw new IllegalArgumentException(errMsg+" : "+callbacks.getBuffer());
		return result;
	}
	
	protected String boxPlotToString(List<List<Double>> data,List<String> names,String colour)
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
			result.append("c(");
			boolean startVector = true;
			for(Double elem:arg)
			{
				if (!startVector) result.append(",");else startVector=false;
				result.append(elem);
			}
			result.append(")");
		}
		
		// names
		if (names != null)
		{
			result.append(",names=c(");
			boolean startVector = true;
			for(String elem:names)
			{
				if (!startVector) result.append(",");else startVector=false;
				result.append('\"');
				result.append(elem);
				result.append('\"');
			}
			result.append(")");
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
		result.append(")");
		return result.toString();
	}
	
	/** Draws a box-plot given the data to plot and stores it in the given file.
	 * 
	 * @param data data from which to plot
	 * @param names names to give to boxes
	 * @param fileName where to store result.
	 */
	public void drawBoxPlot(List<List<Double>> data,List<String> names,String fileName)
	{
		eval("pdf(\""+fileName+"\",7,7)","redirection to pdf("+fileName+") failed");
		eval(boxPlotToString(data, names,"green"),"failed to run boxplot");
		eval("dev.off()","failed to write to "+fileName);
	}
	
	public static void end()
	{
		engine.end();engine=null;
	}
	
}
