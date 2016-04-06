/* Copyright (c) 2006-2010 Neil Walkinshaw and Kirill Bogdanov
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

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Map.Entry;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.junit.Assert;

import statechum.Helper;
import statechum.analysis.Erlang.ErlangLabel;

import com.ericsson.otp.erlang.OtpErlangTuple;

/** This one updates the existing scores with new data. This is why {@link ScoresLoggerRecorder#loadMap} is not empty. */
class ScoresLoggerRecorder extends ScoresLogger
{
	
	/* (non-Javadoc)
	 * @see statechum.analysis.learning.linear.ScoresLogger#saveMap()
	 */
	@Override
	public void saveMap()
	{
		OutputStream of=null;ZipOutputStream outputStream=null;
		try
		{
			of = new FileOutputStream(scoresFileName);
			// using http://java.sun.com/developer/technicalArticles/Programming/compression/
			outputStream = new ZipOutputStream(of);
			for(Entry<String,OtpErlangTuple> entry:scoresMap.entrySet())
			{
				/*OtpErlangTuple tuple = new OtpErlangTuple(new OtpErlangObject[]{
						new OtpErlangString(entry.getKey()),entry.getValue()
				});*/
				ZipEntry zipEntry = new ZipEntry(entry.getKey());
				zipEntry.setMethod(ZipEntry.DEFLATED);
				outputStream.putNextEntry(zipEntry);
				// If I use a Writer here, I'll have to flush its buffer otherwise the output is incomplete.
				outputStream.write(ErlangLabel.dumpErlangObject(entry.getValue()).getBytes());
				outputStream.closeEntry();
				
			}
			outputStream.close();outputStream = null;
			of.close();of = null;
		}
		catch (IOException e) {
			Helper.throwUnchecked("failed to save scores", e);
		}
		finally
		{
			if (outputStream != null) { try { outputStream.close();outputStream=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
			if (of != null) { try { of.close();of=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
		}
	}
	
	/** Records the supplied scores in the map.
	 * 
	 * @param name name of the experiment
	 * @param value scores reported by GD.
	 */
	@Override
	public void checkOrRecord(String name, OtpErlangTuple value)
	{
		OtpErlangTuple knownScores = scoresMap.get(name);Assert.assertNull(knownScores);
		scoresMap.put(name,value);
	}
}