/* Copyright (c) 2013 The University of Sheffield
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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;
import java.util.TreeMap;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.junit.Assert;

import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.Erlang.ErlangLabel;

import com.ericsson.otp.erlang.OtpErlangTuple;

public abstract class ScoresLogger 
{
	protected Map<String,OtpErlangTuple> scoresMap = new TreeMap<String,OtpErlangTuple>();
	
	static final String scoresFileName = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.RESOURCES)+File.separator+"gd_scores.zip";

	/** Loads the data from a map. */
	public void loadMap()
	{
		InputStream is=null;ZipInputStream inputStream = null;
		try {
			is = new FileInputStream(scoresFileName);inputStream = new ZipInputStream(is);
			
			ZipEntry zipEntry = inputStream.getNextEntry();
			while(zipEntry != null)
			{
				BufferedReader rd = new BufferedReader(new InputStreamReader(inputStream));
				String fileString= rd.readLine();Assert.assertNotNull(fileString);
				scoresMap.put(zipEntry.getName(), (OtpErlangTuple)ErlangLabel.parseText(fileString));
				zipEntry = inputStream.getNextEntry();
			/*
			while ((fileString = rd.readLine()) != null) {
				OtpErlangTuple value = (OtpErlangTuple)ErlangLabel.parseText(fileString);Assert.assertEquals(2,value.arity());
				OtpErlangTuple tuple = (OtpErlangTuple)value.elementAt(1);Assert.assertEquals(2,tuple.arity());
				
				scoresMap.put(((OtpErlangString)value.elementAt(0)).stringValue(), tuple);
			}
			*/
			}
		} catch (IOException e) {
			Helper.throwUnchecked("failed to load scores", e);
		}
		finally
		{
			if (inputStream != null) { try { inputStream.close();inputStream=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
			if (is != null) { try { is.close();is=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
		}
	}

	/** Stores the log data in a map. */
	public abstract void saveMap();

	/** Checks that scores corresponding to the supplied name are known and equal to the provided ones.
	 * 
	 * @param name name of the experiment
	 * @param value scores reported by GD.
	 */
	public abstract void checkOrRecord(String name, OtpErlangTuple value);
}