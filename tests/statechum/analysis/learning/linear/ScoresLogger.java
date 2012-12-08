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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.junit.Assert;

import statechum.Configuration;
import statechum.Helper;
import statechum.analysis.Erlang.ErlangLabel;

import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

class ScoresLogger
{
	private Map<String,OtpErlangTuple> scoresMap = new TreeMap<String,OtpErlangTuple>();
	
	private static final String scoresFileName = "resources"+File.separator+"gd_scores.zip";
	
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
			if (inputStream != null)
				try {inputStream.close();} catch (IOException e1) {// ignore this
				}
			if (is != null)
				try {is.close();} catch (IOException e1) {// ignore this
				}
		}
	}
	
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
		}
		catch (IOException e) {
			Helper.throwUnchecked("failed to save scores", e);
		}
		finally
		{
			if (outputStream != null) 
				try { outputStream.close(); } catch (IOException e1) {// ignore this
				}
				
			if (of != null)
				try { of.close(); } catch (IOException e1) {// ignore this
				}
		}
	}
	
	/** Checks that scores corresponding to the supplied name are known and equal to the provided ones.
	 * 
	 * @param name name of the experiment
	 * @param value scores reported by GD.
	 */
	public void check(String name, OtpErlangTuple value)
	{
		OtpErlangTuple knownScores = scoresMap.get(name);Assert.assertNotNull("unknown entry "+name,knownScores);
		Assert.assertEquals(knownScores.arity(),value.arity());
		for(int i=0;i<knownScores.arity();++i)
		{
			OtpErlangList listA=(OtpErlangList)knownScores.elementAt(i), listB= (OtpErlangList)value.elementAt(i);
			Assert.assertEquals(listA.arity(),listB.arity());
			for(int elem=0;elem<listA.arity();++elem)
				Assert.assertEquals(((OtpErlangDouble)listA.elementAt(elem)).doubleValue(),((OtpErlangDouble)listB.elementAt(elem)).doubleValue(),Configuration.fpAccuracy);
		}
	}
	
	/** Records the supplied scores in the map.
	 * 
	 * @param name name of the experiment
	 * @param value scores reported by GD.
	 */
	public void record(String name, OtpErlangTuple value)
	{
		OtpErlangTuple knownScores = scoresMap.get(name);Assert.assertNull(knownScores);
		scoresMap.put(name,value);
	}
}