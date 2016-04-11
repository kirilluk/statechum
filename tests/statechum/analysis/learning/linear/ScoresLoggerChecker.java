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

import org.junit.Assert;

import statechum.Configuration;

import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

class ScoresLoggerChecker extends ScoresLogger
{
	/* (non-Javadoc)
	 * @see statechum.analysis.learning.linear.ScoresLogger#saveMap()
	 */
	@Override
	public void saveMap()
	{
		// dummy implementation for a checker.
	}
	
	/* (non-Javadoc)
	 * @see statechum.analysis.learning.linear.ScoresLogger#checkOrRecord(java.lang.String, com.ericsson.otp.erlang.OtpErlangTuple)
	 */
	@Override
	public void checkOrRecord(String name, OtpErlangTuple value)
	{
		OtpErlangTuple knownScores = scoresMap.get(name);Assert.assertNotNull("missing entry for test"+ name,knownScores);
		Assert.assertEquals(knownScores.arity(),value.arity());
		for(int i=0;i<knownScores.arity();++i)
		{
			OtpErlangList listA=(OtpErlangList)knownScores.elementAt(i), listB= (OtpErlangList)value.elementAt(i);
			Assert.assertEquals(listA.arity(),listB.arity());
			for(int elem=0;elem<listA.arity();++elem)
				Assert.assertEquals(((OtpErlangDouble)listA.elementAt(elem)).doubleValue(),((OtpErlangDouble)listB.elementAt(elem)).doubleValue(),Configuration.fpAccuracy);
		}
	}
}