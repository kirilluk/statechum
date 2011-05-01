/* Copyright (c) 2011 The University of Sheffield.
 * 
 * This file is part of StateChum
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
package statechum;

public class ProgressIndicator
{
	private int cnt=0,p=1;
	private final int total;

	public ProgressIndicator(String nameArg,int t)
	{
		total = t;
		// if total <=0, little will be displayed, I presume this is ok.
		printStr("["+nameArg+" ");
		if (total <= 0) 
		{
			end();p=10;// the assignment to p is for testing
		}
	}
	
	public void next()
	{
		if (cnt < total)
		{// ignore invalid use
			++cnt;
			
			while(cnt > total*p/10)
			{
				++p;printStr(".");
			}
			if (cnt == total) end();
		}
	}
	
	private void printStr(String str)
	{
		if (GlobalConfiguration.getConfiguration().isAssertEnabled())
			System.out.print(str);
	}
	
	private void end()
	{
		printStr(".] ");
	}
	
	/** For testing only. */
	int getP()
	{
		return p;
	}
}