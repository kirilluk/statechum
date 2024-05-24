/* Copyright (c) 2018 The University of Sheffield
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

package statechum.analysis.learning.experiments;

public class ComputePathLengthDistribution 
{
	public final int plsSlotNumber,plsDivisor,plsStateNumber;
	
	public ComputePathLengthDistribution(int slotNumber, int divisor,int stateNumber)
	{
		plsSlotNumber = slotNumber;plsDivisor = divisor;plsStateNumber = stateNumber;
	}
	
	protected int [] statistics = null;
	
	/** Accumulates the number of times a particular path length has been seen, in terms of stateNumber/plsDivisor.
	 * The number of slots is the number of entries. First slot (number zero) is where path length is below stateNumber/plsDivisor,
	 * last slot is for above or equal to stateNumber*(plsSlotNumber-1)/plsDivisor 
	 * every slot n is [stateNumber*n/plsDivisor,stateNumber*(n+1)/plsDivisor[
	 * @param len the length of the current path
	 */
	public void updatePathLengthStatistics(int len)
	{
		if (statistics == null)
			statistics = new int [plsSlotNumber];
		int position =-1;
		if (len >= plsStateNumber*(plsSlotNumber-1)/plsDivisor)
			position = plsSlotNumber -1;
		else
			position = len*plsDivisor/plsStateNumber;
		statistics[position]++;
	}
	
	public void reportLengthStatistics()
	{
		int total = 0;
		for(int i=0;i<plsSlotNumber;++i)
			total+=statistics[i];
		for(int i=0;i<plsSlotNumber-1;++i)
		{
			System.out.printf("%03d - %03d %03d%%\n", 100*i/plsDivisor, 100*(i+1)/plsDivisor, 100*statistics[i]/total);
		}
		System.out.printf("%03d - ... %03d%%\n", 100*(plsSlotNumber-1)/plsDivisor, 100*statistics[plsSlotNumber-1]/total);
	}
	
	public String lengthStatisticsAsString(char separator)
	{
		int total = 0;
		for(int i=0;i<plsSlotNumber;++i)
			total+=statistics[i];
		StringBuilder result = new StringBuilder();
		for(int i=0;i<plsSlotNumber-1;++i)
		{
			result.append((double)statistics[i]/total);result.append(separator);
		}		
		result.append((double)statistics[plsSlotNumber-1]/total);
		return result.toString();
	}
}
