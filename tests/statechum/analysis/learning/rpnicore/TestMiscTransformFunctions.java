/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

package statechum.analysis.learning.rpnicore;

import static statechum.analysis.learning.rpnicore.Transform.HammingDistance;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

public class TestMiscTransformFunctions {
	@Test(expected=IllegalArgumentException.class)
	public final void testHammingDistance0()
	{
		HammingDistance(
				Arrays.asList(new Boolean[]{true}), Arrays.asList(new Boolean[]{}));
	}

	@Test 
	public final void testHammingDistance1()
	{
		Assert.assertEquals(0, HammingDistance(
				Arrays.asList(new Boolean[]{}),
				Arrays.asList(new Boolean[]{})
		));
	}
	
	@Test 
	public final void testHammingDistance2()
	{
		Assert.assertEquals(0, HammingDistance(
				Arrays.asList(new Boolean[]{false}),
				Arrays.asList(new Boolean[]{false})
		));
	}
	
	@Test 
	public final void testHammingDistance3()
	{
		Assert.assertEquals(1, HammingDistance(
				Arrays.asList(new Boolean[]{false}),
				Arrays.asList(new Boolean[]{true})
		));
	}
	
	@Test 
	public final void testHammingDistance4()
	{
		Assert.assertEquals(1, HammingDistance(
				Arrays.asList(new Boolean[]{true,false,false}),
				Arrays.asList(new Boolean[]{true,true,false})
		));
	}
	
	@Test 
	public final void testHammingDistance5()
	{
		Assert.assertEquals(3, HammingDistance(
				Arrays.asList(new Boolean[]{true,true,false}),
				Arrays.asList(new Boolean[]{false,false,true})
		));
	}

	/** Removes all whitespace characters from the string, by iterating through it. 
	 * 
	 * @param str string to transform
	 * @return result of transformation
	 */
	public static String removeWhiteSpace(String str)
	{
		StringBuffer result = new StringBuffer();for(int i=0;i<str.length();++i) if (!Character.isWhitespace(str.charAt(i))) result.append(str.charAt(i));
		return result.toString();
	}
	
	@Test
	public final void testRemoveWhiteSpace1()
	{
		Assert.assertEquals("",removeWhiteSpace(""));
	}
	
	@Test
	public final void testRemoveWhiteSpace2()
	{
		Assert.assertEquals("test",removeWhiteSpace("test"));
	}
	
	@Test
	public final void testRemoveWhiteSpace3()
	{
		Assert.assertEquals("thisisatest343*()",removeWhiteSpace("this is\r a\n\t test 343 *()\n\n\t"));
	}
	

}
