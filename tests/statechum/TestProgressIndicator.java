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

import org.junit.Assert;
import org.junit.Test;

import statechum.ProgressIndicator;

public class TestProgressIndicator {
	@Test
	public final void testProgressIndicator1()
	{
		ProgressIndicator pr = new ProgressIndicator("A",-1);
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator2()
	{
		ProgressIndicator pr = new ProgressIndicator("A",0);
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator3()
	{
		ProgressIndicator pr = new ProgressIndicator("A",1);
		Assert.assertEquals(1, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator4()
	{
		ProgressIndicator pr = new ProgressIndicator("A",2);
		Assert.assertEquals(1, pr.getP());
		pr.next();
		Assert.assertEquals(5, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator5()
	{
		ProgressIndicator pr = new ProgressIndicator("A",3);
		Assert.assertEquals(1, pr.getP());
		pr.next();
		Assert.assertEquals(4, pr.getP());
		pr.next();
		Assert.assertEquals(7, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator6()
	{
		ProgressIndicator pr = new ProgressIndicator("A",10);
		Assert.assertEquals(1, pr.getP());
		for(int i=0;i<10;++i)
		{
			pr.next();
			Assert.assertEquals(i+1, pr.getP());
		}
		pr.next();
		Assert.assertEquals(10, pr.getP());
		pr.next();
		Assert.assertEquals(10, pr.getP());
	}
	
	@Test
	public final void testProgressIndicator7()
	{
		ProgressIndicator pr = new ProgressIndicator("A",15);
		Assert.assertEquals(1, pr.getP());
		pr.next();
		Assert.assertEquals(1, pr.getP());
		pr.next();
		Assert.assertEquals(2, pr.getP());
	}
	
}
