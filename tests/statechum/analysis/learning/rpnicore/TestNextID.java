/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import junit.framework.Assert;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.Configuration.IDMode;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.JUConstants.VERTEXLABEL;
import statechum.analysis.learning.TestRpniLearner;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraphND;

@RunWith(Parameterized.class)
public class TestNextID {
	
	private static Configuration confJung, confString, confSame;

	private final Configuration config;
	@SuppressWarnings("unused")
	private final String description;
	private final LearnerGraphND graph;
	
	public TestNextID(Configuration conf, String desc)
	{
		config = conf;description = desc;
		graph = buildLearnerGraphND("S-a->S\nA1-a->A2\nS-a->S1-b->"+"A-a->A1-a-#ARej\nA1-d->A2-d->A3\nA1-c->A2-c->A3"
				+TestRpniLearner.PTA3, "testCopyGraph4",config,null);
	}
	
	static 
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();

		confJung = config.copy();confJung.setLearnerUseStrings(false);confJung.setLearnerCloneGraph(true);
		confString = config.copy();confString.setLearnerUseStrings(true);confString.setLearnerCloneGraph(true);
		confSame = config.copy();confSame.setLearnerUseStrings(false);confSame.setLearnerCloneGraph(false);		
	}
	
	@Parameters
	public static Collection<Object[]> data() 
	{
		
		List<Object[]> result = new LinkedList<Object[]>();
		
		for(IDMode mode:IDMode.values())
		{
			Configuration confJungM = confJung.copy();confJung.setLearnerIdMode(mode);
			result.add(new Object[]{confJungM,"Jung"});
			Configuration confStringM = confString.copy();confStringM.setLearnerIdMode(mode);
			result.add(new Object[]{confStringM,"String"});
			Configuration confSameM = confSame.copy();confStringM.setLearnerIdMode(mode);
			result.add(new Object[]{confSameM,"Same"});
		}
		
		return result; 
	}
	
	/** Given a test configuration, returns a textual description of its purpose. 
	 * 
	 * @param config configuration to consider
	 * @param kind brief description
	 * 
	 * @return extended description.
	 */ 
	public static String parametersToString(Configuration config, String kind)
	{
		return kind+","+config.getLearnerIdMode();
	}
	
	@Test
	public void testNextID1()
	{
		VertexID id1 = graph.nextID(VERTEXLABEL.ACCEPT,true), 
			id2 = graph.nextID(VERTEXLABEL.ACCEPT, true),
			id3 = graph.nextID(VERTEXLABEL.ACCEPT, true);
		Assert.assertFalse(id1.equals(id2));
		Assert.assertFalse(id1.equals(id3));
		Assert.assertFalse(id2.equals(id3));
	}
	
	@Test
	public void testNextID2()
	{
		VertexID id1 = graph.nextID(VERTEXLABEL.ACCEPT,true), 
			id2 = graph.nextID(VERTEXLABEL.REJECT, true),
			id3 = graph.nextID(VERTEXLABEL.ACCEPT, true);
		Assert.assertFalse(id1.equals(id2));
		Assert.assertFalse(id1.equals(id3));
		Assert.assertFalse(id2.equals(id3));
	}
	
	@Test
	public void testNextID3()
	{
		VertexID id1 = graph.nextID(VERTEXLABEL.NOLABEL,true), 
			id2 = graph.nextID(VERTEXLABEL.ACCEPT, true),
			id3 = graph.nextID(VERTEXLABEL.REJECT, true);
		Assert.assertFalse(id1.equals(id2));
		Assert.assertFalse(id1.equals(id3));
		Assert.assertFalse(id2.equals(id3));
	}
	
	public void checkWithCondition(VERTEXLABEL label)
	{
		VertexID id1 = graph.nextID(label,true), 
		idTmp1 = graph.nextID(label, false),
		idTmp2 = graph.nextID(label, false),
		id2 = graph.nextID(label,true),
		idTmp3 = graph.nextID(label,false),
		idTmp4 = graph.nextID(label,false);
		
		Assert.assertTrue(idTmp1.equals(idTmp2));
		Assert.assertTrue(idTmp3.equals(idTmp4));
		Assert.assertFalse(idTmp1.equals(idTmp3));
		
		Assert.assertFalse(id1.equals(id2));
		Assert.assertFalse(id1.equals(idTmp1));
		Assert.assertFalse(id1.equals(idTmp2));
		Assert.assertFalse(id1.equals(idTmp3));
		Assert.assertFalse(id1.equals(idTmp4));

		Assert.assertTrue(id2.equals(idTmp1)); // id2 is the same as idTmp1 and 2
		Assert.assertTrue(id2.equals(idTmp2));
		Assert.assertFalse(id2.equals(idTmp3));
		Assert.assertFalse(id2.equals(idTmp4));
	}
	
	@Test
	public void testNextIDNonPermanent1()
	{
		checkWithCondition(VERTEXLABEL.ACCEPT);
	}

	@Test
	public void testNextIDNonPermanent2()
	{
		checkWithCondition(VERTEXLABEL.REJECT);
	}
}
