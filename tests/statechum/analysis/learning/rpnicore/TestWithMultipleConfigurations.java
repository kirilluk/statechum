/* Copyright (c) 2013 The University of Sheffield.
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

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import statechum.Configuration;
import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

/** Makes it possible to easily test classes parameterised by a few type of transition matrix.
 */
public class TestWithMultipleConfigurations 
{
	public static final Configuration 
	configStd = Configuration.getDefaultConfiguration().copy(), 
	configTree = Configuration.getDefaultConfiguration().copy(), 
	configArray = Configuration.getDefaultConfiguration().copy(),
	configHash = Configuration.getDefaultConfiguration().copy();

	static
	{
		configArray.setTransitionMatrixImplType(STATETREE.STATETREE_ARRAY);
		configHash.setTransitionMatrixImplType(STATETREE.STATETREE_LINKEDHASH);
		configTree.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
	}
	
	/** Label converter to use. */
	public final ConvertALabel converter;
	
	/** The configuration to use when running tests. */
	public final Configuration mainConfiguration;

	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		Collection<Object []> result = new LinkedList<Object []>();
		result.add(new Object[]{configTree});
		result.add(new Object[]{configArray});
		result.add(new Object[]{configHash});
		return result;
	}
	
	@org.junit.runners.ParameterizedWithName.ParametersToString
	public static String parametersToString(Configuration config)
	{
		return "matrix: "+(config.getTransitionMatrixImplType());
	}

	public TestWithMultipleConfigurations(Configuration argConfig)
	{
		mainConfiguration = argConfig.copy();
		mainConfiguration.setAllowedToCloneNonCmpVertex(true);
		converter = mainConfiguration.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?new Transform.InternStringLabel():null;
	}
	
	/** Converts arrays of labels to lists of labels using config - it does not really matter which configuration is used 
	 * because all of them start from a default one and do not modify label type.
	 * 
	 * @param labels what to convert
	 * @return the outcome of conversion.
	 */
	protected List<Label> labelList(String [] labels)
	{
		return AbstractLearnerGraph.buildList(Arrays.asList(labels),mainConfiguration,converter);
	}
}
