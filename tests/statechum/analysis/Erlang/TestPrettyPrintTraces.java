/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
 * 
 * statechum is free software: you can redistribute it and/or modify
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
package statechum.analysis.Erlang;

import org.junit.*;
import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Label;
import statechum.analysis.learning.RPNILearner;
import statechum.analysis.learning.rpnicore.LearnerGraph;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

/** Tests that AutoAnswers works.
 * 
 * @author kirill
 *
 */
public class TestPrettyPrintTraces {

	@Test
	public void testPrettyPrintTrace1() throws IOException
	{
		File file = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGEXAMPLES),"locker/locker.erl");config.setErlangMboxName(ErlangRuntime.getDefaultRuntime().createNewRunner().getRunnerName());
		ErlangModule.setupErlangConfiguration(config,file);config.setErlangCompileIntoBeamDirectory(true);config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		ErlangModule.loadModule(config);
		final String LBL1 = "{call, read}", LBL2 = "{call, lock}";
		final LearnerGraph gr = buildLearnerGraph("A- "+LBL1+" ->B-"+LBL2+"->B", "testConvertToModuleFailure1", config,null);
		Iterator<Label> lblIter = gr.pathroutines.computeAlphabet().iterator();
		ErlangLabel lbl1 = (ErlangLabel)lblIter.next(),lbl2 = (ErlangLabel)lblIter.next();
		List<Label> trace = Arrays.asList(new Label[]{lbl1,lbl2,lbl2});
		Assert.assertEquals("[{?F(),'call','read'},{?F(),'call','lock'},{?F(),'call','lock'}]",RPNILearner.questionToString(trace));
	}

	private Configuration config;
	
	@BeforeClass
	public static void beforeClass()
	{
		ErlangRuntime.getDefaultRuntime();
	}
	
	@AfterClass
	public static void afterClass()
	{
		ErlangRuntime.getDefaultRuntime().killErlang();
	}
	
	@Before
	public void setup()
	{
		config = Configuration.getDefaultConfiguration().copy();
	}

}
