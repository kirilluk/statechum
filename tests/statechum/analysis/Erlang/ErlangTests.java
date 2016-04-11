/* Copyright (c) 2006, 2007, 2008 The University of Sheffield
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
package statechum.analysis.Erlang;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({
    statechum.analysis.Erlang.TestErlangStartupFailure.class,
    statechum.analysis.Erlang.TestErlangModule.class,
    statechum.analysis.Erlang.TestErlangRunner.class,
    statechum.analysis.Erlang.TestErlangParser.class,
    statechum.analysis.Erlang.TestErlangParser.TestParseBitStrFail.class,
    statechum.analysis.Erlang.TestErlangParser.TestParseDoubleFail.class,
    statechum.analysis.Erlang.TestErlangParser.TestParseInvalidCharsInAtomFail.class,
    statechum.analysis.learning.TestErlangOracleLearner.class,
    statechum.analysis.Erlang.Signatures.TestTypes.class,
    statechum.analysis.Erlang.TestErlangGraphs.class,
    statechum.analysis.Erlang.TestSynapseAuxiliaryFunctions.class,
    statechum.analysis.Erlang.TestSynapse.class
})
public class ErlangTests {// all tests are included in the annotation.
}

