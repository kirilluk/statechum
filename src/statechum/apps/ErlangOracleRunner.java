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
 * 
 */
package statechum.apps;

import java.util.Collection;
import statechum.analysis.Erlang.ErlangModule;

/**
 *
 * @author ramsay
 */
public class ErlangOracleRunner implements Runnable {

	protected Collection<ErlangModule> otherModules;

    protected String mode = "basic";
    protected ErlangModule m;
    protected ErlangModule module = null;
    public ErlangOracleRunner(ErlangModule mod, Collection<ErlangModule> om) {
    	module = mod;

        /*initArgs = "[";
        for(String a:  module.behaviour.initArgs) {
            if(!initArgs.equals("[")) {
                initArgs += ",";
            }
            initArgs += a;
        }
        initArgs += "]";*/
    }

    public void run() {
        ErlangQSMOracle.mode = mode;
        //ErlangQSMOracle.main(new String[]{"test2.out", moduleName, functionName, Alphabet, otherModules});
    }
}
