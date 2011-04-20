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
package statechum.analysis.Erlang.Signatures;

import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

/**
 *
 * @author ramsay
 */
public class AtomSignature extends Signature {

	protected final ArrayList<OtpErlangObject> values;
	
	/** Arbitrary values. */
	public AtomSignature(OtpErlangList attributes)
	{
		if (attributes.arity() != 0) throw new IllegalArgumentException("AtomSignature does not accept attributes");
		values = new ArrayList<OtpErlangObject>(1);values.set(0, new OtpErlangAtom("wibble"));
	}
	
	/** Specific values. */
	public AtomSignature(OtpErlangList attributes,OtpErlangList argValues)
	{
		if (attributes.arity() != 0) throw new IllegalArgumentException("AtomSignature does not accept attributes");

		values = new ArrayList<OtpErlangObject>(argValues.arity());
		for(int i=0;i<argValues.arity();++i)
		{
			if (!(argValues.elementAt(i) instanceof OtpErlangAtom)) throw new IllegalArgumentException("Cannot build an atom from values "+argValues+" some of which are not atoms"); 
			values.add(argValues.elementAt(i));
		}
	}
	
    @Override
	public OtpErlangObject instantiate() {
        return values.isEmpty()? null:values.get(0);
    }
    
    @Override
	public List<OtpErlangObject> instantiateAllAlts() {
    	return values;
    }
}
