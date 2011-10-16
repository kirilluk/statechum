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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import statechum.Configuration;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

/**
 *
 * @author ramsay
 */
public class AtomSignature extends Signature {

	/** Possible values, null means any, but it is also possible to create this with a second 
	 * constructor in which case one may pass an empty set of values, which would make little
	 * sense as this type will become equivalent to ?none. 
	 */
	protected final Set<OtpErlangObject> valuesAsSet;
	protected final List<OtpErlangObject> valuesAsList;
	
	/** Arbitrary values. */
	public AtomSignature(@SuppressWarnings("unused") Configuration config, OtpErlangList attributes)
	{
		if (attributes.arity() != 0) throw new IllegalArgumentException("AtomSignature does not accept attributes");
		valuesAsSet = null;valuesAsList = null;
		erlangTermForThisType = erlangTypeToString(attributes,null);
	}
	
	/** Specific values. */
	public AtomSignature(@SuppressWarnings("unused") Configuration config, OtpErlangList attributes,OtpErlangList argValues)
	{
		if (attributes.arity() != 0) throw new IllegalArgumentException("AtomSignature does not accept attributes");

		valuesAsList = new ArrayList<OtpErlangObject>(argValues.arity());
		valuesAsSet  = new HashSet<OtpErlangObject>(argValues.arity());
		for(int i=0;i<argValues.arity();++i)
		{
			if (!(argValues.elementAt(i) instanceof OtpErlangAtom)) throw new IllegalArgumentException("Cannot build an atom from values "+argValues+" some of which are not atoms"); 
			valuesAsList.add(argValues.elementAt(i));valuesAsSet.add(argValues.elementAt(i));
		}
		erlangTermForThisType = erlangTypeToString(attributes,argValues);
	}
	
    @Override
	public List<OtpErlangObject> instantiateAllAlts() {
    	if (valuesAsList == null) return Collections.singletonList((OtpErlangObject)new OtpErlangAtom("wibble"));
    	return valuesAsList;
    }

	@Override
	public boolean typeCompatible(OtpErlangObject term) {
		if (!(term instanceof OtpErlangAtom)) return false;
		if (valuesAsSet != null) return valuesAsSet.contains(term);
		return true;// if values are not constrained, any will do.
	}
}
