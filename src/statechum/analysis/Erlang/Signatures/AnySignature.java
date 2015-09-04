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
import java.util.List;

import statechum.Configuration;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

/** The underscore (any(), or don't-care) signature
 * @author ramsay
 */
public class AnySignature extends Signature {
	protected final List<OtpErlangObject> values;
	
	public AnySignature(Configuration config, OtpErlangList attributes)
	{
		super();
		if (attributes.arity() != 0) throw new IllegalArgumentException("AnySignature does not accept attributes");
		
		List<OtpErlangObject> result = new ArrayList<OtpErlangObject>();
		switch(config.getErlangAlphabetAnyElements())
		{
		case ANY_INT:
			result.add(new OtpErlangInt(10));
			result.add(new OtpErlangInt(11));
			result.add(new OtpErlangInt(12));
			break;
		case ANY_WIBBLE:
			result.add(new OtpErlangAtom("AnyWibble"));
			break;
		case ANY_WITHLIST:
			// We are allowed anything so lets try a few things...
			if (config.getUseANumberOfValues())
			{
				result.add(new OtpErlangAtom("JustAnythingA"));
				result.add(new OtpErlangList(new OtpErlangObject[] {}));
				result.add(new OtpErlangList(new OtpErlangObject[] {new OtpErlangAtom("WibbleA")}));
				result.add(new OtpErlangList(new OtpErlangObject[] {new OtpErlangAtom("WibbleA"), new OtpErlangAtom("WobbleA")}));
			}
			else
				result.add(new OtpErlangAtom("A"));// the motivation for a shorter label is that longer ones would look really long on large graphs.
			break;
		}
		values = Collections.unmodifiableList(result);
		erlangTermForThisType = erlangTypeToString(attributes,null);
	}
	
	@Override
	public boolean typeCompatible(@SuppressWarnings("unused") OtpErlangObject term) 
	{
		return true;
	}

	@Override
	public List<OtpErlangObject> instantiateAllAlts() {
		return values;
	}
}
