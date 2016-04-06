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

import java.util.Collections;
import java.util.List;

import statechum.Configuration;

import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class FloatSignature extends Signature {

	public FloatSignature(@SuppressWarnings("unused") Configuration config, OtpErlangList attributes)
	{
		if (attributes.arity() != 0) throw new IllegalArgumentException("FloatSignature does not accept attributes");
		erlangTermForThisType = erlangTypeToString(attributes,null);
	}
	
	@Override
	public boolean typeCompatible(OtpErlangObject term) {
		return term instanceof OtpErlangFloat || term instanceof OtpErlangDouble;
	}

	@Override
	public List<OtpErlangObject> instantiateAllAlts() {
		return Collections.singletonList((OtpErlangObject)new OtpErlangDouble(96.63));

	}

	
}
