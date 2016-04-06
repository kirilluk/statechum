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

import java.util.Arrays;
import java.util.List;

import statechum.Configuration;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

/**
 *
 * @author ramsay
 */
public class BooleanSignature  extends Signature {

	public BooleanSignature(@SuppressWarnings("unused") Configuration config, OtpErlangList attributes)
	{
		super();
		if (attributes.arity() != 0) throw new IllegalArgumentException("BooleanSignature does not accept attributes");
		erlangTermForThisType = erlangTypeToString(attributes,null);
	}

	/** Values are not constrained, but term has to be boolean. */
	@Override
	public boolean typeCompatible(OtpErlangObject term) {
		if (!(term instanceof OtpErlangBoolean))
		{
			if (!(term instanceof OtpErlangAtom)) 
				return false;			
			String boolValue = (((OtpErlangAtom)term).atomValue()).toLowerCase();
			return boolValue.equals("true") || boolValue.equals("false");
		}
		return true;
	}

	@Override
	public List<OtpErlangObject> instantiateAllAlts() {
		return Arrays.asList(new OtpErlangObject[]{new OtpErlangBoolean(true),new OtpErlangBoolean(false)});
	}

}
