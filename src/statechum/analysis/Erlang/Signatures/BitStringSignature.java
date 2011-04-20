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

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;

public class BitStringSignature extends Signature {
	public final int Base,Unit;
	
	public BitStringSignature(OtpErlangList attributes,OtpErlangList values) throws OtpErlangRangeException
	{
		if (attributes.arity() != 0) throw new IllegalArgumentException("BitStringSignature does not accept attributes");

		if (values.arity() != 3) throw new IllegalArgumentException("invalid values "+values+" passed to BitStringSignature"); 
		Base = ((OtpErlangLong) values.elementAt(0)).intValue();Unit =  ((OtpErlangLong) values.elementAt(1)).intValue();
		if (!IntSignature.AntiStringAtom.equals(values.elementAt(2))) throw new IllegalArgumentException("The third element of list "+values+" should be an atom to stop it from becoming a string");
	}
	
	@Override
	public OtpErlangObject instantiate() {
		return null;
	}

}
